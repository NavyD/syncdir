use std::{
    collections::HashMap,
    fs, io,
    os::unix::prelude::{MetadataExt, PermissionsExt},
    path::{Path, PathBuf},
};

use anyhow::{anyhow, bail, Context, Error, Result};
use filetime::FileTime;
use log::{debug, log_enabled, trace, warn};
use os_display::Quotable;
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

use crate::{sync::SyncPath, util};

macro_rules! sync_path {
    ($src:expr) => {
        SyncPath::Removed($src)
    };
    ($src:expr, $dst:expr) => {
        if $dst.exists() || $dst.is_symlink() {
            SyncPath::Overriden($src, $dst)
        } else {
            SyncPath::Coppied($src, $dst)
        }
    };
}

struct OptionAttr {
    mode: Option<u32>,
    #[cfg(target_family = "unix")]
    uid: Option<u32>,
    #[cfg(target_family = "unix")]
    gid: Option<u32>,
}

struct Attribute {
    mode: bool,
    #[cfg(unix)]
    ownership: bool,
    /// 是否保留目录文件amtimes
    timestamps: bool,
    /// 是否跟随软链接。默认为false
    xattr: bool,
    // /// 是否保持硬链接
    // links: bool,
}

impl Default for Attribute {
    fn default() -> Self {
        Self {
            mode: true,
            #[cfg(target_family = "unix")]
            ownership: false,
            timestamps: true,
            xattr: true,
            // links: true,
        }
    }
}

struct Copier {
    path_attrs: Option<HashMap<PathBuf, OptionAttr>>,
    default_attr: Attribute,
}

impl Copier {
    fn copy_attrs<P, Q>(&self, src: P, dst: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (src, dst) = (src.as_ref(), dst.as_ref());

        let src_meta = fs::symlink_metadata(src)?;
        let dst_meta = fs::symlink_metadata(dst)?;

        #[cfg(unix)]
        {
            let (uid, gid) = self
                .path_attrs
                .as_ref()
                .and_then(|v| v.get(src).map(|a| (a.uid, a.gid)))
                .unwrap_or_default();
            if uid.is_some() || gid.is_some() || self.default_attr.ownership {
                let (uid, gid) = (
                    uid.unwrap_or_else(|| src_meta.uid()),
                    gid.unwrap_or_else(|| src_meta.gid()),
                );
                trace!(
                    "Changing ownership {}:{} of dst path {}",
                    uid,
                    gid,
                    dst.quote()
                );
                nix::unistd::chown(dst, Some(uid.into()), Some(gid.into()))?;
            }

            for xname in xattr::list(src)? {
                if let Some(xval) = xattr::get(src, &xname)? {
                    trace!(
                        "Changing xattr {:?}={:?} for dst path {}",
                        xname,
                        xval,
                        dst.quote()
                    );
                    xattr::set(dst, xname, &xval)?;
                }
            }
        }

        let new_mode = self
            .path_attrs
            .as_ref()
            .and_then(|v| v.get(dst).and_then(|a| a.mode));
        // The `chmod()` system call that underlies the
        // `fs::set_permissions()` call is unable to change the
        // permissions of a symbolic link. In that case, we just
        // do nothing, since every symbolic link has the same
        // permissions.
        if !dst_meta.is_symlink() {
            if new_mode.is_some() || self.default_attr.mode {
                let new_mode = new_mode.unwrap_or_else(|| src_meta.mode());
                let mut perm = src_meta.permissions();
                trace!(
                    "Changing mode from {:#o} to {:#o} for dst path {}",
                    dst_meta.mode(),
                    new_mode,
                    dst.quote(),
                );
                perm.set_mode(new_mode);
                fs::set_permissions(dst, perm)?;
            }
        } else if let Some(new_mode) = new_mode {
            warn!(
                "Ignoring settings mode from {:#o} to {:#o} for symlink dst path {}",
                dst_meta.mode(),
                new_mode,
                dst.quote()
            );
        }

        if self.default_attr.timestamps {
            let atime = FileTime::from_last_access_time(&src_meta);
            let mtime = FileTime::from_last_modification_time(&src_meta);
            trace!(
                "Changing atime={}ns,mtime={}ns for dst path {}",
                atime.nanoseconds(),
                mtime.nanoseconds(),
                dst.quote()
            );
            if dst_meta.is_symlink() {
                filetime::set_symlink_file_times(dst, atime, mtime)?;
            } else {
                filetime::set_file_times(dst, atime, mtime)?;
            }
        }

        Ok(())
    }

    /// copy all recursively
    pub fn copy<P, Q>(&self, src: P, dst: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (src, dst) = (src.as_ref(), dst.as_ref());
        if !src.exists() {
            bail!("Not found exists path: {}", src.display());
        }

        if src.is_dir() {
            return self.copy_dir(src, dst);
        }
        // from is file
        let to = if dst.is_dir() {
            dst.join(
                src.file_name()
                    .ok_or_else(|| anyhow!("Not found file name for path {}", src.display()))?,
            )
        } else {
            dst.to_path_buf()
        };
        self.copy_file(src, &to)?;
        Ok(())
    }

    fn copy_dir<P, Q>(&self, src: P, dst: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (from, to) = (src.as_ref(), dst.as_ref());
        if !from.is_dir() {
            bail!("Not found from: {}", from.quote());
        }
        if to.exists() && (!to.is_dir() || to.read_dir().map(|mut d| d.next().is_some())?) {
            bail!(
                "Cannot copy a dir {} to exists file or non empty dir {}",
                from.quote(),
                to.quote()
            );
        }

        WalkDir::new(from).into_iter().try_for_each(|entry| {
            let entry = entry?;
            let src = entry.into_path();
            let dst = util::map_path(&src, from, to)?;

            if log_enabled!(log::Level::Trace) {
                let src_ty = if src.is_symlink() {
                    "link"
                } else if src.is_dir() {
                    "dir "
                } else {
                    "file"
                };
                trace!("Copying {} {} to {}", src_ty, src.quote(), dst.quote());
            }

            if let Some(p) = dst.parent() {
                fs::create_dir_all(p)?;
            }
            if src.is_symlink() {
                util::try_remove(&dst)?;
                util::symlink(src.read_link()?, &dst)?;
                self.copy_attrs(&src, &dst)?;
            } else if src.is_dir() {
                if dst.is_symlink() || dst.is_file() {
                    fs::remove_file(&dst)?;
                } else if !dst.exists() {
                    fs::create_dir(&dst)?;
                }
                self.copy_attrs(&src, &dst)?;
            } else if src.is_file() {
                self.copy_file(&src, &dst)?;
            }

            Ok::<_, Error>(())
        })?;
        Ok(())
    }

    /// 复制文件from内容与属性到文件to中。如果to已存在将被覆盖
    ///
    /// * 如果from是一个symlink将读取symlink指向的路径链接到to新symlink
    /// * 否则将使用[std::fs::copy]复制内容到新文件
    pub fn copy_file<P, Q>(&self, from: P, to: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (src, dst) = (from.as_ref(), to.as_ref());

        if let Some(p) = dst.parent().filter(|pp| !pp.exists()) {
            trace!("Creating all dir {}", p.quote());
            fs::create_dir_all(p)?;
        }
        if src.is_symlink() {
            if dst.is_symlink() || dst.is_file() {
                fs::remove_file(dst)?;
            } else if dst.is_dir() {
                fs::remove_dir(dst)?;
            }
            let link = src.read_link()?;
            trace!("Copying sym link {} -> {}", link.quote(), dst.quote());
            util::symlink(&link, dst)?;
        } else {
            trace!("Copying file {} -> {}", src.quote(), dst.quote());
            fs::copy(src, dst)?;
        }

        self.copy_attrs(src, dst)
            .with_context(|| format!("Copy attrs {} -> {}", src.quote(), dst.quote()))?;
        Ok(())
    }

    fn diff<P, Q>(&self, from: P, to: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        todo!()
    }

    //     /// 比较两个文件是否一致
    //     ///
    //     /// ## symlink
    //     ///
    //     /// 如果文件都为symlink，则比较链接路径是否一致，如果存在一个为symlink则直接返回true。
    //     /// 如果配置了[Self::follow_symlinks]=true则将读取symlink指向路径的文件比较
    //     ///
    //     /// 对于symlink本身的权限信息比较将被忽略，
    //     /// 参考[How do file permissions apply to symlinks?](https://superuser.com/a/303063)
    //     ///
    //     /// 注意：[std::fs::metadata]将跟随symlink，而[std::fs::symlink_metadata]会获取symlink本身
    //     pub fn has_changed<P, Q>(&self, new: P, old: Q) -> Result<bool>
    //     where
    //         P: AsRef<Path>,
    //         Q: AsRef<Path>,
    //     {
    //         let (new, old) = (new.as_ref(), old.as_ref());

    //         let (new, old) = if !self.get_attr(old).follow_symlinks {
    //             match (new.is_symlink(), old.is_symlink()) {
    //                 (true, true) => {
    //                     trace!("Reading symlink path for changed");
    //                     return Ok(fs::read_link(new)? == fs::read_link(old)?);
    //                 }
    //                 (false, false) => (new.to_path_buf(), old.to_path_buf()),
    //                 _ => {
    //                     trace!("Found changes with one symlink");
    //                     return Ok(true);
    //                 }
    //             }
    //         } else {
    //             (
    //                 if new.is_symlink() {
    //                     fs::read_link(new)?
    //                 } else {
    //                     new.to_path_buf()
    //                 },
    //                 if old.is_symlink() {
    //                     fs::read_link(old)?
    //                 } else {
    //                     old.to_path_buf()
    //                 },
    //             )
    //         };

    //         #[cfg(target_family = "unix")]
    //         {
    //             let (new_meta, old_meta) = (new.metadata()?, old.metadata()?);
    //             let attr = self.get_attr(&old);
    //             if (attr.mode.is_some()
    //                 && new_meta.permissions().mode() != old_meta.permissions().mode())
    //                 || (attr.uid.is_some() && new_meta.uid() != old_meta.uid())
    //                 || (attr.gid.is_some() && new_meta.gid() != old_meta.gid())
    //             {
    //                 trace!(
    //                     "Found file metadata changed {:?} != {:?}",
    //                     new_meta,
    //                     old_meta
    //                 );
    //                 return Ok(true);
    //             }
    //         }

    //         debug!("Computing the hash of file {}", new.display());
    //         let mut hasher = Sha256::new();
    //         io::copy(&mut fs::File::open(&new)?, &mut hasher)?;
    //         let hash_new = hasher.finalize();

    //         debug!("Computing the hash of file {}", old.display());
    //         let mut hasher = Sha256::new();
    //         io::copy(&mut fs::File::open(&old)?, &mut hasher)?;
    //         let hash_old = hasher.finalize();

    //         if log_enabled!(log::Level::Trace) {
    //             trace!(
    //                 "Comparing hash for file {}: {} and file {}: {}",
    //                 new.display(),
    //                 std::str::from_utf8(&hash_new)?,
    //                 old.display(),
    //                 std::str::from_utf8(&hash_old)?,
    //             );
    //         }
    //         Ok(hash_new == hash_old)
    //     }

    //     fn copy_metadata<P, Q>(&self, from: P, to: Q) -> Result<()>
    //     where
    //         P: AsRef<Path>,
    //         Q: AsRef<Path>,
    //     {
    //         // #[cfg(target_family = "unix")]
    //         // {
    //         //     let (from, to) = (from.as_ref(), to.as_ref());
    //         //     let meta = self.metadata(from)?;
    //         //     // setup rights
    //         //     if let Some(mode) = self.mode {
    //         //         let mut perm = meta.permissions();
    //         //         debug!(
    //         //             "Setting up permission mode from {} to {} for path {}",
    //         //             perm.mode(),
    //         //             mode,
    //         //             to.display()
    //         //         );
    //         //         perm.set_mode(mode);
    //         //         fs::set_permissions(to, perm)?;
    //         //     }
    //         //     if self.uid.is_some() || self.gid.is_some() {
    //         //         debug!(
    //         //             "Setting up owner to {}:{} for path {}",
    //         //             self.uid.unwrap_or_else(|| meta.uid()),
    //         //             self.gid.unwrap_or_else(|| meta.gid()),
    //         //             to.display()
    //         //         );
    //         //         chown(to, self.uid.map(Into::into), self.gid.map(Into::into))?;
    //         //     }
    //         // }
    //         Ok(())
    //     }

    //     fn metadata<P: AsRef<Path>>(&self, p: P) -> io::Result<fs::Metadata> {
    //         // let p = p.as_ref();
    //         // if p.is_symlink() && !self.follow_symlinks {
    //         //     p.symlink_metadata()
    //         // } else {
    //         //     p.metadata()
    //         // }
    //         todo!()
    //     }

    //     fn copy_filetimes<P, Q>(&self, from: P, to: Q) -> Result<()>
    //     where
    //         P: AsRef<Path>,
    //         Q: AsRef<Path>,
    //     {
    //         let (from, to) = (from.as_ref(), to.as_ref());
    //         if let Ok(meta) = from.symlink_metadata() {
    //             if let Err(e) = filetime::set_symlink_file_times(
    //                 to,
    //                 FileTime::from_last_access_time(&meta),
    //                 FileTime::from_last_modification_time(&meta),
    //             ) {
    //                 warn!("Failed to set file {} atime/mtime: {}", to.display(), e);
    //             }
    //         } else {
    //             let meta = from.metadata()?;

    //             if let Err(e) = filetime::set_file_times(
    //                 to,
    //                 FileTime::from_last_access_time(&meta),
    //                 FileTime::from_last_modification_time(&meta),
    //             ) {
    //                 warn!("Failed to set file {} atime/mtime: {}", to.display(), e);
    //             }
    //         }
    //         Ok(())
    //     }
}
