use std::{fs, path::Path};

use anyhow::{anyhow, bail, Context, Error, Result};
use derive_builder::Builder;
use filetime::FileTime;
use getset::Getters;
use globset::{GlobBuilder, GlobMatcher};
#[allow(unused_imports)]
use log::{log_enabled, trace, warn};
use os_display::Quotable;
use walkdir::WalkDir;

use crate::util;

#[derive(Clone, Debug, Default)]
pub struct OptionAttrs {
    pub mode: Option<u32>,
    #[cfg(unix)]
    pub uid: Option<u32>,
    #[cfg(unix)]
    pub gid: Option<u32>,
}

#[derive(Clone, Debug)]
pub struct Attributes {
    pub mode: bool,
    #[cfg(unix)]
    pub ownership: bool,
    /// 是否保留目录文件amtimes
    pub timestamps: bool,
    /// 是否跟随软链接。默认为false
    #[cfg(unix)]
    pub xattr: bool,
    // /// 是否保持硬链接
    // links: bool,
}

impl Default for Attributes {
    fn default() -> Self {
        Self::no_attrs()
    }
}

impl Attributes {
    pub fn no_attrs() -> Self {
        Self {
            mode: false,
            timestamps: false,
            #[cfg(unix)]
            ownership: false,
            #[cfg(unix)]
            xattr: false,
        }
    }

    pub fn all() -> Self {
        Self {
            mode: true,
            timestamps: true,
            #[cfg(unix)]
            ownership: true,
            #[cfg(unix)]
            xattr: true,
        }
    }
}

impl CopierBuilder {
    pub fn try_glob_attrs<T, I>(&mut self, attrs: T) -> Result<&mut Self>
    where
        T: IntoIterator<Item = (String, I)>,
        I: TryInto<OptionAttrs, Error = Error>,
    {
        let a = attrs
            .into_iter()
            .map(|(m, a)| {
                let g = GlobBuilder::new(&m).literal_separator(true).build()?;
                let a = TryInto::<OptionAttrs>::try_into(a)?;
                Ok::<_, Error>((g.compile_matcher(), a))
            })
            .collect::<Result<Vec<_>>>()?;
        self.glob_attrs = Some(Some(a));
        Ok(self)
    }
}

#[derive(Clone, Debug, Default, Builder, Getters)]
#[builder(setter(into, strip_option), default)]
#[getset(get = "pub")]
pub struct Copier {
    #[builder(setter(custom))]
    glob_attrs: Option<Vec<(GlobMatcher, OptionAttrs)>>,
    attrs: Attributes,
}

impl Copier {
    /// 递归的复制目录树。如果src是一个文件且dst是已存在的目录，将会复制文件到dst/中
    pub fn copy<P, Q>(&self, src: P, dst: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (src, dst) = (src.as_ref(), dst.as_ref());
        if !src.exists() {
            bail!("Not found exists path: {}", src.quote());
        }

        if src.is_dir() {
            return self.copy_dir(src, dst);
        }
        // from is file
        let to = if dst.is_dir() {
            dst.join(
                src.file_name()
                    .ok_or_else(|| anyhow!("Not found file name for path {}", src.quote()))?,
            )
        } else {
            dst.to_path_buf()
        };
        self.copy_file(src, to)?;
        Ok(())
    }

    /// 复制文件或仅复制当前目录及属性，但不包括目录中的文件
    pub fn copy_file_or_dir_only<P, Q>(&self, src: P, dst: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (src, dst) = (src.as_ref(), dst.as_ref());
        if src.is_symlink() || src.is_file() {
            self.copy_file(src, dst)?;
        } else {
            fs::create_dir_all(dst)?;
            self.copy_attrs(src, dst)?;
        }
        Ok(())
    }

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
                .glob_attrs
                .as_ref()
                .and_then(|v| {
                    v.iter()
                        .find(|(m, _)| m.is_match(src))
                        .map(|(_, a)| (a.uid, a.gid))
                })
                .unwrap_or_default();
            if uid.is_some() || gid.is_some() || self.attrs.ownership {
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

            if self.attrs.xattr {
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

            use std::os::unix::prelude::{MetadataExt, PermissionsExt};
            let new_mode = self.glob_attrs.as_ref().and_then(|v| {
                v.iter()
                    .find(|(m, _)| m.is_match(src))
                    .and_then(|(_, a)| a.mode)
            });
            // The `chmod()` system call that underlies the
            // `fs::set_permissions()` call is unable to change the
            // permissions of a symbolic link. In that case, we just
            // do nothing, since every symbolic link has the same
            // permissions.
            if !dst_meta.is_symlink() {
                if new_mode.is_some() || self.attrs.mode {
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
        }

        if self.attrs.timestamps {
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
}
