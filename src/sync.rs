use std::{
    fs, io,
    os::unix::fs::symlink,
    path::{Path, PathBuf},
};
#[cfg(target_family = "unix")]
use {
    nix::unistd::chown,
    std::os::unix::prelude::{MetadataExt, PermissionsExt},
};

use anyhow::{anyhow, bail, Error, Result};
use derive_builder::Builder;
use dialoguer::{theme::ColorfulTheme, Confirm};
use filetime::FileTime;
use itertools::Itertools;
use log::{debug, info, log_enabled, trace, warn};
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

use crate::{config::LastDestinationConfig, util};

#[derive(Debug, Clone, Builder)]
pub struct Syncer {
    src: PathBuf,
    dst: PathBuf,
    non_interactive: bool,
    copier: Copier,
    last_conf: LastDestinationConfig,
}

impl Syncer {
    /// 将dst中的文件同步到src中。
    ///
    /// * 如果存在last_dsts则将所有的last_dsts文件整个复制到src目录中，src目录将被清空
    /// * 如果不存在last_dsts将从当前src中加载目录映射到dst中
    ///     * 如果dst的文件不存在于src中则移除src文件
    ///     * 否则比较是否改变
    ///         * 如果src与dst文件不同则覆盖src文件
    ///         * 否则跳过
    pub fn sync_back<T, P>(&self, target_dsts: T) -> Result<()>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let target_dsts = target_dsts.into_iter().collect_vec();
        if let Some(p) = target_dsts
            .iter()
            .map(AsRef::as_ref)
            .find(|p| !p.starts_with(&self.dst))
        {
            bail!(
                "Found a target dst {} not start with dst {}",
                p.display(),
                self.dst.display()
            );
        }

        let last_dsts = self.last_conf.load_last_dsts()?;
        if last_dsts.is_empty() {
            return self.sync_back_with_src(&target_dsts);
        }
        if !self.src.exists() || self.confirm_rm(&self.src)? {
            if target_dsts.is_empty() {
                self.copier.copy(&self.dst, &self.src)?;
            } else {
                for dst in target_dsts {
                    let src = self.get_src_path(&dst)?;
                    self.copier.copy(dst, src)?;
                }
            }
        } else {
            warn!(
                "Skipped sync dst {} back to src {}",
                self.dst.display(),
                self.src.display()
            );
        }
        Ok(())
    }

    pub fn apply_to_dst(&self) -> Result<()> {
        WalkDir::new(&self.src).into_iter().try_for_each(|entry| {
            let entry = entry?;
            if entry.path_is_symlink() || entry.file_type().is_file() {
                let from = entry.path();
                let to = self.get_dst_path(entry.path())?;
                if !to.exists() || self.copier.has_changed(from, &to)? {
                    self.copier.copy_file(from, &to)?;
                }
            }
            Ok::<_, Error>(())
        })?;
        Ok(())
    }

    fn get_dst_path(&self, src_sub: impl AsRef<Path>) -> Result<PathBuf> {
        util::map_path(src_sub.as_ref(), &self.src, &self.dst)
    }

    fn get_src_path(&self, dst_sub: impl AsRef<Path>) -> Result<PathBuf> {
        util::map_path(dst_sub.as_ref(), &self.dst, &self.src)
    }

    fn confirm_rm(&self, p: impl AsRef<Path>) -> Result<bool> {
        let p = p.as_ref();
        if p.exists() {
            Ok(true)
        } else if self.non_interactive
            || Confirm::with_theme(&ColorfulTheme::default())
                .default(true)
                .show_default(true)
                .with_prompt(format!("Do you want to remove path {}", p.display()))
                .interact()?
        {
            if p.is_dir() {
                info!("Removing dir {}", p.display());
                fs::remove_dir_all(p)?;
            } else {
                info!("Removing file {}", p.display());
                fs::remove_file(p)?;
            }
            Ok(true)
        } else {
            warn!("Skipped remove {}", p.display());
            Ok(false)
        }
    }

    fn sync_back_with_src<T, P>(&self, target_dsts: T) -> Result<()>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let target_dsts = target_dsts.into_iter().collect_vec();
        if !self.src.exists() {
            info!(
                "Skipped sync back with empty src {} from dst {}",
                self.src.display(),
                self.dst.display()
            );
            return Ok(());
        }
        for entry in WalkDir::new(&self.src) {
            let entry = entry?;
            let src = entry.path();
            let dst = self.get_dst_path(src)?;

            // skip non target dsts
            if !target_dsts.is_empty() && target_dsts.iter().all(|t| !dst.starts_with(t)) {
                trace!(
                    "Skipped sync dst {} not in {} target dsts",
                    dst.display(),
                    target_dsts.len()
                );
                continue;
            }
            // 在walkdir后不应该找不到path
            if !src.exists() {
                warn!(
                    "Skipped not exists path {} after walk dir {}",
                    src.display(),
                    self.src.display()
                );
                continue;
            }
            // dst不存在 移除src
            if !dst.exists() && self.confirm_rm(src)? {
                continue;
            }

            // dst目录时 当src是 不存在或目录则mkdir -p, 文件则移除
            if dst.is_dir() && (!src.exists() || !src.is_file() || self.confirm_rm(src)?) {
                if let Err(e) = fs::create_dir(src) {
                    // ignore exists dir error: mkdir -p
                    if !src.is_dir() {
                        return Err(e.into());
                    }
                }
                self.copier.copy_metadata(&dst, src)?;
                continue;
            }

            // dst为文件/软链接时 移除src目录
            if !src.is_dir() || self.confirm_rm(src)? {
                self.copier.copy_file(&dst, src)?;
                continue;
            }

            let path_ty_str = |p: &Path| {
                p.metadata()
                    .map(|m| format!("{:?}", m.file_type()))
                    .unwrap_or_else(|_| "non-exists".to_string())
            };
            warn!(
                "Skipped sync {} dst {} to {} src {}",
                path_ty_str(&dst),
                dst.display(),
                path_ty_str(src),
                src.display(),
            );
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Builder, Default)]
pub struct Copier {
    #[cfg(target_family = "unix")]
    mode: Option<u32>,
    #[cfg(target_family = "unix")]
    uid: Option<u32>,
    #[cfg(target_family = "unix")]
    gid: Option<u32>,

    /// 是否跟随软链接。默认为false
    follow_symlinks: bool,
}

impl Copier {
    /// copy all recursively
    pub fn copy<P, Q>(&self, from: P, to: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (from, to) = (from.as_ref(), to.as_ref());
        if !from.exists() {
            bail!("Not found exists path: {}", from.display());
        }

        if from.is_dir() {
            self.copy_dir(from, to)?;
        } else if from.is_file() {
            if !to.exists() {
                self.copy_file(from, to)?;
            } else if to.is_dir() {
                let to =
                    to.join(from.file_name().ok_or_else(|| {
                        anyhow!("Not found file name for path {}", from.display())
                    })?);
                self.copy_file(from, to)?;
            }
        }
        Ok(())
    }

    fn copy_dir<P, Q>(&self, from: P, to: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (from, to) = (from.as_ref(), to.as_ref());
        if !from.is_dir() {
            bail!("Not found from: {}", from.display());
        }
        if to.exists() && (!to.is_dir() || to.read_dir().map(|mut d| d.next().is_some())?) {
            bail!(
                "Cannot copy a dir {} to exists file or non empty dir {}",
                from.display(),
                to.display()
            );
        }

        for entry in WalkDir::new(from) {
            let entry = entry?;
            let src = entry.path();
            let dst = util::map_path(src, from, to)?;
            trace!("Copying path {} to {}", src.display(), dst.display());

            if let Some(p) = dst.parent().filter(|p| !p.exists()) {
                fs::create_dir_all(p)?;
            }
            if src.is_symlink() {
                symlink(src.read_link()?, dst)?;
            } else if src.is_dir() {
                if dst.is_file() || dst.is_symlink() {
                    fs::remove_file(&dst)?;
                }
                if !dst.exists() {
                    fs::create_dir(&dst)?;
                }
                self.copy_metadata(src, &dst)?;
            } else if src.is_file() {
                self.copy_file(src, dst)?;
            }
        }
        Ok(())
    }

    /// 复制文件from到文件to中
    ///
    ///
    /// * 如果from是一个symlink且[Self::follow_symlinks]=true，将读取symlink指向的路径链接到新symlink
    /// * 否则将使用[std::fs::copy]复制内容到新文件
    pub fn copy_file<P, Q>(&self, from: P, to: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (from, to) = (from.as_ref(), to.as_ref());
        if !from.is_file() {
            bail!("Cannot found copy from file {}", from.display());
        }
        // if to.exists()
        if let Some(p) = to.parent().filter(|pp| !pp.exists()) {
            trace!("Creating all dir {}", p.display());
            fs::create_dir_all(p)?;
        }
        if from.is_symlink() && !self.follow_symlinks {
            if to.is_file() || to.is_symlink() {
                fs::remove_file(to)?;
            } else if to.is_dir() {
                fs::remove_dir(to)?;
            }
            let fromp = from.read_link()?;
            trace!("Copying sym link {} -> {}", fromp.display(), to.display());
            symlink(fromp, to)?;
        } else {
            trace!("Copying file {} -> {}", from.display(), to.display());
            fs::copy(from, to)?;
        }

        self.copy_metadata(from, to)?;
        Ok(())
    }

    /// 比较两个文件是否一致
    ///
    /// ## symlink
    ///
    /// 如果文件都为symlink，则比较链接路径是否一致，如果存在一个为symlink则直接返回true。
    /// 如果配置了[Self::follow_symlinks]=true则将读取symlink指向路径的文件比较
    ///
    /// 对于symlink本身的权限信息比较将被忽略，
    /// 参考[How do file permissions apply to symlinks?](https://superuser.com/a/303063)
    ///
    /// 注意：[std::fs::metadata]将跟随symlink，而[std::fs::symlink_metadata]会获取symlink本身
    pub fn has_changed<P, Q>(&self, new: P, old: Q) -> Result<bool>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (new, old) = (new.as_ref(), old.as_ref());

        let (new, old) = if !self.follow_symlinks {
            match (new.is_symlink(), old.is_symlink()) {
                (true, true) => {
                    trace!("Reading symlink path for changed");
                    return Ok(fs::read_link(new)? == fs::read_link(old)?);
                }
                (false, false) => (new.to_path_buf(), old.to_path_buf()),
                _ => {
                    trace!("Found changes with one symlink");
                    return Ok(true);
                }
            }
        } else {
            (
                if new.is_symlink() {
                    fs::read_link(new)?
                } else {
                    new.to_path_buf()
                },
                if old.is_symlink() {
                    fs::read_link(old)?
                } else {
                    old.to_path_buf()
                },
            )
        };

        let (new_meta, old_meta) = (new.metadata()?, old.metadata()?);

        #[cfg(target_family = "unix")]
        {
            if (self.mode.is_some()
                && new_meta.permissions().mode() != old_meta.permissions().mode())
                || (self.uid.is_some() && new_meta.uid() != old_meta.uid())
                || (self.gid.is_some() && new_meta.gid() != old_meta.gid())
            {
                trace!(
                    "Found file metadata changed {:?} != {:?}",
                    new_meta,
                    old_meta
                );
                return Ok(true);
            }
        }

        debug!("Computing the hash of file {}", new.display());
        let mut hasher = Sha256::new();
        io::copy(&mut fs::File::open(&new)?, &mut hasher)?;
        let hash_new = hasher.finalize();

        debug!("Computing the hash of file {}", old.display());
        let mut hasher = Sha256::new();
        io::copy(&mut fs::File::open(&old)?, &mut hasher)?;
        let hash_old = hasher.finalize();

        if log_enabled!(log::Level::Trace) {
            trace!(
                "Comparing hash for file {}: {} and file {}: {}",
                new.display(),
                std::str::from_utf8(&hash_new)?,
                old.display(),
                std::str::from_utf8(&hash_old)?,
            );
        }
        Ok(hash_new == hash_old)
    }

    fn copy_metadata<P, Q>(&self, from: P, to: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (from, to) = (from.as_ref(), to.as_ref());
        let meta = from.metadata()?;

        #[cfg(target_family = "unix")]
        {
            // setup rights
            if let Some(mode) = self.mode {
                let mut perm = meta.permissions();
                debug!(
                    "Setting up permission mode from {} to {} for path {}",
                    perm.mode(),
                    mode,
                    to.display()
                );
                perm.set_mode(mode);
                fs::set_permissions(to, perm)?;
            }
            if self.uid.is_some() || self.gid.is_some() {
                debug!(
                    "Setting up owner to {}:{} for path {}",
                    self.uid.unwrap_or_else(|| meta.uid()),
                    self.gid.unwrap_or_else(|| meta.gid()),
                    to.display()
                );
                chown(to, self.uid.map(Into::into), self.gid.map(Into::into))?;
            }
        }

        if let Err(e) = filetime::set_file_times(
            to,
            FileTime::from_last_access_time(&meta),
            FileTime::from_last_modification_time(&meta),
        ) {
            warn!("Failed to set file {} atime/mtime: {}", to.display(), e);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use once_cell::sync::Lazy;
    use tempfile::{tempdir, TempDir};

    use crate::config::LastDestinationConfigBuilder;

    use super::*;
    use std::{
        fs::File,
        os::unix::fs::symlink,
        thread,
        time::{Duration, SystemTime},
    };

    use fake::{faker::lorem::en::Words, Fake};
    use uuid::Uuid;

    static TMPDIR: Lazy<TempDir> = Lazy::new(|| tempdir().unwrap());

    fn random_tmp_path() -> PathBuf {
        TMPDIR.path().join(Uuid::new_v4().to_string())
    }

    fn create_random_files<P, T>(paths: T) -> Result<Vec<P>>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let mut res = vec![];
        for p in paths {
            let pf = p.as_ref();
            if !pf.is_absolute() {
                bail!("Non absolute path {}", pf.display());
            }
            if pf.extension().is_some() {
                if let Some(pp) = pf.parent() {
                    // trace!("Creating parent dir {}", pf.display());
                    fs::create_dir_all(pp)?;
                }
                let contents = Words(3..10).fake::<Vec<String>>().join(" ");
                // trace!("Writing to {} with contents: {}", pf.display(), contents);
                fs::write(pf, contents)?;
            } else {
                // trace!("Creating a dir {}", pf.display());
                fs::create_dir_all(pf)?;
            }
            res.push(p);
        }
        Ok(res)
    }

    fn create_symlinks<'a, P, T>(sym_paths: T) -> Result<()>
    where
        T: IntoIterator<Item = &'a (P, P)>,
        P: AsRef<Path> + 'a,
    {
        // let mut res = vec![];
        for (from, to) in sym_paths {
            // trace!(
            //     "sym linking {} to {}",
            //     from.as_ref().display(),
            //     to.as_ref().display()
            // );
            if let Some(p) = to.as_ref().parent() {
                fs::create_dir_all(p)?;
            }
            symlink(from, to)?;
            // res.push((from, to))
        }
        // Ok(res)
        Ok(())
    }

    fn build_dir_tree() -> Result<PathBuf> {
        let dir = random_tmp_path();
        let paths = ["a/b/c/1.t", "a/b/2.t", "d/e/3.t"];
        create_random_files(&paths.map(|s| dir.join(s)))?;
        let symlinks = [("a/b/c/1.t", "f/1.t.sym"), ("d/e", "f/e.sym")];
        create_symlinks(&symlinks.map(|(s, t)| (dir.join(s), dir.join(t))))?;

        Ok(dir)
    }

    fn build_tree<P>(p: P) -> Result<P>
    where
        P: AsRef<Path>,
    {
        let base = p.as_ref();
        let mut paths = [
            // "usr",
            // "usr/local",
            // "usr/local/lib",
            // "usr/local/lib/systemd",
            // "usr/local/lib/systemd/system",
            "usr/local/lib/systemd/system/clash.service",
            // "etc",
            "etc/network",
            // "etc/docker",
            "etc/docker/daemon.json",
            // "etc/systemd",
            "etc/systemd/system",
            "etc/wsl.conf",
            "etc/default.t",
            // "etc/netdata",
            "etc/netdata/charts.d.conf",
            // "etc/netdata/go.d",
            "etc/netdata/go.d/wmi.conf",
            "etc/netdata/go.d/postgres.conf",
            "etc/netdata/go.d/prometheus.conf",
            "etc/netdata/go.d.conf",
            "etc/netdata/apps_groups.conf",
            "etc/netdata/netdata.conf",
            // "etc/sudoers.d",
            "etc/sudoers.d/010_nopasswd.t",
            // "etc/cron.d",
            "etc/cron.d/backup-sync.t",
            // "etc/openresty/",
            // "etc/openresty/sites-enabled",
            "etc/openresty/sites-enabled/default.t",
            // symlinks
            // "etc/openresty/sites-enabled/p.navyd.xyz.conf",
            // "etc/openresty/sites-enabled/m.navyd.xyz.conf",
            "etc/openresty/nginx.conf",
            "etc/openresty/sites-available/portainer.navyd.xyz.conf",
            "etc/openresty/sites-available/m.navyd.xyz.conf",
            "etc/openresty/sites-available/p.navyd.xyz.conf",
        ]
        .map(|s| base.join(s));
        // content first
        paths.sort_by_cached_key(|p| -(p.ancestors().count() as isize));
        create_random_files(&paths)?;

        let symlinks = [
            (
                "etc/openresty/sites-available/m.navyd.xyz.conf",
                "etc/openresty/sites-enabled/m.navyd.xyz.conf",
            ),
            (
                "etc/openresty/sites-available/p.navyd.xyz.conf",
                "etc/openresty/sites-enabled/p.navyd.xyz.conf",
            ),
        ]
        .map(|(s, d)| (base.join(s), base.join(d)));
        create_symlinks(&symlinks)?;
        Ok(p)
    }

    fn compare_same_dirs<P, Q>(src: P, dst: Q)
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (src, dst) = (src.as_ref(), dst.as_ref());

        let srcs = WalkDir::new(src)
            .into_iter()
            .map(|e| e.map(|p| p.into_path()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let dsts = WalkDir::new(dst)
            .into_iter()
            .map(|e| e.map(|p| p.into_path()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        if srcs.len() != dsts.len() {
            return;
        }

        assert_eq!(
            srcs.iter().filter(|p| p.is_symlink()).count(),
            dsts.iter().filter(|p| p.is_symlink()).count()
        );
        assert_eq!(
            srcs.iter().filter(|p| p.is_dir()).count(),
            dsts.iter().filter(|p| p.is_dir()).count()
        );
        assert_eq!(
            srcs.iter().filter(|p| p.is_file()).count(),
            dsts.iter().filter(|p| p.is_file()).count()
        );

        let rel_srcs = srcs
            .iter()
            .map(|p| p.strip_prefix(src))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let rel_dsts = dsts
            .iter()
            .map(|p| p.strip_prefix(dst))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(rel_srcs, rel_dsts);

        for i in 0..srcs.len() {
            assert_eq!(
                srcs[i].strip_prefix(src).unwrap(),
                dsts[i].strip_prefix(dst).unwrap()
            );
            compare_same_path(&srcs[i], &dsts[i]);
        }
    }

    fn compare_same_path<P, Q>(src: P, dst: Q)
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (src, dst) = (src.as_ref(), dst.as_ref());
        assert!(src.exists());
        assert!(dst.exists());
        if src.is_symlink() {
            let read_link_end = |p: &Path| {
                let mut p = p.to_path_buf();
                while p.is_symlink() {
                    p = p.read_link().unwrap();
                }
                p
            };
            assert!(dst.is_symlink());
            assert_eq!(read_link_end(src), read_link_end(dst));
        } else {
            let (src_meta, dst_meta) = (src.metadata().unwrap(), dst.metadata().unwrap());
            assert_eq!(src_meta.file_type(), dst_meta.file_type());
            assert_eq!(src_meta.len(), dst_meta.len());
            if src_meta.is_file() {
                assert_eq!(fs::read(src).unwrap(), fs::read(dst).unwrap());
            }
        }
    }

    #[test]
    fn test_metadata_mode() {
        let mut default_cp = Copier::default();
        let mode = 0o777;
        default_cp.mode = Some(mode);
        // create from file
        let from = random_tmp_path();
        fs::write(&from, "test").unwrap();

        // test created time for from
        let created_time_about = SystemTime::now();
        thread::sleep(Duration::from_millis(300));
        assert!(from.metadata().unwrap().created().unwrap() <= created_time_about);

        // copy contents only
        let to = random_tmp_path();
        io::copy(
            &mut File::open(&from).unwrap(),
            &mut File::create(&to).unwrap(),
        )
        .unwrap();

        // set from perms mode
        let from_meta = from.metadata().unwrap();
        let mut from_perm = from_meta.permissions();
        from_perm.set_mode(mode);
        fs::set_permissions(&from, from_perm).unwrap();

        let from_meta = from.metadata().unwrap();
        let to_meta = to.metadata().unwrap();
        assert!(to_meta.is_file());
        // test created time
        assert!(to_meta.created().unwrap() > created_time_about);
        // test not eq perms mode
        assert_ne!(
            from.metadata().unwrap().permissions().mode(),
            to_meta.permissions().mode()
        );

        default_cp.copy_metadata(from, &to).unwrap();

        let to_meta = to.metadata().unwrap();
        // test eq mode after copy metadata
        assert_eq!(from_meta.mode(), to_meta.mode());
    }

    #[test]
    fn test_copy_file_times() {
        let cp = Copier::default();
        let from = random_tmp_path().join("a.t");
        create_random_files(&[&from]).unwrap();
        assert!(from.is_file());
        thread::sleep(Duration::from_millis(300));

        let to = random_tmp_path();
        cp.copy_file(&from, &to).unwrap();

        let (from_meta, to_meta) = (from.metadata().unwrap(), to.metadata().unwrap());
        assert_eq!(from_meta.atime(), to_meta.atime());
        assert_eq!(from_meta.mtime(), to_meta.mtime());
    }

    #[test]
    fn test_copy_dir() {
        let from = build_dir_tree().unwrap();
        let to = random_tmp_path();

        Copier::default().copy_dir(&from, &to).unwrap();

        assert!(from.is_dir());
        assert!(to.is_dir());
        let from_paths = WalkDir::new(&from)
            .into_iter()
            .map(|e| e.map(|p| p.into_path()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let to_paths = WalkDir::new(&to)
            .into_iter()
            .map(|e| e.map(|p| p.into_path()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(from_paths.len(), to_paths.len());

        assert_eq!(
            from_paths
                .iter()
                .map(|p| p.strip_prefix(&from))
                .collect::<Result<Vec<_>, _>>()
                .unwrap(),
            to_paths
                .iter()
                .map(|p| p.strip_prefix(&to))
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
        );
    }

    #[test]
    fn test_copier_has_changed_on_symlink() {
        // setup
        let src = random_tmp_path();
        let plink = random_tmp_path().with_extension("sym");
        let s = Words(3..10).fake::<Vec<String>>().join(" ");
        fs::write(&src, &s).unwrap();
        assert!(src.is_file());
        assert_eq!(fs::read_to_string(&src).unwrap(), s);

        assert!(!plink.exists());
        symlink(&src, &plink).unwrap();
        assert!(plink.is_symlink());

        let copier = Copier::default();
        assert!(plink.is_symlink());
        assert!(copier.has_changed(plink, src).unwrap());
    }

    fn syncer() -> Syncer {
        SyncerBuilder::default()
            .copier(Copier {
                ..Copier::default()
            })
            .last_conf(
                LastDestinationConfigBuilder::default()
                    .path(random_tmp_path())
                    .build()
                    .unwrap(),
            )
            .dst(random_tmp_path().join("dst"))
            .src(random_tmp_path().join("src"))
            .non_interactive(true)
            .build()
            .unwrap()
    }

    #[test]
    fn test_sync_back_with_src_when_empty_src() {
        let sync = syncer();

        let dst_dir = build_tree(&sync.dst).unwrap();
        assert!(dst_dir.is_dir());

        assert!(!sync.src.exists());
        sync.sync_back_with_src([] as [&str; 0]).unwrap();
        assert!(!sync.src.exists());

        fs::create_dir_all(&sync.src).unwrap();
        sync.sync_back_with_src([] as [&str; 0]).unwrap();
        assert!(sync.src.is_dir());
        assert!(sync.src.read_dir().unwrap().next().is_none());
    }

    #[test]
    fn test_sync_back_with_src_when_sames() {
        let sync = syncer();

        let dst_dir = build_tree(&sync.dst).unwrap();
        assert!(dst_dir.is_dir());
        let src_dir = build_tree(&sync.src).unwrap();
        assert!(src_dir.is_dir());

        sync.sync_back_with_src([] as [&str; 0]).unwrap();

        compare_same_dirs(src_dir, dst_dir);
    }

    #[test]
    fn test_sync_back_with_src_when_lost_some_in_srcs() {
        let sync = syncer();

        let dst_dir = build_tree(&sync.dst).unwrap();
        assert!(dst_dir.is_dir());
        let src_dir = build_tree(&sync.src).unwrap();
        assert!(src_dir.is_dir());

        let mut srcs = WalkDir::new(src_dir)
            .into_iter()
            .map(|e| e.map(|p| p.into_path()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let rm_src_files = {
            let mut files = vec![];
            let mut i = 0;
            while i < srcs.len() && files.len() < 2 {
                if srcs[i].is_file() {
                    fs::remove_file(&srcs[i]).unwrap();
                    files.push(srcs.remove(i))
                } else {
                    i += 1;
                }
            }
            files
        };
        let mod_src_files = srcs
            .iter()
            .filter(|p| p.is_file())
            .take(2)
            .map(|p| fs::write(p, Words(3..10).fake::<Vec<String>>().join(" ")).map(|_| p))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        let mod_src_symlinks = srcs
            .iter()
            .filter(|p| p.is_symlink())
            .take(1)
            .map(|p| {
                assert!(p.is_symlink());
                let plink = p.read_link().unwrap();
                let newplink = srcs.iter().find(|s| s.is_file() && **s != plink).unwrap();
                fs::remove_file(p).unwrap();
                debug!(
                    "Re symlink {} from {} to {}",
                    p.display(),
                    plink.display(),
                    newplink.display()
                );
                symlink(newplink, p).unwrap();
                assert!(p.is_symlink());
                p
            })
            .collect_vec();

        sync.sync_back_with_src([] as [&str; 0]).unwrap();

        // 修改的src文件 内容同步dst
        for src in mod_src_files {
            let dst = dst_dir.join(src.strip_prefix(src_dir).unwrap());
            compare_same_path(src, dst);
        }
        // 移除的src文件不会被dst影响
        for src in rm_src_files {
            let dst = dst_dir.join(src.strip_prefix(src_dir).unwrap());
            assert!(!src.exists());
            assert!(dst.is_file());
        }
        // 修改的src symlink 链接path同步
        for src in mod_src_symlinks {
            let dst = dst_dir.join(src.strip_prefix(src_dir).unwrap());
            assert_eq!(src.read_link().unwrap(), dst.read_link().unwrap());
        }
    }

    #[test]
    fn test_sync_back_when_last_conf() {
        let sync = syncer();
        let dst_dir = build_tree(&sync.dst).unwrap();
        let dsts = WalkDir::new(dst_dir)
            .into_iter()
            .map(|e| e.map(|p| p.into_path()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        sync.last_conf.save_last_dsts(&dsts).unwrap();
        sync.sync_back(&[] as &[&str; 0]).unwrap();

        compare_same_dirs(sync.src, dst_dir);
    }

    #[test]
    fn test_sync_back_when_last_conf_in_targets() {
        let sync = syncer();
        let dst_dir = build_tree(&sync.dst).unwrap();
        let dsts = WalkDir::new(dst_dir)
            .min_depth(1)
            .into_iter()
            .map(|e| e.map(|p| p.into_path()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let target_dsts = dsts
            .iter()
            .filter(|p| p.is_dir() && p.read_dir().unwrap().any(|d| d.unwrap().path().is_file()))
            .take(1)
            .collect_vec();

        sync.last_conf.save_last_dsts(&dsts).unwrap();
        sync.sync_back(&target_dsts).unwrap();

        dsts.iter()
            // filter not in target dsts
            .filter(|d| {
                target_dsts
                    .iter()
                    .all(|t| !d.starts_with(t) && !t.starts_with(d))
            })
            .map(|dst| sync.src.join(dst.strip_prefix(dst_dir).unwrap()))
            .for_each(|src| assert!(!dbg!(src).exists()));
        target_dsts
            .iter()
            .map(|dst| (sync.src.join(dst.strip_prefix(dst_dir).unwrap()), dst))
            .for_each(|(src, dst)| compare_same_dirs(src, dst));
    }
}
