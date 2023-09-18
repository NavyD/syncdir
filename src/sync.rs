use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    fs, io,
    os::unix::fs::symlink,
    path::{Path, PathBuf},
};
#[cfg(target_family = "unix")]
use {
    nix::unistd::chown,
    std::os::unix::prelude::{MetadataExt, PermissionsExt},
};

use anyhow::{anyhow, bail, Context, Error, Result};
use derive_builder::Builder;
use dialoguer::{theme::ColorfulTheme, Confirm};
use filetime::FileTime;
use getset::CopyGetters;
use itertools::Itertools;
use log::{debug, info, log_enabled, trace, warn};
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

use crate::{service::LastDestinationListService, util};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SyncPath {
    Removed(PathBuf),
    Coppied(PathBuf, PathBuf),
    Overriden(PathBuf, PathBuf),
}

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

/// 将src目录内容同步到dst目录中
#[derive(Debug, Clone, Builder)]
#[builder(setter(into))]
pub struct Syncer {
    src: PathBuf,
    dst: PathBuf,

    #[builder(default = "true")]
    non_interactive: bool,

    /// 同步复制实现
    #[builder(default)]
    copier: Copier,

    #[builder(default = "false")]
    dry_run: bool,

    last_dsts_srv: LastDestinationListService,
}

impl Syncer {
    /// 将dst中的文件同步到src中。返回复制的(src,dst)列表
    ///
    /// * 如果存在last_dsts则将所有的last_dsts文件整个复制到src目录中，src目录将被清空
    /// * 如果不存在last_dsts将从当前src中加载目录映射到dst中
    ///     * 如果dst的文件不存在于src中则移除src文件
    ///     * 否则比较是否改变
    ///         * 如果src与dst文件不同则覆盖src文件
    ///         * 否则跳过
    ///
    /// 如果target_dsts是相对路径则是相对[Self::dst]
    pub fn sync_back<T, P>(&self, target_dsts: T) -> Result<Vec<SyncPath>>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let target_dsts = target_dsts
            .into_iter()
            .map(|p| {
                let p = p.as_ref();
                if p.is_absolute() {
                    p.to_path_buf()
                } else {
                    self.dst.join(p)
                }
            })
            .collect_vec();
        if let Some(p) = target_dsts.iter().find(|p| !p.starts_with(&self.dst)) {
            bail!(
                "Found a target dst {} not start with dst {}",
                p.display(),
                self.dst.display()
            );
        }

        let last_dsts = self
            .last_dsts_srv
            .load_last_dsts_in_targets(&target_dsts)?
            .unwrap_or_default();
        info!(
            "Found {} last dsts in {} target dsts",
            last_dsts.len(),
            target_dsts.len(),
        );

        if last_dsts.is_empty() {
            self.sync_back_with_src(&target_dsts)
        } else if !self.src.exists() || self.dry_run || self.confirm_rm(&self.src)? {
            if target_dsts.is_empty() {
                self.copier.copy(&self.dst, &self.src)
            } else {
                let mut res = vec![];
                for dst in target_dsts {
                    let src = self.get_src_path(&dst)?;
                    res.extend(self.copier.copy(dst, src)?);
                }
                Ok(res)
            }
        } else {
            warn!(
                "Skipped sync dst {} back to src {}",
                self.dst.display(),
                self.src.display()
            );
            Ok(vec![])
        }
    }

    /// 将src中的文件同步到指定的target_dsts中。扫描src目录，并找到对应的dst目录dsts，
    /// 将src存在的文件复制到dsts中
    ///
    /// 类似[Self::sync_back_with_src()]
    ///
    /// ## Errors
    ///
    /// 在应用过程中由于权限metadata等问题不会创建dst目录
    /// 如果dst目录不存在时
    pub fn apply_to_dst<T, P>(&self, target_dsts: T) -> Result<Vec<SyncPath>>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let target_dsts = target_dsts.into_iter().collect_vec();
        WalkDir::new(&self.src)
            .into_iter()
            .map(|entry| {
                let entry = entry?;
                let src = entry.into_path();
                let dst = self.get_dst_path(&src)?;
                // println!("src {}, dst {}", src.display(), dst.display());

                // skip non target dsts
                if !target_dsts.is_empty() && target_dsts.iter().all(|t| !dst.starts_with(t)) {
                    trace!(
                        "Skipped sync dst {} not in {} target dsts",
                        dst.display(),
                        target_dsts.len()
                    );
                    return Ok(None);
                }

                if self.dry_run {
                    return Ok(Some(sync_path!(src, dst)));
                }

                if dst.is_symlink() || dst.is_file() {
                    return Ok::<_, Error>(if self.confirm_rm(&dst)? {
                        self.copier.copy_file(&src, &dst)?;
                        Some(SyncPath::Overriden(src, dst))
                    } else {
                        None
                    });
                }

                if !dst.exists() {
                    if src.is_dir() {
                        fs::create_dir_all(&dst)?;
                        self.copier.copy_metadata(&src, &dst)?;
                    } else {
                        self.copier.copy_file(&src, &dst)?;
                    }
                    return Ok(Some(SyncPath::Coppied(src, dst)));
                }

                Ok(if src.is_dir() {
                    self.copier.copy_metadata(&src, &dst)?;
                    Some(SyncPath::Overriden(src, dst))
                } else if self.confirm_rm(&dst)? {
                    fs::create_dir(&dst)?;
                    self.copier.copy_metadata(&src, &dst)?;
                    Some(SyncPath::Overriden(src, dst))
                } else {
                    None
                })
            })
            .filter(|res| res.as_ref().map(|opt| opt.is_some()).unwrap_or(true))
            // safety: filter some
            .map(|res| res.map(|opt| opt.unwrap()))
            .collect::<Result<Vec<_>>>()
    }

    /// 根据[LastDestinationListService]的配置与curr src在指定的target dsts中清理
    /// dst目录中的文件，返回被移除的文件
    ///
    /// 从[LastDestinationListService]中加载上次的dsts并映射回last srcs与
    /// 当前curr srcs比较找出当前被删除的dsts路径（不在curr srcs中但存在于last srcs）并移除
    ///
    /// 最后更新last srcs未删除的保存到[LastDestinationListService]
    pub fn clean_dst<T, P>(&self, target_dsts: T) -> Result<Vec<PathBuf>>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let target_dsts = target_dsts.into_iter().collect_vec();
        if log_enabled!(log::Level::Info) {
            info!(
                "Cleaning dst {} in {} targets: [{}]",
                self.dst.display(),
                target_dsts.len(),
                target_dsts.iter().map(|p| p.as_ref().display()).join(", ")
            );
        }

        // 0. read last dsts
        let last_dsts = self
            .last_dsts_srv
            .load_last_dsts_in_targets(&target_dsts)?
            // 即使dsts为空也会执行 保存
            .unwrap_or_default();
        trace!("Loaded {} last dsts: {:?}", last_dsts.len(), last_dsts);

        // 1. load cur srcs
        let curr_srcs = self.load_curr_srcs()?;
        trace!("Loaded {} curr srcs: {:?}", curr_srcs.len(), curr_srcs);

        // 2. find removable srcs and dsts
        let removables = self.find_removable_src_dsts_iter(&curr_srcs, &last_dsts)?;

        if self.dry_run {
            return Ok(removables.map(|(_, dst)| dst).collect_vec());
        }

        // 3. remove dirs. if error then save lasts and reerror
        let get_new_last_dsts = |removeds: &[PathBuf]| {
            debug!("Getting new last dsts from {} curr srcs", curr_srcs.len());
            let mut dsts = curr_srcs
                .iter()
                .map(|p| self.get_dst_path(p))
                .collect::<Result<HashSet<_>>>()?;
            debug!(
                "Extending {} last dsts by filter {} removeds",
                last_dsts.len(),
                removeds.len()
            );
            dsts.extend(
                last_dsts
                    .iter()
                    .filter(|p| !removeds.contains(p))
                    .map(|p| p.to_path_buf()),
            );
            trace!("Got {} new last dsts: {:?}", dsts.len(), dsts);
            Ok::<_, Error>(dsts)
        };
        let rm_dsts = match self.remove_src_dsts(removables) {
            Ok(removeds) => {
                let (_rm_srcs, rm_dsts) = removeds.into_iter().map(|v| (v.0, v.1)).unzip()
                    as (Vec<PathBuf>, Vec<PathBuf>);
                rm_dsts
            }
            Err(mut e) => {
                let errstr = e.to_string();
                if let Some(removed_err) = e.downcast_mut::<DotRootRemovedError>() {
                    warn!(
                        "Trying to save removed {} paths for remove file error: {}",
                        removed_err.src_dsts.len(),
                        errstr
                    );
                    let dsts = removed_err
                        .src_dsts
                        .drain(..)
                        .map(|(_src, dst)| dst)
                        .collect_vec();
                    let new_last_dsts = get_new_last_dsts(&dsts)?;
                    self.last_dsts_srv.save_last_dsts(&new_last_dsts)?;
                }
                return Err(e);
            }
        };
        let new_last_dsts = get_new_last_dsts(&rm_dsts)?;

        // 4. update dirs to file
        self.last_dsts_srv.save_last_dsts(&new_last_dsts)?;
        trace!(
            "Saved {} last dsts: {:?}",
            new_last_dsts.len(),
            new_last_dsts
        );

        Ok(rm_dsts)
    }

    /// 从src路径转换到对应的dst路径：
    /// src=/a/b,dst=/c/d/e: src_sub=a/b/1/2.t => dst_path=/c/d/e/1/2.t
    fn get_dst_path(&self, src_sub: impl AsRef<Path>) -> Result<PathBuf> {
        util::map_path(src_sub.as_ref(), &self.src, &self.dst)
    }

    /// 将dst目录转换为src路径
    fn get_src_path(&self, dst_sub: impl AsRef<Path>) -> Result<PathBuf> {
        util::map_path(dst_sub.as_ref(), &self.dst, &self.src)
    }

    fn confirm_rm(&self, p: impl AsRef<Path>) -> Result<bool> {
        let p = p.as_ref();
        if !p.exists() {
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
            warn!(
                "Skipped remove {} {}",
                p.metadata()
                    .map(|m| format!("{:?}", m.file_type()))
                    .unwrap_or_else(|_| "non-exists".to_string()),
                p.display()
            );
            Ok(false)
        }
    }

    /// 将src映射的dst中的指定的target_dsts中的dst目录同步到src。
    ///
    /// 同步将扫描src目录，并找到对应的dst目录dsts，根据dsts中的类型修改回
    /// src中的文件内容
    ///
    /// 这个功能可能不会工作的很好，仅[load_last_dsts()](LastDestinationListService)
    /// 为空时使用，提供基本的同步
    fn sync_back_with_src<T, P>(&self, target_dsts: T) -> Result<Vec<SyncPath>>
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
            return Ok(vec![]);
        }

        WalkDir::new(&self.src)
            .into_iter()
            .map(|entry| {
                let entry = entry?;
                let src = entry.into_path();
                let dst = self.get_dst_path(&src)?;

                // skip non target dsts
                if !target_dsts.is_empty() && target_dsts.iter().all(|t| !dst.starts_with(t)) {
                    trace!(
                        "Skipped sync dst {} not in {} target dsts",
                        dst.display(),
                        target_dsts.len()
                    );
                    return Ok::<_, Error>(None);
                }

                // 在walkdir后不应该找不到path
                if !src.exists() {
                    warn!(
                        "Skipped not exists path {} after walk dir {}",
                        src.display(),
                        self.src.display()
                    );
                    return Ok(None);
                }

                if self.dry_run {
                    return Ok(Some(if !dst.exists() {
                        SyncPath::Removed(src)
                    } else {
                        SyncPath::Overriden(dst, src)
                    }));
                }

                // dst为文件/软链接时 移除src目录
                if dst.is_symlink() || dst.is_file() {
                    return Ok(if self.confirm_rm(&src)? {
                        self.copier.copy_file(&dst, &src)?;
                        Some(SyncPath::Overriden(dst, src))
                    } else {
                        None
                    });
                }

                // dst不存在 移除src
                if !dst.exists() {
                    return Ok(if self.confirm_rm(&src)? {
                        Some(sync_path!(src))
                    } else {
                        None
                    });
                }

                // dst目录时 当src是 不存在或目录则mkdir -p, 文件则移除
                Ok(if src.is_dir() {
                    self.copier.copy_metadata(&dst, &src)?;
                    Some(SyncPath::Overriden(dst, src))
                } else if self.confirm_rm(&src)? {
                    fs::create_dir(&src)?;
                    self.copier.copy_metadata(&dst, &src)?;
                    Some(SyncPath::Overriden(dst, src))
                } else {
                    None
                })
            })
            .filter(|res| res.as_ref().map(|opt| opt.is_some()).unwrap_or(true))
            // safety: filter some
            .map(|res| res.map(|opt| opt.unwrap()))
            .collect::<Result<Vec<_>>>()
    }

    /// 遍历src目录找到所有文件
    fn load_curr_srcs(&self) -> Result<Vec<PathBuf>> {
        WalkDir::new(&self.src)
            // 移除当前目录
            .min_depth(1)
            .into_iter()
            .map(|res| res.map(|e| e.into_path()).map_err(Into::into))
            .collect::<Result<Vec<_>>>()
    }

    /// 比较当前所有的.root中的path与上次运行前保存的last_paths
    /// 找出不再需要的文件删除.root并返回删除的文件
    ///
    /// 如果在删除的过程中出错，会返回[DotRootRemovedError]
    fn remove_src_dsts<T, P>(&self, src_dsts: T) -> Result<Vec<(P, P)>>
    where
        T: IntoIterator<Item = (P, P)>,
        P: AsRef<Path>,
    {
        let (mut removeds, mut skippeds) = (vec![], vec![]);
        let src_dsts = src_dsts.into_iter().collect_vec();
        debug!(
            "Trying to remove {} src dsts: [{:?}]",
            src_dsts.len(),
            src_dsts
                .iter()
                .map(|(s, d)| format!("({},{})", s.as_ref().display(), d.as_ref().display()))
                .join(",")
        );

        for (src, dst) in src_dsts {
            let (srcf, dstf) = (src.as_ref(), dst.as_ref());
            let removed = confirm_rm(srcf, dstf, self.non_interactive).with_context(|| {
                DotRootRemovedError {
                    desc: format!(
                        "Failed to remove src {} and dst {}",
                        srcf.display(),
                        dstf.display()
                    ),
                    src_dsts: removeds
                        .iter()
                        .map(|(s, d): &(P, P)| (s.as_ref().to_path_buf(), d.as_ref().to_path_buf()))
                        .collect_vec(),
                }
            })?;
            // if rm(src.as_ref(), dst.as_ref())? {
            if removed {
                &mut removeds
            } else {
                &mut skippeds
            }
            .push((src, dst));
        }

        skippeds.sort_by_cached_key(|(src, _)| -(src.as_ref().ancestors().count() as isize));
        info!("Cleaning empty dirs in {} skippeds", skippeds.len());
        // skippeds.into_iter().map(|(src, dst)| rm(src, dst)).collect::<Result<Vec<_>>>();
        let mut new_skippeds = vec![];
        for (src, dst) in skippeds {
            let (srcf, dstf) = (src.as_ref(), dst.as_ref());
            let removed = confirm_rm(srcf, dstf, self.non_interactive).with_context(|| {
                DotRootRemovedError {
                    desc: format!(
                        "Failed to remove src {} and dst {}",
                        srcf.display(),
                        dstf.display()
                    ),
                    src_dsts: removeds
                        .iter()
                        .map(|(s, d): &(P, P)| (s.as_ref().to_path_buf(), d.as_ref().to_path_buf()))
                        .collect_vec(),
                }
            })?;
            if removed {
                &mut removeds
            } else {
                &mut new_skippeds
            }
            .push((src, dst));
        }

        if log_enabled!(log::Level::Trace) {
            trace!(
                "Removed a total of {} files: {:?}. and skipped {} files: {:?}",
                removeds.len(),
                removeds
                    .iter()
                    .map(|(s, d)| format!("({},{})", s.as_ref().display(), d.as_ref().display()))
                    .join(","),
                new_skippeds.len(),
                new_skippeds
                    .iter()
                    .map(|(s, d)| format!("({},{})", s.as_ref().display(), d.as_ref().display()))
                    .join(","),
            );
        }
        Ok(removeds)
    }

    /// 比较当前src与上次的dst路径找出不再存在当前src中的last的路径，
    /// 返回需要删除的last_dsts对应的src路径
    fn find_removable_src_dsts_iter<'a, T, P>(
        &self,
        curr_srcs: T,
        last_dsts: T,
    ) -> Result<impl Iterator<Item = (PathBuf, P)> + Debug + Clone + 'a>
    where
        T: IntoIterator<Item = &'a P> + Clone,
        P: AsRef<Path> + Clone + Debug + 'a,
    {
        let curr_srcs: Vec<&P> = curr_srcs.into_iter().collect_vec();
        let last_dsts: Vec<&P> = last_dsts.into_iter().collect_vec();

        if let Some(p) = curr_srcs
            .iter()
            .chain(&last_dsts)
            .find(|p| !p.as_ref().is_absolute())
        {
            bail!("Found non absolute path {}", p.as_ref().display())
        }

        let last_srcs = last_dsts
            .into_iter()
            // last dst to last src
            .map(|dst| {
                self.get_src_path(dst.as_ref())
                    .map(|src| (src, dst.clone()))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let curr_srcs = curr_srcs
            .into_iter()
            .map(AsRef::as_ref)
            .collect::<HashSet<_>>();
        if log_enabled!(log::Level::Trace) {
            trace!(
                "Finding removable {} last srcs: [{}]. if not contains in {} curr srcs: {:?}",
                last_srcs.len(),
                last_srcs.iter().map(|v| v.0.display()).join(","),
                curr_srcs.len(),
                curr_srcs
            );
        }
        Ok(last_srcs
            .into_iter()
            .filter(move |(src, _)| !curr_srcs.contains(src.as_path())))
    }
}

#[derive(Debug, Clone, Builder, Default, CopyGetters)]
#[builder(setter(into, strip_option), default)]
#[getset(get = "pub", get_copy = "pub")]
pub struct Copier {
    #[cfg(target_family = "unix")]
    mode: Option<u32>,
    #[cfg(target_family = "unix")]
    uid: Option<u32>,
    #[cfg(target_family = "unix")]
    gid: Option<u32>,

    /// 是否跟随软链接。默认为false
    /// TODO: true时目前无法正常工作
    follow_symlinks: bool,

    /// 是否保留目录文件amtimes
    filetimes: bool,

    dry_run: bool,
}

impl Copier {
    /// copy all recursively
    pub fn copy<P, Q>(&self, from: P, to: Q) -> Result<Vec<SyncPath>>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (from, to) = (from.as_ref(), to.as_ref());
        if !from.exists() {
            bail!("Not found exists path: {}", from.display());
        }

        if from.is_dir() {
            return self.copy_dir(from, to);
        }
        // from is file
        let to = if to.is_dir() {
            to.join(
                from.file_name()
                    .ok_or_else(|| anyhow!("Not found file name for path {}", from.display()))?,
            )
        } else {
            to.to_path_buf()
        };
        if !self.dry_run {
            self.copy_file(from, &to)?;
        }
        Ok(vec![sync_path!(from.to_path_buf(), to)])
    }

    fn copy_dir<P, Q>(&self, from: P, to: Q) -> Result<Vec<SyncPath>>
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

        let paths = WalkDir::new(from)
            .into_iter()
            .map(|entry| {
                let entry = entry?;
                let src = entry.into_path();
                let dst = util::map_path(&src, from, to)?;

                if self.dry_run {
                    return Ok(sync_path!(src, dst));
                }

                let overriden = dst.is_symlink() || dst.exists();
                if log_enabled!(log::Level::Trace) {
                    let src_ty_str = if src.is_symlink() {
                        "link"
                    } else if src.is_dir() {
                        "dir "
                    } else {
                        "file"
                    };
                    trace!(
                        "Copying {} {} to{} {}",
                        src_ty_str,
                        src.display(),
                        if overriden { " overriden" } else { "" },
                        dst.display()
                    );
                }

                if let Some(p) = dst.parent().filter(|p| !p.exists()) {
                    fs::create_dir_all(p)?;
                }
                if src.is_symlink() {
                    symlink(src.read_link()?, &dst)?;
                } else if src.is_dir() {
                    if dst.is_symlink() || dst.is_file() {
                        fs::remove_file(&dst)?;
                    } else if !dst.exists() {
                        fs::create_dir(&dst)?;
                    }
                    self.copy_metadata(&src, &dst)?;
                } else if src.is_file() {
                    self.copy_file(&src, &dst)?;
                }

                Ok::<_, Error>(if overriden {
                    SyncPath::Overriden(src, dst)
                } else {
                    SyncPath::Coppied(src, dst)
                })
            })
            .collect::<Result<Vec<_>>>()?;

        if self.filetimes {
            // 注意：由于copy目录dst在添加文件后dst目录mtime被修改，重新修改时间
            WalkDir::new(to)
                .contents_first(true)
                .into_iter()
                .try_for_each(|entry| {
                    let dst = entry?.into_path();
                    let src = util::map_path(&dst, to, from)?;
                    self.copy_filetimes(src, dst)?;
                    Ok::<_, Error>(())
                })?;
        }
        Ok(paths)
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
        Ok(())
    }

    fn copy_filetimes<P, Q>(&self, from: P, to: Q) -> Result<()>
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (from, to) = (from.as_ref(), to.as_ref());
        if let Ok(meta) = from.symlink_metadata() {
            if let Err(e) = filetime::set_symlink_file_times(
                to,
                FileTime::from_last_access_time(&meta),
                FileTime::from_last_modification_time(&meta),
            ) {
                warn!("Failed to set file {} atime/mtime: {}", to.display(), e);
            }
        } else {
            let meta = from.metadata()?;

            if let Err(e) = filetime::set_file_times(
                to,
                FileTime::from_last_access_time(&meta),
                FileTime::from_last_modification_time(&meta),
            ) {
                warn!("Failed to set file {} atime/mtime: {}", to.display(), e);
            }
        }
        Ok(())
    }
}

/// 表示移除文件错误包含已移除了的paths
#[derive(Debug)]
struct DotRootRemovedError {
    src_dsts: Vec<(PathBuf, PathBuf)>,
    desc: String,
}

impl Display for DotRootRemovedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self
            .src_dsts
            .iter()
            .map(|(src, dst)| format!("({}, {})", src.display(), dst.display()))
            .join(", ");
        write!(f, "{}, removed paths: [{}]", self.desc, s)
    }
}

/// 确认是否移除src与对应dst文件，移除成功则返回None，跳过则返回dst路径
///
/// 如果移除失败则返回
fn confirm_rm<P: AsRef<Path>>(src: P, dst: P, non_interactive: bool) -> Result<bool> {
    let (src, dst) = (src.as_ref(), dst.as_ref());
    if non_interactive
        || Confirm::with_theme(&ColorfulTheme::default())
            .default(true)
            .show_default(true)
            .with_prompt(format!(
                "Do you want to remove src path {} and dst path {}",
                src.display(),
                dst.display()
            ))
            .interact()?
    {
        let rm = |p: &Path| {
            if let Ok(d) = p.metadata() {
                if d.is_dir() {
                    if p.read_dir().map(|mut d| d.next().is_some())? {
                        info!("Ignoring remove non empty dir: {}", p.display());
                        return Ok(false);
                    }
                    fs::remove_dir(p)?;
                } else if d.is_file() || d.is_symlink() {
                    fs::remove_file(p)?;
                } else {
                    bail!("Unkown path {} metadata: {:?}", p.display(), d);
                }
            } else {
                trace!("Removing non exists path: {}", p.display());
            }
            Ok::<_, Error>(true)
        };
        info!("Removing file src={}, dst={}", src.display(), dst.display());
        // ignore src result
        rm(src)?;
        // respect only dst result
        rm(dst)
    } else {
        info!(
            "Skipped remove file src={}, dst={}",
            src.display(),
            dst.display()
        );
        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use once_cell::sync::Lazy;
    use pretty_assertions::assert_eq;
    use tempfile::{tempdir, TempDir};

    use crate::service::LastDestinationListServiceBuilder;

    use super::*;
    use std::{
        fs::File,
        os::unix::fs::symlink,
        thread,
        time::{Duration, SystemTime},
    };

    use fake::{faker::lorem::en::Words, Fake};
    use uuid::Uuid;

    fn random_tmp_path() -> PathBuf {
        static TMPDIR: Lazy<TempDir> = Lazy::new(|| tempdir().unwrap());
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
    fn test_copy_dir_when_dry_run() {
        let from = build_dir_tree().unwrap();
        let to = random_tmp_path();
        let expect_from_paths = WalkDir::new(&from)
            .into_iter()
            .map(|entry| {
                entry.map_err(Into::into).and_then(|e| {
                    let src = e.into_path();
                    let dst = util::map_path(&src, &from, &to)?;
                    Ok::<_, Error>(sync_path!(src, dst))
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        let cp = CopierBuilder::default().dry_run(true).build().unwrap();
        let paths = cp.copy_dir(&from, &to).unwrap();

        assert!(from.is_dir());
        assert!(!to.is_dir());
        assert!(!paths.is_empty());
        assert_eq!(expect_from_paths, paths);

        fs::create_dir_all(&to).unwrap();
        let expect_from_paths = WalkDir::new(&from)
            .into_iter()
            .map(|entry| {
                entry.map_err(Into::into).and_then(|e| {
                    let src = e.into_path();
                    let dst = util::map_path(&src, &from, &to)?;
                    Ok::<_, Error>(sync_path!(src, dst))
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let paths = cp.copy_dir(&from, &to).unwrap();
        assert!(to.is_dir());
        assert!(to.read_dir().unwrap().next().is_none());
        assert!(!paths.is_empty());
        assert_eq!(expect_from_paths, paths);
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
            .last_dsts_srv(
                LastDestinationListServiceBuilder::default()
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
        let paths = sync.sync_back_with_src([] as [&str; 0]).unwrap();
        assert!(!sync.src.exists());
        assert!(paths.is_empty());

        fs::create_dir_all(&sync.src).unwrap();
        let paths = sync.sync_back_with_src([] as [&str; 0]).unwrap();
        assert!(sync.src.is_dir());
        assert!(sync.src.read_dir().unwrap().next().is_none());
        assert_eq!(paths.len(), 1);
        assert_eq!(&paths[0], &SyncPath::Overriden(sync.dst, sync.src))
    }

    #[test]
    fn test_sync_back_with_src_when_sames() {
        let sync = syncer();

        let dst_dir = build_tree(&sync.dst).unwrap();
        assert!(dst_dir.is_dir());
        let src_dir = build_tree(&sync.src).unwrap();
        assert!(src_dir.is_dir());

        let paths = sync.sync_back_with_src([] as [&str; 0]).unwrap();
        assert!(!paths.is_empty());
        paths.iter().for_each(|sp| match sp {
            SyncPath::Overriden(_, _) => {}
            _ => panic!("non overriden sync path: {:?}", sp),
        });
        compare_same_dirs(src_dir, dst_dir);
    }

    #[test]
    fn test_sync_back_with_src_when_dry_run() {
        let mut sync = syncer();
        sync.dry_run = true;
        let src_dir = build_tree(&sync.src).unwrap();
        let src_meta = fs::metadata(src_dir).unwrap();
        let mut perm = src_meta.permissions();
        perm.set_readonly(true);
        fs::set_permissions(src_dir, perm).unwrap();
        // test perms
        let e = fs::create_dir_all(src_dir.join(Uuid::new_v4().to_string()));
        assert!(e.is_err());
        assert_eq!(e.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        let expect_paths = WalkDir::new(src_dir)
            .into_iter()
            .map(|entry| {
                entry.map_err(Into::into).and_then(|e| {
                    let src = e.into_path();
                    let dst = util::map_path(&src, src_dir, &sync.dst)?;
                    Ok::<_, Error>(if dst.exists() {
                        sync_path!(dst, src)
                    } else {
                        sync_path!(src)
                    })
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let paths = sync.sync_back_with_src([] as [&str; 0]).unwrap();
        assert_eq!(paths, expect_paths);
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

        let expect_paths = WalkDir::new(src_dir)
            .into_iter()
            .map(|entry| {
                entry.map_err(Into::into).and_then(|e| {
                    let src = e.into_path();
                    let dst = util::map_path(&src, src_dir, dst_dir)?;
                    Ok::<_, Error>(sync_path!(dst, src))
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        let paths = sync.sync_back_with_src([] as [&str; 0]).unwrap();
        assert_eq!(expect_paths, paths);

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

        sync.last_dsts_srv.save_last_dsts(&dsts).unwrap();
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

        sync.last_dsts_srv.save_last_dsts(&dsts).unwrap();
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

    #[test]
    fn test_sync_back_when_last_conf_not_in_targets() {
        let sync = syncer();
        let dst_dir = build_tree(&sync.dst).unwrap();
        let dsts = WalkDir::new(dst_dir)
            .min_depth(1)
            .into_iter()
            .map(|e| e.map(|p| p.into_path()))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let target_dsts = ["a"].map(|s| dst_dir.join(s));
        target_dsts.iter().for_each(|p| assert!(!p.exists()));

        sync.last_dsts_srv.save_last_dsts(&dsts).unwrap();
        sync.sync_back(&target_dsts).unwrap();

        assert!(!sync.src.exists());
    }
    #[test]
    fn test_apply_to_dst_when_empty_dst() {
        let sync = syncer();
        let src_dir = build_tree(&sync.src).unwrap();
        assert!(!sync.dst.exists());

        sync.apply_to_dst(&[] as &[&str; 0]).unwrap();

        compare_same_dirs(src_dir, &sync.dst);
    }

    #[test]
    fn test_apply_to_dst_when_empty_dst_in_targets() {
        let sync = syncer();
        let src_dir = build_tree(&sync.src).unwrap();
        assert!(!sync.dst.exists());

        let fake_dsts = WalkDir::new(src_dir)
            .min_depth(1)
            .into_iter()
            .map(|res| res.unwrap().into_path())
            .map(|p| sync.get_dst_path(p).unwrap())
            .collect_vec();
        let srcs = WalkDir::new(src_dir)
            .min_depth(1)
            .into_iter()
            .map(|res| res.unwrap().into_path())
            .collect_vec();

        let target_dsts = srcs
            .iter()
            .filter(|p| p.is_dir() && p.read_dir().unwrap().any(|d| d.unwrap().path().is_file()))
            .take(1)
            .map(|p| sync.get_dst_path(p).unwrap())
            .collect_vec();
        assert!(!target_dsts.is_empty());

        sync.apply_to_dst(dbg!(&target_dsts)).unwrap();

        fake_dsts
            .iter()
            // filter not in target dsts
            .filter(|d| {
                target_dsts
                    .iter()
                    .all(|t| !d.starts_with(t) && !t.starts_with(d))
            })
            .for_each(|dst| assert!(!dst.exists()));

        target_dsts
            .iter()
            .map(|dst| (sync.get_src_path(dst).unwrap(), dst))
            .for_each(|(src, dst)| compare_same_dirs(src, dst));
    }

    #[test]
    fn test_apply_to_dst_when_sames() {
        let sync = syncer();

        let dst_dir = build_tree(&sync.dst).unwrap();
        assert!(dst_dir.is_dir());
        let src_dir = build_tree(&sync.src).unwrap();
        assert!(src_dir.is_dir());

        sync.apply_to_dst([] as [&str; 0]).unwrap();

        compare_same_dirs(src_dir, dst_dir);
    }

    #[test]
    fn test_apply_to_dst_when_diff_in_both() {
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

        let extra_dst_files =
            create_random_files(["a/b/c/1.t", "d/e/2.t", "f/g"].map(|s| dst_dir.join(s))).unwrap();
        extra_dst_files.iter().for_each(|p| assert!(p.exists()));

        sync.apply_to_dst([] as [&str; 0]).unwrap();

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
        // 保留不在src中的文件
        extra_dst_files.iter().for_each(|p| assert!(p.exists()));
    }
}

#[cfg(test)]
mod clean_tests {
    use std::os::unix::prelude::PermissionsExt;

    use super::*;

    use crate::service::LastDestinationListServiceBuilder;
    use fake::{faker::lorem::zh_cn::Words, Fake};
    use once_cell::sync::Lazy;
    use pretty_assertions::assert_eq;
    use rstest::{fixture, rstest};
    use tempfile::{tempdir, TempDir};
    use uuid::Uuid;

    #[fixture]
    #[once]
    fn mock_syncer() -> Syncer {
        let dir = Path::new("/test.root");
        let (src, dst) = (dir.join(".root"), dir.join("to"));
        SyncerBuilder::default()
            .non_interactive(true)
            .src(src)
            .dst(dst)
            .last_dsts_srv(
                LastDestinationListServiceBuilder::default()
                    .path(dir.join("last_dsts"))
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap()
    }

    #[fixture]
    fn tmp_dotroot() -> Syncer {
        static TMPDIR: Lazy<TempDir> = Lazy::new(|| tempdir().unwrap());

        let dir = TMPDIR.path().join(Uuid::new_v4().to_string());
        let (src, dst) = (dir.join(".root-src"), dir.join(".root-dst"));
        fs::create_dir_all(&src).unwrap();
        fs::create_dir_all(&dst).unwrap();

        SyncerBuilder::default()
            .non_interactive(true)
            .src(src)
            .dst(dst)
            .last_dsts_srv(
                LastDestinationListServiceBuilder::default()
                    .path(dir.join("last_dsts"))
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap()
    }

    #[rstest]
    #[case::empty_when_all_empty(
        // srcs
        &[] as &[&str; 0],
        // dsts
        &[],
        // removable mapped srcs from dsts
        &[],
    )]
    #[case::empty_when_same(
        // srcs
        &["a/b/c/1.txt", "a/b/2.txt"],
        // dsts
        &["a/b/c/1.txt", "a/b/2.txt"],
        // removable mapped srcs from dsts
        &[],
    )]
    #[case::find_one_when_srcs_not_contains_one_in_dsts(
        // srcs
        &["a/b/c/1.txt", "a/b/2.txt", "a/3.txt"],
        // dsts
        &["a/3.txt", "b/4.txt"],
        // removable mapped srcs from dsts
        &["b/4.txt"],
    )]
    #[case::find_all_when_srcs_not_contains_any_in_dsts(
        // srcs
        &["a/b/c/1.txt", "a/b/2.txt", "a/3.txt"],
        // dsts
        &["a/5.txt", "b/4.txt"],
        // removable mapped srcs from dsts
        &["a/5.txt", "b/4.txt"],
    )]
    fn test_find_removable_srcs_ok<P: AsRef<Path>>(
        mock_syncer: &Syncer,
        #[case] curr_srcs: &[P],
        #[case] last_dsts: &[P],
        #[case] expect: &[P],
    ) -> Result<()> {
        let d = mock_syncer;
        assert_eq!(d.dst, PathBuf::from("/test.root/to"));
        let curr_srcs = curr_srcs.iter().map(|s| d.src.join(s)).collect::<Vec<_>>();
        let last_dsts = last_dsts.iter().map(|s| d.dst.join(s)).collect::<Vec<_>>();

        let (srcs, _dsts) = d
            .find_removable_src_dsts_iter(&curr_srcs, &last_dsts)
            .map(|it| {
                it.fold((vec![], vec![]), |mut v, (src, dst)| {
                    v.0.push(src);
                    v.1.push(dst);
                    v
                })
            })?;
        let expect_srcs = expect.iter().map(|p| d.src.join(p)).collect_vec();
        assert_eq!(srcs, expect_srcs);
        Ok(())
    }

    #[rstest]
    #[case::non_absolute_path_in_srcs(&["/a/b.txt", "c.txt"], &[])]
    #[case::non_absolute_path_in_dsts(&[], &["/a/b.txt", "c.txt"])]
    #[case::non_absolute_paths_in_both(&["/a/b.txt", "c.txt"], &["/a/b.txt", "c.txt"])]
    fn test_find_removable_srcs_error_when_exists_non_absolute_paths(
        mock_syncer: &Syncer,
        #[case] curr_srcs: &[&str],
        #[case] last_dsts: &[&str],
    ) {
        let res = mock_syncer.find_removable_src_dsts_iter(curr_srcs, last_dsts);
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .to_string()
            .contains("Found non absolute path"))
    }

    #[rstest]
    #[case::empty(&[] as &[&str; 0], &[])]
    #[case::non_exists(&["a/1.txt"], &["a/1.txt"])]
    fn test_remove_src_dsts_when_non_exists<P>(
        tmp_dotroot: Syncer,
        #[case] srcs: &[P],
        #[case] expect: &[P],
    ) -> Result<()>
    where
        P: AsRef<Path> + Debug,
    {
        let f = |p: &P| (tmp_dotroot.src.join(p), tmp_dotroot.dst.join(p));
        let src_dsts = srcs.iter().map(f).collect_vec();
        let removeds = tmp_dotroot.remove_src_dsts(src_dsts)?;
        assert_eq!(removeds, expect.iter().map(f).collect_vec());
        Ok(())
    }

    #[rstest]
    #[case::non_exists(&["a/1.txt"], &["a/1.txt"])]
    fn test_remove_src_dsts_when_exists<P>(
        tmp_dotroot: Syncer,
        #[case] srcs: &[P],
        #[case] expect: &[P],
    ) -> Result<()>
    where
        P: AsRef<Path> + Debug,
    {
        let f = |p: &P| (tmp_dotroot.src.join(p), tmp_dotroot.dst.join(p));
        let it = srcs.iter().map(f);
        for (src, dst) in it.clone() {
            if let Some(p) = src.parent() {
                fs::create_dir_all(p)?;
            }
            if let Some(p) = dst.parent() {
                fs::create_dir_all(p)?;
            }
            fs::write(src, Words(3..10).fake::<Vec<String>>().join(" "))?;
            fs::write(dst, Words(3..10).fake::<Vec<String>>().join(" "))?;
        }

        let src_dsts = srcs.iter().map(f).collect_vec();
        let removeds = tmp_dotroot.remove_src_dsts(src_dsts)?;
        assert_eq!(removeds, expect.iter().map(f).collect_vec());
        Ok(())
    }

    #[rstest]
    #[case::error_at_first(&["a/b/c", "c/1.txt", "d"], &[])]
    #[case::error_util_first_dir(&["a/1.txt", "b/c/2.txt", "a/b/c", "3.txt", "a/e"], &["a/1.txt", "b/c/2.txt"])]
    /// 最深的dir 最浅的file作为无权限处理
    fn test_remove_src_dsts_error_when_no_perms_dir(
        tmp_dotroot: Syncer,
        #[case] srcs: &[&str],
        #[case] expect: &[&str],
    ) -> Result<()> {
        let mut created_srcs = create_random_files(&tmp_dotroot.src, srcs).unwrap();
        created_srcs.sort_by_cached_key(|p| -(p.ancestors().count() as isize));

        // 最深的dir
        let no_perm_dir = created_srcs.iter().find(|p| p.is_dir()).unwrap();
        let mut perm = no_perm_dir.metadata().unwrap().permissions();
        info!(
            "setting up dir {} from mod {:o} to no perms",
            no_perm_dir.display(),
            perm.mode(),
        );
        perm.set_mode(0o000);
        fs::set_permissions(no_perm_dir, perm).unwrap();

        // 最浅的file
        let no_perm_file = created_srcs.iter().rev().find(|p| p.is_file()).unwrap();
        let mut perm = no_perm_file.metadata().unwrap().permissions();
        info!(
            "setting up file {} from mod {:o} to no perms",
            no_perm_dir.display(),
            perm.mode(),
        );
        perm.set_mode(0o000);
        fs::set_permissions(no_perm_file, perm).unwrap();

        let src_dst_convert = |p| (tmp_dotroot.src.join(p), tmp_dotroot.dst.join(p));
        let res = tmp_dotroot.remove_src_dsts(srcs.iter().map(src_dst_convert).collect_vec());

        assert!(res.is_err());
        let e = res.err().unwrap();
        assert!(e.to_string().contains("Failed to remove src"));
        assert!(e.is::<DotRootRemovedError>());
        let re = e.downcast_ref::<DotRootRemovedError>().unwrap();
        assert_eq!(
            re.src_dsts,
            expect.iter().map(src_dst_convert).collect_vec()
        );
        Ok(())
    }

    /// 创建随机的内容到文件中，paths使用相对pp的路径表示，有后缀名的作为
    /// 文件处理，否则将path创建为目录
    fn create_random_files<T, P, Q>(pp: Q, paths: T) -> Result<Vec<PathBuf>>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let mut res = vec![];
        for p in paths {
            let p = p.as_ref();
            if p.is_absolute() {
                bail!("invalid absolute path {}", p.display());
            }
            let p = pp.as_ref().join(p);
            if p.extension().is_some() {
                if let Some(pp) = p.parent() {
                    trace!("Creating parent dir {}", p.display());
                    fs::create_dir_all(pp)?;
                }
                let contents = Words(3..10).fake::<Vec<String>>().join(" ");
                trace!("Writing to {} with contents: {}", p.display(), contents);
                fs::write(&p, contents)?;
            } else {
                trace!("Creating a dir {}", p.display());
                fs::create_dir_all(&p)?;
            }
            res.push(p);
        }
        Ok(res)
    }

    #[rstest]
    #[case::empty(&[] as &[&str], &[])]
    #[case::file_and_dirs(&["a/b/1.txt", "a/b", "2.t", "c"], &[])]
    #[case::only_files(&["a/b/1.txt", "c/2.t", "3.t"], &[])]
    #[case::empty_in_targets(&[] as &[&str], &["a"])]
    #[case::file_and_dirs_in_targets(&["a/b/1.txt", "a/b", "c/2.txt"], &["a/b"])]
    #[case::only_files_in_targets(&["a/b/1.txt", "c/2.t", "3.t"], &["c"])]
    fn test_clean_dst_when_no_last_dsts(
        tmp_dotroot: Syncer,
        #[case] dsts: &[&str],
        #[case] target_dsts: &[&str],
    ) {
        // pre setup
        create_random_files(&tmp_dotroot.dst, dsts).unwrap();
        let target_dsts_paths = target_dsts
            .iter()
            .map(|s| tmp_dotroot.dst.join(s))
            .collect_vec();

        let removeds = tmp_dotroot.clean_dst(target_dsts_paths).unwrap();

        assert!(removeds.is_empty());
    }

    #[rstest]
    #[case::last_dsts_not_contains_some_srcs_in_targets(&["a/b/1.t", "c", "d/2.t"], &["a/b/1.t"], &["a/b", "d"])]
    #[case::sames_in_targets(&["a/b/1.t", "c"], &["a/b/1.t", "c"], &["a/b", "c"])]
    #[case::empty_last_dsts_in_targets(&["a/b/1.t", "c/2.t", "d/e"], &[], &["a/b", "c"])]
    #[case::empty_srcs_in_targets(&[] as &[&str], &["a/b/1.t", "c/2.t", "d/e"], &["a/b", "d"])]
    #[case::empty_srcs(&[] as &[&str], &["a/b/1.t", "c/2.t", "d/e"], &[])]
    #[case::empty_last_dsts(&["a/b/1.t"], &[], &[])]
    #[case::sames(&["a/b/1.t", "c"], &["a/b/1.t", "c"], &[])]
    #[case::last_dsts_not_contains_some_srcs(&["a/b/1.t", "c", "d/2.t"], &["a/b/1.t"], &[])]
    #[case::srcs_not_contains_some_last_dsts(&["a/b/1.t"], &["a/b/1.t", "c", "d/2.t"], &[])]
    #[case::srcs_not_contains_some_last_dsts_in_targets(&["a/b/1.t"], &["a/b/1.t", "c", "d/2.t"], &["a/b", "c"])]
    fn test_clean_dst_when_last_dsts(
        tmp_dotroot: Syncer,
        #[case] srcs: &[&str],
        #[case] last_dsts: &[&str],
        #[case] target_dsts: &[&str],
    ) {
        create_random_files(&tmp_dotroot.src, srcs).unwrap();
        create_random_files(&tmp_dotroot.dst, last_dsts).unwrap();

        let last_dst_paths = last_dsts
            .iter()
            .flat_map(|p| Path::new(p).ancestors())
            // 当pop到顶时 过滤空str的路径 即不保存src/dst当前路径
            .filter(|p| !p.to_str().unwrap().is_empty())
            // 绝对路径用于检查是否存在
            .map(|p| tmp_dotroot.dst.join(p))
            .collect::<HashSet<_>>();
        let target_dst_paths = target_dsts
            .iter()
            .map(|s| tmp_dotroot.dst.join(s))
            .collect_vec();

        // pre setup last_dsts to load
        tmp_dotroot
            .last_dsts_srv
            .save_last_dsts(&last_dst_paths)
            .unwrap();
        let removeds = tmp_dotroot.clean_dst(target_dst_paths).unwrap();
        removeds.iter().for_each(|p| assert!(!p.exists()));
        last_dst_paths
            .iter()
            .filter(|p| !removeds.contains(p))
            .for_each(|p| assert!(p.exists()));

        let src_mapped_dst_paths = srcs
            .iter()
            // 获取所有唯一的子路径
            .flat_map(|p| Path::new(p).ancestors())
            // 当pop到顶时 过滤空str的路径 即不保存src/dst当前路径
            .filter(|p| !p.to_str().unwrap().is_empty())
            // 获取srcs对应的last_dsts 用于检查对应的dsts是否被移除
            .map(|p| tmp_dotroot.dst.join(p))
            .collect::<HashSet<_>>();
        let expect_removeds = last_dsts
            .iter()
            .flat_map(|s| Path::new(s).ancestors())
            // 当pop到顶时 过滤空str的路径 即不保存src/dst当前路径
            .filter(|p| !p.to_str().unwrap().is_empty())
            // targets 过滤
            .filter(|p| target_dsts.is_empty() || target_dsts.iter().any(|t| p.starts_with(t)))
            .map(|p| tmp_dotroot.dst.join(p))
            // 过滤不在srcs中的dst 即要移除的dst
            .filter(|p| !src_mapped_dst_paths.contains(p))
            .sorted()
            .collect_vec();
        expect_removeds.iter().for_each(|p| assert!(!p.exists()));
        assert_eq!(
            removeds.iter().cloned().sorted().collect_vec(),
            expect_removeds
        );
        let expect_last_dsts = src_mapped_dst_paths
            .iter()
            .chain(
                // new last_dsts应该是从cur srcs中添加last dsts中跳过的部分
                last_dst_paths
                    .iter()
                    .filter(|p| !expect_removeds.contains(p))
                    // 过滤当前targets
                    .filter(|p| {
                        target_dsts.is_empty() || target_dsts.iter().any(|t| p.starts_with(t))
                    }),
            )
            .unique()
            .sorted()
            .collect_vec();
        expect_last_dsts.iter().for_each(|p| {
            assert!(
                p.exists() ||
                // 由于cur srcs存在但last dsts不存在时会保存到 文件中，但不会在模拟的expect_last_dsts实际被创建
                tmp_dotroot.get_src_path(p).unwrap().exists(),
                "expect exists path {}",
                p.display()
            )
        });

        let new_last_dsts = tmp_dotroot.last_dsts_srv.load_last_dsts().unwrap();
        assert_eq!(
            new_last_dsts.iter().sorted().collect_vec(),
            expect_last_dsts
        );
    }
}
