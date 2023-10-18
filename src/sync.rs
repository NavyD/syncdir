use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Error, Result};
use derive_builder::Builder;
use dialoguer::{theme::ColorfulTheme, Confirm};
use itertools::Itertools;
use log::{debug, info, log_enabled, trace, warn};
use os_display::Quotable;
use walkdir::WalkDir;

use crate::{
    cp::Copier,
    service::LastDestinationListService,
    util::{self, get_path_ty},
};

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

impl SyncerBuilder {
    /// 设置绝对路径src。如果p为相对路径则使用[std::env::current_dir]配置
    pub fn try_src(&mut self, p: impl AsRef<Path>) -> Result<&mut Self> {
        let p = p.as_ref();
        if p.is_absolute() {
            self.src = Some(p.to_path_buf());
        } else {
            self.src = Some(std::env::current_dir().map(|d| d.join(p))?);
        }
        Ok(self)
    }

    /// 设置绝对路径dst。如果p为相对路径则使用[std::env::current_dir]配置
    pub fn try_dst(&mut self, p: impl AsRef<Path>) -> Result<&mut Self> {
        let p = p.as_ref();
        if p.is_absolute() {
            self.dst = Some(p.to_path_buf());
        } else {
            self.dst = Some(std::env::current_dir().map(|d| d.join(p))?);
        }
        Ok(self)
    }

    fn validate(&self) -> Result<(), String> {
        if let Some(src) = &self.src {
            if !src.exists() {
                return Err(format!("Not found src {}", src.display()));
            }

            if !src.is_absolute() {
                return Err(format!("Found non absolute src path {}", src.display()));
            }
        }

        if let Some(dst) = &self.dst {
            if !dst.is_absolute() {
                return Err(format!("Found non absolute dst path {}", dst.display()));
            }
        }
        Ok(())
    }
}

/// 将src目录内容同步到dst目录中
#[derive(Debug, Clone, Builder)]
#[builder(setter(into), build_fn(validate = "Self::validate"))]
pub struct Syncer {
    /// 要求使用已存在的绝对路径
    #[builder(setter(into))]
    src: PathBuf,

    /// 要求使用已存在的绝对路径
    #[builder(setter(into))]
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
    ///     * 如果在last_dsts中存在但当前dst不存在时将从src移除对应的路径
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
        let target_dsts = self.get_absolute_target_dsts(target_dsts)?;

        let last_dsts = self
            .last_dsts_srv
            .load_last_dsts_in_targets(&target_dsts)?
            .unwrap_or_default();
        info!(
            "Found {} last dsts in {} target dsts: {:?}",
            last_dsts.len(),
            target_dsts.len(),
            target_dsts,
        );

        if log_enabled!(log::Level::Debug) {
            let dsts = WalkDir::new(&self.dst)
                .into_iter()
                .flat_map(|res| res.map(|e| e.into_path()))
                .collect_vec();
            let srcs = WalkDir::new(&self.src)
                .into_iter()
                .flat_map(|res| res.map(|e| e.into_path()))
                .collect_vec();
            debug!(
                "Syncing back from {} dst {} paths to {} src {} paths \
                with {} last dsts in {} target dsts {:?}",
                self.dst.quote(),
                dsts.len(),
                self.src.quote(),
                srcs.len(),
                last_dsts.len(),
                target_dsts.len(),
                target_dsts,
            );
        }
        if last_dsts.is_empty() {
            return self.sync_back_with_src(&target_dsts);
        }

        if self.dry_run {
            return last_dsts
                .iter()
                .cloned()
                .map(|dst| {
                    self.get_src_path(&dst).map(|src| {
                        if !dst.is_symlink() || !dst.exists() {
                            SyncPath::Removed(src)
                        } else {
                            sync_path!(dst, src)
                        }
                    })
                })
                .collect::<Result<Vec<_>>>();
        }

        // sync back from last dsts to src
        target_dsts
            .into_iter()
            .map(|t_dst| {
                let src = self.get_src_path(&t_dst)?;
                // 用于判断last dsts对应的src文件是否存在
                let curr_srcs = WalkDir::new(&src)
                    .into_iter()
                    .map(|e| e.map(|d| d.into_path()))
                    .collect::<Result<HashSet<_>, _>>()?;
                debug!(
                    "Found {} paths in src {} for sync back in target {}",
                    curr_srcs.len(),
                    src.quote(),
                    t_dst.quote(),
                );

                // src不存在或删除后
                let paths_it = if self.confirm_rm(&src)? {
                    Some(
                        last_dsts
                            .iter()
                            .filter(move |sub_dst| sub_dst.starts_with(&t_dst))
                            .cloned()
                            // 如果last dst不存在则移除src 否则复制dst->src
                            .map(move |dst| {
                                let src = self.get_src_path(&dst)?;
                                let sp = if !dst.exists() && !dst.is_symlink() {
                                    trace!(
                                        "Removing {} src {} for non exists dst {}",
                                        get_path_ty(&src),
                                        src.quote(),
                                        dst.quote(),
                                    );
                                    util::try_remove(&src)?;
                                    SyncPath::Removed(src)
                                } else {
                                    trace!(
                                        "Copying only {} dst {} to {} src {}",
                                        get_path_ty(&dst),
                                        dst.quote(),
                                        get_path_ty(&src),
                                        src.quote(),
                                    );
                                    self.copier.copy_file_or_dir_only(&dst, &src)?;
                                    if curr_srcs.contains(&src) {
                                        SyncPath::Overriden(dst, src)
                                    } else {
                                        SyncPath::Coppied(dst, src)
                                    }
                                };
                                Ok::<_, Error>(sp)
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    )
                } else {
                    None
                }
                .into_iter()
                .flatten();
                Ok::<_, Error>(paths_it)
            })
            .flatten_ok()
            .collect::<Result<Vec<_>>>()
    }

    /// 将src中的文件同步到指定的target_dsts中。扫描src目录，并找到对应的dst目录dsts，
    /// 将src存在的文件复制到dsts中
    ///
    /// 如果target_dsts是相对路径则是相对[Self::dst]
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
        let target_dsts = self.get_absolute_target_dsts(target_dsts)?;
        WalkDir::new(&self.src)
            .contents_first(true)
            .into_iter()
            .map(|entry| {
                let entry = entry?;
                let src = entry.into_path();
                let dst = self.get_dst_path(&src)?;
                // println!("src {}, dst {}", src.display(), dst.display());

                // skip non target dsts
                if target_dsts.iter().all(|t| !dst.starts_with(t)) {
                    trace!(
                        "Skipped sync dst {} not in {} target dsts",
                        dst.quote(),
                        target_dsts.len()
                    );
                    return Ok(None);
                }

                if self.dry_run {
                    return Ok(Some(sync_path!(src, dst)));
                }

                if dst.is_symlink() || dst.is_file() {
                    trace!(
                        "Removing {} dst {} path for {} src {}",
                        get_path_ty(&dst),
                        dst.quote(),
                        get_path_ty(&src),
                        src.quote(),
                    );
                    return Ok::<_, Error>(if self.confirm_rm(&dst)? {
                        self.copier.copy_file(&src, &dst)?;
                        Some(SyncPath::Overriden(src, dst))
                    } else {
                        None
                    });
                }

                if !dst.exists() {
                    trace!(
                        "Copying from {} src {} to non exists dst {} path",
                        get_path_ty(&src),
                        src.quote(),
                        dst.quote(),
                    );
                    if !src.is_symlink() && src.is_dir() {
                        fs::create_dir_all(&dst)?;
                        self.copier.copy_file_or_dir_only(&src, &dst)?;
                    } else {
                        self.copier.copy_file(&src, &dst)?;
                    }
                    return Ok(Some(SyncPath::Coppied(src, dst)));
                }

                trace!(
                    "Copying force from {} src {} to {} dst {} path",
                    get_path_ty(&src),
                    src.quote(),
                    get_path_ty(&dst),
                    dst.quote(),
                );
                Ok(if !src.is_symlink() && src.is_dir() {
                    self.copier.copy_file_or_dir_only(&src, &dst)?;
                    Some(SyncPath::Overriden(src, dst))
                } else if self.confirm_rm(&dst)? {
                    fs::create_dir(&dst)?;
                    self.copier.copy_file_or_dir_only(&src, &dst)?;
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

    /// 根据[LastDestinationListService]的配置last_dsts与当前src映射回的虚拟cur_dsts比较，
    /// 如果在指定的target的last_dsts路径不再cur_dsts存在则清理dst目录中的文件并返回被移除的文件
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
        let target_dsts = self.get_absolute_target_dsts(target_dsts)?;
        if log_enabled!(log::Level::Info) {
            info!(
                "Cleaning dst {} in {} targets: [{}]",
                self.dst.quote(),
                target_dsts.len(),
                target_dsts.iter().map(|p| p.quote()).join(", ")
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
        trace!("Loaded {} curr srcs", curr_srcs.len());

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

    /// 转换路径target_dsts为绝对路径。
    ///
    /// 如果dst是相对路径则使用[Self::dst]连接。如果target_dsts为空时默认加入[Self::dst]
    ///
    /// ## Errors
    ///
    /// * 如果target_dsts中的绝对路径不是在dst中
    /// * 如果target_dsts的路径存在嵌入的路径如`[/a, /a/b]`
    fn get_absolute_target_dsts<T, P>(&self, target_dsts: T) -> Result<Vec<PathBuf>>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let mut res: Vec<PathBuf> = vec![];
        for dst in target_dsts {
            let dst = dst.as_ref();
            let dst = if dst.is_absolute() {
                if !dst.starts_with(&self.dst) {
                    bail!(
                        "Found target absolute path {} that does not match the current dst path {}",
                        dst.quote(),
                        self.dst.quote()
                    );
                }
                dst.to_path_buf()
            } else {
                self.dst.join(dst)
            };

            if let Some(p) = res
                .iter()
                .find(|p| **p != dst && (p.starts_with(&dst) || dst.starts_with(p)))
            {
                bail!("Found nested path {} in target {}", p.quote(), dst.quote());
            }
            res.push(dst);
        }

        if res.is_empty() {
            res.push(self.dst.to_path_buf());
        }
        Ok(res)
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
        if !p.is_symlink() && !p.exists() {
            Ok(true)
        } else if self.non_interactive
            || Confirm::with_theme(&ColorfulTheme::default())
                .default(true)
                .show_default(true)
                .with_prompt(format!("Do you want to remove path {}", p.display()))
                .interact()?
        {
            util::try_remove(p)?;
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
        if !self.src.exists() {
            info!(
                "Skipped sync back with empty src {} from dst {}",
                self.src.quote(),
                self.dst.quote(),
            );
            return Ok(vec![]);
        }

        let target_dsts = self.get_absolute_target_dsts(target_dsts)?;

        if log_enabled!(log::Level::Debug) {
            let srcs = WalkDir::new(&self.src)
                .into_iter()
                .flat_map(|res| res.map(|e| e.into_path()))
                .collect_vec();
            debug!(
                "Syncing back with {} srcs from dst {} to src {} in targets {:?}",
                srcs.len(),
                self.dst.quote(),
                self.src.quote(),
                target_dsts,
            );
        }

        WalkDir::new(&self.src)
            .into_iter()
            .map(|entry| {
                let entry = entry?;
                let src = entry.into_path();
                let dst = self.get_dst_path(&src)?;

                // skip non target dsts
                if target_dsts.iter().all(|t| !dst.starts_with(t)) {
                    trace!(
                        "Skipped sync dst {} not in {} target dsts",
                        dst.quote(),
                        target_dsts.len()
                    );
                    return Ok::<_, Error>(None);
                }

                // 在walkdir后不应该找不到path
                // if !src.exists() {
                //     warn!(
                //         "Skipped not exists path {} after walk dir {}",
                //         src.quote(),
                //         self.src.quote()
                //     );
                //     return Ok(None);
                // }

                if self.dry_run {
                    return Ok(Some(if !dst.exists() {
                        SyncPath::Removed(src)
                    } else {
                        SyncPath::Overriden(dst, src)
                    }));
                }

                // dst为文件/软链接时 移除src目录
                if dst.is_symlink() || dst.is_file() {
                    trace!(
                        "Removing {} src {} path for {} dst {}",
                        get_path_ty(&src),
                        src.quote(),
                        get_path_ty(&dst),
                        dst.quote(),
                    );
                    return Ok(if self.confirm_rm(&src)? {
                        self.copier.copy_file(&dst, &src)?;
                        Some(SyncPath::Overriden(dst, src))
                    } else {
                        None
                    });
                }

                // dst不存在 移除src
                if !dst.exists() {
                    trace!(
                        "Removing {} src {} for non exists dst {}",
                        get_path_ty(&src),
                        src.quote(),
                        dst.quote()
                    );
                    return Ok(if self.confirm_rm(&src)? {
                        Some(sync_path!(src))
                    } else {
                        None
                    });
                }

                // dst目录时 当src时 不存在或目录则mkdir -p, 文件则移除
                trace!(
                    "Removing {} src {} for {} dst {}",
                    get_path_ty(&src),
                    src.quote(),
                    get_path_ty(&dst),
                    dst.quote(),
                );
                Ok(if src.is_dir() {
                    self.copier.copy_file_or_dir_only(&dst, &src)?;
                    Some(SyncPath::Overriden(dst, src))
                } else if self.confirm_rm(&src)? {
                    fs::create_dir(&src)?;
                    self.copier.copy_file_or_dir_only(&dst, &src)?;
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
                "Finding removable {} last srcs not contains in {} current srcs: [{}]",
                last_srcs.len(),
                curr_srcs.len(),
                last_srcs.iter().map(|v| v.0.display()).join(","),
            );
        }
        let it = last_srcs
            .into_iter()
            .filter(move |(src, _)| !curr_srcs.contains(src.as_path()));
        if log_enabled!(log::Level::Trace) {
            let v = it.clone().collect_vec();
            trace!("Found {} Removable src dsts paths: {:?}", v.len(), v);
        }
        Ok(it)
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
