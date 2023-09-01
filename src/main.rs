use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Error, Result};
use derive_builder::Builder;
use dialoguer::{theme::ColorfulTheme, Confirm};
use dotroot::{config::LastDestinationConfig, util};
use itertools::Itertools;
use log::{debug, info, log_enabled, trace, warn};
use rayon::prelude::*;
use walkdir::WalkDir;

fn main() {}

#[derive(Debug, Clone, Builder)]
pub struct DotRoot {
    src: PathBuf,
    dst: PathBuf,
    last_dsts_conf: LastDestinationConfig,
    non_interactive: bool,
}

impl DotRoot {
    pub fn clean<P>(&self, target_srcs: &[P]) -> Result<()>
    where
        P: AsRef<Path>,
    {
        if log_enabled!(log::Level::Info) {
            let mut s = target_srcs.iter().map(|p| p.as_ref().display()).join(", ");
            if s.is_empty() {
                s = "[]".to_string();
            }
            info!("Cleaning in {} targets: {}", target_srcs.len(), s);
        }

        // let mut target_srcs = target_srcs.into_iter();
        // 0. read last dsts from file
        let last_dsts = self.last_dsts_conf.load_last_dsts()?;
        trace!("Loaded {} last dsts: {:?}", last_dsts.len(), last_dsts);

        // 1. load cur srcs
        let curr_srcs = self.load_curr_srcs()?;
        debug!("Loaded {} curr srcs: {:?}", curr_srcs.len(), curr_srcs);

        // 2. find removable srcs and dsts
        let removables = self
            .find_removable_src_dsts_iter(&curr_srcs, &last_dsts)?
            .filter(|(_, dst)| {
                target_srcs.is_empty() || target_srcs.iter().any(|p| dst.starts_with(p))
            });

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
        let new_last_dsts = match self.remove_src_dsts(removables) {
            Ok(removeds) => get_new_last_dsts(
                &(removeds.into_iter().unzip() as (Vec<PathBuf>, Vec<PathBuf>)).1,
            )?,
            Err(mut e) => {
                let errstr = e.to_string();
                if let Some(removed) = e.downcast_mut::<DotRootRemovedError>() {
                    warn!(
                        "Trying to save removed {} paths for remove file error: {}",
                        removed.src_dsts.len(),
                        errstr
                    );
                    let (_, dsts): (Vec<PathBuf>, Vec<PathBuf>) =
                        removed.src_dsts.drain(..).unzip();
                    let new_last_dsts = get_new_last_dsts(&dsts)?;
                    self.last_dsts_conf.save_last_dsts(&new_last_dsts)?;
                }
                return Err(e);
            }
        };

        // 4. update dirs to file
        self.last_dsts_conf.save_last_dsts(&new_last_dsts)?;
        trace!(
            "Saved {} last dsts: {:?}",
            new_last_dsts.len(),
            new_last_dsts
        );
        Ok(())
    }

    /// 遍历src目录找到所有文件
    fn load_curr_srcs(&self) -> Result<Vec<PathBuf>> {
        WalkDir::new(&self.src)
            // 移除当前目录
            .min_depth(1)
            .into_iter()
            .par_bridge()
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
        P: AsRef<Path> + Debug + Clone,
    {
        let (mut removeds, mut skippeds) = (vec![], vec![]);
        let src_dsts = src_dsts.into_iter().collect_vec();
        debug!(
            "Trying to remove {} src dsts: {:?}",
            src_dsts.len(),
            src_dsts
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

        trace!(
            "Removed a total of {} files: {:?}. and skipped {} files: {:?}",
            removeds.len(),
            removeds,
            new_skippeds.len(),
            new_skippeds,
        );

        Ok(removeds)
    }

    /// 从src路径转换到对应的dst路径：
    /// src=/a/b,dst=/c/d/e: src_sub=a/b/1/2.t => dst_path=/c/d/e/1/2.t
    fn get_dst_path(&self, src_sub: impl AsRef<Path>) -> Result<PathBuf> {
        util::map_path(src_sub.as_ref(), &self.src, &self.dst)
    }

    fn get_src_path(&self, dst_sub: impl AsRef<Path>) -> Result<PathBuf> {
        util::map_path(dst_sub.as_ref(), &self.dst, &self.src)
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
    use super::*;
    use dotroot::config::LastDestinationConfigBuilder;
    use fake::{faker::lorem::zh_cn::Words, Fake};
    use once_cell::sync::Lazy;
    use pretty_assertions::assert_eq;
    use rstest::{fixture, rstest};
    use tempfile::{tempdir, TempDir};
    use uuid::Uuid;

    use std::{os::unix::prelude::PermissionsExt, sync::Once};

    use log::LevelFilter;

    #[ctor::ctor]
    fn init() {
        static INIT: Once = Once::new();
        INIT.call_once(|| {
            env_logger::builder()
                .is_test(true)
                .filter_level(LevelFilter::Info)
                .filter_module(env!("CARGO_CRATE_NAME"), LevelFilter::Trace)
                .init();
        });
    }

    #[fixture]
    #[once]
    fn mock_dotroot() -> DotRoot {
        let dir = Path::new("/test.root");
        let (src, dst) = (dir.join(".root"), dir.join("to"));
        DotRootBuilder::default()
            .non_interactive(true)
            .src(src)
            .dst(dst)
            .last_dsts_conf(
                LastDestinationConfigBuilder::default()
                    .path(dir.join("last_dsts"))
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap()
    }

    #[fixture]
    fn tmp_dotroot() -> DotRoot {
        static TMPDIR: Lazy<TempDir> = Lazy::new(|| tempdir().unwrap());

        let dir = TMPDIR.path().join(Uuid::new_v4().to_string());
        let (src, dst) = (dir.join(".root-src"), dir.join(".root-dst"));
        fs::create_dir_all(&src).unwrap();
        fs::create_dir_all(&dst).unwrap();

        DotRootBuilder::default()
            .non_interactive(true)
            .src(src)
            .dst(dst)
            .last_dsts_conf(
                LastDestinationConfigBuilder::default()
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
        mock_dotroot: &DotRoot,
        #[case] curr_srcs: &[P],
        #[case] last_dsts: &[P],
        #[case] expect: &[P],
    ) -> Result<()> {
        let d = mock_dotroot;
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
        mock_dotroot: &DotRoot,
        #[case] curr_srcs: &[&str],
        #[case] last_dsts: &[&str],
    ) {
        let res = mock_dotroot.find_removable_src_dsts_iter(curr_srcs, last_dsts);
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
        tmp_dotroot: DotRoot,
        #[case] srcs: &[P],
        #[case] expects: &[P],
    ) -> Result<()>
    where
        P: AsRef<Path> + Debug,
    {
        let f = |p: &P| (tmp_dotroot.src.join(p), tmp_dotroot.dst.join(p));
        let it = srcs.iter().map(f);
        let removeds = tmp_dotroot.remove_src_dsts(it)?;
        assert_eq!(removeds, expects.iter().map(f).collect_vec());
        Ok(())
    }

    #[rstest]
    #[case::non_exists(&["a/1.txt"], &["a/1.txt"])]
    fn test_remove_src_dsts_when_exists<P>(
        tmp_dotroot: DotRoot,
        #[case] srcs: &[P],
        #[case] expects: &[P],
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

        let removeds = tmp_dotroot.remove_src_dsts(it)?;
        assert_eq!(removeds, expects.iter().map(f).collect_vec());
        Ok(())
    }

    #[rstest]
    #[case::error_at_first(&["a/b/c", "c/1.txt", "d"], &[])]
    #[case::error_util_first_dir(&["a/1.txt", "b/c/2.txt", "a/b/c", "3.txt", "a/e"], &["a/1.txt", "b/c/2.txt"])]
    /// 最深的dir 最浅的file作为无权限处理
    fn test_remove_src_dsts_error_when_no_perms_dir(
        tmp_dotroot: DotRoot,
        #[case] srcs: &[&str],
        #[case] expect: &[&str],
    ) -> Result<()> {
        let mut created_srcs =
            create_random_files(&tmp_dotroot.src.to_str().unwrap(), srcs).unwrap();
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
        let src_dsts = srcs.iter().map(src_dst_convert).collect_vec();
        let res = tmp_dotroot.remove_src_dsts(src_dsts);

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
    fn create_random_files<P>(pp: &P, paths: &[P]) -> Result<Vec<PathBuf>>
    where
        P: AsRef<Path> + Debug,
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
    fn test_clean_when_no_last_dsts(
        tmp_dotroot: DotRoot,
        #[case] srcs: &[&str],
        #[case] target_srcs: &[&str],
    ) {
        // pre setup
        create_random_files(&tmp_dotroot.src.to_str().unwrap(), srcs).unwrap();
        let target_src_paths = target_srcs
            .iter()
            .map(|s| tmp_dotroot.dst.join(s))
            .collect_vec();

        tmp_dotroot.clean(&target_src_paths).unwrap();

        let expect_last_dsts = srcs
            .iter()
            // 获取所有唯一的子路径
            .flat_map(|p| Path::new(p).ancestors())
            // 当pop到顶时 过滤空str的路径 即不保存src/dst当前路径
            .filter(|p| !p.to_str().unwrap().is_empty())
            .unique()
            // 获取srcs对应的last_dsts 用于检查对应的dsts是否被移除
            .map(|p| tmp_dotroot.dst.join(p))
            .sorted()
            .collect_vec();
        let new_last_dsts = tmp_dotroot.last_dsts_conf.load_last_dsts().unwrap();

        assert_eq!(
            new_last_dsts.into_iter().sorted().collect_vec(),
            expect_last_dsts
        );
    }

    #[rstest]
    #[case::empty_srcs(&[] as &[&str], &["a/b/1.t", "c/2.t", "d/e"], &[])]
    #[case::empty_last_dsts(&["a/b/1.t"], &[], &[])]
    #[case::sames(&["a/b/1.t", "c"], &["a/b/1.t", "c"], &[])]
    #[case::last_dsts_not_contains_some_srcs(&["a/b/1.t", "c", "d/2.t"], &["a/b/1.t"], &[])]
    #[case::srcs_not_contains_some_last_dsts(&["a/b/1.t"], &["a/b/1.t", "c", "d/2.t"], &[])]
    #[case::srcs_not_contains_some_last_dsts_in_targets(&["a/b/1.t"], &["a/b/1.t", "c", "d/2.t"], &["a/b", "c"])]
    #[case::last_dsts_not_contains_some_srcs_in_targets(&["a/b/1.t", "c", "d/2.t"], &["a/b/1.t"], &["a/b", "d"])]
    #[case::sames_in_targets(&["a/b/1.t", "c"], &["a/b/1.t", "c"], &["a/b", "c"])]
    #[case::empty_last_dsts_in_targets(&["a/b/1.t", "c/2.t", "d/e"], &[], &["a/b", "c"])]
    #[case::empty_srcs_in_targets(&[] as &[&str], &["a/b/1.t", "c/2.t", "d/e"], &["a/b", "d"])]
    fn test_clean_when_last_dsts(
        tmp_dotroot: DotRoot,
        #[case] srcs: &[&str],
        #[case] last_dsts: &[&str],
        #[case] target_srcs: &[&str],
    ) {
        create_random_files(&tmp_dotroot.src.to_str().unwrap(), srcs).unwrap();
        create_random_files(&tmp_dotroot.dst.to_str().unwrap(), last_dsts).unwrap();

        let last_dst_paths = last_dsts
            .iter()
            .flat_map(|p| Path::new(p).ancestors())
            // 当pop到顶时 过滤空str的路径 即不保存src/dst当前路径
            .filter(|p| !p.to_str().unwrap().is_empty())
            // 绝对路径用于检查是否存在
            .map(|p| tmp_dotroot.dst.join(p))
            .collect::<HashSet<_>>();
        let target_src_paths = target_srcs
            .iter()
            .map(|s| tmp_dotroot.dst.join(s))
            .collect_vec();

        // pre setup last_dsts to load
        tmp_dotroot
            .last_dsts_conf
            .save_last_dsts(&last_dst_paths)
            .unwrap();
        tmp_dotroot.clean(&target_src_paths).unwrap();

        let src_mapped_dst_paths = srcs
            .iter()
            // 获取所有唯一的子路径
            .flat_map(|p| Path::new(p).ancestors())
            // 当pop到顶时 过滤空str的路径 即不保存src/dst当前路径
            .filter(|p| !p.to_str().unwrap().is_empty())
            // 获取srcs对应的last_dsts 用于检查对应的dsts是否被移除
            .map(|p| tmp_dotroot.dst.join(p))
            .collect::<HashSet<_>>();

        let expect_removed = last_dst_paths
            .iter()
            .filter(|p| !src_mapped_dst_paths.contains(*p))
            .filter(|p| {
                target_src_paths.is_empty() || target_src_paths.iter().any(|t| p.starts_with(t))
            })
            .collect::<HashSet<_>>();
        for p in dbg!(&expect_removed) {
            assert!(!p.exists(), "expect non exists path {}", p.display());
        }

        // new last_dsts应该是从cur srcs中添加last dsts中跳过的部分
        let expect_last_dsts = src_mapped_dst_paths
            .iter()
            .chain(
                last_dst_paths
                    .iter()
                    .filter(|p| !expect_removed.contains(p)),
            )
            .unique()
            // targets仅用于清理范围但应该保存dst所有路径防止下次更改targets意外删除文件
            // .filter(|p| {
            //     target_src_paths.is_empty() || target_src_paths.iter().any(|t| p.starts_with(t))
            // })
            .sorted()
            .collect_vec();
        for p in dbg!(&expect_last_dsts) {
            assert!(
                p.exists() ||
                    // 由于cur srcs存在但last dsts不存在时会保存到 文件中，但不会在模拟的expect_last_dsts实际被创建
                    tmp_dotroot.get_src_path(p).unwrap().exists(),
                "expect exists path {}",
                p.display()
            );
        }

        let new_last_dsts = tmp_dotroot.last_dsts_conf.load_last_dsts().unwrap();
        assert_eq!(
            new_last_dsts.iter().sorted().collect_vec(),
            expect_last_dsts
        );
    }
}
