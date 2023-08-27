use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    fs::{self, File},
    hash::Hash,
    io::{self, prelude::*, BufReader},
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Error, Result};
use derive_builder::Builder;
use dialoguer::{theme::ColorfulTheme, Confirm};
use itertools::Itertools;
use log::{debug, info, log_enabled, trace, warn, error};
use mlua::prelude::*;
use rayon::prelude::*;
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

fn main() -> LuaResult<()> {
    Ok(())
}

struct Config {
    last_path: PathBuf,
    non_interactive: bool,
}

#[derive(Debug, Clone, Builder)]
pub struct DotRoot {
    src: PathBuf,
    dst: PathBuf,
    last_dsts_path: PathBuf,
    non_interactive: bool,
}

impl DotRoot {
    // pub fn new<P: AsRef<Path>>(from: P, to: P) -> Result<Self> {
    //     let (from, to) = (from.as_ref(), to.as_ref());
    //     Ok(Self {
    //         dir: from.canonicalize()?,
    //         to: to.canonicalize()?,
    //         non_interactive: false,
    //     })
    // }
    pub fn sync(&self) -> Result<()> {
        WalkDir::new(&self.src)
            .into_iter()
            .par_bridge()
            .try_for_each(|entry| {
                let e = entry?;
                let from = e.path();
                if from.is_file() {
                    let to = self.get_dst_path(from)?;
                    if !to.exists() {
                        info!(
                            "Removing {} for non exists {}",
                            from.display(),
                            to.display()
                        );
                        fs::remove_file(from)?;
                    } else if self.has_changed(from, &to)? {
                        self.copy_file(&*to, from)?;
                    }
                }
                Ok::<_, Error>(())
            })?;
        Ok(())
    }

    pub fn apply(&self) -> Result<()> {
        WalkDir::new(&self.src)
            .into_iter()
            .par_bridge()
            .try_for_each(|entry| {
                let e = entry?;
                if e.path_is_symlink() || e.file_type().is_file() {
                    let from = e.path();
                    let to = self.get_dst_path(e.path())?;
                    if !to.exists() || self.has_changed(from, &to)? {
                        self.copy_file(from, &to)?;
                    }
                }
                Ok::<_, Error>(())
            })?;
        Ok(())
    }

    pub fn clean<P: AsRef<Path>>(&self, target_srcs: &[P]) -> Result<()> {
        // 0. read last dsts from file
        let last_dsts = self.load_last_dsts()?;
        debug!("Loaded {} last dsts", last_dsts.len());

        // 1. load cur srcs
        let curr_srcs = self.load_curr_srcs()?;

        // 2. find removable srcs and dsts
        let removables = self
            .find_removable_src_dsts_iter(&curr_srcs, &last_dsts)?
            .filter(|(src, _)| target_srcs.iter().any(|p| src.starts_with(p)));

        // 3. remove dirs. if error then save lasts and reerror
        let get_removed_last_dsts = |removeds: &[PathBuf]| {
            last_dsts
                .iter()
                .filter(|p| removeds.contains(p))
                .collect_vec()
        };

        let new_last_dsts = match self.remove_src_dsts(removables) {
            Ok(removeds) => get_removed_last_dsts(
                &(removeds.into_iter().unzip() as (Vec<PathBuf>, Vec<PathBuf>)).1,
            ),
            Err(mut e) => {
                if let Some(removed) = e.downcast_mut::<DotRootRemovedError>() {
                    let (_, dsts): (Vec<PathBuf>, Vec<PathBuf>) =
                        removed.src_dsts.drain(..).unzip();
                    let new_last_dsts = get_removed_last_dsts(&dsts);
                    self.save_last_dsts(new_last_dsts)?;
                }
                return Err(e);
            }
        };

        // 4. update dirs to file
        self.save_last_dsts(new_last_dsts)?;
        Ok(())
    }

    fn check_last_dst(&self, dst: impl AsRef<Path>) -> Result<()> {
        let p = dst.as_ref();
        if !p.is_absolute() {
            warn!("Ignored relative path {}", p.display());
        }
        if !p.exists() {
            warn!("Ignored non exists path {}", p.display());
        }
        if p.is_dir() {
            bail!("Disallowed directory type {} was found", p.display());
        }
        Ok(())
    }

    fn load_last_dsts(&self) -> Result<Vec<PathBuf>> {
        info!("Loading last dsts from {}", self.last_dsts_path.display());
        if !self.last_dsts_path.is_file() {
            bail!("{} is not file", self.last_dsts_path.display());
        }
        BufReader::new(File::open(&self.last_dsts_path)?)
            .lines()
            .map(|line| {
                let p = PathBuf::from(line?);
                self.check_last_dst(&p)?;
                Ok::<_, Error>(p)
            })
            .collect::<Result<Vec<_>>>()
    }

    fn load_curr_srcs(&self) -> Result<Vec<PathBuf>> {
        todo!()
    }

    /// 比较当前所有的.root中的path与上次运行前保存的last_paths
    /// 找出不再需要的文件删除.root并返回删除的文件
    ///
    /// 如果在删除的过程中出错，会返回[DotRootRemovedError]
    fn remove_src_dsts<T, P>(&self, src_dsts: T) -> Result<Vec<(P, P)>>
    where
        T: IntoIterator<Item = (P, P)>,
        P: AsRef<Path> + Debug,
    {
        let (mut removeds, mut skippeds) = (vec![], vec![]);
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
            if removed {
                &mut removeds
            } else {
                &mut skippeds
            }
            .push((src, dst));
        }

        debug!(
            "removed a total of {} files: {:?}. and skipped {} files: {:?}",
            removeds.len(),
            removeds,
            skippeds.len(),
            skippeds,
        );
        Ok(removeds)
    }

    fn save_last_dsts<T>(&self, dsts: T) -> Result<()>
    where
        T: IntoIterator,
        T::Item: AsRef<Path>,
    {
        todo!()
    }

    /// 从src路径转换到对应的dst路径：
    /// src=/a/b,dst=/c/d/e: src_sub=a/b/1/2.t => dst_path=/c/d/e/1/2.t
    fn get_dst_path(&self, src_sub: impl AsRef<Path>) -> Result<PathBuf> {
        map_path(src_sub.as_ref(), &self.src, &self.dst)
    }

    fn get_src_path(&self, dst_sub: impl AsRef<Path>) -> Result<PathBuf> {
        map_path(dst_sub.as_ref(), &self.dst, &self.src)
    }

    fn has_changed<P: AsRef<Path>>(&self, new: P, old: P) -> Result<bool> {
        let mut hasher = Sha256::new();
        io::copy(&mut fs::File::open(new)?, &mut hasher)?;
        let hash_new = hasher.finalize();

        let mut hasher = Sha256::new();
        io::copy(&mut fs::File::open(old)?, &mut hasher)?;
        let hash_old = hasher.finalize();
        Ok(hash_new == hash_old)
    }

    fn copy_file<P: AsRef<Path>>(&self, from: P, to: P) -> Result<()> {
        info!(
            "Copying file {} -> {}",
            from.as_ref().display(),
            to.as_ref().display()
        );
        io::copy(
            &mut fs::File::open(from)?,
            &mut fs::File::options()
                .truncate(true)
                .create(true)
                .write(true)
                .open(to)?,
        )?;
        // TODO: rights
        Ok(())
    }

    /// 比较当前src与上次的dst路径找出不再存在当前src中的last的路径，
    /// 返回需要删除的last_dsts对应的src路径
    fn find_removable_src_dsts_iter<'a, P>(
        &self,
        curr_srcs: &'a [P],
        last_dsts: &'a [P],
    ) -> Result<impl Iterator<Item = (PathBuf, P)> + Debug + Clone + 'a>
    where
        P: AsRef<Path> + Clone + Debug,
    {
        if let Some(p) = curr_srcs
            .iter()
            .chain(last_dsts)
            .find(|p| !p.as_ref().is_absolute())
        {
            bail!("Found non absolute path {}", p.as_ref().display())
        }

        let curr_srcs = curr_srcs.iter().map(AsRef::as_ref).collect::<HashSet<_>>();
        trace!(
            "Converting {} last dst paths to last src paths",
            last_dsts.len()
        );
        last_dsts
            .iter()
            // last dst to last src
            .map(|dst| {
                self.get_src_path(dst.as_ref())
                    .map(|src| (src, dst.clone()))
            })
            .collect::<Result<Vec<_>, _>>()
            .map(move |paths| {
                paths
                    .into_iter()
                    .filter(move |(src, _)| !curr_srcs.contains(src.as_path()))
            })
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

fn map_path<P: AsRef<Path>>(src: P, src_prefix: P, dst_prefix: P) -> Result<PathBuf> {
    src.as_ref()
        .strip_prefix(&src_prefix)
        .map(|rel| dst_prefix.as_ref().join(rel))
        .with_context(|| {
            format!(
                "Failed to map src {} with prefix {} to {}",
                src.as_ref().display(),
                src_prefix.as_ref().display(),
                dst_prefix.as_ref().display()
            )
        })
}

/// 确认是否移除src与对应dst文件，移除成功则返回None，跳过则返回dst路径
///
/// 如果移除失败则返回
fn confirm_rm<P: AsRef<Path>>(src: P, dst: P, non_interactive: bool) -> Result<bool> {
    let (src, dst) = (src.as_ref(), dst.as_ref());
    // if !src.exists() ||  {
    //     trace!("ignore removing non exists src file {}", src.display());
    // }
    // if !dst.exists() {
    //     trace!("ignore removing non exists dst file {}", dst.display());
    // }
    if src.is_dir() || dst.is_dir() {
        bail!("Cannot remove a dir {} or {}", src.display(), dst.display());
    }
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
        info!("Removing file src={}, dst={}", src.display(), dst.display());
        if src.exists() {
            std::fs::remove_file(src)?;
        } else {
            trace!("ignore removing non exists src file {}", src.display());
        }
        if dst.exists() {
            std::fs::remove_file(dst)?;
        } else {
            trace!("ignore removing non exists dst file {}", dst.display());
        }
        Ok(true)
    } else {
        debug!(
            "Skipped remove file src={}, dst={}",
            src.display(),
            dst.display()
        );
        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Once;

    use super::*;
    use fake::{faker::lorem::zh_cn::Words, Fake, Faker};
    use log::LevelFilter;
    use pretty_assertions::assert_eq;
    use rstest::{fixture, rstest};
    use tempfile::tempdir;

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
        let tmpdir = Path::new("/test.root");
        let (src, dst) = (tmpdir.join(".root"), tmpdir.join("to"));
        DotRootBuilder::default()
            .non_interactive(true)
            .src(src)
            .dst(dst)
            .last_dsts_path(tmpdir.join("last_dsts"))
            .build()
            .unwrap()
    }

    #[fixture]
    fn tmp_dotroot() -> DotRoot {
        let tmpdir = tempdir().unwrap();
        let (src, dst) = (tmpdir.path().join(".root"), tmpdir.path().join("to"));
        DotRootBuilder::default()
            .non_interactive(true)
            .src(src)
            .dst(dst)
            .last_dsts_path(tmpdir.path().join("last_dsts"))
            .build()
            .unwrap()
    }

    #[test]
    fn test_map_path() -> Result<()> {
        assert_eq!(
            map_path("/a/b/c.txt", "/a", "/")?,
            Into::<PathBuf>::into("/b/c.txt")
        );
        assert_eq!(
            map_path("/a/b/c.txt", "/a", "/z")?,
            Into::<PathBuf>::into("/z/b/c.txt")
        );
        assert!(map_path("/a/b/c.txt", "/b", "/").is_err());
        assert!(map_path("/a/b/c.txt", "b", "/").is_err());
        Ok(())
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
    // 无扩展名的作为dir处理
    #[case::error_at_first(&["a", "b/c/2.txt", "a/b"], &[])]
    #[case::error_util_first_dir(&["a/1.txt", "b/c/2.txt", "a/b/c", "3.txt", "a/e"], &["a/1.txt", "b/c/2.txt"])]
    fn test_remove_src_dsts_error_when_exists_dirs<P>(
        tmp_dotroot: DotRoot,
        #[case] srcs: &[P],
        #[case] expects: &[P],
    ) -> Result<()>
    where
        P: AsRef<Path> + Debug,
    {
        let f = |p: &P| (tmp_dotroot.src.join(p), tmp_dotroot.dst.join(p));
        let src_dsts = srcs.iter().map(f).collect_vec();
        for (src, dst) in &src_dsts {
            if src.extension().is_some() {
                if let Some(p) = src.parent() {
                    fs::create_dir_all(p)?;
                }
                if let Some(p) = dst.parent() {
                    fs::create_dir_all(p)?;
                }
                fs::write(src, Words(3..10).fake::<Vec<String>>().join(" "))?;
                fs::write(dst, Words(3..10).fake::<Vec<String>>().join(" "))?;
            } else {
                fs::create_dir_all(src)?;
                fs::create_dir_all(dst)?;
            }
        }
        let res = tmp_dotroot.remove_src_dsts(src_dsts);
        debug!("remove result: {:?}", res);
        assert!(res.is_err());
        let e = res.err().unwrap();
        assert!(e.to_string().contains("Failed to remove src"));
        assert!(e.is::<DotRootRemovedError>());
        let re = e.downcast_ref::<DotRootRemovedError>().unwrap();
        assert_eq!(re.src_dsts, expects.iter().map(f).collect_vec());
        Ok(())
    }

    #[rstest]
    #[case("", &[""])]
    fn test_load_last_dsts(
        tmp_dotroot: DotRoot,
        #[case] content: &str,
        #[case] expects: &[&str],
    ) -> Result<()> {
        fs::write(&tmp_dotroot.last_dsts_path, content)?;
        assert_eq!(
            tmp_dotroot.load_last_dsts()?,
            expects.iter().map(PathBuf::from).collect_vec()
        );
        Ok(())
    }
}
