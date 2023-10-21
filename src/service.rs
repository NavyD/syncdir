use std::io::prelude::*;
use std::{
    fs::{self, File},
    io::{BufRead, BufReader, BufWriter},
    path::{Path, PathBuf},
};

use anyhow::Result;
use derive_builder::Builder;
use faccess::PathExt;
use itertools::Itertools;
use log::{debug, info, trace, warn};
use os_display::Quotable;

/// last dsts持久化服务
#[derive(Debug, Clone, Builder)]
#[builder(setter(into), build_fn(validate = "Self::validate"))]
pub struct LastDestinationListService {
    path: PathBuf,
}

impl LastDestinationListServiceBuilder {
    fn validate(&self) -> Result<(), String> {
        if let Some(p) = &self.path.as_deref().filter(|p| p.exists()) {
            if !p.readable() {
                return Err(format!("Unreadable last dsts file {}", p.quote()));
            }
            if !p.writable() {
                return Err(format!("Unwritable last dsts file {}", p.quote()));
            }
        }
        Ok(())
    }
}

impl LastDestinationListService {
    /// 加载last dsts路径。如果不存在则返回空list
    #[deprecated]
    pub fn load_last_dsts(&self) -> Result<Vec<PathBuf>> {
        self.load_last_dsts_iter().and_then(|it| {
            it.map(|i| i.collect::<Result<Vec<_>>>())
                .unwrap_or_else(|| Ok(vec![]))
        })
    }

    /// 加载指定范围内的last dsts路径。如果不存在则返回None
    /// 如果target_dsts为空则返回所有dsts路径
    pub fn load_last_dsts_in_targets<T, P>(&self, target_dsts: T) -> Result<Option<Vec<PathBuf>>>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let target_dsts = target_dsts.into_iter().collect_vec();
        self.load_last_dsts_iter().and_then(|opt| {
            if let Some(res) = opt.map(|it| {
                if target_dsts.is_empty() {
                    it.collect::<Result<Vec<_>>>()
                } else {
                    it.filter(|res| {
                        res.as_ref()
                            .map(|p| target_dsts.iter().any(|t| p.starts_with(t)))
                            .unwrap_or(true)
                    })
                    .collect::<Result<Vec<_>>>()
                }
            }) {
                Ok(Some(res?))
            } else {
                Ok(None)
            }
        })
    }

    /// 保存所有dsts
    pub fn save_last_dsts<'a, T, P>(&self, dsts: T) -> Result<()>
    where
        T: IntoIterator<Item = &'a P>,
        P: AsRef<Path> + 'a,
    {
        let p = &self.path;
        debug!("Saving last dsts to {}", p.display());
        if !p.exists() {
            info!("Creating new file {} for save last_dsts", p.display());
            if let Some(pp) = p.parent() {
                fs::create_dir_all(pp)?
            }
        }
        let mut f = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(p)
            .map(BufWriter::new)?;
        for (i, dst) in dsts.into_iter().enumerate() {
            let dst = dst.as_ref();
            trace!("Writing {} line: {}", i, dst.display());
            writeln!(&mut f, "{}", dst.display())?;
        }
        Ok(())
    }

    /// 解析指定文件内容 path 作为last_dsts返回，如果文件不存在返回None
    fn load_last_dsts_iter(&self) -> Result<Option<impl Iterator<Item = Result<PathBuf>>>> {
        let p = &self.path;
        info!("Loading last dsts from {}", p.display());

        if p.exists() {
            let it = BufReader::new(File::open(p)?)
                .lines()
                .map(|res| {
                    res.map(|line| PathBuf::from(line.trim()))
                        .map_err(Into::into)
                })
                .filter(|res| {
                    res.as_deref()
                        .map(|p| {
                            if !p.is_absolute() {
                                warn!("Ignored non absolute path: {}", p.display());
                                return false;
                            }
                            true
                        })
                        .unwrap_or(true)
                });
            Ok(Some(it))
        } else {
            Ok(None)
        }
    }
}

#[cfg(all(test, unix))]
mod tests {

    use rstest::rstest;
    use tempfile::tempdir;

    use super::*;

    #[rstest]
    #[case::empty(&[] as &[&str], &[])]
    #[case::general(&["/a/b/c/1.txt", "/a/b"], &["/a/b/c/1.txt", "/a/b"])]
    #[case::ignore_relative_paths(&["/a/b/c/1.txt", "a/b", "/c"], &["/a/b/c/1.txt", "/c"])]
    fn test_load_last_dsts(#[case] paths: &[&str], #[case] expects: &[&str]) -> Result<()> {
        let tmpdir = tempdir()?;
        let conf = LastDestinationListService {
            path: tmpdir.path().join("last-dsts"),
        };
        conf.save_last_dsts(paths)?;
        let dsts = conf.load_last_dsts()?;
        let expects = expects.iter().map(Path::new).collect::<Vec<_>>();
        assert_eq!(dsts.len(), expects.len());
        assert_eq!(dsts, expects);
        Ok(())
    }

    #[rstest]
    #[case::empty(&["/a/b/c/1.txt", "/a/b"], &[])]
    #[case::general(&["/a/b/c/1.txt", "/a/b"], &["/"])]
    #[case::ignore_relative_paths(&["/a/b/c/1.txt", "a/b", "/c"], &["/c"])]
    fn test_load_last_dsts_in_targets(
        #[case] paths: &[&str],
        #[case] targets: &[&str],
    ) -> Result<()> {
        // let targets = targets.iter().map(|p| p.);
        let tmpdir = tempdir()?;
        let conf = LastDestinationListService {
            path: tmpdir.path().join("last-dsts"),
        };

        conf.save_last_dsts(paths)?;
        let dsts = conf.load_last_dsts_in_targets(targets)?;

        let f = |p: &Path| targets.is_empty() || targets.iter().any(|t| p.starts_with(t));
        assert!(dsts.is_some());
        let dsts = dsts.unwrap();
        assert!(dsts.iter().map(|p| p.as_path()).all(f));

        let expect = paths
            .iter()
            .map(Path::new)
            .filter(|p| f(p))
            .sorted()
            .collect_vec();
        let mut dsts = dsts;
        dsts.sort();
        assert_eq!(dsts, expect);
        Ok(())
    }

    #[test]
    fn test_load_last_dsts_empty_when_non_exists_file() -> Result<()> {
        let tmpdir = tempdir()?;
        let conf = LastDestinationListService {
            path: tmpdir.path().join("last-dsts"),
        };
        let dsts = conf.load_last_dsts()?;
        assert!(dsts.is_empty());

        let dsts = conf.load_last_dsts_in_targets(&[] as &[&str; 0])?;
        assert!(dsts.is_none());
        Ok(())
    }
}
