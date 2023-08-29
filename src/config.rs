use std::io::prelude::*;
use std::{
    fs::{self, File},
    io::{BufRead, BufReader, BufWriter},
    path::{Path, PathBuf},
};

use anyhow::Result;
use derive_builder::Builder;
use log::{debug, info, trace, warn};

#[derive(Debug, Clone, Builder)]
pub struct LastDestinationConfig {
    path: PathBuf,
    // strict: bool,
}

impl LastDestinationConfig {
    pub fn load_last_dsts(&self) -> Result<Vec<PathBuf>> {
        self.load_last_dsts_iter()
            .and_then(|it| it.collect::<Result<Vec<_>>>())
    }

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

    /// 解析指定文件内容 path 作为last_dsts返回
    ///
    /// 如果文件不存在会创建空文件则返回空的iter
    fn load_last_dsts_iter(&self) -> Result<impl Iterator<Item = Result<PathBuf>>> {
        let p = &self.path;
        info!("Loading last dsts from {}", p.display());

        if !p.exists() {
            info!("Creating new empty file {} for last_dsts", p.display());
            if let Some(pp) = p.parent() {
                fs::create_dir_all(pp)?
            }
            File::create(p)?;
        };

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
        Ok(it)
    }
}

#[cfg(test)]
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
        let conf = LastDestinationConfig {
            path: tmpdir.path().join("last-dsts"),
        };
        conf.save_last_dsts(paths)?;
        let dsts = conf.load_last_dsts()?;
        let expects = expects.iter().map(Path::new).collect::<Vec<_>>();
        assert_eq!(dsts.len(), expects.len());
        assert_eq!(dsts, expects);
        Ok(())
    }

    #[test]
    fn test_load_last_dsts_empty_when_non_exists_file() -> Result<()> {
        let tmpdir = tempdir()?;
        let conf = LastDestinationConfig {
            path: tmpdir.path().join("last-dsts"),
        };
        let dsts = conf.load_last_dsts()?;
        assert!(dsts.is_empty());
        Ok(())
    }
}
