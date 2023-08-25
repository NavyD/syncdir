use std::{
    collections::HashSet,
    fmt::Display,
    fs,
    hash::Hash,
    io,
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Error, Result};
use dialoguer::{theme::ColorfulTheme, Confirm};
use itertools::Itertools;
use log::{debug, info, log_enabled};
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

pub struct DotRoot {
    dir: PathBuf,
    to: PathBuf,
    non_interactive: bool,
    last_path: PathBuf,
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
        WalkDir::new(&self.dir)
            .into_iter()
            .par_bridge()
            .try_for_each(|entry| {
                let e = entry?;
                let from = e.path();
                if from.is_file() {
                    let to = self.to_path(from)?;
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
        WalkDir::new(&self.dir)
            .into_iter()
            .par_bridge()
            .try_for_each(|entry| {
                let e = entry?;
                if e.path_is_symlink() || e.file_type().is_file() {
                    let from = e.path();
                    let to = self.to_path(e.path())?;
                    if !to.exists() || self.has_changed(from, &to)? {
                        self.copy_file(from, &to)?;
                    }
                }
                Ok::<_, Error>(())
            })?;
        Ok(())
    }

    pub fn clean(&self) -> Result<()> {
        todo!()
    }

    fn to_path(&self, path: impl AsRef<Path>) -> Result<PathBuf> {
        let path = path.as_ref();
        let rel = path.strip_prefix(&self.dir)?;
        Ok(self.to.join(rel))
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

    /// 比较当前所有的.root中的path与上次运行前保存的last_paths
    /// 找出不再需要的文件删除.root并返回删除的文件
    ///
    /// 如果在删除的过程中出错，会返回[DotRootRemovedError]
    fn remove_nonexists_from_last_in_targets<P>(
        &self,
        currs: &HashSet<P>,
        lasts: &HashSet<P>,
        targets: &[P],
    ) -> Result<Vec<P>>
    where
        P: AsRef<Path> + Eq + Hash + Clone,
    {
        if targets.is_empty() {
            bail!("Empty targets")
        }
        if let Some(p) = currs
            .iter()
            .chain(lasts)
            .chain(targets)
            .find(|p| !p.as_ref().is_absolute())
        {
            bail!("Found non absolute path in {}", p.as_ref().display())
        }
        let mut paths = currs - lasts;
        // find dirs in from_targets
        paths.retain(|p| targets.iter().any(|t| p.as_ref().starts_with(t)));
        if log_enabled!(log::Level::Info) {
            info!(
                "Found removable {} paths for targets {}",
                paths.len(),
                itertools::intersperse(
                    targets.iter().map(|p| p.as_ref().to_str().unwrap_or(":")),
                    ","
                )
                .collect::<String>()
            );
        }

        let (removeds, skippeds) = paths.into_iter().try_fold((vec![], vec![]), |mut v, p| {
            let removed = self.confirm_rm(p.as_ref()).with_context(|| {
                DotRootRemovedError(
                    v.0.iter()
                        .map(|e: &P| e.as_ref().to_path_buf())
                        .collect_vec(),
                )
            })?;
            if removed {
                v.0.push(p)
            } else {
                v.1.push(p)
            }
            Ok::<_, Error>(v)
        })?;
        info!(
            "Cleanup successfully removed a total of {} files and skipped {} files",
            removeds.len(),
            skippeds.len()
        );
        Ok(removeds)
    }

    fn confirm_rm<P: AsRef<Path>>(&self, p: P) -> Result<bool> {
        let p = p.as_ref();
        if self.non_interactive
            || Confirm::with_theme(&ColorfulTheme::default())
                .default(true)
                .show_default(true)
                .with_prompt(format!("Do you want to remove {}", p.display()))
                .interact()?
        {
            info!("Removing file {}", p.display());
            std::fs::remove_file(p)?;
            Ok(true)
        } else {
            debug!("Skipping remove file {}", p.display());
            Ok(false)
        }
    }
}

/// 表示移除文件错误包含已移除了的paths
struct DotRootRemovedError<T: AsRef<Path>>(Vec<T>);

impl<T> Display for DotRootRemovedError<T>
where
    T: AsRef<Path>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = itertools::intersperse(
            self.0.iter().map(|p| p.as_ref().to_str().unwrap_or(":")),
            ",",
        )
        .collect::<String>();
        write!(f, "{s}")
    }
}

#[cfg(test)]
mod tests {
    use tempfile::{tempdir, TempDir};

    use super::*;

    #[test]
    fn test_clean() -> Result<()> {
        let tmpdir = tempdir()?;
        let cur_paths = ["a/"].map(Path::new);
        let last_paths = [""].map(Path::new);
        // let dotroot = DotRoot::new(tmpdir.path().join("a"), tmpdir.path().join("b"))?;
        // let removable_paths = dotroot.clean(&cur_paths, &last_paths, &[])?;

        Ok(())
    }
}
