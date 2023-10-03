use std::{
    fs, io,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};

pub fn map_path<P, Q, R>(src: P, src_base: Q, dst_base: R) -> Result<PathBuf>
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
    R: AsRef<Path>,
{
    let (src, src_base, dst_base) = (src.as_ref(), src_base.as_ref(), dst_base.as_ref());
    src.strip_prefix(src_base)
        .map(|rel| dst_base.join(rel))
        .with_context(|| {
            format!(
                "Failed to map src {} with base {} to dst base {}",
                src.display(),
                src_base.display(),
                dst_base.display()
            )
        })
}

pub fn symlink<P, Q>(from: P, to: Q) -> Result<()>
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    #[cfg(unix)]
    {
        use std::os::unix::fs;
        fs::symlink(from, to)?;
    }

    #[cfg(windows)]
    {
        use std::os::windows::fs;
        let from = from.as_ref();
        if from.is_dir() {
            fs::symlink_dir(from, to)?;
        } else {
            fs::symlink_file(from, to)?;
        }
    }
    Ok(())
}

pub fn remove(p: impl AsRef<Path>) -> io::Result<()> {
    let p = p.as_ref();
    if p.is_symlink() || p.is_file() {
        fs::remove_file(p)
    } else {
        fs::remove_dir_all(p)
    }
}

/// 直接移除路径p如果不存在则不会返回err
pub fn try_remove(p: impl AsRef<Path>) -> io::Result<()> {
    if let Err(e) = remove(p) {
        match e.kind() {
            io::ErrorKind::NotFound => {}
            _ => return io::Result::Err(e),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
