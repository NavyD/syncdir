use std::path::{Path, PathBuf};

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
