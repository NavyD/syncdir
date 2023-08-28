use std::path::{Path, PathBuf};

use anyhow::{Context, Result};

pub fn map_path<P: AsRef<Path>>(src: P, src_prefix: P, dst_prefix: P) -> Result<PathBuf> {
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
