use std::{fs, os::unix, path::Path};

use anyhow::{bail, Result};
use tempfile::TempDir;
use walkdir::WalkDir;

pub struct TestEnv {
    /// Temporary working directory.
    temp_dir: TempDir,
}

impl TestEnv {
    pub fn new<T, P>(paths: T) -> Self
    where
        P: AsRef<Path>,
        T: IntoIterator<Item = P>,
    {
        let temp_dir = create_tree(paths, false).expect("create dir tree");
        Self { temp_dir }
    }

    pub fn root(&self) -> &Path {
        self.temp_dir.path()
    }
}

pub fn assert_same_path<P, Q>(src: P, dst: Q)
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    let (src, dst) = (src.as_ref(), dst.as_ref());
    if src.is_symlink() {
        assert!(dst.is_symlink());
        let read_link_end = |p: &Path| {
            let mut p = p.to_path_buf();
            while p.is_symlink() {
                p = p.read_link().unwrap();
            }
            p
        };
        assert_eq!(read_link_end(src), read_link_end(dst));
    } else {
        assert!(!dst.is_symlink());
        assert!(src.exists());
        assert!(dst.exists());
        let (src_meta, dst_meta) = (src.metadata().unwrap(), dst.metadata().unwrap());
        assert_eq!(src_meta.file_type(), dst_meta.file_type());
        assert_eq!(src_meta.len(), dst_meta.len());
        if src_meta.is_file() {
            assert_eq!(fs::read(src).unwrap(), fs::read(dst).unwrap());
        }
    }
}

pub fn assert_same_dir<P, Q>(src: P, dst: Q)
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
        assert_same_path(&srcs[i], &dsts[i]);
    }
}

fn create_tree<P, T>(rel_paths: T, bad_symlink: bool) -> Result<TempDir>
where
    T: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    let tmpdir = tempfile::Builder::new().prefix("syncdir-tests").tempdir()?;
    let root = tmpdir.path();
    let dir = root.join("tree");
    let sym_dir = root.join("symlinks");
    let bad_sym_src_dir = root.join("bad-symlinks-src");
    let bad_sym_dst_dir = root.join("bad-symlinks-dst");

    for relp in rel_paths {
        let relp = relp.as_ref();
        if relp.is_absolute() {
            bail!(
                "Absolute path {} not allowed for create temp tree",
                relp.display()
            );
        }

        let p = dir.join(relp);
        if let Some(pp) = p.parent() {
            fs::create_dir_all(pp)?;
        }
        if p.extension().is_some() {
            fs::write(&p, p.display().to_string())?;
        } else {
            fs::create_dir(&p)?;
        }

        let dst = sym_dir.join(relp);
        if let Some(pp) = dst.parent() {
            fs::create_dir_all(pp)?;
        }
        if !p.exists() {
            bail!("Not found exists src path {} for symlink", p.display());
        }
        unix::fs::symlink(p, dst)?;

        if bad_symlink {
            let dst = bad_sym_dst_dir.join(relp);
            if let Some(pp) = dst.parent() {
                fs::create_dir_all(pp)?;
            }
            let src = bad_sym_src_dir.join(relp);
            if src.is_symlink() || src.exists() {
                bail!("Found exists src path {} for bad symlink", src.display());
            }
            unix::fs::symlink(src, dst)?;
        }
    }

    Ok(tmpdir)
}
