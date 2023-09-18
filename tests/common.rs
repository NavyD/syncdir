use std::{
    fs, io,
    os::unix,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{anyhow, bail, Error, Result};
use filetime::FileTime;
use itertools::Itertools;
use pretty_assertions::assert_eq;
use syncdir::{
    service::{LastDestinationListService, LastDestinationListServiceBuilder},
    sync::{Copier, CopierBuilder},
};
use tempfile::{NamedTempFile, TempDir};
use walkdir::WalkDir;

pub struct TestEnv {
    last_dsts_holder: Option<(NamedTempFile, LastDestinationListService)>,

    dst: Option<TempDir>,
    src: TempDir,
    copier: Copier,
}

impl TestEnv {
    pub fn new<T, I, P>(rel_srcs: T) -> Self
    where
        T: IntoIterator<Item = I>,
        I: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        Self {
            src: create_tree(rel_srcs, Some("syncdir-src-")).expect("create dir tree"),
            last_dsts_holder: None,
            dst: None,
            copier: CopierBuilder::default().build().unwrap(),
        }
    }

    /// 将修改rel_mod_srcs中对应的src路径文件的内容等
    pub fn with_mod_srcs<F>(self, mod_fn: F) -> Self
    where
        F: Fn(&Path) -> Result<()>,
    {
        WalkDir::new(self.src.path())
            .into_iter()
            .try_for_each(|entry| mod_fn(&entry?.into_path()))
            .unwrap();
        self
    }

    pub fn with_dsts<T, I, P>(mut self, rel_dsts: T) -> Self
    where
        T: IntoIterator<Item = I>,
        I: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let dst = create_tree(rel_dsts, Some("syncdir-dst-")).unwrap();
        self.dst = Some(dst);
        self
    }

    /// 使用cp用于比较src与被复制后的dst目录内容，
    /// 如果未指定则会使用默认的cp
    pub fn with_copier(mut self, copier: Copier) -> Self {
        self.copier = copier;
        self
    }

    pub fn with_last_dsts<T, P>(mut self, rel_last_dsts: T) -> Self
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let tmp_last_dst = tempfile::Builder::new()
            .prefix("syncdir-last-dsts-")
            .tempfile()
            .unwrap();
        let last_dsts_srv = LastDestinationListServiceBuilder::default()
            .path(tmp_last_dst.path().to_path_buf())
            .build()
            .unwrap();

        let paths = rel_last_dsts
            .into_iter()
            .map(|p| {
                let p = p.as_ref();
                if p.is_absolute() {
                    bail!("Found absolute path {}", p.display());
                }
                self.dst
                    .as_ref()
                    .ok_or_else(|| anyhow!("Not found dst for last dsts"))
                    .map(|dst| dst.path().join(p))
            })
            .collect::<Result<Vec<_>>>()
            .unwrap();

        last_dsts_srv.save_last_dsts(&paths).unwrap();
        self.last_dsts_holder = Some((tmp_last_dst, last_dsts_srv));
        self
    }

    pub fn src_root(&self) -> &Path {
        self.src.path()
    }

    pub fn dst_root(&self) -> &Path {
        self.dst.as_ref().unwrap().path()
    }

    pub fn copier(&self) -> &Copier {
        &self.copier
    }

    pub fn last_dsts_srv(&self) -> Option<&LastDestinationListService> {
        self.last_dsts_holder.as_ref().map(|v| &v.1)
    }

    pub fn assert_synced<T, P>(&self, rel_target_dsts: T)
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let (src, dst) = (self.src_root(), self.dst_root());
        let target_dsts = rel_target_dsts.into_iter().collect_vec();
        if target_dsts.is_empty() {
            self.assert_same_dir(src, dst);
        } else {
            for relp in target_dsts {
                let (src, dst) = (src.join(&relp), dst.join(&relp));
                if dst.exists() {
                    self.assert_same_dir(src, dst);
                }
            }
        }
    }

    pub fn assert_same_dir<P, Q>(&self, src: P, dst: Q)
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

        assert_eq!(
            srcs.iter()
                .filter(|p| p.is_symlink())
                .flat_map(|p| p.strip_prefix(src))
                .collect_vec(),
            dsts.iter()
                .filter(|p| p.is_symlink())
                .flat_map(|p| p.strip_prefix(dst))
                .collect_vec(),
            "diff symlinks in src {}, dst {}",
            src.display(),
            dst.display(),
        );
        assert_eq!(
            srcs.iter()
                .filter(|p| p.is_dir())
                .flat_map(|p| p.strip_prefix(src))
                .collect_vec(),
            dsts.iter()
                .filter(|p| p.is_dir())
                .flat_map(|p| p.strip_prefix(dst))
                .collect_vec(),
            "diff dirs in src {}, dst {}",
            src.display(),
            dst.display(),
        );
        assert_eq!(
            srcs.iter()
                .filter(|p| p.is_file())
                .flat_map(|p| p.strip_prefix(src))
                .collect_vec(),
            dsts.iter()
                .filter(|p| p.is_file())
                .flat_map(|p| p.strip_prefix(dst))
                .collect_vec(),
            "diff files in src {}, dst {}",
            src.display(),
            dst.display(),
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

        for relp in rel_srcs {
            self.assert_same_path(src.join(relp), dst.join(relp));
        }
    }

    pub fn assert_same_path<P, Q>(&self, src: P, dst: Q)
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        let (src, dst) = (src.as_ref(), dst.as_ref());

        if src.is_symlink() {
            assert!(dst.is_symlink());
            self.assert_filetimes(src, dst);
            let read_link_end = |p: &Path| {
                let mut p = p.to_path_buf();
                while p.is_symlink() {
                    p = p.read_link().unwrap();
                }
                p
            };
            assert_eq!(
                read_link_end(src),
                read_link_end(dst),
                "read symlink src={}, dst={}",
                src.display(),
                dst.display()
            );
        } else {
            assert!(!dst.is_symlink());
            assert!(src.exists());
            assert!(dst.exists());
            self.assert_filetimes(src, dst);
            let (src_meta, dst_meta) = (src.metadata().unwrap(), dst.metadata().unwrap());
            assert_eq!(src_meta.file_type(), dst_meta.file_type());
            assert_eq!(src_meta.len(), dst_meta.len());
            if src_meta.is_file() {
                assert_eq!(fs::read(src).unwrap(), fs::read(dst).unwrap());
            }
        }

        // assert perms
        let (src_meta, dst_meta) = (metadata(src).unwrap(), metadata(dst).unwrap());
        if let Some(mode) = self.copier.mode() {
            use std::os::unix::fs::PermissionsExt;
            let mut src_perm = src_meta.permissions();
            src_perm.set_mode(mode);
            assert_eq!(src_perm, dst_meta.permissions())
        } else {
            // 默认复制权限
            assert_eq!(src_meta.permissions(), dst_meta.permissions());
        }
    }

    /// WARN: 应该在读取文件前调用否则会更新last_accessed time导致测试失败
    fn assert_filetimes<P, Q>(&self, src: P, dst: Q)
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        if self.copier.filetimes() {
            let (src, dst) = (src.as_ref(), dst.as_ref());

            let get_am_times = |p: &Path| {
                let meta = metadata(p).unwrap();
                (
                    Into::<FileTime>::into(meta.accessed().unwrap()),
                    Into::<FileTime>::into(meta.modified().unwrap()),
                )
            };
            let (src_atime, src_mtime) = get_am_times(src);
            let (dst_atime, dst_mtime) = get_am_times(dst);
            // 注意：由于copy会访问src文件导致src atime被修改，而目录dst在添加文件后mtime被修改
            assert_eq!(
                dst_mtime,
                src_mtime,
                "mtime for src={},dst={}",
                src.display(),
                dst.display()
            );
            // WARN: 由于assert会访问目录导致access时间被更新，使用误差时间
            let diff = src_atime.nanoseconds() as i128 - dst_atime.nanoseconds() as i128;
            let limit = Duration::from_millis(100);
            let r = limit.as_nanos() as i128;
            assert!(
                (-r..=r).contains(&diff),
                "atime diff more than {}ms for src={}: {:?},dst={}: {:?}",
                limit.as_millis(),
                src.display(),
                src_atime,
                dst.display(),
                dst_atime,
            );
        }
    }
}

/// 创建一个目录树
///
/// * 使用嵌入的paths表示对路径`paths[0]`进行多级链接
/// * `paths[0]`通过是否存在后缀，区分创建的是dir还是file
/// * 如果`paths[0].ext == N`则表示不会创建这个路径，用于表示错误的symlink
/// * 所有的paths必须是相对路径
pub fn create_tree<T, I, P>(rel_paths: T, prefix: Option<&str>) -> Result<TempDir>
where
    T: IntoIterator<Item = I>,
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    let root = {
        let mut b = tempfile::Builder::new();
        if let Some(prefix) = prefix {
            b.prefix(prefix);
        }
        b.tempdir()?
    };

    let create_dir_or_file = |p: &Path| {
        if let Some(pp) = p.parent() {
            fs::create_dir_all(pp)?;
        }
        if let Some(ext) = p.extension() {
            if ext.to_str() == Some("N") {
                return Ok(());
            }
            fs::write(p, p.display().to_string())?;
        } else {
            fs::create_dir(p)?;
        }
        Ok::<_, Error>(())
    };

    for paths in rel_paths {
        let paths: Vec<PathBuf> = paths
            .into_iter()
            .map(|p| {
                let p: &Path = p.as_ref();
                if p.is_absolute() {
                    Err(anyhow!(
                        "Found absolute path {} for create tree",
                        p.display()
                    ))
                } else {
                    Ok(root.path().join(p))
                }
            })
            .collect::<Result<Vec<_>>>()?;
        if paths.is_empty() {
            bail!("Found empty nest paths")
        }

        let p = &paths[0];
        create_dir_or_file(p)?;

        let mut from = p;
        // create multi link for p
        for to in &paths[1..] {
            if to.is_symlink() || to.is_file() {
                fs::remove_file(to)?;
            } else if to.is_dir() {
                fs::remove_dir(to)?;
            } else if let Some(pp) = to.parent() {
                fs::create_dir_all(pp)?;
            }
            unix::fs::symlink(from, to)?;

            from = to;
        }
    }
    Ok(root)
}

fn metadata(p: impl AsRef<Path>) -> io::Result<fs::Metadata> {
    let p = p.as_ref();
    if p.is_symlink() {
        p.symlink_metadata()
    } else {
        p.metadata()
    }
}
