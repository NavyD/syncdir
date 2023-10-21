use std::{
    collections::HashSet,
    fmt::Debug,
    fs, io,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{bail, Context, Error, Result};
use filetime::FileTime;
use itertools::Itertools;
use os_display::Quotable;
use pretty_assertions::assert_eq;
use sha2::{Digest, Sha256};
use syncdir::{
    cp::{Copier, CopierBuilder},
    service::{LastDestinationListService, LastDestinationListServiceBuilder},
};
use tempfile::{NamedTempFile, TempDir};
use walkdir::WalkDir;

pub struct TestEnv {
    _last_dsts_holder: NamedTempFile,
    last_dsts_srv: LastDestinationListService,
    last_dsts: Vec<PathBuf>,

    dst: Option<TempDir>,
    dst_paths: Vec<TreePath<PathBuf>>,
    src: TempDir,
    src_paths: Vec<TreePath<PathBuf>>,
    copier: Copier,
    workdir: Option<PathBuf>,
}

impl TestEnv {
    pub fn new<T, I, P>(rel_paths: T) -> Self
    where
        T: IntoIterator<Item = I>,
        I: TryInto<TreePath<P>, Error = Error>,
        P: AsRef<Path>,
    {
        let last_dsts_holder = tempfile::Builder::new()
            .prefix("syncdir-last-dsts-")
            .tempfile()
            .unwrap();
        let (src, src_paths) = create_tree_paths(rel_paths, Some("syncdir-src-")).unwrap();
        Self {
            src,
            last_dsts_srv: LastDestinationListServiceBuilder::default()
                .path(last_dsts_holder.path().to_path_buf())
                .build()
                .unwrap(),
            _last_dsts_holder: last_dsts_holder,
            dst: None,
            copier: CopierBuilder::default().build().unwrap(),
            workdir: None,
            src_paths,
            dst_paths: vec![],
            last_dsts: vec![],
        }
    }

    #[allow(dead_code)]
    pub fn with_workdir<P: AsRef<Path>>(mut self, p: P) -> Self {
        let p = p.as_ref().to_path_buf();
        std::env::set_current_dir(&p).unwrap();
        self.workdir = Some(p);
        self
    }

    /// 将修改rel_mod_srcs中对应的src路径文件的内容等
    #[allow(dead_code)]
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
        I: TryInto<TreePath<P>, Error = Error>,
        P: AsRef<Path>,
    {
        let (dst, dst_paths) = create_tree_paths(rel_dsts, Some("syncdir-dst-")).unwrap();
        self.dst = Some(dst);
        self.dst_paths = dst_paths;
        self
    }

    /// 使用cp用于比较src与被复制后的dst目录内容，
    /// 如果未指定则会使用默认的cp
    pub fn with_copier(mut self, copier: Copier) -> Self {
        self.copier = copier;
        self
    }

    pub fn with_last_dsts_paths<T, I, P>(self, rel_last_dsts: T) -> Self
    where
        T: IntoIterator<Item = I>,
        I: TryInto<TreePath<P>, Error = Error>,
        P: AsRef<Path>,
    {
        let paths = rel_last_dsts
            .into_iter()
            .map(|tp| TryInto::<TreePath<P>>::try_into(tp).unwrap())
            .flat_map(|tp| {
                tp.created_paths()
                    .iter()
                    .map(|p| p.as_ref().to_path_buf())
                    .collect_vec()
            });
        self.with_last_dsts(paths)
    }

    pub fn with_last_dsts<T, P>(mut self, rel_last_dsts: T) -> Self
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let paths = rel_last_dsts
            .into_iter()
            .map(|p| {
                let p = p.as_ref();
                assert!(!p.is_absolute(), "Found absolute path {}", p.display());
                self.dst_root().join(p)
            })
            .collect_vec();

        self.last_dsts_srv.save_last_dsts(&paths).unwrap();
        self.last_dsts = paths;
        self
    }

    pub fn src_root(&self) -> &Path {
        let p = self.src.path();
        self.workdir
            .as_deref()
            .and_then(|d| p.strip_prefix(d).ok())
            .unwrap_or(p)
    }

    pub fn dst_root(&self) -> &Path {
        let p = self.dst.as_ref().unwrap().path();
        self.workdir
            .as_deref()
            .and_then(|d| p.strip_prefix(d).ok())
            .unwrap_or(p)
    }

    pub fn copier(&self) -> &Copier {
        &self.copier
    }

    pub fn last_dsts_srv(&self) -> &LastDestinationListService {
        &self.last_dsts_srv
    }

    pub fn get_rel_target_dsts<T, P>(&self, rel_target_dsts: T) -> Vec<PathBuf>
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let mut v = rel_target_dsts
            .into_iter()
            .map(|p| p.as_ref().to_path_buf())
            .collect_vec();
        if v.is_empty() {
            v.push(PathBuf::from(""));
        }
        v
    }

    /// 测试 [syncdir::sync::Syncer::sync_back]
    ///
    /// 思路：
    ///
    /// * 如果last dsts存在
    ///     * 所有last dsts对应的src应该与dst一致
    ///     * 非last dsts对应的src都不同（仅在测试中，实际是可能一样的）
    ///
    /// * 如果last dsts不存在
    ///     * 检查同步后dst目录的路径与src是否一致
    ///     * 检查同步后非dst的目录对应的src目录都应不存在
    pub fn assert_sync_back<T, P>(&self, rel_target_dsts: T)
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let (src, dst) = (self.src_root(), self.dst_root());

        let rel_target_dsts = self.get_rel_target_dsts(rel_target_dsts);
        let target_dsts = rel_target_dsts.iter().map(|p| dst.join(p)).collect_vec();

        let last_dsts_in_t = self
            .last_dsts
            .iter()
            .filter(|p| target_dsts.iter().any(|t| p.starts_with(t)))
            .collect_vec();
        if !last_dsts_in_t.is_empty() {
            let rel_last_dsts = last_dsts_in_t
                .iter()
                .flat_map(|p| p.strip_prefix(dst).map(|p| src.join(p)))
                .sorted()
                .collect_vec();
            // 检查在last dst中的目录对应的src目录一致
            for p in &rel_last_dsts {
                let (src, dst) = (src.join(p), dst.join(p));
                self.assert_same_path(src, dst);
            }
            let dsts_not_in_t = self
                .dst_paths
                .iter()
                .flat_map(|tp| tp.created_ancestors())
                .filter(|p| rel_target_dsts.iter().all(|t| !p.starts_with(t)))
                .collect_vec();
            let srcs_not_in_t = self
                .src_paths
                .iter()
                .flat_map(|tp| tp.created_ancestors())
                .filter(|p| rel_target_dsts.iter().all(|t| !p.starts_with(t)))
                .collect::<HashSet<_>>();
            // TODO: 检查非last dsts中的路径 暂时未有好的方法检查，仅检查last dst外的对应src是否存在
            for p in &dsts_not_in_t {
                let (src, dst) = (src.join(p), dst.join(p));
                assert!(dst.symlink_metadata().is_ok());
                if srcs_not_in_t.contains(p) {
                    assert!(src.symlink_metadata().is_ok());
                } else {
                    assert!(src.symlink_metadata().is_err());
                }
            }
        } else {
            let dsts = self
                .dst_paths
                .iter()
                .flat_map(|tp| tp.created_ancestors())
                .filter(|p| rel_target_dsts.iter().any(|t| p.starts_with(t)))
                .collect::<HashSet<_>>();
            // 检查同步后dst目录的路径与src是否一致
            for p in &dsts {
                self.assert_same_path(src.join(p), dst.join(p));
            }
            let srcs = self
                .src_paths
                .iter()
                .flat_map(|tp| tp.created_ancestors())
                .filter(|p| rel_target_dsts.iter().any(|t| p.starts_with(t)))
                .collect::<HashSet<_>>();
            // 检查同步后非dst的目录对应的src目录都应不存在
            for p in &srcs - &dsts {
                let (subsrc, subdst) = (src.join(p), dst.join(p));
                assert!(
                    subdst.symlink_metadata().is_err(),
                    "expect non exists dst {}",
                    subdst.display()
                );
                let subsrc_md = subsrc.symlink_metadata();
                assert!(
                    subsrc_md.is_err(),
                    "expect non exists src {}: {:?}",
                    subsrc.display(),
                    subsrc_md,
                );
            }
        }
    }

    /// 测试 apply to dst
    ///
    /// 思路：
    ///
    /// 对于指定每个target dst与对应的src目录应该是完全一样的
    pub fn assert_applied<T, P>(&self, rel_target_dsts: T)
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        let (src, dst) = (self.src_root(), self.dst_root());
        let rel_target_dsts = rel_target_dsts.into_iter().collect_vec();
        if rel_target_dsts.is_empty() {
            self.assert_same_dir(src, dst);
        } else {
            for p in rel_target_dsts {
                let (src, dst) = (src.join(&p), dst.join(&p));
                self.assert_same_dir(src, dst);
            }
        }
    }

    pub fn assert_clean_dst<T, U, P, Q>(&self, rel_target_dsts: T, cleaned_dsts: U)
    where
        T: IntoIterator<Item = P>,
        P: AsRef<Path>,
        U: IntoIterator<Item = Q>,
        Q: AsRef<Path>,
    {
        let rel_target_dsts = self.get_rel_target_dsts(rel_target_dsts);
        let srcs = self
            .src_paths
            .iter()
            .flat_map(|tp| tp.created_ancestors())
            .collect::<HashSet<_>>();
        let dsts = self
            .dst_paths
            .iter()
            .flat_map(|tp| tp.created_ancestors())
            .collect::<HashSet<_>>();
        let expect_cleaneds = dsts
            .iter()
            // 不包含在当前的src中
            .filter(|p| !srcs.contains(*p))
            // 不包含在非targets中
            .filter(|p| rel_target_dsts.iter().any(|t| p.starts_with(t)))
            // 绝对路径
            .map(|s| self.dst_root().join(s))
            .sorted()
            .collect_vec();
        let cleaned_dsts: Vec<PathBuf> = cleaned_dsts
            .into_iter()
            .map(|p| p.as_ref().to_path_buf())
            .sorted()
            .collect_vec();
        for p in &cleaned_dsts {
            assert!(!p.exists() && !p.is_symlink());
        }
        assert_eq!(expect_cleaneds, cleaned_dsts);
        dsts.iter()
            .map(|p| self.dst_root().join(p))
            .filter(|p| !expect_cleaneds.contains(p))
            .for_each(|p| assert!(p.exists() || p.is_symlink()));
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
                "diff symlink src={}, dst={}",
                src.display(),
                dst.display(),
            );
        } else if src.exists() {
            assert!(!dst.is_symlink());
            assert!(src.exists(), "expect exists {}", src.display());
            assert!(dst.exists());
            self.assert_filetimes(src, dst);
            let (src_meta, dst_meta) = (src.metadata().unwrap(), dst.metadata().unwrap());
            assert_eq!(src_meta.file_type(), dst_meta.file_type());
            assert_eq!(src_meta.len(), dst_meta.len());
            if src_meta.is_file() {
                assert_eq!(
                    calc_digest(src).unwrap(),
                    calc_digest(dst).unwrap(),
                    "diff digest src {} and dst {}",
                    src.display(),
                    dst.display()
                );
            }
        } else {
            assert!(
                !dst.is_symlink() && !dst.exists(),
                "expect non exists dst {} and src {}",
                dst.quote(),
                src.quote()
            );
            return;
        }

        // assert perms
        #[cfg(unix)]
        {
            let (src_meta, dst_meta) = (metadata(src).unwrap(), metadata(dst).unwrap());
            if let Some(mode) = self.copier.path_attrs().as_ref().and_then(|a| {
                a.iter()
                    .find(|(m, _)| m.is_match(src))
                    .and_then(|(_, a)| a.mode)
            }) {
                use std::os::unix::fs::PermissionsExt;
                let mut src_perm = src_meta.permissions();
                src_perm.set_mode(mode);
                assert_eq!(src_perm, dst_meta.permissions())
            } else if self.copier.attrs().mode {
                assert_eq!(src_meta.permissions(), dst_meta.permissions());
            }
        }
    }

    /// WARN: 应该在读取文件前调用否则会更新last_accessed time导致测试失败
    fn assert_filetimes<P, Q>(&self, src: P, dst: Q)
    where
        P: AsRef<Path>,
        Q: AsRef<Path>,
    {
        if self.copier.attrs().timestamps {
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
            // FIXME: 由于copy会访问src文件导致src atime被修改，而目录dst在添加文件后mtime被修改
            assert_eq!(
                dst_mtime.seconds(),
                src_mtime.seconds(),
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

/// 表示一个相对路径的文件或目录及存在多个关联的相对路径软链接
/// * 使用嵌入的paths表示对路径`paths[0]`进行多级链接
/// * `paths[0]`通过是否存在后缀`/`，区分创建的是dir还是file
/// * 如果`paths[0].ext == N`则表示不会创建这个路径，用于表示错误的symlink
/// * 所有的paths必须是相对路径
#[derive(Clone)]
pub struct TreePath<T: AsRef<Path>>(Vec<T>);

impl<T: AsRef<Path>> Debug for TreePath<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TreePath")
            .field(&self.0.iter().map(|p| p.as_ref()).collect_vec())
            .finish()
    }
}

impl<T: AsRef<Path>> TreePath<T> {
    const BAD_SYM_EXT: &str = "N";

    pub fn new<I>(paths: I) -> Result<Self>
    where
        I: IntoIterator<Item = T>,
    {
        let value = paths.into_iter().collect_vec();
        if value.is_empty() {
            bail!("Empty array")
        }
        let p = value[0].as_ref();
        if p.is_absolute() {
            bail!("absolute path: {}", p.display())
        }
        if Self::is_bad_sym_path(p) && value.len() == 1 {
            bail!("invalid bad sym paths")
        }
        // check repeat
        for i in 0..value.len() {
            for j in 0..value.len() {
                if i != j && value[i].as_ref() == value[j].as_ref() {
                    bail!(
                        "Repeated path {} in index {},{}",
                        value[i].as_ref().display(),
                        i,
                        j
                    );
                }
            }
        }
        Ok(Self(value))
    }

    fn is_bad_sym_path(p: impl AsRef<Path>) -> bool {
        p.as_ref()
            .extension()
            .and_then(|s| s.to_str())
            .map(|s| s == Self::BAD_SYM_EXT)
            .unwrap_or_default()
    }

    pub fn path(&self) -> &T {
        &self.0[0]
    }

    pub fn is_dir(&self) -> bool {
        self.path()
            .as_ref()
            .to_str()
            .and_then(|s| s.as_bytes().last())
            .map(|b| *b as char == '/')
            .unwrap_or_default()
    }

    pub fn is_file(&self) -> bool {
        !self.is_dir()
    }

    pub fn is_bad_sym(&self) -> bool {
        Self::is_bad_sym_path(self.path())
    }

    pub fn links(&self) -> &[T] {
        &self.0[1..]
    }

    pub fn created_paths(&self) -> &[T] {
        if self.is_bad_sym() {
            self.links()
        } else {
            &self.0
        }
    }

    pub fn created_ancestors(&self) -> impl Iterator<Item = &Path> {
        self.created_paths()
            .iter()
            .flat_map(|p| p.as_ref().ancestors())
            .unique()
    }

    fn create(&self, base: impl AsRef<Path>) -> Result<()> {
        let p = base.as_ref().join(self.path());
        if !self.is_bad_sym() {
            if let Some(pp) = p.parent() {
                fs::create_dir_all(pp).with_context(|| {
                    format!(
                        "create all dir {} type: {:?}",
                        pp.display(),
                        pp.symlink_metadata().map(|m| m.file_type())
                    )
                })?;
            }
            if self.is_file() {
                fs::write(&p, p.display().to_string())?;
            } else {
                fs::create_dir(&p)?;
            }
        }

        let mut from = p;
        // create multi link for p
        for p in self.links() {
            let to = base.as_ref().join(p);
            if to.is_symlink() || to.is_file() {
                fs::remove_file(&to)?;
            } else if to.is_dir() {
                fs::remove_dir(&to)?;
            } else if let Some(pp) = to.parent() {
                fs::create_dir_all(pp)?;
            }
            #[cfg(unix)]
            std::os::unix::fs::symlink(from, &to)?;

            #[cfg(windows)]
            {
                use std::os::windows::fs;
                if from.is_dir() {
                    fs::symlink_dir(from, &to)?;
                } else {
                    fs::symlink_file(from, &to)?;
                }
            }
            from = to;
        }
        Ok(())
    }
}

impl<'a> TryFrom<&'a str> for TreePath<&'a str> {
    type Error = Error;

    fn try_from(value: &'a str) -> std::result::Result<Self, Self::Error> {
        Self::new([value])
    }
}

impl<'a, T: AsRef<Path> + Clone> TryFrom<&'a [T]> for TreePath<T> {
    type Error = Error;

    fn try_from(value: &'a [T]) -> std::result::Result<Self, Self::Error> {
        Self::new(value.iter().cloned())
    }
}

impl<'a, T> TryFrom<&&'a [T]> for TreePath<T>
where
    T: AsRef<Path> + Clone,
{
    type Error = Error;

    fn try_from(value: &&'a [T]) -> std::result::Result<Self, Self::Error> {
        Self::new(value.iter().cloned())
    }
}

impl<T: AsRef<Path>> TryFrom<Vec<T>> for TreePath<T> {
    type Error = Error;

    fn try_from(value: Vec<T>) -> std::result::Result<Self, Self::Error> {
        Self::new(value)
    }
}

/// 通过 [TreePath] 创建一个目录树
pub fn create_tree_paths<T, I, P>(
    rel_paths: T,
    prefix: Option<&str>,
) -> Result<(TempDir, Vec<TreePath<PathBuf>>)>
where
    T: IntoIterator<Item = I>,
    I: TryInto<TreePath<P>, Error = Error>,
    P: AsRef<Path>,
{
    let root = {
        let mut b = tempfile::Builder::new();
        if let Some(prefix) = prefix {
            b.prefix(prefix);
        }
        b.tempdir()?
    };

    let base = root.path();
    let tree_paths = rel_paths
        .into_iter()
        .map(|i| {
            let tp: TreePath<P> = i.try_into()?;
            tp.create(base).with_context(|| {
                format!(
                    "create tree path {:?} failure in base {}",
                    tp,
                    base.display()
                )
            })?;
            TreePath::new(
                tp.0.into_iter()
                    .map(|p| p.as_ref().to_path_buf())
                    .collect_vec(),
            )
        })
        .collect::<Result<Vec<_>>>()?;

    // 检查实际创建路径
    let cur_paths = WalkDir::new(base)
        .into_iter()
        .flat_map(|res| res.map(|e| e.into_path()))
        .sorted()
        .collect_vec();
    let expect_paths = tree_paths
        .iter()
        .flat_map(|tp| tp.created_paths())
        // parent dirs of file path. contains ""
        .flat_map(|p| p.ancestors())
        .unique()
        .map(|p| base.join(p))
        .sorted()
        .collect_vec();
    assert_eq!(
        cur_paths,
        expect_paths,
        "diff {} cur with {} expect paths",
        cur_paths.len(),
        expect_paths.len()
    );
    Ok((root, tree_paths))
}

fn metadata(p: impl AsRef<Path>) -> io::Result<fs::Metadata> {
    let p = p.as_ref();
    if p.is_symlink() {
        p.symlink_metadata()
    } else {
        p.metadata()
    }
}

fn calc_digest(p: impl AsRef<Path>) -> Result<[u8; 32]> {
    let mut hasher = Sha256::new();
    io::copy(&mut fs::File::open(p)?, &mut hasher)?;
    let hash = hasher.finalize();
    Ok(hash.into())
}
