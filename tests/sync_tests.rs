use common::{assert_same_dir, TestEnv};
use filetime::{set_file_times, set_symlink_file_times, FileTime};
use itertools::Itertools;
use pretty_assertions::assert_eq;
use rstest::rstest;
use syncdir::{
    sync::{CopierBuilder, SyncPath},
    util, CRATE_NAME,
};
use tempfile::TempDir;
use walkdir::WalkDir;

mod common;

use std::{path::Path, sync::Once};

use log::LevelFilter;

#[ctor::ctor]
fn init() {
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        env_logger::builder()
            .is_test(true)
            .filter_level(LevelFilter::Info)
            .filter_module(CRATE_NAME, LevelFilter::Trace)
            .init();
    });
}

static MOCK_PATHS: [&str; 20] = [
    "usr/local/lib/systemd/system/clash.service",
    "etc/network",
    "etc/docker/daemon.json",
    "etc/systemd/system",
    "etc/wsl.conf",
    "etc/default.t",
    "etc/netdata/charts.d.conf",
    "etc/netdata/go.d/wmi.conf",
    "etc/netdata/go.d/postgres.conf",
    "etc/netdata/go.d/prometheus.conf",
    "etc/netdata/go.d.conf",
    "etc/netdata/apps_groups.conf",
    "etc/netdata/netdata.conf",
    "etc/sudoers.d/010_nopasswd.t",
    "etc/cron.d/backup-sync.t",
    "etc/openresty/sites-enabled/default.t",
    "etc/openresty/nginx.conf",
    "etc/openresty/sites-available/portainer.navyd.xyz.conf",
    "etc/openresty/sites-available/m.navyd.xyz.conf",
    "etc/openresty/sites-available/p.navyd.xyz.conf",
];

static ONLY_FILES: [&str; 2] = ["a.t", "b.t"];
static SIMPLE: [&str; 3] = ["a/b/c/1.t", "a/b/2.t", "d/e/3.t"];

#[rstest]
#[case::simple(SIMPLE)]
#[case::only_files(ONLY_FILES)]
#[case::mock_paths(MOCK_PATHS)]
fn test_copy_simple<const N: usize>(#[case] paths: [&str; N]) {
    let te = TestEnv::new(paths);
    let src = te.root();
    let cp = CopierBuilder::default().build().unwrap();
    let test = |dst: &Path| {
        let dst_exists = dst.exists();
        let paths = cp.copy(src, dst).unwrap();
        let non_coppieds = paths
            .iter()
            .filter(|p| !matches!(p, SyncPath::Coppied(_, _)))
            .collect_vec();
        let coppieds = paths
            .iter()
            .filter(|p| matches!(p, SyncPath::Coppied(_, _)))
            .collect_vec();
        if dst_exists {
            assert_eq!(non_coppieds.len(), 1);
            assert_eq!(coppieds.len(), paths.len() - 1);
        } else {
            assert_eq!(coppieds, paths.iter().collect_vec());
        }
        assert_same_dir(src, dst)
    };

    test(TempDir::new().unwrap().path());
    test(&TempDir::new().unwrap().path().join("notexists"))
}

#[rstest]
#[case::simple(SIMPLE)]
#[case::only_files(ONLY_FILES)]
#[case::mock_paths(MOCK_PATHS)]
fn test_copy_file_times<const N: usize>(#[case] paths: [&str; N]) {
    let te = TestEnv::new(paths);
    let to = TempDir::new().unwrap();
    let (src_base, dst_base) = (te.root(), to.path());

    let am_times = |p: &Path| {
        if p.is_symlink() {
            (
                FileTime::from_unix_time(1640966409, 0),
                FileTime::from_unix_time(1609430409, 0),
            )
        } else {
            (
                // a: 2022 00:00:00 GMT+0800
                FileTime::from_unix_time(1640966400, 0),
                // m: 2021 00:00:00 GMT+0800
                FileTime::from_unix_time(1609430400, 0),
            )
        }
    };

    let get_am_times = |p: &Path| {
        let meta = if p.is_symlink() {
            p.symlink_metadata()
        } else {
            p.metadata()
        }
        .unwrap();
        (
            Into::<FileTime>::into(meta.accessed().unwrap()),
            Into::<FileTime>::into(meta.modified().unwrap()),
        )
    };

    for entry in WalkDir::new(src_base) {
        let p = entry.unwrap().into_path();
        let (atime, mtime) = am_times(&p);
        if p.is_symlink() {
            set_symlink_file_times(&p, atime, mtime).unwrap();
        } else {
            set_file_times(&p, atime, mtime).unwrap();
        }

        assert_eq!(
            get_am_times(&p),
            am_times(&p),
            "filetime for path: {}",
            p.display()
        );
    }

    let cp = CopierBuilder::default().filetimes(true).build().unwrap();
    cp.copy(src_base, dst_base).unwrap();

    for entry in WalkDir::new(dst_base) {
        let dst = entry.unwrap().into_path();
        let (dst_atime, dst_mtime) = get_am_times(&dst);
        // 注意：由于copy会访问src文件导致src atime被修改，而目录dst在添加文件后mtime被修改
        assert_eq!(
            dst_mtime,
            am_times(&dst).1,
            "filetime for path: {}",
            dst.display()
        );
        let src = util::map_path(&dst, dst_base, src_base).unwrap();
        assert_eq!(
            dst_atime,
            get_am_times(&src).0,
            "filetime for path: {}",
            dst.display()
        );
    }
}
