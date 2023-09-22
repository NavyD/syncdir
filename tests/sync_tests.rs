use common::TestEnv;
use filetime::{set_file_times, set_symlink_file_times, FileTime};
use itertools::Itertools;
use pretty_assertions::assert_eq;
use rstest::rstest;
use syncdir::{
    sync::{CopierBuilder, SyncPath, SyncerBuilder},
    CRATE_NAME,
};
use tempfile::TempDir;
use walkdir::WalkDir;

mod common;

use std::{collections::HashSet, fs, path::Path, sync::Once};

use log::LevelFilter;

static SIMPLE: [&[&str]; 3] = [&["a/b/c/1.t"], &["a/b/2.t"], &["d/e/3.t"]];
static DEFAULT_PATHS: [&[&str]; 35] = [
    &["boot/cmdline.txt"],
    &["boot/config.txt"],
    &["home/git/.ssh/authorized_keys"],
    &["home/git/.ssh/id_ed25519.pub"],
    &["var", "symlinks-var"],
    &["var/www/ariang/public/css/bootstrap-3.4.1.min.css"],
    &["var/www/ariang/public/index.html"],
    &["var/www/ariang/public/js/angular-packages-1.6.10.min.js"],
    &["var/www/ariang/public/robots.txt"],
    &["var/www/yacd/public/assets/Config-35023b66.css"],
    &["var/www/yacd/public/index.html"],
    &["usr/local/lib/systemd/system/frpc.service"],
    &["usr/local/lib/systemd/system/clash.service"],
    // empty dir link
    &["etc/network", "etc/symlinks/network"],
    // ext N: not create file
    &[
        "etc/badnetwork.N",
        "etc/badsymlinks/badnetwork",
        "etc/symlinks/badnetwork",
    ],
    &["etc/docker/daemon.json"],
    &["etc/systemd/system"],
    &["etc/wsl.conf"],
    &["etc/default.t"],
    &["etc/netdata", "etc/symlinks/netdata"],
    &["etc/netdata/charts.d.conf"],
    &["etc/netdata/go.d/wmi.conf"],
    &["etc/netdata/go.d/postgres.conf"],
    &["etc/netdata/go.d/prometheus.conf"],
    &["etc/netdata/go.d.conf"],
    &["etc/netdata/apps_groups.conf"],
    &["etc/netdata/netdata.conf"],
    &["etc/sudoers.d/010_nopasswd.t"],
    &["etc/cron.d/backup-sync.t"],
    &["etc/openresty", "etc/symlinks/openresty"],
    &["etc/openresty/sites-enabled/default.t"],
    &["etc/openresty/nginx.conf"],
    &["etc/openresty/sites-available/portainer.navyd.xyz.conf"],
    &[
        "etc/openresty/sites-available/m.navyd.xyz.conf",
        "etc/openresty/sites-enabled/m.navyd.xyz.conf",
    ],
    &[
        "etc/openresty/sites-available/p.navyd.xyz.conf",
        "etc/openresty/sites-enabled/p.navyd.xyz.conf",
        // multi link to one path
        "etc/symlinks/multi-link.p.navyd.xyz.conf",
    ],
];

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

#[rstest]
#[case(&DEFAULT_PATHS)]
fn test_copy_simple(#[case] paths: &[&[&str]]) {
    let te = TestEnv::new(paths.iter().copied());
    let src = te.src_root();
    let cp = te.copier();
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
        te.assert_same_dir(src, dst)
    };

    test(TempDir::new().unwrap().path());
    test(&TempDir::new().unwrap().path().join("notexists"))
}

#[rstest]
#[case(&DEFAULT_PATHS)]
fn test_copy_file_times(#[case] paths: &[&[&str]]) {
    let te = TestEnv::new(paths.iter().copied())
        .with_copier(CopierBuilder::default().filetimes(true).build().unwrap());
    let to = TempDir::new().unwrap();
    let (src_base, dst_base) = (te.src_root(), to.path());

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

    for entry in WalkDir::new(src_base).contents_first(true) {
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

    te.copier().copy(src_base, dst_base).unwrap();

    te.assert_same_dir(src_base, dst_base);
}

#[rstest]
#[case(&DEFAULT_PATHS)]
fn test_copy_when_dry_run(#[case] paths: &[&[&str]]) {
    let te = TestEnv::new(paths.iter().copied());
    let to = TempDir::new().unwrap();
    let (src_base, dst_base) = (te.src_root(), to.path());

    fs::create_dir_all(dst_base).unwrap();
    let mut perm = dst_base.metadata().unwrap().permissions();
    perm.set_readonly(true);
    fs::set_permissions(dst_base, perm).unwrap();

    let cp = CopierBuilder::default()
        .dry_run(true)
        .filetimes(true)
        .build()
        .unwrap();
    let paths = cp.copy(src_base, dst_base).unwrap();
    assert!(!paths.is_empty());
}

#[rstest]
// #[case::all(&MOCK_PATHS, &MOCK_PATHS[..10], &[&MOCK_PATHS[..], &SIMPLE[..]].concat(), &["etc"])]
// #[case::mock_paths(&MOCK_PATHS, &[], &[],&[])]
// #[case::mock_paths(&MOCK_PATHS, &MOCK_PATHS, &[],&[])]
// #[case::mock_paths(&MOCK_PATHS, &MOCK_PATHS[..10], &[],&[])]
#[case::target_not_in_dsts(&DEFAULT_PATHS, &DEFAULT_PATHS[..5], &DEFAULT_PATHS.map(|v| v[0]), &["etc/openresty"])]
#[case(&DEFAULT_PATHS, &DEFAULT_PATHS, &[&DEFAULT_PATHS.map(|v| v[0])[..], &SIMPLE.map(|v| v[0])].concat(), &["home", "boot", "etc/openresty"])]
fn test_sync_back(
    #[case] srcs: &[&[&str]],
    #[case] dsts: &[&[&str]],
    #[case] last_dsts: &[&str],
    #[case] target_dsts: &[&str],
) {
    let te = TestEnv::new(srcs.iter().copied())
        .with_dsts(dsts.iter().copied())
        .with_last_dsts(last_dsts)
        .with_copier(CopierBuilder::default().build().unwrap());

    let sync = SyncerBuilder::default()
        .copier(te.copier().clone())
        .last_dsts_srv(te.last_dsts_srv().unwrap().clone())
        .dst(te.dst_root())
        .src(te.src_root())
        .non_interactive(true)
        .build()
        .unwrap();

    let _paths = sync.sync_back(target_dsts).unwrap();
    te.assert_synced(target_dsts);
}

#[rstest]
#[case(&DEFAULT_PATHS, &[], &[], &[])]
#[case(&DEFAULT_PATHS, &DEFAULT_PATHS, &[&DEFAULT_PATHS.map(|v| v[0])[..], &SIMPLE.map(|v| v[0])].concat(), &["home", "boot", "etc/openresty"])]
fn test_apply_to_dst(
    #[case] srcs: &[&[&str]],
    #[case] dsts: &[&[&str]],
    #[case] last_dsts: &[&str],
    #[case] target_dsts: &[&str],
) {
    let te = TestEnv::new(srcs.iter().copied())
        .with_dsts(dsts.iter().copied())
        .with_last_dsts(last_dsts)
        .with_copier(CopierBuilder::default().build().unwrap());

    let sync = SyncerBuilder::default()
        .copier(te.copier().clone())
        .last_dsts_srv(te.last_dsts_srv().unwrap().clone())
        .dst(te.dst_root())
        .src(te.src_root())
        .non_interactive(true)
        .build()
        .unwrap();

    let _paths = sync.apply_to_dst(target_dsts).unwrap();
    te.assert_synced(target_dsts);
}

#[rstest]
#[case(&DEFAULT_PATHS, &[], &[], &[])]
#[case(&DEFAULT_PATHS, &DEFAULT_PATHS, &[&DEFAULT_PATHS.map(|v| v[0])[..], &SIMPLE.map(|v| v[0])].concat(), &["home", "boot", "etc/openresty"])]
fn test_clean_dst(
    #[case] srcs: &[&[&str]],
    #[case] dsts: &[&[&str]],
    #[case] last_dsts: &[&str],
    #[case] target_dsts: &[&str],
) {
    let te = TestEnv::new(srcs.iter().copied())
        .with_dsts(dsts.iter().copied())
        .with_last_dsts(last_dsts)
        .with_copier(CopierBuilder::default().build().unwrap());
    let dst = te.dst_root();

    let sync = SyncerBuilder::default()
        .copier(te.copier().clone())
        .last_dsts_srv(te.last_dsts_srv().unwrap().clone())
        .dst(dst)
        .src(te.src_root())
        .non_interactive(true)
        .build()
        .unwrap();

    let paths = sync.clean_dst(target_dsts).unwrap();

    let target_dsts = {
        let mut v = target_dsts.iter().copied().collect_vec();
        if v.is_empty() {
            v.push("");
        }
        v
    };

    let src_paths = srcs
        .iter()
        .flat_map(|v| v.iter())
        // .map(|s| src.join(s))
        .collect::<HashSet<_>>();
    let expect_cleaneds = dsts
        .iter()
        .flat_map(|v| v.iter())
        // 不包含在当前的src中
        .filter(|p| !src_paths.contains(p))
        // 不包含在非targets中
        .filter(|p| target_dsts.iter().any(|t| p.starts_with(t)))
        // 绝对路径
        .map(|s| dst.join(s))
        .collect_vec();
    let paths = paths.into_iter().sorted().collect_vec();
    assert_eq!(paths, expect_cleaneds);
    for p in &expect_cleaneds {
        assert!(!p.exists());
    }
    // 非expect_cleaneds应该都存在
    for p in dsts
        .iter()
        .flat_map(|v| v.iter())
        .map(|s| dst.join(s))
        .filter(|p| !expect_cleaneds.contains(p))
    {
        assert!(
            // 忽略不存在的软链接:N
            p.exists() || p.is_symlink() || p.extension().unwrap() == "N",
            "expect exists path: {}",
            p.display()
        );
    }
}
