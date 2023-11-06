mod common;

use std::{path::Path, sync::Once};

use anyhow::{bail, Error};
use common::{TestEnv, TreePath};
use log::{warn, LevelFilter};
use rstest::rstest;
use syncdir::{
    cp::{Attributes, Copier, CopierBuilder},
    sync::SyncerBuilder,
    CRATE_NAME,
};

static SIMPLE: [&[&str]; 3] = [&["a/b/c/1.t"], &["a/b/2.t"], &["d/e/3.t"]];
static PATHS: [&[&str]; 28] = [
    &["boot/cmdline.txt"],
    &["boot/config.txt"],
    &["home/git/.ssh/authorized_keys"],
    &["home/git/.ssh/id_ed25519.pub"],
    &["var/", "symlinks-var"],
    &["var/www/yacd/public/assets/Config-35023b66.css"],
    &["var/www/yacd/public/index.html"],
    &["usr/local/lib/systemd/system/frpc.service"],
    &["usr/local/lib/systemd/system/clash.service"],
    // empty dir link
    &["etc/network/", "etc/symlinks/network"],
    &["etc/docker/daemon.json"],
    &["etc/systemd/system/"],
    &["etc/wsl.conf"],
    &["etc/default"],
    &["etc/netdata/", "etc/symlinks/netdata"],
    &["etc/netdata/charts.d.conf"],
    &["etc/netdata/go.d/prometheus.conf"],
    &["etc/netdata/go.d.conf"],
    &["etc/netdata/netdata.conf"],
    &["etc/sudoers.d/010_nopasswd"],
    &["etc/openresty/", "etc/symlinks/openresty"],
    &["etc/openresty/sites-enabled/default"],
    &["etc/openresty/nginx.conf"],
    &["etc/openresty/sites-available/c.navyd.xyz.conf"],
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
    &[
        "etc/badnetwork.N/",
        "etc/badsymlinks/badnetwork",
        "etc/symlinks/badnetwork",
    ],
    &[
        "etc/gitea/app.ini.N",
        "etc/badsymlinks/app.ini",
        "etc/symlinks/badnetwork",
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

/// 提升权限运行`cargo test`参考：[Running tests with elevated privileges #5999](https://github.com/rust-lang/cargo/issues/5999)
#[allow(unused)]
fn build_apply_cp(globs: &[&str], user: Option<&str>) -> anyhow::Result<Copier> {
    #[cfg(target_os = "linux")]
    {
        use caps::{CapSet, Capability};
        use nix::unistd::User;
        use syncdir::cp::OptionAttrs;

        let uid = if let Some(user) = user {
            User::from_name(user)?.map(|u| u.uid.as_raw())
        } else {
            None
        };
        if !caps::has_cap(None, CapSet::Permitted, Capability::CAP_CHOWN)? {
            bail!("Not found CAP_CHOWN for tests. please rerun with `CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUNNER='sudo -E'`")
        }

        let ga = globs
            .iter()
            .map(|s| {
                (
                    *s,
                    OptionAttrs {
                        uid,
                        ..Default::default()
                    },
                )
            })
            .collect::<Vec<_>>();
        CopierBuilder::default()
            .attrs(Attributes::all())
            .try_glob_attrs(ga)?
            .build()
            .map_err(Into::into)
    }
    #[cfg(not(target_os = "linux"))]
    {
        bail!("unsupported")
    }
}

#[rstest]
#[case(&PATHS, &PATHS[..10], &[] as &[&str],&[])]
#[case(&PATHS, &PATHS[10..], &[] as &[&str],&["home", "boot", "etc/openresty"])]
#[case(&PATHS, &PATHS[..10], &PATHS, &[])]
#[case(&PATHS, &PATHS[10..], &[&PATHS[5..], &SIMPLE].concat(), &["home", "boot", "etc/openresty"])]
fn test_sync_back<'a, T, E, P>(
    #[case] srcs: &[T],
    #[case] dsts: &[T],
    #[case] last_dsts: &[E],
    #[case] target_dsts: &[&str],
) where
    T: TryInto<TreePath<P>, Error = Error> + Clone,
    E: TryInto<TreePath<P>, Error = Error> + Clone,
    P: AsRef<Path>,
{
    let te = TestEnv::new(srcs.iter().cloned())
        .with_dsts(dsts.iter().cloned())
        .with_last_dsts_paths(last_dsts.iter().cloned())
        .with_copier(CopierBuilder::default().build().unwrap());

    let sync = SyncerBuilder::default()
        .apply_copier(te.copier().clone())
        .last_dsts_srv(te.last_dsts_srv().clone())
        .dst(te.dst_root())
        .src(te.src_root())
        .non_interactive(true)
        .build()
        .unwrap();

    let _paths = sync.sync_back(target_dsts).unwrap();
    te.assert_sync_back(target_dsts);
}

#[rstest]
#[case(&PATHS, &PATHS[..10], &[] as &[&str],&[])]
#[case(&PATHS, &PATHS[10..], &[] as &[&str],&["home", "boot", "etc/openresty"])]
#[case(&PATHS, &PATHS[..10], &PATHS, &[])]
#[case(&PATHS, &PATHS[10..], &[&PATHS[5..], &SIMPLE].concat(), &["home", "boot", "etc/openresty"])]
fn test_apply<'a, T, E, P>(
    #[case] srcs: &[T],
    #[case] dsts: &[T],
    #[case] last_dsts: &[E],
    #[case] target_dsts: &[&str],
) where
    T: TryInto<TreePath<P>, Error = Error> + Clone,
    E: TryInto<TreePath<P>, Error = Error> + Clone,
    P: AsRef<Path>,
{
    let cps = [
        Some(
            CopierBuilder::default()
                .attrs(Attributes::all())
                .build()
                .unwrap(),
        ),
        Some(
            CopierBuilder::default()
                .attrs(Attributes {
                    #[cfg(unix)]
                    ownership: false,
                    mode: false,
                    ..Attributes::all()
                })
                .build()
                .unwrap(),
        ),
        build_apply_cp(&["**/etc/**"], Some("nobody")).map_or_else(
            |e| {
                warn!("Unsupported apply copier for tests. using default cp: {e}");
                None
            },
            Some,
        ),
    ];

    for cp in cps.into_iter().flatten() {
        let te = TestEnv::new(srcs.iter().cloned())
            .with_dsts(dsts.iter().cloned())
            .with_last_dsts_paths(last_dsts.iter().cloned())
            .with_copier(cp);

        let sync = SyncerBuilder::default()
            .apply_copier(te.copier().clone())
            .last_dsts_srv(te.last_dsts_srv().clone())
            .dst(te.dst_root())
            .src(te.src_root())
            .non_interactive(true)
            .build()
            .unwrap();

        let _paths = sync.apply_to_dst(target_dsts).unwrap();
        te.assert_applied(target_dsts);
    }
}

#[rstest]
#[case(&PATHS, &PATHS[..10], &[] as &[&str],&[])]
#[case(&PATHS, &PATHS[10..], &[] as &[&str],&["home", "boot", "etc/openresty"])]
#[case(&PATHS, &PATHS[..10], &PATHS, &[])]
#[case(&PATHS, &PATHS[10..], &[&PATHS[5..], &SIMPLE].concat(), &["home", "boot", "etc/openresty"])]
fn test_clean_dst<'a, T, E, P>(
    #[case] srcs: &[T],
    #[case] dsts: &[T],
    #[case] last_dsts: &[E],
    #[case] target_dsts: &[&str],
) where
    T: TryInto<TreePath<P>, Error = Error> + Clone,
    E: TryInto<TreePath<P>, Error = Error> + Clone,
    P: AsRef<Path>,
{
    let te = TestEnv::new(srcs.iter().cloned())
        .with_dsts(dsts.iter().cloned())
        .with_last_dsts_paths(last_dsts.iter().cloned())
        .with_copier(CopierBuilder::default().build().unwrap());

    let sync = SyncerBuilder::default()
        .apply_copier(te.copier().clone())
        .last_dsts_srv(te.last_dsts_srv().clone())
        .dst(te.dst_root())
        .src(te.src_root())
        .non_interactive(true)
        .build()
        .unwrap();

    let paths = sync.clean_dst(target_dsts).unwrap();
    te.assert_clean_dst(target_dsts, paths);
}
