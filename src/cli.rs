use std::{
    fs::{self, File},
    path::PathBuf,
};

use crate::{
    config::Config,
    cp::{Attributes, CopierBuilder},
    service::{LastDestinationListService, LastDestinationListServiceBuilder},
    sync::{SyncPath, Syncer, SyncerBuilder},
    CRATE_NAME,
};
use anyhow::{anyhow, bail, Result};
use clap::{value_parser, Parser, Subcommand};
use directories::ProjectDirs;
use log::{debug, trace, warn, LevelFilter};
use os_display::Quotable;
use serde::Deserialize;
use toml::Table;

#[derive(Parser, Debug, Clone)]
#[command(author, about, version = format!(
    r#"{}
git commit: {}
git desc: {}
build date: {}
rustc version: {}"#,
    env!("CARGO_PKG_VERSION"),
    env!("VERGEN_GIT_SHA"),
    env!("VERGEN_GIT_DESCRIBE"),
    env!("VERGEN_BUILD_TIMESTAMP"),
    env!("VERGEN_RUSTC_SEMVER"),
))]
pub struct Opts {
    /// log level
    #[arg(short, long, default_value_t = 0, action = clap::ArgAction::Count, value_parser = value_parser!(u8).range(0..=5))]
    verbose: u8,

    #[arg(short, long, default_value = ProjectDirs::from("xyz", "navyd", env!("CARGO_CRATE_NAME"))
        .map(|p| p.config_dir().display().to_string())
        .unwrap_or_default())]
    config_dir: PathBuf,

    /// 从chezmoi.toml中加载key: `[data.syncdir]`中的配置
    #[arg(short = 'C', long)]
    chezmoi_conf: Option<PathBuf>,

    #[command(subcommand)]
    command: OptCommand,
}

impl Opts {
    pub fn run(&self) -> Result<()> {
        self.set_log()?;

        match &self.command {
            OptCommand::Apply { sub_opts } => {
                self.apply(sub_opts)?;
            }
            OptCommand::Sync {
                sub_opts,
                clear_src_empty_dirs: _,
            } => {
                self.sync(sub_opts)?;
            }
            OptCommand::List => self.list()?,
        }
        Ok(())
    }

    fn format_syncpath(&self, p: &SyncPath) -> String {
        match p {
            SyncPath::Removed(src) => format!("{} -", src.display()),
            SyncPath::Coppied(src, dst) => format!("{} → {}", src.display(), dst.display()),
            SyncPath::Overriden(src, dst) => format!("{} ⇉ {}", src.display(), dst.display()),
        }
    }

    fn print_paths<'a, T>(&self, paths: T)
    where
        T: IntoIterator<Item = &'a SyncPath>,
    {
        if self.verbose < 1 {
            return;
        }
        for p in paths {
            println!("{}", self.format_syncpath(p));
        }
    }

    fn list(&self) -> Result<()> {
        let srv = self.build_last_dsts_srv()?;
        if let Some(paths) = srv.load_last_dsts_in_targets(&[] as &[&str])? {
            if self.verbose >= 1 {
                for p in &paths {
                    println!("{}", p.display());
                }
            }
            println!("Found {} last dsts", paths.len());
        } else {
            println!(
                "Not found last dsts in config dir {}",
                self.config_dir.display()
            );
        }
        Ok(())
    }

    fn sync(&self, sub_opts: &SubOpts) -> Result<()> {
        let sync = self.build_syncer(sub_opts)?;
        println!(
            "Syncing back from destination {} to source {}{}",
            sync.dst().quote(),
            sync.src().quote(),
            sub_opts.dry_str(),
        );
        let paths = sync.sync_back(sub_opts.target_dsts.as_deref().unwrap_or_default())?;
        let total = paths.len();
        let (removeds, coppieds, overridens) = count_sync_paths(&paths);
        self.print_paths(&paths);
        println!(
            "A total of {} paths were synced back{}, including {} copies, {} overrides, and {} deletions",
            total,
            sub_opts.dry_str(),
            coppieds,
            overridens,
            removeds,
        );
        Ok(())
    }

    fn apply(&self, sub_opts: &SubOpts) -> Result<()> {
        let sync = self.build_syncer(sub_opts)?;

        let target_dsts = sub_opts.target_dsts.as_deref().unwrap_or_default();
        let dry_str = sub_opts.dry_str();
        println!(
            "Syncing source {} to destination {}{}",
            sync.dst().quote(),
            sync.src().quote(),
            dry_str,
        );
        let paths = sync.apply_to_dst(target_dsts)?;
        let total = paths.len();
        let (removeds, coppieds, overridens) = count_sync_paths(&paths);
        self.print_paths(&paths);
        println!(
            "A total of {} paths were synced{}, \
            including {} copies, {} overrides, and {} deletions",
            total, dry_str, coppieds, overridens, removeds
        );

        println!();
        println!("Cleaning destination {}{}", sync.dst().quote(), dry_str);
        let paths = sync.clean_dst(target_dsts)?;
        let total = paths.len();
        for p in paths {
            println!("rm {}", p.display());
        }
        println!("A total of {} paths were cleaned{}", total, dry_str);
        Ok(())
    }

    fn set_log(&self) -> Result<()> {
        let level = match self.verbose {
            0 => LevelFilter::Off,
            1 => LevelFilter::Error,
            2 => LevelFilter::Warn,
            3 => LevelFilter::Info,
            4 => LevelFilter::Debug,
            5 => LevelFilter::Trace,
            _ => bail!("Too many verboses"),
        };
        env_logger::builder()
            .filter_level(LevelFilter::Error)
            .filter_module(CRATE_NAME, level)
            .init();
        Ok(())
    }

    fn build_last_dsts_srv(&self) -> Result<LastDestinationListService> {
        let p = self.config_dir.join("last-dsts");
        if p.parent().map(|pp| pp.exists()).unwrap_or_default() && !p.exists() {
            // 创建空文件占用 防止当使用root用户指定配置文件夹时使用
            File::create(p)?;
        }
        LastDestinationListServiceBuilder::default()
            .path(self.config_dir.join("last-dsts"))
            .build()
            .map_err(Into::into)
    }

    fn build_syncer(&self, sub_opts: &SubOpts) -> Result<Syncer> {
        if !sub_opts.dry_run && !self.config_dir.exists() {
            debug!("Creating config dir in {}", self.config_dir.display());
            fs::create_dir_all(&self.config_dir)?;
        }

        let conf_path = self.config_dir.join("config.toml");
        let mut config = if let Some(p) = &self.chezmoi_conf {
            debug!("Loading config from chezmoi conf {}", p.quote());
            toml::from_str::<Table>(&fs::read_to_string(p)?)?
                .get("data")
                .and_then(|v| v.get(CRATE_NAME))
                .map_or_else(
                    || {
                        warn!(
                            "Not found key `[data.{}]` in chezmoi config {}",
                            CRATE_NAME,
                            p.quote()
                        );
                        Ok(None)
                    },
                    |v| Config::deserialize(v.clone()).map(Some),
                )?
        } else if conf_path.exists() {
            debug!("Loading config from {}", conf_path.quote());
            Some(toml::from_str::<Config>(&fs::read_to_string(conf_path)?)?)
        } else {
            None
        };

        let (apply_cp, back_cp) = (
            CopierBuilder::default().attrs(Attributes::all()).build()?,
            CopierBuilder::default()
                .attrs(Attributes::no_attrs())
                .build()?,
        );

        let (apply_cp, back_cp) = if let Some(config) = config.as_mut() {
            trace!("Trying to convert config into Copier: {:?}", config);
            let apply_cp = if let Some(apply) = config.apply.take() {
                match apply.try_into() {
                    Ok(v) => v,
                    Err(e) => {
                        if let OptCommand::Sync {
                            sub_opts: _,
                            clear_src_empty_dirs: _,
                        } = &self.command
                        {
                            warn!("Ignored incorrect apply config during sync: {}", e);
                            apply_cp
                        } else {
                            return Err(e);
                        }
                    }
                }
            } else {
                apply_cp
            };
            let back_cp = if let Some(back) = config.back.take() {
                match back.try_into() {
                    Ok(v) => v,
                    Err(e) => {
                        if let OptCommand::Apply { sub_opts: _ } = &self.command {
                            warn!("Ignored incorrect back config during apply: {}", e);
                            back_cp
                        } else {
                            return Err(e);
                        }
                    }
                }
            } else {
                back_cp
            };
            (apply_cp, back_cp)
        } else {
            (apply_cp, back_cp)
        };

        debug!(
            "Finding syncdir src,dst with arg {:?} and config {:?}",
            (&sub_opts.src, &sub_opts.dst),
            config.as_ref().map(|c| (c.src.as_ref(), c.dst.as_ref())),
        );
        let src = sub_opts.src.as_ref().map_or_else(
            || {
                config
                    .as_ref()
                    .and_then(|c| c.src.as_ref())
                    .ok_or_else(|| anyhow!("Invalid src arg"))
            },
            Ok,
        )?;
        let dst = sub_opts.dst.as_ref().map_or_else(
            || {
                config
                    .as_ref()
                    .and_then(|c| c.dst.as_ref())
                    .ok_or_else(|| anyhow!("Invalid dst arg"))
            },
            Ok,
        )?;

        SyncerBuilder::default()
            .try_src(src)?
            .try_dst(dst)?
            .dry_run(sub_opts.dry_run)
            .apply_copier(apply_cp)
            .back_copier(back_cp)
            .clear_src_empty_dirs_on_back(
                if let OptCommand::Sync {
                    sub_opts: _,
                    clear_src_empty_dirs,
                } = &self.command
                {
                    *clear_src_empty_dirs
                } else {
                    false
                },
            )
            .last_dsts_srv(self.build_last_dsts_srv()?)
            .build()
            .map_err(Into::into)
    }
}

#[derive(clap::Args, Debug, Clone)]
struct SubOpts {
    /// 需要同步到dst的源目录。 默认为空，如果在配置文件中和命令行中都未指定则会错误退出
    #[arg(short, long)]
    src: Option<PathBuf>,

    /// 需要从src同步到的目标目录。 默认为空，如果在配置文件中和命令行中都未指定则会错误退出
    #[arg(short, long)]
    dst: Option<PathBuf>,

    #[arg(short, long)]
    target_dsts: Option<Vec<PathBuf>>,

    #[arg(long)]
    dry_run: bool,
}

impl SubOpts {
    fn dry_str(&self) -> &str {
        if self.dry_run {
            " for dry running"
        } else {
            ""
        }
    }
}

#[derive(Debug, Subcommand, Clone)]
enum OptCommand {
    /// 将src文件应用到dst中
    Apply {
        #[command(flatten)]
        sub_opts: SubOpts,
    },

    /// 将dst的文件同步到src中
    Sync {
        #[command(flatten)]
        sub_opts: SubOpts,
        #[arg(long)]
        clear_src_empty_dirs: bool,
    },

    /// 打印last dsts
    List,
}

/// 统计paths返回 (removeds, coppieds, overridens)
fn count_sync_paths<'a, T>(paths: T) -> (usize, usize, usize)
where
    T: IntoIterator<Item = &'a SyncPath>,
{
    let (mut removeds, mut coppieds, mut overridens) = (0, 0, 0);
    for p in paths {
        *(match p {
            SyncPath::Removed(_) => &mut removeds,
            SyncPath::Coppied(_, _) => &mut coppieds,
            SyncPath::Overriden(_, _) => &mut overridens,
        }) += 1;
    }
    (removeds, coppieds, overridens)
}
