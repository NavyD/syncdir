use std::{fs, path::PathBuf};

use crate::{
    service::{LastDestinationListService, LastDestinationListServiceBuilder},
    sync::{SyncPath, Syncer, SyncerBuilder},
    CRATE_NAME,
};
use anyhow::{bail, Result};
use clap::{value_parser, Parser, Subcommand};
use directories::ProjectDirs;
use log::{debug, LevelFilter};

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Opts {
    /// log level
    #[arg(short, long, default_value_t = 0, action = clap::ArgAction::Count, value_parser = value_parser!(u8).range(0..=5))]
    verbose: u8,

    #[arg(short, long, default_value = ProjectDirs::from("xyz", "navyd", env!("CARGO_CRATE_NAME"))
        .map(|p| p.config_dir().display().to_string())
        .unwrap_or_default())]
    config_dir: PathBuf,

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
            OptCommand::Sync { sub_opts } => {
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
            for p in &paths {
                println!("{}", p.display());
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
            sub_opts.dst.display(),
            sub_opts.src.display(),
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
            sub_opts.src.display(),
            sub_opts.dst.display(),
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
        println!("Cleaning destination {}{}", sub_opts.dst.display(), dry_str);
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
            .filter_module(module_path!(), level)
            .filter_module(CRATE_NAME, level)
            .init();
        Ok(())
    }

    fn build_last_dsts_srv(&self) -> Result<LastDestinationListService> {
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
        let last_dsts_srv = self.build_last_dsts_srv()?;
        SyncerBuilder::default()
            .try_src(&sub_opts.src)?
            .try_dst(&sub_opts.dst)?
            .dry_run(sub_opts.dry_run)
            .last_dsts_srv(last_dsts_srv)
            .build()
            .map_err(Into::into)
    }
}

#[derive(clap::Args, Debug, Clone)]
struct SubOpts {
    #[arg(short, long, default_value = ".")]
    src: PathBuf,

    #[arg(short, long)]
    dst: PathBuf,

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
