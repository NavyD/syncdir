use std::{path::PathBuf, process::exit};

use anyhow::{bail, Result};
use clap::{value_parser, Parser, Subcommand};
use directories::ProjectDirs;
use itertools::Itertools;
use log::LevelFilter;
use syncdir::{
    service::LastDestinationListServiceBuilder,
    sync::{SyncPath, Syncer, SyncerBuilder},
};

fn main() {
    if let Err(e) = Cli::new().and_then(|cli| cli.run()) {
        eprintln!("Failed to run on error: {e}");
        exit(1);
    }
}

#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// log level
    #[arg(short, long, default_value_t = 0, action = clap::ArgAction::Count, value_parser = value_parser!(u8).range(0..=5))]
    verbose: u8,

    #[arg(short, long, default_value = ProjectDirs::from("xyz", "navyd", env!("CARGO_CRATE_NAME"))
        .map(|p| p.config_dir().display().to_string())
        .unwrap_or_default())]
    config_dir: PathBuf,

    #[command(subcommand)]
    command: Commands,
}

impl Args {
    fn validate(&self) -> Result<()> {
        let mut paths = vec![&self.config_dir];
        match &self.command {
            Commands::Apply { sub_args } | Commands::Sync { sub_args } => {
                paths.push(&sub_args.src);
                paths.push(&sub_args.dst);
                if let Some(t) = &sub_args.target_dsts {
                    paths.extend(t)
                }
            }
            _ => {}
        }
        for p in paths {
            if !p.exists() {
                bail!("Not found path {}", p.display());
            }
        }
        Ok(())
    }
}

#[derive(clap::Args, Debug, Clone)]
struct SubArgs {
    #[arg(short, long)]
    src: PathBuf,

    #[arg(short, long)]
    dst: PathBuf,

    #[arg(short, long)]
    target_dsts: Option<Vec<PathBuf>>,

    #[arg(long)]
    dry_run: bool,
}

#[derive(Debug, Subcommand, Clone)]
enum Commands {
    /// 将src文件应用到dst中
    Apply {
        #[command(flatten)]
        sub_args: SubArgs,
    },

    /// 将dst的文件同步到src中
    Sync {
        #[command(flatten)]
        sub_args: SubArgs,
    },

    List,
}

#[derive(Debug, Clone)]
struct Cli {
    args: Args,
}

impl Cli {
    fn new() -> Result<Self> {
        Ok(Self {
            args: Args::parse(),
        })
    }

    fn run(&self) -> Result<()> {
        self.args.validate()?;
        self.set_log()?;

        match &self.args.command {
            Commands::Apply { sub_args } => {
                let sync = self.build_syncer(sub_args)?;
                apply(&sync, sub_args)?;
            }
            Commands::Sync { sub_args } => {
                let sync = self.build_syncer(sub_args)?;
                println!(
                    "{}Syncing back from dst {} to src {}",
                    if sub_args.dry_run { "Dry running " } else { "" },
                    sub_args.dst.display(),
                    sub_args.src.display(),
                );
                let paths = sync.sync_back(sub_args.target_dsts.as_deref().unwrap_or_default())?;
                println!(
                    "{}Sync back total {} paths:",
                    if sub_args.dry_run { "Dry running " } else { "" },
                    paths.len()
                );
                paths
                    .iter()
                    .sorted_unstable()
                    .for_each(|p| println!("{:?}", p));
            }
            _ => todo!(),
        }
        Ok(())
    }

    fn build_syncer(&self, sub_args: &SubArgs) -> Result<Syncer> {
        let last_dsts_p = self.args.config_dir.join("last-dsts");
        let last_dsts_srv = LastDestinationListServiceBuilder::default()
            .path(last_dsts_p)
            .build()?;
        SyncerBuilder::default()
            .src(sub_args.src.canonicalize()?)
            .dst(sub_args.dst.canonicalize()?)
            .dry_run(sub_args.dry_run)
            .last_dsts_srv(last_dsts_srv)
            .build()
            .map_err(Into::into)
    }

    fn set_log(&self) -> Result<()> {
        let level = match self.args.verbose {
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
            .init();
        Ok(())
    }
}

fn apply(sync: &Syncer, sub_args: &SubArgs) -> Result<()> {
    let target_dsts = sub_args.target_dsts.as_deref().unwrap_or_default();
    println!(
        "{}Applying src {} to dst {}",
        if sub_args.dry_run { "Dry running " } else { "" },
        sub_args.src.display(),
        sub_args.dst.display(),
    );
    let paths = sync.apply_to_dst(target_dsts)?;

    println!(
        "{}Synced total {} paths:",
        if sub_args.dry_run { "Dry running " } else { "" },
        paths.len()
    );
    let coppieds = paths
        .iter()
        .filter(|p| matches!(p, SyncPath::Coppied(_, _)))
        .collect_vec();
    println!(
        "{}Coppied {} paths:",
        if sub_args.dry_run { "Dry running " } else { "" },
        coppieds.len()
    );
    coppieds.into_iter().for_each(|p| {
        if let SyncPath::Coppied(s, d) = p {
            println!(
                "{}Coppied {} from {}",
                if sub_args.dry_run { "Dry running " } else { "" },
                d.display(),
                s.display()
            );
        }
    });

    let overridens = paths
        .iter()
        .filter(|p| matches!(p, SyncPath::Overriden(_, _)))
        .collect_vec();
    println!(
        "{}Overriden {} paths:",
        if sub_args.dry_run { "Dry running " } else { "" },
        overridens.len()
    );
    overridens.into_iter().for_each(|p| {
        if let SyncPath::Overriden(s, d) = p {
            println!(
                "{}Overriden {} from {}",
                if sub_args.dry_run { "Dry running " } else { "" },
                d.display(),
                s.display()
            );
        }
    });

    println!(
        "{}Cleaning dst {}",
        if sub_args.dry_run { "Dry running " } else { "" },
        sub_args.dst.display()
    );
    let paths = sync.clean_dst(target_dsts)?;

    println!(
        "{}Cleaned total {} paths:",
        if sub_args.dry_run { "Dry running " } else { "" },
        paths.len()
    );
    paths.into_iter().for_each(|p| {
        println!(
            "{}Cleaned {}",
            if sub_args.dry_run { "Dry running " } else { "" },
            p.display()
        )
    });
    Ok(())
}
