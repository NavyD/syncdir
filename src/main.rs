use std::process::exit;

use clap::Parser;
use syncdir::cli::Opts;

fn main() {
    if let Err(e) = Opts::parse().run() {
        eprintln!("Failed to run on error: {e}");
        exit(1)
    }
}
