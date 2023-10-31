pub mod cli;
pub mod config;
pub mod cp;
pub mod service;
pub mod sync;
pub mod util;

pub static CRATE_NAME: &str = env!("CARGO_CRATE_NAME");

#[cfg(test)]
mod tests {
    use std::sync::Once;

    use log::LevelFilter;

    use crate::CRATE_NAME;

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
}
