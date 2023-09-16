pub mod service;
pub mod util;
pub mod sync;
pub mod cli;

#[cfg(test)]
mod tests {
    use std::sync::Once;

    use log::LevelFilter;

    #[ctor::ctor]
    fn init() {
        static INIT: Once = Once::new();
        INIT.call_once(|| {
            env_logger::builder()
                .is_test(true)
                .filter_level(LevelFilter::Info)
                .filter_module(env!("CARGO_CRATE_NAME"), LevelFilter::Trace)
                .init();
        });
    }
}
