use crate::cp::OptionAttrs;
use anyhow::bail;
use indexmap::IndexMap;
use log::{debug, warn};
#[cfg(unix)]
use nix::unistd::{Group, User};
use serde::Deserialize;

#[derive(Deserialize, Default, Debug)]
pub struct Attributes {
    user: Option<String>,
    group: Option<String>,
    mode: Option<u32>,
}

#[derive(Deserialize, Debug)]
pub struct Config {
    #[serde(rename = "attrs")]
    pub path_attrs: IndexMap<String, Attributes>,
}

impl TryFrom<Attributes> for OptionAttrs {
    type Error = anyhow::Error;

    fn try_from(a: Attributes) -> Result<Self, Self::Error> {
        let uid = if let Some(name) = a.user {
            if cfg!(unix) {
                debug!("Looking up user by name {}", name);
                if let Some(u) = User::from_name(&name)? {
                    Some(u.uid.as_raw())
                } else {
                    bail!("Found none user by name {}", name)
                }
            } else {
                warn!(
                    "Ignoring config user {} on unsupported os {}",
                    name,
                    std::env::consts::OS
                );
                None
            }
        } else {
            None
        };

        let gid = if let Some(name) = a.group {
            if cfg!(unix) {
                debug!("Looking up group by name {}", name);
                if let Some(g) = Group::from_name(&name)? {
                    Some(g.gid.as_raw())
                } else {
                    bail!("Found none group by name {}", name)
                }
            } else {
                warn!(
                    "Ignoring config group {} on unsupported os {}",
                    name,
                    std::env::consts::OS
                );
                None
            }
        } else {
            None
        };

        Ok(Self {
            gid,
            mode: a.mode,
            uid,
        })
    }
}
