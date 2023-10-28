use crate::cp::{Attributes, Copier, CopierBuilder, OptionAttrs};
use anyhow::{bail, Error};
use indexmap::IndexMap;
use log::{debug, warn};
#[cfg(unix)]
use nix::unistd::{Group, User};
use serde::Deserialize;

#[derive(Deserialize, Default, Debug)]
pub struct GlobAttributes {
    user: Option<String>,
    group: Option<String>,
    mode: Option<u32>,
}

#[derive(Deserialize, Debug)]
pub struct CopyAttributes {
    pub mode: Option<bool>,
    pub ownership: Option<bool>,
    pub timestamps: Option<bool>,
    pub xattr: Option<bool>,
    pub all: Option<bool>,
}

#[derive(Deserialize, Debug)]
pub struct CopierOpt {
    #[serde(rename = "attrs")]
    pub glob_attrs: IndexMap<String, GlobAttributes>,
    pub copy: CopyAttributes,
}

#[derive(Deserialize, Debug, Default)]
pub struct Config {
    pub apply: Option<CopierOpt>,
    pub back: Option<CopierOpt>,
}

impl TryFrom<GlobAttributes> for OptionAttrs {
    type Error = anyhow::Error;

    fn try_from(a: GlobAttributes) -> Result<Self, Self::Error> {
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

impl From<CopyAttributes> for Attributes {
    fn from(v: CopyAttributes) -> Self {
        let mut a = if v.all.unwrap_or_default() {
            Attributes::all()
        } else {
            Default::default()
        };

        if v.mode.unwrap_or_default() {
            a.mode = true
        }
        if v.ownership.unwrap_or_default() {
            a.ownership = true
        }
        if v.timestamps.unwrap_or_default() {
            a.timestamps = true
        }
        if v.xattr.unwrap_or_default() {
            a.xattr = true
        }
        a
    }
}

impl TryFrom<CopierOpt> for Copier {
    type Error = Error;

    fn try_from(value: CopierOpt) -> Result<Self, Self::Error> {
        CopierBuilder::default()
            .attrs(value.copy)
            .try_glob_attrs(value.glob_attrs)?
            .build()
            .map_err(Into::into)
    }
}
