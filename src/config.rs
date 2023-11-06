#![allow(unused_imports)]
use std::path::PathBuf;

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
    #[serde(rename = "attrs", default)]
    pub glob_attrs: IndexMap<String, GlobAttributes>,
    pub copy: CopyAttributes,
}

#[derive(Deserialize, Debug, Default)]
pub struct Config {
    pub apply: Option<CopierOpt>,
    pub back: Option<CopierOpt>,
    pub src: Option<PathBuf>,
    pub dst: Option<PathBuf>,
}

impl TryFrom<GlobAttributes> for OptionAttrs {
    type Error = anyhow::Error;

    #[allow(unused_variables)]
    fn try_from(a: GlobAttributes) -> Result<Self, Self::Error> {
        let uid: Option<u32> = if let Some(name) = a.user {
            #[cfg(unix)]
            {
                debug!("Looking up user by name {}", name);
                if let Some(u) = User::from_name(&name)? {
                    Some(u.uid.as_raw())
                } else {
                    bail!("Found none user by name {}", name)
                }
            }

            #[cfg(not(unix))]
            {
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

        let gid: Option<u32> = if let Some(name) = a.group {
            #[cfg(unix)]
            {
                debug!("Looking up group by name {}", name);
                if let Some(g) = Group::from_name(&name)? {
                    Some(g.gid.as_raw())
                } else {
                    bail!("Found none group by name {}", name)
                }
            }

            #[cfg(not(unix))]
            {
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
            #[cfg(unix)]
            gid,
            #[cfg(unix)]
            uid,
            mode: a.mode,
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

        if let Some(mode) = v.mode {
            a.mode = mode
        }
        if let Some(timestamps) = v.timestamps {
            a.timestamps = timestamps
        }
        #[cfg(unix)]
        {
            if let Some(ownership) = v.ownership {
                a.ownership = ownership
            }
            if let Some(xattr) = v.xattr {
                a.xattr = xattr
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allow_glob_attrs_empty() {
        let s = r#"
[apply.copy]
all = true
        "#;
        toml::from_str::<Config>(s).unwrap();
    }

    #[test]
    fn test_attributes_overriden_in_all() {
        let s = r#"
[apply.copy]
all = true
# overriden
ownership = false
mode = false
"#;
        let config = toml::from_str::<Config>(s).unwrap();
        let ca = config.apply.map(|a| a.copy).unwrap();
        let a: Attributes = ca.try_into().unwrap();
        assert!(!a.mode);
        assert!(a.timestamps);

        #[cfg(unix)]
        {
            assert!(!a.ownership);
            assert!(a.xattr);
        }
    }
}
