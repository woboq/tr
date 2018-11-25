/* Copyright (C) 2018 Olivier Goffart <ogoffart@woboq.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

extern crate proc_macro2;
extern crate syn;
#[macro_use]
extern crate clap;
extern crate failure;
#[macro_use]
extern crate tr;
extern crate chrono;

use clap::{App, Arg};
use failure::Error;
use std::collections::HashMap;
use syn::export::ToTokens;

mod crate_visitor;
mod extract_messages;
mod generator;

#[derive(Clone, Debug)]
pub enum SpecArg {
    MsgId(u32),
    Context(u32),
}

#[derive(Default, Clone, Debug)]
pub struct Spec {
    pub args: Vec<SpecArg>,
    pub comment: Option<String>,
    pub argnum: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Location {
    pub file: std::path::PathBuf,
    pub line: usize,
}

#[derive(Debug, Clone, Default, Eq, PartialEq, Hash)]
pub struct MessageKey(String, String);

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Message {
    msgctxt: Option<String>,
    msgid: String,
    plural: Option<String>,
    locations: Vec<Location>,
    comments: Option<String>,
    /// that's just keeping the count, so they can be sorted
    index: usize,
}

pub struct OutputDetails {
    omit_header: bool,
    copyright_holder: Option<String>,
    package_name: Option<String>,
    package_version: Option<String>,
    bugs_address: Option<String>,
}

fn main() -> Result<(), Error> {
    tr_init!(concat!(env!("CARGO_MANIFEST_DIR"), "/lang/"));

    // The options are made to be compatible with xgetext options
    let matches = App::new("xtr")
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .arg(
            Arg::with_name("domain")
                .short("d")
                .long("default-domain")
                .value_name("domain")
                .help(&tr!("Use name.po for output (instead of messages.po)")),
        ).arg(
            Arg::with_name("OUTPUT")
                .short("o")
                .long("output")
                .value_name("file")
                .help(&tr!(
                    "Write output to specified file (instead of messages.po)."
                )),
        ).arg(
            Arg::with_name("KEYWORDS")
                .short("k")
                .long("keyword")
                .value_name("keywordspec")
                .use_delimiter(false)
                .multiple(true)
                .help(&tr!(
                    // documentation for keywordspec goes here
                    "Specify keywordspec as an additional keyword to be looked for.\
                     Refer to the xgettext documentation for more info."
                )),
        ).arg(
            Arg::with_name("omit-header")
                .long("omit-header")
                .help(&tr!(r#"Don’t write header with ‘msgid ""’ entry"#)),
        ).arg(
            Arg::with_name("copyright-holder")
                .long("copyright-holder")
                .value_name("string")
                .help(&tr!("Set the copyright holder in the output.")),
        ).arg(
            Arg::with_name("package-name")
                .long("package-name")
                .value_name("package")
                .help(&tr!("Set the package name in the header of the output.")),
        ).arg(
            Arg::with_name("package-version")
                .long("package-version")
                .value_name("version")
                .help(&tr!("Set the package version in the header of the output.")),
        ).arg(
            Arg::with_name("msgid-bugs-address")
                .long("msgid-bugs-address")
                .value_name("email@address")
                .help(&tr!(
                    "Set the reporting address for msgid bugs. This is the email address \
                     or URL to which the translators shall report bugs in the untranslated strings"
                )),
        ).arg(
            Arg::with_name("INPUT")
                // documentation for the input
                .help(&tr!("Rust file to parse"))
                .required(true)
                .index(1),
        ).get_matches();

    let keywords = matches
        .values_of("KEYWORDS")
        .map(|x| x.collect())
        .unwrap_or_else(|| {
            vec![
                "tr",
                "gettext",
                "dgettext:2",
                "dcgettext:2",
                "ngettext:1,2",
                "dngettext:2,3",
                "dcngettext:2,3",
                "gettext_noop",
                "pgettext:1c,2",
                "dpgettext:2c,3",
                "dcpgettext:2c,3",
                "npgettext:1c,2,3",
                "dnpgettext:2c,3,4",
                "dcnpgettext:2c,3,4",
            ]
        });
    let mut specs = HashMap::new();
    for k in keywords {
        if let Some(colon) = k.find(':') {
            let (name, desc) = k.split_at(colon);
            let spec = desc[1..]
                .split(',')
                .map(|d| {
                    if d.ends_with('c') {
                        return SpecArg::Context(
                            d[..d.len() - 1].parse().expect("invalid keyword spec"),
                        );
                    }
                    SpecArg::MsgId(d.parse().expect("invalid keyword spec"))
                    // TODO: comment or argnum
                }).collect();
            specs.insert(
                name.to_owned(),
                Spec {
                    args: spec,
                    comment: None,
                    argnum: None,
                },
            );
        } else {
            specs.insert(k.to_owned(), Spec::default());
        }
    }

    let mut results = HashMap::new();

    crate_visitor::visit_crate(
        matches.value_of("INPUT").expect("Missing crate root"),
        |path, source, file| {
            extract_messages::extract_messages(
                &mut results,
                &specs,
                file.into_token_stream(),
                source,
                path,
            )
        },
    )?;

    let od = OutputDetails {
        omit_header: matches.is_present("omit-header"),
        copyright_holder: matches.value_of("copyright-holder").map(str::to_owned),
        package_name: matches.value_of("package-name").map(str::to_owned),
        package_version: matches.value_of("package-version").map(str::to_owned),
        bugs_address: matches.value_of("msgid-bugs-address").map(str::to_owned),
    };

    let mut messages: Vec<_> = results.values().collect();
    messages.sort_by_key(|m| m.index);
    generator::generate(
        matches
            .value_of("OUTPUT")
            .map(|s| s.to_owned())
            .unwrap_or_else(|| format!("{}.po", matches.value_of("domain").unwrap_or("messages"))),
        od,
        messages,
    )?;

    Ok(())
}
