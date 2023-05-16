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

use quote::ToTokens;
use tr::{tr, tr_init};

use anyhow::{anyhow, Error};
use clap::{arg, command, Arg, ArgAction};
use std::collections::HashMap;
use std::str::FromStr;

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

/// How much [Message](Message) location information to include in the
/// output.
#[derive(PartialEq, Debug)]
pub enum AddLocation {
    /// Format the locations output as ‘#: filename:line’
    /// This is the default.
    Full,
    /// Format the locations output as ‘#: filename`
    File,
    /// Don't include the message locations.
    Never,
}

impl FromStr for AddLocation {
    type Err = anyhow::Error;

    /// Create an [AddLocation](AddLocation) from a &str. Valid inputs
    /// are: "full", "file" or "never".
    fn from_str(s: &str) -> Result<AddLocation, Self::Err> {
        match s {
            "full" => Ok(AddLocation::Full),
            "file" => Ok(AddLocation::File),
            "never" => Ok(AddLocation::Never),
            _ => Err(anyhow!(
                "\"{0}\" is not a valid --add-location option. Valid \
                    options are \"full\", \"file\" or \"never\".",
                s
            )),
        }
    }
}

pub struct OutputDetails {
    omit_header: bool,
    copyright_holder: Option<String>,
    package_name: Option<String>,
    package_version: Option<String>,
    bugs_address: Option<String>,
    charset: String,
    add_location: AddLocation,
}

fn main() -> Result<(), Error> {
    tr_init!(concat!(env!("CARGO_MANIFEST_DIR"), "/lang/"));

    // The options are made to be compatible with xgetext options
    let matches = command!()
        .about(tr!(
            "Extract strings from a rust crate to be translated with gettext"
        ))
        .arg(
            Arg::new("domain")
                .short('d')
                .long("default-domain")
                .value_name("domain")
                .help(&tr!("Use name.po for output (instead of messages.po)")),
        )
        .arg(arg!(OUTPUT: -o --output <file>).help(&tr!(
            "Write output to specified file (instead of messages.po)."
        )))
        .arg(
            arg!(KEYWORDS: -k --keywords <keywordspec>)
                .action(ArgAction::Append)
                .help(&tr!(
                    // documentation for keywordspec goes here
                    "Specify keywordspec as an additional keyword to be looked for. \
                     Refer to the xgettext documentation for more info."
                )),
        )
        .arg(
            Arg::new("omit-header")
                .long("omit-header")
                .help(&tr!(r#"Don’t write header with ‘msgid ""’ entry"#)),
        )
        .arg(
            Arg::new("copyright-holder")
                .long("copyright-holder")
                .value_name("string")
                .help(&tr!("Set the copyright holder in the output.")),
        )
        .arg(
            Arg::new("package-name")
                .long("package-name")
                .value_name("package")
                .help(&tr!("Set the package name in the header of the output.")),
        )
        .arg(
            Arg::new("package-version")
                .long("package-version")
                .value_name("version")
                .help(&tr!("Set the package version in the header of the output.")),
        )
        .arg(
            Arg::new("msgid-bugs-address")
                .long("msgid-bugs-address")
                .value_name("email@address")
                .help(&tr!(
                    "Set the reporting address for msgid bugs. This is the email address \
                     or URL to which the translators shall report bugs in the untranslated strings"
                )),
        )
        .arg(
            Arg::new("charset")
                .long("charset")
                .value_name("encoding")
                .default_value("UTF-8")
                .help(&tr!(
                    "The encoding used for the characters in the POT file's locale."
                )),
        )
        .arg(
            Arg::new("add-location")
                .long("add-location")
                .short('n')
                .help(&tr!(
                    "How much message location information to include in the output. \
                     (default). If the type is ‘full’ (the default), it generates the \
                     lines with both file name and line number: ‘#: filename:line’. \
                     If it is ‘file’, the line number part is omitted: ‘#: filename’. \
                     If it is ‘never’, nothing is generated."
                ))
                .value_name("type")
                .value_parser(["full", "file", "never"])
                .default_value("full"),
        )
        .arg(
            Arg::new("INPUT")
                // documentation for the input
                .help(&tr!("Main rust files to parse (will recurse into modules)"))
                .required(true)
                .action(ArgAction::Append),
        )
        .get_matches();

    let keywords = matches
        .get_occurrences("KEYWORDS")
        .map(|x| x.flatten().map(String::as_str).collect())
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
                })
                .collect();
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

    let inputs = matches
        .get_occurrences::<String>("INPUT")
        .expect("Missing crate root");
    for i in inputs.flatten() {
        crate_visitor::visit_crate(i, |path, source, file| {
            extract_messages::extract_messages(
                &mut results,
                &specs,
                file.into_token_stream(),
                source,
                path,
            )
        })?;
    }

    let od = OutputDetails {
        omit_header: matches.contains_id("omit-header"),
        copyright_holder: matches.get_one("copyright-holder").cloned(),
        package_name: matches.get_one("package-name").cloned(),
        package_version: matches.get_one("package-version").cloned(),
        bugs_address: matches.get_one("msgid-bugs-address").cloned(),
        charset: matches
            .get_one::<String>("charset")
            .expect("expected charset to have a default value")
            .clone(),
        add_location: AddLocation::from_str(
            matches
                .get_one::<String>("add-location")
                .expect("expected add-location to have a default value"),
        )
        .expect("expected add-location to be a valid value"),
    };

    let mut messages: Vec<_> = results.values().collect();
    messages.sort_by_key(|m| m.index);
    generator::generate(
        matches.get_one("OUTPUT").cloned().unwrap_or_else(|| {
            format!(
                "{}.po",
                matches
                    .get_one("domain")
                    .map(String::as_str)
                    .unwrap_or("messages")
            )
        }),
        od,
        messages,
    )?;

    Ok(())
}
