extern crate proc_macro2;
extern crate syn;
#[macro_use]
extern crate clap;
extern crate failure;
#[macro_use]
extern crate tr;

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

#[derive(Debug, Clone)]
struct Location {
    pub file: String,
    pub line: usize,
}

#[derive(Debug, Clone, Default)]
pub struct Message {
    msgctxt: Option<String>,
    msgid: String,
    plural: Option<String>,
    locations: Vec<Location>,
    comments: Option<String>,
}

fn main() -> Result<(), Error> {
    let matches = App::new("xtr")
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        //        .arg(Arg::with_name("domain").short("d").long("default-domain")
        //            .help(tr!("Use name.po for output (instead of messages.po)")))
        .arg(
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
                    "Specify keywordspec as an additional keyword to be looked for.\
                     Refer to the xgettext documentation for more info."
                )),
        ).arg(
            Arg::with_name("INPUT")
                .help(&tr!("Rust file to parse"))
                .required(true)
                .index(1),
        ).get_matches();

    let specs: HashMap<String, Spec> = if let Some(val) = matches.values_of("KEYWORDS") {
        let mut specs = HashMap::new();
        for k in val {
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
        specs
    } else {
        [
            ("gettext".into(), Default::default()),
            ("tr".into(), Default::default()),
        ]
            .iter()
            .cloned()
            .collect()
    };

    let mut ex = extract_messages::Extractor {
        specs: &specs,
        results: Vec::new(),
    };

    crate_visitor::visit_crate(
        matches.value_of("INPUT").expect("Missing crate root"),
        |_, _, file| ex.extract_messages(file.into_token_stream()),
    )?;

    generator::generate(
        matches.value_of("OUTPUT").unwrap_or("messages.po"),
        ex.results,
    )?;

    Ok(())
}
