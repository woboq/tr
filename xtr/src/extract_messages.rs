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

use super::{Message, MessageKey, Spec, SpecArg};
use failure::Error;
use proc_macro2::{TokenStream, TokenTree};
use std::collections::HashMap;
use std::path::PathBuf;

pub fn extract_messages(
    results: &mut HashMap<MessageKey, Message>,
    specs: &HashMap<String, Spec>,
    stream: TokenStream,
    source: &str,
    path: &PathBuf,
) -> Result<(), Error> {
    let mut ex = Extractor {
        results,
        specs,
        path,
        source_lines: split_lines(source),
    };
    ex.extract_messages(stream)
}

fn split_lines(source: &str) -> Vec<&str> {
    if cfg!(procmacro2_semver_exempt) {
        source.split('\n').collect()
    } else {
        Vec::new()
    }
}

#[allow(dead_code)]
struct Extractor<'a> {
    results: &'a mut HashMap<MessageKey, Message>,
    specs: &'a HashMap<String, Spec>,
    path: &'a PathBuf,
    source_lines: Vec<&'a str>,
}

impl<'a> Extractor<'a> {
    fn extract_messages(&mut self, stream: TokenStream) -> Result<(), Error> {
        let mut token_iter = stream.into_iter().peekable();
        while let Some(token) = token_iter.next() {
            match token {
                TokenTree::Group(group) => {
                    self.extract_messages(group.stream())?;
                }
                TokenTree::Ident(ident) => {
                    if let Some(spec) = self.specs.get(&ident.to_string()) {
                        let mut skip = false;
                        if let Some(TokenTree::Punct(punct)) = token_iter.peek() {
                            if punct.to_string() == "!" {
                                // allow macros
                                skip = true;
                            }
                        }
                        if skip {
                            token_iter.next();
                        }

                        if let Some(TokenTree::Group(group)) = token_iter.peek() {
                            self.found_string(spec, group.stream());
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn found_string(&mut self, spec: &Spec, stream: TokenStream) {
        let mut token_iter = stream.into_iter().peekable();

        let mut msgctxt: Option<String> = None;
        let mut msgid: Option<proc_macro2::Literal> = None;
        let mut plural: Option<String> = None;

        if spec.args.is_empty() {
            let mut literal = if let Some(TokenTree::Literal(literal)) = token_iter.next() {
                literal
            } else {
                return; // syntax error
            };

            let mut token = token_iter.next();
            if let Some(TokenTree::Punct(punct)) = token.clone() {
                if punct.to_string() == "=" {
                    token = token_iter.next();
                    if let Some(TokenTree::Punct(punct)) = token.clone() {
                        if punct.to_string() == ">" {
                            if let Some(TokenTree::Literal(lit)) = token_iter.next() {
                                msgctxt = literal_to_string(&literal);
                                literal = lit;
                                token = token_iter.next();
                            } else {
                                return; // syntax error
                            }
                        }
                    }
                }
            }
            msgid = Some(literal.clone());
            if let Some(TokenTree::Punct(punct)) = token {
                if punct.to_string() == "|" {
                    if let Some(TokenTree::Literal(lit)) = token_iter.next() {
                        plural = literal_to_string(&lit);
                    }
                }
            }
        } else {
            let mut args = Vec::new();
            'm: loop {
                if let Some(TokenTree::Literal(literal)) = token_iter.peek() {
                    args.push(Some(literal.clone()));
                } else {
                    args.push(None);
                }

                // skip to the comma
                while let Some(token) = token_iter.next() {
                    if let TokenTree::Punct(punct) = token {
                        if punct.to_string() == "," {
                            continue 'm;
                        }
                    }
                }
                break;
            }

            if let Some(num) = spec.argnum {
                if args.len() != num as usize {
                    return;
                }
            }
            for a in spec.args.iter() {
                match a {
                    SpecArg::MsgId(i) => {
                        if msgid.is_some() {
                            plural = args
                                .get(*i as usize - 1)
                                .and_then(|x| x.as_ref())
                                .and_then(|lit| literal_to_string(lit));
                        } else if let Some(lit) = args.get(*i as usize - 1) {
                            msgid = lit.clone();
                        }
                    }
                    SpecArg::Context(i) => {
                        msgctxt = args
                            .get(*i as usize - 1)
                            .and_then(|x| x.as_ref())
                            .and_then(|lit| literal_to_string(lit));
                    }
                }
            }
        }

        if let Some(lit) = msgid {
            if let Some(msgid) = literal_to_string(&lit) {
                let key = MessageKey(msgid.clone(), msgctxt.clone().unwrap_or_default());
                let index = self.results.len();
                let mut message = self.results.entry(key).or_insert_with(|| Message {
                    msgctxt,
                    msgid,
                    index,
                    ..Default::default()
                });
                if plural.is_some() {
                    message.plural = plural;
                }

                // Extract the location and the comments from lit and merge it into message
                #[cfg(procmacro2_semver_exempt)]
                {
                    let span = lit.span();
                    let mut line = span.start().line;
                    if line > 0 {
                        message.locations.push(super::Location {
                            file: self.path.clone(),
                            line,
                        });
                    }

                    line -= 1;
                    while line > 1 {
                        line -= 1;
                        let line_str = self.source_lines.get(line).unwrap().trim();
                        if line_str.starts_with("//") {
                            let line_str = line_str.trim_start_matches('/').trim_start();
                            message.comments = if let Some(ref string) = message.comments {
                                Some(format!("{}\n{}", line_str, string))
                            } else {
                                Some(line_str.to_owned())
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
        }
    }
}

fn literal_to_string(lit: &proc_macro2::Literal) -> Option<String> {
    match syn::parse_str::<syn::LitStr>(&lit.to_string()) {
        Ok(lit) => Some(lit.value()),
        Err(_) => None,
    }
}

#[test]
fn test_extract_messages() {
    fn make(msg: &str, p: &str, ctx: &str, co: &str, loc: &[usize]) -> Message {
        use super::Location;
        let opt = |x: &str| {
            if x.is_empty() {
                None
            } else {
                Some(x.to_owned())
            }
        };
        let (locations, comments) = if cfg!(procmacro2_semver_exempt) {
            (
                loc.iter()
                    .map(|l| Location {
                        file: "myfile.rs".to_owned().into(),
                        line: *l,
                    }).collect(),
                opt(co),
            )
        } else {
            (Vec::new(), None)
        };
        Message {
            msgctxt: opt(ctx),
            msgid: msg.into(),
            plural: opt(p),
            locations,
            comments,
            index: 0,
        }
    }

    let source = r##"fn foo() {
        // comment 1
        let x = tr!("Message 1");
        // comment does not count

        // comment 2
        let x = tr!("ctx" => "Message 2");
        // comment  does not count

        let x = tr!("Message 3" | "Messages 3" % x);

        // comment 4
        let x = tr!("ctx4" => "Message 4" | "Messages 4" % x);

        foobar1((foo(bar, boo)),2, "foobar1");
        foobar2(1,"foobar2", "foobar2s", f("5", "4"), "ctx");

        //recursive
        let x = tr!("rec1 {}", tr!("rec2"));

        let x = tr!(r#"raw\"ctx""# => r#"\raw\"#);

        // comment does not count : xgettext takes the comment next to the string
        let x = tr!(
            //multi line
            "multi-line \
            second line"
        );

        let d = tr!("dup1");
        let d = tr!("ctx" => "dup1");
        let d = tr!("dup1");
        let d = tr!("ctx" => "dup1");
    }"##;

    let r = vec![
        make("Message 1", "", "", "comment 1", &[3]),
        make("Message 2", "", "ctx", "comment 2", &[7]),
        make("Message 3", "Messages 3", "", "", &[10]),
        make("Message 4", "Messages 4", "ctx4", "comment 4", &[13]),
        make("foobar1", "", "", "", &[15]),
        make("foobar2", "foobar2s", "ctx", "", &[16]),
        make("rec1 {}", "", "", "recursive", &[19]),
        make("rec2", "", "", "recursive", &[19]),
        make(r#"\raw\"#, "", r#"raw\"ctx""#, "", &[21]),
        make("multi-line second line", "", "", "multi line", &[26]),
        make("dup1", "", "", "", &[30, 32]),
        make("dup1", "", "ctx", "", &[31, 33]),
    ];

    let specs = [
        ("tr".into(), Default::default()),
        (
            "foobar1".into(),
            Spec {
                args: vec![SpecArg::MsgId(3)],
                ..Default::default()
            },
        ),
        (
            "foobar2".into(),
            Spec {
                args: vec![SpecArg::MsgId(2), SpecArg::MsgId(3), SpecArg::Context(5)],
                ..Default::default()
            },
        ),
    ]
        .iter()
        .cloned()
        .collect();

    let mut results = HashMap::new();

    let mut ex = Extractor {
        results: &mut results,
        specs: &specs,
        path: &"myfile.rs".to_owned().into(),
        source_lines: split_lines(source),
    };
    use std::str::FromStr;
    ex.extract_messages(proc_macro2::TokenStream::from_str(source).unwrap())
        .unwrap();
    let mut messages: Vec<_> = ex.results.values().collect();
    messages.sort_by_key(|m| m.index);
    let mlen = messages.len();
    for (a, b) in r.iter().zip(messages) {
        let mut b = b.clone();
        b.index = 0;
        assert_eq!(*a, b);
        /*assert_eq!(a.msgid, b.msgid);
        assert_eq!(a.plural, b.plural);
        assert_eq!(a.msgctxt, b.msgctxt);
        assert_eq!(a.comment, b.comment);
        assert_eq!(a.locations, b.locations);*/
    }
    assert_eq!(r.len(), mlen);
}
