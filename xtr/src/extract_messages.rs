use super::{Location, Message, Spec, SpecArg};
use failure::Error;
use proc_macro2::{TokenStream, TokenTree};
use std::collections::HashMap;
use std::path::PathBuf;

pub fn extract_messages(
    results: &mut Vec<Message>,
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
    results: &'a mut Vec<Message>,
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
                            skip = false;
                        }

                        if let Some(TokenTree::Group(group)) = token_iter.peek() {
                            self.found_string(spec, group.stream());
                            skip = true;
                        }
                        if skip {
                            token_iter.next();
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

        let mut msg = Message::default();
        //let num_str = args.iter().position(|&r| r.is_none()) .unwrap_or_else(args.len());
        if spec.args.is_empty() {
            if let Some(Some(lit)) = args.first() {
                if let Some(s) = literal_to_string(lit) {
                    msg.msgid = s.clone();
                    self.extract_position_and_comment(&mut msg, lit.span());
                    self.results.push(msg);
                }
            }
            // TODO: other cases
            return;
        }

        let mut msgset = false;
        for a in spec.args.iter() {
            match a {
                SpecArg::MsgId(i) => {
                    if msgset {
                        msg.plural = args
                            .get(*i as usize - 1)
                            .and_then(|x| x.as_ref())
                            .and_then(|lit| literal_to_string(lit));
                    } else if let Some(Some(lit)) = args.get(*i as usize - 1) {
                        if let Some(s) = literal_to_string(lit) {
                            msg.msgid = s.clone();
                            self.extract_position_and_comment(&mut msg, lit.span());
                            msgset = true;
                        }
                    }
                }
                SpecArg::Context(i) => {
                    msg.msgctxt = args
                        .get(*i as usize - 1)
                        .and_then(|x| x.as_ref())
                        .and_then(|lit| literal_to_string(lit));
                }
            }
        }
        if msgset {
            self.results.push(msg)
        }
    }

    #[allow(unused_variables)]
    fn extract_position_and_comment(&self, message: &mut Message, span: proc_macro2::Span) {
        #[cfg(procmacro2_semver_exempt)]
        {
            let mut line = span.start().line;
            if line > 0 {
                message.locations.push(Location {
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

fn literal_to_string(lit: &proc_macro2::Literal) -> Option<String> {
    match syn::parse_str::<syn::LitStr>(&lit.to_string()) {
        Ok(lit) => Some(lit.value()),
        Err(_) => None,
    }
}
