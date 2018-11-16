use super::{Message, Spec, SpecArg};
use failure::Error;
use proc_macro2::{TokenStream, TokenTree};
use std::collections::HashMap;

pub struct Extractor<'a> {
    pub specs: &'a HashMap<String, Spec>,
    pub results: Vec<Message>,
}

impl<'a> Extractor<'a> {
    pub fn extract_messages(&mut self, stream: TokenStream) -> Result<(), Error> {
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
                let p = literal_to_string(&literal);
                args.push(p);
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
            if let Some(Some(s)) = args.first() {
                msg.msgid = s.clone();
                self.results.push(msg)
            }
            // TODO: other cases
            return;
        }

        let mut msgset = false;
        for a in spec.args.iter() {
            match a {
                SpecArg::MsgId(i) => {
                    if msgset {
                        msg.plural = args.get(*i as usize - 1).unwrap_or(&None).clone();
                    } else if let Some(Some(s)) = args.get(*i as usize - 1) {
                        msg.msgid = s.clone();
                        msgset = true;
                    }
                }
                SpecArg::Context(i) => {
                    msg.msgctxt = args.get(*i as usize - 1).unwrap_or(&None).clone();
                }
            }
        }
        if msgset {
            self.results.push(msg)
        }
    }
}

fn literal_to_string(lit: &proc_macro2::Literal) -> Option<String> {
    match syn::parse_str::<syn::LitStr>(&lit.to_string()) {
        Ok(lit) => Some(lit.value()),
        Err(_) => None,
    }
}
