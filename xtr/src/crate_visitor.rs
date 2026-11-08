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

use anyhow::Error;
use std::mem::replace;
use std::path::{Path, PathBuf};
use syn::ext::IdentExt;
use syn::visit::Visit;

/**
 *  Parse a crate to visit every module. The `visitor` function will be called for every file
 *  in the crate. The argument of the visitor are: the path of the file, the full string content
 *  of the file, and a parsed syn::File representation of this file.
 */
pub fn visit_crate<P: AsRef<Path>, V>(crate_root: P, visitor: V) -> Result<(), Error>
where
    V: FnMut(&PathBuf, &str, &syn::File) -> Result<(), Error>,
{
    let mut parser = Parser {
        scope: Scope::default(),
        mod_error: None,
        mod_visitor: visitor,
    };
    parser.parse_mod(crate_root.as_ref(), true)
}

/// Where the module being visited is
#[derive(Default)]
struct Scope {
    current_path: PathBuf,  // The current file being parsed
    mod_dir: PathBuf,       // The directory containing the files of the `mod foo;` declarations
    path_attr_dir: PathBuf, // The directory `#[path = "..."]` attributes are relative to
}

struct Parser<ModVisitor> {
    scope: Scope,
    mod_error: Option<Error>, // An error occuring while visiting the modules
    mod_visitor: ModVisitor,
}

impl<ModVisitor> Parser<ModVisitor>
where
    ModVisitor: FnMut(&PathBuf, &str, &syn::File) -> Result<(), Error>,
{
    /// Parse the file of a module and visit its sub-modules.
    ///
    /// `owns_dir` is true for files whose sub-modules are in the same directory: the crate root,
    /// `mod.rs` files, and files loaded with a `#[path]` attribute. The sub-modules of any other
    /// `foo.rs` are in the `foo/` directory.
    fn parse_mod(&mut self, mod_path: &Path, owns_dir: bool) -> Result<(), Error> {
        let s = std::fs::read_to_string(mod_path)?;
        let fi = syn::parse_file(&s)?;

        let path_attr_dir: PathBuf = mod_path.parent().unwrap().into();
        let mod_dir = if owns_dir {
            path_attr_dir.clone()
        } else {
            path_attr_dir.join(mod_path.file_stem().unwrap())
        };
        let scope = Scope {
            current_path: mod_path.into(),
            mod_dir,
            path_attr_dir,
        };
        let parent_scope = replace(&mut self.scope, scope);

        self.visit_file(&fi);
        let result = match self.mod_error.take() {
            Some(err) => Err(err),
            None => (self.mod_visitor)(&self.scope.current_path, &s, &fi),
        };

        self.scope = parent_scope;
        result
    }
}

impl<'ast, ModVisitor> Visit<'ast> for Parser<ModVisitor>
where
    ModVisitor: FnMut(&PathBuf, &str, &syn::File) -> Result<(), Error>,
{
    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        if self.mod_error.is_some() {
            return;
        }

        let mod_name = item.ident.unraw().to_string();

        if item.content.is_some() {
            let mod_dir = self.scope.mod_dir.join(&mod_name);
            let scope = Scope {
                current_path: self.scope.current_path.clone(),
                path_attr_dir: mod_dir.clone(),
                mod_dir,
            };
            let parent_scope = replace(&mut self.scope, scope);
            syn::visit::visit_item_mod(self, item);
            self.scope = parent_scope;
            return;
        }

        // Determine the path of the inner module's file
        for attr in &item.attrs {
            match &attr.meta {
                syn::Meta::NameValue(syn::MetaNameValue {
                    path,
                    value:
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(s),
                            ..
                        }),
                    ..
                }) if path.is_ident("path") => {
                    let mod_path = self.scope.path_attr_dir.join(s.value());
                    return self
                        .parse_mod(&mod_path, true)
                        .unwrap_or_else(|err| self.mod_error = Some(err));
                }
                _ => {}
            }
        }

        let adjacent = self.scope.mod_dir.join(format!("{}.rs", mod_name));
        let subdir = self.scope.mod_dir.join(&mod_name).join("mod.rs");
        for (mod_path, owns_dir) in [(adjacent, false), (subdir, true)] {
            if mod_path.is_file() {
                return self
                    .parse_mod(&mod_path, owns_dir)
                    .unwrap_or_else(|err| self.mod_error = Some(err));
            }
        }

        panic!(
            "No file with module definition for `mod {}` in file {:?}",
            mod_name, self.scope.current_path
        );
    }
}

#[cfg(test)]
mod tests {
    use super::visit_crate;

    /// Write the files in a temporary directory, and return the sorted list of files visited
    /// from the first one, relative to that directory.
    fn visited(files: &[(&str, &str)]) -> Vec<String> {
        // Removes the directory when dropped, also when visit_crate panics
        struct TempDir(std::path::PathBuf);
        impl Drop for TempDir {
            fn drop(&mut self) {
                let _ = std::fs::remove_dir_all(&self.0);
            }
        }
        static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let temp_dir =
            TempDir(std::env::temp_dir().join(format!("xtr-test-{}-{}", std::process::id(), id)));
        let dir = &temp_dir.0;
        for (path, content) in files {
            let path = dir.join(path);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, content).unwrap();
        }
        let mut result = Vec::new();
        visit_crate(dir.join(files[0].0), |path, _, _| {
            let path = path.strip_prefix(dir).unwrap();
            result.push(path.to_str().unwrap().replace('\\', "/"));
            Ok(())
        })
        .unwrap();
        result.sort();
        result
    }

    /// Check that each of the files is visited exactly once from the first one
    fn assert_visits_all(files: &[(&str, &str)]) {
        let mut expected: Vec<_> = files.iter().map(|(path, _)| path.to_string()).collect();
        expected.sort();
        assert_eq!(visited(files), expected);
    }

    #[test]
    fn crate_root() {
        // lib.rs and main.rs, also inside a directory, own the directory they are in
        assert_visits_all(&[("lib.rs", "mod foo;"), ("foo.rs", "")]);
        assert_visits_all(&[
            ("src/main.rs", "mod foo; mod bar;"),
            ("src/foo.rs", ""),
            ("src/bar/mod.rs", ""),
        ]);
        // so does any other file given as the crate root
        assert_visits_all(&[("bin/tool.rs", "mod foo;"), ("bin/foo.rs", "")]);
    }

    #[test]
    fn non_mod_rs_file() {
        // `mod bar;` in foo.rs is foo/bar.rs, not the bar.rs next to it
        assert_visits_all(&[
            ("lib.rs", "mod foo; mod bar;"),
            ("foo.rs", "mod bar;"),
            ("foo/bar.rs", ""),
            ("bar.rs", ""),
        ]);
        // or foo/bar/mod.rs, and so on at any depth
        assert_visits_all(&[
            ("lib.rs", "mod foo;"),
            ("foo.rs", "mod bar; mod baz;"),
            ("foo/bar/mod.rs", ""),
            ("foo/baz.rs", "mod qux;"),
            ("foo/baz/qux.rs", ""),
        ]);
    }

    #[test]
    fn mod_rs_file() {
        // sub-modules of mod.rs are in its own directory
        assert_visits_all(&[
            ("lib.rs", "mod foo;"),
            ("foo/mod.rs", "mod bar; mod baz;"),
            ("foo/bar.rs", "mod qux;"),
            ("foo/bar/qux.rs", ""),
            ("foo/baz/mod.rs", "mod quux;"),
            ("foo/baz/quux.rs", ""),
        ]);
    }

    #[test]
    fn inline_modules() {
        // inline modules add a directory, at any depth
        assert_visits_all(&[
            ("lib.rs", "mod a { mod b; mod c { mod d; } }"),
            ("a/b.rs", ""),
            ("a/c/d.rs", ""),
        ]);
        // in a file that is not mod.rs, below the directory named after the file
        assert_visits_all(&[
            ("lib.rs", "mod foo;"),
            ("foo.rs", "mod a { mod b; }"),
            ("foo/a/b.rs", ""),
        ]);
        // in mod.rs, below its own directory
        assert_visits_all(&[
            ("lib.rs", "mod foo;"),
            ("foo/mod.rs", "mod a { mod b; }"),
            ("foo/a/b.rs", ""),
        ]);
        // inline modules do not affect the sibling declarations that follow them
        assert_visits_all(&[
            ("lib.rs", "mod a { mod b; } mod c;"),
            ("a/b.rs", ""),
            ("c.rs", ""),
        ]);
    }

    #[test]
    fn path_attribute() {
        // relative to the file's directory, also in a file that is not mod.rs
        assert_visits_all(&[
            ("lib.rs", "#[path = \"x/y.rs\"] mod z; mod foo;"),
            ("x/y.rs", ""),
            ("foo.rs", "#[path = \"other.rs\"] mod p;"),
            ("other.rs", ""),
        ]);
        // `..` can go out of the directory, and is kept in the visited path
        assert_eq!(
            visited(&[
                ("src/lib.rs", "#[path = \"../shared/s.rs\"] mod s;"),
                ("shared/s.rs", ""),
            ]),
            ["src/../shared/s.rs", "src/lib.rs"]
        );
        // inside an inline module, relative to the inline module's directory
        assert_visits_all(&[
            ("lib.rs", "mod a { #[path = \"p.rs\"] mod q; } mod foo;"),
            ("a/p.rs", ""),
            ("foo.rs", "mod b { #[path = \"p.rs\"] mod q; }"),
            ("foo/b/p.rs", ""),
        ]);
        // the sub-modules of a #[path] file are in its own directory, whatever its name
        assert_visits_all(&[
            ("lib.rs", "#[path = \"other/p.rs\"] mod p;"),
            ("other/p.rs", "mod q;"),
            ("other/q.rs", ""),
        ]);
    }

    #[test]
    fn raw_identifiers() {
        assert_visits_all(&[
            ("lib.rs", "mod r#move; mod r#type { mod r#loop; }"),
            ("move.rs", "mod r#async;"),
            ("move/async.rs", ""),
            ("type/loop.rs", ""),
        ]);
    }

    #[test]
    fn every_declaration_is_visited() {
        // cfg attributes are ignored, so messages of all platforms are extracted
        assert_visits_all(&[
            (
                "lib.rs",
                "#[cfg(unix)] mod unix; #[cfg(windows)] mod windows;",
            ),
            ("unix.rs", ""),
            ("windows.rs", ""),
        ]);
    }

    #[test]
    #[should_panic(expected = "No file with module definition for `mod missing`")]
    fn missing_file() {
        visited(&[("lib.rs", "mod missing;")]);
    }
}
