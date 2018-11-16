use failure::Error;
use std::fs::File;
use std::io::Read;
use std::mem::swap;
use std::path::{Path, PathBuf};
use syn;
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
        current_path: PathBuf::default(),
        mod_dir: PathBuf::default(),
        mod_error: None,
        mod_visitor: visitor,
    };
    parser.parse_mod(crate_root)
}

struct Parser<ModVisitor> {
    current_path: PathBuf, // The current file being parsed
    mod_dir: PathBuf,
    mod_error: Option<Error>, // An error occuring while visiting the modules
    mod_visitor: ModVisitor,
}

impl<ModVisitor> Parser<ModVisitor>
where
    ModVisitor: FnMut(&PathBuf, &str, &syn::File) -> Result<(), Error>,
{
    fn parse_mod<P: AsRef<Path>>(&mut self, mod_path: P) -> Result<(), Error> {
        let mut s = String::new();
        let mut f = File::open(&mod_path)?;
        f.read_to_string(&mut s)?;

        let fi = syn::parse_file(&s)?;

        let mut current_path = mod_path.as_ref().into();
        let mut mod_dir = mod_path.as_ref().parent().unwrap().into();

        swap(&mut self.current_path, &mut current_path);
        swap(&mut self.mod_dir, &mut mod_dir);

        self.visit_file(&fi);
        if let Some(err) = self.mod_error.take() {
            return Err(err);
        }

        (self.mod_visitor)(&current_path, &s, &fi)?;

        swap(&mut self.current_path, &mut current_path);
        swap(&mut self.mod_dir, &mut mod_dir);

        Ok(())
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

        if item.content.is_some() {
            let mut parent = self.mod_dir.join(item.ident.to_string());
            swap(&mut self.mod_dir, &mut parent);
            syn::visit::visit_item_mod(self, item);
            swap(&mut self.mod_dir, &mut parent);
            return;
        }

        // Determine the path of the inner module's file
        for attr in &item.attrs {
            match attr.interpret_meta() {
                Some(syn::Meta::NameValue(syn::MetaNameValue {
                    ident: ref id,
                    lit: syn::Lit::Str(ref s),
                    ..
                }))
                    if id == "path" =>
                {
                    let mod_path = self.mod_dir.join(&s.value());
                    return self
                        .parse_mod(mod_path)
                        .unwrap_or_else(|err| self.mod_error = Some(err));
                }
                _ => {}
            }
        }

        let mod_name = item.ident.to_string();
        let mut subdir = self.mod_dir.join(mod_name.clone());
        subdir.push("mod.rs");
        if subdir.is_file() {
            return self
                .parse_mod(subdir)
                .unwrap_or_else(|err| self.mod_error = Some(err));
        }
        let adjacent = self.mod_dir.join(&format!("{}.rs", mod_name));
        if adjacent.is_file() {
            return self
                .parse_mod(adjacent)
                .unwrap_or_else(|err| self.mod_error = Some(err));
        }

        panic!(
            "No file with module definition for `mod {}` in file {:?}",
            mod_name, self.current_path
        );
    }
}
