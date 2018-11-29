/* Copyright (C) 2018 Olivier Goffart <ogoffart@woboq.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES
OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

//! # Internationalisation helper
//!
//! This crate maily expose a macro that wraps gettext in a convinient ways.
//! See the documentation of the [tr! macro](macro.tr.html).
//!
//! To translate a rust crate, simply wrap your string within the [`tr!` macro](macro.tr.html).
//! One can then use the `xtr` binary to extract all the translatable from a crate in a `.po`
//! file. GNU gettext tools can be used to process and translate these strings.
//!
//! The tr! macro also support support rust-like formating.
//!
//! Example:
//!
//! ```
//! #[macro_use]
//! extern crate tr;
//! fn main() {
//!     // use the tr_init macro to tell gettext where to look for translations
//!     tr_init!("/usr/share/locale/");
//!     let folder = if let Some(folder) = std::env::args().nth(1) {
//!         folder
//!     } else {
//!         println!("{}", tr!("Please give folder name"));
//!         return;
//!     };
//!     match std::fs::read_dir(&folder) {
//!         Err(e) => {
//!             println!("{}", tr!("Could not read directory '{}'\nError: {}",
//!                                 folder, e));
//!         }
//!         Ok(r) => {
//!             // Singlular/plural formating
//!             println!("{}", tr!(
//!                 "The directory {} has one file" | "The directory {} has {n} files" % r.count(),
//!                 folder
//!             ));
//!         }
//!     }
//! }
//! ```
//!

extern crate gettextrs;

#[doc(hidden)]
pub mod runtime_format {
    //! poor man's dynamic formater.
    //!
    //! This module create a simple dynamic formater which replaces '{}' or '{n}' with the
    //! argument.
    //!
    //! This does not use the runtime_fmt crate because it needs nightly compiler
    //!
    //! TODO: better error reporting and support for more replacement option

    /// The result of the runtime_format! macro.
    /// This implements the Display
    pub struct FormatArg<'a> {
        #[doc(hidden)]
        pub format_str: &'a str,
        #[doc(hidden)]
        pub args: &'a [(&'static str, &'a ::std::fmt::Display)],
    }

    impl<'a> ::std::fmt::Display for FormatArg<'a> {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            let mut arg_idx = 0;
            let mut pos = 0;
            while let Some(mut p) = self.format_str[pos..].find(|x| x=='{'|| x=='}') {
                if self.format_str.len() - pos < p + 1 {
                    break;
                }
                p += pos;

                // Skip escaped }
                if self.format_str.get(p..=p) == Some("}") {
                    self.format_str[pos..=p].fmt(f)?;
                    if self.format_str.get(p+1..=p+1) == Some("}") {
                        pos = p + 2;
                    } else {
                        // FIXME! this is an error, it should be reported  ('}' must be escaped)
                        pos = p + 1;
                    }
                    continue;
                }

                // Skip escaped {
                if self.format_str.get(p+1..=p+1) == Some("{") {
                    self.format_str[pos..=p].fmt(f)?;
                    pos = p + 2;
                    continue;
                }

                // Find the argument
                let end = if let Some(end) = self.format_str[p..].find('}') {
                    end + p
                } else {
                    // FIXME! this is an error, it should be reported
                    self.format_str[pos..=p].fmt(f)?;
                    pos = p + 1;
                    continue;
                };
                let argument = self.format_str[p + 1..end].trim();
                let pa = if p == end - 1 {
                    arg_idx += 1;
                    arg_idx - 1
                } else if let Ok(n) = argument.parse::<usize>() {
                    n
                } else if let Some(p) = self
                    .args
                    .iter()
                    .position(|x| x.0 == argument)
                {
                    p
                } else {
                    // FIXME! this is an error, it should be reported
                    self.format_str[pos..end].fmt(f)?;
                    pos = end;
                    continue;
                };

                // format the part before the '{'
                self.format_str[pos..p].fmt(f)?;
                if let Some(a) = self.args.get(pa) {
                    a.1.fmt(f)?;
                } else {
                    // FIXME! this is an error, it should be reported
                    self.format_str[p..=end].fmt(f)?;
                }
                pos = end + 1;
            }
            self.format_str[pos..].fmt(f)
        }
    }

    #[doc(hidden)]
    /// runtime_format! macro. See runtime_format module documentation.
    #[macro_export]
    macro_rules! runtime_format {
        ($fmt:expr) => {{
            // TODO! check if 'fmt' does not have {}
            format!("{}", $fmt)
        }};
        ($fmt:expr,  $($tail:tt)* ) => {{
            let format_str = $fmt;
            let fa = $crate::runtime_format::FormatArg {
                format_str: AsRef::as_ref(&format_str),
                //args: &[ $( $crate::runtime_format!(@parse_arg $e) ),* ],
                args: $crate::runtime_format!(@parse_args [] $($tail)*)
            };
            format!("{}", fa)
        }};

        (@parse_args [$($args:tt)*]) => { &[ $( $args ),* ]  };
        (@parse_args [$($args:tt)*] $name:ident) => {
            $crate::runtime_format!(@parse_args [$($args)* (stringify!($name) , &$name)])
        };
        (@parse_args [$($args:tt)*] $name:ident, $($tail:tt)*) => {
            $crate::runtime_format!(@parse_args [$($args)* (stringify!($name) , &$name)] $($tail)*)
        };
        (@parse_args [$($args:tt)*] $name:ident = $e:expr) => {
            $crate::runtime_format!(@parse_args [$($args)* (stringify!($name) , &$e)])
        };
        (@parse_args [$($args:tt)*] $name:ident = $e:expr, $($tail:tt)*) => {
            $crate::runtime_format!(@parse_args [$($args)* (stringify!($name) , &$e)] $($tail)*)
        };
        (@parse_args [$($args:tt)*] $e:expr) => {
            $crate::runtime_format!(@parse_args [$($args)* ("" , &$e)])
        };
        (@parse_args [$($args:tt)*] $e:expr, $($tail:tt)*) => {
            $crate::runtime_format!(@parse_args [$($args)* ("" , &$e)] $($tail)*)
        };
    }

    #[cfg(test)]
    mod tests {
        #[test]
        fn test_format() {
            assert_eq!(runtime_format!("Hello"), "Hello");
            assert_eq!(runtime_format!("Hello {}!", "world"), "Hello world!");
            assert_eq!(runtime_format!("Hello {0}!", "world"), "Hello world!");
            assert_eq!(
                runtime_format!("Hello -{1}- -{0}-", 40 + 5, "World"),
                "Hello -World- -45-"
            );
            assert_eq!(
                runtime_format!(format!("Hello {{}}!"), format!("{}", "world")),
                "Hello world!"
            );
            assert_eq!(
                runtime_format!("Hello -{}- -{}-", 40 + 5, "World"),
                "Hello -45- -World-"
            );
            assert_eq!(
                runtime_format!("Hello {name}!", name = "world"),
                "Hello world!"
            );
            let name = "world";
            assert_eq!(runtime_format!("Hello {name}!", name), "Hello world!");
            assert_eq!(runtime_format!("{} {}!", "Hello", name), "Hello world!");
            assert_eq!(runtime_format!("{} {name}!", "Hello", name), "Hello world!");
            assert_eq!(
                runtime_format!("{0} {name}!", "Hello", name = "world"),
                "Hello world!"
            );

            assert_eq!(runtime_format!("Hello {{0}} {}", "world"), "Hello {0} world");
        }
    }
}

#[doc(hidden)]
pub mod internal {

    fn domain_from_module(module: &str) -> &str {
        module.split("::").next().unwrap_or("")
    }

    fn mangle_context(ctx: &'static str, s: &'static str) -> String {
        format!("{}\u{4}{}", ctx, s)
    }
    fn demangle_context(r: String) -> String {
        if let Some(x) = r.split("\u{4}").last() {
            return x.to_owned();
        }
        r
    }

    pub fn gettext(module: &'static str, s: &'static str) -> String {
        gettextrs::dgettext(domain_from_module(module), s)
    }
    pub fn gettext_ctx(module: &'static str, ctx: &'static str, s: &'static str) -> String {
        demangle_context(gettextrs::dgettext(
            domain_from_module(module),
            &mangle_context(ctx, s),
        ))
    }
    pub fn ngettext(
        module: &'static str,
        singular: &'static str,
        plural: &'static str,
        n: u32,
    ) -> String {
        gettextrs::dngettext(domain_from_module(module), singular, plural, n)
    }
    pub fn ngettext_ctx(
        module: &'static str,
        ctx: &'static str,
        singular: &'static str,
        plural: &'static str,
        n: u32,
    ) -> String {
        demangle_context(gettextrs::dngettext(
            domain_from_module(module),
            &mangle_context(ctx, singular),
            &mangle_context(ctx, plural),
            n,
        ))
    }
    pub fn init<T: Into<Vec<u8>>>(module: &'static str, dir: T) {
        gettextrs::bindtextdomain::<Vec<u8>>(domain_from_module(module).into(), dir.into());

        static START: std::sync::Once = std::sync::ONCE_INIT;
        START.call_once(|| {
            gettextrs::setlocale(gettextrs::LocaleCategory::LcAll, "");
        });
    }

}

/// Macro used to translate a string.
///
/// ```
/// # #[macro_use] extern crate tr;
/// // Prints "Hello world!", or a translated version depending on the locale
/// println!("{}", tr!("Hello world!"));
/// ```
///
/// The string to translate need to be a string literal, as it has to be extracted by
/// the `xtr` tool. One can add more argument following a subset of rust formating
///
/// ```
/// # #[macro_use] extern crate tr;
/// let name = "Olivier";
/// // Prints "Hello, Olivier!",  or a translated version of that.
/// println!("{}", tr!("Hello, {}!", name));
/// ```
///
/// Plural are using the `"singular" | "plural" % count` syntax. `{n}` will be replaced
/// by the count.
///
/// ```
/// # #[macro_use] extern crate tr;
/// let number_of_items = 42;
/// println!("{}", tr!("There is one item" | "There are {n} items" % number_of_items));
/// ```
///
/// Normal formating rules can also be used:
///
/// ```
/// # #[macro_use] extern crate tr;
/// let number_of_items = 42;
/// let folder_name = "/tmp";
/// println!("{}", tr!("There is one item in folder {}"
///        | "There are {n} items in folder {}" % number_of_items, folder_name));
/// ```
///
///
/// If the same string appears several time in the crate, it is necessary to add a
/// disambiguation context, using the `"context" =>` syntax:
///
/// ```
/// # #[macro_use] extern crate tr;
/// // These two strings are both "Open" in english, but they may be different in a
/// // foreign language. Hence, a context string is necessary.
/// let action_name = tr!("File Menu" => "Open");
/// let state = tr!("Document State" => "Open");
/// ```
///
/// To enable the translation, one must first call the `tr_init!` macro once in the crate.
/// To translate the strings, one can use the `xtr` utility to extract the string,
/// and use the other GNU gettext tools to translate them.
///
#[macro_export]
macro_rules! tr {
    ($msgid:tt, $($tail:tt)* ) => {
        $crate::runtime_format!($crate::internal::gettext(module_path!(), $msgid), $($tail)*)
    };
    ($msgid:tt) => {
        $crate::runtime_format!($crate::internal::gettext(module_path!(), $msgid))
    };
    ($msgctx:tt => $msgid:tt, $($tail:tt)* ) => {
         $crate::runtime_format!($crate::internal::gettext_ctx(module_path!(), $msgctx, $msgid), $($tail)*)
    };
    ($msgid:tt | $plur:tt % $n:expr, $($tail:tt)* ) => {{
        let n = $n;
        $crate::runtime_format!($crate::internal::ngettext(module_path!(), $msgid, $plur, n as u32), $($tail)*, n=n)
    }};
    ($msgctx:tt => $msgid:tt | $plur:tt % $n:expr, $($tail:tt)* ) => {{
         let n = $n;
         $crate::runtime_format!($crate::internal::ngettext_ctx(module_path!(), $msgctx, $msgid, $plur, $n), $($tail)*, n)
    }};

    ($msgctx:tt => $msgid:tt) => {
         $crate::runtime_format!($crate::internal::gettext_ctx(module_path!(), $msgctx, $msgid))
    };
    ($msgid:tt | $plur:tt % $n:expr) => {{
        let n = $n;
        $crate::runtime_format!($crate::internal::ngettext(module_path!(), $msgid, $plur, n as u32), n)
    }};
    ($msgctx:tt => $msgid:tt | $plur:tt % $n:expr) => {{
        let n = $n;
        $crate::runtime_format!($crate::internal::ngettext_ctx(module_path!(), $msgctx, $msgid, $plur, n as u32), n)
    }};
}

/// Initialize the translation for a crate.
///
/// The macro must be called to specify the path in which the .mo files can be looked for.
#[macro_export]
macro_rules! tr_init {
    ($path:expr) => {
        $crate::internal::init(module_path!(), $path)
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(tr!("Hello {}", "world"), "Hello world");
        assert_eq!(tr!("ctx" => "Hello {}", "world"), "Hello world");

        assert_eq!(
            tr!("I have one item" | "I have {n} items" % 1),
            "I have one item"
        );
        assert_eq!(
            tr!("ctx" => "I have one item" | "I have {n} items" % 42),
            "I have 42 items"
        );
        assert_eq!(
            tr!("{} have one item" | "{} have {n} items" % 42, "I"),
            "I have 42 items"
        );
        assert_eq!(
            tr!("ctx" => "{0} have one item" | "{0} have {n} items" % 42, "I"),
            "I have 42 items"
        );
    }
}
