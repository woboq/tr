# Localisation of rust applications

[![docs.rs](https://docs.rs/tr/badge.svg)](https://docs.rs/tr)

This repository provides tools for localizing Rust applications, making it easier to translate your software to different languages.

There are two crates

* `tr` is a runtime library wrapping gettext (currently), in order to provide a
  convenient way to localize an application.

* `xtr` is a binary similar to GNU's `xgettext` which extract string from a rust crate.
  It can extract strings of crate using the `tr` macro from this sibling crate, or using other
  gettext based localisation crates such as [`gettext-rs`](https://crates.io/crates/gettext-rs),
  [`gettext`](https://crates.io/crates/gettext), [`rocket_i18n`](https://github.com/BaptisteGelez/rocket_i18n)

# How to translate a rust application

1. Annotate the strings in your source code with the write macro/functions. You can use
    * The `tr!` macro from this `tr` crate (still work in progress), or
    * The gettext function from the `gettext` or the `gettext-rs` crate

2. Run the `xtr` program over your crate to extract the string in a .pot file

3. Use the GNU gettext tools to merge, translate, and generate the .mo files

# About `tr!`

 * The name comes from Qt's `tr()` function. It is a short name since it will be placed on most
   string literal.
 * The macro can do rust-style formatting. This makes it possible to re-order the arguments in the translations.
 * `Hello {}` or `Hello {0}` or Hello `Hello {name}` works.
 * Currently, the default backend uses the [`gettext-rs`](https://crates.io/crates/gettext-rs) crate,
   but this could be changed to [`gettext`](https://crates.io/crates/gettext) in the future.
 * Plurals are handled by gettext, which support the different plurals forms of several languages.

## Future plans

 * Validity of the formatting in the original or translation is not done yet, but could be done in the
   future
 * More advanced formatting that would allow for gender or case can be done as an extension to the
   formatting rules. Since the macro takes the arguments directly, it will be possible to extend the
   formatting engine with a [scripting system](https://techbase.kde.org/Localization/Concepts/Transcript)
   or something like ICU MessageFormat.
 * Formatting date/number in a localized fashion.

## Example

```Rust
#[macro_use]
extern crate tr;
fn main() {
    // use the tr_init macro to tell gettext where to look for translations
    tr_init!("/usr/share/locale/");
    let folder = if let Some(folder) = std::env::args().nth(1) {
        folder
    } else {
        println!("{}", tr!("Please give folder name"));
        return;
    };
    match std::fs::read_dir(&folder) {
        Err(e) => {
            println!("{}", tr!("Could not read directory '{}'\nError: {}",
                                folder, e));
        }
        Ok(r) => {
            // Singlular/plural formating
            println!("{}", tr!(
                "The directory {} has one file" | "The directory {} has {n} files" % r.count(),
                folder
            ));
        }
    }
}
```

# About `xtr`

`xtr` is a tool that extract translated strings from the source code of a rust crate.
The tool is supposed to be compatible with any gettext based functions. But support for the
special syntax of the tr! macro has been added.

## Usage

```
xtr src/main.rs -o example.pot
```

This will extract strings from all the crate's modules and create a file `example.pot`.
You can now use the gettext tools to translate this file.

## Differences with `xgettext`

`xtr` is basically to be used in place of `xgettext` for Rust code.
`xgettext` does not currently support the rust language. We can get decent result
using the C language, but:

 * `xgettext` will not work properly if the code contains comments or string escaping that is
   not compatible with Rust's rules. (Rules for comments, or string escaping are different in
   Rust and in C. Think about raw literal, embedded comments, lifetime, ...)
   `xtr` uses the lexer from the `proc_macro2` crate so it parse rust code.
 * `xgettext` cannot be told to extract string out of a macro, while `xtr` will ignore the `!`
   token. So `gettext(...)` or `gettext!(...)` will work.
 * `xgettext` cannot handle the rust rules within the string literal. `xtr` will have no problem
   with rust's raw literal or rust's escape sequence.
 * `xtr` can also parse the `mod` keyword, and easily parse all the files in a crate.
 * Finally, `xtr` can also parse the more advanced syntax within the `tr!` macro.

# Licence

 * The `tr` crate is licensed under the [MIT](https://opensource.org/licenses/MIT) license.

 * The `xtr` program is a binary used only for development and is in the
   [GNU Affero General Public License (AGPL)](https://www.gnu.org/licenses/agpl-3.0.en.html).

# Contribution

Contributions are welcome. Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion
in this crate by you, should be licensed under the MIT license.

## Request for feedback

Please fill your suggestions as issues. Or help by commenting on https://github.com/woboq/tr/issues/1



