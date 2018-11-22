# Localisation of rust applications

This repository is an attempt to make it possible to localize rust application.
There are two crates

* `tr` is a runtime library wrapping gettext (currently), in order to provide
  convenient way to localize an application.

* `xtr` is a binary similar to GNU's `xgettext` which extract string from a rust crate.
  It can extract strings of crate using the `tr` macro from this sibling crate, or using other
  gettext based localisation crates such as (https://crates.io/crates/gettext-rs)[`gettext-rs`],
  (https://crates.io/crates/gettext)[`gettext`], (https://github.com/BaptisteGelez/rocket_i18n)[`rocket_i18n`]

# How to translate a rust application

1. Annotate the strings in your source code with the write macro/functions. You can use
  * The the `tr!` macro from this `tr` crate (still work in progress), or
  * The gettext function from the `gettext` or the `gettext-rs` crate

2. Run the `xtr` program over your crate to extract the string in a .pot file

3. Use the GNU gettext tools to merge, translate, and generate the .mo files

# About `xtr`

## Why is another tool needed? Can we not simply use `xgettext` ?

At the moment, `xgettext` does not support the rust language. We can get decent result
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

 ## Line number / comments

 In order to support line number and comment, `xtr` uses some unstable API from the proc_macro2
 crate. To enable it, one need to set this environment variable when compiling:
 `RUSTFLAGS='--cfg procmacro2_semver_exempt'`

# About `tr!`

Properties of the `tr!` macro:


# Licence

The `tr` crate is licensed under the MIT license.

The `xtr` program is a binary used only for development and is in the AGPL license.





