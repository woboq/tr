# Changelog

## 0.1.10 - 2024-08-11

### tr

 - Use the name of the crate in `set_translator!` instead of the full module (#24)
 - Allow `format_args!` as argument to `tr!` (#25)

### xtr

 - Support module with raw identifier (#19)
 - Allow `mod bar;` in `foo.rs` refer to `foo/bar/mod.rs` (#23)

## 0.1.9 - 2023-06-12

### xtr

 -  Turn --omit-header back into flag (#17)

## 0.1.8 - 2023-05-16

### xtr

 - Restore `--default-domain` flag (#15)

## 0.1.7 - 2023-04-21

### tr

 - Updated dependencies

### xtr

 - Fix Panic with test modules in external files (#12)
 - Updated dependencies

## 0.1.6 - 2021-02-14

### tr

 - Updated dependencies

### xtr

 - Fix compilation with syn 1.0.58 <https://github.com/woboq/tr/issues/8>
