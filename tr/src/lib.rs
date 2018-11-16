extern crate gettextrs;

pub mod runtime_format {
    //! poor man's dynamic formater.
    //! This module create a simple dynamic formater which replaces '{}' or '{n}' with the
    //! argument.
    //! TODO: better error ereporting and support for more replacement option

    pub struct FormatArg<'a> {
        pub format_str: &'a str,
        pub positional_args: &'a [&'a ::std::fmt::Display],
    }

    impl<'a> ::std::fmt::Display for FormatArg<'a> {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            let mut arg_idx = 0;
            let mut pos = 0;
            while let Some(mut p) = self.format_str[pos..].find('{') {
                if self.format_str.len() - pos < p + 1 {
                    break;
                }
                p += pos;
                let end = if let Some(end) = self.format_str[p..].find('}') {
                    end + p
                } else {
                    return Err(::std::fmt::Error);
                };
                let pa = if p == end - 1 {
                    arg_idx += 1;
                    arg_idx - 1
                } else {
                    self.format_str[p + 1..end]
                        .parse::<usize>()
                        .map_err(|_| ::std::fmt::Error)?
                };
                self.format_str[pos..p].fmt(f)?;
                self.positional_args
                    .get(pa)
                    .ok_or(::std::fmt::Error)?
                    .fmt(f)?;
                pos += end + 1 - pos;
            }
            self.format_str[pos..].fmt(f)
        }
    }

    #[macro_export]
    macro_rules! runtime_format {
        ($fmt:expr $(, $e:expr )* ) => {{
            let format_str = $fmt;
            let fa = $crate::runtime_format::FormatArg {
                format_str: AsRef::as_ref(&format_str),
                positional_args: &[ $( &$e ),* ],
            };
            format!("{}", fa)
        }};
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
        }
    }
}

pub mod internal {

    fn domain_from_module(module: &str) -> &str {
        module.split("::").next().unwrap_or("")
    }

    pub fn gettext(module: &'static str, s: &'static str) -> String {
        gettextrs::dgettext(domain_from_module(module), s)
    }
    pub fn gettext_ctx(module: &'static str, ctx: &'static str, s: &'static str) -> String {
        let r = gettextrs::dgettext(domain_from_module(module), &format!("{}\u{4}{}", ctx, s));
        if let Some(x) = r.split("\u{4}").last() {
            return x.to_owned();
        }
        r
    }

    pub fn init<T: Into<Vec<u8>>>(module: &'static str, dir: T) {
        gettextrs::bindtextdomain::<Vec<u8>>(domain_from_module(module).into(), dir.into());

        static START: std::sync::Once = std::sync::ONCE_INIT;
        START.call_once(|| {
            gettextrs::setlocale(gettextrs::LocaleCategory::LcAll, "");
        });
    }

}

#[macro_export]
macro_rules! tr {
    ($msgid:expr $(, $e:expr )* ) => {
        $crate::runtime_format!( $crate::internal::gettext(module_path!(), $msgid)  $(,$e)*)
    };
    ($msgctx:expr => $msgid:expr $(, $e:expr )* ) => {
         $crate::runtime_format!( $crate::internal::gettext_ctx(module_path!(), $msgctx, $msgid) $(,$e),*)
    };
}

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
    }
}
