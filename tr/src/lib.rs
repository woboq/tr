extern crate gettextrs;

pub mod runtime_format {
    //! poor man's dynamic formater.
    //! This module create a simple dynamic formater which replaces '{}' or '{n}' with the
    //! argument.
    //! TODO: better error reporting and support for more replacement option

    pub struct FormatArg<'a> {
        pub format_str: &'a str,
        pub args: &'a [(&'static str, &'a ::std::fmt::Display)],
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
                } else if let Ok(n) = self.format_str[p + 1..end].parse::<usize>() {
                    n
                } else if let Some(p) = self
                    .args
                    .iter()
                    .position(|x| x.0 == &self.format_str[p + 1..end])
                {
                    p
                } else {
                    return Err(::std::fmt::Error);
                };
                self.format_str[pos..p].fmt(f)?;
                self.args.get(pa).ok_or(::std::fmt::Error)?.1.fmt(f)?;
                pos += end + 1 - pos;
            }
            self.format_str[pos..].fmt(f)
        }
    }

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
        }
    }
}

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
