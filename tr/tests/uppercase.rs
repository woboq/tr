use tr::{set_translator, tr, unset_translator, Translator};

struct UpperCaseTranslator;
impl crate::Translator for UpperCaseTranslator {
    fn translate<'a>(
        &'a self,
        string: &'a str,
        _context: Option<&'a str>,
    ) -> std::borrow::Cow<'a, str> {
        string.to_uppercase().into()
    }

    fn ntranslate<'a>(
        &'a self,
        n: u64,
        singular: &'a str,
        plural: &'a str,
        _context: Option<&'a str>,
    ) -> std::borrow::Cow<'a, str> {
        if n == 1 {
            singular.to_uppercase().into()
        } else {
            plural.to_uppercase().into()
        }
    }
}

#[test]
fn uppercase() {
    let arc = std::sync::Arc::new(UpperCaseTranslator);
    set_translator!(arc);

    assert_eq!(tr!("Hello"), "HELLO");
    assert_eq!(tr!("ctx" => "Hello"), "HELLO");
    assert_eq!(tr!("Hello {}", "world"), "HELLO world");
    assert_eq!(tr!("ctx" => "Hello {}", tr!("world")), "HELLO WORLD");

    assert_eq!(
        tr!("I have one item" | "I have {n} items" % 1),
        "I HAVE ONE ITEM"
    );
    assert_eq!(
        tr!("ctx" => "I have one item" | "I have {n} items" % 42),
        "I HAVE {N} ITEMS" // uppercased n is not replaced
    );

    unset_translator!();
    assert_eq!(tr!("Hello"), "Hello");
}
