/* Copyright (C) 2025 SixtyFPS GmbH <info@slint.dev>

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

use std::collections::HashMap;

/// Use this type to load `.po` files directly in your application for translations.
///
/// Construct the `PoTranslator` from either a path via [`Self::from_path`] or a vec of
/// data via [`Self::from_vec_u8`].
///
/// `PoTranslator` implements the [`crate::Translator`] trait and can be passed to
/// [`crate::set_translator!`].
#[cfg(feature = "po-translator")]
pub struct PoTranslator(RSPoLibTranslator);

#[cfg(feature = "po-translator")]
impl PoTranslator {
    /// Constructs a `PoTranslator` from the given path.
    pub fn from_path(path: &std::path::Path) -> Result<Self, MoPoTranslatorLoadError> {
        let options = rspolib::FileOptions::from(path);
        Ok(Self(
            rspolib::pofile(options)
                .map_err(|parse_error| MoPoTranslatorLoadError::PoParseError(parse_error.into()))
                .and_then(RSPoLibTranslator::try_from)?,
        ))
    }

    /// Constructs a `PoTranslator` from the given raw vec u8 that must be valid `.po` file contents.
    pub fn from_vec_u8(data: Vec<u8>) -> Result<Self, MoPoTranslatorLoadError> {
        let options = rspolib::FileOptions::from(data);
        Ok(Self(
            rspolib::pofile(options)
                .map_err(|parse_error| MoPoTranslatorLoadError::PoParseError(parse_error.into()))
                .and_then(RSPoLibTranslator::try_from)?,
        ))
    }
}

#[cfg(feature = "po-translator")]
impl crate::Translator for PoTranslator {
    fn translate<'a>(
        &'a self,
        string: &'a str,
        context: Option<&'a str>,
    ) -> std::borrow::Cow<'a, str> {
        self.0.translate(string, context)
    }

    fn ntranslate<'a>(
        &'a self,
        n: u64,
        singular: &'a str,
        plural: &'a str,
        context: Option<&'a str>,
    ) -> std::borrow::Cow<'a, str> {
        self.0.ntranslate(n, singular, plural, context)
    }
}

/// Use this type to load `.mo` files directly in your application for translations.
///
/// Construct the `MoTranslator` from either a path via [`Self::from_path`] or a vec of
/// data via [`Self::from_vec_u8`].
///
/// `MoTranslator` implements the [`crate::Translator`] trait and can be passed to
/// [`crate::set_translator!`].
#[cfg(feature = "mo-translator")]
pub struct MoTranslator(RSPoLibTranslator);

#[cfg(feature = "mo-translator")]
impl MoTranslator {
    /// Constructs a `MoTranslator` from the given path.
    pub fn from_path(path: &std::path::Path) -> Result<Self, MoPoTranslatorLoadError> {
        let options = rspolib::FileOptions::from(path);
        Ok(Self(
            rspolib::mofile(options)
                .map_err(|parse_error| MoPoTranslatorLoadError::MoParseError(parse_error.into()))
                .and_then(RSPoLibTranslator::try_from)?,
        ))
    }

    /// Constructs a `MoTranslator` from the given raw vec u8 that must be valid `.mo` file contents.
    pub fn from_vec_u8(data: Vec<u8>) -> Result<Self, MoPoTranslatorLoadError> {
        let options = rspolib::FileOptions::from(data);
        Ok(Self(
            rspolib::mofile(options)
                .map_err(|parse_error| MoPoTranslatorLoadError::MoParseError(parse_error.into()))
                .and_then(RSPoLibTranslator::try_from)?,
        ))
    }
}

#[cfg(feature = "mo-translator")]
impl crate::Translator for MoTranslator {
    fn translate<'a>(
        &'a self,
        string: &'a str,
        context: Option<&'a str>,
    ) -> std::borrow::Cow<'a, str> {
        self.0.translate(string, context)
    }

    fn ntranslate<'a>(
        &'a self,
        n: u64,
        singular: &'a str,
        plural: &'a str,
        context: Option<&'a str>,
    ) -> std::borrow::Cow<'a, str> {
        self.0.ntranslate(n, singular, plural, context)
    }
}

/// Use the `RSPoLibTranslator` to load messages from a `.po` or `.mo` files.
///
/// Convert your [`rspolib::POFile`] or [`rspolib::MOFile`] into this type
/// using [`RSPoLibTranslator::try_from`].
///
/// `RSPoLibTranslator` implements the [`crate::Translator`] trait and can then
/// be passed to [`crate::set_translator!`].
struct RSPoLibTranslator {
    /// Translations are indexed by message id, optional, plural message id, and optional context.
    translations: HashMap<TranslationKey, Translation>,
    plural_rules: plural_rule_parser::Expression,
}

impl RSPoLibTranslator {
    fn new(
        entries: impl IntoIterator<Item = POMOEntry>,
        metadata: &HashMap<String, String>,
    ) -> Result<Self, MoPoTranslatorLoadError> {
        let translations = entries
            .into_iter()
            .filter_map(|entry| {
                let translation = if entry.msgid_plural.is_some() {
                    Some(Translation::Plural(entry.msgstr_plural.into_boxed_slice()))
                } else {
                    entry.msgstr.map(|msgstr| Translation::Singular(msgstr))
                };

                let key = TranslationKey {
                    message_id: entry.msgid,
                    plural_message_id: entry.msgid_plural,
                    context: entry.msgctxt,
                };

                translation.map(|t| (key, t))
            })
            .collect();

        let plural_rules = metadata
            .get("Plural-Forms")
            .and_then(|entry| {
                entry.split(';').find_map(|sub_entry| {
                    let (key, expression) = sub_entry.split_once('=')?;
                    if key == "plural" {
                        Some(
                            plural_rule_parser::parse_rule_expression(expression).map_err(
                                |parse_error| MoPoTranslatorLoadError::InvalidPluralRules {
                                    rules: expression.to_string(),
                                    error: parse_error.0.to_string(),
                                },
                            ),
                        )
                    } else {
                        None
                    }
                })
            })
            .unwrap_or_else(|| Ok(plural_rule_parser::parse_rule_expression("n != 1").unwrap()))?;

        Ok(RSPoLibTranslator {
            translations,
            plural_rules,
        })
    }
}

impl TryFrom<rspolib::MOFile> for RSPoLibTranslator {
    type Error = MoPoTranslatorLoadError;
    fn try_from(mofile: rspolib::MOFile) -> Result<Self, Self::Error> {
        RSPoLibTranslator::new(
            mofile.entries.into_iter().map(
                |rspolib::MOEntry {
                     msgid,
                     msgstr,
                     msgstr_plural,
                     msgid_plural,
                     msgctxt,
                 }| {
                    POMOEntry {
                        msgid,
                        msgstr,
                        msgid_plural,
                        msgstr_plural,
                        msgctxt,
                    }
                },
            ),
            &mofile.metadata,
        )
    }
}

impl TryFrom<rspolib::POFile> for RSPoLibTranslator {
    type Error = MoPoTranslatorLoadError;
    fn try_from(mofile: rspolib::POFile) -> Result<Self, Self::Error> {
        RSPoLibTranslator::new(
            mofile.entries.into_iter().map(
                |rspolib::POEntry {
                     msgid,
                     msgstr,
                     msgstr_plural,
                     msgid_plural,
                     msgctxt,
                     ..
                 }| {
                    POMOEntry {
                        msgid,
                        msgstr,
                        msgid_plural,
                        msgstr_plural,
                        msgctxt,
                    }
                },
            ),
            &mofile.metadata,
        )
    }
}

#[derive(PartialEq, Eq, Hash)]
struct TranslationKey {
    message_id: String,
    plural_message_id: Option<String>,
    context: Option<String>,
}

enum Translation {
    Singular(String),
    Plural(Box<[String]>),
}

struct POMOEntry {
    msgid: String,
    msgstr: Option<String>,
    msgid_plural: Option<String>,
    msgstr_plural: Vec<String>,
    msgctxt: Option<String>,
}

/// This error type is returned when creating a [`PoTranslator`] or [`MoTranslator`]
/// and an error occurding during parsing.
#[non_exhaustive]
pub enum MoPoTranslatorLoadError {
    /// This variant describes a failure during parsing of the `.po` file.
    PoParseError(Box<dyn std::error::Error>),
    /// This variant describes a failure during parsing of the `.mo` file.
    MoParseError(Box<dyn std::error::Error>),
    /// This variant describes a failure during parsing of the plural rules.
    InvalidPluralRules {
        /// A copy of the plural rules that could not be parsed.
        rules: String,
        /// The error that occured during parsing of the plural rules.
        error: String,
    },
}

impl std::error::Error for MoPoTranslatorLoadError {}

impl core::fmt::Display for MoPoTranslatorLoadError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::PoParseError(error) => {
                write!(f, "Error parsing `po` file: {}", error)
            }
            Self::MoParseError(error) => {
                write!(f, "Error parsing `mo` file: {}", error)
            }
            Self::InvalidPluralRules { rules, error } => {
                write!(f, "Error parsing plural rules '{}': {}", rules, error)
            }
        }
    }
}

impl core::fmt::Debug for MoPoTranslatorLoadError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl crate::Translator for RSPoLibTranslator {
    fn translate<'a>(
        &'a self,
        message_id: &'a str,
        context: Option<&'a str>,
    ) -> std::borrow::Cow<'a, str> {
        std::borrow::Cow::Borrowed(
            self.translations
                .get(&(message_id, None, context) as &dyn TranslationLookup)
                .and_then(|translation| match translation {
                    Translation::Singular(message) => Some(message.as_str()),
                    Translation::Plural(_) => None,
                })
                .unwrap_or(message_id),
        )
    }

    fn ntranslate<'a>(
        &'a self,
        n: u64,
        singular: &'a str,
        plural: &'a str,
        context: Option<&'a str>,
    ) -> std::borrow::Cow<'a, str> {
        std::borrow::Cow::Borrowed(
            self.translations
                .get(&(singular, Some(plural), context) as &dyn TranslationLookup)
                .and_then(|translation| match translation {
                    Translation::Singular(_) => None,
                    Translation::Plural(items) => {
                        let translation_pick = self.plural_rules.evaluate(n);
                        items.get(translation_pick as usize).map(|s| s.as_str())
                    }
                })
                .unwrap_or_else(|| if n == 1 { singular } else { plural }),
        )
    }
}

/// Helper trait to permit lookup of translations without copying the key,
/// by using a dyn trait object as the borrowed type for the (String, Option<String>)
/// key tuple.
trait TranslationLookup {
    fn message_id(&self) -> &str;
    fn plural_message_id(&self) -> Option<&str>;
    fn context(&self) -> Option<&str>;
}

impl TranslationLookup for TranslationKey {
    fn message_id(&self) -> &str {
        &self.message_id
    }

    fn plural_message_id(&self) -> Option<&str> {
        self.plural_message_id.as_deref()
    }

    fn context(&self) -> Option<&str> {
        self.context.as_deref()
    }
}

impl<'a> TranslationLookup for (&'a str, Option<&'a str>, Option<&'a str>) {
    fn message_id(&self) -> &str {
        self.0
    }

    fn plural_message_id(&self) -> Option<&str> {
        self.1
    }

    fn context(&self) -> Option<&str> {
        self.2
    }
}

impl std::hash::Hash for dyn TranslationLookup + '_ {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.message_id().hash(state);
        self.plural_message_id().hash(state);
        self.context().hash(state);
    }
}

impl std::cmp::PartialEq for dyn TranslationLookup + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.message_id() == other.message_id()
            && self.plural_message_id() == other.plural_message_id()
            && self.context() == other.context()
    }
}

impl std::cmp::Eq for dyn TranslationLookup + '_ {}

impl<'a> std::borrow::Borrow<dyn TranslationLookup + 'a> for TranslationKey {
    fn borrow(&self) -> &(dyn TranslationLookup + 'a) {
        self
    }
}

mod plural_rule_parser {
    pub enum BinaryOp {
        And,
        Or,
        Modulo,
        Equal,
        NotEqual,
        Greater,
        Smaller,
        GreaterOrEqual,
        SmallerOrEqual,
    }

    pub enum SubExpression {
        NumberLiteral(u64),
        NVariable,
        Condition {
            condition: u16,
            true_expr: u16,
            false_expr: u16,
        },
        BinaryOp {
            op: BinaryOp,
            lhs: u16,
            rhs: u16,
        },
    }

    impl SubExpression {
        fn evaluate(&self, sub_expressions: &[SubExpression], n: u64) -> u64 {
            match self {
                Self::NumberLiteral(value) => *value,
                Self::NVariable => n,
                Self::Condition {
                    condition,
                    true_expr,
                    false_expr,
                } => {
                    if sub_expressions[*condition as usize].evaluate(sub_expressions, n) != 0 {
                        sub_expressions[*true_expr as usize].evaluate(sub_expressions, n)
                    } else {
                        sub_expressions[*false_expr as usize].evaluate(sub_expressions, n)
                    }
                }
                Self::BinaryOp { op, lhs, rhs } => {
                    let lhs_value = sub_expressions[*lhs as usize].evaluate(sub_expressions, n);
                    let rhs_value = sub_expressions[*rhs as usize].evaluate(sub_expressions, n);
                    match op {
                        BinaryOp::And => (lhs_value != 0 && rhs_value != 0) as u64,
                        BinaryOp::Or => (lhs_value != 0 || rhs_value != 0) as u64,
                        BinaryOp::Modulo => lhs_value % rhs_value,
                        BinaryOp::Equal => (lhs_value == rhs_value) as u64,
                        BinaryOp::NotEqual => (lhs_value != rhs_value) as u64,
                        BinaryOp::Greater => (lhs_value > rhs_value) as u64,
                        BinaryOp::Smaller => (lhs_value < rhs_value) as u64,
                        BinaryOp::GreaterOrEqual => (lhs_value >= rhs_value) as u64,
                        BinaryOp::SmallerOrEqual => (lhs_value <= rhs_value) as u64,
                    }
                }
            }
        }
    }

    #[cfg(test)]
    struct DisplayExpression<'a>(usize, &'a [SubExpression]);

    #[cfg(test)]
    impl<'a> DisplayExpression<'a> {
        fn sub(&self, index: u16) -> Self {
            Self(index as usize, self.1)
        }
    }

    #[cfg(test)]
    impl<'a> std::fmt::Display for DisplayExpression<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.1[self.0] {
                SubExpression::NumberLiteral(value) => write!(f, "{}", value),
                SubExpression::NVariable => write!(f, "n"),
                SubExpression::Condition {
                    condition,
                    true_expr,
                    false_expr,
                } => {
                    write!(
                        f,
                        "({} ? {} : {})",
                        self.sub(*condition),
                        self.sub(*true_expr),
                        self.sub(*false_expr)
                    )
                }
                SubExpression::BinaryOp { op, lhs, rhs } => {
                    let op_str = match op {
                        BinaryOp::And => "&",
                        BinaryOp::Or => "|",
                        BinaryOp::Modulo => "%",
                        BinaryOp::Equal => "=",
                        BinaryOp::NotEqual => "!=",
                        BinaryOp::Greater => ">",
                        BinaryOp::Smaller => "<",
                        BinaryOp::GreaterOrEqual => "≥",
                        BinaryOp::SmallerOrEqual => "≤",
                    };
                    write!(f, "({} {} {})", self.sub(*lhs), op_str, self.sub(*rhs))
                }
            }
        }
    }

    #[derive(Default)]
    struct ExpressionBuilder(Vec<SubExpression>);
    impl ExpressionBuilder {
        fn add(&mut self, sub_expr: SubExpression) -> u16 {
            let index = self.0.len();
            self.0.push(sub_expr);
            index as u16
        }
    }

    pub struct Expression {
        sub_expressions: Box<[SubExpression]>,
    }

    impl From<ExpressionBuilder> for Expression {
        fn from(expression_builder: ExpressionBuilder) -> Self {
            Self {
                sub_expressions: expression_builder.0.into_boxed_slice(),
            }
        }
    }

    impl Expression {
        pub fn evaluate(&self, n: u64) -> usize {
            self.sub_expressions
                .last()
                .map(|expr| expr.evaluate(&self.sub_expressions, n) as usize)
                .unwrap_or(0)
        }
    }

    pub struct ParseError<'a>(pub &'static str, &'a [u8]);
    impl std::fmt::Debug for ParseError<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "ParseError({}, rest={:?})",
                self.0,
                std::str::from_utf8(self.1).unwrap()
            )
        }
    }
    pub fn parse_rule_expression(string: &str) -> Result<Expression, ParseError<'_>> {
        let ascii = string.as_bytes();
        let mut expression_builder = ExpressionBuilder::default();
        let s = parse_expression(ascii, &mut expression_builder)?;
        if !s.rest.is_empty() {
            return Err(ParseError("extra character in string", s.rest));
        }
        if matches!(s.ty, Ty::Boolean) {
            let true_expr = expression_builder.add(SubExpression::NumberLiteral(1));
            let false_expr = expression_builder.add(SubExpression::NumberLiteral(0));
            expression_builder.add(SubExpression::Condition {
                condition: s.expr,
                true_expr,
                false_expr,
            });
        }
        Ok(expression_builder.into())
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum Ty {
        Number,
        Boolean,
    }

    struct ParsingState<'a> {
        expr: u16,
        rest: &'a [u8],
        ty: Ty,
    }

    impl ParsingState<'_> {
        fn skip_whitespace(self) -> Self {
            let rest = skip_whitespace(self.rest);
            Self { rest, ..self }
        }
    }

    /// `<condition> ('?' <expr> : <expr> )?`
    fn parse_expression<'a>(
        string: &'a [u8],
        builder: &mut ExpressionBuilder,
    ) -> Result<ParsingState<'a>, ParseError<'a>> {
        let string = skip_whitespace(string);
        let state = parse_condition(string, builder)?.skip_whitespace();
        if state.ty != Ty::Boolean {
            return Ok(state);
        }
        if let Some(rest) = state.rest.strip_prefix(b"?") {
            let s1 = parse_expression(rest, builder)?.skip_whitespace();
            let rest = s1
                .rest
                .strip_prefix(b":")
                .ok_or(ParseError("expected ':'", s1.rest))?;
            let s2 = parse_expression(rest, builder)?;
            if s1.ty != s2.ty {
                return Err(ParseError(
                    "incompatible types in ternary operator",
                    s2.rest,
                ));
            }
            Ok(ParsingState {
                expr: builder.add(SubExpression::Condition {
                    condition: state.expr,
                    true_expr: s1.expr,
                    false_expr: s2.expr,
                }),
                rest: skip_whitespace(s2.rest),
                ty: s2.ty,
            })
        } else {
            Ok(state)
        }
    }

    /// `<and_expr> ("||" <condition>)?`
    fn parse_condition<'a>(
        string: &'a [u8],
        builder: &mut ExpressionBuilder,
    ) -> Result<ParsingState<'a>, ParseError<'a>> {
        let string = skip_whitespace(string);
        let state = parse_and_expr(string, builder)?.skip_whitespace();
        if state.rest.is_empty() {
            return Ok(state);
        }
        if let Some(rest) = state.rest.strip_prefix(b"||") {
            let state2 = parse_condition(rest, builder)?;
            if state.ty != Ty::Boolean || state2.ty != Ty::Boolean {
                return Err(ParseError("incompatible types in || operator", state2.rest));
            }
            Ok(ParsingState {
                expr: builder.add(SubExpression::BinaryOp {
                    lhs: state.expr,
                    rhs: state2.expr,
                    op: BinaryOp::Or,
                }),
                ty: Ty::Boolean,
                rest: skip_whitespace(state2.rest),
            })
        } else {
            Ok(state)
        }
    }

    /// `<cmp_expr> ("&&" <and_expr>)?`
    fn parse_and_expr<'a>(
        string: &'a [u8],
        builder: &mut ExpressionBuilder,
    ) -> Result<ParsingState<'a>, ParseError<'a>> {
        let string = skip_whitespace(string);
        let state = parse_cmp_expr(string, builder)?.skip_whitespace();
        if state.rest.is_empty() {
            return Ok(state);
        }
        if let Some(rest) = state.rest.strip_prefix(b"&&") {
            let state2 = parse_and_expr(rest, builder)?;
            if state.ty != Ty::Boolean || state2.ty != Ty::Boolean {
                return Err(ParseError("incompatible types in || operator", state2.rest));
            }
            Ok(ParsingState {
                expr: builder.add(SubExpression::BinaryOp {
                    lhs: state.expr,
                    rhs: state2.expr,
                    op: BinaryOp::And,
                }),
                ty: Ty::Boolean,
                rest: skip_whitespace(state2.rest),
            })
        } else {
            Ok(state)
        }
    }

    /// `<value> ('=='|'!='|'<'|'>'|'<='|'>=' <cmp_expr>)?`
    fn parse_cmp_expr<'a>(
        string: &'a [u8],
        builder: &mut ExpressionBuilder,
    ) -> Result<ParsingState<'a>, ParseError<'a>> {
        let string = skip_whitespace(string);
        let mut state = parse_value(string, builder)?;
        state.rest = skip_whitespace(state.rest);
        if state.rest.is_empty() {
            return Ok(state);
        }

        for (token, op) in [
            (b"==" as &[u8], BinaryOp::Equal),
            (b"!=", BinaryOp::NotEqual),
            (b"<=", BinaryOp::SmallerOrEqual),
            (b">=", BinaryOp::GreaterOrEqual),
            (b"<", BinaryOp::Smaller),
            (b">", BinaryOp::Greater),
        ] {
            if let Some(rest) = state.rest.strip_prefix(token) {
                let state2 = parse_cmp_expr(rest, builder)?;
                if state.ty != Ty::Number || state2.ty != Ty::Number {
                    return Err(ParseError("incompatible types in comparison", state2.rest));
                }
                return Ok(ParsingState {
                    expr: builder.add(SubExpression::BinaryOp {
                        lhs: state.expr,
                        rhs: state2.expr,
                        op,
                    }),
                    ty: Ty::Boolean,
                    rest: skip_whitespace(state2.rest),
                });
            }
        }
        Ok(state)
    }

    /// `<term> ('%' <term>)?`
    fn parse_value<'a>(
        string: &'a [u8],
        builder: &mut ExpressionBuilder,
    ) -> Result<ParsingState<'a>, ParseError<'a>> {
        let string = skip_whitespace(string);
        let mut state = parse_term(string, builder)?;
        state.rest = skip_whitespace(state.rest);
        if state.rest.is_empty() {
            return Ok(state);
        }
        if let Some(rest) = state.rest.strip_prefix(b"%") {
            let state2 = parse_term(rest, builder)?;
            if state.ty != Ty::Number || state2.ty != Ty::Number {
                return Err(ParseError("incompatible types in % operator", state2.rest));
            }
            Ok(ParsingState {
                expr: builder.add(SubExpression::BinaryOp {
                    lhs: state.expr,
                    rhs: state2.expr,
                    op: BinaryOp::Modulo,
                }),
                ty: Ty::Number,
                rest: skip_whitespace(state2.rest),
            })
        } else {
            Ok(state)
        }
    }

    fn parse_term<'a>(
        string: &'a [u8],
        builder: &mut ExpressionBuilder,
    ) -> Result<ParsingState<'a>, ParseError<'a>> {
        let string = skip_whitespace(string);
        let state = match string
            .first()
            .ok_or(ParseError("unexpected end of string", string))?
        {
            b'n' => ParsingState {
                expr: builder.add(SubExpression::NVariable),
                rest: &string[1..],
                ty: Ty::Number,
            },
            b'(' => {
                let mut s = parse_expression(&string[1..], builder)?;
                s.rest = s
                    .rest
                    .strip_prefix(b")")
                    .ok_or(ParseError("expected ')'", s.rest))?;
                s
            }
            x if x.is_ascii_digit() => {
                let (n, rest) = parse_number(string)?;
                ParsingState {
                    expr: builder.add(SubExpression::NumberLiteral(n as _)),
                    rest,
                    ty: Ty::Number,
                }
            }
            _ => return Err(ParseError("unexpected token", string)),
        };
        Ok(state)
    }
    fn parse_number(string: &[u8]) -> Result<(i32, &[u8]), ParseError<'_>> {
        let end = string
            .iter()
            .position(|&c| !c.is_ascii_digit())
            .unwrap_or(string.len());
        let n = std::str::from_utf8(&string[..end])
            .expect("string is valid utf-8")
            .parse()
            .map_err(|_| ParseError("can't parse number", string))?;
        Ok((n, &string[end..]))
    }
    fn skip_whitespace(mut string: &[u8]) -> &[u8] {
        // slice::trim_ascii_start when MSRV >= 1.80
        while !string.is_empty() && string[0].is_ascii_whitespace() {
            string = &string[1..];
        }
        string
    }

    #[test]
    fn test_parse_rule_expression() {
        #[track_caller]
        fn p(string: &str) -> String {
            let expr = parse_rule_expression(string).expect("parse error");
            DisplayExpression(
                expr.sub_expressions
                    .len()
                    .checked_sub(1)
                    .expect("no expression found"),
                &expr.sub_expressions,
            )
            .to_string()
        }

        // en
        assert_eq!(p("n != 1"), "((n != 1) ? 1 : 0)");
        // fr
        assert_eq!(p("n > 1"), "((n > 1) ? 1 : 0)");
        // ar
        assert_eq!(
            p("(n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 ? 4 : 5)"),
            "((n = 0) ? 0 : ((n = 1) ? 1 : ((n = 2) ? 2 : ((((n % 100) ≥ 3) & ((n % 100) ≤ 10)) ? 3 : (((n % 100) ≥ 11) ? 4 : 5)))))"
        );
        // ga
        assert_eq!(p("n==1 ? 0 : n==2 ? 1 : (n>2 && n<7) ? 2 :(n>6 && n<11) ? 3 : 4"), "((n = 1) ? 0 : ((n = 2) ? 1 : (((n > 2) & (n < 7)) ? 2 : (((n > 6) & (n < 11)) ? 3 : 4))))");
        // ja
        assert_eq!(p("0"), "0");
        // pl
        assert_eq!(
            p("(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)"),
            "((n = 1) ? 0 : ((((n % 10) ≥ 2) & (((n % 10) ≤ 4) & (((n % 100) < 10) | ((n % 100) ≥ 20)))) ? 1 : 2))",
        );

        // ru
        assert_eq!(
            p("(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)"),
            "((((n % 10) = 1) & ((n % 100) != 11)) ? 0 : ((((n % 10) ≥ 2) & (((n % 10) ≤ 4) & (((n % 100) < 10) | ((n % 100) ≥ 20)))) ? 1 : 2))",
        );
    }
}

#[test]
fn single_message() {
    use crate::Translator;

    let mut synthetic_mofile = rspolib::MOFile::new(rspolib::FileOptions::default());
    synthetic_mofile.entries.push(rspolib::MOEntry {
        msgid: "Big Error".to_string(),
        msgstr: Some("Großer Fehler".to_string()),
        ..Default::default()
    });
    synthetic_mofile.entries.push(rspolib::MOEntry {
        msgid: "Small Error".to_string(),
        msgstr: Some("Kleiner Fehler".to_string()),
        ..Default::default()
    });
    synthetic_mofile.entries.push(rspolib::MOEntry {
        msgid: "Small Error".to_string(),
        msgstr: Some("Kleiner Fehler im Kontext".to_string()),
        msgctxt: Some("some context".to_string()),
        ..Default::default()
    });

    let translator = RSPoLibTranslator::try_from(synthetic_mofile).unwrap();
    assert_eq!(translator.translate("Big Error", None), "Großer Fehler");
    assert_eq!(translator.translate("Small Error", None), "Kleiner Fehler");
    assert_eq!(
        translator.translate("Small Error", Some("some context")),
        "Kleiner Fehler im Kontext"
    );
}

#[test]
fn plural_message() {
    use crate::Translator;

    let mut synthetic_mofile = rspolib::MOFile::new(rspolib::FileOptions::default());
    synthetic_mofile.entries.push(rspolib::MOEntry {
        msgid: "{n} file".to_string(),
        msgid_plural: Some("{n} files".to_string()),
        msgstr: None,
        msgstr_plural: vec!["{n} Datei".to_string(), "{n} Dateien".to_string()],
        ..Default::default()
    });
    synthetic_mofile.metadata.insert(
        "Plural-Forms".to_string(),
        "nplurals=2; plural=(n != 1)".to_string(),
    );

    let translator = RSPoLibTranslator::try_from(synthetic_mofile).unwrap();
    assert_eq!(
        translator.ntranslate(1, "{n} file", "{n} files", None),
        "{n} Datei"
    );
    assert_eq!(
        translator.ntranslate(0, "{n} file", "{n} files", None),
        "{n} Dateien"
    );
    assert_eq!(
        translator.ntranslate(3, "{n} file", "{n} files", None),
        "{n} Dateien"
    );
}
