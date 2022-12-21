//! # Convert from SCSS AST to CSS
use std::io::Write;

use codemap::{CodeMap, Span};

use crate::{
    atrule::SupportsRule, error::SassResult, parse::Stmt, style::Style, value::SassNumber, Options,
};

pub(crate) fn serialize_number(
    number: &SassNumber,
    options: &Options,
    span: Span,
) -> SassResult<String> {
    let map = CodeMap::new();
    let mut serializer = Serializer::new(options, &map, false, span);

    serializer.visit_number(number)?;

    Ok(serializer.finish_for_expr())
}

pub(crate) fn inspect_number(
    number: &SassNumber,
    options: &Options,
    span: Span,
) -> SassResult<String> {
    let map = CodeMap::new();
    let mut serializer = Serializer::new(options, &map, true, span);

    serializer.visit_number(number)?;

    Ok(serializer.finish_for_expr())
}

pub(crate) struct Serializer<'a> {
    indentation: usize,
    options: &'a Options<'a>,
    inspect: bool,
    indent_width: usize,
    quote: bool,
    buffer: Vec<u8>,
    map: &'a CodeMap,
    span: Span,
}

impl<'a> Serializer<'a> {
    pub fn new(options: &'a Options<'a>, map: &'a CodeMap, inspect: bool, span: Span) -> Self {
        Self {
            inspect,
            quote: true,
            indentation: 0,
            indent_width: 2,
            options,
            buffer: Vec::new(),
            map,
            span,
        }
    }

    pub fn visit_number(&mut self, number: &SassNumber) -> SassResult<()> {
        if let Some(as_slash) = &number.as_slash {
            self.visit_number(&as_slash.0)?;
            self.buffer.push(b'/');
            self.visit_number(&as_slash.1)?;
            return Ok(());
        }

        if !self.inspect && number.unit.is_complex() {
            return Err((
                format!(
                    "{} isn't a valid CSS value.",
                    inspect_number(number, self.options, self.span)?
                ),
                self.span,
            )
                .into());
        }

        self.write_float(number.num);
        write!(&mut self.buffer, "{}", number.unit)?;

        Ok(())
    }

    fn write_float(&mut self, float: f64) {
        if float.is_infinite() && float.is_sign_negative() {
            self.buffer.extend_from_slice(b"-Infinity");
            return;
        } else if float.is_infinite() {
            self.buffer.extend_from_slice(b"Infinity");
            return;
        }

        // todo: can optimize away intermediate buffer
        let mut buffer = String::with_capacity(3);

        if float < 0.0 {
            buffer.push('-');
        }

        let num = float.abs();

        if self.options.is_compressed() && num < 1.0 {
            buffer.push_str(
                format!("{:.10}", num)[1..]
                    .trim_end_matches('0')
                    .trim_end_matches('.'),
            );
        } else {
            buffer.push_str(
                format!("{:.10}", num)
                    .trim_end_matches('0')
                    .trim_end_matches('.'),
            );
        }

        if buffer.is_empty() || buffer == "-" || buffer == "-0" {
            buffer = "0".to_owned();
        }

        self.buffer.append(&mut buffer.into_bytes());
    }

    pub fn visit_group(&mut self, stmt: Stmt, previous_was_group_end: bool) -> SassResult<()> {
        if previous_was_group_end && !self.buffer.is_empty() {
            self.buffer.push(b'\n');
        }

        // let len = self.buffer.len();

        self.visit_stmt(stmt)?;

        // if len != self.buffer.len() && !group_starts_with_media {}

        Ok(())
    }

    fn finish_for_expr(self) -> String {
        // SAFETY: todo
        unsafe { String::from_utf8_unchecked(self.buffer) }
    }

    pub fn finish(self) -> String {
        let is_not_ascii = self.buffer.iter().any(|&c| !c.is_ascii());

        // SAFETY: todo
        let mut as_string = unsafe { String::from_utf8_unchecked(self.buffer) };

        if is_not_ascii && self.options.is_compressed() {
            as_string.insert_str(0, "\u{FEFF}")
        } else if is_not_ascii {
            as_string.insert_str(0, "@charset \"UTF-8\";\n")
        }

        as_string
    }

    fn write_indentation(&mut self) {
        if self.options.is_compressed() {
            return;
        }

        self.buffer.reserve(self.indentation);
        for _ in 0..self.indentation {
            self.buffer.push(b' ');
        }
    }

    fn write_style(&mut self, style: Style) -> SassResult<()> {
        if !self.options.is_compressed() {
            self.write_indentation();
        }

        self.buffer
            .extend_from_slice(style.property.resolve_ref().as_bytes());
        self.buffer.push(b':');

        if !style.declared_as_custom_property && !self.options.is_compressed() {
            self.buffer.push(b' ');
        }

        let value_as_str = style
            .value
            .node
            .to_css_string(style.value.span, self.options.is_compressed())?;
        self.buffer.extend_from_slice(value_as_str.as_bytes());

        self.buffer.push(b';');

        if !self.options.is_compressed() {
            self.buffer.push(b'\n');
        }

        Ok(())
    }

    fn write_import(&mut self, import: String, modifiers: Option<String>) -> SassResult<()> {
        self.write_indentation();
        self.buffer.extend_from_slice(b"@import ");
        write!(&mut self.buffer, "{}", import)?;

        if let Some(modifiers) = modifiers {
            self.buffer.push(b' ');
            self.buffer.extend_from_slice(modifiers.as_bytes());
        }

        self.buffer.extend_from_slice(b";\n");

        Ok(())
    }

    fn write_comment(&mut self, comment: String, span: Span) -> SassResult<()> {
        if self.options.is_compressed() && !comment.starts_with("/*!") {
            return Ok(());
        }

        self.write_indentation();
        let col = self.map.look_up_pos(span.low()).position.column;
        let mut lines = comment.lines();

        if let Some(line) = lines.next() {
            self.buffer.extend_from_slice(line.trim_start().as_bytes());
        }

        let lines = lines
            .map(|line| {
                let diff = (line.len() - line.trim_start().len()).saturating_sub(col);
                format!("{}{}", " ".repeat(diff), line.trim_start())
            })
            .collect::<Vec<String>>()
            .join("\n");

        if !lines.is_empty() {
            write!(&mut self.buffer, "\n{}", lines)?;
        }

        if !self.options.is_compressed() {
            self.buffer.push(b'\n');
        }

        Ok(())
    }

    fn requires_semicolon(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Style(_) | Stmt::Import(_, _) => true,
            Stmt::UnknownAtRule(rule, _) => !rule.has_body,
            _ => false,
        }
    }

    fn write_children(&mut self, children: Vec<Stmt>) -> SassResult<()> {
        if self.options.is_compressed() {
            self.buffer.push(b'{');
        } else {
            self.buffer.extend_from_slice(b" {\n");
        }

        self.indentation += self.indent_width;
        for child in children {
            self.visit_stmt(child)?;
        }
        self.indentation -= self.indent_width;

        if self.options.is_compressed() {
            self.buffer.push(b'}');
        } else {
            self.write_indentation();
            self.buffer.extend_from_slice(b"}\n");
        }

        Ok(())
    }

    fn write_optional_space(&mut self) {
        if !self.options.is_compressed() {
            self.buffer.push(b' ');
        }
    }

    fn write_supports_rule(&mut self, supports_rule: SupportsRule) -> SassResult<()> {
        self.write_indentation();
        self.buffer.extend_from_slice(b"@supports");

        if !supports_rule.params.is_empty() {
            self.buffer.push(b' ');
            self.buffer
                .extend_from_slice(supports_rule.params.as_bytes());
        }

        self.write_children(supports_rule.body)?;

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: Stmt) -> SassResult<()> {
        if stmt.is_invisible() {
            return Ok(());
        }

        match stmt {
            Stmt::RuleSet { selector, body, .. } => {
                let selector = selector.into_selector().remove_placeholders();

                self.write_indentation();
                write!(&mut self.buffer, "{}", selector)?;

                self.write_children(body)?;
            }
            Stmt::Media(media_rule, ..) => {
                self.write_indentation();
                self.buffer.extend_from_slice(b"@media ");
                self.buffer.extend_from_slice(media_rule.query.as_bytes());

                self.write_children(media_rule.body)?;
            }
            Stmt::UnknownAtRule(unknown_at_rule, ..) => {
                self.write_indentation();
                self.buffer.push(b'@');
                self.buffer
                    .extend_from_slice(unknown_at_rule.name.as_bytes());

                if !unknown_at_rule.params.is_empty() {
                    write!(&mut self.buffer, " {}", unknown_at_rule.params)?;
                }

                if !unknown_at_rule.has_body {
                    debug_assert!(unknown_at_rule.body.is_empty());
                    self.buffer.extend_from_slice(b";\n");
                    return Ok(());
                } else if unknown_at_rule.body.iter().all(Stmt::is_invisible) {
                    self.buffer.extend_from_slice(b" {}\n");
                    return Ok(());
                }

                self.write_children(unknown_at_rule.body)?;
            }
            Stmt::Style(style) => self.write_style(style)?,
            Stmt::Comment(comment, span) => self.write_comment(comment, span)?,
            Stmt::KeyframesRuleSet(keyframes_rule_set) => {
                self.write_indentation();
                // todo: i bet we can do something like write_with_separator to avoid extra allocation
                let selector = keyframes_rule_set
                    .selector
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                self.buffer.extend_from_slice(selector.as_bytes());

                self.write_children(keyframes_rule_set.body)?;
            }
            Stmt::Import(import, modifier) => self.write_import(import, modifier)?,
            Stmt::Supports(supports_rule, _) => self.write_supports_rule(supports_rule)?,
        }

        Ok(())
    }
}
