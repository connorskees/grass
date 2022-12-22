use std::io::Write;

use codemap::{CodeMap, Span, Spanned};

use crate::{
    ast::{CssStmt, Style, SupportsRule},
    color::{Color, ColorFormat, NAMED_COLORS},
    error::SassResult,
    utils::hex_char_for,
    value::{fuzzy_equals, CalculationArg, SassCalculation, SassNumber, Value},
    Options,
};

pub(crate) fn serialize_color(color: &Color, options: &Options, span: Span) -> String {
    let map = CodeMap::new();
    let mut serializer = Serializer::new(options, &map, false, span);

    serializer.visit_color(color);

    serializer.finish_for_expr()
}

pub(crate) fn serialize_calculation(
    calculation: &SassCalculation,
    options: &Options,
    span: Span,
) -> SassResult<String> {
    let map = CodeMap::new();
    let mut serializer = Serializer::new(options, &map, false, span);

    serializer.visit_calculation(calculation)?;

    Ok(serializer.finish_for_expr())
}

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

    fn write_comma_separator(&mut self) {
        self.buffer.push(b',');
        self.write_optional_space();
    }

    fn visit_calculation(&mut self, calculation: &SassCalculation) -> SassResult<()> {
        // todo: superfluous allocation
        self.buffer
            .extend_from_slice(calculation.name.to_string().as_bytes());
        self.buffer.push(b'(');

        if let Some((last, slice)) = calculation.args.split_last() {
            for arg in slice {
                self.write_calculation_arg(arg)?;
                self.write_comma_separator();
            }

            self.write_calculation_arg(last)?;
        }

        self.buffer.push(b')');

        Ok(())
    }

    fn write_calculation_arg(&mut self, arg: &CalculationArg) -> SassResult<()> {
        match arg {
            CalculationArg::Number(num) => self.visit_number(num)?,
            CalculationArg::Calculation(calc) => {
                self.visit_calculation(calc)?;
            }
            CalculationArg::String(s) | CalculationArg::Interpolation(s) => {
                self.buffer.extend_from_slice(s.as_bytes())
            }
            CalculationArg::Operation { lhs, op, rhs } => {
                let paren_left = match &**lhs {
                    CalculationArg::Interpolation(..) => true,
                    CalculationArg::Operation { op: op2, .. } => op2.precedence() < op.precedence(),
                    _ => false,
                };

                if paren_left {
                    self.buffer.push(b'(');
                }

                self.write_calculation_arg(&**lhs)?;

                if paren_left {
                    self.buffer.push(b')');
                }

                let operator_whitespace = !self.options.is_compressed() || op.precedence() == 1;

                if operator_whitespace {
                    self.buffer.push(b' ');
                }

                // todo: avoid allocation with `write_binary_operator` method
                self.buffer.extend_from_slice(op.to_string().as_bytes());

                if operator_whitespace {
                    self.buffer.push(b' ');
                }

                let paren_right = match &**rhs {
                    CalculationArg::Interpolation(..) => true,
                    CalculationArg::Operation { op: op2, .. } => {
                        CalculationArg::parenthesize_calculation_rhs(*op, *op2)
                    }
                    _ => false,
                };

                if paren_right {
                    self.buffer.push(b'(');
                }

                self.write_calculation_arg(&**rhs)?;

                if paren_right {
                    self.buffer.push(b')');
                }
            }
        }

        Ok(())
    }

    fn write_rgb(&mut self, color: &Color) {
        let is_opaque = fuzzy_equals(color.alpha().0, 1.0);

        if is_opaque {
            self.buffer.extend_from_slice(b"rgb(");
        } else {
            self.buffer.extend_from_slice(b"rgba(");
        }

        self.write_float(color.red().0);
        self.buffer.extend_from_slice(b", ");
        self.write_float(color.green().0);
        self.buffer.extend_from_slice(b", ");
        self.write_float(color.blue().0);

        if !is_opaque {
            self.buffer.extend_from_slice(b", ");
            self.write_float(color.alpha().0);
        }

        self.buffer.push(b')');
    }

    fn write_hsl(&mut self, color: &Color) {
        let is_opaque = fuzzy_equals(color.alpha().0, 1.0);

        if is_opaque {
            self.buffer.extend_from_slice(b"hsl(");
        } else {
            self.buffer.extend_from_slice(b"hsla(");
        }

        self.write_float(color.hue().0);
        self.buffer.extend_from_slice(b"deg, ");
        self.write_float(color.saturation().0);
        self.buffer.extend_from_slice(b"%, ");
        self.write_float(color.lightness().0);
        self.buffer.extend_from_slice(b"%");

        if !is_opaque {
            self.buffer.extend_from_slice(b", ");
            self.write_float(color.alpha().0);
        }

        self.buffer.push(b')');
    }

    fn write_hex_component(&mut self, channel: u32) {
        debug_assert!(channel < 256);

        self.buffer.push(hex_char_for(channel >> 4) as u8);
        self.buffer.push(hex_char_for(channel & 0xF) as u8);
    }

    fn is_symmetrical_hex(channel: u32) -> bool {
        channel & 0xF == channel >> 4
    }

    fn can_use_short_hex(color: &Color) -> bool {
        Self::is_symmetrical_hex(color.red().0.round() as u32)
            && Self::is_symmetrical_hex(color.green().0.round() as u32)
            && Self::is_symmetrical_hex(color.blue().0.round() as u32)
    }

    pub fn visit_color(&mut self, color: &Color) {
        let red = color.red().0.round() as u8;
        let green = color.green().0.round() as u8;
        let blue = color.blue().0.round() as u8;

        let name = if fuzzy_equals(color.alpha().0, 1.0) {
            NAMED_COLORS.get_by_rgba([red, green, blue])
        } else {
            None
        };

        if self.options.is_compressed() {
            if !fuzzy_equals(color.alpha().0, 1.0) {
                self.write_rgb(color);
            } else {
                let hex_length = if Self::can_use_short_hex(color) { 4 } else { 7 };
                if name.is_some() && name.unwrap().len() <= hex_length {
                    self.buffer.extend_from_slice(name.unwrap().as_bytes());
                } else if Self::can_use_short_hex(color) {
                    self.buffer.push(b'#');
                    self.buffer.push(hex_char_for(red as u32 & 0xF) as u8);
                    self.buffer.push(hex_char_for(green as u32 & 0xF) as u8);
                    self.buffer.push(hex_char_for(blue as u32 & 0xF) as u8);
                } else {
                    self.buffer.push(b'#');
                    self.write_hex_component(red as u32);
                    self.write_hex_component(green as u32);
                    self.write_hex_component(blue as u32);
                }
            }
        } else {
            if color.format != ColorFormat::Infer {
                match &color.format {
                    ColorFormat::Rgb => self.write_rgb(color),
                    ColorFormat::Hsl => self.write_hsl(color),
                    ColorFormat::Literal(text) => self.buffer.extend_from_slice(text.as_bytes()),
                    ColorFormat::Infer => unreachable!(),
                }
                // Always emit generated transparent colors in rgba format. This works
                // around an IE bug. See sass/sass#1782.
            } else if name.is_some() && !fuzzy_equals(color.alpha().0, 0.0) {
                self.buffer.extend_from_slice(name.unwrap().as_bytes());
            } else if fuzzy_equals(color.alpha().0, 1.0) {
                self.buffer.push(b'#');
                self.write_hex_component(red as u32);
                self.write_hex_component(green as u32);
                self.write_hex_component(blue as u32);
            } else {
                self.write_rgb(color);
            }
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

    pub fn visit_group(&mut self, stmt: CssStmt, previous_was_group_end: bool) -> SassResult<()> {
        if previous_was_group_end && !self.buffer.is_empty() {
            self.buffer.push(b'\n');
        }

        self.visit_stmt(stmt)?;

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

    fn visit_value(&mut self, value: Spanned<Value>) -> SassResult<()> {
        match value.node {
            Value::Dimension {
                num,
                unit,
                as_slash,
            } => self.visit_number(&SassNumber {
                num: num.0,
                unit,
                as_slash,
            })?,
            Value::Color(color) => self.visit_color(&*color),
            Value::Calculation(calc) => self.visit_calculation(&calc)?,
            _ => {
                let value_as_str = value
                    .node
                    .to_css_string(value.span, self.options.is_compressed())?;
                self.buffer.extend_from_slice(value_as_str.as_bytes());
            }
        }

        Ok(())
    }

    fn write_style(&mut self, style: Style) -> SassResult<()> {
        if !self.options.is_compressed() {
            self.write_indentation();
        }

        self.buffer
            .extend_from_slice(style.property.resolve_ref().as_bytes());
        self.buffer.push(b':');

        // todo: _writeFoldedValue and _writeReindentedValue
        if !style.declared_as_custom_property && !self.options.is_compressed() {
            self.buffer.push(b' ');
        }

        self.visit_value(*style.value)?;

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

    fn requires_semicolon(stmt: &CssStmt) -> bool {
        match stmt {
            CssStmt::Style(_) | CssStmt::Import(_, _) => true,
            CssStmt::UnknownAtRule(rule, _) => !rule.has_body,
            _ => false,
        }
    }

    fn write_children(&mut self, children: Vec<CssStmt>) -> SassResult<()> {
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

    fn visit_stmt(&mut self, stmt: CssStmt) -> SassResult<()> {
        if stmt.is_invisible() {
            return Ok(());
        }

        match stmt {
            CssStmt::RuleSet { selector, body, .. } => {
                let selector = selector.into_selector().remove_placeholders();

                self.write_indentation();
                write!(&mut self.buffer, "{}", selector)?;

                self.write_children(body)?;
            }
            CssStmt::Media(media_rule, ..) => {
                self.write_indentation();
                self.buffer.extend_from_slice(b"@media ");
                self.buffer.extend_from_slice(media_rule.query.as_bytes());

                self.write_children(media_rule.body)?;
            }
            CssStmt::UnknownAtRule(unknown_at_rule, ..) => {
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
                } else if unknown_at_rule.body.iter().all(CssStmt::is_invisible) {
                    self.buffer.extend_from_slice(b" {}\n");
                    return Ok(());
                }

                self.write_children(unknown_at_rule.body)?;
            }
            CssStmt::Style(style) => self.write_style(style)?,
            CssStmt::Comment(comment, span) => self.write_comment(comment, span)?,
            CssStmt::KeyframesRuleSet(keyframes_rule_set) => {
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
            CssStmt::Import(import, modifier) => self.write_import(import, modifier)?,
            CssStmt::Supports(supports_rule, _) => self.write_supports_rule(supports_rule)?,
        }

        Ok(())
    }
}
