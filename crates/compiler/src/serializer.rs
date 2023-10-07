use std::io::Write;

use codemap::{CodeMap, Span};

use crate::{
    ast::{CssStmt, MediaQuery, Style, SupportsRule},
    color::{Color, ColorFormat, NAMED_COLORS},
    common::{BinaryOp, Brackets, ListSeparator, QuoteKind},
    error::SassResult,
    selector::{
        Combinator, ComplexSelector, ComplexSelectorComponent, CompoundSelector, Namespace, Pseudo,
        SelectorList, SimpleSelector,
    },
    utils::{hex_char_for, StrWrite},
    value::{
        fuzzy_equals, ArgList, CalculationArg, CalculationName, SassCalculation, SassFunction,
        SassMap, SassNumber, Value,
    },
    Options,
};

pub(crate) fn serialize_selector_list(
    list: &SelectorList,
    options: &Options,
    span: Span,
) -> SassResult<String> {
    let map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &map, false, span, list.is_ascii(), &mut buf);

    serializer.write_selector_list(list)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) fn serialize_calculation_arg(
    arg: &CalculationArg,
    options: &Options,
    span: Span,
) -> SassResult<String> {
    let map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &map, false, span, true, &mut buf);

    serializer.write_calculation_arg(arg)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) fn serialize_number(
    number: &SassNumber,
    options: &Options,
    span: Span,
) -> SassResult<String> {
    let map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &map, false, span, true, &mut buf);

    serializer.visit_number(number)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) fn serialize_value(val: &Value, options: &Options, span: Span) -> SassResult<String> {
    let map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &map, false, span, val.is_ascii(), &mut buf);

    serializer.visit_value(val, span)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) fn inspect_value(val: &Value, options: &Options, span: Span) -> SassResult<String> {
    let map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &map, true, span, val.is_ascii(), &mut buf);

    serializer.visit_value(val, span)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) fn inspect_float(number: f64, options: &Options, span: Span) -> SassResult<String> {
    let map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &map, true, span, true, &mut buf);

    serializer.write_float(number)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) fn inspect_map(map: &SassMap, options: &Options, span: Span) -> SassResult<String> {
    let code_map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &code_map, true, span, map.is_ascii(), &mut buf);

    serializer.visit_map(map, span)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) fn inspect_function_ref(
    func: &SassFunction,
    options: &Options,
    span: Span,
) -> SassResult<String> {
    let code_map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &code_map, true, span, true, &mut buf);

    serializer.visit_function_ref(func, span)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) fn inspect_number(
    number: &SassNumber,
    options: &Options,
    span: Span,
) -> SassResult<String> {
    let map = CodeMap::new();
    let mut buf = Vec::new();
    let mut serializer = Serializer::new(options, &map, true, span, true, &mut buf);

    serializer.visit_number(number)?;

    // SAFETY: todo
    Ok(unsafe { String::from_utf8_unchecked(buf) })
}

pub(crate) struct Serializer<'a, W> {
    indentation: usize,
    options: &'a Options<'a>,
    inspect: bool,
    indent_width: usize,
    // todo: use this field
    _quote: bool,
    is_ascii: bool,
    writer: W,
    map: &'a CodeMap,
    span: Span,
}

impl<'a, W> Serializer<'a, W>
where
    W: Write,
{
    pub fn new(
        options: &'a Options<'a>,
        map: &'a CodeMap,
        inspect: bool,
        span: Span,
        is_ascii: bool,
        writer: W,
    ) -> Self {
        Self {
            inspect,
            _quote: true,
            indentation: 0,
            indent_width: 2,
            options,
            writer,
            is_ascii,
            map,
            span,
        }
    }

    fn omit_spaces_around_complex_component(&self, component: &ComplexSelectorComponent) -> bool {
        self.options.is_compressed()
            && matches!(component, ComplexSelectorComponent::Combinator(..))
    }

    /// Returns whether a byte was written.
    fn write_pseudo_selector(&mut self, pseudo: &Pseudo) -> SassResult<bool> {
        if let Some(sel) = &pseudo.selector {
            if pseudo.name == "not" && sel.is_invisible() {
                return Ok(false);
            }
        }

        self.writer.write_ch(b':')?;

        if !pseudo.is_syntactic_class {
            self.writer.write_ch(b':')?;
        }

        self.writer.write_all(pseudo.name.as_bytes())?;

        if pseudo.argument.is_none() && pseudo.selector.is_none() {
            return Ok(true);
        }

        self.writer.write_ch(b'(')?;
        if let Some(arg) = &pseudo.argument {
            self.writer.write_all(arg.as_bytes())?;
            if pseudo.selector.is_some() {
                self.writer.write_ch(b' ')?;
            }
        }

        if let Some(sel) = &pseudo.selector {
            self.write_selector_list(sel)?;
        }

        self.writer.write_ch(b')')?;

        Ok(true)
    }

    fn write_namespace(&mut self, namespace: &Namespace) -> SassResult<()> {
        match namespace {
            Namespace::Empty => self.writer.write_ch(b'|')?,
            Namespace::Asterisk => self.writer.write_all(b"*|")?,
            Namespace::Other(namespace) => {
                self.writer.write_all(namespace.as_bytes())?;
                self.writer.write_ch(b'|')?;
            }
            Namespace::None => {}
        };

        Ok(())
    }

    /// Returns whether a byte was written.
    fn write_simple_selector(&mut self, simple: &SimpleSelector) -> SassResult<bool> {
        Ok(match simple {
            SimpleSelector::Id(name) => {
                write!(&mut self.writer, "#{}", name)?;
                true
            }
            SimpleSelector::Class(name) => {
                write!(&mut self.writer, ".{}", name)?;
                true
            }
            SimpleSelector::Placeholder(name) => {
                write!(&mut self.writer, "%{}", name)?;
                true
            }
            SimpleSelector::Universal(namespace) => {
                self.write_namespace(namespace)?;
                self.writer.write_ch(b'*')?;
                true
            }
            SimpleSelector::Pseudo(pseudo) => self.write_pseudo_selector(pseudo)?,
            SimpleSelector::Type(name) => {
                self.write_namespace(&name.namespace)?;
                self.writer.write_all(name.ident.as_bytes())?;
                true
            }
            SimpleSelector::Attribute(attr) => {
                write!(&mut self.writer, "{}", attr)?;
                true // an attribute starts with '['
            }
            SimpleSelector::Parent(..) => unreachable!("It should not be possible to format `&`."),
        })
    }

    fn write_compound_selector(&mut self, compound: &CompoundSelector) -> SassResult<()> {
        let mut did_write = false;
        for simple in &compound.components {
            if did_write {
                self.write_simple_selector(simple)?;
            } else {
                if self.write_simple_selector(simple)? {
                    did_write = true;
                }
            }
        }

        // If we emit an empty compound, it's because all of the components got
        // optimized out because they match all selectors, so we just emit the
        // universal selector.
        if !did_write {
            self.writer.write_ch(b'*')?;
        }

        Ok(())
    }

    fn write_complex_selector_component(
        &mut self,
        component: &ComplexSelectorComponent,
    ) -> SassResult<()> {
        match component {
            ComplexSelectorComponent::Combinator(Combinator::NextSibling) => {
                self.writer.write_ch(b'+')?
            }
            ComplexSelectorComponent::Combinator(Combinator::Child) => {
                self.writer.write_ch(b'>')?
            }
            ComplexSelectorComponent::Combinator(Combinator::FollowingSibling) => {
                self.writer.write_ch(b'~')?
            }
            ComplexSelectorComponent::Compound(compound) => {
                self.write_compound_selector(compound)?
            }
        };

        Ok(())
    }

    fn write_complex_selector(&mut self, complex: &ComplexSelector) -> SassResult<()> {
        let mut last_component = None;

        for component in &complex.components {
            if let Some(c) = last_component {
                if !self.omit_spaces_around_complex_component(c)
                    && !self.omit_spaces_around_complex_component(component)
                {
                    self.writer.write_ch(b' ')?;
                }
            }
            self.write_complex_selector_component(component)?;
            last_component = Some(component);
        }

        Ok(())
    }

    fn write_selector_list(&mut self, list: &SelectorList) -> SassResult<()> {
        let complexes = list.components.iter().filter(|c| !c.is_invisible());

        let mut first = true;

        for complex in complexes {
            if first {
                first = false;
            } else {
                self.writer.write_ch(b',')?;
                if complex.line_break {
                    self.write_newline()?;
                } else {
                    self.write_optional_space()?;
                }
            }
            self.write_complex_selector(complex)?;
        }
        Ok(())
    }

    fn write_newline(&mut self) -> SassResult<()> {
        if !self.options.is_compressed() {
            self.writer.write_ch(b'\n')?;
        }
        Ok(())
    }

    fn write_comma_separator(&mut self) -> SassResult<()> {
        self.writer.write_ch(b',')?;
        self.write_optional_space()
    }

    fn write_calculation_name(&mut self, name: CalculationName) -> SassResult<()> {
        match name {
            CalculationName::Calc => self.writer.write_all(b"calc"),
            CalculationName::Min => self.writer.write_all(b"min"),
            CalculationName::Max => self.writer.write_all(b"max"),
            CalculationName::Clamp => self.writer.write_all(b"clamp"),
        }?;

        Ok(())
    }

    fn visit_calculation(&mut self, calculation: &SassCalculation) -> SassResult<()> {
        self.write_calculation_name(calculation.name)?;
        self.writer.write_ch(b'(')?;

        if let Some((last, slice)) = calculation.args.split_last() {
            for arg in slice {
                self.write_calculation_arg(arg)?;
                self.write_comma_separator()?;
            }

            self.write_calculation_arg(last)?;
        }

        self.writer.write_ch(b')')?;

        Ok(())
    }

    fn write_calculation_arg(&mut self, arg: &CalculationArg) -> SassResult<()> {
        match arg {
            CalculationArg::Number(num) => self.visit_number(num)?,
            CalculationArg::Calculation(calc) => {
                self.visit_calculation(calc)?;
            }
            CalculationArg::String(s) | CalculationArg::Interpolation(s) => {
                self.writer.write_all(s.as_bytes())?;
            }
            CalculationArg::Operation { lhs, op, rhs } => {
                let paren_left = match &**lhs {
                    CalculationArg::Interpolation(..) => true,
                    CalculationArg::Operation { op: op2, .. } => op2.precedence() < op.precedence(),
                    _ => false,
                };

                if paren_left {
                    self.writer.write_ch(b'(')?;
                }

                self.write_calculation_arg(lhs)?;

                if paren_left {
                    self.writer.write_ch(b')')?;
                }

                let operator_whitespace =
                    !self.options.is_compressed() || matches!(op, BinaryOp::Plus | BinaryOp::Minus);

                if operator_whitespace {
                    self.writer.write_ch(b' ')?;
                }

                write!(&mut self.writer, "{}", op)?;

                if operator_whitespace {
                    self.writer.write_ch(b' ')?;
                }

                let paren_right = match &**rhs {
                    CalculationArg::Interpolation(..) => true,
                    CalculationArg::Operation { op: op2, .. } => {
                        CalculationArg::parenthesize_calculation_rhs(*op, *op2)
                    }
                    _ => false,
                };

                if paren_right {
                    self.writer.write_ch(b'(')?;
                }

                self.write_calculation_arg(rhs)?;

                if paren_right {
                    self.writer.write_ch(b')')?;
                }
            }
        }

        Ok(())
    }

    fn write_rgb(&mut self, color: &Color) -> SassResult<()> {
        let is_opaque = fuzzy_equals(color.alpha().0, 1.0);

        if is_opaque {
            self.writer.write_all(b"rgb(")?;
        } else {
            self.writer.write_all(b"rgba(")?;
        }

        self.write_float(color.red().0)?;
        self.writer.write_ch(b',')?;
        self.write_optional_space()?;
        self.write_float(color.green().0)?;
        self.writer.write_ch(b',')?;
        self.write_optional_space()?;
        self.write_float(color.blue().0)?;

        if !is_opaque {
            self.writer.write_ch(b',')?;
            self.write_optional_space()?;
            self.write_float(color.alpha().0)?;
        }

        self.writer.write_ch(b')')?;

        Ok(())
    }

    fn write_hsl(&mut self, color: &Color) -> SassResult<()> {
        let is_opaque = fuzzy_equals(color.alpha().0, 1.0);

        if is_opaque {
            self.writer.write_all(b"hsl(")?;
        } else {
            self.writer.write_all(b"hsla(")?;
        }

        self.write_float(color.hue().0)?;
        self.writer.write_all(b"deg, ")?;
        self.write_float(color.saturation().0)?;
        self.writer.write_all(b"%, ")?;
        self.write_float(color.lightness().0)?;
        self.writer.write_ch(b'%')?;

        if !is_opaque {
            self.writer.write_all(b", ")?;
            self.write_float(color.alpha().0)?;
        }

        self.writer.write_ch(b')')?;

        Ok(())
    }

    fn write_hex_component(&mut self, channel: u32) -> SassResult<()> {
        debug_assert!(channel < 256);

        self.writer.write_ch(hex_char_for(channel >> 4) as u8)?;
        self.writer.write_ch(hex_char_for(channel & 0xF) as u8)?;

        Ok(())
    }

    fn is_symmetrical_hex(channel: u32) -> bool {
        channel & 0xF == channel >> 4
    }

    fn can_use_short_hex(color: &Color) -> bool {
        Self::is_symmetrical_hex(color.red().0.round() as u32)
            && Self::is_symmetrical_hex(color.green().0.round() as u32)
            && Self::is_symmetrical_hex(color.blue().0.round() as u32)
    }

    pub fn visit_color(&mut self, color: &Color) -> SassResult<()> {
        let red = color.red().0.round() as u8;
        let green = color.green().0.round() as u8;
        let blue = color.blue().0.round() as u8;

        let name = if fuzzy_equals(color.alpha().0, 1.0) {
            NAMED_COLORS.get_by_rgba([red, green, blue])
        } else {
            None
        };

        #[allow(clippy::unnecessary_unwrap)]
        if self.options.is_compressed() {
            if fuzzy_equals(color.alpha().0, 1.0) {
                let hex_length = if Self::can_use_short_hex(color) { 4 } else { 7 };
                if name.is_some() && name.unwrap().len() <= hex_length {
                    self.writer.write_all(name.unwrap().as_bytes())?;
                } else if Self::can_use_short_hex(color) {
                    self.writer.write_ch(b'#')?;
                    self.writer.write_ch(hex_char_for(red as u32 & 0xF) as u8)?;
                    self.writer
                        .write_ch(hex_char_for(green as u32 & 0xF) as u8)?;
                    self.writer
                        .write_ch(hex_char_for(blue as u32 & 0xF) as u8)?;
                } else {
                    self.writer.write_ch(b'#')?;
                    self.write_hex_component(red as u32)?;
                    self.write_hex_component(green as u32)?;
                    self.write_hex_component(blue as u32)?;
                }
            } else {
                self.write_rgb(color)?;
            }
        } else if color.format != ColorFormat::Infer {
            match &color.format {
                ColorFormat::Rgb => self.write_rgb(color)?,
                ColorFormat::Hsl => self.write_hsl(color)?,
                ColorFormat::Literal(text) => self.writer.write_all(text.as_bytes())?,
                ColorFormat::Infer => unreachable!(),
            }
            // Always emit generated transparent colors in rgba format. This works
            // around an IE bug. See sass/sass#1782.
        } else if name.is_some() && !fuzzy_equals(color.alpha().0, 0.0) {
            self.writer.write_all(name.unwrap().as_bytes())?;
        } else if fuzzy_equals(color.alpha().0, 1.0) {
            self.writer.write_ch(b'#')?;
            self.write_hex_component(red as u32)?;
            self.write_hex_component(green as u32)?;
            self.write_hex_component(blue as u32)?;
        } else {
            self.write_rgb(color)?;
        }

        Ok(())
    }

    fn write_media_query(&mut self, query: &MediaQuery) -> SassResult<()> {
        if let Some(modifier) = &query.modifier {
            write!(&mut self.writer, "{} ", modifier)?;
        }

        if let Some(media_type) = &query.media_type {
            self.writer.write_all(media_type.as_bytes())?;

            if !query.conditions.is_empty() {
                self.writer.write_all(b" and ")?;
            }
        }

        if query.conditions.len() == 1 && query.conditions.first().unwrap().starts_with("(not ") {
            let condition = query.conditions.first().unwrap();
            write!(
                &mut self.writer,
                "not {}",
                &condition["(not ".len()..condition.len() - 1]
            )?;
        } else {
            let operator = if query.conjunction { " and " } else { " or " };
            self.writer
                .write_all(query.conditions.join(operator).as_bytes())?;
        }

        Ok(())
    }

    pub fn visit_number(&mut self, number: &SassNumber) -> SassResult<()> {
        if let Some(as_slash) = &number.as_slash {
            self.visit_number(&as_slash.0)?;
            self.writer.write_ch(b'/')?;
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

        self.write_float(number.num.0)?;
        write!(&mut self.writer, "{}", number.unit)?;

        Ok(())
    }

    fn write_float(&mut self, float: f64) -> SassResult<()> {
        if float.is_infinite() && float.is_sign_negative() {
            self.writer.write_all(b"-Infinity")?;
            return Ok(());
        } else if float.is_infinite() {
            self.writer.write_all(b"Infinity")?;
            return Ok(());
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

        write!(&mut self.writer, "{}", buffer)?;

        Ok(())
    }

    pub fn visit_group(
        &mut self,
        stmt: CssStmt,
        prev_was_group_end: bool,
        prev_requires_semicolon: bool,
        is_first: bool,
    ) -> SassResult<()> {
        if prev_requires_semicolon {
            self.writer.write_ch(b';')?;
        }

        if !is_first {
            self.write_optional_newline()?;
        }

        if prev_was_group_end && !is_first {
            self.write_optional_newline()?;
        }

        self.visit_stmt(stmt)?;

        Ok(())
    }

    pub fn start(&mut self) -> SassResult<()> {
        if !self.is_ascii && self.options.is_compressed() && self.options.allows_charset {
            self.writer.write_all("\u{FEFF}".as_bytes())?;
        } else if !self.is_ascii && self.options.allows_charset {
            self.writer.write_all(b"@charset \"UTF-8\";\n")?;
        }

        Ok(())
    }

    pub fn finish(mut self, prev_requires_semicolon: bool, wrote: bool) -> SassResult<()> {
        if prev_requires_semicolon {
            self.writer.write_ch(b';')?;
        }

        if wrote {
            self.write_optional_newline()?;
        }

        Ok(())
    }

    fn write_indentation(&mut self) -> SassResult<()> {
        if self.options.is_compressed() {
            return Ok(());
        }

        for _ in 0..self.indentation {
            self.writer.write_ch(b' ')?;
        }
        Ok(())
    }

    fn write_list_separator(&mut self, sep: ListSeparator) -> SassResult<()> {
        match (sep, self.options.is_compressed()) {
            (ListSeparator::Space | ListSeparator::Undecided, _) => self.writer.write_ch(b' '),
            (ListSeparator::Comma, true) => self.writer.write_ch(b','),
            (ListSeparator::Comma, false) => self.writer.write_all(b", "),
            (ListSeparator::Slash, true) => self.writer.write_ch(b'/'),
            (ListSeparator::Slash, false) => self.writer.write_all(b" / "),
        }?; // needed to coerce `io::Result` to `SassResult`.
        Ok(())
    }

    fn elem_needs_parens(sep: ListSeparator, elem: &Value) -> bool {
        match elem {
            Value::List(elems, sep2, brackets) => {
                if elems.len() < 2 {
                    return false;
                }

                if *brackets == Brackets::Bracketed {
                    return false;
                }

                match sep {
                    ListSeparator::Comma => *sep2 == ListSeparator::Comma,
                    ListSeparator::Slash => {
                        *sep2 == ListSeparator::Comma || *sep2 == ListSeparator::Slash
                    }
                    _ => *sep2 != ListSeparator::Undecided,
                }
            }
            _ => false,
        }
    }

    fn visit_list(
        &mut self,
        list_elems: &[Value],
        sep: ListSeparator,
        brackets: Brackets,
        span: Span,
    ) -> SassResult<()> {
        if brackets == Brackets::Bracketed {
            self.writer.write_ch(b'[')?;
        } else if list_elems.is_empty() {
            if !self.inspect {
                return Err(("() isn't a valid CSS value.", span).into());
            }

            self.writer.write_all(b"()")?;
            return Ok(());
        }

        let is_singleton = self.inspect
            && list_elems.len() == 1
            && (sep == ListSeparator::Comma || sep == ListSeparator::Slash);

        if is_singleton && brackets != Brackets::Bracketed {
            self.writer.write_ch(b'(')?;
        }

        let (mut x, mut y);
        let elems: &mut dyn Iterator<Item = &Value> = if self.inspect {
            x = list_elems.iter();
            &mut x
        } else {
            y = list_elems.iter().filter(|elem| !elem.is_blank());
            &mut y
        };

        let mut elems = elems.peekable();

        while let Some(elem) = elems.next() {
            if self.inspect {
                let needs_parens = Self::elem_needs_parens(sep, elem);
                if needs_parens {
                    self.writer.write_ch(b'(')?;
                }

                self.visit_value(elem, span)?;

                if needs_parens {
                    self.writer.write_ch(b')')?;
                }
            } else {
                self.visit_value(elem, span)?;
            }

            if elems.peek().is_some() {
                self.write_list_separator(sep)?;
            }
        }

        if is_singleton {
            match sep {
                ListSeparator::Comma => self.writer.write_ch(b','),
                ListSeparator::Slash => self.writer.write_ch(b'/'),
                _ => unreachable!(),
            }?;

            if brackets != Brackets::Bracketed {
                self.writer.write_ch(b')')?;
            }
        }

        if brackets == Brackets::Bracketed {
            self.writer.write_ch(b']')?;
        }

        Ok(())
    }

    fn write_map_element(&mut self, value: &Value, span: Span) -> SassResult<()> {
        let needs_parens = matches!(value, Value::List(_, ListSeparator::Comma, Brackets::None));

        if needs_parens {
            self.writer.write_ch(b'(')?;
        }

        self.visit_value(value, span)?;

        if needs_parens {
            self.writer.write_ch(b')')?;
        }

        Ok(())
    }

    fn visit_map(&mut self, map: &SassMap, span: Span) -> SassResult<()> {
        if !self.inspect {
            return Err((
                format!(
                    "{} isn't a valid CSS value.",
                    inspect_map(map, self.options, span)?
                ),
                span,
            )
                .into());
        }

        self.writer.write_ch(b'(')?;

        let mut elems = map.iter().peekable();

        while let Some((k, v)) = elems.next() {
            self.write_map_element(&k.node, k.span)?;
            self.writer.write_all(b": ")?;
            self.write_map_element(v, k.span)?;
            if elems.peek().is_some() {
                self.writer.write_all(b", ")?;
            }
        }

        self.writer.write_ch(b')')?;

        Ok(())
    }

    fn visit_unquoted_string(&mut self, string: &str) -> SassResult<()> {
        let mut after_newline = false;

        for c in string.bytes() {
            match c {
                b'\n' => {
                    self.writer.write_ch(b' ')?;
                    after_newline = true;
                }
                b' ' => {
                    if !after_newline {
                        self.writer.write_ch(b' ')?;
                    }
                }
                _ => {
                    self.writer.write_ch(c)?;
                    after_newline = false;
                }
            }
        }
        Ok(())
    }

    fn visit_quoted_string(&mut self, force_double_quote: bool, string: &str) -> SassResult<()> {
        let mut has_single_quote = false;
        let mut has_double_quote = false;

        // todo: avoid this allocation by peeking for double quotes (").
        let mut buffer = Vec::new();

        if force_double_quote {
            buffer.push(b'"');
        }
        let mut iter = string.as_bytes().iter().copied().peekable();
        while let Some(c) = iter.next() {
            match c {
                b'\'' => {
                    if force_double_quote {
                        buffer.push(b'\'');
                    } else if has_double_quote {
                        return self.visit_quoted_string(true, string);
                    } else {
                        has_single_quote = true;
                        buffer.push(b'\'');
                    }
                }
                b'"' => {
                    if force_double_quote {
                        buffer.push(b'\\');
                        buffer.push(b'"');
                    } else if has_single_quote {
                        return self.visit_quoted_string(true, string);
                    } else {
                        has_double_quote = true;
                        buffer.push(b'"');
                    }
                }
                b'\x00'..=b'\x08' | b'\x0A'..=b'\x1F' => {
                    buffer.push(b'\\');
                    if c as u32 > 0xF {
                        buffer.push(hex_char_for(c as u32 >> 4) as u8);
                    }
                    buffer.push(hex_char_for(c as u32 & 0xF) as u8);

                    let next = match iter.peek() {
                        Some(v) => *v,
                        None => break,
                    };

                    if next.is_ascii_hexdigit() || next == b' ' || next == b'\t' {
                        buffer.push(b' ');
                    }
                }
                b'\\' => {
                    buffer.push(b'\\');
                    buffer.push(b'\\');
                }
                _ => buffer.push(c),
            }
        }

        if force_double_quote {
            buffer.push(b'"');
            self.writer.write_all(&buffer)?;
        } else {
            let quote = if has_double_quote { b'\'' } else { b'"' };
            self.writer.write_ch(quote)?;
            self.writer.write_all(&buffer)?;
            self.writer.write_ch(quote)?;
        };
        Ok(())
    }

    fn visit_function_ref(&mut self, func: &SassFunction, span: Span) -> SassResult<()> {
        if !self.inspect {
            return Err((
                format!(
                    "{} isn't a valid CSS value.",
                    inspect_function_ref(func, self.options, span)?
                ),
                span,
            )
                .into());
        }

        self.writer.write_all(b"get-function(")?;
        self.visit_quoted_string(false, func.name().as_str())?;
        self.writer.write_ch(b')')?;

        Ok(())
    }

    fn visit_arglist(&mut self, arglist: &ArgList, span: Span) -> SassResult<()> {
        self.visit_list(&arglist.elems, ListSeparator::Comma, Brackets::None, span)
    }

    fn visit_value(&mut self, value: &Value, span: Span) -> SassResult<()> {
        match value {
            Value::Dimension(num) => self.visit_number(num)?,
            Value::Color(color) => self.visit_color(color)?,
            Value::Calculation(calc) => self.visit_calculation(calc)?,
            Value::List(elems, sep, brackets) => self.visit_list(elems, *sep, *brackets, span)?,
            Value::True => self.writer.write_all(b"true")?,
            Value::False => self.writer.write_all(b"false")?,
            Value::Null => {
                if self.inspect {
                    self.writer.write_all(b"null")?
                }
            }
            Value::Map(map) => self.visit_map(map, span)?,
            Value::FunctionRef(func) => self.visit_function_ref(func, span)?,
            Value::String(s, QuoteKind::Quoted) => self.visit_quoted_string(false, s)?,
            Value::String(s, QuoteKind::None) => self.visit_unquoted_string(s)?,
            Value::ArgList(arglist) => self.visit_arglist(arglist, span)?,
        }

        Ok(())
    }

    fn write_style(&mut self, style: Style) -> SassResult<()> {
        if !self.options.is_compressed() {
            self.write_indentation()?;
        }

        self.writer
            .write_all(style.property.resolve_ref().as_bytes())?;
        self.writer.write_ch(b':')?;

        // todo: _writeFoldedValue and _writeReindentedValue
        if !style.declared_as_custom_property && !self.options.is_compressed() {
            self.writer.write_ch(b' ')?;
        }

        self.visit_value(&style.value.node, style.value.span)?;

        Ok(())
    }

    fn write_import(&mut self, import: &str, modifiers: Option<String>) -> SassResult<()> {
        self.write_indentation()?;
        write!(&mut self.writer, "@import {}", import)?;

        if let Some(modifiers) = modifiers {
            self.writer.write_ch(b' ')?;
            self.writer.write_all(modifiers.as_bytes())?;
        }

        Ok(())
    }

    fn write_comment(&mut self, comment: &str, span: Span) -> SassResult<()> {
        if self.options.is_compressed() && !comment.starts_with("/*!") {
            return Ok(());
        }

        self.write_indentation()?;
        let col = self.map.look_up_pos(span.low()).position.column;
        let mut lines = comment.lines();

        if let Some(line) = lines.next() {
            self.writer.write_all(line.trim_start().as_bytes())?;
        }

        for line in lines {
            let diff = (line.len() - line.trim_start().len()).saturating_sub(col);
            write!(
                &mut self.writer,
                "\n{}{}",
                " ".repeat(diff),
                line.trim_start()
            )?;
        }

        Ok(())
    }

    fn write_children(&mut self, mut children: Vec<CssStmt>) -> SassResult<()> {
        if self.options.is_compressed() {
            self.writer.write_ch(b'{')?;
        } else {
            self.writer.write_all(b" {\n")?;
        }

        self.indentation += self.indent_width;

        let last = children.pop();

        for child in children {
            let needs_semicolon = requires_semicolon(&child);
            let did_write = self.visit_stmt(child)?;

            if !did_write {
                continue;
            }

            if needs_semicolon {
                self.writer.write_ch(b';')?;
            }

            self.write_optional_newline()?;
        }

        if let Some(last) = last {
            let needs_semicolon = requires_semicolon(&last);
            let did_write = self.visit_stmt(last)?;

            if did_write {
                if needs_semicolon && !self.options.is_compressed() {
                    self.writer.write_ch(b';')?;
                }

                self.write_optional_newline()?;
            }
        }

        self.indentation -= self.indent_width;

        if self.options.is_compressed() {
            self.writer.write_ch(b'}')?;
        } else {
            self.write_indentation()?;
            self.writer.write_ch(b'}')?;
        }

        Ok(())
    }

    fn write_optional_space(&mut self) -> SassResult<()> {
        if !self.options.is_compressed() {
            self.writer.write_ch(b' ')?;
        }
        Ok(())
    }

    fn write_optional_newline(&mut self) -> SassResult<()> {
        if !self.options.is_compressed() {
            self.writer.write_ch(b'\n')?;
        }
        Ok(())
    }

    fn write_supports_rule(&mut self, supports_rule: SupportsRule) -> SassResult<()> {
        self.write_indentation()?;
        self.writer.write_all(b"@supports")?;

        if !supports_rule.params.is_empty() {
            self.writer.write_ch(b' ')?;
            self.writer.write_all(supports_rule.params.as_bytes())?;
        }

        self.write_children(supports_rule.body)?;

        Ok(())
    }

    /// Returns whether or not text was written
    fn visit_stmt(&mut self, stmt: CssStmt) -> SassResult<bool> {
        if stmt.is_invisible() {
            return Ok(false);
        }

        match stmt {
            CssStmt::RuleSet { selector, body, .. } => {
                self.write_indentation()?;
                self.write_selector_list(&selector.as_selector_list())?;

                self.write_children(body)?;
            }
            CssStmt::Media(media_rule, ..) => {
                self.write_indentation()?;
                self.writer.write_all(b"@media ")?;

                if let Some((last, rest)) = media_rule.query.split_last() {
                    for query in rest {
                        self.write_media_query(query)?;

                        self.writer.write_ch(b',')?;

                        self.write_optional_space()?;
                    }

                    self.write_media_query(last)?;
                }

                self.write_children(media_rule.body)?;
            }
            CssStmt::UnknownAtRule(unknown_at_rule, ..) => {
                self.write_indentation()?;
                self.writer.write_ch(b'@')?;
                self.writer.write_all(unknown_at_rule.name.as_bytes())?;

                if !unknown_at_rule.params.is_empty() {
                    write!(&mut self.writer, " {}", unknown_at_rule.params)?;
                }

                if !unknown_at_rule.has_body {
                    debug_assert!(unknown_at_rule.body.is_empty());
                    return Ok(true);
                } else if unknown_at_rule.body.iter().all(CssStmt::is_invisible) {
                    self.writer.write_all(b" {}")?;
                    return Ok(true);
                }

                self.write_children(unknown_at_rule.body)?;
            }
            CssStmt::Style(style) => self.write_style(style)?,
            CssStmt::Comment(comment, span) => self.write_comment(&comment, span)?,
            CssStmt::KeyframesRuleSet(keyframes_rule_set) => {
                self.write_indentation()?;

                if let Some((first, tail)) = keyframes_rule_set.selector.split_first() {
                    write!(&mut self.writer, "{}", first)?;
                    for selector in tail {
                        write!(&mut self.writer, ", {}", selector)?;
                    }
                }

                self.write_children(keyframes_rule_set.body)?;
            }
            CssStmt::Import(import, modifier) => self.write_import(&import, modifier)?,
            CssStmt::Supports(supports_rule, _) => self.write_supports_rule(supports_rule)?,
        }

        Ok(true)
    }
}

pub(crate) fn requires_semicolon(stmt: &CssStmt) -> bool {
    match stmt {
        CssStmt::Style(_) | CssStmt::Import(_, _) => true,
        CssStmt::UnknownAtRule(rule, _) => !rule.has_body,
        _ => false,
    }
}
