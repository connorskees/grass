#[cfg(feature = "wasm")]
#[wasm_bindgen]
impl StyleSheet {
    pub fn new(input: String) -> Result<String, JsValue> {
        let mut map = CodeMap::new();
        let file = map.add_file("stdin".into(), input);
        Ok(Css::from_stylesheet(StyleSheet(
            StyleSheetParser {
                lexer: &mut Lexer::new(&file).peekmore(),
                nesting: 0,
                map: &mut map,
                path: Path::new(""),
            }
            .parse_toplevel()
            .map_err(|e| raw_to_parse_error(&map, e).to_string())?
            .0,
        ))
        .map_err(|e| raw_to_parse_error(&map, e).to_string())?
        .pretty_print(&map)
        .map_err(|e| raw_to_parse_error(&map, e).to_string())?)
    }
}
