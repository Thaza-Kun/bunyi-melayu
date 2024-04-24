use onc::phonotactics::tags::SyllableTags;
use onc::phonotactics::PhonotacticRule;
use onc::ParseResult as InnerParseResult;

use serde::Deserialize;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Deserialize, Clone, Default)]
pub struct Phonotactic {
    name: String,
    definition: SyllableTags<String>,
}

#[wasm_bindgen]
impl Phonotactic {
    pub(crate) fn as_rule(&self) -> PhonotacticRule {
        PhonotacticRule::with_definitions(self.definition.clone())
    }

    #[wasm_bindgen]
    pub fn parse_string(&mut self, input: String, options: ParseResultOptions) -> ParseResults {
        let text = input.to_lowercase();
        let rule = self.as_rule();
        let s = rule.parse_syllables(&text);
        ParseResults::from_inner(&InnerParseResult::from(s), options)
    }

    #[wasm_bindgen(getter)]
    pub fn name(&self) -> String {
        self.name.clone()
    }
    #[wasm_bindgen(getter)]
    pub fn tags(&self) -> SyllableTagsJson {
        self.definition.clone().into()
    }
}

#[derive(Deserialize)]
pub struct PhonotacticToml {
    pub default: String,
    phonotactic: Vec<Phonotactic>,
}

impl PhonotacticToml {
    pub fn from_toml_str(data: String) -> Result<Self, toml::de::Error> {
        let d: Self = toml::from_str(&data)?;
        Ok(d)
    }

    pub fn get_phonotactics(&self) -> Vec<Phonotactic> {
        self.phonotactic.clone()
    }

    pub fn get_default_phonotactic(&self) -> Option<Phonotactic> {
        self.phonotactic
            .iter()
            .filter(|p| p.name == self.default)
            .collect::<Vec<&Phonotactic>>()
            .first()
            .cloned()
            .cloned()
    }
}

#[wasm_bindgen]
pub struct SyllableTagsJson {
    pub(crate) onset: Vec<String>,
    pub(crate) nucleus: Vec<String>,
    pub(crate) coda: Vec<String>,
}

#[wasm_bindgen]
impl SyllableTagsJson {
    #[wasm_bindgen(getter)]
    pub fn onset(&self) -> Vec<String> {
        self.onset.clone()
    }
    #[wasm_bindgen(getter)]
    pub fn nucleus(&self) -> Vec<String> {
        self.nucleus.clone()
    }
    #[wasm_bindgen(getter)]
    pub fn coda(&self) -> Vec<String> {
        self.coda.clone()
    }
}

impl From<SyllableTags<String>> for SyllableTagsJson {
    fn from(value: SyllableTags<String>) -> Self {
        Self {
            onset: value.onset.items.clone(),
            nucleus: value.nucleus.items.clone(),
            coda: value.coda.items.clone(),
        }
    }
}

// TODO: Proper Result
#[wasm_bindgen]
pub struct ParseResults {
    options: ParseResultOptions,
    full: Option<String>,
    partial: Option<(String, String, String)>,
}

#[wasm_bindgen]
impl ParseResults {
    #[wasm_bindgen(getter)]
    pub fn full(&self) -> Option<String> {
        self.full.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn error(&self) -> bool {
        self.partial.is_some()
    }

    #[wasm_bindgen(getter)]
    pub fn head(&self) -> Option<String> {
        Some(self.partial.clone()?.0)
    }

    #[wasm_bindgen(getter)]
    pub fn mid(&self) -> Option<String> {
        Some(self.partial.clone()?.1)
    }

    #[wasm_bindgen(getter)]
    pub fn tail(&self) -> Option<String> {
        Some(self.partial.clone()?.2)
    }
}

impl ParseResults {
    pub fn new(options: ParseResultOptions) -> Self {
        Self {
            options,
            full: None,
            partial: None,
        }
    }
    pub(crate) fn from_inner(inner: &InnerParseResult, options: ParseResultOptions) -> Self {
        let mut res = ParseResults::new(options);
        if inner.full {
            res.with_full(inner.phrase.as_separated(&res.options.separator));
        } else {
            let mid_tail = if &inner.rest.len() > &1 {
                &inner.rest[0..2]
            } else {
                inner.rest.as_str()
            };
            let tail_rest = if &inner.rest.len() > &1 {
                inner.rest[2..inner.rest.len()].to_string()
            } else {
                "".into()
            };
            let head = inner.phrase.as_contiguous();
            let mid = format!(
                "{head}{tail}",
                head = head.chars().last().unwrap_or(' '),
                tail = mid_tail,
            );
            res.with_partial(
                head[0..head.len().saturating_sub(1)].to_string(),
                mid,
                tail_rest,
            );
        }
        res
    }

    pub fn with_full(&mut self, input: String) {
        self.full = Some(input);
    }

    pub fn with_partial(&mut self, head: String, mid: String, tail: String) {
        self.partial = Some((head, mid, tail));
    }
}

#[wasm_bindgen]
pub struct ParseResultOptions {
    separator: Option<String>,
}

#[wasm_bindgen]
impl ParseResultOptions {
    #[wasm_bindgen(constructor)]
    pub fn new(separator: Option<String>) -> Self {
        Self { separator }
    }
}
