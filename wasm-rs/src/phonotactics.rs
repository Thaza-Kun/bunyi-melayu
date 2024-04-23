use onc::phonotactics::tags::SyllableTags;
use onc::phonotactics::{PhonotacticRule, Phrase};
use onc::IResult;

use serde::Deserialize;
use wasm_bindgen::prelude::*;

use crate::functions::alert;

#[wasm_bindgen]
#[derive(Deserialize, Clone, Default)]
pub struct Phonotactic {
    name: String,
    definition: SyllableTags<String>,
}

// impl From<Phonotactic> for PhonotacticRule {
//     fn from(value: Phonotactic) -> Self {
//         PhonotacticRule::with_definitions(value.definition)
//     }
// }

impl Phonotactic {
    pub fn as_rule(&self) -> PhonotacticRule {
        PhonotacticRule::with_definitions(self.definition.clone())
    }

    pub fn definitions(&self) -> SyllableTags<String> {
        self.definition.clone()
    }
}

#[wasm_bindgen]
impl Phonotactic {
    pub fn parse_string(&mut self, input: String, options: ParseResultOptions) -> ParseResults {
        let text = input.to_lowercase();
        let rule = self.as_rule();
        let s = rule.parse_syllables(&text);
        InnerParseResult::from(s).render(options)
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

struct InnerParseResult<'a> {
    full: bool,
    rest: String,
    phrase: Phrase<&'a str>,
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
            // details: "".into(),
        }
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

impl<'a> InnerParseResult<'a> {
    pub fn render(&self, options: ParseResultOptions) -> ParseResults {
        let mut res = ParseResults::new(options);
        if self.full {
            res.with_full(self.phrase.as_separated(&res.options.separator));
        } else {
            let mid_tail = if &self.rest.len() > &1 {
                &self.rest[0..2]
            } else {
                self.rest.as_str()
            };
            let tail_rest = if &self.rest.len() > &1 {
                self.rest[2..self.rest.len()].to_string()
            } else {
                "".into()
            };
            let head = self.phrase.as_contiguous();
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
}

impl<'a> From<IResult<&'a str, Phrase<&'a str>>> for InnerParseResult<'a> {
    fn from(value: IResult<&'a str, Phrase<&'a str>>) -> Self {
        match value {
            Ok((rest, phrase)) => Self {
                full: rest.is_empty(),
                rest: String::from(rest),
                phrase: phrase,
            },
            Err(e) => {
                alert(&format!("{}", e));
                Self {
                    full: false,
                    rest: "".into(),
                    phrase: Phrase { syllables: vec![] },
                }
            }
        }
    }
}
