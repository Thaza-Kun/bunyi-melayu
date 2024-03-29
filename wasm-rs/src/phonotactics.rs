mod tags;

use itertools::Itertools;
use nom::IResult;
use serde::Deserialize;
use std::{
    fmt::{format, Display},
    option,
};
use wasm_bindgen::prelude::*;

use crate::functions::alert;

use self::tags::SyllableTags;

#[wasm_bindgen]
#[derive(Deserialize, Clone)]
pub struct Phonotactic {
    name: String,
    definition: SyllableTags<String>,
}

impl Phonotactic {
    pub fn new(name: String, definition: SyllableTags<String>) -> Self {
        Self { name, definition }
    }
    pub fn parse_syllables<'a>(
        &'a mut self,
        input: &'a String,
    ) -> IResult<&'a str, Phrase<&'a str>> {
        self.definition
            .as_str()
            .parse_tags(&input)
            .map(|(r, p)| (r, p.with_postprocessing(&self.definition)))
    }
}
#[derive(Deserialize)]
pub struct PhonotacticToml {
    phonotactic: Vec<Phonotactic>,
}

impl PhonotacticToml {
    pub fn from_toml_str(data: String) -> Result<Vec<Phonotactic>, toml::de::Error> {
        let d: Self = toml::from_str(&data)?;
        Ok(d.get_phonotatics())
    }

    pub fn get_phonotatics(&self) -> Vec<Phonotactic> {
        self.phonotactic.clone()
    }
}

#[wasm_bindgen]
pub struct SyllableTagsJson {
    onset: Vec<String>,
    nucleus: Vec<String>,
    coda: Vec<String>,
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

#[wasm_bindgen]
pub struct ParseResults {
    options: ParseResultOptions,
    full: Option<String>,
    partial: Option<(String, String, String)>,
    // details: String,
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

#[wasm_bindgen]
impl Phonotactic {
    pub fn parse_string(&mut self, input: String, options: ParseResultOptions) -> ParseResults {
        let s = self.parse_syllables(&input);
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

pub struct Phrase<O: Copy> {
    syllables: Vec<SyllableUnit<O>>,
}

impl<'a> Phrase<&'a str> {
    fn with_postprocessing(mut self, tags: &'a SyllableTags<String>) -> Self {
        let cloned = self.syllables.clone();
        for (index, (lead, lag)) in cloned.iter().tuple_windows().enumerate() {
            match (lead.coda, lag.onset) {
                (Some(s), None) => {
                    self.syllables[index].coda = None;
                    self.syllables[index + 1].onset = Some(s);
                }
                (Some(s), Some(t)) => {
                    let t: Vec<&'a String> = Vec::from_iter(
                        tags.onset
                            .items
                            .iter()
                            .filter(|a| a == &&format!("{}{}", &s, &t)),
                    );
                    if t.len() != 0 {
                        self.syllables[index].coda = None;
                        self.syllables[index + 1].onset = t.first().map(|a| a.as_str());
                    }
                }
                (_, _) => {}
            }
        }
        self
    }
}

impl<O: Copy> Phrase<O>
where
    SyllableUnit<O>: Display,
{
    pub fn as_separated(&self, separator: &Option<String>) -> String {
        let default_string = &String::from("·");
        let separator = match separator {
            Some(val) => val,
            None => default_string,
        };
        self.syllables.iter().join(&separator)
    }

    pub fn as_contiguous(&self) -> String {
        self.syllables.iter().join(&"")
    }
}

impl<O: Copy> From<Vec<SyllableUnit<O>>> for Phrase<O> {
    fn from(value: Vec<SyllableUnit<O>>) -> Self {
        Self { syllables: value }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct SyllableUnit<O> {
    onset: Option<O>,
    nucleus: O,
    coda: Option<O>,
}

impl<'a> Display for SyllableUnit<&'a str> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.onset.unwrap_or(""),
            self.nucleus,
            self.coda.unwrap_or("")
        )
    }
}

impl<O> From<(Option<O>, O, Option<O>)> for SyllableUnit<O> {
    fn from(value: (Option<O>, O, Option<O>)) -> Self {
        Self {
            onset: value.0,
            nucleus: value.1,
            coda: value.2,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::phonotactics::Phonotactic;

    use super::{tags::SyllableTags, ParseResultOptions, Phrase, SyllableUnit};

    #[test]
    fn test_panic_condition() {
        // Test some combination of words here to get the compiler to panic
        let word = "byi".to_string();
        let definition = SyllableTags::new_ordered(
            vec![
                "ny", "ng", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y",
                "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec!["ng", "m", "n", "p", "t", "k", "s", "h", "l", "r"],
        )
        .as_string();
        let mut malay_phonotactic = Phonotactic::new("Melayu Klasik".into(), definition.clone());
        let result =
            malay_phonotactic.parse_string(word, ParseResultOptions::new(Some("/".into())));
        // let w = word.with_postprocessing(&definition);
        // assert_eq!(
        //     w.syllables,
        //     vec![
        //         SyllableUnit::from((Some("p"), "e", None)),
        //         SyllableUnit::from((Some("n"), "e", None)),
        //         SyllableUnit::from((Some("r"), "a", None)),
        //         SyllableUnit::from((Some("ng"), "a", Some("n")))
        //     ]
        // );
    }
    #[test]
    fn test_penerangan_melayu_lama() {
        let word = "penerangan".to_string();
        let definition = SyllableTags::new_ordered(
            vec![
                "ny", "ng", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y",
                "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec!["ng", "m", "n", "p", "t", "k", "s", "h", "l", "r"],
        )
        .as_string();
        let mut malay_phonotactic = Phonotactic::new("Melayu Klasik".into(), definition.clone());

        let (_rest, word) = malay_phonotactic.parse_syllables(&word).expect("Error");
        let w = word.with_postprocessing(&definition);
        assert_eq!(
            w.syllables,
            vec![
                SyllableUnit::from((Some("p"), "e", None)),
                SyllableUnit::from((Some("n"), "e", None)),
                SyllableUnit::from((Some("r"), "a", None)),
                SyllableUnit::from((Some("ng"), "a", Some("n")))
            ]
        );
    }
    #[test]
    fn test_menyanyi_melayu_lama() {
        let word = "menyanyi".to_string();
        let definition = SyllableTags::new_ordered(
            vec![
                "ny", "ng", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y",
                "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec!["ng", "m", "n", "p", "t", "k", "s", "h", "l", "r"],
        )
        .as_string();
        let mut malay_phonotactic = Phonotactic::new("Melayu Klasik".into(), definition.clone());

        let (_rest, word) = malay_phonotactic.parse_syllables(&word).expect("Error");
        let w = word.with_postprocessing(&definition);
        assert_eq!(
            w.syllables,
            vec![
                SyllableUnit::from((Some("m"), "e", None)),
                SyllableUnit::from((Some("ny"), "a", None)),
                SyllableUnit::from((Some("ny"), "i", None)),
            ]
        );
    }
    #[test]
    fn test_mesyuarat_melayu_klasik() {
        let word = "mesyuarat".to_string();
        let definition = SyllableTags::new_ordered(
            vec![
                "kh", "sy", "gh", "ny", "ng", "q", "f", "m", "n", "p", "t", "c", "k", "b", "d",
                "j", "g", "s", "h", "l", "y", "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec![
                "kh", "sy", "gh", "ng", "q", "f", "b", "m", "n", "p", "t", "k", "s", "h", "l", "r",
            ],
        )
        .as_string();
        let mut arab_phonotactic = Phonotactic::new("Melayu Klasik".into(), definition.clone());
        let (_rem, word) = arab_phonotactic.parse_syllables(&word).expect("Error");
        let w = word.with_postprocessing(&definition);
        assert_eq!(
            w.syllables,
            vec![
                SyllableUnit::from((Some("m"), "e", None)),
                SyllableUnit::from((Some("sy"), "u", None)),
                SyllableUnit::from((None, "a", None)),
                SyllableUnit::from((Some("r"), "a", Some("t")))
            ]
        );
    }

    #[test]
    fn test_musytari_melayu_klasik() {
        let word = "musytari".to_string();
        let definition = SyllableTags::new_ordered(
            vec![
                "kh", "sy", "gh", "ny", "ng", "q", "f", "m", "n", "p", "t", "c", "k", "b", "d",
                "j", "g", "s", "h", "l", "y", "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec![
                "kh", "sy", "gh", "ng", "q", "f", "b", "m", "n", "p", "t", "k", "s", "h", "l", "r",
            ],
        )
        .as_string();
        let mut arab_phonotactic = Phonotactic::new("Melayu Klasik".into(), definition.clone());
        let (_rem, word) = arab_phonotactic.parse_syllables(&word).expect("Error");
        let w = word.with_postprocessing(&definition);
        assert_eq!(
            w.syllables,
            vec![
                SyllableUnit::from((Some("m"), "u", Some("sy"))),
                SyllableUnit::from((Some("t"), "a", None)),
                SyllableUnit::from((Some("r"), "i", None))
            ]
        );
    }

    #[test]
    fn test_ghaib_melayu_klasik() {
        let word = "ghaib".to_string();
        let definition = SyllableTags::new_ordered(
            vec![
                "kh", "sy", "gh", "ny", "ng", "q", "f", "m", "n", "p", "t", "c", "k", "b", "d",
                "j", "g", "s", "h", "l", "y", "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec![
                "kh", "sy", "gh", "ng", "q", "f", "b", "m", "n", "p", "t", "k", "s", "h", "l", "r",
            ],
        )
        .as_string();
        let mut arab_phonotactic = Phonotactic::new("Melayu Klasik".into(), definition.clone());
        let (_rem, word) = arab_phonotactic.parse_syllables(&word).expect("Error");
        let w = word.with_postprocessing(&definition);
        assert_eq!(
            w.syllables,
            vec![
                SyllableUnit::from((Some("gh"), "a", None)),
                SyllableUnit::from((None, "i", Some("b"))),
            ]
        );
    }
}
