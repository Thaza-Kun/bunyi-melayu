pub mod tags;

use itertools::Itertools;
use std::fmt::Display;

use crate::phonotactics::tags::SyllableTags;

pub struct Phrase<O: Copy> {
    pub syllables: Vec<SyllableUnit<O>>,
}

impl<'a> Phrase<&'a str> {
    pub fn with_postprocessing(mut self, tags: &'a SyllableTags<String>) -> Self {
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
pub struct SyllableUnit<O> {
    pub onset: Option<O>,
    pub nucleus: O,
    pub coda: Option<O>,
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