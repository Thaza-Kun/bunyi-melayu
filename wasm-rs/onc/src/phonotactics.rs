pub mod tags;

use itertools::Itertools;
use nom::IResult;
use std::fmt::Display;

use crate::phonotactics::tags::SyllableTags;

#[cfg(feature = "faker")]
use rand::{prelude::SliceRandom, SeedableRng};
#[cfg(feature = "faker")]
use rand_chacha::ChaCha8Rng;

#[derive(Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct PhonotacticRule {
    definition: SyllableTags<String>,
}

#[cfg(feature = "faker")]
pub struct Faker {
    pub rng: ChaCha8Rng,
}

#[cfg(feature = "faker")]
impl Default for Faker {
    fn default() -> Self {
        Self {
            rng: ChaCha8Rng::seed_from_u64(42),
        }
    }
}

impl PhonotacticRule {
    pub fn with_definitions(definition: SyllableTags<String>) -> Self {
        PhonotacticRule { definition }
    }
    pub fn parse_syllables<'a>(&'a self, input: &'a String) -> IResult<&'a str, Phrase<&'a str>> {
        self.definition
            .as_str()
            .parse_tags(&input)
            .map(|(r, p)| (r, p.with_postprocessing(&self.definition)))
    }

    #[cfg(feature = "faker")]
    pub fn generate_fake_word(&self, syllables: usize, rng: &mut ChaCha8Rng) -> String {
        let mut fake = String::new();
        let mut inner_definition = self.definition.clone();
        // Possibly empty onset or coda
        // The onset/coda are inversely weighted by its length (short onset/coda are prioritized)
        inner_definition.onset.items.push("".into());
        inner_definition.coda.items.push("".into());
        for _ in 0..syllables {
            let onset = inner_definition
                .onset
                .items
                .choose_weighted(rng, |i| 2 / (i.len() + 1))
                .unwrap();
            let nucleus = inner_definition.nucleus.items.choose(rng).unwrap();
            let coda = inner_definition
                .coda
                .items
                .choose_weighted(rng, |i| 2 / (i.len() + 1))
                .unwrap();
            fake = format!("{}{}{}{}", fake, onset, nucleus, coda);
        }
        fake
    }
}

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

// TODO: BUG PLASTIK SEPATUTNYA PLAS/TIK BUKAN PLA/STIK
// TODO: BUG SWASTA SEPATUTNYA SWAS/TA BUKAN SWA/STA

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn main() {
        // TODO Make a list of string to test and iterate through all.
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
        let phonotactic = PhonotacticRule::with_definitions(definition.to_owned());

        let (_rest, word) = phonotactic.parse_syllables(&word).expect("Error");
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
}
