use crate::phonotactics::PhonotacticRule;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Serialize, Deserialize, Debug)]
pub struct ReplacementRule {
    #[serde(rename = "awal")]
    prefix: Option<HashMap<String, String>>,
    #[serde(rename = "akhir")]
    postfix: Option<HashMap<String, String>>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AffixRule<POS>
where
    POS: Hash + Eq,
{
    #[serde(rename = "untuk")]
    apply_to: HashSet<POS>,
    #[serde(rename = "awal")]
    prefix: Option<String>,
    #[serde(rename = "akhir")]
    postfix: Option<String>,
    #[serde(rename = "ganti")]
    replace: Option<ReplacementRule>,
}

impl<POS> AffixRule<POS>
where
    POS: Hash + Eq,
{
    pub fn new() -> Self {
        Self {
            apply_to: HashSet::new(),
            prefix: None,
            postfix: None,
            replace: None,
        }
    }

    pub fn pos(&self) -> HashSet<&POS> {
        HashSet::<&POS>::from_iter(self.apply_to.iter())
    }

    pub fn transform_string_with(&self, input: &str, phonotactic: &PhonotacticRule) -> String {
        let text = input.to_string();
        let head = match &self.prefix {
            Some(awal) => match &self.replace {
                Some(ReplacementRule {
                    prefix: Some(a), ..
                }) => {
                    if let Ok((_rest, phrase)) = phonotactic.parse_syllables(&text) {
                        if let Some(first) = phrase.syllables.first() {
                            let mut offset = 0;
                            let default = &"".to_string();
                            let inbetween = if let Some(onset) = first.onset {
                                offset = onset.len();
                                match a.get(onset) {
                                    Some(v) => v.clone(),
                                    None => String::from(onset),
                                }
                            } else {
                                a.get("").unwrap_or(default).clone()
                            };
                            let tail = text[offset..].to_string();
                            format!("{}{}{}", awal.clone(), inbetween, tail)
                        } else {
                            String::new()
                        }
                    } else {
                        String::new()
                    }
                }
                &Some(ReplacementRule { prefix: None, .. }) | None => {
                    format!("{}{}", awal.clone(), text)
                }
            },
            None => format!("{}", text),
        };
        match &self.postfix {
            Some(akhir) => match &self.replace {
                Some(ReplacementRule {
                    postfix: Some(a), ..
                }) => {
                    if let Ok((_rest, phrase)) = phonotactic.parse_syllables(&text) {
                        if let Some(first) = phrase.syllables.first() {
                            let mut offset = 0;
                            let default = &"".to_string();
                            let inbetween = if let Some(onset) = first.onset {
                                offset = onset.len();
                                match a.get(onset) {
                                    Some(v) => v.clone(),
                                    None => String::from(onset),
                                }
                            } else {
                                a.get("").unwrap_or(default).clone()
                            };
                            let tail = text[offset..].to_string();
                            format!("{}{}{}", tail, inbetween, akhir.clone())
                        } else {
                            String::new()
                        }
                    } else {
                        String::new()
                    }
                }
                &Some(ReplacementRule { postfix: None, .. }) | None => {
                    format!("{}{}", head, akhir.clone())
                }
            },
            None => head,
        }
        .to_lowercase()
    }
}
#[cfg(test)]
mod test {
    use super::*;
    use crate::phonotactics::tags::SyllableTags;

    #[derive(Hash, PartialEq, Eq)]
    enum ExamplePOS {
        Verb,
    }

    #[test]
    fn test_imbuhan_awal_mengawal() {
        let word = "bintang".to_string();
        let definition = SyllableTags::new_ordered(
            vec![
                "ny", "ng", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y",
                "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec!["ng", "m", "n", "p", "t", "k", "s", "h", "l", "r"],
        )
        .as_string();
        let impbuhan = AffixRule {
            apply_to: HashSet::from([ExamplePOS::Verb]),
            prefix: Some("me".into()),
            postfix: None,
            replace: Some(ReplacementRule {
                prefix: Some(HashMap::from_iter(vec![("b".into(), "mb".into())])),
                postfix: None,
            }),
        };
        let malay_phonotactic = PhonotacticRule::with_definitions(definition.clone());

        assert_eq!(
            impbuhan.transform_string_with(&word, &malay_phonotactic),
            String::from("membintang")
        )
    }

    #[test]
    fn test_imbuhan_awal_mengambil() {
        let word = "ambil".to_string();
        let definition = SyllableTags::new_ordered(
            vec![
                "ny", "ng", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y",
                "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec!["ng", "m", "n", "p", "t", "k", "s", "h", "l", "r"],
        )
        .as_string();
        let impbuhan = AffixRule {
            apply_to: HashSet::from([ExamplePOS::Verb]),
            prefix: Some("me".into()),
            postfix: None,
            replace: Some(ReplacementRule {
                prefix: Some(HashMap::from_iter(vec![("".into(), "ng".into())])),
                postfix: None,
            }),
        };
        let malay_phonotactic = PhonotacticRule::with_definitions(definition.clone());

        assert_eq!(
            impbuhan.transform_string_with(&word, &malay_phonotactic),
            String::from("mengambil")
        )
    }
}
