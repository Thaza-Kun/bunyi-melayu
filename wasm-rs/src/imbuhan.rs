use std::collections::HashMap;

use itertools::Itertools;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::wasm_bindgen;

use crate::phonotactics::Phonotactic;

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum JenisKata {
    #[serde(rename = "kata nama")]
    Nama,
    #[serde(rename = "kata kerja")]
    Kerja,
    #[serde(rename = "kata sifat")]
    Sifat,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Ganti {
    awal: Option<HashMap<String, String>>,
    akhir: Option<HashMap<String, String>>,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Debug)]
pub struct Imbuhan {
    untuk: Vec<JenisKata>,
    awal: Option<String>,
    akhir: Option<String>,
    #[serde(default)]
    ganti: Option<Ganti>,
}

#[wasm_bindgen]
impl Imbuhan {
    pub fn new() -> Self {
        Self {
            untuk: vec![],
            awal: None,
            akhir: None,
            ganti: None,
        }
    }

    #[wasm_bindgen]
    pub fn contains(&self, kata_nama: bool, kata_kerja: bool, kata_sifat: bool) -> bool {
        (kata_nama && self.untuk.contains(&JenisKata::Nama))
            | (kata_kerja && self.untuk.contains(&JenisKata::Kerja))
            | (kata_sifat && self.untuk.contains(&JenisKata::Sifat))
    }

    pub fn transform_string_with(&self, input: &str, phonotactic: &Phonotactic) -> String {
        let text = input.to_string();
        let head = match &self.awal {
            Some(awal) => match &self.ganti {
                Some(Ganti {
                    awal: Some(a),
                    akhir: _,
                }) => {
                    if let Ok((rest, phrase)) = phonotactic.parse_syllables(&text) {
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
                            format!(
                                "{}{}{}",
                                awal.clone().to_lowercase(),
                                inbetween.to_lowercase(),
                                tail.to_lowercase()
                            )
                        } else {
                            String::new()
                        }
                    } else {
                        String::new()
                    }
                }
                &Some(Ganti { awal: None, .. }) | None => format!("{}{}", awal.clone(), text),
            },
            None => format!("{}", text),
        };
        match &self.akhir {
            Some(akhir) => match &self.ganti {
                Some(Ganti {
                    akhir: Some(a),
                    awal: _,
                }) => {
                    if let Ok((rest, phrase)) = phonotactic.parse_syllables(&text) {
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
                &Some(Ganti { akhir: None, .. }) | None => format!("{}{}", head, akhir.clone()),
            },
            None => head,
        }
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize)]
pub struct ImbuhanToml {
    imbuhan: Vec<Imbuhan>,
}

impl ImbuhanToml {
    pub fn from_toml_str(data: String) -> Result<Vec<Imbuhan>, toml::de::Error> {
        let d: Self = toml::from_str(&data)?;
        Ok(d.imbuhan)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{
        imbuhan::{Ganti, Imbuhan, JenisKata},
        phonotactics::{tags::SyllableTags, Phonotactic},
    };
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
        let impbuhan = Imbuhan {
            untuk: vec![JenisKata::Kerja],
            awal: Some("me".into()),
            akhir: None,
            ganti: Some(Ganti {
                awal: Some(HashMap::from_iter(vec![("b".into(), "mb".into())])),
                akhir: None,
            }),
        };
        let malay_phonotactic = Phonotactic::new("Melayu Klasik".into(), definition.clone());

        let (_rest, phrase) = malay_phonotactic.parse_syllables(&word).expect("Error");
        assert_eq!(
            impbuhan.transform_string_with(&phrase.as_contiguous(), &malay_phonotactic),
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
        let impbuhan = Imbuhan {
            untuk: vec![JenisKata::Kerja],
            awal: Some("me".into()),
            akhir: None,
            ganti: Some(Ganti {
                awal: Some(HashMap::from_iter(vec![("".into(), "ng".into())])),
                akhir: None,
            }),
        };
        let malay_phonotactic = Phonotactic::new("Melayu Klasik".into(), definition.clone());

        let (_rest, phrase) = malay_phonotactic.parse_syllables(&word).expect("Error");
        assert_eq!(
            impbuhan.transform_string_with(&word, &malay_phonotactic),
            String::from("mengambil")
        )
    }
}
