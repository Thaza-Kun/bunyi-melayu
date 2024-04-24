use crate::phonotactics::Phonotactic;
use onc::affixes::AffixRule;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq)]
pub enum JenisKata {
    #[serde(rename = "kata nama")]
    Nama,
    #[serde(rename = "kata kerja")]
    Kerja,
    #[serde(rename = "kata sifat")]
    Sifat,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Debug)]
#[serde(transparent)]
pub struct Imbuhan(AffixRule<JenisKata>);

#[wasm_bindgen]
impl Imbuhan {
    pub fn new() -> Self {
        Self(AffixRule::<JenisKata>::new())
    }

    pub fn contains(&self, kata_nama: bool, kata_kerja: bool, kata_sifat: bool) -> bool {
        (kata_nama && self.0.pos().contains(&JenisKata::Nama))
            | (kata_kerja && self.0.pos().contains(&JenisKata::Kerja))
            | (kata_sifat && self.0.pos().contains(&JenisKata::Sifat))
    }
    pub fn transform_string_with(&self, input: &str, phonotactic: &Phonotactic) -> String {
        self.0.transform_string_with(input, &phonotactic.as_rule())
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
