use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use toml;
use wasm_bindgen::prelude::*;

// COMMENT: Perhaps no need to move this to onc crate because all these are just representations of data with no attached logic.

#[wasm_bindgen]
#[derive(Deserialize)]
pub struct Bunyian {
    pub rantau: RantauBunyian,
    pub kaedah: KaedahBunyian,
    pub makhraj: Option<MakhrajTajwid>,
    jawi: String,
    pub jenis_jawi: JenisJawi,
    rumi: String,
    #[serde(alias = "IPA")]
    ipa: String,
    nasalized: HashMap<String, String>,
    pub nasalization_type: Option<JenisSengauan>,
}

#[wasm_bindgen]
impl Bunyian {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            rantau: RantauBunyian::Dwibibir,
            kaedah: KaedahBunyian::Sengauan,
            makhraj: None,
            jawi: "".into(),
            jenis_jawi: JenisJawi::Kongsi,
            rumi: "".into(),
            ipa: "".into(),
            nasalized: HashMap::new(),
            nasalization_type: None,
        }
    }

    #[wasm_bindgen(getter)]
    pub fn jawi(&self) -> String {
        String::from(&self.jawi)
    }
    #[wasm_bindgen(getter)]
    pub fn rumi(&self) -> String {
        String::from(&self.rumi)
    }
    #[wasm_bindgen(getter)]
    pub fn ipa(&self) -> String {
        String::from(&self.ipa)
    }

    pub fn nasalized(&self, script: String) -> Option<String> {
        self.nasalized.get(&script).map(|a| a.to_owned())
    }
}

#[derive(Deserialize)]
pub struct BunyianToml {
    pub bunyian: Vec<Bunyian>,
}

impl BunyianToml {
    pub fn from_toml_str(data: String) -> Result<Vec<Bunyian>, toml::de::Error> {
        let d: Self = toml::from_str(&data)?;
        Ok(d.bunyian)
    }
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone, Copy)]
#[serde(rename_all = "kebab-case")]
pub enum JenisSengauan {
    Mim,
    Nun,
    Nya,
    Nga,
    Unchanged,
}

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone, Copy)]
#[serde(rename_all = "kebab-case")]
pub enum RantauBunyian {
    Dwibibir,
    BibirGusi,
    LelangitGusi,
    Lelangit,
    #[serde(rename = "lelangit lembut")]
    LelangitLembut,
    #[serde(rename = "anak tekak")]
    AnakTekak,
    Tekak,
}

#[wasm_bindgen]
#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum KaedahBunyian {
    Sengauan,
    #[serde(rename = "letusan tak bersuara")]
    LetusanTakBersuara,
    #[serde(rename = "letusan bersuara")]
    LetusanBersuara,
    #[serde(rename = "geseran tak bersuara")]
    GeseranTakBersuara,
    #[serde(rename = "geseran bersuara")]
    GeseranBersuara,
    #[serde(rename = "malaran tak geser")]
    MalaranTakGeser,
    Getaran,
}

#[wasm_bindgen]
#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum JenisJawi {
    Kongsi,
    Ciptaan,
    Arab,
}

#[wasm_bindgen]
#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum MakhrajTajwid {
    #[serde(rename = "dua bibir")]
    DuaBibir,
    #[serde(rename = "hujung lidah")]
    HujungLidah,
    #[serde(rename = "tepi lidah")]
    TepiLidah,
    #[serde(rename = "tengah lidah")]
    TengahLidah,
    #[serde(rename = "pangkal lidah")]
    PangkalLidah,
    #[serde(rename = "hujung halkum")]
    HujungHalkum,
    #[serde(rename = "tengah halkum")]
    TengahHalkum,
    #[serde(rename = "pangkal halkum")]
    PangkalHalkum,
}
