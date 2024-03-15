use serde::Deserialize;
use toml;
use wasm_bindgen::prelude::*;

// import Javascript's alert method to Rust
#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

// export Rust function greet to be used in JS/TS, the same function signature will be used in JS/TS
#[wasm_bindgen]
pub fn greet(str: &str) {
    alert(&format!("Bye, {}!", str));
}

#[allow(dead_code)]
#[wasm_bindgen]
#[derive(Deserialize)]
pub struct Bunyian {
    pub rantau: RantauBunyian,
    pub kaedah: KaedahBunyian,
    jawi: String,
    pub jenis_jawi: JenisJawi,
    rumi: String,
    #[serde(alias="IPA")]
    ipa: String,
}

#[wasm_bindgen]
impl Bunyian {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            rantau: RantauBunyian::Dwibibir,
            kaedah: KaedahBunyian::Sengauan,
            jawi: "".into(),
            jenis_jawi: JenisJawi::Kongsi,
            rumi: "".into(),
            ipa: "".into(),
        }
    }

    #[wasm_bindgen(getter)]
    pub fn jawi(self) -> String {
        self.jawi
    }
    #[wasm_bindgen(getter)]
    pub fn rumi(self) -> String {
        self.rumi
    }
    #[wasm_bindgen(getter)]
    pub fn ipa(self) -> String {
        self.ipa
    }
}

#[wasm_bindgen]
pub fn from_toml_str(data: String) -> Vec<Bunyian> {
    match BunyianTable::from_toml_str(data) {
        Ok(v) => v ,
        Err(e) => {
            alert(&format!("Error reading string. {}", e));
            vec![]
        }
    }
}

#[derive(Deserialize)]
pub struct BunyianTable {
    pub bunyian: Vec<Bunyian>
}

impl BunyianTable {
    fn from_toml_str(data: String) -> Result<Vec<Bunyian>, toml::de::Error> {
        let d: Self = toml::from_str(&data)?;
        Ok(d.bunyian)
    }
}


#[wasm_bindgen]
#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all="kebab-case")]
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
#[serde(rename_all="lowercase")]
pub enum KaedahBunyian {
    Sengauan,
    #[serde(rename = "letusan bersuara")]
    LetusanBersuara,
    #[serde(rename = "letusan tak bersuara")]
    LetusanTakBersuara,
    #[serde(rename = "geseran bersuara")]
    GeseranBersuara,
    #[serde(rename = "geseran tak bersuara")]
    GeseranTakBersuara,
    #[serde(rename = "malaran tak geser")]
    MalaranTakGeser,
    Getaran,
}

#[wasm_bindgen]
#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all="lowercase")]
pub enum JenisJawi {
    Kongsi,
    Ciptaan,
    Arab,
}
