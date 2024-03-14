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
    rantau: RantauBunyian,
    kaedah: KaedahBunyian,
    jawi: String,
    jenis_jawi: JenisJawi,
    rumi: String,
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
}

#[wasm_bindgen]
pub fn toml_to_bunyians(data: String) -> Vec<Bunyian> {
    if let Ok(v) = toml::from_str(&data) {
        v
    } else {
        alert(&format!("Error reading string."));
        vec![]
    }
}

#[wasm_bindgen]
#[derive(Deserialize, Clone, Copy)]
pub enum RantauBunyian {
    Dwibibir,
    BibirGusi,
    LelangitGusi,
    Lelangit,
    LelangitLembut,
    AnakTekak,
    Tekak,
}

#[wasm_bindgen]
#[derive(Deserialize, Clone, Copy)]
pub enum KaedahBunyian {
    Sengauan,
    LetusanBersuara,
    LetusanTakBersuara,
    GeseranBersuara,
    GeseranTakBersuara,
    MalaranTakGeser,
    Getaran,
}

#[wasm_bindgen]
#[derive(Deserialize, Clone, Copy)]
pub enum JenisJawi {
    Kongsi,
    Ciptaan,
    Arab,
}
