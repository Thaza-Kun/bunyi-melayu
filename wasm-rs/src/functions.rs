use wasm_bindgen::prelude::*;

use crate::{
    imbuhan::{Imbuhan, ImbuhanToml},
    phonology::{Bunyian, BunyianToml},
    phonotactics::{Phonotactic, PhonotacticToml},
};

// import Javascript's alert method to Rust
#[wasm_bindgen]
extern "C" {
    pub(crate) fn alert(s: &str);
}

#[wasm_bindgen]
pub fn parse_bunyian_toml(data: String) -> Vec<Bunyian> {
    match BunyianToml::from_toml_str(data) {
        Ok(v) => v,
        Err(e) => {
            alert(&format!("Error reading string. {}", e));
            vec![]
        }
    }
}

#[wasm_bindgen]
pub fn parse_tatabunyi_toml(data: String) -> Vec<Phonotactic> {
    match PhonotacticToml::from_toml_str(data) {
        Ok(v) => v.get_phonotactics(),
        Err(e) => {
            alert(&format!("Error reading string. {}", e));
            vec![]
        }
    }
}

#[wasm_bindgen]
pub fn parse_default_tatabunyi_toml(data: String) -> Phonotactic {
    match PhonotacticToml::from_toml_str(data) {
        Ok(v) => v
            .get_default_phonotactic()
            .unwrap_or(Phonotactic::default())
            .to_owned()
            .clone(),
        Err(e) => {
            alert(&format!("Error reading string. {}", e));
            Phonotactic::default()
        }
    }
}

#[wasm_bindgen]
pub fn parse_imbuhan_toml(data: String) -> Vec<Imbuhan> {
    match ImbuhanToml::from_toml_str(data) {
        Ok(v) => v,
        Err(e) => {
            alert(&format!("Error reading string. {}", e));
            vec![]
        }
    }
}
