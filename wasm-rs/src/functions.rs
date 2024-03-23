use wasm_bindgen::prelude::*;

use crate::{
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
        Ok(v) => v,
        Err(e) => {
            alert(&format!("Error reading string. {}", e));
            vec![]
        }
    }
}
