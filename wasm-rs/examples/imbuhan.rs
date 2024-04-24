use std::collections::HashSet;

use wasm_rs::functions::{parse_default_tatabunyi_toml, parse_imbuhan_toml};
use wasm_rs::imbuhan::JenisKata;

lazy_static::lazy_static! {
static ref INPUTS: [(&'static str, HashSet<JenisKata>); 3] = [
    ("susyi", HashSet::from([JenisKata::Nama])),
    ("terang", HashSet::from([JenisKata::Nama, JenisKata::Kerja])),
    ("merah", HashSet::from([JenisKata::Sifat]))
    ];
}
const IMBUHAN_CONFIG: &'static str = r#"
[[imbuhan]]
ganti.awal = { "" = "ng", "k" = "ng", "g" = "ngg", "h" = "ng", "q" = "ngq", "b" = "mb", "v" = "mv", "f" = "mf", "p" = "m", "c" = "nc", "d" = "nd", "j" = "nj", "sy" = "nsy", "t" = "n", "z" = "z", "s" = "ny" }
untuk = ["kata kerja"]
awal = "me"

[[imbuhan]]
untuk = ["kata nama"]
awal = "per"

[[imbuhan]]
untuk = ["kata nama", "kata kerja"]
awal = "juru"

[[imbuhan]]
untuk = ["kata kerja", "kata sifat"]
awal = "ter"

[[imbuhan]]
untuk = ["kata sifat"]
awal = "se"

[[imbuhan]]
ganti.awal = { "" = "ng", "k" = "ng", "g" = "ngg", "h" = "ng", "q" = "ngq", "b" = "mb", "v" = "mv", "f" = "mf", "p" = "m", "c" = "nc", "d" = "nd", "j" = "nj", "sy" = "nsy", "t" = "n", "z" = "z", "s" = "ny" }
untuk = ["kata kerja"]
awal = "me"
akhir = "kan"

[[imbuhan]]
untuk = ["kata nama", "kata kerja", "kata sifat"]
awal = "ke"
akhir = "an"
"#;
const PHONOTACTIC_CONFIG: &'static str = r#"
default = "Melayu Moden"

[[phonotactic]]
name = "Melayu Moden"
definition.onset = [["sp", "spr", "sw", "sk", "skr", "st", "str", "kl", "fl", "bl", "pl", "pr", "kr", "gr", "tr", "dr", "kh", "sy", "gh", "ny", "ng", "v", "x", "q", "f", "y", "w", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "r"]]
definition.nucleus = [["a", "e", "i", "o", "u"]]
definition.coda = [["ks", "ns", "nk", "lf", "rt", "rd", "rt", "kh", "sy", "gh", "ng", "q", "f", "b", "m", "n", "p", "t", "k", "s", "h", "l", "r"]]
"#;

fn main() {
    println!("<< Example: Imbuhan >>");
    let imbuhans = parse_imbuhan_toml(IMBUHAN_CONFIG.to_string());
    let phonotactic = parse_default_tatabunyi_toml(PHONOTACTIC_CONFIG.to_string());

    for (n, (input, set)) in INPUTS.iter().enumerate() {
        println!("\n[E.g. {}] ========", n + 1);
        let input = String::from(*input);
        println!("Input\t\t: {}", &input);
        println!("Golongan Kata\t: {:?}", &set);

        let mut items = Vec::<String>::new();

        for imbuhan in &imbuhans {
            if imbuhan.contains(
                set.contains(&JenisKata::Nama),
                set.contains(&JenisKata::Kerja),
                set.contains(&JenisKata::Sifat),
            ) {
                items.push(imbuhan.transform_string_with(&input, &phonotactic));
            }
        }
        println!("Results\t\t: {}", items.join(", "))
    }
}
