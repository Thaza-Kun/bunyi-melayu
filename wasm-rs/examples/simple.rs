use wasm_rs::{
    functions::parse_default_tatabunyi_toml,
    phonotactics::{ParseResultOptions, PhonotacticToml},
};

const INPUT: [&'static str; 5] = ["susyi", "Penerangan", "English", "Ramadhan", "ramadan"];
const CONFIG: &'static str = r#"
default = "Melayu Moden"

[[phonotactic]]
name = "Melayu Lama"
definition.onset = [["ny", "ng", "sw", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y", "w", "r"]]
definition.nucleus = [["a", "e", "i", "o", "u"]]
definition.coda = [["ng", "m", "n", "p", "t", "k", "s", "h", "l", "r"]]

[[phonotactic]]
name = "Melayu Klasik"
definition.onset = [["kh", "sy", "gh", "ny", "ng", "dh", "q", "f", "sw", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y", "w", "r"]]
definition.nucleus = [["a", "e", "i", "o", "u"]]
definition.coda = [["kh", "sy", "gh", "ng", "q", "f", "b", "m", "n", "p", "t", "k", "s", "h", "l", "r"]]

[[phonotactic]]
name = "Melayu Moden"
definition.onset = [["sp", "spr", "sw", "sk", "skr", "st", "str", "kl", "fl", "bl", "pl", "pr", "kr", "gr", "tr", "dr", "kh", "sy", "gh", "ny", "ng", "v", "x", "q", "f", "y", "w", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "r"]]
definition.nucleus = [["a", "e", "i", "o", "u"]]
definition.coda = [["ks", "ns", "nk", "lf", "rt", "rd", "rt", "kh", "sy", "gh", "ng", "q", "f", "b", "m", "n", "p", "t", "k", "s", "h", "l", "r"]]
"#;

fn main() {
    for (n, &i) in INPUT.iter().enumerate() {
        println!("\n[E.g. {}] ========", n + 1);
        let input = String::from(i);
        println!("Input\t\t: {}", &input);

        let mut phonotactic = parse_default_tatabunyi_toml(CONFIG.to_string());

        let result = phonotactic.parse_string(input, ParseResultOptions::new(Some("/".into())));

        println!(
            "{}",
            match result.error() {
                true => format!(
                    "Err parse\t: {}<?|{}|?>{}",
                    result.head().unwrap(),
                    result.mid().unwrap(),
                    result.tail().unwrap()
                ),
                false => format!("Ok parse\t: {}", result.full().unwrap()),
            }
        )
    }
}
