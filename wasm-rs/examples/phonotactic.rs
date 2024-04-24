use wasm_rs::functions::parse_default_tatabunyi_toml;
use wasm_rs::phonotactics::ParseResultOptions;

const INPUTS: [&'static str; 6] = [
    "susyi",
    "Penerangan",
    "English",
    "Ramadhan",
    "ramadan",
    "kupu-kupu",
];
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
    println!("<< Example: Phonotactic>>");
    let mut phonotactic = parse_default_tatabunyi_toml(CONFIG.to_string());

    for (n, &input) in INPUTS.iter().enumerate() {
        println!("\n[E.g. {}] ========", n + 1);
        let input = String::from(input);
        println!("Input\t\t: {}", &input);

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
