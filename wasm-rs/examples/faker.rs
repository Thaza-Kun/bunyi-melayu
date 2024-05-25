use onc::phonotactics::Faker;
use rand::seq::SliceRandom;
use wasm_rs::functions::parse_default_tatabunyi_toml;

const CONFIG: &'static str = r#"
default = "Melayu Lama"

[[phonotactic]]
name = "Melayu Lama"
definition.onset = [["ny", "ng", "sw", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y", "w", "r"]]
definition.nucleus = [["a", "e", "i", "o", "u"]]
definition.coda = [["ng", "m", "n", "p", "t", "k", "s", "h", "l", "r"]]
"#;

fn main() {
    println!("<< Example: Fake word generator>");
    let phonotactic = parse_default_tatabunyi_toml(CONFIG.to_string());
    let mut faker = Faker::default();

    for _ in 0..5 {
        let n = [2, 3, 4, 5].choose(&mut faker.rng).unwrap();
        println!(
            "{}",
            phonotactic.as_rule().generate_fake_word(*n, &mut faker.rng)
        );
    }
}
