use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::combinator::opt;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;

type SyllableUnit<'a> = (Option<&'a str>, &'a str, Option<&'a str>);

fn parse_syllables(input: &str) -> IResult<&str, Vec<SyllableUnit>> {
    // HARDCODING FIRST
    // NOTE: nom::character::complete::one_of can detect single characters. How about digraphs?
    let malay_onset = alt((
        tag_no_case("ny"),
        tag_no_case("ng"),
        tag_no_case("m"),
        tag_no_case("n"),
        tag_no_case("p"),
        tag_no_case("t"),
        tag_no_case("c"),
        tag_no_case("k"),
        tag_no_case("b"),
        tag_no_case("d"),
        tag_no_case("j"),
        tag_no_case("g"),
        tag_no_case("s"),
        tag_no_case("h"),
        tag_no_case("l"),
        tag_no_case("y"),
        tag_no_case("w"),
        tag_no_case("r"),
    ));
    let malay_nucleus = alt((
        tag_no_case("a"),
        tag_no_case("e"),
        tag_no_case("i"),
        tag_no_case("o"),
        tag_no_case("u"),
    ));
    let malay_coda = alt((
        tag_no_case("ng"),
        tag_no_case("m"),
        tag_no_case("n"),
        tag_no_case("p"),
        tag_no_case("t"),
        tag_no_case("k"),
        tag_no_case("s"),
        tag_no_case("h"),
        tag_no_case("l"),
        tag_no_case("r"),
    ));

    // TODO: Prioritize Onset first before Coda
    many0(tuple((opt(malay_onset), malay_nucleus, opt(malay_coda))))(input)
}

#[cfg(test)]
mod test {
    use super::parse_syllables;

    #[test]
    fn test_malay_phonotactic() {
        let word = "penerangan";
        let (rem, codas) = parse_syllables(word).expect("Error");
        assert_eq!(rem, "");
        assert_eq!(
            codas,
            vec![
                (Some("p"), "e", None),
                (Some("n"), "e", None),
                (Some("r"), "a", None),
                (Some("ng"), "a", Some("n"))
            ]
        );
    }
}
