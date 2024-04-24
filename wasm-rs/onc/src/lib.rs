pub mod phonotactics;

pub use nom::IResult;

pub mod affixes;

use crate::phonotactics::Phrase;

pub struct ParseResult<'a> {
    pub full: bool,
    pub rest: String,
    pub phrase: Phrase<&'a str>,
}

impl<'a> From<IResult<&'a str, Phrase<&'a str>>> for ParseResult<'a> {
    fn from(value: IResult<&'a str, Phrase<&'a str>>) -> Self {
        match value {
            Ok((rest, phrase)) => Self {
                full: rest.is_empty(),
                rest: String::from(rest),
                phrase,
            },
            Err(_e) => Self {
                full: false,
                rest: "".into(),
                phrase: Phrase { syllables: vec![] },
            },
        }
    }
}
