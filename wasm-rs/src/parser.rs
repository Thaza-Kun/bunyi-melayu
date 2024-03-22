use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::combinator::opt;
use nom::error::ParseError;
use nom::multi::many0;
use nom::sequence::{tuple, Tuple};
use nom::{Compare, IResult, InputLength, InputTake, Parser};
use itertools::Itertools;

#[derive(Clone)]
struct AltTags<T> {
    items: Vec<T>,
    index: usize,
}

// impl<T, I, O, E> Tuple<I, O, E> for AltTags<T> {
//     fn parse(&mut self, input: I) -> IResult<I, O, E> {
//         todo!()
//     }
// }

impl<'a, I, T, E> Parser<I, I, E> for AltTags<T>
where
    I: InputTake + Compare<T> + Clone,
    T: InputLength + Clone,
    E: ParseError<I>,
{
    fn parse(&mut self, input: I) -> IResult<I, I, E> {
        match tag_no_case::<T, I, E>(self.items[self.index].clone())(input.clone()) {
            e @ Err(_) => {
                if self.index == self.items.len() - 1 {
                    return e;
                } else {
                    self.index += 1;
                    <Self as Parser<I, I, E>>::parse(self, input.clone())
                }
            }
            res => res,
        }
    }
}

impl<'a, I, T, E> Parser<I, SyllableUnit<I>, E> for Syllable<T>
where
    I: InputTake + Compare<T> + Clone,
    T: InputLength + Clone,
    E: ParseError<I>,
{
    fn parse(&mut self, input: I) -> IResult<I, SyllableUnit<I>, E> {
        tuple((
            opt(self.onset.clone()),
            self.nucleus.clone(),
            opt(self.coda.clone()),
        ))(input)
    }
}

impl<T> AltTags<T> {
    pub fn new(items: Vec<T>) -> Self {
        Self { items, index: 0 }
    }
}

struct Syllable<T> {
    onset: AltTags<T>,
    nucleus: AltTags<T>,
    coda: AltTags<T>,
}

impl<T> Syllable<T> {
    pub fn new(onset: Vec<T>, nucleus: Vec<T>, coda: Vec<T>) -> Self {
        Self {
            onset: AltTags::new(onset),
            nucleus: AltTags::new(nucleus),
            coda: AltTags::new(coda),
        }
    }

    pub fn parse(self, input: &'static str) -> IResult<&str, Vec<SyllableUnit<&str>>>
    where
        Self: Parser<&'static str, SyllableUnit<&'static str>, nom::error::Error<&'static str>>,
    {
        many0(self)(input)
    }
}

struct Word<O> {
    syllables: Vec<SyllableUnit<O>>,
}

impl<O: Copy> Word<O> {
    pub fn fill_onset_first(mut self) -> Self {
        let cloned = self.syllables.clone();
        for (index, (lead, lag)) in cloned.iter().tuple_windows().enumerate() {
            match (lead.2, lag.0) {
                (Some(s), None) => {
                    self.syllables[index].2 = None;
                    self.syllables[index + 1].0 = Some(s);

                },
                (_,_) => {}
            }
        }
        self
    }
}

impl<O> From<Vec<SyllableUnit<O>>> for Word<O> {
    fn from(value: Vec<SyllableUnit<O>>) -> Self {
        Self { syllables: value }
    }
}

type SyllableUnit<O> = (Option<O>, O, Option<O>);


#[cfg(test)]
mod test {
    use nom::multi::many0;

    use crate::parser::{Syllable, SyllableUnit, Word};

    #[test]
    fn test_malay_phonotactic() {
        let malay_syllable: Syllable<&str> = Syllable::new(
            vec![
                "ny", "ng", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g", "s", "h", "l", "y",
                "w", "r",
            ],
            vec!["a", "e", "i", "o", "u"],
            vec!["ng", "m", "n", "p", "t", "k", "s", "h", "l", "r"],
        );

        let word = "penerangan";
        let (rem, word) = malay_syllable.parse(word).expect("Error");
        let w = Word::from(word).fill_onset_first();
        assert_eq!(rem, "");
        assert_eq!(
            w.syllables,
            vec![
                (Some("p"), "e", None),
                (Some("n"), "e", None),
                (Some("r"), "a", None),
                (Some("ng"), "a", Some("n"))
                ]
            );
    }
    #[test]
    fn test_malay_arab_phonotactic() {
        let arab_syllable: Syllable<&str> = Syllable::new(
            vec![
                "kh", "sy", "th", "gh", "q", "f", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g",
                "s", "h", "l", "y", "w", "r",
                ],
                vec!["a", "e", "i", "o", "u"],
                vec![
                "kh", "sy", "th", "gh", "q", "f", "m", "n", "p", "t", "c", "k", "b", "d", "j", "g",
                "s", "h", "l", "y", "w", "r",
                ],
            );
            let word = "mesyuarat";
            let (rem, word) = arab_syllable.parse(word).expect("Error");
            let w = Word::from(word).fill_onset_first();
            assert_eq!(rem, "");
            assert_eq!(
                w.syllables,
                vec![
                    (Some("m"), "e", None),
                    (Some("sy"), "u", None),
                    (None, "a", None),
                    (Some("r"), "a", Some("t"))
                    ]
                );
            }
}
