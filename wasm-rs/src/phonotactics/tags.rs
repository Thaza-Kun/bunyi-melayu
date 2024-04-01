use std::io::Stdout;

use nom::{
    bytes::complete::tag_no_case, combinator::opt, error::ParseError, multi::many0,
    sequence::tuple, Compare, IResult, InputLength, InputTake, Parser,
};
use serde::Deserialize;

use super::{Phrase, SyllableUnit};

#[derive(Clone, Deserialize, Default)]
pub(crate) struct AltTagVec<T> {
    pub(crate) items: Vec<T>,
    #[serde(skip)]
    index: usize,
}

#[derive(Clone, Deserialize, Default)]
pub struct SyllableTags<T> {
    pub(crate) onset: AltTagVec<T>,
    pub(crate) nucleus: AltTagVec<T>,
    pub(crate) coda: AltTagVec<T>,
}

impl<T: Ord + InputLength> AltTagVec<T> {
    pub fn new_ordered(mut items: Vec<T>) -> Self {
        items.sort_by(|a, b| b.input_len().cmp(&a.input_len()));
        Self { items, index: 0 }
    }
}

impl<T> AltTagVec<T> {
    pub fn new(items: Vec<T>) -> Self {
        Self { items, index: 0 }
    }
}

impl AltTagVec<String> {
    pub fn as_str(&self) -> AltTagVec<&str> {
        let new = self.items.iter().map(|a| a.as_str()).collect();
        AltTagVec::<&str>::new_ordered(new)
    }
}

impl AltTagVec<&'_ str> {
    pub fn as_string(&self) -> AltTagVec<String> {
        let new = self.items.iter().map(|a| String::from(*a)).collect();
        AltTagVec::<String>::new(new)
    }
}

impl<T> SyllableTags<T> {
    pub fn new(onset: Vec<T>, nucleus: Vec<T>, coda: Vec<T>) -> Self {
        Self {
            onset: AltTagVec::new(onset),
            nucleus: AltTagVec::new(nucleus),
            coda: AltTagVec::new(coda),
        }
    }
    pub fn new_ordered(onset: Vec<T>, nucleus: Vec<T>, coda: Vec<T>) -> Self
    where
        T: Ord + InputLength,
    {
        Self {
            onset: AltTagVec::new_ordered(onset),
            nucleus: AltTagVec::new_ordered(nucleus),
            coda: AltTagVec::new_ordered(coda),
        }
    }

    pub fn parse_tags<'a>(self, input: &'a str) -> IResult<&'a str, Phrase<&'a str>>
    where
        Self: Parser<&'a str, SyllableUnit<&'a str>, nom::error::Error<&'a str>>,
    {
        many0(self)(&input).map(|(rest, parsed)| (rest, Phrase::from(parsed)))
    }
}

impl SyllableTags<String> {
    pub fn as_str(&self) -> SyllableTags<&str> {
        SyllableTags::<&str>::new(
            self.onset.as_str().items,
            self.nucleus.as_str().items,
            self.coda.as_str().items,
        )
    }
}

impl SyllableTags<&'_ str> {
    pub fn as_string(&mut self) -> SyllableTags<String> {
        SyllableTags::<String>::new(
            self.onset.as_string().items,
            self.nucleus.as_string().items,
            self.coda.as_string().items,
        )
    }
}

impl<'a, I, E> Parser<I, I, E> for AltTagVec<&'a str>
where
    I: InputTake + Compare<&'a str> + Clone,
    E: ParseError<I>,
{
    fn parse(&mut self, input: I) -> IResult<I, I, E> {
        match tag_no_case::<&'a str, I, E>(self.items[self.index])(input.clone()) {
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

impl<'a, I, E> Parser<I, SyllableUnit<I>, E> for SyllableTags<&'a str>
where
    I: InputTake + Compare<&'a str> + Clone,
    E: ParseError<I>,
{
    fn parse(&mut self, input: I) -> IResult<I, SyllableUnit<I>, E> {
        tuple((
            opt(self.onset.clone()),
            self.nucleus.clone(),
            opt(self.coda.clone()),
        ))(input)
        .map(|(rest, parsed)| (rest, SyllableUnit::from(parsed)))
    }
}
