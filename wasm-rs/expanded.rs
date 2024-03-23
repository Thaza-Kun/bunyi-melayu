#![feature(prelude_import)]
//! # nom, eating data byte by byte
//!
//! nom is a parser combinator library with a focus on safe parsing,
//! streaming patterns, and as much as possible zero copy.
//!
//! ## Example
//!
//! ```rust
//! use nom::{
//!   IResult,
//!   bytes::complete::{tag, take_while_m_n},
//!   combinator::map_res,
//!   sequence::tuple};
//!
//! #[derive(Debug,PartialEq)]
//! pub struct Color {
//!   pub red:     u8,
//!   pub green:   u8,
//!   pub blue:    u8,
//! }
//!
//! fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
//!   u8::from_str_radix(input, 16)
//! }
//!
//! fn is_hex_digit(c: char) -> bool {
//!   c.is_digit(16)
//! }
//!
//! fn hex_primary(input: &str) -> IResult<&str, u8> {
//!   map_res(
//!     take_while_m_n(2, 2, is_hex_digit),
//!     from_hex
//!   )(input)
//! }
//!
//! fn hex_color(input: &str) -> IResult<&str, Color> {
//!   let (input, _) = tag("#")(input)?;
//!   let (input, (red, green, blue)) = tuple((hex_primary, hex_primary, hex_primary))(input)?;
//!
//!   Ok((input, Color { red, green, blue }))
//! }
//!
//! fn main() {
//!   assert_eq!(hex_color("#2F14DF"), Ok(("", Color {
//!     red: 47,
//!     green: 20,
//!     blue: 223,
//!   })));
//! }
//! ```
//!
//! The code is available on [Github](https://github.com/Geal/nom)
//!
//! There are a few [guides](https://github.com/Geal/nom/tree/main/doc) with more details
//! about [how to write parsers](https://github.com/Geal/nom/blob/main/doc/making_a_new_parser_from_scratch.md),
//! or the [error management system](https://github.com/Geal/nom/blob/main/doc/error_management.md).
//! You can also check out the [recipes] module that contains examples of common patterns.
//!
//! **Looking for a specific combinator? Read the
//! ["choose a combinator" guide](https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md)**
//!
//! If you are upgrading to nom 5.0, please read the
//! [migration document](https://github.com/Geal/nom/blob/main/doc/upgrading_to_nom_5.md).
//!
//! ## Parser combinators
//!
//! Parser combinators are an approach to parsers that is very different from
//! software like [lex](https://en.wikipedia.org/wiki/Lex_(software)) and
//! [yacc](https://en.wikipedia.org/wiki/Yacc). Instead of writing the grammar
//! in a separate syntax and generating the corresponding code, you use very small
//! functions with very specific purposes, like "take 5 bytes", or "recognize the
//! word 'HTTP'", and assemble them in meaningful patterns like "recognize
//! 'HTTP', then a space, then a version".
//! The resulting code is small, and looks like the grammar you would have
//! written with other parser approaches.
//!
//! This gives us a few advantages:
//!
//! - The parsers are small and easy to write
//! - The parsers components are easy to reuse (if they're general enough, please add them to nom!)
//! - The parsers components are easy to test separately (unit tests and property-based tests)
//! - The parser combination code looks close to the grammar you would have written
//! - You can build partial parsers, specific to the data you need at the moment, and ignore the rest
//!
//! Here is an example of one such parser, to recognize text between parentheses:
//!
//! ```rust
//! use nom::{
//!   IResult,
//!   sequence::delimited,
//!   // see the "streaming/complete" paragraph lower for an explanation of these submodules
//!   character::complete::char,
//!   bytes::complete::is_not
//! };
//!
//! fn parens(input: &str) -> IResult<&str, &str> {
//!   delimited(char('('), is_not(")"), char(')'))(input)
//! }
//! ```
//!
//! It defines a function named `parens` which will recognize a sequence of the
//! character `(`, the longest byte array not containing `)`, then the character
//! `)`, and will return the byte array in the middle.
//!
//! Here is another parser, written without using nom's combinators this time:
//!
//! ```rust
//! use nom::{IResult, Err, Needed};
//!
//! # fn main() {
//! fn take4(i: &[u8]) -> IResult<&[u8], &[u8]>{
//!   if i.len() < 4 {
//!     Err(Err::Incomplete(Needed::new(4)))
//!   } else {
//!     Ok((&i[4..], &i[0..4]))
//!   }
//! }
//! # }
//! ```
//!
//! This function takes a byte array as input, and tries to consume 4 bytes.
//! Writing all the parsers manually, like this, is dangerous, despite Rust's
//! safety features. There are still a lot of mistakes one can make. That's why
//! nom provides a list of functions to help in developing parsers.
//!
//! With functions, you would write it like this:
//!
//! ```rust
//! use nom::{IResult, bytes::streaming::take};
//! fn take4(input: &str) -> IResult<&str, &str> {
//!   take(4u8)(input)
//! }
//! ```
//!
//! A parser in nom is a function which, for an input type `I`, an output type `O`
//! and an optional error type `E`, will have the following signature:
//!
//! ```rust,compile_fail
//! fn parser(input: I) -> IResult<I, O, E>;
//! ```
//!
//! Or like this, if you don't want to specify a custom error type (it will be `(I, ErrorKind)` by default):
//!
//! ```rust,compile_fail
//! fn parser(input: I) -> IResult<I, O>;
//! ```
//!
//! `IResult` is an alias for the `Result` type:
//!
//! ```rust
//! use nom::{Needed, error::Error};
//!
//! type IResult<I, O, E = Error<I>> = Result<(I, O), Err<E>>;
//!
//! enum Err<E> {
//!   Incomplete(Needed),
//!   Error(E),
//!   Failure(E),
//! }
//! ```
//!
//! It can have the following values:
//!
//! - A correct result `Ok((I,O))` with the first element being the remaining of the input (not parsed yet), and the second the output value;
//! - An error `Err(Err::Error(c))` with `c` an error that can be built from the input position and a parser specific error
//! - An error `Err(Err::Incomplete(Needed))` indicating that more input is necessary. `Needed` can indicate how much data is needed
//! - An error `Err(Err::Failure(c))`. It works like the `Error` case, except it indicates an unrecoverable error: We cannot backtrack and test another parser
//!
//! Please refer to the ["choose a combinator" guide](https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md) for an exhaustive list of parsers.
//! See also the rest of the documentation [here](https://github.com/Geal/nom/blob/main/doc).
//!
//! ## Making new parsers with function combinators
//!
//! nom is based on functions that generate parsers, with a signature like
//! this: `(arguments) -> impl Fn(Input) -> IResult<Input, Output, Error>`.
//! The arguments of a combinator can be direct values (like `take` which uses
//! a number of bytes or character as argument) or even other parsers (like
//! `delimited` which takes as argument 3 parsers, and returns the result of
//! the second one if all are successful).
//!
//! Here are some examples:
//!
//! ```rust
//! use nom::IResult;
//! use nom::bytes::complete::{tag, take};
//! fn abcd_parser(i: &str) -> IResult<&str, &str> {
//!   tag("abcd")(i) // will consume bytes if the input begins with "abcd"
//! }
//!
//! fn take_10(i: &[u8]) -> IResult<&[u8], &[u8]> {
//!   take(10u8)(i) // will consume and return 10 bytes of input
//! }
//! ```
//!
//! ## Combining parsers
//!
//! There are higher level patterns, like the **`alt`** combinator, which
//! provides a choice between multiple parsers. If one branch fails, it tries
//! the next, and returns the result of the first parser that succeeds:
//!
//! ```rust
//! use nom::IResult;
//! use nom::branch::alt;
//! use nom::bytes::complete::tag;
//!
//! let mut alt_tags = alt((tag("abcd"), tag("efgh")));
//!
//! assert_eq!(alt_tags(&b"abcdxxx"[..]), Ok((&b"xxx"[..], &b"abcd"[..])));
//! assert_eq!(alt_tags(&b"efghxxx"[..]), Ok((&b"xxx"[..], &b"efgh"[..])));
//! assert_eq!(alt_tags(&b"ijklxxx"[..]), Err(nom::Err::Error((&b"ijklxxx"[..], nom::error::ErrorKind::Tag))));
//! ```
//!
//! The **`opt`** combinator makes a parser optional. If the child parser returns
//! an error, **`opt`** will still succeed and return None:
//!
//! ```rust
//! use nom::{IResult, combinator::opt, bytes::complete::tag};
//! fn abcd_opt(i: &[u8]) -> IResult<&[u8], Option<&[u8]>> {
//!   opt(tag("abcd"))(i)
//! }
//!
//! assert_eq!(abcd_opt(&b"abcdxxx"[..]), Ok((&b"xxx"[..], Some(&b"abcd"[..]))));
//! assert_eq!(abcd_opt(&b"efghxxx"[..]), Ok((&b"efghxxx"[..], None)));
//! ```
//!
//! **`many0`** applies a parser 0 or more times, and returns a vector of the aggregated results:
//!
//! ```rust
//! # #[cfg(feature = "alloc")]
//! # fn main() {
//! use nom::{IResult, multi::many0, bytes::complete::tag};
//! use std::str;
//!
//! fn multi(i: &str) -> IResult<&str, Vec<&str>> {
//!   many0(tag("abcd"))(i)
//! }
//!
//! let a = "abcdef";
//! let b = "abcdabcdef";
//! let c = "azerty";
//! assert_eq!(multi(a), Ok(("ef",     vec!["abcd"])));
//! assert_eq!(multi(b), Ok(("ef",     vec!["abcd", "abcd"])));
//! assert_eq!(multi(c), Ok(("azerty", Vec::new())));
//! # }
//! # #[cfg(not(feature = "alloc"))]
//! # fn main() {}
//! ```
//!
//! Here are some basic combinators available:
//!
//! - **`opt`**: Will make the parser optional (if it returns the `O` type, the new parser returns `Option<O>`)
//! - **`many0`**: Will apply the parser 0 or more times (if it returns the `O` type, the new parser returns `Vec<O>`)
//! - **`many1`**: Will apply the parser 1 or more times
//!
//! There are more complex (and more useful) parsers like `tuple`, which is
//! used to apply a series of parsers then assemble their results.
//!
//! Example with `tuple`:
//!
//! ```rust
//! # fn main() {
//! use nom::{error::ErrorKind, Needed,
//! number::streaming::be_u16,
//! bytes::streaming::{tag, take},
//! sequence::tuple};
//!
//! let mut tpl = tuple((be_u16, take(3u8), tag("fg")));
//!
//! assert_eq!(
//!   tpl(&b"abcdefgh"[..]),
//!   Ok((
//!     &b"h"[..],
//!     (0x6162u16, &b"cde"[..], &b"fg"[..])
//!   ))
//! );
//! assert_eq!(tpl(&b"abcde"[..]), Err(nom::Err::Incomplete(Needed::new(2))));
//! let input = &b"abcdejk"[..];
//! assert_eq!(tpl(input), Err(nom::Err::Error((&input[5..], ErrorKind::Tag))));
//! # }
//! ```
//!
//! But you can also use a sequence of combinators written in imperative style,
//! thanks to the `?` operator:
//!
//! ```rust
//! # fn main() {
//! use nom::{IResult, bytes::complete::tag};
//!
//! #[derive(Debug, PartialEq)]
//! struct A {
//!   a: u8,
//!   b: u8
//! }
//!
//! fn ret_int1(i:&[u8]) -> IResult<&[u8], u8> { Ok((i,1)) }
//! fn ret_int2(i:&[u8]) -> IResult<&[u8], u8> { Ok((i,2)) }
//!
//! fn f(i: &[u8]) -> IResult<&[u8], A> {
//!   // if successful, the parser returns `Ok((remaining_input, output_value))` that we can destructure
//!   let (i, _) = tag("abcd")(i)?;
//!   let (i, a) = ret_int1(i)?;
//!   let (i, _) = tag("efgh")(i)?;
//!   let (i, b) = ret_int2(i)?;
//!
//!   Ok((i, A { a, b }))
//! }
//!
//! let r = f(b"abcdefghX");
//! assert_eq!(r, Ok((&b"X"[..], A{a: 1, b: 2})));
//! # }
//! ```
//!
//! ## Streaming / Complete
//!
//! Some of nom's modules have `streaming` or `complete` submodules. They hold
//! different variants of the same combinators.
//!
//! A streaming parser assumes that we might not have all of the input data.
//! This can happen with some network protocol or large file parsers, where the
//! input buffer can be full and need to be resized or refilled.
//!
//! A complete parser assumes that we already have all of the input data.
//! This will be the common case with small files that can be read entirely to
//! memory.
//!
//! Here is how it works in practice:
//!
//! ```rust
//! use nom::{IResult, Err, Needed, error::{Error, ErrorKind}, bytes, character};
//!
//! fn take_streaming(i: &[u8]) -> IResult<&[u8], &[u8]> {
//!   bytes::streaming::take(4u8)(i)
//! }
//!
//! fn take_complete(i: &[u8]) -> IResult<&[u8], &[u8]> {
//!   bytes::complete::take(4u8)(i)
//! }
//!
//! // both parsers will take 4 bytes as expected
//! assert_eq!(take_streaming(&b"abcde"[..]), Ok((&b"e"[..], &b"abcd"[..])));
//! assert_eq!(take_complete(&b"abcde"[..]), Ok((&b"e"[..], &b"abcd"[..])));
//!
//! // if the input is smaller than 4 bytes, the streaming parser
//! // will return `Incomplete` to indicate that we need more data
//! assert_eq!(take_streaming(&b"abc"[..]), Err(Err::Incomplete(Needed::new(1))));
//!
//! // but the complete parser will return an error
//! assert_eq!(take_complete(&b"abc"[..]), Err(Err::Error(Error::new(&b"abc"[..], ErrorKind::Eof))));
//!
//! // the alpha0 function recognizes 0 or more alphabetic characters
//! fn alpha0_streaming(i: &str) -> IResult<&str, &str> {
//!   character::streaming::alpha0(i)
//! }
//!
//! fn alpha0_complete(i: &str) -> IResult<&str, &str> {
//!   character::complete::alpha0(i)
//! }
//!
//! // if there's a clear limit to the recognized characters, both parsers work the same way
//! assert_eq!(alpha0_streaming("abcd;"), Ok((";", "abcd")));
//! assert_eq!(alpha0_complete("abcd;"), Ok((";", "abcd")));
//!
//! // but when there's no limit, the streaming version returns `Incomplete`, because it cannot
//! // know if more input data should be recognized. The whole input could be "abcd;", or
//! // "abcde;"
//! assert_eq!(alpha0_streaming("abcd"), Err(Err::Incomplete(Needed::new(1))));
//!
//! // while the complete version knows that all of the data is there
//! assert_eq!(alpha0_complete("abcd"), Ok(("", "abcd")));
//! ```
//! **Going further:** Read the [guides](https://github.com/Geal/nom/tree/main/doc),
//! check out the [recipes]!
#![deny(missing_docs)]
#[prelude_import]
use std::prelude::rust_2018::*;
#[macro_use]
extern crate std;
#[cfg(feature = "alloc")]
#[macro_use]
extern crate alloc;
/// Lib module to re-export everything needed from `std` or `core`/`alloc`. This is how `serde` does
/// it, albeit there it is not public.
pub mod lib {
    #[cfg(feature = "std")]
    /// internal std exports for no_std compatibility
    pub mod std {
        #[doc(hidden)]
        pub use std::{
            alloc, borrow, boxed, cmp, collections, convert, fmt, hash, iter, mem, ops,
            option, result, slice, str, string, vec,
        };
        /// internal reproduction of std prelude
        #[doc(hidden)]
        pub mod prelude {
            pub use std::prelude as v1;
        }
    }
}
pub use self::bits::*;
pub use self::internal::*;
pub use self::traits::*;
pub use self::str::*;
#[macro_use]
mod macros {}
#[macro_use]
pub mod error {
    //! Error management
    //!
    //! Parsers are generic over their error type, requiring that it implements
    //! the `error::ParseError<Input>` trait.
    use crate::internal::Parser;
    use crate::lib::std::fmt;
    /// This trait must be implemented by the error type of a nom parser.
    ///
    /// There are already implementations of it for `(Input, ErrorKind)`
    /// and `VerboseError<Input>`.
    ///
    /// It provides methods to create an error from some combinators,
    /// and combine existing errors in combinators like `alt`.
    pub trait ParseError<I>: Sized {
        /// Creates an error from the input position and an [ErrorKind]
        fn from_error_kind(input: I, kind: ErrorKind) -> Self;
        /// Combines an existing error with a new one created from the input
        /// position and an [ErrorKind]. This is useful when backtracking
        /// through a parse tree, accumulating error context on the way
        fn append(input: I, kind: ErrorKind, other: Self) -> Self;
        /// Creates an error from an input position and an expected character
        fn from_char(input: I, _: char) -> Self {
            Self::from_error_kind(input, ErrorKind::Char)
        }
        /// Combines two existing errors. This function is used to compare errors
        /// generated in various branches of `alt`.
        fn or(self, other: Self) -> Self {
            other
        }
    }
    /// This trait is required by the `context` combinator to add a static string
    /// to an existing error
    pub trait ContextError<I>: Sized {
        /// Creates a new error from an input position, a static string and an existing error.
        /// This is used mainly in the [context] combinator, to add user friendly information
        /// to errors when backtracking through a parse tree
        fn add_context(_input: I, _ctx: &'static str, other: Self) -> Self {
            other
        }
    }
    /// This trait is required by the `map_res` combinator to integrate
    /// error types from external functions, like [std::str::FromStr]
    pub trait FromExternalError<I, E> {
        /// Creates a new error from an input position, an [ErrorKind] indicating the
        /// wrapping parser, and an external error
        fn from_external_error(input: I, kind: ErrorKind, e: E) -> Self;
    }
    /// default error type, only contains the error' location and code
    pub struct Error<I> {
        /// position of the error in the input data
        pub input: I,
        /// nom error code
        pub code: ErrorKind,
    }
    #[automatically_derived]
    impl<I: ::core::fmt::Debug> ::core::fmt::Debug for Error<I> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Error",
                "input",
                &self.input,
                "code",
                &&self.code,
            )
        }
    }
    #[automatically_derived]
    impl<I> ::core::marker::StructuralPartialEq for Error<I> {}
    #[automatically_derived]
    impl<I: ::core::cmp::PartialEq> ::core::cmp::PartialEq for Error<I> {
        #[inline]
        fn eq(&self, other: &Error<I>) -> bool {
            self.input == other.input && self.code == other.code
        }
    }
    impl<I> Error<I> {
        /// creates a new basic error
        pub fn new(input: I, code: ErrorKind) -> Error<I> {
            Error { input, code }
        }
    }
    impl<I> ParseError<I> for Error<I> {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            Error { input, code: kind }
        }
        fn append(_: I, _: ErrorKind, other: Self) -> Self {
            other
        }
    }
    impl<I> ContextError<I> for Error<I> {}
    impl<I, E> FromExternalError<I, E> for Error<I> {
        /// Create a new error from an input position and an external error
        fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
            Error { input, code: kind }
        }
    }
    /// The Display implementation allows the std::error::Error implementation
    impl<I: fmt::Display> fmt::Display for Error<I> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_fmt(format_args!("error {0:?} at: {1}", self.code, self.input))
        }
    }
    #[cfg(feature = "std")]
    impl<I: fmt::Debug + fmt::Display> std::error::Error for Error<I> {}
    impl<I> ParseError<I> for (I, ErrorKind) {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            (input, kind)
        }
        fn append(_: I, _: ErrorKind, other: Self) -> Self {
            other
        }
    }
    impl<I> ContextError<I> for (I, ErrorKind) {}
    impl<I, E> FromExternalError<I, E> for (I, ErrorKind) {
        fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
            (input, kind)
        }
    }
    impl<I> ParseError<I> for () {
        fn from_error_kind(_: I, _: ErrorKind) -> Self {}
        fn append(_: I, _: ErrorKind, _: Self) -> Self {}
    }
    impl<I> ContextError<I> for () {}
    impl<I, E> FromExternalError<I, E> for () {
        fn from_external_error(_input: I, _kind: ErrorKind, _e: E) -> Self {}
    }
    /// Creates an error from the input position and an [ErrorKind]
    pub fn make_error<I, E: ParseError<I>>(input: I, kind: ErrorKind) -> E {
        E::from_error_kind(input, kind)
    }
    /// Combines an existing error with a new one created from the input
    /// position and an [ErrorKind]. This is useful when backtracking
    /// through a parse tree, accumulating error context on the way
    pub fn append_error<I, E: ParseError<I>>(input: I, kind: ErrorKind, other: E) -> E {
        E::append(input, kind, other)
    }
    /// This error type accumulates errors and their position when backtracking
    /// through a parse tree. With some post processing (cf `examples/json.rs`),
    /// it can be used to display user friendly error messages
    #[cfg(feature = "alloc")]
    pub struct VerboseError<I> {
        /// List of errors accumulated by `VerboseError`, containing the affected
        /// part of input data, and some context
        pub errors: crate::lib::std::vec::Vec<(I, VerboseErrorKind)>,
    }
    #[automatically_derived]
    impl<I: ::core::clone::Clone> ::core::clone::Clone for VerboseError<I> {
        #[inline]
        fn clone(&self) -> VerboseError<I> {
            VerboseError {
                errors: ::core::clone::Clone::clone(&self.errors),
            }
        }
    }
    #[automatically_derived]
    impl<I: ::core::fmt::Debug> ::core::fmt::Debug for VerboseError<I> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "VerboseError",
                "errors",
                &&self.errors,
            )
        }
    }
    #[automatically_derived]
    impl<I> ::core::marker::StructuralPartialEq for VerboseError<I> {}
    #[automatically_derived]
    impl<I: ::core::cmp::PartialEq> ::core::cmp::PartialEq for VerboseError<I> {
        #[inline]
        fn eq(&self, other: &VerboseError<I>) -> bool {
            self.errors == other.errors
        }
    }
    #[cfg(feature = "alloc")]
    /// Error context for `VerboseError`
    pub enum VerboseErrorKind {
        /// Static string added by the `context` function
        Context(&'static str),
        /// Indicates which character was expected by the `char` function
        Char(char),
        /// Error kind given by various nom parsers
        Nom(ErrorKind),
    }
    #[automatically_derived]
    impl ::core::clone::Clone for VerboseErrorKind {
        #[inline]
        fn clone(&self) -> VerboseErrorKind {
            match self {
                VerboseErrorKind::Context(__self_0) => {
                    VerboseErrorKind::Context(::core::clone::Clone::clone(__self_0))
                }
                VerboseErrorKind::Char(__self_0) => {
                    VerboseErrorKind::Char(::core::clone::Clone::clone(__self_0))
                }
                VerboseErrorKind::Nom(__self_0) => {
                    VerboseErrorKind::Nom(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for VerboseErrorKind {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                VerboseErrorKind::Context(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Context",
                        &__self_0,
                    )
                }
                VerboseErrorKind::Char(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Char",
                        &__self_0,
                    )
                }
                VerboseErrorKind::Nom(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Nom",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for VerboseErrorKind {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for VerboseErrorKind {
        #[inline]
        fn eq(&self, other: &VerboseErrorKind) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (
                        VerboseErrorKind::Context(__self_0),
                        VerboseErrorKind::Context(__arg1_0),
                    ) => *__self_0 == *__arg1_0,
                    (
                        VerboseErrorKind::Char(__self_0),
                        VerboseErrorKind::Char(__arg1_0),
                    ) => *__self_0 == *__arg1_0,
                    (
                        VerboseErrorKind::Nom(__self_0),
                        VerboseErrorKind::Nom(__arg1_0),
                    ) => *__self_0 == *__arg1_0,
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
        }
    }
    #[cfg(feature = "alloc")]
    impl<I> ParseError<I> for VerboseError<I> {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            VerboseError {
                errors: <[_]>::into_vec(
                    #[rustc_box]
                    ::alloc::boxed::Box::new([(input, VerboseErrorKind::Nom(kind))]),
                ),
            }
        }
        fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
            other.errors.push((input, VerboseErrorKind::Nom(kind)));
            other
        }
        fn from_char(input: I, c: char) -> Self {
            VerboseError {
                errors: <[_]>::into_vec(
                    #[rustc_box]
                    ::alloc::boxed::Box::new([(input, VerboseErrorKind::Char(c))]),
                ),
            }
        }
    }
    #[cfg(feature = "alloc")]
    impl<I> ContextError<I> for VerboseError<I> {
        fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
            other.errors.push((input, VerboseErrorKind::Context(ctx)));
            other
        }
    }
    #[cfg(feature = "alloc")]
    impl<I, E> FromExternalError<I, E> for VerboseError<I> {
        /// Create a new error from an input position and an external error
        fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
            Self::from_error_kind(input, kind)
        }
    }
    #[cfg(feature = "alloc")]
    impl<I: fmt::Display> fmt::Display for VerboseError<I> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_fmt(format_args!("Parse error:\n"))?;
            for (input, error) in &self.errors {
                match error {
                    VerboseErrorKind::Nom(e) => {
                        f.write_fmt(format_args!("{0:?} at: {1}\n", e, input))?
                    }
                    VerboseErrorKind::Char(c) => {
                        f.write_fmt(
                            format_args!("expected \'{0}\' at: {1}\n", c, input),
                        )?
                    }
                    VerboseErrorKind::Context(s) => {
                        f.write_fmt(
                            format_args!("in section \'{0}\', at: {1}\n", s, input),
                        )?
                    }
                }
            }
            Ok(())
        }
    }
    #[cfg(feature = "std")]
    impl<I: fmt::Debug + fmt::Display> std::error::Error for VerboseError<I> {}
    use crate::internal::{Err, IResult};
    /// Create a new error from an input position, a static string and an existing error.
    /// This is used mainly in the [context] combinator, to add user friendly information
    /// to errors when backtracking through a parse tree
    pub fn context<I: Clone, E: ContextError<I>, F, O>(
        context: &'static str,
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, O, E>
    where
        F: Parser<I, O, E>,
    {
        move |i: I| match f.parse(i.clone()) {
            Ok(o) => Ok(o),
            Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
            Err(Err::Error(e)) => Err(Err::Error(E::add_context(i, context, e))),
            Err(Err::Failure(e)) => Err(Err::Failure(E::add_context(i, context, e))),
        }
    }
    /// Transforms a `VerboseError` into a trace with input position information
    #[cfg(feature = "alloc")]
    pub fn convert_error<I: core::ops::Deref<Target = str>>(
        input: I,
        e: VerboseError<I>,
    ) -> crate::lib::std::string::String {
        use crate::lib::std::fmt::Write;
        use crate::traits::Offset;
        let mut result = crate::lib::std::string::String::new();
        for (i, (substring, kind)) in e.errors.iter().enumerate() {
            let offset = input.offset(substring);
            if input.is_empty() {
                match kind {
                    VerboseErrorKind::Char(c) => {
                        (&mut result)
                            .write_fmt(
                                format_args!(
                                    "{0}: expected \'{1}\', got empty input\n\n",
                                    i,
                                    c,
                                ),
                            )
                    }
                    VerboseErrorKind::Context(s) => {
                        (&mut result)
                            .write_fmt(
                                format_args!("{0}: in {1}, got empty input\n\n", i, s),
                            )
                    }
                    VerboseErrorKind::Nom(e) => {
                        (&mut result)
                            .write_fmt(
                                format_args!("{0}: in {1:?}, got empty input\n\n", i, e),
                            )
                    }
                }
            } else {
                let prefix = &input.as_bytes()[..offset];
                let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;
                let line_begin = prefix
                    .iter()
                    .rev()
                    .position(|&b| b == b'\n')
                    .map(|pos| offset - pos)
                    .unwrap_or(0);
                let line = input[line_begin..]
                    .lines()
                    .next()
                    .unwrap_or(&input[line_begin..])
                    .trim_end();
                let column_number = line.offset(substring) + 1;
                match kind {
                    VerboseErrorKind::Char(c) => {
                        if let Some(actual) = substring.chars().next() {
                            (&mut result)
                                .write_fmt(
                                    format_args!(
                                        "{0}: at line {1}:\n{2}\n{3:>4$}\nexpected \'{5}\', found {6}\n\n",
                                        i,
                                        line_number,
                                        line,
                                        '^',
                                        column_number,
                                        c,
                                        actual,
                                    ),
                                )
                        } else {
                            (&mut result)
                                .write_fmt(
                                    format_args!(
                                        "{0}: at line {1}:\n{2}\n{3:>4$}\nexpected \'{5}\', got end of input\n\n",
                                        i,
                                        line_number,
                                        line,
                                        '^',
                                        column_number,
                                        c,
                                    ),
                                )
                        }
                    }
                    VerboseErrorKind::Context(s) => {
                        (&mut result)
                            .write_fmt(
                                format_args!(
                                    "{0}: at line {1}, in {2}:\n{3}\n{4:>5$}\n\n",
                                    i,
                                    line_number,
                                    s,
                                    line,
                                    '^',
                                    column_number,
                                ),
                            )
                    }
                    VerboseErrorKind::Nom(e) => {
                        (&mut result)
                            .write_fmt(
                                format_args!(
                                    "{0}: at line {1}, in {2:?}:\n{3}\n{4:>5$}\n\n",
                                    i,
                                    line_number,
                                    e,
                                    line,
                                    '^',
                                    column_number,
                                ),
                            )
                    }
                }
            }
                .unwrap();
        }
        result
    }
    /// Indicates which parser returned an error
    #[rustfmt::skip]
    #[allow(deprecated, missing_docs)]
    pub enum ErrorKind {
        Tag,
        MapRes,
        MapOpt,
        Alt,
        IsNot,
        IsA,
        SeparatedList,
        SeparatedNonEmptyList,
        Many0,
        Many1,
        ManyTill,
        Count,
        TakeUntil,
        LengthValue,
        TagClosure,
        Alpha,
        Digit,
        HexDigit,
        OctDigit,
        AlphaNumeric,
        Space,
        MultiSpace,
        LengthValueFn,
        Eof,
        Switch,
        TagBits,
        OneOf,
        NoneOf,
        Char,
        CrLf,
        RegexpMatch,
        RegexpMatches,
        RegexpFind,
        RegexpCapture,
        RegexpCaptures,
        TakeWhile1,
        Complete,
        Fix,
        Escaped,
        EscapedTransform,
        NonEmpty,
        ManyMN,
        Not,
        Permutation,
        Verify,
        TakeTill1,
        TakeWhileMN,
        TooLarge,
        Many0Count,
        Many1Count,
        Float,
        Satisfy,
        Fail,
    }
    #[automatically_derived]
    #[allow(deprecated, missing_docs)]
    impl ::core::fmt::Debug for ErrorKind {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    ErrorKind::Tag => "Tag",
                    ErrorKind::MapRes => "MapRes",
                    ErrorKind::MapOpt => "MapOpt",
                    ErrorKind::Alt => "Alt",
                    ErrorKind::IsNot => "IsNot",
                    ErrorKind::IsA => "IsA",
                    ErrorKind::SeparatedList => "SeparatedList",
                    ErrorKind::SeparatedNonEmptyList => "SeparatedNonEmptyList",
                    ErrorKind::Many0 => "Many0",
                    ErrorKind::Many1 => "Many1",
                    ErrorKind::ManyTill => "ManyTill",
                    ErrorKind::Count => "Count",
                    ErrorKind::TakeUntil => "TakeUntil",
                    ErrorKind::LengthValue => "LengthValue",
                    ErrorKind::TagClosure => "TagClosure",
                    ErrorKind::Alpha => "Alpha",
                    ErrorKind::Digit => "Digit",
                    ErrorKind::HexDigit => "HexDigit",
                    ErrorKind::OctDigit => "OctDigit",
                    ErrorKind::AlphaNumeric => "AlphaNumeric",
                    ErrorKind::Space => "Space",
                    ErrorKind::MultiSpace => "MultiSpace",
                    ErrorKind::LengthValueFn => "LengthValueFn",
                    ErrorKind::Eof => "Eof",
                    ErrorKind::Switch => "Switch",
                    ErrorKind::TagBits => "TagBits",
                    ErrorKind::OneOf => "OneOf",
                    ErrorKind::NoneOf => "NoneOf",
                    ErrorKind::Char => "Char",
                    ErrorKind::CrLf => "CrLf",
                    ErrorKind::RegexpMatch => "RegexpMatch",
                    ErrorKind::RegexpMatches => "RegexpMatches",
                    ErrorKind::RegexpFind => "RegexpFind",
                    ErrorKind::RegexpCapture => "RegexpCapture",
                    ErrorKind::RegexpCaptures => "RegexpCaptures",
                    ErrorKind::TakeWhile1 => "TakeWhile1",
                    ErrorKind::Complete => "Complete",
                    ErrorKind::Fix => "Fix",
                    ErrorKind::Escaped => "Escaped",
                    ErrorKind::EscapedTransform => "EscapedTransform",
                    ErrorKind::NonEmpty => "NonEmpty",
                    ErrorKind::ManyMN => "ManyMN",
                    ErrorKind::Not => "Not",
                    ErrorKind::Permutation => "Permutation",
                    ErrorKind::Verify => "Verify",
                    ErrorKind::TakeTill1 => "TakeTill1",
                    ErrorKind::TakeWhileMN => "TakeWhileMN",
                    ErrorKind::TooLarge => "TooLarge",
                    ErrorKind::Many0Count => "Many0Count",
                    ErrorKind::Many1Count => "Many1Count",
                    ErrorKind::Float => "Float",
                    ErrorKind::Satisfy => "Satisfy",
                    ErrorKind::Fail => "Fail",
                },
            )
        }
    }
    #[automatically_derived]
    #[allow(deprecated, missing_docs)]
    impl ::core::marker::StructuralPartialEq for ErrorKind {}
    #[automatically_derived]
    #[allow(deprecated, missing_docs)]
    impl ::core::cmp::PartialEq for ErrorKind {
        #[inline]
        fn eq(&self, other: &ErrorKind) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    #[allow(deprecated, missing_docs)]
    impl ::core::marker::StructuralEq for ErrorKind {}
    #[automatically_derived]
    #[allow(deprecated, missing_docs)]
    impl ::core::cmp::Eq for ErrorKind {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    #[automatically_derived]
    #[allow(deprecated, missing_docs)]
    impl ::core::hash::Hash for ErrorKind {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_tag, state)
        }
    }
    #[automatically_derived]
    #[allow(deprecated, missing_docs)]
    impl ::core::clone::Clone for ErrorKind {
        #[inline]
        fn clone(&self) -> ErrorKind {
            *self
        }
    }
    #[automatically_derived]
    #[allow(deprecated, missing_docs)]
    impl ::core::marker::Copy for ErrorKind {}
    #[rustfmt::skip]
    #[allow(deprecated)]
    /// Converts an ErrorKind to a number
    pub fn error_to_u32(e: &ErrorKind) -> u32 {
        match *e {
            ErrorKind::Tag => 1,
            ErrorKind::MapRes => 2,
            ErrorKind::MapOpt => 3,
            ErrorKind::Alt => 4,
            ErrorKind::IsNot => 5,
            ErrorKind::IsA => 6,
            ErrorKind::SeparatedList => 7,
            ErrorKind::SeparatedNonEmptyList => 8,
            ErrorKind::Many1 => 9,
            ErrorKind::Count => 10,
            ErrorKind::TakeUntil => 12,
            ErrorKind::LengthValue => 15,
            ErrorKind::TagClosure => 16,
            ErrorKind::Alpha => 17,
            ErrorKind::Digit => 18,
            ErrorKind::AlphaNumeric => 19,
            ErrorKind::Space => 20,
            ErrorKind::MultiSpace => 21,
            ErrorKind::LengthValueFn => 22,
            ErrorKind::Eof => 23,
            ErrorKind::Switch => 27,
            ErrorKind::TagBits => 28,
            ErrorKind::OneOf => 29,
            ErrorKind::NoneOf => 30,
            ErrorKind::Char => 40,
            ErrorKind::CrLf => 41,
            ErrorKind::RegexpMatch => 42,
            ErrorKind::RegexpMatches => 43,
            ErrorKind::RegexpFind => 44,
            ErrorKind::RegexpCapture => 45,
            ErrorKind::RegexpCaptures => 46,
            ErrorKind::TakeWhile1 => 47,
            ErrorKind::Complete => 48,
            ErrorKind::Fix => 49,
            ErrorKind::Escaped => 50,
            ErrorKind::EscapedTransform => 51,
            ErrorKind::NonEmpty => 56,
            ErrorKind::ManyMN => 57,
            ErrorKind::HexDigit => 59,
            ErrorKind::OctDigit => 61,
            ErrorKind::Many0 => 62,
            ErrorKind::Not => 63,
            ErrorKind::Permutation => 64,
            ErrorKind::ManyTill => 65,
            ErrorKind::Verify => 66,
            ErrorKind::TakeTill1 => 67,
            ErrorKind::TakeWhileMN => 69,
            ErrorKind::TooLarge => 70,
            ErrorKind::Many0Count => 71,
            ErrorKind::Many1Count => 72,
            ErrorKind::Float => 73,
            ErrorKind::Satisfy => 74,
            ErrorKind::Fail => 75,
        }
    }
    impl ErrorKind {
        #[rustfmt::skip]
        #[allow(deprecated)]
        /// Converts an ErrorKind to a text description
        pub fn description(&self) -> &str {
            match *self {
                ErrorKind::Tag => "Tag",
                ErrorKind::MapRes => "Map on Result",
                ErrorKind::MapOpt => "Map on Option",
                ErrorKind::Alt => "Alternative",
                ErrorKind::IsNot => "IsNot",
                ErrorKind::IsA => "IsA",
                ErrorKind::SeparatedList => "Separated list",
                ErrorKind::SeparatedNonEmptyList => "Separated non empty list",
                ErrorKind::Many0 => "Many0",
                ErrorKind::Many1 => "Many1",
                ErrorKind::Count => "Count",
                ErrorKind::TakeUntil => "Take until",
                ErrorKind::LengthValue => "Length followed by value",
                ErrorKind::TagClosure => "Tag closure",
                ErrorKind::Alpha => "Alphabetic",
                ErrorKind::Digit => "Digit",
                ErrorKind::AlphaNumeric => "AlphaNumeric",
                ErrorKind::Space => "Space",
                ErrorKind::MultiSpace => "Multiple spaces",
                ErrorKind::LengthValueFn => "LengthValueFn",
                ErrorKind::Eof => "End of file",
                ErrorKind::Switch => "Switch",
                ErrorKind::TagBits => "Tag on bitstream",
                ErrorKind::OneOf => "OneOf",
                ErrorKind::NoneOf => "NoneOf",
                ErrorKind::Char => "Char",
                ErrorKind::CrLf => "CrLf",
                ErrorKind::RegexpMatch => "RegexpMatch",
                ErrorKind::RegexpMatches => "RegexpMatches",
                ErrorKind::RegexpFind => "RegexpFind",
                ErrorKind::RegexpCapture => "RegexpCapture",
                ErrorKind::RegexpCaptures => "RegexpCaptures",
                ErrorKind::TakeWhile1 => "TakeWhile1",
                ErrorKind::Complete => "Complete",
                ErrorKind::Fix => "Fix",
                ErrorKind::Escaped => "Escaped",
                ErrorKind::EscapedTransform => "EscapedTransform",
                ErrorKind::NonEmpty => "NonEmpty",
                ErrorKind::ManyMN => "Many(m, n)",
                ErrorKind::HexDigit => "Hexadecimal Digit",
                ErrorKind::OctDigit => "Octal digit",
                ErrorKind::Not => "Negation",
                ErrorKind::Permutation => "Permutation",
                ErrorKind::ManyTill => "ManyTill",
                ErrorKind::Verify => "predicate verification",
                ErrorKind::TakeTill1 => "TakeTill1",
                ErrorKind::TakeWhileMN => "TakeWhileMN",
                ErrorKind::TooLarge => "Needed data size is too large",
                ErrorKind::Many0Count => "Count occurrence of >=0 patterns",
                ErrorKind::Many1Count => "Count occurrence of >=1 patterns",
                ErrorKind::Float => "Float",
                ErrorKind::Satisfy => "Satisfy",
                ErrorKind::Fail => "Fail",
            }
        }
    }
    /// Prints a message and the input if the parser fails.
    ///
    /// The message prints the `Error` or `Incomplete`
    /// and the parser's calling code.
    ///
    /// It also displays the input in hexdump format
    ///
    /// ```rust
    /// use nom::{IResult, error::dbg_dmp, bytes::complete::tag};
    ///
    /// fn f(i: &[u8]) -> IResult<&[u8], &[u8]> {
    ///   dbg_dmp(tag("abcd"), "tag")(i)
    /// }
    ///
    ///   let a = &b"efghijkl"[..];
    ///
    /// // Will print the following message:
    /// // Error(Position(0, [101, 102, 103, 104, 105, 106, 107, 108])) at l.5 by ' tag ! ( "abcd" ) '
    /// // 00000000        65 66 67 68 69 6a 6b 6c         efghijkl
    /// f(a);
    /// ```
    #[cfg(feature = "std")]
    pub fn dbg_dmp<'a, F, O, E: std::fmt::Debug>(
        f: F,
        context: &'static str,
    ) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], O, E>
    where
        F: Fn(&'a [u8]) -> IResult<&'a [u8], O, E>,
    {
        use crate::HexDisplay;
        move |i: &'a [u8]| match f(i) {
            Err(e) => {
                {
                    ::std::io::_print(
                        format_args!(
                            "{0}: Error({1:?}) at:\n{2}\n",
                            context,
                            e,
                            i.to_hex(8),
                        ),
                    );
                };
                Err(e)
            }
            a => a,
        }
    }
}
pub mod branch {
    //! Choice combinators
    use crate::error::ErrorKind;
    use crate::error::ParseError;
    use crate::internal::{Err, IResult, Parser};
    /// Helper trait for the [alt()] combinator.
    ///
    /// This trait is implemented for tuples of up to 21 elements
    pub trait Alt<I, O, E> {
        /// Tests each parser in the tuple and returns the result of the first one that succeeds
        fn choice(&mut self, input: I) -> IResult<I, O, E>;
    }
    /// Tests a list of parsers one by one until one succeeds.
    ///
    /// It takes as argument a tuple of parsers. There is a maximum of 21
    /// parsers. If you need more, it is possible to nest them in other `alt` calls,
    /// like this: `alt(parser_a, alt(parser_b, parser_c))`
    ///
    /// ```rust
    /// # use nom::error_position;
    /// # use nom::{Err,error::ErrorKind, Needed, IResult};
    /// use nom::character::complete::{alpha1, digit1};
    /// use nom::branch::alt;
    /// # fn main() {
    /// fn parser(input: &str) -> IResult<&str, &str> {
    ///   alt((alpha1, digit1))(input)
    /// };
    ///
    /// // the first parser, alpha1, recognizes the input
    /// assert_eq!(parser("abc"), Ok(("", "abc")));
    ///
    /// // the first parser returns an error, so alt tries the second one
    /// assert_eq!(parser("123456"), Ok(("", "123456")));
    ///
    /// // both parsers failed, and with the default error type, alt will return the last error
    /// assert_eq!(parser(" "), Err(Err::Error(error_position!(" ", ErrorKind::Digit))));
    /// # }
    /// ```
    ///
    /// With a custom error type, it is possible to have alt return the error of the parser
    /// that went the farthest in the input data
    pub fn alt<I: Clone, O, E: ParseError<I>, List: Alt<I, O, E>>(
        mut l: List,
    ) -> impl FnMut(I) -> IResult<I, O, E> {
        move |i: I| l.choice(i)
    }
    /// Helper trait for the [permutation()] combinator.
    ///
    /// This trait is implemented for tuples of up to 21 elements
    pub trait Permutation<I, O, E> {
        /// Tries to apply all parsers in the tuple in various orders until all of them succeed
        fn permutation(&mut self, input: I) -> IResult<I, O, E>;
    }
    /// Applies a list of parsers in any order.
    ///
    /// Permutation will succeed if all of the child parsers succeeded.
    /// It takes as argument a tuple of parsers, and returns a
    /// tuple of the parser results.
    ///
    /// ```rust
    /// # use nom::{Err,error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::character::complete::{alpha1, digit1};
    /// use nom::branch::permutation;
    /// # fn main() {
    /// fn parser(input: &str) -> IResult<&str, (&str, &str)> {
    ///   permutation((alpha1, digit1))(input)
    /// }
    ///
    /// // permutation recognizes alphabetic characters then digit
    /// assert_eq!(parser("abc123"), Ok(("", ("abc", "123"))));
    ///
    /// // but also in inverse order
    /// assert_eq!(parser("123abc"), Ok(("", ("abc", "123"))));
    ///
    /// // it will fail if one of the parsers failed
    /// assert_eq!(parser("abc;"), Err(Err::Error(Error::new(";", ErrorKind::Digit))));
    /// # }
    /// ```
    ///
    /// The parsers are applied greedily: if there are multiple unapplied parsers
    /// that could parse the next slice of input, the first one is used.
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, IResult};
    /// use nom::branch::permutation;
    /// use nom::character::complete::{anychar, char};
    ///
    /// fn parser(input: &str) -> IResult<&str, (char, char)> {
    ///   permutation((anychar, char('a')))(input)
    /// }
    ///
    /// // anychar parses 'b', then char('a') parses 'a'
    /// assert_eq!(parser("ba"), Ok(("", ('b', 'a'))));
    ///
    /// // anychar parses 'a', then char('a') fails on 'b',
    /// // even though char('a') followed by anychar would succeed
    /// assert_eq!(parser("ab"), Err(Err::Error(Error::new("b", ErrorKind::Char))));
    /// ```
    ///
    pub fn permutation<I: Clone, O, E: ParseError<I>, List: Permutation<I, O, E>>(
        mut l: List,
    ) -> impl FnMut(I) -> IResult<I, O, E> {
        move |i: I| l.permutation(i)
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I, J) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I, J, K) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I, J, K, L) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I, J, K, L, M) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
        N: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I, J, K, L, M, N) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    match self.13.parse(input.clone()) {
                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                            let err = err.or(e);
                                                                                                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                        }
                                                                                                                        res => res,
                                                                                                                    }
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
        N: Parser<Input, Output, Error>,
        O: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    match self.13.parse(input.clone()) {
                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                            let err = err.or(e);
                                                                                                                            match self.14.parse(input.clone()) {
                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                    let err = err.or(e);
                                                                                                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                                }
                                                                                                                                res => res,
                                                                                                                            }
                                                                                                                        }
                                                                                                                        res => res,
                                                                                                                    }
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
        N: Parser<Input, Output, Error>,
        O: Parser<Input, Output, Error>,
        P: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    match self.13.parse(input.clone()) {
                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                            let err = err.or(e);
                                                                                                                            match self.14.parse(input.clone()) {
                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                    let err = err.or(e);
                                                                                                                                    match self.15.parse(input.clone()) {
                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                            let err = err.or(e);
                                                                                                                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                                        }
                                                                                                                                        res => res,
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                res => res,
                                                                                                                            }
                                                                                                                        }
                                                                                                                        res => res,
                                                                                                                    }
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
        N: Parser<Input, Output, Error>,
        O: Parser<Input, Output, Error>,
        P: Parser<Input, Output, Error>,
        Q: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    match self.13.parse(input.clone()) {
                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                            let err = err.or(e);
                                                                                                                            match self.14.parse(input.clone()) {
                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                    let err = err.or(e);
                                                                                                                                    match self.15.parse(input.clone()) {
                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                            let err = err.or(e);
                                                                                                                                            match self.16.parse(input.clone()) {
                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                    let err = err.or(e);
                                                                                                                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                                                }
                                                                                                                                                res => res,
                                                                                                                                            }
                                                                                                                                        }
                                                                                                                                        res => res,
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                res => res,
                                                                                                                            }
                                                                                                                        }
                                                                                                                        res => res,
                                                                                                                    }
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
        N: Parser<Input, Output, Error>,
        O: Parser<Input, Output, Error>,
        P: Parser<Input, Output, Error>,
        Q: Parser<Input, Output, Error>,
        R: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error>
    for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    match self.13.parse(input.clone()) {
                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                            let err = err.or(e);
                                                                                                                            match self.14.parse(input.clone()) {
                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                    let err = err.or(e);
                                                                                                                                    match self.15.parse(input.clone()) {
                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                            let err = err.or(e);
                                                                                                                                            match self.16.parse(input.clone()) {
                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                    let err = err.or(e);
                                                                                                                                                    match self.17.parse(input.clone()) {
                                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                                            let err = err.or(e);
                                                                                                                                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                                                        }
                                                                                                                                                        res => res,
                                                                                                                                                    }
                                                                                                                                                }
                                                                                                                                                res => res,
                                                                                                                                            }
                                                                                                                                        }
                                                                                                                                        res => res,
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                res => res,
                                                                                                                            }
                                                                                                                        }
                                                                                                                        res => res,
                                                                                                                    }
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
        N: Parser<Input, Output, Error>,
        O: Parser<Input, Output, Error>,
        P: Parser<Input, Output, Error>,
        Q: Parser<Input, Output, Error>,
        R: Parser<Input, Output, Error>,
        S: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error>
    for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    match self.13.parse(input.clone()) {
                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                            let err = err.or(e);
                                                                                                                            match self.14.parse(input.clone()) {
                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                    let err = err.or(e);
                                                                                                                                    match self.15.parse(input.clone()) {
                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                            let err = err.or(e);
                                                                                                                                            match self.16.parse(input.clone()) {
                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                    let err = err.or(e);
                                                                                                                                                    match self.17.parse(input.clone()) {
                                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                                            let err = err.or(e);
                                                                                                                                                            match self.18.parse(input.clone()) {
                                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                                    let err = err.or(e);
                                                                                                                                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                                                                }
                                                                                                                                                                res => res,
                                                                                                                                                            }
                                                                                                                                                        }
                                                                                                                                                        res => res,
                                                                                                                                                    }
                                                                                                                                                }
                                                                                                                                                res => res,
                                                                                                                                            }
                                                                                                                                        }
                                                                                                                                        res => res,
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                res => res,
                                                                                                                            }
                                                                                                                        }
                                                                                                                        res => res,
                                                                                                                    }
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
        N: Parser<Input, Output, Error>,
        O: Parser<Input, Output, Error>,
        P: Parser<Input, Output, Error>,
        Q: Parser<Input, Output, Error>,
        R: Parser<Input, Output, Error>,
        S: Parser<Input, Output, Error>,
        T: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error>
    for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    match self.13.parse(input.clone()) {
                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                            let err = err.or(e);
                                                                                                                            match self.14.parse(input.clone()) {
                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                    let err = err.or(e);
                                                                                                                                    match self.15.parse(input.clone()) {
                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                            let err = err.or(e);
                                                                                                                                            match self.16.parse(input.clone()) {
                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                    let err = err.or(e);
                                                                                                                                                    match self.17.parse(input.clone()) {
                                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                                            let err = err.or(e);
                                                                                                                                                            match self.18.parse(input.clone()) {
                                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                                    let err = err.or(e);
                                                                                                                                                                    match self.19.parse(input.clone()) {
                                                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                                                            let err = err.or(e);
                                                                                                                                                                            Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                                                                        }
                                                                                                                                                                        res => res,
                                                                                                                                                                    }
                                                                                                                                                                }
                                                                                                                                                                res => res,
                                                                                                                                                            }
                                                                                                                                                        }
                                                                                                                                                        res => res,
                                                                                                                                                    }
                                                                                                                                                }
                                                                                                                                                res => res,
                                                                                                                                            }
                                                                                                                                        }
                                                                                                                                        res => res,
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                res => res,
                                                                                                                            }
                                                                                                                        }
                                                                                                                        res => res,
                                                                                                                    }
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input: Clone,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
        B: Parser<Input, Output, Error>,
        C: Parser<Input, Output, Error>,
        D: Parser<Input, Output, Error>,
        E: Parser<Input, Output, Error>,
        F: Parser<Input, Output, Error>,
        G: Parser<Input, Output, Error>,
        H: Parser<Input, Output, Error>,
        I: Parser<Input, Output, Error>,
        J: Parser<Input, Output, Error>,
        K: Parser<Input, Output, Error>,
        L: Parser<Input, Output, Error>,
        M: Parser<Input, Output, Error>,
        N: Parser<Input, Output, Error>,
        O: Parser<Input, Output, Error>,
        P: Parser<Input, Output, Error>,
        Q: Parser<Input, Output, Error>,
        R: Parser<Input, Output, Error>,
        S: Parser<Input, Output, Error>,
        T: Parser<Input, Output, Error>,
        U: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error>
    for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            match self.0.parse(input.clone()) {
                Err(Err::Error(e)) => {
                    match self.1.parse(input.clone()) {
                        Err(Err::Error(e)) => {
                            let err = e.or(e);
                            match self.2.parse(input.clone()) {
                                Err(Err::Error(e)) => {
                                    let err = err.or(e);
                                    match self.3.parse(input.clone()) {
                                        Err(Err::Error(e)) => {
                                            let err = err.or(e);
                                            match self.4.parse(input.clone()) {
                                                Err(Err::Error(e)) => {
                                                    let err = err.or(e);
                                                    match self.5.parse(input.clone()) {
                                                        Err(Err::Error(e)) => {
                                                            let err = err.or(e);
                                                            match self.6.parse(input.clone()) {
                                                                Err(Err::Error(e)) => {
                                                                    let err = err.or(e);
                                                                    match self.7.parse(input.clone()) {
                                                                        Err(Err::Error(e)) => {
                                                                            let err = err.or(e);
                                                                            match self.8.parse(input.clone()) {
                                                                                Err(Err::Error(e)) => {
                                                                                    let err = err.or(e);
                                                                                    match self.9.parse(input.clone()) {
                                                                                        Err(Err::Error(e)) => {
                                                                                            let err = err.or(e);
                                                                                            match self.10.parse(input.clone()) {
                                                                                                Err(Err::Error(e)) => {
                                                                                                    let err = err.or(e);
                                                                                                    match self.11.parse(input.clone()) {
                                                                                                        Err(Err::Error(e)) => {
                                                                                                            let err = err.or(e);
                                                                                                            match self.12.parse(input.clone()) {
                                                                                                                Err(Err::Error(e)) => {
                                                                                                                    let err = err.or(e);
                                                                                                                    match self.13.parse(input.clone()) {
                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                            let err = err.or(e);
                                                                                                                            match self.14.parse(input.clone()) {
                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                    let err = err.or(e);
                                                                                                                                    match self.15.parse(input.clone()) {
                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                            let err = err.or(e);
                                                                                                                                            match self.16.parse(input.clone()) {
                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                    let err = err.or(e);
                                                                                                                                                    match self.17.parse(input.clone()) {
                                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                                            let err = err.or(e);
                                                                                                                                                            match self.18.parse(input.clone()) {
                                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                                    let err = err.or(e);
                                                                                                                                                                    match self.19.parse(input.clone()) {
                                                                                                                                                                        Err(Err::Error(e)) => {
                                                                                                                                                                            let err = err.or(e);
                                                                                                                                                                            match self.20.parse(input.clone()) {
                                                                                                                                                                                Err(Err::Error(e)) => {
                                                                                                                                                                                    let err = err.or(e);
                                                                                                                                                                                    Err(Err::Error(Error::append(input, ErrorKind::Alt, err)))
                                                                                                                                                                                }
                                                                                                                                                                                res => res,
                                                                                                                                                                            }
                                                                                                                                                                        }
                                                                                                                                                                        res => res,
                                                                                                                                                                    }
                                                                                                                                                                }
                                                                                                                                                                res => res,
                                                                                                                                                            }
                                                                                                                                                        }
                                                                                                                                                        res => res,
                                                                                                                                                    }
                                                                                                                                                }
                                                                                                                                                res => res,
                                                                                                                                            }
                                                                                                                                        }
                                                                                                                                        res => res,
                                                                                                                                    }
                                                                                                                                }
                                                                                                                                res => res,
                                                                                                                            }
                                                                                                                        }
                                                                                                                        res => res,
                                                                                                                    }
                                                                                                                }
                                                                                                                res => res,
                                                                                                            }
                                                                                                        }
                                                                                                        res => res,
                                                                                                    }
                                                                                                }
                                                                                                res => res,
                                                                                            }
                                                                                        }
                                                                                        res => res,
                                                                                    }
                                                                                }
                                                                                res => res,
                                                                            }
                                                                        }
                                                                        res => res,
                                                                    }
                                                                }
                                                                res => res,
                                                            }
                                                        }
                                                        res => res,
                                                    }
                                                }
                                                res => res,
                                            }
                                        }
                                        res => res,
                                    }
                                }
                                res => res,
                            }
                        }
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    impl<
        Input,
        Output,
        Error: ParseError<Input>,
        A: Parser<Input, Output, Error>,
    > Alt<Input, Output, Error> for (A,) {
        fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
            self.0.parse(input)
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
    > Permutation<Input, (A, B), Error> for (FnA, FnB) {
        fn permutation(&mut self, mut input: Input) -> IResult<Input, (A, B), Error> {
            let mut res = (Option::<A>::None, Option::<B>::None);
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (Some(a), Some(b)) => return Ok((input, (a, b))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
    > Permutation<Input, (A, B, C), Error> for (FnA, FnB, FnC) {
        fn permutation(&mut self, mut input: Input) -> IResult<Input, (A, B, C), Error> {
            let mut res = (Option::<A>::None, Option::<B>::None, Option::<C>::None);
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (Some(a), Some(b), Some(c)) => return Ok((input, (a, b, c))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
    > Permutation<Input, (A, B, C, D), Error> for (FnA, FnB, FnC, FnD) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (Some(a), Some(b), Some(c), Some(d)) => {
                        return Ok((input, (a, b, c, d)));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
    > Permutation<Input, (A, B, C, D, E), Error> for (FnA, FnB, FnC, FnD, FnE) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (Some(a), Some(b), Some(c), Some(d), Some(e)) => {
                        return Ok((input, (a, b, c, d, e)));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
    > Permutation<Input, (A, B, C, D, E, F), Error> for (FnA, FnB, FnC, FnD, FnE, FnF) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (Some(a), Some(b), Some(c), Some(d), Some(e), Some(f)) => {
                        return Ok((input, (a, b, c, d, e, f)));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (Some(a), Some(b), Some(c), Some(d), Some(e), Some(f), Some(g)) => {
                        return Ok((input, (a, b, c, d, e, f, g)));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                    ) => return Ok((input, (a, b, c, d, e, f, g, h))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                    ) => return Ok((input, (a, b, c, d, e, f, g, h, i))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                    ) => return Ok((input, (a, b, c, d, e, f, g, h, i, j))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J, K), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                    ) => return Ok((input, (a, b, c, d, e, f, g, h, i, j, k))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J, K, L), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK, FnL) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                    ) => return Ok((input, (a, b, c, d, e, f, g, h, i, j, k, l))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK, FnL, FnM) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                    ) => return Ok((input, (a, b, c, d, e, f, g, h, i, j, k, l, m))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK, FnL, FnM, FnN) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
                Option::<N>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.13.is_none() {
                    match self.13.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.13 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                        Some(n),
                    ) => return Ok((input, (a, b, c, d, e, f, g, h, i, j, k, l, m, n))),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK, FnL, FnM, FnN, FnO) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
                Option::<N>::None,
                Option::<O>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.13.is_none() {
                    match self.13.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.13 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.14.is_none() {
                    match self.14.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.14 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                        Some(n),
                        Some(o),
                    ) => {
                        return Ok((input, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Error>
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
    ) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
                Option::<N>::None,
                Option::<O>::None,
                Option::<P>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.13.is_none() {
                    match self.13.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.13 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.14.is_none() {
                    match self.14.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.14 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.15.is_none() {
                    match self.15.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.15 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                        Some(n),
                        Some(o),
                        Some(p),
                    ) => {
                        return Ok((
                            input,
                            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p),
                        ));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Error>
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
    ) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Error> {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
                Option::<N>::None,
                Option::<O>::None,
                Option::<P>::None,
                Option::<Q>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.13.is_none() {
                    match self.13.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.13 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.14.is_none() {
                    match self.14.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.14 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.15.is_none() {
                    match self.15.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.15 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.16.is_none() {
                    match self.16.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.16 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                        Some(n),
                        Some(o),
                        Some(p),
                        Some(q),
                    ) => {
                        return Ok((
                            input,
                            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q),
                        ));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
        FnR: Parser<Input, R, Error>,
    > Permutation<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Error>
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
        FnR,
    ) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<
            Input,
            (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
            Error,
        > {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
                Option::<N>::None,
                Option::<O>::None,
                Option::<P>::None,
                Option::<Q>::None,
                Option::<R>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.13.is_none() {
                    match self.13.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.13 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.14.is_none() {
                    match self.14.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.14 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.15.is_none() {
                    match self.15.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.15 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.16.is_none() {
                    match self.16.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.16 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.17.is_none() {
                    match self.17.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.17 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                        Some(n),
                        Some(o),
                        Some(p),
                        Some(q),
                        Some(r),
                    ) => {
                        return Ok((
                            input,
                            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r),
                        ));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
        FnR: Parser<Input, R, Error>,
        FnS: Parser<Input, S, Error>,
    > Permutation<
        Input,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
        Error,
    >
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
        FnR,
        FnS,
    ) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<
            Input,
            (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
            Error,
        > {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
                Option::<N>::None,
                Option::<O>::None,
                Option::<P>::None,
                Option::<Q>::None,
                Option::<R>::None,
                Option::<S>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.13.is_none() {
                    match self.13.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.13 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.14.is_none() {
                    match self.14.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.14 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.15.is_none() {
                    match self.15.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.15 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.16.is_none() {
                    match self.16.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.16 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.17.is_none() {
                    match self.17.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.17 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.18.is_none() {
                    match self.18.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.18 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                        Some(n),
                        Some(o),
                        Some(p),
                        Some(q),
                        Some(r),
                        Some(s),
                    ) => {
                        return Ok((
                            input,
                            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s),
                        ));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        T,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
        FnR: Parser<Input, R, Error>,
        FnS: Parser<Input, S, Error>,
        FnT: Parser<Input, T, Error>,
    > Permutation<
        Input,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
        Error,
    >
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
        FnR,
        FnS,
        FnT,
    ) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<
            Input,
            (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
            Error,
        > {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
                Option::<N>::None,
                Option::<O>::None,
                Option::<P>::None,
                Option::<Q>::None,
                Option::<R>::None,
                Option::<S>::None,
                Option::<T>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.13.is_none() {
                    match self.13.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.13 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.14.is_none() {
                    match self.14.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.14 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.15.is_none() {
                    match self.15.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.15 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.16.is_none() {
                    match self.16.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.16 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.17.is_none() {
                    match self.17.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.17 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.18.is_none() {
                    match self.18.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.18 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.19.is_none() {
                    match self.19.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.19 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                        Some(n),
                        Some(o),
                        Some(p),
                        Some(q),
                        Some(r),
                        Some(s),
                        Some(t),
                    ) => {
                        return Ok((
                            input,
                            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t),
                        ));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        T,
        U,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
        FnR: Parser<Input, R, Error>,
        FnS: Parser<Input, S, Error>,
        FnT: Parser<Input, T, Error>,
        FnU: Parser<Input, U, Error>,
    > Permutation<
        Input,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
        Error,
    >
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
        FnR,
        FnS,
        FnT,
        FnU,
    ) {
        fn permutation(
            &mut self,
            mut input: Input,
        ) -> IResult<
            Input,
            (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
            Error,
        > {
            let mut res = (
                Option::<A>::None,
                Option::<B>::None,
                Option::<C>::None,
                Option::<D>::None,
                Option::<E>::None,
                Option::<F>::None,
                Option::<G>::None,
                Option::<H>::None,
                Option::<I>::None,
                Option::<J>::None,
                Option::<K>::None,
                Option::<L>::None,
                Option::<M>::None,
                Option::<N>::None,
                Option::<O>::None,
                Option::<P>::None,
                Option::<Q>::None,
                Option::<R>::None,
                Option::<S>::None,
                Option::<T>::None,
                Option::<U>::None,
            );
            loop {
                let mut err: Option<Error> = None;
                if res.0.is_none() {
                    match self.0.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.0 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.1.is_none() {
                    match self.1.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.1 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.2.is_none() {
                    match self.2.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.2 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.3.is_none() {
                    match self.3.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.3 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.4.is_none() {
                    match self.4.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.4 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.5.is_none() {
                    match self.5.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.5 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.6.is_none() {
                    match self.6.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.6 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.7.is_none() {
                    match self.7.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.7 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.8.is_none() {
                    match self.8.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.8 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.9.is_none() {
                    match self.9.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.9 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.10.is_none() {
                    match self.10.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.10 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.11.is_none() {
                    match self.11.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.11 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.12.is_none() {
                    match self.12.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.12 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.13.is_none() {
                    match self.13.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.13 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.14.is_none() {
                    match self.14.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.14 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.15.is_none() {
                    match self.15.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.15 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.16.is_none() {
                    match self.16.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.16 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.17.is_none() {
                    match self.17.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.17 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.18.is_none() {
                    match self.18.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.18 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.19.is_none() {
                    match self.19.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.19 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if res.20.is_none() {
                    match self.20.parse(input.clone()) {
                        Ok((i, o)) => {
                            input = i;
                            res.20 = Some(o);
                            continue;
                        }
                        Err(Err::Error(e)) => {
                            err = Some(
                                match err {
                                    Some(err) => err.or(e),
                                    None => e,
                                },
                            );
                        }
                        Err(e) => return Err(e),
                    };
                }
                if let Some(err) = err {
                    return Err(
                        Err::Error(Error::append(input, ErrorKind::Permutation, err)),
                    );
                }
                match res {
                    (
                        Some(a),
                        Some(b),
                        Some(c),
                        Some(d),
                        Some(e),
                        Some(f),
                        Some(g),
                        Some(h),
                        Some(i),
                        Some(j),
                        Some(k),
                        Some(l),
                        Some(m),
                        Some(n),
                        Some(o),
                        Some(p),
                        Some(q),
                        Some(r),
                        Some(s),
                        Some(t),
                        Some(u),
                    ) => {
                        return Ok((
                            input,
                            (
                                a,
                                b,
                                c,
                                d,
                                e,
                                f,
                                g,
                                h,
                                i,
                                j,
                                k,
                                l,
                                m,
                                n,
                                o,
                                p,
                                q,
                                r,
                                s,
                                t,
                                u,
                            ),
                        ));
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
}
pub mod combinator {
    //! General purpose combinators
    #![allow(unused_imports)]
    #[cfg(feature = "alloc")]
    use crate::lib::std::boxed::Box;
    use crate::error::{ErrorKind, FromExternalError, ParseError};
    use crate::internal::*;
    use crate::lib::std::borrow::Borrow;
    use crate::lib::std::convert::Into;
    #[cfg(feature = "std")]
    use crate::lib::std::fmt::Debug;
    use crate::lib::std::mem::transmute;
    use crate::lib::std::ops::{Range, RangeFrom, RangeTo};
    use crate::traits::{AsChar, InputIter, InputLength, InputTakeAtPosition, ParseTo};
    use crate::traits::{Compare, CompareResult, Offset, Slice};
    /// Return the remaining input.
    ///
    /// ```rust
    /// # use nom::error::ErrorKind;
    /// use nom::combinator::rest;
    /// assert_eq!(rest::<_,(_, ErrorKind)>("abc"), Ok(("", "abc")));
    /// assert_eq!(rest::<_,(_, ErrorKind)>(""), Ok(("", "")));
    /// ```
    #[inline]
    pub fn rest<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
    where
        T: Slice<RangeFrom<usize>>,
        T: InputLength,
    {
        Ok((input.slice(input.input_len()..), input))
    }
    /// Return the length of the remaining input.
    ///
    /// ```rust
    /// # use nom::error::ErrorKind;
    /// use nom::combinator::rest_len;
    /// assert_eq!(rest_len::<_,(_, ErrorKind)>("abc"), Ok(("abc", 3)));
    /// assert_eq!(rest_len::<_,(_, ErrorKind)>(""), Ok(("", 0)));
    /// ```
    #[inline]
    pub fn rest_len<T, E: ParseError<T>>(input: T) -> IResult<T, usize, E>
    where
        T: InputLength,
    {
        let len = input.input_len();
        Ok((input, len))
    }
    /// Maps a function on the result of a parser.
    ///
    /// ```rust
    /// use nom::{Err,error::ErrorKind, IResult,Parser};
    /// use nom::character::complete::digit1;
    /// use nom::combinator::map;
    /// # fn main() {
    ///
    /// let mut parser = map(digit1, |s: &str| s.len());
    ///
    /// // the parser will count how many characters were returned by digit1
    /// assert_eq!(parser.parse("123456"), Ok(("", 6)));
    ///
    /// // this will fail if digit1 fails
    /// assert_eq!(parser.parse("abc"), Err(Err::Error(("abc", ErrorKind::Digit))));
    /// # }
    /// ```
    pub fn map<I, O1, O2, E, F, G>(
        mut parser: F,
        mut f: G,
    ) -> impl FnMut(I) -> IResult<I, O2, E>
    where
        F: Parser<I, O1, E>,
        G: FnMut(O1) -> O2,
    {
        move |input: I| {
            let (input, o1) = parser.parse(input)?;
            Ok((input, f(o1)))
        }
    }
    /// Applies a function returning a `Result` over the result of a parser.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::character::complete::digit1;
    /// use nom::combinator::map_res;
    /// # fn main() {
    ///
    /// let mut parse = map_res(digit1, |s: &str| s.parse::<u8>());
    ///
    /// // the parser will convert the result of digit1 to a number
    /// assert_eq!(parse("123"), Ok(("", 123)));
    ///
    /// // this will fail if digit1 fails
    /// assert_eq!(parse("abc"), Err(Err::Error(("abc", ErrorKind::Digit))));
    ///
    /// // this will fail if the mapped function fails (a `u8` is too small to hold `123456`)
    /// assert_eq!(parse("123456"), Err(Err::Error(("123456", ErrorKind::MapRes))));
    /// # }
    /// ```
    pub fn map_res<I: Clone, O1, O2, E: FromExternalError<I, E2>, E2, F, G>(
        mut parser: F,
        mut f: G,
    ) -> impl FnMut(I) -> IResult<I, O2, E>
    where
        F: Parser<I, O1, E>,
        G: FnMut(O1) -> Result<O2, E2>,
    {
        move |input: I| {
            let i = input.clone();
            let (input, o1) = parser.parse(input)?;
            match f(o1) {
                Ok(o2) => Ok((input, o2)),
                Err(e) => {
                    Err(Err::Error(E::from_external_error(i, ErrorKind::MapRes, e)))
                }
            }
        }
    }
    /// Applies a function returning an `Option` over the result of a parser.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::character::complete::digit1;
    /// use nom::combinator::map_opt;
    /// # fn main() {
    ///
    /// let mut parse = map_opt(digit1, |s: &str| s.parse::<u8>().ok());
    ///
    /// // the parser will convert the result of digit1 to a number
    /// assert_eq!(parse("123"), Ok(("", 123)));
    ///
    /// // this will fail if digit1 fails
    /// assert_eq!(parse("abc"), Err(Err::Error(("abc", ErrorKind::Digit))));
    ///
    /// // this will fail if the mapped function fails (a `u8` is too small to hold `123456`)
    /// assert_eq!(parse("123456"), Err(Err::Error(("123456", ErrorKind::MapOpt))));
    /// # }
    /// ```
    pub fn map_opt<I: Clone, O1, O2, E: ParseError<I>, F, G>(
        mut parser: F,
        mut f: G,
    ) -> impl FnMut(I) -> IResult<I, O2, E>
    where
        F: Parser<I, O1, E>,
        G: FnMut(O1) -> Option<O2>,
    {
        move |input: I| {
            let i = input.clone();
            let (input, o1) = parser.parse(input)?;
            match f(o1) {
                Some(o2) => Ok((input, o2)),
                None => Err(Err::Error(E::from_error_kind(i, ErrorKind::MapOpt))),
            }
        }
    }
    /// Applies a parser over the result of another one.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::character::complete::digit1;
    /// use nom::bytes::complete::take;
    /// use nom::combinator::map_parser;
    /// # fn main() {
    ///
    /// let mut parse = map_parser(take(5u8), digit1);
    ///
    /// assert_eq!(parse("12345"), Ok(("", "12345")));
    /// assert_eq!(parse("123ab"), Ok(("", "123")));
    /// assert_eq!(parse("123"), Err(Err::Error(("123", ErrorKind::Eof))));
    /// # }
    /// ```
    pub fn map_parser<I, O1, O2, E: ParseError<I>, F, G>(
        mut parser: F,
        mut applied_parser: G,
    ) -> impl FnMut(I) -> IResult<I, O2, E>
    where
        F: Parser<I, O1, E>,
        G: Parser<O1, O2, E>,
    {
        move |input: I| {
            let (input, o1) = parser.parse(input)?;
            let (_, o2) = applied_parser.parse(o1)?;
            Ok((input, o2))
        }
    }
    /// Creates a new parser from the output of the first parser, then apply that parser over the rest of the input.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::bytes::complete::take;
    /// use nom::number::complete::u8;
    /// use nom::combinator::flat_map;
    /// # fn main() {
    ///
    /// let mut parse = flat_map(u8, take);
    ///
    /// assert_eq!(parse(&[2, 0, 1, 2][..]), Ok((&[2][..], &[0, 1][..])));
    /// assert_eq!(parse(&[4, 0, 1, 2][..]), Err(Err::Error((&[0, 1, 2][..], ErrorKind::Eof))));
    /// # }
    /// ```
    pub fn flat_map<I, O1, O2, E: ParseError<I>, F, G, H>(
        mut parser: F,
        mut applied_parser: G,
    ) -> impl FnMut(I) -> IResult<I, O2, E>
    where
        F: Parser<I, O1, E>,
        G: FnMut(O1) -> H,
        H: Parser<I, O2, E>,
    {
        move |input: I| {
            let (input, o1) = parser.parse(input)?;
            applied_parser(o1).parse(input)
        }
    }
    /// Optional parser, will return `None` on [`Err::Error`].
    ///
    /// To chain an error up, see [`cut`].
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::combinator::opt;
    /// use nom::character::complete::alpha1;
    /// # fn main() {
    ///
    /// fn parser(i: &str) -> IResult<&str, Option<&str>> {
    ///   opt(alpha1)(i)
    /// }
    ///
    /// assert_eq!(parser("abcd;"), Ok((";", Some("abcd"))));
    /// assert_eq!(parser("123;"), Ok(("123;", None)));
    /// # }
    /// ```
    pub fn opt<I: Clone, O, E: ParseError<I>, F>(
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, Option<O>, E>
    where
        F: Parser<I, O, E>,
    {
        move |input: I| {
            let i = input.clone();
            match f.parse(input) {
                Ok((i, o)) => Ok((i, Some(o))),
                Err(Err::Error(_)) => Ok((i, None)),
                Err(e) => Err(e),
            }
        }
    }
    /// Calls the parser if the condition is met.
    ///
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, IResult};
    /// use nom::combinator::cond;
    /// use nom::character::complete::alpha1;
    /// # fn main() {
    ///
    /// fn parser(b: bool, i: &str) -> IResult<&str, Option<&str>> {
    ///   cond(b, alpha1)(i)
    /// }
    ///
    /// assert_eq!(parser(true, "abcd;"), Ok((";", Some("abcd"))));
    /// assert_eq!(parser(false, "abcd;"), Ok(("abcd;", None)));
    /// assert_eq!(parser(true, "123;"), Err(Err::Error(Error::new("123;", ErrorKind::Alpha))));
    /// assert_eq!(parser(false, "123;"), Ok(("123;", None)));
    /// # }
    /// ```
    pub fn cond<I, O, E: ParseError<I>, F>(
        b: bool,
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, Option<O>, E>
    where
        F: Parser<I, O, E>,
    {
        move |input: I| {
            if b {
                match f.parse(input) {
                    Ok((i, o)) => Ok((i, Some(o))),
                    Err(e) => Err(e),
                }
            } else {
                Ok((input, None))
            }
        }
    }
    /// Tries to apply its parser without consuming the input.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::combinator::peek;
    /// use nom::character::complete::alpha1;
    /// # fn main() {
    ///
    /// let mut parser = peek(alpha1);
    ///
    /// assert_eq!(parser("abcd;"), Ok(("abcd;", "abcd")));
    /// assert_eq!(parser("123;"), Err(Err::Error(("123;", ErrorKind::Alpha))));
    /// # }
    /// ```
    pub fn peek<I: Clone, O, E: ParseError<I>, F>(
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, O, E>
    where
        F: Parser<I, O, E>,
    {
        move |input: I| {
            let i = input.clone();
            match f.parse(input) {
                Ok((_, o)) => Ok((i, o)),
                Err(e) => Err(e),
            }
        }
    }
    /// returns its input if it is at the end of input data
    ///
    /// When we're at the end of the data, this combinator
    /// will succeed
    ///
    /// ```
    /// # use std::str;
    /// # use nom::{Err, error::ErrorKind, IResult};
    /// # use nom::combinator::eof;
    ///
    /// # fn main() {
    /// let parser = eof;
    /// assert_eq!(parser("abc"), Err(Err::Error(("abc", ErrorKind::Eof))));
    /// assert_eq!(parser(""), Ok(("", "")));
    /// # }
    /// ```
    pub fn eof<I: InputLength + Clone, E: ParseError<I>>(input: I) -> IResult<I, I, E> {
        if input.input_len() == 0 {
            let clone = input.clone();
            Ok((input, clone))
        } else {
            Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof)))
        }
    }
    /// Transforms Incomplete into `Error`.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::bytes::streaming::take;
    /// use nom::combinator::complete;
    /// # fn main() {
    ///
    /// let mut parser = complete(take(5u8));
    ///
    /// assert_eq!(parser("abcdefg"), Ok(("fg", "abcde")));
    /// assert_eq!(parser("abcd"), Err(Err::Error(("abcd", ErrorKind::Complete))));
    /// # }
    /// ```
    pub fn complete<I: Clone, O, E: ParseError<I>, F>(
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, O, E>
    where
        F: Parser<I, O, E>,
    {
        move |input: I| {
            let i = input.clone();
            match f.parse(input) {
                Err(Err::Incomplete(_)) => {
                    Err(Err::Error(E::from_error_kind(i, ErrorKind::Complete)))
                }
                rest => rest,
            }
        }
    }
    /// Succeeds if all the input has been consumed by its child parser.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::combinator::all_consuming;
    /// use nom::character::complete::alpha1;
    /// # fn main() {
    ///
    /// let mut parser = all_consuming(alpha1);
    ///
    /// assert_eq!(parser("abcd"), Ok(("", "abcd")));
    /// assert_eq!(parser("abcd;"),Err(Err::Error((";", ErrorKind::Eof))));
    /// assert_eq!(parser("123abcd;"),Err(Err::Error(("123abcd;", ErrorKind::Alpha))));
    /// # }
    /// ```
    pub fn all_consuming<I, O, E: ParseError<I>, F>(
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, O, E>
    where
        I: InputLength,
        F: Parser<I, O, E>,
    {
        move |input: I| {
            let (input, res) = f.parse(input)?;
            if input.input_len() == 0 {
                Ok((input, res))
            } else {
                Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof)))
            }
        }
    }
    /// Returns the result of the child parser if it satisfies a verification function.
    ///
    /// The verification function takes as argument a reference to the output of the
    /// parser.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::combinator::verify;
    /// use nom::character::complete::alpha1;
    /// # fn main() {
    ///
    /// let mut parser = verify(alpha1, |s: &str| s.len() == 4);
    ///
    /// assert_eq!(parser("abcd"), Ok(("", "abcd")));
    /// assert_eq!(parser("abcde"), Err(Err::Error(("abcde", ErrorKind::Verify))));
    /// assert_eq!(parser("123abcd;"),Err(Err::Error(("123abcd;", ErrorKind::Alpha))));
    /// # }
    /// ```
    pub fn verify<I: Clone, O1, O2, E: ParseError<I>, F, G>(
        mut first: F,
        second: G,
    ) -> impl FnMut(I) -> IResult<I, O1, E>
    where
        F: Parser<I, O1, E>,
        G: Fn(&O2) -> bool,
        O1: Borrow<O2>,
        O2: ?Sized,
    {
        move |input: I| {
            let i = input.clone();
            let (input, o) = first.parse(input)?;
            if second(o.borrow()) {
                Ok((input, o))
            } else {
                Err(Err::Error(E::from_error_kind(i, ErrorKind::Verify)))
            }
        }
    }
    /// Returns the provided value if the child parser succeeds.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::combinator::value;
    /// use nom::character::complete::alpha1;
    /// # fn main() {
    ///
    /// let mut parser = value(1234, alpha1);
    ///
    /// assert_eq!(parser("abcd"), Ok(("", 1234)));
    /// assert_eq!(parser("123abcd;"), Err(Err::Error(("123abcd;", ErrorKind::Alpha))));
    /// # }
    /// ```
    pub fn value<I, O1: Clone, O2, E: ParseError<I>, F>(
        val: O1,
        mut parser: F,
    ) -> impl FnMut(I) -> IResult<I, O1, E>
    where
        F: Parser<I, O2, E>,
    {
        move |input: I| parser.parse(input).map(|(i, _)| (i, val.clone()))
    }
    /// Succeeds if the child parser returns an error.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::combinator::not;
    /// use nom::character::complete::alpha1;
    /// # fn main() {
    ///
    /// let mut parser = not(alpha1);
    ///
    /// assert_eq!(parser("123"), Ok(("123", ())));
    /// assert_eq!(parser("abcd"), Err(Err::Error(("abcd", ErrorKind::Not))));
    /// # }
    /// ```
    pub fn not<I: Clone, O, E: ParseError<I>, F>(
        mut parser: F,
    ) -> impl FnMut(I) -> IResult<I, (), E>
    where
        F: Parser<I, O, E>,
    {
        move |input: I| {
            let i = input.clone();
            match parser.parse(input) {
                Ok(_) => Err(Err::Error(E::from_error_kind(i, ErrorKind::Not))),
                Err(Err::Error(_)) => Ok((i, ())),
                Err(e) => Err(e),
            }
        }
    }
    /// If the child parser was successful, return the consumed input as produced value.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::combinator::recognize;
    /// use nom::character::complete::{char, alpha1};
    /// use nom::sequence::separated_pair;
    /// # fn main() {
    ///
    /// let mut parser = recognize(separated_pair(alpha1, char(','), alpha1));
    ///
    /// assert_eq!(parser("abcd,efgh"), Ok(("", "abcd,efgh")));
    /// assert_eq!(parser("abcd;"),Err(Err::Error((";", ErrorKind::Char))));
    /// # }
    /// ```
    pub fn recognize<I: Clone + Offset + Slice<RangeTo<usize>>, O, E: ParseError<I>, F>(
        mut parser: F,
    ) -> impl FnMut(I) -> IResult<I, I, E>
    where
        F: Parser<I, O, E>,
    {
        move |input: I| {
            let i = input.clone();
            match parser.parse(i) {
                Ok((i, _)) => {
                    let index = input.offset(&i);
                    Ok((i, input.slice(..index)))
                }
                Err(e) => Err(e),
            }
        }
    }
    /// if the child parser was successful, return the consumed input with the output
    /// as a tuple. Functions similarly to [recognize](fn.recognize.html) except it
    /// returns the parser output as well.
    ///
    /// This can be useful especially in cases where the output is not the same type
    /// as the input, or the input is a user defined type.
    ///
    /// Returned tuple is of the format `(consumed input, produced output)`.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::combinator::{consumed, value, recognize, map};
    /// use nom::character::complete::{char, alpha1};
    /// use nom::bytes::complete::tag;
    /// use nom::sequence::separated_pair;
    ///
    /// fn inner_parser(input: &str) -> IResult<&str, bool> {
    ///     value(true, tag("1234"))(input)
    /// }
    ///
    /// # fn main() {
    ///
    /// let mut consumed_parser = consumed(value(true, separated_pair(alpha1, char(','), alpha1)));
    ///
    /// assert_eq!(consumed_parser("abcd,efgh1"), Ok(("1", ("abcd,efgh", true))));
    /// assert_eq!(consumed_parser("abcd;"),Err(Err::Error((";", ErrorKind::Char))));
    ///
    ///
    /// // the first output (representing the consumed input)
    /// // should be the same as that of the `recognize` parser.
    /// let mut recognize_parser = recognize(inner_parser);
    /// let mut consumed_parser = map(consumed(inner_parser), |(consumed, output)| consumed);
    ///
    /// assert_eq!(recognize_parser("1234"), consumed_parser("1234"));
    /// assert_eq!(recognize_parser("abcd"), consumed_parser("abcd"));
    /// # }
    /// ```
    pub fn consumed<I, O, F, E>(mut parser: F) -> impl FnMut(I) -> IResult<I, (I, O), E>
    where
        I: Clone + Offset + Slice<RangeTo<usize>>,
        E: ParseError<I>,
        F: Parser<I, O, E>,
    {
        move |input: I| {
            let i = input.clone();
            match parser.parse(i) {
                Ok((remaining, result)) => {
                    let index = input.offset(&remaining);
                    let consumed = input.slice(..index);
                    Ok((remaining, (consumed, result)))
                }
                Err(e) => Err(e),
            }
        }
    }
    /// Transforms an [`Err::Error`] (recoverable) to [`Err::Failure`] (unrecoverable)
    ///
    /// This commits the parse result, preventing alternative branch paths like with
    /// [`nom::branch::alt`][crate::branch::alt].
    ///
    /// # Example
    ///
    /// Without `cut`:
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// # use nom::character::complete::{one_of, digit1};
    /// # use nom::combinator::rest;
    /// # use nom::branch::alt;
    /// # use nom::sequence::preceded;
    /// # fn main() {
    ///
    /// fn parser(input: &str) -> IResult<&str, &str> {
    ///   alt((
    ///     preceded(one_of("+-"), digit1),
    ///     rest
    ///   ))(input)
    /// }
    ///
    /// assert_eq!(parser("+10 ab"), Ok((" ab", "10")));
    /// assert_eq!(parser("ab"), Ok(("", "ab")));
    /// assert_eq!(parser("+"), Ok(("", "+")));
    /// # }
    /// ```
    ///
    /// With `cut`:
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult, error::Error};
    /// # use nom::character::complete::{one_of, digit1};
    /// # use nom::combinator::rest;
    /// # use nom::branch::alt;
    /// # use nom::sequence::preceded;
    /// use nom::combinator::cut;
    /// # fn main() {
    ///
    /// fn parser(input: &str) -> IResult<&str, &str> {
    ///   alt((
    ///     preceded(one_of("+-"), cut(digit1)),
    ///     rest
    ///   ))(input)
    /// }
    ///
    /// assert_eq!(parser("+10 ab"), Ok((" ab", "10")));
    /// assert_eq!(parser("ab"), Ok(("", "ab")));
    /// assert_eq!(parser("+"), Err(Err::Failure(Error { input: "", code: ErrorKind::Digit })));
    /// # }
    /// ```
    pub fn cut<I, O, E: ParseError<I>, F>(
        mut parser: F,
    ) -> impl FnMut(I) -> IResult<I, O, E>
    where
        F: Parser<I, O, E>,
    {
        move |input: I| match parser.parse(input) {
            Err(Err::Error(e)) => Err(Err::Failure(e)),
            rest => rest,
        }
    }
    /// automatically converts the child parser's result to another type
    ///
    /// it will be able to convert the output value and the error value
    /// as long as the `Into` implementations are available
    ///
    /// ```rust
    /// # use nom::IResult;
    /// use nom::combinator::into;
    /// use nom::character::complete::alpha1;
    /// # fn main() {
    ///
    ///  fn parser1(i: &str) -> IResult<&str, &str> {
    ///    alpha1(i)
    ///  }
    ///
    ///  let mut parser2 = into(parser1);
    ///
    /// // the parser converts the &str output of the child parser into a Vec<u8>
    /// let bytes: IResult<&str, Vec<u8>> = parser2("abcd");
    /// assert_eq!(bytes, Ok(("", vec![97, 98, 99, 100])));
    /// # }
    /// ```
    pub fn into<I, O1, O2, E1, E2, F>(
        mut parser: F,
    ) -> impl FnMut(I) -> IResult<I, O2, E2>
    where
        O1: Into<O2>,
        E1: Into<E2>,
        E1: ParseError<I>,
        E2: ParseError<I>,
        F: Parser<I, O1, E1>,
    {
        move |input: I| match parser.parse(input) {
            Ok((i, o)) => Ok((i, o.into())),
            Err(Err::Error(e)) => Err(Err::Error(e.into())),
            Err(Err::Failure(e)) => Err(Err::Failure(e.into())),
            Err(Err::Incomplete(e)) => Err(Err::Incomplete(e)),
        }
    }
    /// Creates an iterator from input data and a parser.
    ///
    /// Call the iterator's [ParserIterator::finish] method to get the remaining input if successful,
    /// or the error value if we encountered an error.
    ///
    /// On [`Err::Error`], iteration will stop. To instead chain an error up, see [`cut`].
    ///
    /// ```rust
    /// use nom::{combinator::iterator, IResult, bytes::complete::tag, character::complete::alpha1, sequence::terminated};
    /// use std::collections::HashMap;
    ///
    /// let data = "abc|defg|hijkl|mnopqr|123";
    /// let mut it = iterator(data, terminated(alpha1, tag("|")));
    ///
    /// let parsed = it.map(|v| (v, v.len())).collect::<HashMap<_,_>>();
    /// let res: IResult<_,_> = it.finish();
    ///
    /// assert_eq!(parsed, [("abc", 3usize), ("defg", 4), ("hijkl", 5), ("mnopqr", 6)].iter().cloned().collect());
    /// assert_eq!(res, Ok(("123", ())));
    /// ```
    pub fn iterator<Input, Output, Error, F>(
        input: Input,
        f: F,
    ) -> ParserIterator<Input, Error, F>
    where
        F: Parser<Input, Output, Error>,
        Error: ParseError<Input>,
    {
        ParserIterator {
            iterator: f,
            input,
            state: Some(State::Running),
        }
    }
    /// Main structure associated to the [iterator] function.
    pub struct ParserIterator<I, E, F> {
        iterator: F,
        input: I,
        state: Option<State<E>>,
    }
    impl<I: Clone, E, F> ParserIterator<I, E, F> {
        /// Returns the remaining input if parsing was successful, or the error if we encountered an error.
        pub fn finish(mut self) -> IResult<I, (), E> {
            match self.state.take().unwrap() {
                State::Running | State::Done => Ok((self.input, ())),
                State::Failure(e) => Err(Err::Failure(e)),
                State::Incomplete(i) => Err(Err::Incomplete(i)),
            }
        }
    }
    impl<'a, Input, Output, Error, F> core::iter::Iterator
    for &'a mut ParserIterator<Input, Error, F>
    where
        F: FnMut(Input) -> IResult<Input, Output, Error>,
        Input: Clone,
    {
        type Item = Output;
        fn next(&mut self) -> Option<Self::Item> {
            if let State::Running = self.state.take().unwrap() {
                let input = self.input.clone();
                match (self.iterator)(input) {
                    Ok((i, o)) => {
                        self.input = i;
                        self.state = Some(State::Running);
                        Some(o)
                    }
                    Err(Err::Error(_)) => {
                        self.state = Some(State::Done);
                        None
                    }
                    Err(Err::Failure(e)) => {
                        self.state = Some(State::Failure(e));
                        None
                    }
                    Err(Err::Incomplete(i)) => {
                        self.state = Some(State::Incomplete(i));
                        None
                    }
                }
            } else {
                None
            }
        }
    }
    enum State<E> {
        Running,
        Done,
        Failure(E),
        Incomplete(Needed),
    }
    /// a parser which always succeeds with given value without consuming any input.
    ///
    /// It can be used for example as the last alternative in `alt` to
    /// specify the default case.
    ///
    /// ```rust
    /// # use nom::{Err,error::ErrorKind, IResult};
    /// use nom::branch::alt;
    /// use nom::combinator::{success, value};
    /// use nom::character::complete::char;
    /// # fn main() {
    ///
    /// let mut parser = success::<_,_,(_,ErrorKind)>(10);
    /// assert_eq!(parser("xyz"), Ok(("xyz", 10)));
    ///
    /// let mut sign = alt((value(-1, char('-')), value(1, char('+')), success::<_,_,(_,ErrorKind)>(1)));
    /// assert_eq!(sign("+10"), Ok(("10", 1)));
    /// assert_eq!(sign("-10"), Ok(("10", -1)));
    /// assert_eq!(sign("10"), Ok(("10", 1)));
    /// # }
    /// ```
    pub fn success<I, O: Clone, E: ParseError<I>>(
        val: O,
    ) -> impl Fn(I) -> IResult<I, O, E> {
        move |input: I| Ok((input, val.clone()))
    }
    /// A parser which always fails.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, IResult};
    /// use nom::combinator::fail;
    ///
    /// let s = "string";
    /// assert_eq!(fail::<_, &str, _>(s), Err(Err::Error((s, ErrorKind::Fail))));
    /// ```
    pub fn fail<I, O, E: ParseError<I>>(i: I) -> IResult<I, O, E> {
        Err(Err::Error(E::from_error_kind(i, ErrorKind::Fail)))
    }
}
mod internal {
    //! Basic types to build the parsers
    use self::Needed::*;
    use crate::error::{self, ErrorKind};
    use crate::lib::std::fmt;
    use core::num::NonZeroUsize;
    /// Holds the result of parsing functions
    ///
    /// It depends on the input type `I`, the output type `O`, and the error type `E`
    /// (by default `(I, nom::ErrorKind)`)
    ///
    /// The `Ok` side is a pair containing the remainder of the input (the part of the data that
    /// was not parsed) and the produced value. The `Err` side contains an instance of `nom::Err`.
    ///
    /// Outside of the parsing code, you can use the [Finish::finish] method to convert
    /// it to a more common result type
    pub type IResult<I, O, E = error::Error<I>> = Result<(I, O), Err<E>>;
    /// Helper trait to convert a parser's result to a more manageable type
    pub trait Finish<I, O, E> {
        /// converts the parser's result to a type that is more consumable by error
        /// management libraries. It keeps the same `Ok` branch, and merges `Err::Error`
        /// and `Err::Failure` into the `Err` side.
        ///
        /// *warning*: if the result is `Err(Err::Incomplete(_))`, this method will panic.
        /// - "complete" parsers: It will not be an issue, `Incomplete` is never used
        /// - "streaming" parsers: `Incomplete` will be returned if there's not enough data
        /// for the parser to decide, and you should gather more data before parsing again.
        /// Once the parser returns either `Ok(_)`, `Err(Err::Error(_))` or `Err(Err::Failure(_))`,
        /// you can get out of the parsing loop and call `finish()` on the parser's result
        fn finish(self) -> Result<(I, O), E>;
    }
    impl<I, O, E> Finish<I, O, E> for IResult<I, O, E> {
        fn finish(self) -> Result<(I, O), E> {
            match self {
                Ok(res) => Ok(res),
                Err(Err::Error(e)) | Err(Err::Failure(e)) => Err(e),
                Err(Err::Incomplete(_)) => {
                    ::std::rt::begin_panic(
                        "Cannot call `finish()` on `Err(Err::Incomplete(_))`: this result means that the parser does not have enough data to decide, you should gather more data and try to reapply  the parser instead",
                    );
                }
            }
        }
    }
    /// Contains information on needed data if a parser returned `Incomplete`
    pub enum Needed {
        /// Needs more data, but we do not know how much
        Unknown,
        /// Contains the required data size in bytes
        Size(NonZeroUsize),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Needed {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Needed::Unknown => ::core::fmt::Formatter::write_str(f, "Unknown"),
                Needed::Size(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Size",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Needed {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Needed {
        #[inline]
        fn eq(&self, other: &Needed) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (Needed::Size(__self_0), Needed::Size(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for Needed {}
    #[automatically_derived]
    impl ::core::cmp::Eq for Needed {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<NonZeroUsize>;
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Needed {
        #[inline]
        fn clone(&self) -> Needed {
            let _: ::core::clone::AssertParamIsClone<NonZeroUsize>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Needed {}
    impl Needed {
        /// Creates `Needed` instance, returns `Needed::Unknown` if the argument is zero
        pub fn new(s: usize) -> Self {
            match NonZeroUsize::new(s) {
                Some(sz) => Needed::Size(sz),
                None => Needed::Unknown,
            }
        }
        /// Indicates if we know how many bytes we need
        pub fn is_known(&self) -> bool {
            *self != Unknown
        }
        /// Maps a `Needed` to `Needed` by applying a function to a contained `Size` value.
        #[inline]
        pub fn map<F: Fn(NonZeroUsize) -> usize>(self, f: F) -> Needed {
            match self {
                Unknown => Unknown,
                Size(n) => Needed::new(f(n)),
            }
        }
    }
    /// The `Err` enum indicates the parser was not successful
    ///
    /// It has three cases:
    ///
    /// * `Incomplete` indicates that more data is needed to decide. The `Needed` enum
    /// can contain how many additional bytes are necessary. If you are sure your parser
    /// is working on full data, you can wrap your parser with the `complete` combinator
    /// to transform that case in `Error`
    /// * `Error` means some parser did not succeed, but another one might (as an example,
    /// when testing different branches of an `alt` combinator)
    /// * `Failure` indicates an unrecoverable error. As an example, if you recognize a prefix
    /// to decide on the next parser to apply, and that parser fails, you know there's no need
    /// to try other parsers, you were already in the right branch, so the data is invalid
    ///
    pub enum Err<E> {
        /// There was not enough data
        Incomplete(Needed),
        /// The parser had an error (recoverable)
        Error(E),
        /// The parser had an unrecoverable error: we got to the right
        /// branch and we know other branches won't work, so backtrack
        /// as fast as possible
        Failure(E),
    }
    #[automatically_derived]
    impl<E: ::core::fmt::Debug> ::core::fmt::Debug for Err<E> {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Err::Incomplete(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Incomplete",
                        &__self_0,
                    )
                }
                Err::Error(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Error",
                        &__self_0,
                    )
                }
                Err::Failure(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Failure",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl<E: ::core::clone::Clone> ::core::clone::Clone for Err<E> {
        #[inline]
        fn clone(&self) -> Err<E> {
            match self {
                Err::Incomplete(__self_0) => {
                    Err::Incomplete(::core::clone::Clone::clone(__self_0))
                }
                Err::Error(__self_0) => Err::Error(::core::clone::Clone::clone(__self_0)),
                Err::Failure(__self_0) => {
                    Err::Failure(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    #[automatically_derived]
    impl<E> ::core::marker::StructuralPartialEq for Err<E> {}
    #[automatically_derived]
    impl<E: ::core::cmp::PartialEq> ::core::cmp::PartialEq for Err<E> {
        #[inline]
        fn eq(&self, other: &Err<E>) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (Err::Incomplete(__self_0), Err::Incomplete(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    (Err::Error(__self_0), Err::Error(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    (Err::Failure(__self_0), Err::Failure(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
        }
    }
    impl<E> Err<E> {
        /// Tests if the result is Incomplete
        pub fn is_incomplete(&self) -> bool {
            if let Err::Incomplete(_) = self { true } else { false }
        }
        /// Applies the given function to the inner error
        pub fn map<E2, F>(self, f: F) -> Err<E2>
        where
            F: FnOnce(E) -> E2,
        {
            match self {
                Err::Incomplete(n) => Err::Incomplete(n),
                Err::Failure(t) => Err::Failure(f(t)),
                Err::Error(t) => Err::Error(f(t)),
            }
        }
        /// Automatically converts between errors if the underlying type supports it
        pub fn convert<F>(e: Err<F>) -> Self
        where
            E: From<F>,
        {
            e.map(crate::lib::std::convert::Into::into)
        }
    }
    impl<T> Err<(T, ErrorKind)> {
        /// Maps `Err<(T, ErrorKind)>` to `Err<(U, ErrorKind)>` with the given `F: T -> U`
        pub fn map_input<U, F>(self, f: F) -> Err<(U, ErrorKind)>
        where
            F: FnOnce(T) -> U,
        {
            match self {
                Err::Incomplete(n) => Err::Incomplete(n),
                Err::Failure((input, k)) => Err::Failure((f(input), k)),
                Err::Error((input, k)) => Err::Error((f(input), k)),
            }
        }
    }
    impl<T> Err<error::Error<T>> {
        /// Maps `Err<error::Error<T>>` to `Err<error::Error<U>>` with the given `F: T -> U`
        pub fn map_input<U, F>(self, f: F) -> Err<error::Error<U>>
        where
            F: FnOnce(T) -> U,
        {
            match self {
                Err::Incomplete(n) => Err::Incomplete(n),
                Err::Failure(error::Error { input, code }) => {
                    Err::Failure(error::Error {
                        input: f(input),
                        code,
                    })
                }
                Err::Error(error::Error { input, code }) => {
                    Err::Error(error::Error {
                        input: f(input),
                        code,
                    })
                }
            }
        }
    }
    #[cfg(feature = "alloc")]
    use crate::lib::std::{borrow::ToOwned, string::String, vec::Vec};
    #[cfg(feature = "alloc")]
    impl Err<(&[u8], ErrorKind)> {
        /// Obtaining ownership
        pub fn to_owned(self) -> Err<(Vec<u8>, ErrorKind)> {
            self.map_input(ToOwned::to_owned)
        }
    }
    #[cfg(feature = "alloc")]
    impl Err<(&str, ErrorKind)> {
        /// Obtaining ownership
        pub fn to_owned(self) -> Err<(String, ErrorKind)> {
            self.map_input(ToOwned::to_owned)
        }
    }
    #[cfg(feature = "alloc")]
    impl Err<error::Error<&[u8]>> {
        /// Obtaining ownership
        pub fn to_owned(self) -> Err<error::Error<Vec<u8>>> {
            self.map_input(ToOwned::to_owned)
        }
    }
    #[cfg(feature = "alloc")]
    impl Err<error::Error<&str>> {
        /// Obtaining ownership
        pub fn to_owned(self) -> Err<error::Error<String>> {
            self.map_input(ToOwned::to_owned)
        }
    }
    impl<E: Eq> Eq for Err<E> {}
    impl<E> fmt::Display for Err<E>
    where
        E: fmt::Debug,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Err::Incomplete(Needed::Size(u)) => {
                    f.write_fmt(format_args!("Parsing requires {0} bytes/chars", u))
                }
                Err::Incomplete(Needed::Unknown) => {
                    f.write_fmt(format_args!("Parsing requires more data"))
                }
                Err::Failure(c) => f.write_fmt(format_args!("Parsing Failure: {0:?}", c)),
                Err::Error(c) => f.write_fmt(format_args!("Parsing Error: {0:?}", c)),
            }
        }
    }
    #[cfg(feature = "std")]
    use std::error::Error;
    #[cfg(feature = "std")]
    impl<E> Error for Err<E>
    where
        E: fmt::Debug,
    {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            None
        }
    }
    /// All nom parsers implement this trait
    pub trait Parser<I, O, E> {
        /// A parser takes in input type, and returns a `Result` containing
        /// either the remaining input and the output value, or an error
        fn parse(&mut self, input: I) -> IResult<I, O, E>;
        /// Maps a function over the result of a parser
        fn map<G, O2>(self, g: G) -> Map<Self, G, O>
        where
            G: Fn(O) -> O2,
            Self: core::marker::Sized,
        {
            Map {
                f: self,
                g,
                phantom: core::marker::PhantomData,
            }
        }
        /// Creates a second parser from the output of the first one, then apply over the rest of the input
        fn flat_map<G, H, O2>(self, g: G) -> FlatMap<Self, G, O>
        where
            G: FnMut(O) -> H,
            H: Parser<I, O2, E>,
            Self: core::marker::Sized,
        {
            FlatMap {
                f: self,
                g,
                phantom: core::marker::PhantomData,
            }
        }
        /// Applies a second parser over the output of the first one
        fn and_then<G, O2>(self, g: G) -> AndThen<Self, G, O>
        where
            G: Parser<O, O2, E>,
            Self: core::marker::Sized,
        {
            AndThen {
                f: self,
                g,
                phantom: core::marker::PhantomData,
            }
        }
        /// Applies a second parser after the first one, return their results as a tuple
        fn and<G, O2>(self, g: G) -> And<Self, G>
        where
            G: Parser<I, O2, E>,
            Self: core::marker::Sized,
        {
            And { f: self, g }
        }
        /// Applies a second parser over the input if the first one failed
        fn or<G>(self, g: G) -> Or<Self, G>
        where
            G: Parser<I, O, E>,
            Self: core::marker::Sized,
        {
            Or { f: self, g }
        }
        /// automatically converts the parser's output and error values to another type, as long as they
        /// implement the `From` trait
        fn into<O2: From<O>, E2: From<E>>(self) -> Into<Self, O, O2, E, E2>
        where
            Self: core::marker::Sized,
        {
            Into {
                f: self,
                phantom_out1: core::marker::PhantomData,
                phantom_err1: core::marker::PhantomData,
                phantom_out2: core::marker::PhantomData,
                phantom_err2: core::marker::PhantomData,
            }
        }
    }
    impl<'a, I, O, E, F> Parser<I, O, E> for F
    where
        F: FnMut(I) -> IResult<I, O, E> + 'a,
    {
        fn parse(&mut self, i: I) -> IResult<I, O, E> {
            self(i)
        }
    }
    #[cfg(feature = "alloc")]
    use alloc::boxed::Box;
    #[cfg(feature = "alloc")]
    impl<'a, I, O, E> Parser<I, O, E> for Box<dyn Parser<I, O, E> + 'a> {
        fn parse(&mut self, input: I) -> IResult<I, O, E> {
            (**self).parse(input)
        }
    }
    /// Implementation of `Parser::map`
    pub struct Map<F, G, O1> {
        f: F,
        g: G,
        phantom: core::marker::PhantomData<O1>,
    }
    impl<'a, I, O1, O2, E, F: Parser<I, O1, E>, G: Fn(O1) -> O2> Parser<I, O2, E>
    for Map<F, G, O1> {
        fn parse(&mut self, i: I) -> IResult<I, O2, E> {
            match self.f.parse(i) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, (self.g)(o))),
            }
        }
    }
    /// Implementation of `Parser::flat_map`
    pub struct FlatMap<F, G, O1> {
        f: F,
        g: G,
        phantom: core::marker::PhantomData<O1>,
    }
    impl<
        'a,
        I,
        O1,
        O2,
        E,
        F: Parser<I, O1, E>,
        G: Fn(O1) -> H,
        H: Parser<I, O2, E>,
    > Parser<I, O2, E> for FlatMap<F, G, O1> {
        fn parse(&mut self, i: I) -> IResult<I, O2, E> {
            let (i, o1) = self.f.parse(i)?;
            (self.g)(o1).parse(i)
        }
    }
    /// Implementation of `Parser::and_then`
    pub struct AndThen<F, G, O1> {
        f: F,
        g: G,
        phantom: core::marker::PhantomData<O1>,
    }
    impl<'a, I, O1, O2, E, F: Parser<I, O1, E>, G: Parser<O1, O2, E>> Parser<I, O2, E>
    for AndThen<F, G, O1> {
        fn parse(&mut self, i: I) -> IResult<I, O2, E> {
            let (i, o1) = self.f.parse(i)?;
            let (_, o2) = self.g.parse(o1)?;
            Ok((i, o2))
        }
    }
    /// Implementation of `Parser::and`
    pub struct And<F, G> {
        f: F,
        g: G,
    }
    impl<
        'a,
        I,
        O1,
        O2,
        E,
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
    > Parser<I, (O1, O2), E> for And<F, G> {
        fn parse(&mut self, i: I) -> IResult<I, (O1, O2), E> {
            let (i, o1) = self.f.parse(i)?;
            let (i, o2) = self.g.parse(i)?;
            Ok((i, (o1, o2)))
        }
    }
    /// Implementation of `Parser::or`
    pub struct Or<F, G> {
        f: F,
        g: G,
    }
    impl<
        'a,
        I: Clone,
        O,
        E: crate::error::ParseError<I>,
        F: Parser<I, O, E>,
        G: Parser<I, O, E>,
    > Parser<I, O, E> for Or<F, G> {
        fn parse(&mut self, i: I) -> IResult<I, O, E> {
            match self.f.parse(i.clone()) {
                Err(Err::Error(e1)) => {
                    match self.g.parse(i) {
                        Err(Err::Error(e2)) => Err(Err::Error(e1.or(e2))),
                        res => res,
                    }
                }
                res => res,
            }
        }
    }
    /// Implementation of `Parser::into`
    pub struct Into<F, O1, O2: From<O1>, E1, E2: From<E1>> {
        f: F,
        phantom_out1: core::marker::PhantomData<O1>,
        phantom_err1: core::marker::PhantomData<E1>,
        phantom_out2: core::marker::PhantomData<O2>,
        phantom_err2: core::marker::PhantomData<E2>,
    }
    impl<
        'a,
        I: Clone,
        O1,
        O2: From<O1>,
        E1,
        E2: crate::error::ParseError<I> + From<E1>,
        F: Parser<I, O1, E1>,
    > Parser<I, O2, E2> for Into<F, O1, O2, E1, E2> {
        fn parse(&mut self, i: I) -> IResult<I, O2, E2> {
            match self.f.parse(i) {
                Ok((i, o)) => Ok((i, o.into())),
                Err(Err::Error(e)) => Err(Err::Error(e.into())),
                Err(Err::Failure(e)) => Err(Err::Failure(e.into())),
                Err(Err::Incomplete(e)) => Err(Err::Incomplete(e)),
            }
        }
    }
}
pub mod multi {
    //! Combinators applying their child parser multiple times
    use crate::error::ErrorKind;
    use crate::error::ParseError;
    use crate::internal::{Err, IResult, Needed, Parser};
    #[cfg(feature = "alloc")]
    use crate::lib::std::vec::Vec;
    use crate::traits::{InputLength, InputTake, ToUsize};
    use core::num::NonZeroUsize;
    /// Don't pre-allocate more than 64KiB when calling `Vec::with_capacity`.
    ///
    /// Pre-allocating memory is a nice optimization but count fields can't
    /// always be trusted. We should clamp initial capacities to some reasonable
    /// amount. This reduces the risk of a bogus count value triggering a panic
    /// due to an OOM error.
    ///
    /// This does not affect correctness. Nom will always read the full number
    /// of elements regardless of the capacity cap.
    #[cfg(feature = "alloc")]
    const MAX_INITIAL_CAPACITY_BYTES: usize = 65536;
    /// Repeats the embedded parser, gathering the results in a `Vec`.
    ///
    /// This stops on [`Err::Error`] and returns the results that were accumulated. To instead chain an error up, see
    /// [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    ///
    /// *Note*: if the parser passed in accepts empty inputs (like `alpha0` or `digit0`), `many0` will
    /// return an error, to prevent going into an infinite loop
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed, IResult};
    /// use nom::multi::many0;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   many0(tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", vec!["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Ok(("123", vec!["abc"])));
    /// assert_eq!(parser("123123"), Ok(("123123", vec![])));
    /// assert_eq!(parser(""), Ok(("", vec![])));
    /// ```
    #[cfg(feature = "alloc")]
    pub fn many0<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        E: ParseError<I>,
    {
        move |mut i: I| {
            let mut acc = crate::lib::std::vec::Vec::with_capacity(4);
            loop {
                let len = i.input_len();
                match f.parse(i.clone()) {
                    Err(Err::Error(_)) => return Ok((i, acc)),
                    Err(e) => return Err(e),
                    Ok((i1, o)) => {
                        if i1.input_len() == len {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Many0)),
                            );
                        }
                        i = i1;
                        acc.push(o);
                    }
                }
            }
        }
    }
    /// Runs the embedded parser, gathering the results in a `Vec`.
    ///
    /// This stops on [`Err::Error`] if there is at least one result,  and returns the results that were accumulated. To instead chain an error up,
    /// see [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    ///
    /// *Note*: If the parser passed to `many1` accepts empty inputs
    /// (like `alpha0` or `digit0`), `many1` will return an error,
    /// to prevent going into an infinite loop.
    ///
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::multi::many1;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   many1(tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", vec!["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Ok(("123", vec!["abc"])));
    /// assert_eq!(parser("123123"), Err(Err::Error(Error::new("123123", ErrorKind::Tag))));
    /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
    /// ```
    #[cfg(feature = "alloc")]
    pub fn many1<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        E: ParseError<I>,
    {
        move |mut i: I| match f.parse(i.clone()) {
            Err(Err::Error(err)) => Err(Err::Error(E::append(i, ErrorKind::Many1, err))),
            Err(e) => Err(e),
            Ok((i1, o)) => {
                let mut acc = crate::lib::std::vec::Vec::with_capacity(4);
                acc.push(o);
                i = i1;
                loop {
                    let len = i.input_len();
                    match f.parse(i.clone()) {
                        Err(Err::Error(_)) => return Ok((i, acc)),
                        Err(e) => return Err(e),
                        Ok((i1, o)) => {
                            if i1.input_len() == len {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Many1)),
                                );
                            }
                            i = i1;
                            acc.push(o);
                        }
                    }
                }
            }
        }
    }
    /// Applies the parser `f` until the parser `g` produces a result.
    ///
    /// Returns a tuple of the results of `f` in a `Vec` and the result of `g`.
    ///
    /// `f` keeps going so long as `g` produces [`Err::Error`]. To instead chain an error up, see [`cut`][crate::combinator::cut].
    ///
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::multi::many_till;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, (Vec<&str>, &str)> {
    ///   many_till(tag("abc"), tag("end"))(s)
    /// };
    ///
    /// assert_eq!(parser("abcabcend"), Ok(("", (vec!["abc", "abc"], "end"))));
    /// assert_eq!(parser("abc123end"), Err(Err::Error(Error::new("123end", ErrorKind::Tag))));
    /// assert_eq!(parser("123123end"), Err(Err::Error(Error::new("123123end", ErrorKind::Tag))));
    /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
    /// assert_eq!(parser("abcendefg"), Ok(("efg", (vec!["abc"], "end"))));
    /// ```
    #[cfg(feature = "alloc")]
    pub fn many_till<I, O, P, E, F, G>(
        mut f: F,
        mut g: G,
    ) -> impl FnMut(I) -> IResult<I, (Vec<O>, P), E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        G: Parser<I, P, E>,
        E: ParseError<I>,
    {
        move |mut i: I| {
            let mut res = crate::lib::std::vec::Vec::new();
            loop {
                let len = i.input_len();
                match g.parse(i.clone()) {
                    Ok((i1, o)) => return Ok((i1, (res, o))),
                    Err(Err::Error(_)) => {
                        match f.parse(i.clone()) {
                            Err(Err::Error(err)) => {
                                return Err(
                                    Err::Error(E::append(i, ErrorKind::ManyTill, err)),
                                );
                            }
                            Err(e) => return Err(e),
                            Ok((i1, o)) => {
                                if i1.input_len() == len {
                                    return Err(
                                        Err::Error(E::from_error_kind(i1, ErrorKind::ManyTill)),
                                    );
                                }
                                res.push(o);
                                i = i1;
                            }
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
        }
    }
    /// Alternates between two parsers to produce a list of elements.
    ///
    /// This stops when either parser returns [`Err::Error`]  and returns the results that were accumulated. To instead chain an error up, see
    /// [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `sep` Parses the separator between list elements.
    /// * `f` Parses the elements of the list.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed, IResult};
    /// use nom::multi::separated_list0;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   separated_list0(tag("|"), tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser("abc|abc|abc"), Ok(("", vec!["abc", "abc", "abc"])));
    /// assert_eq!(parser("abc123abc"), Ok(("123abc", vec!["abc"])));
    /// assert_eq!(parser("abc|def"), Ok(("|def", vec!["abc"])));
    /// assert_eq!(parser(""), Ok(("", vec![])));
    /// assert_eq!(parser("def|abc"), Ok(("def|abc", vec![])));
    /// ```
    #[cfg(feature = "alloc")]
    pub fn separated_list0<I, O, O2, E, F, G>(
        mut sep: G,
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        G: Parser<I, O2, E>,
        E: ParseError<I>,
    {
        move |mut i: I| {
            let mut res = Vec::new();
            match f.parse(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    res.push(o);
                    i = i1;
                }
            }
            loop {
                let len = i.input_len();
                match sep.parse(i.clone()) {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i1, _)) => {
                        if i1.input_len() == len {
                            return Err(
                                Err::Error(E::from_error_kind(i1, ErrorKind::SeparatedList)),
                            );
                        }
                        match f.parse(i1.clone()) {
                            Err(Err::Error(_)) => return Ok((i, res)),
                            Err(e) => return Err(e),
                            Ok((i2, o)) => {
                                res.push(o);
                                i = i2;
                            }
                        }
                    }
                }
            }
        }
    }
    /// Alternates between two parsers to produce a list of elements until [`Err::Error`].
    ///
    /// Fails if the element parser does not produce at least one element.$
    ///
    /// This stops when either parser returns [`Err::Error`]  and returns the results that were accumulated. To instead chain an error up, see
    /// [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `sep` Parses the separator between list elements.
    /// * `f` Parses the elements of the list.
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::multi::separated_list1;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   separated_list1(tag("|"), tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser("abc|abc|abc"), Ok(("", vec!["abc", "abc", "abc"])));
    /// assert_eq!(parser("abc123abc"), Ok(("123abc", vec!["abc"])));
    /// assert_eq!(parser("abc|def"), Ok(("|def", vec!["abc"])));
    /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
    /// assert_eq!(parser("def|abc"), Err(Err::Error(Error::new("def|abc", ErrorKind::Tag))));
    /// ```
    #[cfg(feature = "alloc")]
    pub fn separated_list1<I, O, O2, E, F, G>(
        mut sep: G,
        mut f: F,
    ) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        G: Parser<I, O2, E>,
        E: ParseError<I>,
    {
        move |mut i: I| {
            let mut res = Vec::new();
            match f.parse(i.clone()) {
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    res.push(o);
                    i = i1;
                }
            }
            loop {
                let len = i.input_len();
                match sep.parse(i.clone()) {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i1, _)) => {
                        if i1.input_len() == len {
                            return Err(
                                Err::Error(E::from_error_kind(i1, ErrorKind::SeparatedList)),
                            );
                        }
                        match f.parse(i1.clone()) {
                            Err(Err::Error(_)) => return Ok((i, res)),
                            Err(e) => return Err(e),
                            Ok((i2, o)) => {
                                res.push(o);
                                i = i2;
                            }
                        }
                    }
                }
            }
        }
    }
    /// Repeats the embedded parser `m..=n` times
    ///
    /// This stops before `n` when the parser returns [`Err::Error`]  and returns the results that were accumulated. To instead chain an error up, see
    /// [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `m` The minimum number of iterations.
    /// * `n` The maximum number of iterations.
    /// * `f` The parser to apply.
    ///
    /// *Note*: If the parser passed to `many1` accepts empty inputs
    /// (like `alpha0` or `digit0`), `many1` will return an error,
    /// to prevent going into an infinite loop.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed, IResult};
    /// use nom::multi::many_m_n;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   many_m_n(0, 2, tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", vec!["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Ok(("123", vec!["abc"])));
    /// assert_eq!(parser("123123"), Ok(("123123", vec![])));
    /// assert_eq!(parser(""), Ok(("", vec![])));
    /// assert_eq!(parser("abcabcabc"), Ok(("abc", vec!["abc", "abc"])));
    /// ```
    #[cfg(feature = "alloc")]
    pub fn many_m_n<I, O, E, F>(
        min: usize,
        max: usize,
        mut parse: F,
    ) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        E: ParseError<I>,
    {
        move |mut input: I| {
            if min > max {
                return Err(Err::Failure(E::from_error_kind(input, ErrorKind::ManyMN)));
            }
            let max_initial_capacity = MAX_INITIAL_CAPACITY_BYTES
                / crate::lib::std::mem::size_of::<O>().max(1);
            let mut res = crate::lib::std::vec::Vec::with_capacity(
                min.min(max_initial_capacity),
            );
            for count in 0..max {
                let len = input.input_len();
                match parse.parse(input.clone()) {
                    Ok((tail, value)) => {
                        if tail.input_len() == len {
                            return Err(
                                Err::Error(E::from_error_kind(input, ErrorKind::ManyMN)),
                            );
                        }
                        res.push(value);
                        input = tail;
                    }
                    Err(Err::Error(e)) => {
                        if count < min {
                            return Err(
                                Err::Error(E::append(input, ErrorKind::ManyMN, e)),
                            );
                        } else {
                            return Ok((input, res));
                        }
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
            Ok((input, res))
        }
    }
    /// Repeats the embedded parser, counting the results
    ///
    /// This stops on [`Err::Error`]. To instead chain an error up, see
    /// [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    ///
    /// *Note*: if the parser passed in accepts empty inputs (like `alpha0` or `digit0`), `many0` will
    /// return an error, to prevent going into an infinite loop
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed, IResult};
    /// use nom::multi::many0_count;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, usize> {
    ///   many0_count(tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", 2)));
    /// assert_eq!(parser("abc123"), Ok(("123", 1)));
    /// assert_eq!(parser("123123"), Ok(("123123", 0)));
    /// assert_eq!(parser(""), Ok(("", 0)));
    /// ```
    pub fn many0_count<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, usize, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        E: ParseError<I>,
    {
        move |i: I| {
            let mut input = i;
            let mut count = 0;
            loop {
                let input_ = input.clone();
                let len = input.input_len();
                match f.parse(input_) {
                    Ok((i, _)) => {
                        if i.input_len() == len {
                            return Err(
                                Err::Error(E::from_error_kind(input, ErrorKind::Many0Count)),
                            );
                        }
                        input = i;
                        count += 1;
                    }
                    Err(Err::Error(_)) => return Ok((input, count)),
                    Err(e) => return Err(e),
                }
            }
        }
    }
    /// Runs the embedded parser, counting the results.
    ///
    /// This stops on [`Err::Error`] if there is at least one result. To instead chain an error up,
    /// see [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    ///
    /// *Note*: If the parser passed to `many1` accepts empty inputs
    /// (like `alpha0` or `digit0`), `many1` will return an error,
    /// to prevent going into an infinite loop.
    ///
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::multi::many1_count;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, usize> {
    ///   many1_count(tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", 2)));
    /// assert_eq!(parser("abc123"), Ok(("123", 1)));
    /// assert_eq!(parser("123123"), Err(Err::Error(Error::new("123123", ErrorKind::Many1Count))));
    /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Many1Count))));
    /// ```
    pub fn many1_count<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, usize, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        E: ParseError<I>,
    {
        move |i: I| {
            let i_ = i.clone();
            match f.parse(i_) {
                Err(Err::Error(_)) => {
                    Err(Err::Error(E::from_error_kind(i, ErrorKind::Many1Count)))
                }
                Err(i) => Err(i),
                Ok((i1, _)) => {
                    let mut count = 1;
                    let mut input = i1;
                    loop {
                        let len = input.input_len();
                        let input_ = input.clone();
                        match f.parse(input_) {
                            Err(Err::Error(_)) => return Ok((input, count)),
                            Err(e) => return Err(e),
                            Ok((i, _)) => {
                                if i.input_len() == len {
                                    return Err(
                                        Err::Error(E::from_error_kind(i, ErrorKind::Many1Count)),
                                    );
                                }
                                count += 1;
                                input = i;
                            }
                        }
                    }
                }
            }
        }
    }
    /// Runs the embedded parser `count` times, gathering the results in a `Vec`
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    /// * `count` How often to apply the parser.
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::multi::count;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   count(tag("abc"), 2)(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", vec!["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Err(Err::Error(Error::new("123", ErrorKind::Tag))));
    /// assert_eq!(parser("123123"), Err(Err::Error(Error::new("123123", ErrorKind::Tag))));
    /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
    /// assert_eq!(parser("abcabcabc"), Ok(("abc", vec!["abc", "abc"])));
    /// ```
    #[cfg(feature = "alloc")]
    pub fn count<I, O, E, F>(
        mut f: F,
        count: usize,
    ) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: Clone + PartialEq,
        F: Parser<I, O, E>,
        E: ParseError<I>,
    {
        move |i: I| {
            let mut input = i.clone();
            let max_initial_capacity = MAX_INITIAL_CAPACITY_BYTES
                / crate::lib::std::mem::size_of::<O>().max(1);
            let mut res = crate::lib::std::vec::Vec::with_capacity(
                count.min(max_initial_capacity),
            );
            for _ in 0..count {
                let input_ = input.clone();
                match f.parse(input_) {
                    Ok((i, o)) => {
                        res.push(o);
                        input = i;
                    }
                    Err(Err::Error(e)) => {
                        return Err(Err::Error(E::append(i, ErrorKind::Count, e)));
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
            Ok((input, res))
        }
    }
    /// Runs the embedded parser repeatedly, filling the given slice with results.
    ///
    /// This parser fails if the input runs out before the given slice is full.
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    /// * `buf` The slice to fill
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::multi::fill;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, [&str; 2]> {
    ///   let mut buf = ["", ""];
    ///   let (rest, ()) = fill(tag("abc"), &mut buf)(s)?;
    ///   Ok((rest, buf))
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", ["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Err(Err::Error(Error::new("123", ErrorKind::Tag))));
    /// assert_eq!(parser("123123"), Err(Err::Error(Error::new("123123", ErrorKind::Tag))));
    /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
    /// assert_eq!(parser("abcabcabc"), Ok(("abc", ["abc", "abc"])));
    /// ```
    pub fn fill<'a, I, O, E, F>(
        f: F,
        buf: &'a mut [O],
    ) -> impl FnMut(I) -> IResult<I, (), E> + 'a
    where
        I: Clone + PartialEq,
        F: Fn(I) -> IResult<I, O, E> + 'a,
        E: ParseError<I>,
    {
        move |i: I| {
            let mut input = i.clone();
            for elem in buf.iter_mut() {
                let input_ = input.clone();
                match f(input_) {
                    Ok((i, o)) => {
                        *elem = o;
                        input = i;
                    }
                    Err(Err::Error(e)) => {
                        return Err(Err::Error(E::append(i, ErrorKind::Count, e)));
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
            Ok((input, ()))
        }
    }
    /// Repeats the embedded parser, calling `g` to gather the results.
    ///
    /// This stops on [`Err::Error`]. To instead chain an error up, see
    /// [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    /// * `init` A function returning the initial value.
    /// * `g` The function that combines a result of `f` with
    ///       the current accumulator.
    ///
    /// *Note*: if the parser passed in accepts empty inputs (like `alpha0` or `digit0`), `many0` will
    /// return an error, to prevent going into an infinite loop
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed, IResult};
    /// use nom::multi::fold_many0;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   fold_many0(
    ///     tag("abc"),
    ///     Vec::new,
    ///     |mut acc: Vec<_>, item| {
    ///       acc.push(item);
    ///       acc
    ///     }
    ///   )(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", vec!["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Ok(("123", vec!["abc"])));
    /// assert_eq!(parser("123123"), Ok(("123123", vec![])));
    /// assert_eq!(parser(""), Ok(("", vec![])));
    /// ```
    pub fn fold_many0<I, O, E, F, G, H, R>(
        mut f: F,
        mut init: H,
        mut g: G,
    ) -> impl FnMut(I) -> IResult<I, R, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        G: FnMut(R, O) -> R,
        H: FnMut() -> R,
        E: ParseError<I>,
    {
        move |i: I| {
            let mut res = init();
            let mut input = i;
            loop {
                let i_ = input.clone();
                let len = input.input_len();
                match f.parse(i_) {
                    Ok((i, o)) => {
                        if i.input_len() == len {
                            return Err(
                                Err::Error(E::from_error_kind(input, ErrorKind::Many0)),
                            );
                        }
                        res = g(res, o);
                        input = i;
                    }
                    Err(Err::Error(_)) => {
                        return Ok((input, res));
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
        }
    }
    /// Repeats the embedded parser, calling `g` to gather the results.
    ///
    /// This stops on [`Err::Error`] if there is at least one result. To instead chain an error up,
    /// see [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    /// * `init` A function returning the initial value.
    /// * `g` The function that combines a result of `f` with
    ///       the current accumulator.
    ///
    /// *Note*: If the parser passed to `many1` accepts empty inputs
    /// (like `alpha0` or `digit0`), `many1` will return an error,
    /// to prevent going into an infinite loop.
    ///
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::multi::fold_many1;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   fold_many1(
    ///     tag("abc"),
    ///     Vec::new,
    ///     |mut acc: Vec<_>, item| {
    ///       acc.push(item);
    ///       acc
    ///     }
    ///   )(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", vec!["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Ok(("123", vec!["abc"])));
    /// assert_eq!(parser("123123"), Err(Err::Error(Error::new("123123", ErrorKind::Many1))));
    /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Many1))));
    /// ```
    pub fn fold_many1<I, O, E, F, G, H, R>(
        mut f: F,
        mut init: H,
        mut g: G,
    ) -> impl FnMut(I) -> IResult<I, R, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        G: FnMut(R, O) -> R,
        H: FnMut() -> R,
        E: ParseError<I>,
    {
        move |i: I| {
            let _i = i.clone();
            let init = init();
            match f.parse(_i) {
                Err(Err::Error(_)) => {
                    Err(Err::Error(E::from_error_kind(i, ErrorKind::Many1)))
                }
                Err(e) => Err(e),
                Ok((i1, o1)) => {
                    let mut acc = g(init, o1);
                    let mut input = i1;
                    loop {
                        let _input = input.clone();
                        let len = input.input_len();
                        match f.parse(_input) {
                            Err(Err::Error(_)) => {
                                break;
                            }
                            Err(e) => return Err(e),
                            Ok((i, o)) => {
                                if i.input_len() == len {
                                    return Err(
                                        Err::Failure(E::from_error_kind(i, ErrorKind::Many1)),
                                    );
                                }
                                acc = g(acc, o);
                                input = i;
                            }
                        }
                    }
                    Ok((input, acc))
                }
            }
        }
    }
    /// Repeats the embedded parser `m..=n` times, calling `g` to gather the results
    ///
    /// This stops before `n` when the parser returns [`Err::Error`]. To instead chain an error up, see
    /// [`cut`][crate::combinator::cut].
    ///
    /// # Arguments
    /// * `m` The minimum number of iterations.
    /// * `n` The maximum number of iterations.
    /// * `f` The parser to apply.
    /// * `init` A function returning the initial value.
    /// * `g` The function that combines a result of `f` with
    ///       the current accumulator.
    ///
    /// *Note*: If the parser passed to `many1` accepts empty inputs
    /// (like `alpha0` or `digit0`), `many1` will return an error,
    /// to prevent going into an infinite loop.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed, IResult};
    /// use nom::multi::fold_many_m_n;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   fold_many_m_n(
    ///     0,
    ///     2,
    ///     tag("abc"),
    ///     Vec::new,
    ///     |mut acc: Vec<_>, item| {
    ///       acc.push(item);
    ///       acc
    ///     }
    ///   )(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", vec!["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Ok(("123", vec!["abc"])));
    /// assert_eq!(parser("123123"), Ok(("123123", vec![])));
    /// assert_eq!(parser(""), Ok(("", vec![])));
    /// assert_eq!(parser("abcabcabc"), Ok(("abc", vec!["abc", "abc"])));
    /// ```
    pub fn fold_many_m_n<I, O, E, F, G, H, R>(
        min: usize,
        max: usize,
        mut parse: F,
        mut init: H,
        mut fold: G,
    ) -> impl FnMut(I) -> IResult<I, R, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        G: FnMut(R, O) -> R,
        H: FnMut() -> R,
        E: ParseError<I>,
    {
        move |mut input: I| {
            if min > max {
                return Err(Err::Failure(E::from_error_kind(input, ErrorKind::ManyMN)));
            }
            let mut acc = init();
            for count in 0..max {
                let len = input.input_len();
                match parse.parse(input.clone()) {
                    Ok((tail, value)) => {
                        if tail.input_len() == len {
                            return Err(
                                Err::Error(E::from_error_kind(tail, ErrorKind::ManyMN)),
                            );
                        }
                        acc = fold(acc, value);
                        input = tail;
                    }
                    Err(Err::Error(err)) => {
                        if count < min {
                            return Err(
                                Err::Error(E::append(input, ErrorKind::ManyMN, err)),
                            );
                        } else {
                            break;
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok((input, acc))
        }
    }
    /// Gets a number from the parser and returns a
    /// subslice of the input of that size.
    /// If the parser returns `Incomplete`,
    /// `length_data` will return an error.
    /// # Arguments
    /// * `f` The parser to apply.
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed, IResult};
    /// use nom::number::complete::be_u16;
    /// use nom::multi::length_data;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &[u8]) -> IResult<&[u8], &[u8]> {
    ///   length_data(be_u16)(s)
    /// }
    ///
    /// assert_eq!(parser(b"\x00\x03abcefg"), Ok((&b"efg"[..], &b"abc"[..])));
    /// assert_eq!(parser(b"\x00\x03a"), Err(Err::Incomplete(Needed::new(2))));
    /// ```
    pub fn length_data<I, N, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, I, E>
    where
        I: InputLength + InputTake,
        N: ToUsize,
        F: Parser<I, N, E>,
        E: ParseError<I>,
    {
        move |i: I| {
            let (i, length) = f.parse(i)?;
            let length: usize = length.to_usize();
            if let Some(needed) = length
                .checked_sub(i.input_len())
                .and_then(NonZeroUsize::new)
            {
                Err(Err::Incomplete(Needed::Size(needed)))
            } else {
                Ok(i.take_split(length))
            }
        }
    }
    /// Gets a number from the first parser,
    /// takes a subslice of the input of that size,
    /// then applies the second parser on that subslice.
    /// If the second parser returns `Incomplete`,
    /// `length_value` will return an error.
    /// # Arguments
    /// * `f` The parser to apply.
    /// * `g` The parser to apply on the subslice.
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::number::complete::be_u16;
    /// use nom::multi::length_value;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &[u8]) -> IResult<&[u8], &[u8]> {
    ///   length_value(be_u16, tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser(b"\x00\x03abcefg"), Ok((&b"efg"[..], &b"abc"[..])));
    /// assert_eq!(parser(b"\x00\x03123123"), Err(Err::Error(Error::new(&b"123"[..], ErrorKind::Tag))));
    /// assert_eq!(parser(b"\x00\x03a"), Err(Err::Incomplete(Needed::new(2))));
    /// ```
    pub fn length_value<I, O, N, E, F, G>(
        mut f: F,
        mut g: G,
    ) -> impl FnMut(I) -> IResult<I, O, E>
    where
        I: Clone + InputLength + InputTake,
        N: ToUsize,
        F: Parser<I, N, E>,
        G: Parser<I, O, E>,
        E: ParseError<I>,
    {
        move |i: I| {
            let (i, length) = f.parse(i)?;
            let length: usize = length.to_usize();
            if let Some(needed) = length
                .checked_sub(i.input_len())
                .and_then(NonZeroUsize::new)
            {
                Err(Err::Incomplete(Needed::Size(needed)))
            } else {
                let (rest, i) = i.take_split(length);
                match g.parse(i.clone()) {
                    Err(Err::Incomplete(_)) => {
                        Err(Err::Error(E::from_error_kind(i, ErrorKind::Complete)))
                    }
                    Err(e) => Err(e),
                    Ok((_, o)) => Ok((rest, o)),
                }
            }
        }
    }
    /// Gets a number from the first parser,
    /// then applies the second parser that many times.
    /// # Arguments
    /// * `f` The parser to apply to obtain the count.
    /// * `g` The parser to apply repeatedly.
    /// ```rust
    /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
    /// use nom::number::complete::u8;
    /// use nom::multi::length_count;
    /// use nom::bytes::complete::tag;
    /// use nom::combinator::map;
    ///
    /// fn parser(s: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
    ///   length_count(map(u8, |i| {
    ///      println!("got number: {}", i);
    ///      i
    ///   }), tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser(&b"\x02abcabcabc"[..]), Ok(((&b"abc"[..], vec![&b"abc"[..], &b"abc"[..]]))));
    /// assert_eq!(parser(b"\x03123123123"), Err(Err::Error(Error::new(&b"123123123"[..], ErrorKind::Tag))));
    /// ```
    #[cfg(feature = "alloc")]
    pub fn length_count<I, O, N, E, F, G>(
        mut f: F,
        mut g: G,
    ) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: Clone,
        N: ToUsize,
        F: Parser<I, N, E>,
        G: Parser<I, O, E>,
        E: ParseError<I>,
    {
        move |i: I| {
            let (i, count) = f.parse(i)?;
            let mut input = i.clone();
            let mut res = Vec::new();
            for _ in 0..count.to_usize() {
                let input_ = input.clone();
                match g.parse(input_) {
                    Ok((i, o)) => {
                        res.push(o);
                        input = i;
                    }
                    Err(Err::Error(e)) => {
                        return Err(Err::Error(E::append(i, ErrorKind::Count, e)));
                    }
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
            Ok((input, res))
        }
    }
}
pub mod sequence {
    //! Combinators applying parsers in sequence
    use crate::error::ParseError;
    use crate::internal::{IResult, Parser};
    /// Gets an object from the first parser,
    /// then gets another object from the second parser.
    ///
    /// # Arguments
    /// * `first` The first parser to apply.
    /// * `second` The second parser to apply.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed};
    /// # use nom::Needed::Size;
    /// use nom::sequence::pair;
    /// use nom::bytes::complete::tag;
    ///
    /// let mut parser = pair(tag("abc"), tag("efg"));
    ///
    /// assert_eq!(parser("abcefg"), Ok(("", ("abc", "efg"))));
    /// assert_eq!(parser("abcefghij"), Ok(("hij", ("abc", "efg"))));
    /// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::Tag))));
    /// assert_eq!(parser("123"), Err(Err::Error(("123", ErrorKind::Tag))));
    /// ```
    pub fn pair<I, O1, O2, E: ParseError<I>, F, G>(
        mut first: F,
        mut second: G,
    ) -> impl FnMut(I) -> IResult<I, (O1, O2), E>
    where
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
    {
        move |input: I| {
            let (input, o1) = first.parse(input)?;
            second.parse(input).map(|(i, o2)| (i, (o1, o2)))
        }
    }
    /// Matches an object from the first parser and discards it,
    /// then gets an object from the second parser.
    ///
    /// # Arguments
    /// * `first` The opening parser.
    /// * `second` The second parser to get object.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed};
    /// # use nom::Needed::Size;
    /// use nom::sequence::preceded;
    /// use nom::bytes::complete::tag;
    ///
    /// let mut parser = preceded(tag("abc"), tag("efg"));
    ///
    /// assert_eq!(parser("abcefg"), Ok(("", "efg")));
    /// assert_eq!(parser("abcefghij"), Ok(("hij", "efg")));
    /// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::Tag))));
    /// assert_eq!(parser("123"), Err(Err::Error(("123", ErrorKind::Tag))));
    /// ```
    pub fn preceded<I, O1, O2, E: ParseError<I>, F, G>(
        mut first: F,
        mut second: G,
    ) -> impl FnMut(I) -> IResult<I, O2, E>
    where
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
    {
        move |input: I| {
            let (input, _) = first.parse(input)?;
            second.parse(input)
        }
    }
    /// Gets an object from the first parser,
    /// then matches an object from the second parser and discards it.
    ///
    /// # Arguments
    /// * `first` The first parser to apply.
    /// * `second` The second parser to match an object.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed};
    /// # use nom::Needed::Size;
    /// use nom::sequence::terminated;
    /// use nom::bytes::complete::tag;
    ///
    /// let mut parser = terminated(tag("abc"), tag("efg"));
    ///
    /// assert_eq!(parser("abcefg"), Ok(("", "abc")));
    /// assert_eq!(parser("abcefghij"), Ok(("hij", "abc")));
    /// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::Tag))));
    /// assert_eq!(parser("123"), Err(Err::Error(("123", ErrorKind::Tag))));
    /// ```
    pub fn terminated<I, O1, O2, E: ParseError<I>, F, G>(
        mut first: F,
        mut second: G,
    ) -> impl FnMut(I) -> IResult<I, O1, E>
    where
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
    {
        move |input: I| {
            let (input, o1) = first.parse(input)?;
            second.parse(input).map(|(i, _)| (i, o1))
        }
    }
    /// Gets an object from the first parser,
    /// then matches an object from the sep_parser and discards it,
    /// then gets another object from the second parser.
    ///
    /// # Arguments
    /// * `first` The first parser to apply.
    /// * `sep` The separator parser to apply.
    /// * `second` The second parser to apply.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed};
    /// # use nom::Needed::Size;
    /// use nom::sequence::separated_pair;
    /// use nom::bytes::complete::tag;
    ///
    /// let mut parser = separated_pair(tag("abc"), tag("|"), tag("efg"));
    ///
    /// assert_eq!(parser("abc|efg"), Ok(("", ("abc", "efg"))));
    /// assert_eq!(parser("abc|efghij"), Ok(("hij", ("abc", "efg"))));
    /// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::Tag))));
    /// assert_eq!(parser("123"), Err(Err::Error(("123", ErrorKind::Tag))));
    /// ```
    pub fn separated_pair<I, O1, O2, O3, E: ParseError<I>, F, G, H>(
        mut first: F,
        mut sep: G,
        mut second: H,
    ) -> impl FnMut(I) -> IResult<I, (O1, O3), E>
    where
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
        H: Parser<I, O3, E>,
    {
        move |input: I| {
            let (input, o1) = first.parse(input)?;
            let (input, _) = sep.parse(input)?;
            second.parse(input).map(|(i, o2)| (i, (o1, o2)))
        }
    }
    /// Matches an object from the first parser and discards it,
    /// then gets an object from the second parser,
    /// and finally matches an object from the third parser and discards it.
    ///
    /// # Arguments
    /// * `first` The first parser to apply and discard.
    /// * `second` The second parser to apply.
    /// * `third` The third parser to apply and discard.
    ///
    /// ```rust
    /// # use nom::{Err, error::ErrorKind, Needed};
    /// # use nom::Needed::Size;
    /// use nom::sequence::delimited;
    /// use nom::bytes::complete::tag;
    ///
    /// let mut parser = delimited(tag("("), tag("abc"), tag(")"));
    ///
    /// assert_eq!(parser("(abc)"), Ok(("", "abc")));
    /// assert_eq!(parser("(abc)def"), Ok(("def", "abc")));
    /// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::Tag))));
    /// assert_eq!(parser("123"), Err(Err::Error(("123", ErrorKind::Tag))));
    /// ```
    pub fn delimited<I, O1, O2, O3, E: ParseError<I>, F, G, H>(
        mut first: F,
        mut second: G,
        mut third: H,
    ) -> impl FnMut(I) -> IResult<I, O2, E>
    where
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
        H: Parser<I, O3, E>,
    {
        move |input: I| {
            let (input, _) = first.parse(input)?;
            let (input, o2) = second.parse(input)?;
            third.parse(input).map(|(i, _)| (i, o2))
        }
    }
    /// Helper trait for the tuple combinator.
    ///
    /// This trait is implemented for tuples of parsers of up to 21 elements.
    pub trait Tuple<I, O, E> {
        /// Parses the input and returns a tuple of results of each parser.
        fn parse(&mut self, input: I) -> IResult<I, O, E>;
    }
    impl<
        Input,
        Output,
        Error: ParseError<Input>,
        F: Parser<Input, Output, Error>,
    > Tuple<Input, (Output,), Error> for (F,) {
        fn parse(&mut self, input: Input) -> IResult<Input, (Output,), Error> {
            self.0.parse(input).map(|(i, o)| (i, (o,)))
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
    > Tuple<Input, (A, B), Error> for (FnA, FnB) {
        fn parse(&mut self, input: Input) -> IResult<Input, (A, B), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    Ok((i, (o, o)))
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
    > Tuple<Input, (A, B, C), Error> for (FnA, FnB, FnC) {
        fn parse(&mut self, input: Input) -> IResult<Input, (A, B, C), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        Ok((i, (o, o, o)))
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
    > Tuple<Input, (A, B, C, D), Error> for (FnA, FnB, FnC, FnD) {
        fn parse(&mut self, input: Input) -> IResult<Input, (A, B, C, D), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            Ok((i, (o, o, o, o)))
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
    > Tuple<Input, (A, B, C, D, E), Error> for (FnA, FnB, FnC, FnD, FnE) {
        fn parse(&mut self, input: Input) -> IResult<Input, (A, B, C, D, E), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                Ok((i, (o, o, o, o, o)))
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
    > Tuple<Input, (A, B, C, D, E, F), Error> for (FnA, FnB, FnC, FnD, FnE, FnF) {
        fn parse(&mut self, input: Input) -> IResult<Input, (A, B, C, D, E, F), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    Ok((i, (o, o, o, o, o, o)))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        Ok((i, (o, o, o, o, o, o, o)))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            Ok((i, (o, o, o, o, o, o, o, o)))
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                Ok((i, (o, o, o, o, o, o, o, o, o)))
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    Ok((i, (o, o, o, o, o, o, o, o, o, o)))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        Ok((i, (o, o, o, o, o, o, o, o, o, o, o)))
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK, FnL) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            Ok((i, (o, o, o, o, o, o, o, o, o, o, o, o)))
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK, FnL, FnM) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                Ok((i, (o, o, o, o, o, o, o, o, o, o, o, o, o)))
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK, FnL, FnM, FnN) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                {
                                                                    let (i, o) = self.13.parse(i.clone())?;
                                                                    Ok((i, (o, o, o, o, o, o, o, o, o, o, o, o, o, o)))
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Error>
    for (FnA, FnB, FnC, FnD, FnE, FnF, FnG, FnH, FnI, FnJ, FnK, FnL, FnM, FnN, FnO) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                {
                                                                    let (i, o) = self.13.parse(i.clone())?;
                                                                    {
                                                                        let (i, o) = self.14.parse(i.clone())?;
                                                                        Ok((i, (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o)))
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Error>
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
    ) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                {
                                                                    let (i, o) = self.13.parse(i.clone())?;
                                                                    {
                                                                        let (i, o) = self.14.parse(i.clone())?;
                                                                        {
                                                                            let (i, o) = self.15.parse(i.clone())?;
                                                                            Ok((i, (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o)))
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Error>
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
    ) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Error> {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                {
                                                                    let (i, o) = self.13.parse(i.clone())?;
                                                                    {
                                                                        let (i, o) = self.14.parse(i.clone())?;
                                                                        {
                                                                            let (i, o) = self.15.parse(i.clone())?;
                                                                            {
                                                                                let (i, o) = self.16.parse(i.clone())?;
                                                                                Ok((i, (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o)))
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
        FnR: Parser<Input, R, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Error>
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
        FnR,
    ) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<
            Input,
            (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
            Error,
        > {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                {
                                                                    let (i, o) = self.13.parse(i.clone())?;
                                                                    {
                                                                        let (i, o) = self.14.parse(i.clone())?;
                                                                        {
                                                                            let (i, o) = self.15.parse(i.clone())?;
                                                                            {
                                                                                let (i, o) = self.16.parse(i.clone())?;
                                                                                {
                                                                                    let (i, o) = self.17.parse(i.clone())?;
                                                                                    Ok((
                                                                                        i,
                                                                                        (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o),
                                                                                    ))
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
        FnR: Parser<Input, R, Error>,
        FnS: Parser<Input, S, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Error>
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
        FnR,
        FnS,
    ) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<
            Input,
            (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
            Error,
        > {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                {
                                                                    let (i, o) = self.13.parse(i.clone())?;
                                                                    {
                                                                        let (i, o) = self.14.parse(i.clone())?;
                                                                        {
                                                                            let (i, o) = self.15.parse(i.clone())?;
                                                                            {
                                                                                let (i, o) = self.16.parse(i.clone())?;
                                                                                {
                                                                                    let (i, o) = self.17.parse(i.clone())?;
                                                                                    {
                                                                                        let (i, o) = self.18.parse(i.clone())?;
                                                                                        Ok((
                                                                                            i,
                                                                                            (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o),
                                                                                        ))
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        T,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
        FnR: Parser<Input, R, Error>,
        FnS: Parser<Input, S, Error>,
        FnT: Parser<Input, T, Error>,
    > Tuple<Input, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Error>
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
        FnR,
        FnS,
        FnT,
    ) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<
            Input,
            (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
            Error,
        > {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                {
                                                                    let (i, o) = self.13.parse(i.clone())?;
                                                                    {
                                                                        let (i, o) = self.14.parse(i.clone())?;
                                                                        {
                                                                            let (i, o) = self.15.parse(i.clone())?;
                                                                            {
                                                                                let (i, o) = self.16.parse(i.clone())?;
                                                                                {
                                                                                    let (i, o) = self.17.parse(i.clone())?;
                                                                                    {
                                                                                        let (i, o) = self.18.parse(i.clone())?;
                                                                                        {
                                                                                            let (i, o) = self.19.parse(i.clone())?;
                                                                                            Ok((
                                                                                                i,
                                                                                                (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o),
                                                                                            ))
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<
        Input: Clone,
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        T,
        U,
        Error: ParseError<Input>,
        FnA: Parser<Input, A, Error>,
        FnB: Parser<Input, B, Error>,
        FnC: Parser<Input, C, Error>,
        FnD: Parser<Input, D, Error>,
        FnE: Parser<Input, E, Error>,
        FnF: Parser<Input, F, Error>,
        FnG: Parser<Input, G, Error>,
        FnH: Parser<Input, H, Error>,
        FnI: Parser<Input, I, Error>,
        FnJ: Parser<Input, J, Error>,
        FnK: Parser<Input, K, Error>,
        FnL: Parser<Input, L, Error>,
        FnM: Parser<Input, M, Error>,
        FnN: Parser<Input, N, Error>,
        FnO: Parser<Input, O, Error>,
        FnP: Parser<Input, P, Error>,
        FnQ: Parser<Input, Q, Error>,
        FnR: Parser<Input, R, Error>,
        FnS: Parser<Input, S, Error>,
        FnT: Parser<Input, T, Error>,
        FnU: Parser<Input, U, Error>,
    > Tuple<
        Input,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
        Error,
    >
    for (
        FnA,
        FnB,
        FnC,
        FnD,
        FnE,
        FnF,
        FnG,
        FnH,
        FnI,
        FnJ,
        FnK,
        FnL,
        FnM,
        FnN,
        FnO,
        FnP,
        FnQ,
        FnR,
        FnS,
        FnT,
        FnU,
    ) {
        fn parse(
            &mut self,
            input: Input,
        ) -> IResult<
            Input,
            (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
            Error,
        > {
            {
                let (i, o) = self.0.parse(input.clone())?;
                {
                    let (i, o) = self.1.parse(i.clone())?;
                    {
                        let (i, o) = self.2.parse(i.clone())?;
                        {
                            let (i, o) = self.3.parse(i.clone())?;
                            {
                                let (i, o) = self.4.parse(i.clone())?;
                                {
                                    let (i, o) = self.5.parse(i.clone())?;
                                    {
                                        let (i, o) = self.6.parse(i.clone())?;
                                        {
                                            let (i, o) = self.7.parse(i.clone())?;
                                            {
                                                let (i, o) = self.8.parse(i.clone())?;
                                                {
                                                    let (i, o) = self.9.parse(i.clone())?;
                                                    {
                                                        let (i, o) = self.10.parse(i.clone())?;
                                                        {
                                                            let (i, o) = self.11.parse(i.clone())?;
                                                            {
                                                                let (i, o) = self.12.parse(i.clone())?;
                                                                {
                                                                    let (i, o) = self.13.parse(i.clone())?;
                                                                    {
                                                                        let (i, o) = self.14.parse(i.clone())?;
                                                                        {
                                                                            let (i, o) = self.15.parse(i.clone())?;
                                                                            {
                                                                                let (i, o) = self.16.parse(i.clone())?;
                                                                                {
                                                                                    let (i, o) = self.17.parse(i.clone())?;
                                                                                    {
                                                                                        let (i, o) = self.18.parse(i.clone())?;
                                                                                        {
                                                                                            let (i, o) = self.19.parse(i.clone())?;
                                                                                            {
                                                                                                let (i, o) = self.20.parse(i.clone())?;
                                                                                                Ok((
                                                                                                    i,
                                                                                                    (
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                        o,
                                                                                                    ),
                                                                                                ))
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    impl<I, E: ParseError<I>> Tuple<I, (), E> for () {
        fn parse(&mut self, input: I) -> IResult<I, (), E> {
            Ok((input, ()))
        }
    }
    ///Applies a tuple of parsers one by one and returns their results as a tuple.
    ///There is a maximum of 21 parsers
    /// ```rust
    /// # use nom::{Err, error::ErrorKind};
    /// use nom::sequence::tuple;
    /// use nom::character::complete::{alpha1, digit1};
    /// let mut parser = tuple((alpha1, digit1, alpha1));
    ///
    /// assert_eq!(parser("abc123def"), Ok(("", ("abc", "123", "def"))));
    /// assert_eq!(parser("123def"), Err(Err::Error(("123def", ErrorKind::Alpha))));
    /// ```
    pub fn tuple<I, O, E: ParseError<I>, List: Tuple<I, O, E>>(
        mut l: List,
    ) -> impl FnMut(I) -> IResult<I, O, E> {
        move |i: I| l.parse(i)
    }
}
mod traits {
    //! Traits input types have to implement to work with nom combinators
    use crate::error::{ErrorKind, ParseError};
    use crate::internal::{Err, IResult, Needed};
    use crate::lib::std::iter::{Copied, Enumerate};
    use crate::lib::std::ops::{Range, RangeFrom, RangeFull, RangeTo};
    use crate::lib::std::slice::Iter;
    use crate::lib::std::str::from_utf8;
    use crate::lib::std::str::CharIndices;
    use crate::lib::std::str::Chars;
    use crate::lib::std::str::FromStr;
    #[cfg(feature = "alloc")]
    use crate::lib::std::string::String;
    #[cfg(feature = "alloc")]
    use crate::lib::std::vec::Vec;
    /// Abstract method to calculate the input length
    pub trait InputLength {
        /// Calculates the input length, as indicated by its name,
        /// and the name of the trait itself
        fn input_len(&self) -> usize;
    }
    impl<'a, T> InputLength for &'a [T] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a str {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for (&'a [u8], usize) {
        #[inline]
        fn input_len(&self) -> usize {
            self.0.len() * 8 - self.1
        }
    }
    /// Useful functions to calculate the offset between slices and show a hexdump of a slice
    pub trait Offset {
        /// Offset between the first byte of self and the first byte of the argument
        fn offset(&self, second: &Self) -> usize;
    }
    impl Offset for [u8] {
        fn offset(&self, second: &Self) -> usize {
            let fst = self.as_ptr();
            let snd = second.as_ptr();
            snd as usize - fst as usize
        }
    }
    impl<'a> Offset for &'a [u8] {
        fn offset(&self, second: &Self) -> usize {
            let fst = self.as_ptr();
            let snd = second.as_ptr();
            snd as usize - fst as usize
        }
    }
    impl Offset for str {
        fn offset(&self, second: &Self) -> usize {
            let fst = self.as_ptr();
            let snd = second.as_ptr();
            snd as usize - fst as usize
        }
    }
    impl<'a> Offset for &'a str {
        fn offset(&self, second: &Self) -> usize {
            let fst = self.as_ptr();
            let snd = second.as_ptr();
            snd as usize - fst as usize
        }
    }
    /// Helper trait for types that can be viewed as a byte slice
    pub trait AsBytes {
        /// Casts the input type to a byte slice
        fn as_bytes(&self) -> &[u8];
    }
    impl<'a> AsBytes for &'a str {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            (*self).as_bytes()
        }
    }
    impl AsBytes for str {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self.as_ref()
        }
    }
    impl<'a> AsBytes for &'a [u8] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 0] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 0] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 1] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 1] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 2] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 2] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 3] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 3] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 4] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 4] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 5] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 5] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 6] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 6] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 7] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 7] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 8] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 8] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 9] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 9] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 10] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 10] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 11] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 11] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 12] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 12] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 13] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 13] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 14] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 14] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 15] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 15] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 16] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 16] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 17] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 17] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 18] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 18] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 19] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 19] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 20] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 20] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 21] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 21] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 22] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 22] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 23] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 23] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 24] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 24] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 25] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 25] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 26] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 26] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 27] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 27] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 28] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 28] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 29] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 29] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 30] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 30] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 31] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 31] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    impl<'a> AsBytes for &'a [u8; 32] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            *self
        }
    }
    impl AsBytes for [u8; 32] {
        #[inline(always)]
        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
    /// Transforms common types to a char for basic token parsing
    pub trait AsChar {
        /// makes a char from self
        fn as_char(self) -> char;
        /// Tests that self is an alphabetic character
        ///
        /// Warning: for `&str` it recognizes alphabetic
        /// characters outside of the 52 ASCII letters
        fn is_alpha(self) -> bool;
        /// Tests that self is an alphabetic character
        /// or a decimal digit
        fn is_alphanum(self) -> bool;
        /// Tests that self is a decimal digit
        fn is_dec_digit(self) -> bool;
        /// Tests that self is an hex digit
        fn is_hex_digit(self) -> bool;
        /// Tests that self is an octal digit
        fn is_oct_digit(self) -> bool;
        /// Gets the len in bytes for self
        fn len(self) -> usize;
    }
    impl AsChar for u8 {
        #[inline]
        fn as_char(self) -> char {
            self as char
        }
        #[inline]
        fn is_alpha(self) -> bool {
            (self >= 0x41 && self <= 0x5A) || (self >= 0x61 && self <= 0x7A)
        }
        #[inline]
        fn is_alphanum(self) -> bool {
            self.is_alpha() || self.is_dec_digit()
        }
        #[inline]
        fn is_dec_digit(self) -> bool {
            self >= 0x30 && self <= 0x39
        }
        #[inline]
        fn is_hex_digit(self) -> bool {
            (self >= 0x30 && self <= 0x39) || (self >= 0x41 && self <= 0x46)
                || (self >= 0x61 && self <= 0x66)
        }
        #[inline]
        fn is_oct_digit(self) -> bool {
            self >= 0x30 && self <= 0x37
        }
        #[inline]
        fn len(self) -> usize {
            1
        }
    }
    impl<'a> AsChar for &'a u8 {
        #[inline]
        fn as_char(self) -> char {
            *self as char
        }
        #[inline]
        fn is_alpha(self) -> bool {
            (*self >= 0x41 && *self <= 0x5A) || (*self >= 0x61 && *self <= 0x7A)
        }
        #[inline]
        fn is_alphanum(self) -> bool {
            self.is_alpha() || self.is_dec_digit()
        }
        #[inline]
        fn is_dec_digit(self) -> bool {
            *self >= 0x30 && *self <= 0x39
        }
        #[inline]
        fn is_hex_digit(self) -> bool {
            (*self >= 0x30 && *self <= 0x39) || (*self >= 0x41 && *self <= 0x46)
                || (*self >= 0x61 && *self <= 0x66)
        }
        #[inline]
        fn is_oct_digit(self) -> bool {
            *self >= 0x30 && *self <= 0x37
        }
        #[inline]
        fn len(self) -> usize {
            1
        }
    }
    impl AsChar for char {
        #[inline]
        fn as_char(self) -> char {
            self
        }
        #[inline]
        fn is_alpha(self) -> bool {
            self.is_ascii_alphabetic()
        }
        #[inline]
        fn is_alphanum(self) -> bool {
            self.is_alpha() || self.is_dec_digit()
        }
        #[inline]
        fn is_dec_digit(self) -> bool {
            self.is_ascii_digit()
        }
        #[inline]
        fn is_hex_digit(self) -> bool {
            self.is_ascii_hexdigit()
        }
        #[inline]
        fn is_oct_digit(self) -> bool {
            self.is_digit(8)
        }
        #[inline]
        fn len(self) -> usize {
            self.len_utf8()
        }
    }
    impl<'a> AsChar for &'a char {
        #[inline]
        fn as_char(self) -> char {
            *self
        }
        #[inline]
        fn is_alpha(self) -> bool {
            self.is_ascii_alphabetic()
        }
        #[inline]
        fn is_alphanum(self) -> bool {
            self.is_alpha() || self.is_dec_digit()
        }
        #[inline]
        fn is_dec_digit(self) -> bool {
            self.is_ascii_digit()
        }
        #[inline]
        fn is_hex_digit(self) -> bool {
            self.is_ascii_hexdigit()
        }
        #[inline]
        fn is_oct_digit(self) -> bool {
            self.is_digit(8)
        }
        #[inline]
        fn len(self) -> usize {
            self.len_utf8()
        }
    }
    /// Abstracts common iteration operations on the input type
    pub trait InputIter {
        /// The current input type is a sequence of that `Item` type.
        ///
        /// Example: `u8` for `&[u8]` or `char` for `&str`
        type Item;
        /// An iterator over the input type, producing the item and its position
        /// for use with [Slice]. If we're iterating over `&str`, the position
        /// corresponds to the byte index of the character
        type Iter: Iterator<Item = (usize, Self::Item)>;
        /// An iterator over the input type, producing the item
        type IterElem: Iterator<Item = Self::Item>;
        /// Returns an iterator over the elements and their byte offsets
        fn iter_indices(&self) -> Self::Iter;
        /// Returns an iterator over the elements
        fn iter_elements(&self) -> Self::IterElem;
        /// Finds the byte position of the element
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool;
        /// Get the byte offset from the element's position in the stream
        fn slice_index(&self, count: usize) -> Result<usize, Needed>;
    }
    /// Abstracts slicing operations
    pub trait InputTake: Sized {
        /// Returns a slice of `count` bytes. panics if count > length
        fn take(&self, count: usize) -> Self;
        /// Split the stream at the `count` byte offset. panics if count > length
        fn take_split(&self, count: usize) -> (Self, Self);
    }
    impl<'a> InputIter for &'a [u8] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        #[inline]
        fn iter_indices(&self) -> Self::Iter {
            self.iter_elements().enumerate()
        }
        #[inline]
        fn iter_elements(&self) -> Self::IterElem {
            self.iter().copied()
        }
        #[inline]
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            self.iter().position(|b| predicate(*b))
        }
        #[inline]
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            if self.len() >= count {
                Ok(count)
            } else {
                Err(Needed::new(count - self.len()))
            }
        }
    }
    impl<'a> InputTake for &'a [u8] {
        #[inline]
        fn take(&self, count: usize) -> Self {
            &self[0..count]
        }
        #[inline]
        fn take_split(&self, count: usize) -> (Self, Self) {
            let (prefix, suffix) = self.split_at(count);
            (suffix, prefix)
        }
    }
    impl<'a> InputIter for &'a str {
        type Item = char;
        type Iter = CharIndices<'a>;
        type IterElem = Chars<'a>;
        #[inline]
        fn iter_indices(&self) -> Self::Iter {
            self.char_indices()
        }
        #[inline]
        fn iter_elements(&self) -> Self::IterElem {
            self.chars()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            for (o, c) in self.char_indices() {
                if predicate(c) {
                    return Some(o);
                }
            }
            None
        }
        #[inline]
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            let mut cnt = 0;
            for (index, _) in self.char_indices() {
                if cnt == count {
                    return Ok(index);
                }
                cnt += 1;
            }
            if cnt == count {
                return Ok(self.len());
            }
            Err(Needed::Unknown)
        }
    }
    impl<'a> InputTake for &'a str {
        #[inline]
        fn take(&self, count: usize) -> Self {
            &self[..count]
        }
        #[inline]
        fn take_split(&self, count: usize) -> (Self, Self) {
            let (prefix, suffix) = self.split_at(count);
            (suffix, prefix)
        }
    }
    /// Dummy trait used for default implementations (currently only used for `InputTakeAtPosition` and `Compare`).
    ///
    /// When implementing a custom input type, it is possible to use directly the
    /// default implementation: If the input type implements `InputLength`, `InputIter`,
    /// `InputTake` and `Clone`, you can implement `UnspecializedInput` and get
    /// a default version of `InputTakeAtPosition` and `Compare`.
    ///
    /// For performance reasons, you might want to write a custom implementation of
    /// `InputTakeAtPosition` (like the one for `&[u8]`).
    pub trait UnspecializedInput {}
    /// Methods to take as much input as possible until the provided function returns true for the current element.
    ///
    /// A large part of nom's basic parsers are built using this trait.
    pub trait InputTakeAtPosition: Sized {
        /// The current input type is a sequence of that `Item` type.
        ///
        /// Example: `u8` for `&[u8]` or `char` for `&str`
        type Item;
        /// Looks for the first element of the input type for which the condition returns true,
        /// and returns the input up to this position.
        ///
        /// *streaming version*: If no element is found matching the condition, this will return `Incomplete`
        fn split_at_position<P, E: ParseError<Self>>(
            &self,
            predicate: P,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool;
        /// Looks for the first element of the input type for which the condition returns true
        /// and returns the input up to this position.
        ///
        /// Fails if the produced slice is empty.
        ///
        /// *streaming version*: If no element is found matching the condition, this will return `Incomplete`
        fn split_at_position1<P, E: ParseError<Self>>(
            &self,
            predicate: P,
            e: ErrorKind,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool;
        /// Looks for the first element of the input type for which the condition returns true,
        /// and returns the input up to this position.
        ///
        /// *complete version*: If no element is found matching the condition, this will return the whole input
        fn split_at_position_complete<P, E: ParseError<Self>>(
            &self,
            predicate: P,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool;
        /// Looks for the first element of the input type for which the condition returns true
        /// and returns the input up to this position.
        ///
        /// Fails if the produced slice is empty.
        ///
        /// *complete version*: If no element is found matching the condition, this will return the whole input
        fn split_at_position1_complete<P, E: ParseError<Self>>(
            &self,
            predicate: P,
            e: ErrorKind,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool;
    }
    impl<
        T: InputLength + InputIter + InputTake + Clone + UnspecializedInput,
    > InputTakeAtPosition for T {
        type Item = <T as InputIter>::Item;
        fn split_at_position<P, E: ParseError<Self>>(
            &self,
            predicate: P,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.position(predicate) {
                Some(n) => Ok(self.take_split(n)),
                None => Err(Err::Incomplete(Needed::new(1))),
            }
        }
        fn split_at_position1<P, E: ParseError<Self>>(
            &self,
            predicate: P,
            e: ErrorKind,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.position(predicate) {
                Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
                Some(n) => Ok(self.take_split(n)),
                None => Err(Err::Incomplete(Needed::new(1))),
            }
        }
        fn split_at_position_complete<P, E: ParseError<Self>>(
            &self,
            predicate: P,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.split_at_position(predicate) {
                Err(Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
                res => res,
            }
        }
        fn split_at_position1_complete<P, E: ParseError<Self>>(
            &self,
            predicate: P,
            e: ErrorKind,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.split_at_position1(predicate, e) {
                Err(Err::Incomplete(_)) => {
                    if self.input_len() == 0 {
                        Err(Err::Error(E::from_error_kind(self.clone(), e)))
                    } else {
                        Ok(self.take_split(self.input_len()))
                    }
                }
                res => res,
            }
        }
    }
    impl<'a> InputTakeAtPosition for &'a [u8] {
        type Item = u8;
        fn split_at_position<P, E: ParseError<Self>>(
            &self,
            predicate: P,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.iter().position(|c| predicate(*c)) {
                Some(i) => Ok(self.take_split(i)),
                None => Err(Err::Incomplete(Needed::new(1))),
            }
        }
        fn split_at_position1<P, E: ParseError<Self>>(
            &self,
            predicate: P,
            e: ErrorKind,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.iter().position(|c| predicate(*c)) {
                Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                Some(i) => Ok(self.take_split(i)),
                None => Err(Err::Incomplete(Needed::new(1))),
            }
        }
        fn split_at_position_complete<P, E: ParseError<Self>>(
            &self,
            predicate: P,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.iter().position(|c| predicate(*c)) {
                Some(i) => Ok(self.take_split(i)),
                None => Ok(self.take_split(self.input_len())),
            }
        }
        fn split_at_position1_complete<P, E: ParseError<Self>>(
            &self,
            predicate: P,
            e: ErrorKind,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.iter().position(|c| predicate(*c)) {
                Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                Some(i) => Ok(self.take_split(i)),
                None => {
                    if self.is_empty() {
                        Err(Err::Error(E::from_error_kind(self, e)))
                    } else {
                        Ok(self.take_split(self.input_len()))
                    }
                }
            }
        }
    }
    impl<'a> InputTakeAtPosition for &'a str {
        type Item = char;
        fn split_at_position<P, E: ParseError<Self>>(
            &self,
            predicate: P,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.find(predicate) {
                Some(i) => {
                    unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) }
                }
                None => Err(Err::Incomplete(Needed::new(1))),
            }
        }
        fn split_at_position1<P, E: ParseError<Self>>(
            &self,
            predicate: P,
            e: ErrorKind,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.find(predicate) {
                Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                Some(i) => {
                    unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) }
                }
                None => Err(Err::Incomplete(Needed::new(1))),
            }
        }
        fn split_at_position_complete<P, E: ParseError<Self>>(
            &self,
            predicate: P,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.find(predicate) {
                Some(i) => {
                    unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) }
                }
                None => {
                    unsafe {
                        Ok((
                            self.get_unchecked(self.len()..),
                            self.get_unchecked(..self.len()),
                        ))
                    }
                }
            }
        }
        fn split_at_position1_complete<P, E: ParseError<Self>>(
            &self,
            predicate: P,
            e: ErrorKind,
        ) -> IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool,
        {
            match self.find(predicate) {
                Some(0) => Err(Err::Error(E::from_error_kind(self, e))),
                Some(i) => {
                    unsafe { Ok((self.get_unchecked(i..), self.get_unchecked(..i))) }
                }
                None => {
                    if self.is_empty() {
                        Err(Err::Error(E::from_error_kind(self, e)))
                    } else {
                        unsafe {
                            Ok((
                                self.get_unchecked(self.len()..),
                                self.get_unchecked(..self.len()),
                            ))
                        }
                    }
                }
            }
        }
    }
    /// Indicates whether a comparison was successful, an error, or
    /// if more data was needed
    pub enum CompareResult {
        /// Comparison was successful
        Ok,
        /// We need more data to be sure
        Incomplete,
        /// Comparison failed
        Error,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CompareResult {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    CompareResult::Ok => "Ok",
                    CompareResult::Incomplete => "Incomplete",
                    CompareResult::Error => "Error",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for CompareResult {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for CompareResult {
        #[inline]
        fn eq(&self, other: &CompareResult) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    /// Abstracts comparison operations
    pub trait Compare<T> {
        /// Compares self to another value for equality
        fn compare(&self, t: T) -> CompareResult;
        /// Compares self to another value for equality
        /// independently of the case.
        ///
        /// Warning: for `&str`, the comparison is done
        /// by lowercasing both strings and comparing
        /// the result. This is a temporary solution until
        /// a better one appears
        fn compare_no_case(&self, t: T) -> CompareResult;
    }
    fn lowercase_byte(c: u8) -> u8 {
        match c {
            b'A'..=b'Z' => c - b'A' + b'a',
            _ => c,
        }
    }
    impl<'a, 'b> Compare<&'b [u8]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8]) -> CompareResult {
            let pos = self.iter().zip(t.iter()).position(|(a, b)| a != b);
            match pos {
                Some(_) => CompareResult::Error,
                None => {
                    if self.len() >= t.len() {
                        CompareResult::Ok
                    } else {
                        CompareResult::Incomplete
                    }
                }
            }
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8]) -> CompareResult {
            if self.iter().zip(t).any(|(a, b)| lowercase_byte(*a) != lowercase_byte(*b))
            {
                CompareResult::Error
            } else if self.len() < t.len() {
                CompareResult::Incomplete
            } else {
                CompareResult::Ok
            }
        }
    }
    impl<
        T: InputLength + InputIter<Item = u8> + InputTake + UnspecializedInput,
        O: InputLength + InputIter<Item = u8> + InputTake,
    > Compare<O> for T {
        #[inline(always)]
        fn compare(&self, t: O) -> CompareResult {
            let pos = self
                .iter_elements()
                .zip(t.iter_elements())
                .position(|(a, b)| a != b);
            match pos {
                Some(_) => CompareResult::Error,
                None => {
                    if self.input_len() >= t.input_len() {
                        CompareResult::Ok
                    } else {
                        CompareResult::Incomplete
                    }
                }
            }
        }
        #[inline(always)]
        fn compare_no_case(&self, t: O) -> CompareResult {
            if self
                .iter_elements()
                .zip(t.iter_elements())
                .any(|(a, b)| lowercase_byte(a) != lowercase_byte(b))
            {
                CompareResult::Error
            } else if self.input_len() < t.input_len() {
                CompareResult::Incomplete
            } else {
                CompareResult::Ok
            }
        }
    }
    impl<'a, 'b> Compare<&'b str> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b str) -> CompareResult {
            self.compare(AsBytes::as_bytes(t))
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b str) -> CompareResult {
            self.compare_no_case(AsBytes::as_bytes(t))
        }
    }
    impl<'a, 'b> Compare<&'b str> for &'a str {
        #[inline(always)]
        fn compare(&self, t: &'b str) -> CompareResult {
            self.as_bytes().compare(t.as_bytes())
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b str) -> CompareResult {
            let pos = self
                .chars()
                .zip(t.chars())
                .position(|(a, b)| a.to_lowercase().ne(b.to_lowercase()));
            match pos {
                Some(_) => CompareResult::Error,
                None => {
                    if self.len() >= t.len() {
                        CompareResult::Ok
                    } else {
                        CompareResult::Incomplete
                    }
                }
            }
        }
    }
    impl<'a, 'b> Compare<&'b [u8]> for &'a str {
        #[inline(always)]
        fn compare(&self, t: &'b [u8]) -> CompareResult {
            AsBytes::as_bytes(self).compare(t)
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8]) -> CompareResult {
            AsBytes::as_bytes(self).compare_no_case(t)
        }
    }
    /// Look for a token in self
    pub trait FindToken<T> {
        /// Returns true if self contains the token
        fn find_token(&self, token: T) -> bool;
    }
    impl<'a> FindToken<u8> for &'a [u8] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, self).is_some()
        }
    }
    impl<'a> FindToken<u8> for &'a str {
        fn find_token(&self, token: u8) -> bool {
            self.as_bytes().find_token(token)
        }
    }
    impl<'a, 'b> FindToken<&'a u8> for &'b [u8] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl<'a, 'b> FindToken<&'a u8> for &'b str {
        fn find_token(&self, token: &u8) -> bool {
            self.as_bytes().find_token(token)
        }
    }
    impl<'a> FindToken<char> for &'a [u8] {
        fn find_token(&self, token: char) -> bool {
            self.iter().any(|i| *i == token as u8)
        }
    }
    impl<'a> FindToken<char> for &'a str {
        fn find_token(&self, token: char) -> bool {
            self.chars().any(|i| i == token)
        }
    }
    impl<'a> FindToken<char> for &'a [char] {
        fn find_token(&self, token: char) -> bool {
            self.iter().any(|i| *i == token)
        }
    }
    impl<'a, 'b> FindToken<&'a char> for &'b [char] {
        fn find_token(&self, token: &char) -> bool {
            self.find_token(*token)
        }
    }
    /// Look for a substring in self
    pub trait FindSubstring<T> {
        /// Returns the byte position of the substring if it is found
        fn find_substring(&self, substr: T) -> Option<usize>;
    }
    impl<'a, 'b> FindSubstring<&'b [u8]> for &'a [u8] {
        fn find_substring(&self, substr: &'b [u8]) -> Option<usize> {
            if substr.len() > self.len() {
                return None;
            }
            let (&substr_first, substr_rest) = match substr.split_first() {
                Some(split) => split,
                None => return Some(0),
            };
            if substr_rest.is_empty() {
                return memchr::memchr(substr_first, self);
            }
            let mut offset = 0;
            let haystack = &self[..self.len() - substr_rest.len()];
            while let Some(position) = memchr::memchr(
                substr_first,
                &haystack[offset..],
            ) {
                offset += position;
                let next_offset = offset + 1;
                if &self[next_offset..][..substr_rest.len()] == substr_rest {
                    return Some(offset);
                }
                offset = next_offset;
            }
            None
        }
    }
    impl<'a, 'b> FindSubstring<&'b str> for &'a [u8] {
        fn find_substring(&self, substr: &'b str) -> Option<usize> {
            self.find_substring(AsBytes::as_bytes(substr))
        }
    }
    impl<'a, 'b> FindSubstring<&'b str> for &'a str {
        fn find_substring(&self, substr: &'b str) -> Option<usize> {
            self.find(substr)
        }
    }
    /// Used to integrate `str`'s `parse()` method
    pub trait ParseTo<R> {
        /// Succeeds if `parse()` succeeded. The byte slice implementation
        /// will first convert it to a `&str`, then apply the `parse()` function
        fn parse_to(&self) -> Option<R>;
    }
    impl<'a, R: FromStr> ParseTo<R> for &'a [u8] {
        fn parse_to(&self) -> Option<R> {
            from_utf8(self).ok().and_then(|s| s.parse().ok())
        }
    }
    impl<'a, R: FromStr> ParseTo<R> for &'a str {
        fn parse_to(&self) -> Option<R> {
            self.parse().ok()
        }
    }
    /// Slicing operations using ranges.
    ///
    /// This trait is loosely based on
    /// `Index`, but can actually return
    /// something else than a `&[T]` or `&str`
    pub trait Slice<R> {
        /// Slices self according to the range argument
        fn slice(&self, range: R) -> Self;
    }
    impl<'a> Slice<Range<usize>> for &'a str {
        fn slice(&self, range: Range<usize>) -> Self {
            &self[range]
        }
    }
    impl<'a> Slice<RangeTo<usize>> for &'a str {
        fn slice(&self, range: RangeTo<usize>) -> Self {
            &self[range]
        }
    }
    impl<'a> Slice<RangeFrom<usize>> for &'a str {
        fn slice(&self, range: RangeFrom<usize>) -> Self {
            &self[range]
        }
    }
    impl<'a> Slice<RangeFull> for &'a str {
        fn slice(&self, range: RangeFull) -> Self {
            &self[range]
        }
    }
    impl<'a, T> Slice<Range<usize>> for &'a [T] {
        fn slice(&self, range: Range<usize>) -> Self {
            &self[range]
        }
    }
    impl<'a, T> Slice<RangeTo<usize>> for &'a [T] {
        fn slice(&self, range: RangeTo<usize>) -> Self {
            &self[range]
        }
    }
    impl<'a, T> Slice<RangeFrom<usize>> for &'a [T] {
        fn slice(&self, range: RangeFrom<usize>) -> Self {
            &self[range]
        }
    }
    impl<'a, T> Slice<RangeFull> for &'a [T] {
        fn slice(&self, range: RangeFull) -> Self {
            &self[range]
        }
    }
    impl InputLength for [u8; 0] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 0] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 0] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 0]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 0]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 0]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 0]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 0]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 0]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 0] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 0] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 1] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 1] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 1] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 1]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 1]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 1]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 1]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 1]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 1]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 1] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 1] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 2] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 2] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 2] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 2]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 2]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 2]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 2]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 2]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 2]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 2] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 2] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 3] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 3] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 3] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 3]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 3]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 3]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 3]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 3]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 3]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 3] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 3] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 4] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 4] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 4] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 4]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 4]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 4]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 4]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 4]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 4]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 4] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 4] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 5] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 5] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 5] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 5]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 5]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 5]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 5]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 5]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 5]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 5] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 5] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 6] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 6] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 6] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 6]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 6]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 6]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 6]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 6]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 6]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 6] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 6] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 7] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 7] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 7] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 7]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 7]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 7]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 7]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 7]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 7]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 7] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 7] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 8] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 8] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 8] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 8]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 8]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 8]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 8]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 8]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 8]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 8] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 8] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 9] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 9] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 9] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 9]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 9]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 9]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 9]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 9]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 9]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 9] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 9] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 10] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 10] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 10] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 10]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 10]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 10]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 10]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 10]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 10]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 10] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 10] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 11] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 11] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 11] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 11]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 11]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 11]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 11]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 11]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 11]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 11] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 11] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 12] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 12] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 12] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 12]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 12]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 12]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 12]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 12]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 12]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 12] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 12] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 13] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 13] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 13] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 13]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 13]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 13]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 13]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 13]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 13]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 13] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 13] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 14] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 14] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 14] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 14]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 14]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 14]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 14]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 14]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 14]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 14] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 14] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 15] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 15] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 15] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 15]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 15]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 15]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 15]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 15]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 15]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 15] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 15] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 16] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 16] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 16] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 16]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 16]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 16]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 16]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 16]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 16]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 16] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 16] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 17] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 17] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 17] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 17]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 17]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 17]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 17]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 17]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 17]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 17] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 17] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 18] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 18] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 18] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 18]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 18]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 18]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 18]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 18]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 18]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 18] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 18] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 19] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 19] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 19] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 19]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 19]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 19]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 19]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 19]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 19]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 19] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 19] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 20] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 20] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 20] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 20]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 20]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 20]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 20]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 20]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 20]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 20] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 20] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 21] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 21] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 21] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 21]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 21]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 21]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 21]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 21]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 21]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 21] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 21] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 22] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 22] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 22] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 22]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 22]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 22]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 22]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 22]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 22]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 22] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 22] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 23] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 23] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 23] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 23]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 23]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 23]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 23]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 23]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 23]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 23] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 23] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 24] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 24] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 24] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 24]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 24]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 24]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 24]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 24]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 24]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 24] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 24] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 25] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 25] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 25] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 25]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 25]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 25]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 25]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 25]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 25]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 25] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 25] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 26] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 26] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 26] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 26]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 26]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 26]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 26]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 26]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 26]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 26] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 26] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 27] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 27] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 27] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 27]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 27]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 27]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 27]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 27]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 27]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 27] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 27] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 28] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 28] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 28] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 28]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 28]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 28]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 28]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 28]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 28]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 28] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 28] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 29] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 29] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 29] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 29]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 29]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 29]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 29]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 29]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 29]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 29] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 29] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 30] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 30] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 30] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 30]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 30]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 30]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 30]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 30]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 30]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 30] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 30] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 31] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 31] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 31] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 31]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 31]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 31]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 31]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 31]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 31]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 31] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 31] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    impl InputLength for [u8; 32] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputLength for &'a [u8; 32] {
        #[inline]
        fn input_len(&self) -> usize {
            self.len()
        }
    }
    impl<'a> InputIter for &'a [u8; 32] {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Copied<Iter<'a, u8>>;
        fn iter_indices(&self) -> Self::Iter {
            (&self[..]).iter_indices()
        }
        fn iter_elements(&self) -> Self::IterElem {
            (&self[..]).iter_elements()
        }
        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            (&self[..]).position(predicate)
        }
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            (&self[..]).slice_index(count)
        }
    }
    impl<'a> Compare<[u8; 32]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: [u8; 32]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: [u8; 32]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl<'a, 'b> Compare<&'b [u8; 32]> for &'a [u8] {
        #[inline(always)]
        fn compare(&self, t: &'b [u8; 32]) -> CompareResult {
            self.compare(&t[..])
        }
        #[inline(always)]
        fn compare_no_case(&self, t: &'b [u8; 32]) -> CompareResult {
            self.compare_no_case(&t[..])
        }
    }
    impl FindToken<u8> for [u8; 32] {
        fn find_token(&self, token: u8) -> bool {
            memchr::memchr(token, &self[..]).is_some()
        }
    }
    impl<'a> FindToken<&'a u8> for [u8; 32] {
        fn find_token(&self, token: &u8) -> bool {
            self.find_token(*token)
        }
    }
    /// Abstracts something which can extend an `Extend`.
    /// Used to build modified input slices in `escaped_transform`
    pub trait ExtendInto {
        /// The current input type is a sequence of that `Item` type.
        ///
        /// Example: `u8` for `&[u8]` or `char` for `&str`
        type Item;
        /// The type that will be produced
        type Extender;
        /// Create a new `Extend` of the correct type
        fn new_builder(&self) -> Self::Extender;
        /// Accumulate the input into an accumulator
        fn extend_into(&self, acc: &mut Self::Extender);
    }
    #[cfg(feature = "alloc")]
    impl ExtendInto for [u8] {
        type Item = u8;
        type Extender = Vec<u8>;
        #[inline]
        fn new_builder(&self) -> Vec<u8> {
            Vec::new()
        }
        #[inline]
        fn extend_into(&self, acc: &mut Vec<u8>) {
            acc.extend(self.iter().cloned());
        }
    }
    #[cfg(feature = "alloc")]
    impl ExtendInto for &[u8] {
        type Item = u8;
        type Extender = Vec<u8>;
        #[inline]
        fn new_builder(&self) -> Vec<u8> {
            Vec::new()
        }
        #[inline]
        fn extend_into(&self, acc: &mut Vec<u8>) {
            acc.extend_from_slice(self);
        }
    }
    #[cfg(feature = "alloc")]
    impl ExtendInto for str {
        type Item = char;
        type Extender = String;
        #[inline]
        fn new_builder(&self) -> String {
            String::new()
        }
        #[inline]
        fn extend_into(&self, acc: &mut String) {
            acc.push_str(self);
        }
    }
    #[cfg(feature = "alloc")]
    impl ExtendInto for &str {
        type Item = char;
        type Extender = String;
        #[inline]
        fn new_builder(&self) -> String {
            String::new()
        }
        #[inline]
        fn extend_into(&self, acc: &mut String) {
            acc.push_str(self);
        }
    }
    #[cfg(feature = "alloc")]
    impl ExtendInto for char {
        type Item = char;
        type Extender = String;
        #[inline]
        fn new_builder(&self) -> String {
            String::new()
        }
        #[inline]
        fn extend_into(&self, acc: &mut String) {
            acc.push(*self);
        }
    }
    /// Helper trait to convert numbers to usize.
    ///
    /// By default, usize implements `From<u8>` and `From<u16>` but not
    /// `From<u32>` and `From<u64>` because that would be invalid on some
    /// platforms. This trait implements the conversion for platforms
    /// with 32 and 64 bits pointer platforms
    pub trait ToUsize {
        /// converts self to usize
        fn to_usize(&self) -> usize;
    }
    impl ToUsize for u8 {
        #[inline]
        fn to_usize(&self) -> usize {
            *self as usize
        }
    }
    impl ToUsize for u16 {
        #[inline]
        fn to_usize(&self) -> usize {
            *self as usize
        }
    }
    impl ToUsize for usize {
        #[inline]
        fn to_usize(&self) -> usize {
            *self
        }
    }
    #[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
    impl ToUsize for u32 {
        #[inline]
        fn to_usize(&self) -> usize {
            *self as usize
        }
    }
    #[cfg(target_pointer_width = "64")]
    impl ToUsize for u64 {
        #[inline]
        fn to_usize(&self) -> usize {
            *self as usize
        }
    }
    /// Equivalent From implementation to avoid orphan rules in bits parsers
    pub trait ErrorConvert<E> {
        /// Transform to another error type
        fn convert(self) -> E;
    }
    impl<I> ErrorConvert<(I, ErrorKind)> for ((I, usize), ErrorKind) {
        fn convert(self) -> (I, ErrorKind) {
            ((self.0).0, self.1)
        }
    }
    impl<I> ErrorConvert<((I, usize), ErrorKind)> for (I, ErrorKind) {
        fn convert(self) -> ((I, usize), ErrorKind) {
            ((self.0, 0), self.1)
        }
    }
    use crate::error;
    impl<I> ErrorConvert<error::Error<I>> for error::Error<(I, usize)> {
        fn convert(self) -> error::Error<I> {
            error::Error {
                input: self.input.0,
                code: self.code,
            }
        }
    }
    impl<I> ErrorConvert<error::Error<(I, usize)>> for error::Error<I> {
        fn convert(self) -> error::Error<(I, usize)> {
            error::Error {
                input: (self.input, 0),
                code: self.code,
            }
        }
    }
    #[cfg(feature = "alloc")]
    impl<I> ErrorConvert<error::VerboseError<I>> for error::VerboseError<(I, usize)> {
        fn convert(self) -> error::VerboseError<I> {
            error::VerboseError {
                errors: self.errors.into_iter().map(|(i, e)| (i.0, e)).collect(),
            }
        }
    }
    #[cfg(feature = "alloc")]
    impl<I> ErrorConvert<error::VerboseError<(I, usize)>> for error::VerboseError<I> {
        fn convert(self) -> error::VerboseError<(I, usize)> {
            error::VerboseError {
                errors: self.errors.into_iter().map(|(i, e)| ((i, 0), e)).collect(),
            }
        }
    }
    impl ErrorConvert<()> for () {
        fn convert(self) {}
    }
    #[cfg(feature = "std")]
    /// Helper trait to show a byte slice as a hex dump
    pub trait HexDisplay {
        /// Converts the value of `self` to a hex dump, returning the owned
        /// `String`.
        fn to_hex(&self, chunk_size: usize) -> String;
        /// Converts the value of `self` to a hex dump beginning at `from` address, returning the owned
        /// `String`.
        fn to_hex_from(&self, chunk_size: usize, from: usize) -> String;
    }
    #[cfg(feature = "std")]
    static CHARS: &[u8] = b"0123456789abcdef";
    #[cfg(feature = "std")]
    impl HexDisplay for [u8] {
        #[allow(unused_variables)]
        fn to_hex(&self, chunk_size: usize) -> String {
            self.to_hex_from(chunk_size, 0)
        }
        #[allow(unused_variables)]
        fn to_hex_from(&self, chunk_size: usize, from: usize) -> String {
            let mut v = Vec::with_capacity(self.len() * 3);
            let mut i = from;
            for chunk in self.chunks(chunk_size) {
                let s = {
                    let res = ::alloc::fmt::format(format_args!("{0:08x}", i));
                    res
                };
                for &ch in s.as_bytes().iter() {
                    v.push(ch);
                }
                v.push(b'\t');
                i += chunk_size;
                for &byte in chunk {
                    v.push(CHARS[(byte >> 4) as usize]);
                    v.push(CHARS[(byte & 0xf) as usize]);
                    v.push(b' ');
                }
                if chunk_size > chunk.len() {
                    for j in 0..(chunk_size - chunk.len()) {
                        v.push(b' ');
                        v.push(b' ');
                        v.push(b' ');
                    }
                }
                v.push(b'\t');
                for &byte in chunk {
                    if (byte >= 32 && byte <= 126) || byte >= 128 {
                        v.push(byte);
                    } else {
                        v.push(b'.');
                    }
                }
                v.push(b'\n');
            }
            String::from_utf8_lossy(&v[..]).into_owned()
        }
    }
    #[cfg(feature = "std")]
    impl HexDisplay for str {
        #[allow(unused_variables)]
        fn to_hex(&self, chunk_size: usize) -> String {
            self.to_hex_from(chunk_size, 0)
        }
        #[allow(unused_variables)]
        fn to_hex_from(&self, chunk_size: usize, from: usize) -> String {
            self.as_bytes().to_hex_from(chunk_size, from)
        }
    }
}
pub mod bits {
    //! Bit level parsers
    //!
    pub mod complete {
        //! Bit level parsers
        //!
        use crate::error::{ErrorKind, ParseError};
        use crate::internal::{Err, IResult};
        use crate::lib::std::ops::{AddAssign, Div, RangeFrom, Shl, Shr};
        use crate::traits::{InputIter, InputLength, Slice, ToUsize};
        /// Generates a parser taking `count` bits
        ///
        /// # Example
        /// ```rust
        /// # use nom::bits::complete::take;
        /// # use nom::IResult;
        /// # use nom::error::{Error, ErrorKind};
        /// // Input is a tuple of (input: I, bit_offset: usize)
        /// fn parser(input: (&[u8], usize), count: usize)-> IResult<(&[u8], usize), u8> {
        ///  take(count)(input)
        /// }
        ///
        /// // Consumes 0 bits, returns 0
        /// assert_eq!(parser(([0b00010010].as_ref(), 0), 0), Ok((([0b00010010].as_ref(), 0), 0)));
        ///
        /// // Consumes 4 bits, returns their values and increase offset to 4
        /// assert_eq!(parser(([0b00010010].as_ref(), 0), 4), Ok((([0b00010010].as_ref(), 4), 0b00000001)));
        ///
        /// // Consumes 4 bits, offset is 4, returns their values and increase offset to 0 of next byte
        /// assert_eq!(parser(([0b00010010].as_ref(), 4), 4), Ok((([].as_ref(), 0), 0b00000010)));
        ///
        /// // Tries to consume 12 bits but only 8 are available
        /// assert_eq!(parser(([0b00010010].as_ref(), 0), 12), Err(nom::Err::Error(Error{input: ([0b00010010].as_ref(), 0), code: ErrorKind::Eof })));
        /// ```
        pub fn take<I, O, C, E: ParseError<(I, usize)>>(
            count: C,
        ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
            C: ToUsize,
            O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O>,
        {
            let count = count.to_usize();
            move |(input, bit_offset): (I, usize)| {
                if count == 0 {
                    Ok(((input, bit_offset), 0u8.into()))
                } else {
                    let cnt = (count + bit_offset).div(8);
                    if input.input_len() * 8 < count + bit_offset {
                        Err(
                            Err::Error(
                                E::from_error_kind((input, bit_offset), ErrorKind::Eof),
                            ),
                        )
                    } else {
                        let mut acc: O = 0_u8.into();
                        let mut offset: usize = bit_offset;
                        let mut remaining: usize = count;
                        let mut end_offset: usize = 0;
                        for byte in input.iter_elements().take(cnt + 1) {
                            if remaining == 0 {
                                break;
                            }
                            let val: O = if offset == 0 {
                                byte.into()
                            } else {
                                ((byte << offset) as u8 >> offset).into()
                            };
                            if remaining < 8 - offset {
                                acc += val >> (8 - offset - remaining);
                                end_offset = remaining + offset;
                                break;
                            } else {
                                acc += val << (remaining - (8 - offset));
                                remaining -= 8 - offset;
                                offset = 0;
                            }
                        }
                        Ok(((input.slice(cnt..), end_offset), acc))
                    }
                }
            }
        }
        /// Generates a parser taking `count` bits and comparing them to `pattern`
        pub fn tag<I, O, C, E: ParseError<(I, usize)>>(
            pattern: O,
            count: C,
        ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength + Clone,
            C: ToUsize,
            O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O>
                + PartialEq,
        {
            let count = count.to_usize();
            move |input: (I, usize)| {
                let inp = input.clone();
                take(count)(input)
                    .and_then(|(i, o)| {
                        if pattern == o {
                            Ok((i, o))
                        } else {
                            Err(
                                Err::Error({
                                    crate::error::make_error(inp, ErrorKind::TagBits)
                                }),
                            )
                        }
                    })
            }
        }
        /// Parses one specific bit as a bool.
        ///
        /// # Example
        /// ```rust
        /// # use nom::bits::complete::bool;
        /// # use nom::IResult;
        /// # use nom::error::{Error, ErrorKind};
        ///
        /// fn parse(input: (&[u8], usize)) -> IResult<(&[u8], usize), bool> {
        ///     bool(input)
        /// }
        ///
        /// assert_eq!(parse(([0b10000000].as_ref(), 0)), Ok((([0b10000000].as_ref(), 1), true)));
        /// assert_eq!(parse(([0b10000000].as_ref(), 1)), Ok((([0b10000000].as_ref(), 2), false)));
        /// ```
        pub fn bool<I, E: ParseError<(I, usize)>>(
            input: (I, usize),
        ) -> IResult<(I, usize), bool, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let (res, bit): (_, u32) = take(1usize)(input)?;
            Ok((res, bit != 0))
        }
    }
    pub mod streaming {
        //! Bit level parsers
        //!
        use crate::error::{ErrorKind, ParseError};
        use crate::internal::{Err, IResult, Needed};
        use crate::lib::std::ops::{AddAssign, Div, RangeFrom, Shl, Shr};
        use crate::traits::{InputIter, InputLength, Slice, ToUsize};
        /// Generates a parser taking `count` bits
        pub fn take<I, O, C, E: ParseError<(I, usize)>>(
            count: C,
        ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
            C: ToUsize,
            O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O>,
        {
            let count = count.to_usize();
            move |(input, bit_offset): (I, usize)| {
                if count == 0 {
                    Ok(((input, bit_offset), 0u8.into()))
                } else {
                    let cnt = (count + bit_offset).div(8);
                    if input.input_len() * 8 < count + bit_offset {
                        Err(Err::Incomplete(Needed::new(count as usize)))
                    } else {
                        let mut acc: O = 0_u8.into();
                        let mut offset: usize = bit_offset;
                        let mut remaining: usize = count;
                        let mut end_offset: usize = 0;
                        for byte in input.iter_elements().take(cnt + 1) {
                            if remaining == 0 {
                                break;
                            }
                            let val: O = if offset == 0 {
                                byte.into()
                            } else {
                                ((byte << offset) as u8 >> offset).into()
                            };
                            if remaining < 8 - offset {
                                acc += val >> (8 - offset - remaining);
                                end_offset = remaining + offset;
                                break;
                            } else {
                                acc += val << (remaining - (8 - offset));
                                remaining -= 8 - offset;
                                offset = 0;
                            }
                        }
                        Ok(((input.slice(cnt..), end_offset), acc))
                    }
                }
            }
        }
        /// Generates a parser taking `count` bits and comparing them to `pattern`
        pub fn tag<I, O, C, E: ParseError<(I, usize)>>(
            pattern: O,
            count: C,
        ) -> impl Fn((I, usize)) -> IResult<(I, usize), O, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength + Clone,
            C: ToUsize,
            O: From<u8> + AddAssign + Shl<usize, Output = O> + Shr<usize, Output = O>
                + PartialEq,
        {
            let count = count.to_usize();
            move |input: (I, usize)| {
                let inp = input.clone();
                take(count)(input)
                    .and_then(|(i, o)| {
                        if pattern == o {
                            Ok((i, o))
                        } else {
                            Err(
                                Err::Error({
                                    crate::error::make_error(inp, ErrorKind::TagBits)
                                }),
                            )
                        }
                    })
            }
        }
        /// Parses one specific bit as a bool.
        ///
        /// # Example
        /// ```rust
        /// # use nom::bits::complete::bool;
        /// # use nom::IResult;
        /// # use nom::error::{Error, ErrorKind};
        ///
        /// fn parse(input: (&[u8], usize)) -> IResult<(&[u8], usize), bool> {
        ///     bool(input)
        /// }
        ///
        /// assert_eq!(parse(([0b10000000].as_ref(), 0)), Ok((([0b10000000].as_ref(), 1), true)));
        /// assert_eq!(parse(([0b10000000].as_ref(), 1)), Ok((([0b10000000].as_ref(), 2), false)));
        /// ```
        pub fn bool<I, E: ParseError<(I, usize)>>(
            input: (I, usize),
        ) -> IResult<(I, usize), bool, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let (res, bit): (_, u32) = take(1usize)(input)?;
            Ok((res, bit != 0))
        }
    }
    use crate::error::{ErrorKind, ParseError};
    use crate::internal::{Err, IResult, Needed, Parser};
    use crate::lib::std::ops::RangeFrom;
    use crate::traits::{ErrorConvert, Slice};
    /// Converts a byte-level input to a bit-level input, for consumption by a parser that uses bits.
    ///
    /// Afterwards, the input is converted back to a byte-level parser, with any remaining bits thrown
    /// away.
    ///
    /// # Example
    /// ```
    /// use nom::bits::{bits, streaming::take};
    /// use nom::error::Error;
    /// use nom::sequence::tuple;
    /// use nom::IResult;
    ///
    /// fn parse(input: &[u8]) -> IResult<&[u8], (u8, u8)> {
    ///     bits::<_, _, Error<(&[u8], usize)>, _, _>(tuple((take(4usize), take(8usize))))(input)
    /// }
    ///
    /// let input = &[0x12, 0x34, 0xff, 0xff];
    ///
    /// let output = parse(input).expect("We take 1.5 bytes and the input is longer than 2 bytes");
    ///
    /// // The first byte is consumed, the second byte is partially consumed and dropped.
    /// let remaining = output.0;
    /// assert_eq!(remaining, [0xff, 0xff]);
    ///
    /// let parsed = output.1;
    /// assert_eq!(parsed.0, 0x01);
    /// assert_eq!(parsed.1, 0x23);
    /// ```
    pub fn bits<I, O, E1, E2, P>(mut parser: P) -> impl FnMut(I) -> IResult<I, O, E2>
    where
        E1: ParseError<(I, usize)> + ErrorConvert<E2>,
        E2: ParseError<I>,
        I: Slice<RangeFrom<usize>>,
        P: Parser<(I, usize), O, E1>,
    {
        move |input: I| match parser.parse((input, 0)) {
            Ok(((rest, offset), result)) => {
                let remaining_bytes_index = offset / 8
                    + if offset % 8 == 0 { 0 } else { 1 };
                Ok((rest.slice(remaining_bytes_index..), result))
            }
            Err(Err::Incomplete(n)) => Err(Err::Incomplete(n.map(|u| u.get() / 8 + 1))),
            Err(Err::Error(e)) => Err(Err::Error(e.convert())),
            Err(Err::Failure(e)) => Err(Err::Failure(e.convert())),
        }
    }
    /// Counterpart to `bits`, `bytes` transforms its bit stream input into a byte slice for the underlying
    /// parser, allowing byte-slice parsers to work on bit streams.
    ///
    /// A partial byte remaining in the input will be ignored and the given parser will start parsing
    /// at the next full byte.
    ///
    /// ```
    /// use nom::bits::{bits, bytes, streaming::take};
    /// use nom::combinator::rest;
    /// use nom::error::Error;
    /// use nom::sequence::tuple;
    /// use nom::IResult;
    ///
    /// fn parse(input: &[u8]) -> IResult<&[u8], (u8, u8, &[u8])> {
    ///   bits::<_, _, Error<(&[u8], usize)>, _, _>(tuple((
    ///     take(4usize),
    ///     take(8usize),
    ///     bytes::<_, _, Error<&[u8]>, _, _>(rest)
    ///   )))(input)
    /// }
    ///
    /// let input = &[0x12, 0x34, 0xff, 0xff];
    ///
    /// assert_eq!(parse( input ), Ok(( &[][..], (0x01, 0x23, &[0xff, 0xff][..]) )));
    /// ```
    pub fn bytes<I, O, E1, E2, P>(
        mut parser: P,
    ) -> impl FnMut((I, usize)) -> IResult<(I, usize), O, E2>
    where
        E1: ParseError<I> + ErrorConvert<E2>,
        E2: ParseError<(I, usize)>,
        I: Slice<RangeFrom<usize>> + Clone,
        P: Parser<I, O, E1>,
    {
        move |(input, offset): (I, usize)| {
            let inner = if offset % 8 != 0 {
                input.slice((1 + offset / 8)..)
            } else {
                input.slice((offset / 8)..)
            };
            let i = (input, offset);
            match parser.parse(inner) {
                Ok((rest, res)) => Ok(((rest, 0), res)),
                Err(Err::Incomplete(Needed::Unknown)) => {
                    Err(Err::Incomplete(Needed::Unknown))
                }
                Err(Err::Incomplete(Needed::Size(sz))) => {
                    Err(
                        match sz.get().checked_mul(8) {
                            Some(v) => Err::Incomplete(Needed::new(v)),
                            None => {
                                Err::Failure(E2::from_error_kind(i, ErrorKind::TooLarge))
                            }
                        },
                    )
                }
                Err(Err::Error(e)) => Err(Err::Error(e.convert())),
                Err(Err::Failure(e)) => Err(Err::Failure(e.convert())),
            }
        }
    }
}
pub mod bytes {
    //! Parsers recognizing bytes streams
    pub mod complete {
        //! Parsers recognizing bytes streams, complete input version
        use crate::error::ErrorKind;
        use crate::error::ParseError;
        use crate::internal::{Err, IResult, Parser};
        use crate::lib::std::ops::RangeFrom;
        use crate::lib::std::result::Result::*;
        use crate::traits::{
            Compare, CompareResult, FindSubstring, FindToken, InputIter, InputLength,
            InputTake, InputTakeAtPosition, Slice, ToUsize,
        };
        /// Recognizes a pattern
        ///
        /// The input data will be compared to the tag combinator's argument and will return the part of
        /// the input that matches the argument
        ///
        /// It will return `Err(Err::Error((_, ErrorKind::Tag)))` if the input doesn't match the pattern
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::tag;
        ///
        /// fn parser(s: &str) -> IResult<&str, &str> {
        ///   tag("Hello")(s)
        /// }
        ///
        /// assert_eq!(parser("Hello, World!"), Ok((", World!", "Hello")));
        /// assert_eq!(parser("Something"), Err(Err::Error(Error::new("Something", ErrorKind::Tag))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
        /// ```
        pub fn tag<T, Input, Error: ParseError<Input>>(
            tag: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + Compare<T>,
            T: InputLength + Clone,
        {
            move |i: Input| {
                let tag_len = tag.input_len();
                let t = tag.clone();
                let res: IResult<_, _, Error> = match i.compare(t) {
                    CompareResult::Ok => Ok(i.take_split(tag_len)),
                    _ => {
                        let e: ErrorKind = ErrorKind::Tag;
                        Err(Err::Error(Error::from_error_kind(i, e)))
                    }
                };
                res
            }
        }
        /// Recognizes a case insensitive pattern.
        ///
        /// The input data will be compared to the tag combinator's argument and will return the part of
        /// the input that matches the argument with no regard to case.
        ///
        /// It will return `Err(Err::Error((_, ErrorKind::Tag)))` if the input doesn't match the pattern.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::tag_no_case;
        ///
        /// fn parser(s: &str) -> IResult<&str, &str> {
        ///   tag_no_case("hello")(s)
        /// }
        ///
        /// assert_eq!(parser("Hello, World!"), Ok((", World!", "Hello")));
        /// assert_eq!(parser("hello, World!"), Ok((", World!", "hello")));
        /// assert_eq!(parser("HeLlO, World!"), Ok((", World!", "HeLlO")));
        /// assert_eq!(parser("Something"), Err(Err::Error(Error::new("Something", ErrorKind::Tag))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
        /// ```
        pub fn tag_no_case<T, Input, Error: ParseError<Input>>(
            tag: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + Compare<T>,
            T: InputLength + Clone,
        {
            move |i: Input| {
                let tag_len = tag.input_len();
                let t = tag.clone();
                let res: IResult<_, _, Error> = match (i).compare_no_case(t) {
                    CompareResult::Ok => Ok(i.take_split(tag_len)),
                    _ => {
                        let e: ErrorKind = ErrorKind::Tag;
                        Err(Err::Error(Error::from_error_kind(i, e)))
                    }
                };
                res
            }
        }
        /// Parse till certain characters are met.
        ///
        /// The parser will return the longest slice till one of the characters of the combinator's argument are met.
        ///
        /// It doesn't consume the matched character.
        ///
        /// It will return a `Err::Error(("", ErrorKind::IsNot))` if the pattern wasn't met.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::is_not;
        ///
        /// fn not_space(s: &str) -> IResult<&str, &str> {
        ///   is_not(" \t\r\n")(s)
        /// }
        ///
        /// assert_eq!(not_space("Hello, World!"), Ok((" World!", "Hello,")));
        /// assert_eq!(not_space("Sometimes\t"), Ok(("\t", "Sometimes")));
        /// assert_eq!(not_space("Nospace"), Ok(("", "Nospace")));
        /// assert_eq!(not_space(""), Err(Err::Error(Error::new("", ErrorKind::IsNot))));
        /// ```
        pub fn is_not<T, Input, Error: ParseError<Input>>(
            arr: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            T: FindToken<<Input as InputTakeAtPosition>::Item>,
        {
            move |i: Input| {
                let e: ErrorKind = ErrorKind::IsNot;
                i.split_at_position1_complete(|c| arr.find_token(c), e)
            }
        }
        /// Returns the longest slice of the matches the pattern.
        ///
        /// The parser will return the longest slice consisting of the characters in provided in the
        /// combinator's argument.
        ///
        /// It will return a `Err(Err::Error((_, ErrorKind::IsA)))` if the pattern wasn't met.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::is_a;
        ///
        /// fn hex(s: &str) -> IResult<&str, &str> {
        ///   is_a("1234567890ABCDEF")(s)
        /// }
        ///
        /// assert_eq!(hex("123 and voila"), Ok((" and voila", "123")));
        /// assert_eq!(hex("DEADBEEF and others"), Ok((" and others", "DEADBEEF")));
        /// assert_eq!(hex("BADBABEsomething"), Ok(("something", "BADBABE")));
        /// assert_eq!(hex("D15EA5E"), Ok(("", "D15EA5E")));
        /// assert_eq!(hex(""), Err(Err::Error(Error::new("", ErrorKind::IsA))));
        /// ```
        pub fn is_a<T, Input, Error: ParseError<Input>>(
            arr: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            T: FindToken<<Input as InputTakeAtPosition>::Item>,
        {
            move |i: Input| {
                let e: ErrorKind = ErrorKind::IsA;
                i.split_at_position1_complete(|c| !arr.find_token(c), e)
            }
        }
        /// Returns the longest input slice (if any) that matches the predicate.
        ///
        /// The parser will return the longest slice that matches the given predicate *(a function that
        /// takes the input and returns a bool)*.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// use nom::bytes::complete::take_while;
        /// use nom::character::is_alphabetic;
        ///
        /// fn alpha(s: &[u8]) -> IResult<&[u8], &[u8]> {
        ///   take_while(is_alphabetic)(s)
        /// }
        ///
        /// assert_eq!(alpha(b"latin123"), Ok((&b"123"[..], &b"latin"[..])));
        /// assert_eq!(alpha(b"12345"), Ok((&b"12345"[..], &b""[..])));
        /// assert_eq!(alpha(b"latin"), Ok((&b""[..], &b"latin"[..])));
        /// assert_eq!(alpha(b""), Ok((&b""[..], &b""[..])));
        /// ```
        pub fn take_while<F, Input, Error: ParseError<Input>>(
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
        {
            move |i: Input| i.split_at_position_complete(|c| !cond(c))
        }
        /// Returns the longest (at least 1) input slice that matches the predicate.
        ///
        /// The parser will return the longest slice that matches the given predicate *(a function that
        /// takes the input and returns a bool)*.
        ///
        /// It will return an `Err(Err::Error((_, ErrorKind::TakeWhile1)))` if the pattern wasn't met.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::take_while1;
        /// use nom::character::is_alphabetic;
        ///
        /// fn alpha(s: &[u8]) -> IResult<&[u8], &[u8]> {
        ///   take_while1(is_alphabetic)(s)
        /// }
        ///
        /// assert_eq!(alpha(b"latin123"), Ok((&b"123"[..], &b"latin"[..])));
        /// assert_eq!(alpha(b"latin"), Ok((&b""[..], &b"latin"[..])));
        /// assert_eq!(alpha(b"12345"), Err(Err::Error(Error::new(&b"12345"[..], ErrorKind::TakeWhile1))));
        /// ```
        pub fn take_while1<F, Input, Error: ParseError<Input>>(
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
        {
            move |i: Input| {
                let e: ErrorKind = ErrorKind::TakeWhile1;
                i.split_at_position1_complete(|c| !cond(c), e)
            }
        }
        /// Returns the longest (m <= len <= n) input slice  that matches the predicate.
        ///
        /// The parser will return the longest slice that matches the given predicate *(a function that
        /// takes the input and returns a bool)*.
        ///
        /// It will return an `Err::Error((_, ErrorKind::TakeWhileMN))` if the pattern wasn't met or is out
        /// of range (m <= len <= n).
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::take_while_m_n;
        /// use nom::character::is_alphabetic;
        ///
        /// fn short_alpha(s: &[u8]) -> IResult<&[u8], &[u8]> {
        ///   take_while_m_n(3, 6, is_alphabetic)(s)
        /// }
        ///
        /// assert_eq!(short_alpha(b"latin123"), Ok((&b"123"[..], &b"latin"[..])));
        /// assert_eq!(short_alpha(b"lengthy"), Ok((&b"y"[..], &b"length"[..])));
        /// assert_eq!(short_alpha(b"latin"), Ok((&b""[..], &b"latin"[..])));
        /// assert_eq!(short_alpha(b"ed"), Err(Err::Error(Error::new(&b"ed"[..], ErrorKind::TakeWhileMN))));
        /// assert_eq!(short_alpha(b"12345"), Err(Err::Error(Error::new(&b"12345"[..], ErrorKind::TakeWhileMN))));
        /// ```
        pub fn take_while_m_n<F, Input, Error: ParseError<Input>>(
            m: usize,
            n: usize,
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + InputIter + InputLength + Slice<RangeFrom<usize>>,
            F: Fn(<Input as InputIter>::Item) -> bool,
        {
            move |i: Input| {
                let input = i;
                match input.position(|c| !cond(c)) {
                    Some(idx) => {
                        if idx >= m {
                            if idx <= n {
                                let res: IResult<_, _, Error> = if let Ok(index) = input
                                    .slice_index(idx)
                                {
                                    Ok(input.take_split(index))
                                } else {
                                    Err(
                                        Err::Error(
                                            Error::from_error_kind(input, ErrorKind::TakeWhileMN),
                                        ),
                                    )
                                };
                                res
                            } else {
                                let res: IResult<_, _, Error> = if let Ok(index) = input
                                    .slice_index(n)
                                {
                                    Ok(input.take_split(index))
                                } else {
                                    Err(
                                        Err::Error(
                                            Error::from_error_kind(input, ErrorKind::TakeWhileMN),
                                        ),
                                    )
                                };
                                res
                            }
                        } else {
                            let e = ErrorKind::TakeWhileMN;
                            Err(Err::Error(Error::from_error_kind(input, e)))
                        }
                    }
                    None => {
                        let len = input.input_len();
                        if len >= n {
                            match input.slice_index(n) {
                                Ok(index) => Ok(input.take_split(index)),
                                Err(_needed) => {
                                    Err(
                                        Err::Error(
                                            Error::from_error_kind(input, ErrorKind::TakeWhileMN),
                                        ),
                                    )
                                }
                            }
                        } else if len >= m && len <= n {
                            let res: IResult<_, _, Error> = Ok((
                                input.slice(len..),
                                input,
                            ));
                            res
                        } else {
                            let e = ErrorKind::TakeWhileMN;
                            Err(Err::Error(Error::from_error_kind(input, e)))
                        }
                    }
                }
            }
        }
        /// Returns the longest input slice (if any) till a predicate is met.
        ///
        /// The parser will return the longest slice till the given predicate *(a function that
        /// takes the input and returns a bool)*.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// use nom::bytes::complete::take_till;
        ///
        /// fn till_colon(s: &str) -> IResult<&str, &str> {
        ///   take_till(|c| c == ':')(s)
        /// }
        ///
        /// assert_eq!(till_colon("latin:123"), Ok((":123", "latin")));
        /// assert_eq!(till_colon(":empty matched"), Ok((":empty matched", ""))); //allowed
        /// assert_eq!(till_colon("12345"), Ok(("", "12345")));
        /// assert_eq!(till_colon(""), Ok(("", "")));
        /// ```
        pub fn take_till<F, Input, Error: ParseError<Input>>(
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
        {
            move |i: Input| i.split_at_position_complete(|c| cond(c))
        }
        /// Returns the longest (at least 1) input slice till a predicate is met.
        ///
        /// The parser will return the longest slice till the given predicate *(a function that
        /// takes the input and returns a bool)*.
        ///
        /// It will return `Err(Err::Error((_, ErrorKind::TakeTill1)))` if the input is empty or the
        /// predicate matches the first input.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::take_till1;
        ///
        /// fn till_colon(s: &str) -> IResult<&str, &str> {
        ///   take_till1(|c| c == ':')(s)
        /// }
        ///
        /// assert_eq!(till_colon("latin:123"), Ok((":123", "latin")));
        /// assert_eq!(till_colon(":empty matched"), Err(Err::Error(Error::new(":empty matched", ErrorKind::TakeTill1))));
        /// assert_eq!(till_colon("12345"), Ok(("", "12345")));
        /// assert_eq!(till_colon(""), Err(Err::Error(Error::new("", ErrorKind::TakeTill1))));
        /// ```
        pub fn take_till1<F, Input, Error: ParseError<Input>>(
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
        {
            move |i: Input| {
                let e: ErrorKind = ErrorKind::TakeTill1;
                i.split_at_position1_complete(|c| cond(c), e)
            }
        }
        /// Returns an input slice containing the first N input elements (Input[..N]).
        ///
        /// It will return `Err(Err::Error((_, ErrorKind::Eof)))` if the input is shorter than the argument.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::take;
        ///
        /// fn take6(s: &str) -> IResult<&str, &str> {
        ///   take(6usize)(s)
        /// }
        ///
        /// assert_eq!(take6("1234567"), Ok(("7", "123456")));
        /// assert_eq!(take6("things"), Ok(("", "things")));
        /// assert_eq!(take6("short"), Err(Err::Error(Error::new("short", ErrorKind::Eof))));
        /// assert_eq!(take6(""), Err(Err::Error(Error::new("", ErrorKind::Eof))));
        /// ```
        ///
        /// The units that are taken will depend on the input type. For example, for a
        /// `&str` it will take a number of `char`'s, whereas for a `&[u8]` it will
        /// take that many `u8`'s:
        ///
        /// ```rust
        /// use nom::error::Error;
        /// use nom::bytes::complete::take;
        ///
        /// assert_eq!(take::<_, _, Error<_>>(1usize)("💙"), Ok(("", "💙")));
        /// assert_eq!(take::<_, _, Error<_>>(1usize)("💙".as_bytes()), Ok((b"\x9F\x92\x99".as_ref(), b"\xF0".as_ref())));
        /// ```
        pub fn take<C, Input, Error: ParseError<Input>>(
            count: C,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputIter + InputTake,
            C: ToUsize,
        {
            let c = count.to_usize();
            move |i: Input| match i.slice_index(c) {
                Err(_needed) => {
                    Err(Err::Error(Error::from_error_kind(i, ErrorKind::Eof)))
                }
                Ok(index) => Ok(i.take_split(index)),
            }
        }
        /// Returns the input slice up to the first occurrence of the pattern.
        ///
        /// It doesn't consume the pattern. It will return `Err(Err::Error((_, ErrorKind::TakeUntil)))`
        /// if the pattern wasn't met.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::take_until;
        ///
        /// fn until_eof(s: &str) -> IResult<&str, &str> {
        ///   take_until("eof")(s)
        /// }
        ///
        /// assert_eq!(until_eof("hello, worldeof"), Ok(("eof", "hello, world")));
        /// assert_eq!(until_eof("hello, world"), Err(Err::Error(Error::new("hello, world", ErrorKind::TakeUntil))));
        /// assert_eq!(until_eof(""), Err(Err::Error(Error::new("", ErrorKind::TakeUntil))));
        /// assert_eq!(until_eof("1eof2eof"), Ok(("eof2eof", "1")));
        /// ```
        pub fn take_until<T, Input, Error: ParseError<Input>>(
            tag: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + FindSubstring<T>,
            T: InputLength + Clone,
        {
            move |i: Input| {
                let t = tag.clone();
                let res: IResult<_, _, Error> = match i.find_substring(t) {
                    None => {
                        Err(Err::Error(Error::from_error_kind(i, ErrorKind::TakeUntil)))
                    }
                    Some(index) => Ok(i.take_split(index)),
                };
                res
            }
        }
        /// Returns the non empty input slice up to the first occurrence of the pattern.
        ///
        /// It doesn't consume the pattern. It will return `Err(Err::Error((_, ErrorKind::TakeUntil)))`
        /// if the pattern wasn't met.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::complete::take_until1;
        ///
        /// fn until_eof(s: &str) -> IResult<&str, &str> {
        ///   take_until1("eof")(s)
        /// }
        ///
        /// assert_eq!(until_eof("hello, worldeof"), Ok(("eof", "hello, world")));
        /// assert_eq!(until_eof("hello, world"), Err(Err::Error(Error::new("hello, world", ErrorKind::TakeUntil))));
        /// assert_eq!(until_eof(""), Err(Err::Error(Error::new("", ErrorKind::TakeUntil))));
        /// assert_eq!(until_eof("1eof2eof"), Ok(("eof2eof", "1")));
        /// assert_eq!(until_eof("eof"), Err(Err::Error(Error::new("eof", ErrorKind::TakeUntil))));
        /// ```
        pub fn take_until1<T, Input, Error: ParseError<Input>>(
            tag: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + FindSubstring<T>,
            T: InputLength + Clone,
        {
            move |i: Input| {
                let t = tag.clone();
                let res: IResult<_, _, Error> = match i.find_substring(t) {
                    None => {
                        Err(Err::Error(Error::from_error_kind(i, ErrorKind::TakeUntil)))
                    }
                    Some(0) => {
                        Err(Err::Error(Error::from_error_kind(i, ErrorKind::TakeUntil)))
                    }
                    Some(index) => Ok(i.take_split(index)),
                };
                res
            }
        }
        /// Matches a byte string with escaped characters.
        ///
        /// * The first argument matches the normal characters (it must not accept the control character)
        /// * The second argument is the control character (like `\` in most languages)
        /// * The third argument matches the escaped characters
        /// # Example
        /// ```
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// # use nom::character::complete::digit1;
        /// use nom::bytes::complete::escaped;
        /// use nom::character::complete::one_of;
        ///
        /// fn esc(s: &str) -> IResult<&str, &str> {
        ///   escaped(digit1, '\\', one_of(r#""n\"#))(s)
        /// }
        ///
        /// assert_eq!(esc("123;"), Ok((";", "123")));
        /// assert_eq!(esc(r#"12\"34;"#), Ok((";", r#"12\"34"#)));
        /// ```
        ///
        pub fn escaped<'a, Input: 'a, Error, F, G, O1, O2>(
            mut normal: F,
            control_char: char,
            mut escapable: G,
        ) -> impl FnMut(Input) -> IResult<Input, Input, Error>
        where
            Input: Clone + crate::traits::Offset + InputLength + InputTake
                + InputTakeAtPosition + Slice<RangeFrom<usize>> + InputIter,
            <Input as InputIter>::Item: crate::traits::AsChar,
            F: Parser<Input, O1, Error>,
            G: Parser<Input, O2, Error>,
            Error: ParseError<Input>,
        {
            use crate::traits::AsChar;
            move |input: Input| {
                let mut i = input.clone();
                while i.input_len() > 0 {
                    let current_len = i.input_len();
                    match normal.parse(i.clone()) {
                        Ok((i2, _)) => {
                            if i2.input_len() == 0 {
                                return Ok((input.slice(input.input_len()..), input));
                            } else if i2.input_len() == current_len {
                                let index = input.offset(&i2);
                                return Ok(input.take_split(index));
                            } else {
                                i = i2;
                            }
                        }
                        Err(Err::Error(_)) => {
                            if i.iter_elements().next().unwrap().as_char()
                                == control_char
                            {
                                let next = control_char.len_utf8();
                                if next >= i.input_len() {
                                    return Err(
                                        Err::Error(
                                            Error::from_error_kind(input, ErrorKind::Escaped),
                                        ),
                                    );
                                } else {
                                    match escapable.parse(i.slice(next..)) {
                                        Ok((i2, _)) => {
                                            if i2.input_len() == 0 {
                                                return Ok((input.slice(input.input_len()..), input));
                                            } else {
                                                i = i2;
                                            }
                                        }
                                        Err(e) => return Err(e),
                                    }
                                }
                            } else {
                                let index = input.offset(&i);
                                if index == 0 {
                                    return Err(
                                        Err::Error(
                                            Error::from_error_kind(input, ErrorKind::Escaped),
                                        ),
                                    );
                                }
                                return Ok(input.take_split(index));
                            }
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok((input.slice(input.input_len()..), input))
            }
        }
        /// Matches a byte string with escaped characters.
        ///
        /// * The first argument matches the normal characters (it must not match the control character)
        /// * The second argument is the control character (like `\` in most languages)
        /// * The third argument matches the escaped characters and transforms them
        ///
        /// As an example, the chain `abc\tdef` could be `abc    def` (it also consumes the control character)
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// # use std::str::from_utf8;
        /// use nom::bytes::complete::{escaped_transform, tag};
        /// use nom::character::complete::alpha1;
        /// use nom::branch::alt;
        /// use nom::combinator::value;
        ///
        /// fn parser(input: &str) -> IResult<&str, String> {
        ///   escaped_transform(
        ///     alpha1,
        ///     '\\',
        ///     alt((
        ///       value("\\", tag("\\")),
        ///       value("\"", tag("\"")),
        ///       value("\n", tag("n")),
        ///     ))
        ///   )(input)
        /// }
        ///
        /// assert_eq!(parser("ab\\\"cd"), Ok(("", String::from("ab\"cd"))));
        /// assert_eq!(parser("ab\\ncd"), Ok(("", String::from("ab\ncd"))));
        /// ```
        #[cfg(feature = "alloc")]
        pub fn escaped_transform<Input, Error, F, G, O1, O2, ExtendItem, Output>(
            mut normal: F,
            control_char: char,
            mut transform: G,
        ) -> impl FnMut(Input) -> IResult<Input, Output, Error>
        where
            Input: Clone + crate::traits::Offset + InputLength + InputTake
                + InputTakeAtPosition + Slice<RangeFrom<usize>> + InputIter,
            Input: crate::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            O1: crate::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            O2: crate::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            <Input as InputIter>::Item: crate::traits::AsChar,
            F: Parser<Input, O1, Error>,
            G: Parser<Input, O2, Error>,
            Error: ParseError<Input>,
        {
            use crate::traits::AsChar;
            move |input: Input| {
                let mut index = 0;
                let mut res = input.new_builder();
                let i = input.clone();
                while index < i.input_len() {
                    let current_len = i.input_len();
                    let remainder = i.slice(index..);
                    match normal.parse(remainder.clone()) {
                        Ok((i2, o)) => {
                            o.extend_into(&mut res);
                            if i2.input_len() == 0 {
                                return Ok((i.slice(i.input_len()..), res));
                            } else if i2.input_len() == current_len {
                                return Ok((remainder, res));
                            } else {
                                index = input.offset(&i2);
                            }
                        }
                        Err(Err::Error(_)) => {
                            if remainder.iter_elements().next().unwrap().as_char()
                                == control_char
                            {
                                let next = index + control_char.len_utf8();
                                let input_len = input.input_len();
                                if next >= input_len {
                                    return Err(
                                        Err::Error(
                                            Error::from_error_kind(
                                                remainder,
                                                ErrorKind::EscapedTransform,
                                            ),
                                        ),
                                    );
                                } else {
                                    match transform.parse(i.slice(next..)) {
                                        Ok((i2, o)) => {
                                            o.extend_into(&mut res);
                                            if i2.input_len() == 0 {
                                                return Ok((i.slice(i.input_len()..), res));
                                            } else {
                                                index = input.offset(&i2);
                                            }
                                        }
                                        Err(e) => return Err(e),
                                    }
                                }
                            } else {
                                if index == 0 {
                                    return Err(
                                        Err::Error(
                                            Error::from_error_kind(
                                                remainder,
                                                ErrorKind::EscapedTransform,
                                            ),
                                        ),
                                    );
                                }
                                return Ok((remainder, res));
                            }
                        }
                        Err(e) => return Err(e),
                    }
                }
                Ok((input.slice(index..), res))
            }
        }
    }
    pub mod streaming {
        //! Parsers recognizing bytes streams, streaming version
        use crate::error::ErrorKind;
        use crate::error::ParseError;
        use crate::internal::{Err, IResult, Needed, Parser};
        use crate::lib::std::ops::RangeFrom;
        use crate::lib::std::result::Result::*;
        use crate::traits::{
            Compare, CompareResult, FindSubstring, FindToken, InputIter, InputLength,
            InputTake, InputTakeAtPosition, Slice, ToUsize,
        };
        /// Recognizes a pattern.
        ///
        /// The input data will be compared to the tag combinator's argument and will return the part of
        /// the input that matches the argument.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::streaming::tag;
        ///
        /// fn parser(s: &str) -> IResult<&str, &str> {
        ///   tag("Hello")(s)
        /// }
        ///
        /// assert_eq!(parser("Hello, World!"), Ok((", World!", "Hello")));
        /// assert_eq!(parser("Something"), Err(Err::Error(Error::new("Something", ErrorKind::Tag))));
        /// assert_eq!(parser("S"), Err(Err::Error(Error::new("S", ErrorKind::Tag))));
        /// assert_eq!(parser("H"), Err(Err::Incomplete(Needed::new(4))));
        /// ```
        pub fn tag<T, Input, Error: ParseError<Input>>(
            tag: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + InputLength + Compare<T>,
            T: InputLength + Clone,
        {
            move |i: Input| {
                let tag_len = tag.input_len();
                let t = tag.clone();
                let res: IResult<_, _, Error> = match i.compare(t) {
                    CompareResult::Ok => Ok(i.take_split(tag_len)),
                    CompareResult::Incomplete => {
                        Err(Err::Incomplete(Needed::new(tag_len - i.input_len())))
                    }
                    CompareResult::Error => {
                        let e: ErrorKind = ErrorKind::Tag;
                        Err(Err::Error(Error::from_error_kind(i, e)))
                    }
                };
                res
            }
        }
        /// Recognizes a case insensitive pattern.
        ///
        /// The input data will be compared to the tag combinator's argument and will return the part of
        /// the input that matches the argument with no regard to case.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::streaming::tag_no_case;
        ///
        /// fn parser(s: &str) -> IResult<&str, &str> {
        ///   tag_no_case("hello")(s)
        /// }
        ///
        /// assert_eq!(parser("Hello, World!"), Ok((", World!", "Hello")));
        /// assert_eq!(parser("hello, World!"), Ok((", World!", "hello")));
        /// assert_eq!(parser("HeLlO, World!"), Ok((", World!", "HeLlO")));
        /// assert_eq!(parser("Something"), Err(Err::Error(Error::new("Something", ErrorKind::Tag))));
        /// assert_eq!(parser(""), Err(Err::Incomplete(Needed::new(5))));
        /// ```
        pub fn tag_no_case<T, Input, Error: ParseError<Input>>(
            tag: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + InputLength + Compare<T>,
            T: InputLength + Clone,
        {
            move |i: Input| {
                let tag_len = tag.input_len();
                let t = tag.clone();
                let res: IResult<_, _, Error> = match (i).compare_no_case(t) {
                    CompareResult::Ok => Ok(i.take_split(tag_len)),
                    CompareResult::Incomplete => {
                        Err(Err::Incomplete(Needed::new(tag_len - i.input_len())))
                    }
                    CompareResult::Error => {
                        let e: ErrorKind = ErrorKind::Tag;
                        Err(Err::Error(Error::from_error_kind(i, e)))
                    }
                };
                res
            }
        }
        /// Parse till certain characters are met.
        ///
        /// The parser will return the longest slice till one of the characters of the combinator's argument are met.
        ///
        /// It doesn't consume the matched character.
        ///
        /// It will return a `Err::Incomplete(Needed::new(1))` if the pattern wasn't met.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// use nom::bytes::streaming::is_not;
        ///
        /// fn not_space(s: &str) -> IResult<&str, &str> {
        ///   is_not(" \t\r\n")(s)
        /// }
        ///
        /// assert_eq!(not_space("Hello, World!"), Ok((" World!", "Hello,")));
        /// assert_eq!(not_space("Sometimes\t"), Ok(("\t", "Sometimes")));
        /// assert_eq!(not_space("Nospace"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(not_space(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn is_not<T, Input, Error: ParseError<Input>>(
            arr: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            T: FindToken<<Input as InputTakeAtPosition>::Item>,
        {
            move |i: Input| {
                let e: ErrorKind = ErrorKind::IsNot;
                i.split_at_position1(|c| arr.find_token(c), e)
            }
        }
        /// Returns the longest slice of the matches the pattern.
        ///
        /// The parser will return the longest slice consisting of the characters in provided in the
        /// combinator's argument.
        ///
        /// # Streaming specific
        /// *Streaming version* will return a `Err::Incomplete(Needed::new(1))` if the pattern wasn't met
        /// or if the pattern reaches the end of the input.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// use nom::bytes::streaming::is_a;
        ///
        /// fn hex(s: &str) -> IResult<&str, &str> {
        ///   is_a("1234567890ABCDEF")(s)
        /// }
        ///
        /// assert_eq!(hex("123 and voila"), Ok((" and voila", "123")));
        /// assert_eq!(hex("DEADBEEF and others"), Ok((" and others", "DEADBEEF")));
        /// assert_eq!(hex("BADBABEsomething"), Ok(("something", "BADBABE")));
        /// assert_eq!(hex("D15EA5E"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(hex(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn is_a<T, Input, Error: ParseError<Input>>(
            arr: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            T: FindToken<<Input as InputTakeAtPosition>::Item>,
        {
            move |i: Input| {
                let e: ErrorKind = ErrorKind::IsA;
                i.split_at_position1(|c| !arr.find_token(c), e)
            }
        }
        /// Returns the longest input slice (if any) that matches the predicate.
        ///
        /// The parser will return the longest slice that matches the given predicate *(a function that
        /// takes the input and returns a bool)*.
        ///
        /// # Streaming Specific
        /// *Streaming version* will return a `Err::Incomplete(Needed::new(1))` if the pattern reaches the end of the input.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// use nom::bytes::streaming::take_while;
        /// use nom::character::is_alphabetic;
        ///
        /// fn alpha(s: &[u8]) -> IResult<&[u8], &[u8]> {
        ///   take_while(is_alphabetic)(s)
        /// }
        ///
        /// assert_eq!(alpha(b"latin123"), Ok((&b"123"[..], &b"latin"[..])));
        /// assert_eq!(alpha(b"12345"), Ok((&b"12345"[..], &b""[..])));
        /// assert_eq!(alpha(b"latin"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(alpha(b""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn take_while<F, Input, Error: ParseError<Input>>(
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
        {
            move |i: Input| i.split_at_position(|c| !cond(c))
        }
        /// Returns the longest (at least 1) input slice that matches the predicate.
        ///
        /// The parser will return the longest slice that matches the given predicate *(a function that
        /// takes the input and returns a bool)*.
        ///
        /// It will return an `Err(Err::Error((_, ErrorKind::TakeWhile1)))` if the pattern wasn't met.
        ///
        /// # Streaming Specific
        /// *Streaming version* will return a `Err::Incomplete(Needed::new(1))` or if the pattern reaches the end of the input.
        ///
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::streaming::take_while1;
        /// use nom::character::is_alphabetic;
        ///
        /// fn alpha(s: &[u8]) -> IResult<&[u8], &[u8]> {
        ///   take_while1(is_alphabetic)(s)
        /// }
        ///
        /// assert_eq!(alpha(b"latin123"), Ok((&b"123"[..], &b"latin"[..])));
        /// assert_eq!(alpha(b"latin"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(alpha(b"12345"), Err(Err::Error(Error::new(&b"12345"[..], ErrorKind::TakeWhile1))));
        /// ```
        pub fn take_while1<F, Input, Error: ParseError<Input>>(
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
        {
            move |i: Input| {
                let e: ErrorKind = ErrorKind::TakeWhile1;
                i.split_at_position1(|c| !cond(c), e)
            }
        }
        /// Returns the longest (m <= len <= n) input slice  that matches the predicate.
        ///
        /// The parser will return the longest slice that matches the given predicate *(a function that
        /// takes the input and returns a bool)*.
        ///
        /// It will return an `Err::Error((_, ErrorKind::TakeWhileMN))` if the pattern wasn't met.
        /// # Streaming Specific
        /// *Streaming version* will return a `Err::Incomplete(Needed::new(1))`  if the pattern reaches the end of the input or is too short.
        ///
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::streaming::take_while_m_n;
        /// use nom::character::is_alphabetic;
        ///
        /// fn short_alpha(s: &[u8]) -> IResult<&[u8], &[u8]> {
        ///   take_while_m_n(3, 6, is_alphabetic)(s)
        /// }
        ///
        /// assert_eq!(short_alpha(b"latin123"), Ok((&b"123"[..], &b"latin"[..])));
        /// assert_eq!(short_alpha(b"lengthy"), Ok((&b"y"[..], &b"length"[..])));
        /// assert_eq!(short_alpha(b"latin"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(short_alpha(b"ed"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(short_alpha(b"12345"), Err(Err::Error(Error::new(&b"12345"[..], ErrorKind::TakeWhileMN))));
        /// ```
        pub fn take_while_m_n<F, Input, Error: ParseError<Input>>(
            m: usize,
            n: usize,
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + InputIter + InputLength,
            F: Fn(<Input as InputIter>::Item) -> bool,
        {
            move |i: Input| {
                let input = i;
                match input.position(|c| !cond(c)) {
                    Some(idx) => {
                        if idx >= m {
                            if idx <= n {
                                let res: IResult<_, _, Error> = if let Ok(index) = input
                                    .slice_index(idx)
                                {
                                    Ok(input.take_split(index))
                                } else {
                                    Err(
                                        Err::Error(
                                            Error::from_error_kind(input, ErrorKind::TakeWhileMN),
                                        ),
                                    )
                                };
                                res
                            } else {
                                let res: IResult<_, _, Error> = if let Ok(index) = input
                                    .slice_index(n)
                                {
                                    Ok(input.take_split(index))
                                } else {
                                    Err(
                                        Err::Error(
                                            Error::from_error_kind(input, ErrorKind::TakeWhileMN),
                                        ),
                                    )
                                };
                                res
                            }
                        } else {
                            let e = ErrorKind::TakeWhileMN;
                            Err(Err::Error(Error::from_error_kind(input, e)))
                        }
                    }
                    None => {
                        let len = input.input_len();
                        if len >= n {
                            match input.slice_index(n) {
                                Ok(index) => Ok(input.take_split(index)),
                                Err(_needed) => {
                                    Err(
                                        Err::Error(
                                            Error::from_error_kind(input, ErrorKind::TakeWhileMN),
                                        ),
                                    )
                                }
                            }
                        } else {
                            let needed = if m > len { m - len } else { 1 };
                            Err(Err::Incomplete(Needed::new(needed)))
                        }
                    }
                }
            }
        }
        /// Returns the longest input slice (if any) till a predicate is met.
        ///
        /// The parser will return the longest slice till the given predicate *(a function that
        /// takes the input and returns a bool)*.
        ///
        /// # Streaming Specific
        /// *Streaming version* will return a `Err::Incomplete(Needed::new(1))` if the match reaches the
        /// end of input or if there was not match.
        ///
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// use nom::bytes::streaming::take_till;
        ///
        /// fn till_colon(s: &str) -> IResult<&str, &str> {
        ///   take_till(|c| c == ':')(s)
        /// }
        ///
        /// assert_eq!(till_colon("latin:123"), Ok((":123", "latin")));
        /// assert_eq!(till_colon(":empty matched"), Ok((":empty matched", ""))); //allowed
        /// assert_eq!(till_colon("12345"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(till_colon(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn take_till<F, Input, Error: ParseError<Input>>(
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
        {
            move |i: Input| i.split_at_position(|c| cond(c))
        }
        /// Returns the longest (at least 1) input slice till a predicate is met.
        ///
        /// The parser will return the longest slice till the given predicate *(a function that
        /// takes the input and returns a bool)*.
        ///
        /// # Streaming Specific
        /// *Streaming version* will return a `Err::Incomplete(Needed::new(1))` if the match reaches the
        /// end of input or if there was not match.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::streaming::take_till1;
        ///
        /// fn till_colon(s: &str) -> IResult<&str, &str> {
        ///   take_till1(|c| c == ':')(s)
        /// }
        ///
        /// assert_eq!(till_colon("latin:123"), Ok((":123", "latin")));
        /// assert_eq!(till_colon(":empty matched"), Err(Err::Error(Error::new(":empty matched", ErrorKind::TakeTill1))));
        /// assert_eq!(till_colon("12345"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(till_colon(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn take_till1<F, Input, Error: ParseError<Input>>(
            cond: F,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTakeAtPosition,
            F: Fn(<Input as InputTakeAtPosition>::Item) -> bool,
        {
            move |i: Input| {
                let e: ErrorKind = ErrorKind::TakeTill1;
                i.split_at_position1(|c| cond(c), e)
            }
        }
        /// Returns an input slice containing the first N input elements (Input[..N]).
        ///
        /// # Streaming Specific
        /// *Streaming version* if the input has less than N elements, `take` will
        /// return a `Err::Incomplete(Needed::new(M))` where M is the number of
        /// additional bytes the parser would need to succeed.
        /// It is well defined for `&[u8]` as the number of elements is the byte size,
        /// but for types like `&str`, we cannot know how many bytes correspond for
        /// the next few chars, so the result will be `Err::Incomplete(Needed::Unknown)`
        ///
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// use nom::bytes::streaming::take;
        ///
        /// fn take6(s: &str) -> IResult<&str, &str> {
        ///   take(6usize)(s)
        /// }
        ///
        /// assert_eq!(take6("1234567"), Ok(("7", "123456")));
        /// assert_eq!(take6("things"), Ok(("", "things")));
        /// assert_eq!(take6("short"), Err(Err::Incomplete(Needed::Unknown)));
        /// ```
        pub fn take<C, Input, Error: ParseError<Input>>(
            count: C,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputIter + InputTake + InputLength,
            C: ToUsize,
        {
            let c = count.to_usize();
            move |i: Input| match i.slice_index(c) {
                Err(i) => Err(Err::Incomplete(i)),
                Ok(index) => Ok(i.take_split(index)),
            }
        }
        /// Returns the input slice up to the first occurrence of the pattern.
        ///
        /// It doesn't consume the pattern.
        ///
        /// # Streaming Specific
        /// *Streaming version* will return a `Err::Incomplete(Needed::new(N))` if the input doesn't
        /// contain the pattern or if the input is smaller than the pattern.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// use nom::bytes::streaming::take_until;
        ///
        /// fn until_eof(s: &str) -> IResult<&str, &str> {
        ///   take_until("eof")(s)
        /// }
        ///
        /// assert_eq!(until_eof("hello, worldeof"), Ok(("eof", "hello, world")));
        /// assert_eq!(until_eof("hello, world"), Err(Err::Incomplete(Needed::Unknown)));
        /// assert_eq!(until_eof("hello, worldeo"), Err(Err::Incomplete(Needed::Unknown)));
        /// assert_eq!(until_eof("1eof2eof"), Ok(("eof2eof", "1")));
        /// ```
        pub fn take_until<T, Input, Error: ParseError<Input>>(
            tag: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + InputLength + FindSubstring<T>,
            T: Clone,
        {
            move |i: Input| {
                let t = tag.clone();
                let res: IResult<_, _, Error> = match i.find_substring(t) {
                    None => Err(Err::Incomplete(Needed::Unknown)),
                    Some(index) => Ok(i.take_split(index)),
                };
                res
            }
        }
        /// Returns the non empty input slice up to the first occurrence of the pattern.
        ///
        /// It doesn't consume the pattern.
        ///
        /// # Streaming Specific
        /// *Streaming version* will return a `Err::Incomplete(Needed::new(N))` if the input doesn't
        /// contain the pattern or if the input is smaller than the pattern.
        /// # Example
        /// ```rust
        /// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
        /// use nom::bytes::streaming::take_until1;
        ///
        /// fn until_eof(s: &str) -> IResult<&str, &str> {
        ///   take_until1("eof")(s)
        /// }
        ///
        /// assert_eq!(until_eof("hello, worldeof"), Ok(("eof", "hello, world")));
        /// assert_eq!(until_eof("hello, world"), Err(Err::Incomplete(Needed::Unknown)));
        /// assert_eq!(until_eof("hello, worldeo"), Err(Err::Incomplete(Needed::Unknown)));
        /// assert_eq!(until_eof("1eof2eof"), Ok(("eof2eof", "1")));
        /// assert_eq!(until_eof("eof"),  Err(Err::Error(Error::new("eof", ErrorKind::TakeUntil))));
        /// ```
        pub fn take_until1<T, Input, Error: ParseError<Input>>(
            tag: T,
        ) -> impl Fn(Input) -> IResult<Input, Input, Error>
        where
            Input: InputTake + InputLength + FindSubstring<T>,
            T: Clone,
        {
            move |i: Input| {
                let t = tag.clone();
                let res: IResult<_, _, Error> = match i.find_substring(t) {
                    None => Err(Err::Incomplete(Needed::Unknown)),
                    Some(0) => {
                        Err(Err::Error(Error::from_error_kind(i, ErrorKind::TakeUntil)))
                    }
                    Some(index) => Ok(i.take_split(index)),
                };
                res
            }
        }
        /// Matches a byte string with escaped characters.
        ///
        /// * The first argument matches the normal characters (it must not accept the control character)
        /// * The second argument is the control character (like `\` in most languages)
        /// * The third argument matches the escaped characters
        /// # Example
        /// ```
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// # use nom::character::complete::digit1;
        /// use nom::bytes::streaming::escaped;
        /// use nom::character::streaming::one_of;
        ///
        /// fn esc(s: &str) -> IResult<&str, &str> {
        ///   escaped(digit1, '\\', one_of("\"n\\"))(s)
        /// }
        ///
        /// assert_eq!(esc("123;"), Ok((";", "123")));
        /// assert_eq!(esc("12\\\"34;"), Ok((";", "12\\\"34")));
        /// ```
        ///
        pub fn escaped<Input, Error, F, G, O1, O2>(
            mut normal: F,
            control_char: char,
            mut escapable: G,
        ) -> impl FnMut(Input) -> IResult<Input, Input, Error>
        where
            Input: Clone + crate::traits::Offset + InputLength + InputTake
                + InputTakeAtPosition + Slice<RangeFrom<usize>> + InputIter,
            <Input as InputIter>::Item: crate::traits::AsChar,
            F: Parser<Input, O1, Error>,
            G: Parser<Input, O2, Error>,
            Error: ParseError<Input>,
        {
            use crate::traits::AsChar;
            move |input: Input| {
                let mut i = input.clone();
                while i.input_len() > 0 {
                    let current_len = i.input_len();
                    match normal.parse(i.clone()) {
                        Ok((i2, _)) => {
                            if i2.input_len() == 0 {
                                return Err(Err::Incomplete(Needed::Unknown));
                            } else if i2.input_len() == current_len {
                                let index = input.offset(&i2);
                                return Ok(input.take_split(index));
                            } else {
                                i = i2;
                            }
                        }
                        Err(Err::Error(_)) => {
                            if i.iter_elements().next().unwrap().as_char()
                                == control_char
                            {
                                let next = control_char.len_utf8();
                                if next >= i.input_len() {
                                    return Err(Err::Incomplete(Needed::new(1)));
                                } else {
                                    match escapable.parse(i.slice(next..)) {
                                        Ok((i2, _)) => {
                                            if i2.input_len() == 0 {
                                                return Err(Err::Incomplete(Needed::Unknown));
                                            } else {
                                                i = i2;
                                            }
                                        }
                                        Err(e) => return Err(e),
                                    }
                                }
                            } else {
                                let index = input.offset(&i);
                                return Ok(input.take_split(index));
                            }
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Err(Err::Incomplete(Needed::Unknown))
            }
        }
        /// Matches a byte string with escaped characters.
        ///
        /// * The first argument matches the normal characters (it must not match the control character)
        /// * The second argument is the control character (like `\` in most languages)
        /// * The third argument matches the escaped characters and transforms them
        ///
        /// As an example, the chain `abc\tdef` could be `abc    def` (it also consumes the control character)
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, Needed, IResult};
        /// # use std::str::from_utf8;
        /// use nom::bytes::streaming::{escaped_transform, tag};
        /// use nom::character::streaming::alpha1;
        /// use nom::branch::alt;
        /// use nom::combinator::value;
        ///
        /// fn parser(input: &str) -> IResult<&str, String> {
        ///   escaped_transform(
        ///     alpha1,
        ///     '\\',
        ///     alt((
        ///       value("\\", tag("\\")),
        ///       value("\"", tag("\"")),
        ///       value("\n", tag("n")),
        ///     ))
        ///   )(input)
        /// }
        ///
        /// assert_eq!(parser("ab\\\"cd\""), Ok(("\"", String::from("ab\"cd"))));
        /// ```
        #[cfg(feature = "alloc")]
        pub fn escaped_transform<Input, Error, F, G, O1, O2, ExtendItem, Output>(
            mut normal: F,
            control_char: char,
            mut transform: G,
        ) -> impl FnMut(Input) -> IResult<Input, Output, Error>
        where
            Input: Clone + crate::traits::Offset + InputLength + InputTake
                + InputTakeAtPosition + Slice<RangeFrom<usize>> + InputIter,
            Input: crate::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            O1: crate::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            O2: crate::traits::ExtendInto<Item = ExtendItem, Extender = Output>,
            <Input as InputIter>::Item: crate::traits::AsChar,
            F: Parser<Input, O1, Error>,
            G: Parser<Input, O2, Error>,
            Error: ParseError<Input>,
        {
            use crate::traits::AsChar;
            move |input: Input| {
                let mut index = 0;
                let mut res = input.new_builder();
                let i = input.clone();
                while index < i.input_len() {
                    let current_len = i.input_len();
                    let remainder = i.slice(index..);
                    match normal.parse(remainder.clone()) {
                        Ok((i2, o)) => {
                            o.extend_into(&mut res);
                            if i2.input_len() == 0 {
                                return Err(Err::Incomplete(Needed::Unknown));
                            } else if i2.input_len() == current_len {
                                return Ok((remainder, res));
                            } else {
                                index = input.offset(&i2);
                            }
                        }
                        Err(Err::Error(_)) => {
                            if remainder.iter_elements().next().unwrap().as_char()
                                == control_char
                            {
                                let next = index + control_char.len_utf8();
                                let input_len = input.input_len();
                                if next >= input_len {
                                    return Err(Err::Incomplete(Needed::Unknown));
                                } else {
                                    match transform.parse(i.slice(next..)) {
                                        Ok((i2, o)) => {
                                            o.extend_into(&mut res);
                                            if i2.input_len() == 0 {
                                                return Err(Err::Incomplete(Needed::Unknown));
                                            } else {
                                                index = input.offset(&i2);
                                            }
                                        }
                                        Err(e) => return Err(e),
                                    }
                                }
                            } else {
                                return Ok((remainder, res));
                            }
                        }
                        Err(e) => return Err(e),
                    }
                }
                Err(Err::Incomplete(Needed::Unknown))
            }
        }
    }
}
pub mod character {
    //! Character specific parsers and combinators
    //!
    //! Functions recognizing specific characters
    pub mod complete {
        //! Character specific parsers and combinators, complete input version.
        //!
        //! Functions recognizing specific characters.
        use crate::branch::alt;
        use crate::combinator::opt;
        use crate::error::ErrorKind;
        use crate::error::ParseError;
        use crate::internal::{Err, IResult};
        use crate::lib::std::ops::{Range, RangeFrom, RangeTo};
        use crate::traits::{
            AsChar, FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition,
            Slice,
        };
        use crate::traits::{Compare, CompareResult};
        /// Recognizes one character.
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{ErrorKind, Error}, IResult};
        /// # use nom::character::complete::char;
        /// fn parser(i: &str) -> IResult<&str, char> {
        ///     char('a')(i)
        /// }
        /// assert_eq!(parser("abc"), Ok(("bc", 'a')));
        /// assert_eq!(parser(" abc"), Err(Err::Error(Error::new(" abc", ErrorKind::Char))));
        /// assert_eq!(parser("bc"), Err(Err::Error(Error::new("bc", ErrorKind::Char))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Char))));
        /// ```
        pub fn char<I, Error: ParseError<I>>(
            c: char,
        ) -> impl Fn(I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar,
        {
            move |i: I| match (i)
                .iter_elements()
                .next()
                .map(|t| {
                    let b = t.as_char() == c;
                    (&c, b)
                })
            {
                Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                _ => Err(Err::Error(Error::from_char(i, c))),
            }
        }
        /// Recognizes one character and checks that it satisfies a predicate
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{ErrorKind, Error}, Needed, IResult};
        /// # use nom::character::complete::satisfy;
        /// fn parser(i: &str) -> IResult<&str, char> {
        ///     satisfy(|c| c == 'a' || c == 'b')(i)
        /// }
        /// assert_eq!(parser("abc"), Ok(("bc", 'a')));
        /// assert_eq!(parser("cd"), Err(Err::Error(Error::new("cd", ErrorKind::Satisfy))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Satisfy))));
        /// ```
        pub fn satisfy<F, I, Error: ParseError<I>>(
            cond: F,
        ) -> impl Fn(I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar,
            F: Fn(char) -> bool,
        {
            move |i: I| match (i)
                .iter_elements()
                .next()
                .map(|t| {
                    let c = t.as_char();
                    let b = cond(c);
                    (c, b)
                })
            {
                Some((c, true)) => Ok((i.slice(c.len()..), c)),
                _ => Err(Err::Error(Error::from_error_kind(i, ErrorKind::Satisfy))),
            }
        }
        /// Recognizes one of the provided characters.
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind};
        /// # use nom::character::complete::one_of;
        /// assert_eq!(one_of::<_, _, (&str, ErrorKind)>("abc")("b"), Ok(("", 'b')));
        /// assert_eq!(one_of::<_, _, (&str, ErrorKind)>("a")("bc"), Err(Err::Error(("bc", ErrorKind::OneOf))));
        /// assert_eq!(one_of::<_, _, (&str, ErrorKind)>("a")(""), Err(Err::Error(("", ErrorKind::OneOf))));
        /// ```
        pub fn one_of<I, T, Error: ParseError<I>>(
            list: T,
        ) -> impl Fn(I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar + Copy,
            T: FindToken<<I as InputIter>::Item>,
        {
            move |i: I| match (i).iter_elements().next().map(|c| (c, list.find_token(c)))
            {
                Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                _ => Err(Err::Error(Error::from_error_kind(i, ErrorKind::OneOf))),
            }
        }
        /// Recognizes a character that is not in the provided characters.
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind};
        /// # use nom::character::complete::none_of;
        /// assert_eq!(none_of::<_, _, (&str, ErrorKind)>("abc")("z"), Ok(("", 'z')));
        /// assert_eq!(none_of::<_, _, (&str, ErrorKind)>("ab")("a"), Err(Err::Error(("a", ErrorKind::NoneOf))));
        /// assert_eq!(none_of::<_, _, (&str, ErrorKind)>("a")(""), Err(Err::Error(("", ErrorKind::NoneOf))));
        /// ```
        pub fn none_of<I, T, Error: ParseError<I>>(
            list: T,
        ) -> impl Fn(I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar + Copy,
            T: FindToken<<I as InputIter>::Item>,
        {
            move |i: I| match (i)
                .iter_elements()
                .next()
                .map(|c| (c, !list.find_token(c)))
            {
                Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
                _ => Err(Err::Error(Error::from_error_kind(i, ErrorKind::NoneOf))),
            }
        }
        /// Recognizes the string "\r\n".
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult};
        /// # use nom::character::complete::crlf;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     crlf(input)
        /// }
        ///
        /// assert_eq!(parser("\r\nc"), Ok(("c", "\r\n")));
        /// assert_eq!(parser("ab\r\nc"), Err(Err::Error(Error::new("ab\r\nc", ErrorKind::CrLf))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::CrLf))));
        /// ```
        pub fn crlf<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: Slice<Range<usize>> + Slice<RangeFrom<usize>>,
            T: InputIter,
            T: Compare<&'static str>,
        {
            match input.compare("\r\n") {
                CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                _ => {
                    let e: ErrorKind = ErrorKind::CrLf;
                    Err(Err::Error(E::from_error_kind(input, e)))
                }
            }
        }
        /// Recognizes a string of any char except '\r\n' or '\n'.
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::not_line_ending;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     not_line_ending(input)
        /// }
        ///
        /// assert_eq!(parser("ab\r\nc"), Ok(("\r\nc", "ab")));
        /// assert_eq!(parser("ab\nc"), Ok(("\nc", "ab")));
        /// assert_eq!(parser("abc"), Ok(("", "abc")));
        /// assert_eq!(parser(""), Ok(("", "")));
        /// assert_eq!(parser("a\rb\nc"), Err(Err::Error(Error { input: "a\rb\nc", code: ErrorKind::Tag })));
        /// assert_eq!(parser("a\rbc"), Err(Err::Error(Error { input: "a\rbc", code: ErrorKind::Tag })));
        /// ```
        pub fn not_line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: InputIter + InputLength,
            T: Compare<&'static str>,
            <T as InputIter>::Item: AsChar,
            <T as InputIter>::Item: AsChar,
        {
            match input
                .position(|item| {
                    let c = item.as_char();
                    c == '\r' || c == '\n'
                })
            {
                None => Ok((input.slice(input.input_len()..), input)),
                Some(index) => {
                    let mut it = input.slice(index..).iter_elements();
                    let nth = it.next().unwrap().as_char();
                    if nth == '\r' {
                        let sliced = input.slice(index..);
                        let comp = sliced.compare("\r\n");
                        match comp {
                            CompareResult::Ok => {
                                Ok((input.slice(index..), input.slice(..index)))
                            }
                            _ => {
                                let e: ErrorKind = ErrorKind::Tag;
                                Err(Err::Error(E::from_error_kind(input, e)))
                            }
                        }
                    } else {
                        Ok((input.slice(index..), input.slice(..index)))
                    }
                }
            }
        }
        /// Recognizes an end of line (both '\n' and '\r\n').
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::line_ending;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     line_ending(input)
        /// }
        ///
        /// assert_eq!(parser("\r\nc"), Ok(("c", "\r\n")));
        /// assert_eq!(parser("ab\r\nc"), Err(Err::Error(Error::new("ab\r\nc", ErrorKind::CrLf))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::CrLf))));
        /// ```
        pub fn line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: InputIter + InputLength,
            T: Compare<&'static str>,
        {
            match input.compare("\n") {
                CompareResult::Ok => Ok((input.slice(1..), input.slice(0..1))),
                CompareResult::Incomplete => {
                    Err(Err::Error(E::from_error_kind(input, ErrorKind::CrLf)))
                }
                CompareResult::Error => {
                    match input.compare("\r\n") {
                        CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                        _ => Err(Err::Error(E::from_error_kind(input, ErrorKind::CrLf))),
                    }
                }
            }
        }
        /// Matches a newline character '\n'.
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::newline;
        /// fn parser(input: &str) -> IResult<&str, char> {
        ///     newline(input)
        /// }
        ///
        /// assert_eq!(parser("\nc"), Ok(("c", '\n')));
        /// assert_eq!(parser("\r\nc"), Err(Err::Error(Error::new("\r\nc", ErrorKind::Char))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Char))));
        /// ```
        pub fn newline<I, Error: ParseError<I>>(input: I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar,
        {
            char('\n')(input)
        }
        /// Matches a tab character '\t'.
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::tab;
        /// fn parser(input: &str) -> IResult<&str, char> {
        ///     tab(input)
        /// }
        ///
        /// assert_eq!(parser("\tc"), Ok(("c", '\t')));
        /// assert_eq!(parser("\r\nc"), Err(Err::Error(Error::new("\r\nc", ErrorKind::Char))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Char))));
        /// ```
        pub fn tab<I, Error: ParseError<I>>(input: I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar,
        {
            char('\t')(input)
        }
        /// Matches one byte as a character. Note that the input type will
        /// accept a `str`, but not a `&[u8]`, unlike many other nom parsers.
        ///
        /// *Complete version*: Will return an error if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{character::complete::anychar, Err, error::{Error, ErrorKind}, IResult};
        /// fn parser(input: &str) -> IResult<&str, char> {
        ///     anychar(input)
        /// }
        ///
        /// assert_eq!(parser("abc"), Ok(("bc",'a')));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Eof))));
        /// ```
        pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
        where
            T: InputIter + InputLength + Slice<RangeFrom<usize>>,
            <T as InputIter>::Item: AsChar,
        {
            let mut it = input.iter_indices();
            match it.next() {
                None => Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof))),
                Some((_, c)) => {
                    match it.next() {
                        None => Ok((input.slice(input.input_len()..), c.as_char())),
                        Some((idx, _)) => Ok((input.slice(idx..), c.as_char())),
                    }
                }
            }
        }
        /// Recognizes zero or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z
        ///
        /// *Complete version*: Will return the whole input if no terminating token is found (a non
        /// alphabetic character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::complete::alpha0;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     alpha0(input)
        /// }
        ///
        /// assert_eq!(parser("ab1c"), Ok(("1c", "ab")));
        /// assert_eq!(parser("1c"), Ok(("1c", "")));
        /// assert_eq!(parser(""), Ok(("", "")));
        /// ```
        pub fn alpha0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position_complete(|item| !item.is_alpha())
        }
        /// Recognizes one or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z
        ///
        /// *Complete version*: Will return an error if there's not enough input data,
        /// or the whole input if no terminating token is found  (a non alphabetic character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::alpha1;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     alpha1(input)
        /// }
        ///
        /// assert_eq!(parser("aB1c"), Ok(("1c", "aB")));
        /// assert_eq!(parser("1c"), Err(Err::Error(Error::new("1c", ErrorKind::Alpha))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Alpha))));
        /// ```
        pub fn alpha1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position1_complete(|item| !item.is_alpha(), ErrorKind::Alpha)
        }
        /// Recognizes zero or more ASCII numerical characters: 0-9
        ///
        /// *Complete version*: Will return an error if there's not enough input data,
        /// or the whole input if no terminating token is found (a non digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::complete::digit0;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     digit0(input)
        /// }
        ///
        /// assert_eq!(parser("21c"), Ok(("c", "21")));
        /// assert_eq!(parser("21"), Ok(("", "21")));
        /// assert_eq!(parser("a21c"), Ok(("a21c", "")));
        /// assert_eq!(parser(""), Ok(("", "")));
        /// ```
        pub fn digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position_complete(|item| !item.is_dec_digit())
        }
        /// Recognizes one or more ASCII numerical characters: 0-9
        ///
        /// *Complete version*: Will return an error if there's not enough input data,
        /// or the whole input if no terminating token is found (a non digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::digit1;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     digit1(input)
        /// }
        ///
        /// assert_eq!(parser("21c"), Ok(("c", "21")));
        /// assert_eq!(parser("c1"), Err(Err::Error(Error::new("c1", ErrorKind::Digit))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Digit))));
        /// ```
        ///
        /// ## Parsing an integer
        /// You can use `digit1` in combination with [`map_res`] to parse an integer:
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::combinator::map_res;
        /// # use nom::character::complete::digit1;
        /// fn parser(input: &str) -> IResult<&str, u32> {
        ///   map_res(digit1, str::parse)(input)
        /// }
        ///
        /// assert_eq!(parser("416"), Ok(("", 416)));
        /// assert_eq!(parser("12b"), Ok(("b", 12)));
        /// assert!(parser("b").is_err());
        /// ```
        ///
        /// [`map_res`]: crate::combinator::map_res
        pub fn digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input
                .split_at_position1_complete(
                    |item| !item.is_dec_digit(),
                    ErrorKind::Digit,
                )
        }
        /// Recognizes zero or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
        ///
        /// *Complete version*: Will return the whole input if no terminating token is found (a non hexadecimal digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::complete::hex_digit0;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     hex_digit0(input)
        /// }
        ///
        /// assert_eq!(parser("21cZ"), Ok(("Z", "21c")));
        /// assert_eq!(parser("Z21c"), Ok(("Z21c", "")));
        /// assert_eq!(parser(""), Ok(("", "")));
        /// ```
        pub fn hex_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position_complete(|item| !item.is_hex_digit())
        }
        /// Recognizes one or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
        ///
        /// *Complete version*: Will return an error if there's not enough input data,
        /// or the whole input if no terminating token is found (a non hexadecimal digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::hex_digit1;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     hex_digit1(input)
        /// }
        ///
        /// assert_eq!(parser("21cZ"), Ok(("Z", "21c")));
        /// assert_eq!(parser("H2"), Err(Err::Error(Error::new("H2", ErrorKind::HexDigit))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::HexDigit))));
        /// ```
        pub fn hex_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input
                .split_at_position1_complete(
                    |item| !item.is_hex_digit(),
                    ErrorKind::HexDigit,
                )
        }
        /// Recognizes zero or more octal characters: 0-7
        ///
        /// *Complete version*: Will return the whole input if no terminating token is found (a non octal
        /// digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::complete::oct_digit0;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     oct_digit0(input)
        /// }
        ///
        /// assert_eq!(parser("21cZ"), Ok(("cZ", "21")));
        /// assert_eq!(parser("Z21c"), Ok(("Z21c", "")));
        /// assert_eq!(parser(""), Ok(("", "")));
        /// ```
        pub fn oct_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position_complete(|item| !item.is_oct_digit())
        }
        /// Recognizes one or more octal characters: 0-7
        ///
        /// *Complete version*: Will return an error if there's not enough input data,
        /// or the whole input if no terminating token is found (a non octal digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::oct_digit1;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     oct_digit1(input)
        /// }
        ///
        /// assert_eq!(parser("21cZ"), Ok(("cZ", "21")));
        /// assert_eq!(parser("H2"), Err(Err::Error(Error::new("H2", ErrorKind::OctDigit))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::OctDigit))));
        /// ```
        pub fn oct_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input
                .split_at_position1_complete(
                    |item| !item.is_oct_digit(),
                    ErrorKind::OctDigit,
                )
        }
        /// Recognizes zero or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z
        ///
        /// *Complete version*: Will return the whole input if no terminating token is found (a non
        /// alphanumerical character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::complete::alphanumeric0;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     alphanumeric0(input)
        /// }
        ///
        /// assert_eq!(parser("21cZ%1"), Ok(("%1", "21cZ")));
        /// assert_eq!(parser("&Z21c"), Ok(("&Z21c", "")));
        /// assert_eq!(parser(""), Ok(("", "")));
        /// ```
        pub fn alphanumeric0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position_complete(|item| !item.is_alphanum())
        }
        /// Recognizes one or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z
        ///
        /// *Complete version*: Will return an error if there's not enough input data,
        /// or the whole input if no terminating token is found (a non alphanumerical character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::alphanumeric1;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     alphanumeric1(input)
        /// }
        ///
        /// assert_eq!(parser("21cZ%1"), Ok(("%1", "21cZ")));
        /// assert_eq!(parser("&H2"), Err(Err::Error(Error::new("&H2", ErrorKind::AlphaNumeric))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::AlphaNumeric))));
        /// ```
        pub fn alphanumeric1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input
                .split_at_position1_complete(
                    |item| !item.is_alphanum(),
                    ErrorKind::AlphaNumeric,
                )
        }
        /// Recognizes zero or more spaces and tabs.
        ///
        /// *Complete version*: Will return the whole input if no terminating token is found (a non space
        /// character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::complete::space0;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     space0(input)
        /// }
        ///
        /// assert_eq!(parser(" \t21c"), Ok(("21c", " \t")));
        /// assert_eq!(parser("Z21c"), Ok(("Z21c", "")));
        /// assert_eq!(parser(""), Ok(("", "")));
        /// ```
        pub fn space0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar + Clone,
        {
            input
                .split_at_position_complete(|item| {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t')
                })
        }
        /// Recognizes one or more spaces and tabs.
        ///
        /// *Complete version*: Will return an error if there's not enough input data,
        /// or the whole input if no terminating token is found (a non space character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::space1;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     space1(input)
        /// }
        ///
        /// assert_eq!(parser(" \t21c"), Ok(("21c", " \t")));
        /// assert_eq!(parser("H2"), Err(Err::Error(Error::new("H2", ErrorKind::Space))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Space))));
        /// ```
        pub fn space1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar + Clone,
        {
            input
                .split_at_position1_complete(
                    |item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                    },
                    ErrorKind::Space,
                )
        }
        /// Recognizes zero or more spaces, tabs, carriage returns and line feeds.
        ///
        /// *Complete version*: will return the whole input if no terminating token is found (a non space
        /// character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::complete::multispace0;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     multispace0(input)
        /// }
        ///
        /// assert_eq!(parser(" \t\n\r21c"), Ok(("21c", " \t\n\r")));
        /// assert_eq!(parser("Z21c"), Ok(("Z21c", "")));
        /// assert_eq!(parser(""), Ok(("", "")));
        /// ```
        pub fn multispace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar + Clone,
        {
            input
                .split_at_position_complete(|item| {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                })
        }
        /// Recognizes one or more spaces, tabs, carriage returns and line feeds.
        ///
        /// *Complete version*: will return an error if there's not enough input data,
        /// or the whole input if no terminating token is found (a non space character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::complete::multispace1;
        /// fn parser(input: &str) -> IResult<&str, &str> {
        ///     multispace1(input)
        /// }
        ///
        /// assert_eq!(parser(" \t\n\r21c"), Ok(("21c", " \t\n\r")));
        /// assert_eq!(parser("H2"), Err(Err::Error(Error::new("H2", ErrorKind::MultiSpace))));
        /// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::MultiSpace))));
        /// ```
        pub fn multispace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar + Clone,
        {
            input
                .split_at_position1_complete(
                    |item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                    },
                    ErrorKind::MultiSpace,
                )
        }
        pub(crate) fn sign<T, E: ParseError<T>>(input: T) -> IResult<T, bool, E>
        where
            T: Clone + InputTake,
            T: for<'a> Compare<&'a [u8]>,
        {
            use crate::bytes::complete::tag;
            use crate::combinator::value;
            let (i, opt_sign) = opt(
                alt((value(false, tag(&b"-"[..])), value(true, tag(&b"+"[..])))),
            )(input)?;
            let sign = opt_sign.unwrap_or(true);
            Ok((i, sign))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i8<T, E: ParseError<T>>(input: T) -> IResult<T, i8, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
            }
            let mut value: i8 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i8))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i8))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i16<T, E: ParseError<T>>(input: T) -> IResult<T, i16, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
            }
            let mut value: i16 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i16))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i16))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i32<T, E: ParseError<T>>(input: T) -> IResult<T, i32, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
            }
            let mut value: i32 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i32))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i32))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i64<T, E: ParseError<T>>(input: T) -> IResult<T, i64, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
            }
            let mut value: i64 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i64))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i64))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i128<T, E: ParseError<T>>(input: T) -> IResult<T, i128, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
            }
            let mut value: i128 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i128))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i128))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u8<T, E: ParseError<T>>(input: T) -> IResult<T, u8, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
            }
            let mut value: u8 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value.checked_mul(10).and_then(|v| v.checked_add(d as u8))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u16<T, E: ParseError<T>>(input: T) -> IResult<T, u16, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
            }
            let mut value: u16 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value.checked_mul(10).and_then(|v| v.checked_add(d as u16))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u32<T, E: ParseError<T>>(input: T) -> IResult<T, u32, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
            }
            let mut value: u32 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value.checked_mul(10).and_then(|v| v.checked_add(d as u32))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u64<T, E: ParseError<T>>(input: T) -> IResult<T, u64, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
            }
            let mut value: u64 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value.checked_mul(10).and_then(|v| v.checked_add(d as u64))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u128<T, E: ParseError<T>>(input: T) -> IResult<T, u128, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(i, ErrorKind::Digit)));
            }
            let mut value: u128 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value
                            .checked_mul(10)
                            .and_then(|v| v.checked_add(d as u128))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Ok((i.slice(i.input_len()..), value))
        }
    }
    pub mod streaming {
        //! Character specific parsers and combinators, streaming version
        //!
        //! Functions recognizing specific characters
        use crate::branch::alt;
        use crate::combinator::opt;
        use crate::error::ErrorKind;
        use crate::error::ParseError;
        use crate::internal::{Err, IResult, Needed};
        use crate::lib::std::ops::{Range, RangeFrom, RangeTo};
        use crate::traits::{
            AsChar, FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition,
            Slice,
        };
        use crate::traits::{Compare, CompareResult};
        /// Recognizes one character.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{ErrorKind, Error}, Needed, IResult};
        /// # use nom::character::streaming::char;
        /// fn parser(i: &str) -> IResult<&str, char> {
        ///     char('a')(i)
        /// }
        /// assert_eq!(parser("abc"), Ok(("bc", 'a')));
        /// assert_eq!(parser("bc"), Err(Err::Error(Error::new("bc", ErrorKind::Char))));
        /// assert_eq!(parser(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn char<I, Error: ParseError<I>>(
            c: char,
        ) -> impl Fn(I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter + InputLength,
            <I as InputIter>::Item: AsChar,
        {
            move |i: I| match (i)
                .iter_elements()
                .next()
                .map(|t| {
                    let b = t.as_char() == c;
                    (&c, b)
                })
            {
                None => Err(Err::Incomplete(Needed::new(c.len() - i.input_len()))),
                Some((_, false)) => Err(Err::Error(Error::from_char(i, c))),
                Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
            }
        }
        /// Recognizes one character and checks that it satisfies a predicate
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{ErrorKind, Error}, Needed, IResult};
        /// # use nom::character::streaming::satisfy;
        /// fn parser(i: &str) -> IResult<&str, char> {
        ///     satisfy(|c| c == 'a' || c == 'b')(i)
        /// }
        /// assert_eq!(parser("abc"), Ok(("bc", 'a')));
        /// assert_eq!(parser("cd"), Err(Err::Error(Error::new("cd", ErrorKind::Satisfy))));
        /// assert_eq!(parser(""), Err(Err::Incomplete(Needed::Unknown)));
        /// ```
        pub fn satisfy<F, I, Error: ParseError<I>>(
            cond: F,
        ) -> impl Fn(I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar,
            F: Fn(char) -> bool,
        {
            move |i: I| match (i)
                .iter_elements()
                .next()
                .map(|t| {
                    let c = t.as_char();
                    let b = cond(c);
                    (c, b)
                })
            {
                None => Err(Err::Incomplete(Needed::Unknown)),
                Some((_, false)) => {
                    Err(Err::Error(Error::from_error_kind(i, ErrorKind::Satisfy)))
                }
                Some((c, true)) => Ok((i.slice(c.len()..), c)),
            }
        }
        /// Recognizes one of the provided characters.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::character::streaming::one_of;
        /// assert_eq!(one_of::<_, _, (_, ErrorKind)>("abc")("b"), Ok(("", 'b')));
        /// assert_eq!(one_of::<_, _, (_, ErrorKind)>("a")("bc"), Err(Err::Error(("bc", ErrorKind::OneOf))));
        /// assert_eq!(one_of::<_, _, (_, ErrorKind)>("a")(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn one_of<I, T, Error: ParseError<I>>(
            list: T,
        ) -> impl Fn(I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar + Copy,
            T: FindToken<<I as InputIter>::Item>,
        {
            move |i: I| match (i).iter_elements().next().map(|c| (c, list.find_token(c)))
            {
                None => Err(Err::Incomplete(Needed::new(1))),
                Some((_, false)) => {
                    Err(Err::Error(Error::from_error_kind(i, ErrorKind::OneOf)))
                }
                Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
            }
        }
        /// Recognizes a character that is not in the provided characters.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::character::streaming::none_of;
        /// assert_eq!(none_of::<_, _, (_, ErrorKind)>("abc")("z"), Ok(("", 'z')));
        /// assert_eq!(none_of::<_, _, (_, ErrorKind)>("ab")("a"), Err(Err::Error(("a", ErrorKind::NoneOf))));
        /// assert_eq!(none_of::<_, _, (_, ErrorKind)>("a")(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn none_of<I, T, Error: ParseError<I>>(
            list: T,
        ) -> impl Fn(I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter,
            <I as InputIter>::Item: AsChar + Copy,
            T: FindToken<<I as InputIter>::Item>,
        {
            move |i: I| match (i)
                .iter_elements()
                .next()
                .map(|c| (c, !list.find_token(c)))
            {
                None => Err(Err::Incomplete(Needed::new(1))),
                Some((_, false)) => {
                    Err(Err::Error(Error::from_error_kind(i, ErrorKind::NoneOf)))
                }
                Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
            }
        }
        /// Recognizes the string "\r\n".
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::crlf;
        /// assert_eq!(crlf::<_, (_, ErrorKind)>("\r\nc"), Ok(("c", "\r\n")));
        /// assert_eq!(crlf::<_, (_, ErrorKind)>("ab\r\nc"), Err(Err::Error(("ab\r\nc", ErrorKind::CrLf))));
        /// assert_eq!(crlf::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(2))));
        /// ```
        pub fn crlf<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: InputIter,
            T: Compare<&'static str>,
        {
            match input.compare("\r\n") {
                CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(2))),
                CompareResult::Error => {
                    let e: ErrorKind = ErrorKind::CrLf;
                    Err(Err::Error(E::from_error_kind(input, e)))
                }
            }
        }
        /// Recognizes a string of any char except '\r\n' or '\n'.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
        /// # use nom::character::streaming::not_line_ending;
        /// assert_eq!(not_line_ending::<_, (_, ErrorKind)>("ab\r\nc"), Ok(("\r\nc", "ab")));
        /// assert_eq!(not_line_ending::<_, (_, ErrorKind)>("abc"), Err(Err::Incomplete(Needed::Unknown)));
        /// assert_eq!(not_line_ending::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::Unknown)));
        /// assert_eq!(not_line_ending::<_, (_, ErrorKind)>("a\rb\nc"), Err(Err::Error(("a\rb\nc", ErrorKind::Tag ))));
        /// assert_eq!(not_line_ending::<_, (_, ErrorKind)>("a\rbc"), Err(Err::Error(("a\rbc", ErrorKind::Tag ))));
        /// ```
        pub fn not_line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: InputIter + InputLength,
            T: Compare<&'static str>,
            <T as InputIter>::Item: AsChar,
            <T as InputIter>::Item: AsChar,
        {
            match input
                .position(|item| {
                    let c = item.as_char();
                    c == '\r' || c == '\n'
                })
            {
                None => Err(Err::Incomplete(Needed::Unknown)),
                Some(index) => {
                    let mut it = input.slice(index..).iter_elements();
                    let nth = it.next().unwrap().as_char();
                    if nth == '\r' {
                        let sliced = input.slice(index..);
                        let comp = sliced.compare("\r\n");
                        match comp {
                            CompareResult::Incomplete => {
                                Err(Err::Incomplete(Needed::Unknown))
                            }
                            CompareResult::Error => {
                                let e: ErrorKind = ErrorKind::Tag;
                                Err(Err::Error(E::from_error_kind(input, e)))
                            }
                            CompareResult::Ok => {
                                Ok((input.slice(index..), input.slice(..index)))
                            }
                        }
                    } else {
                        Ok((input.slice(index..), input.slice(..index)))
                    }
                }
            }
        }
        /// Recognizes an end of line (both '\n' and '\r\n').
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::line_ending;
        /// assert_eq!(line_ending::<_, (_, ErrorKind)>("\r\nc"), Ok(("c", "\r\n")));
        /// assert_eq!(line_ending::<_, (_, ErrorKind)>("ab\r\nc"), Err(Err::Error(("ab\r\nc", ErrorKind::CrLf))));
        /// assert_eq!(line_ending::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn line_ending<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: InputIter + InputLength,
            T: Compare<&'static str>,
        {
            match input.compare("\n") {
                CompareResult::Ok => Ok((input.slice(1..), input.slice(0..1))),
                CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(1))),
                CompareResult::Error => {
                    match input.compare("\r\n") {
                        CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                        CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(2))),
                        CompareResult::Error => {
                            Err(Err::Error(E::from_error_kind(input, ErrorKind::CrLf)))
                        }
                    }
                }
            }
        }
        /// Matches a newline character '\\n'.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::newline;
        /// assert_eq!(newline::<_, (_, ErrorKind)>("\nc"), Ok(("c", '\n')));
        /// assert_eq!(newline::<_, (_, ErrorKind)>("\r\nc"), Err(Err::Error(("\r\nc", ErrorKind::Char))));
        /// assert_eq!(newline::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn newline<I, Error: ParseError<I>>(input: I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter + InputLength,
            <I as InputIter>::Item: AsChar,
        {
            char('\n')(input)
        }
        /// Matches a tab character '\t'.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::tab;
        /// assert_eq!(tab::<_, (_, ErrorKind)>("\tc"), Ok(("c", '\t')));
        /// assert_eq!(tab::<_, (_, ErrorKind)>("\r\nc"), Err(Err::Error(("\r\nc", ErrorKind::Char))));
        /// assert_eq!(tab::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn tab<I, Error: ParseError<I>>(input: I) -> IResult<I, char, Error>
        where
            I: Slice<RangeFrom<usize>> + InputIter + InputLength,
            <I as InputIter>::Item: AsChar,
        {
            char('\t')(input)
        }
        /// Matches one byte as a character. Note that the input type will
        /// accept a `str`, but not a `&[u8]`, unlike many other nom parsers.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data.
        /// # Example
        ///
        /// ```
        /// # use nom::{character::streaming::anychar, Err, error::ErrorKind, IResult, Needed};
        /// assert_eq!(anychar::<_, (_, ErrorKind)>("abc"), Ok(("bc",'a')));
        /// assert_eq!(anychar::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn anychar<T, E: ParseError<T>>(input: T) -> IResult<T, char, E>
        where
            T: InputIter + InputLength + Slice<RangeFrom<usize>>,
            <T as InputIter>::Item: AsChar,
        {
            let mut it = input.iter_indices();
            match it.next() {
                None => Err(Err::Incomplete(Needed::new(1))),
                Some((_, c)) => {
                    match it.next() {
                        None => Ok((input.slice(input.input_len()..), c.as_char())),
                        Some((idx, _)) => Ok((input.slice(idx..), c.as_char())),
                    }
                }
            }
        }
        /// Recognizes zero or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non alphabetic character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::alpha0;
        /// assert_eq!(alpha0::<_, (_, ErrorKind)>("ab1c"), Ok(("1c", "ab")));
        /// assert_eq!(alpha0::<_, (_, ErrorKind)>("1c"), Ok(("1c", "")));
        /// assert_eq!(alpha0::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn alpha0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position(|item| !item.is_alpha())
        }
        /// Recognizes one or more lowercase and uppercase ASCII alphabetic characters: a-z, A-Z
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non alphabetic character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::alpha1;
        /// assert_eq!(alpha1::<_, (_, ErrorKind)>("aB1c"), Ok(("1c", "aB")));
        /// assert_eq!(alpha1::<_, (_, ErrorKind)>("1c"), Err(Err::Error(("1c", ErrorKind::Alpha))));
        /// assert_eq!(alpha1::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn alpha1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position1(|item| !item.is_alpha(), ErrorKind::Alpha)
        }
        /// Recognizes zero or more ASCII numerical characters: 0-9
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::digit0;
        /// assert_eq!(digit0::<_, (_, ErrorKind)>("21c"), Ok(("c", "21")));
        /// assert_eq!(digit0::<_, (_, ErrorKind)>("a21c"), Ok(("a21c", "")));
        /// assert_eq!(digit0::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position(|item| !item.is_dec_digit())
        }
        /// Recognizes one or more ASCII numerical characters: 0-9
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::digit1;
        /// assert_eq!(digit1::<_, (_, ErrorKind)>("21c"), Ok(("c", "21")));
        /// assert_eq!(digit1::<_, (_, ErrorKind)>("c1"), Err(Err::Error(("c1", ErrorKind::Digit))));
        /// assert_eq!(digit1::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position1(|item| !item.is_dec_digit(), ErrorKind::Digit)
        }
        /// Recognizes zero or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non hexadecimal digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::hex_digit0;
        /// assert_eq!(hex_digit0::<_, (_, ErrorKind)>("21cZ"), Ok(("Z", "21c")));
        /// assert_eq!(hex_digit0::<_, (_, ErrorKind)>("Z21c"), Ok(("Z21c", "")));
        /// assert_eq!(hex_digit0::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn hex_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position(|item| !item.is_hex_digit())
        }
        /// Recognizes one or more ASCII hexadecimal numerical characters: 0-9, A-F, a-f
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non hexadecimal digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::hex_digit1;
        /// assert_eq!(hex_digit1::<_, (_, ErrorKind)>("21cZ"), Ok(("Z", "21c")));
        /// assert_eq!(hex_digit1::<_, (_, ErrorKind)>("H2"), Err(Err::Error(("H2", ErrorKind::HexDigit))));
        /// assert_eq!(hex_digit1::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn hex_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position1(|item| !item.is_hex_digit(), ErrorKind::HexDigit)
        }
        /// Recognizes zero or more octal characters: 0-7
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non octal digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::oct_digit0;
        /// assert_eq!(oct_digit0::<_, (_, ErrorKind)>("21cZ"), Ok(("cZ", "21")));
        /// assert_eq!(oct_digit0::<_, (_, ErrorKind)>("Z21c"), Ok(("Z21c", "")));
        /// assert_eq!(oct_digit0::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn oct_digit0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position(|item| !item.is_oct_digit())
        }
        /// Recognizes one or more octal characters: 0-7
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non octal digit character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::oct_digit1;
        /// assert_eq!(oct_digit1::<_, (_, ErrorKind)>("21cZ"), Ok(("cZ", "21")));
        /// assert_eq!(oct_digit1::<_, (_, ErrorKind)>("H2"), Err(Err::Error(("H2", ErrorKind::OctDigit))));
        /// assert_eq!(oct_digit1::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn oct_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position1(|item| !item.is_oct_digit(), ErrorKind::OctDigit)
        }
        /// Recognizes zero or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non alphanumerical character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::alphanumeric0;
        /// assert_eq!(alphanumeric0::<_, (_, ErrorKind)>("21cZ%1"), Ok(("%1", "21cZ")));
        /// assert_eq!(alphanumeric0::<_, (_, ErrorKind)>("&Z21c"), Ok(("&Z21c", "")));
        /// assert_eq!(alphanumeric0::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn alphanumeric0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position(|item| !item.is_alphanum())
        }
        /// Recognizes one or more ASCII numerical and alphabetic characters: 0-9, a-z, A-Z
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non alphanumerical character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::alphanumeric1;
        /// assert_eq!(alphanumeric1::<_, (_, ErrorKind)>("21cZ%1"), Ok(("%1", "21cZ")));
        /// assert_eq!(alphanumeric1::<_, (_, ErrorKind)>("&H2"), Err(Err::Error(("&H2", ErrorKind::AlphaNumeric))));
        /// assert_eq!(alphanumeric1::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn alphanumeric1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            input.split_at_position1(|item| !item.is_alphanum(), ErrorKind::AlphaNumeric)
        }
        /// Recognizes zero or more spaces and tabs.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non space character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::space0;
        /// assert_eq!(space0::<_, (_, ErrorKind)>(" \t21c"), Ok(("21c", " \t")));
        /// assert_eq!(space0::<_, (_, ErrorKind)>("Z21c"), Ok(("Z21c", "")));
        /// assert_eq!(space0::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn space0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar + Clone,
        {
            input
                .split_at_position(|item| {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t')
                })
        }
        /// Recognizes one or more spaces and tabs.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non space character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::space1;
        /// assert_eq!(space1::<_, (_, ErrorKind)>(" \t21c"), Ok(("21c", " \t")));
        /// assert_eq!(space1::<_, (_, ErrorKind)>("H2"), Err(Err::Error(("H2", ErrorKind::Space))));
        /// assert_eq!(space1::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn space1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar + Clone,
        {
            input
                .split_at_position1(
                    |item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t')
                    },
                    ErrorKind::Space,
                )
        }
        /// Recognizes zero or more spaces, tabs, carriage returns and line feeds.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non space character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::multispace0;
        /// assert_eq!(multispace0::<_, (_, ErrorKind)>(" \t\n\r21c"), Ok(("21c", " \t\n\r")));
        /// assert_eq!(multispace0::<_, (_, ErrorKind)>("Z21c"), Ok(("Z21c", "")));
        /// assert_eq!(multispace0::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn multispace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar + Clone,
        {
            input
                .split_at_position(|item| {
                    let c = item.as_char();
                    !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                })
        }
        /// Recognizes one or more spaces, tabs, carriage returns and line feeds.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there's not enough input data,
        /// or if no terminating token is found (a non space character).
        /// # Example
        ///
        /// ```
        /// # use nom::{Err, error::ErrorKind, IResult, Needed};
        /// # use nom::character::streaming::multispace1;
        /// assert_eq!(multispace1::<_, (_, ErrorKind)>(" \t\n\r21c"), Ok(("21c", " \t\n\r")));
        /// assert_eq!(multispace1::<_, (_, ErrorKind)>("H2"), Err(Err::Error(("H2", ErrorKind::MultiSpace))));
        /// assert_eq!(multispace1::<_, (_, ErrorKind)>(""), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        pub fn multispace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar + Clone,
        {
            input
                .split_at_position1(
                    |item| {
                        let c = item.as_char();
                        !(c == ' ' || c == '\t' || c == '\r' || c == '\n')
                    },
                    ErrorKind::MultiSpace,
                )
        }
        pub(crate) fn sign<T, E: ParseError<T>>(input: T) -> IResult<T, bool, E>
        where
            T: Clone + InputTake + InputLength,
            T: for<'a> Compare<&'a [u8]>,
        {
            use crate::bytes::streaming::tag;
            use crate::combinator::value;
            let (i, opt_sign) = opt(
                alt((value(false, tag(&b"-"[..])), value(true, tag(&b"+"[..])))),
            )(input)?;
            let sign = opt_sign.unwrap_or(true);
            Ok((i, sign))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i8<T, E: ParseError<T>>(input: T) -> IResult<T, i8, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: i8 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i8))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i8))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i16<T, E: ParseError<T>>(input: T) -> IResult<T, i16, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: i16 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i16))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i16))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i32<T, E: ParseError<T>>(input: T) -> IResult<T, i32, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: i32 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i32))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i32))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i64<T, E: ParseError<T>>(input: T) -> IResult<T, i64, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: i64 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i64))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i64))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn i128<T, E: ParseError<T>>(input: T) -> IResult<T, i128, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength + InputTake + Clone,
            <T as InputIter>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, sign) = sign(input.clone())?;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: i128 = 0;
            if sign {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(d as i128))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            } else {
                for (pos, c) in i.iter_indices() {
                    match c.as_char().to_digit(10) {
                        None => {
                            if pos == 0 {
                                return Err(
                                    Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                );
                            } else {
                                return Ok((i.slice(pos..), value));
                            }
                        }
                        Some(d) => {
                            match value
                                .checked_mul(10)
                                .and_then(|v| v.checked_sub(d as i128))
                            {
                                None => {
                                    return Err(
                                        Err::Error(E::from_error_kind(input, ErrorKind::Digit)),
                                    );
                                }
                                Some(v) => value = v,
                            }
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u8<T, E: ParseError<T>>(input: T) -> IResult<T, u8, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: u8 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value.checked_mul(10).and_then(|v| v.checked_add(d as u8))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u16<T, E: ParseError<T>>(input: T) -> IResult<T, u16, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: u16 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value.checked_mul(10).and_then(|v| v.checked_add(d as u16))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u32<T, E: ParseError<T>>(input: T) -> IResult<T, u32, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: u32 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value.checked_mul(10).and_then(|v| v.checked_add(d as u32))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u64<T, E: ParseError<T>>(input: T) -> IResult<T, u64, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: u64 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value.checked_mul(10).and_then(|v| v.checked_add(d as u64))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
        /// will parse a number in text form to a number
        ///
        /// *Complete version*: can parse until the end of input.
        pub fn u128<T, E: ParseError<T>>(input: T) -> IResult<T, u128, E>
        where
            T: InputIter + Slice<RangeFrom<usize>> + InputLength,
            <T as InputIter>::Item: AsChar,
        {
            let i = input;
            if i.input_len() == 0 {
                return Err(Err::Incomplete(Needed::new(1)));
            }
            let mut value: u128 = 0;
            for (pos, c) in i.iter_indices() {
                match c.as_char().to_digit(10) {
                    None => {
                        if pos == 0 {
                            return Err(
                                Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                            );
                        } else {
                            return Ok((i.slice(pos..), value));
                        }
                    }
                    Some(d) => {
                        match value
                            .checked_mul(10)
                            .and_then(|v| v.checked_add(d as u128))
                        {
                            None => {
                                return Err(
                                    Err::Error(E::from_error_kind(i, ErrorKind::Digit)),
                                );
                            }
                            Some(v) => value = v,
                        }
                    }
                }
            }
            Err(Err::Incomplete(Needed::new(1)))
        }
    }
    /// Tests if byte is ASCII alphabetic: A-Z, a-z
    ///
    /// # Example
    ///
    /// ```
    /// # use nom::character::is_alphabetic;
    /// assert_eq!(is_alphabetic(b'9'), false);
    /// assert_eq!(is_alphabetic(b'a'), true);
    /// ```
    #[inline]
    pub fn is_alphabetic(chr: u8) -> bool {
        (chr >= 0x41 && chr <= 0x5A) || (chr >= 0x61 && chr <= 0x7A)
    }
    /// Tests if byte is ASCII digit: 0-9
    ///
    /// # Example
    ///
    /// ```
    /// # use nom::character::is_digit;
    /// assert_eq!(is_digit(b'a'), false);
    /// assert_eq!(is_digit(b'9'), true);
    /// ```
    #[inline]
    pub fn is_digit(chr: u8) -> bool {
        chr >= 0x30 && chr <= 0x39
    }
    /// Tests if byte is ASCII hex digit: 0-9, A-F, a-f
    ///
    /// # Example
    ///
    /// ```
    /// # use nom::character::is_hex_digit;
    /// assert_eq!(is_hex_digit(b'a'), true);
    /// assert_eq!(is_hex_digit(b'9'), true);
    /// assert_eq!(is_hex_digit(b'A'), true);
    /// assert_eq!(is_hex_digit(b'x'), false);
    /// ```
    #[inline]
    pub fn is_hex_digit(chr: u8) -> bool {
        (chr >= 0x30 && chr <= 0x39) || (chr >= 0x41 && chr <= 0x46)
            || (chr >= 0x61 && chr <= 0x66)
    }
    /// Tests if byte is ASCII octal digit: 0-7
    ///
    /// # Example
    ///
    /// ```
    /// # use nom::character::is_oct_digit;
    /// assert_eq!(is_oct_digit(b'a'), false);
    /// assert_eq!(is_oct_digit(b'9'), false);
    /// assert_eq!(is_oct_digit(b'6'), true);
    /// ```
    #[inline]
    pub fn is_oct_digit(chr: u8) -> bool {
        chr >= 0x30 && chr <= 0x37
    }
    /// Tests if byte is ASCII alphanumeric: A-Z, a-z, 0-9
    ///
    /// # Example
    ///
    /// ```
    /// # use nom::character::is_alphanumeric;
    /// assert_eq!(is_alphanumeric(b'-'), false);
    /// assert_eq!(is_alphanumeric(b'a'), true);
    /// assert_eq!(is_alphanumeric(b'9'), true);
    /// assert_eq!(is_alphanumeric(b'A'), true);
    /// ```
    #[inline]
    pub fn is_alphanumeric(chr: u8) -> bool {
        is_alphabetic(chr) || is_digit(chr)
    }
    /// Tests if byte is ASCII space or tab
    ///
    /// # Example
    ///
    /// ```
    /// # use nom::character::is_space;
    /// assert_eq!(is_space(b'\n'), false);
    /// assert_eq!(is_space(b'\r'), false);
    /// assert_eq!(is_space(b' '), true);
    /// assert_eq!(is_space(b'\t'), true);
    /// ```
    #[inline]
    pub fn is_space(chr: u8) -> bool {
        chr == b' ' || chr == b'\t'
    }
    /// Tests if byte is ASCII newline: \n
    ///
    /// # Example
    ///
    /// ```
    /// # use nom::character::is_newline;
    /// assert_eq!(is_newline(b'\n'), true);
    /// assert_eq!(is_newline(b'\r'), false);
    /// assert_eq!(is_newline(b' '), false);
    /// assert_eq!(is_newline(b'\t'), false);
    /// ```
    #[inline]
    pub fn is_newline(chr: u8) -> bool {
        chr == b'\n'
    }
}
mod str {}
pub mod number {
    //! Parsers recognizing numbers
    pub mod complete {
        //! Parsers recognizing numbers, complete input version
        use crate::branch::alt;
        use crate::bytes::complete::tag;
        use crate::character::complete::{char, digit1, sign};
        use crate::combinator::{cut, map, opt, recognize};
        use crate::error::ParseError;
        use crate::error::{make_error, ErrorKind};
        use crate::internal::*;
        use crate::lib::std::ops::{Range, RangeFrom, RangeTo};
        use crate::sequence::{pair, tuple};
        use crate::traits::{
            AsBytes, AsChar, Compare, InputIter, InputLength, InputTake,
            InputTakeAtPosition, Offset, Slice,
        };
        /// Recognizes an unsigned 1 byte integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_u8;
        ///
        /// let parser = |s| {
        ///   be_u8(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"\x03abcefg"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Error((&[][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 1;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let res = input.iter_elements().next().unwrap();
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 2 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_u16;
        ///
        /// let parser = |s| {
        ///   be_u16(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0003)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_u16<I, E: ParseError<I>>(input: I) -> IResult<I, u16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 2;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u16;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u16;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 3 byte integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_u24;
        ///
        /// let parser = |s| {
        ///   be_u24(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x000305)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_u24<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 3;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u32;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u32;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 4 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_u32;
        ///
        /// let parser = |s| {
        ///   be_u32(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00030507)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_u32<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 4;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u32;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u32;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 8 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_u64;
        ///
        /// let parser = |s| {
        ///   be_u64(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0001020304050607)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_u64<I, E: ParseError<I>>(input: I) -> IResult<I, u64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 8;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u64;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u64;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 16 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_u128;
        ///
        /// let parser = |s| {
        ///   be_u128(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00010203040506070001020304050607)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_u128<I, E: ParseError<I>>(input: I) -> IResult<I, u128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 16;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u128;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u128;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a signed 1 byte integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_i8;
        ///
        /// let parser = |s| {
        ///   be_i8(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"\x03abcefg"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Error((&[][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_i8<I, E: ParseError<I>>(input: I) -> IResult<I, i8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u8.map(|x| x as i8).parse(input)
        }
        /// Recognizes a big endian signed 2 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_i16;
        ///
        /// let parser = |s| {
        ///   be_i16(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0003)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_i16<I, E: ParseError<I>>(input: I) -> IResult<I, i16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u16.map(|x| x as i16).parse(input)
        }
        /// Recognizes a big endian signed 3 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_i24;
        ///
        /// let parser = |s| {
        ///   be_i24(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x000305)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_i24<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u24
                .map(|x| {
                    if x & 0x80_00_00 != 0 {
                        (x | 0xff_00_00_00) as i32
                    } else {
                        x as i32
                    }
                })
                .parse(input)
        }
        /// Recognizes a big endian signed 4 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_i32;
        ///
        /// let parser = |s| {
        ///   be_i32(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00030507)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_i32<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u32.map(|x| x as i32).parse(input)
        }
        /// Recognizes a big endian signed 8 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_i64;
        ///
        /// let parser = |s| {
        ///   be_i64(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0001020304050607)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_i64<I, E: ParseError<I>>(input: I) -> IResult<I, i64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u64.map(|x| x as i64).parse(input)
        }
        /// Recognizes a big endian signed 16 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_i128;
        ///
        /// let parser = |s| {
        ///   be_i128(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00010203040506070001020304050607)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_i128<I, E: ParseError<I>>(input: I) -> IResult<I, i128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u128.map(|x| x as i128).parse(input)
        }
        /// Recognizes an unsigned 1 byte integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_u8;
        ///
        /// let parser = |s| {
        ///   le_u8(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"\x03abcefg"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Error((&[][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 1;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let res = input.iter_elements().next().unwrap();
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 2 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_u16;
        ///
        /// let parser = |s| {
        ///   le_u16(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0300)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_u16<I, E: ParseError<I>>(input: I) -> IResult<I, u16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 2;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u16;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u16) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 3 byte integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_u24;
        ///
        /// let parser = |s| {
        ///   le_u24(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x050300)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_u24<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 3;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u32;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u32) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 4 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_u32;
        ///
        /// let parser = |s| {
        ///   le_u32(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07050300)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_u32<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 4;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u32;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u32) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 8 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_u64;
        ///
        /// let parser = |s| {
        ///   le_u64(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0706050403020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_u64<I, E: ParseError<I>>(input: I) -> IResult<I, u64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 8;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u64;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u64) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 16 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_u128;
        ///
        /// let parser = |s| {
        ///   le_u128(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07060504030201000706050403020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_u128<I, E: ParseError<I>>(input: I) -> IResult<I, u128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 16;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let mut res = 0u128;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u128) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a signed 1 byte integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_i8;
        ///
        /// let parser = |s| {
        ///   le_i8(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"\x03abcefg"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Error((&[][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_i8<I, E: ParseError<I>>(input: I) -> IResult<I, i8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u8.map(|x| x as i8).parse(input)
        }
        /// Recognizes a little endian signed 2 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_i16;
        ///
        /// let parser = |s| {
        ///   le_i16(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0300)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_i16<I, E: ParseError<I>>(input: I) -> IResult<I, i16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u16.map(|x| x as i16).parse(input)
        }
        /// Recognizes a little endian signed 3 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_i24;
        ///
        /// let parser = |s| {
        ///   le_i24(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x050300)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_i24<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u24
                .map(|x| {
                    if x & 0x80_00_00 != 0 {
                        (x | 0xff_00_00_00) as i32
                    } else {
                        x as i32
                    }
                })
                .parse(input)
        }
        /// Recognizes a little endian signed 4 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_i32;
        ///
        /// let parser = |s| {
        ///   le_i32(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07050300)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_i32<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u32.map(|x| x as i32).parse(input)
        }
        /// Recognizes a little endian signed 8 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_i64;
        ///
        /// let parser = |s| {
        ///   le_i64(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0706050403020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_i64<I, E: ParseError<I>>(input: I) -> IResult<I, i64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u64.map(|x| x as i64).parse(input)
        }
        /// Recognizes a little endian signed 16 bytes integer.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_i128;
        ///
        /// let parser = |s| {
        ///   le_i128(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07060504030201000706050403020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_i128<I, E: ParseError<I>>(input: I) -> IResult<I, i128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u128.map(|x| x as i128).parse(input)
        }
        /// Recognizes an unsigned 1 byte integer
        ///
        /// Note that endianness does not apply to 1 byte numbers.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::u8;
        ///
        /// let parser = |s| {
        ///   u8(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"\x03abcefg"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Error((&[][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 1;
            if input.input_len() < bound {
                Err(Err::Error(make_error(input, ErrorKind::Eof)))
            } else {
                let res = input.iter_elements().next().unwrap();
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes an unsigned 2 bytes integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u16 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u16 integer.
        /// *complete version*: returns an error if there is not enough input data
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::u16;
        ///
        /// let be_u16 = |s| {
        ///   u16(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u16(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0003)));
        /// assert_eq!(be_u16(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_u16 = |s| {
        ///   u16(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u16(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0300)));
        /// assert_eq!(le_u16(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn u16<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u16,
                crate::number::Endianness::Little => le_u16,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u16,
            }
        }
        /// Recognizes an unsigned 3 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u24 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u24 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::u24;
        ///
        /// let be_u24 = |s| {
        ///   u24(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u24(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x000305)));
        /// assert_eq!(be_u24(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_u24 = |s| {
        ///   u24(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u24(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x050300)));
        /// assert_eq!(le_u24(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn u24<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u24,
                crate::number::Endianness::Little => le_u24,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u24,
            }
        }
        /// Recognizes an unsigned 4 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u32 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u32 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::u32;
        ///
        /// let be_u32 = |s| {
        ///   u32(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u32(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00030507)));
        /// assert_eq!(be_u32(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_u32 = |s| {
        ///   u32(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u32(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07050300)));
        /// assert_eq!(le_u32(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn u32<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u32,
                crate::number::Endianness::Little => le_u32,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u32,
            }
        }
        /// Recognizes an unsigned 8 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u64 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u64 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::u64;
        ///
        /// let be_u64 = |s| {
        ///   u64(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u64(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0001020304050607)));
        /// assert_eq!(be_u64(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_u64 = |s| {
        ///   u64(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u64(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0706050403020100)));
        /// assert_eq!(le_u64(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn u64<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u64,
                crate::number::Endianness::Little => le_u64,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u64,
            }
        }
        /// Recognizes an unsigned 16 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u128 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u128 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::u128;
        ///
        /// let be_u128 = |s| {
        ///   u128(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u128(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00010203040506070001020304050607)));
        /// assert_eq!(be_u128(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_u128 = |s| {
        ///   u128(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u128(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07060504030201000706050403020100)));
        /// assert_eq!(le_u128(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn u128<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u128,
                crate::number::Endianness::Little => le_u128,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u128,
            }
        }
        /// Recognizes a signed 1 byte integer
        ///
        /// Note that endianness does not apply to 1 byte numbers.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::i8;
        ///
        /// let parser = |s| {
        ///   i8(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"\x03abcefg"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Error((&[][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn i8<I, E: ParseError<I>>(i: I) -> IResult<I, i8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            u8.map(|x| x as i8).parse(i)
        }
        /// Recognizes a signed 2 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i16 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i16 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::i16;
        ///
        /// let be_i16 = |s| {
        ///   i16(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i16(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0003)));
        /// assert_eq!(be_i16(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_i16 = |s| {
        ///   i16(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i16(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0300)));
        /// assert_eq!(le_i16(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn i16<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i16,
                crate::number::Endianness::Little => le_i16,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i16,
            }
        }
        /// Recognizes a signed 3 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i24 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i24 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::i24;
        ///
        /// let be_i24 = |s| {
        ///   i24(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i24(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x000305)));
        /// assert_eq!(be_i24(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_i24 = |s| {
        ///   i24(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i24(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x050300)));
        /// assert_eq!(le_i24(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn i24<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i24,
                crate::number::Endianness::Little => le_i24,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i24,
            }
        }
        /// Recognizes a signed 4 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i32 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i32 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::i32;
        ///
        /// let be_i32 = |s| {
        ///   i32(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i32(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00030507)));
        /// assert_eq!(be_i32(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_i32 = |s| {
        ///   i32(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i32(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07050300)));
        /// assert_eq!(le_i32(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn i32<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i32,
                crate::number::Endianness::Little => le_i32,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i32,
            }
        }
        /// Recognizes a signed 8 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i64 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i64 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::i64;
        ///
        /// let be_i64 = |s| {
        ///   i64(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i64(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0001020304050607)));
        /// assert_eq!(be_i64(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_i64 = |s| {
        ///   i64(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i64(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0706050403020100)));
        /// assert_eq!(le_i64(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn i64<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i64,
                crate::number::Endianness::Little => le_i64,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i64,
            }
        }
        /// Recognizes a signed 16 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i128 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i128 integer.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::i128;
        ///
        /// let be_i128 = |s| {
        ///   i128(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i128(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00010203040506070001020304050607)));
        /// assert_eq!(be_i128(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        ///
        /// let le_i128 = |s| {
        ///   i128(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i128(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07060504030201000706050403020100)));
        /// assert_eq!(le_i128(&b"\x01"[..]), Err(Err::Error((&[0x01][..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn i128<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i128,
                crate::number::Endianness::Little => le_i128,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i128,
            }
        }
        /// Recognizes a big endian 4 bytes floating point number.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_f32;
        ///
        /// let parser = |s| {
        ///   be_f32(s)
        /// };
        ///
        /// assert_eq!(parser(&[0x41, 0x48, 0x00, 0x00][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(parser(&b"abc"[..]), Err(Err::Error((&b"abc"[..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_f32<I, E: ParseError<I>>(input: I) -> IResult<I, f32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match be_u32(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f32::from_bits(o))),
            }
        }
        /// Recognizes a big endian 8 bytes floating point number.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::be_f64;
        ///
        /// let parser = |s| {
        ///   be_f64(s)
        /// };
        ///
        /// assert_eq!(parser(&[0x40, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(parser(&b"abc"[..]), Err(Err::Error((&b"abc"[..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn be_f64<I, E: ParseError<I>>(input: I) -> IResult<I, f64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match be_u64(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f64::from_bits(o))),
            }
        }
        /// Recognizes a little endian 4 bytes floating point number.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_f32;
        ///
        /// let parser = |s| {
        ///   le_f32(s)
        /// };
        ///
        /// assert_eq!(parser(&[0x00, 0x00, 0x48, 0x41][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(parser(&b"abc"[..]), Err(Err::Error((&b"abc"[..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_f32<I, E: ParseError<I>>(input: I) -> IResult<I, f32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match le_u32(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f32::from_bits(o))),
            }
        }
        /// Recognizes a little endian 8 bytes floating point number.
        ///
        /// *Complete version*: Returns an error if there is not enough input data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::le_f64;
        ///
        /// let parser = |s| {
        ///   le_f64(s)
        /// };
        ///
        /// assert_eq!(parser(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x29, 0x40][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(parser(&b"abc"[..]), Err(Err::Error((&b"abc"[..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn le_f64<I, E: ParseError<I>>(input: I) -> IResult<I, f64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match le_u64(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f64::from_bits(o))),
            }
        }
        /// Recognizes a 4 byte floating point number
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian f32 float,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian f32 float.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::f32;
        ///
        /// let be_f32 = |s| {
        ///   f32(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_f32(&[0x41, 0x48, 0x00, 0x00][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(be_f32(&b"abc"[..]), Err(Err::Error((&b"abc"[..], ErrorKind::Eof))));
        ///
        /// let le_f32 = |s| {
        ///   f32(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_f32(&[0x00, 0x00, 0x48, 0x41][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(le_f32(&b"abc"[..]), Err(Err::Error((&b"abc"[..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn f32<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, f32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_f32,
                crate::number::Endianness::Little => le_f32,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_f32,
            }
        }
        /// Recognizes an 8 byte floating point number
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian f64 float,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian f64 float.
        /// *complete version*: returns an error if there is not enough input data
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::f64;
        ///
        /// let be_f64 = |s| {
        ///   f64(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_f64(&[0x40, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(be_f64(&b"abc"[..]), Err(Err::Error((&b"abc"[..], ErrorKind::Eof))));
        ///
        /// let le_f64 = |s| {
        ///   f64(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_f64(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x29, 0x40][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(le_f64(&b"abc"[..]), Err(Err::Error((&b"abc"[..], ErrorKind::Eof))));
        /// ```
        #[inline]
        pub fn f64<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, f64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_f64,
                crate::number::Endianness::Little => le_f64,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_f64,
            }
        }
        /// Recognizes a hex-encoded integer.
        ///
        /// *Complete version*: Will parse until the end of input if it has less than 8 bytes.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::hex_u32;
        ///
        /// let parser = |s| {
        ///   hex_u32(s)
        /// };
        ///
        /// assert_eq!(parser(&b"01AE"[..]), Ok((&b""[..], 0x01AE)));
        /// assert_eq!(parser(&b"abc"[..]), Ok((&b""[..], 0x0ABC)));
        /// assert_eq!(parser(&b"ggg"[..]), Err(Err::Error((&b"ggg"[..], ErrorKind::IsA))));
        /// ```
        #[inline]
        pub fn hex_u32<'a, E: ParseError<&'a [u8]>>(
            input: &'a [u8],
        ) -> IResult<&'a [u8], u32, E> {
            let (i, o) = crate::bytes::complete::is_a(
                &b"0123456789abcdefABCDEF"[..],
            )(input)?;
            let (parsed, remaining) = if o.len() <= 8 {
                (o, i)
            } else {
                (&input[..8], &input[8..])
            };
            let res = parsed
                .iter()
                .rev()
                .enumerate()
                .map(|(k, &v)| {
                    let digit = v as char;
                    digit.to_digit(16).unwrap_or(0) << (k * 4)
                })
                .sum();
            Ok((remaining, res))
        }
        /// Recognizes floating point number in a byte string and returns the corresponding slice.
        ///
        /// *Complete version*: Can parse until the end of input.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::recognize_float;
        ///
        /// let parser = |s| {
        ///   recognize_float(s)
        /// };
        ///
        /// assert_eq!(parser("11e-1"), Ok(("", "11e-1")));
        /// assert_eq!(parser("123E-02"), Ok(("", "123E-02")));
        /// assert_eq!(parser("123K-01"), Ok(("K-01", "123")));
        /// assert_eq!(parser("abc"), Err(Err::Error(("abc", ErrorKind::Char))));
        /// ```
        #[rustfmt::skip]
        pub fn recognize_float<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: Clone + Offset,
            T: InputIter,
            <T as InputIter>::Item: AsChar,
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            recognize(
                tuple((
                    opt(alt((char('+'), char('-')))),
                    alt((
                        map(tuple((digit1, opt(pair(char('.'), opt(digit1))))), |_| ()),
                        map(tuple((char('.'), digit1)), |_| ()),
                    )),
                    opt(
                        tuple((
                            alt((char('e'), char('E'))),
                            opt(alt((char('+'), char('-')))),
                            cut(digit1),
                        )),
                    ),
                )),
            )(input)
        }
        #[doc(hidden)]
        pub fn recognize_float_or_exceptions<T, E: ParseError<T>>(
            input: T,
        ) -> IResult<T, T, E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: Clone + Offset,
            T: InputIter + InputTake + Compare<&'static str>,
            <T as InputIter>::Item: AsChar,
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            alt((
                |i: T| {
                    recognize_float::<_, E>(i.clone())
                        .map_err(|e| match e {
                            crate::Err::Error(_) => {
                                crate::Err::Error(E::from_error_kind(i, ErrorKind::Float))
                            }
                            crate::Err::Failure(_) => {
                                crate::Err::Failure(E::from_error_kind(i, ErrorKind::Float))
                            }
                            crate::Err::Incomplete(needed) => {
                                crate::Err::Incomplete(needed)
                            }
                        })
                },
                |i: T| {
                    crate::bytes::complete::tag_no_case::<_, _, E>("nan")(i.clone())
                        .map_err(|_| crate::Err::Error(
                            E::from_error_kind(i, ErrorKind::Float),
                        ))
                },
                |i: T| {
                    crate::bytes::complete::tag_no_case::<_, _, E>("inf")(i.clone())
                        .map_err(|_| crate::Err::Error(
                            E::from_error_kind(i, ErrorKind::Float),
                        ))
                },
                |i: T| {
                    crate::bytes::complete::tag_no_case::<_, _, E>("infinity")(i.clone())
                        .map_err(|_| crate::Err::Error(
                            E::from_error_kind(i, ErrorKind::Float),
                        ))
                },
            ))(input)
        }
        /// Recognizes a floating point number in text format
        ///
        /// It returns a tuple of (`sign`, `integer part`, `fraction part` and `exponent`) of the input
        /// data.
        ///
        /// *Complete version*: Can parse until the end of input.
        ///
        pub fn recognize_float_parts<T, E: ParseError<T>>(
            input: T,
        ) -> IResult<T, (bool, T, T, i32), E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Slice<Range<usize>>,
            T: Clone + Offset,
            T: InputIter + InputTake,
            <T as InputIter>::Item: AsChar + Copy,
            T: InputTakeAtPosition + InputLength,
            <T as InputTakeAtPosition>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
            T: AsBytes,
        {
            let (i, sign) = sign(input.clone())?;
            let (i, zeroes) = match i.as_bytes().iter().position(|c| *c != b'0') {
                Some(index) => i.take_split(index),
                None => i.take_split(i.input_len()),
            };
            let (i, mut integer) = match i
                .as_bytes()
                .iter()
                .position(|c| !(*c >= b'0' && *c <= b'9'))
            {
                Some(index) => i.take_split(index),
                None => i.take_split(i.input_len()),
            };
            if integer.input_len() == 0 && zeroes.input_len() > 0 {
                integer = zeroes.slice(zeroes.input_len() - 1..);
            }
            let (i, opt_dot) = opt(tag(&b"."[..]))(i)?;
            let (i, fraction) = if opt_dot.is_none() {
                let i2 = i.clone();
                (i2, i.slice(..0))
            } else {
                let mut zero_count = 0usize;
                let mut position = None;
                for (pos, c) in i.as_bytes().iter().enumerate() {
                    if *c >= b'0' && *c <= b'9' {
                        if *c == b'0' {
                            zero_count += 1;
                        } else {
                            zero_count = 0;
                        }
                    } else {
                        position = Some(pos);
                        break;
                    }
                }
                let position = position.unwrap_or(i.input_len());
                let index = if zero_count == 0 {
                    position
                } else if zero_count == position {
                    position - zero_count + 1
                } else {
                    position - zero_count
                };
                (i.slice(position..), i.slice(..index))
            };
            if integer.input_len() == 0 && fraction.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Float)));
            }
            let i2 = i.clone();
            let (i, e) = match i.as_bytes().iter().next() {
                Some(b'e') => (i.slice(1..), true),
                Some(b'E') => (i.slice(1..), true),
                _ => (i, false),
            };
            let (i, exp) = if e {
                cut(crate::character::complete::i32)(i)?
            } else {
                (i2, 0)
            };
            Ok((i, (sign, integer, fraction, exp)))
        }
        use crate::traits::ParseTo;
        /// Recognizes floating point number in text format and returns a f32.
        ///
        /// *Complete version*: Can parse until the end of input.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::float;
        ///
        /// let parser = |s| {
        ///   float(s)
        /// };
        ///
        /// assert_eq!(parser("11e-1"), Ok(("", 1.1)));
        /// assert_eq!(parser("123E-02"), Ok(("", 1.23)));
        /// assert_eq!(parser("123K-01"), Ok(("K-01", 123.0)));
        /// assert_eq!(parser("abc"), Err(Err::Error(("abc", ErrorKind::Float))));
        /// ```
        pub fn float<T, E: ParseError<T>>(input: T) -> IResult<T, f32, E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Slice<Range<usize>>,
            T: Clone + Offset + ParseTo<f32> + Compare<&'static str>,
            T: InputIter + InputLength + InputTake,
            <T as InputIter>::Item: AsChar + Copy,
            <T as InputIter>::IterElem: Clone,
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
            T: AsBytes,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, s) = recognize_float_or_exceptions(input)?;
            match s.parse_to() {
                Some(f) => Ok((i, f)),
                None => {
                    Err(
                        crate::Err::Error(
                            E::from_error_kind(i, crate::error::ErrorKind::Float),
                        ),
                    )
                }
            }
        }
        /// Recognizes floating point number in text format and returns a f64.
        ///
        /// *Complete version*: Can parse until the end of input.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::double;
        ///
        /// let parser = |s| {
        ///   double(s)
        /// };
        ///
        /// assert_eq!(parser("11e-1"), Ok(("", 1.1)));
        /// assert_eq!(parser("123E-02"), Ok(("", 1.23)));
        /// assert_eq!(parser("123K-01"), Ok(("K-01", 123.0)));
        /// assert_eq!(parser("abc"), Err(Err::Error(("abc", ErrorKind::Float))));
        /// ```
        pub fn double<T, E: ParseError<T>>(input: T) -> IResult<T, f64, E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Slice<Range<usize>>,
            T: Clone + Offset + ParseTo<f64> + Compare<&'static str>,
            T: InputIter + InputLength + InputTake,
            <T as InputIter>::Item: AsChar + Copy,
            <T as InputIter>::IterElem: Clone,
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
            T: AsBytes,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, s) = recognize_float_or_exceptions(input)?;
            match s.parse_to() {
                Some(f) => Ok((i, f)),
                None => {
                    Err(
                        crate::Err::Error(
                            E::from_error_kind(i, crate::error::ErrorKind::Float),
                        ),
                    )
                }
            }
        }
    }
    pub mod streaming {
        //! Parsers recognizing numbers, streaming version
        use crate::branch::alt;
        use crate::bytes::streaming::tag;
        use crate::character::streaming::{char, digit1, sign};
        use crate::combinator::{cut, map, opt, recognize};
        use crate::error::{ErrorKind, ParseError};
        use crate::internal::*;
        use crate::lib::std::ops::{RangeFrom, RangeTo};
        use crate::sequence::{pair, tuple};
        use crate::traits::{
            AsBytes, AsChar, Compare, InputIter, InputLength, InputTake,
            InputTakeAtPosition, Offset, Slice,
        };
        /// Recognizes an unsigned 1 byte integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_u8;
        ///
        /// let parser = |s| {
        ///   be_u8::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01abcd"[..]), Ok((&b"\x01abcd"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn be_u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 1;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(1)))
            } else {
                let res = input.iter_elements().next().unwrap();
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 2 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_u16;
        ///
        /// let parser = |s| {
        ///   be_u16::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01abcd"[..]), Ok((&b"abcd"[..], 0x0001)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn be_u16<I, E: ParseError<I>>(input: I) -> IResult<I, u16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 2;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u16;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u16;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 3 byte integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_u24;
        ///
        /// let parser = |s| {
        ///   be_u24::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02abcd"[..]), Ok((&b"abcd"[..], 0x000102)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(2))));
        /// ```
        #[inline]
        pub fn be_u24<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 3;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u32;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u32;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 4 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_u32;
        ///
        /// let parser = |s| {
        ///   be_u32::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03abcd"[..]), Ok((&b"abcd"[..], 0x00010203)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(3))));
        /// ```
        #[inline]
        pub fn be_u32<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 4;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u32;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u32;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 8 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_u64;
        ///
        /// let parser = |s| {
        ///   be_u64::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcd"[..]), Ok((&b"abcd"[..], 0x0001020304050607)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(7))));
        /// ```
        #[inline]
        pub fn be_u64<I, E: ParseError<I>>(input: I) -> IResult<I, u64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 8;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u64;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u64;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a big endian unsigned 16 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_u128;
        ///
        /// let parser = |s| {
        ///   be_u128::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x10\x11\x12\x13\x14\x15abcd"[..]), Ok((&b"abcd"[..], 0x00010203040506070809101112131415)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(15))));
        /// ```
        #[inline]
        pub fn be_u128<I, E: ParseError<I>>(input: I) -> IResult<I, u128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 16;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u128;
                for byte in input.iter_elements().take(bound) {
                    res = (res << 8) + byte as u128;
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a signed 1 byte integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_i8;
        ///
        /// let parser = be_i8::<_, (_, ErrorKind)>;
        ///
        /// assert_eq!(parser(&b"\x00\x01abcd"[..]), Ok((&b"\x01abcd"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn be_i8<I, E: ParseError<I>>(input: I) -> IResult<I, i8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u8.map(|x| x as i8).parse(input)
        }
        /// Recognizes a big endian signed 2 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_i16;
        ///
        /// let parser = be_i16::<_, (_, ErrorKind)>;
        ///
        /// assert_eq!(parser(&b"\x00\x01abcd"[..]), Ok((&b"abcd"[..], 0x0001)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(2))));
        /// ```
        #[inline]
        pub fn be_i16<I, E: ParseError<I>>(input: I) -> IResult<I, i16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u16.map(|x| x as i16).parse(input)
        }
        /// Recognizes a big endian signed 3 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_i24;
        ///
        /// let parser = be_i24::<_, (_, ErrorKind)>;
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02abcd"[..]), Ok((&b"abcd"[..], 0x000102)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(3))));
        /// ```
        #[inline]
        pub fn be_i24<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u24
                .map(|x| {
                    if x & 0x80_00_00 != 0 {
                        (x | 0xff_00_00_00) as i32
                    } else {
                        x as i32
                    }
                })
                .parse(input)
        }
        /// Recognizes a big endian signed 4 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_i32;
        ///
        /// let parser = be_i32::<_, (_, ErrorKind)>;
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03abcd"[..]), Ok((&b"abcd"[..], 0x00010203)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(4))));
        /// ```
        #[inline]
        pub fn be_i32<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u32.map(|x| x as i32).parse(input)
        }
        /// Recognizes a big endian signed 8 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_i64;
        ///
        /// let parser = be_i64::<_, (_, ErrorKind)>;
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcd"[..]), Ok((&b"abcd"[..], 0x0001020304050607)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(7))));
        /// ```
        #[inline]
        pub fn be_i64<I, E: ParseError<I>>(input: I) -> IResult<I, i64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u64.map(|x| x as i64).parse(input)
        }
        /// Recognizes a big endian signed 16 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_i128;
        ///
        /// let parser = be_i128::<_, (_, ErrorKind)>;
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x10\x11\x12\x13\x14\x15abcd"[..]), Ok((&b"abcd"[..], 0x00010203040506070809101112131415)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(15))));
        /// ```
        #[inline]
        pub fn be_i128<I, E: ParseError<I>>(input: I) -> IResult<I, i128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            be_u128.map(|x| x as i128).parse(input)
        }
        /// Recognizes an unsigned 1 byte integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_u8;
        ///
        /// let parser = le_u8::<_, (_, ErrorKind)>;
        ///
        /// assert_eq!(parser(&b"\x00\x01abcd"[..]), Ok((&b"\x01abcd"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn le_u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 1;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(1)))
            } else {
                let res = input.iter_elements().next().unwrap();
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 2 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_u16;
        ///
        /// let parser = |s| {
        ///   le_u16::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01abcd"[..]), Ok((&b"abcd"[..], 0x0100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn le_u16<I, E: ParseError<I>>(input: I) -> IResult<I, u16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 2;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u16;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u16) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 3 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_u24;
        ///
        /// let parser = |s| {
        ///   le_u24::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02abcd"[..]), Ok((&b"abcd"[..], 0x020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(2))));
        /// ```
        #[inline]
        pub fn le_u24<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 3;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u32;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u32) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 4 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_u32;
        ///
        /// let parser = |s| {
        ///   le_u32::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03abcd"[..]), Ok((&b"abcd"[..], 0x03020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(3))));
        /// ```
        #[inline]
        pub fn le_u32<I, E: ParseError<I>>(input: I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 4;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u32;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u32) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 8 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_u64;
        ///
        /// let parser = |s| {
        ///   le_u64::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcd"[..]), Ok((&b"abcd"[..], 0x0706050403020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(7))));
        /// ```
        #[inline]
        pub fn le_u64<I, E: ParseError<I>>(input: I) -> IResult<I, u64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 8;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u64;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u64) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a little endian unsigned 16 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_u128;
        ///
        /// let parser = |s| {
        ///   le_u128::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x10\x11\x12\x13\x14\x15abcd"[..]), Ok((&b"abcd"[..], 0x15141312111009080706050403020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(15))));
        /// ```
        #[inline]
        pub fn le_u128<I, E: ParseError<I>>(input: I) -> IResult<I, u128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 16;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(bound - input.input_len())))
            } else {
                let mut res = 0u128;
                for (index, byte) in input.iter_indices().take(bound) {
                    res += (byte as u128) << (8 * index);
                }
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes a signed 1 byte integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_i8;
        ///
        /// let parser = le_i8::<_, (_, ErrorKind)>;
        ///
        /// assert_eq!(parser(&b"\x00\x01abcd"[..]), Ok((&b"\x01abcd"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn le_i8<I, E: ParseError<I>>(input: I) -> IResult<I, i8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u8.map(|x| x as i8).parse(input)
        }
        /// Recognizes a little endian signed 2 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_i16;
        ///
        /// let parser = |s| {
        ///   le_i16::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01abcd"[..]), Ok((&b"abcd"[..], 0x0100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn le_i16<I, E: ParseError<I>>(input: I) -> IResult<I, i16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u16.map(|x| x as i16).parse(input)
        }
        /// Recognizes a little endian signed 3 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_i24;
        ///
        /// let parser = |s| {
        ///   le_i24::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02abcd"[..]), Ok((&b"abcd"[..], 0x020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(2))));
        /// ```
        #[inline]
        pub fn le_i24<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u24
                .map(|x| {
                    if x & 0x80_00_00 != 0 {
                        (x | 0xff_00_00_00) as i32
                    } else {
                        x as i32
                    }
                })
                .parse(input)
        }
        /// Recognizes a little endian signed 4 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_i32;
        ///
        /// let parser = |s| {
        ///   le_i32::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03abcd"[..]), Ok((&b"abcd"[..], 0x03020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(3))));
        /// ```
        #[inline]
        pub fn le_i32<I, E: ParseError<I>>(input: I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u32.map(|x| x as i32).parse(input)
        }
        /// Recognizes a little endian signed 8 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_i64;
        ///
        /// let parser = |s| {
        ///   le_i64::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcd"[..]), Ok((&b"abcd"[..], 0x0706050403020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(7))));
        /// ```
        #[inline]
        pub fn le_i64<I, E: ParseError<I>>(input: I) -> IResult<I, i64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u64.map(|x| x as i64).parse(input)
        }
        /// Recognizes a little endian signed 16 bytes integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_i128;
        ///
        /// let parser = |s| {
        ///   le_i128::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x10\x11\x12\x13\x14\x15abcd"[..]), Ok((&b"abcd"[..], 0x15141312111009080706050403020100)));
        /// assert_eq!(parser(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(15))));
        /// ```
        #[inline]
        pub fn le_i128<I, E: ParseError<I>>(input: I) -> IResult<I, i128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            le_u128.map(|x| x as i128).parse(input)
        }
        /// Recognizes an unsigned 1 byte integer
        ///
        /// Note that endianness does not apply to 1 byte numbers.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::u8;
        ///
        /// let parser = |s| {
        ///   u8::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"\x03abcefg"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn u8<I, E: ParseError<I>>(input: I) -> IResult<I, u8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            let bound: usize = 1;
            if input.input_len() < bound {
                Err(Err::Incomplete(Needed::new(1)))
            } else {
                let res = input.iter_elements().next().unwrap();
                Ok((input.slice(bound..), res))
            }
        }
        /// Recognizes an unsigned 2 bytes integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u16 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u16 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::u16;
        ///
        /// let be_u16 = |s| {
        ///   u16::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u16(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0003)));
        /// assert_eq!(be_u16(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(1))));
        ///
        /// let le_u16 = |s| {
        ///   u16::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u16(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0300)));
        /// assert_eq!(le_u16(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn u16<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u16,
                crate::number::Endianness::Little => le_u16,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u16,
            }
        }
        /// Recognizes an unsigned 3 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u24 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u24 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::u24;
        ///
        /// let be_u24 = |s| {
        ///   u24::<_,(_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u24(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x000305)));
        /// assert_eq!(be_u24(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(2))));
        ///
        /// let le_u24 = |s| {
        ///   u24::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u24(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x050300)));
        /// assert_eq!(le_u24(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(2))));
        /// ```
        #[inline]
        pub fn u24<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u24,
                crate::number::Endianness::Little => le_u24,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u24,
            }
        }
        /// Recognizes an unsigned 4 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u32 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u32 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::u32;
        ///
        /// let be_u32 = |s| {
        ///   u32::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u32(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00030507)));
        /// assert_eq!(be_u32(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(3))));
        ///
        /// let le_u32 = |s| {
        ///   u32::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u32(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07050300)));
        /// assert_eq!(le_u32(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(3))));
        /// ```
        #[inline]
        pub fn u32<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u32,
                crate::number::Endianness::Little => le_u32,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u32,
            }
        }
        /// Recognizes an unsigned 8 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u64 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u64 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::u64;
        ///
        /// let be_u64 = |s| {
        ///   u64::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u64(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0001020304050607)));
        /// assert_eq!(be_u64(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(7))));
        ///
        /// let le_u64 = |s| {
        ///   u64::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u64(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0706050403020100)));
        /// assert_eq!(le_u64(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(7))));
        /// ```
        #[inline]
        pub fn u64<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u64,
                crate::number::Endianness::Little => le_u64,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u64,
            }
        }
        /// Recognizes an unsigned 16 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian u128 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian u128 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::u128;
        ///
        /// let be_u128 = |s| {
        ///   u128::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_u128(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00010203040506070001020304050607)));
        /// assert_eq!(be_u128(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(15))));
        ///
        /// let le_u128 = |s| {
        ///   u128::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_u128(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07060504030201000706050403020100)));
        /// assert_eq!(le_u128(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(15))));
        /// ```
        #[inline]
        pub fn u128<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, u128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_u128,
                crate::number::Endianness::Little => le_u128,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_u128,
            }
        }
        /// Recognizes a signed 1 byte integer
        ///
        /// Note that endianness does not apply to 1 byte numbers.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::i8;
        ///
        /// let parser = |s| {
        ///   i8::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&b"\x00\x03abcefg"[..]), Ok((&b"\x03abcefg"[..], 0x00)));
        /// assert_eq!(parser(&b""[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn i8<I, E: ParseError<I>>(i: I) -> IResult<I, i8, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            u8.map(|x| x as i8).parse(i)
        }
        /// Recognizes a signed 2 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i16 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i16 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::i16;
        ///
        /// let be_i16 = |s| {
        ///   i16::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i16(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0003)));
        /// assert_eq!(be_i16(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(1))));
        ///
        /// let le_i16 = |s| {
        ///   i16::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i16(&b"\x00\x03abcefg"[..]), Ok((&b"abcefg"[..], 0x0300)));
        /// assert_eq!(le_i16(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn i16<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i16, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i16,
                crate::number::Endianness::Little => le_i16,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i16,
            }
        }
        /// Recognizes a signed 3 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i24 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i24 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::i24;
        ///
        /// let be_i24 = |s| {
        ///   i24::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i24(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x000305)));
        /// assert_eq!(be_i24(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(2))));
        ///
        /// let le_i24 = |s| {
        ///   i24::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i24(&b"\x00\x03\x05abcefg"[..]), Ok((&b"abcefg"[..], 0x050300)));
        /// assert_eq!(le_i24(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(2))));
        /// ```
        #[inline]
        pub fn i24<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i24,
                crate::number::Endianness::Little => le_i24,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i24,
            }
        }
        /// Recognizes a signed 4 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i32 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i32 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::i32;
        ///
        /// let be_i32 = |s| {
        ///   i32::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i32(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00030507)));
        /// assert_eq!(be_i32(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(3))));
        ///
        /// let le_i32 = |s| {
        ///   i32::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i32(&b"\x00\x03\x05\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07050300)));
        /// assert_eq!(le_i32(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(3))));
        /// ```
        #[inline]
        pub fn i32<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i32,
                crate::number::Endianness::Little => le_i32,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i32,
            }
        }
        /// Recognizes a signed 8 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i64 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i64 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::i64;
        ///
        /// let be_i64 = |s| {
        ///   i64::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i64(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0001020304050607)));
        /// assert_eq!(be_i64(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(7))));
        ///
        /// let le_i64 = |s| {
        ///   i64::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i64(&b"\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x0706050403020100)));
        /// assert_eq!(le_i64(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(7))));
        /// ```
        #[inline]
        pub fn i64<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i64,
                crate::number::Endianness::Little => le_i64,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i64,
            }
        }
        /// Recognizes a signed 16 byte integer
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian i128 integer,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian i128 integer.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::i128;
        ///
        /// let be_i128 = |s| {
        ///   i128::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_i128(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x00010203040506070001020304050607)));
        /// assert_eq!(be_i128(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(15))));
        ///
        /// let le_i128 = |s| {
        ///   i128::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_i128(&b"\x00\x01\x02\x03\x04\x05\x06\x07\x00\x01\x02\x03\x04\x05\x06\x07abcefg"[..]), Ok((&b"abcefg"[..], 0x07060504030201000706050403020100)));
        /// assert_eq!(le_i128(&b"\x01"[..]), Err(Err::Incomplete(Needed::new(15))));
        /// ```
        #[inline]
        pub fn i128<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, i128, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_i128,
                crate::number::Endianness::Little => le_i128,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_i128,
            }
        }
        /// Recognizes a big endian 4 bytes floating point number.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_f32;
        ///
        /// let parser = |s| {
        ///   be_f32::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&[0x40, 0x29, 0x00, 0x00][..]), Ok((&b""[..], 2.640625)));
        /// assert_eq!(parser(&[0x01][..]), Err(Err::Incomplete(Needed::new(3))));
        /// ```
        #[inline]
        pub fn be_f32<I, E: ParseError<I>>(input: I) -> IResult<I, f32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match be_u32(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f32::from_bits(o))),
            }
        }
        /// Recognizes a big endian 8 bytes floating point number.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::be_f64;
        ///
        /// let parser = |s| {
        ///   be_f64::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&[0x40, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(parser(&[0x01][..]), Err(Err::Incomplete(Needed::new(7))));
        /// ```
        #[inline]
        pub fn be_f64<I, E: ParseError<I>>(input: I) -> IResult<I, f64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match be_u64(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f64::from_bits(o))),
            }
        }
        /// Recognizes a little endian 4 bytes floating point number.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_f32;
        ///
        /// let parser = |s| {
        ///   le_f32::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&[0x00, 0x00, 0x48, 0x41][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(parser(&[0x01][..]), Err(Err::Incomplete(Needed::new(3))));
        /// ```
        #[inline]
        pub fn le_f32<I, E: ParseError<I>>(input: I) -> IResult<I, f32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match le_u32(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f32::from_bits(o))),
            }
        }
        /// Recognizes a little endian 8 bytes floating point number.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::le_f64;
        ///
        /// let parser = |s| {
        ///   le_f64::<_, (_, ErrorKind)>(s)
        /// };
        ///
        /// assert_eq!(parser(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x48, 0x41][..]), Ok((&b""[..], 3145728.0)));
        /// assert_eq!(parser(&[0x01][..]), Err(Err::Incomplete(Needed::new(7))));
        /// ```
        #[inline]
        pub fn le_f64<I, E: ParseError<I>>(input: I) -> IResult<I, f64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match le_u64(input) {
                Err(e) => Err(e),
                Ok((i, o)) => Ok((i, f64::from_bits(o))),
            }
        }
        /// Recognizes a 4 byte floating point number
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian f32 float,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian f32 float.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::f32;
        ///
        /// let be_f32 = |s| {
        ///   f32::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_f32(&[0x41, 0x48, 0x00, 0x00][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(be_f32(&b"abc"[..]), Err(Err::Incomplete(Needed::new(1))));
        ///
        /// let le_f32 = |s| {
        ///   f32::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_f32(&[0x00, 0x00, 0x48, 0x41][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(le_f32(&b"abc"[..]), Err(Err::Incomplete(Needed::new(1))));
        /// ```
        #[inline]
        pub fn f32<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, f32, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_f32,
                crate::number::Endianness::Little => le_f32,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_f32,
            }
        }
        /// Recognizes an 8 byte floating point number
        ///
        /// If the parameter is `nom::number::Endianness::Big`, parse a big endian f64 float,
        /// otherwise if `nom::number::Endianness::Little` parse a little endian f64 float.
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::streaming::f64;
        ///
        /// let be_f64 = |s| {
        ///   f64::<_, (_, ErrorKind)>(nom::number::Endianness::Big)(s)
        /// };
        ///
        /// assert_eq!(be_f64(&[0x40, 0x29, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(be_f64(&b"abc"[..]), Err(Err::Incomplete(Needed::new(5))));
        ///
        /// let le_f64 = |s| {
        ///   f64::<_, (_, ErrorKind)>(nom::number::Endianness::Little)(s)
        /// };
        ///
        /// assert_eq!(le_f64(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x29, 0x40][..]), Ok((&b""[..], 12.5)));
        /// assert_eq!(le_f64(&b"abc"[..]), Err(Err::Incomplete(Needed::new(5))));
        /// ```
        #[inline]
        pub fn f64<I, E: ParseError<I>>(
            endian: crate::number::Endianness,
        ) -> fn(I) -> IResult<I, f64, E>
        where
            I: Slice<RangeFrom<usize>> + InputIter<Item = u8> + InputLength,
        {
            match endian {
                crate::number::Endianness::Big => be_f64,
                crate::number::Endianness::Little => le_f64,
                #[cfg(target_endian = "little")]
                crate::number::Endianness::Native => le_f64,
            }
        }
        /// Recognizes a hex-encoded integer.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::hex_u32;
        ///
        /// let parser = |s| {
        ///   hex_u32(s)
        /// };
        ///
        /// assert_eq!(parser(b"01AE;"), Ok((&b";"[..], 0x01AE)));
        /// assert_eq!(parser(b"abc"), Err(Err::Incomplete(Needed::new(1))));
        /// assert_eq!(parser(b"ggg"), Err(Err::Error((&b"ggg"[..], ErrorKind::IsA))));
        /// ```
        #[inline]
        pub fn hex_u32<'a, E: ParseError<&'a [u8]>>(
            input: &'a [u8],
        ) -> IResult<&'a [u8], u32, E> {
            let (i, o) = crate::bytes::streaming::is_a(
                &b"0123456789abcdefABCDEF"[..],
            )(input)?;
            let (parsed, remaining) = if o.len() <= 8 {
                (o, i)
            } else {
                (&input[..8], &input[8..])
            };
            let res = parsed
                .iter()
                .rev()
                .enumerate()
                .map(|(k, &v)| {
                    let digit = v as char;
                    digit.to_digit(16).unwrap_or(0) << (k * 4)
                })
                .sum();
            Ok((remaining, res))
        }
        /// Recognizes a floating point number in text format and returns the corresponding part of the input.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if it reaches the end of input.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// use nom::number::streaming::recognize_float;
        ///
        /// let parser = |s| {
        ///   recognize_float(s)
        /// };
        ///
        /// assert_eq!(parser("11e-1;"), Ok((";", "11e-1")));
        /// assert_eq!(parser("123E-02;"), Ok((";", "123E-02")));
        /// assert_eq!(parser("123K-01"), Ok(("K-01", "123")));
        /// assert_eq!(parser("abc"), Err(Err::Error(("abc", ErrorKind::Char))));
        /// ```
        #[rustfmt::skip]
        pub fn recognize_float<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: Clone + Offset,
            T: InputIter,
            <T as InputIter>::Item: AsChar,
            T: InputTakeAtPosition + InputLength,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            recognize(
                tuple((
                    opt(alt((char('+'), char('-')))),
                    alt((
                        map(tuple((digit1, opt(pair(char('.'), opt(digit1))))), |_| ()),
                        map(tuple((char('.'), digit1)), |_| ()),
                    )),
                    opt(
                        tuple((
                            alt((char('e'), char('E'))),
                            opt(alt((char('+'), char('-')))),
                            cut(digit1),
                        )),
                    ),
                )),
            )(input)
        }
        #[doc(hidden)]
        pub fn recognize_float_or_exceptions<T, E: ParseError<T>>(
            input: T,
        ) -> IResult<T, T, E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: Clone + Offset,
            T: InputIter + InputTake + InputLength + Compare<&'static str>,
            <T as InputIter>::Item: AsChar,
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
        {
            alt((
                |i: T| {
                    recognize_float::<_, E>(i.clone())
                        .map_err(|e| match e {
                            crate::Err::Error(_) => {
                                crate::Err::Error(E::from_error_kind(i, ErrorKind::Float))
                            }
                            crate::Err::Failure(_) => {
                                crate::Err::Failure(E::from_error_kind(i, ErrorKind::Float))
                            }
                            crate::Err::Incomplete(needed) => {
                                crate::Err::Incomplete(needed)
                            }
                        })
                },
                |i: T| {
                    crate::bytes::streaming::tag_no_case::<_, _, E>("nan")(i.clone())
                        .map_err(|_| crate::Err::Error(
                            E::from_error_kind(i, ErrorKind::Float),
                        ))
                },
                |i: T| {
                    crate::bytes::streaming::tag_no_case::<_, _, E>("inf")(i.clone())
                        .map_err(|_| crate::Err::Error(
                            E::from_error_kind(i, ErrorKind::Float),
                        ))
                },
                |i: T| {
                    crate::bytes::streaming::tag_no_case::<
                        _,
                        _,
                        E,
                    >("infinity")(i.clone())
                        .map_err(|_| crate::Err::Error(
                            E::from_error_kind(i, ErrorKind::Float),
                        ))
                },
            ))(input)
        }
        /// Recognizes a floating point number in text format
        ///
        /// It returns a tuple of (`sign`, `integer part`, `fraction part` and `exponent`) of the input
        /// data.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        pub fn recognize_float_parts<T, E: ParseError<T>>(
            input: T,
        ) -> IResult<T, (bool, T, T, i32), E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: Clone + Offset,
            T: InputIter + crate::traits::ParseTo<i32>,
            <T as InputIter>::Item: AsChar,
            T: InputTakeAtPosition + InputTake + InputLength,
            <T as InputTakeAtPosition>::Item: AsChar,
            T: for<'a> Compare<&'a [u8]>,
            T: AsBytes,
        {
            let (i, sign) = sign(input.clone())?;
            let (i, zeroes) = match i.as_bytes().iter().position(|c| *c != b'0') {
                Some(index) => i.take_split(index),
                None => i.take_split(i.input_len()),
            };
            let (i, mut integer) = match i
                .as_bytes()
                .iter()
                .position(|c| !(*c >= b'0' && *c <= b'9'))
            {
                Some(index) => i.take_split(index),
                None => i.take_split(i.input_len()),
            };
            if integer.input_len() == 0 && zeroes.input_len() > 0 {
                integer = zeroes.slice(zeroes.input_len() - 1..);
            }
            let (i, opt_dot) = opt(tag(&b"."[..]))(i)?;
            let (i, fraction) = if opt_dot.is_none() {
                let i2 = i.clone();
                (i2, i.slice(..0))
            } else {
                let mut zero_count = 0usize;
                let mut position = None;
                for (pos, c) in i.as_bytes().iter().enumerate() {
                    if *c >= b'0' && *c <= b'9' {
                        if *c == b'0' {
                            zero_count += 1;
                        } else {
                            zero_count = 0;
                        }
                    } else {
                        position = Some(pos);
                        break;
                    }
                }
                let position = match position {
                    Some(p) => p,
                    None => return Err(Err::Incomplete(Needed::new(1))),
                };
                let index = if zero_count == 0 {
                    position
                } else if zero_count == position {
                    position - zero_count + 1
                } else {
                    position - zero_count
                };
                (i.slice(position..), i.slice(..index))
            };
            if integer.input_len() == 0 && fraction.input_len() == 0 {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Float)));
            }
            let i2 = i.clone();
            let (i, e) = match i.as_bytes().iter().next() {
                Some(b'e') => (i.slice(1..), true),
                Some(b'E') => (i.slice(1..), true),
                _ => (i, false),
            };
            let (i, exp) = if e {
                cut(crate::character::streaming::i32)(i)?
            } else {
                (i2, 0)
            };
            Ok((i, (sign, integer, fraction, exp)))
        }
        /// Recognizes floating point number in text format and returns a f32.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::float;
        ///
        /// let parser = |s| {
        ///   float(s)
        /// };
        ///
        /// assert_eq!(parser("11e-1"), Ok(("", 1.1)));
        /// assert_eq!(parser("123E-02"), Ok(("", 1.23)));
        /// assert_eq!(parser("123K-01"), Ok(("K-01", 123.0)));
        /// assert_eq!(parser("abc"), Err(Err::Error(("abc", ErrorKind::Float))));
        /// ```
        pub fn float<T, E: ParseError<T>>(input: T) -> IResult<T, f32, E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: Clone + Offset,
            T: InputIter + InputLength + InputTake + crate::traits::ParseTo<f32>
                + Compare<&'static str>,
            <T as InputIter>::Item: AsChar,
            <T as InputIter>::IterElem: Clone,
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
            T: AsBytes,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, s) = recognize_float_or_exceptions(input)?;
            match s.parse_to() {
                Some(f) => Ok((i, f)),
                None => {
                    Err(
                        crate::Err::Error(
                            E::from_error_kind(i, crate::error::ErrorKind::Float),
                        ),
                    )
                }
            }
        }
        /// Recognizes floating point number in text format and returns a f64.
        ///
        /// *Streaming version*: Will return `Err(nom::Err::Incomplete(_))` if there is not enough data.
        ///
        /// ```rust
        /// # use nom::{Err, error::ErrorKind, Needed};
        /// # use nom::Needed::Size;
        /// use nom::number::complete::double;
        ///
        /// let parser = |s| {
        ///   double(s)
        /// };
        ///
        /// assert_eq!(parser("11e-1"), Ok(("", 1.1)));
        /// assert_eq!(parser("123E-02"), Ok(("", 1.23)));
        /// assert_eq!(parser("123K-01"), Ok(("K-01", 123.0)));
        /// assert_eq!(parser("abc"), Err(Err::Error(("abc", ErrorKind::Float))));
        /// ```
        pub fn double<T, E: ParseError<T>>(input: T) -> IResult<T, f64, E>
        where
            T: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
            T: Clone + Offset,
            T: InputIter + InputLength + InputTake + crate::traits::ParseTo<f64>
                + Compare<&'static str>,
            <T as InputIter>::Item: AsChar,
            <T as InputIter>::IterElem: Clone,
            T: InputTakeAtPosition,
            <T as InputTakeAtPosition>::Item: AsChar,
            T: AsBytes,
            T: for<'a> Compare<&'a [u8]>,
        {
            let (i, s) = recognize_float_or_exceptions(input)?;
            match s.parse_to() {
                Some(f) => Ok((i, f)),
                None => {
                    Err(
                        crate::Err::Error(
                            E::from_error_kind(i, crate::error::ErrorKind::Float),
                        ),
                    )
                }
            }
        }
    }
    /// Configurable endianness
    pub enum Endianness {
        /// Big endian
        Big,
        /// Little endian
        Little,
        /// Will match the host's endianness
        Native,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Endianness {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    Endianness::Big => "Big",
                    Endianness::Little => "Little",
                    Endianness::Native => "Native",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Endianness {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Endianness {
        #[inline]
        fn eq(&self, other: &Endianness) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for Endianness {}
    #[automatically_derived]
    impl ::core::cmp::Eq for Endianness {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Endianness {
        #[inline]
        fn clone(&self) -> Endianness {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Endianness {}
}
