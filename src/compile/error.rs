//! Wrapper module for compilation errors

use super::Span;
use thiserror::Error;

/// An error occuring from compiling a `Regex`
///
/// ## Displaying errors
///
/// You may notice that this does not provide an implementation of [`Display`]. That is because the
/// way that errors should be displayed is context-dependent, for example:
///
/// * Is the pattern user-generated? How will the user actually see the error?
/// * Is it written in code somewhere, panicking? How much information do you want to see?
///
/// As such, the current plan is for methods on this type to provide implementations of `Display`,
/// in a number of levels of detail.
///
/// This feature will be added soon.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct CompileError {
    /// The original compiled pattern that produced this error
    pub pattern: String,
    pub kind: ErrorKind,
}

/// The type of compilation error that occured
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum ErrorKind {
    Syntax(SyntaxError),
}

/// An error resulting from invalid pattern syntax
#[derive(Copy, Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct SyntaxError {
    /// The location of the error in the pattern string
    pub span: Span,
    /// The "primary" message describing the error, e.g. "unexpected close parenthesis" or
    /// "repetition operators require an operand before them"
    pub msg: SyntaxErrorKind,
    /// Any additional hints that might be useful, e.g. "to match a literal '*', escape it with
    /// '\*'"
    pub help: &'static [HelpMsg],
}

/// The concrete types of syntax errors that can occur
#[derive(Copy, Clone, Debug, Error)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum SyntaxErrorKind {
    #[error("pattern must be non-empty")]
    EmptyPattern,
    #[error("open {name} has no matching close {name}")]
    UnclosedDelim { name: &'static str }, // help: EscapeToMatchLiteral
    #[error("unexpected close {name}")]
    UnexpectedCloseDelim { name: &'static str }, // help: EscapeToMatchLiteral
    #[error("unexpected postfix operator")]
    UnexpectedPostfix,
    // double postfix operators are currently illegal because we're going to use '<.>?' in the
    // future for non-greedy matching, and that shouldn't be backwards-incompatible.
    #[error("double postfix operators are not currently allowed")]
    DoublePostfixOp, // help: [ NonGreedyMatchersUnimplemented ]
}

/// A help message, either to a programmer or Regex writer
///
/// The addressee of the message can be retrieved with the [`kind`] method.
///
/// [`kind`]: Self::kind
#[derive(Copy, Clone, Debug, Error)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum HelpMsg {
    #[error("to match a literal {name}, use '{escaped}'")]
    EscapeToMatchLiteral { name: &'static str, escaped: &'static str },
    #[error("non-greedy matchers have not yet been implemented")]
    NonGreedyMatchersUnimplemented,
}

impl HelpMsg {
    /// Returns the kind of help message that this corresponds to
    ///
    /// Internally, this is used for filtering out help messages when diplaying a [`CompileError`]
    /// with different configurations.
    pub fn kind(&self) -> HelpKind {
        match self {
            HelpMsg::EscapeToMatchLiteral { .. } | HelpMsg::NonGreedyMatchersUnimplemented => {
                HelpKind::RegexWriter
            }
        }
    }
}

/// The entity that a help message from an error is directed to
#[derive(Copy, Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum HelpKind {
    /// A help message for a direct user of this library — typically regarding features of the
    /// library
    Programmer,
    /// A help message directed at whoever wrote the input pattern — whether that's an end-user or
    /// the same as the programmer using this library
    RegexWriter,
}
