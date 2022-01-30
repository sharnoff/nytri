//! # `nytri` ("n-eye-tree") — Not Your Typical Regex Implementation
//!
//! `nytri` is a low-level Regular Expressions library with a focus on user-control.
//!
//! What does this mean? In practice, it means that `nytri` is designe to provide its users an unusual
//! level of access to the underlying Regex engine. Things like:
//!
//! * Stateful matching — i.e. feeding pieces of a match piece-by-piece
//! * Saving & restoring match state
//! * ... and many more planned.
//!
//! **This library is under initial development.** It should not be used in any serious setting, and
//! does not yet come close to the feature set that should be expected by a quality regex
//! implementation.
//!
//! ## Current & Planned features
//!
//! Before reading this list, note: `nytri` will always guarantee linear-time matching. Because of this,
//! some features are not possible to implement fully (like backreferences).
//!
//! Current:
//!
//! * [x] (very) Basic full-text matching (grouping w/ parens, alternatives, literals)
//! * [x] Stateful matching (via `Matcher::feed` & `Matcher::is_match`)
//! * [x] Matcher state save & restore (via `Matcher::get_state` & `Matcher::from_state`)
//!
//! Planned:
//!
//! * [ ] Error display options & configuration
//! * [ ] Syntax:
//!     * [ ] All of: `.`, `*`, `+`, and `?` — with the standard meanings.
//!     * [ ] Character escapes
//!     * [ ] (ASCII) Character classes
//!     * [ ] Unicode character classes
//! * [ ] Matcher configuration — will be clarified in the future
//! * [ ] Extracting capture groups (Named and unnamed)
//!     * [ ] Possible: intentionally don't capture certain groups, for efficiency.
//! * [ ] Possessive capture groups
//! * [ ] Lookahead & lookbehind (plus their negative variants)
//! * [ ] Generic atom and text types (e.g. alternatives to `char` and `&str`, or matching on `&[u8]`
//!     implemented as "just another text type")
//! * [ ] Limited support for backreferences (there is a useful subset that can be matched in linear-time)

mod compile;
mod nfa;

pub use compile::{CompileError, ErrorKind, HelpKind, HelpMsg, SyntaxError, SyntaxErrorKind};

use nfa::Nfa;
use std::borrow::Borrow;

/// A compiled regex
pub struct Regex {
    nfa: Nfa,
}

impl Regex {
    /// Compiles the pattern into a `Regex`, returning a `CompileError` on failure
    pub fn new(pattern: &str) -> Result<Self, CompileError> {
        let nfa = compile::compile_nfa(pattern)?;
        Ok(Regex { nfa })
    }

    /// Produces a [`Matcher`] that borrows from this `Regex`
    ///
    /// All standard regex operations go through the matcher.
    ///
    /// ## Borrowing
    ///
    /// It is sometimes desirable to have have some other way of referencing this `Regex` (e.g. by
    /// direct ownership, shared ownership à la `Rc`/`Arc`, etc.) — particularly because matchers
    /// yield back control to the caller, which can complicate borrow semantics. The
    /// [`Matcher::new`] method is provided for those cases, and accepts any reference to the
    /// underlying `Regex`.
    pub fn matcher(&self) -> MatcherRef {
        let this: &Regex = self;
        Matcher::new(this)
    }
}

/// A match-searching object, referencing a compiled [`Regex`]
///
/// By default, this type refers to an owned `Regex`, but a generic type can be provided to change
/// the type of reference to the `Regex`. The [`MatcherRef`] type is provided for your convenience.
///
/// Typically, instances of this type are constructed with the [`matcher`] method on `Regex`. You
/// can also use [`Matcher::new`] to use any reference type (implementing `Borrow<Regex>`).
///
/// [`matcher`]: Regex::matcher
/// [`matcher_ref`]: Regex::matcher_ref
pub struct Matcher<R> {
    matcher: nfa::Matcher<RegexRef<R>>,
}

/// A match-searching object, referencing a compiled [`Regex`]
///
/// Returned by [`Regex::matcher`].
///
/// This alias is typically preferred in the "standard" usage pattern — i.e. when the input is
/// completely known beforehand or control is not yielded during matching. This is how most regex
/// libraries expose their interface, and the `MatcherRef` borrows the underlying compiled regex to
/// accomodate this.
///
/// However, `Matcher`s are stateful & control can be yielded between providing input. Using a
/// `MatcherRef` with this pattern can make lifetime management tricky — in these cases, using a
/// plain [`Matcher`] (either owning the `Regex` directly, or with `Rc`/`Arc`) is often much
/// simpler.
pub type MatcherRef<'re> = Matcher<&'re Regex>;

/// The saved state of a matcher, available for comparison or restoration
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct MatcherState {
    state: nfa::MatcherState,
}

impl<R: Borrow<Regex>> Matcher<R> {
    /// Returns a new `Matcher`, using the provided reference to a compiled `Regex`
    pub fn new(re: R) -> Self {
        Matcher { matcher: Nfa::matcher(RegexRef { re }) }
    }

    /// Copies the internal state of the matcher, for later comparison or restoration with
    /// [`from_state`]
    ///
    /// [`from_state`]: Self::from_state
    pub fn get_state(&self) -> MatcherState {
        MatcherState { state: self.matcher.get_state() }
    }

    /// Constructs the `Matcher` from a previous state
    ///
    /// The state must have been returned from a previous call to [`get_state`] by a matcher for
    /// the same `Regex` — or a `Regex` with the same *pattern*. These properties are not (yet)
    /// checked; you must be careful.
    ///
    /// Please note: This function may be
    pub fn from_state(re: R, state: MatcherState) -> Self {
        Matcher {
            matcher: nfa::Matcher::from_state(RegexRef { re }, state.state),
        }
    }

    /// Feeds a string into the matcher, returning the original reference for chaining
    pub fn feed(&mut self, string: &str) -> &mut Self {
        self.matcher.feed(string);
        self
    }

    /// Returns whether the whole contents of previously-fed strings represent a match
    pub fn is_match(&self) -> bool {
        self.matcher.is_match()
    }
}

/// Helper type for granting access to the underlying NFA from arbitrary reference types
struct RegexRef<R> {
    re: R,
}

impl<R: Borrow<Regex>> AsRef<Nfa> for RegexRef<R> {
    fn as_ref(&self) -> &Nfa {
        &self.re.borrow().nfa
    }
}
