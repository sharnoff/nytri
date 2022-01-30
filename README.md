# `nytri` — Not Your Typical Regex Implementation

`nytri` is a low-level Regular Expressions library with a focus on user-control.

What does this mean? In practice, it means that `nytri` is designe to provide its users an unusual
level of access to the underlying Regex engine. Things like:

* Stateful matching — i.e. feeding pieces of a match piece-by-piece
* Saving & restoring match state
* ... and many more planned.

**This library is under initial development.** It should not be used in any serious setting, and
does not yet come close to the feature set that should be expected by a quality regex
implementation.

## Current & Planned features

Before reading this list, note: `nytri` will always guarantee linear-time matching. Because of this,
some features are not possible to implement fully (like backreferences).

Current:

* [x] (very) Basic full-text matching (grouping w/ parens, alternatives, literals)
* [x] Stateful matching (via `Matcher::feed` & `Matcher::is_match`)
* [x] Matcher state save & restore (via `Matcher::get_state` & `Matcher::from_state`)

Planned: 

* [ ] Error display options & configuration
* [ ] Syntax:
    * [ ] All of: `.`, `*`, `+`, and `?` — with the standard meanings.
    * [ ] Character escapes
    * [ ] (ASCII) Character classes
    * [ ] Unicode character classes
* [ ] Matcher configuration — will be clarified in the future
* [ ] Extracting capture groups (Named and unnamed)
    * [ ] Possible: intentionally don't capture certain groups, for efficiency.
* [ ] Possessive capture groups
* [ ] Lookahead & lookbehind (plus their negative variants)
* [ ] Generic atom and text types (e.g. alternatives to `char` and `&str`, or matching on `&[u8]`
    implemented as "just another text type")
* [ ] Limited support for backreferences (there is a useful subset that can be matched in linear-time)
