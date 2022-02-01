//! Testing for pattern parsing

use super::parse::parse;
use super::{HelpMsg, Span, SyntaxError, SyntaxErrorKind};

/// Helper macro for constructing abstract syntax trees — because all node references are
/// represented as `AstNodeId`s, not boxed nodes.
///
/// The sytnax is the same as standard struct initialization, with a couple subtle differences to
/// note:
///
///  1. Embedded notes cannot refer to local variables. Things like `AstNodeKind::Literal` still
///     can, but all the values that would normally be `AstNodeId`s cannot. This is because we'd
///     have no way of determining its node Id.
///
///  2. `AstNodeKind` variants are identified manually, by name (without the leading
///     `AstNodeKind::`).
///
/// For examples, have a look through the rest of this file — there's a number of them :)
macro_rules! ast {
    (Ast { root: $($root_tt:tt)* }) => {{
        #[allow(unused_mut)] // unused_mut in case we encounter an error
        let mut nodes: Vec<super::AstNode> = Vec::new();

        let root = ast!(@AstNode nodes $($root_tt)*);
        super::Ast {
            root,
            nodes,
        }
    }};

    (@AstNode $ns:ident AstNode { $($tt:tt)* } $(,)?) => {{
        ast!(@AstNode@inner $ns $($tt)*)
    }};
    (@Option<AstNode> $ns:ident None $(,)?) => { None };
    (@Option<AstNode> $ns:ident Some($($inner:tt)*) $(,)?) => { Some(ast!(@AstNode $ns $($inner)*)) };

    (@Vec<AstNode> $ns:ident vec![$( AstNode { $($tt:tt)* } ),* $(,)?]) => {
        vec![$( ast!(@AstNode@inner $ns $($tt)*), )*]
    };
    // Vec<Option> can't be matched with repetitions, so we have to construct them recursively
    (@Vec<Option<AstNode>> $ns:ident vec![$($inner:tt)*] $(,)?) => {
        ast!(@Vec<Option<AstNode>>@inner $ns [] $($inner)*)
    };
    (@Vec<Option<AstNode>>@inner $ns:ident [$($ids:expr,)*]) => {
        vec![$($ids),*]
    };
    (@Vec<Option<AstNode>>@inner $ns:ident [$($ids:expr,)*] None $(, $($rest:tt)*)?) => {
        ast!(@Vec<Option<AstNode>>@inner $ns [$($ids,)* None,] $($($rest)*)?)
    };
    (@Vec<Option<AstNode>>@inner $ns:ident [$($ids:expr,)*] Some($($inner:tt)*) $(, $($rest:tt)*)?) => {
        ast!(@Vec<Option<AstNode>>@inner $ns [$($ids,)* Some(ast!(@AstNode $ns $($inner)*)),] $($($rest)*)?)
    };

    (@push $ns:ident $($node:tt)*) => {{
        let node = $($node)*;
        let id = super::AstNodeId($ns.len());
        $ns.push(node);
        id
    }};
    (@AstNode@inner $ns:ident span: Span { $($span:tt)* }, kind: $variant:ident ($($kind_inner:tt)*) $(,)? ) => {
        ast!(@push $ns super::AstNode {
            span: super::Span { $($span)* },
            kind: ast!(@AstNodeKind $ns $variant ($($kind_inner)*)),
        })
    };
    (@AstNode@inner $ns:ident kind: $variant:ident ($($kind_inner:tt)*), span: Span { $($span:tt)* } $(,)? ) => {
        ast!(@push $ns AstNode {
            span: super::Span { $($span)* },
            kind: ast!(@AstNodeKind $ns $variant ($($kind_inner)*)),
        });
    };
    (@AstNodeKind $ns:ident Literal($($inner:tt)*)) => {
        super::AstNodeKind::Literal($($inner)*)
    };
    (@AstNodeKind $ns:ident Group($($inner:tt)*)) => {
        super::AstNodeKind::Group(ast!(@Option<AstNode> $ns $($inner)*))
    };
    (@AstNodeKind $ns:ident Concat($($inner:tt)*)) => {
        super::AstNodeKind::Concat(ast!(@Vec<AstNode> $ns $($inner)*))
    };
    (@AstNodeKind $ns:ident Alternate($($inner:tt)*)) => {
        super::AstNodeKind::Alternate(ast!(@Vec<Option<AstNode>> $ns $($inner)*))
    };
}

// "foobar"
#[test]
fn literal() {
    let pat = "foobar";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 6 },
            kind: Literal("foobar"),
        }
    });

    assert_eq!(parse(pat), Ok(expected));
}

// "foo|bar|baz"
#[test]
fn alt() {
    let pat = "foo|bar|baz";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 11 },
            kind: Alternate(vec![
                Some(AstNode {
                    span: Span { start: 0, end: 3 },
                    kind: Literal("foo"),
                }),
                Some(AstNode {
                    span: Span { start: 4, end: 7 },
                    kind: Literal("bar"),
                }),
                Some(AstNode {
                    span: Span { start: 8, end: 11 },
                    kind: Literal("baz"),
                }),
            ]),
        },
    });

    assert_eq!(parse(pat), Ok(expected));
}

// "|"
#[test]
fn alt_double_empty() {
    let pat = "|";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 1 },
            kind: Alternate(vec![None, None]),
        },
    });

    assert_eq!(parse(pat), Ok(expected));
}

// "a||b"
#[test]
fn alt_double_pipe() {
    let pat = "a||b";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 4 },
            kind: Alternate(vec![
                Some(AstNode {
                    span: Span { start: 0, end: 1 },
                    kind: Literal("a"),
                }),
                None,
                Some(AstNode {
                    span: Span { start: 3, end: 4 },
                    kind: Literal("b"),
                }),
            ]),
        },
    });

    assert_eq!(parse(pat), Ok(expected));
}

// "(|)"
#[test]
fn alt_double_empty_in_group() {
    let pat = "(|)";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 3 },
            kind: Group(Some(AstNode {
                span: Span { start: 1, end: 2 },
                kind: Alternate(vec![None, None]),
            })),
        },
    });

    assert_eq!(parse(pat), Ok(expected));
}

// "(foo|bar)"
#[test]
fn alt_in_group() {
    let pat = "(foo|bar)";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 9 },
            kind: Group(Some(AstNode {
                span: Span { start: 1, end: 8 },
                kind: Alternate(vec![
                    Some(AstNode {
                        span: Span { start: 1, end: 4 },
                        kind: Literal("foo"),
                    }),
                    Some(AstNode {
                        span: Span { start: 5, end: 8 },
                        kind: Literal("bar"),
                    }),
                ]),
            })),
        },
    });

    assert_eq!(parse(pat), Ok(expected));
}

// "()()()"
#[test]
fn concat_groups_empty() {
    let pat = "()()()";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 6 },
            kind: Concat(vec![
                AstNode {
                    span: Span { start: 0, end: 2 },
                    kind: Group(None),
                },
                AstNode {
                    span: Span { start: 2, end: 4 },
                    kind: Group(None),
                },
                AstNode {
                    span: Span { start: 4, end: 6 },
                    kind: Group(None),
                },
            ]),
        }
    });

    assert_eq!(parse(pat), Ok(expected));
}

// "(foo)(bar)(baz)"
#[test]
fn concat_groups() {
    let pat = "(foo)(bar)(baz)";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 15 },
            kind: Concat(vec![
                AstNode {
                    span: Span { start: 0, end: 5 },
                    kind: Group(Some(AstNode {
                        span: Span { start: 1, end: 4 },
                        kind: Literal("foo"),
                    })),
                },
                AstNode {
                    span: Span { start: 5, end: 10 },
                    kind: Group(Some(AstNode {
                        span: Span { start: 6, end: 9 },
                        kind: Literal("bar"),
                    })),
                },
                AstNode {
                    span: Span { start: 10, end: 15 },
                    kind: Group(Some(AstNode {
                        span: Span { start: 11, end: 14 },
                        kind: Literal("baz"),
                    })),
                },
            ]),
        }
    });

    assert_eq!(parse(pat), Ok(expected));
}

// "()|()"
#[test]
fn alt_groups_empty() {
    let pat = "()|()";

    let expected = ast!(Ast {
        root: AstNode {
            span: Span { start: 0, end: 5 },
            kind: Alternate(vec![
                Some(AstNode {
                    span: Span { start: 0, end: 2 },
                    kind: Group(None),
                }),
                Some(AstNode {
                    span: Span { start: 3, end: 5 },
                    kind: Group(None),
                }),
            ]),
        },
    });

    assert_eq!(parse(pat), Ok(expected));
}

// ""
#[test]
fn empty_pat_error() {
    let pat = "";

    let expected_err = SyntaxError {
        span: Span { start: 0, end: 0 },
        msg: SyntaxErrorKind::EmptyPattern,
        help: &[],
    };

    assert_eq!(parse(pat), Err(expected_err));
}

// "foo(bar"
#[test]
fn unclosed_paren_error() {
    let pat = "foo(bar";

    let expected_err = SyntaxError {
        span: Span { start: 3, end: 4 },
        msg: SyntaxErrorKind::UnclosedDelim { name: "parenthesis" },
        help: &[HelpMsg::EscapeToMatchLiteral { name: "open parenthesis", escaped: r"\(" }],
    };

    assert_eq!(parse(pat), Err(expected_err));
}
