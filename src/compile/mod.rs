//! Regex parsing & compilation

use crate::nfa::Nfa;
use std::fmt::{self, Debug, Formatter};
use std::ops::{Index, Range};

mod error;
mod parse;
#[cfg(test)]
mod parse_test;

pub use error::{CompileError, ErrorKind, HelpKind, HelpMsg, SyntaxError, SyntaxErrorKind};

/// Compiles the pattern into an NFA
pub fn compile_nfa(p: &str) -> Result<Nfa, CompileError> {
    let ast = parse::parse(p).map_err(|e| CompileError {
        pattern: p.to_owned(),
        kind: ErrorKind::Syntax(e),
    })?;

    Nfa::build(ast).map_err(|k| CompileError { pattern: p.to_owned(), kind: k })
}

pub struct Ast<'pat> {
    root: AstNodeId,
    nodes: Vec<AstNode<'pat>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AstNodeId(usize);

pub struct AstNode<'pat> {
    pub span: Span,
    pub kind: AstNodeKind<'pat>,
}

pub enum AstNodeKind<'pat> {
    Literal(&'pat str),
    Group(Option<AstNodeId>),
    // Note: the list will always have >= 2 entries
    Concat(Vec<AstNodeId>),
    // Note: the list will always have >= 2 entries, though entries equal to `None` *are* included
    // in this count.
    Alternate(Vec<Option<AstNodeId>>),
    Optional(AstNodeId),
    ZeroOrMore(AstNodeId),
    OneOrMore(AstNodeId),
}

impl<'pat> Ast<'pat> {
    /// Returns a reference to the root node of the AST
    pub fn root(&self) -> &AstNode {
        &self[self.root]
    }
}

impl<'pat> AstNode<'pat> {
    /// Returns whether the AST node corresponds to a postfix operator
    pub fn is_postfix(&self) -> bool {
        use AstNodeKind::*;

        match &self.kind {
            Optional(_) | ZeroOrMore(_) | OneOrMore(_) => true,
            Literal(_) | Group(_) | Concat(_) | Alternate(_) => false,
        }
    }
}

impl<'pat> Index<AstNodeId> for Ast<'pat> {
    type Output = AstNode<'pat>;

    fn index(&self, id: AstNodeId) -> &AstNode<'pat> {
        &self.nodes[id.0]
    }
}

impl<'pat> Index<&AstNodeId> for Ast<'pat> {
    type Output = AstNode<'pat>;

    fn index(&self, id: &AstNodeId) -> &AstNode<'pat> {
        &self.nodes[id.0]
    }
}

/// A span of the input pattern, typically referenced in an error message
///
/// Spans can be trivially converted into an equivalent `Range` with the [`to_range`] method.
///
/// [`to_range`]: Self::to_range
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    /// The starting byte position within the pattern
    pub start: usize,
    /// The ending byte position within the pattern, exclusive
    pub end: usize,
}

impl Span {
    /// Converts the `Span` into an equivalent `Range`
    pub fn to_range(self) -> Range<usize> {
        self.start..self.end
    }

    /// Returns the minimum span that contains the two inputs
    pub(crate) fn join(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl<'pat> Debug for Ast<'pat> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        struct DebugNode<'ast, 'pat>(&'ast Ast<'pat>, AstNodeId);

        impl<'a, 'p> Debug for DebugNode<'a, 'p> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                let node = &self.0[self.1];

                f.debug_struct("AstNode")
                    .field("id", &self.1)
                    .field("span", &node.span)
                    .field("kind", &DebugNodeKind(self.0, &node.kind))
                    .finish()
            }
        }

        struct DebugNodeKind<'ast, 'pat>(&'ast Ast<'pat>, &'ast AstNodeKind<'pat>);

        impl<'a, 'p> Debug for DebugNodeKind<'a, 'p> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                use AstNodeKind::*;

                match &self.1 {
                    Literal(pat) => f.debug_tuple("Literal").field(pat).finish(),
                    Group(opt) => f
                        .debug_tuple("Group")
                        .field(&opt.map(|id| DebugNode(&self.0, id)))
                        .finish(),
                    Concat(ids) => {
                        let nodes = ids
                            .iter()
                            .map(|id| DebugNode(&self.0, *id))
                            .collect::<Vec<_>>();

                        f.debug_tuple("Concat").field(&nodes).finish()
                    }
                    Alternate(ids) => {
                        let nodes = ids
                            .iter()
                            .map(|opt| opt.map(|id| DebugNode(&self.0, id)))
                            .collect::<Vec<_>>();

                        f.debug_tuple("Alternate").field(&nodes).finish()
                    }
                    Optional(id) => f
                        .debug_tuple("Optional")
                        .field(&DebugNode(&self.0, *id))
                        .finish(),
                    ZeroOrMore(id) => f
                        .debug_tuple("ZeroOrMore")
                        .field(&DebugNode(&self.0, *id))
                        .finish(),
                    OneOrMore(id) => f
                        .debug_tuple("OneOrMore")
                        .field(&DebugNode(&self.0, *id))
                        .finish(),
                }
            }
        }

        f.debug_struct("Ast")
            .field("root", &DebugNode(self, self.root))
            .finish()
    }
}

#[cfg(test)]
impl<'p1, 'p2> PartialEq<Ast<'p2>> for Ast<'p1> {
    fn eq(&self, other: &Ast<'p2>) -> bool {
        // Comparing equality is a little tricky because we can't directly compare AstNodeIds. In
        // theory, the same AST should have the same distribution of AstNodeIds, but we don't want
        // to *rely* on that.
        //
        // So we have to compare by structural equality instead — i.e. that each each AstNodeId
        // refers to an equivalent sub-tree.
        //
        // We implemented this with a stack because it was more fun :P

        use AstNodeKind::*;

        let mut nodes_stack = vec![(&self[self.root], &other[other.root])];

        while let Some((x, y)) = nodes_stack.pop() {
            if x.span != y.span {
                return false;
            }

            match (&x.kind, &y.kind) {
                (Literal(px), Literal(py)) if px == py => (),

                (Group(None), Group(None)) => (),
                (Group(Some(x_id)), Group(Some(y_id))) => {
                    nodes_stack.push((&self[x_id], &other[y_id]));
                }

                (Concat(x_ids), Concat(y_ids)) if x_ids.len() == y_ids.len() => {
                    for (x_id, y_id) in x_ids.iter().zip(y_ids.iter()).rev() {
                        nodes_stack.push((&self[x_id], &other[y_id]));
                    }
                }

                (Alternate(x_ids), Alternate(y_ids)) if x_ids.len() == y_ids.len() => {
                    for (x_oid, y_oid) in x_ids.iter().zip(y_ids.iter()).rev() {
                        match (x_oid, y_oid) {
                            (None, None) => (),
                            (Some(x_id), Some(y_id)) => {
                                nodes_stack.push((&self[x_id], &other[y_id]))
                            }
                            _ => return false,
                        }
                    }
                }
                (Optional(x_id), Optional(y_id)) => nodes_stack.push((&self[x_id], &other[y_id])),
                (ZeroOrMore(x_id), ZeroOrMore(y_id)) => {
                    nodes_stack.push((&self[x_id], &other[y_id]))
                }
                (OneOrMore(x_id), OneOrMore(y_id)) => nodes_stack.push((&self[x_id], &other[y_id])),

                _ => return false,
            }
        }

        true
    }
}

#[cfg(test)]
impl<'pat> Eq for Ast<'pat> {}
