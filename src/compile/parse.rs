//! The actual implementation of Regex pattern parsing

use super::error::{HelpMsg, SyntaxError, SyntaxErrorKind};
use super::{Ast, AstNode, AstNodeId, AstNodeKind, Span};
use std::iter::Peekable;
use std::mem;
use std::str::CharIndices;

/// Helper macro for defining patterns that match a set of characters
macro_rules! char_pat {
    (macro_rules! $name:ident = $($chars:literal),+ $(,)?) => {
        macro_rules! $name {
            () => { $($chars)|+ }
        }
    }
}

pub fn parse(pattern: &str) -> Result<Ast, SyntaxError> {
    // This function implements a fairly simple shift-reduce parser. Throughout parsing, we add
    // completed AST nodes into the backing vector (`nodes`), and we return from inside the loop
    // once we reach the end of the input — i.e. the final "reduce" action just returns.

    // A "partial" node.
    #[derive(Debug)]
    enum PartialNode {
        // A completed node
        Node(AstNodeId),
        // A list of concatenated nodes
        Concat(Vec<AstNodeId>),
        // A parenthesized sub-expression. The full parsing for such a group would be:
        Group {
            start_span: Span,
        },
        // // A run of literal values in the regex. The actual contents are determined by the span
        Literal {
            span: Span,
        },
        // A list of alternatives (i.e. <EXPR> "|" <EXPR> ...)
        Alternate {
            span: Span,
            alts: Vec<Option<AstNodeId>>,
            trailing_pipe: bool,
        },
    }

    use PartialNode::*;

    // Completed AST nodes
    let mut nodes: Vec<AstNode> = Vec::new();

    // Note: the stack is always non-empty.
    let mut stack: Vec<PartialNode> = vec![];

    // An iterator over characters *and their span*
    let mut chars = CharPeeker::new(pattern);

    let reserved_chars = &['|', '(', ')'];
    let is_special_char = |c| reserved_chars.contains(&c);

    char_pat!(macro_rules! ends_concat = '|', ')');
    char_pat!(macro_rules! ends_alt = ')');

    enum Consume {
        Yes,
        No,
    }

    loop {
        let consume = match (chars.peek2(), stack.as_mut_slice()) {
            // ----- Success & end-Errors -----

            // Success condition: If we get to the end of the input with a single completed
            // node, then we finished our AST
            ([], [Node(id)]) => return Ok(Ast { root: *id, nodes }),

            // The string was empty. We shouldn't be given empty strings.
            ([], []) => {
                return Err(SyntaxError {
                    span: Span { start: 0, end: 0 },
                    msg: SyntaxErrorKind::EmptyPattern,
                    help: &[],
                })
            }

            // Unclosed parenthesis
            ([], [.., Group { start_span }] | [.., Group { start_span }, Node(_)]) => {
                return Err(SyntaxError {
                    span: *start_span,
                    msg: SyntaxErrorKind::UnclosedDelim { name: "parenthesis" },
                    help: &[HelpMsg::EscapeToMatchLiteral {
                        name: "open parenthesis",
                        escaped: r"\(",
                    }],
                })
            }

            // ----- Literal -----

            // shift: Literal + * -> Literal  (special case for efficiency)
            ([(sp, c), ..], [.., Literal { span }]) if !is_special_char(*c) => {
                *span = span.join(*sp);
                Consume::Yes
            }

            // shift: * -> Literal
            ([(span, c), ..], _) if !is_special_char(*c) => {
                stack.push(Literal { span: *span });
                Consume::Yes
            }

            // reduce: Literal -> Node
            (_, [.., Literal { span }]) => {
                let node = AstNode {
                    span: *span,
                    kind: AstNodeKind::Literal(&pattern[span.start..span.end]),
                };

                let id = AstNodeId(nodes.len());
                nodes.push(node);

                stack.pop();
                stack.push(Node(id));
                Consume::No
            }

            // ----- Concat -----

            // reduce: Node + Node -> Concat
            (_, [.., Node(fst_id), Node(snd_id)]) => {
                let node = Concat(vec![*fst_id, *snd_id]);

                let _ = (stack.pop(), stack.pop());
                stack.push(node);
                Consume::No
            }

            // reduce: Concat + Node -> Concat
            (_, [.., Concat(n_ids), Node(id)]) => {
                n_ids.push(*id);
                stack.pop(); // Remove Node from the stack
                Consume::No
            }

            // reduce: Concat -> Node
            ([] | [(_, ends_concat!()), ..], [.., Concat(n_ids)]) => {
                let fst = n_ids.first().unwrap();
                let lst = n_ids.last().unwrap();
                let node = AstNode {
                    span: nodes[fst.0].span.join(nodes[lst.0].span),
                    kind: AstNodeKind::Concat(mem::take(n_ids)),
                };

                let id = AstNodeId(nodes.len());
                nodes.push(node);

                // Remove the existing Concat & replace it:
                stack.pop();
                stack.push(Node(id));
                Consume::No
            }

            // ----- Alternate -----

            // reduce: Alternate (empty end) + Node -> Alternate (nonempty end)
            (_, [.., Alternate { span, alts, trailing_pipe }, Node(id)]) if *trailing_pipe => {
                *span = span.join(nodes[id.0].span);
                alts.push(Some(*id));
                *trailing_pipe = false;
                stack.pop();
                Consume::No
            }

            // shift: Node + "|" -> Alternate
            ([(span, '|'), ..], [.., Node(id)]) => {
                let span = nodes[id.0].span.join(*span);
                let alt = Alternate {
                    span,
                    alts: vec![Some(*id)],
                    trailing_pipe: true,
                };

                stack.pop();
                stack.push(alt);
                Consume::Yes
            }

            // shift: [group start] + "|" -> Alternate
            ([(span, '|'), ..], [.., Group { .. }] | []) => {
                let alt = Alternate {
                    span: *span,
                    alts: vec![None], // single alt for the empty start
                    trailing_pipe: true,
                };
                stack.push(alt);
                Consume::Yes
            }

            // shift: Alternate (nonempty end) + "|" -> Alternate (empty end)
            ([(p, '|'), ..], [.., Alternate { span, trailing_pipe, .. }]) if !*trailing_pipe => {
                *span = span.join(*p);
                *trailing_pipe = true;
                Consume::Yes
            }

            // reduce: Alternate -> Node
            ([] | [(_, ends_alt!()), ..], [.., Alternate { span, alts, trailing_pipe }]) => {
                if *trailing_pipe {
                    alts.push(None);
                }

                let node = AstNode {
                    span: *span,
                    kind: AstNodeKind::Alternate(mem::take(alts)),
                };

                let id = AstNodeId(nodes.len());
                nodes.push(node);

                stack.pop();
                stack.push(Node(id));
                Consume::No
            }

            // ----- Group -----

            // shift: '(' -> Group
            ([(span, '('), ..], _) => {
                stack.push(Group { start_span: *span });
                Consume::Yes
            }

            // shift: Group + Node + ')' -> Node
            ([(span, ')'), ..], [.., Group { start_span }, Node(id)]) => {
                let node = AstNode {
                    span: start_span.join(*span),
                    kind: AstNodeKind::Group(Some(*id)),
                };

                let id = AstNodeId(nodes.len());
                nodes.push(node);

                // Remove the Group and Node
                let _ = (stack.pop(), stack.pop());
                stack.push(Node(id));
                Consume::Yes
            }

            // shift: Group + ')' -> Node
            ([(span, ')'), ..], [.., Group { start_span }]) => {
                let node = AstNode {
                    span: start_span.join(*span),
                    kind: AstNodeKind::Group(None),
                };

                let id = AstNodeId(nodes.len());
                nodes.push(node);

                stack.pop();
                stack.push(Node(id));
                Consume::Yes
            }

            // ----- Errors -----

            // * + ')' -> Error
            ([(span, ')'), ..], _) => {
                return Err(SyntaxError {
                    span: *span,
                    msg: SyntaxErrorKind::UnexpectedCloseDelim { name: "parenthesis" },
                    help: &[HelpMsg::EscapeToMatchLiteral {
                        name: "close parenthesis",
                        escaped: r"\)",
                    }],
                })
            }

            // ----- Nothing else applies -----
            #[allow(unused_variables)]
            (next, _) => {
                // // for debugging the parser, it can be useful to print the stack:
                // println!("stack: {:?}", stack);
                // println!("next: {:?}", next);
                unreachable!();
            }
        };

        if let Consume::Yes = consume {
            chars.consume();
        }
    }
}

struct CharPeeker<'s> {
    original_len: usize,
    next_len: usize,
    next: [(Span, char); 2],
    chars: Peekable<CharIndices<'s>>,
}

const DEFAULT_NEXT: (Span, char) = (Span { start: usize::MAX, end: usize::MAX }, '\0');

impl<'s> CharPeeker<'s> {
    fn new(string: &'s str) -> Self {
        let original_len = string.len();
        let mut next = [DEFAULT_NEXT; 2];
        let mut chars = string.char_indices().peekable();

        let mut next_len = 0;

        if let Some((x_start, x_ch)) = chars.next() {
            let x_end = match chars.next() {
                None => original_len,
                Some((y_start, y_ch)) => {
                    let y_end = chars.peek().map(|&(idx, _)| idx).unwrap_or(original_len);
                    next[1] = (Span { start: y_start, end: y_end }, y_ch);
                    next_len += 1;
                    y_start
                }
            };

            next_len += 1;
            next[0] = (Span { start: x_start, end: x_end }, x_ch);
        }

        CharPeeker { original_len, next_len, next, chars }
    }

    fn peek2(&self) -> &[(Span, char)] {
        &self.next[..self.next_len]
    }

    /// Marks the next character as consumed, advancing the items in `peek2` by one
    fn consume(&mut self) {
        self.next = [self.next[1], DEFAULT_NEXT];
        if let Some((start, ch)) = self.chars.next() {
            let end = self
                .chars
                .peek()
                .map(|&(idx, _)| idx)
                .unwrap_or(self.original_len);
            self.next[1] = (Span { start, end }, ch);
        } else {
            self.next_len -= 1;
        }
    }
}
