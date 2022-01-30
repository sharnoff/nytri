//! Representation and construction of NFAs for use in the matching engine
//!
//! The NFA implementation here is originally based on the sample implementation by Russ Cox, as
//! part of a series of articles on regular expressions. See: <https://swtch.com/~rsc/regexp>,
//! particularly ["Regular Expression Matching Can Be Simple And Fast"] and the
//! [sample NFA implementation].
//!
//! ["Regular Expression Matching Can Be Simple And Fast"]: https://swtch.com/~rsc/regexp/regexp1.html
//! [sample NFA implementation]: https://swtch.com/~rsc/regexp/nfa.c.txt

use crate::compile::{Ast, AstNode, AstNodeId, AstNodeKind, ErrorKind};
use std::collections::HashSet;
use std::mem;
use std::ops::Index;

#[derive(Debug)]
pub struct Nfa {
    // The set of starting states, none of which are a `Split`. We have it this way so that
    // matching never has to deal with split states in the set of current states.
    start_states: Vec<StateId>,
    match_state: StateId,
    states: Vec<State>,
}

impl Nfa {
    /// Builds the NFA to represent the pattern's AST
    ///
    /// Currently does not return any errors, though that may change in the future.
    pub fn build(ast: Ast) -> Result<Self, ErrorKind> {
        // How this function works:
        //
        // At the start of building the NFA, we only know one concrete piece of information: the
        // final "match" state — because we construct it. In order to construct each state, they
        // need the "output" states, or the states that they have transitions into.
        //
        // So we construct the NFA from back to front, pushing to `state_stack` with the entrypoint
        // state for each node in the AST. This can be thought of as "returning" in a hypothetical
        // equivalent recursive implementation of the algorithm.
        //
        // Because the recursion would actually be quite complex, we use an auxiliary "instruction
        // stack" to handle various types of control flow & post-processing. For example,
        // Instruction::DoConcat allows us to emulate "looping" backwards through the nodes we're
        // concatenating. Similarly, Instruction::CollectAlternate performs the post-processing
        // that would be necessary if our hypothetical recursive implementation made all the states
        // for the child nodes before joining them together with `Split` states.

        /// A single element in `instrs_stack` - an instruction to indicate what operation to
        /// perform
        ///
        /// The algorithm has finished once
        enum Instruction<'ast> {
            // Instruction to visit an AST node, adding its entry state to `state_stack`. Also
            // given is the state that should be transitioned into from the final state resulting
            // from the node
            Visit { node: &'ast AstNode<'ast>, out: StateId },
            // When popped, pushes the state to `state_stack`. This exists so that we can delay a
            // push to the state stack, keeping the states in order.
            //
            // Realistically, this doesn't matter right now — but it might later, and it'll be
            // useful to have this.
            PhantomLink { state: StateId },
            // Collect the results from creating the states for several alternate states
            CollectAlternate { alts_count: usize },
            // Pop an item off the state stack & concatenate `remaining` to lead to it, eventually
            // pushing the entrypoint back onto the stack.
            //
            // Does nothing if `remaining` is empty.
            DoConcat { remaining: &'ast [AstNodeId] },
        }

        let mut states = vec![State::Match];
        let match_state = StateId(0);

        // Stack of "instructions" for executing the algorithm
        let mut instrs_stack = vec![Instruction::Visit { node: ast.root(), out: match_state }];

        // Stack of completed states, collected (and pushed to) when a node is popped off its stack
        // for the second time
        let mut state_stack = Vec::new();

        while let Some(instr) = instrs_stack.pop() {
            use AstNodeKind::{Alternate, Concat, Group, Literal};
            use Instruction::{CollectAlternate, DoConcat, PhantomLink, Visit};

            match instr {
                Visit { node, mut out } => match &node.kind {
                    Literal(s) => {
                        for c in s.chars().rev() {
                            let id = StateId(states.len());
                            states.push(State::Trans(c, out));
                            out = id;
                        }
                        state_stack.push(out);
                    }
                    Group(None) => state_stack.push(out),
                    Group(Some(id)) => instrs_stack.push(Visit { node: &ast[id], out }),
                    Alternate(ids) => {
                        debug_assert!(ids.len() >= 2);
                        instrs_stack.push(CollectAlternate { alts_count: ids.len() });

                        // It doesn't really matter what order we add alternates in — there's no
                        // backtracking-like preference to deal with right now, so we can do it
                        // however's easiest.
                        for opt_id in ids {
                            match opt_id {
                                Some(id) => instrs_stack.push(Visit { node: &ast[id], out }),
                                None => instrs_stack.push(PhantomLink { state: out }),
                            }
                        }
                    }
                    Concat(ids) => {
                        debug_assert!(ids.len() >= 2);
                        state_stack.push(out);
                        instrs_stack.push(DoConcat { remaining: ids });
                    }
                },
                PhantomLink { state } => state_stack.push(state),
                CollectAlternate { alts_count } => {
                    let mut out = state_stack.pop().unwrap();
                    for _ in 1..alts_count {
                        let next = state_stack.pop().unwrap();
                        let id = StateId(states.len());
                        states.push(State::Split(next, out));
                        out = id;
                    }
                    state_stack.push(out);
                }
                // If there's nothing remaining, then the final value we pushed onto the state
                // stack should be "returned" as the entrypoint to the concatenation - so we leave
                // it as-is.
                DoConcat { remaining: [] } => (),
                DoConcat { remaining: [remaining @ .., last] } => {
                    let out = state_stack.pop().unwrap();
                    instrs_stack.push(DoConcat { remaining });
                    instrs_stack.push(Visit { node: &ast[last], out });
                }
            }
        }

        // At the end of traversing the entire AST, we should be left with just a single state in
        // the stack.
        assert_eq!(state_stack.len(), 1);
        let initial_start_state = state_stack.pop().unwrap();

        // Populate the starting states vector with everything reachable by a Split state through
        // the initial starting state.
        //
        // Initially, `start_states` includes split states. We do a post-processing step to filter
        // out all of the split states.
        let mut start_states = HashSet::new();
        let mut visit_stack = vec![initial_start_state];

        while let Some(id) = visit_stack.pop() {
            // if this state wasn't already in `start_states`, ...
            if start_states.insert(id) {
                if let State::Split(x, y) = states[id.0] {
                    visit_stack.push(x);
                    visit_stack.push(y);
                }
            }
        }

        let start_states = start_states
            .into_iter()
            .filter(|id| match states[id.0] {
                State::Split(_, _) => false,
                _ => true,
            })
            .collect();

        Ok(Nfa { start_states, match_state, states })
    }

    /// Produces a matcher for the NFA, tracking the compiled NFA with the provided reference
    pub fn matcher<R: AsRef<Self>>(this: R) -> Matcher<R> {
        let states = this.as_ref().start_states.clone();
        let last_used = vec![0; this.as_ref().states.len()];

        Matcher { nfa: this, seen: 0, states, last_used }
    }
}

/// Intermediate matching structure, produced from an NFA
pub struct Matcher<N> {
    pub nfa: N,
    // Total count of all of the characters seen
    seen: usize,
    // The current states active at this point in the matching attempt
    states: Vec<StateId>,
    // Mapping of StateId -> last value of `seen` for which it was active. If it was never active,
    // its "last used" value is zero.
    last_used: Vec<usize>,
}

/// The preserved state of the matcher, without any reference to the underlying NFA
///
/// This can be used to save, store, or compare matcher states, which may be useful for a variety
/// of reasons.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct MatcherState {
    seen: usize,
    states: Vec<StateId>,
    last_used: Vec<usize>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct StateId(usize);

#[derive(Debug, Copy, Clone)]
enum State {
    Trans(char, StateId),
    Split(StateId, StateId),
    Match,
}

impl<N: AsRef<Nfa>> Matcher<N> {
    /// Returns a copy of the current state of the matcher
    pub fn get_state(&self) -> MatcherState {
        MatcherState {
            seen: self.seen,
            states: self.states.clone(),
            last_used: self.last_used.clone(),
        }
    }

    /// Constructs a new `Matcher` from a previously-saved state
    pub fn from_state(nfa: N, state: MatcherState) -> Self {
        Matcher {
            nfa,
            seen: state.seen,
            states: state.states,
            last_used: state.last_used,
        }
    }

    /// Feeds the string into the matcher, updating the internal state
    pub fn feed(&mut self, string: &str) {
        for c_in in string.chars() {
            // Pre-increment `seen`, so that we don't add duplicate states.
            self.seen += 1;

            let mut stack = Vec::new();
            for s in mem::replace(&mut self.states, Vec::new()) {
                match self.nfa.as_ref().states.as_slice()[s] {
                    State::Trans(c, new) => {
                        if c == c_in {
                            stack.push(new);
                            self.add_states(&mut stack);
                        }
                    }
                    State::Split(_, _) => unreachable!(),
                    State::Match => (),
                }
            }
        }
    }

    // Adds all of the states from the provided stack, using it as scratch space to avoid expensive
    // recursive calls
    fn add_states(&mut self, stack: &mut Vec<StateId>) {
        while let Some(s) = stack.pop() {
            // If we've already added this state to the current set, don't do anything more.
            if mem::replace(&mut self.last_used[s.0], self.seen) == self.seen {
                continue;
            }

            match self.nfa.as_ref().states.as_slice()[s] {
                State::Trans(_, _) | State::Match => self.states.push(s),
                // `self.states` does not include split states, so we need to "recursively" add the
                // two states this is split into. (We've implemented this in a loop, without actual
                // recursion, to make this more efficient)
                State::Split(x, y) => {
                    stack.push(x);
                    stack.push(y);
                }
            }
        }
    }

    /// Returns true if the full contents of the strings provided so far are a match
    pub fn is_match(&self) -> bool {
        // When using a more advanced state set, we can probably do this in O(1) time. For now,
        // O(n) is ok.

        self.states.contains(&self.nfa.as_ref().match_state)
    }
}

impl Index<StateId> for [State] {
    type Output = State;

    fn index(&self, idx: StateId) -> &State {
        &self[idx.0]
    }
}
