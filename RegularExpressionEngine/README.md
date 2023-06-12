
Part 1: NFAs

The first part of the project involved implementing functions for working with NFAs. The goal was to implement the move and epsilon closure functions, as well as an accept function.

An NFA (Non-Deterministic Finite Automaton) is represented by a 5-tuple (Σ, Q, q0, F, δ), where:

Σ is a finite alphabet,
Q is a finite set of states,
q0 ∈ Q is the start state,
F ⊆ Q is the set of accept states, and
δ : Q × (Σ ∪ {ε}) → 𝒫(Q) is the transition function.
In the implementation, the NFA is represented using record syntax in OCaml with the following type:

type ('q, 's) transition = 'q * 's option * 'q
type ('q, 's) nfa_t = {
sigma : 's list;
qs : 'q list;
q0 : 'q;
fs : 'q list;
delta : ('q, 's) transition list;
}

The type transition represents NFA transitions, where each transition is a 3-tuple (q1, s, q2), indicating a transition from state q1 to state q2 on symbol s. The NFA itself is represented by the type nfa_t, which contains the alphabet sigma, the set of states qs, the start state q0, the set of accept states fs, and the list of transitions delta.

The functions implemented for working with NFAs are as follows:

move nfa qs s:
This function takes an NFA, a set of initial states (qs), and a symbol option (s). It returns the set of states that the NFA might be in after making one transition on the symbol (or epsilon if None), starting from any of the initial states.
e_closure nfa qs:
This function takes an NFA and a set of initial states (qs). It returns the set of states that the NFA might be in after making zero or more epsilon transitions, starting from the initial states.
accept nfa s:
This function takes an NFA and a string (s). It determines whether the NFA accepts the string by simulating the NFA's transitions based on the input string and checking if it reaches an accept state.
Part 2: DFAs

Part2:

In the second part of the project, the goal was to implement the nfa_to_dfa function, which converts an NFA to a DFA using the subset construction algorithm. The DFA is represented by the type ('q list, 's) nfa_t, where the states are sets of states from the NFA.

To implement the nfa_to_dfa function, several helper functions were suggested:

new_states nfa qs:
Given an NFA and a DFA state (represented as a set of states), this function computes all the DFA states that can be reached from a transition out of qs, including the dead state.
new_trans nfa qs:
Given an NFA and a DFA state, this function computes all the transitions coming from qs in the DFA.
new_finals nfa qs:
Given an NFA and a DFA state, this function determines if qs is a final state in the DFA.
The nfa_to_dfa function itself converts an NFA to an equivalent DFA by using the subset construction algorithm. The language recognized by the NFA remains the same after conversion.



Part 3: Regular Expressions

In this part of the project, the goal is to convert a regular expression into an NFA (Nondeterministic Finite Automaton). The Regexp module defines a type called regexp_t to represent regular expressions. Here's a description of the regexp_t type:

Empty_String: Represents the regular expression that recognizes the empty string (ε).
Char c: Represents the regular expression that accepts the single character c.
Union (r1, r2): Represents the regular expression that is the union of r1 and r2.
Concat (r1, r2): Represents the regular expression that is the concatenation of r1 followed by r2.
Star r: Represents the regular expression that is the Kleene closure of r.
To convert a regular expression regexp to an NFA, the function regexp_to_nfa needs to be implemented. This function takes a regexp as input and returns an NFA (nfa_t) that accepts the same language as the regular expression.
