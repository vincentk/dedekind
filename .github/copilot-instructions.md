# Review instructions

This repository is a C++23 library written under three rules, the Juliet
Posture (`docs/paper/jlt/`), and a linter that checks them
(`tools/jlt-lint/`).  Beyond the usual review, read the following with every
pull request.

## What the program takes on trust

`tools/jlt-lint/axioms.txt` is the ledger of the library's axioms: every entry
of a law (a trait) at one type whose value is `true`.  No proof stands behind
an axiom; the compiler believes it and selects code by it.  For every line the
pull request **adds** or **alters** in that file, check the claim at its
declaration and say, in a comment on that line:

- **Is it true?**  Read the law's definition (the trait's primary template and
  its documentation) and the type's operations.  A false axiom is a bug: the
  compiler will select code whose correctness rests on it.  Give a
  counterexample where you find one (two values, and what the law says of
  them).
- **Is it needed?**  Does any code read the law at this type, or does it
  follow from other entries (a conditional law at a form, an entry at a part)?
  A redundant axiom is a finding.
- **Is it there twice?**  The same law at the same type, or at a type that is
  the same after aliases.

A removed line needs no review unless the law is still read at that type.

`tools/jlt-lint/laws.txt` is the ledger of the conditional laws: every other
entry of a law that can make it true, an entry at a form
(`template <typename F, typename G> injective<Then<F, G>> = injective<F> &&
injective<G>`) or one whose value is computed.  Each is a rule the compiler
applies at every instance of its form, again with no proof behind it.  For
every line the pull request adds or alters there, check:

- **Does the law follow** at every instance of the form, from the clause
  after `requires` and the value after `=`?  A rule that is true of the
  intended instances but not of all of them is a finding: give the instance.
- **Is it needed, and is it there once?**  As for axioms.

A change that adds or alters an entry of a law at one type, set to `true`,
without changing `axioms.txt`, is a finding: the ledger is held by CI
(`tools/jlt-lint/axioms.py --check`, which holds `laws.txt` too), and the
two must move together.

## The linter's gates

`tools/jlt-lint/scope` pins counts per file.  A changed number there is a
change of verdict: say whether the pull request's description accounts for
it, and treat a count that grows (a refusal, an undecided definition, a
finding of a deny-rule) without a reason as a finding.
