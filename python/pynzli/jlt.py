"""Jlt: the arrow DSL (#961) — involutive endomorphisms over two objects.

A fluent, point-free calculus over the *real* category arrows, exposed as
duck-typed handles: an "arrow" is the protocol ``__call__`` / ``__rshift__``
plus the free functions ``dom`` / ``cod``, not a base class.  The objects are
the two carriers ``bool`` and ``int``; the primitive arrows on a carrier ``T``
are ``id(T)`` (identity), ``refl(T)`` (:logic's reflection involution
``logic_complement``: ``not`` (¬) on ``bool``, the order-reversing ``~`` on the
``int`` chain), and the NNO step ``succ(T)`` / ``pred(T)`` (#1001), total and
saturating at the chain's bounds: ``succ(bool)(True) is True``.

    from pynzli.jlt import id, refl, dom, cod

    (id(bool) >> refl(bool))(False)   # True   -- compose, then apply
    (refl(int) >> refl(int))(5)       # 5      -- refl is an involution: r∘r = id
    dom(id(int)) is int               # True   -- dom/cod are type objects
    id(bool) >> refl(int)             # TypeError -- not composable (bool ≠ int)

The tensor exhibit (#1107) adds handles to ``:periodic``'s tensors: an
``alphabet("ACGT")``, its ``power(a, n)`` (the words of length n, never built),
``free_monoid(a, L)`` (the words shorter than L), and ``symmetric(p, g)``, or
``p & fixed_by(g)`` as Trsk spells a set under a predicate (the
words a symmetry fixes: ``reverse``, ``rotate(d)``, or an arrow on every
letter then the reversal, ``power(complement) >> reverse``, the reverse
complement, where the complement is the alphabet's ``dual``, its one
order-reversing bijection, or a ``letter_map`` written out; ``is_involution``
asks its law, by the type for ``dual``, on the table for a ``letter_map``).  ``count_factors`` gives a count as factors, each a
popcount; ``count`` its residue modulo 2**64; ``exact_count`` the count or
``None`` where no word holds it, so a genome of 2**200 words is refused as a
sequence and counted as a tensor.  ``*`` is the outer product.

A ``window(i, n)`` projects a word onto its letters i to i + n - 1;
``cylinder(p, w, s)`` is the words of ``p`` whose window lies in ``s``, its count
pushed down (the set's, times the alphabet's for each free leg); and
``occurrences(p, s)`` counts ``s`` in ``p``'s words with its positions,
``exact_occurrences`` the same or ``None``.  ``join(p, s)`` is the same
count as a query's: the windows' offsets times the cylinder on each, a
tensor whose ``count`` is the product of its factors.  The Jlt paper's
Listing 5, the restriction sites (words equal to their reverse complement)
in all genomes, the README's text between the markers:

.. [listing5:all-genomes]

>>> nucleotides = alphabet("ACGT")
>>> complement = dual(nucleotides)
>>> reverse_complement = map(complement) >> reverse
>>> sequences = words(nucleotides)
>>> palindromes = sequences & fixed_by(reverse_complement)
>>> def sites(n):
...     return palindromes & length(n)
>>> def cuts(N, n):
...     return count(join(sequences & length(N), sites(n)))
>>> cuts(12, 6), cuts(32, 6) == 27 << 58
(1835008, True)

.. [/listing5:all-genomes]

>>> cuts(12, 7)  # no letter is its own complement
0
>>> exact_count(join(power(nucleotides, 40), sites(6))) is None  # past 2**64
True
>>> occurrences(power(nucleotides, 12), sites(6))  # the same count
1835008

The second panel of Listing 5 (#1135) scans **one** sequence, the caller's
``bytes``, read where they are.  Two lines count the same sites: the query,
the sequence's hexamers met with the sites, ``windows(6) & sites(6)`` a stage
named before any sequence is given; and the plan, the matcher
specialised to the section's words (``automaton``, the minimal automaton,
built here at run time by the function C++ runs at compile time).  A plan
is a value: ``==`` compares the languages, ``repr`` is its EXPLAIN.

.. [listing5:one-genome]

>>> sequence = b"TTGAATTCAAGCTTA"
>>> hexamers = windows(6)
>>> cut_sites = hexamers & sites(6)
>>> count(sequence >> cut_sites)
2
>>> count(sequence >> rank(nucleotides) >> matches(automaton(sites(6))))
2

.. [/listing5:one-genome]

>>> plan = automaton(sites(6))
>>> plan
automaton(states=277, accepting=64)
>>> count(sequence >> rank(nucleotides) >> matches(plan))  # the stage form: >> counting
2
>>> collect(sequence >> rank(nucleotides) >> matches(plan))  # where a site ends
[7, 13]
>>> collect(sequence >> cut_sites)  # the restriction map: where each site begins
[2, 8]
>>> sites(6)(b"GAATTC"), sites(6)(b"GAATTA"), sites(6)(b"GAATT")
(True, False, False)
>>> count(b"GAANTTC" >> rank(nucleotides) >> matches(plan))  # N breaks it
0
>>> count(b"GAANTTC" >> rank(nucleotides) >> matches(plan, Holes.skip))
1
>>> plan == automaton(sites(6))
True

Arrows are **extensional** (functions): ``id(T) >> f`` and ``f`` are equal on
every input.  Composability (``cod(f) == dom(g)``) is enforced structurally --
composing across objects raises.  Structural reduction (``simplify`` / ``cata``)
is *intensional* and lives on the C++ side; it is vacuous here.  ``id`` /
``refl`` are the real ``:morphism`` / involution arrows, witnessing ``IsArrow``
(and ``IsEndomorphism`` / ``IsInvolution``) in C++.
"""

# `_jlt` is the exhibit's own private native extension module (pynzli._jlt),
# NumPy-style; this pure-Python facade re-exports it under the public name.
from ._jlt import alphabet
from ._jlt import automaton
from ._jlt import cod
from ._jlt import collect
from ._jlt import count
from ._jlt import counting
from ._jlt import count_factors
from ._jlt import cylinder
from ._jlt import dom
from ._jlt import dual
from ._jlt import exact_count
from ._jlt import exact_occurrences
from ._jlt import fixed_by
from ._jlt import free_monoid
from ._jlt import Holes
from ._jlt import id
from ._jlt import is_involution
from ._jlt import is_square
from ._jlt import join
from ._jlt import length
from ._jlt import letter_map
from ._jlt import map
from ._jlt import matches
from ._jlt import occurrences
from ._jlt import pack
from ._jlt import power
from ._jlt import pred
from ._jlt import rank
from ._jlt import refl
from ._jlt import reverse
from ._jlt import rotate
from ._jlt import succ
from ._jlt import symmetric
from ._jlt import where
from ._jlt import window
from ._jlt import windows
from ._jlt import words

__all__ = [
    "alphabet",
    "automaton",
    "cod",
    "collect",
    "count",
    "counting",
    "count_factors",
    "cylinder",
    "dom",
    "dual",
    "exact_count",
    "exact_occurrences",
    "fixed_by",
    "free_monoid",
    "Holes",
    "id",
    "is_involution",
    "is_square",
    "join",
    "length",
    "letter_map",
    "map",
    "matches",
    "occurrences",
    "pack",
    "power",
    "pred",
    "rank",
    "refl",
    "reverse",
    "rotate",
    "succ",
    "symmetric",
    "where",
    "window",
    "windows",
    "words",
]
