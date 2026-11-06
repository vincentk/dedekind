"""Listing 5 from Python, against what a Python user would otherwise write (#1154).

Run it with an interpreter where pynzli is installed.  Not a test: nothing here
passes or fails on a time, though the three ways must count the same.
"""
import itertools
import random
import re
import statistics
import time

from pynzli import jlt

N = 10_000_000
N_LOOP = 1_000_000  # the interpreter's loop is slow
REPEATS = 5

bases = jlt.abc("ACGT")
pali6 = jlt.words(bases) & jlt.len(6) & jlt.fixed_by(~jlt.rev)
scanner = jlt.automaton(pali6)
text = bytes(random.Random(0).choices(b"ACGT", k=N))

COMPLEMENT = bytes.maketrans(b"ACGT", b"TGCA")
SITES = frozenset(
    w
    for w in map(bytes, itertools.product(b"ACGT", repeat=6))
    if w == w.translate(COMPLEMENT)[::-1]
)
assert len(SITES) == 64
OVERLAPPING = re.compile(b"(?=(" + b"|".join(sorted(SITES)) + b"))")


def with_pynzli(t):
    return jlt.count(t >> scanner)


def with_re(t):
    return sum(1 for _ in OVERLAPPING.finditer(t))


def with_a_loop(t):
    return sum(t[i : i + 6] in SITES for i in range(len(t) - 5))


def drawn_by_pynzli(_):
    return jlt.count(jlt.random_text(bases, N) >> scanner)


def ns_per_letter(call, argument, letters):
    times = []
    for _ in range(REPEATS):
        start = time.perf_counter()
        call(argument)
        times.append((time.perf_counter() - start) * 1e9 / letters)
    return statistics.median(times)


few = text[:N_LOOP]
assert with_pynzli(few) == with_re(few) == with_a_loop(few)

print(f"{N} letters, median of {REPEATS} ({N_LOOP} for the loop).\n")
print("| Row | ns a letter |")
print("|---|---:|")
print(f"| pynzli, the table, from bytes | {ns_per_letter(with_pynzli, text, N):.2f} |")
print(f"| pynzli, drawn and scanned | {ns_per_letter(drawn_by_pynzli, None, N):.2f} |")
print(f"| re, 64 alternatives in a lookahead | {ns_per_letter(with_re, text, N):.2f} |")
print(f"| a Python loop over a set | {ns_per_letter(with_a_loop, few, N_LOOP):.2f} |")
