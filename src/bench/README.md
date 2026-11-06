# The Listing 5 benchmark

How fast is the scan of [Listing 5](../../README.md), and why? This counts the palindromic hexamers in a text of DNA, four ways, and keeps the compiler's output beside the times. Tracked in [#1154](https://github.com/vincentk/dedekind/issues/1154).

> **Work in progress.** Figures are ballpark, from shared GitHub runners, and will be replaced by one run with the machine named.

## Kick the tyres

From Python, with nothing to build where a wheel fits:

```bash
pip install pynzli
```

```python
import random, re, time
from pynzli import jlt

bases = jlt.abc("ACGT")
pali6 = jlt.words(bases) & jlt.len(6) & jlt.fixed_by(~jlt.rev)
text = bytes(random.Random(0).choices(b"ACGT", k=10_000_000))

start = time.perf_counter()
print(jlt.count(text >> pali6), time.perf_counter() - start)
```

To compare with what Python offers by itself, [`python/listing5_bench.py`](python/listing5_bench.py) adds `re` and a loop over a set, and asserts that all of them count the same.

From C++, with LLVM clang 22 or later:

```bash
make bench         # what a wheel ships: -O2, no coverage; fails on a regression
make bench-native  # the same at -O3 -march=native, for information
```

Results and disassembly land in `build/bench/`. In CI, the `benchmark` job runs on main and on PRs labelled `benchmark`, and uploads them as an artifact.

## The result

One core, baseline SSE2, from raw ASCII.

| Counting the sites | ns per letter | GB/s |
|---|---:|---:|
| A plain read of the same memory, for scale | under 0.05 | 20–40 |
| The library, `count(text >> pali6)`, by the law | 0.3–0.4 | 2.5–3.9 |
| The library, by its table | about 2 | about 0.5 |
| Python, a loop over a set | 100–150 | 0.01 |
| Python, `re` with 64 alternatives in a lookahead | 200–250 | 0.005 |

A text the size of a human genome, 3.1 billion letters, takes about a second, and peak memory stays at about 10.7 MB from 10^7 to 10^10 letters, as the text is read where it lies. All rows count the same number of sites.

`re` is a backtracking engine and is here as what a Python user reaches for, not as regular expressions at large; RE2 and Hyperscan are not measured.

## Why it is fast

A vector unit can only help when two things hold:

1. **No position depends on another.** A table scan fails this, as each state needs the last. The law `fixed_by(~rev)` at a fixed length gives it: a window is independent pairs.
2. **The data can be batched.** Contiguous small values, and operations the unit has. A 256-entry byte lookup fails this; comparing a byte with four letters does not.

So `count` has one more overload, keyed on what the section's type already says, `Symmetric<Power<std::byte, Letters, N>, Reverse, Reflection>`. A block of the text goes through in stages, each small enough for the compiler to see as a loop: ranks, one pass per pair, a sum. Ranks that reflect sum to three, which the encoding carries into the machine's arithmetic. There are no intrinsics and no hand-written loops, the alphabet and the length are values, so Python gets it too, and any other shape takes the plain fold, whose answer a test holds equal.

**The rule:** a specialization is keyed on a law or a data shape, never on a particular problem or compiler. It guarantees the answer; vectorising stays the compiler's choice.

## What did not work

| Attempt | What happened |
|---|---|
| The guard inside one window predicate | Never inlined: a call per window, slower than the table |
| A 64-bit counter | Vectorises, but about 4 windows a pass, as each count widens to a 64-bit lane |
| One bit per base in bytes | At baseline, slower than the table: reversing bits a byte at a time is dear. Fast at `-march=native` |

## Reading the compiler

`-O3` once made the library's table 2.5 times slower than a hand-written loop on the same table, with `-march=native` not to blame. In the assembly:

```
movups (%rdx),%xmm0     16-byte load of the cursor...
movups %xmm0,(%r11)
mov    %r9w,0x20(%rsi)  ...which a 2-byte and an 8-byte store wrote one step earlier
mov    %r12,0x28(%rsi)
```

A load spanning two earlier stores can't be forwarded, so it waits for them, on every letter: a store-forwarding stall. The source was `unfold_view`'s iterator, copying a state only so that two positions could compare; they now compare by a step count.

## Open

- [ ] Whether that fix closes the gap at `-O3`, and the rest of the gap in "drawn and scanned".
- [ ] Why the law's plan gains nothing from `-march=native` while simpler loops gain 4×.
- [ ] `llvm-mca` on the hot loops, and full `lscpu` with each run.
- [ ] A real assembly, with the file named and its lowercase and `N` runs handled.

## Machines

Shared runners vary: a plain read ranged from 19 to 42 GB/s across runs. Quote ratios within a run, name the CPU, and take absolute figures from one known machine.
