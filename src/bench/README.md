# The Listing 5 benchmark

How fast is the scan of [Listing 5](../../README.md), and why? This counts the palindromic hexamers in a text of DNA, four ways, and keeps the compiler's output beside the times. Tracked in [#1154](https://github.com/vincentk/dedekind/issues/1154).

Figures are from one run on one shared GitHub runner, an AMD EPYC 9V74, Clang 23, `-O2`, unless a row says otherwise. Absolute numbers move between runners; ratios within a run are what to quote.

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

One core, baseline SSE2, from raw ASCII. *The table* is Knuth, Morris and Pratt's matcher, for many words Aho and Corasick's, generic over the alphabet. *The law* embeds the alphabet in the vector unit's operations.

| Counting the sites | ns per letter | Relative |
|---|---:|---:|
| The library, `count(text >> pali6)`, by the law | 0.39 | 1 |
| `pynzli`, by the law, the same | 0.38 | 1 |
| The library or `pynzli`, by the table | 1.9 | 5× |
| The same table, a hand-written loop, for reference | 1.8 | 4.6× |
| Each window asked, as defined | 21 | 54× |
| Python, a loop over a set | 121 | 310× |
| Python, `re` with 64 alternatives in a lookahead | 173 | 450× |

On 3.1 billion letters, about a human genome, past the cache: a plain read of the same memory at 33 GB/s, the law at 2.6 GB/s, the table at 0.52. So about 1.2 seconds, by calculation, and the scan is bound by the core and not by memory. The scan's own storage is fixed, a block's worth for the law and one state for the table, and the text is read where it lies. Drawn rather than stored, by the table, peak memory for the whole process stays at about 10.7 MB from 10^7 to 10^10 letters. All rows count the same number of sites, and the job fails if they do not.

Drawing a letter from `random_text` costs 2.45 ns, and drawing and scanning it by the table 2.58: the scan adds about 5% to making its own input.

`re` is a backtracking engine, here as what a Python user reaches for and not as regular expressions at large; RE2 and Hyperscan are not measured.

## Why it is fast

A vector unit can only help when two things hold:

1. **No position depends on another.** A table scan fails this, as each state needs the last. The law `fixed_by(~rev)` at a fixed length gives it: a window is independent pairs.
2. **The data can be batched.** Contiguous small values, and operations the unit has. A 256-entry byte lookup fails this; comparing a byte with four letters does not.

So `count` has one more overload, keyed on what the section's type already says, `Symmetric<Power<std::byte, Letters, N>, Reverse, Reflection>`. A block of the text goes through in stages, each small enough for the compiler to see as a loop: ranks, one pass per pair, a sum. Ranks that reflect sum to three, which the encoding carries into the machine's arithmetic. There are no intrinsics and no hand-written loops, the alphabet and the length are values, so Python gets it too, and any other shape takes the plain fold, whose answer a test holds equal.

**The rule:** a specialization is keyed on a law or a data shape, never on a particular problem or compiler. It guarantees the answer; vectorising stays the compiler's choice.

The overload is narrow, DNA's and RNA's in practice: exactly four letters, a reflection, a word of even length up to 16. It names a law of biochemistry and no instruction of any machine. C++26's `std::simd` would make the lanes explicit.

## What did not work

| Attempt | What happened |
|---|---|
| The guard inside one window predicate | Never inlined: a call per window, slower than the table |
| A 64-bit counter | Vectorises, but about 4 windows a pass, as each count widens to a 64-bit lane |
| One bit per base in bytes | At baseline, slower than the table: reversing bits a byte at a time is dear. Fast at `-march=native` |

## Reading the compiler

`-O3` once made the library's table 2.5 times slower than a hand-written loop on the same table, and `-march=native` was not to blame. In the assembly:

```
movups (%rdx),%xmm0     16-byte load of the cursor...
movups %xmm0,(%r11)
mov    %r9w,0x20(%rsi)  ...which a 2-byte and an 8-byte store wrote one step earlier
mov    %r12,0x28(%rsi)
```

A load spanning two earlier stores can't be forwarded, so it waits for them, on every letter: a store-forwarding stall. The source was `unfold_view`'s iterator, copying a state only so that two positions could compare; they now compare by a step count. That took the table to within 2 to 12% of the loop at every flag setting, halved "drawn and scanned", and removed code.

## Oddities and open questions

Each is one `make bench` away. Pull requests welcome.

- The law's gain from `-O3 -march=native` depends on the CPU: 1.3× on an AVX2 EPYC 7763, while on an AVX-512 EPYC 9V74 it gets slower, 0.39 to 0.44 ns, though it does use the wider registers. The unblocked fold on ranks gains 4× there, 0.76 to 0.18. What limits the block's stages?
- Why is a hash and a mask cheaper than the hash alone at baseline, 1.06 against 1.36 ns?
- A bare hash falls to 0.19 ns with native flags, as the multiplies vectorise, while `random_text` stays at 2.45 ns.
- A set of bases per byte goes from the slowest law row to a fast one with native flags, 3.6 to 0.20 ns.
- Two bits a base, one word holding 27 windows, reaches 0.06 ns per base at native flags and isn't in the library.
- [ ] `llvm-mca` on the hot loops, and full `lscpu` with each run.
- [ ] A real assembly, with the file named and its lowercase and `N` runs handled.

## Machines

Shared runners vary: a plain read ranged from 19 to 42 GB/s across runs. Quote ratios within a run, name the CPU, and take absolute figures from one known machine.
