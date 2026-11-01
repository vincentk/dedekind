/**
 * @file src/test/cpp/modules/dedekind/python/research_binding_times.cpp
 * @brief Research, not a showcase: Listing 5's count at binding times and
 *        loop types, to see what the optimiser does with each.
 *
 * The axes:
 *   - which symmetry: restriction sites (the reverse complement) or plain
 *     palindromes (the reversal), chosen @b statically (a template
 *     parameter, so only one instantiation exists) or @b dynamically (a
 *     run-time argument selecting between two);
 *   - the count word @c N the lengths, offsets and counts are in: 32 or 64
 *     bits (a narrow type a hint that the loops are short; the counts wrap
 *     modulo the word, so only the 64-bit values are the counts);
 *   - the stage: both lengths free (a), the site's length bound to EcoRI's
 *     six (b), the genome's length bound to twelve (b′).
 * The fixture script emits each at -O2 and -O3 and tabulates the IR; nothing
 * is asserted of the optimiser.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <cstddef>
#include <cstdint>

import dedekind.category;

using dedekind::category::letter_map;
using dedekind::category::letters;
using dedekind::category::occurrences;
using dedekind::category::power;
using dedekind::category::reverse;
using dedekind::category::slice;
using dedekind::category::symmetric;

namespace {
constexpr auto capitals = slice(std::byte{'A'}, std::byte{'['});
constexpr auto nucleotides = capitals >> letters("ACGT");
constexpr auto complement = letter_map("ACGT", "TGCA");

// The sites of length n in all genomes of length L, with their positions:
// restriction sites if complemented, else palindromes.
template <typename N, bool Complemented>
constexpr N sites(N L, N n) {
  if constexpr (Complemented) {
    return occurrences(
        power<N>(nucleotides, L),
        symmetric(power<N>(nucleotides, n), power(complement) >> reverse));
  } else {
    return occurrences(power<N>(nucleotides, L),
                       symmetric(power<N>(nucleotides, n), reverse));
  }
}
// The same, the symmetry chosen at run time.
template <typename N>
constexpr N sites(N L, N n, bool complemented) {
  return complemented ? sites<N, true>(L, n) : sites<N, false>(L, n);
}
// 7 · 4^3 · 4^6 either way: a site and a palindrome of even length both
// merge their legs in pairs.
static_assert(sites<std::uint64_t, true>(12, 6) == 1'835'008 &&
              sites<std::uint64_t, false>(12, 6) == 1'835'008 &&
              sites<std::uint64_t, true>(12, 7) == 0 &&
              sites<std::uint64_t, false>(12, 7) == 1'572'864 &&
              sites<std::uint64_t>(12, 6, true) == 1'835'008);
}  // namespace

// One symbol per variant, so the IR is read function by function:
// research_<stage>_<static|dynamic>_<width>.
#define DEDEKIND_RESEARCH_VARIANTS(T, W)                                       \
  extern "C" __attribute__((noinline)) T research_a_static_##W(T L, T n) {     \
    return sites<T, true>(L, n);                                               \
  }                                                                            \
  extern "C" __attribute__((noinline)) T research_b_static_##W(T L) {          \
    return sites<T, true>(L, T{6});                                            \
  }                                                                            \
  extern "C" __attribute__((noinline)) T research_bp_static_##W(T n) {         \
    return sites<T, true>(T{12}, n);                                           \
  }                                                                            \
  extern "C" __attribute__((noinline)) T research_a_dynamic_##W(T L, T n,      \
                                                                bool c) {      \
    return sites<T>(L, n, c);                                                  \
  }                                                                            \
  extern "C" __attribute__((noinline)) T research_b_dynamic_##W(T L, bool c) { \
    return sites<T>(L, T{6}, c);                                               \
  }                                                                            \
  extern "C" __attribute__((noinline)) T research_bp_dynamic_##W(T n,          \
                                                                 bool c) {     \
    return sites<T>(T{12}, n, c);                                              \
  }

// 8 and 16 bits are no count words (IsCountWord): their products promote to
// int.  Narrow loop types beside a wide count would take a separate index
// type in the library.
DEDEKIND_RESEARCH_VARIANTS(std::uint32_t, u32)
DEDEKIND_RESEARCH_VARIANTS(std::uint64_t, u64)
