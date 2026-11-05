/**
 * @file src/test/cpp/modules/dedekind/python/showcase_14_ecori_table.cpp
 * @brief Showcase 14 — the Jlt paper's Listing 5, its right panel: a
 *        restriction site's table, by binding time.
 *
 * EcoRI recognises GAATTC.  Told so, the compiler builds the matcher's table,
 * Knuth, Morris and Pratt's, and what remains is one pass over the sequence.
 * The same library code at the divisions of table and sequence:
 *   (a) @c witness_windows(s, n): no table, each window tested;
 *   (b) @c witness_table_at_run_time(w, s, n): the site a value, so the table
 *       is built by the call;
 *   (c) @c witness_ecori_cuts(s, n): the table a constant, the sequence not;
 *   (d) @c witness_ecori_cuts_on_dna(): both known to the optimiser alone;
 *   (e) @c witness_ecori_dna(): both bound by @c constexpr, a constant.
 * The functions are @c extern "C" and never inlined, so each is its own symbol
 * in the IR the fixture script reads at @c -O2.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>

import dedekind.category;

using dedekind::category::abc;
using dedekind::category::automaton;
using dedekind::category::bytes;
using dedekind::category::fixed_by;
using dedekind::category::Letters;
using dedekind::category::matches;
using dedekind::category::rev;
using dedekind::category::site;
using dedekind::category::Site;
using dedekind::category::windows;
using dedekind::category::word_index;
using dedekind::category::words;

namespace {
constexpr auto bases = abc("ACGT");
constexpr auto sites = words(bases) & windows(6) & fixed_by(~rev);
constexpr auto dna = bytes("TTGAATTCNNNNNNNAAGCTTA");
constexpr auto ecori = *automaton(site(bases, "GAATTC"));
static_assert(count(dna >> matches(ecori)) == 1 &&
              count(dna >> windows(6) & sites) == 2);
}  // namespace

/** @brief (a) No table: each window tested against the 64 sites. */
extern "C" __attribute__((noinline)) std::uint64_t witness_windows(
    const std::byte* s, std::size_t n) {
  return count(std::span<const std::byte>{s, n} >> windows(6) & sites);
}
/** @brief (b) The site known only at run time: the same plan, built here. */
extern "C" __attribute__((noinline)) std::uint64_t witness_table_at_run_time(
    const char* word, const std::byte* s, std::size_t n) {
  const Letters on = bases.coalgebra().f;
  const auto plan =
      automaton(Site<6>{on, word_index(on, std::string_view{word, 6})});
  return count(std::span<const std::byte>{s, n} >> matches(plan)).value_or(0);
}
/** @brief (c) The table a constant, the sequence a value: the residual. */
extern "C" __attribute__((noinline)) std::uint64_t witness_ecori_cuts(
    const std::byte* s, std::size_t n) {
  return count(std::span<const std::byte>{s, n} >> matches(ecori));
}
/** @brief (d) Both known, nothing bound: what the optimiser finds alone. */
extern "C" __attribute__((noinline)) std::uint64_t witness_ecori_cuts_on_dna() {
  return count(dna >> matches(ecori));
}
/** @brief (e) Both bound by @c constexpr: the constant in the IR. */
extern "C" __attribute__((noinline)) std::uint64_t witness_ecori_dna() {
  constexpr std::size_t cuts = count(dna >> matches(ecori));
  return cuts;
}
