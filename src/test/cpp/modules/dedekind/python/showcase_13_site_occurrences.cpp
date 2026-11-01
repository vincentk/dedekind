/**
 * @file src/test/cpp/modules/dedekind/python/showcase_13_site_occurrences.cpp
 * @brief Showcase 13 — the Jlt paper's Listing 5: how often a restriction
 *        site occurs in all genomes, by binding time.
 *
 * A restriction site is a word equal to its reverse complement (EcoRI's
 * GAATTC).  In all 4^N genomes of length N, the sites of length n occur
 * (N − n + 1) · 4^(n/2) · 4^(N − n) times, none for odd n: the window's legs
 * merged through the reverse complement (Pólya), every other leg free, at
 * each of the N − n + 1 offsets (aggregate pushdown, R2).
 *
 * The same function at four binding times (Jones, Gomard and Sestoft's
 * division of the inputs into the known and the unknown):
 *   (a) @c witness_sites(N, n): both at run time;
 *   (b) @c witness_sites_of_6(N): the site's length known, EcoRI's six;
 *   (b′) @c witness_sites_in_12(n): the genome's length known, twelve;
 *   (c) @c witness_sites_12_6(): both known, @c ret @c i64 @c 1835008.
 * The functions are @c extern "C" and never inlined, so each is its own
 * symbol in the IR the fixture script reads at @c -O2.
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
// The nucleotides among the capitals, and their Watson–Crick pairing.
constexpr auto capitals = slice(std::byte{'A'}, std::byte{'['});
constexpr auto nucleotides = capitals >> letters("ACGT");
constexpr auto complement = letter_map("ACGT", "TGCA");

// The sites of length n in all genomes of length N, with their positions.
constexpr std::size_t site_occurrences(std::size_t N, std::size_t n) {
  const auto sites =
      symmetric(power(nucleotides, n), power(complement) >> reverse);
  return occurrences(power(nucleotides, N), sites);
}
static_assert(site_occurrences(12, 6) == 1'835'008 &&  // 7 · 4^3 · 4^6
              site_occurrences(12, 7) == 0 &&          // no odd site
              site_occurrences(6, 6) == 64 &&          // 4^3, one window
              site_occurrences(4, 6) == 0);            // no room
}  // namespace

/** @brief (a) Both lengths at run time. */
extern "C" __attribute__((noinline)) std::uint64_t witness_sites(
    std::uint64_t N, std::uint64_t n) {
  return site_occurrences(N, n);
}
/** @brief (b) The site's length known: EcoRI's six. */
extern "C" __attribute__((noinline)) std::uint64_t witness_sites_of_6(
    std::uint64_t N) {
  return site_occurrences(N, 6);
}
/** @brief (b′) The genome's length known: twelve. */
extern "C" __attribute__((noinline)) std::uint64_t witness_sites_in_12(
    std::uint64_t n) {
  return site_occurrences(12, n);
}
/** @brief (c) Both known: the count, a constant in the IR. */
extern "C" __attribute__((noinline)) std::uint64_t witness_sites_12_6() {
  return site_occurrences(12, 6);
}
