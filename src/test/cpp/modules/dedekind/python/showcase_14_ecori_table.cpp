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

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <ranges>
#include <span>
#include <string_view>
#include <type_traits>

import dedekind.category;

using dedekind::category::abc;
using dedekind::category::automaton;
using dedekind::category::bytes;
using dedekind::category::fixed_by;
using dedekind::category::len;
using dedekind::category::matches;
using dedekind::category::prefix;
using dedekind::category::rev;
using dedekind::category::words;

namespace {
constexpr auto bases = abc("ACGT");
constexpr auto pali6 = words(bases) & len(6) & fixed_by(~rev);
constexpr auto dna = bytes(
    "GAATTCGAGCTCGGTACCCGGGGATCC"
    "TCTAGAGTCGACCTGCAGGCATGCAAGCTT");
constexpr auto ecori = *automaton<4, 6, 4, 8>(pali6 & prefix("GAATTC"));
constexpr auto scanner = *automaton(pali6 & prefix("GA"));
static_assert(count(dna >> matches(ecori)) == 1 && count(dna >> pali6) == 10 &&
              count(dna >> scanner) == 2);
}  // namespace

namespace {
using dedekind::category::run;
using dedekind::category::run_list;
template <typename Plan>
concept PlansAtCompileTime =
    requires { typename std::bool_constant<Plan::value().has_value()>; };
/** @return a plan's states, 0 where a capacity is too small, -1 where the
 *  evaluator's budget ran out: the measurement, read back from the IR. */
template <typename Plan>
constexpr std::int64_t measured() {
  if constexpr (PlansAtCompileTime<Plan>) {
    constexpr auto plan = Plan::value();
    return plan ? static_cast<std::int64_t>(plan->next.size()) : 0;
  } else {
    return -1;
  }
}
/** @brief The sites whose code lies in [lo, hi): a prefix, as a run of codes;
 *  each letter of the prefix narrows the run by four.  K and Q the capacities
 *  the plan's bounds name. */
template <std::size_t lo, std::size_t hi, std::size_t K, std::size_t Q>
struct Prefix {
  static constexpr auto value() {
    const auto all = run_list<128, 4, 6>(pali6);
    return automaton<4, 6, K, Q>(*all & run<2>(lo, hi), 6uz);
  }
};
constexpr std::size_t eco_index = 2109;  // GAATTC
constexpr std::size_t hind_index = 159;  // AAGCTT
template <std::size_t K, std::size_t Q>
struct Digest {
  static constexpr auto value() {
    return automaton<4, 6, K, Q>(
        run<2>(hind_index, hind_index + 1) | run<2>(eco_index, eco_index + 1),
        6uz);
  }
};
/** @brief The section's words alone, read off all 4^6 codes one by one. */
struct RunListOnly {
  static constexpr auto value() { return run_list<128, 4, 6>(pali6); }
};
/** @brief A palindromic hexamer's code from its half-site: three digits, then
 *  their reflections in reverse.  A half read in order gives codes in order. */
constexpr std::size_t code_of_half(std::size_t h) {
  const std::size_t a = h / 16, b = (h / 4) % 4, c = h % 4;
  return (h * 64) + ((3 - c) * 16) + ((3 - b) * 4) + (3 - a);
}
/** @brief One more site, as a run of one code. */
template <std::size_t K>
struct AddSite {
  constexpr dedekind::category::AtMost<std::size_t, K> operator()(
      dedekind::category::AtMost<std::size_t, K> steps, std::size_t h) const {
    const std::size_t code = code_of_half(h);
    return steps.with(code).with(code + 1);
  }
};
/** @brief The table alone: the sites given by their halves in [lo, hi), so
 *  only 4^3 at most are visited, none tested. */
template <std::size_t lo, std::size_t hi, std::size_t K, std::size_t Q>
struct Halves {
  static constexpr auto value() {
    constexpr std::size_t KL = 2 * (hi - lo);
    const dedekind::category::RunList<std::size_t, KL> sites{
        std::ranges::fold_left(std::views::iota(lo, hi),
                               dedekind::category::AtMost<std::size_t, KL>{},
                               AddSite<KL>{})};
    return automaton<4, 6, K, Q>(sites, 6uz);
  }
};
}  // namespace

namespace {
template <std::size_t K, std::size_t Q>
struct PrefixGa {
  static constexpr auto value() {
    return automaton<4, 6, K, Q>(pali6 & prefix("GA"));
  }
};
struct PrefixGaDefault {
  static constexpr auto value() { return automaton(pali6 & prefix("GA")); }
};
struct PrefixGaatt {
  static constexpr auto value() {
    return automaton<4, 6, 4, 8>(pali6 & prefix("GAATTC"));
  }
};
}  // namespace
// The panel's spelling: the words listed by the law, then the table.
extern "C" std::int64_t budget_panel_ga() {
  return measured<PrefixGa<8, 12>>();
}
extern "C" std::int64_t budget_panel_ga_default() {
  return measured<PrefixGaDefault>();
}
extern "C" std::int64_t budget_panel_one_word() {
  return measured<PrefixGaatt>();
}

// Whether the list of the section's words fits by itself, and each table with
// its sites given by their halves: what the budget buys, apart.
extern "C" bool budget_run_list_alone() {
  return PlansAtCompileTime<RunListOnly>;
}
extern "C" std::int64_t budget_table_ga() {
  return measured<Halves<32, 36, 8, 12>>();
}
extern "C" std::int64_t budget_table_g() {
  return measured<Halves<32, 48, 32, 53>>();
}
extern "C" std::int64_t budget_table_all_64() {
  return measured<Halves<0, 64, 32, 288>>();
}

// The budget, by how much of the library is kept: the first letters of the
// site, G = 2 · 4^5.  The states are 12, 11, 52 and 277 by the minimal
// automaton; each with the least capacities that hold it, or the menu's.
extern "C" std::int64_t budget_digest_2() { return measured<Digest<4, 13>>(); }
extern "C" std::int64_t budget_prefix_ga() {
  return measured<Prefix<2048, 2304, 8, 12>>();
}
extern "C" std::int64_t budget_prefix_g() {
  return measured<Prefix<2048, 3072, 32, 53>>();
}
extern "C" std::int64_t budget_prefix_g_menu() {
  return measured<Prefix<2048, 3072, 32, 320>>();
}
extern "C" std::int64_t budget_all_64() {
  return measured<Prefix<0, 4096, 32, 288>>();
}

/** @brief (a) No table: each window tested against the 64 sites. */
extern "C" __attribute__((noinline)) std::uint64_t witness_windows(
    const std::byte* s, std::size_t n) {
  return count(std::span<const std::byte>{s, n} >> pali6);
}
/** @brief (b) The site known only at run time: the same plan, built here. */
extern "C" __attribute__((noinline)) std::uint64_t witness_table_at_run_time(
    const char* word, const std::byte* s, std::size_t n) {
  const auto plan =
      automaton<4, 6, 4, 8>(pali6 & prefix(std::string_view{word, 6}));
  return count(std::span<const std::byte>{s, n} >> matches(plan)).value_or(0);
}
/** @brief (c) The table a constant, the sequence a value: the residual. */
extern "C" __attribute__((noinline)) std::uint64_t witness_ecori_cuts(
    const std::byte* s, std::size_t n) {
  return count(std::span<const std::byte>{s, n} >> matches(ecori));
}
/** @brief (c′) The panel's scanner, by default capacities. */
extern "C" __attribute__((noinline)) std::uint64_t witness_found(
    const std::byte* s, std::size_t n) {
  return count(std::span<const std::byte>{s, n} >> matches(scanner));
}
/** @brief (d) Both known, nothing bound: what the optimiser finds alone. */
extern "C" __attribute__((noinline)) std::uint64_t witness_ecori_cuts_on_dna() {
  return count(dna >> matches(ecori));
}
/** @brief (e′) The panel's scanner on the panel's sequence, bound. */
extern "C" __attribute__((noinline)) std::uint64_t witness_panel_dna() {
  constexpr std::size_t cuts = count(dna >> scanner);
  return cuts;
}
/** @brief (e) Both bound by @c constexpr: the constant in the IR. */
extern "C" __attribute__((noinline)) std::uint64_t witness_ecori_dna() {
  constexpr std::size_t cuts = count(dna >> matches(ecori));
  return cuts;
}
