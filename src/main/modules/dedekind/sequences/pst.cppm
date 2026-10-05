/**
 * @file dedekind/sequences/pst.cppm
 * @partition :pst
 * @brief The Pst fragment decided by one fold along the chain.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section pst__Overview
 * A set over a finite chain is a finite table @f$\chi : C \to L@f$, read off
 * @c chain_view(lo, hi) as it is walked.  Equality, and with it the quantifiers
 * and the subset identity, is exhaustion of the chain and lives upstream
 * (@c sets:boundaries, @c sets:quantifier); what needs the range view is here:
 * the @b runs of constant level @f$\ne \bot@f$ (the α-cuts) as a
 * @c std::ranges pipeline, on a window whose ends are values or on a whole
 * truth chain.  O(N) steps, O(1) state, nothing stored.  Not here: ℤ (the walk
 * would have to jump between the term's cut points) and the dense carriers (no
 * step).
 */
module;

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <functional>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>

export module dedekind.sequences:pst;

import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :fold;
import :ranges;

namespace dedekind::sequences {
using namespace dedekind::category;
using dedekind::sets::Ø;
using dedekind::sets::𝔸;

/** @brief A set object over a finite chain, leaf or lattice node (@c IsLSet
 *  asks for the subobject vocabulary a node does not carry).
 *  @tparam S the set. */
export template <typename S>
concept IsPstSet = dedekind::sets::IsSetObject<S> &&
                   IsFiniteChain<typename std::remove_cvref_t<S>::Domain> &&
                   requires { typename std::remove_cvref_t<S>::logic_species; };
using dedekind::sets::chain_bottom;
using dedekind::sets::chain_top;

/** @brief A maximal run of constant level @f$\ne \bot@f$.
 *  @tparam C the chain.  @tparam Ω the species' truth values. */
export template <typename C, typename Ω>
struct Run {
  Ω level;
  C lo;
  C hi;
  friend constexpr bool operator==(const Run&, const Run&) = default;
};

namespace detail_pst {
/** @brief A point of the table: @c (x, χ(x)).  @tparam S the set. */
template <IsPstSet S>
struct Levelled {
  S s;
  constexpr auto operator()(const typename S::Domain& x) const {
    return std::pair{x, s(x)};
  }
};
struct SameLevel {
  constexpr bool operator()(const auto& a, const auto& b) const {
    return a.second == b.second;
  }
};
template <typename L>
struct Inhabited {
  constexpr bool operator()(const auto& chunk) const {
    return (*chunk.begin()).second != L::False;
  }
};
struct ToRun {
  constexpr auto operator()(const auto& chunk) const {
    auto first = *chunk.begin();
    auto last = first;
    for (const auto& p : chunk) last = p;
    return Run{first.second, first.first, last.first};
  }
};
}  // namespace detail_pst

/** @brief The runs of @c s on the window: the table chunked by level, the ⊥
 *  chunks dropped.  @tparam S the set.  @param s the set.  @param lo the
 *  window's bottom.  @param hi its top. */
export template <IsPstSet S>
constexpr auto runs(const S& s, const typename S::Domain& lo,
                    const typename S::Domain& hi) {
  using L = typename S::logic_species;
  return chain_view{lo, hi} |
         std::views::transform(detail_pst::Levelled<S>{s}) |
         std::views::chunk_by(detail_pst::SameLevel{}) |
         std::views::filter(detail_pst::Inhabited<L>{}) |
         std::views::transform(detail_pst::ToRun{});
}

/** @brief The runs over a whole truth chain, ⊥ to ⊤.
 *  @tparam S a set over a truth chain. */
export template <IsPstSet S>
  requires IsPst<typename S::Domain>
constexpr auto runs(const S& s) {
  using C = typename S::Domain;
  return runs(s, chain_bottom<C>(), chain_top<C>());
}

/** @section pst__Formal_Verification */
namespace detail_pst_witness {
using dedekind::sets::π;
/** @brief A view's runs as (count, first, last). */
template <typename View>
consteval auto summary(View v) {
  std::size_t n = 0;
  std::ranges::range_value_t<View> first{}, last{};
  for (const auto r : v) {
    if (n == 0) first = r;
    last = r;
    ++n;
  }
  return std::tuple{n, first, last};
}
inline constexpr auto above_bottom = 𝔸<Ternary>{} | (π > Ternary::False);
static_assert(summary(runs(above_bottom)) ==
                  std::tuple{std::size_t{1},
                             Run{true, Ternary::Unknown, Ternary::True},
                             Run{true, Ternary::Unknown, Ternary::True}},
              "{x > ⊥} on K₃ is one run of level ⊤: [U, ⊤].");
inline constexpr auto above5 = 𝔸<int>{} | (π > 5);
inline constexpr auto below3 = 𝔸<int>{} | (π < 3);
static_assert(std::ranges::equal(runs(above5, 0, 20),
                                 runs(𝔸<int>{} | (π >= 6), 0, 20)) &&
                  std::ranges::empty(runs(above5 & below3, 0, 20)) &&
                  summary(runs(above5 | below3, 0, 9)) ==
                      std::tuple{std::size_t{2}, Run{true, 0, 2},
                                 Run{true, 6, 9}},
              "on a window: {x > 5} = {x ≥ 6} by their runs, (x > 5) ∧ (x < 3) "
              "has none, and a union with a gap has two.");
}  // namespace detail_pst_witness

}  // namespace dedekind::sequences
