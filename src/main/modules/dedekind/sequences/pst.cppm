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
 * (@c sets:boundaries, @c sets:quantifier); what needs the range view is here,
 * as @c std::ranges pipelines: the @b runs of constant level @f$\ne \bot@f$
 * (the fibres of χ, its staircase) and the @b α-cuts @f$\{x : \chi(x) \ge
 * \ell\}@f$, one Boolean run-list per level, nested --- the normal form of the
 * Pst fragment.  On a window whose ends are values or on a whole truth chain.
 * O(N) steps, O(1) state, nothing stored.  Not here: ℤ (the walk would have to
 * jump between the term's cut points) and the dense carriers (no step).
 *
 * Wikipedia: Alpha cut (fuzzy set), Run-length encoding, Dedekind cut
 *
 * @note "Zerfallen alle Punkte der Geraden in zwei Klassen von der Art, daß
 *       jeder Punkt der ersten Klasse links von jedem Punkt der zweiten Klasse
 *       liegt, so existiert ein und nur ein Punkt, welcher diese Einteilung
 *       aller Punkte in zwei Klassen hervorbringt."
 *       -- Richard Dedekind, Stetigkeit und irrationale Zahlen (1872), §3.
 *       [Trans: "If all points of the line fall into two classes such that
 *       every point of the first class lies to the left of every point of the
 *       second, then there exists one and only one point which produces this
 *       division of all points into two classes."  An α-cut on a chain is that
 *       division, read off χ's table rather than the continuum.]
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
                   HasCoveringStep<typename std::remove_cvref_t<S>::Domain> &&
                   requires { typename std::remove_cvref_t<S>::logic_species; };
using dedekind::sets::chain_bottom;
using dedekind::sets::chain_top;

/** @brief A maximal run of constant level @f$\ne \bot@f$ (a fibre of χ), or
 *  of an α-cut (@c Ω @c = @c bool).
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
/** @brief A point of the α-cut at @c level: @c (x, χ(x) ≥ level).
 *  @tparam S the set. */
template <IsPstSet S>
struct Thresholded {
  S s;
  typename S::logic_species::Ω level;
  constexpr auto operator()(const typename S::Domain& x) const {
    return std::pair{x, s(x) >= level};
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
/** @brief A table of @c (x, level) chunked by level, the ⊥ chunks dropped: the
 *  runs.  @tparam L the species of the levels.  @tparam Table the table. */
template <typename L, std::ranges::forward_range Table>
constexpr auto runs_of(Table table) {
  return std::move(table) | std::views::chunk_by(SameLevel{}) |
         std::views::filter(Inhabited<L>{}) | std::views::transform(ToRun{});
}
}  // namespace detail_pst

/** @brief The runs of constant level @f$\ne \bot@f$ of @c s on the window: the
 *  fibres of χ, its staircase.  @tparam S the set.  @param s the set.
 *  @param lo the window's bottom.  @param hi its top. */
export template <IsPstSet S>
constexpr auto runs(const S& s, const typename S::Domain& lo,
                    const typename S::Domain& hi) {
  using L = typename S::logic_species;
  return detail_pst::runs_of<L>(
      chain_view{lo, hi} | std::views::transform(detail_pst::Levelled<S>{s}));
}
/** @brief The α-cut @f$\{x : \chi(x) \ge \ell\}@f$ of @c s on the window, as
 *  its Boolean runs; the cuts are nested in @c level, and on a Boole-valued set
 *  the cut at ⊤ is @c runs.  @tparam S the set.  @param s the set.
 *  @param level the threshold @f$\ell@f$.  @param lo the window's bottom.
 *  @param hi its top. */
export template <IsPstSet S>
constexpr auto alpha_cut(const S& s, typename S::logic_species::Ω level,
                         const typename S::Domain& lo,
                         const typename S::Domain& hi) {
  return detail_pst::runs_of<Boole>(
      chain_view{lo, hi} |
      std::views::transform(detail_pst::Thresholded<S>{s, level}));
}

/** @brief The runs over a whole truth chain, ⊥ to ⊤.
 *  @tparam S a set over a truth chain. */
export template <IsPstSet S>
  requires IsPst<typename S::Domain>
constexpr auto runs(const S& s) {
  using C = typename S::Domain;
  return runs(s, chain_bottom<C>(), chain_top<C>());
}
export template <IsPstSet S>
  requires IsPst<typename S::Domain>
constexpr auto alpha_cut(const S& s, typename S::logic_species::Ω level) {
  using C = typename S::Domain;
  return alpha_cut(s, level, chain_bottom<C>(), chain_top<C>());
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
