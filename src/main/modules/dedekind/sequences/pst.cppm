/**
 * @file dedekind/sequences/pst.cppm
 * @partition :pst
 * @brief The Pst fragment's run-lists: a decidable set over a finite chain,
 * read off the chain as it is walked.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section pst__Overview
 * A set over a finite chain is a finite table @f$\chi : C \to L@f$.  Equality,
 * and with it the quantifiers and the subset identity, is exhaustion of the
 * chain and lives upstream (@c sets:boundaries, @c sets:quantifier).  What
 * needs the range view is here: the @b runs of a decidable set, its maximal
 * runs of membership along the chain, as a @c std::ranges pipeline over
 * @c chain_view, on a window whose ends are values or on a whole truth chain.
 * O(N) steps, O(1) state, nothing stored.
 *
 * An L-valued set is read through its decidable sets on @f$\Omega@f$, pulled
 * back along χ with the generic @c preimage (@c category:morphism): the
 * @b fibre of χ at @f$\ell@f$, @f$\chi^{-1}(\ell) = @c preimage(χ, η(ℓ))@f$,
 * and the @b α-cut @f$\{\chi \ge \ell\} = @c preimage(χ, 𝔸<Ω> | (π ≥ ℓ))@f$.
 * The α-cuts are nested in @f$\ell@f$ and are the Pst normal form; their runs
 * are this partition's.  Not here: ℤ (the walk would have to jump between the
 * term's cut points) and the dense carriers (no step).
 *
 * Wikipedia: Alpha cut (fuzzy set), Fiber (mathematics), Run-length encoding,
 * Dedekind cut
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
using dedekind::sets::chain_bottom;
using dedekind::sets::chain_top;
using dedekind::sets::HasDecidableMembership;
using dedekind::sets::Ø;
using dedekind::sets::𝔸;

/** @brief A decidable set object over a finite chain with the covering step:
 *  a leaf or a lattice node whose χ answers in @f$\mathbb{B}@f$ (the Δ grade).
 *  An L-valued set enters through its α-cuts.  @tparam S the set. */
export template <typename S>
concept IsPstSet =
    dedekind::sets::IsSetObject<S> && HasDecidableMembership<S> &&
    IsFiniteChain<typename std::remove_cvref_t<S>::Domain> &&
    HasCoveringStep<typename std::remove_cvref_t<S>::Domain>;

/** @brief A maximal run of membership, @c [lo, hi].  @tparam C the chain. */
export template <typename C>
struct Run {
  C lo;
  C hi;
  friend constexpr bool operator==(const Run&, const Run&) = default;
};

namespace detail_pst {
/** @brief A point of the table: @c (x, χ(x)).  @tparam S the set. */
template <IsPstSet S>
struct Table {
  S s;
  constexpr std::pair<typename S::Domain, bool> operator()(
      const typename S::Domain& x) const {
    return {x, s(x)};
  }
};
struct SameMembership {
  constexpr bool operator()(const auto& a, const auto& b) const {
    return a.second == b.second;
  }
};
struct Inhabited {
  constexpr bool operator()(const auto& chunk) const {
    return (*chunk.begin()).second;
  }
};
struct ToRun {
  constexpr auto operator()(const auto& chunk) const {
    auto first = *chunk.begin();
    auto last = first;
    for (const auto& p : chunk) last = p;
    return Run{first.first, last.first};
  }
};
}  // namespace detail_pst

/** @brief The runs of @c s on the window: the table chunked by membership, the
 *  non-member chunks dropped.  @tparam S the set.  @param s the set.
 *  @param lo the window's bottom.  @param hi its top. */
export template <IsPstSet S>
constexpr auto runs(const S& s, const typename S::Domain& lo,
                    const typename S::Domain& hi) {
  return chain_view{lo, hi} | std::views::transform(detail_pst::Table<S>{s}) |
         std::views::chunk_by(detail_pst::SameMembership{}) |
         std::views::filter(detail_pst::Inhabited{}) |
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
using dedekind::sets::η;
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
                             Run{Ternary::Unknown, Ternary::True},
                             Run{Ternary::Unknown, Ternary::True}},
              "{x > ⊥} on K₃ is one run: [U, ⊤].");
inline constexpr auto above5 = 𝔸<int>{} | (π > 5);
inline constexpr auto below3 = 𝔸<int>{} | (π < 3);
static_assert(std::ranges::equal(runs(above5, 0, 20),
                                 runs(𝔸<int>{} | (π >= 6), 0, 20)) &&
                  std::ranges::empty(runs(above5 & below3, 0, 20)) &&
                  summary(runs(above5 | below3, 0, 9)) ==
                      std::tuple{std::size_t{2}, Run{0, 2}, Run{6, 9}},
              "on a window: {x > 5} = {x ≥ 6} by their runs, (x > 5) ∧ (x < 3) "
              "has none, and a union with a gap has two.");
// An L-valued set through its decidable sets on Ω: χ = id on K₃, valued in K₃.
// The α-cut {χ ≥ U} is the upper ray on Ω pulled back along χ, [U, ⊤]; the
// fibre of χ at U is the point η(U) pulled back, [U, U].
inline constexpr auto hedge = 𝔸<Ternary, Kleene>{} | Identity<Ternary>{};
inline constexpr auto cut_u =
    𝔸<Ternary>{} | preimage(hedge, 𝔸<Ternary>{} | (π >= Ternary::Unknown));
inline constexpr auto fibre_u =
    𝔸<Ternary>{} | preimage(hedge, η(Ternary::Unknown));
static_assert(IsPstSet<decltype(cut_u)> && IsPstSet<decltype(fibre_u)> &&
                  summary(runs(cut_u)) ==
                      std::tuple{std::size_t{1},
                                 Run{Ternary::Unknown, Ternary::True},
                                 Run{Ternary::Unknown, Ternary::True}} &&
                  summary(runs(fibre_u)) ==
                      std::tuple{std::size_t{1},
                                 Run{Ternary::Unknown, Ternary::Unknown},
                                 Run{Ternary::Unknown, Ternary::Unknown}},
              "the α-cut {χ ≥ U} of χ = id on K₃ is [U, ⊤] and the fibre at U "
              "is [U, U]: decidable sets, read as runs.");
}  // namespace detail_pst_witness

}  // namespace dedekind::sequences
