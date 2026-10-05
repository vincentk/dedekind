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
 * @c chain_view(lo, hi) as it is walked: @f$\exists = \bigvee \chi@f$ and
 * @f$\forall = \bigwedge \chi@f$ in @f$L@f$ (the Goguen quantifiers, the
 * finite-chain filling of @c sets:quantifier's L-valued reading, #980), @c ==
 * and @c ⊆ decided in @c bool, and the runs of constant level @f$\ne \bot@f$
 * (the α-cuts) as a @c std::ranges pipeline.  O(N) steps, O(1) state, nothing
 * stored.  The window's ends are values; a truth chain supplies its own.  Not
 * here: ℤ (the walk would have to jump between the term's cut points) and the
 * dense carriers (no step).
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
/** @brief Two Pst sets over one chain, answering in one species.
 *  @tparam A the left set.  @tparam B the right set. */
export template <typename A, typename B>
concept IsPstPair =
    IsPstSet<A> && IsPstSet<B> &&
    std::same_as<typename A::Domain, typename B::Domain> &&
    std::same_as<typename A::logic_species, typename B::logic_species>;

/** @brief A truth chain's ends, from its species.  @tparam C the chain. */
export template <IsPst C>
constexpr C chain_bottom() {
  return classifier_logic_t<C>::False;
}
export template <IsPst C>
constexpr C chain_top() {
  return classifier_logic_t<C>::True;
}

/** @brief The finite table @f$\chi@f$ on the window, as a range. */
template <IsPstSet S>
constexpr auto table(const S& s, const typename S::Domain& lo,
                     const typename S::Domain& hi) {
  return chain_view{lo, hi} | std::views::transform(s);
}

/** @brief @f$\exists x \in [lo, hi].\,\chi(x) = \bigvee \chi@f$ in @c L.
 *  @tparam S the set.  @param s the set.  @param lo the window's bottom.
 *  @param hi its top. */
export template <IsPstSet S>
constexpr typename S::logic_species::Ω exists(const S& s,
                                              const typename S::Domain& lo,
                                              const typename S::Domain& hi) {
  using L = typename S::logic_species;
  return fold(table(s, lo, hi), L::False, L::OR);
}
/** @brief @f$\forall x \in [lo, hi].\,\chi(x) = \bigwedge \chi@f$ in @c L. */
export template <IsPstSet S>
constexpr typename S::logic_species::Ω forall(const S& s,
                                              const typename S::Domain& lo,
                                              const typename S::Domain& hi) {
  using L = typename S::logic_species;
  return fold(table(s, lo, hi), L::True, L::AND);
}
/** @brief The tables agree at every point of the window: decided. */
export template <typename A, typename B>
  requires IsPstPair<A, B>
constexpr bool equal(const A& a, const B& b, const typename A::Domain& lo,
                     const typename A::Domain& hi) {
  return std::ranges::equal(table(a, lo, hi), table(b, lo, hi));
}
/** @brief Goguen's L-subset, @f$\chi_A \le \chi_B@f$ pointwise: decided. */
export template <typename A, typename B>
  requires IsPstPair<A, B>
constexpr bool subset(const A& a, const B& b, const typename A::Domain& lo,
                      const typename A::Domain& hi) {
  return std::ranges::equal(table(a, lo, hi), table(b, lo, hi),
                            std::less_equal<>{});
}

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

/** @brief The whole-chain forms for a truth chain, ⊥ to ⊤.
 *  @tparam S a set over a truth chain. */
export template <IsPstSet S>
  requires IsPst<typename S::Domain>
constexpr auto exists(const S& s) {
  using C = typename S::Domain;
  return exists(s, chain_bottom<C>(), chain_top<C>());
}
export template <IsPstSet S>
  requires IsPst<typename S::Domain>
constexpr auto forall(const S& s) {
  using C = typename S::Domain;
  return forall(s, chain_bottom<C>(), chain_top<C>());
}
export template <typename A, typename B>
  requires IsPstPair<A, B> && IsPst<typename A::Domain>
constexpr bool equal(const A& a, const B& b) {
  using C = typename A::Domain;
  return equal(a, b, chain_bottom<C>(), chain_top<C>());
}
export template <typename A, typename B>
  requires IsPstPair<A, B> && IsPst<typename A::Domain>
constexpr bool subset(const A& a, const B& b) {
  using C = typename A::Domain;
  return subset(a, b, chain_bottom<C>(), chain_top<C>());
}
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
inline constexpr auto top = 𝔸<bool>{} | (π == true);
static_assert(exists(top) && !forall(top) && equal(top, top) &&
                  !equal(top, 𝔸<bool>{}) && subset(top, 𝔸<bool>{}) &&
                  !subset(𝔸<bool>{}, top) && !exists(Ø<bool>{}),
              "{⊤} on 𝔹: ∃, ∀, ==, ⊆ decided on the finite table.");
inline constexpr auto above_bottom = 𝔸<Ternary>{} | (π > Ternary::False);
static_assert(forall(above_bottom, Ternary::Unknown, Ternary::True) &&
                  summary(runs(above_bottom)) ==
                      std::tuple{std::size_t{1},
                                 Run{true, Ternary::Unknown, Ternary::True},
                                 Run{true, Ternary::Unknown, Ternary::True}},
              "{x > ⊥} on K₃: ∀ on [U, ⊤], and one run of level ⊤.");
inline constexpr auto above5 = 𝔸<int>{} | (π > 5);
inline constexpr auto below3 = 𝔸<int>{} | (π < 3);
static_assert(equal(above5, 𝔸<int>{} | (π >= 6), 0, 20) &&
                  !exists(above5 & below3, 0, 20) &&
                  summary(runs(above5 | below3, 0, 9)) ==
                      std::tuple{std::size_t{2}, Run{true, 0, 2},
                                 Run{true, 6, 9}},
              "on a window: {x > 5} = {x ≥ 6}, (x > 5) ∧ (x < 3) = Ø by "
              "evaluation, and a union with a gap is two runs.");
}  // namespace detail_pst_witness

}  // namespace dedekind::sequences
