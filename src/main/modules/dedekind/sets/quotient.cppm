/**
 * @file dedekind/sets/quotient.cppm
 * @partition :quotient
 * @brief Quotient construction Set / EquivRel as a DSL primitive.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section quotient__Description
 * Per #567 (textbook construction exhibit), the DSL gains a @c quotient
 * operator that constructs a Set as the quotient of an underlying Set by
 * an equivalence relation, with the result's @c Domain resolved through
 * a trait registry (@c quotient_carrier_t) keyed on the (PairsDomain,
 * EquivRel) pair.
 *
 * The motivating exhibit is the textbook ℚ = (ℤ × ℤ_≠0) / ~ construction:
 *
 *     constexpr auto numerators   = ℤ;
 *     constexpr auto z = element<ℤ>;
 *     constexpr auto denominators = Set{z | (z != 0)};
 *     constexpr auto pairs        = cartesian_product(numerators,
 * denominators); constexpr auto cross_mult   = CrossMultEquiv<...>{}; constexpr
 * auto ℚ_constructed = quotient(pairs, cross_mult);
 *     static_assert(std::same_as<typename decltype(ℚ_constructed)::Domain,
 *                                Rational<...>>);
 *
 * Per the @b structuralist reading: the carrier inhabiting the quotient
 * (here @c Rational<I>) is structurally exhibited as the quotient's
 * Domain, with the existing carrier class providing the canonical-
 * representative semantics (@c Rational<I>::simplify normalises via
 * gcd; equality cross-multiplies — exactly what the equivalence relation
 * specifies).
 *
 * The same operator is intended to construct ℂ = ℝ[i]/(i²+1) and
 * 𝔻 = ℝ[ε]/(ε²) as worked exhibits (per the #567 issue body); the
 * specialisations of @c quotient_carrier for those constructions are
 * registered downstream alongside the @c Complex<F> / @c Dual<F>
 * carriers.
 *
 * @section quotient__Sequencing
 * - Depends on the post-#551 @c Set / @c cartesian_product surface (in
 *   @c :expressions) and on the @c Ω / @c element machinery.
 * - The operator is structural-only at this phase: membership of a
 *   value @c v in the quotient is universal (every value of @c Domain
 *   is the canonical representative of @b some pair in the underlying
 *   Set, by virtue of the construction).  A finer membership check
 *   (existence-of-pair-witness) is a follow-up — see #567's "Open
 *   design questions" block.
 *
 * @note "Diviser une difficulté en autant de parcelles qu'il se pourrait, et
 *  qu'il serait requis pour les mieux résoudre."
 *       — René Descartes, *Discours de la méthode*, II (1637).
 *       [Trans: "Divide each difficulty into as many parts as feasible, and
 *        as may be needed to resolve it."]
 *       The quotient operator divides the construction of named algebraic
 *       structures (ℚ, ℂ, 𝔻) into two textbook steps: a Set of
 *       representatives × an equivalence relation.
 */
module;

#include <concepts>
#include <type_traits>
#include <utility>

export module dedekind.sets:quotient;

import dedekind.category;
import :boundaries;   // Ω, UniversalSet
import :expressions;  // Set, cartesian_product, element

namespace dedekind::sets {
using namespace dedekind::category;

/** @brief Trait registry: maps a (PairsDomain, EquivRel) pair to the
 *  canonical carrier representative of the quotient.
 *
 *  Primary template is intentionally empty; specialisations register
 *  per-construction carrier choices.  The @c quotient operator below
 *  concept-gates on the existence of @c quotient_carrier_t, so a
 *  call site with no registered specialisation produces a clear
 *  substitution-failure diagnostic rather than a silent default.
 *
 *  Canonical specialisations (registered downstream):
 *  - @c quotient_carrier<std::pair<I, I>, @c CrossMultEquiv<I>> = @c
 * Rational<I> — ℚ as the quotient of ℤ × ℤ_≠0 by cross-multiplication.
 *
 *  Future specialisations (per #567):
 *  - @c quotient_carrier<Polynomial<F>, @c IdealMod<i² + 1>> = @c Complex<F>
 *    — ℂ as the quotient of ℝ[i] by the ideal (i² + 1).
 *  - @c quotient_carrier<Polynomial<F>, @c IdealMod<ε²>> = @c Dual<F>
 *    — 𝔻 as the quotient of ℝ[ε] by the ideal (ε²).
 */
export template <typename PairsDomain, typename EquivRel>
struct quotient_carrier {};

export template <typename PairsDomain, typename EquivRel>
using quotient_carrier_t =
    typename quotient_carrier<PairsDomain, EquivRel>::type;

/** @brief The quotient Set @c Pairs / @c EquivRel.
 *
 *  @details @c Domain is resolved via @c quotient_carrier_t — the
 *  canonical-representative carrier registered for the
 *  (Pairs::Domain, EquivRel) pair.  Membership (`operator()`) is
 *  universal at this phase: every value of @c Domain is the canonical
 *  representative of @b some pair in @c pairs by virtue of the
 *  construction.  See the partition's @b @c quotient__Sequencing block
 *  for the finer membership-check follow-up.
 *
 *  The structural claim this artefact delivers is the @b type identity:
 *  @c decltype(quotient(...))::Domain @c == @c <expected carrier>.
 *  This is what makes the textbook construction `ℚ = (ℤ × ℤ_≠0) / ~`
 *  observable in code as a @c static_assert.
 */
export template <typename Pairs, typename EquivRel>
  requires requires {
    typename quotient_carrier<typename std::remove_cvref_t<Pairs>::Domain,
                              EquivRel>::type;
  }
struct Quotient {
  using PairsType = std::remove_cvref_t<Pairs>;
  using PairsDomain = typename PairsType::Domain;
  using Domain = quotient_carrier_t<PairsDomain, EquivRel>;
  using Codomain = typename PairsType::Codomain;
  using logic_species = typename PairsType::logic_species;
  using cardinality_type = typename PairsType::cardinality_type;

  PairsType pairs;
  EquivRel rel;

  constexpr Quotient(const PairsType& p, EquivRel r)
      : pairs(p), rel(std::move(r)) {}

  /** @brief Universal membership at the quotient phase.
   *
   *  Every value of @c Domain is the canonical representative of some
   *  pair in @c pairs by construction; the finer existence-of-pair-
   *  witness check is a follow-up.
   */
  constexpr typename logic_species::Ω operator()(const Domain&) const {
    return logic_species::True;
  }
};

/** @brief Construct the quotient of a Set of pairs by an equivalence
 *  relation, with @c Domain resolved through the @c quotient_carrier
 *  trait registry.
 *
 *  @tparam Pairs    A Set whose Domain is the equivalence-class
 *                   representative type (typically @c std::pair<...>).
 *  @tparam EquivRel A type whose specialisation of @c quotient_carrier
 *                   names the canonical carrier representative.
 *
 *  Concept-gated on the existence of @c quotient_carrier_t at the
 *  (Pairs::Domain, EquivRel) pair: call sites without a registered
 *  specialisation get a substitution-failure diagnostic rather than a
 *  silent default.
 */
export template <typename Pairs, typename EquivRel>
  requires requires {
    typename quotient_carrier<typename std::remove_cvref_t<Pairs>::Domain,
                              EquivRel>::type;
  }
constexpr auto quotient(const Pairs& pairs, EquivRel rel) {
  return Quotient<std::remove_cvref_t<Pairs>, EquivRel>{pairs, std::move(rel)};
}

}  // namespace dedekind::sets
