/**
 * @file sets.cppm
 * @brief The Dedekind Set Model: Concrete Collections and Set Morphisms.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section sets__Set_Theory
 * This module provides concrete implementations for the abstract laws
 * defined in dedekind.ontology. While the ontology defines the requirements
 * for a set, this module provides the mechanisms to construct them.
 *
 * @details
 * In the Dedekind architecture, set theory bridges logic (Level -1)
 * and magnitude. By implementing monadic and comonadic
 * extensions for specific structures, we enable:
 * - Extensional Sets: Materialized collections (Singleton, Finite Sets).
 * - Intensional Sets: Rule-based definitions (Comprehensions).
 * - Set Morphisms: Automatic lifting of functions (fmap) and filters (bind).
 *
 * @section sets__The_Comprehension_Syntax
 * This module enables symbolic set-builder notation:
 * { f | x % S | P } -> The mapping of elements in S satisfying P by f.
 *
 * @see dedekind.sets:singleton (The Atomic Point)
 * @see dedekind.sets:expressions (The Symbolic Calculus)
 *
 * Wikipedia: Set theory, Axiom of Specification, Naive Set Theory
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Das Wesen der Mathematik liegt in ihrer Freiheit." — Georg Cantor,
 * *Grundlagen einer allgemeinen Mannigfaltigkeitslehre* (1883).
 * [Trans: "The essence of mathematics lies entirely in its freedom."]
 */
module;

#include <concepts>     // std::convertible_to
#include <type_traits>  // std::remove_cvref_t (HasSetSurface decays cv/ref)

export module dedekind.sets;

export import :cardinality;
export import :boundaries;
export import :computability;
export import :expressions;
export import :extensional;
export import :mereology;
export import :singleton;
export import :relational;
export import :quotient;

import dedekind.category; // IsSet (the ETCS-axiomatic gate composed by HasSetSurface)

namespace dedekind::sets {

/** @section sets__User_Facing_Surface
 *
 * @brief Master concept + checklist for the ergonomic set-DSL surface.
 *
 * @details
 * Per @c :etcs vs @c :expressions polarity: @c :etcs reifies the 10 ETCS
 * axioms with almost no regard for ergonomics; @c :expressions and its
 * siblings take ergonomics as the @em sine @em qua @em non (set-builder
 * notation is presumed blackboard-readable).  This block names the
 * ergonomic surface every user-facing set type should expose.
 *
 * Composition: @c HasSetSurface is the @b strict set-theoretic superset
 * of @c :etcs::IsSet (adds cv/ref decay only — no extra structural
 * requirements).  The @c Has* checklist sub-concepts below are
 * standalone affordances composed at use sites rather than bundled
 * into the master.
 */

/** @brief Callable wrapper around set-level union (∪), used so the
 *         abstract lattice claim @c :category:mereology::IsSetLattice
 *         can be instantiated on the set-pair operators. */
struct set_join {
  template <typename S>
  constexpr auto operator()(const S& a, const S& b) const {
    return a | b;
  }
};

/** @brief Callable wrapper around set-level intersection (∩). */
struct set_meet {
  template <typename S>
  constexpr auto operator()(const S& a, const S& b) const {
    return a & b;
  }
};

/** @brief Sub-concept: @c s(v) is well-formed and returns a value
 *         convertible to the set's logic species @c Ω. */
export template <typename S>
concept HasMembershipOperator =
    requires(const S& s, const typename S::Ambient& v) {
      { s(v) } -> std::convertible_to<typename S::logic_species::Ω>;
    };

/** @brief Sub-concept: @c !s is well-formed.
 *  @details Result type intentionally unconstrained at this slice
 *           (Sollbruchstelle).  Strict variant @c IsSet<decltype(!s)>
 *           can land later when more set types prove closure under
 *           complement. */
export template <typename S>
concept HasComplementOperator = requires(const S& s) {
  { !s };
};

/** @brief Sub-concept: lattice-of-sets surface — operations on PAIRS
 *         OF SETS (∪, ∩, △).
 *  @details Distinct from @c :order::HasLatticeOperators which acts on
 *           PAIRS OF ELEMENTS — different ontological layers, same
 *           lattice abstraction.  The axiomatic claim ("this IS a
 *           lattice of sets") lives upstream as
 *           @c :category:mereology::IsSetLattice<S, set_join, set_meet>;
 *           callers needing the axiomatic gate compose both. */
export template <typename S>
concept HasSetOperators = requires(const S& a, const S& b) {
  { a | b };
  { a & b };
  { a ^ b };
};

/** @brief Sub-concept: @c s.cardinality() returns the carrier's
 *         @c cardinality_type tag. */
export template <typename S>
concept HasCardinalityInterface = requires(const S& s) {
  typename S::cardinality_type;
  { s.cardinality() };
};

/** @brief Master: ergonomic counterpart of @c :etcs::IsSet — strict
 *         set-theoretic superset that adds cv/ref decay (so callers
 *         can pass @c decltype(expr) directly).
 *
 *  @details Every @c IsSet<T> satisfies @c HasSetSurface<T>; the
 *           superset is proper because reference- / cv-qualified
 *           variants satisfy @c HasSetSurface but not the strict
 *           @c :etcs::IsSet.  Granular DSL affordances (membership,
 *           complement, lattice ops, cardinality) are the @c Has*
 *           sub-concepts above — composed at use sites rather than
 *           bundled here. */
export template <typename S>
concept HasSetSurface = dedekind::category::IsSet<std::remove_cvref_t<S>>;

/** @section sets__User_Facing_Surface_Witnesses
 *  @details Canonical green witnesses pinning the master concept +
 *           every sub-concept on @c SingletonSet, the most fully-
 *           equipped concrete set type today.  Strict-superset
 *           property: a reference-qualified decltype still satisfies
 *           @c HasSetSurface. */
namespace _user_facing_witnesses {
using _S1 = SingletonSet<int, dedekind::category::ClassicalLogic>;
static_assert(HasMembershipOperator<_S1>);
static_assert(HasComplementOperator<_S1>);
static_assert(HasSetOperators<_S1>);
static_assert(HasCardinalityInterface<_S1>);

// Strict-superset claim: HasSetSurface fires wherever IsSet fires...
static_assert(dedekind::category::IsSet<_S1> ? HasSetSurface<_S1> : true);
static_assert(HasSetSurface<_S1>);
// ...and also fires on ref/cv variants the strict IsSet rejects.
static_assert(!dedekind::category::IsSet<_S1 const&>);
static_assert(HasSetSurface<_S1 const&>);
}  // namespace _user_facing_witnesses

}  // namespace dedekind::sets
