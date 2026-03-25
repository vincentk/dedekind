/**
 * @file ontology:mereology.cppm
 * @brief Level 1: The Rules of Presence (Topos-Aware Sets).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :mereology
 * @build_order 2
 * @dependency :logic, :category
 *
 * @section Mereology: The Geometry of Existence
 * This partition defines the "Body" of the Dedekind species. In the
 * structuralist ontology, Mereology establishes the relationship between
 * 'Parts' and 'Wholes' as a mapping between a Domain and a Logic.
 *
 * @details
 * Membership is defined as a morphism to a Subobject Classifier (Ω).
 * By decoupling the "Body" from the "Truth," the same mereological laws
 * are applied across different logical universes:
 * - IsSet: The universal rule-based predicate for membership.
 * - IsBooleanSet: The Classical {True, False} universe (The Binary Prime).
 * - IsKleeneSet: The Indeterminate {True, False, Unknown} universe.
 *
 * @section Structural_Anchors
 * Standard C++ operators are anchored here as Set Morphisms,
 * lifting logical connectives into latticial operations:
 * - operator&&, operator& : Intersection (The Meet).
 * - operator||, operator| : Union (The Join).
 * - operator! : Complement (The Remainder).
 *
 * @tparam S The Set implementation type being verified.
 * @tparam L The Logic species (Ω) governing the membership predicate.
 *           Defaults to ClassicalLogic for zero-overhead arithmetic.
 *
 * Wikipedia: Mereology, Subobject classifier, Topos theory
 */
module;

#include <compare>
#include <concepts>
#include <functional>

export module dedekind.sets:singleton;

import dedekind.ontology;

/**
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::sets {

using namespace dedekind::ontology;

/** @brief {x}: The Atom. Extensional (Size 1). */
export template <typename T, typename L = ClassicalLogic>
struct SingletonSet {
  T pivot;
  using element_type = T;
  using logic_species = L;
  using cardinality_type = Finite;
  using base_set_type = SingletonSet<T, L>;

  constexpr typename L::type contains(const T& v) const {
    return (v == pivot) ? L::True : L::False;
  }

  /** @section Extensionality_Proof */
  constexpr std::size_t size() const { return 1; }
  constexpr std::size_t upper_bound() const { return 1; }
};

/** @section The_Set_Monad_Realization */

/** @brief η: T -> SingletonSet<T> (The Unit) */
export template <typename T>
constexpr auto singleton(T&& value) {
  return SingletonSet<std::decay_t<T>>{std::forward<T>(value)};
}

/** @section The_Set_Monad: The Categorical Identity */

/**
 * @section Singleton_Kleisli_Triple
 * @brief The Bricks of the Singleton Monad.
 */

/** @section Singleton_Unit (η) */
// We only need T here. We 'fix' the logic to Classical.
template <typename T>
struct dedekind::ontology::η<SingletonSet, T> {
  constexpr auto operator()(const T& x) const {
    // We explicitly construct the Classical variety.
    return SingletonSet<T, ClassicalLogic>{x};
  }
};

/** @section Bind (>>=) */
export template <typename T, typename L, typename Func>
constexpr auto operator>>=(const SingletonSet<T, L>& s, Func&& f) {
  /**
   * @details Kleisli Bind for Singletons:
   * 1. Sample the internal species (The Pull).
   * 2. Apply the Kleisli Arrow f: T -> SingletonSet<U, L>.
   */
  return std::forward<Func>(f)(s.pivot);
}

/** @section Singleton_CoKleisli_Triple */

/** @section Singleton_Counit (ε) */
template <typename T>
struct ε<SingletonSet, T> {
  // Extraction is logic-agnostic, so we can use a variadic match here
  // or just match the Classical version.
  template <typename L>
  constexpr T operator()(const SingletonSet<T, L>& s) const {
    return s.pivot;
  }
};

/** @section Extend (<<=) */
export template <typename T, typename L, typename Func>
constexpr auto operator<<=(const SingletonSet<T, L>& s, Func&& f) {
  using U = std::invoke_result_t<Func, SingletonSet<T, L>>;
  // Co-Kleisli Extend: apply contextual logic and re-wrap.
  return SingletonSet<U, L>{std::forward<Func>(f)(s)};
}

/** @section The_Final_Ontology_Proof */
namespace {
// A simple cross-species transformation: int -> bool
constexpr auto is_even = arrow<int, bool>([](int x) { return x % 2 == 0; });

using IntSet = SingletonSet<int, ClassicalLogic>;
using BoolSet = SingletonSet<bool, ClassicalLogic>;

// The Proof: "Lifting 'is_even' into the Singleton context"
static_assert(
    IsArrow<decltype(fmap<SingletonSet>(is_even)), IntSet, BoolSet>,
    "PR Failure: SingletonSet failed to discover its Functorial Highway.");

// The Action: "Executing the lifted morphism"
static_assert(((IntSet{42} >> fmap<SingletonSet>(is_even))
               << extract<SingletonSet>) == true,
              "PR Failure: The Set Monad failed to preserve the truth value of "
              "the species.");
}  // namespace

};  // namespace dedekind::sets

namespace dedekind::ontology {
// Re-open the ontology namespace to provide the specialization
export template <typename T>
struct η<dedekind::sets::SingletonSet, T> {
  auto operator()(const T& x) const {
    return dedekind::sets::SingletonSet<T>{x};
  }
};
}  // namespace dedekind::ontology
