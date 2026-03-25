/**
 * @file dedekind/sets/singleton.cppm
 * @brief The Atomic Body: Implementation of the Singleton Species {x}.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @module dedekind.sets:singleton
 * @dependency dedekind.ontology
 *
 * @section The_Singleton_Atom: Mereological Singularity
 * In the Dedekind universe, the Singleton is the "Unit of Presence."
 * It serves as the canonical implementation of a Pointed Set and acts
 * as the 'pure' or 'return' operation (η) for the Set Monad.
 *
 * @details
 * This structure bridges Level 0a (Species) and Level 1 (Mereology):
 * - It is Extensional: It exists in memory as a single 'pivot' element.
 * - It is a Monad: It supports the Kleisli Highway (>>=) via direct
 * application.
 * - It is a Comonad: It supports the Co-Kleisli Pull (<<=) via extraction (ε).
 *
 * @section Structural_Role
 * The SingletonSet provides the baseline proof for the Unified Highway Bridge.
 * By defining η and ε here, we allow fmap to be derived automatically
 * for any morphism f: T -> U lifted into the Singleton context.
 *
 * @tparam T The underlying Species of the pivot element.
 * @tparam L The Subobject Classifier (Ω) governing the set's logic.
 *           Defaults to ClassicalLogic {True, False}.
 *
 * Wikipedia: Singleton (mathematics), Unit element, Monad (category theory)
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

/** @section Extend (<<=) */
export template <typename T, typename L, typename Func>
constexpr auto operator<<=(const SingletonSet<T, L>& s, Func&& f) {
  using U = std::invoke_result_t<Func, SingletonSet<T, L>>;
  // Co-Kleisli Extend: apply contextual logic and re-wrap.
  return SingletonSet<U, L>{std::forward<Func>(f)(s)};
}

};  // namespace dedekind::sets

namespace dedekind::ontology {
using namespace dedekind::sets;
// Re-open the ontology namespace to provide the specialization
export template <typename T>
struct η<SingletonSet, T> {
  constexpr auto operator()(const T& x) const { return SingletonSet<T>{x}; }
};

/** @section Singleton_Counit (ε) */
export template <typename T>
struct ε<SingletonSet, T> {
  // Extraction is logic-agnostic, so we can use a variadic match here
  // or just match the Classical version.
  template <typename L>
  constexpr T operator()(const SingletonSet<T, L>& s) const {
    return s.pivot;
  }
};
};  // namespace dedekind::ontology

/** @section The_Final_Ontology_Proof */
namespace dedekind::sets {
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
}  // namespace dedekind::sets
