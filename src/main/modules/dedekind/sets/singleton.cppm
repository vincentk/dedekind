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

import dedekind.category;

import :mereology;
import :boundaries;
import :expressions;

/**
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::sets {

using namespace dedekind::category;

/** @brief {x}: The Atom. Extensional (Size 1). */
export template <typename T, typename L = ClassicalLogic>
struct SingletonSet {
  T pivot;
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = Finite;
  using is_extensional_tag = void;
  using base_set_type = SingletonSet<T, L>;

  /** @section Algebraic_Axioms */
  template <typename Op>
  static constexpr bool is_associative_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  template <typename Op>
  static constexpr bool is_idempotent_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  // This satisfies IsProperPart and IsSet simultaneously
  constexpr auto operator()(const T& v) const {
    return (v == pivot) ? L::True : L::False;
  }

  constexpr T origin() const { return pivot; }

  /** @section Extensionality_Proof */
  constexpr std::size_t size() const { return 1; }
  constexpr std::size_t upper_bound() const { return 1; }
  constexpr auto cardinality() const { return Finite{}; }

  /** @section Mereological_Relation (sqsubseteq) */

  // 1. Manual definition to satisfy: { a <= b } -> typename L::Ω
  constexpr typename L::Ω operator<=(const SingletonSet& other) const {
    return (pivot == other.pivot) ? L::True : L::False;
  }

  // 2. Manual equality for IsExtensional
  constexpr bool operator==(const SingletonSet& other) const {
    return pivot == other.pivot;
  }

  // 3. DELETE the spaceship to stop the compiler from generating
  // a 'bool'-returning operator<= that breaks the concept.
  auto operator<=>(const SingletonSet&) const = delete;

  // S1 <= S2 (Is S1 a part of S2?)
  template <typename S>
  constexpr typename L::Ω operator<=(const S& other) const {
    // If it's another singleton, compare pivots
    if constexpr (requires { other.pivot; }) {
      return (pivot == other.pivot) ? L::True : L::False;
    }
    // If it's the Universal Set, it's always true
    if constexpr (std::is_base_of_v<Boundaries, S>) {
      return other(*this);  // Let the boundary decide (Ω returns True)
    }
    return L::False;
  }

  /** @section Mereological_Lattice_Audit */

  /** @section Unified_Lattice_Operations (Level 1) */

  template <typename U, typename L2>
  constexpr auto operator|(const SingletonSet<U, L2>& other) const {
    // Return a structural Join: {x | x == pivot || x == other.pivot}
    return var<Ω<T, L>> % Ω<T, L>{} |
           [s1 = *this, s2 = other](const T& x) { return s1(x) || s2(x); };
  }

  template <typename U, typename L2>
  constexpr auto operator&(const SingletonSet<U, L2>& other) const {
    // Return a structural Meet: {x | x == pivot && x == other.pivot}
    return var<Ω<T, L>> % Ω<T, L>{} |
           [s1 = *this, s2 = other](const T& x) { return s1(x) && s2(x); };
  }

  // Complement: !{a}
  // In a strict sense, this is the relative complement (Universe \ {a}).
  // For the sake of the lattice, we return the Universal boundary.
  constexpr auto operator!() const { return Ω<T, L>{}; }
};

static_assert(IsPointedSet<SingletonSet<int>, int> &&
                  IsExtensional<SingletonSet<int>>,
              "Mereology: SingletonSet must satisfy the Singleton axiom.");

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

/** @section The_Final_Ontology_Proof
 * Deferred while `dedekind.sets` is being retargeted to the updated
 * `dedekind.category` hub/spoke functor API.
 */
