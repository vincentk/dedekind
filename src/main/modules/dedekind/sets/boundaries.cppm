/**
 * @file dedekind/sets/boundaries.cppm
 * @brief The Extremal Identities: Universal (V) and Empty (∅) Sets.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @module dedekind.sets:boundaries
 * @dependency dedekind.ontology
 *
 * @section The_Structural_Limits: ⊥ and ⊤
 * In the Dedekind topos, the boundaries of a Species define the 'North
 * and South poles' of the set-lattice. This partition implements the
 * identities required for a Bounded Lattice over any Species:
 * - UniversalSet (V): The 'Top' (⊤). The extensional whole of a Species.
 * - EmptySet (∅): The 'Bottom' (⊥). The mereological remainder of the whole.
 *
 * @details
 * These sets are the 'First-Class Citizens' of the Mereological System:
 * 1. Self-Awareness: Each boundary knows its 'Ambient Species' (The context).
 * 2. Duality: They are mutually defined via the Complement Morphism (!).
 * 3. Identity: They serve as the unit elements for Union (|) and Intersection
 * (&).
 *
 * @section Semantic_Role
 * While a SingletonSet represents an 'Atomic Part', the boundaries
 * represent the 'Absolute State' of the Species. In a 'Family' (A Set
 * of Sets), these objects serve as the terminal bounds of the collection.
 *
 * @tparam Species The underlying domain (e.g., Integers, Booleans).
 * @tparam L The Subobject Classifier (Ω) governing the truth logic.
 *
 * Wikipedia: Universal set, Empty set, Bounded lattice, Identity element
 */
module;

#include <compare>
#include <concepts>
#include <functional>

export module dedekind.sets:boundaries;

import dedekind.ontology;

using namespace dedekind::ontology;

/**
 * @section Mereology: The study of parts and wholes.
 * @section Mereology: The Hierarchy of Order.
 */
namespace dedekind::sets {

/** @brief ∅: The Initial Object. Extensional (Size 0). */
export template <typename T, typename L = ClassicalLogic>
struct Ø final {
  using element_type = T;
  using logic_species = L;
  using cardinality_type = Finite;
  using base_set_type = Ø<T, L>;

  /** @section Lattice_Laws: Absorption and Identity */
  constexpr Ø operator&(const Ø&) const { return *this; }
  constexpr Ø operator|(const Ø&) const { return *this; }

  /** @section Extensionality_Proof */
  constexpr std::size_t size() const { return 0; }

  // The Duality: !∅ = V
  // Forward declaration to satisfy the compiler for the UniversalSet.
  constexpr auto operator!() const;

  // The Axiom: Total Absence
  constexpr typename L::type operator()(const T&) const { return L::False; }

  constexpr auto operator<=>(const Ø&) const = default;

  // Required by IsInitialObject
  constexpr cardinality_type cardinality() const { return cardinality_type{}; }
  constexpr std::size_t upper_bound() const { return 0; }

  /** @section Lattice_Laws: The Bottom (⊥) */

  // 1. Ø | S = S (Identity for Union)
  template <typename S>
    requires requires(S s, T v) {
      { s(v) };
    }
  friend constexpr S operator|(const Ø&, const S& s) {
    return s;
  }

  // 2. Ø & S = Ø (Annihilation for Intersection)
  template <typename S>
    requires requires(S s, T v) {
      { s(v) };
    }
  friend constexpr Ø operator&(const Ø& self, const S&) {
    return self;
  }

  /** @section Symmetry: S | Ø and S & Ø */
  template <typename S>
    requires requires(S s, T v) {
      { s(v) };
    }
  friend constexpr S operator|(const S& s, const Ø&) {
    return s;
  }

  template <typename S>
    requires requires(S s, T v) {
      { s(v) };
    }
  friend constexpr Ø operator&(const S&, const Ø& self) {
    return self;
  }
};

/**
 * @struct UniversalSet
 * @brief U: The Terminal Object.
 * @details Intentional but Decidable: The rule "x ∈ U" always returns True.
 */
export template <typename T, typename L = ClassicalLogic, typename C = ℵ_0>
struct Ω final {
  using element_type = T;
  using cardinality_type = C;
  using base_set_type = Ω<T, L>;
  using logic_species = L;

  constexpr auto operator!() const { return Ø<T, L>{}; }

  constexpr auto operator<=>(const Ω&) const = default;

  /** @section Lattice_Laws: Absorption and Identity */
  constexpr Ω operator&(const Ω&) const { return *this; }
  constexpr Ω operator|(const Ω&) const { return *this; }

  // Note: You'll eventually want overloads for:
  // Universal | Any = Universal
  // Universal & Any = Any

  // The Axiom: Total Presence
  constexpr typename L::type operator()(const T&) const { return L::True; }

  constexpr cardinality_type cardinality() const { return cardinality_type{}; }

  /** @section Lattice_Laws: The Top (⊤) */

  // 1. Ω | S = Ω (Annihilation)
  // We use a "Shallow" check: Does S look like a set (is it callable)?
  template <typename S>
    requires requires(S s, T v) {
      { s(v) };
    }
  friend constexpr Ω operator|(const Ω& self, const S&) {
    return self;
  }

  // 2. Ω & S = S (Identity)
  template <typename S>
    requires requires(S s, T v) {
      { s(v) };
    }
  friend constexpr S operator&(const Ω&, const S& s) {
    return s;
  }

  /** @section Symmetry: S | Ω and S & Ω */
  template <typename S>
    requires requires(S s, T v) {
      { s(v) };
    }
  friend constexpr Ω operator|(const S&, const Ω& self) {
    return self;
  }

  template <typename S>
    requires requires(S s, T v) {
      { s(v) };
    }
  friend constexpr S operator&(const S& s, const Ω&) {
    return s;
  }
};

template <typename T, typename L>
constexpr auto Ø<T, L>::operator!() const {
  return Ω<T, L>{};
}

static_assert(IsSet<Ω<int>>, "The universal set must satisfy IsSet.");

static_assert(IsSet<Ø<int>>);

/** @section The_Seal_of_Initiality */
// This is your 'override'. If EmptySet fails the concept,
// the build stops right here with a clear error.
static_assert(IsInitialObject<Ø<int>>,
              "Ø must satisfy the Initial Object axiom.");

};  // namespace dedekind::sets
