/**
 * @file dedekind/category/etcs.cppm
 * @partition :etcs
 * @brief Level 4: Elementary Theory of the Category of Sets (ETCS) facade.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "In the mathematical development of recent decades, the notion of
 *  set has not only played a fundamental role, but it has itself
 *  passed through a long process of refinement."
 *  — F. William Lawvere, "An Elementary Theory of the Category of Sets"
 *
 * @details
 * This partition lifts topoi-level subobject operations into a focused ETCS
 * set interface. In this codebase, a "set" is represented as a subobject
 * classified by a characteristic morphism χ: A → Ω.
 *
 * @section ETCS_Axiom_Mapping Lawvere's 10 Axioms mapped to dedekind
 *
 * The following table maps each of the 10 axioms of Lawvere's ETCS to the
 * corresponding C++23 implementation. Items marked (asp.) are aspirational.
 *
 * | ETCS Axiom                    | C++23 Implementation                 |
 * Partition       |
 * |:------------------------------|:-------------------------------------|:----------------|
 * | **1. Composition**            | `operator>>` / `IsArrow`             |
 * `:morphism`     | | **2. Identity**               | `Identity<T>` /
 * `Cat::id_c(x)`       | `:small`        | | **3. Terminal Object (1)**    |
 * `One` (`std::monostate`)             | `:limit`        | | **4.
 * Well-Pointedness**       | `s.χ(x) → Ω` (global element eval)   | `:topoi` |
 * | **5. Cartesian Product**      | `std::pair` / `IsProduct`            |
 * `:cartesian`    | | **6. Exponentiation (B^A)**   | `Exponential<A,B>` /
 * `IsExponential` | `:cartesian`    | | **7. Subobject Classifier**   |
 * `Subobject<A,χ>` / `classify<A>(p)`  | `:topoi`        | | **8. Empty Set
 * (∅)**          | `Zero` (`std::nullptr_t`)            | `:limit`        | |
 * **9. NNO (ℕ)**               | `SpeciesTraits<unsigned>`             |
 * `:numeric`      | | **10. Axiom of Choice**       | `meet`/`join` lattice
 * dispatcher     | `:etcs` (asp.)  |
 *
 * @see Lawvere, F.W. (1964) "An Elementary Theory of the Category of Sets"
 * @see McLarty, C. (1993) "Numbers can be just what they have to"
 */

module;

#include <concepts>
#include <utility>

export module dedekind.category:etcs;

import :logic;
import :morphism;
import :topoi;

namespace dedekind::category {

/**
 * @concept IsSetObject
 * @brief A categorical set object represented as a subobject S ↣ A.
 */
export template <typename S, typename A>
concept IsSetObject = IsSubobject<S, A> && requires {
  typename S::Ambient;
  requires std::same_as<typename S::Ambient, A>;
};

/**
 * @concept HasTernarySupport
 * @brief True when a set object's classifier returns ternary truth values.
 */
export template <typename S>
concept HasTernarySupport =
    IsSubobject<S, typename S::Ambient> &&
    std::same_as<Cod<decltype(std::declval<S>().χ)>, Ternary>;

/**
 * @concept IsCompatibleSetPair
 * @brief Two set objects over the same ambient species and same Ω codomain.
 */
export template <typename S1, typename S2>
concept IsCompatibleSetPair =
    IsSubobject<S1, typename S1::Ambient> &&
    IsSubobject<S2, typename S2::Ambient> &&
    std::same_as<typename S1::Ambient, typename S2::Ambient> &&
    std::same_as<Cod<decltype(std::declval<S1>().χ)>,
                 Cod<decltype(std::declval<S2>().χ)>>;

/** @brief ETCS intersection: materialize A ∩ B from χ_A ∧ χ_B. */
export template <typename S1, typename S2>
  requires IsCompatibleSetPair<S1, S2>
constexpr auto set_intersection(const S1& lhs, const S2& rhs) {
  using A = typename S1::Ambient;
  return classify<A>(lhs.χ && rhs.χ);
}

/** @brief ETCS union: materialize A ∪ B from χ_A ∨ χ_B. */
export template <typename S1, typename S2>
  requires IsCompatibleSetPair<S1, S2>
constexpr auto set_union(const S1& lhs, const S2& rhs) {
  using A = typename S1::Ambient;
  return classify<A>(lhs.χ || rhs.χ);
}

/** @brief ETCS complement: materialize A^c from ¬χ_A. */
export template <typename S>
  requires IsSubobject<S, typename S::Ambient>
constexpr auto set_complement(const S& s) {
  using A = typename S::Ambient;
  return classify<A>(!s.χ);
}

/** @brief Lattice alias: meet = intersection. */
export template <typename S1, typename S2>
  requires IsCompatibleSetPair<S1, S2>
constexpr auto meet(const S1& lhs, const S2& rhs) {
  return set_intersection(lhs, rhs);
}

/** @brief Lattice alias: join = union. */
export template <typename S1, typename S2>
  requires IsCompatibleSetPair<S1, S2>
constexpr auto join(const S1& lhs, const S2& rhs) {
  return set_union(lhs, rhs);
}

/**
 * @concept IsSet
 * @brief Alias concept for set objects represented as subobjects.
 *
 * @details
 * IsSet intentionally models set-hood via the existing topoi interface:
 * a carrier T is a set exactly when it is a valid subobject over its
 * declared ambient species.
 */
export template <typename T>
concept IsSet = IsSetObject<T, typename T::Ambient>;

}  // namespace dedekind::category
