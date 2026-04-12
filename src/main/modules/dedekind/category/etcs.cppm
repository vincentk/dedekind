/**
 * @file etcs.cppm
 * @partition :etcs
 * @brief Level 0c: Elementary Theory of the Category of Sets (ETCS).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * @section ETCS: The Categorical Axiomatization of Set Theory
 * "In the mathematical development of recent decades, the notion of
 *  set has not only played a fundamental role, but it has itself
 *  passed through a long process of refinement."
 *  — F. William Lawvere, "An Elementary Theory of the Category of Sets"
 *
 * @details
 * This partition provides the axiomatic foundations of ETCS, allowing
 * the Dedekind Universe to treat "Sets" as objects within a category
 * rather than collections defined by membership (ZF).
 *
 * We utilize the 'Ontology Bridge' (SpeciesTraits) to ensure that
 * primitive types (int, bool, size_t) can be treated as first-class
 * categorical objects without violating their C++ nature.
 *
 * @build_order 0
 * @dependency :species, :logic
 *
 * Wikipedia: Elementary Theory of the Category of Sets
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:etcs;

import :morphism;
import :logic;
import :species;
import :topoi;

namespace dedekind::category {

/**
 * @concept IsSetObject
 * @brief ETCS-facing alias for a subobject S ↣ A.
 */
export template <typename S, typename A>
concept IsSetObject = IsSubobject<S, A>;

/**
 * @concept HasTernarySupport
 * @brief Subobject with χ: A -> Ω_K3 (Ternary support classifier).
 */
export template <typename S>
concept HasTernarySupport = requires(S s) {
  typename S::Ambient;
  requires IsSubobject<S, typename S::Ambient>;
  requires std::same_as<Cod<decltype(s.χ)>, Ternary>;
};

/**
 * @concept IsCompatibleSetPair
 * @brief Two subobjects that share ambient species and Ω codomain.
 */
export template <typename S1, typename S2>
concept IsCompatibleSetPair = requires(S1 s1, S2 s2) {
  typename S1::Ambient;
  typename S2::Ambient;
  requires IsSubobject<S1, typename S1::Ambient>;
  requires IsSubobject<S2, typename S2::Ambient>;
  requires std::same_as<typename S1::Ambient, typename S2::Ambient>;
  requires std::same_as<Cod<decltype(s1.χ)>, Cod<decltype(s2.χ)>>;
};

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
 * @brief The Categorical Seal of Set-hood.
 * @details A Set is a Subobject (S ↣ A) that matures into a Small Category.
 * This identifies the set not as a buffer of values, but as a position 
 * within the categorical system of relations.
 /**
 * @section ETCS_Axiom_Mapping The 10 Axioms of ETCS
 * Following the Lawvere-Tierney axiomatisation, the dedekind library maps 
 * the formal requirements of the Category of Sets directly to C++23 structural 
 * invariants. This mapping ensures that the compiler serves as a formal 
 * verification engine for mathematical truth.
 *
 * | ETCS Axiom                     | C++23 Implementation           | Categorical Role              |
 * |:-------------------------------|:-------------------------------|:------------------------------|
 * | **1. Composition**             | `operator>>` / `IsArrow`       | Morphic Associativity         |
 * | **2. Identity**                | `Cat::id_c(x)`                 | Identity Morphism             |
 * | **3. Terminal Object**         | `One` (`std::monostate`)       | Unique Sink (1)               |
 * | **4. Well-Pointedness**        | `IsSet::contains(x) -> Ω`      | Global Elements (1 → X)       |
 * | **5. Cartesian Product**       | `IsProduct` (`std::pair`)      | Product (A × B)               |
 * | **6. Exponentiation**          | `IsExponential` (Lambda NTTP)  | Internal Hom-set (B^A)        |
 * | **7. Equalizers**              | `classify<A>(p)`               | Subobject Classification      |
 * | **8. Empty Set**               | `Zero` (`std::nullptr_t`)      | Initial Object (0)            |
 * | **9. Infinity (NNO)**          | `dedekind.numeric` Grounding   | Natural Numbers Object        |
 * | **10. Axiom of Choice**        | Lattice Dispatcher             | Existential Selection         |
 *
 * @note By anchoring these axioms in Level 0, the subsequent synthesis 
 * of the `dedekind.sets` module inherits a verified, algebraic definition 
 * of Set-hood that enables aggressive structural pruning by the LLVM backend.
 */
export template <typename T>
concept IsSet = 
  // 1. Every Set is an object in a Small Category (Level 0b)
  IsCategory<T> && 
  // 2. Every Set is a Subobject classified by Ω (Level 1)
  requires(T s) {
    typename T::Ambient;
    requires IsSubobject<T, typename T::Ambient>;
    
    /**
     * @section ETCS_Well_Pointedness
     * Membership is the evaluation of the subobject's characteristic 
     * morphism χ at a specific point in the ambient species.
     */
    { s.contains(std::declval<typename T::Ambient>()) } 
      -> std::same_as<Cod<decltype(s.χ)>>;
  };

}  // namespace dedekind::category