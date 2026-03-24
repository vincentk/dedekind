/**
 * @file ontology:morphologies.cppm
 * @partition :morphologies
 * @brief Level 3.5: The Study of Algebraic Forms (Les Morphismes des
 * Structures).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Morphologies: The Synthesis of Law and Scale
 * In the Bourbaki tradition, a Morphology is the study of a structure as it
 * takes a specific form under constraints of Magnitude (Cardinality) or
 * Relation (Order).
 *
 * This partition defines the "Realized" species:
 * - Cyclic Structures: Algebra constrained by a Finite Modulus.
 * - Ordered Fields: Algebra constrained by a Total Order.
 * - Archimedean Species: Algebra constrained by the Successor Morphism.
 *
 * @build_order 6
 * @dependency :algebra, :cardinalities, :order
 *
 * Wikipedia: Structuralism (philosophy of mathematics), Cyclic group, Ordered
 * field
 */
module;

#include <concepts>
#include <functional>

export module dedekind.ontology:morphologies;

import :algebra;        // The Abstract Laws (Groups, Rings)
import :cardinalities;  // The Scale of Magnitude (IsFinite, IsCountable)
import :order;          // The Rules of Relation (IsTotallyOrdered, IsDense)

namespace dedekind::ontology {

/**
 * @concept IsCyclic
 * @brief The "Clock" Soul: An Abelian Group that wraps after n steps.
 * @details A structure is Cyclic if its cardinality is Finite and matches
 *          its own Modulus.
 */
export template <typename T>
concept IsCyclic =
    IsAbelianGroup<T> && IsFinite<typename T::cardinality_type> &&
    requires(typename T::element_type a) {
      /** @brief The Modulus: The circumference/order of the cycle. */
      { T::modulus() } -> std::convertible_to<typename T::element_type>;

      /** @brief The Remainder Morphism: x mod n. */
      { a % T::modulus() } -> std::same_as<typename T::element_type>;

      /** @brief Axiom: The set is its own remainder. */
      requires(a % T::modulus() == a);

      /** @brief Proof: The size of the set matches the cycle length. */
      requires(T::cardinality() == T::modulus());
    };

/**
 * @concept IsCyclicRing
 * @brief The Modular Arithmetic morphology (Z/nZ).
 */
export template <typename T>
concept IsCyclicRing = IsCommutativeRing<T> && IsCyclic<T>;

/**
 * @concept IsOrderedField
 * @brief A Field morphology where operations preserve the Total Order.
 * @details Axiom: If a < b, then a + c < b + c.
 *          Axiom: If a < b and 0 < c, then ac < bc.
 */
export template <typename T>
concept IsOrderedField = IsField<T> && IsTotallyOrdered<T>;

/**
 * @concept IsArchimedeanField
 * @brief A morphology representing "Measurable" continuous space.
 * @details Every element can be exceeded by repeated addition of the identity.
 */
export template <typename T>
concept IsArchimedeanField = IsOrderedField<T> && IsArchimedean<T>;

/**
 * @concept IsDedekindCompleteField
 * @brief The "Absolute" Morphology: The Real Number Line (R).
 * @details A Field that is both Archimedean and Dedekind-Complete.
 */
export template <typename T>
concept IsDedekindCompleteField =
    IsArchimedeanField<T> && IsDedekindComplete<T>;

}  // namespace dedekind::ontology
