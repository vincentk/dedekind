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
 * @dependency :algebra, :order
 *
 * Wikipedia: Structuralism (philosophy of mathematics), Cyclic group, Ordered
 * field
 */
module;

#include <concepts>
#include <functional>

export module dedekind.morphologies:archimedean;

import dedekind.algebra; // The Abstract Laws (Groups, Rings)
import dedekind.order;   // The Rules of Relation (IsTotallyOrdered, IsDense)

namespace dedekind::morphologies {
using namespace dedekind::algebra;
using namespace dedekind::order;

/**
 * @concept IsCyclic
 * @brief The Dedekind Chain (Kette): A structure defined by a closed
 *        successor mapping.
 *
 * @details Dedekind's Axiom: A system S is cyclic if there exists a
 *          mapping f: S -> S such that S is the 'Chain' of some
 *          element g (the generator).
 */
export template <typename T>
concept IsCyclic = IsAbelianGroup<T> &&
                   // Removed 'typename' from value access (is_countable)
                   (T::cardinality_type::is_countable == true) &&
                   requires(typename T::element_type a) {
                     {
                       T::successor(a)
                     } -> std::same_as<typename T::element_type>;
                     {
                       T::generator()
                     } -> std::same_as<typename T::element_type>;
                   };

/** @concept IsSimplyInfinite
 *  @brief Dedekind's definition of the Natural/Integer 'Line'.
 */
export template <typename T>
concept IsSimplyInfinite =
    IsCyclic<T> && (T::cardinality_type::is_finite == false);

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

/**
 * @concept IsMinkowskiSummable
 * @brief Species that support set-based addition.
 * @details A + B = { a + b : a ∈ A, b ∈ B }.
 */
export template <typename S>
concept IsMinkowskiSummable =
    IsSet<S> && IsAbelianGroup<typename S::element_type> && requires(S a, S b) {
      { a + b } -> std::same_as<S>;
    };

}  // namespace dedekind::morphologies
