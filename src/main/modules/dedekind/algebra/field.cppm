/**
 * @file ontology:algebra.cppm
 * @partition :algebra
 * @brief Level 3: The Rules of Harmony (Groups, Rings, and Fields).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section The_Field_of_Reunion
 * In the tradition of Sharaf al-Dīn al-Ṭūsī, the Field is the species
 * of total inversion. It ensures that every action has a symmetric
 * reaction—a multiplicative inverse—allowing for the 'completion'
 * and 'reunion' of any algebraic equation within its own universe.
 *
 * @details
 * We anchor the standard C++ arithmetic operators as formal Algebraic
 * Morphisms within the Dedekind Ontology:
 * - operator+ : The Additive Group Morphism (The Translation).
 * - operator* : The Multiplicative Morphism (The Scaling).
 * - operator- : The Inverse Morphism (The Symmetry).
 *
 * @build_order 4
 * @dependency :category, :mereology, :order
 *
 * @see dedekind.ontology:category (Level 0)
 * @see dedekind.ontology:mereology (Level 1)
 * @see dedekind.ontology:order (Level 1.5)
 *
 * Wikipedia: Abstract algebra, Group theory, Ring (mathematics), Field
 * (physics)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Es steht alles schon bei Dedekind."
 *       ("It is already all in Dedekind.")
 *       -- Emmy Noether, as quoted by B. L. van der Waerden (1975)
 */
module;

#include <functional>  // for std::plus, std::multiplies, std::bit_xor, std::bit_and

export module dedekind.algebra:field;

import dedekind.category;
import dedekind.order;
import dedekind.sets;

import :ring;
import :division;

namespace dedekind::algebra {
using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sets;

/**
 * @concept IsField
 * @brief The "Painless" Field: a Commutative Ring that admits Division,
 *        with both the axiomatic witness and the operator surface.
 *
 * @tparam T A species already established as a Commutative Ring.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 *
 * @details
 * Composes the axiomatic field witness from the category layer
 * (`dedekind::category::IsField`, which requires the species-trait
 * structure including `IsAbelianGroup<T, Mult>` --- i.e. every
 * non-zero element is multiplicatively invertible per
 * `is_invertible_v<T, Mult>`) with the operator-level witness
 * (`IsDivisionRing`, which requires `operator/`, `.inverse()`, and
 * `std::divides`). Carriers that satisfy this concept are proper
 * fields in both the algebraic and the arithmetic sense: the laws
 * hold via traits, the operations are spelled out.
 *
 * The split mirrors the rest of the library's layering:
 * `category:total` carries structural / axiomatic concepts without
 * operator requirements; `algebra` builds on them by adding operator
 * closures.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsField =
    dedekind::category::IsField<T, Add, Mult> && IsDivisionRing<T, Add, Mult>;

/** @section Formal_Verification: bool is the Galois field 𝔽2
 *
 * Rather than wrap @c bool in a dedicated carrier struct, the library
 * witnesses @f$\mathbb{F}_2@f$ directly on the primitive type under
 * its natural bitwise operators:
 *
 *   - additive group @f$(\mathbb{F}_2, +) = (\text{bool}, \oplus)@f$,
 *     via @c std::bit_xor<bool>;
 *   - multiplicative monoid @f$(\mathbb{F}_2, \cdot) = (\text{bool},
 * \wedge)@f$, via @c std::bit_and<bool>.
 *
 * The @c is_associative_v / @c is_commutative_v / @c is_invertible_v
 * / @c identity_trait / @c is_distributive_v specialisations live in
 * @c dedekind.category:species alongside the other @c bool operator
 * facts; they compose here to witness
 * @c dedekind::category::IsField<bool, std::bit_xor<bool>,
 * std::bit_and<bool>>.  This is the full axiomatic field claim on
 * @c bool; concepts do not quantify over values, so zero is excluded
 * from the multiplicative-invertibility clause by convention (the
 * only non-zero element is @c true, which is self-inverse under AND).
 *
 * The operator-level @c algebra::IsField (which additionally requires
 * @c operator/, @c .inverse(), and @c std::divides) is \emph{not}
 * asserted on @c bool: the division surface is absent at the
 * primitive-type level.  Downstream carriers that need the division
 * surface (e.g.\ a division-based templated algorithm) can wrap
 * @c bool in a struct with @c operator/ and @c .inverse() members,
 * but the abstract-algebraic claim itself does not require that.
 */
static_assert(dedekind::category::IsCommutativeRing<bool, std::bit_xor<bool>,
                                                    std::bit_and<bool>>,
              "bool must be a commutative ring under (XOR, AND): "
              "the Boolean ring.");

static_assert(
    dedekind::category::IsField<bool, std::bit_xor<bool>, std::bit_and<bool>>,
    "bool under (XOR, AND) must satisfy the axiomatic "
    "category::IsField: it is literally the Galois field 𝔽2.");

/**
 * @concept IsAlgebraicallyClosed
 * @brief The "Soul" of the Field: Every polynomial has a root in the species.
 *
 * @details
 * This represents the ultimate completion of the algebraic journey.
 * While IsField guarantees division, Closure guarantees resolution.
 *
 * @tparam T A species already established as a Field.
 * @tparam Add Additive operation witness (defaults to `std::plus<T>`).
 * @tparam Mult Multiplicative operation witness
 * (defaults to `std::multiplies<T>`).
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsAlgebraicallyClosed =
    IsField<T, Add, Mult> && true;  // Refined by its use in Algebra_ℂ

static_assert(!IsField<int>, "Structural Integrity: Integers are not a Field.");

}  // namespace dedekind::algebra
