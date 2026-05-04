/**
 * @file dedekind/algebra/field.cppm
 * @partition :algebra
 * @brief The Rules of Harmony (Groups, Rings, and Fields).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section field__The_Field_of_Reunion
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
 * @see dedekind.ontology:category
 * @see dedekind.ontology:mereology
 * @see dedekind.ontology:order
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
#include <type_traits>  // for std::is_arithmetic_v (HasFieldOperators std-library analog)

export module dedekind.algebra:field;

import dedekind.category;
import dedekind.order;
import dedekind.sets;

import :ring;
import :group;  // HasGroupOperatorsMul (HasFieldOperators's mul half)
import :division;

namespace dedekind::algebra {
using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sets;

/**
 * @concept HasFieldOperators
 * @brief @b Pure @b syntactic @b shape: T closes strictly under the
 *        full field operator surface @c +, binary @c -, unary @c -,
 *        @c *, @c /, and exposes the multiplicative unit @c T{1}.
 *
 * @details
 * The aggregator concept for the field-operator surface.  Equal in
 * content to @c HasRingOperators<T> @c && @c HasGroupOperatorsMul<T>:
 *
 *   - @c HasRingOperators<T> --- @c +, binary @c -, unary @c -,
 *     @c * close strictly on @c T (the additive-group + ring surface);
 *   - @c HasGroupOperatorsMul<T> --- @c *, @c /, @c T{1} close
 *     strictly on @c T (the multiplicative-group surface).
 *
 * The shared @c * clause is idempotent in C++ concepts; the union
 * names the full set of operators a field-style callsite needs to
 * write plain field arithmetic in C++ syntax.
 *
 * Canonical positive witness: @c Rational<I> for any
 * @c HasRingOperators @c I.
 *
 * Shape vs.\ semantics: @c HasFieldOperators<unsigned int> @b fires
 * (the literal @c +, @c -, @c *, @c / all close on @c unsigned int
 * and @c unsigned int{1} is well-formed), even though the textbook
 * claim "@c unsigned int is a field" is false (integer @c / is
 * truncation, not field inverse; @c 5/3 = @c 1, not @c 5/3).  The
 * semantic claim lives in @c IsField, gated by the species-trait
 * registry; @c HasFieldOperators is shape-only.  The shape concept
 * @b does refuse on narrow unsigned types (@c unsigned char, @c
 * unsigned short --- integer promotion lifts the result of @c +
 * and @c * to @c int) and on @c bool (same reason).
 *
 * Introduced under #394 as the user-requested ℚ-deal companion of
 * @c HasRingOperators / @c HasGroupOperatorsAdd.
 *
 * @section field__StdLibrary_Analog
 * The C++ standard library's closest analog is @c std::is_arithmetic_v<T>
 * (in @c <type_traits>): @c true iff @c T is one of the built-in
 * arithmetic types (the integral types --- including @c bool,
 * @c char, the signed/unsigned variants --- and the floating-point
 * types).  The two predicates are not equivalent but carry a clear
 * relationship:
 *
 *   - @c HasFieldOperators is @b broader: it accepts user-defined
 *     types whose literal field operator surface closes
 *     (@c Rational<I>, @c Complex<F>, @c Real<F>); @c
 *     std::is_arithmetic does not lift to user types.
 *
 *   - @c HasFieldOperators is @b narrower in two specific places where
 *     the literal-operator surface does NOT close on @c T: @c bool
 *     (logical operators replace the arithmetic role; @c + / @c *
 *     promote to @c int) and the narrow unsigned types
 *     (@c unsigned char, @c unsigned short --- integer promotion
 *     lifts results to @c int, breaking @c T-closure).
 *
 * In short: @c HasFieldOperators is the user-extensible refinement of
 * what @c std::is_arithmetic names mechanically for the built-in
 * types only.  The static_asserts below pin the relationship at the
 * built-in carriers where the two agree and where they intentionally
 * diverge.
 *
 * @see https://en.cppreference.com/w/cpp/types/is_arithmetic
 */
export template <typename T>
concept HasFieldOperators = HasRingOperators<T> && HasGroupOperatorsMul<T>;

// std::is_arithmetic ↔ HasFieldOperators: the built-in arithmetic
// types where the literal field-operator surface closes on T.
static_assert(std::is_arithmetic_v<int> && HasFieldOperators<int>);
static_assert(std::is_arithmetic_v<long> && HasFieldOperators<long>);
static_assert(std::is_arithmetic_v<float> && HasFieldOperators<float>);
static_assert(std::is_arithmetic_v<double> && HasFieldOperators<double>);

// Where they diverge: bool and the narrow unsigned types satisfy
// std::is_arithmetic but not HasFieldOperators (literal-operator
// surface does not close on T due to integer promotion lifting
// results to int).
static_assert(std::is_arithmetic_v<bool> && !HasFieldOperators<bool>);
static_assert(std::is_arithmetic_v<unsigned char> &&
              !HasFieldOperators<unsigned char>);
static_assert(std::is_arithmetic_v<unsigned short> &&
              !HasFieldOperators<unsigned short>);

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

/** @section field__Formal_Verification: bool is the Galois field 𝔽2
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
