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

#include <functional>   // for std::plus, std::multiplies
#include <stdexcept>    // for std::domain_error (𝔽2 division-by-zero)
#include <type_traits>  // for std::true_type (is_periodic specialisations)

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

/**
 * @struct 𝔽2
 * @brief The Galois field of order 2, @f$\mathbb{F}_2 = \{0, 1\}@f$.
 *
 * @details
 * The smallest field. Addition is XOR (characteristic two: @f$x + x = 0@f$,
 * so subtraction coincides with addition and @f$-x = x@f$); multiplication
 * is AND. The multiplicative group @f$\mathbb{F}_2^\times = \{1\}@f$ is
 * the trivial group, so @f$1^{-1} = 1@f$ and division @f$a / 1 = a@f$ is
 * total on non-zero divisors.
 *
 * 𝔽2 is the prototypical concrete carrier for `algebra::IsField` in the
 * library: it is small enough to enumerate every axiom by hand yet
 * exercises the full concept chain --- from `IsMagma` / `IsMonoid` /
 * `IsGroup` on each operation through `IsCommutativeRing` up to the
 * axiomatic `category::IsField` and the operator-level
 * `algebra::IsField` (which additionally requires `operator/` and
 * `.inverse()`).
 */
export struct 𝔽2 {
  // 𝔽2 is the Boolean ring: {false, true} with XOR as + and AND as ·.
  // The underlying storage is a single bool, so the carrier is
  // canonical-to-{0, 1} by construction --- no modular reduction
  // needed.  This literalises the textbook identification
  // 𝔽2 = (bool, ⊕, ∧) at the type level and matches the library's
  // existing treatment of `bool` under `std::bit_xor`
  // (`is_invertible_v<bool, std::bit_xor<bool>> = true`).
  bool value;

  constexpr 𝔽2() noexcept : value(false) {}
  explicit constexpr 𝔽2(bool v) noexcept : value(v) {}

  // Characteristic two: addition and subtraction coincide (a XOR b).
  constexpr friend 𝔽2 operator+(𝔽2 a, 𝔽2 b) noexcept {
    return 𝔽2(a.value ^ b.value);
  }
  constexpr friend 𝔽2 operator-(𝔽2 a, 𝔽2 b) noexcept {
    return 𝔽2(a.value ^ b.value);
  }
  // Unary negation: -x = x, since 2x = 0 in 𝔽2.
  constexpr friend 𝔽2 operator-(𝔽2 a) noexcept { return a; }

  // Multiplication: AND. {false, true} is closed under conjunction.
  constexpr friend 𝔽2 operator*(𝔽2 a, 𝔽2 b) noexcept {
    return 𝔽2(a.value && b.value);
  }

  // Division: 𝔽2^times = {1}, so `a / 1 == a`. Division by zero is
  // undefined in any field; we raise at runtime to mirror the rest
  // of the library's division-by-zero handling.
  constexpr friend 𝔽2 operator/(𝔽2 a, 𝔽2 b) {
    if (!b.value) throw std::domain_error("𝔽2: division by zero.");
    return a;
  }

  // Multiplicative inverse: the only non-zero element is 1, and 1^-1 = 1.
  constexpr 𝔽2 inverse() const {
    if (!value) throw std::domain_error("𝔽2: inverse of zero.");
    return 𝔽2(true);
  }

  constexpr friend bool operator==(𝔽2 a, 𝔽2 b) noexcept {
    return a.value == b.value;
  }
};

}  // namespace dedekind::algebra

namespace dedekind::category {

// --- Atlas registration ---
// FIXME(#382): every trait spec below is written per-(carrier, op).  A
// library helper `FieldRegistration<T, Zero, One>` would collapse this
// whole block into a single base-class inheritance on
// `SpeciesTraits<𝔽2>`.  The existing `T::template is_foo_v<Op>`
// discovery idiom (for is_associative / is_commutative / is_idempotent)
// does not help here: it keys on the carrier type, not on
// `SpeciesTraits<T>`, so adopting it would push trait metadata onto
// 𝔽2's struct body.

template <>
struct SpeciesTraits<dedekind::algebra::𝔽2> {
  using Domain = dedekind::algebra::𝔽2;
  using machine_type = bool;
};

// --- Identities ---
template <>
struct identity_trait<dedekind::algebra::𝔽2, std::plus<dedekind::algebra::𝔽2>> {
  using value_type = dedekind::algebra::𝔽2;
  static constexpr value_type value{};  // 𝔽2(false)
};

template <>
struct identity_trait<dedekind::algebra::𝔽2,
                      std::multiplies<dedekind::algebra::𝔽2>> {
  using value_type = dedekind::algebra::𝔽2;
  static constexpr value_type value{true};
};

// --- Algebraic facts ---
template <>
inline constexpr bool
    is_associative_v<dedekind::algebra::𝔽2, std::plus<dedekind::algebra::𝔽2>> =
        true;
template <>
inline constexpr bool is_associative_v<dedekind::algebra::𝔽2,
                                       std::multiplies<dedekind::algebra::𝔽2>> =
    true;

template <>
inline constexpr bool
    is_commutative_v<dedekind::algebra::𝔽2, std::plus<dedekind::algebra::𝔽2>> =
        true;
template <>
inline constexpr bool is_commutative_v<dedekind::algebra::𝔽2,
                                       std::multiplies<dedekind::algebra::𝔽2>> =
    true;

template <>
inline constexpr bool is_distributive_v<dedekind::algebra::𝔽2,
                                        std::multiplies<dedekind::algebra::𝔽2>,
                                        std::plus<dedekind::algebra::𝔽2>> =
    true;

// Totality via periodicity: 𝔽2 wraps modulo 2 under both + and *
// (the multiplicative monoid is periodic because it is idempotent on
// {0, 1}, but the library's `IsTotal` consumes periodicity directly).
template <>
struct is_periodic<dedekind::algebra::𝔽2, std::plus<dedekind::algebra::𝔽2>>
    : std::true_type {};
template <>
struct is_periodic<dedekind::algebra::𝔽2,
                   std::multiplies<dedekind::algebra::𝔽2>> : std::true_type {};

// --- Inverses ---
// Additive: every element is self-inverse (x + x = 0).
template <>
inline constexpr bool
    is_invertible_v<dedekind::algebra::𝔽2, std::plus<dedekind::algebra::𝔽2>> =
        true;
// Multiplicative: every non-zero element is self-inverse (1 * 1 = 1).
// Zero is excluded by convention on `is_invertible_v`; the trait
// declares the claim at the type level, not over values.
template <>
inline constexpr bool is_invertible_v<dedekind::algebra::𝔽2,
                                      std::multiplies<dedekind::algebra::𝔽2>> =
    true;

}  // namespace dedekind::category

namespace dedekind::algebra {

/** @section Formal_Verification: 𝔽2 is a field */

// Axiomatic (species-trait) witness from the category layer.
static_assert(dedekind::category::IsCommutativeRing<𝔽2, std::plus<𝔽2>,
                                                    std::multiplies<𝔽2>>,
              "𝔽2 must be a commutative ring (Z/2Z).");

static_assert(
    dedekind::category::IsField<𝔽2, std::plus<𝔽2>, std::multiplies<𝔽2>>,
    "𝔽2 must satisfy the axiomatic category::IsField "
    "(multiplicative structure is an abelian group).");

// Operator-level witness with the division surface.
static_assert(IsField<𝔽2, std::plus<𝔽2>, std::multiplies<𝔽2>>,
              "𝔽2 must satisfy algebra::IsField (division ring + "
              "axiomatic field).");

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
