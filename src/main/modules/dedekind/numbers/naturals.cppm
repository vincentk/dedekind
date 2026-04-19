/**
 * @file ontology:numbers.cppm
 * @brief Level 4: The Dictionary of Species (The Registry).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :numbers
 * @build_order 7
 * @dependency :algebra, :topology, :cardinalities
 *
 * @section Numbers: The Realization of the Soul
 * This partition is the final "Registry" of the ontology. It maps concrete
 * C++ types to their formal algebraic and topological identities.
 *
 * @details
 * We "Bless" the coordinate species by verifying their rungs on the ladder:
 * - IsNatural  : N (ℕ) - The Discrete Monoid.
 * - IsInteger  : Z (ℤ) - The Euclidean Group.
 * - IsRational : Q (ℚ) - The Countable Dense Field.
 * - IsReal     : R (ℝ) - The Continuous Dedekind-Complete Field.
 *
 * @section Structural_Mapping
 * This is where we perform the final 'Lifting'. We prove that 'int'
 * satisfies 'Group_ℤ' and that 'double' is a hardware-constrained
 * approximation of 'Field_ℝ'.
 *
 * @anchors C++ Fundamental Types: bool, char, int, long, float, double.
 *
 * Wikipedia: Number, Natural number, Integer, Rational number, Real number
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Having been introduced there to this art with an amazing method of
 * teaching by means of the nine figures of the Indians, I loved the knowledge
 * of such an art to such an extent above all other arts...that I learned with
 * very earnest application...anything to be studied concerning it and its
 * various methods used in Egypt, in Syria, in Greece, in Sicily, and in
 * Provence." — Leonardo Fibonacci, *Liber Abaci*, Prologus (~1202).
 */
module;

#include <concepts>
#include <functional>

export module dedekind.numbers:naturals;

import dedekind.category;
import dedekind.sets;
import :scalars;
import :booleans;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @concept IsNatural
 * @brief Structural concept for a commutative semiring with total order
 *        (the intensional ℕ).
 *
 * @details Deliberately *not* restricted to `std::unsigned_integral<N>` so
 * that user-defined certified natural-number types (e.g.
 * `ExtensionalCardinal<N>`) can satisfy the concept without being built-in C++
 * types.  The required operations are exactly those that characterise ℕ as a
 * commutative semiring with a total order:
 *
 *  - Additive monoid: `+`.
 *  - Multiplicative monoid: `*`.
 *  - Total order: `<=`.
 *  - No subtraction required — that is what distinguishes ℕ from ℤ.
 *
 * **Embedding from `std::unsigned_integral`:** machine unsigned types are the
 * extensional/IEEE-policy approximation of ℕ.  Use
 * `embed_unsigned_integral<N>(v)` to inject a machine value into a certified
 * `IsNatural` domain, and `realize_to_size_t(sentinel)` to project back.
 *
 * Wikipedia: Semiring, Peano axioms
 */
export template <typename N>
concept IsNatural = std::regular<N> && requires(N a, N b) {
  { a + b } -> std::same_as<N>;
  { a * b } -> std::same_as<N>;
  { a <= b } -> std::convertible_to<bool>;
};

/**
 * @concept IsNaturalNumber
 * @brief Alias for the machine/extensional natural-number species.
 *
 * @details `std::unsigned_integral` types are the IEEE-policy realisation of
 * ℕ — they satisfy `IsNatural` structurally (unsigned arithmetic wraps, so
 * the semiring laws hold), but they are identified separately here because
 * they are the *output* of `realize_to_size_t` and the *input* of
 * `embed_unsigned_integral`, not the preferred certified domain for new code.
 */
export template <typename T>
concept IsNaturalNumber = std::unsigned_integral<T>;

/**
 * @concept Monoid_ℕ
 * @brief The Parametric Algebraic Soul of the Natural Numbers.
 *
 * @tparam M The Monoid structure.
 * @tparam E The underlying Element species.
 */
export template <typename M, typename E = typename M::Domain>
concept Monoid_ℕ = IsNatural<E> && requires(const M& m) {
  { m.cardinality() } -> std::same_as<std::size_t>;
};

/**
 * @brief Canonical embedding 𝔹 ↪ ℕ: bool → unsigned.
 * @details False maps to 0, True maps to 1.
 */
export inline constexpr auto embed_𝔹_ℕ =
    arrow<bool, unsigned>([](const bool& b) noexcept { return b ? 1u : 0u; });

/**
 * @brief Canonical injection from `std::unsigned_integral` into any
 *        `IsNatural` domain `N` via its single-argument constructor.
 *
 * @details `std::unsigned_integral` types are the machine/extensional
 * approximation of ℕ.  This arrow is the Liskov injection into a certified
 * `IsNatural` domain (e.g. `ExtensionalCardinal<K>`).  The reverse direction —
 * projecting a certified natural back to a machine width — is
 * `realize_to_size_t(sentinel)`.
 *
 * @tparam N  The target `IsNatural` type.
 * @tparam U  A `std::unsigned_integral` source type (deduced).
 */
export template <IsNatural N, std::unsigned_integral U>
constexpr N embed_unsigned_integral(U v) {
  return N{v};
}

};  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ)>> =
        true;
}  // namespace dedekind::category
