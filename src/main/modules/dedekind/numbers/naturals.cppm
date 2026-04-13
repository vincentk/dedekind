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
 * @brief N is a Pointed, Closed, Archimedean, Commutative Monoid
 *        under BOTH Addition and Multiplication.
 * Wikipedia: Semiring, Peano axioms
 */
export template <typename N>
concept IsNatural = std::unsigned_integral<N>;

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

};  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ)>> =
        true;
}  // namespace dedekind::category
