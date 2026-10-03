/**
 * @file dedekind/numbers/real.cppm
 * @partition :real
 * @brief The real line: @c ℝ (the coat-hanger --- a set-indexed field over the
 *        order-complete carrier @c QuadraticReal<2>) and the machine ambient
 *        @c ℝ_d over the finite doubles @c 𝕃<double>.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Die Zahlen sind freie Schöpfungen des menschlichen Geistes."
 *       ("Numbers are free creations of the human mind.")
 *       -- Richard Dedekind, Was sind und was sollen die Zahlen? (1888)
 */
module;

#include <concepts>
#include <type_traits>

export module dedekind.numbers:real;

import dedekind.algebra; // HasRingOperators / HasFieldOperators (canonical-spine witnesses)
import dedekind.category;
import dedekind.morphologies; // 𝕃<double>, the carrier of ℝ_d
import dedekind.order;
import dedekind.sets;
import :quadratic;  // QuadraticReal — the coat-hanger carrier ℝ points to
import :rational;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::order;
using namespace dedekind::sets;

// Canonical machine realization for the real scalar carrier.
export using machine_real_scalar = double;

/** @brief The canonical real-number universe @c ℝ @c = @c
 *         𝔸<QuadraticReal<2>, Boole, ℶ_1> --- the coat-hanger.
 *
 *  @details Per #559 the named species symbols denote @b universe values
 *  (constexpr @c 𝔸 instances over the carrier).  @c ℝ's carrier is
 *  the decidable FIELD @c QuadraticReal<2> @c = ℚ(√2): the @b universe @c ℝ is
 * a set-indexed field (@c algebra::IsField, as @c ℚ @c = @c 𝔸<Rational> is),
 * and its @b carrier is order-complete in the library's @b structural surrogate
 *  sense (totally ordered + dense + extrema).  So the Ddk algebra hangs off
 *  @c ℝ the way analysis bootstraps from the reals.
 *
 *  @b Honest @b imperfection: ℚ(√2) is @b countable and so @b not genuinely
 *  Dedekind-complete (it only passes the surrogate, which @c ℚ passes too); the
 *  @c ℶ_1 cardinality tags the continuum we @b model, not the materialised
 *  carrier.  Machine-@c double computation lives on @c ℝ_d (below).  Further
 *  extensions (numerical subalgebras) and transcendentals (symbolic @c Expr)
 *  grow it.  A documented placeholder --- not a false postulate on an
 *  uninhabited carrier.
 *
 *  @c ℂ and @c 𝔻 are the sibling coat-hanger universe values
 *  (ℂ = ℝ[i]/(i²+1) = 𝔸<Complex<QuadraticReal<2>>>, 𝔻 = ℝ[ε]/(ε²) =
 *  𝔸<Dual<QuadraticReal<2>>>), the 2nd-order quotient functors over this same
 *  ℝ.
 */
export inline constexpr auto ℝ =
    dedekind::sets::𝔸<QuadraticReal<2>, Boole, ℶ_1>{};

static_assert(std::same_as<std::remove_cvref_t<decltype(ℝ)>,
                           dedekind::sets::𝔸<QuadraticReal<2>, Boole, ℶ_1>>,
              "ℝ is the universe 𝔸<QuadraticReal<2>, Boole, ℶ_1> — the "
              "coat-hanger realised as ℚ(√2).");
static_assert(std::same_as<typename std::remove_cvref_t<decltype(ℝ)>::Domain,
                           QuadraticReal<2>>,
              "ℝ's carrier IS QuadraticReal<2> = ℚ(√2).");

// The coat-hanger is load-bearing at TWO distinct levels (not one value
// satisfying both concepts): the set-indexed @c algebra::IsField holds on the
// UNIVERSE ℝ (exactly as it does on ℚ = 𝔸⟨Rational⟩), while order-completeness
// is a CARRIER property — @c IsDedekindComplete is an order concept a
// Universe does not itself model — so it is asserted on ℚ(√2).  The claim
// is therefore: ℝ is a field, and its carrier is order-complete (surrogate).
static_assert(dedekind::algebra::IsField<std::remove_cvref_t<decltype(ℝ)>>,
              "ℝ (the universe) is a set-indexed field, like ℚ = 𝔸⟨Rational⟩.");
static_assert(IsDedekindComplete<QuadraticReal<2>>,
              "ℝ's CARRIER ℚ(√2) is order-complete (structural surrogate) — a "
              "carrier-level property, not a property of the set ℝ itself.");

/** @brief The machine-real ambient: the finite doubles @c 𝕃<double>.
 *
 *  @details NaN and +/-inf are excluded by the carrier's type, so the order
 *  is total and membership is decidable: @c ℝ_d is @c Boole, like @c ℝ.  The
 *  @c ℶ_1 tag names the continuum @c ℝ_d approximates, as it does on @c ℝ;
 *  the carrier itself is the finite set of dyadic rationals a double holds.
 *  Only the @c (min, max) lattice reduct is claimed: IEEE rounding breaks
 *  the associativity of @c + and @c *.  Rule of thumb: compute on @c ℝ_d;
 *  model on @c ℝ. */
export inline constexpr auto ℝ_d =
    dedekind::sets::𝔸<dedekind::morphologies::𝕃<machine_real_scalar>, Boole,
                      ℶ_1>{};

}  // namespace dedekind::numbers
