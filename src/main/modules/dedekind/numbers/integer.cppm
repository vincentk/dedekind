/**
 * @file dedekind/numbers/integer.cppm
 * @partition :integer
 * @brief ℤ, the universe over @c sets::Integer, and its witnesses.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Die ganzen Zahlen hat der liebe Gott gemacht, alles andere ist
 * Menschenwerk."
 *       ("God made the integers; all else is the work of man.")
 *       -- Leopold Kronecker, Jahresbericht der DMV 2 (1891, reported)
 */
module;

#include <concepts>
#include <functional>
#include <type_traits>

export module dedekind.numbers:integer;

import dedekind.algebra;
import dedekind.category;
import dedekind.order; // ℤ's chain
import dedekind.sets;
import :natural;
export import :cardinality;

namespace dedekind::numbers {
using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;

/** @brief The integer carrier @c Rational<I> defaults to: ℤ's @c Integer.
 *  ℚ slice step 3: deleted with @c Rational<Z> (#1035). */
export using default_integer = dedekind::sets::Integer;

/** @section integer__ℤ
 *  @c ℤ is the universe @c 𝔸<Integer>: Grothendieck's completion of ℕ, exact
 *  above the word as @c ℕ @c = @c 𝔸<Numeral> is, a ring under @c +, @c *
 *  and unary @c -, ordered compatibly with @c + (@c IsOrderedAdditiveGroup),
 *  and no multiplicative group (ℚ is its field of fractions). */
export inline constexpr auto ℤ = dedekind::sets::𝔸<dedekind::sets::Integer>{};

static_assert(
    std::same_as<std::remove_cvref_t<decltype(ℤ)>,
                 dedekind::sets::𝔸<dedekind::sets::Integer, Boole, ℵ_0>>,
    "ℤ is the universe 𝔸<Integer>, countable.");
static_assert(
    dedekind::algebra::IsOrderedAdditiveGroup<dedekind::sets::Integer> &&
        dedekind::algebra::IsOrderedCommutativeRing<dedekind::sets::Integer> &&
        !dedekind::algebra::IsOrderedMultiplicativeGroup<
            dedekind::sets::Integer>,
    "ℤ is an ordered additive group and an ordered commutative ring, not a "
    "multiplicative group: only ±1 are units");
// ℤ inhabits Ddk: an algebraic set, closed under +, * and unary -, which ℕ
// is not (Figure 1, Ddk = Trsk ∩ Alg).
static_assert(dedekind::algebra::IsAlgebraOnSet<
                  decltype(ℤ), std::plus<dedekind::sets::Integer>,
                  std::multiplies<dedekind::sets::Integer>,
                  std::negate<dedekind::sets::Integer>>,
              "ℤ is an algebraic set (Ddk): closed under +, * and unary -.");
static_assert(dedekind::order::IsTotallyOrdered<dedekind::sets::Integer>,
              "ℤ is a chain.");

}  // namespace dedekind::numbers
