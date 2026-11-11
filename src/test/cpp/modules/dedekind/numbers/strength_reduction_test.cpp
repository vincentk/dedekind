/** @file dedekind/numbers/strength_reduction_test.cpp
 *
 * @brief The rungs of the number tower as embeddings: @c unsigned ↪ ℕ is
 * monic, @c ℤ ↪ ℚ and @c ℚ ↪ ℝ are ring embeddings (@b S legs, witnessed as
 * @c EmbedsAsSubalgebra) that preserve the arithmetic of the intermediate
 * sets; @c unsigned @c = @c ℤ/2^wℤ is the periodic base
 * (@c IsCongruenceQuotient); and the graphs of the rungs compose relationally,
 * with an arithmetic arrow between them.  The chain through ℕ ↪ ℤ awaits ℤ's
 * honest carrier (#1035).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <utility>

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.numbers;
import dedekind.order;
import dedekind.relational;
import dedekind.sets;

using namespace dedekind::numbers;
using namespace dedekind::category;
using namespace dedekind::morphologies;
using namespace dedekind::order;
using namespace dedekind::sets;
using namespace dedekind::relational;

namespace {

using R2 = QuadraticReal<2>;  // ℝ = ℚ(√2) coat-hanger carrier

// =====================  S: the chain is monic embeddings =====================
static_assert(IsMonicArrow<std::decay_t<decltype(embed_uint_ℕ_)>>,
              "unsigned ↪ ℕ is monic.");
static_assert(
    dedekind::algebra::EmbedsAsSubalgebra<std::decay_t<decltype(embed_ℤ_ℚ_)>>,
    "ℤ ↪ ℚ is a Birkhoff S-leg (monic ring embedding).");
static_assert(
    dedekind::algebra::EmbedsAsSubalgebra<std::decay_t<decltype(embed_ℚ_ℝ)>>,
    "ℚ ↪ ℝ is a Birkhoff S-leg (monic ring embedding).");

// ==============  Arithmetic on the intermediate sets (ring rungs) ============
// The upper rungs preserve +, × (they are ring embeddings) — arithmetic done in
// ℤ / ℚ agrees with arithmetic done after embedding into ℝ.
static_assert(embed_ℤ_ℚ_(Integer{4} + Integer{6}) ==
                  embed_ℤ_ℚ_(Integer{4}) + embed_ℤ_ℚ_(Integer{6}),
              "ℤ ↪ ℚ preserves + on the intermediate ℤ set.");
static_assert(embed_ℚ_ℝ(Rational{3, 2} * Rational{4, 1}) ==
                  embed_ℚ_ℝ(Rational{3, 2}) * embed_ℚ_ℝ(Rational{4, 1}),
              "ℚ ↪ ℝ preserves × on the intermediate ℚ set.");

// ================  Modular quotient: unsigned = ℤ/2^w is the base ============
static_assert(IsCongruenceQuotient<Modular<8u>, std::plus<unsigned>>,
              "unsigned's + on ℤ/8ℤ is a congruence quotient of ℤ (8 | 2^w).");
static_assert(!IsCongruenceQuotient<Modular<12u>, std::plus<unsigned>>,
              "Modular<12> is NOT a machine congruence quotient (12 ∤ 2^w).");

// =========  RELATIONAL composition: Γ_f ; Γ_g, arithmetic BETWEEN rungs ======
// Here we compose the GRAPHS directly
// via the relative product Γ_f ; Γ_g = Γ_{f;g} — the ∃b over the ℚ/ℝ
// intermediate discharged by functionality (§graph.cppm).  This is the payoff
// the user asked for: an arithmetic arrow (×2 on ℚ) sits BETWEEN two inclusion
// relations, all composed relationally.
constexpr auto scale2 = dedekind::category::arrow<Rational, Rational>(
    [](const Rational& q) { return q * Rational{2}; });

// Γ(ℤ↪ℚ) ; Γ(×2 on ℚ) ; Γ(ℚ↪ℝ)  :  a functional relation ℤ ↬ ℝ, r == 2·z.
constexpr auto Γ_scaled = graph(embed_ℤ_ℚ_) >> graph(scale2) >>
                          graph(embed_ℚ_ℝ);
static_assert(IsSet<decltype(Γ_scaled)>,
              "the relational composite is again a functional graph ℤ ↬ ℝ.");
static_assert(
    Γ_scaled(std::pair{Integer{5}, R2{10}}),
    "(5, 10) ∈ Γ: 2·5 = 10 in ℝ — arithmetic embedded between rungs.");
static_assert(!Γ_scaled(std::pair{Integer{5}, R2{11}}), "(5, 11) ∉ Γ.");
// Relational composition and arrow composition coincide (Γ_f;Γ_g == Γ_{f;g}).
static_assert(Γ_scaled(std::pair{Integer{3}, R2{6}}) ==
                  graph(embed_ℤ_ℚ_ >> scale2 >>
                        embed_ℚ_ℝ)(std::pair{Integer{3}, R2{6}}),
              "relational ; and arrow ∘ agree pointwise.");

}  // namespace

TEST_CASE(
    "Relational composition of the tower graphs, arithmetic between rungs",
    "[numbers][tower][strength-reduction][relational]") {
  // graph(ℤ↪ℚ) ; graph(×2 on ℚ) ; graph(ℚ↪ℝ) at runtime: the functional
  // relative product over the ℚ/ℝ intermediates, r == 2·z.
  const auto Γ = graph(embed_ℤ_ℚ_) >> graph(scale2) >> graph(embed_ℚ_ℝ);
  for (int z = 0; z <= 6; ++z) {
    CAPTURE(z);
    CHECK(Γ(std::pair{Integer{z}, R2{2 * z}}));
    CHECK_FALSE(Γ(std::pair{Integer{z}, R2{2 * z + 1}}));
  }
}
