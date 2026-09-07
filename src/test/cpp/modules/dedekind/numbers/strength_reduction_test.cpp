/** @file dedekind/numbers/strength_reduction_test.cpp
 *
 * @brief Strength reduction on the number tower: pull a ℝ-halfspace back
 * through the composite inclusion chain to a native, decidable, MODULAR
 * unsigned comparison.
 *
 * @f[
 *   \texttt{unsigned} \;\hookrightarrow\; \mathbb{N} \;\hookrightarrow\;
 *   \mathbb{Z} \;\hookrightarrow\; \mathbb{Q} \;\hookrightarrow\; \mathbb{R}
 * @f]
 *
 * The chain is four monic, order-preserving embeddings (@b S legs).  The upper
 * two rungs (@c embed_ℤ_ℚ_, @c embed_ℚ_ℝ) are @b ring embeddings, witnessed as
 * @c EmbedsAsSubalgebra; the lower two (@c unsigned ↪ ℕ, @c ℕ ↪ ℤ) are
 * witnessed
 * @c IsMonicArrow --- ℕ is a rig, not a ring, so the ring-embedding claim
 * starts at ℤ ↪ ℚ.  Composed into one arrow @c Φ, its graph @c graph(Φ) is the
 * composite Trsk relation
 * @f$\texttt{unsigned}\times\mathbb{R}\;|\;r=\Phi(u)@f$ (an @c IsRelation).
 *
 * Applying the halfspace criterion @f$\{x \le 10\}@f$ on the ℝ image and
 * recovering the pre-image in @c unsigned is a @b strength reduction: the
 * composite predicate @f$u \mapsto (\Phi(u) \le 10)@f$ (embed through four
 * carriers, compare in @f$\mathbb{Q}(\sqrt2)@f$) collapses to the native
 * @f$u \le 10@f$ --- a first-class compile-time @c Halfspace<unsigned,10> ---
 * because @c Φ is monotone.
 *
 * The @b modular half: @c unsigned @c = @c ℤ/2^wℤ is the periodic base
 * (@c IsCongruenceQuotient).  The wrap is the honest quotient, and it shows why
 * the chain is monic+monotone but @b not a ring homomorphism: arithmetic in
 * @c unsigned wraps before the embedding, so @f$\Phi(u+v) \ne
 * \Phi(u)+\Phi(v)@f$.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <limits>
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

namespace {

using R2 = QuadraticReal<2>;  // ℝ = ℚ(√2) coat-hanger carrier

// --- The composite inclusion chain, as ONE arrow (the user's graph(f>>g>>h)).
// -- unsigned ↪ ℕ ↪ ℤ ↪ ℚ ↪ ℝ, each rung a monic order-preserving embedding.
constexpr auto Φ = embed_uint_ℕ_ >> lift_ℕ_ℤ_ >> embed_ℤ_ℚ_ >> embed_ℚ_ℝ;

// graph(Φ) IS the composite Trsk relation  unsigned × ℝ | r == Φ(u)
// (§graph.cppm).
constexpr auto Γ_Φ = dedekind::sets::graph(Φ);

constexpr R2 ten = R2{10};

// =====================  S: the chain is monic embeddings =====================
static_assert(IsMonicArrow<std::decay_t<decltype(embed_uint_ℕ_)>>,
              "unsigned ↪ ℕ is monic.");
static_assert(IsMonicArrow<std::decay_t<decltype(lift_ℕ_ℤ_)>>,
              "ℕ ↪ ℤ is monic.");
static_assert(
    dedekind::algebra::EmbedsAsSubalgebra<std::decay_t<decltype(embed_ℤ_ℚ_)>>,
    "ℤ ↪ ℚ is a Birkhoff S-leg (monic ring embedding).");
static_assert(
    dedekind::algebra::EmbedsAsSubalgebra<std::decay_t<decltype(embed_ℚ_ℝ)>>,
    "ℚ ↪ ℝ is a Birkhoff S-leg (monic ring embedding).");

// graph(Φ) is the composite relation: a functional graph on unsigned × ℝ.
static_assert(IsSet<decltype(Γ_Φ)>,
              "graph(Φ) is the ETCS relation unsigned × ℝ | r == Φ(u).");
static_assert(Γ_Φ(std::pair{3u, Φ(3u)}), "(3, Φ(3)) lies on graph(Φ).");
static_assert(!Γ_Φ(std::pair{3u, R2::root()}), "(3, √2) does not.");

// ==================  Strength reduction: {x ≤ 10} pulls back =================
// The composite ℝ-predicate reduces to the native unsigned comparison — and the
// reduced form is a first-class COMPILE-TIME Halfspace<unsigned, 10, ≤>.
using ReducedLeTen =
    Halfspace<unsigned, 10u, Direction::Downward, Strictness::NonStrict>;
constexpr ReducedLeTen native_le_ten{};

// Boundary: 10 ∈ {x ≤ 10}, 11 ∉ — via the ℝ chain AND the native reduction.
static_assert((Φ(10u) <= ten) == (native_le_ten(10u) == ClassicalLogic::True));
static_assert((Φ(10u) <= ten) && (10u <= 10u), "10 survives the pullback.");
static_assert(!(Φ(11u) <= ten) && !(11u <= 10u), "11 does not.");

// ==============  Arithmetic on the intermediate sets (ring rungs) ============
// The upper rungs preserve +, × (they are ring embeddings) — arithmetic done in
// ℤ / ℚ agrees with arithmetic done after embedding into ℝ.
static_assert(embed_ℤ_ℚ_(default_integer{4} + default_integer{6}) ==
                  embed_ℤ_ℚ_(default_integer{4}) +
                      embed_ℤ_ℚ_(default_integer{6}),
              "ℤ ↪ ℚ preserves + on the intermediate ℤ set.");
static_assert(embed_ℚ_ℝ(Rational<>{3, 2} * Rational<>{4, 1}) ==
                  embed_ℚ_ℝ(Rational<>{3, 2}) * embed_ℚ_ℝ(Rational<>{4, 1}),
              "ℚ ↪ ℝ preserves × on the intermediate ℚ set.");

// ================  Modular quotient: unsigned = ℤ/2^w is the base ============
static_assert(IsCongruenceQuotient<Modular<8u>, std::plus<unsigned>>,
              "unsigned's + on ℤ/8ℤ is a congruence quotient of ℤ (8 | 2^w).");
static_assert(!IsCongruenceQuotient<Modular<12u>, std::plus<unsigned>>,
              "Modular<12> is NOT a machine congruence quotient (12 ∤ 2^w).");

// The wrap is why the chain is monic+monotone but NOT a ring homomorphism:
// arithmetic in unsigned wraps (the ℤ/2^w quotient) BEFORE the embedding.
constexpr unsigned umax = std::numeric_limits<unsigned>::max();
static_assert(
    Φ(umax + 5u) != Φ(umax) + Φ(5u),
    "Φ is NOT a +-homomorphism: (UINT_MAX+5) wraps to 4 in unsigned "
    "before embedding, so Φ(u+v) ≠ Φ(u)+Φ(v) — the ℤ/2^w quotient at "
    "the base breaks naturality, though every rung is monic+monotone.");
// Yet the strength reduction is ROBUST to the wrap: it holds for every unsigned
// VALUE, because the quotient happens in unsigned and the embedding is
// faithful.
static_assert((Φ(umax + 5u) <= ten) == (umax + 5u <= 10u),
              "wrapped value 4 obeys the pullback: both sides TRUE.");
static_assert((Φ(umax) <= ten) == (umax <= 10u),
              "un-wrapped huge value obeys the pullback: both sides FALSE.");

// =========  RELATIONAL composition: Γ_f ; Γ_g, arithmetic BETWEEN rungs ======
// Φ above composes the ARROWS then graphs.  Here we compose the GRAPHS directly
// via the relative product Γ_f ; Γ_g = Γ_{f;g} — the ∃b over the ℚ/ℝ
// intermediate discharged by functionality (§graph.cppm).  This is the payoff
// the user asked for: an arithmetic arrow (×2 on ℚ) sits BETWEEN two inclusion
// relations, all composed relationally.
constexpr auto scale2 = dedekind::category::arrow<Rational<>, Rational<>>(
    [](const Rational<>& q) { return q * Rational<>{2}; });

// Γ(ℤ↪ℚ) ; Γ(×2 on ℚ) ; Γ(ℚ↪ℝ)  :  a functional relation ℤ ↬ ℝ, r == 2·z.
constexpr auto Γ_scaled = graph(embed_ℤ_ℚ_) >> graph(scale2) >>
                          graph(embed_ℚ_ℝ);
static_assert(IsSet<decltype(Γ_scaled)>,
              "the relational composite is again a functional graph ℤ ↬ ℝ.");
static_assert(
    Γ_scaled(std::pair{default_integer{5}, R2{10}}),
    "(5, 10) ∈ Γ: 2·5 = 10 in ℝ — arithmetic embedded between rungs.");
static_assert(!Γ_scaled(std::pair{default_integer{5}, R2{11}}), "(5, 11) ∉ Γ.");
// Relational composition and arrow composition coincide (Γ_f;Γ_g == Γ_{f;g}).
static_assert(Γ_scaled(std::pair{default_integer{3}, R2{6}}) ==
                  graph(embed_ℤ_ℚ_ >> scale2 >>
                        embed_ℚ_ℝ)(std::pair{default_integer{3}, R2{6}}),
              "relational ; and arrow ∘ agree pointwise.");

}  // namespace

TEST_CASE("Strength reduction: {x ≤ 10} on ℝ ⟵ native modular unsigned ≤ 10",
          "[numbers][tower][strength-reduction][S][modular]") {
  // The pre-image of {x ≤ 10} through the composite chain is exactly {0..10},
  // recovered as a native unsigned comparison — the strength reduction, swept.
  for (unsigned u = 0; u <= 20u; ++u) {
    CAPTURE(u);
    CHECK((Φ(u) <= ten) == (u <= 10u));
  }
}

TEST_CASE("Strength reduction is robust to the ℤ/2^w wrap (modular pre-image)",
          "[numbers][tower][strength-reduction][modular]") {
  // Values produced by wrapping arithmetic still obey the pullback, because the
  // wrap (the quotient) precedes the monotone embedding.
  for (unsigned k = 1; k <= 12u; ++k) {
    const unsigned wrapped = umax + k;  // = k - 1 (mod 2^w)
    CAPTURE(k, wrapped);
    CHECK((Φ(wrapped) <= ten) == (wrapped <= 10u));
  }
}

TEST_CASE(
    "Relational composition of the tower graphs, arithmetic between rungs",
    "[numbers][tower][strength-reduction][relational]") {
  // graph(ℤ↪ℚ) ; graph(×2 on ℚ) ; graph(ℚ↪ℝ) at runtime: the functional
  // relative product over the ℚ/ℝ intermediates, r == 2·z.
  const auto Γ = graph(embed_ℤ_ℚ_) >> graph(scale2) >> graph(embed_ℚ_ℝ);
  for (int z = 0; z <= 6; ++z) {
    CAPTURE(z);
    CHECK(Γ(std::pair{default_integer{z}, R2{2 * z}}));
    CHECK_FALSE(Γ(std::pair{default_integer{z}, R2{2 * z + 1}}));
  }
}
