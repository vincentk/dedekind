/**
 * @file real_bridge.cppm
 * @brief The Birkhoff @b S leg @f$\mathbb{Q}\hookrightarrow\mathbb{R}@f$: a
 *        genuine @b monic @b homomorphism into the @f$\mathbb{Q}(\sqrt2)@f$
 *        coat-hanger, with @b computed witnesses, plus its reified subobject
 *        inclusion @f$\iota=\operatorname{graph}(\text{embed})@f$.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section real_bridge__The_S_Leg
 * @f$\mathbb{R}=\Omega\langle\mathbb{Q}(\sqrt2)\rangle@f$ is the algebraic
 * coat-hanger (a first-class @c IsField value; see @c :quadratic).  This
 * partition hangs the first HSP leg off it: @f$\mathbb{Q}\hookrightarrow
 * \mathbb{R}@f$, @f$q\mapsto q+0\sqrt2@f$, as an @c EmbedsAsSubalgebra arrow.
 *
 * The contrast with the retired @c PlatonicReal is the whole point.  There the
 * subalgebra legs were @b postulated on an uninhabited stub; here the arrow
 * lands in a genuine, inhabited field, so the homomorphism and injectivity laws
 * are @b run at compile time (@c embed(a+b)==embed(a)+embed(b), @c embed
 * injective) rather than declared on faith.  The opt-in
 * @c is_homomorphism_v / @c is_monic_arrow_v registrations below are backed,
 * one-for-one, by the static_asserts in @c real_bridge__Formal_Verification.
 */

module;

#include <concepts>
#include <type_traits>
#include <utility>

export module dedekind.numbers:real_bridge;

import dedekind.algebra; // EmbedsAsSubalgebra, IsHomomorphism, is_homomorphism_v
import dedekind.category;   // IsArrow, IsMonicArrow, is_monic_arrow_v, IsSet
import dedekind.relational; // graph(f): the reified inclusion ι
import :rational;           // Rational<default_integer> — the ℚ carrier
import :quadratic;  // QuadraticReal<2> — the ℝ = ℚ(√2) coat-hanger carrier

namespace dedekind::numbers {

/**
 * @class EmbedRationalToReal
 * @brief The field embedding @f$\mathbb{Q}\hookrightarrow\mathbb{R}
 *        =\mathbb{Q}(\sqrt2)@f$, @f$q\mapsto q+0\sqrt2@f$.
 *
 * @details A @b genuine arrow (not a stub): @c Domain and @c Codomain are both
 * inhabited, @c std::regular field carriers, so the arrow is the @b pointwise
 * map whose homomorphism law @c embed(a+b)==embed(a)+embed(b) is an
 * element-level identity the compiler can evaluate.  Its image is exactly the
 * @c b=0 subfield
 * @f$\{a+0\sqrt2\}\subset\mathbb{Q}(\sqrt2)@f$ --- an isomorphic copy of
 * @f$\mathbb{Q}@f$, i.e.\ a genuine subalgebra.
 *
 * @tparam I the integer carrier of the rational field (defaults to
 *         @c default_integer, so @c Codomain is the canonical @c
 * QuadraticReal<2>).
 */
export template <typename I = default_integer>
struct EmbedRationalToReal {
  using Domain = Rational<I>;
  using Codomain = QuadraticReal<2, Rational<I>>;

  /** @brief @f$q\mapsto q+0\sqrt2@f$ (the @c QuadraticReal(Q) lift). */
  constexpr Codomain operator()(const Domain& q) const { return Codomain{q}; }
};

/** @brief The canonical S-leg instance
 * @f$\mathbb{Q}\hookrightarrow\mathbb{R}@f$. */
export inline constexpr EmbedRationalToReal<> embed_ℚ_ℝ{};

}  // namespace dedekind::numbers

// ---------------------------------------------------------------------------
// Opt-in HSP-leg registrations.  Each is BACKED by a computed witness in the
// real_bridge__Formal_Verification section below (the audit trail): the
// declaration is honest exactly because the law it asserts is run there.
// ---------------------------------------------------------------------------
namespace dedekind::algebra {
/** @brief S-leg is structure-preserving (witness: preserves +,×,0,1). */
template <typename I>
inline constexpr bool
    is_homomorphism_v<dedekind::numbers::EmbedRationalToReal<I>> = true;
}  // namespace dedekind::algebra

namespace dedekind::category {
/** @brief S-leg is monic (witness: @f$a\neq b\Rightarrow embed(a)\neq
 * embed(b)@f$). */
template <typename I>
inline constexpr bool
    is_monic_arrow_v<dedekind::numbers::EmbedRationalToReal<I>> = true;
}  // namespace dedekind::category

namespace dedekind::numbers {

/** @section real_bridge__Formal_Verification
 *  The S-leg's defining laws, @b computed over inhabited carriers.
 */
namespace {
using Emb = EmbedRationalToReal<>;
using Q = Rational<>;
using R2 = QuadraticReal<2>;

constexpr Q a{2, 3};  // 2/3
constexpr Q b{3, 5};  // 3/5

// --- Homomorphism: preserves the F of the (A, F) field (+, ×, 0, 1). ---
static_assert(embed_ℚ_ℝ(a + b) == embed_ℚ_ℝ(a) + embed_ℚ_ℝ(b),
              "S-leg preserves + (embed(a+b) = embed(a)+embed(b)).");
static_assert(embed_ℚ_ℝ(a* b) == embed_ℚ_ℝ(a) * embed_ℚ_ℝ(b),
              "S-leg preserves × (embed(a·b) = embed(a)·embed(b)).");
static_assert(embed_ℚ_ℝ(Q{}) == R2{}, "S-leg preserves 0.");
static_assert(embed_ℚ_ℝ(Q{1}) == R2{1}, "S-leg preserves 1.");

// --- Monic: injective, so the image is a GENUINE subalgebra. ---
static_assert(a != b && embed_ℚ_ℝ(a) != embed_ℚ_ℝ(b),
              "S-leg is injective (a ≠ b ⇒ embed(a) ≠ embed(b)).");

// --- The image is the ℚ-subfield {b = 0} ⊂ ℚ(√2) (an iso copy of ℚ). ---
static_assert(embed_ℚ_ℝ(a).radical_part() == Q{},
              "image lands in the ℚ-subfield {b = 0} of ℚ(√2).");

// --- The concept-level payoff: ℚ ↪ ℝ classifies as a Birkhoff S-leg. ---
static_assert(dedekind::category::IsArrow<Emb>, "S-leg is an arrow.");
static_assert(dedekind::algebra::IsHomomorphism<Emb>,
              "S-leg is a homomorphism.");
static_assert(dedekind::category::IsMonicArrow<Emb>, "S-leg is monic.");
static_assert(dedekind::algebra::EmbedsAsSubalgebra<Emb>,
              "ℚ ↪ ℝ = ℚ(√2) is a Birkhoff S-leg: a monic homomorphism onto a "
              "subalgebra --- the arrow Section 5's Figure 5 hangs on.");

// --- The reification: ι = graph(embed) is the subobject inclusion ℚ ↪ ℝ, ---
// --- a functional (single-valued) relation on ℚ×ℝ.  The ARROW is a function;
// ---
// --- its GRAPH is the IsRelation.  (It is NOT an IsFunctor: a field hom ---
// --- preserves operations, not composition of arrows between categories.) ---
constexpr auto ι_ℚ_ℝ = dedekind::sets::graph(embed_ℚ_ℝ);
static_assert(
    dedekind::category::IsSet<decltype(ι_ℚ_ℝ)>,
    "ι = graph(embed) is an ETCS subobject of ℚ×ℝ (a functional relation).");
static_assert(ι_ℚ_ℝ(std::pair{a, embed_ℚ_ℝ(a)}),
              "(q, q+0√2) lies on the inclusion ι.");
static_assert(!ι_ℚ_ℝ(std::pair{a, R2::root()}),
              "(q, √2) does not lie on ι — √2 is not in the image of ℚ.");
}  // namespace

}  // namespace dedekind::numbers
