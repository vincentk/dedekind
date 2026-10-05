/**
 * @file dedekind/numbers/integer.cppm
 * @partition :integer
 * @brief Minimal number taxonomy concepts for reintegration.
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
#include <limits>  // the window's ends: the saturation witnesses
#include <numeric>
#include <optional>  // the cover / Lambek answers, 1 + N
#include <type_traits>

export module dedekind.numbers:integer;

import dedekind.algebra;
import dedekind.category;
import dedekind.order; // IsTotallyOrdered gates the step arrows' monotonicity
import dedekind.relational; // graph / dagger: the step in the allegory
import dedekind.sets;
import :natural;
export import :cardinality;

namespace dedekind::numbers {
using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;

/** @brief Default signed-integer carrier used by downstream numeric
 *  layers (Rational<I>, embeddings).  Post-#670-sibling (ℚ retarget):
 *  anchored on @c sets::SignedCardinality (saturating ℤ proxy with
 *  ±ℵ_0 / NaZ escalation), mirroring how @c ℤ = @c 𝔸<SignedCardinality>
 *  in @c :integer.  This is the discipline-consistent canonical ℤ
 *  carrier --- @c ℚ = @c 𝔸<Rational<default_integer>> now uses the
 *  saturating variant uniformly.
 *
 *  Pre-retarget value: @c SignedExtensionalCardinal<> (cyclic finite
 *  fragment).  The retarget is what the user asked for: "re-target
 *  rationals / ℚ in the same way as ℤ, ℕ, 𝔹".
 */
export using default_integer = dedekind::sets::SignedCardinality;

/** @section integer__Saturating_ℤ (#670)
 *
 * @c ℤ is the universe @c 𝔸<SignedCardinality>, using the @b saturating
 * variant @c sets::SignedCardinality as the carrier (rather than the
 * @b cyclic finite fragment @c SignedExtensionalCardinal<>).  This
 * mirrors the @c ℕ pattern (@c ℕ @c = @c 𝔸<Cardinality>, where
 * @c Cardinality is the saturating @f$\mathbb{N} \cup \{\aleph_0\}@f$
 * variant) --- the project's stance is now consistent across @c ℕ and
 * @c ℤ: bounded representations @b escalate (saturate to
 * @f$\pm \aleph_0@f$) rather than wrap.
 *
 * The carrier @c sets::SignedCardinality is the project's documented
 * bona-fide proxy for @f$\mathbb{Z}@f$ modulo physical limits
 * (cf.\ @c cardinality.cppm:923-924, "the library's bona-fide proxy
 * for ℤ modulo physical limits") --- arithmetic saturates to
 * @f$\pm \aleph_0@f$ on overflow rather than wrapping modulo
 * @f$2^{N \cdot 64}@f$.  This is what the in-line scout-algebra
 * surface (#664) requires for translation-invariant halfspace-pivot
 * transport: the @c IsOrderedAdditiveGroup marker in
 * @c :algebra:ordered_algebra is specialised to @c true for
 * @c SignedCardinality and to @c false (default) for the cyclic
 * finite fragment, exactly because the saturating discipline
 * preserves order under translation and the cyclic one does not at
 * the wrap boundary.
 */
export inline constexpr auto ℤ =
    dedekind::sets::𝔸<dedekind::sets::SignedCardinality>{};

static_assert(std::same_as<std::remove_cvref_t<decltype(ℤ)>,
                           dedekind::sets::𝔸<dedekind::sets::SignedCardinality,
                                             Boole, ℵ_0>>,
              "ℤ is the universe 𝔸<SignedCardinality>, mirroring "
              "ℕ = 𝔸<Cardinality> (#670).");
static_assert(std::same_as<typename std::remove_cvref_t<decltype(ℤ)>::Domain,
                           dedekind::sets::SignedCardinality>,
              "ℤ's underlying carrier IS SignedCardinality — the project's "
              "bona-fide saturating ℤ proxy (per cardinality.cppm:923-924). "
              "Mirrors ℕ's underlying carrier being Cardinality.");

// The saturating ℤ proxy inhabits the algebraic concept chain the
// in-line scout-algebra surface uses (#664).  Pinning the witness:
// IsOrderedAdditiveGroup<SignedCardinality> holds via the
// is_translation_invariant_ordered marker specialised in
// :algebra:ordered_algebra.
static_assert(dedekind::algebra::IsOrderedAdditiveGroup<
                  dedekind::sets::SignedCardinality>,
              "ℤ's carrier SignedCardinality must satisfy "
              "IsOrderedAdditiveGroup --- the structural binding "
              "between ℤ and the in-line scout-algebra halfspace "
              "pipe (#664 / #670).");

// ℤ inhabits Ddk: it is an algebraic set.  The universe ℤ is an IsSet over
// the SignedCardinality carrier, which is closed under its ring operations
// +, *, and --- unlike ℕ --- unary - (SignedCardinality is an abelian group
// under +, so negation closes on ℤ).  So IsAlgebraOnSet
// fires: ℤ as an object of Ddk = Trsk ∩ Alg (Figure 1), a ring.
static_assert(
    dedekind::algebra::IsAlgebraOnSet<
        decltype(ℤ), std::plus<dedekind::sets::SignedCardinality>,
        std::multiplies<dedekind::sets::SignedCardinality>,
        std::negate<dedekind::sets::SignedCardinality>>,
    "ℤ is an algebraic set (Ddk): a set whose carrier SignedCardinality is "
    "closed under the ring operations +, *, and the unary - negation loop.");

// ===========================================================================
// Initial Ring + Grothendieck Group witnesses on @c SignedCardinality
// (closes part of #446).
//
// Two universal-property witnesses anchoring @c SignedCardinality
// simultaneously:
//   * @c IsInitialRing<SignedCardinality> — for every ring @c R there
//     exists a unique ring homomorphism @c SignedCardinality @c → @c R
//     (e.g.\ @c χ_{Modular<n>} as the mod-n reduction).
//   * @c IsGrothendieckGroup<SignedCardinality, Cardinality> —
//     @c SignedCardinality is the free abelian group on the
//     commutative monoid @c Cardinality; the closure-forcing operator
//     @c Cardinality @c - @c Cardinality @c → @c SignedCardinality
//     realises the construction at the operator level.
//
// Universal-property content (existence + uniqueness of the canonical
// homomorphisms) is the engineer's honesty obligation; the test
// suite exercises the operational behaviour at concrete targets.
// ===========================================================================

static_assert(
    dedekind::algebra::IsInitialRing<dedekind::sets::SignedCardinality>,
    "SignedCardinality is the canonical Initial Ring witness: for every "
    "ring R there exists a unique ring homomorphism SignedCardinality → R "
    "(e.g. χ_{Modular<n>} = mod-n reduction).  Universal-property content "
    "is the engineer's honesty obligation.");

static_assert(
    dedekind::algebra::IsGrothendieckGroup<dedekind::sets::SignedCardinality,
                                           dedekind::sets::Cardinality>,
    "SignedCardinality is the canonical Grothendieck group of Cardinality: "
    "the free abelian group on the commutative monoid (Cardinality, +, 0).  "
    "The closure-forcing operator Cardinality - Cardinality → "
    "SignedCardinality realises the Grothendieck construction at the "
    "operator level.");

// Honest Rejection: ℤ is the initial ring AND the Grothendieck group
// of ℕ (asserted above), but NOT a multiplicative group --- non-units
// (everything except ±1) lack multiplicative inverses.  The
// @c IsOrderedMultiplicativeGroup gate in @c :algebra:ordered_algebra
// therefore rejects ℤ; use ℚ (Rational<default_integer>, the field of
// fractions of ℤ; pinned at @c rational.cppm) wherever multiplicative
// order-compatibility is required.  Cross-partition invariant pinned in
// main per the static_assert-in-main pattern.
static_assert(
    !dedekind::algebra::IsOrderedMultiplicativeGroup<
        dedekind::sets::SignedCardinality>,
    "ℤ (SignedCardinality) is NOT a multiplicative group --- non-units "
    "lack multiplicative inverses.  ℚ (the field of fractions of ℤ) is "
    "the right carrier for multiplicative halfspace scaling.");

}  // namespace dedekind::numbers

namespace dedekind::category {
using dedekind::sets::SignedCardinality;
/** @brief @c S ⊣ P on the saturating ℤ.  Both legs are monotone
 *  (@c order:completeness); the unit @f$x \le P(S(x))@f$ and the counit
 *  @f$S(P(y)) \le y@f$ hold everywhere, at the ends by saturation
 *  (@f$P(S(\max)) = P(+\aleph_0) = +\aleph_0 \ge \max@f$).  The reverse
 *  @c P ⊣ S fails at the top, @f$P(S(\max)) = +\aleph_0 > \max@f$, and @c S is
 *  no bijection, @f$S(\max) = S(+\aleph_0)@f$: saturation keeps one of the two
 *  adjunctions ℤ proper has, and the witnesses in the numbers block say which.
 */
template <>
inline constexpr bool
    is_adjoint_v<Successor<SignedCardinality>, Predecessor<SignedCardinality>,
                 std::less_equal<>> = true;
}  // namespace dedekind::category

namespace dedekind::numbers {
// ℤ = @c SignedCardinality satisfies the STRICT @c category::IsRing.  This
// holds via the carrier's @b saturating totality (@c is_saturating<SC,+/*> in
// @c :cardinality --- overflow escalates to @f$\pm\aleph_0@f$, one reading of
// Eqn 2), which already passes the @c IsTotal gate; the axiom traits
// (associativity / commutativity / identity / distributivity + additive
// inverse) are the ordered-additive-group / Initial-Ring pins above.  These
// static_asserts merely @b pin what already held --- closing the gap that the
// strict concept was documented but never mechanically witnessed here.  (ℤ is
// @b not exact/unbounded; the honest totality posture is saturation.)
static_assert(
    dedekind::category::IsRing<
        dedekind::sets::SignedCardinality,
        std::plus<dedekind::sets::SignedCardinality>,
        std::multiplies<dedekind::sets::SignedCardinality>>,
    "ℤ = SignedCardinality is a strict category::IsRing (via saturating "
    "totality) --- ℕ/ℤ/ℚ all strict-total, now witnessed.");
// A ring is a fortiori a semiring; and ℤ is NOT a field (non-units lack
// multiplicative inverses) --- the textbook ℤ, pinned strictly.
static_assert(dedekind::category::IsSemiring<
                  dedekind::sets::SignedCardinality,
                  std::plus<dedekind::sets::SignedCardinality>,
                  std::multiplies<dedekind::sets::SignedCardinality>>,
              "ℤ is a fortiori a strict category::IsSemiring.");
static_assert(
    !dedekind::category::IsField<
        dedekind::sets::SignedCardinality,
        std::plus<dedekind::sets::SignedCardinality>,
        std::multiplies<dedekind::sets::SignedCardinality>>,
    "ℤ is NOT a field --- only ±1 are multiplicative units (ℚ is its field "
    "of fractions).");

// S ⊣ P on ℤ's proxy, registered above as the adjunction it is, and only that
// way round: saturation at ±ℵ₀ keeps the unit and counit of S ⊣ P and breaks
// the counit of P ⊣ S at the top.  Away from the ends P inverts S on values;
// at the top S(max) = S(+ℵ₀), so S is no bijection and nothing declares it one.
// [Z, S] is not an iso either (S(−1) = Z): ℤ has the NNO shape without being
// the NNO.  It is a group.
namespace detail_step_adjunction {
using dedekind::relational::dagger;
using dedekind::relational::graph;
using Z = SignedCardinality;
using S = Successor<Z>;
using P = Predecessor<Z>;
inline constexpr Z largest_finite = finite_signed_cardinality(
    std::numeric_limits<SignedExtensionalCardinal<>::signed_type>::max());
inline constexpr Z smallest_finite = finite_signed_cardinality(
    std::numeric_limits<SignedExtensionalCardinal<>::signed_type>::min());
consteval bool inverts_on_sample() {
  for (int a = -3; a <= 3; ++a) {
    const Z x = finite_signed_cardinality(a);
    if (P{}(S{}(x)) != x || S{}(P{}(x)) != x) return false;
  }
  return true;
}
// The unit x ≤ P(S(x)) and the counit S(P(y)) ≤ y at the ends, where only
// saturation holds them.
consteval bool unit_and_counit_at_the_ends() {
  return largest_finite <= P{}(S{}(largest_finite)) &&
         S{}(P{}(smallest_finite)) <= smallest_finite;
}
// P ⊣ S would need P(S(x)) ≤ x; at the top P(S(max)) = +ℵ₀ > max.
consteval bool reverse_counit_fails_at_the_top() {
  return !(P{}(S{}(largest_finite)) <= largest_finite);
}
consteval bool saturation_breaks_the_bijection() {
  return S{}(largest_finite) == S{}(Z{PositiveInfinity{}});
}
// In the allegory every map is adjoint to its converse; on ℤ the converse of
// the successor's graph IS the predecessor's graph, pointwise on a sample.
consteval bool converse_is_predecessor_on_sample() {
  for (int a = -3; a <= 3; ++a)
    for (int b = -3; b <= 3; ++b) {
      const std::pair p{finite_signed_cardinality(a),
                        finite_signed_cardinality(b)};
      if (dagger(graph(S{}))(p) != graph(P{})(p)) return false;
    }
  return true;
}
consteval bool lambek_fails_at_minus_one() {
  const Z minus_one = finite_signed_cardinality(-1);
  const auto back = Out<Z>{}(In<Z>{}(std::optional<Z>{minus_one}));
  if (!back.has_value()) return true;
  return *back != minus_one;
}
}  // namespace detail_step_adjunction
static_assert(
    IsGaloisConnection<detail_step_adjunction::S, detail_step_adjunction::P> &&
        !IsGaloisConnection<detail_step_adjunction::P,
                            detail_step_adjunction::S> &&
        !IsIsomorphism<detail_step_adjunction::S>,
    "S ⊣ P on the saturating ℤ, and not P ⊣ S: the step is no "
    "bijection at the top.");
static_assert(detail_step_adjunction::inverts_on_sample() &&
                  detail_step_adjunction::unit_and_counit_at_the_ends() &&
                  detail_step_adjunction::reverse_counit_fails_at_the_top() &&
                  detail_step_adjunction::saturation_breaks_the_bijection(),
              "P inverts S on [−3, 3]; at the ends saturation holds S ⊣ P and "
              "breaks P ⊣ S: P(S(max)) = +ℵ₀.");
static_assert(detail_step_adjunction::converse_is_predecessor_on_sample(),
              "Γ_S° = Γ_P on ℤ: the order adjunction and the allegory's f ⊣ f° "
              "coincide.");
static_assert(!IsIsomorphism<In<detail_step_adjunction::Z>> &&
                  detail_step_adjunction::lambek_fails_at_minus_one(),
              "[Z, S] is not an iso on ℤ: S(−1) = Z.  A group, not the NNO.");
}  // namespace dedekind::numbers
