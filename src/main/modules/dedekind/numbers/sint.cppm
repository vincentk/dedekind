/**
 * @file dedekind/numbers/sint.cppm
 * @partition :sint
 * @module dedekind.numbers:sint
 * @brief Level 4: The std::signed_integral family — operator surface ✓,
 *        axiomatic ring ✗ (Honest Rejection: dual error-direction to
 *        :uint).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Honest_Stance
 * The signed-integer family (@c int, @c long, @c long @c long, the
 * narrow widths modulo integer promotion) presents the @b dual
 * category-theoretic error to its unsigned sibling.  Where @c unsigned
 * @c int claims @b more structure than @c ℕ has (modular additive
 * inverses), @c int claims @b less than @c ℤ has: the literal C++ ring
 * operators (@c +, binary @c -, unary @c -, @c *) all close on the
 * carrier and the order layer is fine, but @b signed-overflow @b is
 * @b undefined @b behaviour --- which means closure-under-arithmetic
 * fails as a stable axiom.  The library's posture is honest in both
 * directions: @c HasRingOperators<int> and @c HasGroupOperatorsAdd<int>
 * fire (the operator surface really is there), but @c IsRing<int,
 * std::plus, std::multiplies> and @c Group_ℤ<int> do @b not (the
 * axiomatic-ring witness would be a false claim under UB).  The
 * exact-@c ℤ reading lives one carrier step deeper, on @c
 * SignedCardinality (with @c ±ℵ_0 saturation and @c NaZ).
 *
 * @section Symmetric_Honest_Rejection
 * @c unsigned @c int and @c int are rejected as witnesses for @c ℕ and
 * @c ℤ respectively for @b opposite reasons --- but the rejection is
 * @b symmetric @b in @b form: each carrier's claim is mismatched with
 * the textbook structure it would purport to inhabit, and the type
 * system refuses the mismatch in both directions.  See the symmetric
 * @c numbers:uint partition (closes #417) for the @c std::unsigned_integral
 * counterpart.
 *
 * @section Honesty_Obligation
 * The witness blocks below are the project's @b honesty obligation made
 * structural: each @c static_assert is an engineer's claim that the
 * named carrier exhibits (or does @b not exhibit) the named property.
 * Curry--Howard reading per Wadler 2015; professional grounding per the
 * NSPE @c Code @c of @c Ethics @c for @c Engineers (canons III + IV).
 * See the @c §"poor man's theorem prover" footnote in @c report.tex /
 * @c paper.tex for the full literature anchor.
 *
 * @section Ergonomics_Doctrine
 * Honest Rejection is structurally correct, but a separate question
 * is whether it harms practitioner ergonomics in practice.  The
 * project's answer is doctrinal, not technical: the recommended
 * style is @b "Intensional @b first; @b realize @b when @b you @b
 * mean @b it".  Practitioners write code over the @b canonical
 * mathematical set ( @c var<ℤ>, predicates, comprehension DSL,
 * abstract algebraic operations on @c SignedCardinality directly)
 * and only descend to machine integers via an @b explicit
 * realize-step ( @c realize_to_size_t / future @c
 * realize_signed_to_machine, see #429).  The realize-step carries a
 * proof obligation — the caller chooses a sentinel for the
 * transfinite case, and the type system mechanically forces the
 * caller to handle that case.  This posture does NOT add
 * heterogeneous arithmetic overloads (e.g.\ @c SignedCardinality
 * @c + @c int → @c SignedCardinality) that would smuggle machine-
 * int-style code into the variant carrier; doing so would
 * structurally undo the Honest Rejection by routing machine-int
 * UB-on-overflow back through the variant operator surface.  See
 * the paper's @c §Intensional @c First, @c Realize @c When @c You
 * @c Mean @c It section for the full argument.
 *
 * @note "Die ganzen Zahlen hat der liebe Gott gemacht, alles andere ist
 *        Menschenwerk."
 *       ("God made the integers; all else is the work of man.")
 *       — Leopold Kronecker, Jahresbericht der DMV 2 (1891, reported).
 */
module;

#include <concepts>
#include <functional>
#include <limits>

export module dedekind.numbers:sint;

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sequences;
import dedekind.sets;
import :integer;  // for Group_ℤ + extensional_integer alias

namespace dedekind::numbers {

// ===========================================================================
// (1) Universal machine→variant lift: std::signed_integral → SignedCardinality
// ===========================================================================

/**
 * @brief Universal machine-to-variant lift: @c std::signed_integral
 *        @c → @c SignedCardinality.  Closes part of #418.
 *
 * @details The structure-honest lift from a machine signed integer
 * (which only @b carries the ring operator surface — UB on overflow
 * defeats the axiomatic ring laws) into the variant @c ℤ-proxy
 * @c SignedCardinality (with @c ±ℵ_0 saturation and @c NaZ).  Where
 * the @c std::signed_integral source has incomplete axiomatic
 * structure, the @c SignedCardinality target carries full @c IsRing
 * via the saturating semantics — moving the carrier @b up the
 * carrier lattice rather than @b across it.
 *
 * Companion to @c embed_unsigned_to_Cardinality (in @c :uint, post-
 * #417) on the symmetric unsigned side.  Together the two arrows
 * complete the embedding chains
 *
 *   @c 𝔹 @c → @c std::unsigned_integral @c → @c ℕ
 *   @c 𝕂3 @c → @c std::signed_integral @c → @c ℤ
 *
 * documented in the carrier-lattice section of @c report.tex /
 * @c paper.tex.
 *
 * @tparam S Any @c std::signed_integral source type.
 */
export template <std::signed_integral S>
constexpr dedekind::sets::SignedCardinality embed_signed_to_SignedCardinality(
    S v) {
  // The lift funnels into @c finite_signed_cardinality, which stores
  // into @c SignedExtensionalCardinal<>'s magnitude (a @c
  // ExtensionalCardinal<> with @c std::size_t limb).  On 32-bit
  // platforms, @c long @c long is wider than @c std::size_t — so the
  // limb-width is the actual binding constraint, not @c long @c long.
  // Pin the guard against the @b finite-fragment limb width so the
  // injectivity claim holds on every platform.
  static_assert(std::numeric_limits<S>::digits <=
                    std::numeric_limits<std::size_t>::digits,
                "embed_signed_to_SignedCardinality requires every value of S "
                "to be representable in the SignedExtensionalCardinal<>'s limb "
                "type (std::size_t for the magnitude).  On platforms where "
                "this fires (e.g. 32-bit std::size_t with 64-bit long long, or "
                "wider extended integer types), an explicit ±ℵ_0 escalation "
                "on out-of-range values would be needed; without it the "
                "conversion would silently truncate and break injectivity.");
  return dedekind::sets::finite_signed_cardinality(v);
}

/**
 * @brief Concrete monic arrow: int ↪ SignedCardinality (variant ℤ-proxy).
 *
 * @details The named-arrow form of @c
 * embed_signed_to_SignedCardinality<int>, registered as monic in
 * @c is_monic_arrow_v so downstream @c IsMonicArrow callsites can
 * find it.  Distinct @c int values yield distinct @c SignedCardinality
 * values (injective on the finite fragment).
 */
export inline constexpr auto embed_int_SignedCardinality_ =
    dedekind::category::arrow<int, dedekind::sets::SignedCardinality>(
        [](const int& i) noexcept -> dedekind::sets::SignedCardinality {
          return embed_signed_to_SignedCardinality(i);
        });

// ===========================================================================
// (2) Family classification — std::signed_integral as operator-surface ✓ /
//     axiomatic-ring ✗
// ===========================================================================

// (2a) Positive operator-shape witnesses — the literal C++ surface
// (+, binary -, unary -, *, pre/post ++, --) really does close on the
// canonical signed-integral representatives.  These witnesses do
// @b not claim axiomatic ring laws hold (those are rejected
// separately below).
static_assert(dedekind::algebra::HasRingOperators<int>,
              "int closes the literal ring operator surface "
              "(+, binary -, unary -, *).");
static_assert(dedekind::algebra::HasRingOperators<long>,
              "long closes the literal ring operator surface.");
static_assert(dedekind::algebra::HasRingOperators<long long>,
              "long long closes the literal ring operator surface.");
static_assert(dedekind::algebra::HasGroupOperatorsAdd<int>,
              "int closes the additive-group operator surface "
              "(+, unary -, T{}).");
static_assert(dedekind::algebra::HasGroupOperatorsAdd<long>,
              "long closes the additive-group operator surface.");
static_assert(dedekind::algebra::HasGroupOperatorsAdd<long long>,
              "long long closes the additive-group operator surface.");
static_assert(dedekind::algebra::HasSuccessorOperators<int>,
              "int closes the successor / predecessor operator surface "
              "(pre/post ++, --).");
static_assert(dedekind::algebra::HasSuccessorOperators<long>,
              "long closes the successor / predecessor operator surface.");
static_assert(dedekind::algebra::HasSuccessorOperators<long long>,
              "long long closes the successor / predecessor operator surface.");

// (2b) Negative axiomatic witnesses — the load-bearing Honest
// Rejection.  Signed-overflow UB defeats closure-under-arithmetic
// as a stable axiom; the strict abelian-group / ring witnesses
// must @b not fire on @c std::signed_integral primitives.  The
// exact-@c ℤ reading lives on @c SignedCardinality post-#402.
static_assert(!Group_ℤ<int>,
              "int must NOT satisfy Group_ℤ: signed-overflow UB defeats "
              "the strict abelian-group proof under the math-wins-over-C++ "
              "stance.  SignedCardinality is the exact-ℤ witness "
              "(carrier home: dedekind::sets:cardinality).");
static_assert(!Group_ℤ<long>,
              "long must NOT satisfy Group_ℤ for the same reason.");
static_assert(!Group_ℤ<long long>,
              "long long must NOT satisfy Group_ℤ for the same reason.");
static_assert(!dedekind::algebra::IsArithmeticAdditiveGroup<int>,
              "int must NOT satisfy IsArithmeticAdditiveGroup: same "
              "UB-on-overflow reason as the Group_ℤ rejection.");
static_assert(!dedekind::algebra::IsArithmeticAdditiveGroup<long>,
              "long must NOT satisfy IsArithmeticAdditiveGroup.");
static_assert(!dedekind::algebra::IsArithmeticAdditiveGroup<long long>,
              "long long must NOT satisfy IsArithmeticAdditiveGroup.");

// Order surface — every @c std::signed_integral is totally ordered at
// the machine layer; the order layer is honest while the arithmetic
// layer is broken by UB.
static_assert(dedekind::order::HasTotalOrderOperators<int>,
              "int carries the total-order operator surface.");
static_assert(dedekind::order::HasTotalOrderOperators<long>,
              "long carries the total-order operator surface.");
static_assert(dedekind::order::HasTotalOrderOperators<long long>,
              "long long carries the total-order operator surface.");
static_assert(dedekind::order::IsTotallyOrdered<int>,
              "int is axiomatically totally ordered.");
static_assert(dedekind::order::IsDirectedSet<int>,
              "int is a directed set (net domain).");

// Sequence witness: machine signed int is a valid sequence codomain.
static_assert(
    dedekind::sequences::IsFiniteSequence<dedekind::sequences::FinitePath<int>>,
    "FinitePath<int> is a bona-fide finite sequence.");

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool is_monic_arrow_v<
    std::decay_t<decltype(dedekind::numbers::embed_int_SignedCardinality_)>> =
    true;
}  // namespace dedekind::category
