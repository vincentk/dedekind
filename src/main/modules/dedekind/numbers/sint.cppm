/**
 * @file dedekind/numbers/sint.cppm
 * @partition :sint
 * @brief std::signed_integral — Honest Rejection (UB-on-overflow).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section sint__Honest_Stance
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
 * @section sint__Symmetric_Honest_Rejection
 * @c unsigned @c int and @c int are rejected as witnesses for @c ℕ and
 * @c ℤ respectively for @b opposite reasons --- but the rejection is
 * @b symmetric @b in @b form: each carrier's claim is mismatched with
 * the textbook structure it would purport to inhabit, and the type
 * system refuses the mismatch in both directions.  See the symmetric
 * @c numbers:uint partition (closes #417) for the @c std::unsigned_integral
 * counterpart.
 *
 * @section sint__Honesty_Obligation
 * The witness blocks below are the project's @b honesty obligation made
 * structural: each @c static_assert is an engineer's claim that the
 * named carrier exhibits (or does @b not exhibit) the named property.
 * Curry--Howard reading per Wadler 2015; professional grounding per the
 * NSPE @c Code @c of @c Ethics @c for @c Engineers (canons III + IV).
 * See the @c §"poor man's theorem prover" footnote in @c report.tex /
 * @c paper.tex for the full literature anchor.
 *
 * @section sint__Ergonomics_Doctrine
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
#include <cstddef>  // std::size_t (used in the digits-width static_assert
                    // in embed_sint_ℤ's per-value entry point)
#include <functional>
#include <limits>
#include <type_traits>  // std::remove_cvref_t (no-narrowing pin in
                        // embed_sint_ℤ's set-level overload)
#include <utility>      // std::forward (used in embed_sint_ℤ's set-level lift)

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
 * Companion to @c embed_uint_ℕ (in @c :uint, post-#417 / renamed
 * under #457) on the symmetric unsigned side.  Together the two arrows
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
constexpr dedekind::sets::SignedCardinality embed_sint_ℤ(S v) {
  // The lift funnels into @c finite_signed_cardinality, which stores
  // into @c SignedExtensionalCardinal<>'s magnitude (a @c
  // ExtensionalCardinal<> with @c std::size_t limb).  On 32-bit
  // platforms, @c long @c long is wider than @c std::size_t — so the
  // limb-width is the actual binding constraint, not @c long @c long.
  // Pin the guard against the @b finite-fragment limb width so the
  // injectivity claim holds on every platform.
  static_assert(std::numeric_limits<S>::digits <=
                    std::numeric_limits<std::size_t>::digits,
                "embed_sint_ℤ requires every value of S "
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
 * embed_sint_ℤ<int>, registered as monic in
 * @c is_monic_arrow_v so downstream @c IsMonicArrow callsites can
 * find it.  Distinct @c int values yield distinct @c SignedCardinality
 * values (injective on the finite fragment).
 */
export inline constexpr auto embed_sint_ℤ_ =
    dedekind::category::arrow<int, dedekind::sets::SignedCardinality>(
        [](const int& i) noexcept -> dedekind::sets::SignedCardinality {
          return embed_sint_ℤ(i);
        });

/**
 * @brief Set-level lift of @c embed_sint_ℤ_: image of an @c int-set
 *        @c S under the canonical mono @c int ↪ ℤ.
 *
 * @details Layer-1 entry per #602, sister to @c embed_𝔹_ℕ (PR #624),
 * @c embed_𝔹_𝕂3 (PR #626), and @c embed_uint_ℕ (PR #628): names the
 * construction at the call site rather than re-spelling
 * @c image(embed_sint_ℤ_, S).  Accepted input @c S is anything
 * @c dedekind::sets::image already dispatches on —
 * @c SingletonSet (@c :sets:singleton),
 * @c std::set<int> / @c std::unordered_set<int>
 * (@c :sets:extensional); lazy predicate sets join the dispatch
 * table when #602's layer 2 lands.
 *
 * @note This overload is on the @b set side; the per-value entry
 * point @c embed_sint_ℤ(S @c v) — the templated function above for
 * any @c std::signed_integral @c S — is unchanged and kept for the
 * value-level call sites in @c :sint and @c :integer.  Overload
 * resolution disambiguates via the @c requires-clause: the per-value
 * overload requires @c std::signed_integral, this one requires
 * the set's element type to be exactly @c int (the no-narrowing pin
 * below) and @c image to be well-formed; no @c S satisfies both at
 * once.
 *
 * Mathematically: the image of @c S under the canonical mono
 * @c int @c ↪ @c ℤ is a subset of the finite fragment of @c
 * SignedCardinality containing whichever @c int values are in @c S.
 */
// No-narrowing pin: the source set's element type must be EXACTLY
// @c int.  Without this gate, sets over a wider signed type
// (e.g. @c long, @c long @c long) or over @c unsigned would
// satisfy the bare @c image-well-formedness requires-clause via
// implicit conversion at the per-element @c embed_sint_ℤ_ call site,
// silently truncating (long@→int) or applying the
// implementation-defined unsigned@→int narrowing conversion, in
// either case bypassing the @c digits-safety @c static_assert in
// the per-value @c embed_sint_ℤ(S).  We pin both the @c Domain-exposing
// carriers (SingletonSet, dedekind::sets::Set, …) and the @c
// value_type-exposing carriers (std::set, std::unordered_set) in a single
// disjunctive constraint — either typedef must match exactly @c int for the
// overload to fire.  Same shape as PR #628's @c embed_uint_ℕ no-narrowing pin.
export template <typename S>
  requires(
              requires {
                typename std::remove_cvref_t<S>::Domain;
                requires std::same_as<typename std::remove_cvref_t<S>::Domain,
                                      int>;
              } ||
              requires {
                typename std::remove_cvref_t<S>::value_type;
                requires std::same_as<
                    typename std::remove_cvref_t<S>::value_type, int>;
              }) &&
          requires(S&& s) {
            dedekind::sets::image(embed_sint_ℤ_, std::forward<S>(s));
          }
constexpr auto embed_sint_ℤ(S&& s) {
  return dedekind::sets::image(embed_sint_ℤ_, std::forward<S>(s));
}

// Set-level lift witnesses: @c embed_sint_ℤ on @c SingletonSet<int>{42}
// lands at @c finite_signed_cardinality(42), and on @c SingletonSet<int>{-7}
// at @c finite_signed_cardinality(-7).  Pinned at the @b value level so
// the pivot equality is constant-evaluated, not just the codomain type.
// Mirrors PR #624 / #626 / #628's witnesses — same shape, different
// (carrier, codomain) pair.
static_assert(
    embed_sint_ℤ(
        dedekind::sets::SingletonSet<int, dedekind::category::ClassicalLogic>{
            42})
            .pivot == dedekind::sets::finite_signed_cardinality(42),
    "embed_sint_ℤ(SingletonSet<int>{42}) lands at "
    "finite_signed_cardinality(42) on the SignedCardinality carrier.");
static_assert(embed_sint_ℤ(dedekind::sets::SingletonSet<
                               int, dedekind::category::ClassicalLogic>{-7})
                      .pivot == dedekind::sets::finite_signed_cardinality(-7),
              "embed_sint_ℤ(SingletonSet<int>{-7}) lands at "
              "finite_signed_cardinality(-7) on the SignedCardinality carrier "
              "(negative-value witness — the symmetric complement to "
              "embed_uint_ℕ's non-negative-only fragment).");

// Concept-level witness: the result realises the categorical image
// of the source set under the canonical mono int ↪ ℤ — Subobject
// of @c Cod<embed_sint_ℤ_> = SignedCardinality per @c :category:image.
static_assert(
    dedekind::category::IsImageOf<
        decltype(embed_sint_ℤ(dedekind::sets::SingletonSet<
                              int, dedekind::category::ClassicalLogic>{42})),
        decltype(embed_sint_ℤ_)>,
    "embed_sint_ℤ(S) realises IsImageOf<result, embed_sint_ℤ_>: result "
    "is a Subobject of Cod<embed_sint_ℤ_> = SignedCardinality, "
    "witnessing the categorical image of S under the canonical mono "
    "int ↪ ℤ.");

/**
 * @brief Type-honest absolute value at the machine layer:
 *        @c int @c → @c unsigned.
 *
 * @details The natural categorical signature for absolute value on
 * the signed-integer carrier is @c int @c → @c unsigned (the image
 * is exactly the non-negative ints, which canonically embed into
 * @c unsigned).  The C++ standard library's @c std::abs(int) @c → @c
 * int is type-@b dishonest about this — its result is always non-
 * negative but the return type doesn't say so, and the caller has to
 * know.  This function is the type-honest sibling: the @c unsigned
 * return type announces non-negativity at the type level.
 *
 * Honest-extension property: @b total at @c INT_MIN where @c
 * std::abs is UB.  The implementation works in @c unsigned modular
 * arithmetic (which is well-defined for all input bit-patterns) and
 * relies on @c 0u @c - @c static_cast<unsigned>(INT_MIN) yielding
 * @c |INT_MIN| as an @c unsigned (well-defined for any @c int width
 * and any two's-complement / sign-magnitude / ones'-complement ABI;
 * the project does not pin a 32-bit assumption).  This avoids the
 * UB that @c std::abs(INT_MIN) triggers via @c -INT_MIN overflow.
 *
 * Categorical classification: @b neither @b monic @b nor @b epic.
 *   * Not monic: distinct inputs @c +n and @c -n map to the same
 *     @c unsigned magnitude (sign-folding).
 *   * Not epic onto @c unsigned: the image is the half-range @c [0,
 *     @c |INT_MIN|], missing the upper half of @c unsigned.  Restricted
 *     to @c [0, @c |INT_MIN|] as codomain the function would be epic —
 *     but C++ has no clean "non-negative int" type to express that
 *     codomain (cf. @c std::numeric_limits<int>::digits for the
 *     width-relative phrasing).
 *
 * Variant-layer counterpart: @c abs : @c SignedCardinality @c → @c
 * Cardinality (in @c sets:cardinality), which IS surjective onto
 * the saturating @c Cardinality (because @c Cardinality has an @c
 * ℵ_0 sentinel and no upper bound on the finite fragment) — see the
 * @c Relationship_to_std_abs section in that function's docstring.
 */
export constexpr unsigned abs_int_unsigned(int x) noexcept {
  // The conditional is a clarity move; both branches reduce to
  // @c -static_cast<unsigned>(x) modulo two's-complement arithmetic.
  // Authored explicitly to make the type-honest reading obvious to a
  // reader: "if negative, negate after widening to unsigned".
  return x < 0 ? -static_cast<unsigned>(x) : static_cast<unsigned>(x);
}

// Type-signature witness: the function's return type IS @c unsigned,
// not @c int — that's the load-bearing fact this partition adds vis-
// à-vis @c std::abs.
static_assert(std::same_as<decltype(abs_int_unsigned(0)), unsigned>,
              "abs_int_unsigned has a type-honest unsigned codomain — the "
              "non-negativity of the result is announced at the type level, "
              "unlike std::abs(int) → int.");

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

// Bitwise / lattice operator surface — std::signed_integral carries
// the four bitwise operators (mirrors the unsigned-side claim in
// :uint). No axiomatic lattice claim is made here.
static_assert(dedekind::order::HasLatticeOperators<int>,
              "int carries the bitwise / lattice operator surface "
              "(&, |, ^, ~).");
static_assert(dedekind::order::HasLatticeOperators<long>,
              "long carries the bitwise / lattice operator surface.");
static_assert(dedekind::order::HasLatticeOperators<long long>,
              "long long carries the bitwise / lattice operator surface.");

// Axiomatic-ring rejection — the strictly weaker downstream consequence
// of the @c !Group_ℤ / @c !IsArithmeticAdditiveGroup pins above.
// Pinned explicitly so the std::signed_integral reader sees the
// dichotomy in one place: the operator surface fires, the axiomatic
// ring witness does not.
static_assert(
    !dedekind::category::IsRing<int, std::plus<int>, std::multiplies<int>>,
    "int is NOT an axiomatic ring under std::plus / "
    "std::multiplies: signed-overflow UB defeats closure "
    "before the ring axioms are reachable.");
static_assert(
    !dedekind::category::IsRing<long, std::plus<long>, std::multiplies<long>>,
    "long is NOT an axiomatic ring: same reason as int.");
static_assert(!dedekind::category::IsRing<long long, std::plus<long long>,
                                          std::multiplies<long long>>,
              "long long is NOT an axiomatic ring: same reason as int.");

// Sequence witness: machine signed int is a valid sequence codomain.
static_assert(
    dedekind::sequences::IsFiniteSequence<dedekind::sequences::FinitePath<int>>,
    "FinitePath<int> is a bona-fide finite sequence.");

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_sint_ℤ_)>> =
        true;
static_assert(
    IsInjective<std::decay_t<decltype(dedekind::numbers::embed_sint_ℤ_)>>,
    "embed_sint_ℤ_ (int → SignedCardinality) is "
    "registered injective.");

// IsEmbeddingFunctor witness (#633): @c embed_sint_ℤ_ is fully faithful
// (discrete source) + injective on objects (sign-magnitude bijection
// between machine signed values and the corresponding SignedCardinality
// witnesses).
template <>
inline constexpr bool is_embedding_functor_v<
    std::decay_t<decltype(dedekind::numbers::embed_sint_ℤ_)>> = true;
static_assert(
    IsEmbeddingFunctor<
        std::decay_t<decltype(dedekind::numbers::embed_sint_ℤ_)>>,
    "embed_sint_ℤ_ realises IsEmbeddingFunctor per #633's Mac Lane reading.");

// ===========================================================================
// Mazur-equivalence pilot (#591): @c std::signed_integral as @c ℤ for
// inputs in @c [std::numeric_limits<S>::min(), @c
// std::numeric_limits<S>::max()] (the unsigned partition's natural
// progression, generalised across the @c std::signed_integral family).
// ===========================================================================
//
// Per Mazur's @em When @em is @em one @em thing @em equal @em to @em some
// @em other @em thing? (Proof and Other Dilemmas 2008), the principled
// relaxation of strict equality on a carrier is a named equivalence
// relation under which the algebraic laws hold even when they don't hold
// in the bit-perfect sense.
//
// For @c std::signed_integral @c S, the relation @c std::equal_to<S> IS
// a textbook equivalence relation on @c S (reflexive / symmetric /
// transitive by the std::regular contract).  Together with the @c
// embed_sint_ℤ_ injection above (S → SignedCardinality, registered monic
// / injective), this pins the structural reading: @c S modulo @c
// std::equal_to<S> is equivalent (in the Mazur sense) to a bounded slice
// of @c ℤ for inputs in @c [std::numeric_limits<S>::min(), @c
// std::numeric_limits<S>::max()].  Outside that bound, signed-overflow
// UB makes the equivalence non-classical --- the unbounded variant
// carrier @c SignedCardinality is the right home for inputs in the
// saturation regime.
//
// This is the signed-side natural progression of the unsigned-as-ℕ
// slice in @c numbers:uint (#591); together they cover the std::integral
// family.  The @c double @c ↔ @c ℝ slice (ε-equivalence) is the harder
// case deferred until both integer slices stabilise.

static_assert(IsEquivalence<int, std::equal_to<int>>,
              "Mazur (#591): std::equal_to<int> must satisfy IsEquivalence; "
              "together with embed_sint_ℤ_ this pins (int / ==) ≡ ℤ for "
              "inputs in [INT_MIN, INT_MAX].");
static_assert(IsEquivalence<long, std::equal_to<long>>,
              "Mazur (#591): std::equal_to<long> must satisfy IsEquivalence; "
              "sister witness for the wider signed rung.");
static_assert(
    IsEquivalence<long long, std::equal_to<long long>>,
    "Mazur (#591): std::equal_to<long long> must satisfy IsEquivalence; "
    "the widest std::signed_integral rung.");
}  // namespace dedekind::category
