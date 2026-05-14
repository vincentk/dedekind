/**
 * @file dedekind/numbers/uint.cppm
 * @partition :uint
 * @brief std::unsigned_integral as ℤ/2^wℤ — modular ring, not ℕ.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section uint__Honest_Stance
 * Each width-@c w instance of @c std::unsigned_integral (`unsigned int`,
 * `unsigned long`, `std::size_t`, plus the narrow widths modulo integer
 * promotion) is the finite cyclic ring @c ℤ/2^wℤ under modular
 * arithmetic --- a @b commutative @b ring, with full additive inverses
 * via mod wrap, NOT the textbook @c ℕ.  Calling @c unsigned @c int
 * "natural numbers" is a category-theoretic error in the direction of
 * claiming @b more structure than @c ℕ has: the textbook @c ℕ is a
 * commutative semiring @b without additive inverses.  The library's
 * textbook @c ℕ lives one carrier step deeper, on the variant
 * @c Cardinality (with @c ℵ_0 saturation).
 *
 * @section uint__Three_Layers
 * The @c std::unsigned_integral family bridges three categorical layers:
 *
 *   1. @b Width-ladder colimit (within the family): @c unsigned @c char
 *      @c ↪ @c unsigned @c short @c ↪ @c unsigned @c int @c ↪ @c unsigned
 *      @c long is a colimit ladder of finite cyclic rings; each step is
 *      a ring-homomorphism on the non-overflow fragment.  Witnessed
 *      structurally below.
 *
 *   2. @b Machine → variant forgetful: @c embed_uint_ℕ
 *      lifts the modular ring into the saturating commutative monoid
 *      @c Cardinality, @b forgetting the additive inverse the source
 *      carries via mod wrap.
 *
 *   3. @b Modular<2^w> correspondence: @c std::unsigned_integral @c T is
 *      semantically isomorphic to @c morphologies::Modular<2^w> as a
 *      cyclic ring, but does @b NOT satisfy @c morphologies::IsCyclic
 *      (which requires the @c Domain / @c generator() / @c successor()
 *      member API that primitive integer types lack).  The structural
 *      sibling @c Modular<N> @b does satisfy @c IsCyclic.  Both satisfy
 *      the axiomatic @c category::IsCyclicGroup<T, std::plus<T>>.  The
 *      asymmetry is documented + witnessed below.
 *
 * @section uint__Adjunction_Vocabulary_Cross_Reference
 * The categorical layers above use vocabulary defined in the @c
 * :adjunction partition of @c dedekind.category (PR #437):
 *
 *   * @c HasAdjunctionShape<F, U> --- the 2-parameter structural form,
 *     names just the signatures of an adjoint pair without committing
 *     to specific unit / counit components.
 *   * @c IsFreeFunctor<F, U> / @c IsForgetfulFunctor<U, F> ---
 *     directional aliases that read in either direction at the call
 *     site.
 *   * @c IsAdjunction<Left, Right, Unit, Counit> --- the 4-parameter
 *     witness form, requires explicit unit and counit components
 *     whose typing lets downstream code state the triangle identities.
 *
 * The @b machine→variant @b forgetful arrow @c
 * embed_uint_ℕ is a structurally forgetful Set-arrow
 * (Ring → CMon, dropping the additive inverse) rather than a forgetful
 * @b functor in the strict @c IsForgetfulFunctor sense; reading it as
 * the action-component of the variant Grothendieck adjunction is the
 * natural categorical bridge.  The @b variant Grothendieck @b
 * adjunction itself ( @c Cardinality @c ⊣ @c SignedCardinality, with
 * @c embed_uint_sint_ as the unit and @c abs as the retraction-not-counit) is
 * cross-referenced from the carrier-lattice section of
 * @c report.tex / @c paper.tex but @b not reified as an
 * @c IsAdjunction witness in this partition (that belongs to #402's
 * closing PR or a dedicated successor).
 *
 * @section uint__Honesty_Obligation
 * The witness blocks below are the project's @b honesty obligation made
 * structural: each @c static_assert is an engineer's claim that the
 * named carrier exhibits the named property.  The Curry--Howard
 * reading (Wadler 2015, @c Propositions @c as @c Types) is that types
 * are propositions and type-checking is mechanical proof verification
 * --- the engineer asserts, the compiler discharges.  The professional
 * grounding is the NSPE @c Code @c of @c Ethics @c for @c Engineers
 * (Canons III + IV: truthfulness in public statements; faithful
 * agency).  See the @c §"poor man's theorem prover" footnote in
 * @c report.tex / @c paper.tex for the full literature anchor.
 *
 * @note "Die Reihe der natürlichen Zahlen N ist ein einfach unendliches
 *        System."
 *       ("The series of natural numbers N is a simply infinite system.")
 *       — Richard Dedekind, *Was sind und was sollen die Zahlen?*
 *         (1888), §6.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <limits>
#include <type_traits>  // std::remove_cvref_t (no-narrowing pin in
                        // embed_uint_ℕ's set-level overload)
#include <utility>      // std::forward (used in embed_uint_ℕ's set-level lift)

export module dedekind.morphologies:uint;

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sequences;
import dedekind.sets;
import :cyclic;

namespace dedekind::morphologies {
using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @brief Canonical embedding 𝔹 ↪ ℕ: bool → unsigned.
 * @details False maps to 0, True maps to 1.
 */
export inline constexpr auto embed_𝔹_uint_ =
    arrow<bool, unsigned>([](const bool& b) noexcept { return b ? 1u : 0u; });

// ===========================================================================
// (1) Universal machine→variant lift: std::unsigned_integral → Cardinality
// ===========================================================================

/**
 * @brief Universal machine-to-variant lift: @c std::unsigned_integral
 *        @c → @c Cardinality.  Closes part of #417.
 *
 * @details The structure-forgetting lift from the modular finite ring
 * @c ℤ/2^wℤ (which is what @c std::unsigned_integral honestly is — a
 * commutative ring with full additive inverses via mod wrap, NOT @c ℕ
 * in the textbook sense) into the saturating commutative monoid
 * @c Cardinality (the variant @c ℕ-proxy with @c ℵ_0 escalation; no
 * additive inverse).
 *
 * Categorically the arrow @b forgets the modular ring structure: the
 * source has additive inverses (every @c u admits @c (2^w - u) as its
 * additive inverse via mod wrap), while @c Cardinality has none.  The
 * arrow is @b not a ring homomorphism (it cannot be — the codomain is
 * not a ring).  It is the canonical Set-injection on the finite
 * fragment, lifting the machine value @c u to
 * @c Cardinality{ExtensionalCardinal<>{u}}; the @c ℵ_0 alternative on
 * the codomain stays unreached on the embedding direction.
 *
 * Companion to @c embed_sint_ℤ (filed under #418),
 * which provides the symmetric lift on the @c std::signed_integral
 * side.  The lift completes the embedding chain
 * @c 𝔹 @c → @c std::unsigned_integral @c → @c ℕ documented in the
 * carrier-lattice section of @c report.tex / @c paper.tex.
 *
 * @tparam U Any @c std::unsigned_integral source type.
 */
export template <std::unsigned_integral U>
constexpr dedekind::sets::Cardinality embed_uint_ℕ(U v) {
  static_assert(std::numeric_limits<U>::digits <=
                    std::numeric_limits<std::size_t>::digits,
                "embed_uint_ℕ requires every value of U to "
                "be representable as std::size_t; otherwise the conversion "
                "would silently truncate and break injectivity.  On platforms "
                "where this fires (e.g. 32-bit std::size_t with 64-bit "
                "unsigned long long), a wider Cardinality finite-fragment "
                "constructor or an explicit ℵ_0 escalation on overflow would "
                "be needed.");
  return dedekind::sets::finite_cardinality(static_cast<std::size_t>(v));
}

/**
 * @brief Concrete monic arrow: unsigned ↪ Cardinality (variant ℕ-proxy).
 *
 * @details The named-arrow form of @c
 * embed_uint_ℕ<unsigned>, registered as monic in
 * @c is_monic_arrow_v so downstream @c IsMonicArrow / @c
 * IsRingHomomorphism callsites can find it.  Distinct unsigned values
 * yield distinct @c Cardinality values (injective on the finite
 * fragment).
 */
export inline constexpr auto embed_uint_ℕ_ =
    dedekind::category::arrow<unsigned, dedekind::sets::Cardinality>(
        [](const unsigned& u) noexcept -> dedekind::sets::Cardinality {
          return embed_uint_ℕ(u);
        });

/**
 * @brief Set-level lift of @c embed_uint_ℕ_: image of an @c unsigned-set
 *        @c S under the canonical mono @c unsigned ↪ ℕ.
 *
 * @details Layer-1 entry per #602, sister to @c embed_𝔹_ℕ (PR #624) and
 * @c embed_𝔹_𝕂3 (PR #626): names the construction at the call site
 * rather than re-spelling @c image(embed_uint_ℕ_, S).  Accepted input
 * @c S is anything @c dedekind::sets::image already dispatches on —
 * @c SingletonSet (@c :sets:singleton),
 * @c std::set<unsigned> / @c std::unordered_set<unsigned>
 * (@c :sets:extensional); lazy predicate sets join the dispatch table
 * when #602's layer 2 lands.
 *
 * @note This overload is on the @b set side; the per-value entry point
 * @c embed_uint_ℕ(U @c v) — the templated function above for any
 * @c std::unsigned_integral @c U — is unchanged and kept for the value-
 * level call sites that already exist in @c :uint and @c :integer (see
 * @c uint_test.cpp, @c tower_test.cpp, @c embed_uint_sint_).  Overload
 * resolution disambiguates via the @c requires-clause: the per-value
 * overload requires @c std::unsigned_integral, this one requires
 * @c image to be well-formed, and no @c U satisfies both at once.
 *
 * Mathematically: the image of @c S under the canonical mono
 * @c unsigned @c ↪ @c ℕ is a subset of the finite fragment of @c
 * Cardinality containing whichever @c unsigned values are in @c S.
 */
// No-narrowing pin: the source set's element type must be EXACTLY
// @c unsigned.  Without this gate, sets over @c int (sign reinterpret:
// @c int(-1) → @c unsigned(UINT_MAX)) or over a wider unsigned
// (truncation: @c unsigned @c long @c long(UINT_MAX+1) → @c unsigned(0))
// would satisfy the bare @c image-well-formedness requires-clause via
// implicit conversion at the per-element @c embed_uint_ℕ_ call site,
// silently breaking the canonical-mono contract and bypassing the
// @c digits-safety @c static_assert in the per-value @c embed_uint_ℕ(U).
// We pin both the @c Domain-exposing carriers (SingletonSet,
// dedekind::sets::Set, …) and the @c value_type-exposing carriers
// (std::set, std::unordered_set) in a single disjunctive constraint —
// either typedef must match exactly @c unsigned for the overload to
// fire.
export template <typename S>
  requires(
              requires {
                typename std::remove_cvref_t<S>::Domain;
                requires std::same_as<typename std::remove_cvref_t<S>::Domain,
                                      unsigned>;
              } ||
              requires {
                typename std::remove_cvref_t<S>::value_type;
                requires std::same_as<
                    typename std::remove_cvref_t<S>::value_type, unsigned>;
              }) &&
          requires(S&& s) {
            dedekind::sets::image(embed_uint_ℕ_, std::forward<S>(s));
          }
constexpr auto embed_uint_ℕ(S&& s) {
  return dedekind::sets::image(embed_uint_ℕ_, std::forward<S>(s));
}

// (The no-narrowing pin in the @c requires-clause above is the substantive
// guard.  Negative-witness @c static_asserts of the form
// @c "!requires @c { @c embed_uint_ℕ(SingletonSet<int>{0}); @c }" trigger
// a hard "no matching function" diagnostic on clang-22 inside the
// nested-requires context rather than absorbing as SFINAE; pin via
// runtime tests in @c uint_test.cpp instead.)

// Set-level lift witness: @c embed_uint_ℕ on @c SingletonSet<unsigned>{42}
// lands at @c finite_cardinality(42).  Pinned at the @b value level so
// the pivot equality is constant-evaluated, not just the codomain type.
// Mirrors PR #624's witnesses for @c embed_𝔹_ℕ and PR #626's for
// @c embed_𝔹_𝕂3 — same shape, different (carrier, codomain) pair.
static_assert(embed_uint_ℕ(dedekind::sets::SingletonSet<
                               unsigned, dedekind::category::ClassicalLogic>{
                               42u})
                      .pivot == dedekind::sets::finite_cardinality(42),
              "embed_uint_ℕ(SingletonSet<unsigned>{42}) lands at "
              "finite_cardinality(42) on the Cardinality carrier.");
static_assert(embed_uint_ℕ(dedekind::sets::SingletonSet<
                               unsigned, dedekind::category::ClassicalLogic>{
                               0u})
                      .pivot == dedekind::sets::finite_cardinality(0),
              "embed_uint_ℕ(SingletonSet<unsigned>{0}) lands at "
              "finite_cardinality(0) on the Cardinality carrier.");

// Concept-level witness: the result realises the categorical image of
// the source set under the canonical mono unsigned ↪ ℕ — Subobject
// of @c Cod<embed_uint_ℕ_> = Cardinality per @c :category:image.
static_assert(
    dedekind::category::IsImageOf<
        decltype(embed_uint_ℕ(dedekind::sets::SingletonSet<
                              unsigned, dedekind::category::ClassicalLogic>{
            42u})),
        decltype(embed_uint_ℕ_)>,
    "embed_uint_ℕ(S) realises IsImageOf<result, embed_uint_ℕ_>: result "
    "is a Subobject of Cod<embed_uint_ℕ_> = Cardinality, witnessing the "
    "categorical image of S under the canonical mono unsigned ↪ ℕ.");

// ===========================================================================
// (2) Family classification — std::unsigned_integral as ℤ/2^wℤ
// ===========================================================================
//
// Witnesses below pin the std-unsigned-integral classification for the
// three canonical representatives in a single block, each annotated with
// the textbook reading.  Per-carrier witnesses already exist scattered in
// @c algebra:ring / @c category:total / etc.; this partition consolidates
// the textbook narrative into a single @c numbers/-side audit trail.

// Operator shape — the C++ surface that maps to the modular ring laws.
static_assert(dedekind::algebra::HasRingOperators<unsigned int>,
              "unsigned int closes the literal ring operator surface "
              "(+, binary -, unary -, *) modulo wrap.");
static_assert(dedekind::algebra::HasRingOperators<unsigned long>,
              "unsigned long closes the literal ring operator surface.");
static_assert(dedekind::algebra::HasRingOperators<std::size_t>,
              "std::size_t closes the literal ring operator surface.");
static_assert(dedekind::algebra::HasGroupOperatorsAdd<unsigned int>,
              "unsigned int closes the additive-group operator surface "
              "(+, unary -, T{}); modular wrap supplies the inverse.");
static_assert(dedekind::algebra::HasGroupOperatorsAdd<unsigned long>,
              "unsigned long closes the additive-group operator surface.");
static_assert(dedekind::algebra::HasGroupOperatorsAdd<std::size_t>,
              "std::size_t closes the additive-group operator surface.");

// Axiomatic algebra — the textbook ℤ/2^wℤ commutative ring.  The
// width @c w is platform-dependent (numeric_limits<unsigned int>::digits;
// commonly 32 on LP64 / LLP64 systems but not guaranteed by the
// standard).  Witness messages are kept width-agnostic.
static_assert(
    dedekind::algebra::IsCommutativeRing<unsigned int, std::plus<unsigned int>,
                                         std::multiplies<unsigned int>>,
    "unsigned int IS the commutative ring ℤ/2^wℤ at the unsigned-int width "
    "under modular arithmetic — additive inverses via mod wrap, * commutes.");
static_assert(dedekind::algebra::IsCommutativeRing<
                  unsigned long, std::plus<unsigned long>,
                  std::multiplies<unsigned long>>,
              "unsigned long IS the commutative ring ℤ/2^wℤ at the platform "
              "long width.");
static_assert(
    dedekind::algebra::IsCommutativeRing<std::size_t, std::plus<std::size_t>,
                                         std::multiplies<std::size_t>>,
    "std::size_t IS the commutative ring ℤ/2^wℤ at the pointer "
    "width.");

// IsArithmeticRing seal (PR #394): strict ring proof + literal C++
// operators agree on the carrier.
static_assert(dedekind::algebra::IsArithmeticRing<unsigned int>,
              "unsigned int: strict ring proof + literal C++ operators "
              "agree under modular wrap.");
static_assert(dedekind::algebra::IsArithmeticRing<unsigned long>,
              "unsigned long: strict ring proof + literal C++ operators "
              "agree under modular wrap.");
static_assert(dedekind::algebra::IsArithmeticRing<std::size_t>,
              "std::size_t: strict ring proof + literal C++ operators "
              "agree under modular wrap.");

// Cyclic-group structure (axiomatic): additive cyclic group of order
// @c 2^w generated by @c 1.  See section (3) below for the structural
// (member-API) IsCyclic story.
static_assert(
    dedekind::category::IsCyclicGroup<unsigned int, std::plus<unsigned int>>,
    "unsigned int is the additive cyclic group ℤ/2^wℤ at the "
    "unsigned-int width under +.");

// Negative axiomatic witness: NOT a field.  Reason: @c 0 has no
// multiplicative inverse; even-numbered elements share the @c 2
// zero-divisor in @c ℤ/2^wℤ (e.g.\ @c 2 @c · @c 2^(w-1) @c ≡ @c 0); the
// modular ring is not an integral domain unless @c 2^w is prime, which
// it isn't for @c w @c ≥ @c 2.  Pinned as an explicit honesty
// statement: the commutative-ring claim is real, but it stops short of
// field.
static_assert(!dedekind::algebra::IsField<unsigned int, std::plus<unsigned int>,
                                          std::multiplies<unsigned int>>,
              "unsigned int is NOT a field: 0 has no multiplicative inverse, "
              "and ℤ/2^wℤ has zero divisors (2 · 2^(w-1) ≡ 0).");

// Order surface — every @c std::unsigned_integral is totally ordered at
// the machine layer.
static_assert(dedekind::order::HasTotalOrderOperators<unsigned int>,
              "unsigned int carries the total-order operator surface.");
static_assert(dedekind::order::HasTotalOrderOperators<unsigned long>,
              "unsigned long carries the total-order operator surface.");
static_assert(dedekind::order::HasTotalOrderOperators<std::size_t>,
              "std::size_t carries the total-order operator surface.");
static_assert(dedekind::order::IsTotallyOrdered<unsigned int>,
              "unsigned int is axiomatically totally ordered.");
static_assert(dedekind::order::IsDirectedSet<unsigned int>,
              "unsigned int is a directed set (net domain).");
static_assert(dedekind::order::IsDirectedSet<unsigned long>,
              "unsigned long is a directed set (net domain).");
static_assert(dedekind::order::IsDirectedSet<std::size_t>,
              "std::size_t is a directed set (net domain) — anchors the "
              "default C++ subscript Idx as a bona fide algebraic index.");

// Bitwise / lattice operator surface — std::unsigned_integral carries
// the four bitwise operators, satisfying the syntactic-shape concept
// from :order:lattice. The bundled IsOrderLattice axiomatic claim is
// not made here (it lives at the Boolean-ring reading on :boolean).
static_assert(dedekind::order::HasLatticeOperators<unsigned int>,
              "unsigned int carries the bitwise / lattice operator surface "
              "(&, |, ^, ~).");
static_assert(dedekind::order::HasLatticeOperators<unsigned long>,
              "unsigned long carries the bitwise / lattice operator surface.");
static_assert(dedekind::order::HasLatticeOperators<std::size_t>,
              "std::size_t carries the bitwise / lattice operator surface.");

// Sequence witness: machine unsigned is a valid sequence codomain.
static_assert(dedekind::sequences::IsFiniteSequence<
                  dedekind::sequences::FinitePath<unsigned int>>,
              "FinitePath<unsigned int> is a bona-fide finite sequence.");

// ===========================================================================
// (3) Modular<N> / IsCyclic correspondence
// ===========================================================================
//
// @c std::unsigned_integral @c T is @b semantically isomorphic to
// @c morphologies::Modular<2^w> as a cyclic ring, but the two are not
// the @b same C++ type:
//
//   * @c Modular<N> (in @c morphologies:cyclic) ships an explicit
//     @c Domain / @c generator() / @c successor() member API and
//     therefore satisfies the @b structural @c morphologies::IsCyclic
//     concept.  It is the textbook ℤ/Nℤ ring carrier.
//   * @c std::unsigned_integral primitives have no such member API
//     (they are built-in scalars) and therefore do @b NOT satisfy
//     @c morphologies::IsCyclic, even though they are axiomatically
//     cyclic groups under @c +.
//
// Both @b do satisfy the axiomatic @c category::IsCyclicGroup<T,
// std::plus<T>>: that concept tests @c IsAbelianGroup plus the
// @c is_cyclic_group_v opt-in trait, which is registered for both
// @c std::unsigned_integral primitives (in @c category:total) and for
// @c Modular<N> (in @c morphologies:cyclic).  Asymmetry pinned below.

// Negative structural witness: the primitive does not carry the
// member-API surface of the structural @c IsCyclic concept.
static_assert(!dedekind::morphologies::IsCyclic<unsigned int>,
              "unsigned int is axiomatically cyclic (IsCyclicGroup ✓) but "
              "does NOT satisfy the structural morphologies::IsCyclic "
              "concept — primitives lack the Domain / generator() / "
              "successor() member API; the structural sibling Modular<N> "
              "carries that API.");

// Positive structural witness on the explicit ring carrier.
static_assert(
    dedekind::morphologies::IsCyclic<dedekind::morphologies::Modular<256>>,
    "Modular<256> satisfies the structural IsCyclic concept "
    "(carries Domain / generator() / successor()).");
static_assert(dedekind::category::IsCyclicGroup<
                  dedekind::morphologies::Modular<256>,
                  std::plus<dedekind::morphologies::Modular<256>>>,
              "Modular<256> also satisfies axiomatic IsCyclicGroup under +.");

// ===========================================================================
// (4) Width-ladder ring-homomorphism witness
// ===========================================================================
//
// The chain @c unsigned @c char @c ↪ @c unsigned @c short @c ↪ @c
// unsigned @c int @c ↪ @c unsigned @c long is a colimit ladder of
// finite cyclic rings (each step preserves @c + and @c * on the non-
// overflow fragment).  We pin one representative step here as a
// structural witness; the full ladder is mechanical from the C++
// integer-promotion rules.
//
// Note: narrow-width @c HasGroupOperatorsAdd refuses on @c unsigned
// @c short / @c unsigned @c char because integer promotion lifts
// literal @c + to @c int (see PR #394's Pattern-(b) discussion in
// @c algebra:group).  To witness preservation on the @b non-overflow
// fragment of @c unsigned @c short ↪ @c unsigned @c int, we cast the
// computed result back to @c unsigned @c short on the source side and
// pick operands whose sum / product still fit in @c unsigned @c short.
// (Without the cast-back, integer promotion makes both sides compute
// in @c int and the equality holds even for values that @b would
// overflow modulo @c 2^16 — masking the witness's intent.)

static_assert(
    static_cast<unsigned int>(
        static_cast<unsigned short>(static_cast<unsigned short>(0xFFFE) +
                                    static_cast<unsigned short>(1))) ==
        static_cast<unsigned int>(0xFFFE) + static_cast<unsigned int>(1),
    "Width-ladder ring-hom witness: + commutes with the @c unsigned "
    "@c short ↪ @c unsigned @c int inclusion on the non-overflow fragment "
    "(0xFFFE + 1 = 0xFFFF fits in unsigned short, no wrap on either side).");
static_assert(
    static_cast<unsigned int>(
        static_cast<unsigned short>(static_cast<unsigned short>(0x00FF) *
                                    static_cast<unsigned short>(0x0101))) ==
        static_cast<unsigned int>(0x00FF) * static_cast<unsigned int>(0x0101),
    "Width-ladder ring-hom witness: * commutes with the inclusion on the "
    "non-overflow fragment (0xFF · 0x101 = 0xFFFF fits in unsigned short).");

}  // namespace dedekind::morphologies

namespace dedekind::category {
using namespace dedekind::morphologies;
template <>
inline constexpr bool is_monic_arrow_v<std::decay_t<decltype(embed_uint_ℕ_)>> =
    true;
static_assert(IsInjective<std::decay_t<decltype(embed_uint_ℕ_)>>,
              "embed_uint_ℕ_ (unsigned → Cardinality) is "
              "registered injective.");

// IsEmbeddingFunctor witness (#633): @c embed_uint_ℕ_ is fully faithful
// (discrete source) + injective on objects (machine unsigned values map
// bijectively to their Cardinality witnesses in @c [0, 2^width)).
template <>
inline constexpr bool
    is_embedding_functor_v<std::decay_t<decltype(embed_uint_ℕ_)>> = true;
static_assert(
    IsEmbeddingFunctor<std::decay_t<decltype(embed_uint_ℕ_)>>,
    "embed_uint_ℕ_ realises IsEmbeddingFunctor per #633's Mac Lane reading.");

// IsMonotone witness (#664 morphism vocabulary): the canonical inclusion
// @c unsigned @c → @c Cardinality preserves order — both carriers are
// totally ordered and the embedding sends @c k to @c finite_cardinality(k),
// which is comparable to other finite cardinalities via the same numeric
// magnitude.
template <>
inline constexpr bool
    is_monotone_v<std::decay_t<decltype(embed_uint_ℕ_)>, std::less_equal<>> =
        true;
static_assert(IsMonotone<std::decay_t<decltype(embed_uint_ℕ_)>>,
              "embed_uint_ℕ_ (unsigned → Cardinality) is monotone — "
              "the canonical inclusion preserves the numeric order.");

// ===========================================================================
// Mazur-equivalence pilot (#591): @c std::unsigned_integral as @c ℕ for
// inputs strictly less than @c 2^width.
// ===========================================================================
//
// Per Mazur's @em When @em is @em one @em thing @em equal @em to @em some
// @em other @em thing? (Proof and Other Dilemmas 2008), the principled
// relaxation of strict equality on a carrier is a named equivalence
// relation under which the algebraic laws hold even when they don't hold
// in the bit-perfect sense.
//
// For @c std::unsigned_integral T, the relation @c std::equal_to<T>
// IS a textbook equivalence relation on T (reflexive / symmetric /
// transitive by the std::regular contract that std::unsigned_integral
// inherits).  Together with the @c embed_uint_ℕ_ injection above
// (T-as-modular-ring → Cardinality-as-ℕ, registered monic / injective),
// this pins the structural reading: @c T modulo @c std::equal_to<T> is
// equivalent (in the Mazur sense) to a bounded prefix of @c ℕ for inputs
// strictly less than @c 2^width(T).  Above the bound, modular wraparound
// makes the equivalence non-classical --- the unbounded variant carrier
// @c Cardinality is the right home for inputs in the saturation regime.
//
// This is the unsigned-side @b first @b slice of the Mazur-equivalence
// escape hatch (#591); the @c double @c ↔ @c ℝ slice (ε-equivalence)
// will follow once the unsigned wiring stabilises.
//
// The witnesses below pin the equivalence concept from
// @c category:equivalence on the std::unsigned_integral family, paired
// with the existing @c embed_uint_ℕ_ injection that already names the
// structural map @c T @c → @c ℕ.

static_assert(IsEquivalence<unsigned int, std::equal_to<unsigned int>>,
              "Mazur (#591): std::equal_to<unsigned int> must satisfy "
              "IsEquivalence; together with embed_uint_ℕ_ this pins "
              "(unsigned int / ==) ≡ ℕ for inputs < "
              "2^std::numeric_limits<unsigned int>::digits "
              "(platform-dependent; typically 2^32 but not guaranteed by "
              "the C++ standard).");
static_assert(IsEquivalence<unsigned long, std::equal_to<unsigned long>>,
              "Mazur (#591): std::equal_to<unsigned long> must satisfy "
              "IsEquivalence; sister witness for the wider unsigned rung.");
static_assert(
    IsEquivalence<unsigned long long, std::equal_to<unsigned long long>>,
    "Mazur (#591): std::equal_to<unsigned long long> must satisfy "
    "IsEquivalence; the widest std::unsigned_integral rung.");
static_assert(IsEquivalence<std::size_t, std::equal_to<std::size_t>>,
              "Mazur (#591): std::equal_to<std::size_t> must satisfy "
              "IsEquivalence; the canonical bounded-cardinality witness "
              "for the indexing surface.");

template <>
inline constexpr bool is_monic_arrow_v<std::decay_t<decltype(embed_𝔹_uint_)>> =
    true;
static_assert(IsInjective<std::decay_t<decltype(embed_𝔹_uint_)>>,
              "embed_𝔹_uint_ (𝔹 ↪ ℕ) is registered injective.");

// IsMonotone witness (#664 morphism vocabulary): @c false @c ↦ @c 0 and
// @c true @c ↦ @c 1, which respects the canonical Boolean order
// @c false @c ≤ @c true via the numeric order @c 0 @c ≤ @c 1.
template <>
inline constexpr bool
    is_monotone_v<std::decay_t<decltype(embed_𝔹_uint_)>, std::less_equal<>> =
        true;
static_assert(IsMonotone<std::decay_t<decltype(embed_𝔹_uint_)>>,
              "embed_𝔹_uint_ (𝔹 ↪ ℕ) is monotone — preserves the "
              "Boolean / numeric order.");
}  // namespace dedekind::category
