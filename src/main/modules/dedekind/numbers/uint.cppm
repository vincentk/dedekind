/**
 * @file dedekind/numbers/uint.cppm
 * @partition :uint
 * @module dedekind.numbers:uint
 * @brief Level 4: The std::unsigned_integral family — explicit textbook
 *        classification as ℤ/2^wℤ + universal machine→variant lift +
 *        Modular<N> / IsCyclic correspondence.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Honest_Stance
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
 * @section Three_Layers
 * The @c std::unsigned_integral family bridges three categorical layers:
 *
 *   1. @b Width-ladder colimit (within the family): @c unsigned @c char
 *      @c ↪ @c unsigned @c short @c ↪ @c unsigned @c int @c ↪ @c unsigned
 *      @c long is a colimit ladder of finite cyclic rings; each step is
 *      a ring-homomorphism on the non-overflow fragment.  Witnessed
 *      structurally below.
 *
 *   2. @b Machine → variant forgetful: @c embed_unsigned_to_Cardinality
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
 * @section Honesty_Obligation
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

export module dedekind.numbers:uint;

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;
import dedekind.order;
import dedekind.sequences;
import dedekind.sets;

namespace dedekind::numbers {

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
 * Companion to @c embed_signed_to_SignedCardinality (filed under #418),
 * which provides the symmetric lift on the @c std::signed_integral
 * side.  The lift completes the embedding chain
 * @c 𝔹 @c → @c std::unsigned_integral @c → @c ℕ documented in the
 * carrier-lattice section of @c report.tex / @c paper.tex.
 *
 * @tparam U Any @c std::unsigned_integral source type.
 */
export template <std::unsigned_integral U>
constexpr dedekind::sets::Cardinality embed_unsigned_to_Cardinality(U v) {
  return dedekind::sets::finite_cardinality(static_cast<std::size_t>(v));
}

/**
 * @brief Concrete monic arrow: unsigned ↪ Cardinality (variant ℕ-proxy).
 *
 * @details The named-arrow form of @c
 * embed_unsigned_to_Cardinality<unsigned>, registered as monic in
 * @c is_monic_arrow_v so downstream @c IsMonicArrow / @c
 * IsRingHomomorphism callsites can find it.  Distinct unsigned values
 * yield distinct @c Cardinality values (injective on the finite
 * fragment).
 */
export inline constexpr auto embed_unsigned_Cardinality_ =
    dedekind::category::arrow<unsigned, dedekind::sets::Cardinality>(
        [](const unsigned& u) noexcept -> dedekind::sets::Cardinality {
          return embed_unsigned_to_Cardinality(u);
        });

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

// Axiomatic algebra — the textbook ℤ/2^wℤ commutative ring.
static_assert(
    dedekind::algebra::IsCommutativeRing<unsigned int, std::plus<unsigned int>,
                                         std::multiplies<unsigned int>>,
    "unsigned int IS the commutative ring ℤ/2^32ℤ under modular "
    "arithmetic — additive inverses via mod wrap, * commutes.");
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
    "unsigned int is the additive cyclic group ℤ/2^32ℤ under +.");

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
// @c algebra:group).  The ring-homomorphism shape is the @b lifted
// operator agreement, not the @c short-typed surface.

static_assert(
    static_cast<unsigned int>(static_cast<unsigned short>(0xFFFF) +
                              static_cast<unsigned short>(1)) ==
        static_cast<unsigned int>(0xFFFF) + static_cast<unsigned int>(1),
    "Width-ladder ring-hom witness: + commutes with the @c unsigned "
    "@c short ↪ @c unsigned @c int inclusion on the non-overflow fragment.");
static_assert(
    static_cast<unsigned int>(static_cast<unsigned short>(0x100) *
                              static_cast<unsigned short>(0x100)) ==
        static_cast<unsigned int>(0x100) * static_cast<unsigned int>(0x100),
    "Width-ladder ring-hom witness: * commutes with the inclusion (the "
    "non-overflow fragment fits in unsigned int even when overflow on "
    "unsigned short would wrap).");

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool is_monic_arrow_v<
    std::decay_t<decltype(dedekind::numbers::embed_unsigned_Cardinality_)>> =
    true;
}  // namespace dedekind::category
