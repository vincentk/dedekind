/**
 * @file dedekind/numbers/integral.cppm
 * @partition :integral
 * @brief std::integral umbrella over uint / sint / bool.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section integral__Honest_Stance
 * The standard-library hierarchy
 *
 *   @c std::integral @c = @c std::unsigned_integral @c ||
 *                       @c std::signed_integral @c || @c std::same_as<bool>
 *
 * unifies under a single concept three carriers that this library has
 * classified as @b structurally @b distinct algebras:
 *
 *   * @c std::unsigned_integral — finite cyclic ring @c ℤ/2^wℤ (full
 *     axiomatic ring under modular wrap; closes #417, see @c :uint).
 *   * @c std::signed_integral  — literal ring-operator surface ✓,
 *     axiomatic ring laws ✗ (UB on overflow; closes #418, see @c :sint).
 *   * @c bool                  — Boolean rig under (∨, ∧); Galois field
 *     𝔽₂ under (⊕, ∧); not a field under arithmetic (closes #400 /
 *     PR #407, see @c :boolean).
 *
 * Generic code constrained on @c std::integral @c T therefore covers
 * three different algebras whose only durable common ground is the
 * @b syntactic operator surface — nothing axiomatic survives the union.
 *
 * @section integral__Dispatch_Pattern
 * Callers that need axiomatic guarantees must dispatch on the sibling
 * concepts rather than on the umbrella:
 *
 *   * @c std::unsigned_integral — pin @c IsRing / @c IsCyclicGroup.
 *   * @c std::signed_integral  — pin @c HasRingOperators (operator
 *     surface only; the axiomatic ring witness would be a false claim
 *     under signed-overflow UB).
 *   * @c bool                  — pin @c IsField under @c std::bit_xor /
 *     @c std::bit_and; @b not under @c std::plus / @c std::multiplies
 *     (bool's arithmetic operator surface promotes to @c int and the
 *     resulting structure is not a field).
 *
 * @section integral__Honesty_Obligation
 * The umbrella's @b negative claim (no std::integral sibling is a field
 * under arithmetic) is discharged by combining three witnesses, two of
 * which are pinned upstream and not restated here (vacuous restatements
 * are noise — see the project's cross-partition assertion-style memo):
 * @c !IsField<unsigned @c int> in @c :uint, @c !IsField<int> in
 * @c algebra/field.cppm (the global signed-family witness).  The single
 * pin in this partition closes the umbrella by addressing the third
 * sibling — @c bool under arithmetic operators — whose rejection has
 * not been pinned anywhere else (the @c :boolean partition pins the
 * XOR/AND field @b acceptance, which is a structurally different
 * object).
 *
 * @note "Wer wagt es, Rittersmann oder Knapp,
 *        Zu tauchen in diesen Schlund?"
 *       ("Who dares, knight or squire, / To dive into this gulf?")
 *       — Friedrich Schiller, *Der Taucher* (1797).
 */
module;

#include <bit>      // std::has_single_bit (N | 2^w soundness on unsigned)
#include <compare>  // std::three_way_comparable (gated in IsInteger)
#include <concepts>
#include <functional>
#include <type_traits>  // std::make_unsigned_t (has_single_bit well-formedness)
#include <utility>      // std::pair (the translation-graph carrier in preimage)

export module dedekind.morphologies:integral;

import dedekind.algebra;
import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :cyclic;

namespace dedekind::morphologies {
using namespace dedekind::algebra;
using namespace dedekind::order;
using namespace dedekind::sets;

/**
 * @concept IsInteger: integer-shape operator-surface concept (Euclidean
 *          ring shape — additive group + multiplicative monoid + Euclidean
 *          pair, with @c three_way_comparable for the order surface).
 *
 *  @b Composition: @c IsAlgebra over @c (+, @c *, @c %) supplies binary
 *  closure for the three Euclidean operators; the @c requires clause
 *  adds binary @c - (closure-of-subtraction), unary @c - (additive
 *  inverse), default @c T{} (the additive identity), and binary @c /
 *  (Euclidean quotient).  @c std::three_way_comparable gates the
 *  ordering surface without demanding strict reflexivity (the
 *  ℝ-as-double / IEEE-754 fragment fails reflexivity by NaN-vs-NaN,
 *  so we deliberately stop short of @c IsPreOrdered here).
 */
export template <typename T>
concept IsInteger =
    IsAlgebra<T, std::plus<T>, std::multiplies<T>, std::modulus<T>> &&
    // IsPreOrdered<T> may be rejected, as reflexivity is not guaranteed.
    // Instead, require the non-reflexive variants:
    std::three_way_comparable<T> && requires(T a, T b) {
      { a - b } -> std::same_as<T>;
      { -a } -> std::same_as<T>;
      { T{} } -> std::same_as<T>;
      { a / b } -> std::same_as<T>;
    };

/** @brief Saturating-ℤ-shape integer: integer surface + translation-invariant
 *         order (sentinels propagate cleanly; no cyclic wrap).  The variant
 *         ℤ proxy @c SignedCardinality is the canonical witness.
 *
 *  The classifier uses @c is_translation_invariant_ordered_v as the
 *  carrier-promise marker (registered in @c :algebra:scout_algebra on
 *  @c SignedCardinality); the cyclic alternative is gated below by
 *  @c IsCyclicGroup<T, std::plus<T>> rather than the duck-typed
 *  @c morphologies::IsCyclic shape concept, which checks for the
 *  @c Modular<N>-shaped member API (@c Domain / @c generator() /
 *  @c successor()) and would not fire on @c SignedExtensionalCardinal<>.
 */
export template <typename T>
concept IsSaturatingInteger =
    IsInteger<T> && is_translation_invariant_ordered_v<T>;

/** @brief Cyclic-ℤ-shape integer: integer surface + abelian-group-cyclic
 *         under @c +.  @c SignedExtensionalCardinal<> (sign-magnitude
 *         @c ℤ/2^{N*64}ℤ) is the canonical witness via
 *         @c IsCyclicGroup<SignedExtensionalCardinal<1>, std::plus<>>
 *         pinned in @c :sets:cardinality.
 */
export template <typename T>
concept IsCyclicInteger =
    IsInteger<T> && dedekind::category::IsCyclicGroup<T, std::plus<T>>;

/** @section integral__Formal_Verification Umbrella negative claims under
 * arithmetic.
 *
 * The per-sibling field rejections are pinned upstream and are NOT restated
 * here (vacuous restatements are noise — see the project's cross-partition
 * assertion-style memo):
 *   * @c !IsField<unsigned int, std::plus, std::multiplies> --- pinned in
 *     @c :uint (closes #417).
 *   * @c !IsField<int> --- pinned at @c algebra/field.cppm (the global
 *     umbrella witness for the signed family; signed-overflow UB defeats
 *     closure under arithmetic before the field axioms are reachable).
 *
 * The @c bool sibling is the only one whose arithmetic-operator field
 * rejection is genuinely new at the umbrella level: the @c :boolean
 * partition pins @c IsField<𝔹, std::bit_xor, std::bit_and> (𝔹 = 𝔽₂ under
 * XOR / AND), but the arithmetic-operator reading is a structurally
 * different object and the rejection has not been pinned before.  Filed
 * here so the umbrella reader does not silently inherit the XOR / AND
 * result.
 */
static_assert(
    !dedekind::category::IsField<bool, std::plus<bool>, std::multiplies<bool>>,
    "bool is NOT a field under std::plus / std::multiplies: "
    "the arithmetic operators promote to int; the field reading "
    "of bool lives at std::bit_xor / std::bit_and (see :boolean).");

static_assert(!dedekind::category::IsClosedUnder<bool, std::plus<>>,
              "bool is NOT closed under std::plus / std::multiplies. Instead, "
              "it promotes to int.");

static_assert(dedekind::category::IsClosedUnder<bool, std::plus<bool>>,
              "... but std::plus<bool> casts the result under the hood.");

static_assert(!IsAlgebra<bool, std::plus<>, std::multiplies<>>,
              "bool is NOT closed under std::plus / std::multiplies. Instead, "
              "it promotes to int.");

/** @section integral__IsInteger_Witnesses
 *
 * @c ExtensionalCardinal<> and @c SignedExtensionalCardinal<> are the
 * project's two structurally-faithful @c IsInteger witnesses: cyclic
 * ℤ/2^{N·64}ℤ (unsigned wrap) and overflow-free signed-magnitude
 * @c ℤ on the same limb width.  The variant ℤ proxy
 * @c SignedCardinality is the saturating sibling — it inherits
 * @c IsInteger transitively through its finite-fragment alternative
 * but is gated separately via @c IsSaturatingInteger above.
 */
static_assert(IsInteger<ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must satisfy IsInteger (Euclidean "
              "ring: +, -, *, /, % with two's-complement wrapping).");

static_assert(IsInteger<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> must satisfy IsInteger (Euclidean "
              "signed ring: sign-magnitude arithmetic, overflow-free up to "
              "2^{N*64 - 1}).");

/** @section integral__Residue_Preimage
 *
 * @brief preimage of a residue class through the translation @f$x\mapsto
 * x+K@f$: again a residue class, shifted.  @f$\{x \mid x+K \equiv R \pmod N\} =
 *        \{x \mid x \equiv R-K \pmod N\} =@f$ @c Congruence<N, (R−K) mod N>.
 *
 * @details The RESIDUE sibling of the halfspace @c preimage
 * (@c :halfspace_transport): where a bound's pivot shifts by @c −K, a residue
 * class's representative shifts by @c −K (mod N).  On the finite quotient
 * @f$\mathbb{Z}/L@f$ --- the discrete circle, one torus factor --- this is a
 * @b rotation of the class: exactly the bound-PRESERVING closed form the torus
 * needs, since the circle carries no order, so the residue class (not the
 * halfspace) is the structured predicate.  Closes @c FIXME(#797): the @c %
 * reduction's residue-class predicate @c Congruence now has its transport.  It
 * is the closed-form specialisation of the general @c preimage (@c :graph): the
 * defining property @f$\mathrm{preimage}(x{+}K, C)(a) \iff C(a+K)@f$ is
 * witnessed in @c cyclic_test.
 *
 * @b Carrier gate: @c T @c = @c decltype(N), an @b UNSIGNED machine integer, so
 * the graph carrier @b is the residue's own integer type --- the result
 * @c Congruence<N,r> is a predicate on exactly that @c Domain (HOMOGENEOUS with
 * the source, and @c Congruence reduces @c T natively, no cast).  The carrier
 * is
 * @c ℤ/2^wℤ, where the translation @f$x\mapsto x+K@f$ is @b total (it wraps, no
 * overflow UB --- unlike a @b signed carrier, on which @c INT_MAX+K is
 * undefined so the graph is not a total function and the defining property
 * cannot hold at the boundary; the signed branch is therefore @b not admitted).
 * @c x+K folds mod the width @b before @c mod @c N, so the reduction agrees
 * exactly when
 * @c N @c | @c 2^w --- i.e.\ @c N is a power of two (@c std::has_single_bit),
 * the
 * @c IsCongruenceQuotient side-condition (#803).  This is precisely the
 * discrete circle @c ℤ/L the torus rotates on (@c L a power of two).  @c bool
 * and the saturating/variant ℤ-proxy @c SignedCardinality (whose @c ±ℵ_0
 * saturation breaks residue-equivalence, and which @c Congruence cannot reduce)
 * are declined by construction.
 */
export template <typename T, auto K, auto N, decltype(N) R, typename L>
  requires std::same_as<std::remove_cvref_t<T>, decltype(N)> &&
           std::unsigned_integral<decltype(N)> &&
           (std::has_single_bit(
               static_cast<std::make_unsigned_t<decltype(N)>>(N)))
constexpr auto preimage(
    const Set<std::pair<T, T>, L, ProjAddConstProj<1, K, Rel::Eq, 2>>&,
    Congruence<N, R>) {
  // Compute in the carrier's own arithmetic (unsigned ℤ/2^w): apply the graph's
  // static_cast<T>(K) FIRST (so a huge shift folds exactly as the graph sees
  // it), then the modular subtraction.  The unsigned wrap of (R − k) is
  // harmless because N | 2^w (has_single_bit): 2^w ≡ 0 (mod N), so (R − k) mod
  // N is the true residue with no widening and no signed overflow.
  constexpr decltype(N) k = static_cast<decltype(N)>(K);  // the graph's shift
  constexpr decltype(N) r = static_cast<decltype(N)>(R - k) % N;
  return Congruence<N, r>{};
}

namespace {
// The residue rotates by −K (mod N) on ℤ/N.  Witnessed on the CYCLIC carrier
// unsigned = ℤ/2^wℤ with N=4 a power of two (N | 2^w) --- the discrete circle
// ℤ/L the torus rotates on.  The graph carrier is the residue's own integer
// type (T = decltype(N) = unsigned), so the result is homogeneous.
inline constexpr auto ℤ4 = dedekind::sets::Ω<unsigned>;
static_assert(std::same_as<decltype(preimage(ℤ4 * ℤ4 | π1 + fix(1_c) == π2,
                                             Congruence<4u, 2u>{})),
                           Congruence<4u, 1u>>,
              "preimage(x↦x+1, {x≡2 mod4}) = {x≡1 mod4} on ℤ/2^w.");
static_assert(std::same_as<decltype(preimage(ℤ4 * ℤ4 | π1 + fix(3_c) == π2,
                                             Congruence<4u, 2u>{})),
                           Congruence<4u, 3u>>,
              "preimage(x↦x+3, {x≡2 mod4}) = {x≡3 mod4} (2−3 ≡ 3 mod 4).");
}  // namespace

/** @section integral__Classifier_Partition_Witnesses
 *
 * The two structural ℤ proxies partition cleanly across
 * @c IsSaturatingInteger / @c IsCyclicInteger.  These pins document the
 * intended classification so future trait changes cannot silently invert
 * the partition.
 */
static_assert(IsSaturatingInteger<SignedCardinality>,
              "SignedCardinality is the canonical saturating-ℤ witness "
              "(translation-invariant order; ±ℵ_0 / NaZ escalation).");
static_assert(!IsCyclicInteger<SignedCardinality>,
              "SignedCardinality is NOT cyclic: it is the saturating "
              "ℤ proxy, not a finite cyclic group.");
static_assert(IsCyclicInteger<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> is the canonical cyclic-ℤ "
              "witness (sign-magnitude ℤ/2^{N*64}ℤ under +).");
static_assert(!IsSaturatingInteger<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> is NOT saturating: it wraps "
              "cyclically rather than escalating to a sentinel.");

}  // namespace dedekind::morphologies
