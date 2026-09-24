/**
 * @file dedekind/algebra/scout_algebra.cppm
 * @partition :scout_algebra
 * @brief Ordered-algebra markers and concepts: carriers whose order is
 *        compatible with their group / ring / field structure (#664, #895).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section scout_algebra__Motivation
 *
 * Compatibility of a total order with the algebraic operations is a
 * @f$\forall@f$-law over VALUES, not a property of types or operation
 * signatures.  A concept can only inspect the latter, so the order-
 * compatibility axioms are carried by @b opt-in @b markers
 * (@c is_translation_invariant_ordered / @c is_scaling_invariant_ordered)
 * that a carrier specialises when its arithmetic genuinely preserves the
 * order.  The markers are then composed with the type-indexed algebraic
 * concepts (@c IsAbelianGroup / @c IsCommutativeRing / @c IsField) to give
 * the ordered-algebra concepts (@c IsOrderedAdditiveGroup /
 * @c IsOrderedCommutativeRing / @c IsOrderedMultiplicativeGroup).  The
 * additive marker is the translation-invariance axiom for an ordered group,
 * made a per-carrier opt-in; the field concepts add the Artin--Schreier
 * order-compatibility axioms that pin an @b ordered (formally real) field.
 *
 * @section scout_algebra__Consumers
 *
 * These concepts are the algebraic preconditions for halfspace transport
 * and the ordered-numeric carriers: they are consumed by
 * @c :algebra:halfspace_transport, @c :numbers:complex, @c :numbers:rational,
 * @c :numbers:quadratic, @c :numbers:integer, and
 * @c :morphologies:integral.
 *
 * @note This partition previously also hosted the symbolic scout-algebra
 *       layer (@c GroupScout, @c AffineImageOfHalfspace, and the
 *       @c element<T> @c + @c bound<k> affine factory operators).  Those
 *       were @c export ed here and re-exported by @c dedekind.algebra, so
 *       this was a @b public (exported) API, not internal scaffolding.  It
 *       was removed under #895 (scout sunset) as intentional pre-1.0
 *       cleanup; the removal was safe because the layer had zero live
 *       (non-test) consumers.  Only the ordered-algebra concept layer
 *       survives here; relocating it to a better-named partition is
 *       deferred to a later PR.
 *
 * Wikipedia: Ordered field, Linearly ordered group, Artin-Schreier theory
 *
 * @note "Die Zahlen sind freie Schöpfungen des menschlichen Geistes."
 *       ("Numbers are free creations of the human mind.")
 *       -- Richard Dedekind, Was sind und was sollen die Zahlen?,
 *          Vorwort (1888)
 */
module;

#include <functional>
#include <type_traits>

export module dedekind.algebra:scout_algebra;

import dedekind.category; // IsAbelianGroup, IsCommutativeRing, IsField
import dedekind.sets;     // SignedCardinality

namespace dedekind::algebra {

/**
 * @brief Marker trait: @c T is a non-cyclic ordered additive group
 *        whose order is translation-invariant, with the saturating
 *        escape contract for values that would exceed representable
 *        capacity.
 *
 * @details
 * Defaults to @c false.  Carriers opt in via specialisation when their
 * arithmetic is @b not @b cyclic --- i.e.\ they @b saturate or
 * @b escalate (e.g.\ to @f$\pm \aleph_0@f$ sentinels) rather than
 * wrapping at the representable bound.  Saturation preserves the
 * partial order @f$\le@f$ under translation (if @c a @c <= @c b then
 * @c a+c @c <= @c b+c, including the case where both saturate to the
 * same sentinel); wrapping does @b not (the modular wrap reverses the
 * order at the boundary).
 *
 * @note The structural fact this marker discriminates is @b not
 *       boundedness (every C++ carrier is bounded), but @b cyclicity.
 *       A saturating carrier is bounded but non-cyclic: arithmetic
 *       past the bound escalates to a distinguishable sentinel
 *       (@f$\pm \aleph_0@f$ or @c NaZ for indeterminate forms in the
 *       project's @c SignedCardinality variant), keeping
 *       translation-invariance honest @b including at the boundary.
 *       Modular carriers (@c unsigned @c int, the finite
 *       @c sets::SignedExtensionalCardinal<N>) wrap, reversing order;
 *       they fail the marker by default.
 *
 * @note The project put deliberate effort into the saturating
 *       variants (@c sets::SignedCardinality, @c sets::Cardinality)
 *       precisely so they would be bona-fide proxies for
 *       @f$\mathbb{Z}@f$ / @f$\mathbb{N}@f$ rather than cyclic
 *       (cf.\ @c cardinality.cppm:878-967 --- "@c SignedCardinality
 *       is the signed counterpart of @c Cardinality (the ℕ ∪ ℵ_0
 *       variant): ... saturating ... not periodic ... the library's
 *       bona-fide proxy for ℤ modulo physical limits").  The marker
 *       opt-in below honours that effort: it is specialised for
 *       @c SignedCardinality, @b not for the cyclic finite-fragment
 *       @c SignedExtensionalCardinal<N>.
 */
export template <typename T>
struct is_translation_invariant_ordered : std::false_type {};

/** @brief @c sets::SignedCardinality is the project's bona-fide
 *  saturating proxy for @f$\mathbb{Z}@f$: arithmetic escalates to
 *  @f$\pm \aleph_0@f$ on overflow and propagates @c NaZ on
 *  indeterminate forms, rather than wrapping modulo capacity
 *  (cf.\ @c cardinality.cppm:923-967).  This is the structural
 *  guarantee the marker certifies: translation preserves the partial
 *  order @f$\le@f$ on the finite fragment; saturation collapses
 *  ordering past the boundary into the @f$\pm \aleph_0@f$ sentinel
 *  (the meet still holds: both sides land at the same sentinel).  The
 *  finite-fragment carrier @c SignedExtensionalCardinal<N> is
 *  intentionally @b not opted in: it is cyclic
 *  (mod @f$2^{N \cdot 64}@f$), so its addition would reverse the
 *  order at the wrap boundary.  The variant @c SignedCardinality is
 *  what callers should use whenever they want translation-invariance
 *  at the type level (i.e.\ the halfspace pipe). */
export template <>
struct is_translation_invariant_ordered<dedekind::sets::SignedCardinality>
    : std::true_type {};

export template <typename T>
inline constexpr bool is_translation_invariant_ordered_v =
    is_translation_invariant_ordered<T>::value;

/**
 * @brief The MULTIPLICATIVE order-compatibility axiom marker (O2): the
 *        positive cone is closed under @c *, i.e.\ @c 0≤a ∧ @c 0≤b ⟹
 *        @c 0≤a·b --- equivalently, scaling by a positive preserves the
 *        order.  The multiplicative sibling of
 *        @c is_translation_invariant_ordered (O1, scaling ↔ translation).
 *
 * @details Defaults @c false_type; a carrier opts in only when its order is
 * genuinely compatible with @c * (an @b ordered @b field).  This cannot be a
 * concept check: O2 is a @f$\forall@f$-law over VALUES, and a concept inspects
 * only types / operation signatures --- the same wall that makes
 * @c is_invertible_v / @c is_associative_v opt-in traits, not concept checks.
 *
 * @warning An earlier design leaned on @c order::IsTotallyOrdered<T> to stand
 * in for this axiom ("a separate marker would only re-state what the upstream
 * concepts prove").  That is FALSE: @c std::totally_ordered is mere syntactic
 * comparability and does NOT prove O2.  @f$\mathbb{F}_5@f$ orders its
 * representatives @c 0<1<2<3<4 (so it is @c std::totally_ordered) yet
 * @c 1·3=3 > 2·3=1 flips the order and @c −1=2² is a square --- it is a field
 * that is NOT formally real, and @c Complex<𝔽₅> splits into zero divisors.
 * The structural @c <=> check belongs at this opt-in site (per carrier), not
 * in the concept --- exactly as for the additive marker.
 */
export template <typename T>
struct is_scaling_invariant_ordered : std::false_type {};

export template <typename T>
inline constexpr bool is_scaling_invariant_ordered_v =
    is_scaling_invariant_ordered<T>::value;

/**
 * @concept IsOrderedAdditiveGroup
 * @brief An additive group whose order is translation-invariant.
 *
 * @details
 * Combines the algebraic gate (@c IsAdditiveGroup) with the @b axiom
 * marker (@c is_translation_invariant_ordered_v).  This is the right
 * precondition for halfspace-pivot transport: shifting
 * @f$\{x \mid x > k\}@f$ by @c +c yields @f$\{y \mid y > k+c\}@f$
 * exactly when @c c-translation preserves the order, which is what
 * the marker certifies.
 *
 * @note Earlier drafts also required @c std::totally_ordered<T>.
 *       Dropped because the project's saturating ℤ proxy
 *       (@c sets::SignedCardinality, a @c std::variant) uses
 *       custom comparison operators (specialised on the saturating
 *       semantics and on @c NaZ propagation), which do not satisfy
 *       the @c std::totally_ordered structural concept --- yet the
 *       carrier @b is the right one for translation-invariant
 *       ordered-group semantics.  The marker carries the order claim;
 *       a separate @c std::totally_ordered structural check would
 *       Honest-Reject the very carriers the marker is supposed to
 *       accept.  The structural order check belongs at the marker
 *       opt-in site, not at the concept.
 *
 * Modular carriers (@c unsigned @c int as @f$\mathbb{Z}/2^N\mathbb{Z}@f$)
 * satisfy @c IsAdditiveGroup but NOT this concept --- they fail the
 * marker by default.  Honest Rejection on halfspace-pipe attempts with
 * modular carriers.
 */
export template <typename T>
concept IsOrderedAdditiveGroup =
    dedekind::category::IsAbelianGroup<T, std::plus<T>> &&
    is_translation_invariant_ordered_v<T>;

/**
 * @concept IsOrderedMultiplicativeGroup
 * @brief An ordered field --- the textbook home of multiplicative
 *        scaling of halfspaces.
 *
 * @details
 * A field with a total order compatible with BOTH operations.  Composed,
 * symmetrically with the additive sibling, from the field axioms plus the two
 * order-compatibility axiom markers --- NOT from @c order::IsTotallyOrdered:
 *   * @c category::IsField<T, ...> --- the type-indexed field axioms
 *     on @c T (the multiplicative group on the non-zero cone
 *     @c (T \\ {0}, @c *) is abelian with inverses).
 *   * @c is_translation_invariant_ordered_v<T> --- O1, the additive
 *     compatibility axiom (@c a≤b ⟹ @c a+c≤b+c).
 *   * @c is_scaling_invariant_ordered_v<T> --- O2, the multiplicative
 *     compatibility axiom (@c 0≤a,0≤b ⟹ @c 0≤a·b).
 *
 * O1 ∧ O2 on a field is exactly an ORDERED FIELD, hence (Artin--Schreier)
 * FORMALLY REAL: @c −1 is not a sum of squares.  That is the precondition for
 * halfspace-pivot transport under scaling (@f$\{x \mid x > k\}@f$ scaled by
 * @c k_E>0 yields @f$\{y \mid y > k\cdot k_E\}@f$; @c k_E<0 flips direction),
 * and for @c Complex<T>=T[i]/(x²+1) being a field (@c x²+1 irreducible).
 *
 * @warning Do NOT reintroduce @c order::IsTotallyOrdered here.  It is mere
 * syntactic comparability and does not prove O2: a field can be
 * @c std::totally_ordered by representatives yet have @c −1 a square
 * (@f$\mathbb{F}_5@f$), which this concept must reject.  Compatibility is a
 * value-law, carried by the markers at their per-carrier opt-in sites --- the
 * same design as @c IsOrderedAdditiveGroup.  @c Rational<I>, @c QuadraticReal,
 * @c Real all opt into both markers; @c Complex<R> never opts into @c O2 (no
 * order compatible with complex @c *).
 */
export template <typename T>
concept IsOrderedMultiplicativeGroup =
    dedekind::category::IsField<T, std::plus<T>, std::multiplies<T>> &&
    is_translation_invariant_ordered_v<T> && is_scaling_invariant_ordered_v<T>;

/**
 * @concept IsOrderedCommutativeRing
 * @brief A commutative ring whose carrier is ordered under the
 *        additive translation marker.
 *
 * @details
 * Composes:
 *   * @c category::IsCommutativeRing<T, +, *> --- @c T is an
 *     axiomatic commutative ring.
 *   * @c is_translation_invariant_ordered_v<T> --- @c T's order is
 *     compatible with the additive group (re-use of the additive
 *     marker; in a commutative ring with ordered addition,
 *     positive-scalar multiplication automatically preserves order
 *     and negative-scalar multiplication automatically reverses).
 *
 * @c ℤ (@c SignedCardinality) satisfies this; @c ℚ also does (a field
 * is a fortiori a commutative ring), and the two are distinguished by
 * the stronger @c IsOrderedMultiplicativeGroup gate where only the
 * ring structure is available.
 */
export template <typename T>
concept IsOrderedCommutativeRing =
    dedekind::category::IsCommutativeRing<T, std::plus<T>,
                                          std::multiplies<T>> &&
    is_translation_invariant_ordered_v<T>;

}  // namespace dedekind::algebra
