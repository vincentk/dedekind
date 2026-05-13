/**
 * @file dedekind/algebra/initial_ring.cppm
 * @partition :initial_ring
 * @brief @c ℤ as a Form — Initial Ring and Grothendieck group of @c ℕ.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section initial_ring__Two_Universal_Properties_For_ℤ
 * The categorical identity of @c ℤ has two textbook universal-
 * property readings, both load-bearing for the project:
 *
 *   1. @b Initial @b object @b in @b Ring.  For every (commutative,
 *      unital) ring @c R there exists a unique ring homomorphism
 *      @c χ_R @c : @c ℤ @c → @c R, sending @c 1_ℤ @c ↦ @c 1_R and
 *      determined uniquely by ring-axiom preservation.  Reified as
 *      @c IsInitialRing<R>.
 *
 *   2. @b Grothendieck @b group @b of @b ℕ.  @c ℤ is the free abelian
 *      group on the commutative monoid @c (ℕ, @c +, @c 0) — the left
 *      adjoint of the forgetful functor @c Ab @c → @c CMon applied
 *      at @c ℕ.  Universal property: for every abelian group @c G and
 *      monoid homomorphism @c φ @c : @c ℕ @c → @c G, there exists a
 *      unique group homomorphism @c φ̂ @c : @c ℤ @c → @c G extending
 *      @c φ.  Reified as @c IsGrothendieckGroup<G, M>.
 *
 * @section initial_ring__Why_Reify_Both
 * The two readings are mathematically equivalent up to canonical
 * isomorphism, but they sit in @b different categories ( @c Ring vs
 * @c Ab) and bind to different operational consumers in the project:
 *
 *   * The initial-object reading is what justifies @c χ_{Modular<n>}
 *     as a unique ring homomorphism (the mod-@c n reduction).
 *   * The Grothendieck reading is what the closure-forcing operator
 *     @c Cardinality @c - @c Cardinality @c → @c SignedCardinality
 *     (PR #433) realises operationally.
 *
 * Reifying both as separate concepts gives the practitioner @b
 * formal, @b type-checked @b documentation that @c SignedCardinality
 * inhabits both Forms; downstream generic code can bind to whichever
 * universal property is relevant for its argument.
 *
 * @section initial_ring__Honesty_Obligation
 * Existence and uniqueness of the canonical homomorphisms remain the
 * engineer's honesty obligation.  C++ concepts cannot quantify
 * universally over the target ring (in @c IsInitialRing) or the
 * target abelian group (in @c IsGrothendieckGroup).  What the type
 * system pins is the @b structural shape — the carrier is a ring /
 * abelian group of the appropriate kind, plus the opt-in trait that
 * names it as the universal one.  Witnesses on @c SignedCardinality
 * exercise the universal property at one concrete target each
 * (@c Modular<n> for the initial-ring side; @c Cardinality for the
 * Grothendieck side) — the static_asserts pin the shape, the test
 * suite pins the operational behaviour.
 *
 * @section initial_ring__Architecture
 * Three-layer chain mirroring the NNO story (PR #447):
 *
 *   @c IsInitialRing  →  @c SignedCardinality  →  @c ℤ
 *   @c IsGrothendieckGroup<·, Cardinality>  →  @c SignedCardinality  →  @c ℤ
 *
 * The first chain anchors the ring-side universal property; the
 * second anchors the abelian-group-side universal property.  Both
 * land on the same carrier.
 *
 * @section initial_ring__Bridge_to_Limit
 * @c IsInitialRing instantiates the @b same universal-property
 * pattern that @c :limit's @c IsInitialObject<Zero> instantiates,
 * just at a different categorical level — initial in the @c Ring
 * category rather than initial in @c Set.  The two readings live at
 * different levels (forgetful functor @c U @c : @c Ring @c → @c Set
 * does @b not preserve initial: @c ℤ is initial in @c Ring but its
 * underlying set is not @c Zero @c = @c std::nullptr_t).  A
 * non-vacuous structural witness of this bridge requires reifying
 * @c IsTerminalRing for the trivial / zero ring (whose carrier is
 * @c One @c = @c std::monostate from @c :limit) — filed as a sibling
 * concern; the static_assert here would be premature without the
 * dual concept.  See @c :f_algebra for the analogous bridge at the
 * F-Alg category level.
 *
 * @note "Adjoint functors arise everywhere."
 *       — Saunders Mac Lane.
 */
module;

#include <functional>
#include <type_traits>

export module dedekind.algebra:initial_ring;

import dedekind.category;
import dedekind.sets;
import :group;
import :ring;

namespace dedekind::algebra {
using namespace dedekind::sets;

// ===========================================================================
// (1) Initial Ring — ℤ as the initial object in Ring.
// ===========================================================================

/**
 * @brief Opt-in trait: @c R is the initial object in the category
 *        of (commutative, unital) rings.
 *
 * @details Default: @c false.  Specialised to @c true on the
 * canonical exact-@c ℤ carrier ( @c SignedCardinality, registered
 * downstream where the carrier type is visible).
 *
 * Universal-property content (existence + uniqueness of the
 * canonical ring homomorphism @c χ_R @c : @c R @c → @c S for every
 * ring @c S) is the engineer's honesty obligation.  This trait is
 * the type-system anchor: a carrier opts in, and downstream concept
 * checks fire.
 */
export template <typename R>
inline constexpr bool is_initial_ring_v = false;

/**
 * @concept IsInitialRing
 * @brief @c R is the initial object in @c Ring — for every ring
 *        @c S there exists a unique ring homomorphism @c R @c → @c S.
 *
 * @details Composes:
 *   * The structural ring claim ( @c IsRing<R, std::plus<R>,
 *     std::multiplies<R>>): @c R must actually @b be a ring.
 *   * The opt-in trait @c is_initial_ring_v<R>: the engineer's
 *     claim that this specific ring is the initial one.
 *
 * Universal-property content is the engineer's honesty obligation.
 *
 * @tparam R The carrier type claimed to be the initial ring.
 */
export template <typename R>
concept IsInitialRing =
    dedekind::category::IsCommutativeRing<R, std::plus<R>,
                                          std::multiplies<R>> &&
    is_initial_ring_v<R>;

// ===========================================================================
// (2) Grothendieck Group — ℤ as the free abelian group on ℕ.
// ===========================================================================

/**
 * @brief Opt-in trait: @c G is the Grothendieck group of the
 *        commutative monoid @c M.
 *
 * @details Default: @c false.  Specialised to @c true on the
 * canonical pair @c (SignedCardinality, Cardinality), registered
 * downstream where both carrier types are visible.
 *
 * Universal-property content (existence + uniqueness of the
 * extension @c φ̂ @c : @c G @c → @c A for every abelian group @c A
 * and monoid homomorphism @c φ @c : @c M @c → @c A) is the
 * engineer's honesty obligation.
 */
export template <typename G, typename M>
inline constexpr bool is_grothendieck_group_v = false;

/**
 * @concept IsGrothendieckGroup
 * @brief @c G is the Grothendieck group of the commutative monoid
 *        @c M — the free abelian group on @c M, equivalently the
 *        left adjoint of the forgetful functor @c Ab @c → @c CMon
 *        applied at @c M.
 *
 * @details Composes:
 *   * The structural abelian-group claim ( @c IsAbelianGroup<G,
 *     std::plus<G>>): @c G must actually @b be an abelian group.
 *   * The structural commutative-monoid claim ( @c
 *     IsCommutativeMonoid<M, std::plus<M>>): @c M must actually @b be
 *     a commutative monoid.
 *   * The opt-in trait @c is_grothendieck_group_v<G, M>: the
 *     engineer's claim that this specific pair forms a Grothendieck-
 *     completion adjunction at the monoid @c M.
 *
 * Universal-property content (existence + uniqueness of the
 * extension @c φ̂) is the engineer's honesty obligation.  Operational
 * realisation: the closure-forcing operator @c M @c - @c M @c →
 * @c G implements the Grothendieck construction at the operator
 * level; the project's @c Cardinality @c - @c Cardinality @c →
 * @c SignedCardinality is the canonical instance.
 *
 * @tparam G The carrier type claimed to be the Grothendieck group.
 * @tparam M The commutative monoid being completed.
 */
export template <typename G, typename M>
concept IsGrothendieckGroup =
    dedekind::category::IsAbelianGroup<G, std::plus<G>> &&
    dedekind::category::IsCommutativeMonoid<M, std::plus<M>> &&
    is_grothendieck_group_v<G, M>;

template <>
inline constexpr bool is_initial_ring_v<SignedCardinality> = true;

template <>
inline constexpr bool is_grothendieck_group_v<SignedCardinality, Cardinality> =
    true;

static_assert(IsInitialRing<SignedCardinality>);
static_assert(IsGrothendieckGroup<SignedCardinality, Cardinality>);

}  // namespace dedekind::algebra
