/**
 * @file dedekind/sets/singleton.cppm
 * @partition :singleton
 * @brief The equality atom: the point {x} as a comprehension over 𝔸<T>.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @dependency dedekind.ontology
 *
 * @section singleton__The_Singleton_Atom
 * In the Dedekind universe, the Singleton is the "Unit of Presence."
 * It serves as the canonical implementation of a Pointed Set and provides
 * the concrete singleton constructor used in sets-level monadic workflows.
 *
 * @details
 * This structure bridges Level 0a (Species) and Level 1 (Mereology):
 * - It is Extensional: It exists in memory as a single 'pivot' element.
 * - It is a Monad: It supports the Kleisli Highway (>>=) via direct
 * application.
 * - It is a Comonad: It supports the Co-Kleisli Pull (<<=) via extraction (ε).
 *
 * Category-level η/ε specializations for Singleton are currently deferred
 * while dedekind.sets is being retargeted to the updated hub/spoke interfaces
 * in dedekind.category.
 *
 * @section singleton__Structural_Role
 * The Singleton provides the baseline proof for the Unified Highway Bridge.
 * Its sets-layer operations (constructor, bind, extend) are available now;
 * category-layer η/ε and derived fmap wiring is intentionally postponed to a
 * follow-up integration pass.
 *
 * @tparam T The underlying Species of the pivot element.
 * @tparam L The Subobject Classifier (Ω) governing the set's logic.
 *           Defaults to Boole {True, False}.
 *
 * Wikipedia: Singleton (mathematics), Unit element, Monad (category theory)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "What is clear and easily comprehended attracts, the complicated
 * repels us."
 *       -- David Hilbert, Mathematical Problems (1900)
 */
module;

#include <compare>
#include <concepts>
#include <functional>
#include <type_traits>  // std::remove_cvref_t for the IsArrow Dom/Cod plumbing
#include <utility>      // std::forward for the image() overload

export module dedekind.sets:singleton;

import dedekind.category;

import :cardinality;
import :boundaries;
import :expressions;

/**
 * @section singleton__Mereology
 * @section singleton__Mereology_2
 */
namespace dedekind::sets {

using namespace dedekind::category;

/** @brief The equality atom @c {x : T | x == pivot}, the datum of the pure
 *  equality theory, the one leaf every regular carrier offers (a point in ℂ is
 *  this).  On a chain the cut subsumes it, @c x == p ⟺ x ≥ p ∧ x ≤ p.  The
 *  pivot is a value; the set is @c Finite whatever the species.
 *  @tparam T the carrier (needs @c ==). */
export template <typename T>
struct Point {
  using Domain = T;
  using Codomain = bool;
  using cardinality_type = Finite;
  T pivot{};

  constexpr Point() = default;
  /** @brief Implicit and converting, so a literal of another type is one
   *  user-defined conversion, the carrier's own. */
  template <typename U>
    requires std::convertible_to<U&&, T>
  constexpr Point(U&& v)  // NOLINT(google-explicit-constructor)
      : pivot(static_cast<T>(std::forward<U>(v))) {}

  /** @brief χ(x) = [x == pivot]. */
  constexpr bool operator()(const T& v) const { return v == pivot; }
  /** @brief The foreign carriers this datum admits: any @c U with a cross-type
   *  @c == against @c T.  Read by the comprehension's heterogeneous χ. */
  template <typename U>
  static constexpr bool admits = !std::same_as<std::remove_cvref_t<U>, T> &&
                                 requires(const U& x, const T& v) {
                                   { x == v } -> std::convertible_to<bool>;
                                 };
  /** @brief Membership of a foreign value, compared unnarrowed:
   *  @c Point<int>{1}(1.5) is @c false. */
  template <typename U>
    requires admits<U>
  constexpr bool operator()(const U& x) const {
    return x == pivot;
  }

  constexpr std::size_t size() const { return 1; }
};

/** @brief @c π @c == @c v with a carrier @b value: the point datum, which the
 *  set former binds (@c 𝔹 | (π == true) is @c {true}).  The grammar's tags are
 *  empty types, not values; @c π == fix(c) keeps its binder in @c :order.
 *  @tparam T the carrier, a regular value type. */
export template <std::regular T>
  requires(!std::is_empty_v<T>)
constexpr Point<T> operator==(Projection<0>, T v) {
  return Point<T>{std::move(v)};
}

/** @brief @f$\{x\}@f$ as a set: the equality atom over the universe of @c T.
 *  An alias, not a noun: its operators are every comprehension's (the
 *  reducer's nodes); what is specific to a point follows as free functions.
 *  @tparam T the carrier.
 *  @tparam L the species the membership answer is valued in. */
export template <typename T, IsOckhamAlgebra L = Boole>
using Singleton = Comprehension<𝔸<T, L>, Point<T>>;

/** @brief The pivot of a point, @c ε of the comonad reading (the counit
 *  @c {x} ↦ @c x). */
export template <typename T, IsOckhamAlgebra L, IsCardinality C>
constexpr T origin(const Comprehension<𝔸<T, L, C>, Point<T>>& s) {
  return s.predicate.pivot;
}

/** @brief Two points are the same set iff their pivots agree, whatever the
 *  species each was tagged with. */
export template <typename T, IsOckhamAlgebra L1, IsCardinality C1,
                 IsOckhamAlgebra L2, IsCardinality C2>
constexpr bool operator==(const Comprehension<𝔸<T, L1, C1>, Point<T>>& a,
                          const Comprehension<𝔸<T, L2, C2>, Point<T>>& b) {
  return a.predicate.pivot == b.predicate.pivot;
}

/** @brief @c {pivot} ⊆ S ⟺ pivot ∈ S, in the shared species @c L (the
 *  universal set keeps its own @c X ⊆ 𝔸 overload). */
export template <typename T, IsOckhamAlgebra L, IsCardinality C, IsLSet S>
  requires std::same_as<typename S::logic_species, L> &&
           (!requires { typename S::is_universal_boundary; }) &&
           (!std::same_as<S, Comprehension<𝔸<T, L, C>, Point<T>>>)
constexpr typename L::Ω operator<=(const Comprehension<𝔸<T, L, C>, Point<T>>& s,
                                   const S& other) {
  return other(s.predicate.pivot);
}

/** @brief A point is the whole universe only on the unit carrier: @c {x} ==
 * 𝔸<T> iff @c T has exactly one value (@c category::One).  On every other
 * carrier it is @c false --- the @c forall leg for the @c == fragment
 *  (@c 𝔸<bool>{} | (π == fix(v)) collapses to a @c Singleton<bool>, and
 *  @c forall asks whether that point is all of 𝔹).  Lives here, not in
 *  @c :order, so ADL finds it from @c sets-level generic code. */
export template <typename T, IsOckhamAlgebra L, IsCardinality C1,
                 IsOckhamAlgebra L2, IsCardinality C>
constexpr bool operator==(const Comprehension<𝔸<T, L, C1>, Point<T>>&,
                          const 𝔸<T, L2, C>&) {
  return std::same_as<T, dedekind::category::One>;
}
export template <typename T, IsOckhamAlgebra L, IsCardinality C1,
                 IsOckhamAlgebra L2, IsCardinality C>
constexpr bool operator==(const 𝔸<T, L2, C>& u,
                          const Comprehension<𝔸<T, L, C1>, Point<T>>& s) {
  return s == u;
}

/** @brief Complement of a point on the @b two-element carrier is the other
 *  point: @c ~{b} @c = @c {!b}.  On a larger carrier the complement of a point
 *  is not a point, so there is deliberately no overload there and the generic
 *  @c Not node applies.  Lives next to @c Singleton so ADL finds it wherever
 *  the type is used. */
export template <IsOckhamAlgebra L, IsCardinality C>
constexpr auto operator~(const Comprehension<𝔸<bool, L, C>, Point<bool>>& s) {
  return 𝔸<bool, L>{} | Point<bool>{!s.predicate.pivot};
}

/** @brief Product of two points: @f$\{a\}\times\{b\}=\{(a,b)\}@f$, the
 *  point of the pair.  A structural collapse (the product-side analogue of
 *  the complement-pair collapse), so a product of points is
 *  @b equality-comparable, not merely membership-testable:
 *  @c η(a)*η(b) @c == @c η(std::pair{a,b}).  General products keep the
 *  predicate-set form of @c :expressions cartesian_product. */
export template <typename T1, IsOckhamAlgebra L1, IsCardinality C1, typename T2,
                 IsOckhamAlgebra L2, IsCardinality C2>
constexpr auto operator*(const Comprehension<𝔸<T1, L1, C1>, Point<T1>>& a,
                         const Comprehension<𝔸<T2, L2, C2>, Point<T2>>& b) {
  return Singleton<std::pair<T1, T2>, L1>{
      std::pair{a.predicate.pivot, b.predicate.pivot}};
}

static_assert(IsSet<Singleton<int>>, "A singleton must be a set.");

static_assert(IsExtensional<Singleton<int>>,
              "Mereology: Singleton must satisfy the Singleton axiom.");
static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<int>(Singleton<int>{0}))>,
    "Singleton must lift to an ETCS set object.");

/** @section singleton__The_Set_Monad_Realization
 *  The singleton @f$x \mapsto \{x\}@f$ is the @b unit of the power-set @b
 * monad. It is the power-set instance of the categorical unit machinery in
 *  @c dedekind.category: the hub-dispatched @c η / @c pure and its siblings
 *  @c μ / @c ε / @c δ (@c :monad, laws checked by @c IsMonad; @c η / @c ε live
 * in
 *  @c :natural), whose bona-fide monad-and-comonad (@c IsFrobenius, #632)
 * carrier is @c std::tuple (@c :kleisli).  The power-set monad's own hub (@c η
 * + the union-flatten @c μ) is tracked in #691.  @b Distinct from the two other
 *  reifications of the power object: @c dedekind.order:powerset (#830) is the
 *  @c Sub(C) subobject @b lattice @c 𝔓 (a @c Set), and
 *  the @b enumeration (the characteristic relation @f$\chi : S \to 2@f$ over a
 *  countable carrier) is #840. */

/** @brief @c singleton: @f$T \to \mathrm{Singleton}\langle T\rangle@f$ ---
 *  the power-set monad's unit @f$\eta@f$ (see the section note). */
export template <IsOckhamAlgebra L = Boole, typename T>
constexpr auto singleton(T&& value) {
  return 𝔸<std::decay_t<T>, L>{} |
         Point<std::decay_t<T>>{std::forward<T>(value)};
}

/** @brief @c η --- the idiomatic spelling of @c singleton: the power-set
 *  monad's unit @f$\eta : x \mapsto \{x\}@f$.  The name matches the categorical
 *  unit @c dedekind::category::η (the hub-dispatched @c η / @c pure of
 *  @c :monad, §above) and the grammar's @c η generator (paper Listing~2).
 *  @f$\eta(x)@f$ is the least set containing @c x, so
 *  @f$\eta(x) \in \mathfrak{P}(S) \iff x \in S@f$.  The monad's @c μ
 *  (union-flatten) and full hub are #691; the @b enumerated power set is
 *  #840; the @c Sub(C) subobject @b lattice @c 𝔓 is
 *  @c order:powerset (#830) --- three distinct reifications of the power
 * object, all sharing this unit. */
export template <typename T>
constexpr auto η(T&& value) {
  return singleton(std::forward<T>(value));
}

/** @brief @c ι --- the singleton read as an @b inclusion (a @b mono), the other
 *  categorical hat of the same map @c η / @c singleton names.  Where @c η is
 * the power-set monad's @b unit, @c ι is the injection of each point as its
 *  singleton subobject, @f$\iota : T \rightarrowtail \mathcal{P}(T)@f$,
 *  @f$x \mapsto \{x\}@f$ --- the @b atoms of the power-set Boolean algebra, and
 *  injective (mathematically a mono).  It follows the @c iota-for-inclusion
 *  convention it shares with the coproduct injections @c ι_1 / @c ι_2
 *  (@c :cartesian) and the Galois inclusion @c ι of the ceiling/floor
 *  adjunctions @c ⌈·⌉ ⊣ ι ⊣ ⌊·⌋ (@c :adjunction).  Same underlying map as
 *  @c η; the name picks out the @b subobject-inclusion reading.
 *  @note This is mathematical motivation, not a concept claim: @c ι(x) returns
 * a
 *  point set (its callable is the membership classifier @f$T \to
 * \Omega@f$),
 *  @b not a reified arrow carrying an @c IsMonicArrow monicity witness
 *  (@c :morphism).  Reifying/certifying the unit arrow is a separate step. */
export template <typename T>
constexpr auto ι(T&& value) {
  return singleton(std::forward<T>(value));
}

// @c η is exactly @c singleton (the unit), only more idiomatic at call sites.
static_assert(std::same_as<decltype(η(0)), decltype(singleton(0))>,
              "η is the singleton unit alias.");

/** @section singleton__The_Set_Monad: The Categorical Identity */

/**
 * @section singleton__Singleton_Kleisli_Triple
 * @brief The Bricks of the Singleton Monad.
 */

/** @section singleton__Bind (>>=) */
export template <typename T, IsOckhamAlgebra L, IsCardinality C, typename Func>
constexpr auto operator>>=(const Comprehension<𝔸<T, L, C>, Point<T>>& s,
                           Func&& f) {
  /**
   * @details Kleisli Bind for points:
   * 1. Sample the pivot (The Pull).
   * 2. Apply the Kleisli Arrow f: T -> Singleton<U, L>.
   */
  return std::forward<Func>(f)(s.predicate.pivot);
}

/** @section singleton__Singleton_CoKleisli_Triple */

/** @section singleton__Extend (<<=) */
export template <typename T, IsOckhamAlgebra L, IsCardinality C, typename Func>
constexpr auto operator<<=(const Comprehension<𝔸<T, L, C>, Point<T>>& s,
                           Func&& f) {
  using U = std::invoke_result_t<Func, Comprehension<𝔸<T, L, C>, Point<T>>>;
  // Co-Kleisli Extend: apply contextual logic and re-wrap.
  return 𝔸<U, L>{} | Point<U>{std::forward<Func>(f)(s)};
}

/**
 * @section singleton__Image
 * @brief Image of a @c Singleton under an @c IsArrow.
 *
 * @details For an arrow @c f @c : @c T @c → @c U and a singleton
 * @c {x} @c ⊂ @c T, the categorical image is @c f({x}) @c = @c {f(x)}
 * @c ⊂ @c U.  This is the cardinality-1 instance of the powerset-monad
 * Kleisli bind: equivalent to @c s @c >>= @c (η @c ∘ @c f), where @c η
 * wraps a value in a singleton (the Singleton-monad unit).
 *
 * @section singleton__Image_Categorical_Anchor
 * Type-level breadcrumbs (placed downstream in
 * @c morphologies:archimedean rather than below in this partition; see
 * the closing note for why) tie the image construction back to:
 *   - @c IsArrow: the source-side requirement.  @c f's @c Domain must
 *     match the singleton's @c pivot type.
 *   - The Kleisli triple's @c >>= (above): @c image(f, @c s) @c is the
 *     Singleton specialisation of the Set-monad's bind, factored
 *     through @c η.
 *   - The image's tier ( @c IsExtensional, @c HasDecidableMembership):
 *     preserved by the lift, since @c Singleton has cardinality 1
 *     in both source and target.
 *
 * Filed under #602's layer-1 plan: per-shape image dispatch, Singleton
 * source as the entry point.  The same shape generalises to
 * @c ExtensionalSet (sister source post-#598) and predicate sets
 * (lazy / iso-witnessed cases).  Note: the per-wrapper overload shape
 * is itself due for dissolution under #607's Juliet-clean refactor;
 * this slice lands the entry-point breadcrumbs in their current form.
 */
export template <IsOckhamAlgebra L, IsCardinality C, IsArrow F>
constexpr auto image(
    F&& f, const Comprehension<𝔸<Dom<std::remove_cvref_t<F>>, L, C>,
                               Point<Dom<std::remove_cvref_t<F>>>>& s) {
  using U = Cod<std::remove_cvref_t<F>>;
  return 𝔸<U, L>{} | Point<U>{std::forward<F>(f)(s.predicate.pivot)};
}

/** @section singleton__Image_Terminal_Morphism (#661)
 *  @brief Terminal-codomain-arrow collapse for @c image(F, S): when
 *         @c F is an @c IsTerminalMorphism (@c F: @c T @c → @c One),
 *         the image is structurally degenerate.
 *  @details
 *  - @c image(F, @c Ø<T, @c L>) → @c Ø<One, @c L> — empty source maps
 *    to empty image.
 *  - @c image(F, @c 𝔸<T, @c L, @c C>) →
 *    @c Singleton<One, @c L>{One{}} — inhabited source collapses
 *    to the singleton on @c One.
 *  - @c image(F, a point over @c T) falls through to the generic
 *    @c image(F, point) above (correct: the point @c {F(pivot)} =
 *    @c {One{}}).
 *
 *  Predicate-based @c Comprehension sources fall through to the
 *  symbolic-fallback @c image() in @c :expressions (inhabitation
 *  undecidable in general). */
export template <IsOckhamAlgebra L, typename T, IsCardinality C, typename F>
  requires IsTerminalMorphism<std::remove_cvref_t<F>> &&
           std::same_as<Dom<std::remove_cvref_t<F>>, T>
constexpr auto image(F&&, const 𝔸<T, L, C>&) {
  return singleton<L>(One{});
}

export template <typename L, typename T, typename F>
  requires dedekind::category::IsTerminalMorphism<std::remove_cvref_t<F>> &&
           std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>, T>
constexpr auto image(F&&, const Ø<T, L>&) {
  return Ø<dedekind::category::One, L>{};
}

// Breadcrumbs for `image(f, Singleton)` live downstream in
// `morphologies:archimedean` (the natural home for Peano-successor
// witnesses).  The structural claims pinned there:
//   (i)   `image` is defined for @c IsArrow inputs.
//   (ii)  Tier preservation: cardinality 1 ↦ 1, @c IsExtensional preserved on
//   the codomain side. (iii) Kleisli factoring: @c image(f, s) == @c (s @c >>=
//   @c (η @c ∘ @c f))
//         — the cardinality-1 instance of the powerset-monad bind.
// Placing the witness downstream lets us avoid contaminating
// @c :singleton with its own assertion machinery (per PR #604 review).

};  // namespace dedekind::sets
