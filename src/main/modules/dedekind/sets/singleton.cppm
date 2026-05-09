/**
 * @file dedekind/sets/singleton.cppm
 * @partition :singleton
 * @brief The Atomic Body: Implementation of the Singleton Species {x}.
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
 * Category-level η/ε specializations for SingletonSet are currently deferred
 * while dedekind.sets is being retargeted to the updated hub/spoke interfaces
 * in dedekind.category.
 *
 * @section singleton__Structural_Role
 * The SingletonSet provides the baseline proof for the Unified Highway Bridge.
 * Its sets-layer operations (constructor, bind, extend) are available now;
 * category-layer η/ε and derived fmap wiring is intentionally postponed to a
 * follow-up integration pass.
 *
 * @tparam T The underlying Species of the pivot element.
 * @tparam L The Subobject Classifier (Ω) governing the set's logic.
 *           Defaults to ClassicalLogic {True, False}.
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
import :mereology;
import :boundaries;
import :expressions;

/**
 * @section singleton__Mereology
 * @section singleton__Mereology_2
 */
namespace dedekind::sets {

using namespace dedekind::category;

/** @brief {x}: The Atom. Extensional (Size 1). */
export template <typename T, typename L = ClassicalLogic>
struct SingletonSet {
  T pivot;
  using Domain = T;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = Finite;
  using base_set_type = SingletonSet<T, L>;

  /** @section singleton__Algebraic_Axioms */
  template <typename Op>
  static constexpr bool is_associative_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  template <typename Op>
  static constexpr bool is_idempotent_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  // This satisfies IsProperPart and IsSet simultaneously
  constexpr auto operator()(const T& v) const {
    return (v == pivot) ? L::True : L::False;
  }

  constexpr T origin() const { return pivot; }

  /** @section singleton__Extensionality_Proof */
  constexpr std::size_t size() const { return 1; }
  constexpr std::size_t upper_bound() const { return 1; }
  constexpr auto cardinality() const { return Finite{}; }

  /** @section singleton__Mereological_Relation (sqsubseteq) */

  // 2. Manual equality for IsExtensional
  constexpr bool operator==(const SingletonSet& other) const {
    return pivot == other.pivot;
  }

  // 3. DELETE the spaceship to stop the compiler from generating
  // a 'bool'-returning operator<= that breaks the concept.
  auto operator<=>(const SingletonSet&) const = delete;

  // S1 <= S2 (Is S1 a part of S2?)
  template <typename S>
    requires IsSet<S>
  constexpr typename L::Ω operator<=(const S& other) const {
    return (other.contains(pivot)) ? L::True : L::False;
  }

  /** @section singleton__Mereological_Lattice_Audit */

  /** @section singleton__Unified_Lattice_Operations */

  template <typename U, typename L2>
  constexpr auto operator|(const SingletonSet<U, L2>& other) const {
    // Return a structural Join: {x | x == pivot || x == other.pivot}
    return element<Ω<T, L>> |
           [s1 = *this, s2 = other](const T& x) { return s1(x) || s2(x); };
  }

  template <typename U, typename L2>
  constexpr auto operator&(const SingletonSet<U, L2>& other) const {
    // Return a structural Meet: {x | x == pivot && x == other.pivot}
    return element<Ω<T, L>> |
           [s1 = *this, s2 = other](const T& x) { return s1(x) && s2(x); };
  }

  /** @brief Symmetric difference @c {a} @c △ @c {b} (#469).
   *  @details Pointwise XOR: @c x @c ∈ @c {a} @c △ @c {b} iff
   *  @c x @c == @c a @c XOR @c x @c == @c b.  When @c a @c == @c b
   *  the result is empty; otherwise it is the 2-element set
   *  @c {a, @c b}.  Both cases are uniformly expressed by the same
   *  lambda predicate; we do @b not collapse to @c Ø at the type
   *  level even when the singleton TYPES match (@c SingletonSet<T,
   *  @c L> stores its pivot as a runtime value, so type equality
   *  does not imply pivot equality — same trap as
   *  @c BooleanEqPredicate; see @c expressions.cppm:operator^). */
  template <typename U, typename L2>
  constexpr auto operator^(const SingletonSet<U, L2>& other) const {
    // The `element<Ω<T, L>> | lambda` chain produces a Comprehension;
    // wrap in `Set{...}` to materialise an actual Set the caller can
    // invoke.  Without this, callers got `Comprehension does not provide
    // a call operator` errors at the test site.
    return Set{element<Ω<T, L>> | [s1 = *this, s2 = other](const T& x) {
      // SingletonSet::operator() returns L::Ω directly,
      // so the lift_logic<L> calls are defensive: they
      // normalise if L1 or L2 ever returns bool.
      const auto a = dedekind::category::lift_logic<L>(s1(x));
      const auto b = dedekind::category::lift_logic<L>(s2(x));
      return L::OR(L::AND(a, L::NOT(b)), L::AND(L::NOT(a), b));
    }};
  }

  // Complement: !{a}
  // In a strict sense, this is the relative complement (Universe \ {a}).
  // For the sake of the lattice, we return the Universal boundary.
  constexpr auto operator!() const { return UniversalSet<T, L>{}; }
};

// ---------------------------------------------------------------------------
// Singleton ^ Set / Set ^ Singleton — symmetric difference on the Atom
// (#469 review-driven specialisations).
//
// Sound version: produce a lambda-Set whose predicate evaluates the
// pointwise XOR.  When @c S has decidable (ClassicalLogic) membership
// the predicate could be specialised further at construction time:
//   if pivot ∈ S → result = S - {pivot} → predicate s(x) && x != pivot
//   if pivot ∉ S → result = S + {pivot} → predicate s(x) || x == pivot
// That lossy-membership-pivot specialisation is a follow-on micro-
// optimisation; this slice keeps the operator surface complete and
// correct without engineering the predicate-rewrite branch.
// ---------------------------------------------------------------------------

export template <typename T, typename L1, typename L2, typename P>
constexpr auto operator^(const SingletonSet<T, L1>& s,
                         const Set<T, L2, P>& other) {
  // The asymmetry is one-sided: `singleton(v)` always lands in
  // ClassicalLogic, while `Set{x % UniversalSet<T> | …}` ascends through
  // NaturalLogic and routinely arrives as TernaryLogic.  Take the
  // result logic from that same side (L2): the singleton's bool lifts
  // through `lift_logic<L2>` cleanly, and the Set's predicate is
  // already in L2.
  return Set{element<Ω<T, L2>> | [s, other](const T& x) {
    const auto a = dedekind::category::lift_logic<L2>(s(x));
    const auto b = dedekind::category::lift_logic<L2>(other(x));
    return L2::OR(L2::AND(a, L2::NOT(b)), L2::AND(L2::NOT(a), b));
  }};
}

export template <typename T, typename L1, typename L2, typename P>
constexpr auto operator^(const Set<T, L1, P>& other,
                         const SingletonSet<T, L2>& s) {
  return s ^ other;
}

static_assert(IsSet<SingletonSet<int>>, "A singleton must be a set.");
static_assert(IsExtensional<SingletonSet<int>>,
              "Mereology: SingletonSet must satisfy the Singleton axiom.");
static_assert(
    dedekind::category::IsSet<
        decltype(dedekind::category::ambient_set<int>(SingletonSet<int>{0}))>,
    "SingletonSet must lift to an ETCS set object.");
static_assert(dedekind::category::HasCanonicalSetCCC<int>,
              "Breadcrumb to :cartesian: singleton ambient int has canonical "
              "CCC witness.");

/** @section singleton__The_Set_Monad_Realization */

/** @brief η: T -> SingletonSet<T> (The Unit) */
export template <typename T>
constexpr auto singleton(T&& value) {
  return SingletonSet<std::decay_t<T>>{std::forward<T>(value)};
}

/** @section singleton__The_Set_Monad: The Categorical Identity */

/**
 * @section singleton__Singleton_Kleisli_Triple
 * @brief The Bricks of the Singleton Monad.
 */

/** @section singleton__Bind (>>=) */
export template <typename T, typename L, typename Func>
constexpr auto operator>>=(const SingletonSet<T, L>& s, Func&& f) {
  /**
   * @details Kleisli Bind for Singletons:
   * 1. Sample the internal species (The Pull).
   * 2. Apply the Kleisli Arrow f: T -> SingletonSet<U, L>.
   */
  return std::forward<Func>(f)(s.pivot);
}

/** @section singleton__Singleton_CoKleisli_Triple */

/** @section singleton__Extend (<<=) */
export template <typename T, typename L, typename Func>
constexpr auto operator<<=(const SingletonSet<T, L>& s, Func&& f) {
  using U = std::invoke_result_t<Func, SingletonSet<T, L>>;
  // Co-Kleisli Extend: apply contextual logic and re-wrap.
  return SingletonSet<U, L>{std::forward<Func>(f)(s)};
}

/**
 * @section singleton__Image
 * @brief Image of a @c SingletonSet under an @c IsArrow.
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
 *   - The image's tier (@c IsCompileTimeEnumerable, @c IsFiniteSet,
 *     @c HasDecidableMembership): all preserved by the lift, since
 *     @c SingletonSet has cardinality 1 in both source and target.
 *
 * Filed under #602's layer-1 plan: per-shape image dispatch, Singleton
 * source as the entry point.  The same shape generalises to
 * @c ExtensionalSet (sister source post-#598) and predicate sets
 * (lazy / iso-witnessed cases).  Note: the per-wrapper overload shape
 * is itself due for dissolution under #607's Juliet-clean refactor;
 * this slice lands the entry-point breadcrumbs in their current form.
 */
export template <typename L, dedekind::category::IsArrow F>
constexpr auto image(
    F&& f,
    const SingletonSet<dedekind::category::Dom<std::remove_cvref_t<F>>, L>& s) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  return SingletonSet<U, L>{std::forward<F>(f)(s.pivot)};
}

// Breadcrumbs for `image(f, SingletonSet)` live downstream in
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

/** @section singleton__The_Final_Ontology_Proof
 * Deferred while `dedekind.sets` is being retargeted to the updated
 * `dedekind.category` hub/spoke functor API.
 */
