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

/** @brief @f$\{x\}@f$: the atom, the point @c {x : T | x == pivot}.  The pivot
 *  rides as a VALUE, so one type @c Singleton<T> covers every point and a
 *  @c constexpr instance still folds at compile time (@c {n | 3<n<5} is the
 *  constant @c Singleton<int>{4}).  Extensional of size 1, hence decidable
 *  whatever the ambient logic: @c cardinality_type is @c Finite.
 *  @tparam T the carrier (needs @c ==).
 *  @tparam L the logic species the membership answer is valued in. */
export template <typename T, typename L = Boole>
struct Singleton : SetExpr<Singleton<T, L>, T, L> {
  using cardinality_type = Finite;
  using base_set_type = Singleton<T, L>;
  using is_static_singleton_tag =
      void;  // read by elevate_meet: a point stays bare

  T pivot{};

  constexpr Singleton() = default;
  constexpr explicit Singleton(T v) : pivot(v) {}

  /** @section singleton__Algebraic_Axioms */
  template <typename Op>
  static constexpr bool is_associative_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;
  template <typename Op>
  static constexpr bool is_idempotent_v =
      std::is_same_v<Op, std::bit_and<base_set_type>> ||
      std::is_same_v<Op, std::bit_or<base_set_type>>;

  /** @brief χ(x) = [x == pivot], valued in @c L::Ω. */
  constexpr typename L::Ω operator()(const T& v) const {
    return (v == pivot) ? L::True : L::False;
  }
  /** @brief Heterogeneous membership: any value of another type @c U with a
   *  cross-type @c == against @c T (the variant proxies @c Cardinality /
   *  @c SignedCardinality against an @c int point, #423/#425; a @c double
   *  against an @c int point).  The comparison happens in the pair's common
   *  type, never by narrowing @c x to @c T: @c Singleton<int>{1}(1.5) is
   *  @c False. */
  template <typename U>
    requires(!std::same_as<std::remove_cvref_t<U>, T>) &&
            requires(const U& x, const T& v) {
              { x == v } -> std::convertible_to<bool>;
            }
  constexpr typename L::Ω operator()(const U& x) const {
    return (x == pivot) ? L::True : L::False;
  }

  constexpr T origin() const { return pivot; }
  /** @section singleton__Extensionality_Proof */
  constexpr std::size_t size() const { return 1; }
  constexpr std::size_t upper_bound() const { return 1; }
  constexpr auto cardinality() const { return Finite{}; }

  /** @brief Two points are the same set iff their pivots agree, whatever the
   *  logic species each was tagged with. */
  template <typename L2>
  constexpr bool operator==(const Singleton<T, L2>& other) const {
    return pivot == other.pivot;
  }
  auto operator<=>(const Singleton&) const = delete;

  /** @brief @c {pivot} ⊆ S ⟺ pivot ∈ S, in the shared species @c L (the
   *  universal set keeps its own @c X ⊆ 𝔸 overload). */
  template <typename S>
    requires IsLSet<S> && std::same_as<typename S::logic_species, L> &&
             (!requires { typename S::is_universal_boundary; })
  constexpr typename L::Ω operator<=(const S& other) const {
    return other(pivot);
  }

  /** @brief Union of two atoms @f$\{a\}\cup\{b\}@f$ as the recoverable
   *  reducer @c category::Join node: both pivots survive structurally
   *  (reachable as @c .predicate().lhs / @c .rhs), the prerequisite for the
   *  power-set monad's union-flatten @c μ (#691). */
  template <typename U, typename L2>
    requires std::same_as<L2, L>
  constexpr auto operator|(const Singleton<U, L2>& other) const {
    using Or = dedekind::category::Join<Singleton<T, L>, Singleton<U, L2>>;
    return Comprehension<𝔸<T, L>, Or>{Or{*this, other}};
  }
  /** @brief Singleton-bounded meet, same species only (a cross-species meet
   *  routes through the lifting overload in @c :expressions, #894). */
  template <typename U, typename L2>
    requires std::same_as<L2, L>
  constexpr auto operator&(const Singleton<U, L2>& other) const& {
    return Comprehension{*this, [s2 = other](const T& x) { return s2(x); }};
  }
  // FIXME(#992): de-lambda to a @c category::Meet node the way @c | was.
  template <typename U, typename L2>
    requires std::same_as<L2, L>
  constexpr auto operator&(const Singleton<U, L2>& other) const&& {
    const auto meet_pred = [s1 = *this, s2 = other](const T& x) {
      return s1(x) && s2(x);
    };
    return Comprehension{𝔸<T, L>{}, meet_pred};
  }
  /** @brief Symmetric difference @c {a} @c △ @c {b}: empty when @c a == b,
   *  else the two-element set, decided pointwise (the pivots are values, so
   *  equal TYPES do not mean equal sets). */
  template <typename U, typename L2>
  constexpr auto operator^(const Singleton<U, L2>& other) const {
    const auto xor_pred = [s1 = *this, s2 = other](const T& x) {
      const auto a = dedekind::category::lift_logic<L>(s1(x));
      const auto b = dedekind::category::lift_logic<L>(s2(x));
      return L::OR(L::AND(a, L::RFL(b)), L::AND(L::RFL(a), b));
    };
    return Comprehension{𝔸<T, L>{}, xor_pred};
  }
};
/** @brief CTAD: @c Singleton{4} deduces @c Singleton<int>. */
export template <typename T>
Singleton(T) -> Singleton<T>;

/** @brief A point is the whole universe only on the unit carrier: @c {x} ==
 * 𝔸<T> iff @c T has exactly one value (@c category::One).  On every other
 * carrier it is @c false --- the @c forall leg for the @c == fragment
 *  (@c 𝔸<bool>{} | (π == fix(v)) collapses to a @c Singleton<bool>, and
 *  @c forall asks whether that point is all of 𝔹).  Lives here, not in
 *  @c :order, so ADL finds it from @c sets-level generic code. */
export template <typename T, typename L, typename L2, typename C>
constexpr bool operator==(const Singleton<T, L>&, const 𝔸<T, L2, C>&) {
  return std::same_as<T, dedekind::category::One>;
}
export template <typename T, typename L, typename L2, typename C>
constexpr bool operator==(const 𝔸<T, L2, C>& u, const Singleton<T, L>& s) {
  return s == u;
}

/** @brief Complement of a point on the @b two-element carrier is the other
 *  point: @c ~{b} @c = @c {!b}.  On a larger carrier the complement of a point
 *  is not a point, so there is deliberately no overload there and the generic
 *  @c Not node applies.  Lives next to @c Singleton so ADL finds it wherever
 *  the type is used. */
export template <typename L>
constexpr auto operator~(const Singleton<bool, L>& s) {
  return Singleton<bool, L>{!s.pivot};
}

// ---------------------------------------------------------------------------
// Singleton ^ Set / Set ^ Singleton — symmetric difference on the Atom
// (#469 review-driven specialisations).
//
// Sound version: produce a lambda-Set whose predicate evaluates the
// pointwise XOR.  When @c S has decidable (Boole) membership
// the predicate could be specialised further at construction time:
//   if pivot ∈ S → result = S - {pivot} → predicate s(x) && x != pivot
//   if pivot ∉ S → result = S + {pivot} → predicate s(x) || x == pivot
// That lossy-membership-pivot specialisation is a follow-on micro-
// optimisation; this slice keeps the operator surface complete and
// correct without engineering the predicate-rewrite branch.
// ---------------------------------------------------------------------------

/** @brief Product of two singletons: @f$\{a\}\times\{b\}=\{(a,b)\}@f$,
 * collapsed to the @c Singleton of the pair.  A structural collapse (the
 * product-side analogue of the complement-pair collapse), so a singleton
 * product is
 *  @b equality-comparable, not merely membership-testable:
 *  @c η(a)*η(b) @c == @c η(std::pair{a,b}).  General (non-singleton) products
 *  keep the predicate-set form of @c :expressions cartesian_product. */
export template <typename T1, typename L1, typename T2, typename L2>
constexpr auto operator*(const Singleton<T1, L1>& a,
                         const Singleton<T2, L2>& b) {
  return Singleton<std::pair<T1, T2>, L1>{std::pair{a.pivot, b.pivot}};
}

/** @brief @c {a} @c △ @c S for a comprehension @c S: both answers are lifted
 *  into the @b join of the singleton's species and the comprehension's @b own
 *  species (which may sit above its base's tag @c L2), and the symmetric
 *  difference is computed there. */
export template <typename T, typename L1, typename L2, typename P, typename C>
  requires dedekind::category::HaveLogicJoin<
      L1, typename Comprehension<𝔸<T, L2, C>, P>::logic_species>
constexpr auto operator^(const Singleton<T, L1>& s,
                         const Comprehension<𝔸<T, L2, C>, P>& other) {
  using L = dedekind::category::join_logic_t<
      L1, typename Comprehension<𝔸<T, L2, C>, P>::logic_species>;
  const auto xor_pred = [s, other](const T& x) {
    const auto a = dedekind::category::lift_logic<L>(s(x));
    const auto b = dedekind::category::lift_logic<L>(other(x));
    return L::OR(L::AND(a, L::RFL(b)), L::AND(L::RFL(a), b));
  };
  return Comprehension{𝔸<T, L>{}, xor_pred};
}

export template <typename T, typename L1, typename L2, typename P, typename C>
  requires dedekind::category::HaveLogicJoin<
      L2, typename Comprehension<𝔸<T, L1, C>, P>::logic_species>
constexpr auto operator^(const Comprehension<𝔸<T, L1, C>, P>& other,
                         const Singleton<T, L2>& s) {
  return s ^ other;
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
export template <typename T>
constexpr auto singleton(T&& value) {
  return Singleton<std::decay_t<T>>{std::forward<T>(value)};
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
 *  @c Singleton (its callable is the membership classifier @f$T \to
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
export template <typename T, typename L, typename Func>
constexpr auto operator>>=(const Singleton<T, L>& s, Func&& f) {
  /**
   * @details Kleisli Bind for Singletons:
   * 1. Sample the internal species (The Pull).
   * 2. Apply the Kleisli Arrow f: T -> Singleton<U, L>.
   */
  return std::forward<Func>(f)(s.pivot);
}

/** @section singleton__Singleton_CoKleisli_Triple */

/** @section singleton__Extend (<<=) */
export template <typename T, typename L, typename Func>
constexpr auto operator<<=(const Singleton<T, L>& s, Func&& f) {
  using U = std::invoke_result_t<Func, Singleton<T, L>>;
  // Co-Kleisli Extend: apply contextual logic and re-wrap.
  return Singleton<U, L>{std::forward<Func>(f)(s)};
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
export template <typename L, dedekind::category::IsArrow F>
constexpr auto image(
    F&& f,
    const Singleton<dedekind::category::Dom<std::remove_cvref_t<F>>, L>& s) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  return Singleton<U, L>{std::forward<F>(f)(s.pivot)};
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
 *  - @c image(F, @c Singleton<T, @c L>) falls through to the
 *    generic @c image(F, @c Singleton) above (correct: returns
 *    @c Singleton<One, L>{F(pivot)} = @c Singleton<One, L>{One{}}).
 *
 *  Predicate-based @c Comprehension sources fall through to the
 *  symbolic-fallback @c image() in @c :expressions (inhabitation
 *  undecidable in general). */
export template <typename L, typename T, typename C, typename F>
  requires dedekind::category::IsTerminalMorphism<std::remove_cvref_t<F>> &&
           std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>, T>
constexpr auto image(F&&, const 𝔸<T, L, C>&) {
  return Singleton<dedekind::category::One, L>{dedekind::category::One{}};
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
