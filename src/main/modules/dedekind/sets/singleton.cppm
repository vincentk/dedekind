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

/** @brief Structural complement wrapper.
 *  @details Holds @c S const* (nullable so the @c static @c χ shape
 *           witness can default-construct) and exposes the complement
 *           set surface.  Free-function @c operator! on a populated
 *           @c Complement returns @c *source, giving structural
 *           involution: @c !!s @b is @c s as the same object. */
export template <typename S>
struct Complement {
  using Domain = typename S::Domain;
  using Codomain = typename S::Codomain;
  using logic_species = typename S::logic_species;
  using cardinality_type = ℵ_0;

  /** @brief Member-shape mirror for the @c IsSubobject contract. */
  struct Member {
    Domain value;
  };

  S const* source = nullptr;

  /** @brief Static shape witness (nullptr source — pure SHAPE for the
   *         @c IsSubobject / @c IsSet concept checks; the @c source
   *         pointer is intentionally null on this witness because the
   *         concept machinery only reads types, never invokes
   *         @c operator()).  Runtime queries against the static @c χ
   *         return @c L::False defensively (an empty-witness predicate
   *         is vacuously false). */
  static const Complement χ;

  constexpr Domain ι(const Member& m) const { return m.value; }

  /** @brief Membership: @c v @c ∈ @c !S iff @c v @c ∉ @c S.
   *  @details Null-source path returns @c L::False (the static @c χ
   *           witness uses nullptr — see above). */
  constexpr auto operator()(const Domain& v) const {
    return source ? !((*source)(v)) : Codomain{logic_species::False};
  }

  constexpr cardinality_type cardinality() const { return {}; }
};

template <typename S>
inline const Complement<S> Complement<S>::χ{};

/** @brief Involution: @c !Complement<S> @b is @c *source (same object).
 *  @details Three overloads (const&/&/&&) shadow the generic
 *           @c IsPredicate-based @c operator! in @c :category:topoi
 *           for any cv/ref qualification on the @c Complement. */
export template <typename S>
constexpr S const& operator!(const Complement<S>& c) {
  return *c.source;
}

export template <typename S>
constexpr S const& operator!(Complement<S>& c) {
  return *c.source;
}

export template <typename S>
constexpr S const& operator!(Complement<S>&& c) {
  return *c.source;
}

/** @brief {x}: The Atom. Extensional (Size 1). */
export template <typename T, typename L = ClassicalLogic>
struct SingletonSet {
  T pivot;
  // ~ arrow / morphism / subobject classifier jargon
  using Domain = T;
  using Codomain = typename L::Ω;

  // ~ topoi jargon: Member-shape mirror of Subobject's; the IsSubobject
  // contract reads the Member-to-T projection through ι below.  The
  // SingletonSet's only true inhabitant is @c pivot; @c Member is the
  // structural wrapper carrying a T value.
  struct Member {
    T value;
  };

  /** @brief ι: {x} ↣ T — Member unwrap.  Identical pattern to
   *  Subobject<A, χ>::ι; the inclusion projects the Member's
   *  T-value back to the ambient. */
  constexpr T ι(const Member& m) const { return m.value; }

  /** @brief χ: T → Ω — arrow-form classifier for the IsSubobject
   *  contract.  Static self-reference: the IsSubobject contract
   *  reads only the SHAPE (signature + Domain match), not the
   *  semantics; the actual membership query lives in @c operator()
   *  below (instance-aware, returns True iff @c v @c == @c pivot).
   *  Same pattern as Ø / UniversalSet's χ in @c :sets:boundaries. */
  static const SingletonSet χ;

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
  //
  // Constrained to operands that share our logic species so the verdict
  // returned by @c other(pivot) (a @c S::logic_species::Ω value)
  // is type-compatible with our return type @c L::Ω.  Avoids the
  // bool-conversion trap of @c (Ternary::True ? ... : ...): on
  // non-Boolean logics @c L::Ω is an enum class that's not contextually
  // convertible to @c bool.  Cross-logic mereology would need an explicit
  // logic-embedding arrow; that's a follow-up.
  template <typename S>
    requires IsSet<S> && std::same_as<typename S::logic_species, L>
  constexpr typename L::Ω operator<=(const S& other) const {
    return other(pivot);
  }

  /** @section singleton__Mereological_Lattice_Audit */

  /** @section singleton__Unified_Lattice_Operations */

  /** @brief Union of two atoms @f$\{a\}\cup\{b\}@f$ as the @b recoverable
   *  reducer @c category::Join node (#892; was @c OrPredicate).  It is the #365
   *  replacement for the opaque comprehension lambda.  No @c element scout, no
   *  lambda: the two pivots survive in @c decltype (reachable as
   *  @c .predicate().lhs / @c .rhs), so the union can
   *  be @b flattened and inspected structurally --- the prerequisite for the
   *  power-set monad's @c μ (union-flatten, #691) and the Frobenius comonoid
   *  @c δ on sets/relations (#842).  One non-ref-qualified overload: the old
   * lvalue/rvalue split was a @c Comprehension-base-dangling workaround this
   * form does not need, and it is correct for @b distinct atoms (the old lvalue
   * comprehension over
   *  @c *this computed @f$\{a\}\cap(a\vee b)=\{a\}@f$ for
   * @f$\{a\}\cup\{b\}@f$).
   */
  template <typename U, typename L2>
    requires std::same_as<L2, L>
  constexpr auto operator|(const SingletonSet<U, L2>& other) const {
    using Or =
        dedekind::category::Join<SingletonSet<T, L>, SingletonSet<U, L2>>;
    return Set<T, L, Or>{Or{*this, other}};
  }

  /** @brief Singleton-bounded meet (lvalue self).  Same-species only: a mixed
   *  @c L2 would combine in the left species and instantiate @c L::AND on a
   *  foreign @c Ω; a cross-species combine routes through the lifting overload
   *  in @c :expressions instead (#894). */
  template <typename U, typename L2>
    requires std::same_as<L2, L>
  constexpr auto operator&(const SingletonSet<U, L2>& other) const& {
    return Comprehension{*this, [s2 = other](const T& x) { return s2(x); }};
  }

  // FIXME(#842): the meet @c & below still routes through the deprecated
  // @c element scout + a lambda; de-lambda it to a named @c category::Meet node
  // the way @c | above was, when the meet side (the Frobenius multiplication)
  // is needed.  Union went first as the @c μ (#691) / comonoid blocker.
  template <typename U, typename L2>
    requires std::same_as<L2, L>
  constexpr auto operator&(const SingletonSet<U, L2>& other) const&& {
    return element<𝔸<T, L>> |
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
    // The `element<𝔸<T, L>> | lambda` chain produces a Comprehension;
    // wrap in `Set{...}` to materialise an actual Set the caller can
    // invoke.  Without this, callers got `Comprehension does not provide
    // a call operator` errors at the test site.
    return Set{element<𝔸<T, L>> | [s1 = *this, s2 = other](const T& x) {
      // SingletonSet::operator() returns L::Ω directly,
      // so the lift_logic<L> calls are defensive: they
      // normalise if L1 or L2 ever returns bool.
      const auto a = dedekind::category::lift_logic<L>(s1(x));
      const auto b = dedekind::category::lift_logic<L>(s2(x));
      return L::OR(L::AND(a, L::RFL(b)), L::AND(L::RFL(a), b));
    }};
  }

  /** @brief Complement @c !{a} @c = @c {x @c ∈ @c T @c | @c x @c ≠ @c a}.
   *  @details Returns @c Complement<SingletonSet> pointing at @c this;
   *           the free @c operator! on @c Complement unwraps to
   *           @c *source, so @c !!s @b is @c s (same object). */
  constexpr Complement<SingletonSet<T, L>> operator!() const {
    return Complement<SingletonSet<T, L>>{this};
  }
};

// Out-of-class χ definition: completes the IsSubobject self-reference
// declared in-class at @c SingletonSet::χ above.  Default-initializes
// @c pivot via @c T{} (the SHAPE is what the contract reads; the
// semantic membership query goes through the instance's
// @c operator() which IS pivot-aware).
template <typename T, typename L>
inline const SingletonSet<T, L> SingletonSet<T, L>::χ{};

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

/** @brief Product of two singletons: @f$\{a\}\times\{b\}=\{(a,b)\}@f$,
 * collapsed to the @c SingletonSet of the pair.  A structural collapse (the
 * product-side analogue of the complement-pair collapse), so a singleton
 * product is
 *  @b equality-comparable, not merely membership-testable:
 *  @c η(a)*η(b) @c == @c η(std::pair{a,b}).  General (non-singleton) products
 *  keep the predicate-set form of @c :expressions cartesian_product. */
export template <typename T1, typename L1, typename T2, typename L2>
constexpr auto operator*(const SingletonSet<T1, L1>& a,
                         const SingletonSet<T2, L2>& b) {
  return SingletonSet<std::pair<T1, T2>, L1>{std::pair{a.pivot, b.pivot}};
}

export template <typename T, typename L1, typename L2, typename P>
constexpr auto operator^(const SingletonSet<T, L1>& s,
                         const Set<T, L2, P>& other) {
  // The asymmetry is one-sided: `singleton(v)` always lands in
  // ClassicalLogic, while `Set{x % UniversalSet<T> | …}` ascends through
  // NaturalLogic and routinely arrives as TernaryLogic.  Take the
  // result logic from that same side (L2): the singleton's bool lifts
  // through `lift_logic<L2>` cleanly, and the Set's predicate is
  // already in L2.
  return Set{element<𝔸<T, L2>> | [s, other](const T& x) {
    const auto a = dedekind::category::lift_logic<L2>(s(x));
    const auto b = dedekind::category::lift_logic<L2>(other(x));
    return L2::OR(L2::AND(a, L2::RFL(b)), L2::AND(L2::RFL(a), b));
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

/** @brief @c singleton: @f$T \to \mathrm{SingletonSet}\langle T\rangle@f$ ---
 *  the power-set monad's unit @f$\eta@f$ (see the section note). */
export template <typename T>
constexpr auto singleton(T&& value) {
  return SingletonSet<std::decay_t<T>>{std::forward<T>(value)};
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
 *  @c SingletonSet (its callable is the membership classifier @f$T \to
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

/** @brief Explicit @c !{a} overload — shadows the generic
 *         @c IsPredicate-based @c operator! in @c :category:topoi so
 *         @c !singleton picks the set-typed complement, not a
 *         @c Morphism wrapper.  Mirrors the @c Set<T,L,P> overrides
 *         in @c :sets:expressions. */
export template <typename T, typename L>
constexpr auto operator!(const SingletonSet<T, L>& s) {
  return s.operator!();
}

export template <typename T, typename L>
constexpr auto operator!(SingletonSet<T, L>& s) {
  return s.operator!();
}

export template <typename T, typename L>
constexpr auto operator!(SingletonSet<T, L>&& s) {
  return s.operator!();
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
 *   - The image's tier ( @c IsExtensional, @c HasDecidableMembership):
 *     preserved by the lift, since @c SingletonSet has cardinality 1
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
    const SingletonSet<dedekind::category::Dom<std::remove_cvref_t<F>>, L>& s) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  return SingletonSet<U, L>{std::forward<F>(f)(s.pivot)};
}

/** @section singleton__Image_Terminal_Morphism (#661)
 *  @brief Terminal-codomain-arrow collapse for @c image(F, S): when
 *         @c F is an @c IsTerminalMorphism (@c F: @c T @c → @c One),
 *         the image is structurally degenerate.
 *  @details
 *  - @c image(F, @c Ø<T, @c L>) → @c Ø<One, @c L> — empty source maps
 *    to empty image.
 *  - @c image(F, @c UniversalSet<T, @c L, @c C>) →
 *    @c SingletonSet<One, @c L>{One{}} — inhabited source collapses
 *    to the singleton on @c One.
 *  - @c image(F, @c SingletonSet<T, @c L>) falls through to the
 *    generic @c image(F, @c SingletonSet) above (correct: returns
 *    @c SingletonSet<One, L>{F(pivot)} = @c SingletonSet<One, L>{One{}}).
 *
 *  Predicate-based @c Set<T, L, P> sources fall through to the
 *  symbolic-fallback @c image() in @c :expressions (inhabitation
 *  undecidable in general). */
export template <typename L, typename T, typename C, typename F>
  requires dedekind::category::IsTerminalMorphism<std::remove_cvref_t<F>> &&
           std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>, T>
constexpr auto image(F&&, const UniversalSet<T, L, C>&) {
  return SingletonSet<dedekind::category::One, L>{dedekind::category::One{}};
}

export template <typename L, typename T, typename F>
  requires dedekind::category::IsTerminalMorphism<std::remove_cvref_t<F>> &&
           std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>, T>
constexpr auto image(F&&, const Ø<T, L>&) {
  return Ø<dedekind::category::One, L>{};
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

// The Set-monad structure on SingletonSet (η = `singleton`, ε = `origin`,
// Kleisli `operator>>=` / co-Kleisli `operator<<=`) lives directly on the
// carrier above in this partition and is exercised at the value level in
// singleton_test.cpp (the Functor Highway).  A separate `singleton_functor`
// hub struct was retired: it was unused (only referenced by its own
// static_asserts and by test prose), and its `Τ_cat = category::Set<
// SingletonSet<T>>` was the project's lone `category::Set<SetType>` --- a
// category whose objects are set-VALUES.  A set is an OBJECT of Set (Lawvere),
// not itself a category, so we do not model a category-of-sets-as-objects here.

/** @section singleton__The_Final_Ontology_Proof
 * Deferred while `dedekind.sets` is being retargeted to the updated
 * `dedekind.category` hub/spoke functor API.
 */
