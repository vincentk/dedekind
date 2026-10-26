/**
 * @file dedekind/sets/expressions.cppm
 * @partition :expressions
 * @brief Set-builder DSL — comprehension + Boolean connectives.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section expressions__Description
 * This partition provides the principal "set-builder" abstraction:
 * Comprehension<B, P> -- an intensional set whose membership test is a
 * compile-time callable predicate ranging into a subobject classifier L::Omega.
 *
 * Key constructs exported:
 *  - Comprehension<B,P>   -- the point-free set-builder node {x in B | P(x)}.
 *  - Boolean connectives &&, ||, ! lifted to predicate combinators.
 *  - operator<=           -- subset relation (same-predicate -> True;
 *                            heterogeneous -> Unknown via Kleene).
 *  - the expressions across carriers (A × B, A + B, the ℕ / ℤ widening) are
 *    @c :heterogeneous.
 *  - Relation, SetFunction -- subobjects of products.
 *  - relates, is_single_valued_at -- point-wise witnesses.
 *  - power_set / 𝔓         -- default (deleted) gate here; the ordered/convex
 *                            specialisation is dedekind.order:powerset
 * (#830).
 *
 * @section expressions__Canonical_Examples
 * ```cpp
 * const auto xs = ℕ | (χ < 512);  // {n in ℕ | n < 512}
 * const auto grid = cartesian_product(xs, xs);  // xs x xs
 * ```
 *
 * @section expressions__References
 * - Lawvere, F.W. (1964) -- ETCS axioms @cite lawvere1964etcs
 * - Lambek & Scott (1988) -- higher-order categorical logic @cite
 * lambek1988higher
 * - Pierce (1991) -- basic category theory @cite pierce1991basic
 *
 * @quote
 * "Future users of large data banks must be protected from having to know how
 * the data is organized in the machine."
 * -- E. F. Codd, A Relational Model of Data for Large Shared Data Banks (1970)
 *
 * @note "What is objective must be common to many minds and consequently
 * transmissible from one to the other."
 *       -- Henri Poincare, The Value of Science (1905)
 */
module;

#include <compare>
#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>
#include <variant>

export module dedekind.sets:expressions;

import dedekind.category;
import :boundaries;     // For 𝔸, Ø
import :cardinality;    // For Cardinality / SignedCardinality (cross-carrier
                        // meet)
import :computability;  // HasDecidableMembership
import :terms;          // the term grammar: projections, atoms, RelAnd / RelOr

namespace dedekind::sets {
using namespace dedekind::category;

// ── What a set object IS (actual representation) ─────────────────────────────
// The object-level "is a set" concept is @c IsSubobject<T, T::Domain>: @c T
// classified by a characteristic predicate χ (its @c operator()).  What the
// carriers store differs: @c Subobject<A, Chi> (category:topoi) holds JUST the
// classifier @c Chi χ, its ambient being the @b type parameter @c A, not a
// stored value; @c Comprehension<B,P> holds both its @c base and its
// @c predicate, and membership conjoins them.  In either case @c ι is the
// @c Member inclusion (@c ι(Member{a}) = @c a), not a projection.  @c
// IsSet<T> is then @c IsSubobject PLUS the CATEGORY commitment (the ambient
// satisfies the ETCS axioms / is a CCC --- the ambient IS the category Set).
//
// Re-export the predicate→set promotion into dedekind::sets (POLA): @c
// ambient_set forwards to @c classify --- it constructs a classified set object
// over ambient @c A from a predicate; it does @b not build a stored
// (ambient, predicate) pair.  It is DEFINED upstream in category:etcs (beside
// @c IsSet / @c classify); re-exported here (not moved) so the "build a set
// from a predicate" constructor lives at the sets DSL surface without inverting
// the category → sets dependency.
//
// The CONCEPTUAL reading "a set object = ambient (whole, π₁) × predicate χ
// (part-selector, π₂)" --- the mereological / product model --- is a @b model,
// recorded on GH #824 / #826, NOT the current representation: it is literally a
// stored product only in @c Comprehension<Base,P> (which holds @c base + @c
// predicate).  Reifying it uniformly (@c IsSubobject as a free algebra) is a
// deferred north-star (#824), not what the code does today.
export using dedekind::category::ambient_set;

/** @brief Opt-in CRTP base that supplies the ETCS @b set surface to a
 *  @b specialized set expression.  It provides @c Member, @c ι (the subobject
 *  inclusion), @c Codomain, @c logic_species, and the set-lattice trait
 *  members.  It keys the classifier χ off the @c Derived's own @c operator().
 *
 *  @details This makes @c IsSet reachable by @b inheritance for expression
 *  structs (@ref Comprehension, and any wrapper that is "morally a set"),
 *  removing the surface boilerplate each would otherwise duplicate.  It is
 *  @b opt-in, @b never a precondition: @c IsSet stays a @b structural concept,
 *  and a leaf may satisfy it by hand.  The
 *  @c Derived must expose @c operator()(Domain)@c → @c Codomain (its χ). */
export template <typename Derived, typename DomainT, typename L>
struct SetExpr {
  using Domain = DomainT;
  using Codomain = typename L::Ω;
  using logic_species = L;

  /** @brief A member is a carrier value; @c ι projects it back into the
   *  ambient (the subobject inclusion the ETCS axioms read). */
  struct Member {
    DomainT value;
  };
  constexpr DomainT ι(const Member& m) const { return m.value; }

  // NOTE: SetExpr is the ETCS @b subobject surface (Domain/Codomain/Member/ι/χ)
  // and NOTHING more.  It deliberately does @b not register algebra laws:
  // subobject-hood is orthogonal to whether any operation is associative or
  // idempotent.  A blanket @c is_associative_v<Op>=true / @c
  // is_idempotent_v<Op> here would be READ by @c category::is_associative_v's
  // member-discovery
  // (@c :species) for @b every @c Op — claiming, e.g., that a @c Halfspace is
  // associative under @c std::plus<Halfspace>, an operation that does not
  // exist. The honest default (@c is_associative<T,Op> = @c false_type) applies
  // unless a carrier registers a specific @c (Op) it truly satisfies.  See #806
  // review.
};

/** @brief The species of a predicate's answer, @b relative to the base's
 *  species @c L: an answer already in @c L::Ω is in @c L (so a @c Chain<int>
 *  universe accepts an @c int-valued predicate), otherwise the answer type
 *  names its species (@c classifier_logic_t; no default). */
template <typename L, typename Answer>
struct answer_logic {
  using type = dedekind::category::classifier_logic_t<Answer>;
};
template <typename L, typename Answer>
  requires std::same_as<std::remove_cvref_t<Answer>, typename L::Ω>
struct answer_logic<L, Answer> {
  using type = L;
};

/** @brief The logic species of @c {base @c | @c pred}: the @b join (in the
 *  species semilattice, @c category::join_logic_t) of the base's species and
 *  the species of the predicate's answer, so a Kleene predicate over a Boole
 *  base lifts the base rather than truncating itself, and a Kleene predicate
 *  over a @c Percent base answers in @c Percent.  A pair with no join (a @c K₃
 *  answer over a @c Chain<int> base) is refused here. */
template <typename Base, typename Predicate>
using comprehension_logic_t =
    join_logic_t<typename Base::logic_species,
                 typename answer_logic<
                     typename Base::logic_species,
                     std::invoke_result_t<const Predicate&,
                                          const typename Base::Domain&>>::type>;

/** @brief The cardinality bound a comprehension inherits: the datum's when it
 *  declares one, else the base's.
 *  @tparam Predicate the datum.
 *  @tparam Base the base set. */
/** @brief A predicate or set that declares its cardinality: @c
 *  T::cardinality_type is formed.  @tparam T the candidate. */
template <typename T>
concept HasCardinalityType = requires { typename T::cardinality_type; };

template <typename Predicate, IsSetObject Base>
struct datum_cardinality {
  using type = typename Base::cardinality_type;
};
template <typename Predicate, IsSetObject Base>
  requires HasCardinalityType<Predicate>
struct datum_cardinality<Predicate, Base> {
  using type = typename Predicate::cardinality_type;
};

export template <typename Base, typename Predicate>
struct Comprehension
    : SetExpr<Comprehension<Base, Predicate>, typename Base::Domain,
              comprehension_logic_t<Base, Predicate>> {
  Base base;  // by VALUE: a comprehension OWNS its base.  A reference member
              // would dangle when the constructor binds an rvalue base (the
              // aggregate form extended the temporary's lifetime; a constructor
              // parameter does not).
  Predicate predicate;

  /** @brief Explicit two-argument constructor.  Needed because @ref SetExpr is
   * a base class, so @c Comprehension is no longer an aggregate --- the
   *  @c Comprehension{base, pred} sites (e.g. @c Singleton::operator|) route
   *  here instead of through aggregate init. */
  constexpr Comprehension(const Base& b, Predicate p)
      : base(b), predicate(static_cast<Predicate&&>(p)) {}
  /** @brief The former over the @b universe of the carrier: @c {x ∈ 𝔸 | P}
   *  from the predicate alone (the base is default-constructible, as @c 𝔸<T,L>
   *  is).  This is the one set former of the Δ₀ core,
   *  @c Comprehension<𝔸<T,L>, P>. */
  constexpr explicit Comprehension(Predicate p)
    requires std::default_initializable<Base>
      : base{}, predicate(static_cast<Predicate&&>(p)) {}

  // The datum's bound when it declares one (a point is Finite whatever the
  // universe), else the base's: a restriction can only shrink.
  using cardinality_type = typename datum_cardinality<Predicate, Base>::type;
  constexpr cardinality_type cardinality() const { return {}; }

  /** @brief χ: the comprehension's characteristic map --- @c x @c ∈ @c {S @c |
   *  @c P} @c ⟺ @c x @c ∈ @c S @c ∧ @c P(x).  A comprehension @b is a set, so
   * it
   *  @b is its own predicate (@c IsSet @c ⟹ @c IsPredicate). */
  constexpr auto operator()(const typename Base::Domain& x) const {
    // Both answers are lifted into the JOIN species before the conjunction
    // (Σ ↪ Ω: a Boole answer becomes a decided Kleene one), so a Boole base
    // with a Kleene predicate keeps Unknown instead of failing to convert it,
    // and a Kleene base with a bool predicate lifts the predicate as before.
    using L = comprehension_logic_t<Base, Predicate>;
    return L::AND(dedekind::category::lift_logic<L>(base(x)),
                  dedekind::category::lift_logic<L>(predicate(x)));
  }
  /** @brief χ on the components of a product carrier: a comprehension over
   *  @c A @c × @c B is a binary relation, and @c r(a, @c b) @b is
   *  @c r(⟨a, b⟩), the two presentations of one arrow by the product's
   *  universal property, @f$\mathrm{Hom}(A\times B, \Omega) \cong
   *  \mathrm{Multi}((A, B), \Omega)@f$ (#1047). */
  template <typename D = typename Base::Domain>
    requires IsPairing<D>
  constexpr auto operator()(const leg_1_t<D>& a, const leg_2_t<D>& b) const {
    return (*this)(D{a, b});
  }
  /** @brief Heterogeneous χ: a value of a type @c U the datum @b declares it
   *  admits (@c Predicate::admits<U>) is asked unnarrowed, so
   *  @c Singleton<int>{1}(1.5) is @c False.  The universe is not asked
   *  (@c χ_𝔸 @c ≡ @c ⊤); a proper base must itself admit @c U.
   *  @tparam U the foreign carrier. */
  template <typename U>
    requires(!std::same_as<std::remove_cvref_t<U>, typename Base::Domain>) &&
            requires { requires Predicate::template admits<U>; } &&
            (Is𝔸<Base> || std::invocable<const Base&, const U&>)
  constexpr auto operator()(const U& x) const {
    using L = comprehension_logic_t<Base, Predicate>;
    if constexpr (Is𝔸<Base>)
      return dedekind::category::lift_logic<L>(predicate(x));
    else
      return L::AND(dedekind::category::lift_logic<L>(base(x)),
                    dedekind::category::lift_logic<L>(predicate(x)));
  }

  /** @brief Size when the datum carries it and the base is the whole
   *  universe: the point @c {p} over @c 𝔸<T> has size 1. */
  constexpr std::size_t size() const
    requires Is𝔸<Base> && requires(const Predicate& p) {
      { p.size() } -> std::convertible_to<std::size_t>;
    }
  {
    return predicate.size();
  }
  /** @brief The ETCS cardinality bound a datum-sized comprehension exposes to
   *  @c bound_meet / @c bound_join: its exact size. */
  constexpr std::size_t upper_bound() const
    requires Is𝔸<Base> && requires(const Predicate& p) {
      { p.size() } -> std::convertible_to<std::size_t>;
    }
  {
    return predicate.size();
  }
};

/** @brief A datum over @c T: a predicate on @c T (an arrow into a truth
 *  object) that is not itself a set.  @c Point, @c Bounds, @c LowerCut.
 *  @tparam P the candidate.
 *  @tparam T the carrier. */
export template <typename P, typename T>
concept IsDatum =
    IsPredicate<P> && std::same_as<Dom<P>, T> && !IsSetObject<P> && !IsLSet<P>;

/** @brief The set former @c 𝔸<T, L>{} @c & @c P = @f$\{x \in T \mid P(x)\}@f$
 *  for a datum @c P (a point @c π == v, a cut @c π > v): the meet with the
 *  subobject @c P classifies (#826; @c | is the join only).
 *  @tparam T the carrier.
 *  @tparam L the species.
 *  @tparam C the cardinality.
 *  @tparam P the datum. */
export template <typename T, IsOckhamAlgebra L, IsCardinality C, IsDatum<T> P>
constexpr auto operator&(const 𝔸<T, L, C>&, P p) {
  return Comprehension<𝔸<T, L, C>, P>{std::move(p)};
}

/** @brief @c 𝔸<T> @c & @c atom for an atom on the unary @c π
 *  (@c ℕ @c & @c (π @c % @c 3 @c == @c 0)): the atom evaluated at each element.
 *  @tparam T the carrier.  @tparam P the atom. */
export template <typename T, IsOckhamAlgebra L, IsCardinality C,
                 IsRelPredicate P>
  requires(only_slot(slots_v<P>, 0)) && (!contains_self_v<P>) &&
          std::convertible_to<std::invoke_result_t<const P&, const T&>, bool>
constexpr auto operator&(const 𝔸<T, L, C>&, P p) {
  bind_check<T>(p);
  return Comprehension<𝔸<T, L, C>, P>{std::move(p)};
}

/** @brief @c 𝔸<T> @c & @c pred for a predicate built at run time: bound to
 *  the carrier now, every operator admitted there or refused
 *  (@c not_admitted), every constant meeting it.  Pairs take the product
 *  former.  @tparam T the carrier.  @tparam K the constants' type. */
export template <typename T, IsOckhamAlgebra L, IsCardinality C, typename K>
  requires(!requires { typename T::first_type; })
constexpr auto operator&(const 𝔸<T, L, C>&, Pred<K> p) {
  bind_check<T>(p);
  return Comprehension<𝔸<T, L, C>, Pred<K>>{std::move(p)};
}

/** @brief @c S @c | @c P for a set and a non-set: refused.  The filter is
 *  the meet, @c S @c & @c P (#826); @c | is the join of two sets only.
 *  @tparam S the set.  @tparam P the would-be filter. */
export template <typename S, typename P>
  requires IsSetObject<S> && (!IsSetObject<P>) && (!IsLSet<P>)
void operator|(const S&, const P&) = delete;

/** @brief The universal predicate: accepts every element of T. */
export template <typename T>
struct UniversalPredicate {
  using Domain = T;
  constexpr bool operator()(const T&) const { return true; }
};

/**
 * @brief The empty predicate: rejects every element of T.
 *
 * Used as the structural witness of a contradiction proven at compile time
 * (e.g. `(x > 5) && (x < 3)` collapsing via `structured_and`). When a `Set`'s
 * predicate is `EmptyPredicate<T>`, the set equals `Ø<T, L>`.
 */
export template <typename T>
struct EmptyPredicate {
  using Domain = T;
  constexpr bool operator()(const T&) const { return false; }
};

/**
 * @brief The archetypal undecidable predicate: answers @c Unknown everywhere.
 *
 * @details The Kleene-interior companion of @c UniversalPredicate (always
 * @f$\top@f$) and @c EmptyPredicate (always @f$\bot@f$).  Its characteristic
 * map is constantly @c Ternary::Unknown, the interior of the chain @f$K_3 =
 * \{\bot < U < \top\}@f$, so membership is @b never decided: for any point @c x
 * its answer sits outside the decided core @f$\Sigma = \{\top,\bot\}@f$ that
 * @c is_decided detects.  It is intrinsically three-valued, hence @c
 * Kleene-tagged, and a
 * @c Set carrying it fails @c HasDecidableMembership.
 *
 * It exists to exercise the lattice reducer against a genuinely undecidable
 * operand.  The Kleene annihilators are what recover a decided answer without
 * ever consulting @c Unknown: a meet with the bottom @c Ø (@f$x \wedge \bot =
 * \bot@f$, @c AND @c = @c min, and @c min(U,\bot) @c = @c \bot) or a join with
 * the top @c 𝔸 (@f$x \vee \top = \top@f$, @c OR @c = @c max, @c max(U,\top) @c
 * =
 * @c \top) annihilates it, and the decided boundary is recovered structurally.
 */
export template <typename T>
struct UnknownPredicate {
  using Domain = T;
  using Codomain = typename Kleene::Ω;
  using logic_species = Kleene;
  constexpr Codomain operator()(const T&) const { return Ternary::Unknown; }
};

// It is a bona fide characteristic map χ: T → Ω, not an ad-hoc callable: an
// @c IsArrow (Domain/Codomain) into the truth-object Ω, so the reducer and the
// subobject surface treat it exactly as any other membership predicate.
static_assert(IsCharacteristic<UnknownPredicate<int>>,
              "UnknownPredicate is a characteristic map χ: T → Ω");
// The archetype's defining property: it never lands on a decided bound, so it
// sits strictly inside the Kleene chain (@c is_decided is the decided-core test
// from @c :logic).  This binds the undecidability claim to a compile-time
// witness rather than prose.
static_assert(UnknownPredicate<int>{}(0) == Ternary::Unknown &&
                  UnknownPredicate<int>{}(42) == Ternary::Unknown,
              "UnknownPredicate answers Unknown everywhere");
static_assert(!is_decided<Kleene>(UnknownPredicate<int>{}(0)),
              "UnknownPredicate is never in the decided core Σ = {⊤,⊥}");

// A Kleene predicate over a Boole base: the comprehension's species is the
// JOIN (Kleene), Unknown survives, and a False base still annihilates it.
static_assert(
    std::same_as<Comprehension<𝔸<int>, UnknownPredicate<int>>::logic_species,
                 Kleene>,
    "{𝔸<int,Boole> | Unknown} is Kleene-classified (the join)");
static_assert(Comprehension{𝔸<int>{}, UnknownPredicate<int>{}}(0) ==
                  Ternary::Unknown,
              "a Boole base lifts; the predicate's Unknown is preserved");
static_assert(Comprehension{Ø<int>{}, UnknownPredicate<int>{}}(0) ==
                  Ternary::False,
              "a False base annihilates Unknown (Kleene AND = min)");

// The codomain-leg value finalizer `finalize_combine` (#894) is hoisted to
// `:boundaries` (beside `codomain_reduce_t`), so the upstream boundary
// operators and the general subobject operators below share ONE implementation
// of the value-finalization law (boundary output → decided Boole codomain; all
// else passes through) instead of duplicating it.  Reachable here through
// `import :boundaries`; a new expressions-level operator needs only
// `return finalize_combine(...)` and inherits the rule.

/** @brief Bring a set object to the species @c TargetL along the dominance:
 *  the identity when it is already there, else the comprehension
 *  @c {x ∈ 𝔸<T, TargetL> | s(x)}.  Its species is the join, which is
 *  @c TargetL since @c s's species embeds into it, and its χ is
 *  @c lift_logic<TargetL>(s(x)) (the universe answers ⊤): the lift @b is
 *  composition with the dominance embedding, no wrapper type.  It does not
 *  preserve the operand's structural type (interval / halfspace), so a
 *  cross-species combine materialises pointwise rather than collapsing
 *  structurally.  Constrained to a registered inclusion, so a downward or
 *  unsupported lift is rejected at the gate. */
export template <typename TargetL, IsSetObject S>
  requires dedekind::category::LiftsTo<typename S::logic_species, TargetL>
constexpr auto lift_to(const S& s) {
  if constexpr (std::same_as<typename S::logic_species, TargetL>) {
    return s;
  } else {
    return Comprehension{𝔸<typename S::Domain, TargetL,
                           typename universe_t<S>::cardinality_type>{},
                         s};
  }
}

// The set complement is the reducer's own @c Not<A> node (@c :lattice), not a
// bespoke @c :sets wrapper: @c ¬A is @c Not<A> (a first-class subobject when
// @c A is one), its χ the codomain reflection @c Ω::RFL of @c A's, and the
// reducer's @c is_complement_pair_v / @c de_morgan_of / @c is_not_node_v
// recognise it directly.  So the retired @c NegatedPredicate wrapper, its
// @c IsComplementPair / @c IsNegatedPredicate_v traits, and the hand-rolled
// @c !! peel / @c are_complement_sets_v collapse all fold into the reducer's
// complement laws (#834 / #829 / #946).

/** @section expressions__Reducer_Leaf_Combiner
 *
 *  The set meet / join route through the generic lattice-law term reducer
 *  (@c category:lattice_term, #865/#890) under @c subobject_order<L>.  The
 *  reducer owns the structural laws (bounded, idempotence, absorption, ...);
 *  the @b domain-specific collapse of two order-incomparable set leaves is the
 *  injected @b leaf-combiner @c SetCombine.  It reduces the @c structured_and /
 *  @c structured_or of two halfspaces into an interval, say.  It reaches @c
 * :order via ADL (order is downstream of sets, so ADL is the cycle-free
 * customisation point), exactly as the pre-reducer @c operator& / @c operator|
 * did. */

/** @brief Value-level elevate of a @c structured_and result to the meet's
 *  normal-form value: an empty reduction is the initial object @c Ø; a
 *  finite reduction (a point) is itself a set-like leaf (returned bare); any
 *  other reduction is a named predicate wrapped back into a comprehension.
 *  (Extracted verbatim from the pre-reducer @c operator& structured_and branch
 *  so @c SetCombine's type and this value stay in lockstep.)
 *  @tparam T the carrier.
 *  @tparam L the species.
 *  @tparam Reduced the reducer's result: an @c EmptyPredicate, a sized leaf, or
 *          a named predicate. */
export template <typename T, IsOckhamAlgebra L, typename Reduced>
constexpr auto elevate_meet(Reduced reduced) {
  using Result = std::decay_t<Reduced>;
  if constexpr (std::same_as<Result, EmptyPredicate<T>>) {
    return Ø<T, L>{};
  } else if constexpr (HasCardinalityType<Result>) {
    // Nested (not &&-chained): a Result without cardinality_type must not
    // instantiate the inner probe.
    if constexpr (std::same_as<typename Result::cardinality_type, Finite>) {
      return reduced;
    } else {
      return Comprehension<𝔸<T, L>, Result>{std::move(reduced)};
    }
  } else {
    return Comprehension<𝔸<T, L>, Result>{std::move(reduced)};
  }
}

/** @brief Value-level elevate of a @c structured_or result to the join's
 *  normal-form value: a covering pair is the universe @c 𝔸 (returned bare);
 *  any other reduction (a wider halfspace) is wrapped back into a @c Set.
 *  Dual of @c elevate_meet. */
export template <typename T, typename L, typename Reduced>
constexpr auto elevate_join(Reduced reduced) {
  using Result = std::decay_t<Reduced>;
  if constexpr (std::same_as<Result, 𝔸<T, L>>) {
    return reduced;
  } else {
    return Comprehension<𝔸<T, L>, Result>{std::move(reduced)};
  }
}

/** @brief The injected leaf-combiner (the reducer's @c Combine policy) for
 *  the subobject lattice: at an order-incomparable residual, hand the two set
 *  leaves to the carrier's domain @c ∧ / @c ∨ on their runtime data and let the
 *  reducer fold the result again.  Two plain sets over one universe combine
 *  through their predicates (@c structured_and / @c structured_or via ADL,
 *  elevated back to a set); a value leaf (an @c Interval) combines directly.
 *  Each leg is present only where the carrier actually combines the pair, so
 *  elsewhere the residual stays a @c Meet / @c Join node. */
export struct SetCombine {
  template <typename T, typename L, typename C, typename PA, typename PB>
    requires requires(const PA& a, const PB& b) { structured_and(a, b); }
  static constexpr auto meet_value(const Comprehension<𝔸<T, L, C>, PA>& a,
                                   const Comprehension<𝔸<T, L, C>, PB>& b) {
    return elevate_meet<T, L>(structured_and(a.predicate, b.predicate));
  }
  template <typename T, typename L, typename C, typename PA, typename PB>
    requires requires(const PA& a, const PB& b) { structured_or(a, b); }
  static constexpr auto join_value(const Comprehension<𝔸<T, L, C>, PA>& a,
                                   const Comprehension<𝔸<T, L, C>, PB>& b) {
    return elevate_join<T, L>(structured_or(a.predicate, b.predicate));
  }
  template <typename RA, typename RB>
    requires requires(const RA& a, const RB& b) { structured_and(a, b); }
  static constexpr auto meet_value(const RA& a, const RB& b) {
    return structured_and(a, b);
  }
  template <typename RA, typename RB>
    requires requires(const RA& a, const RB& b) { structured_or(a, b); }
  static constexpr auto join_value(const RA& a, const RB& b) {
    return structured_or(a, b);
  }
};

// ── The set-object legs of the opaque arm (IsSetObject, :boundaries)
// ──────────
//
// A comprehension's universe is its BASE's universe: the mereological whole is
// what the base is a part of, restriction only shrinks the part.  So `ℕ | P`
// has universe 𝔸<Cardinality>, and `(ℕ | P) | Q` the same --- the recursion
// bottoms out at the universe, which is its own universe.
export template <typename Base, typename Predicate>
constexpr auto universe(const Comprehension<Base, Predicate>&) {
  // The leg lives in the COMPREHENSION's species: the base's universe,
  // re-tagged to the join when the answer lifted the base (a Kleene predicate
  // over a Boole base), so the leg's species equals the set's (Is𝔸Of).
  return 𝔸<typename Base::Domain,
           typename Comprehension<Base, Predicate>::logic_species,
           typename universe_t<Base>::cardinality_type>{};
}

// A comprehension over a universe 𝔸<T,L> is the opaque arm: the default leg
// applies and the predicate P is its χ datum --- the very object `operator&`
// hands to `structured_and`, so the leg names what the reducer already reads.
export template <typename T, IsOckhamAlgebra L, typename P, IsCardinality C>
constexpr P classifier(const Comprehension<𝔸<T, L, C>, P>& s) {
  return s.predicate;
}

static_assert(
    std::same_as<universe_t<Comprehension<𝔸<int, dedekind::category::Boole>,
                                          UniversalPredicate<int>>>,
                 𝔸<int>>,
    "the universe of a comprehension over 𝔸<int> is 𝔸<int>.");
static_assert(IsSetObject<Comprehension<𝔸<int>, UniversalPredicate<int>>>,
              "a comprehension is a set object.");
static_assert(
    std::same_as<
        universe_t<Comprehension<Comprehension<𝔸<int>, UniversalPredicate<int>>,
                                 UniversalPredicate<int>>>,
        𝔸<int>>,
    "a nested comprehension's universe is the base's universe (the whole).");

}  // namespace dedekind::sets

namespace dedekind::category {
// A Set's value is determined by its type only when its predicate is stateless.
// A runtime-stateful predicate (a field-carrying P such as a Point's pivot)
// makes two same-type Sets potentially distinct, so the reducer's type-based
// idempotence must NOT collapse them; gate it on the predicate's emptiness.
template <typename T, IsOckhamAlgebra L, typename P,
          dedekind::sets::IsCardinality C>
inline constexpr bool idempotent_leaf_v<
    dedekind::sets::Comprehension<dedekind::sets::𝔸<T, L, C>, P>> =
    std::is_empty_v<P>;

}  // namespace dedekind::category

namespace dedekind::sets {

// ── Free set combinators over IsSet (#892) ──────────────────────────────────
// The meet / join / complement, retired as Set MEMBERS, as free combinators
// over the structural IsSet concept.  Set, Meet, Join and the boundaries
// therefore compose uniformly, so nested expressions like @c (A|B) & !(A&B)
// resolve without an inheritance hierarchy.  Each folds the reducer term
// through
// @c subobject_reduce_t and materialises the normal form; the irreducible meet
// / join becomes a @c Meet / @c Join carrying its operand sets (#892).

/** @brief The predicate of a PLAIN set, a @c Comprehension over the universe
 *  @c 𝔸<T,L> (@c ::type absent otherwise);
 *  @c PlainSet gates the plain-set-only branches (complement pair, predicate
 *  negation) so a compound node (@c Meet / @c Join) takes the node path.
 */
template <typename S>
struct set_predicate {};
template <typename T, typename L, typename P, typename C>
struct set_predicate<Comprehension<𝔸<T, L, C>, P>> {
  using type = P;
};
template <typename S>
concept PlainSet = requires { typename set_predicate<S>::type; };

/** @brief The subobject-lattice meet @c A @c & @c B, over any two @c
 * IsSubobject operands sharing a carrier and logic.  It folds @c Meet<A,B>
 * through the reducer; @c SetCombine supplies the domain @c structured_and at
 * the incomparable residual.  The normal form materialises to @c Ø, @c 𝔸, an
 *  operand, or a structured leaf.  An irreducible meet becomes a @c Meet
 *  carrying both operand sets. */
export template <typename LHS, typename RHS>
  requires IsSetObject<LHS> && IsSetObject<RHS> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           std::same_as<typename LHS::logic_species,
                        typename RHS::logic_species>
constexpr auto operator&(const LHS& lhs, const RHS& rhs) {
  return subobject_reduce<typename LHS::logic_species, SetCombine>(
      Meet<LHS, RHS>{lhs, rhs});
}

/** @brief The subobject-lattice join @c A @c | @c B, dual to @c operator&.  An
 *  irreducible join becomes a @c Join carrying both operand sets. */
export template <typename LHS, typename RHS>
  requires IsSetObject<LHS> && IsSetObject<RHS> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           std::same_as<typename LHS::logic_species,
                        typename RHS::logic_species>
constexpr auto operator|(const LHS& lhs, const RHS& rhs) {
  return subobject_reduce<typename LHS::logic_species, SetCombine>(
      Join<LHS, RHS>{lhs, rhs});
}

/** @brief Cross-species meet (#894, step i): two subobjects over the same
 *  carrier but @b different codomains join to the more expressive one (the
 * species semilattice's join, @c category::join_logic_t; refused when no
 * shipped species is above both); lift both there, then the same-species meet
 * above folds them.  This lets a mixed
 *  @c Boole @c ∩ @c Kleene combine into the reducer at all.  Same-species
 *  combines are untouched (this overload requires the species to @b differ, so
 *  it never competes with the meet above). */
export template <typename LHS, typename RHS>
  requires IsSetObject<LHS> && IsSetObject<RHS> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           (!std::same_as<typename LHS::logic_species,
                          typename RHS::logic_species>) &&
           dedekind::category::HaveLogicJoin<typename LHS::logic_species,
                                             typename RHS::logic_species>
constexpr auto operator&(const LHS& lhs, const RHS& rhs) {
  using Log =
      join_logic_t<typename LHS::logic_species, typename RHS::logic_species>;
  return lift_to<Log>(lhs) & lift_to<Log>(rhs);
}

/** @brief Cross-species join, dual to the cross-species meet (#894, step i). */
export template <typename LHS, typename RHS>
  requires IsSetObject<LHS> && IsSetObject<RHS> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           (!std::same_as<typename LHS::logic_species,
                          typename RHS::logic_species>) &&
           dedekind::category::HaveLogicJoin<typename LHS::logic_species,
                                             typename RHS::logic_species>
constexpr auto operator|(const LHS& lhs, const RHS& rhs) {
  using Log =
      join_logic_t<typename LHS::logic_species, typename RHS::logic_species>;
  return lift_to<Log>(lhs) | lift_to<Log>(rhs);
}

/** @brief The set complement @c ~A (#829: @c ~ is the SET complement; @c ! is
 *  the predicate complement, which @c category::operator! supplies as a formal
 *  @c Morphism).  @c ~A is the reducer's own @c Not<A> node --- a first-class
 *  subobject whose χ is the codomain reflection @c Ω::RFL of @c A's, generic
 *  over any bounded chain (@c Boole @c !, @c Kleene @c ¬U=U, and any registered
 *  @c Chain).  There is no bespoke wrapper and no @c is_set_node_v tag: the
 * gate is the structural @c IsSubobject, and because @c category defines no
 *  @c operator~ there is nothing to out-prioritise, so the tag that once
 *  disambiguated the two @c operator! overloads is gone (#963).  The @c const
 *  reference parameter (matching @c operator& / @c operator|) makes this a
 *  FALLBACK that loses partial ordering to a type-specific complement, so a
 *  @c :order @c Halfspace / @c Singleton @c operator~ still wins for its own
 *  type (its closed-form collapse, e.g. @c ~Above<5> = @c AtMost<5>).
 *
 *  @details The complement is a certified @b involution: @c ~~A ≡ A, peeled
 *  here at the leaf (the reducer's @c ¬¬A→A).  Because @c ~A is a @c Not node,
 *  the reducer's own complement laws apply in @c operator& / @c operator|:
 *  @c ¬(A∧B)→¬A∨¬B (De Morgan) and @c a∧¬a→⊥ / @c a∨¬a→⊤ when the codomain
 *  @c Ω is complemented. */
export template <IsPredicate P>
  requires IsSetObject<std::remove_cvref_t<P>>
constexpr auto operator~(const P& p) {
  using D = std::remove_cvref_t<P>;
  if constexpr (dedekind::category::is_not_node_v<D>) {
    // ¬¬A ≡ A: peel the Not node (the reducer's involution, at the leaf).
    return p.base;
  } else {
    return dedekind::category::Not<D>{p};
  }
}

/** @brief The complement as an @b arrow on the object @f$\mathrm{Sub}(T, L)@f$
 *  (@c Subobjects, @c :boundaries), @f$S \mapsto \neg S@f$: an endomorphism,
 * its domain and codomain the one object, presented by every set object over
 *  @c T in @c L.  It is an involution by the reducer's own law
 *  @f$\neg\neg A \to A@f$: @c operator~ peels the node, so @c ~~s @b is @c s,
 *  by type, for every presentation @c s; the certificate reads that law at the
 *  representative.  The first arrow on sets @c category::IsInvolution admits:
 *  the concept quantifies over the object, not over one value type
 *  (@c morphism__Objects_And_Their_Presentations).
 *  @tparam T the carrier.  @tparam L the logic species. */
export template <typename T, IsOckhamAlgebra L = Boole>
struct Complement {
  using Domain = Subobjects<T, L>;
  using Codomain = Subobjects<T, L>;
  template <typename S>
    requires dedekind::category::Presents<S, Subobjects<T, L>>
  constexpr auto operator()(const S& s) const {
    return ~s;
  }
  /** @brief @f$\neg\neg = \mathrm{id}@f$, read off the reducer at the
   *  universe: @c ~~𝔸 is @c 𝔸 by type. */
  template <typename X>
  static constexpr bool is_involutive_v =
      std::same_as<X, Subobjects<T, L>> &&
      std::same_as<
          std::remove_cvref_t<decltype(~~std::declval<const 𝔸<T, L>&>())>,
          𝔸<T, L>>;
};
}  // namespace dedekind::sets
namespace dedekind::category {
/** @brief The complement reverses inclusion: @f$a \subseteq b \Rightarrow
 *  \neg b \subseteq \neg a@f$, for every carrier and logic. */
template <typename T, typename L>
struct is_antimonotone<dedekind::sets::Complement<T, L>, std::less_equal<>>
    : std::true_type {};
/** @brief The complement is its own antitone adjoint, @f$a \subseteq \neg b
 *  \iff b \subseteq \neg a@f$: the De Morgan algebra @f$\mathrm{Sub}(T, L)@f$
 *  as a Galois connection, for every carrier and logic. */
template <typename T, typename L>
struct is_adjoint<dedekind::sets::Complement<T, L>,
                  dedekind::sets::Complement<T, L>, std::less_equal<>>
    : std::true_type {};
}  // namespace dedekind::category
namespace dedekind::sets {
namespace detail_complement_witness {
inline constexpr auto three = 𝔸<int>{} & (π == 3);
static_assert(dedekind::category::IsInvolution<Complement<int>> &&
                  dedekind::category::IsInvolution<Complement<int, Kleene>>,
              "the complement is an involution on Sub(int): an arrow on an "
              "object presented by many types, through IsInvolution itself");
static_assert(dedekind::category::IsContravariantFunctor<Complement<int>> &&
                  dedekind::category::IsGaloisConnection<Complement<int>,
                                                         Complement<int>>,
              "the complement reverses inclusion and is its own adjoint: "
              "a ⊆ ¬b ⟺ b ⊆ ¬a, the De Morgan algebra Sub(int) categorically");
static_assert(
    std::same_as<std::remove_cvref_t<
                     decltype(Complement<int>{}(Complement<int>{}(three)))>,
                 std::remove_cvref_t<decltype(three)>> &&
        !Complement<int>{}(three)(3) && Complement<int>{}(three)(4),
    "at a comprehension: ~~s is s by type, ~s the complement at points");
// The identity on the object composes with the arrows on it, both ways.
static_assert(
    IsArrow<Identity<Subobjects<int>>> &&
        std::same_as<
            std::remove_cvref_t<decltype(Compose<Identity<Subobjects<int>>,
                                                 Complement<int>>{}(three))>,
            std::remove_cvref_t<decltype(Complement<int>{}(three))>> &&
        std::same_as<std::remove_cvref_t<
                         decltype(Compose<Complement<int>,
                                          Identity<Subobjects<int>>>{}(three))>,
                     std::remove_cvref_t<decltype(Complement<int>{}(three))>>,
    "id ∘ ¬ = ¬ = ¬ ∘ id on Sub(int)");
}  // namespace detail_complement_witness

/** @brief Symmetric difference @c A @c △ @c B (set-theoretic XOR; #469), a FREE
 *  combinator over @c IsSubobject (like @c & / @c | / @c ~), so it composes
 *  uniformly whether an operand is a @c Set, a @c Meet / @c Join, or a
 *  @c Not complement node.  The textbook identity @c A@c △@c B @c = @c (A@c ∩
 *  @c ¬B)@c ∪@c (¬A@c ∩@c B): no bespoke XOR predicate --- @c ~ / @c & / @c |
 *  carry the per-carrier logic and the reducer collapses the result, including
 *  @c A@c △@c ¬A @c = @c 𝔸 via the join-complement law on a complemented
 *  codomain.  The disjoint fast path (@c A@c ∩@c B @c = @c Ø @c ⟹ @c A@c △@c B
 *  @c = @c A@c ∪@c B) is kept for the halfspace-style compile-time collapse. */
export template <typename LHS, typename RHS>
  requires IsSetObject<LHS> && IsSetObject<RHS> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           std::same_as<typename LHS::logic_species,
                        typename RHS::logic_species>
constexpr auto operator^(const LHS& a, const RHS& b) {
  if constexpr (IsInitialObject<std::decay_t<decltype(a & b)>>) {
    return a | b;
  } else {
    return (a & ~b) | (~a & b);
  }
}

/** @brief Cross-species symmetric difference: both operands are lifted into
 *  the join of their species and @c △ is computed there (as for @c & and
 *  @c |).  @tparam LHS / @tparam RHS set objects on one carrier, of different
 *  species with a join. */
export template <IsSetObject LHS, IsSetObject RHS>
  requires std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           (!std::same_as<typename LHS::logic_species,
                          typename RHS::logic_species>) &&
           dedekind::category::HaveLogicJoin<typename LHS::logic_species,
                                             typename RHS::logic_species>
constexpr auto operator^(const LHS& a, const RHS& b) {
  using Log =
      join_logic_t<typename LHS::logic_species, typename RHS::logic_species>;
  return lift_to<Log>(a) ^ lift_to<Log>(b);
}

// The set-lattice operations ARE the operators @c operator& / @c operator| /
// @c operator~ above: ONE surface per operation, no free-function aliases.  The
// meet COLLAPSES ({x>5}∩{x>3} → {x>5}) and the collapse is TYPE-observable in
// the result.  Complement is the reducer's @c Not node (@c :lattice), so its
// laws (@c ¬¬A→A, De Morgan, @c a∧¬a→⊥) apply.  Membership @c in / @c in_via
// lives in @c :category:concrete (χ-evaluation, not a lattice op).

/** @section expressions__Complement_Is_An_Involution
 *  The set complement is an involution: @c ~~A ≡ A at the @b type level for a
 *  subobject.  @c ~A is the reducer's @c Not node, and @c operator~ peels
 *  @c Not<Not<A>> → @c A at the leaf (the reducer's @c ¬¬A→A), so the
 *  double-negation is structurally self-inverse. */
namespace detail_complement_involution {
using UnivSizeSet = Comprehension<𝔸<std::size_t, dedekind::category::Boole>,
                                  UniversalPredicate<std::size_t>>;
static_assert(
    std::same_as<std::remove_cvref_t<decltype(~~std::declval<UnivSizeSet>())>,
                 UnivSizeSet>,
    "~~A ≡ A: the set complement is a structural involution");
static_assert(dedekind::category::logic_negation_is_involutive_v<
                  dedekind::category::Boole>,
              ":involution certifies the logic negation the complement reduces "
              "to is an involution");
}  // namespace detail_complement_involution

/** @brief @c inclusion_arrow(S) --- the inclusion ι_S: S ↪ Domain<S> of a
 *  subobject as a first-class @c IsArrow (@c Domain = @c S::Member,
 *  @c Codomain = @c S::Domain, routing through @c S's own @c ι).  A thin
 *  arrow-wrapper of the @c ι that @c Set / @c Subobject already expose, so it
 *  can serve as a cospan arrow.  The meet-as-pullback (#881): @c A @c & @c B is
 *  the pullback of the cospan @c inclusion_arrow(A), @c inclusion_arrow(B).
 *
 *  @note FIXME(#887): this is a @b localized reification.  Categorically @c ι
 * is already an arrow, but every subobject exposes it only as the @c .ι @b
 * member
 *  @b function (the @c #681 structural-refactor call shape), so it is not a
 *  nameable @c IsArrow that @c IsPullback can take as a cospan leg.  #887
 * tracks promoting the inclusion to a first-class arrow intrinsic to each
 * subobject carrier (strengthening @c IsSubobject) and retiring this wrapper.
 */
export template <typename S>
  requires dedekind::category::IsSubobject<S, typename S::Domain>
struct SubobjectInclusion {
  using Domain = typename S::Member;
  using Codomain = typename S::Domain;
  /** @brief Structural monic opt-in: a subobject inclusion @c ι: S ↪ A is a
   *  monomorphism by definition, so @c IsMonicArrow fires via its tag-discovery
   *  branch.  A tag (not an @c is_monic_arrow_v partial spec) because a
   *  variable-template partial spec does not cross the module boundary from
   *  @c :sets to @c :morphism (cf. the note at @c is_monic_arrow_v). */
  using is_monic_arrow_tag = void;
  /** @brief The subobject whose canonical inclusion this arrow reifies; held so
   *  @c operator() routes through @b its @c ι rather than re-deriving
   *  @c m.value, staying faithful to any subobject that customises @c ι. */
  S subobject;
  constexpr Codomain operator()(const Domain& m) const {
    return subobject.ι(m);
  }
};
export template <typename S>
  requires dedekind::category::IsSubobject<
      std::remove_cvref_t<S>, typename std::remove_cvref_t<S>::Domain>
constexpr SubobjectInclusion<std::remove_cvref_t<S>> inclusion_arrow(
    const S& s) {
  return {s};
}

// ── The apex views: meet = pullback, join = pushout in Sub(U) (#881)
// ──────────
//
// A lattice node is a set object structurally (:boundaries) and carries only
// the arrow vocabulary in :category.  The categorical reading of A & B as the
// PULLBACK of the cospan ι_A, ι_B (and of A | B as the PUSHOUT of the span
// from Ø) needs the apex's member shape and its legs, π_1 / π_2 resp. ι1 / ι2
// on members --- set semantics, so they are a view taken here, not a property
// of the node.  The view owns nothing: it evaluates through the node's χ.

/** @brief The pullback apex of an applied meet: the same set, viewed with the
 *  subobject surface and the projection legs @c π_1 / @c π_2 (@c IsPullback).
 */
export template <typename A, typename B>
  requires IsSetObject<Meet<A, B>> && IsSubobject<A, typename A::Domain> &&
           IsSubobject<B, typename B::Domain>
struct PullbackApex {
  Meet<A, B> node;
  using Domain = typename Meet<A, B>::Domain;
  using Codomain = typename Meet<A, B>::Codomain;
  using logic_species = typename Meet<A, B>::logic_species;
  struct Member {
    Domain value;
  };
  constexpr Domain ι(const Member& m) const { return m.value; }
  constexpr typename A::Member π_1(const Member& m) const { return {m.value}; }
  constexpr typename B::Member π_2(const Member& m) const { return {m.value}; }
  constexpr auto operator()(const Domain& x) const { return node(x); }
};
export template <typename A, typename B>
constexpr auto as_pullback(const Meet<A, B>& m) {
  return PullbackApex<A, B>{m};
}

/** @brief The pushout apex of an applied join: the same set, viewed with the
 *  subobject surface and the coprojection colegs @c ι1 / @c ι2
 *  (@c IsPushout).  Dual of @ref PullbackApex. */
export template <typename A, typename B>
  requires IsSetObject<Join<A, B>> && IsSubobject<A, typename A::Domain> &&
           IsSubobject<B, typename B::Domain>
struct PushoutApex {
  Join<A, B> node;
  using Domain = typename Join<A, B>::Domain;
  using Codomain = typename Join<A, B>::Codomain;
  using logic_species = typename Join<A, B>::logic_species;
  struct Member {
    Domain value;
  };
  constexpr Domain ι(const Member& m) const { return m.value; }
  constexpr Member ι1(const typename A::Member& m) const {
    return Member{m.value};
  }
  constexpr Member ι2(const typename B::Member& m) const {
    return Member{m.value};
  }
  constexpr auto operator()(const Domain& x) const { return node(x); }
};
export template <typename A, typename B>
constexpr auto as_pushout(const Join<A, B>& j) {
  return PushoutApex<A, B>{j};
}

/** @brief @c InitialObjectArrow<Init, Target> --- the unique arrow from the
 *  initial object into any subobject (@c Init @c → @c Target), i.e. the empty
 *  function / the @c ⊥-of-Sub(U) leg.  Dual to @c inclusion_arrow: where that
 *  reifies an inclusion into the ambient, this reifies the initiality mediator
 *  out of @c Ø.  Gated on @c category::IsInitialObject<Init> so only a genuine
 *  initial object (@c Ø, tagged) can be the source; the body forwards the
 *  (vacuous) @c Member value and is unreachable in practice (@c Ø has no
 *  members).  Canonical span leg for a pushout @c A @c ← @c Ø @c → @c B,
 *  replacing hand-rolled per-instance span structs.  #881. */
export template <typename Init, typename Target>
  requires dedekind::category::IsInitialObject<Init> &&
           dedekind::category::IsSubobject<Init, typename Init::Domain> &&
           dedekind::category::IsSubobject<Target, typename Target::Domain>
struct InitialObjectArrow {
  using Domain = typename Init::Member;
  using Codomain = typename Target::Member;
  constexpr Codomain operator()(const Domain& m) const { return {m.value}; }
};

// Out-of-class χ definition retired (#681 structural refactor).  The
// @c IsSubobject concept now recognises @c Set as the characteristic
// morphism via its @c operator() call shape, not via a named static
// @c χ member.  Removing the static eliminates the @c Predicate{}
// default-construction cascade that previously blocked comprehension-
// DSL Sets from participating in the @c IsSubobject / Form-chain
// surfaces.

/** @brief Symbolic image-of-intensional-Set predicate (#602 layer 1).
 *
 *  @details For a source intensional @c Set{x | P(x)} and an arrow
 *  @c f: T → U, the categorical image is @c Set{y | ∃x ∈ T. P(x) ∧ y == f(x)}.
 *  On a transfinite carrier T the existential is generally undecidable
 *  (predicate-defined source, no enumerable elements); this predicate
 *  reflects the indecision honestly by returning
 *  @c Kleene::Unknown for every query.
 *
 *  Specializations that decide membership for monic arrows with
 *  structural inverses, or for finite source carriers, are layer 2 of
 *  #602 (per-arrow / per-carrier dispatch).  This default predicate
 *  completes the layer-1 API surface so @c image(f, intensional_set)
 *  is well-formed and type-checked, with the result honestly tagged
 *  @c Kleene.
 *
 *  Captureless / no source-set + arrow storage in the closure: keeps
 *  the predicate value-light and structurally compatible with the
 *  comprehension-DSL paths (captureless predicates stay preferable for
 *  symbolic-image purposes).
 *
 *  @tparam U The codomain element type — read off from @c Cod<F> at
 *  the @c image call site.  Parameterising by @c U keeps the predicate
 *  type distinct per result instantiation.
 */
export template <typename U>
struct SymbolicImagePredicate {
  constexpr auto operator()(const U&) const {
    return dedekind::category::Kleene::Unknown;
  }
};

/** @brief image(f, {x ∈ 𝔸 | P}) — image of an intensional set under
 *         an arrow.  Layer 1 of #602.
 *
 *  @details Sister overload in the @c image dispatch table, alongside
 *   - @c image(f, Singleton) (@c :sets:singleton; extensional, exact).
 *   - @c image(f, std::set / std::unordered_set) (@c :sets:extensional;
 *     extensional, exact via enumeration).
 *   - @b this overload (intensional, symbolic with @c Kleene).
 *
 *  The result is itself an intensional @c Set on the codomain
 *  @c U = Cod<F>, with @c Kleene as the logic species and the
 *  always-Unknown @c SymbolicImagePredicate as the predicate.  This
 *  honestly admits the indecision — the existential @c ∃x ∈ T. P(x)
 *  ∧ y == f(x) is undecidable on transfinite carriers without further
 *  structure (a partial inverse for @c f, or finiteness of @c T).
 *
 *  @c IsArrow<F> is the gate (the project's general arrow concept);
 *  @c IsMonicArrow<F> would be the natural strengthening for the
 *  decidable-specialisation path (layer 2).
 */
export template <dedekind::category::IsArrow F, IsSetObject S>
  requires std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>,
                        typename S::Domain>
constexpr auto image(F&&, const S&) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  return Comprehension<𝔸<U, dedekind::category::Kleene>,
                       SymbolicImagePredicate<U>>{SymbolicImagePredicate<U>{}};
}

/** @brief Composed predicate for the iso-decidable @c image(f, Set)
 *         specialisation below: @c y @c ↦ @c S(f^{-1}(y)) where @c S
 *         is the source set's characteristic and @c f^{-1} is the
 *         iso's inverse.
 *
 *  @details Captures (i) the source set @c S by value and (ii) the
 *  inverse arrow @c FInv by value, so the closure is self-contained
 *  and constexpr-friendly at the call site.  Not default-constructible
 *  in general --- a @c Comprehension stores its @c Predicate by value and
 *  has no default constructor, so callers must always supply both
 *  components to the in-class aggregate initialiser.  The source set's
 *  @c operator() already performs @c lift_logic<L> on the inner
 *  result, and @c S.operator() on the unwrapped @c x returns @c L::Ω
 *  directly, so the composition preserves the source logic species
 *  without going through Ternary.
 *
 *  @c SourceSet is the source set type; @c FInv is the
 *  type returned by @c inverse(f) for the iso @c f.
 */
template <typename SourceSet, typename FInv>
struct ComposedIsoImagePredicate {
  SourceSet source;
  FInv f_inverse;

  template <typename U>
  constexpr auto operator()(const U& y) const {
    return source(f_inverse(y));
  }
};

/** @brief image(iso f, {x ∈ 𝔸 | P}) — @b decidable specialisation for
 *         isomorphism arrows (#602 Layer 2 entry).
 *
 *  @details When @c f admits an inverse @c f^{-1} (the @c IsIsomorphism
 *  gate), the image of an intensional Set under @c f is mechanically
 *  recoverable via predicate composition:
 *
 *  @code
 *    image(f, S) = { y ∈ Cod(f) | S(f^{-1}(y)) }
 *  @endcode
 *
 *  The result is a @c Comprehension<𝔸<U, L>, ComposedIsoImagePredicate<...>>
 *  --- the
 *  same logic species @c L as the source (no demotion to Ternary), and
 *  a composed predicate that evaluates membership through the inverse.
 *  This is strictly stronger than the @c IsArrow fallback above, which
 *  honestly returns the always-Unknown @c SymbolicImagePredicate; iso
 *  arrows preserve decidability of the image because the existential
 *  @c ∃x ∈ T. P(x) ∧ y == f(x) reduces to a single test @c P(f^{-1}(y)).
 *
 *  Overload resolution picks this specialisation over the @c IsArrow
 *  one by partial-ordering (@c IsIsomorphism subsumes @c IsArrow).
 *
 *  @section expressions__Image_Iso_Layer2_Anchor
 *
 *  Layer-2 entry from the @c #602 layering proposal: the
 *  "predicate-set with iso" path that the Layer-1 overload's docstring
 *  flagged as the natural strengthening.  Layer-2 continues with the
 *  retract path immediately below (Case A / #659), and further with
 *  enumerable-source (Case B / #660) and terminal-codomain (Case C /
 *  #661) over follow-up slices.
 */
export template <dedekind::category::IsIsomorphism F, IsSetObject S>
  requires std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>,
                        typename S::Domain>
constexpr auto image(F&& f, const S& s) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  using L = typename S::logic_species;
  // Unqualified call so ADL routes to the inverse overload for f's
  // type (e.g.\ Identity<T>, TaggedNegate) in dedekind::category.
  // This PR exports the relevant inverse overloads (a small boy-scout
  // edit in :morphism, see commit history); the unqualified shape is
  // what the IsIsomorphism concept itself uses, and matches the
  // codebase's ADL-hook style for partial categorical primitives.
  auto f_inv = inverse(std::forward<F>(f));
  using FInv = std::remove_cvref_t<decltype(f_inv)>;
  using NewPredicate = ComposedIsoImagePredicate<S, FInv>;
  return Comprehension<𝔸<U, L>, NewPredicate>{
      NewPredicate{s, std::move(f_inv)}};
}

/** @brief image of the @b unbounded universe under an iso @c F:T→U is the
 *  universe @b of the codomain, @c 𝔸<U>: an iso is surjective, so it fixes the
 *  universe setwise --- but onto @c U, not @c T.  For an endo-iso (@c U==T)
 * this is @c 𝔸<T> unchanged; for a heterogeneous iso (e.g.\ @c Modular<2> → @c
 * bool) it correctly returns @c 𝔸<bool> so callers can query codomain values.
 */
export template <typename T, typename L, typename C,
                 dedekind::category::IsIsomorphism F>
  requires(std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>, T> &&
           !dedekind::category::IsTerminalMorphism<std::remove_cvref_t<F>>)
constexpr auto image(F&&, const 𝔸<T, L, C>&) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  return 𝔸<U, L, C>{};  // iso |U| = |T|, so the cardinality carries
}
// The terminal case (@c id<One>, which is BOTH an iso and the unique One→One
// terminal morphism) is excluded here and handled by the terminal-morphism
// @c image overload in @c :singleton --- otherwise the two identically-typed
// overloads are ambiguous for @c image(id<One>(), 𝔸<One>).

/** @brief Composed predicate for the retract-decidable @c image(f, Set)
 *         specialisation: @c y @c ↦ @c let @c mx @c = @c retract(f)(y);
 *         @c mx.has_value() @c ? @c S(mx.value()) @c : @c L::False.
 *
 *  @details Operational shape:
 *  - When @c retract(f)(y) has a value, the membership query factors
 *    through the source set's @c operator() at that preimage.
 *  - When the retract returns nothing, @c y is structurally outside
 *    @c image(F, S) and the predicate returns @c L::False directly.
 *
 *  Stores the source set and the retract callable by value.  Sister
 *  predicate shape to @c ComposedIsoImagePredicate above (#657 / #602
 *  Case A's iso sibling), differing in that the retract is partial so
 *  the membership query has to case-split on the @c has_value branch.
 */
template <typename SourceSet, typename RetractFn, typename L>
struct ComposedRetractImagePredicate {
  SourceSet source;
  RetractFn retract_fn;

  template <typename U>
  constexpr typename L::Ω operator()(const U& y) const {
    auto mx = retract_fn(y);
    if (mx.has_value()) {
      return source(*mx);
    }
    return L::False;
  }
};

/** @brief image(monic-with-retract f, {x ∈ 𝔸 | P}) --- @b decidable
 *         specialisation for monic arrows that ship a retract (a partial
 *         inverse via the @c retract(f) ADL hook).  #602 Layer 2 / Case A.
 *
 *  @details When @c f is a monomorphism (@c IsMonicArrow) AND ships a
 *  retract @c retract(f) @c : @c Cod(f) @c → @c std::optional<Dom(f)>,
 *  the image of an intensional Set is mechanically recoverable via the
 *  retract:
 *
 *  @code
 *    image(f, S)(y)
 *      = let mx = retract(f)(y);
 *        mx.has_value() ? S(*mx) : L::False
 *  @endcode
 *
 *  The retract contract is @c std::optional -shaped specifically (the
 *  @c IsRetractableArrow concept in @c :morphism gates on
 *  @c std::same_as<std::optional<Dom<F>>>); generalising to the
 *  project's broader @c IsPotential surface (@c Partial<T> /
 *  @c TernaryResult<T> etc.) is a deliberate follow-up.
 *
 *  This is independent of the @c IsIsomorphism specialisation above
 *  (#657 sister): although every iso has a structural total retract,
 *  this PR does @b not auto-register a @c retract hook for arbitrary
 *  iso arrows, so iso routes through the @c IsIsomorphism overload
 *  unchanged.  The retract path handles the monic-but-not-iso case ---
 *  most carrier-lattice embeddings in the project (@c embed_𝔹_ℕ,
 *  @c embed_uint_ℕ, @c embed_sint_ℤ, ...) are monic and admit a
 *  natural retract, and can opt in to this path by registering a
 *  @c retract overload alongside their existing @c is_monic_arrow_v
 *  declaration.
 *
 *  The result keeps the source's logic species @c L (no demotion to
 *  Ternary) and stores the composed predicate carrying both the source
 *  set and the retract callable.
 *
 *  @par Disambiguation against the iso overload
 *  The @c !IsIsomorphism guard in the requires-clause keeps iso arrows
 *  on the simpler unconditional-inverse path above (#657) in the
 *  hypothetical case where a user did opt an iso arrow into the
 *  retract path (by registering a retract hook on it).  Without that
 *  guard, an iso arrow that also happened to be retractable would be
 *  ambiguous between the two overloads; with it, iso always wins.
 */
export template <dedekind::category::IsRetractableArrow F, IsSetObject S>
  requires std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>,
                        typename S::Domain> &&
           (!dedekind::category::IsIsomorphism<std::remove_cvref_t<F>>)
constexpr auto image(F&& f, const S& s) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  using L = typename S::logic_species;
  // Unqualified call so ADL routes to the retract overload registered
  // for f's type in the appropriate namespace.  The IsRetractableArrow
  // concept guarantees the call is well-formed.
  auto retract_fn = retract(std::forward<F>(f));
  using RetractFn = std::remove_cvref_t<decltype(retract_fn)>;
  using NewPredicate = ComposedRetractImagePredicate<S, RetractFn, L>;
  return Comprehension<𝔸<U, L>, NewPredicate>{
      NewPredicate{s, std::move(retract_fn)}};
}

// A NON-injective arrow's image is decided ANALYTICALLY, point-free, as the
// union of its (mono) branch images --- @c image on the reflection branches of
// the sign-fold @c abs in @c dedekind.order (§3.3, Listing 13).  There is no
// operational retract/cofibre fibre-walk here: the monic @c IsRetractableArrow
// overload above is the only retract path, and it is a single lookup.

// The set complement operators @c ! / @c ~ are free combinators over IsSet,
// defined right after the Set class (with @c & / @c |); see there.

/** @brief @c Set @c ^ @c Ø @c = @c Set (symmetric difference with empty
 *         is identity).  Symmetric of @c Ø::operator^(S): picks up
 *         @c S @c ^ @c Ø when the boundary is on the right, keeping the
 *         structural collapse type-level.  The boundary's species tag is
 *         immaterial (∅ is decided), so it is not matched against the
 *         set's. */
export template <typename T, typename L, typename Predicate, typename C,
                 typename LB>
constexpr auto operator^(const Comprehension<𝔸<T, L, C>, Predicate>& s,
                         const Ø<T, LB>&) {
  return s;
}

/** @brief @c Set @c ^ @c 𝔸 @c = @c ¬Set (symmetric difference with the
 *         universe is the complement).  Symmetric of @c 𝔸::operator^(S);
 *         the universe's species tag is immaterial (⊤ is decided).  @c ~ is
 *         the SET complement (a set object); @c ! would be the predicate
 *         complement, an arrow. */
export template <typename T, typename L, typename C, typename Predicate,
                 typename LB, typename CB>
constexpr auto operator^(const Comprehension<𝔸<T, L, C>, Predicate>& s,
                         const 𝔸<T, LB, CB>&) {
  return ~s;
}

/** @section expressions__Comprehension_Codomain
 *
 * A comprehension's codomain is coherent by construction: its species is
 * @c comprehension_logic_t, the semilattice join of the base's species and the
 * species read off the predicate's actual answer type (@c answer_logic), and
 * @c operator() lifts both answers into that join before the conjunction.  A
 * predicate whose answer names no species, or whose species has no join with
 * the base's, is refused at the type rather than mis-typed @c Boole.
 */

/** @section expressions__Logical_Lifting */

/**
 * @brief Downstream specialization hook for structured predicate conjunction.
 * @details
 * If a downstream module defines a free `structured_and(p1, p2)` overload
 * discoverable via ADL for the predicate types, `operator&&` will dispatch to
 * that overload. Otherwise it falls back to an opaque lambda composition.
 */
template <typename P1, typename P2>
concept HasStructuredAnd =
    requires(const std::decay_t<P1>& p1, const std::decay_t<P2>& p2) {
      { structured_and(p1, p2) };
    };

/**
 * @brief Downstream specialization hook for structured predicate disjunction.
 * @details Mirrors `HasStructuredAnd` through `structured_or(p1, p2)`.
 */
template <typename P1, typename P2>
concept HasStructuredOr =
    requires(const std::decay_t<P1>& p1, const std::decay_t<P2>& p2) {
      { structured_or(p1, p2) };
    };

/** @brief A term of the set grammar, the operand sort of @c && / @c ||: an
 *  arrow (a predicate, a set, a node over arrows) or a point-free relational
 *  fragment (@c π_1 @c ⋈ @c π_2, tagged @c is_rel_predicate, which meets its
 *  domain only at the former @c &).  A bare truth value is not a term:
 *  @c bool @c && @c bool stays the host language's, and a constant beside a
 *  predicate lifts through @c :topoi.
 *  @tparam P the operand. */
export template <typename P>
concept IsLogicalTerm = IsArrow<std::remove_cvref_t<P>> || requires {
  typename std::remove_cvref_t<P>::is_rel_predicate;
};

/** @brief Predicate-level conjunction @c p1 @c && @c p2: @c structured_and when
 *  a domain collapse applies, else the reducer's @c Meet node, a callable
 *  predicate @f$\chi_1 \wedge \chi_2@f$ that also carries its operands via
 *  @c π_1 / @c π_2.  It is the @b same AST the set-level combinators use, so a
 *  set-builder @c Set{x @c | @c p1 @c && @c p2} shares one representation with
 *  @c A @c & @c B.  There is no separate @c AndPredicate.  A pair of
 *  predicates over one species is @c :topoi's conjunction (@c IsPredicatePair,
 *  which closes on @c IsPredicate); this one takes every other pair of terms.
 *  @tparam P1 the left term.  @tparam P2 the right term. */
export template <IsLogicalTerm P1, IsLogicalTerm P2>
  requires(!IsPredicatePair<P1, P2>)
constexpr auto operator&&(P1&& p1, P2&& p2) {
  if constexpr (IsRelPredicate<P1> && IsRelPredicate<P2>) {
    // Two relational predicates conjoin pointwise and stay relational, so the
    // result binds at a former like each of them does.
    return RelAnd<std::decay_t<P1>, std::decay_t<P2>>{std::forward<P1>(p1),
                                                      std::forward<P2>(p2)};
  } else if constexpr (HasStructuredAnd<P1, P2>) {
    return structured_and(std::forward<P1>(p1), std::forward<P2>(p2));
  } else {
    return Meet<std::decay_t<P1>, std::decay_t<P2>>{std::forward<P1>(p1),
                                                    std::forward<P2>(p2)};
  }
}

static_assert(!IsLogicalTerm<bool> && !IsLogicalTerm<int>,
              "a bare truth value is not a term: bool && bool is C++'s.");

/** @brief Predicate-level disjunction @c p1 @c || @c p2, dual to @c operator&&:
 *  @c structured_or when a collapse applies, else the reducer's @c Join node;
 *  a pair of predicates over one species is @c :topoi's (@c IsPredicatePair).
 *  @tparam P1 the left term.  @tparam P2 the right term. */
export template <IsLogicalTerm P1, IsLogicalTerm P2>
  requires(!IsPredicatePair<P1, P2>)
constexpr auto operator||(P1&& p1, P2&& p2) {
  if constexpr (IsRelPredicate<P1> && IsRelPredicate<P2>) {
    return RelOr<std::decay_t<P1>, std::decay_t<P2>>{std::forward<P1>(p1),
                                                     std::forward<P2>(p2)};
  } else if constexpr (HasStructuredOr<P1, P2>) {
    return structured_or(std::forward<P1>(p1), std::forward<P2>(p2));
  } else {
    return Join<std::decay_t<P1>, std::decay_t<P2>>{std::forward<P1>(p1),
                                                    std::forward<P2>(p2)};
  }
}

}  // namespace dedekind::sets

namespace dedekind::sets {

// The relation CORE --- the @c Relation / @c SetFunction aliases, the
// @c IsRelation concept, and the @c relates / @c dom / @c cod / @c apply /
// @c is_single_valued_at query surface --- moved OUT of @c :sets into
// @c dedekind.relational:dyadic (#792 follow-up).  A relation is a downstream
// concept (@c category → @c sets → @b relational), so keeping its type and
// query surface in @c sets inverted the layering; breaking that cycle is the
// point.  @c sets keeps only the @b powerset (below), which is genuine
// set-theory, not relation algebra.

/** @brief A set-shaped carrier: exposes the ambient @c Domain and the
 *  @c logic_species.  The gate constraint for the power-set customization
 *  point; the ordered specialisation conjoins it (so it @b subsumes this gate
 *  and wins by partial ordering for the carriers it handles). */
export template <typename S>
concept SetShaped = requires {
  typename std::remove_cvref_t<S>::Domain;
  typename std::remove_cvref_t<S>::logic_species;
};

/**
 * @brief Power set @f$\mathfrak{P}(S)@f$ --- the @b default declaration
 * (customization-point gate).
 *
 * @details Establishes the @c :sets-level signature for @c power_set / @c 𝔓 and
 * is @c =delete d, so a @b set-shaped base with no decidable power set is a
 * clean type error (type-check failure by default).  The decidable
 * specialisations live downstream, where the subset order does:
 * @c dedekind.order:powerset gives the ordered / convex case
 * (@f$\mathfrak{P}(S) = \mathbb{A}\langle\mathrm{Sub}(C)\rangle \mid X
 * \subseteq S@f$ over the subobject domain @c Sub(C), an interval), covering @c
 * 𝔸 /
 * @c Singleton / @c Halfspace / @c Interval by coercion; a finite-carrier
 * / erased case may follow (#830).  The one exception is @f$\mathfrak{P}
 * (\emptyset) = \{\emptyset\}@f$, which needs no subobject domain and is a
 * closed form here in @c :sets (below).  Same shape as @c exists / @c forall
 * (an
 * algebraic default lifted by per-carrier decidable specialisations).  A
 * non-set-shaped argument fails @c SetShaped and matches nothing (also a type
 * error).
 * @see Lambek and Scott @cite lambek1988higher
 */
export template <typename S>
  requires SetShaped<S>
auto power_set(const S&) = delete;
/** @brief Textbook fraktur-P alias for @c power_set (the deleted default gate).
 *  Mirrored on the Python side as @c dedekind.sets.𝔓. */
export template <typename S>
  requires SetShaped<S>
auto 𝔓(const S&) = delete;

/**
 * @brief @f$\mathfrak{P}(\emptyset) = \{\emptyset\}@f$ --- the one power set
 * that needs no subobject normal-form, so it is a closed form here in @c :sets.
 *
 * @details The empty set has exactly one subset (itself), so its power set is
 * the @b singleton @f$\{\emptyset\}@f$ (cardinality @f$1 = 2^0@f$, @b not
 * @f$\emptyset@f$).  That singleton is exactly the universe over the
 * one-inhabitant domain @c Ø<T,L>: every empty set is equal to every other
 * (@c Ø's cross-carrier @c ==), so @c 𝔸 over the empty-set carrier has a single
 * inhabitant, @f$\emptyset@f$ itself.  No @c Sub, no ordered carrier, no
 * @c Rice-walled subobject enumeration --- which is why the @c empty node of
 * the grammar can be discharged upstream of the interval specialisation.  This
 * is a more-specialised overload than the deleted @c SetShaped gate, so it wins
 * by partial ordering.
 */
export template <typename T, typename L>
constexpr auto power_set(const Ø<T, L>&) {
  return 𝔸<Ø<T, L>, L, Finite>{};
}
export template <typename T, typename L>
constexpr auto 𝔓(const Ø<T, L>&) {
  return power_set(Ø<T, L>{});
}

// NOTE: the relation query surface (@c relates / @c dom / @c cod / @c apply /
// @c is_single_valued_at) moved to @c dedekind.relational:dyadic alongside the
// @c Relation type --- see the relation-core note above the powerset.

/** @section expressions__SetExpr_Witnesses
 *  A @ref Comprehension @b is a set --- @c IsSet is reached by inheriting
 *  @ref SetExpr and supplying the χ.  Witnessed over the universal and empty
 *  bases (the two extremes of the subobject lattice); the same holds for any
 *  @c IsSet base a comprehension refines. */
}  // namespace dedekind::sets

// ── Functional and entire by structure (Table 3): a graph atom is the graph
// of x ↦ t(x), so functional; entire too where t is total on the first
// carrier and lands in the second.
namespace dedekind::category {

template <typename A, typename B, typename L, typename C, typename P>
  requires dedekind::sets::IsGraphAtom<P>
inline constexpr bool is_right_unique_v<dedekind::sets::Comprehension<
    dedekind::sets::𝔸<std::pair<A, B>, L, C>, P>> = true;

template <typename A, typename B, typename L, typename C, typename P>
  requires dedekind::sets::IsGraphAtom<P>
inline constexpr bool is_left_total_v<dedekind::sets::Comprehension<
    dedekind::sets::𝔸<std::pair<A, B>, L, C>, P>> =
    std::same_as<dedekind::sets::value_at_t<std::pair<A, B>,
                                            dedekind::sets::graph_term_t<P>>,
                 B> &&
    dedekind::sets::is_total_term_v<std::pair<A, B>,
                                    dedekind::sets::graph_term_t<P>>;

}  // namespace dedekind::category

namespace dedekind::sets {

namespace detail_setexpr_witness {
struct all_in {
  constexpr bool operator()(int) const { return true; }
};
static_assert(IsSet<Comprehension<𝔸<int>, all_in>>,
              "{𝔸 | P} is a first-class set: IsSet by SetExpr + its own χ.");
static_assert(IsSet<Comprehension<Ø<int>, all_in>>,
              "{Ø | P} is a first-class set.");

// #834/#829: a bare Comprehension is an IsSubobject, so ~ (the set complement)
// is the reducer's Not node over it --- a genuine set-complement subobject, not
// a formal arrow.  ~~ peels back to the Comprehension (the involution).
using CompN = Comprehension<𝔸<int>, all_in>;
static_assert(
    std::same_as<std::remove_cvref_t<decltype(~std::declval<CompN>())>,
                 dedekind::category::Not<CompN>>,
    "~(A & pred) is the set-complement Not<Comprehension>.");
static_assert(
    std::same_as<std::remove_cvref_t<decltype(~~std::declval<CompN>())>, CompN>,
    "~~(A & pred) peels back to the Comprehension (involution).");

// #834: the bare ETCS subobject (classify / ambient_set) complements the same
// way --- ~s is the Not node over it, a set-complement subobject (what the
// retired set_complement produced), not a formal arrow.
using SubN = std::remove_cvref_t<decltype(classify<int>(all_in{}))>;
static_assert(
    IsSetObject<std::remove_cvref_t<decltype(~std::declval<SubN>())>>,
    "~classify(f) is a set object (the complement node, structurally), not a "
    "formal arrow.");
}  // namespace detail_setexpr_witness

}  // namespace dedekind::sets
