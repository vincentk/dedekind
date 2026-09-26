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
 * Set<T, L, Predicate> -- an intensional set whose membership test is a
 * compile-time callable predicate ranging into a subobject classifier L::Omega.
 *
 * Key constructs exported:
 *  - Set<T,L,P>           -- ETCS-compatible intensional set.
 *  - Comprehension<B,P>   -- the point-free set-builder node {x in B | P(x)};
 *                            the retired scout element/BoundScout is gone
 * (#895).
 *  - Boolean connectives &&, ||, ! lifted to predicate combinators.
 *  - operator<=           -- subset relation (same-predicate -> True;
 *                            heterogeneous -> Unknown via Kleene).
 *  - cartesian_product    -- A x B as a Set of pairs.
 *  - Relation, SetFunction -- subobjects of products.
 *  - relates, is_single_valued_at -- point-wise witnesses.
 *  - power_set / 𝔓         -- default (deleted) gate here; the ordered/convex
 *                            specialisation is dedekind.order:powerset
 * (#830).
 *
 * @section expressions__Canonical_Examples
 * ```cpp
 * const auto xs = ℕ | (χ < fix(512_c));  // {n in ℕ | n < 512}
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
import :mereology;      // For mereology lattice concepts
import :computability;  // For NaturalLogic / HasDecidableMembership

namespace dedekind::sets {
using namespace dedekind::category;

// ── What a set object IS (actual representation) ─────────────────────────────
// The object-level "is a set" concept is @c IsSubobject<T, T::Domain>: @c T
// classified by a characteristic predicate χ (its @c operator()).  In the
// carriers this is stored as JUST the classifier --- @c Subobject<A, Chi> holds
// @c Chi χ (category:topoi), @c Set<T,L,P> holds @c P predicate_ --- and the
// ambient is the @b type parameter (@c A / @c T), not a stored value; @c ι is
// the @c Member inclusion (@c ι(Member{a}) = @c a), not a projection.  @c
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
 *  and @c Set<T,L,P> / @c SingletonSet keep satisfying it by hand.  The
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

export template <typename Base, typename Predicate>
struct Comprehension
    : SetExpr<Comprehension<Base, Predicate>, typename Base::Domain,
              typename Base::logic_species> {
  Base base;  // by VALUE: a comprehension OWNS its base.  A reference member
              // would dangle when the constructor binds an rvalue base (the
              // aggregate form extended the temporary's lifetime; a constructor
              // parameter does not).
  Predicate predicate;

  /** @brief Explicit two-argument constructor.  Needed because @ref SetExpr is
   * a base class, so @c Comprehension is no longer an aggregate --- the
   *  @c Comprehension{base, pred} sites (e.g. @c SingletonSet::operator|) route
   *  here instead of through aggregate init. */
  constexpr Comprehension(const Base& b, Predicate p)
      : base(b), predicate(static_cast<Predicate&&>(p)) {}

  // Forward Base's cardinality so NaturalLogic / IsCountable can read off
  // the carrier axis on a comprehension's effective magnitude.  A
  // predicate-restricted comprehension is at most as large as its base
  // (P-restriction can only shrink the membership set), so inheriting the
  // base's cardinality bound is sound for the carrier-axis resolver
  // (#622).  Sharper bounds (singleton-bounded comprehensions etc.) are
  // tracked via the @c size() probe below, not via this typedef.
  using cardinality_type = typename Base::cardinality_type;

  /** @brief χ: the comprehension's characteristic map --- @c x @c ∈ @c {S @c |
   *  @c P} @c ⟺ @c x @c ∈ @c S @c ∧ @c P(x).  A comprehension @b is a set, so
   * it
   *  @b is its own predicate (@c IsSet @c ⟹ @c IsPredicate). */
  constexpr auto operator()(const typename Base::Domain& x) const {
    // Combine under the base's logic (@c L::AND), first @b lifting the
    // (commonly
    // @c bool) predicate result into @c L::Ω.  A @c bool cast would collapse
    // ternary membership (@c Ternary::False, underlying −1, reads as @c true),
    // and @c L::AND(Ternary, bool) is ill-formed (@c Ternary is a scoped enum),
    // so a @c Kleene base needs the lift.  A comprehension @c {S|P} is
    // S-membership ∧ P.
    using L = typename Base::logic_species;
    const auto p = predicate(x);
    if constexpr (std::same_as<std::remove_cvref_t<decltype(p)>, typename L::Ω>)
      return L::AND(base(x), p);
    else
      return L::AND(base(x), p ? L::True : L::False);
  }

  /** @brief Size when the base exposes a probe element (@c pivot) and a
   *         @c size().  For singleton-bounded bases (size 1), the
   *         predicate is probed once at @c base.pivot and the result
   *         is @c base.size() if the probe holds, @c 0 otherwise.
   *         Larger enumerable bases would need iteration — out of
   *         scope here (FIXME(#685)). */
  constexpr std::size_t size() const
    requires requires(const Base& b, const Predicate& p) {
      b.pivot;
      b.size();
      { p(b.pivot) } -> std::convertible_to<bool>;
    }
  {
    return predicate(base.pivot) ? base.size() : 0;
  }
};

/** @brief Boolean equality predicate for compile-time pruning over 𝔹.
 *
 *  Defined here (rather than further down where the @c FiniteBooleanSet
 *  collapse machinery lives) because the bool-truthy comprehension form
 *  @c 𝔹 @c | @c BooleanEqPredicate{true} needs the type complete (#408).
 *  The collapse-machinery uses further down still see the same definition:
 *  it is the single source of truth for the bool-domain predicate.
 */
export struct BooleanEqPredicate {
  bool expected;

  constexpr bool operator()(bool v) const { return v == expected; }
};

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

// The codomain-leg value finalizer `finalize_combine` (#894) is hoisted to
// `:boundaries` (beside `codomain_reduce_t`), so the upstream boundary
// operators and the general subobject operators below share ONE implementation
// of the value-finalization law (boundary output → decided Boole codomain; all
// else passes through) instead of duplicating it.  Reachable here through
// `import :boundaries`; a new expressions-level operator needs only
// `return finalize_combine(...)` and inherits the rule.

/** @brief The join of two logic species (#894): 𝔹 ⊑ K₃ under the dominance, so
 *  the more expressive Ω --- K₃ if either operand is K₃, else 𝔹.  The codomain
 * a cross-species combine reduces at. */
export template <typename L1, typename L2>
using join_logic_t =
    std::conditional_t < std::same_as<L1, dedekind::category::Kleene> ||
    std::same_as<L2, dedekind::category::Kleene>,
      dedekind::category::Kleene, dedekind::category::Boole > ;

/** @brief A subobject re-tagged to a more expressive codomain @c TargetL: its χ
 *  lifts through the Rosolini dominance (@c lift_logic) into @c TargetL::Ω.
 *
 *  @details Used to bring the operands of a cross-species combine to one
 * codomain before the reducer folds them (#894): the same-species meet/join
 * needs both χ valued in one Ω, and a mixed @c Meet term would shred the
 * reducer.  This is the
 *  @b naive lift --- it does @b not preserve the operand's structural type
 *  (interval / halfspace), so a cross-species combine materialises as a
 *  @c MeetSet / @c JoinSet (membership correct, pointwise) rather than
 *  structurally collapsing.  The structure-preserving lift + codomain-follows-
 *  normal-form is the follow-up (see #894). */
export template <typename S, typename TargetL>
  requires dedekind::category::LiftsTo<typename S::logic_species, TargetL>
struct SpeciesLifted
    : SetExpr<SpeciesLifted<S, TargetL>, typename S::Domain, TargetL> {
  /** @brief The wrapped subobject, whose χ is lifted into @c TargetL::Ω. */
  S base;
  /** @brief Wrap @c s; the lift is a no-op semantically, only the codomain
   *  advertised by @c operator() changes. */
  constexpr explicit SpeciesLifted(S s) : base(std::move(s)) {}
  /** @brief χ at @c x, lifted through the dominance into @c TargetL::Ω. */
  constexpr typename TargetL::Ω operator()(const typename S::Domain& x) const {
    return dedekind::category::lift_logic<TargetL>(base(x));
  }
};

/** @brief Bring a subobject to codomain @c TargetL: identity when it is already
 *  there, else wrap it in @c SpeciesLifted.  Constrained to a registered
 *  dominance inclusion (@c LiftsTo), so a downward or unsupported lift (e.g.
 *  @c lift_to<Boole> of a Kleene set) is rejected at the gate rather
 *  than failing inside @c lift_logic. */
export template <typename TargetL, typename S>
  requires dedekind::category::LiftsTo<
      typename std::remove_cvref_t<S>::logic_species, TargetL>
constexpr auto lift_to(const S& s) {
  if constexpr (std::same_as<typename std::remove_cvref_t<S>::logic_species,
                             TargetL>) {
    return s;
  } else {
    return SpeciesLifted<std::remove_cvref_t<S>, TargetL>{s};
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

export template <typename T, typename L, typename Predicate>
class Set;

/** @brief Extensional finite bool-domain result for collapsed 𝔹 operations. */
export template <typename L>
struct FiniteBooleanSet {
  using Domain = bool;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = Finite;

  typename L::Ω at_false;
  typename L::Ω at_true;

  constexpr typename L::Ω operator()(bool v) const {
    return v ? at_true : at_false;
  }

  constexpr bool operator==(const Ø<bool, L>&) const {
    return at_false == L::False && at_true == L::False;
  }

  constexpr bool operator==(const UniversalSet<bool, L, Finite>&) const {
    return at_false == L::True && at_true == L::True;
  }

  friend constexpr bool operator==(const Ø<bool, L>& empty,
                                   const FiniteBooleanSet& s) {
    return s == empty;
  }

  friend constexpr bool operator==(
      const UniversalSet<bool, L, Finite>& universe,
      const FiniteBooleanSet& s) {
    return s == universe;
  }

  constexpr auto operator|(const FiniteBooleanSet& other) const {
    return FiniteBooleanSet{
        L::OR(at_false, other.at_false),
        L::OR(at_true, other.at_true),
    };
  }

  constexpr auto operator&(const FiniteBooleanSet& other) const {
    return FiniteBooleanSet{
        L::AND(at_false, other.at_false),
        L::AND(at_true, other.at_true),
    };
  }
};

export template <typename L>
constexpr auto operator|(const Set<bool, L, BooleanEqPredicate>& lhs,
                         const FiniteBooleanSet<L>& rhs) {
  return FiniteBooleanSet<L>{
      L::OR(lhs(false), rhs(false)),
      L::OR(lhs(true), rhs(true)),
  };
}

export template <typename L>
constexpr auto operator|(const FiniteBooleanSet<L>& lhs,
                         const Set<bool, L, BooleanEqPredicate>& rhs) {
  return rhs | lhs;
}

export template <typename L>
constexpr auto operator&(const Set<bool, L, BooleanEqPredicate>& lhs,
                         const FiniteBooleanSet<L>& rhs) {
  return FiniteBooleanSet<L>{
      L::AND(lhs(false), rhs(false)),
      L::AND(lhs(true), rhs(true)),
  };
}

export template <typename L>
constexpr auto operator&(const FiniteBooleanSet<L>& lhs,
                         const Set<bool, L, BooleanEqPredicate>& rhs) {
  return rhs & lhs;
}

/** @brief @c bool @c BooleanEqPredicate meet.  @c BooleanEqPredicate is
 *  RUNTIME-stateful (same TYPE, different @c expected field), so the generic
 *  reducer's TYPE-based idempotent law would wrongly collapse two distinct bool
 *  singletons.  Compute the finite meet directly, more specialised than the
 *  generic @c IsSubobject combinators, so it wins; a finite bool set is
 *  extensional, so the result is a @c FiniteBooleanSet. */
export template <typename L>
constexpr auto operator&(const Set<bool, L, BooleanEqPredicate>& a,
                         const Set<bool, L, BooleanEqPredicate>& b) {
  return FiniteBooleanSet<L>{L::AND(a(false), b(false)),
                             L::AND(a(true), b(true))};
}
/** @brief @c bool @c BooleanEqPredicate join, dual to the meet above. */
export template <typename L>
constexpr auto operator|(const Set<bool, L, BooleanEqPredicate>& a,
                         const Set<bool, L, BooleanEqPredicate>& b) {
  return FiniteBooleanSet<L>{L::OR(a(false), b(false)),
                             L::OR(a(true), b(true))};
}

// ---------------------------------------------------------------------------
// Cross-carrier meet on the variant pair (existential proof, slice of #362)
// ---------------------------------------------------------------------------
//
// When two sets are intersected and their carriers are related by the
// canonical embedding ℕ ↪ ℤ, the result's carrier should be the @b
// tighter of the two — the library picks the pullback along the
// embedding.  Mathematically:
//
//   A ⊂ ℕ,  B ⊂ ℤ,  ℕ ↪ ℤ
//   ⇒  A ∩ B ⊂ ℕ  (not ⊂ ℤ).
//
// This is the @b carrier @b strength-reduction rule: the carrier
// "lattice" (ℕ < ℤ < ℚ < ℝ < ℂ) drives the resulting carrier of binary
// set operations.  #362 tracks the @b general framework (any pair in
// the lattice); the overloads below land the @b existential @b proof
// for the variant pair (Cardinality, SignedCardinality), which is the
// load-bearing case for Paper 3's type-directed-collapse story.
//
// The result predicate evaluates @c lhs(v) @c && @c rhs(lift(v)) where
// @c v has the smaller carrier's type and @c lift is the exported
// @c lift_cardinality_to_signed from @c :cardinality (the single
// source of truth for the variant-level ℕ ↪ ℤ embedding; reachable
// here because @c :expressions imports @c :cardinality).
//
// Future iterations under #362 will replace this hand-coded pair with
// a @c carrier_lattice_meet_t<T1, T2> trait and a generic overload
// that dispatches across the full lattice.  See
// @c docs/design/carrier-lattice.md for the design discussion.

/** @brief Cross-carrier meet @c Set<Cardinality> @c & @c
 *         Set<SignedCardinality> @c → @c Set<Cardinality> (carrier
 *         strength-reduction; closes a slice of #362).  The
 *         intersection is contained in ℕ, so the result carrier is
 *         @c Cardinality. */
export template <typename L, typename P1, typename P2>
constexpr auto operator&(const Set<Cardinality, L, P1>& lhs,
                         const Set<SignedCardinality, L, P2>& rhs) {
  auto predicate = [lhs, rhs](const Cardinality& v) {
    return L::AND(lhs(v), rhs(lift_cardinality_to_signed(v)));
  };
  return Set<Cardinality, L, decltype(predicate)>{predicate};
}

/** @brief Symmetric: @c Set<SignedCardinality> @c & @c
 *         Set<Cardinality> delegates to the canonical direction.  The
 *         result still tightens to @c Set<Cardinality>. */
export template <typename L, typename P1, typename P2>
constexpr auto operator&(const Set<SignedCardinality, L, P1>& lhs,
                         const Set<Cardinality, L, P2>& rhs) {
  return rhs & lhs;
}

namespace detail {
/** @brief Helpers for the partial inverse of @c
 *         lift_cardinality_to_signed (the canonical ℕ ↪ ℤ embedding).
 *
 *  @c sc_is_in_natural_image(v) returns whether @c v lies in the
 *  image of ℕ inside @c SignedCardinality — equivalently, whether
 *  @c v is neither negative finite, @c -ℵ_0, nor @c NaZ.  When that
 *  predicate holds, @c project_signed_to_natural(v) projects @c v
 *  back to @c Cardinality.  Calling @c project_signed_to_natural on
 *  values outside the image violates its precondition.
 *
 *  This is the operational shadow of "ℕ ⊂ ℤ as a partial reverse":
 *  the injection ℕ ↪ ℤ has a partial inverse on the non-negative
 *  fragment of ℤ.  @c std::optional is intentionally not used here —
 *  the predicate-then-project split keeps the cross-carrier @c
 *  operator| below in pure @c constexpr-friendly territory and lets
 *  the predicate be reused on the @c Set's branching choice. */
constexpr bool sc_is_negative_finite(const SignedCardinality& v) noexcept {
  if (!std::holds_alternative<SignedExtensionalCardinal<>>(v)) return false;
  return std::get<SignedExtensionalCardinal<>>(v).negative;
}

constexpr bool sc_is_in_natural_image(const SignedCardinality& v) noexcept {
  if (std::holds_alternative<NaZ>(v)) return false;
  if (std::holds_alternative<NegativeInfinity>(v)) return false;
  if (sc_is_negative_finite(v)) return false;
  return true;
}

constexpr Cardinality project_signed_to_natural(
    const SignedCardinality& v) noexcept {
  // Pre: sc_is_in_natural_image(v) holds.
  if (std::holds_alternative<PositiveInfinity>(v)) {
    return Cardinality{ℵ_0{}};
  }
  return Cardinality{std::get<SignedExtensionalCardinal<>>(v).magnitude};
}
}  // namespace detail

/** @brief Cross-carrier join @c Set<Cardinality> @c | @c
 *         Set<SignedCardinality> @c → @c Set<SignedCardinality>
 *         (carrier widening; closes a slice of #362).  The union may
 *         contain negative integers from the @c rhs side, so the
 *         result carrier widens to ℤ.
 *
 *  Membership for @c v @c : @c ℤ:
 *    * If @c v lives in the image of ℕ ↪ ℤ (non-negative, not @c NaZ,
 *      not @c -ℵ_0), evaluate @c lhs on the projected ℕ value and OR
 *      with @c rhs(v).
 *    * Otherwise @c v is not in ℕ, so @c lhs(v) is structurally @c
 *      false; the union reduces to @c rhs(v). */
export template <typename L, typename P1, typename P2>
constexpr auto operator|(const Set<Cardinality, L, P1>& lhs,
                         const Set<SignedCardinality, L, P2>& rhs) {
  auto predicate = [lhs, rhs](const SignedCardinality& v) {
    if (detail::sc_is_in_natural_image(v)) {
      return L::OR(lhs(detail::project_signed_to_natural(v)), rhs(v));
    }
    return rhs(v);
  };
  return Set<SignedCardinality, L, decltype(predicate)>{predicate};
}

/** @brief Symmetric: @c Set<SignedCardinality> @c | @c Set<Cardinality>
 *         delegates to the canonical direction.  The result still
 *         widens to @c Set<SignedCardinality>. */
export template <typename L, typename P1, typename P2>
constexpr auto operator|(const Set<SignedCardinality, L, P1>& lhs,
                         const Set<Cardinality, L, P2>& rhs) {
  return rhs | lhs;
}

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
 *  finite / static-singleton reduction is itself a set-like leaf (returned
 *  bare); any other reduction is a named predicate wrapped back into a @c Set.
 *  (Extracted verbatim from the pre-reducer @c operator& structured_and branch
 *  so @c SetCombine's type and this value stay in lockstep.) */
export template <typename T, typename L, typename Reduced>
constexpr auto elevate_meet(Reduced reduced) {
  using Result = std::decay_t<Reduced>;
  if constexpr (std::same_as<Result, EmptyPredicate<T>>) {
    return Ø<T, L>{};
  } else if constexpr (requires { typename Result::is_static_singleton_tag; }) {
    return reduced;
  } else if constexpr (requires { typename Result::cardinality_type; }) {
    // Nested (not &&-chained): a Result without cardinality_type must not
    // instantiate the inner probe.
    if constexpr (std::same_as<typename Result::cardinality_type, Finite>) {
      return reduced;
    } else {
      return Set<T, L, Result>{std::move(reduced)};
    }
  } else {
    return Set<T, L, Result>{std::move(reduced)};
  }
}

/** @brief Value-level elevate of a @c structured_or result to the join's
 *  normal-form value: a covering pair is the universe @c 𝔸 (returned bare);
 *  any other reduction (a wider halfspace) is wrapped back into a @c Set.
 *  Dual of @c elevate_meet. */
export template <typename T, typename L, typename Reduced>
constexpr auto elevate_join(Reduced reduced) {
  using Result = std::decay_t<Reduced>;
  if constexpr (std::same_as<Result, UniversalSet<T, L>>) {
    return reduced;
  } else {
    return Set<T, L, Result>{std::move(reduced)};
  }
}

namespace detail_reducer {
/** @brief Type-level companion of @c elevate_meet: the leaf @c SetCombine::meet
 *  yields for a combinable set-leaf pair, or @c law_inactive when the leaves
 *  are not both sets over one carrier or no @c structured_and applies. */
template <typename RA, typename RB>
struct combine_meet {
  using type = law_inactive;
};
template <typename T, typename L, typename PA, typename PB>
  requires requires(const PA& a, const PB& b) { structured_and(a, b); }
struct combine_meet<Set<T, L, PA>, Set<T, L, PB>> {
  using type = decltype(elevate_meet<T, L>(
      structured_and(std::declval<const PA&>(), std::declval<const PB&>())));
};

template <typename RA, typename RB>
struct combine_join {
  using type = law_inactive;
};
template <typename T, typename L, typename PA, typename PB>
  requires requires(const PA& a, const PB& b) { structured_or(a, b); }
struct combine_join<Set<T, L, PA>, Set<T, L, PB>> {
  using type = decltype(elevate_join<T, L>(
      structured_or(std::declval<const PA&>(), std::declval<const PB&>())));
};
}  // namespace detail_reducer

/** @brief The injected leaf-combiner (the reducer's 4th @c reduce<> policy) for
 *  the subobject lattice: at an order-incomparable residual, hand the two set
 *  leaves to the carrier's domain @c ∧ / @c ∨ (@c structured_and /
 *  @c structured_or via ADL) and let the reducer re-reduce the result. */
export struct SetCombine {
  template <typename RA, typename RB>
  static consteval auto meet() {
    return std::type_identity<
        typename detail_reducer::combine_meet<RA, RB>::type>{};
  }
  template <typename RA, typename RB>
  static consteval auto join() {
    return std::type_identity<
        typename detail_reducer::combine_join<RA, RB>::type>{};
  }
};

/** @section expressions__Reducer_Set_Lift
 *
 *  The reducer's meet / join (@c category::Meet / Join) lifted into @c IsSet.
 *  The lattice nodes carry the ALGEBRA only: operands, pointwise evaluation,
 * the
 *  @c IsProduct pairing.  They know nothing of ETCS.  Here in @c sets we add
 * the Set-specific SUBOBJECT surface: @c Member, @c ι, the pullback legs.  The
 *  intersection / union is then a genuine @c IsSet.  The lift @b inherits the
 *  lattice node, so its two operands @b are the underlying sets it carries.
 *  (Pierce: the meet @c A∩B remembers it is the pullback of @c A ↩ U ↩ B.)  The
 *  free @c π_1 / @c π_2 recover the operands @b by const-reference, so
 *  @c π_1(meet) is a bona fide @c IsSet and no sub-structure is copied.  The
 *  node composes recursively: an operand may itself be a @c MeetSet, so a set
 *  expression is a set of sets.
 *
 *  @c ι defaults to the IDENTITY inclusion, HOMOGENEOUS BY DEFAULT.  A member
 * of
 *  @c A∩B is a @c T-value lying in both operands.  Its inclusion into the
 *  ambient @c T is the identity, exactly as @c Set / @c Ø / @c UniversalSet
 *  already spell @c ι(m) @c = @c m.value.  A subobject over a DIFFERENT carrier
 *  (a subset composed with a type conversion, e.g.\ @c ℕ ↪ ℤ) specialises @c ι.
 *  That is a downstream / §5-HSP concern, not this generic contract. */
export template <typename A, typename B>
  requires IsSubobject<A, typename A::Domain> &&
           IsSubobject<B, typename B::Domain> &&
           std::same_as<typename A::Domain, typename B::Domain> &&
           std::same_as<typename A::logic_species, typename B::logic_species>
struct MeetSet : Meet<A, B> {  // A ∩ B as a subobject carrying A and B
  using Domain = typename A::Domain;
  using Codomain = typename A::Codomain;
  using logic_species = typename A::logic_species;
  struct Member {
    Domain value;
  };
  /** @brief ι: A∩B ↣ T, the trivial identity inclusion (homogeneous). */
  constexpr Domain ι(const Member& m) const { return m.value; }
  /** @brief π1 / π2: the pullback co-restriction legs A∩B ↪ A, A∩B ↪ B.  The
   *  shared T-value re-viewed as a member of each operand (identity on it). */
  constexpr typename A::Member π1(const Member& m) const { return {m.value}; }
  constexpr typename B::Member π2(const Member& m) const { return {m.value}; }
  constexpr MeetSet(A a, B b) : Meet<A, B>{std::move(a), std::move(b)} {}
};

export template <typename A, typename B>
  requires IsSubobject<A, typename A::Domain> &&
           IsSubobject<B, typename B::Domain> &&
           std::same_as<typename A::Domain, typename B::Domain> &&
           std::same_as<typename A::logic_species, typename B::logic_species>
struct JoinSet : Join<A, B> {  // A ∪ B as a subobject carrying A and B
  using Domain = typename A::Domain;
  using Codomain = typename A::Codomain;
  using logic_species = typename A::logic_species;
  struct Member {
    Domain value;
  };
  /** @brief ι: A∪B ↣ T, the trivial identity inclusion (homogeneous). */
  constexpr Domain ι(const Member& m) const { return m.value; }
  /** @brief ι1 / ι2, the pushout coprojection colegs A ↪ A∪B, B ↪ A∪B: an
   *  operand member (a T-value in A resp. B, hence in the union) injects as a
   *  member of the join (dual to MeetSet's co-restriction legs). */
  constexpr Member ι1(const typename A::Member& m) const {
    return Member{m.value};
  }
  constexpr Member ι2(const typename B::Member& m) const {
    return Member{m.value};
  }
  constexpr JoinSet(A a, B b) : Join<A, B>{std::move(a), std::move(b)} {}
};

export template <typename T, typename L, typename Predicate>
class Set {
 public:
  // ~ arrow / morphism / subobject classifier jargon
  using Domain = T;
  using Codomain = typename L::Ω;

  // ~ topoi jargon: Member-shape mirror of Subobject's; the IsSubobject
  // contract reads the Member-to-T projection through ι below.  Same
  // shape as Ø / UniversalSet / SingletonSet's Member.
  struct Member {
    T value;
  };

  /** @brief ι: Set ↣ T — Member unwrap.  Identical pattern to
   *  Subobject<A, χ>::ι and SingletonSet::ι; the inclusion projects
   *  the Member's T-value back to the ambient. */
  constexpr T ι(const Member& m) const { return m.value; }

  /** @brief χ: T → Ω — arrow-form classifier for the IsSubobject
   *  contract — historically a static self-reference (now retired).
   *  Post-#681 structural refactor: @c Set @b is the characteristic
   *  morphism via @c operator() below; the @c IsSubobject concept
   *  recognises this structurally with @c { s(a) } @c -> @c IsΩ
   *  rather than via a named @c .χ member.  No static-init cascade for
   *  non-default-constructible Predicates (e.g.\ capturing-lambda
   *  predicates produced by the comprehension DSL). */

  using logic_species = L;
  using cardinality_type = ℵ_0;

  // NOTE: no blanket is_associative_v<Op> / is_idempotent_v<Op> here.  Those
  // would be read by category::is_associative_v's member-discovery for EVERY Op
  // (claiming, e.g., that a Set --- or a graph(f), which is a Set<pair> --- is
  // associative under std::plus, an operation it has no closed meaning for).
  // Same honesty fix as SetExpr (#806 review); nothing consumed them for set
  // types.  A carrier registers a specific (Op) it truly satisfies instead.

  // Store the predicate as a concrete type, not a std::function
  constexpr Set(Predicate p) : predicate_(std::move(p)) {}

  /** @brief Construct from a comprehension value (@c Set{scout | pred}).
   *  @deprecated The paper-aligned grammar is the bare comprehension
   *  @c scout|pred, with no @c Set{...} wrapper.  As of #895 the bare
   *  @c Comprehension is an @c IsSubobject, so it carries the full
   *  set-complement surface --- @c ~(scout|pred) is the set complement (the
   *  reducer's @c Not node), and meet / join apply via @c IsSubobject.  So this
   *  wrapping
   *  constructor no longer adds any capability; it stays ONLY to keep the
   *  ~90 live @c Set{scout|pred} call sites compiling until they migrate to the
   *  bare grammar (PR B of #895).  This is a soft (documentation) deprecation
   *  only: a hard @c [[deprecated]] would break those call sites under
   *  @c -Werror before the migration. */
  // FIXME(#948): stores the predicate only, dropping the comprehension's base;
  // membership becomes P(x) rather than base(x) ∧ P(x).  Sound for universal
  // ambient bases (base(x) ≡ ⊤); wrong for non-universal bases.
  template <typename B, typename P>
    requires std::same_as<Predicate, P>
  constexpr Set(Comprehension<B, P> cp) : predicate_(std::move(cp.predicate)) {}

  constexpr auto operator()(const T& v) const {
    return dedekind::category::lift_logic<L>(predicate_(v));
  }

  /** @brief Const reference to the underlying predicate.  Most callers
   *  should use @c operator() for membership and let
   *  the predicate stay encapsulated; this getter exists for scalar-
   *  interop layers (Python facades, IR fixtures) that need to extract
   *  value-level state from a predicate whose fields are part of its
   *  public contract (e.g.\ @c LPSolutionPredicate carrying
   *  @c point and @c feasible ).  Does not expose mutation; the
   *  predicate's encapsulation discipline remains the predicate's
   *  responsibility, not the Set's. */
  constexpr const Predicate& predicate() const { return predicate_; }

  constexpr cardinality_type cardinality() const { return {}; }

  // The meet / join / complement operators are no longer Set MEMBERS: they are
  // free combinators over IsSet (below the class), so Set, MeetSet, JoinSet and
  // the boundaries all compose uniformly (combinators over Jlt structural
  // types, not an inheritance hierarchy).  #892.

  // Symmetric difference @c A △ B (XOR) is a FREE combinator over @c
  // IsSubobject below the class (like @c & / @c | / @c ~), so @c A △ ~B
  // composes when @c ~B is a @c Not node rather than a @c Set.  #469 / #892.

  /** @brief Same-predicate subset: always True (identity). */
  constexpr typename L::Ω operator<=(const Set& /*other*/) const {
    return L::True;
  }

  /**
   * @brief Subset test: this ⊆ other for heterogeneous predicate types.
   *
   * For intensional sets over potentially infinite domains, the general subset
   * question is undecidable without witnesses. This overload is only available
   * when the ambient logic supports Unknown.
   */
  template <typename OtherPredicate>
    requires(!std::same_as<Predicate, OtherPredicate>) &&
            requires { L::Unknown; }
  constexpr typename L::Ω operator<=(const Set<T, L, OtherPredicate>&) const {
    return L::Unknown;
  }

  /**
   * @brief Point-wise subset evidence: returns true iff this(x) implies
   * other(x) at the given witness point x.
   */
  template <typename OtherPredicate>
  constexpr typename L::Ω is_subset_of_at(
      const Set<T, L, OtherPredicate>& other, const T& x) const {
    const auto in_this = (*this)(x);
    const auto in_other = other(x);
    // Implication in Ω: a => b  is  (!a) OR b.
    return L::OR(L::RFL(in_this), in_other);
  }

 private:
  template <typename, typename, typename>
  friend class Set;

  Predicate predicate_;
};

}  // namespace dedekind::sets

namespace dedekind::category {
// A Set's value is determined by its type only when its predicate is stateless.
// A runtime-stateful predicate (a field-carrying P such as BooleanEqPredicate)
// makes two same-type Sets potentially distinct, so the reducer's type-based
// idempotence must NOT collapse them; gate it on the predicate's emptiness.
template <typename T, typename L, typename P>
inline constexpr bool idempotent_leaf_v<dedekind::sets::Set<T, L, P>> =
    std::is_empty_v<P>;

// A materialized MeetSet / JoinSet still IS the reducer's meet / join node (it
// derives Meet / Join and carries the same operands).  Teach the structural
// pattern-matchers to see through the :sets materialization boundary, so a
// later A & (A | B) applies absorption even though A | B already materialized
// as a JoinSet<A, B> (#865's A ∩ (A ∪ B) → A over materialized nodes).
template <typename Elem, typename A, typename B>
inline constexpr bool
    is_join_containing_v<Elem, dedekind::sets::JoinSet<A, B>> =
        std::same_as<Elem, A> || std::same_as<Elem, B>;
template <typename Elem, typename A, typename B>
inline constexpr bool
    is_meet_containing_v<Elem, dedekind::sets::MeetSet<A, B>> =
        std::same_as<Elem, A> || std::same_as<Elem, B>;

// Value-determinism recurses through the materialized node as it does through
// the bare Meet / Join, so a MeetSet / JoinSet of value-determined operands
// stays collapsible under idempotence and absorption.
template <typename A, typename B>
inline constexpr bool idempotent_leaf_v<dedekind::sets::MeetSet<A, B>> =
    idempotent_leaf_v<A> && idempotent_leaf_v<B>;
template <typename A, typename B>
inline constexpr bool idempotent_leaf_v<dedekind::sets::JoinSet<A, B>> =
    idempotent_leaf_v<A> && idempotent_leaf_v<B>;
}  // namespace dedekind::category

namespace dedekind::sets {

// ── Free set combinators over IsSet (#892) ──────────────────────────────────
// The meet / join / complement, retired as Set MEMBERS, as free combinators
// over the structural IsSet concept.  Set, MeetSet, JoinSet and the boundaries
// therefore compose uniformly, so nested expressions like @c (A|B) & !(A&B)
// resolve without an inheritance hierarchy.  Each folds the reducer term
// through
// @c subobject_reduce_t and materialises the normal form; the irreducible meet
// / join becomes a @c MeetSet / @c JoinSet carrying its operand sets (#892).

/** @brief The predicate of a PLAIN @c Set<T,L,P> (@c ::type absent otherwise);
 *  @c PlainSet gates the plain-set-only branches (complement pair, predicate
 *  negation) so a compound node (@c MeetSet / @c JoinSet) takes the node path.
 */
template <typename S>
struct set_predicate {};
template <typename T, typename L, typename P>
struct set_predicate<Set<T, L, P>> {
  using type = P;
};
template <typename S>
concept PlainSet = requires { typename set_predicate<S>::type; };

// The reducer's complement / bounded laws construct the ABSTRACT lattice bounds
// @c LatticeBottom / @c LatticeTop over the injected order.  Over
// @c subobject_order<L> those bounds ARE the set boundaries @c Ø<T,L> / @c
// 𝔸<T,L> (registered via @c is_lattice_bottom_for / @c is_lattice_top_for in
// @c :boundaries), so @c operator& / @c operator| materialise them back to the
// concrete boundary value.  This is the forward dual of that recognition: the
// bounded law RECOGNISES Ø/𝔸 as bounds; the complement law GENERATES them.
template <typename R>
inline constexpr bool is_subobject_bottom_v = false;
template <typename T, typename L>
inline constexpr bool is_subobject_bottom_v<
    dedekind::category::LatticeBottom<T, subobject_order<L>>> = true;
template <typename R>
inline constexpr bool is_subobject_top_v = false;
template <typename T, typename L>
inline constexpr bool
    is_subobject_top_v<dedekind::category::LatticeTop<T, subobject_order<L>>> =
        true;

/** @brief The subobject-lattice meet @c A @c & @c B, over any two @c
 * IsSubobject operands sharing a carrier and logic.  It folds @c Meet<A,B>
 * through the reducer; @c SetCombine supplies the domain @c structured_and at
 * the incomparable residual.  The normal form materialises to @c Ø, @c 𝔸, an
 *  operand, or a structured leaf.  An irreducible meet becomes a @c MeetSet
 *  carrying both operand sets. */
export template <typename LHS, typename RHS>
  requires IsSubobject<LHS, typename LHS::Domain> &&
           IsSubobject<RHS, typename RHS::Domain> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           std::same_as<typename LHS::logic_species,
                        typename RHS::logic_species>
constexpr auto operator&(const LHS& lhs, const RHS& rhs) {
  using T = typename LHS::Domain;
  using Log = typename LHS::logic_species;
  // a ∧ ¬a = ⊥ collapses INSIDE the reducer: A & ~A is Meet<A, Not<A>>, and
  // meet_complement_law fires when the CODOMAIN Ω is complemented (Boole yes;
  // Kleene's 3-chain no, ¬U=U) --- no bespoke complement-pair branch here.
  {
    using R = subobject_reduce_t<Meet<LHS, RHS>, Log, SetCombine>;
    // Domain leg = the reducer; codomain leg = finalize_combine (a boundary
    // result factors through Σ, so it is re-tagged to the Boolean codomain).
    if constexpr (IsBoundaryObject<R>) {
      return finalize_combine(R{});
    } else if constexpr (is_subobject_bottom_v<R>) {
      return finalize_combine(Ø<T, Log>{});  // reducer's abstract ⊥ over Sub(T)
    } else if constexpr (is_subobject_top_v<R>) {
      return finalize_combine(
          UniversalSet<T, Log>{});  // abstract ⊤ over Sub(T)
    } else if constexpr (std::same_as<R, LHS>) {
      return finalize_combine(lhs);
    } else if constexpr (std::same_as<R, RHS>) {
      return finalize_combine(rhs);
    } else if constexpr (std::same_as<R, Meet<LHS, RHS>>) {
      // Irreducible: the intersection AS a set, carrying both operands (#892).
      return finalize_combine(MeetSet<LHS, RHS>{lhs, rhs});
    } else if constexpr (std::same_as<R, Meet<RHS, LHS>>) {
      // Irreducible, but the reducer commutatively canonicalised the operands
      // (RB ≤ RA); carry them in that order (e.g. a Not operand that did not
      // collapse).  MeetSet, not the structured_and leaf below --- a Not node
      // has no @c .predicate().
      return finalize_combine(MeetSet<RHS, LHS>{rhs, lhs});
    } else {
      // SetCombine collapsed two plain-set leaves via structured_and.
      return finalize_combine(elevate_meet<T, Log>(
          structured_and(lhs.predicate(), rhs.predicate())));
    }
  }
}

/** @brief The subobject-lattice join @c A @c | @c B, dual to @c operator&.  An
 *  irreducible join becomes a @c JoinSet carrying both operand sets. */
export template <typename LHS, typename RHS>
  requires IsSubobject<LHS, typename LHS::Domain> &&
           IsSubobject<RHS, typename RHS::Domain> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           std::same_as<typename LHS::logic_species,
                        typename RHS::logic_species>
constexpr auto operator|(const LHS& lhs, const RHS& rhs) {
  using T = typename LHS::Domain;
  using Log = typename LHS::logic_species;
  // a ∨ ¬a = ⊤ collapses INSIDE the reducer (dual of the meet): A | ~A is
  // Join<A, Not<A>>, and join_complement_law fires when the CODOMAIN Ω is
  // complemented --- no bespoke complement-pair branch here.
  {
    using R = subobject_reduce_t<Join<LHS, RHS>, Log, SetCombine>;
    // Domain leg = the reducer; codomain leg = finalize_combine (a boundary
    // result factors through Σ, so it is re-tagged to the Boolean codomain).
    if constexpr (IsBoundaryObject<R>) {
      return finalize_combine(R{});
    } else if constexpr (is_subobject_bottom_v<R>) {
      return finalize_combine(Ø<T, Log>{});  // reducer's abstract ⊥ over Sub(T)
    } else if constexpr (is_subobject_top_v<R>) {
      return finalize_combine(
          UniversalSet<T, Log>{});  // abstract ⊤ over Sub(T)
    } else if constexpr (std::same_as<R, LHS>) {
      return finalize_combine(lhs);
    } else if constexpr (std::same_as<R, RHS>) {
      return finalize_combine(rhs);
    } else if constexpr (std::same_as<R, Join<LHS, RHS>>) {
      return finalize_combine(JoinSet<LHS, RHS>{lhs, rhs});
    } else if constexpr (std::same_as<R, Join<RHS, LHS>>) {
      // Commutatively canonicalised operands (see the meet dual above).
      return finalize_combine(JoinSet<RHS, LHS>{rhs, lhs});
    } else {
      return finalize_combine(elevate_join<T, Log>(
          structured_or(lhs.predicate(), rhs.predicate())));
    }
  }
}

/** @brief Cross-species meet (#894, step i): two subobjects over the same
 *  carrier but @b different codomains join to the more expressive one; lift
 * both there, then the same-species meet above folds them.  This lets a mixed
 *  @c Boole @c ∩ @c Kleene combine into the reducer at all.  Same-species
 *  combines are untouched (this overload requires the species to @b differ, so
 *  it never competes with the meet above). */
export template <typename LHS, typename RHS>
  requires IsSubobject<LHS, typename LHS::Domain> &&
           IsSubobject<RHS, typename RHS::Domain> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           (!std::same_as<typename LHS::logic_species,
                          typename RHS::logic_species>) &&
           dedekind::category::LiftsTo<
               typename LHS::logic_species,
               join_logic_t<typename LHS::logic_species,
                            typename RHS::logic_species>> &&
           dedekind::category::LiftsTo<
               typename RHS::logic_species,
               join_logic_t<typename LHS::logic_species,
                            typename RHS::logic_species>>
constexpr auto operator&(const LHS& lhs, const RHS& rhs) {
  using Log =
      join_logic_t<typename LHS::logic_species, typename RHS::logic_species>;
  return lift_to<Log>(lhs) & lift_to<Log>(rhs);
}

/** @brief Cross-species join, dual to the cross-species meet (#894, step i). */
export template <typename LHS, typename RHS>
  requires IsSubobject<LHS, typename LHS::Domain> &&
           IsSubobject<RHS, typename RHS::Domain> &&
           std::same_as<typename LHS::Domain, typename RHS::Domain> &&
           (!std::same_as<typename LHS::logic_species,
                          typename RHS::logic_species>) &&
           dedekind::category::LiftsTo<
               typename LHS::logic_species,
               join_logic_t<typename LHS::logic_species,
                            typename RHS::logic_species>> &&
           dedekind::category::LiftsTo<
               typename RHS::logic_species,
               join_logic_t<typename LHS::logic_species,
                            typename RHS::logic_species>>
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
  requires dedekind::category::IsSubobject<
      std::remove_cvref_t<P>, typename std::remove_cvref_t<P>::Domain>
constexpr auto operator~(const P& p) {
  using D = std::remove_cvref_t<P>;
  if constexpr (dedekind::category::is_not_node_v<D>) {
    // ¬¬A ≡ A: peel the Not node (the reducer's involution, at the leaf).
    return p.base;
  } else {
    return dedekind::category::Not<D>{p};
  }
}

/** @brief Symmetric difference @c A @c △ @c B (set-theoretic XOR; #469), a FREE
 *  combinator over @c IsSubobject (like @c & / @c | / @c ~), so it composes
 *  uniformly whether an operand is a @c Set, a @c MeetSet / @c JoinSet, or a
 *  @c Not complement node.  The textbook identity @c A@c △@c B @c = @c (A@c ∩
 *  @c ¬B)@c ∪@c (¬A@c ∩@c B): no bespoke XOR predicate --- @c ~ / @c & / @c |
 *  carry the per-carrier logic and the reducer collapses the result, including
 *  @c A@c △@c ¬A @c = @c 𝔸 via the join-complement law on a complemented
 *  codomain.  The disjoint fast path (@c A@c ∩@c B @c = @c Ø @c ⟹ @c A@c △@c B
 *  @c = @c A@c ∪@c B) is kept for the halfspace-style compile-time collapse. */
export template <typename LHS, typename RHS>
  requires dedekind::category::IsSubobject<LHS, typename LHS::Domain> &&
           dedekind::category::IsSubobject<RHS, typename RHS::Domain> &&
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
using UnivSizeSet = Set<std::size_t, dedekind::category::Boole,
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
 *  comprehension-DSL paths.  (Historically also default-constructible
 *  to satisfy the now-retired @c Set::χ static-initialiser; that
 *  static is gone post-#681, but captureless predicates are still
 *  preferable for symbolic-image purposes.)
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

/** @brief image(f, Set<T, L, P>) — image of an intensional Set under
 *         an arrow.  Layer 1 of #602.
 *
 *  @details Sister overload in the @c image dispatch table, alongside
 *   - @c image(f, SingletonSet) (@c :sets:singleton; extensional, exact).
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
export template <typename T, typename L, typename P,
                 dedekind::category::IsArrow F>
  requires std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>, T>
constexpr auto image(F&&, const Set<T, L, P>&) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  return Set<U, dedekind::category::Kleene, SymbolicImagePredicate<U>>{
      SymbolicImagePredicate<U>{}};
}

/** @brief Composed predicate for the iso-decidable @c image(f, Set)
 *         specialisation below: @c y @c ↦ @c S(f^{-1}(y)) where @c S
 *         is the source set's characteristic and @c f^{-1} is the
 *         iso's inverse.
 *
 *  @details Captures (i) the source set @c S by value and (ii) the
 *  inverse arrow @c FInv by value, so the closure is self-contained
 *  and constexpr-friendly at the call site.  Not default-constructible
 *  in general --- @c Set<T,L,P> stores a @c Predicate by value and has
 *  no default constructor, so callers must always supply both
 *  components to the in-class aggregate initialiser.  The Set's
 *  @c operator() already performs @c lift_logic<L> on the inner
 *  result, and @c S.operator() on the unwrapped @c x returns @c L::Ω
 *  directly, so the composition preserves the source logic species
 *  without going through Ternary.
 *
 *  @c SourceSet is the @c Set<T,L,P> instantiation; @c FInv is the
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

/** @brief image(iso f, Set<T, L, P>) — @b decidable specialisation for
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
 *  The result is a @c Set<U, L, ComposedIsoImagePredicate<...>> --- the
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
export template <typename T, typename L, typename P,
                 dedekind::category::IsIsomorphism F>
  requires std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>, T>
constexpr auto image(F&& f, const Set<T, L, P>& s) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  // Unqualified call so ADL routes to the inverse overload for f's
  // type (e.g.\ Identity<T>, TaggedNegate) in dedekind::category.
  // This PR exports the relevant inverse overloads (a small boy-scout
  // edit in :morphism, see commit history); the unqualified shape is
  // what the IsIsomorphism concept itself uses, and matches the
  // codebase's ADL-hook style for partial categorical primitives.
  auto f_inv = inverse(std::forward<F>(f));
  using FInv = std::remove_cvref_t<decltype(f_inv)>;
  using NewPredicate = ComposedIsoImagePredicate<Set<T, L, P>, FInv>;
  return Set<U, L, NewPredicate>{NewPredicate{s, std::move(f_inv)}};
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
constexpr auto image(F&&, const UniversalSet<T, L, C>&) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  return 𝔸<U, L, C>;  // iso |U| = |T|, so the cardinality carries
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

/** @brief image(monic-with-retract f, Set<T, L, P>) --- @b decidable
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
export template <typename T, typename L, typename P,
                 dedekind::category::IsRetractableArrow F>
  requires std::same_as<dedekind::category::Dom<std::remove_cvref_t<F>>, T> &&
           (!dedekind::category::IsIsomorphism<std::remove_cvref_t<F>>)
constexpr auto image(F&& f, const Set<T, L, P>& s) {
  using U = dedekind::category::Cod<std::remove_cvref_t<F>>;
  // Unqualified call so ADL routes to the retract overload registered
  // for f's type in the appropriate namespace.  The IsRetractableArrow
  // concept guarantees the call is well-formed.
  auto retract_fn = retract(std::forward<F>(f));
  using RetractFn = std::remove_cvref_t<decltype(retract_fn)>;
  using NewPredicate =
      ComposedRetractImagePredicate<Set<T, L, P>, RetractFn, L>;
  return Set<U, L, NewPredicate>{NewPredicate{s, std::move(retract_fn)}};
}

// A NON-injective arrow's image is decided ANALYTICALLY, point-free, as the
// union of its (mono) branch images --- @c image on the reflection branches of
// the sign-fold @c abs in @c dedekind.order (§3.3, Listing 13).  There is no
// operational retract/cofibre fibre-walk here: the monic @c IsRetractableArrow
// overload above is the only retract path, and it is a single lookup.

// The set complement operators @c ! / @c ~ are free combinators over IsSet,
// defined right after the Set class (with @c & / @c |); see there.

/** @brief @c Set @c ^ @c Ø @c = @c Set (symmetric difference with empty
 *         is identity; #469).  Symmetric of @c Ø::operator^(S) above —
 *         this overload picks up @c S @c ^ @c Ø when the boundary is on
 *         the right, keeping the structural collapse type-level rather
 *         than falling through to the lambda. */
export template <typename T, typename L, typename Predicate>
constexpr auto operator^(const Set<T, L, Predicate>& s, const Ø<T, L>&) {
  return s;
}

/** @brief @c Set @c ^ @c 𝔸 @c = @c ¬Set (symmetric difference with the
 *         universe is the complement; #469).  Symmetric of
 *         @c 𝔸::operator^(S) above. */
export template <typename T, typename L, typename C, typename Predicate>
constexpr auto operator^(const Set<T, L, Predicate>& s,
                         const UniversalSet<T, L, C>&) {
  return !s;
}

/** @section expressions__Set_Codomain_Reconciliation (#928)
 *
 * The logic species every @c Set deduction guide picks for its @c L slot must
 * keep the wrapper's codomain coherent.  @c Set::operator() is
 * @c lift_logic<L>(predicate(x)): @c lift_logic embeds a decided @c bool onto
 * the target poles but passes a value ALREADY in a species (a non-@c bool @c Ω)
 * THROUGH unchanged.  So the wrapper is coherent (its @c Codomain @c = @c L::Ω
 * equals what @c operator() returns) iff the wrapped predicate's membership
 * answer either is @c bool (which embeds into any @c L) or already IS @c L::Ω.
 *
 * The single source of truth is therefore the wrapped predicate's ACTUAL
 * @c operator() RETURN type, not a declared @c logic_species tag.  The tag was
 * only ever a PROXY: a predicate can OMIT it, range over a countable carrier
 * (so
 * @c NaturalLogic reads @c Boole), yet still RETURN @c Ternary; keying off the
 * tag would then demote the codomain to @c Boole while @c operator() still
 * returns @c Ternary --- a declared-codomain vs actual-return MISMATCH (@c
 * lift_logic passes a non-@c bool answer through unchanged, so no value is
 * truncated; the wrapper is just mis-typed and fails @c IsSet, #928).  So @c
 * set_logic_t joins the answer's own species (@c GetLogic of the
 * return type) UP with the carrier axis, and @c CoherentWrap checks the answer
 * against the result.  All guides that pick an @c L share this principle: the
 * identity @c Set(Species) CTAD derives @c L from the bare node's own answer,
 * while the point-free
 * comprehension @c Set(Comprehension<B,P>) derives from the WHOLE
 * comprehension's answer --- @c Comprehension::operator() combines @c base(x)
 * under @c B's logic, so it joins @c B's species UP with the @c P-axis @c
 * set_logic_t (a Kleene base with a @c bool predicate still returns @c
 * Kleene::Ω, which a @c P-only derivation would mis-type @c Boole).  So the @c
 * A|pred paths are covered, not only the bare-node one.  The @c
 * UniversalPredicate / boundary
 * guides are EXEMPT: their wrapped predicate answers @c bool by construction,
 * so any @c L is coherent and they keep the plain carrier-axis @c NaturalLogic.
 *
 * - carrier-axis verdict @c NaturalLogic (continuum ⟹ @c Kleene) keeps the
 *   canonical continuum ambient ℝ = @c 𝔸<QuadraticReal,Boole,ℶ_1>
 *   semi-decidable: ℝ returns @c bool, but its ℶ_1 cardinality (not a logic
 *   tag) carries the undecidability, so join(Kleene, Boole) = Kleene holds the
 *   guard;
 * - the answer's species closes the dual gap: any countable-carrier predicate
 *   returning @c Ternary --- @c 𝔸<int,Kleene>, an @c A|pred halfspace carved
 *   from it, an UNTAGGED species whose @c operator() merely returns @c Ternary,
 *   or a @c Ternary-returning comprehension predicate --- has @c GetLogic =
 *   @c Kleene, so join(Boole, Kleene) = Kleene rather than the demoted @c Boole
 *   that made a @c !IsSet wrapper.
 *
 * @c CoherentWrap is the Sollbruchstelle: it admits the wrap iff the answer
 * type is @c bool or exactly @c set_logic_t::Ω.  @c join_logic_t only models 𝔹
 * ⊑ K₃, so a @c Percent- or @c Chain-returning predicate collapses to a @c
 * Boole wrap whose @c Ω (@c bool) is NOT its @c Percentage / chain return; it
 * is rejected rather than silently mis-typed.  FIXME(#945): place @c Chain / @c
 * Percent in the logic-species lattice so such ambients wrap coherently instead
 * of being rejected.
 */

/** @brief The type predicate @c P returns for a member query on @c Domain ---
 *  the authority for a wrapping @c Set's codomain (a @c logic_species tag is
 *  only a proxy for it).
 *  @note Deliberately the @b ungated sibling of @c category::OmegaOf: same
 *  @c const / @c decay extraction, but @b no @c LogicalMap / @c IsΩ gate.
 *  @c CoherentWrap must read the RAW answer of a predicate whose return is
 *  @e not a truth-object (@c Percentage, a @c Chain value) in order to REJECT
 *  it; @c OmegaOf requires @c LogicalMap and so is ill-formed on exactly those
 *  predicates, which is why it cannot centralize this contract.  Reuse ends at
 *  the extraction shape.
 *  @tparam P a predicate / set-node; @tparam Domain the carrier queried. */
template <typename P, typename Domain>
using membership_answer_t = std::remove_cvref_t<
    std::invoke_result_t<const std::decay_t<P>&, const Domain&>>;

/** @brief The coherent @c Set codomain species: @c join_logic_t of the
 *  carrier-axis @c NaturalLogic<Carrier> verdict and @c GetLogic of @c P's
 *  actual answer on @c Domain.  Rationale: @ref
 *  expressions__Set_Codomain_Reconciliation.
 *  @tparam Carrier the set-node whose cardinality feeds the carrier axis;
 *  @tparam P the wrapped predicate; @tparam Domain the carrier queried. */
template <typename Carrier, typename P, typename Domain>
using set_logic_t = join_logic_t<typename NaturalLogic<Carrier>::type,
                                 typename dedekind::category::GetLogic<
                                     membership_answer_t<P, Domain>>::type>;

/** @brief Whether wrapping @c P (answering on @c Domain, carrier axis from
 *  @c Carrier) yields a coherent @c Set: the wrapper's declared @c Ω is what
 *  @c P actually RETURNS, i.e. @c P's answer is @c bool or exactly
 *  @c set_logic_t::Ω.  An answer outside that is a declared-codomain vs
 *  actual-return mismatch (not a lost value), the Sollbruchstelle rejected here
 *  (@c Percent / @c Chain, FIXME(#945)).  Rationale: @ref
 *  expressions__Set_Codomain_Reconciliation.
 *  @tparam Carrier carrier axis source; @tparam P wrapped predicate;
 *  @tparam Domain the carrier queried. */
template <typename Carrier, typename P, typename Domain>
concept CoherentWrap =
    requires(const P& p, const Domain& x) { p(x); } &&
    (std::same_as<membership_answer_t<P, Domain>, bool> ||
     std::same_as<membership_answer_t<P, Domain>,
                  typename set_logic_t<Carrier, P, Domain>::Ω>);

/** @brief One-argument convenience: a bare set-node @c Species is its own
 *  carrier @b and predicate.
 *  @tparam Species the set-node wrapped by the @c Set(Species) CTAD. */
export template <typename Species>
concept CoherentSetWrap =
    CoherentWrap<Species, Species, typename Species::Domain>;

/** @brief The @c Set codomain species for a bare set-node @c Species.
 *  @tparam Species the set-node wrapped by the @c Set(Species) CTAD. */
template <typename Species>
using wrapped_logic_t = set_logic_t<Species, Species, typename Species::Domain>;

/** @brief The point-free comprehension @c A|pred (deprecated scout spelling
 *  @c element<A>|pred): codomain derived from the whole comprehension's return
 *  per @ref expressions__Set_Codomain_Reconciliation.
 *  @tparam B the comprehension's base (carrier axis + logic species);
 *  @tparam P the wrapped predicate.
 *  FIXME(#948): the wrap stores @c P only (the converting ctor drops @c B), so
 *  a NON-universal base's membership is lost; harmless for universal ambient
 *  bases (@c base(x) ≡ ⊤), general fix tracked separately. */
export template <typename B, typename P>
  requires CoherentWrap<B, P, typename B::Domain>
Set(Comprehension<B, P>)
    -> Set<typename B::Domain,
           join_logic_t<set_logic_t<B, P, typename B::Domain>,
                        typename B::logic_species>,
           P>;

// Enforce ETCS compliance also here:
static_assert(
    IsSet<decltype(ambient_set<int>(Set<int, Boole, UniversalPredicate<int>>{
        UniversalPredicate<int>{}}))>,
    "The canonical intensional Set<T, L, Predicate> must lift to an ETCS set ");

static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<int>(
        Set<int, dedekind::category::Boole, UniversalPredicate<int>>{
            UniversalPredicate<int>{}}))>,
    "The canonical intensional Set<T, L, Predicate> must lift to an ETCS set "
    "object.");

/** @section expressions__Identity_CTAD
 *  Bare set-node wrap; shares the codomain reconciliation of @ref
 *  expressions__Set_Codomain_Reconciliation with the comprehension / scout
 *  guides above. */
template <typename Species>
  requires CoherentSetWrap<Species>
Set(Species)
    -> Set<typename Species::Domain, wrapped_logic_t<Species>, Species>;

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

/** @brief Predicate-level conjunction @c p1 @c && @c p2: @c structured_and when
 *  a domain collapse applies, else the reducer's @c Meet node, a callable
 *  predicate @f$\chi_1 \wedge \chi_2@f$ that also carries its operands via
 *  @c π_1 / @c π_2.  It is the @b same AST the set-level combinators use, so a
 *  set-builder @c Set{x @c | @c p1 @c && @c p2} shares one representation with
 *  @c A @c & @c B.  There is no separate @c AndPredicate. */
export template <typename P1, typename P2>
constexpr auto operator&&(P1&& p1, P2&& p2) {
  if constexpr (HasStructuredAnd<P1, P2>) {
    return structured_and(std::forward<P1>(p1), std::forward<P2>(p2));
  } else {
    return Meet<std::decay_t<P1>, std::decay_t<P2>>{std::forward<P1>(p1),
                                                    std::forward<P2>(p2)};
  }
}

/** @brief Predicate-level disjunction @c p1 @c || @c p2, dual to @c operator&&:
 *  @c structured_or when a collapse applies, else the reducer's @c Join node.
 */
export template <typename P1, typename P2>
constexpr auto operator||(P1&& p1, P2&& p2) {
  if constexpr (HasStructuredOr<P1, P2>) {
    return structured_or(std::forward<P1>(p1), std::forward<P2>(p2));
  } else {
    return Join<std::decay_t<P1>, std::decay_t<P2>>{std::forward<P1>(p1),
                                                    std::forward<P2>(p2)};
  }
}

}  // namespace dedekind::sets

namespace dedekind::sets {

/**
 * @brief Membership predicate of a Cartesian product: @f$(x,y)\in A\times B
 *        \iff x\in A \wedge y\in B@f$.
 *
 * @details A @b named functor (not a capturing lambda), so a product @c Set
 * carries a structural, comparable predicate type rather than an opaque closure
 * (#844, [[feedback_no_lambdas_opacity]]).  It stores the two component @c Set
 * operands and evaluates each on its projection, going through @c
 * Set::operator() so the logic lift is preserved.  Mirrors the @c
 * dedekind::relational::RelAnd shape, which already ships as a @c Set<pair>
 * predicate across translation units.
 */
export template <typename A, typename B>
struct ProductMembership {
  A a;
  B b;
  // @c auto (not @c bool): inherit the operands' logic species so a Kleene
  // component keeps @c Unknown rather than collapsing under a bool cast.
  template <typename P>
  constexpr auto operator()(const P& p) const {
    return a(p.first) && b(p.second);
  }
};

/**
 * @brief Cartesian product of two sets: {(a,b) | a ∈ A, b ∈ B}.
 *
 * Constructs a Set whose domain is std::pair<T1,T2> and whose membership
 * predicate checks element-wise membership in both component sets.
 */
export template <typename T1, typename L1, typename P1, typename T2,
                 typename L2, typename P2>
  requires std::same_as<L1, L2>
constexpr auto cartesian_product(const Set<T1, L1, P1>& a,
                                 const Set<T2, L2, P2>& b) {
  using Pair = std::pair<T1, T2>;
  using Pred = ProductMembership<Set<T1, L1, P1>, Set<T2, L2, P2>>;
  return Set<Pair, L1, Pred>{Pred{a, b}};
}

/**
 * @brief Cartesian product over ambient species values.
 *
 * Lifts each ambient species to its full carrier set and delegates to the
 * Set×Set cartesian product.
 */
export template <typename A, typename B>
  requires requires {
    typename std::remove_cvref_t<A>::Domain;
    typename std::remove_cvref_t<B>::Domain;
    typename NaturalLogic<std::remove_cvref_t<A>>::type;
    typename NaturalLogic<std::remove_cvref_t<B>>::type;
  } && std::same_as<typename NaturalLogic<std::remove_cvref_t<A>>::type,
                    typename NaturalLogic<std::remove_cvref_t<B>>::type>
constexpr auto cartesian_product(const A& a, const B& b) {
  // Materialise BOTH operands to a concrete @c Set (the identity @c
  // Set(Species) CTAD accepts a bare ambient AND re-wraps an already-@c Set
  // operand) and delegate to the @c Set x @c Set overload.  This normalisation
  // is load- bearing: it TERMINATES the generic dispatch.  Spelling @c a @c *
  // @c b here instead would recurse on a mixed @c UniversalSet x @c Set pair
  // (no
  // @c operator*(UniversalSet, Set), so it re-enters this generic).
  const auto left = Set{a};
  const auto right = Set{b};
  return cartesian_product(left, right);
}

/**
 * @brief The @b universal set of products: @f$\mathbb{A}_A \times \mathbb{A}_B
 * =
 *        \mathbb{A}_{A\times B}@f$, spelled as the universe over the product
 *        carrier (@c IsProduct).
 *
 * @details Two @b total factors carry no restriction to lift, so the product
 * @b is the pure product universe @c 𝔸<pair<A,B>> --- not a refinement of it.
 * This is the base case of the cylinder decomposition @f$A\times B =
 * \pi_1^{-1}(A)\cap\pi_2^{-1}(B)@f$: with @c A, @c B universal both cylinders
 * are the whole universe, so their intersection is too.  A @b restricted
 * factor instead lifts its predicate onto its axis (@c dedekind.order:
 * @c π_I over a halfspace), refining this universe into a proper subobject.
 * The old melting form (@c pa(first) @c && @c pb(second) captured in an
 * anonymous closure) discarded the factor structure; keeping the universe
 * explicit lets @c dom / @c cod read the factors back.
 */
// The cartesian-product cardinality is the JOIN of the factors on the lattice
// Finite < ℵ<0> < ℵ<1> < …: |A×B| = |A|·|B| = max(|A|,|B|) for infinite
// factors, Finite only when both are finite.  The primary is left INCOMPLETE
// (no @c type): a cardinality pair outside the known lattice (Finite / ℵ<N>) is
// HONESTLY REJECTED at compile time rather than silently downgraded to ℵ_0 ---
// a blanket fallback would misclassify e.g. a custom uncountable tag × Finite
// as countable.
template <typename CA, typename CB>
struct product_cardinality;
template <>
struct product_cardinality<Finite, Finite> {
  using type = Finite;
};
template <std::size_t N>
struct product_cardinality<Finite, ℵ<N>> {
  using type = ℵ<N>;
};
template <std::size_t N>
struct product_cardinality<ℵ<N>, Finite> {
  using type = ℵ<N>;
};
template <std::size_t M, std::size_t N>
struct product_cardinality<ℵ<M>, ℵ<N>> {
  using type = ℵ<(M > N ? M : N)>;
};

export template <typename A, typename LA, typename CA, typename B, typename LB,
                 typename CB>
  requires std::same_as<LA, LB>
constexpr auto operator*(const UniversalSet<A, LA, CA>&,
                         const UniversalSet<B, LB, CB>&) {
  using CC = typename product_cardinality<CA, CB>::type;
  // 𝔸 × 𝔸 = 𝔸<pair>.  Codomain leg (#894): the universe is decided → Boole.
  return finalize_combine(𝔸<std::pair<A, B>, LA, CC>);
}

/** @brief Infix sugar for cartesian product over sets. */
export template <typename T1, typename L1, typename P1, typename T2,
                 typename L2, typename P2>
  requires std::same_as<L1, L2>
constexpr auto operator*(const Set<T1, L1, P1>& a, const Set<T2, L2, P2>& b) {
  return cartesian_product(a, b);
}

/** @brief Infix sugar for cartesian product over ambient species values. */
export template <typename A, typename B>
  requires requires {
    typename std::remove_cvref_t<A>::Domain;
    typename std::remove_cvref_t<B>::Domain;
    typename NaturalLogic<std::remove_cvref_t<A>>::type;
    typename NaturalLogic<std::remove_cvref_t<B>>::type;
  } && std::same_as<typename NaturalLogic<std::remove_cvref_t<A>>::type,
                    typename NaturalLogic<std::remove_cvref_t<B>>::type>
constexpr auto operator*(const A& a, const B& b) {
  return cartesian_product(a, b);
}

using CanonicalIntSet =
    Set<int, dedekind::category::Boole, UniversalPredicate<int>>;
using CanonicalIntProductSet =
    decltype(cartesian_product(std::declval<const CanonicalIntSet&>(),
                               std::declval<const CanonicalIntSet&>()));
using CanonicalIntProductDomain = typename CanonicalIntProductSet::Domain;

static_assert(
    dedekind::category::IsProduct<CanonicalIntProductDomain, int, int>,
    "sets::cartesian_product must expose a std::pair product domain.");

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
 * @c Singleton / @c Halfspace / @c OrderInterval by coercion; a finite-carrier
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
  return 𝔸<Ø<T, L>, L, Finite>;
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
namespace detail_setexpr_witness {
struct all_in {
  constexpr bool operator()(int) const { return true; }
};
static_assert(IsSet<Comprehension<UniversalSet<int>, all_in>>,
              "{𝔸 | P} is a first-class set: IsSet by SetExpr + its own χ.");
static_assert(IsSet<Comprehension<Ø<int>, all_in>>,
              "{Ø | P} is a first-class set.");

// #834/#829: a bare Comprehension is an IsSubobject, so ~ (the set complement)
// is the reducer's Not node over it --- a genuine set-complement subobject, not
// a formal arrow.  ~~ peels back to the Comprehension (the involution).
using CompN = Comprehension<UniversalSet<int>, all_in>;
static_assert(
    std::same_as<std::remove_cvref_t<decltype(~std::declval<CompN>())>,
                 dedekind::category::Not<CompN>>,
    "~(A | pred) is the set-complement Not<Comprehension>.");
static_assert(
    std::same_as<std::remove_cvref_t<decltype(~~std::declval<CompN>())>, CompN>,
    "~~(A | pred) peels back to the Comprehension (involution).");

// #834: the bare ETCS subobject (classify / ambient_set) complements the same
// way --- ~s is the Not node over it, a set-complement subobject (what the
// retired set_complement produced), not a formal arrow.
using SubN = std::remove_cvref_t<decltype(classify<int>(all_in{}))>;
static_assert(
    dedekind::category::IsSubobject<
        std::remove_cvref_t<decltype(~std::declval<SubN>())>, int>,
    "~classify(f) is a set-complement subobject, not a formal arrow.");
}  // namespace detail_setexpr_witness

// ── The point-free projection scout ────────────────────────────────────────
/**
 * @brief @c π --- the point-free variable, a @b domain-less scout.
 *
 * @details Where @c element<ℕ> bakes the carrier into the scout's type, @c π
 * leaves it open.  A comparison @c π @c ⋈ @c fix(V) fixes only the @b shape
 * (direction and pivot) as an @c UnboundHalfspace / @c UnboundSingleton; a
 * later @c carrier @c | @c ... instantiates it at the carrier's @c Domain,
 * reusing
 * @c Halfspace / @c Singleton.  @c π is the unary projection; the product
 * coordinates @c π1 / @c π2 follow with the relational surface (#783).
 *
 * @note @c Projection itself is an @b empty symbolic tag (not callable), so it
 * does @b not satisfy @c category::IsProductProjection --- it is the DSL
 * @b spelling of a coordinate, consumed by the comparison operators.  The
 * actual product-projection is the @b accessor @c coord below (@c coord<1> /
 * @c coord<2>), which @b delegates to @c category::π_1 / @c category::π_2 (the
 * canonical projections in @c :limit) and IS a certified
 * @c IsProductProjection --- the static_assert after @c coord pins that
 * agreement.  So @c π1 / @c π2 (tags) and @c category::π_1 / @c π_2 (accessors)
 * are two spellings of one notion, bridged by @c coord.  The symbolic-tag
 * surface + its comparison operators (which build @c ProjRel / @c ProjBound in
 * @c :order) grew up with the comprehension DSL; unifying it with @c :limit is
 * a @b non-urgent follow-up (#878).
 * @see dedekind::category::π_1, dedekind::category::π_2
 * @see dedekind::category::IsProductProjection
 */
export template <IsRingIntegral auto Slot>
struct Projection {};

export inline constexpr Projection<0> π{};
// Cosmetic alias for the sole projection π (the bound element of a
// single-carrier comprehension): a plain element also reads as @c χ,
// e.g. @c ℕ @c | @c χ @c <= @c fix(5_c).  Same type and behaviour as π (a
// distinct inline object, so a distinct address), just a second spelling.
// (Plain @c x is deliberately not offered: it shadows the pervasive local
// element parameter named @c x and would trip @c -Wshadow.)
export inline constexpr Projection<0> χ{};

// ── Product projections: the relational (point-free) variables ─────────────
/**
 * @brief @c π1 / @c π2 --- the coordinate projections of a pair, the positional
 *        variables of the point-free relational surface.
 *
 * @details A comparison @c π_I @c ⋈ @c π_J or @c π_I @c ⋈ @c fix(V) builds a
 * @b strongly-typed predicate on a pair (no lambda); @c && conjoins them
 * (@c || joins), distinct from the set-level @c & / @c | on whole relations;
 * and @c product @c | @c predicate restricts the product to the relation.
 * @c && binds looser than @c |, so the comprehension parenthesises the meet:
 * @c ℕ*ℕ @c | @c (π1 @c < @c π2 @c && @c π1 @c > @c fix(5_c)) is the relation
 * @f$\{(x,y) \mid x<y \wedge x>5\}@f$ as an @c IsSet on @c ℕ×ℕ.
 */
export inline constexpr Projection<1> π1{};
export inline constexpr Projection<2> π2{};
export inline constexpr Projection<3> π3{};

// Blackboard coordinate aliases so a pair/triple predicate reads as
// @f$z = f(x,y)@f$: the mathematical-italic @c 𝑥/𝑦/𝑧 (U+1D465..7) are
// letter-category identifiers (like @c 𝔸/ℕ), NOT ASCII @c x/y/z, so they carry
// the blackboard look without colliding with ordinary variable names.
export inline constexpr Projection<1> 𝑥{};
export inline constexpr Projection<2> 𝑦{};
export inline constexpr Projection<3> 𝑧{};

/** @brief The @c I-th component of a pair (1 = @c first, 2 = @c second).
 *  Binary products only, so an out-of-range slot (the unary @c π, or @c π3) is
 *  a hard error rather than a silent alias for @c .second.
 *  @note The index @b type is a call-site free variable (@c IsRingIntegral);
 *  the @c requires guard pins the @b value domain to @c {1,2} (binary
 *  products), independent of that type.  Extending @c coord to higher arities
 *  (@c π3 / @c 𝑧 already exist as tags) is the arity-aware follow-up (#824). */
export template <IsRingIntegral auto I, typename P>
  requires(I == 1 || I == 2)
constexpr decltype(auto) coord(const P& p) {
  // Reuse @c :limit's canonical product projections rather than re-deriving
  // @c .first / @c .second here: halfspace's @c coord and @c category::π_1 /
  // @c π_2 are the @b same projection (@c IsProductProjection).  Letting the
  // two surfaces share one accessor stops halfspace shadowing @c :limit.
  if constexpr (I == 1)
    return dedekind::category::π_1(p);
  else
    return dedekind::category::π_2(p);
}

// The two wheels know each other: @c :limit's @c π_1 / @c π_2 are exactly the
// @c coord accessors above, and they are certified @c IsProductProjection there
// (@c limit.cppm).  Pin the agreement so the surfaces cannot silently diverge.
static_assert(
    dedekind::category::IsProductProjection<
        decltype([](const std::pair<int, bool>& p) {
          return dedekind::category::π_1(p);
        }),
        std::pair<int, bool>, int>,
    "coord<1> = category::π_1 is a certified product projection (left).");
static_assert(coord<1>(std::pair{7, false}) ==
                      dedekind::category::π_1(std::pair{7, false}) &&
                  coord<2>(std::pair{7, false}) ==
                      dedekind::category::π_2(std::pair{7, false}),
              "coord IS category::π_1 / π_2 (one projection, two call sites).");

}  // namespace dedekind::sets
