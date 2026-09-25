/**
 * @file dedekind/category/posetal.cppm
 * @partition :posetal
 * @brief Posetal categories — categories derived from partial orders.
 *
 * @section posetal__Categorical_Definition
 * A Posetal Category is a category where for any two objects A and B, there is
 * **at most one** morphism from A to B. In this framework:
 * - Objects are elements of the set.
 * - Morphisms represent the relation (a ≤ b).
 * - Identity morphisms correspond to Reflexivity (a ≤ a).
 * - Composition corresponds to Transitivity (a ≤ b and b ≤ c implies a ≤ c).
 * - Skeletality in the category corresponds to Antisymmetry (a ≤ b and b ≤ a
 * implies a = b).
 *
 * @section posetal__Order_Structure
 * This structure corresponds to a **Partially Ordered Set (Poset)**. Unlike a
 * Preorder, a Posetal Category is skeletal, meaning isomorphic objects are
 * identical.
 *
 * Textbook defaults in this partition:
 * - Relation defaults to `std::less_equal<T>` (the canonical order witness).
 * - Logic defaults to `Boole` (Boolean Ω).
 *
 * @quote
 * "In a sense, the most basic category is a partially ordered set;
 *  the arrows are just the instances of the order relation."
 *  — Saunders Mac Lane, *Categories for the Working Mathematician*
 *
 * @see https://en.wikipedia.org/wiki/Partially_ordered_set
 * @see https://en.wikipedia.org/wiki/Preorder
 *
 * @tparam T The type of objects in the poset.
 * @tparam Rel The relation defining the order (the Morphism Generator).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Science, in other words, is a system of relations."
 *       -- Henri Poincare, The Value of Science (1905)
 */
module;

#include <algorithm>
#include <concepts>
#include <functional>
#include <utility>

export module dedekind.category:posetal;

import :logic;
import :mereology;
import :species;   // Sup / Inf: the value-returning join / meet defaults (#934)
import :morphism;  // IsArrow / IsBijectiveArrow / Identity (gates for
                   // IsMonotone / IsAntiMonotone / IsOrderIsomorphism)
import :thin;      // IsThinCategory — the faithful row-1 inclusion that
                   // IsPosetal explicitly imports per #698 Slice 1.

namespace dedekind::category {

/**
 * @concept IsPosetal
 * @brief A Category where morphisms are governed by a specific Logic Species
 * (Ω).
 *
 * This definition reifies the Poset as a skeletal category over a Topos L.
 * By default, it assumes Classical (Boolean) logic, but it can be
 * parameterized to support intuitionistic or fuzzy relations.
 *
 * @details
 * @c IsPosetal is the strict refinement of @c IsThinCategory (#698 row 1):
 * a posetal category is a thin category that is @b also @b skeletal
 * (antisymmetric).  The faithful inclusion @c IsPosetal @c ⊊
 * @c IsThinCategory is encoded definitionally in the signature per the
 * project's @em "faithful specialization in the type signature from day
 * one" posture (#698).
 *
 * @tparam T   The Domain (Objects).
 * @tparam Rel The Relation (Morphisms).
 * @tparam L   The Logic Species (The Subobject Classifier).
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = Boole>
concept IsPosetal =
    IsThinCategory<T, Rel, L> &&  // Faithful inclusion: every poset IS thin
                                  // (preorder + antisymmetric); #698 Slice 1.
    IsPartialOrder<T, Rel, typename L::Ω> && requires(Rel rel, T a, T b) {
      // The relation must yield a result from the logical classifier
      { rel(a, b) } -> std::same_as<typename L::Ω>;
    };

/**
 * @concept IsTotallyOrderedPosetal
 * @brief Posetal refinement where the underlying order is total/linear.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = Boole>
concept IsTotallyOrderedPosetal =
    IsPosetal<T, Rel, L> && IsTotalOrder<T, Rel, typename L::Ω>;

/**
 * @concept IsOrderMeetSemilattice
 * @brief Order-theoretic meet-semilattice as a commutative refinement.
 *
 * @details
 * In the module hierarchy, `:mereology` provides the upstream associative +
 * idempotent meet band. `:posetal` refines that structure with commutativity,
 * yielding the order-theoretic meet-semilattice notion.
 *
 * @note Naming history: an `IsOrderMeetSemilatticeSignature` shape mixin
 * was previously bundled into this concept's body; it was retired
 * because @c IsMereologicalMeetSemilattice transitively requires
 * @c IsMereologicalMeetMagma, which already checks the operator
 * surface (@c meet(a, b) @c -> @c convertible_to<T>).  Callsites that
 * need just the operator-surface check use the magma concept directly.
 */
export template <typename T, typename Meet = Inf>
concept IsOrderMeetSemilattice =
    IsMereologicalMeetSemilattice<T, Meet> && IsCommutative<T, Meet>;

/**
 * @concept IsCertifiedOrderMeetSemilattice
 * @brief Trait-certified meet-semilattice (associative + idempotent +
 * commutative).
 */
export template <typename T, typename Meet = Inf>
concept IsCertifiedOrderMeetSemilattice = IsOrderMeetSemilattice<T, Meet>;

/**
 * @concept IsOrderJoinSemilattice
 * @brief Order-theoretic join-semilattice as a commutative refinement.
 *
 * @details
 * In the module hierarchy, `:mereology` provides the upstream associative +
 * idempotent join band. `:posetal` refines that structure with commutativity,
 * yielding the order-theoretic join-semilattice notion.
 *
 * @note Naming history: an `IsOrderJoinSemilatticeSignature` shape mixin
 * was previously bundled into this concept's body; it was retired
 * because @c IsMereologicalJoinSemilattice transitively requires
 * @c IsMereologicalJoinMagma, which already checks the operator
 * surface (@c join(a, b) @c -> @c convertible_to<T>).  Callsites that
 * need just the operator-surface check use the magma concept directly.
 */
export template <typename T, typename Join = Sup>
concept IsOrderJoinSemilattice =
    IsMereologicalJoinSemilattice<T, Join> && IsCommutative<T, Join>;

/**
 * @concept IsCertifiedOrderJoinSemilattice
 * @brief Trait-certified join-semilattice (associative + idempotent +
 * commutative).
 */
export template <typename T, typename Join = Sup>
concept IsCertifiedOrderJoinSemilattice = IsOrderJoinSemilattice<T, Join>;

/**
 * @concept IsOrderLatticeOperations
 * @brief Order-theoretic lattice operations as commutative + absorptive
 * refinement over upstream mereological lattice operations.
 *
 * @note Naming history: this concept previously bundled an
 * @c IsOrderLatticeOperationsSignature signature mixin alongside the
 * semantic clauses, mixing syntax (operator surface check) with
 * semantics (axiomatic lattice claims).  The signature mixin was
 * removed (and the now-unused signature concept retired) because
 * @c IsOrderJoinSemilattice / @c IsOrderMeetSemilattice already imply
 * the operator surface through their upstream mereological magma
 * concepts (@c IsMereologicalJoinMagma / @c IsMereologicalMeetMagma).
 * The bundled concept here is purely semantic.
 */
export template <typename T, typename Join = Sup, typename Meet = Inf>
concept IsOrderLatticeOperations =
    IsMereologicalLatticeOperations<T, Join, Meet> &&
    IsOrderJoinSemilattice<T, Join> && IsOrderMeetSemilattice<T, Meet> &&
    IsAbsorptive<T, Join, Meet>;

/**
 * @concept IsCertifiedOrderLatticeOperations
 * @brief Trait-certified lattice operations (commutative semilattices +
 * absorption).
 */
export template <typename T, typename Join = Sup, typename Meet = Inf>
concept IsCertifiedOrderLatticeOperations =
    IsOrderLatticeOperations<T, Join, Meet>;

/**
 * @concept IsOrderDistributiveLatticeOperations
 * @brief Distributive lattice refinement over `IsOrderLatticeOperations`.
 *
 * @see https://en.wikipedia.org/wiki/Distributive_lattice
 */
export template <typename T, typename Join = Sup, typename Meet = Inf>
concept IsOrderDistributiveLatticeOperations =
    IsOrderLatticeOperations<T, Join, Meet> && IsDistributive<T, Join, Meet> &&
    IsDistributive<T, Meet, Join>;

/**
 * @concept IsCertifiedOrderDistributiveLatticeOperations
 * @brief Trait-certified distributive lattice refinement.
 */
export template <typename T, typename Join = Sup, typename Meet = Inf>
concept IsCertifiedOrderDistributiveLatticeOperations =
    IsOrderDistributiveLatticeOperations<T, Join, Meet> &&
    IsDistributive<T, Join, Meet> && IsDistributive<T, Meet, Join>;

using DefaultJoin = Sup;
using DefaultMeet = Inf;

/**
 * @concept IsPathProjection
 * @brief Projection contract used by `check_path` to map objects into an
 * orderable relation carrier.
 */
export template <typename Project, typename T, typename Rel, typename Ω>
concept IsPathProjection = requires(Project project, T x, Rel rel) {
  { project(x) };
  { rel(project(x), project(x)) } -> std::same_as<Ω>;
};

// Upstream/downstream alignment: posetal concepts refine mereological ones.
static_assert(IsOrderMeetSemilattice<int, DefaultMeet>);
static_assert(IsMereologicalMeetSemilattice<int, DefaultMeet>);
static_assert(IsOrderJoinSemilattice<int, DefaultJoin>);
static_assert(IsMereologicalJoinSemilattice<int, DefaultJoin>);
static_assert(IsOrderLatticeOperations<int, DefaultJoin, DefaultMeet>);
static_assert(IsMereologicalLatticeOperations<int, DefaultJoin, DefaultMeet>);
static_assert(
    IsOrderDistributiveLatticeOperations<int, DefaultJoin, DefaultMeet>);

// Compatibility aliases are intentionally locked to the refined concepts.
static_assert(IsCertifiedOrderMeetSemilattice<int, DefaultMeet>);
static_assert(IsCertifiedOrderMeetSemilattice<int, DefaultMeet> ==
              IsOrderMeetSemilattice<int, DefaultMeet>);
static_assert(IsCertifiedOrderJoinSemilattice<int, DefaultJoin>);
static_assert(IsCertifiedOrderJoinSemilattice<int, DefaultJoin> ==
              IsOrderJoinSemilattice<int, DefaultJoin>);
static_assert(IsCertifiedOrderLatticeOperations<int, DefaultJoin, DefaultMeet>);
static_assert(
    IsCertifiedOrderLatticeOperations<int, DefaultJoin, DefaultMeet> ==
    IsOrderLatticeOperations<int, DefaultJoin, DefaultMeet>);
static_assert(IsCertifiedOrderDistributiveLatticeOperations<int, DefaultJoin,
                                                            DefaultMeet>);
static_assert(
    IsCertifiedOrderDistributiveLatticeOperations<int, DefaultJoin,
                                                  DefaultMeet> ==
    IsOrderDistributiveLatticeOperations<int, DefaultJoin, DefaultMeet>);

/**
 * @brief Verify that a two-step path A→B→C exists in the posetal category.
 *
 * @details
 * This helper checks a two-edge witness `A→B` and `B→C` using the supplied
 * relation and projector. The result is expressed in the truth-value codomain
 * Ω of the chosen Logic species L, so it works uniformly for Classical,
 * Ternary, or any other pluggable logical universe — no `bool` hard-codes.
 *
 * If `Rel` also models a posetal relation for the projected carrier,
 * transitivity gives the direct edge `A→C` as a derived fact.
 *
 * @tparam T   Object type.
 * @tparam Rel Relation type used to witness each edge.
 * @tparam L   Logic species providing `AND` and the `Ω` codomain.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = Boole, typename Project = std::identity>
  requires IsPathProjection<Project, T, Rel, typename L::Ω>
constexpr typename L::Ω check_path(T a, T b, T c, Project project = {}) {
  const auto rel = Rel{};
  // A two-step path exists iff both edges are present.
  // Transitivity (axiom of IsPosetal) guarantees the direct edge A→C.
  return L::AND(rel(project(a), project(b)), rel(project(b), project(c)));
}

static_assert(check_path<int, std::less_equal<int>>(1, 2, 3));
static_assert(!check_path<int, std::less_equal<int>>(3, 2, 1));

/**
 * @brief Opt-in drill-down projector for min/max result carriers.
 *
 * @details
 * This helper specializes the upstream `arrow_drill_down` projector from
 * `:mereology` to extract the `min` component used as an order witness.
 *
 * @tparam Whole Carrier type accepted by `arrow_drill_down(whole)` where the
 *         projected object exposes a `min` field.
 * @param whole The projected carrier.
 * @return Reference to the `min` component used as the relation witness.
 */
export template <typename Whole>
constexpr decltype(auto) arrow_drill_down_min(const Whole& whole)
  requires requires { arrow_drill_down(whole).min; }
{
  return arrow_drill_down(whole).min;
}

constexpr std::ranges::min_max_result<int> p1{1, 0};
constexpr std::ranges::min_max_result<int> p2{2, 0};
constexpr std::ranges::min_max_result<int> p3{3, 0};

static_assert(
    IsPathProjection<
        decltype(arrow_drill_down_min<const std::ranges::min_max_result<int>*>),
        const std::ranges::min_max_result<int>*, std::less_equal<int>, bool>);

static_assert(
    check_path<const std::ranges::min_max_result<int>*, std::less_equal<int>,
               Boole>(
        &p1, &p2, &p3,
        arrow_drill_down_min<const std::ranges::min_max_result<int>*>),
    "Opt-in operator-> drill-down must preserve posetal path semantics.");

// ---------------------------------------------------------------------------
// Order-aware morphism classes (#664 acceptance criteria, 2026-05-14).
//
// Sibling vocabulary to @c :morphism's @c IsMonicArrow / @c IsEpicArrow /
// @c IsBijectiveArrow: those are the order-FREE morphism classes (injective /
// surjective / bijective predicates on a single arrow @c F).  The classes
// below add the order-AWARE classes (monotone / anti-monotone / order-
// isomorphism / order-anti-isomorphism), parameterised by the ambient
// order relation @c Op.  Per Mac Lane CWM ("the arrows are just the
// instances of the order relation"), monotone maps are the arrows between
// posetal categories — this is their natural home.
//
// Pattern: same as @c :morphism's @c is_monic_arrow_v / @c IsMonicArrow
// pair — an opt-in trait that defaults to @c false plus a concept that
// gates on @c IsArrow @c && the trait.  White-list witnesses where the
// proof is reachable at landing time; downstream partitions late-bind
// additional proofs by adding their own trait specialisations.  No wrapper
// structs (Juliet posture): the trait carries the structural claim
// without an extra type.
//
// Variance as a LOGIC PROGRAM (#908).  Read together, these specialisations
// form a small Datalog-style rule base whose engine is the compiler's own
// template resolution: a CONSTRAINED partial specialisation IS a Horn clause,
// its @c requires clause the body, its head the trait it sets.  So variance is
// almost never TAGGED per type; it is INFERRED by the same "inference composes,
// tagging explodes" discipline as the rest of the library.  The base facts are
// the irreducible leaves --- @c is_monotone_v<Identity,Op> (covariant) and
// @c is_antimonotone_v<NegationArrow,Op> (contravariant) --- and every
// structured arrow derives its variance from its parts:
//   Copy   @c Δ:A→A×A   monotone  ∀ carrier/order (pairing of monotone legs);
//   Merge  @c ∧:A×A→A   monotone  ⟸ @c IsOrderMeetSemilattice (glb is
//   monotone); Tensor @c R⊗S       monotone  ⟸ @c IsMonotone(R) @c ∧ @c
//   IsMonotone(S), and
//                        antitone  ⟸ both legs antitone (⊗ is a bifunctor).
//   Compose @c g∘f    variance MULTIPLIES: @c co∘co=co, @c anti∘anti=co,
//                      @c co∘anti=anti (registered below, on @c :morphism's
//                      reified @c Compose<F,G>).
// (The Copy / Merge / Tensor clauses live in @c :cartesian_bicategory, next to
// the arrows they classify.)  With composition seated on @c Compose, the rule
// base is closed under the categorical operations (identity, composition,
// product); atomic maps remain the only white-listed base facts.
//
// User's mnemonic ("iso / mono => enabling"): order-isos enable clean
// halfspace-pivot transport (result is again a halfspace); monos-without-
// inverse enable image-with-witness transport (halfspace + a divisibility
// witness on the preimage, e.g.\ scaling on a ring that is not a field).
// The vocabulary below names the enabling shapes so order-transport can
// dispatch on them rather than on per-case positive/negative branching.
// ---------------------------------------------------------------------------

/**
 * @brief User-declared monotonicity witness for an arrow type @c F
 *        with respect to the ambient order relation @c Op.
 *
 * @details Monotonicity cannot be verified at compile time in general
 *          (it quantifies over all input pairs); users specialise this
 *          to @c true to declare that @c F preserves the order @c Op
 *          (∀ @c x, @c y: @c Op(x, @c y) @c ⇒ @c Op(F(x), @c F(y))).
 *          The compiler trusts the declaration; the public review
 *          process is the audit trail.  Mirrors @c is_monic_arrow_v
 *          in @c :morphism (same opt-in pattern).
 *
 *          Default relation @c std::less_equal<> matches the partition
 *          convention (see partition header).
 */
export template <typename F, typename Op = std::less_equal<>>
inline constexpr bool is_monotone_v = false;

/**
 * @concept IsMonotone
 * @brief An arrow @c F declared to be @b order-preserving with respect
 *        to the ambient order relation @c Op.
 *
 * @details ∀ @c x, @c y in @c Dom<F>: @c Op(x, @c y) @c ⇒ @c
 *          Op(F(x), @c F(y)).  Per Mac Lane, this is the canonical
 *          shape of an arrow between posetal categories.
 */
export template <typename F, typename Op = std::less_equal<>>
concept IsMonotone = IsArrow<F> && is_monotone_v<F, Op>;

/**
 * @brief User-declared anti-monotonicity witness for an arrow type
 *        @c F with respect to the ambient order relation @c Op.
 *
 * @details ∀ @c x, @c y: @c Op(x, @c y) @c ⇒ @c Op(F(y), @c F(x))
 *          (order @b reversed).  Mirrors @c is_monotone_v; same
 *          opt-in trait pattern.  Composition rule (audit-trail
 *          property): @c IsAntiMonotone @c ∘ @c IsAntiMonotone @c =
 *          @c IsMonotone (two flips cancel).
 */
export template <typename F, typename Op = std::less_equal<>>
inline constexpr bool is_antimonotone_v = false;

/**
 * @concept IsAntiMonotone
 * @brief An arrow @c F declared to be @b order-reversing with respect
 *        to the ambient order relation @c Op.
 */
export template <typename F, typename Op = std::less_equal<>>
concept IsAntiMonotone = IsArrow<F> && is_antimonotone_v<F, Op>;

/**
 * @concept IsVariant
 * @brief An arrow with a @b definite variance: order-preserving
 *        (@c IsMonotone) or order-reversing (@c IsAntiMonotone).  The
 *        leg-shape of a Galois connection, whose adjoints are always one or
 *        the other.  Mirrors @c :limit::IsBoundaryObject's @c "X @c || @c dual"
 *        disjunction shape.
 * @note Variance is DERIVED for the structured arrows and ASSERTED for the
 *       irreducible leaf maps (#908).  Derived: @c Identity and the negation
 *       @c NegationArrow (below), and the spider @c Copy / @c Merge whose
 *       monotonicity is a theorem gated on @c IsOrderMeetSemilattice
 *       (@c :cartesian_bicategory).  Asserted: atomic order-preserving maps
 *       (the @c ℕ↪ℤ / @c sint / @c uint embeddings) register @c is_monotone_v
 *       directly, since their variance is a fact about that specific map, not a
 *       consequence of its type ("inference composes, tagging explodes": tag
 *       only the irreducible leaves).  A general op-derived
 * variance-composition law (@c co∘co=co, @c antitone∘antitone=monotone) awaits
 * a reified composition arrow; the two-axis (contravariant-domain /
 *       covariant-codomain) reading is the hyperdoctrine extension.
 */
export template <typename F, typename Op = std::less_equal<>>
concept IsVariant =
    // F is an arrow (each disjunct below also implies it, but stated up front
    // so the @c Dom<F> / @c Cod<F> ties are well-formed; the @c &&
    // short-circuits before they are evaluated for a non-arrow).
    IsArrow<F> &&
    // @c Op is not arbitrary: variance is measured against a homogeneous binary
    // relation living on one of the arrow's OWN carriers --- its domain @b or
    // its codomain.  The disjunction is load-bearing: @c Copy<A>'s order is the
    // product order @c ≤× on its @b codomain @c A×A, not an order on its domain
    // @c A, so a domain-only tie would wrongly reject it.
    (
        requires(Op op, Dom<F> a) { op(a, a); } ||
        requires(Op op, Cod<F> c) { op(c, c); }) &&
    (IsMonotone<F, Op> || IsAntiMonotone<F, Op>);

/**
 * @concept IsOrderIsomorphism
 * @brief An arrow that is both an order-preserving map and a bijection.
 *
 * @details Composes the categorical bijection certificate
 *          (@c :morphism::IsBijectiveArrow @c = @c IsMonicArrow @c &&
 *          @c IsEpicArrow) with the order-preservation trait above.
 *          This is the textbook "enabling" class for clean halfspace-
 *          pivot transport in @c :algebra:halfspace_transport: the image
 *          of a halfspace under an order-iso is again a halfspace.
 */
export template <typename F, typename Op = std::less_equal<>>
concept IsOrderIsomorphism = IsBijectiveArrow<F> && IsMonotone<F, Op>;

/**
 * @concept IsOrderAntiIsomorphism
 * @brief An arrow that is both an order-reversing map and a bijection.
 *
 * @details Composes @c IsBijectiveArrow with @c IsAntiMonotone.  Under
 *          this class, halfspace-pivot transport remains clean but the
 *          direction (Upward / Downward) is @b flipped — the structural
 *          reason multiplicative scaling by a negative scalar on an
 *          ordered field flips the halfspace's direction.
 */
export template <typename F, typename Op = std::less_equal<>>
concept IsOrderAntiIsomorphism = IsBijectiveArrow<F> && IsAntiMonotone<F, Op>;

// Identity arrows are monotone under any relation: id(x) = x, so
// Op(x, y) ⇒ Op(id(x), id(y)) holds trivially.
template <typename T, typename Op>
inline constexpr bool is_monotone_v<Identity<T>, Op> = true;

// Identity arrows are therefore order-isomorphisms (they are bijective
// via the @c :morphism registration AND monotone via the above).
static_assert(IsMonotone<Identity<int>>,
              "Identity must be recognised as a monotone arrow.");
static_assert(IsOrderIsomorphism<Identity<int>>,
              "Identity must be recognised as an order-isomorphism "
              "(bijection + monotone).");

// Antitone mirror of the Identity monotone witness above: the logic negation
// arrow @c NegationArrow<L> (¬ = @c L::RFL on the truth object @c Ω, reified in
// @c :logic beside @c logic_complement) is @b order-reversing on the truth
// chain --- @c Op(a,b) @c ⇒ @c Op(¬b,¬a) --- so it grounds @c IsAntiMonotone /
// @c IsVariant the way @c Identity grounds @c IsMonotone (#908).  Registered
// for every @c Op, the antitone analogue of the universal
// @c is_monotone_v<Identity<T>,Op>.  (¬¬=id makes it an involution, morally an
// order-anti-isomorphism; the bijection certificate lives on @c :involution, so
// this claims only the variance.)
template <typename L, typename Op>
inline constexpr bool is_antimonotone_v<NegationArrow<L>, Op> = true;

static_assert(IsAntiMonotone<NegationArrow<Boole>>,
              "Logic negation ¬ is the canonical order-reversing arrow "
              "(the antitone mirror of Identity's monotone witness).");
static_assert(IsAntiMonotone<NegationArrow<Kleene>>,
              "K₃ negation reflects the chain about Unknown: order-reversing.");
static_assert(IsVariant<NegationArrow<Boole>>,
              "¬ has a definite variance (antitone), hence IsVariant.");

// Covariant mirror of the negation witness: the dominance inclusion
// @c ι:𝔹↪Ω (@c LiftLogic<L>, reified in @c :logic beside @c lift_logic) is
// @b order-PRESERVING --- @c ⊥≤⊤ embeds as @c False≤True in @c Ω --- so it
// grounds @c IsMonotone / @c IsVariant on the CODOMAIN axis of a classifier
// @c χ:A→Ω the way @c NegationArrow grounds the antitone case (#908).  It is
// the covariant leg the two-axis reading (#894/#897) postcomposes onto.
// Registered for every @c Op, the covariant analogue of the universal
// @c is_monotone_v<Identity<T>,Op>.
template <typename L, typename Op>
inline constexpr bool is_monotone_v<LiftLogic<L>, Op> = true;

static_assert(IsMonotone<LiftLogic<Boole>>,
              "The dominance inclusion 𝔹↪Ω is order-preserving (⊥↦False, "
              "⊤↦True): the covariant codomain axis.");
static_assert(IsMonotone<LiftLogic<Kleene>>,
              "𝔹↪K₃ embeds bool as {False,True}, order-preserving.");
static_assert(IsVariant<LiftLogic<Boole>>,
              "ι has a definite variance (covariant), hence IsVariant.");

// The COMPOSITION clause of the variance logic program (#908), registered on
// the reified composition arrow @c Compose<F,G> = @c g∘f (@c :morphism).  The
// classic sign rule: variance MULTIPLIES along composition, so two definite
// variances compose to a definite one and two flips cancel.
//   co∘co   = co        (monotone ∘ monotone)
//   anti∘anti = co       (two order-reversals cancel)
//   co∘anti = anti,  anti∘co = anti
// Each is a Horn clause whose premises gate on the @c IsMonotone /
// @c IsAntiMonotone CONCEPTS of the legs; the disjunction in one specialization
// (rather than two same-headed specializations) keeps overload selection
// unambiguous.  This is the rule that was MISSING while composition was only
// the ad-hoc @c operator>>; reifying @c Compose seated it.
template <typename F, typename G, typename Op>
  requires((IsMonotone<F, Op> && IsMonotone<G, Op>) ||
           (IsAntiMonotone<F, Op> && IsAntiMonotone<G, Op>))
inline constexpr bool is_monotone_v<Compose<F, G>, Op> = true;

template <typename F, typename G, typename Op>
  requires((IsMonotone<F, Op> && IsAntiMonotone<G, Op>) ||
           (IsAntiMonotone<F, Op> && IsMonotone<G, Op>))
inline constexpr bool is_antimonotone_v<Compose<F, G>, Op> = true;

static_assert(IsMonotone<Compose<Identity<int>, Identity<int>>>,
              "co∘co=co: identity composed with itself is monotone.");
static_assert(IsMonotone<Compose<NegationArrow<Boole>, NegationArrow<Boole>>>,
              "anti∘anti=co: double negation ¬∘¬ is order-PRESERVING (two "
              "flips cancel).");
static_assert(IsAntiMonotone<Compose<Identity<bool>, NegationArrow<Boole>>>,
              "co∘anti=anti: ¬ after id is order-reversing.");

// ---------------------------------------------------------------------------
// The product of posets is a poset (componentwise order lift).
//
// The order on @c std::pair<A,B> is the COMPONENTWISE product order
//   (a,c) ≤× (b,d)  :⟺  a ≤ b  ∧  c ≤ d,
// NOT the lexicographic @c std::less_equal<std::pair<...>>.  @c ProductLeq is
// the introduction rule ("from a≤b and c≤d infer (a,c)≤(b,d)"): it ANDs the two
// component comparisons and returns the classifier's @c Ω.  The posetal axioms
// then lift componentwise --- a product of reflexive / transitive /
// antisymmetric relations is again reflexive / transitive / antisymmetric ---
// so
// @c IsPosetal<std::pair<A,B>, ≤×> is INFERRED whenever both components are
// posetal, exactly as @c :numbers lifts species traits componentwise onto a
// composite carrier.  Downstream (@c :cartesian_bicategory) the comonoid's
// copy / merge legs are declared monotone against THIS order, not the lex one.
// ---------------------------------------------------------------------------

/** @brief Componentwise product order on @c std::pair @c ≤×: @c (a,c)≤(b,d)
 *         @c :⟺ @c a≤b @c ∧ @c c≤d.  The order-theoretic product of two posets,
 *         @b not the lexicographic @c std::less_equal on the pair.
 *  @tparam LeqA the order on the first component.
 *  @tparam LeqB the order on the second component. */
export template <typename LeqA, typename LeqB>
struct ProductLeq {
  /** @brief AND the two component comparisons.
   *  @param x the left pair @c (a,c).
   *  @param y the right pair @c (b,d).
   *  @return @c a≤b @c && @c c≤d, the componentwise product-order verdict. */
  template <typename A, typename B>
  constexpr bool operator()(const std::pair<A, B>& x,
                            const std::pair<A, B>& y) const {
    return LeqA{}(x.first, y.first) && LeqB{}(x.second, y.second);
  }
};

// The three posetal axioms lift componentwise onto the pair under @c ≤×.
template <typename A, typename B, typename LeqA, typename LeqB>
inline constexpr bool is_reflexive_v<std::pair<A, B>, ProductLeq<LeqA, LeqB>> =
    is_reflexive_v<A, LeqA> && is_reflexive_v<B, LeqB>;
template <typename A, typename B, typename LeqA, typename LeqB>
inline constexpr bool is_transitive_v<std::pair<A, B>, ProductLeq<LeqA, LeqB>> =
    is_transitive_v<A, LeqA> && is_transitive_v<B, LeqB>;
template <typename A, typename B, typename LeqA, typename LeqB>
inline constexpr bool
    is_antisymmetric_v<std::pair<A, B>, ProductLeq<LeqA, LeqB>> =
        is_antisymmetric_v<A, LeqA> && is_antisymmetric_v<B, LeqB>;

// Product-of-posets is a poset: the componentwise lift makes @c ≤× a certified
// order, so the pair carrier the comonoid copies into is a genuine poset.
static_assert(
    IsPosetal<std::pair<int, int>,
              ProductLeq<std::less_equal<int>, std::less_equal<int>>>,
    "ℤ×ℤ under the componentwise product order ≤× is a poset (product of "
    "posets is a poset).");

}  // namespace dedekind::category
