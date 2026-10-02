/**
 * @file dedekind/topology/neighborhood.cppm
 * @brief The Rules of Continuity (Neighborhoods, Limits, and Cuts).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :topology
 * @build_order 6
 * @dependency :algebra, :order
 *
 * @section neighborhood__Topology
 * This partition establishes the qualitative "shape" of our species. It
 * transforms discrete algebraic structures (like Q) into continuous spaces
 * (like R) by defining the concepts of "closeness" and "convergence".
 *
 * @details
 * This module defines the formal boundaries of the continuum:
 * - IsOpen / IsClosed: The "Skin" and "Body" of a set.
 * - IsNeighborhood: The "Space Around" a point.
 * - IsSequence / HasLimit: The "Path" to a point (Convergence).
 * - IsDedekindComplete: The "Seamless" property (No gaps).
 *
 * @section neighborhood__Structural_Synthesis
 * We synthesize the Order from (:order) and the Metric from (:algebra)
 * to define the 'Dedekind Cut'. This is the ultimate "Promotion" in the
 * library: moving from the Discrete (N, Z) to the Continuous (R).
 *
 * @anchors C++ Concepts: std::floating_point (The Machine Approximation of R).
 *
 * Wikipedia: Topology, Dedekind cut, Metric space, Limit of a sequence
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "I am superior, sir, in many ways. But I would gladly give it up, to be
 * Human."
 *       -- Data, Star Trek: The Next Generation, "Encounter at Farpoint" (1987)
 */
module;
#include <concepts>
#include <functional>
#include <type_traits>  // std::remove_cvref_t

export module dedekind.topology:neighborhood;

import dedekind.category;
import dedekind.sets;
import dedekind.order;

namespace dedekind::topology {
using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

/**
 * @concept HasDiscreteCarrier
 * @brief The set's carrier is a @b discrete chain, so its order topology is
 *        the @b discrete topology --- and there @b every subset is clopen.
 *
 * @details Detected structurally from the domain: a carrier with an NNO step
 *          (@c category::HasNNOStep --- @c successor / @c predecessor exist:
 *          the built-in @c std::integral types and the ℕ proxy
 *          @c Cardinality; the ℤ proxy @c SignedCardinality joins the moment it
 *          declares its step) is successor-isolated, so no subset has a limit
 *          point outside itself and every subset is both @b open and
 *          @b closed.  The step IS the discreteness witness: Peano's axioms
 *          (an injective successor with induction) leave no element strictly
 *          between @c n and @c succ(n), so a carrier that overloads
 *          @c successor / @c predecessor asserts exactly that covering relation
 *          --- the same kind of semantic contract as registering
 *          @c is_associative_v.  A dense carrier (@c Rational, @c Cut) has no
 *          such step and must not declare one.  This is the structural source
 * of clopen-ness on discrete carriers (#905); a @b dense carrier (@c Rational,
 * @c Cut) has no step, so its open / closed status falls to the boundary
 * structure
 *          --- the genuine @c open @c ⊋ @c clopen witness a discrete carrier
 *          cannot provide.
 *
 * @note This is also the @b bridge between two topologies that the library
 *       keeps apart on purpose.  @c IsOpen / @c IsClosed / @c IsClopen below
 *       speak the carrier's @b order @b topology; @c
 * sets::HasDecidableMembership speaks the @b decidability (Sierpiński)
 * topology, in which "clopen = decidable" (Smyth / Rosolini / Escardó).  On a
 * dense carrier the two differ --- an open ray on ℚ is decidable (exact
 * rationals) yet not order-closed --- so neither concept may be defined from
 * the other.  On a discrete carrier the order topology IS discrete, every
 * subset is order-clopen, and the two readings coincide: that is this concept.
 */
export template <typename S>
concept HasDiscreteCarrier =
    dedekind::category::IsPredicate<S> &&
    (std::integral<dedekind::category::Dom<S>> ||
     dedekind::category::HasNNOStep<dedekind::category::Dom<S>>);

/**
 * @concept IsOpen
 * @brief A set where every point has a neighborhood entirely within the set.
 * @note Arity: Updated to structuralist 1-arg IsSet.
 */
/** @section neighborhood__Order_Reading
 *  The order-topology reading of @c order's shapes, inferred from their
 *  @b strictness --- the datum is order-theoretic (does the pivot belong:
 *  @c > vs @c ≥), the open / closed verdict is its topological consequence
 *  on a dense chain: a strict ray is open, a non-strict one closed, a point is
 *  closed, finite meets / joins preserve both, and complement swaps them.  On a
 *  discrete carrier @ref HasDiscreteCarrier makes everything clopen regardless.
 *  No shape carries a hand tag. */
/** @brief Is @c S order-open by its strictness?  @c false unless a
 *  specialisation below (or a carrier's own) says so. */
export template <typename S>
inline constexpr bool is_order_open_v = false;
/** @brief Is @c S order-closed by its strictness?  Dual of
 *  @ref is_order_open_v. */
export template <typename S>
inline constexpr bool is_order_closed_v = false;
template <typename T, Direction D, typename L>
inline constexpr bool is_order_open_v<Halfspace<T, D, Strictness::Strict, L>> =
    true;
template <typename T, Direction D, typename L>
inline constexpr bool
    is_order_closed_v<Halfspace<T, D, Strictness::NonStrict, L>> = true;
template <typename T, typename L>
inline constexpr bool is_order_closed_v<Singleton<T, L>> = true;
template <typename A, typename B>
inline constexpr bool is_order_open_v<Meet<A, B>> =
    is_order_open_v<A> && is_order_open_v<B>;
template <typename A, typename B>
inline constexpr bool is_order_closed_v<Meet<A, B>> =
    is_order_closed_v<A> && is_order_closed_v<B>;
template <typename A, typename B>
inline constexpr bool is_order_open_v<Join<A, B>> =
    is_order_open_v<A> && is_order_open_v<B>;
template <typename A, typename B>
inline constexpr bool is_order_closed_v<Join<A, B>> =
    is_order_closed_v<A> && is_order_closed_v<B>;
template <typename A>
inline constexpr bool is_order_open_v<Not<A>> = is_order_closed_v<A>;
template <typename A>
inline constexpr bool is_order_closed_v<Not<A>> = is_order_open_v<A>;

/**
 * @concept IsOpen
 * @brief A set where every point has a neighborhood entirely within the set
 *        --- in the carrier's @b order topology.
 * @details Inferred, never tagged: ∅ and X (the boundary subobjects) are
 *          clopen in every topology; on a discrete carrier every subset is
 *          clopen (@ref HasDiscreteCarrier); otherwise the shape's strictness
 *          decides (@ref is_order_open_v).
 */
export template <typename S>
concept IsOpen =
    dedekind::category::IsPredicate<S> &&
    (dedekind::category::IsBoundaryObject<S> || HasDiscreteCarrier<S> ||
     is_order_open_v<std::remove_cvref_t<S>>);  // decltype(x) may be const

/**
 * @concept IsClosed
 * @brief A set that contains all its limit points --- in the carrier's
 *        @b order topology.  Same three-leg inference as @ref IsOpen.
 */
export template <typename S>
concept IsClosed =
    dedekind::category::IsPredicate<S> &&
    (dedekind::category::IsBoundaryObject<S> || HasDiscreteCarrier<S> ||
     is_order_closed_v<std::remove_cvref_t<S>>);

/**
 * @concept IsClopen
 * @brief A set that is BOTH open and closed --- the topological face of
 *        @b decidability.
 *
 * @details In synthetic topology (Smyth / Rosolini / Escardó, the project's own
 *          @c Rosolini-dominance foundation) @b open @c = semidecidable /
 *          affirmable and @b closed @c = refutable, so @b clopen @c = @b open
 *          @c ∩ @b closed @c = @b decidable.  @c IsClopen is the @b topological
 *          conservative certificate of that in the carrier's @b order
 *          topology; @c sets::HasDecidableMembership (@c logic_species @c ==
 *          @c Boole) is the certificate in the @b decidability (Sierpiński)
 *          topology.  They are @b different @b topologies, so neither is
 *          defined from the other: on a dense carrier an open ray on ℚ is
 *          decidable yet not order-closed, and a Kleene-tagged set on a
 *          discrete carrier is order-clopen yet not recognised decidable (the
 *          #847 gap).  On a discrete carrier (@ref HasDiscreteCarrier) every
 *          subset is order-clopen, so there the decidability reader @b implies
 *          the order reader; the converse still fails for a Kleene-tagged set,
 *          whose @c logic_species discreteness does not change.  Stone duality
 *          glues the decidability reading to the Boolean-ring one (#903,
 *          #894); the clopen sublattice of @c Ω measures decidability =
 *          disconnectedness (@c Boole totally disconnected → fully decidable; a
 *          Kleene chain highly connected → only the poles @c ⊥ / @c ⊤ clopen).
 *  @see dedekind::sets::HasDecidableMembership
 */
export template <typename S>
concept IsClopen = IsOpen<S> && IsClosed<S>;

// The boundary sets Ø, 𝔸 are the archetypal clopen sets (∅ and X are open ∧
// closed in EVERY topology) and the ⊥/⊤ bounds of Sub(U).  IsClopen and
// HasDecidableMembership are INDEPENDENT certificates (see the concept doc):
// they coincide on the Boole-tagged core witnessed here, but a Kleene-tagged
// clopen boundary (Ø<int,Kleene>) is clopen yet NOT recognized-decidable ---
// the #847 gap the #894 codomain reduction closes by retagging Ø<T,L> →
// Ø<T,Boole>. So the two witnesses below record the COINCIDENCE on the core,
// not an implication.
static_assert(
    IsClopen<Ø<int, Boole>> && IsClopen<𝔸<int, Boole>>,
    "Ø and 𝔸 are clopen: ∅ and X are open ∧ closed in every topology");
static_assert(HasDecidableMembership<Ø<int, Boole>> &&
                  HasDecidableMembership<𝔸<int, Boole>>,
              "and decidable: on the boundary objects both readings hold");

/**
 * @concept IsNeighborhood
 * @brief A set that "surrounds" a point p.
 * @details Synthesized from the Open set morphology.
 */
export template <typename N, typename T>
concept IsNeighborhood =
    IsOpen<N> &&
    // A neighbourhood must be able to SURROUND a point, so it cannot be empty:
    // exclude the initial boundary.  Ø is IsOpen (clopen) but is a
    // neighbourhood of no point; point-aware containment is a runtime property,
    // so this is the type-level guard (#904 CP).
    !dedekind::category::IsInitialObject<N> && requires(N n, T p) {
      { n(p) } -> IsΩ;
    };

// Regression (#904): Ø is open but is a neighbourhood of no point.
static_assert(!IsNeighborhood<Ø<int, Boole>, int>,
              "the empty set is IsOpen but not a neighbourhood");

/**
 * @section neighborhood__Topology_2
 */

export template <typename S>
inline constexpr bool is_convex_v = false;

/**
 * @concept IsConvex
 * @brief A Set that satisfies the convexity theorem (no holes).
 */
export template <typename S>
concept IsConvex =
    dedekind::category::IsPredicate<S> && is_convex_v<std::remove_cvref_t<S>>;

/**
 * @concept IsConvexMagmoid
 * @brief Convex sets form a Magmoid under the intersection operation.
 * @details We use the Categorical Magmoid here to avoid a circular
 * dependency on the Algebra module's Magma.
 */
// Convexity of order's shapes: a principal up-/down-set and a point are
// convex, and a finite meet of convex sets is convex.
template <typename T, Direction D, Strictness St, typename L>
inline constexpr bool is_convex_v<Halfspace<T, D, St, L>> = true;
template <typename T, typename L>
inline constexpr bool is_convex_v<Singleton<T, L>> = true;
template <typename A, typename B>
inline constexpr bool is_convex_v<Meet<A, B>> =
    is_convex_v<A> && is_convex_v<B>;

// The dense-carrier witnesses (a strict ray on ℚ is open and NOT closed) live
// downstream with the carriers that are genuinely dense and totally ordered:
// numbers/neighborhood_test.  Here only the discrete reading is pinned.
static_assert(IsClopen<Halfspace<int, Direction::Upward, Strictness::Strict>>,
              "a ray on a discrete carrier is clopen");
static_assert(IsConvex<Interval<int, Strictness::Strict, Strictness::Strict>>,
              "an interval is convex");

}  // namespace dedekind::topology
