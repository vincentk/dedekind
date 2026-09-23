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
 * @brief The set's carrier is a @b discrete space, so its order topology is the
 *        @b discrete topology --- and there @b every subset is clopen.
 *
 * @details Detected structurally from the domain: an @c std::integral carrier
 *          (@c ℤ, @c ℕ, @c bool, ...) is successor-isolated (there is no point
 *          strictly between @c n and @c n+1), so no subset has a limit point
 *          outside itself.  Every subset is therefore both @b open and
 *          @b closed.  This is the structural source of clopen-ness on discrete
 *          carriers (#905): it replaces the per-shape @c is_open / @c is_closed
 *          hand-tags on integer shapes, and it repairs the #904 CP finding that
 *          @c Ray<int,...> "carries only @c is_open_tag" although @c {n>p} @c =
 *          @c {n≥p+1} is genuinely clopen on the discrete order.
 *
 * @note @c order::IsDiscrete is the order-theoretic sibling, but it is
 *       architecturally withheld on @c int (signed @c + is not a @c Magma, so
 *       @c IsSuccessor fails) and is not exported; @c std::integral is the
 *       sanctioned discreteness proxy the topology layer keys on (issue #905:
 *       "a discrete carrier, e.g.\ @c std::integral").  A @b dense carrier
 *       (@c Rational, @c Cut) is @c !HasDiscreteCarrier, so open/closed there
 *       falls to the boundary structure --- the genuine @c open @c ⊋ @c clopen
 *       witness @c int cannot provide.
 */
export template <typename S>
concept HasDiscreteCarrier = dedekind::category::IsPredicate<S> &&
                             std::integral<dedekind::category::Dom<S>>;

/**
 * @concept IsOpen
 * @brief A set where every point has a neighborhood entirely within the set.
 * @note Arity: Updated to structuralist 1-arg IsSet.
 */
export template <typename S>
concept IsOpen =
    dedekind::category::IsPredicate<S> &&
    // INFERENCE (not tag), in preference order:
    //  * ∅ and X --- the boundary subobjects (⊥/⊤) --- are clopen in EVERY
    //    topology, so their openness is derived from @c IsBoundaryObject
    //    (#904);
    //  * a @c HasDiscreteCarrier set is clopen (discrete topology, #905);
    //  * otherwise the shape's boundary structure is reified as @c is_open_tag
    //    --- the dense-carrier seam @c IsOpen cannot infer, since the boundary
    //    enum lives downstream of this partition (see @c :interval).
    (dedekind::category::IsBoundaryObject<S> || HasDiscreteCarrier<S> ||
     requires { typename S::is_open_tag; });

/**
 * @concept IsClosed
 * @brief A set that contains all its limit points.
 */
export template <typename S>
concept IsClosed =
    dedekind::category::IsPredicate<S> &&
    // Same three-leg inference as @c IsOpen: boundary object (#904), discrete
    // carrier (#905), else the reified @c is_closed_tag dense-carrier seam.
    (dedekind::category::IsBoundaryObject<S> || HasDiscreteCarrier<S> ||
     requires { typename S::is_closed_tag; });

/**
 * @concept IsClopen
 * @brief A set that is BOTH open and closed --- the topological face of
 *        @b decidability.
 *
 * @details In synthetic topology (Smyth / Rosolini / Escardó, the project's own
 *          @c Rosolini-dominance foundation) @b open @c = semidecidable /
 *          affirmable and @b closed @c = refutable, so @b clopen @c = @b open
 *          @c ∩ @b closed @c = @b decidable.  @c IsClopen is the @b topological
 *          conservative certificate of that; @c sets::HasDecidableMembership
 *          (@c logic_species @c == @c Boole) is the @b classifier one.  They
 * are
 *          @b independent --- neither is defined from the other, and they
 *          coincide only on the Boole-tagged core (a Kleene-tagged clopen set
 * is clopen but NOT recognized-decidable, the #847 gap).  Both approximate the
 * synthetic-topology identity "clopen = decidable" from the topology and
 * classifier sides, which Stone duality glues to the Boolean-ring reading
 * (#903, #894).  The clopen sublattice of @c Ω measures decidability =
 * disconnectedness (@c Boole totally disconnected → fully decidable; a Kleene
 * chain highly connected → only the poles @c ⊥ /
 *          @c ⊤ clopen).
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
    IsClopen<Ø<int, Boole>> && IsClopen<UniversalSet<int, Boole>>,
    "Ø and 𝔸 are clopen: ∅ and X are open ∧ closed in every topology");
static_assert(HasDecidableMembership<Ø<int, Boole>> &&
                  HasDecidableMembership<UniversalSet<int, Boole>>,
              "and, on the Boole core, decidable: the two independent "
              "certificates coincide there (they do not imply each other)");

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
concept IsConvex = dedekind::category::IsPredicate<S> && is_convex_v<S>;

/**
 * @concept IsConvexMagmoid
 * @brief Convex sets form a Magmoid under the intersection operation.
 * @details We use the Categorical Magmoid here to avoid a circular
 * dependency on the Algebra module's Magma.
 */
export template <typename S>
concept IsConvexMagmoid = IsConvex<S> && requires(S a, S b) {
  { a & b } -> std::same_as<S>;
};

/**
 * @concept IsHalfSpace
 * @brief A Convex Set defined by a single "Naked" boundary (Ray).
 */
export template <typename S>
concept IsHalfSpace = IsConvex<S> && requires { typename S::is_ray_tag; } &&
                      requires(S s) { s.pivot(); };

/**
 * @concept IsRay
 * @brief A set representing all points greater than (or less than) a pivot.
 */
export template <typename R, typename T>
concept IsRay = IsTotallyOrdered<T> && requires(T pivot) {
  { R::upward_from(pivot) } -> std::same_as<R>;
  { R::downward_from(pivot) } -> std::same_as<R>;
};

/**
 * @concept IsInterval
 * @brief A "Molecule" formed by the intersection of two Half-Spaces.
 */
export template <typename S>
concept IsInterval = IsConvex<S> && requires {
  typename S::lower_ray_type;
  typename S::upper_ray_type;
  requires IsHalfSpace<typename S::lower_ray_type>;
  requires IsHalfSpace<typename S::upper_ray_type>;
};

}  // namespace dedekind::topology
