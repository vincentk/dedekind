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
 * @concept IsOpen
 * @brief A set where every point has a neighborhood entirely within the set.
 * @note Arity: Updated to structuralist 1-arg IsSet.
 */
export template <typename S>
concept IsOpen =
    dedekind::category::IsPredicate<S> && requires { typename S::is_open_tag; };

/**
 * @concept IsClosed
 * @brief A set that contains all its limit points.
 */
export template <typename S>
concept IsClosed = dedekind::category::IsPredicate<S> &&
                   requires { typename S::is_closed_tag; };

/**
 * @concept IsClopen
 * @brief A set that is BOTH open and closed --- the topological face of
 *        @b decidability.
 *
 * @details In synthetic topology (Smyth / Rosolini / Escardó, the project's own
 *          @c Rosolini-dominance foundation) @b open @c = semidecidable /
 *          affirmable and @b closed @c = refutable, so @b clopen @c = @b open
 *          @c ∩ @b closed @c = @b decidable.  This is the topological name for
 *          @c sets::HasDecidableMembership and the @c Σ @c ∩ @c ¬Σ core of
 * #894, glued to the algebraic Boolean-ring reading by Stone duality (#903).
 *          The clopen sublattice of @c Ω @b is the decidable fragment, and its
 *          size measures decidability = disconnectedness: @c Boole is totally
 *          disconnected (every proposition clopen → fully decidable); a Kleene
 *          chain is highly connected (only the poles @c ⊥ / @c ⊤ clopen → a
 *          large undecidable interior).
 */
export template <typename S>
concept IsClopen = IsOpen<S> && IsClosed<S>;

// The boundary sets Ø, 𝔸 are the archetypal clopen sets (∅ and X are open ∧
// closed in EVERY topology) and the ⊥/⊤ decidable core of Sub(U): the Stone
// bridge in miniature (#903).  On the decided (Boole) core, clopen and
// HasDecidableMembership coincide.  (A Kleene-tagged boundary is still clopen
// but conservatively NOT recognized-decidable --- the #847 gap the #894
// codomain reduction closes by retagging Ø<T,L> → Ø<T,Boole>.)
static_assert(
    IsClopen<Ø<int, Boole>> && IsClopen<UniversalSet<int, Boole>>,
    "Ø and 𝔸 are clopen: ∅ and X are open ∧ closed in every topology");
static_assert(HasDecidableMembership<Ø<int, Boole>> &&
                  HasDecidableMembership<UniversalSet<int, Boole>>,
              "and decidable: the clopen boundary core IS the decidable core "
              "(Stone: clopen = decidable)");

/**
 * @concept IsNeighborhood
 * @brief A set that "surrounds" a point p.
 * @details Synthesized from the Open set morphology.
 */
export template <typename N, typename T>
concept IsNeighborhood = IsOpen<N> && requires(N n, T p) {
  { n(p) } -> IsΩ;
};

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
