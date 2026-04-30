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
 * @concept IsNeighborhood
 * @brief A set that "surrounds" a point p.
 * @details Synthesized from the Open set morphology.
 */
export template <typename N, typename T>
concept IsNeighborhood = IsOpen<N> && requires(N n, T p) {
  { n(p) } -> LogicalValue;
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
