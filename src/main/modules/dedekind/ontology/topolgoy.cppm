/**
 * @file ontology:topology.cppm
 * @brief Topological Foundations: Openness, Closure, and Boundaries.
 * 
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 * 
 * @section Topology: The study of neighborhoods, boundaries, and continuity.
 * @details This partition establishes the qualitative "shape" of our sets before 
 *          they are quantified by coordinates.
 * Wikipedia: Topology, Open set, Closed set, Limit point
 */

export module dedekind.ontology:topology;

import :mereology;

namespace dedekind::ontology {

/**
 * @concept IsOpen
 * @brief A set where every point has a neighborhood entirely within the set.
 * @note Wikipedia: Open set
 */
export template <typename S>
concept IsOpen = IsSet<S, typename S::element_type> && requires {
    typename S::is_open_tag;
};

/**
 * @concept IsClosed
 * @brief A set that contains all its limit points.
 * @note Wikipedia: Closed set
 */
export template <typename S>
concept IsClosed = IsSet<S, typename S::element_type> && requires {
    typename S::is_closed_tag;
};

/**
 * @concept IsSequence
 * @brief A discrete mapping from the Natural Numbers to a target species.
 * @tparam Seq The sequence type.
 * @tparam T The element type of the mapping.
 * @note Wikipedia: Sequence, Infinite sequence
 */
export template <typename Seq, typename T>
concept IsSequence = requires(Seq s, size_t n) {
    { s[n] } -> std::convertible_to<T>;
};

/** 
 * @section Topology: The study of solid shapes and boundaries.
 * 
 * @brief Axiom: Is the set morphologically solid (no holes)?
 * Wikipedia: Convex set
 */
export template <typename S>
inline constexpr bool is_convex_v = false;

/**
 * @concept IsConvex
 * @brief A Set that satisfies the convexity theorem.
 * Wikipedia: Convex set, Line segment
 */
export template <typename S>
concept IsConvex = IsSet<S, typename S::element_type> && is_convex_v<S>;

/**
 * @concept IsHalfSpace
 * @brief A Convex Set defined by a single "Naked" boundary.
 * Wikipedia: Half-space (geometry), Ray (geometry)
 */
export template <typename S>
concept IsHalfSpace = IsConvex<S> && requires {
    typename S::is_ray_tag; // Structural proof
    S::bound;               // The naked limit
};

/**
 * @concept IsInterval
 * @brief A "Molecule" formed by the intersection (Meet) of two Half-Spaces.
 * Wikipedia: Interval (mathematics)
 */
export template <typename S>
concept IsInterval = IsConvex<S> && requires {
    typename S::lower_ray_type;
    typename S::upper_ray_type;
    requires IsHalfSpace<typename S::lower_ray_type>;
    requires IsHalfSpace<typename S::upper_ray_type>;
};

} // namespace dedekind::ontology
