export module dedekind.ontology:topology;

import :mereology;

namespace dedekind::ontology {

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
