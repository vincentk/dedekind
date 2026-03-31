/**
 * @file ontology:topology.cppm
 * @partition :shapes
 * @brief Level 3.1: Topological Mereology (Rays and Intervals).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * @section Shapes: The Geometry of Boundaries
 * This partition defines the fundamental morphological units of the continuum.
 * Following the Dedekind construction, we define the "Half-Space" as a Ray
 * and the "Bounded Space" as an Interval.
 *
 * @details
 * - Ray: An Idempotent Semigroupoid representing a directed infinity.
 * - Interval: The mereological intersection (Meet) of two opposing Rays.
 * - Direction: The categorical orientation (Upward/Downward) of the mapping.
 *
 * @build_order 6.1
 * @dependency :topology, :order, :mereology
 */
module;
#include <concepts>
#include <functional>
export module dedekind.topology:shapes;

import dedekind.order;
import dedekind.sets;
import :topology;

namespace dedekind::topology {

using namespace dedekind::sets;
using namespace dedekind::order;

export enum class Direction { Upward, Downward };

/**
 * @class Ray
 * @brief A Half-Space satisfying the Idempotent Semigroupoid laws.
 */
export template <IsTotallyOrdered T, Direction D, typename L = ClassicalLogic>
class Ray {
 public:
  using element_type = T;
  using is_open_tag = void;
  using is_ray_tag = void;

  constexpr explicit Ray(T pivot) : pivot_(pivot) {}

  constexpr T pivot() const { return pivot_; }

  /** @section Logic: Characteristic Function */
  constexpr auto operator()(const T& x) const {
    if constexpr (D == Direction::Upward)
      return x > pivot_ ? L::True : L::False;
    else
      return x < pivot_ ? L::True : L::False;
  }

  /** @section Algebraic_Laws: The Rhyme of the Lattice */
  friend constexpr Ray operator|(const Ray& a, const Ray& b) {
    if constexpr (D == Direction::Upward)
      return Ray{std::min(a.pivot_, b.pivot_)};  // Union of upward rays
    else
      return Ray{std::max(a.pivot_, b.pivot_)};  // Union of downward rays
  }

  friend constexpr Ray operator&(const Ray& a, const Ray& b) {
    if constexpr (D == Direction::Upward)
      return Ray{std::max(a.pivot_, b.pivot_)};  // Intersection
    else
      return Ray{std::min(a.pivot_, b.pivot_)};
  }

 private:
  T pivot_;
};

/**
 * @class Interval
 * @brief The "Molecule" formed by the intersection of two Rays.
 */
export template <IsTotallyOrdered T, typename L = ClassicalLogic>
class Interval {
 public:
  using element_type = T;
  using lower_ray_type = Ray<T, Direction::Upward, L>;
  using upper_ray_type = Ray<T, Direction::Downward, L>;

  constexpr Interval(T low, T high) : lower_(low), upper_(high) {}

  constexpr auto operator()(const T& x) const { return lower_(x) && upper_(x); }

  constexpr T lower_bound() const { return lower_.pivot(); }
  constexpr T upper_bound() const { return upper_.pivot(); }

 private:
  lower_ray_type lower_;
  upper_ray_type upper_;
};

/** @section Trait_Registration */
export template <typename T, Direction D, typename L>
inline constexpr bool is_convex_v<Ray<T, D, L>> = true;

export template <typename T, typename L>
inline constexpr bool is_convex_v<Interval<T, L>> = true;

/** @section Formal_Verification */
static_assert(IsJoinSemilattice<Ray<double, Direction::Upward>>,
              "Axiom Failure: Rays must be Directed Sets (Join-Semilattices).");

static_assert(IsMeetSemilattice<Ray<double, Direction::Upward>>,
              "Axiom Failure: Rays must satisfy Idempotent Intersection.");

}  // namespace dedekind::topology
