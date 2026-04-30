/**
 * @file dedekind/topology/interval.cppm
 * @partition :shapes
 * @brief Level 3.1: Topological Mereology (Rays and Intervals).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 *
 * @section interval__Shapes: The Geometry of Boundaries
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
 *
 * @note "Ang hindi marunong lumingon sa pinanggalingan ay hindi makakarating sa
 * kanyang paroroonan." — Jose Rizal, Tagalog Wikiquote. [Trans: "One who does
 * not know how to look back to where one came from will never reach one's
 * destination."]
 */
module;
#include <cassert>
#include <concepts>
#include <functional>
export module dedekind.topology:interval;

import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :neighborhood;

namespace dedekind::topology {

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

// @c Direction is shared with @c dedekind::order::halfspace (the
// upstream NTTP-pivot Halfspace family).  Re-exported here as an alias
// so the runtime-pivot @c Ray / @c HalfSpace family in this partition
// reads against the same enum the order layer already publishes —
// removes a duplicate definition and keeps the two halfspace families
// (compile-time NTTP-pivot vs runtime-pivot) speaking the same
// orientation vocabulary.  Per #414 / cross-family alignment sweep.
export using Direction = dedekind::order::Direction;

// @c Boundary is the runtime-pivot family's boundary-inclusion enum;
// @c dedekind::order::Strictness is the NTTP-pivot family's boundary-
// inclusion enum.  They are isomorphic — see @c to_strictness /
// @c to_boundary helpers below.  Kept as separate enums for now (the
// two families already use them as NTTPs in distinct surfaces); a
// unified-enum sweep is a future refactor.
export enum class Boundary { Open, Closed };

/** @brief Cross-family conversion: runtime-pivot @c Boundary to
 *         NTTP-pivot @c Strictness.  @c Boundary::Open ↔
 *         @c Strictness::Strict; @c Boundary::Closed ↔
 *         @c Strictness::NonStrict.  @c constexpr so it composes in
 *         NTTP positions across the two families. */
export constexpr dedekind::order::Strictness to_strictness(
    Boundary b) noexcept {
  return b == Boundary::Open ? dedekind::order::Strictness::Strict
                             : dedekind::order::Strictness::NonStrict;
}

/** @brief Cross-family conversion: NTTP-pivot @c Strictness to
 *         runtime-pivot @c Boundary.  Inverse of @c to_strictness. */
export constexpr Boundary to_boundary(dedekind::order::Strictness s) noexcept {
  return s == dedekind::order::Strictness::Strict ? Boundary::Open
                                                  : Boundary::Closed;
}

// Cross-family equivalence witnesses (round-trip invariance).
static_assert(to_strictness(Boundary::Open) ==
              dedekind::order::Strictness::Strict);
static_assert(to_strictness(Boundary::Closed) ==
              dedekind::order::Strictness::NonStrict);
static_assert(to_boundary(dedekind::order::Strictness::Strict) ==
              Boundary::Open);
static_assert(to_boundary(dedekind::order::Strictness::NonStrict) ==
              Boundary::Closed);
static_assert(to_boundary(to_strictness(Boundary::Open)) == Boundary::Open);
static_assert(to_boundary(to_strictness(Boundary::Closed)) == Boundary::Closed);

namespace detail {

template <Boundary B>
struct BoundaryTag {};

template <>
struct BoundaryTag<Boundary::Open> {
  using is_open_tag = void;
};

template <>
struct BoundaryTag<Boundary::Closed> {
  using is_closed_tag = void;
};

template <Boundary Lower, Boundary Upper>
struct IntervalBoundaryTag {};

template <>
struct IntervalBoundaryTag<Boundary::Open, Boundary::Open> {
  using is_open_tag = void;
};

template <>
struct IntervalBoundaryTag<Boundary::Closed, Boundary::Closed> {
  using is_closed_tag = void;
};

}  // namespace detail

/**
 * @class Ray
 * @brief A Half-Space satisfying the Idempotent Semigroupoid laws.
 */
export template <IsTotallyOrdered T, Direction D, Boundary B = Boundary::Open,
                 typename L = ClassicalLogic>
class Ray : public detail::BoundaryTag<B> {
 public:
  using Domain = T;
  using Codomain = typename L::Ω;
  using is_ray_tag = void;

  /** @section interval__Algebraic_Axioms */
  template <typename Op>
  static constexpr bool is_associative_v = true;

  template <typename Op>
  static constexpr bool is_idempotent_v = true;

  constexpr explicit Ray(T pivot) : pivot_(pivot) {}

  constexpr T pivot() const { return pivot_; }

  /** @section interval__Logic: Characteristic Function */
  constexpr auto operator()(const T& x) const {
    if constexpr (D == Direction::Upward)
      return (B == Boundary::Open ? x > pivot_ : x >= pivot_) ? L::True
                                                              : L::False;
    else
      return (B == Boundary::Open ? x < pivot_ : x <= pivot_) ? L::True
                                                              : L::False;
  }

  /** @section interval__Algebraic_Laws: The Rhyme of the Lattice */
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

  // Inside Ray<T, Dir, L>
  /** @section interval__Lattice_Laws */
  constexpr auto operator<=(const Ray& other) const {
    if constexpr (D == Direction::Upward)
      return (pivot_ >= other.pivot_) ? L::True : L::False;
    else
      return (pivot_ <= other.pivot_) ? L::True : L::False;
  }

 private:
  T pivot_;
};

/**
 * @class Interval
 * @brief The "Molecule" formed by the intersection of two Rays.
 */
export template <IsTotallyOrdered T, Boundary Lower = Boundary::Open,
                 Boundary Upper = Boundary::Open, typename L = ClassicalLogic>
class Interval : public detail::IntervalBoundaryTag<Lower, Upper> {
 public:
  using Domain = T;
  using Codomain = typename L::Ω;
  using lower_ray_type = Ray<T, Direction::Upward, Lower, L>;
  using upper_ray_type = Ray<T, Direction::Downward, Upper, L>;

  constexpr Interval(T low, T high) : lower_(low), upper_(high) {}

  constexpr auto operator()(const T& x) const { return lower_(x) && upper_(x); }

  constexpr T lower_bound() const { return lower_.pivot(); }
  constexpr T upper_bound() const { return upper_.pivot(); }

  /** @brief Greatest lower bound (infimum) — satisfies HasExtrema. */
  constexpr T infimum() const { return lower_.pivot(); }
  /** @brief Least upper bound (supremum) — satisfies HasExtrema. */
  constexpr T supremum() const { return upper_.pivot(); }
  static constexpr Boundary lower_boundary = Lower;
  static constexpr Boundary upper_boundary = Upper;

 private:
  lower_ray_type lower_;
  upper_ray_type upper_;
};

/** @section interval__Trait_Registration */
export template <typename T, Direction D, Boundary B, typename L>
inline constexpr bool is_convex_v<Ray<T, D, B, L>> = true;

export template <typename T, Boundary Lower, Boundary Upper, typename L>
inline constexpr bool is_convex_v<Interval<T, Lower, Upper, L>> = true;

/**
 * @class HalfSpace
 * @brief A runtime-direction Ray — the general form of a half-space.
 *
 * @details
 * While Ray<T,D,B,L> encodes direction at compile time, HalfSpace<T,B,L>
 * stores direction at runtime. This makes it the natural witness for the
 * IsRay<R,T> concept, which requires a single type R to produce both
 * upward and downward half-spaces through static factory methods:
 *
 *   HalfSpace<T>::upward_from(pivot)   — { x | x > pivot }  (Open)
 *   HalfSpace<T>::downward_from(pivot) — { x | x < pivot }  (Open)
 *
 * The two types are complementary:
 *   - Ray<T,D,B,L>  — compile-time direction; no runtime overhead.
 *   - HalfSpace<T,B,L> — runtime direction; satisfies IsRay<R,T>.
 *
 * Both satisfy IsHalfSpace.
 *
 * @tparam T The element species (must be totally ordered).
 * @tparam B The boundary policy (Open or Closed).
 * @tparam L The subobject classifier logic.
 */
export template <IsTotallyOrdered T, Boundary B = Boundary::Open,
                 typename L = ClassicalLogic>
class HalfSpace : public detail::BoundaryTag<B> {
 public:
  using Domain = T;
  using Codomain = typename L::Ω;
  using is_ray_tag = void;

  template <typename Op>
  static constexpr bool is_associative_v = true;
  template <typename Op>
  static constexpr bool is_idempotent_v = true;

  /** @brief Factory: { x | x > pivot } or { x | x >= pivot }. */
  static constexpr HalfSpace upward_from(T pivot) {
    return HalfSpace{pivot, Direction::Upward};
  }
  /** @brief Factory: { x | x < pivot } or { x | x <= pivot }. */
  static constexpr HalfSpace downward_from(T pivot) {
    return HalfSpace{pivot, Direction::Downward};
  }

  /** @brief Construct from a compile-time Ray (direction preserving). */
  template <Direction D>
  constexpr explicit HalfSpace(const Ray<T, D, B, L>& ray)
      : pivot_(ray.pivot()), dir_(D) {}

  constexpr T pivot() const { return pivot_; }
  constexpr Direction direction() const { return dir_; }

  /** @brief Characteristic morphism χ: T → Ω. */
  constexpr Codomain operator()(const T& x) const {
    if (dir_ == Direction::Upward)
      return (B == Boundary::Open ? x > pivot_ : x >= pivot_) ? L::True
                                                              : L::False;
    else
      return (B == Boundary::Open ? x < pivot_ : x <= pivot_) ? L::True
                                                              : L::False;
  }

  /** @brief Intersection: yields the stricter of two same-direction halves. */
  friend constexpr HalfSpace operator&(const HalfSpace& a, const HalfSpace& b) {
    if (a.dir_ == Direction::Upward)
      return HalfSpace{std::max(a.pivot_, b.pivot_), Direction::Upward};
    else
      return HalfSpace{std::min(a.pivot_, b.pivot_), Direction::Downward};
  }

  /** @brief Union: yields the looser of two same-direction halves. */
  friend constexpr HalfSpace operator|(const HalfSpace& a, const HalfSpace& b) {
    if (a.dir_ == Direction::Upward)
      return HalfSpace{std::min(a.pivot_, b.pivot_), Direction::Upward};
    else
      return HalfSpace{std::max(a.pivot_, b.pivot_), Direction::Downward};
  }

 private:
  constexpr HalfSpace(T pivot, Direction d) : pivot_(pivot), dir_(d) {}

  T pivot_;
  Direction dir_;
};

export template <typename T, Boundary B, typename L>
inline constexpr bool is_convex_v<HalfSpace<T, B, L>> = true;

/** @section interval__Formal_Verification */

// Every Ray and Interval is a convex set (no holes).
// Note: int (not double) because IsTotallyOrdered<double> is withheld in
// dedekind (NaN violates reflexivity). See order.cppm.
static_assert(IsConvex<Ray<int, Direction::Upward>>,
              "An upward ray must satisfy IsConvex.");
static_assert(IsConvex<Interval<int>>,
              "An open interval must satisfy IsConvex.");
static_assert(IsConvex<HalfSpace<int>>, "A half-space must satisfy IsConvex.");

}  // namespace dedekind::topology

/**
 * @section interval__The_Topological_Bridge
 * We re-open the category namespace to reify the Ray as a formal Species
 * and establish its Algebraic Harmony.
 */
namespace dedekind::category {

export template <typename T, dedekind::topology::Direction D,
                 dedekind::topology::Boundary B, typename L>
struct SpeciesTraits<dedekind::topology::Ray<T, D, B, L>> {
  using species = ClassicalLogic;
  using Domain = T;
  using Codomain = T;

  static constexpr auto cardinality =
      std::integral<T> ? CardinalityTag::Countable : CardinalityTag::Continuum;
};

/** @section interval__Mereological_Harmony */

// We must explicitly register the Lattice Axioms for IsSet to resolve.
template <typename T, dedekind::topology::Direction D,
          dedekind::topology::Boundary B, typename L>
inline constexpr bool
    is_associative_v<dedekind::topology::Ray<T, D, B, L>, std::bit_and<>> =
        true;

template <typename T, dedekind::topology::Direction D,
          dedekind::topology::Boundary B, typename L>
inline constexpr bool
    is_idempotent_v<dedekind::topology::Ray<T, D, B, L>, std::bit_and<>> = true;

template <typename T, dedekind::topology::Direction D,
          dedekind::topology::Boundary B, typename L>
inline constexpr bool
    is_associative_v<dedekind::topology::Ray<T, D, B, L>, std::bit_or<>> = true;

template <typename T, dedekind::topology::Direction D,
          dedekind::topology::Boundary B, typename L>
inline constexpr bool
    is_idempotent_v<dedekind::topology::Ray<T, D, B, L>, std::bit_or<>> = true;

/** @section interval__HalfSpace_Harmony */

export template <typename T, dedekind::topology::Boundary B, typename L>
struct SpeciesTraits<dedekind::topology::HalfSpace<T, B, L>> {
  using species = ClassicalLogic;
  using Domain = T;
  using Codomain = T;

  static constexpr auto cardinality =
      std::integral<T> ? CardinalityTag::Countable : CardinalityTag::Continuum;
};

template <typename T, dedekind::topology::Boundary B, typename L>
inline constexpr bool
    is_associative_v<dedekind::topology::HalfSpace<T, B, L>, std::bit_and<>> =
        true;

template <typename T, dedekind::topology::Boundary B, typename L>
inline constexpr bool
    is_idempotent_v<dedekind::topology::HalfSpace<T, B, L>, std::bit_and<>> =
        true;

template <typename T, dedekind::topology::Boundary B, typename L>
inline constexpr bool
    is_associative_v<dedekind::topology::HalfSpace<T, B, L>, std::bit_or<>> =
        true;

template <typename T, dedekind::topology::Boundary B, typename L>
inline constexpr bool
    is_idempotent_v<dedekind::topology::HalfSpace<T, B, L>, std::bit_or<>> =
        true;

}  // namespace dedekind::category
