/**
 * @file dedekind/sequences/ranges.cppm
 * @partition :ranges
 * @brief Level 2.6: The Integer Interval — A Serendipitous Bridge.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Serendipity: The Triple Nature of the Integer Interval
 * For any integral type T, the integer interval [a, b) is simultaneously:
 *
 *   1. **A Sequence** (IsTerminalSet via as_sequence()):
 *      The injective enumeration f(i) = a + i, f: ℕ → T.
 *
 *   2. **A Set with unique elements:**
 *      f is injective ⟹ no element appears twice (set axiom holds).
 *
 *   3. **A Convex Set** (IsConvex):
 *      x ∈ [a,b), y ∈ [a,b), x ≤ z ≤ y  ⟹  z ∈ [a,b).
 *      It is the integer analog of a topological interval.
 *
 * @details
 * IntegerInterval<T, Lower, Upper, L> reifies this triple nature within the
 * dedekind framework:
 *   - IsPredicate / IsConvex:      Domain = T, Codomain = Ω,
 *                                  operator()(T) → Ω.
 *   - IsCountableSet / IsTerminalSet: as_sequence() → Path<T>, size().
 *   - Uniqueness:                  formalized as a static_assert on
 *                                  injectivity of the sequence generator.
 *
 * The default boundary convention [lo, hi) mirrors std::ranges::iota_view
 * and standard C++ iterator practice.
 *
 * Wikipedia: Integer lattice, Convex set, Injective function
 */
module;

#include <concepts>
#include <cstddef>
#include <cstdint>

export module dedekind.sequences:ranges;

import dedekind.category;
import dedekind.sets;
import dedekind.topology;
import :net;
import :path;

namespace dedekind::sequences {
using namespace dedekind::category;
using namespace dedekind::topology;

/**
 * @concept IsConvexEnumerable
 * @brief A convex set that is also finitely enumerable (a terminal set).
 *
 * @details This concept captures the serendipitous combination found in
 * integer intervals: they are topologically convex (no holes) AND finitely
 * enumerable (a terminal countable set). Integer intervals are the canonical
 * witness for this concept.
 *
 * @see IntegerInterval
 */
export template <typename S>
concept IsConvexEnumerable = IsConvex<S> && IsTerminalSet<S>;

namespace detail {

/**
 * @brief Boundary tag mixin for the open/closed corners of IntegerInterval.
 * Mirrors the convention in dedekind::topology::detail::IntervalBoundaryTag.
 */
template <Boundary Lower, Boundary Upper>
struct IntegerIntervalBoundaryMixin {};

template <>
struct IntegerIntervalBoundaryMixin<Boundary::Open, Boundary::Open> {
  using is_open_tag = void;
};

template <>
struct IntegerIntervalBoundaryMixin<Boundary::Closed, Boundary::Closed> {
  using is_closed_tag = void;
};

}  // namespace detail

/**
 * @class IntegerInterval
 * @brief The integer interval — serendipitously a sequence, a set, and a
 * convex set.
 *
 * @tparam T      An integral type (the element species).
 * @tparam Lower  Boundary policy for the lower bound (default: Closed).
 * @tparam Upper  Boundary policy for the upper bound (default: Open).
 * @tparam L      The subobject classifier logic (default: ClassicalLogic).
 *
 * @note The default [lo, hi) is the integer analog of std::ranges::iota_view.
 *
 * @par Triple nature (static assertions in the test suite)
 * | Property     | Concept              | Witness                     |
 * |:-------------|:---------------------|:----------------------------|
 * | Convex set   | IsConvex             | is_convex_v registration    |
 * | Unique elems | —                    | injectivity of as_sequence()|
 * | Finite seq   | IsTerminalSet        | size() + as_sequence()      |
 * | All three    | IsConvexEnumerable   | combination                 |
 */
export template <std::integral T, Boundary Lower = Boundary::Closed,
                 Boundary Upper = Boundary::Open, typename L = ClassicalLogic>
class IntegerInterval
    : public detail::IntegerIntervalBoundaryMixin<Lower, Upper> {
 public:
  using Domain = T;
  using Codomain = typename L::Ω;

  static constexpr Boundary lower_boundary = Lower;
  static constexpr Boundary upper_boundary = Upper;

  constexpr IntegerInterval(T lo, T hi) : lo_(lo), hi_(hi) {}

  /**
   * @brief Characteristic morphism χ: T → Ω (the predicate / set view).
   * @details Is x a member of this interval?
   */
  constexpr Codomain operator()(const T& x) const noexcept {
    if constexpr (Lower == Boundary::Closed && Upper == Boundary::Open)
      return (x >= lo_ && x < hi_) ? L::True : L::False;
    else if constexpr (Lower == Boundary::Closed && Upper == Boundary::Closed)
      return (x >= lo_ && x <= hi_) ? L::True : L::False;
    else if constexpr (Lower == Boundary::Open && Upper == Boundary::Open)
      return (x > lo_ && x < hi_) ? L::True : L::False;
    else  // Open, Closed
      return (x > lo_ && x <= hi_) ? L::True : L::False;
  }

  /**
   * @brief Cardinality of the interval as a finite set.
   * @details Computes the number of integers in the interval exactly.
   */
  constexpr std::size_t size() const noexcept {
    // Use int64_t to avoid signed overflow / unsigned wrap when adjusting
    // the bounds by ±1 for open boundaries.
    using W = std::int64_t;
    const W wlo = static_cast<W>(lo_);
    const W whi = static_cast<W>(hi_);
    const W elo = (Lower == Boundary::Closed) ? wlo : wlo + W(1);
    const W ehi = (Upper == Boundary::Open) ? whi - W(1) : whi;
    return (ehi >= elo) ? static_cast<std::size_t>(ehi - elo + W(1)) : 0u;
  }

  /**
   * @brief Sequence view: the injective enumeration f(i) = lo + i.
   *
   * @details
   * The generator is injective: i ≠ j ⟹ f(i) ≠ f(j). This witnesses
   * the uniqueness (set) property of the integer interval. Callers
   * should read at most size() terms to stay within the interval.
   *
   * @return A Path<T> whose first size() elements enumerate the interval.
   */
  constexpr auto as_sequence() const {
    // Use int64_t for the same reason as size(): avoid signed overflow /
    // unsigned wrap when the lower boundary is open.
    using W = std::int64_t;
    const W start = (Lower == Boundary::Closed) ? static_cast<W>(lo_)
                                                : static_cast<W>(lo_) + W(1);
    return Path<T>{[start](std::size_t i) {
      return static_cast<T>(start + static_cast<W>(i));
    }};
  }

  constexpr T lower_bound() const noexcept { return lo_; }
  constexpr T upper_bound() const noexcept { return hi_; }

  /** @brief Greatest lower bound (infimum) — satisfies HasExtrema. */
  constexpr T infimum() const noexcept { return lo_; }
  /** @brief Least upper bound (supremum) — satisfies HasExtrema. */
  constexpr T supremum() const noexcept { return hi_; }

 private:
  T lo_, hi_;
};

}  // namespace dedekind::sequences

// --- Trait registrations (re-open peer namespaces, mirroring the pattern in
//     dedekind::topology::interval.cppm) ---

namespace dedekind::topology {

/**
 * @brief Register IntegerInterval as a convex set.
 * @details An integer interval is a contiguous lattice segment with no holes,
 *          satisfying the topological definition of convexity over ℤ.
 */
export template <std::integral T, Boundary Lower, Boundary Upper, typename L>
inline constexpr bool
    is_convex_v<dedekind::sequences::IntegerInterval<T, Lower, Upper, L>> =
        true;

}  // namespace dedekind::topology

namespace dedekind::category {

/** @brief Register IntegerInterval in the species atlas. */
export template <std::integral T, dedekind::topology::Boundary Lower,
                 dedekind::topology::Boundary Upper, typename L>
struct SpeciesTraits<dedekind::sequences::IntegerInterval<T, Lower, Upper, L>> {
  using Domain = T;
  using Codomain = typename L::Ω;
  using cardinality_type = dedekind::sets::Finite;
};

}  // namespace dedekind::category
