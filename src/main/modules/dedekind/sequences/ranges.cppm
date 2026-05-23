/**
 * @file dedekind/sequences/ranges.cppm
 * @partition :ranges
 * @brief The Integer Interval — A Serendipitous Bridge.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section ranges__Serendipity
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
 *
 * @note "This procedure is the demonstration by recurrence."
 *       -- Henri Poincare, Science and Hypothesis (1901)
 */
module;

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <ranges>

export module dedekind.sequences:ranges;

import dedekind.category;
import dedekind.order; // OrderInterval / Halfspace — the halfspace→iota_view
                       // bridge for #703 Slice 1
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

namespace dedekind::sequences {

// Anchor: IntegerInterval<int> satisfies IsConvex (the trait registration above
// is the proof; this assert makes it machine-checkable from the use site).
static_assert(dedekind::topology::IsConvex<IntegerInterval<int>>,
              "IntegerInterval must satisfy IsConvex (contiguous, no holes).");

// Anchor: IsConvexEnumerable is the combined witness for the triple nature.
static_assert(
    IsConvexEnumerable<IntegerInterval<int>>,
    "IntegerInterval must satisfy IsConvexEnumerable (convex + terminal set).");

/**
 * @section ranges__Halfspace_To_Iota_View_Bridge (#703 Slice 1)
 *
 * @brief The typed→runtime half of the halfspace ↔ iota_view isomorphism:
 *        convert a compile-time-bounded @c order::OrderInterval into the
 *        equivalent @c std::ranges::iota_view (closed-open boundary).
 *
 * @details An @c order::OrderInterval<T, Lo, Hi, SL, SU> is the meet of two
 * opposing halfspaces — a typed-Δ⁰₁ predicate.  @c std::ranges::iota_view
 * is its range view: the same set of integers, accessed as a view rather
 * than as a predicate.  This adapter maps the typed predicate to a
 * runtime-bounded iota_view, normalising the four (SL, SU) strictness
 * combinations to iota_view's canonical @c [start, bound) shape:
 *
 *   - lower @c Strict     ⇒ @c start = Lo + 1   (predicate @c x > Lo)
 *   - lower @c NonStrict  ⇒ @c start = Lo       (predicate @c x ≥ Lo)
 *   - upper @c Strict     ⇒ @c bound = Hi       (predicate @c x < Hi)
 *   - upper @c NonStrict  ⇒ @c bound = Hi + 1   (predicate @c x ≤ Hi)
 *
 * The reverse direction (@c from_iota_view) requires an explicit typed
 * target because @c iota_view's bounds are runtime data while
 * @c OrderInterval's are template parameters — deferred to Slice 2 along
 * with the @c IsIsomorphism witness for the pair.  Deeper Form-chain
 * witnessing of @c iota_view as an object (subobject-of-ambient lattice
 * shape) is the Slice 3+ direction.
 */
export template <std::integral T, T Lo, T Hi, dedekind::order::Strictness SL,
                 dedekind::order::Strictness SU, typename L>
constexpr std::ranges::iota_view<T, T> to_iota_view(
    const dedekind::order::OrderInterval<T, Lo, Hi, SL, SU, L>&) {
  constexpr T start =
      (SL == dedekind::order::Strictness::Strict) ? static_cast<T>(Lo + 1) : Lo;
  constexpr T raw_bound =
      (SU == dedekind::order::Strictness::Strict) ? Hi : static_cast<T>(Hi + 1);
  // Clamp: an empty OrderInterval (e.g. {x : 5 < x < 5}) must produce an
  // empty iota_view, not a wrapped one — iota(start, bound<start) is
  // ill-formed and would compute a wrapped size on unsigned T.
  constexpr T bound = raw_bound < start ? start : raw_bound;
  return std::ranges::views::iota(start, bound);
}

}  // namespace dedekind::sequences
