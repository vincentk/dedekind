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

#include <array>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <optional>
#include <ranges>
#include <type_traits>

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
 * @section ranges__Halfspace_To_Iota_View_Bridge (#703 Slices 1–2)
 *
 * @brief The halfspace ↔ iota_view isomorphism — typed @c OrderInterval
 *        ↔ runtime-bounded @c std::ranges::iota_view, witnessed at the
 *        value level by a round-trip.
 *
 * @details An @c order::OrderInterval<T, Lo, Hi, SL, SU> is the meet of two
 * opposing halfspaces — a typed-Δ⁰₁ predicate.  @c std::ranges::iota_view
 * is its range view: the same set of integers, accessed as a view rather
 * than as a predicate.  The pair @c (to_iota_view, from_iota_view)
 * normalises the four (SL, SU) strictness combinations to iota_view's
 * canonical @c [start, bound) shape:
 *
 *   - lower @c Strict     ⇒ @c start = Lo + 1   (predicate @c x > Lo)
 *   - lower @c NonStrict  ⇒ @c start = Lo       (predicate @c x ≥ Lo)
 *   - upper @c Strict     ⇒ @c bound = Hi       (predicate @c x < Hi)
 *   - upper @c NonStrict  ⇒ @c bound = Hi + 1   (predicate @c x ≤ Hi)
 *
 * The iso is @b value-level: it relates the singleton @c OI{} to a
 * specific @c iota_view value, not the @c OrderInterval @b type to the
 * @c iota_view @b type.  @c OrderInterval's bounds are template
 * parameters and @c iota_view's are runtime data, so @c from_iota_view
 * must be told the target type and verifies the runtime bounds match
 * what @c to_iota_view would produce — returning @c std::optional<OI>
 * (Honest-Rejection on mismatch).  The round-trip
 * @c from_iota_view<OI>(to_iota_view(OI{})) is the iso witness, pinned
 * by @c static_assert below.  A heavier categorical @c IsIsomorphism
 * reification (arrows-as-structs with @c inverse()) is intentionally
 * not done: it would over-claim a type-level iso, which the
 * typed/runtime asymmetry forbids.  Slice 3+: @c iota_view as a
 * Form-chain object (subobject-of-ambient lattice shape).
 */
namespace detail {

/** @brief The @c [start, bound) iota_view bounds an @c OrderInterval
 *         normalises to (shared between @c to_iota_view and
 *         @c from_iota_view).  Specialised on @c OrderInterval shape;
 *         the overflow corners (lower-Strict / upper-NonStrict at T's
 *         max) are forbidden at compile time. */
template <typename OI>
struct iota_bounds_of;

template <std::integral T, auto Lo, auto Hi, dedekind::order::Strictness SL,
          dedekind::order::Strictness SU, typename L>
  requires std::convertible_to<decltype(Lo), T> &&
           std::convertible_to<decltype(Hi), T>
struct iota_bounds_of<dedekind::order::OrderInterval<T, Lo, Hi, SL, SU, L>> {
  static constexpr T lo_t = static_cast<T>(Lo);
  static constexpr T hi_t = static_cast<T>(Hi);
  static constexpr T tmax = std::numeric_limits<T>::max();

  // Honest-Rejection at compile time for the corners where the iota_view's
  // bounds cannot be represented in T: lower-Strict at T's max would need
  // start = max+1, and upper-NonStrict at T's max would need bound = max+1
  // (iota's exclusive upper bound has no way to encode "include max").
  static_assert(SL != dedekind::order::Strictness::Strict || lo_t != tmax,
                "to_iota_view: lower-Strict at T's max would need start = "
                "max+1, which is not representable in T.  Use a different "
                "lower boundary, or a wider carrier.");
  static_assert(SU != dedekind::order::Strictness::NonStrict || hi_t != tmax,
                "to_iota_view: upper-NonStrict at T's max would need bound = "
                "max+1 (iota's exclusive upper), which is not representable "
                "in T.  Use upper-Strict (predicate x < max), or a wider "
                "carrier.");

  // The strictness ±1 is now safe in T (overflow corners excluded above).
  static constexpr T start = (SL == dedekind::order::Strictness::Strict)
                                 ? static_cast<T>(lo_t + 1)
                                 : lo_t;
  static constexpr T raw_bound = (SU == dedekind::order::Strictness::Strict)
                                     ? hi_t
                                     : static_cast<T>(hi_t + 1);
  // Empty intervals (e.g. {x : 5 < x < 5}) yield raw_bound < start; clamp so
  // the resulting iota_view is honestly empty rather than wrapped.
  static constexpr T bound = raw_bound < start ? start : raw_bound;
};

}  // namespace detail

/** @brief The typed→runtime half of the iso: project an @c OrderInterval to
 *         its canonical @c std::ranges::iota_view (the same set of integers
 *         viewed as a range rather than a predicate). */
export template <std::integral T, auto Lo, auto Hi,
                 dedekind::order::Strictness SL, dedekind::order::Strictness SU,
                 typename L>
  requires std::convertible_to<decltype(Lo), T> &&
           std::convertible_to<decltype(Hi), T>
constexpr std::ranges::iota_view<T, T> to_iota_view(
    const dedekind::order::OrderInterval<T, Lo, Hi, SL, SU, L>&) {
  using B = detail::iota_bounds_of<
      dedekind::order::OrderInterval<T, Lo, Hi, SL, SU, L>>;
  return std::ranges::views::iota(B::start, B::bound);
}

/** @section ranges__Materialize — the last plank of the bridge
 *  @c intensional @c → @c intensional-finite @c → @c materialise @c → @c
 *  extensional.
 *
 *  @brief Realise a finite interval domain into its @c ExtensionalSet, keeping
 *         the members that satisfy @c chi.  The interval is the @b bounded meet
 *         that carries the finiteness certificate (@c cardinality_type @c =
 *         @c Finite, so @c IsExtensional); @ref to_iota_view is the @b only
 *         on-ramp from that bounded meet to a scannable range, and the fold is
 *         the existing @c dedekind::sets::materialise.  An @b unbounded set has
 *         no @c to_iota_view and cannot reach here — the Rice wall made
 *         structural rather than checked.  Two-argument form takes the
 *         @c argmax (or any) predicate; the one-argument form realises the
 * whole interval (@c chi @c = @c ⊤).
 */
export template <std::integral T, auto Lo, auto Hi,
                 dedekind::order::Strictness SL, dedekind::order::Strictness SU,
                 typename L, typename Chi>
auto materialise(const dedekind::order::OrderInterval<T, Lo, Hi, SL, SU, L>& oi,
                 Chi chi) {
  return dedekind::sets::from_std(
      dedekind::sets::materialise(to_iota_view(oi), chi));
}

export template <std::integral T, auto Lo, auto Hi,
                 dedekind::order::Strictness SL, dedekind::order::Strictness SU,
                 typename L>
auto materialise(
    const dedekind::order::OrderInterval<T, Lo, Hi, SL, SU, L>& oi) {
  return materialise(oi, [](const T&) { return true; });
}

/** @section ranges__Argmax_Over_A_Bounded_Domain
 *
 *  The optimum as a filtration, carried with its own finite domain so it flows
 *  straight into @ref materialise as the single argument the endorsed surface
 *  @c materialise(argmax(Ω|[0,N], cost)) calls.
 */

/** @brief A finite set bundled with its scannable domain: an @c argmax result
 *         (or any refinement of a bounded interval) carrying both the interval
 *         (the @c IsExtensional range to scan) and the membership predicate
 * (the optimal filter).  This is the "domain-carrying bounded set" that lets
 *         @c materialise take a single argument. */
export template <typename OI, typename P>
struct BoundedSet {
  OI domain;
  P pred;
  using Domain = typename OI::Domain;
  /** @brief Membership: in the domain @b and optimal. */
  constexpr bool operator()(const Domain& x) const {
    return static_cast<bool>(domain(x)) && pred(x);
  }
  /** @brief The scannable bound (@c IsExtensional): the domain's cardinality is
   *  an addressable @c size_t — the licence to realise. */
  constexpr std::size_t size() const { return domain.size(); }
};

/** @brief @c argmax over a bounded (closed-interval) domain: the §3.3 forall-
 *         filter @c {x ∈ dom | ∀x'∈dom. cost(x') ≤ cost(x)}, with @c ≤ pulled
 *         back through @c cost.  Returns a @ref BoundedSet — intensional (the
 *         @c ∀ is decidable @b because @c dom is finite) and carrying its
 *         domain, so @c materialise realises it.  IsSet-valued: @c ∅ /
 * singleton (unique optimiser, a function) / larger (ties, a proper relation).
 */
export template <std::integral T, auto Lo, auto Hi,
                 dedekind::order::Strictness SL, dedekind::order::Strictness SU,
                 typename L, typename Cost, typename Order = std::less_equal<>>
constexpr auto argmax(
    const dedekind::order::OrderInterval<T, Lo, Hi, SL, SU, L>& dom, Cost cost,
    Order order = {}) {
  // @c x is optimal iff @c ∀x'∈dom. @c order(cost(x'), cost(x)) --- "no x'
  // beats x under @c order".  @c Order defaults to @c ≤ (argmax); pass @c
  // std::greater_equal for @b argmin, or a semiring @c ⊕-relative comparator to
  // rank by a dioid's order rather than the codomain's.
  auto pred = [dom, cost, order](const T& x) {
    bool dominant = true;
    for (const T xp : to_iota_view(dom))
      dominant = dominant && order(cost(xp), cost(x));
    return dominant;
  };
  using OI = dedekind::order::OrderInterval<T, Lo, Hi, SL, SU, L>;
  return BoundedSet<OI, decltype(pred)>{dom, pred};
}

/** @brief @c materialise a @ref BoundedSet: scan its domain, keep the members
 *         its predicate accepts — an ordered @c ExtensionalSet (the @c std::set
 *         flavour).  This is the single-argument call the endorsed
 *         @c materialise(argmax(dom, cost)) surface makes. */
export template <typename OI, typename P>
auto materialise(const BoundedSet<OI, P>& bs) {
  return materialise(bs.domain, bs.pred);
}

/** @brief The @b sequence flavour of @c materialise: realise the first @c N
 *         terms of a sequence (a bra/ket / @c Path / any @c index→value arrow)
 *         into a @c std::array — @b positional and indexed, dual to the set
 *         flavour's @c std::set.  The compile-time @c N is the Kleene bound
 *         (the finite prefix); this is the QM realise — an infinite bra/ket,
 *         bounded to @c [0,N), becomes a concrete finite-dimensional vector.
 *         Selected by the explicit @c N (@c materialise<N>(seq)); the no-@c N
 *         form realises a bounded @b set instead. */
export template <std::size_t N, typename Seq>
constexpr std::array<typename std::remove_cvref_t<Seq>::Codomain, N>
materialise(const Seq& s) {
  using D = typename std::remove_cvref_t<Seq>::Domain;
  std::array<typename std::remove_cvref_t<Seq>::Codomain, N> out{};
  for (std::size_t i = 0; i < N; ++i) out[i] = s(static_cast<D>(i));
  return out;
}

/** @brief The runtime→typed half of the iso (verifying, partial): check
 *         whether the runtime @c iota_view matches what @c to_iota_view
 *         would produce for the target @c OI, and return @c OI{} on match,
 *         @c std::nullopt otherwise.
 *
 *  @details The target @c OrderInterval is supplied at the type level
 *  because its bounds are template parameters; @c iota_view's are runtime
 *  data, so the inverse cannot reconstruct the type — it can only
 *  @b verify the bounds.  This is the honest shape of the asymmetric iso:
 *  bijection at the value level (between @c OI{} and a specific
 *  @c iota_view value), partial-with-verification at the runtime level. */
export template <typename OI>
constexpr std::optional<OI> from_iota_view(
    const std::ranges::iota_view<typename OI::Domain, typename OI::Domain>&
        iv) {
  using B = detail::iota_bounds_of<OI>;
  using T = typename OI::Domain;
  // Compare sizes first.  This is well-defined for any iota_view (including
  // empty and very large) and avoids the @c actual_start @c + @c iv.size()
  // arithmetic that could wrap for large ranges.
  const std::size_t actual_size = static_cast<std::size_t>(iv.size());
  const std::size_t expected_size =
      static_cast<std::size_t>(B::bound - B::start);
  if (actual_size != expected_size) {
    return std::nullopt;
  }
  // Empty interval: both sides empty ⇒ accept (the unique empty interval
  // per @c OI type; @c *iv.begin() is conventionally @c value_ even when
  // @c begin() @c == @c end(), but skipping the read keeps the verifier
  // unambiguously safe and the meaning honest — at the value level the iso
  // identifies @c OI{Empty} with @b any empty iota_view).
  if (actual_size == 0) {
    return OI{};
  }
  // Non-empty: dereferencing begin() is well-defined.
  const T actual_start = *iv.begin();
  if (actual_start != B::start) {
    return std::nullopt;
  }
  return OI{};
}

/** @section ranges__Halfspace_Iota_Round_Trip
 *  The iso witness: a value-level round-trip on a representative
 *  @c OrderInterval pins that @c from_iota_view ∘ @c to_iota_view is the
 *  identity on @c OI{}.  The negative direction is exercised in the test
 *  (a mismatched iota_view ⇒ nullopt). */
namespace halfspace_iota_witness {
using OI =
    dedekind::order::OrderInterval<int, 3, 8,
                                   dedekind::order::Strictness::NonStrict,
                                   dedekind::order::Strictness::Strict>;
static_assert(from_iota_view<OI>(to_iota_view(OI{})).has_value(),
              "Iso witness: from_iota_view ∘ to_iota_view yields OI{} for the "
              "canonical [3, 8) interval.");
}  // namespace halfspace_iota_witness

/** @section ranges__Bridge_Respects_Meet (#703 Slice 3a)
 *  The iota_view bridge is a @b lattice @b homomorphism on the meet:
 *  @c to_iota_view(A @c ∧ B) has @c start = max(start_A, start_B) and
 *  @c bound = min(bound_A, bound_B), i.e.\ exactly the set-intersection
 *  bounds.  Pinned at the type level via the shared
 *  @c iota_bounds_of helper. */
namespace bridge_meet_witness {
using A = dedekind::order::OrderInterval<
    int, 2, 8, dedekind::order::Strictness::NonStrict,
    dedekind::order::Strictness::Strict>;  // [2, 8)
using B = dedekind::order::OrderInterval<
    int, 5, 10, dedekind::order::Strictness::NonStrict,
    dedekind::order::Strictness::Strict>;                           // [5, 10)
using AandB = decltype(dedekind::order::structured_and(A{}, B{}));  // [5, 8)

static_assert(detail::iota_bounds_of<AandB>::start == 5,
              "Bridge respects meet: start of A∩B equals max of starts.");
static_assert(detail::iota_bounds_of<AandB>::bound == 8,
              "Bridge respects meet: bound of A∩B equals min of bounds.");

// Disjoint case: [0, 3) ∩ [5, 10) ⇒ empty OrderInterval (clamped to an
// empty iota_view), not three-way-reduced — the OI tower is closed under
// intersection so the bridge composes uniformly.
using D1 =
    dedekind::order::OrderInterval<int, 0, 3,
                                   dedekind::order::Strictness::NonStrict,
                                   dedekind::order::Strictness::Strict>;
using D2 =
    dedekind::order::OrderInterval<int, 5, 10,
                                   dedekind::order::Strictness::NonStrict,
                                   dedekind::order::Strictness::Strict>;
using D1andD2 = decltype(dedekind::order::structured_and(D1{}, D2{}));
static_assert(detail::iota_bounds_of<D1andD2>::start ==
                  detail::iota_bounds_of<D1andD2>::bound,
              "Disjoint intervals' meet produces an empty iota_view "
              "(start == bound after the clamp).");
}  // namespace bridge_meet_witness

/** @section ranges__Iota_Meet_Semilattice (#703 Slice 3b)
 *
 *  @brief @c std::ranges::iota_view as a Form-chain object: an
 *         @c order::IsOrderMeetSemilattice under subset-inclusion, with
 *         intersection as the meet.
 *
 *  @details Two @c iota_view values @c a, @c b can be intersected: their
 *  meet is the iota_view @c [max(start), min(bound)) clamped to empty if
 *  disjoint.  Intersection is associative, commutative, and idempotent —
 *  the three trait registrations below make
 *  @c IsOrderMeetSemilattice<iota_view<T,T>, IotaIntersection> fire.
 *
 *  @note @b Why @b meet-only @b and @b not @b a @b full @b lattice:
 *  union of two iota_views is @b not in general an iota_view
 *  (@c [0,3) @c ∪ @c [5,10) is not one interval), so iota_view's carrier
 *  is @b not closed under join.  A full lattice on the intervals layer
 *  requires moving to a richer carrier — finite unions of intervals, or
 *  the @c Sub<> subobject lattice from #698 Slice 8.  Tracked as a
 *  follow-up issue; this slice exhibits the honest meet-semilattice
 *  fragment.
 */

/** @brief The meet (intersection) operator on @c std::ranges::iota_view
 *         values: a callable that returns the iota_view of the common
 *         tail, or an empty iota_view on disjoint inputs. */
export struct IotaIntersection {
  template <std::integral T>
  constexpr std::ranges::iota_view<T, T> operator()(
      const std::ranges::iota_view<T, T>& a,
      const std::ranges::iota_view<T, T>& b) const {
    if (a.empty()) return a;
    if (b.empty()) return b;
    // Read the iota_view's start and bound DIRECTLY from its iterators
    // (operator* on iota_view's iterator just returns the stored value),
    // not via @c start @c + @c size().  Computing the bound from the size
    // narrows @c iv.size() (a @c range_size_t, typically @c size_t) back
    // to @c T, which truncates and can trigger signed-overflow UB on huge
    // ranges like @c [INT_MIN, INT_MAX).  Both iterators are well-formed
    // for @c iota_view<T,T> per @c [range.iota.iterator] (no past-the-end
    // dereference UB — the iterator stores @c value_ in itself).
    const T a_start = *a.begin();
    const T b_start = *b.begin();
    const T a_bound = *a.end();
    const T b_bound = *b.end();
    const T new_start = a_start > b_start ? a_start : b_start;
    const T raw_bound = a_bound < b_bound ? a_bound : b_bound;
    // Clamp to empty on disjoint inputs (raw_bound < new_start) so size()
    // doesn't underflow on unsigned T — same shape as to_iota_view's clamp.
    const T new_bound = raw_bound < new_start ? new_start : raw_bound;
    return std::ranges::views::iota(new_start, new_bound);
  }
};

}  // namespace dedekind::sequences

namespace dedekind::category {

// IotaIntersection is the meet on iota_view<T,T>: associative,
// commutative, idempotent — the trait triple that makes
// IsOrderMeetSemilattice fire.
template <std::integral T>
inline constexpr bool is_associative_v<std::ranges::iota_view<T, T>,
                                       dedekind::sequences::IotaIntersection> =
    true;
template <std::integral T>
inline constexpr bool is_commutative_v<std::ranges::iota_view<T, T>,
                                       dedekind::sequences::IotaIntersection> =
    true;
template <std::integral T>
inline constexpr bool is_idempotent_v<std::ranges::iota_view<T, T>,
                                      dedekind::sequences::IotaIntersection> =
    true;

}  // namespace dedekind::category

namespace dedekind::sequences {

// The Form-chain row-4-fragment witness: iota_view is an order-theoretic
// meet-semilattice under intersection (codirected, but not filtered —
// join doesn't fit iota_view; see the section note above).
static_assert(dedekind::order::IsOrderMeetSemilattice<
                  std::ranges::iota_view<int, int>, IotaIntersection>,
              "iota_view<int,int> with IotaIntersection is an order-theoretic "
              "meet-semilattice (associative + commutative + idempotent + the "
              "magma surface meet(a,b) -> iota_view<T,T>).");
static_assert(
    dedekind::order::IsOrderMeetSemilattice<
        std::ranges::iota_view<std::size_t, std::size_t>, IotaIntersection>,
    "Same witness fires on the unsigned (size_t) carrier.");

}  // namespace dedekind::sequences
