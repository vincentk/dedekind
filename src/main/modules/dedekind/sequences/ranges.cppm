/**
 * @file dedekind/sequences/ranges.cppm
 * @brief The bridge between order's halfspaces / intervals and std::ranges
 *        (iota views), plus the bounded sets built on it.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * An interval is @c order's @c Meet<Halfspace↑, Halfspace↓>; this partition
 * reads it as a half-open @c std::ranges::iota_view and back (a total inverse
 * on discrete carriers), and derives the finite-prefix machinery from that.
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
import dedekind.order; // Interval / Halfspace — the halfspace→iota_view
                       // bridge for #703 Slice 1
import dedekind.sets;
import dedekind.topology;
import :net;
import :path;

namespace dedekind::sequences {
using namespace dedekind::category;
using namespace dedekind::topology;

/**
 * @section ranges__Halfspace_To_Iota_View_Bridge (#703 Slices 1–2)
 *
 * @brief The halfspace ↔ iota_view isomorphism — typed @c Interval
 *        ↔ runtime-bounded @c std::ranges::iota_view, witnessed at the
 *        value level by a round-trip.
 *
 * @details An @c order::Interval<T, Lo, Hi, SL, SU> is the meet of two
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
 * specific @c iota_view value, not the @c Interval @b type to the
 * @c iota_view @b type.  @c Interval's bounds are template
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

/** @brief The @c [start, bound) iota_view bounds a finite value set normalises
 *  to (shared between @c to_iota_view and @c from_iota_view): the Interval /
 *  Singleton / Empty kinds of a @c SetVal, read off its value endpoints.  An
 *  unbounded kind (Halfspace / Universe) has no finite range and yields the
 *  empty bounds; a per-arm overload will refuse it once the set is a coproduct
 *  (#970).
 *  @note The representational corners the NTTP form forbade at compile time
 *  (lower-Strict at T's max, upper-NonStrict at T's max) are a precondition on
 *  the value now: iota's exclusive upper bound cannot encode "include max". */
template <std::integral T>
struct IotaBounds {
  T start;
  T bound;
};

template <std::integral T, typename L>
constexpr IotaBounds<T> iota_bounds(const dedekind::order::SetVal<T, L>& s) {
  using K = dedekind::order::SetKind;
  using S = dedekind::order::Strictness;
  switch (s.kind) {
    case K::Singleton:
      return {s.lo, static_cast<T>(s.lo + 1)};
    case K::Interval: {
      const T start = s.sl == S::Strict ? static_cast<T>(s.lo + 1) : s.lo;
      const T raw_bound = s.su == S::Strict ? s.hi : static_cast<T>(s.hi + 1);
      // Empty intervals (e.g. {x : 5 < x < 5}) yield raw_bound < start; clamp
      // so the resulting iota_view is honestly empty rather than wrapped.
      return {start, raw_bound < start ? start : raw_bound};
    }
    default:  // Empty, and the unbounded kinds (no finite range)
      return {T{}, T{}};
  }
}

}  // namespace detail

/** @brief The typed→runtime half of the iso: project a finite value set (the
 *  reduced meet, e.g.\ @c structured_and of two intervals) to its canonical
 *  @c std::ranges::iota_view --- the same set of integers viewed as a range
 *  rather than a predicate. */
export template <std::integral T, typename L>
constexpr std::ranges::iota_view<T, T> to_iota_view(
    const dedekind::order::SetVal<T, L>& s) {
  const auto b = detail::iota_bounds(s);
  return std::ranges::views::iota(b.start, b.bound);
}

/** @brief Project an interval (the meet of its two halfspaces) to its
 *  iota_view through its @c SetVal --- one bounds law. */
export template <std::integral T, dedekind::order::Strictness SL,
                 dedekind::order::Strictness SU, typename L>
constexpr std::ranges::iota_view<T, T> to_iota_view(
    const dedekind::order::Interval<T, SL, SU, L>& oi) {
  return to_iota_view(dedekind::order::to_setval(oi));
}

/** @section ranges__Materialize — the last plank of the bridge
 *  @c intensional @c → @c intensional-finite @c → @c ext @c → @c
 *  extensional.
 *
 *  @brief Realise a finite interval domain into its @c ExtensionalSet, keeping
 *         the members that satisfy @c chi.  The interval is the @b bounded meet
 *         that carries the finiteness certificate (@c cardinality_type @c =
 *         @c Finite, so @c IsExtensional); @ref to_iota_view is the @b only
 *         on-ramp from that bounded meet to a scannable range, and the fold is
 *         the existing @c dedekind::sets::ext.  An @b unbounded set has
 *         no @c to_iota_view and cannot reach here — the Rice wall made
 *         structural rather than checked.  Two-argument form takes the
 *         @c argmax (or any) predicate; the one-argument form realises the
 * whole interval (@c chi @c = @c ⊤).
 */
export template <std::integral T, dedekind::order::Strictness SL,
                 dedekind::order::Strictness SU, typename L, typename Chi>
auto ext(const dedekind::order::Interval<T, SL, SU, L>& oi, Chi chi) {
  return dedekind::sets::from_std(dedekind::sets::ext(to_iota_view(oi), chi));
}

/** @brief One-argument overload: realise the @b whole finite interval (every
 *  member kept).  The @c chi @c = @c ⊤ case of the two-argument @ref ext ---
 *  it delegates there with the canonical tautology / top predicate
 *  @c dedekind::category::classifier_true. */
export template <std::integral T, dedekind::order::Strictness SL,
                 dedekind::order::Strictness SU, typename L>
auto ext(const dedekind::order::Interval<T, SL, SU, L>& oi) {
  // The whole-interval realisation keeps every member: the characteristic map
  // is the canonical tautology / top predicate ⊤ (@ref
  // dedekind::category::classifier_true), not an ad-hoc always-true lambda.
  // Boolean-valued (the default L) so its ⊤ reads as a decided keep.
  return ext(oi, dedekind::category::classifier_true<T>());
}

/** @section ranges__Argmax_Over_A_Bounded_Domain
 *
 *  The optimum as a filtration, carried with its own finite domain so it flows
 *  straight into @ref ext as the single argument the endorsed surface
 *  @c ext(argmax(𝔸|[0,N], cost)) calls.
 */

/** @brief The value-semantic comprehension @f$\{x \in \mathrm{dom} \mid
 * P(x)\}@f$ over a @b finite domain: an @c argmax result (or any refinement of
 * a bounded interval), carrying both the interval (the finite range @ref ext
 * scans via @c to_iota_view) and the refinement predicate @c P.
 *
 *  @details Both this and @c dedekind::sets::Comprehension (the DSL's
 *  @f$\{S\mid P\}@f$) are now @c IsSet via @c dedekind::sets::SetExpr, and both
 *  are callable.  The @b one contract @c Comprehension does not meet as an
 *  @c argmax @b result is @b value @b ownership: @c Comprehension holds
 *  @c const @c Base& (a reference into a named ambient set), whereas an
 *  @c argmax result must @b own its (stateless but @b typed) @c Interval
 *  domain by value to be returned safely, so the scannable bounds survive in
 *  the return value's type.  So @c BoundedSet is precisely the @b value-owning
 *  finite comprehension.  Its membership χ is @c x @c ∈ @c {dom @c | @c P} @c ⟺
 *  @c dom(x) @c ∧ @c P(x).  @c ext realises it by scanning @c to_iota_view of
 *  the concrete @c Interval domain, whose NTTP interval bounds make the
 *  enumeration finite --- that @c to_iota_view gate is what @c ext reads, not
 * an
 *  @c IsExtensional concept and not @c size() (which reports the cardinality,
 *  not what @c ext scans).  @c size() is free because the @c Interval
 *  domain is stateless. */
export template <typename OI, typename P>
struct BoundedSet
    : dedekind::sets::SetExpr<BoundedSet<OI, P>, typename OI::Domain,
                              typename OI::logic_species> {
  OI domain;
  P pred;
  using Domain = typename OI::Domain;
  /** @brief Explicit two-argument constructor.  @ref dedekind::sets::SetExpr is
   *  a base class, so @c BoundedSet is no longer an aggregate --- the
   *  @c BoundedSet{dom, pred} site in @ref argmax routes here instead of
   * through aggregate init. */
  constexpr BoundedSet(OI d, P p)
      : domain(static_cast<OI&&>(d)), pred(static_cast<P&&>(p)) {}
  /** @brief The interval is finite, so the comprehension over it is too.  This
   *  @c cardinality_type is @b metadata (the @c Finite tag), not a gate @ref
   * ext reads: @ref ext scans @c to_iota_view of the concrete @c Interval.
   */
  using cardinality_type = dedekind::sets::Finite;
  /** @brief χ / membership: @c x @c ∈ @c {dom @c | @c P} @c ⟺ in the domain
   *  @b and @c P-optimal.  A comprehension @b is its own characteristic map,
   *  which (with @ref dedekind::sets::SetExpr) makes @c BoundedSet an
   *  @c IsSet --- a first-class DSL citizen, not a bespoke struct. */
  constexpr auto operator()(const Domain& x) const {
    // Combine under the domain's logic (@c L::AND), first lifting the (commonly
    // @c bool) predicate into @c L::Ω --- a @c bool cast would collapse ternary
    // membership (@c Ternary::False, underlying −1, reads as @c true), exactly
    // as @c dedekind::sets::Comprehension guards against.
    using L = typename OI::logic_species;
    const auto p = pred(x);
    if constexpr (std::same_as<std::remove_cvref_t<decltype(p)>, typename L::Ω>)
      return L::AND(domain(x), p);
    else
      return L::AND(domain(x), p ? L::True : L::False);
  }
  /** @brief The domain's cardinality as an addressable @c size_t.  Metadata
   *  (the finite bound), not a gate @ref ext reads. */
  constexpr std::size_t size() const { return dedekind::order::size(domain); }
};

/** @brief Witness: @c BoundedSet @b is a set.  @c IsSet is reached by
 * inheriting
 *  @c dedekind::sets::SetExpr and supplying the χ --- the same opt-in surface
 *  @c Comprehension uses; nominal, never a precondition. */
namespace detail_boundedset_witness {
using WOI =
    dedekind::order::Interval<int, dedekind::order::Strictness::NonStrict,
                              dedekind::order::Strictness::NonStrict,
                              dedekind::category::Boole>;
// The refinement is the canonical tautology ⊤ (@ref classifier_true), reused
// rather than a bespoke always-true functor.
static_assert(
    dedekind::category::IsSet<
        BoundedSet<WOI, decltype(dedekind::category::classifier_true<int>())>>,
    "BoundedSet is a first-class DSL set: the value-owning finite "
    "comprehension {x ∈ dom | P}.");
}  // namespace detail_boundedset_witness

/** @brief The dominance-refinement predicate of an @ref argmax: @c x is optimal
 *  iff @b no member @c x' of the (finite) domain beats it under @c order∘cost,
 *  i.e. @f$\forall x' \in \mathrm{dom}.\ \mathrm{order}(\mathrm{cost}(x'),
 *  \mathrm{cost}(x))@f$.  A @b named functor rather than a capturing lambda, so
 *  the refinement is an inspectable type carried in the @ref BoundedSet
 *  signature @c argmax returns --- the ∀-filter made visible at compile time.
 */
export template <std::integral T, typename OI, typename Cost, typename Order>
  requires requires(const OI& d, const Cost& c, const Order& o, const T& x) {
    to_iota_view(d);                                 // scannable domain
    { o(c(x), c(x)) } -> std::convertible_to<bool>;  // order∘cost testable
  }
struct DominanceRefinement {
  OI dom;
  Cost cost;
  Order order;
  /** @brief χ: is @c x optimal? @c true iff @b no member of the (finite) domain
   *  beats it under @c order∘cost, i.e. @f$\forall x' \in \mathrm{dom}.\
   *  \mathrm{order}(\mathrm{cost}(x'), \mathrm{cost}(x))@f$.  The dominance
   *  membership test @c argmax's @ref BoundedSet carries. */
  constexpr bool operator()(const T& x) const {
    bool dominant = true;
    for (const T xp : to_iota_view(dom))
      dominant = dominant && order(cost(xp), cost(x));
    return dominant;
  }
};

/** @brief @c argmax over a bounded (closed-interval) domain: the §3.3 forall-
 *         filter @c {x ∈ dom | ∀x'∈dom. cost(x') ≤ cost(x)}, with @c ≤ pulled
 *         back through @c cost.  Returns a @ref BoundedSet — intensional (the
 *         @c ∀ is decidable @b because @c dom is finite) and carrying its
 *         domain, so @c ext realises it.  IsSet-valued: @c ∅ /
 * singleton (unique optimiser, a function) / larger (ties, a proper relation).
 */
export template <std::integral T, dedekind::order::Strictness SL,
                 dedekind::order::Strictness SU, typename L, typename Cost,
                 typename Order = std::less_equal<>>
constexpr auto argmax(const dedekind::order::Interval<T, SL, SU, L>& dom,
                      Cost cost, Order order = {}) {
  // @c x is optimal iff @c ∀x'∈dom. @c order(cost(x'), cost(x)) --- "no x'
  // beats x under @c order".  @c Order defaults to @c ≤ (argmax); pass @c
  // std::greater_equal for @b argmin, or a semiring @c ⊕-relative comparator to
  // rank by a dioid's order rather than the codomain's.  The refinement is the
  // named @ref DominanceRefinement functor, not a capturing lambda.
  using OI = dedekind::order::Interval<T, SL, SU, L>;
  using Pred = DominanceRefinement<T, OI, Cost, Order>;
  return BoundedSet<OI, Pred>{dom, Pred{dom, cost, order}};
}

/** @brief @c ext a @ref BoundedSet: scan its domain, keep the members
 *         its predicate accepts — an ordered @c ExtensionalSet (the @c std::set
 *         flavour).  This is the single-argument call the endorsed
 *         @c ext(argmax(dom, cost)) surface makes. */
export template <typename OI, typename P>
auto ext(const BoundedSet<OI, P>& bs) {
  return ext(bs.domain, bs.pred);
}

/** @brief The @b sequence flavour of @c ext: realise the first @c N
 *         terms of a sequence (a bra/ket / @c Path / any @c index→value arrow)
 *         into a @c std::array — @b positional and indexed, dual to the set
 *         flavour's @c std::set.  The compile-time @c N is the Kleene bound
 *         (the finite prefix); this is the QM realise — an infinite bra/ket,
 *         bounded to @c [0,N), becomes a concrete finite-dimensional vector.
 *         Selected by the explicit @c N (@c ext<N>(seq)); the no-@c N
 *         form realises a bounded @b set instead. */
export template <std::size_t N, typename Seq>
constexpr std::array<typename std::remove_cvref_t<Seq>::Codomain, N> ext(
    const Seq& s) {
  using D = typename std::remove_cvref_t<Seq>::Domain;
  std::array<typename std::remove_cvref_t<Seq>::Codomain, N> out{};
  for (std::size_t i = 0; i < N; ++i) out[i] = s(static_cast<D>(i));
  return out;
}

/** @brief The runtime→typed half of the iso, a @b total inverse: rebuild the
 *  interval from the iota_view's @c [start, bound) by undoing the strictness
 *  offsets.  The strictness pair is the type; the endpoints are values, so
 *  every iota_view names an interval (an empty view names an empty interval).
 *
 *  @details The NTTP form could only @em verify a view against a target type,
 *  since the bounds lived in the type; with the endpoints as values the inverse
 *  simply constructs, and @c from_iota_view ∘ @c to_iota_view is the identity
 *  on the endpoints.  Undoing a Strict lower offset at T's min is the mirror of
 *  the @c to_iota_view precondition. */
export template <dedekind::order::Strictness SL, dedekind::order::Strictness SU,
                 typename L = dedekind::category::Boole, std::integral T>
constexpr dedekind::order::Interval<T, SL, SU, L> from_iota_view(
    const std::ranges::iota_view<T, T>& iv) {
  using S = dedekind::order::Strictness;
  // Read start and bound directly from the iterators (as IotaIntersection
  // does): no size arithmetic to narrow or wrap.
  const T start = *iv.begin();
  const T bound = *iv.end();
  const T lo = SL == S::Strict ? static_cast<T>(start - 1) : start;
  const T hi = SU == S::Strict ? bound : static_cast<T>(bound - 1);
  return dedekind::order::make_interval<SL, SU, L>(lo, hi);
}

/** @section ranges__Halfspace_Iota_Round_Trip
 *  The iso witness: a value-level round-trip on a representative
 *  @c Interval pins that @c from_iota_view ∘ @c to_iota_view is the
 *  identity on @c OI{}.  The negative direction is exercised in the test
 *  (a mismatched iota_view ⇒ nullopt). */
namespace halfspace_iota_witness {
using dedekind::order::Strictness;
constexpr auto oi =
    dedekind::order::make_interval<Strictness::NonStrict, Strictness::Strict>(
        3, 8);  // [3, 8)
constexpr auto back =
    from_iota_view<Strictness::NonStrict, Strictness::Strict>(to_iota_view(oi));
static_assert(dedekind::order::lower_pivot(back) == 3 &&
                  dedekind::order::upper_pivot(back) == 8,
              "Iso witness: from_iota_view ∘ to_iota_view is the identity on "
              "the canonical [3, 8) interval's endpoints.");
}  // namespace halfspace_iota_witness

/** @section ranges__Bridge_Respects_Meet (#703 Slice 3a)
 *  The iota_view bridge is a @b lattice @b homomorphism on the meet:
 *  @c to_iota_view(A @c ∧ B) has @c start = max(start_A, start_B) and
 *  @c bound = min(bound_A, bound_B), i.e.\ exactly the set-intersection
 *  bounds.  Pinned at the type level via the shared
 *  @c iota_bounds_of helper. */
namespace bridge_meet_witness {
using dedekind::order::Strictness;
constexpr auto A =
    dedekind::order::make_interval<Strictness::NonStrict, Strictness::Strict>(
        2, 8);  // [2, 8)
constexpr auto B =
    dedekind::order::make_interval<Strictness::NonStrict, Strictness::Strict>(
        5, 10);                                                // [5, 10)
constexpr auto AandB = dedekind::order::structured_and(A, B);  // [5, 8)

static_assert(detail::iota_bounds(AandB).start == 5,
              "Bridge respects meet: start of A∩B equals max of starts.");
static_assert(detail::iota_bounds(AandB).bound == 8,
              "Bridge respects meet: bound of A∩B equals min of bounds.");

// Disjoint case: [0, 3) ∩ [5, 10) ⇒ the empty value (clamped to an empty
// iota_view) --- the meet is closed on the value carrier, so the bridge
// composes uniformly.
constexpr auto D1 =
    dedekind::order::make_interval<Strictness::NonStrict, Strictness::Strict>(
        0, 3);
constexpr auto D2 =
    dedekind::order::make_interval<Strictness::NonStrict, Strictness::Strict>(
        5, 10);
constexpr auto D1andD2 = dedekind::order::structured_and(D1, D2);
static_assert(detail::iota_bounds(D1andD2).start ==
                  detail::iota_bounds(D1andD2).bound,
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
