/**
 * @file dedekind/order/halfspace.cppm
 * @partition :halfspace
 * @brief Compile-time halfspace predicates on ordered carriers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section halfspace__Intervals_As_Values
 * The order reading of the term grammar on a chain: an order atom @c π @c ⋈ @c
 * c on @c 𝔸<T> is an @c Interval, a value whose sides (each an optional bound,
 * open or closed) and pivots are values.  Meets reduce in constant evaluation
 * where the pivots are constants and at runtime otherwise; the complement of an
 * interval is a @c Join of at most two intervals.
 *
 * @section halfspace__DSL_Surface
 *
 *     constexpr auto big   = ℕ | (π > 5);
 *     constexpr auto small = ℕ | (π < 3);
 *     static_assert((big & small) == Ø<Cardinality>{});
 *
 * Wikipedia: Half-space (geometry), Separating hyperplane theorem,
 *
 * @note "Was beweisbar ist, soll in der Wissenschaft nicht ohne Beweis
 *       geglaubt werden."
 *       — Richard Dedekind, *Was sind und was sollen die Zahlen?* (1888),
 *         Vorwort.
 *       [Trans: "What is provable should not be believed without proof
 *       in science."]
 */
module;
#include <algorithm>
#include <concepts>
#include <cstddef>
#include <functional>  // std::plus (the argmax carrier's additive-group gate)
#include <limits>      // std::numeric_limits (machine-carrier boundary check)
#include <optional>    // a cut's sides
#include <type_traits>
#include <utility>

export module dedekind.order:halfspace;

import dedekind.category;
import dedekind.sets;
import dedekind.relational; // IsFunctional/IsEntire (:graph), used by the order
                            // relation witnesses below (#792)
import :poset;  // IsPartiallyOrdered — the SEMANTIC order certificate for
                // max/min
import :total;  // IsTotallyOrdered — the carrier gate of Halfspace / Interval:
                // the interval normal form lives on a certified chain

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::relational;  // converse/diagonal/reflexive/RelAnd/...
using namespace dedekind::category;
// The relation operators (moved to :relational, #792) must be pulled in by
// NAME, not only the directive above: @c order declares its OWN operator>> /
// operator+ / operator& (on halfspaces / projections), and a nearer-scope
// declaration hides using-DIRECTIVE names from unqualified (operator) lookup.
// Before the move these were reached by ADL on the @c Set<pair> operand (ADL
// ignores such hiding); the using-DECLARATIONS restore exactly that candidate
// set, joining --- not losing to --- order's own operators.
using dedekind::relational::operator>>;  // the relative product R;S
using dedekind::relational::operator&;   // relation meet R & S
// relation union is the set-grammar | (the free operator|; no using
// needed)

// @c IsRingIntegral moved to @c :sets:cardinality (#878): it depends only
// on @c std::integral and the cardinal carriers, so it belongs at the
// cardinality authority, not accreted here.  Reached via @c import
// @c dedekind.sets (already imported above).

/** @brief Orientation of a halfspace along the chain. */
export enum class Direction { Upward, Downward };

/** @brief Whether the boundary is strict (`>`, `<`) or inclusive (`>=`, `<=`).
 */
export enum class Strictness { Strict, NonStrict };

/** @brief Flip a @c Direction --- the direction half of a halfspace complement
 *  (@c ~{x>P} is @c {x≤P}).  Reused by the reflection-image pushforward
 *  (@c dedekind.algebra:halfspace_transport) to flip a halfspace's sense under
 *  @f$x \mapsto -x@f$. */
export constexpr Direction flip(Direction d) {
  return d == Direction::Upward ? Direction::Downward : Direction::Upward;
}
/** @brief Flip a @c Strictness --- the strictness half of the same complement
 *  (@c ~{x>P} is @c {x≤P}: @c > relaxes to @c ≤). */
export constexpr Strictness flip(Strictness s) {
  return s == Strictness::Strict ? Strictness::NonStrict : Strictness::Strict;
}

/** @brief Cardinality class of a halfspace's carrier @c T (#848/#927).
 *  @details A carrier that declares its own @c cardinality_type is the
 *  authority (e.g.\ @c numbers::Rational → @c ℵ_0, @c ExtensionalCardinal →
 *  @c Finite): this partition (@c :order) is @b upstream of @c :numbers, so a
 *  countable @b non-integral carrier such as ℚ cannot be recognised
 *  structurally here (@c IsRingIntegral is false on @c Rational), and must
 *  self-declare so the point-free @c A|pred halfspace classifies identically
 *  to the scout comprehension (which inherits the ambient @c C directly).
 *  Otherwise fall back to the @c IsRingIntegral discriminator: the structural
 *  integers @b and the @c Cardinality / @c SignedCardinality (ℕ/ℤ) proxies are
 *  @c ℵ_0, and the real proxies (@c QuadraticReal, @c double) are the continuum
 *  @c ℶ_1.  The bound only has to be tight enough for the countability
 *  verdict (@c IsCountable). */
// Module-private: the fallback is a halfspace-classification heuristic, not a
// general carrier-cardinality authority (that is @c :sets:cardinality).  It
// only feeds @c Halfspace::cardinality_type below; external carriers customise
// through their own @c T::cardinality_type, not this helper.  A finite @b enum
// carrier (e.g.\ @c Ternary) cannot self-declare a member typedef, so it is
// recognised structurally as countable and classified @c ℵ_0 --- the same
// conservative countable BOUND @c bool already gets via @c IsRingIntegral, NOT
// @c Finite.  @c Finite is the @c elevate_meet "return bare, do not @c Set
// -wrap" signal (@c :expressions); a halfspace must not trigger it, and @c ℵ_0
// keeps the pre-existing @c Set-wrapping path and stays countable, matching
// @c Ternary's @c 𝕂3 ambient.
// The @c IsRingIntegral integers / ℕ,ℤ proxies are @c ℵ_0 likewise; the real
// proxies are @c ℶ_1.
template <typename T>
struct carrier_cardinality {
  using type =
      std::conditional_t<std::is_enum_v<T> || IsRingIntegral<T>, ℵ_0, ℶ_1>;
};
template <typename T>
  requires IsCardinality<typename T::cardinality_type>
struct carrier_cardinality<T> {
  // Fires only when the self-declared alias is a genuine @c IsCardinality (a
  // carrier with an unrelated / incomplete @c cardinality_type falls to the
  // primary, not a hard error), and classifies it with the canonical
  // @c IsCountable concept.  Collapse to the
  // COUNTABILITY BOUND (never pass a bare @c Finite through): @c
  // ExtensionalCardinal declares @c Finite, and letting that reach @c
  // Halfspace::cardinality_type would trip @c elevate_meet's Finite "return
  // bare" path for an @c ExtensionalCardinal halfspace.  Countable (incl.\
  // @c Finite) ⟹ @c ℵ_0, uncountable ⟹ @c ℶ_1.
  using type =
      std::conditional_t<IsCountable<typename T::cardinality_type>, ℵ_0, ℶ_1>;
};
template <typename T>
using carrier_cardinality_t = typename carrier_cardinality<T>::type;

// A finite enum carrier classifies ℵ_0 (a conservative countable bound, like
// bool), so a Ternary halfspace stays decidable (Boole), not Kleene (#927), and
// does NOT trip the elevate_meet Finite "return bare" path.
static_assert(std::same_as<carrier_cardinality_t<Ternary>, ℵ_0>,
              "a finite enum carrier (e.g. Ternary) is countable ℵ_0, not the "
              "continuum ℶ_1 (and not Finite, the elevate_meet bare signal).");
// A self-declared @c Finite carrier is likewise collapsed to @c ℵ_0, so its
// halfspace stays Set-wrapped by @c elevate_meet (not returned bare).
static_assert(std::same_as<carrier_cardinality_t<ExtensionalCardinal<>>, ℵ_0>,
              "a self-declared Finite carrier collapses to the countable bound "
              "ℵ_0, never the bare-signal Finite.");
namespace detail_halfspace_witness {
// A carrier whose @c cardinality_type alias is NOT an @c IsCardinality: the
// self-declaring specialization must @b not fire (its IsCardinality constraint
// fails), so @c carrier_cardinality falls back to the primary rather than
// hard-erroring on the missing @c is_countable member.
struct UnrelatedCardinalityAlias {
  using cardinality_type = int;  // not a cardinality (no is_countable etc.)
};
}  // namespace detail_halfspace_witness
static_assert(
    std::same_as<carrier_cardinality_t<
                     detail_halfspace_witness::UnrelatedCardinalityAlias>,
                 ℶ_1>,
    "a carrier with an unrelated/incomplete cardinality_type falls back to the "
    "primary (ℶ_1 here), never making Halfspace ill-formed.");

/** @brief The @c Direction a comparison @c Rel lifts a halfspace to. */
export constexpr Direction dir_of(Rel r) {
  return (r == Rel::Gt || r == Rel::Ge) ? Direction::Upward
                                        : Direction::Downward;
}
/** @brief The @c Strictness a comparison @c Rel lifts a halfspace to. */
export constexpr Strictness strict_of(Rel r) {
  return (r == Rel::Gt || r == Rel::Lt) ? Strictness::Strict
                                        : Strictness::NonStrict;
}
/** @brief Whether a @c Rel is one of the four order comparisons (not Eq/Ne). */
export constexpr bool is_order_rel(Rel r) {
  return r == Rel::Lt || r == Rel::Le || r == Rel::Gt || r == Rel::Ge;
}

/** @section halfspace__Value_First_Meet — the ONE meet law, value-first.
 *
 *  @details The meet is written ONCE here, over a @c Interval whose pivot is a
 *  @b value.  Being @c constexpr it serves BOTH phases (the
 * compile-time/runtime optionality): the set-level @c structured_and below
 * delegates to it, so the meet of two cuts is a @c Interval whatever their
 * pivots, and the Python @c dedekind.lwv surface calls the @b same function on
 * runtime pivots.  The pivot never needs to live in a type.
 *
 *  Value-oriented relational grounding: a halfspace is a point plus a
 * direction,
 *  @f$\{x \mid x \bowtie p\} = \pi_1(\,S \times \eta(p) \mid \pi_1 \bowtie
 *  \pi_2\,)@f$, the pivot in the value @f$\eta(p)@f$ (a @c Singleton); the
 *  singleton is the degenerate point @c Interval::point.  House spelling @c χ
 * /
 *  @c π₁,π₂.
 *
 *  @c Bound is one side of a cut as a value: the bound and whether it is
 *  attained (non-strict) or not (strict).  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Bound {
  V at;
  Strictness strictness = Strictness::NonStrict;
  friend constexpr bool operator==(const Bound&, const Bound&) = default;
};
/** @brief The sides of an inhabited convex set; an absent side is unbounded.
 *  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Span {
  std::optional<Bound<V>> lower;
  std::optional<Bound<V>> upper;
  friend constexpr bool operator==(const Span&, const Span&) = default;
};

// The NNO steps (@c category:nno) as customization points: the two-step brings
// the generic defaults into scope so an unqualified call also admits a
// carrier's own ADL overload (the ℕ proxy's, in @c :sets:cardinality).
using dedekind::category::HasCoveringStep;
using dedekind::category::HasNNOStep;
using dedekind::category::IsPst;
using dedekind::category::predecessor;
using dedekind::category::successor;
// The gate is TRUE on both the machine integers and the ℕ proxy: the latter's
// steps are found by ADL in :sets:cardinality.  This is what restores the ℕ
// point collapse ({x>5} ∩ {x<7} = {6}) that a std::integral gate excluded.
static_assert(HasNNOStep<int>, "machine integers step by ±1.");
static_assert(HasNNOStep<dedekind::sets::Cardinality>,
              "the ℕ proxy steps through its NNO successor / predecessor "
              "(the canonical NNO witness, found by ADL).");
static_assert(HasNNOStep<dedekind::sets::SignedCardinality>,
              "the ℤ proxy steps by ±1 through the same customization points, "
              "so a strict bound on ℤ is attained at its neighbour as on ℕ.");

/** @section halfspace__Positions
 *  The places on a chain a side can sit: just below a value or just above it,
 *  a Dedekind cut named by the value beside it.  An interval's sides and a run
 *  list's steps are positions, so both compare, close and read the carrier's
 *  ends through this one layer. */

/** @brief Which side of its value a position sits on. */
export enum class Side : bool { Below, Above };

/** @brief A position on the chain: just below @c at, or just above it.
 *  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Position {
  V at;
  Side side;
  friend constexpr bool operator==(const Position&, const Position&) = default;
  /** @brief Along the chain: by value, and below a value before above it. */
  friend constexpr bool operator<(const Position& a, const Position& b) {
    return a.at < b.at ||
           (a.at == b.at && a.side == Side::Below && b.side == Side::Above);
  }
};

namespace detail_position {
/** @brief The position a lower side starts at: below an attained bound, above a
 *  strict one.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> start_of(const Bound<V>& b) {
  return {b.at,
          b.strictness == Strictness::NonStrict ? Side::Below : Side::Above};
}
/** @brief The position an upper side ends at: above an attained bound, below a
 *  strict one.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> end_of(const Bound<V>& b) {
  return {b.at,
          b.strictness == Strictness::NonStrict ? Side::Above : Side::Below};
}
/** @brief The lower side starting at @c p.  @tparam V the carrier. */
template <typename V>
constexpr Bound<V> lower_at(const Position<V>& p) {
  return {p.at,
          p.side == Side::Below ? Strictness::NonStrict : Strictness::Strict};
}
/** @brief The upper side ending at @c p.  @tparam V the carrier. */
template <typename V>
constexpr Bound<V> upper_at(const Position<V>& p) {
  return {p.at,
          p.side == Side::Above ? Strictness::NonStrict : Strictness::Strict};
}
/** @brief The carrier's least element, where it has one as a value.
 *  @tparam V the carrier. */
template <typename V>
constexpr std::optional<V> least() {
  if constexpr (std::integral<V>)
    return std::numeric_limits<V>::min();
  else if constexpr (std::same_as<V, Cardinality>)
    return V{};
  else if constexpr (std::same_as<V, SignedCardinality>)
    return V{NegativeInfinity{}};
  else if constexpr (IsPst<V>)
    return dedekind::sets::chain_bottom<V>();
  else
    return std::nullopt;
}
/** @brief The carrier's greatest element, where it has one as a value (ℵ₀ on
 *  ℕ).  @tparam V the carrier. */
template <typename V>
constexpr std::optional<V> greatest() {
  if constexpr (std::integral<V>)
    return std::numeric_limits<V>::max();
  else if constexpr (std::same_as<V, Cardinality>)
    return V{ℵ_0{}};
  else if constexpr (std::same_as<V, SignedCardinality>)
    return V{PositiveInfinity{}};
  else if constexpr (IsPst<V>)
    return dedekind::sets::chain_top<V>();
  else
    return std::nullopt;
}
/** @brief The element covering @c x: none at the greatest element (a saturating
 *  step stays put there) or on a carrier without the step.  @tparam V the
 *  carrier. */
template <typename V>
constexpr std::optional<V> next_above(const V& x) {
  if constexpr (std::integral<V>) {
    if (x == std::numeric_limits<V>::max()) return std::nullopt;
    return successor(x);
  } else if constexpr (HasNNOStep<V>) {
    const V s = successor(x);
    if (s == x) return std::nullopt;
    return s;
  } else {
    return std::nullopt;
  }
}
/** @brief A position in its canonical form: just above a value is just below
 *  the element covering it, where there is one, so on a discrete chain
 *  @c (0, 3) and @c [1, 3) have the same positions.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> canonical(Position<V> p) {
  if (p.side == Side::Above)
    if (const auto n = next_above(p.at)) return {*n, Side::Below};
  return p;
}
/** @brief Whether @c p lies before every element of the carrier.
 *  @tparam V the carrier. */
template <typename V>
constexpr bool before_all(const Position<V>& p) {
  const auto lo = least<V>();
  return lo && !(Position<V>{*lo, Side::Below} < p);
}
/** @brief Whether @c p lies after every element of the carrier.
 *  @tparam V the carrier. */
template <typename V>
constexpr bool after_all(const Position<V>& p) {
  const auto hi = greatest<V>();
  return hi && !(p < Position<V>{*hi, Side::Above});
}
}  // namespace detail_position

/** @brief A convex subset of a chain as a VALUE --- an interval: each side an
 *  optional @c Bound, with a runtime-evaluable membership χ.  Closed under ∩;
 *  a union or a complement leaves it unless one operand covers the other.  No
 * side is the universe; both sides closed at one value is a point; no @c Span
 * at all is the one empty set, to which crossing bounds normalise.  The
 * bounds ride as values, never as NTTPs.  Consume it through @c &, @c |, @c ==,
 * @c <=, membership and the @c half / @c point / @c empty / @c universe / @c
 * bounded factories.
 *
 *  The carrier is gated on the registered chain (@c IsTotallyOrdered): a cut
 *  is a normal form only on a chain.
 *  @tparam V the carrier.  @tparam L the species.
 */
export template <typename V = long long, typename L = Boole>
  requires IsTotallyOrdered<V> && IsOckhamAlgebra<L>
struct Interval : dedekind::sets::SetExpr<Interval<V, L>, V, L> {
  using Domain = V;
  using Codomain = typename L::Ω;
  using cardinality_type = carrier_cardinality_t<V>;

  /** @brief The universe, 𝔸. */
  constexpr Interval() = default;
  /** @brief The sides, or @c nullopt for Ø: a convex set is @c 1 @c + @c Span.
   *  Built only through the factories below, so always in normal form. */
  constexpr const std::optional<Span<V>>& span() const { return span_; }

  /** @brief χ(x): runtime membership, decided from the value fields.
   *
   *  @details The two DECIDED cases (⊥ / ⊤) need no carrier comparison, so
   *  they are answered in this small front and the carrier comparisons live in
   *  @c contains_bounded behind it.  The split is load-bearing for the IR
   *  exhibits: on a variant carrier (ℕ = @c Cardinality) the comparisons are
   *  heavy enough that the inliner declines a single fat function, and a
   *  constant @c Interval then stays a call instead of folding to @c ret @c i1
   *  @c false.  A small front inlines, the loads from the constant fold, and
   *  the bounded call is dead code. */
  constexpr bool contains(const V& x) const {
    if (!span_) return false;
    if (!span_->lower && !span_->upper) return true;
    return contains_bounded(x);
  }

  /** @brief The sides' comparisons against the carrier. */
  constexpr bool contains_bounded(const V& x) const {
    const auto& [lower, upper] = *span_;
    const bool lo_ok =
        !lower || (lower->strictness == Strictness::Strict ? x > lower->at
                                                           : x >= lower->at);
    const bool hi_ok =
        !upper || (upper->strictness == Strictness::Strict ? x < upper->at
                                                           : x <= upper->at);
    return lo_ok && hi_ok;
  }

  /** @brief ETCS χ: @c contains lifted to the logic species @c L. */
  constexpr typename L::Ω operator()(const V& x) const {
    return contains(x) ? L::True : L::False;
  }

  /** @brief The ray @c {x ⋈ pivot}.  Whether it is empty, or all of the
   *  carrier, at the carrier's end is read by @c == @c Ø / @c == @c 𝔸. */
  static constexpr Interval half(V pivot, Direction d, Strictness s) {
    if (d == Direction::Upward)
      return Interval{Span<V>{Bound<V>{pivot, s}, {}}};
    return Interval{Span<V>{{}, Bound<V>{pivot, s}}};
  }
  static constexpr Interval point(V pivot) {
    return Interval{Span<V>{Bound<V>{pivot}, Bound<V>{pivot}}};
  }
  /** @brief The two boundary sets as values --- the meet's annihilator (@c Ø)
   *  and unit (@c 𝔸).  Factories (not raw aggregate init) so callers never
   * spell the empty-base brace of the @c SetExpr aggregate. */
  static constexpr Interval empty() { return Interval{std::nullopt}; }
  static constexpr Interval universe() { return {}; }
  /** @brief The bounded meet @c [lo, hi] with its strictness pair, collapsed:
   *  empty if the bounds cross, the single point where the chain admits exactly
   *  one element, else the interval.  The ONE collapse law every meet rides.
   *  The point collapse gates on the NNO steps (@c HasNNOStep: successor /
   *  predecessor, an axiom of the category), so the ℕ proxy @c Cardinality
   *  collapses to its point exactly as a machine integer does. */
  static constexpr Interval bounded(V lo, Strictness sl, V hi, Strictness su) {
    const bool either_strict =
        sl == Strictness::Strict || su == Strictness::Strict;
    if (either_strict ? (lo >= hi) : (lo > hi)) return empty();
    // With lo < hi already established, pred(hi) is defined (hi ≥ 1).
    if constexpr (HasNNOStep<V>) {
      const V el = sl == Strictness::Strict ? successor(lo) : lo;
      const V eu = su == Strictness::Strict ? predecessor(hi) : hi;
      if (el > eu) return empty();
      if (el == eu) return point(el);
    }
    return Interval{Span<V>{Bound<V>{lo, sl}, Bound<V>{hi, su}}};
  }
  /** @brief The set with the given sides: through @c bounded when both are
   *  present, so it is collapsed.  @param lo the lower side.  @param hi the
   *  upper side. */
  static constexpr Interval of_sides(const std::optional<Bound<V>>& lo,
                                     const std::optional<Bound<V>>& hi) {
    if (lo && hi)
      return bounded(lo->at, lo->strictness, hi->at, hi->strictness);
    if (lo) return half(lo->at, Direction::Upward, lo->strictness);
    if (hi) return half(hi->at, Direction::Downward, hi->strictness);
    return universe();
  }

 private:
  std::optional<Span<V>> span_ = Span<V>{};
  constexpr explicit Interval(std::optional<Span<V>> s) : span_(std::move(s)) {}
};

// The value set is a set OBJECT --- (reified universe, χ) --- so whatever the
// Python surface exports (it exports Interval) conforms to IsSetObject by
// construction.  A MUST for the exported type, pinned here at its definition.
static_assert(
    dedekind::sets::IsSetObject<Interval<int>>,
    "Interval is a set object: the value normal form over its universe.");
static_assert(
    dedekind::sets::IsSetObject<Interval<long long>>,
    "the Python surface's Set (Interval<long long>) is a set object.");

}  // namespace dedekind::order
namespace dedekind::sets {
/** @brief An order atom @c π @c ⋈ @c k on a chain is the ray it names,
 *  whatever the constant's type: the constant meets the carrier exactly or the
 *  binding raises.  Declared beside @c 𝔸 and the atom, so ADL finds it.
 *  @tparam T the carrier.  @tparam R the order relation.
 *  @tparam K the constant's type. */
export template <dedekind::order::IsTotallyOrdered T,
                 dedekind::category::IsOckhamAlgebra L, IsCardinality C, Rel R,
                 typename K>
  requires(dedekind::order::is_order_rel(R))
constexpr dedekind::order::Interval<T, L> operator|(
    const 𝔸<T, L, C>&, const Cmp<R, Projection<0>, Const<K>>& c) {
  return dedekind::order::Interval<T, L>::half(carrier_cast<T>(c.b.value),
                                               dedekind::order::dir_of(R),
                                               dedekind::order::strict_of(R));
}
}  // namespace dedekind::sets
namespace dedekind::order {

namespace detail_interval {
/** @brief The tighter of two lower sides: the larger bound; at a tie the
 *  strict one.  An absent side is no constraint.  @tparam V the carrier. */
template <typename V>
constexpr std::optional<Bound<V>> tighter_lower(
    const std::optional<Bound<V>>& a, const std::optional<Bound<V>>& b) {
  if (!a) return b;
  if (!b) return a;
  if (a->at != b->at) return a->at > b->at ? a : b;
  return a->strictness == Strictness::Strict ? a : b;
}
/** @brief The dual: the smaller bound; at a tie the strict one. */
template <typename V>
constexpr std::optional<Bound<V>> tighter_upper(
    const std::optional<Bound<V>>& a, const std::optional<Bound<V>>& b) {
  if (!a) return b;
  if (!b) return a;
  if (a->at != b->at) return a->at < b->at ? a : b;
  return a->strictness == Strictness::Strict ? a : b;
}
}  // namespace detail_interval

/** @brief The ONE meet law: the tighter side on each end, collapsed by
 *  @c bounded (crossing sides to @c Ø, a single admitted element to its point).
 *  @c constexpr, so evaluable at compile time or runtime. */
export template <typename V, typename L>
constexpr Interval<V, L> reduce_meet(const Interval<V, L>& a,
                                     const Interval<V, L>& b) {
  using S = Interval<V, L>;
  if (!a.span() || !b.span()) return S::empty();
  return S::of_sides(
      detail_interval::tighter_lower(a.span()->lower, b.span()->lower),
      detail_interval::tighter_upper(a.span()->upper, b.span()->upper));
}

namespace detail_interval {
/** @brief Where an inhabited interval starts and ends: its sides' positions in
 *  canonical form, an absent side the carrier's own end where it has one.
 *  @tparam V the carrier. */
template <typename V>
constexpr std::pair<std::optional<Position<V>>, std::optional<Position<V>>>
extent(const Span<V>& s) {
  using detail_position::canonical;
  return {s.lower
              ? std::optional{canonical(detail_position::start_of(*s.lower))}
              : std::nullopt,
          s.upper ? std::optional{canonical(detail_position::end_of(*s.upper))}
                  : std::nullopt};
}
/** @brief Whether an interval has no element: no sides at all, or sides that
 *  meet or cross, or a side beyond the carrier's end ({x > max}, {x < 0} on
 *  ℕ, {n > ℵ₀}).  @tparam V the carrier. */
template <typename V, typename L>
constexpr bool is_empty(const Interval<V, L>& s) {
  if (!s.span()) return true;
  const auto [lo, hi] = extent(*s.span());
  if (lo && detail_position::after_all(*lo)) return true;
  if (hi && detail_position::before_all(*hi)) return true;
  return lo && hi && !(*lo < *hi);
}
/** @brief Whether an interval is the whole carrier: each present side at or
 *  past the carrier's end ({x ≤ ℵ₀} on ℕ).  @tparam V the carrier. */
template <typename V, typename L>
constexpr bool is_whole(const Interval<V, L>& s) {
  if (!s.span()) return false;
  const auto [lo, hi] = extent(*s.span());
  return (!lo || detail_position::before_all(*lo)) &&
         (!hi || detail_position::after_all(*hi));
}
}  // namespace detail_interval

/** @brief Equality of two intervals as sets of the carrier, in the ambient
 *  logic @c L::Ω: both empty, or the same sides compared as canonical
 *  positions.  On a carrier with the step this is @b extensional: a strict
 *  bound at @c a is attained at the element covering @c a, so @c (0, 3) and
 *  @c [1, 3) are the same set of ℤ and compare equal; on a dense carrier they
 *  differ.  Gated on decidable equality of the carrier. */
export template <typename V, typename L>
  requires std::equality_comparable<V>
constexpr typename L::Ω operator==(const Interval<V, L>& a,
                                   const Interval<V, L>& b) {
  const bool ea = detail_interval::is_empty(a);
  const bool eb = detail_interval::is_empty(b);
  const bool eq = (ea || eb) ? (ea && eb)
                             : detail_interval::extent(*a.span()) ==
                                   detail_interval::extent(*b.span());
  return eq ? L::True : L::False;
}
/** @brief An interval against the boundaries: @c Ø when it has no element,
 *  @c 𝔸 when its sides reach the carrier's ends.  On a truth chain the
 *  boundaries decide by exhausting the chain instead (sets:boundaries). */
export template <IsTotallyOrdered T, IsOckhamAlgebra L>
  requires(!IsPst<T>)
constexpr bool operator==(const Ø<T, L>&, const Interval<T, L>& s) {
  return detail_interval::is_empty(s);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L>
  requires(!IsPst<T>)
constexpr bool operator==(const Interval<T, L>& s, const Ø<T, L>& e) {
  return e == s;
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality CU>
  requires(!IsPst<T>)
constexpr bool operator==(const 𝔸<T, L, CU>&, const Interval<T, L>& s) {
  return detail_interval::is_whole(s);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality CU>
  requires(!IsPst<T>)
constexpr bool operator==(const Interval<T, L>& s, const 𝔸<T, L, CU>& u) {
  return u == s;
}
namespace detail_interval_witness {
using dedekind::category::Ternary;
using dedekind::sets::Cardinality;
using dedekind::sets::ℵ_0;
static_assert(
    Interval<int>::bounded(0, Strictness::Strict, 3, Strictness::Strict) ==
        Interval<int>::bounded(1, Strictness::NonStrict, 3, Strictness::Strict),
    "(0, 3) = [1, 3) on ℤ: the strict bound closes at the next element.");
static_assert(
    !(Interval<Ternary>::half(Ternary::True, Direction::Upward,
                              Strictness::Strict) ==
      Interval<Ternary>::half(Ternary::True, Direction::Upward,
                              Strictness::NonStrict)),
    "{x > ⊤} is not {x ≥ ⊤}: at a saturating end a strict bound has no closed "
    "form.");
static_assert(
    !(Interval<Cardinality>::half(Cardinality{ℵ_0{}}, Direction::Downward,
                                  Strictness::Strict) ==
      Interval<Cardinality>::half(Cardinality{ℵ_0{}}, Direction::Downward,
                                  Strictness::NonStrict)),
    "{x < ℵ₀}, the finite fragment, is not {x ≤ ℵ₀} = 𝔸.");
static_assert(Ø<int>{} == Interval<int>::half(std::numeric_limits<int>::max(),
                                              Direction::Upward,
                                              Strictness::Strict) &&
                  𝔸<Cardinality>{} ==
                      Interval<Cardinality>::half(Cardinality{ℵ_0{}},
                                                  Direction::Downward,
                                                  Strictness::NonStrict),
              "the carrier's ends: {x > max} is Ø, {n ≤ ℵ₀} is 𝔸.");
}  // namespace detail_interval_witness

/** @section halfspace__Halfspace_Structural_Algebra — ADL hooks for operator&&.
 */

/** @brief The meet of two intervals, under the reducer's customization-point
 *  name: the one @c reduce_meet.  Declared here, before @c axis_factor, so the
 *  recursion over a @c RelAnd tree is closed over its own outputs (#872). */
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Interval<V, L> structured_and(const Interval<V, L>& a,
                                        const Interval<V, L>& b) {
  return reduce_meet(a, b);
}

/** @section halfspace__Union_And_Complement
 *  The union of two intervals and the complement of one leave the intervals:
 *  they are run lists (@c order:runs), the normal form of sets of a chain
 *  built from intervals. */

/** @section halfspace__PointFree_Decidability
 *
 * The point-free comprehension @c ℕ @c | @c pred reduces to a bare
 * @c Halfspace, whose species is the universe's (@c Boole): decidable
 * membership, with no carrier-axis resolver in between. */
namespace detail_pointfree_decidability {
using PointFree = decltype(ℕ | (χ > 5));
static_assert(HasDecidableMembership<decltype(ℕ | (χ > 5))>,
              "ℕ | x>5 is a decidable (Boole) set.");
}  // namespace detail_pointfree_decidability

// The comparison an atom makes, its evaluation and its De Morgan dual, and the
// relational-predicate concept, are the term grammar's (sets:terms).
export using dedekind::sets::IsRelPredicate;
export using dedekind::sets::negate;
export using dedekind::sets::Rel;
export using dedekind::sets::rel_apply;

/** @brief The product's own membership conjoined with a relational
 *  restriction, so a bind over a @b restricted product keeps that membership
 *  and cannot admit pairs outside it (a full product contributes a
 *  trivially-true @c product). */
export template <typename P, typename RP>
struct ProductRestrict {
  using is_rel_predicate = void;
  P product;
  RP rp;
  // NOTE: @c product here is a bool-returning rel-predicate (or the always-true
  // universal), so the boolean cast is safe in the graph DSL.  A ternary-valued
  // product would corrupt via @c static_cast<bool> (Ternary::False's underlying
  // −1 casts to true); a logic-preserving @c L::AND needs @c L threaded through
  // @c ProductRestrict, tracked as a follow-up (not reachable by construction
  // today, since restrictions are decidable comparisons).
  template <typename Pair>
  constexpr bool operator()(const Pair& p) const {
    return static_cast<bool>(product(p)) && rp(p);
  }
};

/** @brief @c 𝔸<A×B> @c | @c relPred --- the relation as a set object on
 *  @c A×B.  The universal product carries no factor restriction, so the
 *  relation's membership @b is the rel-predicate: the pure product universe
 *  refined to a subobject.
 *  @tparam T1,T2 the factor carriers; @tparam L the logic; @tparam C the
 *  product's cardinality class; @tparam RP the rel-predicate.
 *  @param rp the rel-predicate on pairs.
 *  @return @c Set<pair<T1,T2>, L, RP>. */
export template <typename T1, typename T2, typename L, typename C,
                 IsRelPredicate RP>
constexpr auto operator|(const 𝔸<std::pair<T1, T2>, L, C>&, RP rp) {
  bind_check<std::pair<T1, T2>>(rp);
  return Comprehension<𝔸<std::pair<T1, T2>, L>, RP>{rp};
}

/** @brief @c product @c | @c relPred --- the relation as a set object on
 *  @c A×B, keeping the product's own membership (so a restricted product
 *  bounds the relation).
 *  @details Generic over any set object on a pair carrier (a @c Set<pair,…>, a
 *  lattice node, a comprehension): the restricted χ datum is the product's
 *  @b classifier leg --- @c P for a @c Set, the node itself for a node.
 *  @tparam S an @c IsSetObject whose @c Domain is a pair; @tparam RP the
 *  rel-predicate.
 *  @param prod the product set object; @param rp the rel-predicate on pairs.
 *  @return @c Set<Domain, L, ProductRestrict<classifier, RP>>. */
export template <typename S, IsRelPredicate RP>
  requires dedekind::sets::IsSetObject<S> && requires {
    typename S::Domain::first_type;  // a pair carrier: both coordinates
    typename S::Domain::second_type;
  }
constexpr auto operator|(const S& prod, RP rp) {
  bind_check<typename S::Domain>(rp);
  using X = std::remove_cvref_t<decltype(classifier(prod))>;
  return Comprehension<𝔸<typename S::Domain, typename S::logic_species>,
                       ProductRestrict<X, RP>>{
      ProductRestrict<X, RP>{classifier(prod), rp}};
}

/**
 * @section halfspace__Restricted_Products
 * @brief A @b restricted factor lifts to a @b cylinder on its axis, so
 *        @c operator* keeps the factor predicates instead of dropping them.
 *
 * @details The product is the universal set of products @c 𝔸<pair> refined by
 * the two @b cylinders @f$A\times B = \pi_1^{-1}(A)\cap\pi_2^{-1}(B)@f$.  A
 * total factor's cylinder is the whole universe (nothing to add); a halfspace
 * factor @f$\{x \bowtie p\}@f$ lifts to the projection halfspace
 * @f$\pi_I \bowtie p@f$ --- the @b same comparison atom the graph surface uses,
 * so a restricted domain and a functional graph read in one vocabulary and @c
 * dom can recover the factor from the @c π_I conjunct.
 */

/** @brief The comparison a halfspace @c (Direction, Strictness) lifts to. */
export constexpr Rel rel_of(Direction d, Strictness s) {
  if (d == Direction::Upward) {
    return s == Strictness::Strict ? Rel::Gt : Rel::Ge;
  }
  return s == Strictness::Strict ? Rel::Lt : Rel::Le;
}

/** @brief @f$\pi_I^{-1}(S)@f$: the cylinder of a set @c S on axis @c I of a
 *  pair, a relational predicate.  @tparam I the axis.  @tparam S the set. */
export template <IsRingIntegral auto I, typename S>
struct InSlot {
  using is_rel_predicate = void;
  S set;
  template <typename P>
  constexpr bool operator()(const P& p) const {
    return static_cast<bool>(set(coord<I>(p)));
  }
};

// restricted × total, total × restricted, restricted × restricted.
export template <IsTotallyOrdered T, IsOckhamAlgebra L, typename T2,
                 IsCardinality C2>
constexpr auto operator*(const Interval<T, L>& a, const 𝔸<T2, L, C2>&) {
  return 𝔸<std::pair<T, T2>, L>{} | InSlot<1, Interval<T, L>>{a};
}
export template <typename T1, IsCardinality C1, IsTotallyOrdered T,
                 IsOckhamAlgebra L>
constexpr auto operator*(const 𝔸<T1, L, C1>&, const Interval<T, L>& b) {
  return 𝔸<std::pair<T1, T>, L>{} | InSlot<2, Interval<T, L>>{b};
}
export template <IsTotallyOrdered Ta, IsTotallyOrdered Tb, IsOckhamAlgebra L>
constexpr auto operator*(const Interval<Ta, L>& a, const Interval<Tb, L>& b) {
  return 𝔸<std::pair<Ta, Tb>, L>{} |
         (InSlot<1, Interval<Ta, L>>{a} && InSlot<2, Interval<Tb, L>>{b});
}

/**
 * @section halfspace__Projections
 * @brief @c dom / @c cod as the projections @f$\pi_A / \pi_B@f$, recovering the
 *        factor from the relation's @b structure (the inverse of @c cylinder).
 *
 * @details These are the @b π_A / π_B side of Table 3 (the four properties of a
 * relation): @c dom is @f$\pi_A(R)@f$, @c cod is @f$\pi_B(R)@f$.  Read off the
 * axis-@f$I@f$ cylinder atom structurally: a comparison bound becomes
 * the halfspace factor, no axis-@f$I@f$ bound leaves the declared universe
 * @c 𝔸<T_I> (honest exactly when @c R is entire on that side).  This is the
 * @b free case; the @b existential @f$\{a\mid\exists b.R(a,b)\}@f$ that a
 * coupled, non-entire relation needs is the separate Rice-gated operation.
 * The order-layer overloads specialise the sets-layer @c 𝔸<T1> fallback (they
 * win by @c IsRelPredicate).
 */

/** @brief Recover the axis-@c I factor from a relational predicate.  Default:
 *  no axis-@c I structure, so the declared universe @c 𝔸<TI>. */
export template <IsRingIntegral auto I, typename TI, typename L, typename P>
constexpr auto axis_factor(const P&) {
  return 𝔸<TI, L>{};  // preserve the relation's logic species
}

/** @brief An order atom on axis @c I, @c π_I @c ⋈ @c c: the ray it names
 *  (Eq/Ne fall to the default).  The slot is matched to the axis by value, so
 *  two integral spellings of one axis agree. */
export template <IsRingIntegral auto I, IsTotallyOrdered TI, IsOckhamAlgebra L,
                 IsRingIntegral auto Slot, Rel R, typename K>
  requires(is_order_rel(R) && Slot == I)
constexpr auto axis_factor(const Cmp<R, Projection<Slot>, Const<K>>& c) {
  return Interval<TI, L>::half(carrier_cast<TI>(c.b.value), dir_of(R),
                               strict_of(R));
}
/** @brief A cylinder on axis @c I: its set. */
export template <IsRingIntegral auto I, IsTotallyOrdered TI, IsOckhamAlgebra L,
                 IsRingIntegral auto Slot, typename S>
  requires(Slot == I)
constexpr S axis_factor(const InSlot<Slot, S>& c) {
  return c.set;
}

/** @brief A meet of cylinders: the factor on axis @c I is the @b intersection
 *  of both children's factors on that axis, so two bounds on the same axis
 *  (@c π1<=5 @c && @c π1<=3) meet to the tighter one rather than dropping
 * either.
 */
export template <IsRingIntegral auto I, typename TI, typename L, typename A,
                 typename B>
constexpr auto axis_factor(const dedekind::relational::RelAnd<A, B>& r) {
  auto fa = axis_factor<I, TI, L>(r.a);
  auto fb = axis_factor<I, TI, L>(r.b);
  if constexpr (requires { typename decltype(fa)::is_universal_boundary; }) {
    return fb;  // a does not constrain axis I; the factor is b's
  } else if constexpr (requires {
                         typename decltype(fb)::is_universal_boundary;
                       }) {
    return fa;  // b does not constrain axis I; the factor is a's
  } else {
    // BOTH constrain axis I: the set-level meet of the recovered factors (a
    // cut or, from a deeper meet, a Interval), through structured_and directly
    // --- the set-level operator& is declared below this point.
    return dedekind::order::structured_and(fa, fb);
  }
}

/** @brief A restricted product bounding a graph: the factor lives on the
 *  product (cylinder) side; the graph @c rp couples the axes, so it is
 *  transparent to a single-axis projection. */
export template <IsRingIntegral auto I, typename TI, typename L, typename Pp,
                 typename RP>
constexpr auto axis_factor(const ProductRestrict<Pp, RP>& r) {
  return axis_factor<I, TI, L>(r.product);
}

/** @brief @f$\pi_A(R)@f$ --- the domain factor, recovered structurally
 *  (preserving the relation's logic species @c L). */
export template <typename T1, typename T2, typename L, IsRelPredicate P,
                 typename C>
constexpr auto dom(const Comprehension<𝔸<std::pair<T1, T2>, L, C>, P>& r) {
  return axis_factor<1, T1, L>(r.predicate);
}

/** @brief @f$\pi_B(R)@f$ --- the codomain factor, recovered structurally. */
export template <typename T1, typename T2, typename L, IsRelPredicate P,
                 typename C>
constexpr auto cod(const Comprehension<𝔸<std::pair<T1, T2>, L, C>, P>& r) {
  return axis_factor<2, T2, L>(r.predicate);
}

/** @section halfspace__Formal_Verification (relational surface) */

// less-than on 𝔹×𝔹, a strongly-typed point-free relation, membership-checked.
static_assert((𝔹 * 𝔹 | π1 < π2)(std::pair{false, true}),
              "(false, true) ∈ {(x,y) | x < y}.");
static_assert(!(𝔹 * 𝔹 | π1 < π2)(std::pair{true, true}),
              "(true, true) ∉ {(x,y) | x < y}.");

// a meet of two projection predicates: {(x,y) | x ≤ y ∧ y == true}.
static_assert((𝔹 * 𝔹 | (π1 <= π2 && π2 == true))(std::pair{false, true}),
              "(false, true) satisfies x ≤ y ∧ y = true.");
static_assert(!(𝔹 * 𝔹 | (π1 <= π2 && π2 == true))(std::pair{false, false}),
              "(false, false) fails y = true.");

// NESTED meet: BOTH operands of the outer && are themselves RelAnd (relational-
// native, no bare :order operand), so this is the case the FIXME(#824) warns
// about --- yet ADL still reaches structured_and THROUGH RelAnd's projection-
// atom template args, so the marker survives and the comprehension restricts.
// relpred is therefore closed under &&/|| for the projection sub-grammar; the
// fall-through only bites non-projection rel-predicates (e.g. two diag()).
static_assert(
    IsRelPredicate<decltype((π1 <= π2 && π2 == true) && (π1 <= π2 && π1 < π2))>,
    "nested (RelAnd && RelAnd) stays IsRelPredicate via ADL on the atom args.");
static_assert((𝔹 * 𝔹 | ((π1 <= π2 && π2 == true) &&
                        (π1 <= π2 && π1 < π2)))(std::pair{false, true}),
              "(false, true) satisfies (x ≤ y ∧ y = true) ∧ (x ≤ y ∧ x < y).");

// ── converse and the bracket-free relation query ───────────────────────────
// SwapPred / converse and IsPairLike / is_relation moved DOWN to
// dedekind.relational:dyadic (#792) --- pure Set<pair> algebra, no ordering.
// The ordered witnesses below stay here and reach them by ADL on their
// dedekind::sets::Set arguments (order imports dedekind.relational).

// converse swaps the coordinates; is_relation certifies the product Domain.
static_assert(converse(𝔹* 𝔹 | π1 < π2)(std::pair{true, false}),
              "converse of < contains (true, false): false < true.");
static_assert(is_relation(𝔹* 𝔹 | π1 < π2),
              "𝔹*𝔹 | π1 < π2 is a relation (IsSet on a product).");

// The arrow lift 𝑦 == ap(f, 𝑥) is the graph of f; for the identity, Δ.
static_assert((𝔹 * 𝔹 | (𝑦 == ap(dedekind::category::Identity<bool>{},
                                𝑥)))(std::pair{true, true}) &&
                  !(𝔹 * 𝔹 | (𝑦 == ap(dedekind::category::Identity<bool>{},
                                     𝑥)))(std::pair{true, false}),
              "𝑦 == ap(id, 𝑥) is the diagonal.");

// successor graph: {(a,b) | b = a + 1} = ℕ * ℕ | π1 + 1 == π2.
static_assert((ℕ * ℕ | π1 + 1 == π2)(std::pair{finite_cardinality(4),
                                               finite_cardinality(5)}),
              "4 + 1 == 5: (4,5) ∈ the successor graph.");
static_assert(!(ℕ * ℕ | π1 + 1 == π2)(std::pair{finite_cardinality(4),
                                                finite_cardinality(6)}),
              "4 + 1 != 6: (4,6) ∉ the successor graph.");
// A RESTRICTED domain: the successor graph over {x ≤ 5} × ℕ.  operator* keeps
// the ≤5 bound by lifting it to the π1 cylinder, so x = 7 is excluded even
// though 8 = 7+1: the factor predicate is not dropped.
static_assert(((ℕ | (π <= 5)) * ℕ | π1 + 1 == π2)(std::pair{
                  finite_cardinality(4), finite_cardinality(5)}),
              "(4,5): 4 ≤ 5 ∧ 5 = 4+1, in the restricted successor graph.");
static_assert(!((ℕ | (π <= 5)) * ℕ | π1 + 1 == π2)(std::pair{
                  finite_cardinality(7), finite_cardinality(8)}),
              "(7,8): 7 ≰ 5, excluded though 8 = 7+1.");
// doubling graph: {(a,b) | b = 2a} = ℕ * ℕ | π1 * 2 == π2.
static_assert((ℕ * ℕ | π1 * 2 == π2)(std::pair{finite_cardinality(3),
                                               finite_cardinality(6)}),
              "3 * 2 == 6: (3,6) ∈ the doubling graph.");
static_assert(!(ℕ * ℕ | π1 * 2 == π2)(std::pair{finite_cardinality(3),
                                                finite_cardinality(7)}),
              "3 * 2 != 7: (3,7) ∉ the doubling graph.");
// affine graph: {(a,b) | b = 3a+1} = ℕ * ℕ | π1 * 3 + 1 == π2
// (the Collatz odd branch, :collatz).
static_assert((ℕ * ℕ | π1 * 3 + 1 == π2)(std::pair{finite_cardinality(7),
                                                   finite_cardinality(22)}),
              "3·7+1 == 22: (7,22) ∈ the affine graph.");
static_assert(!(ℕ * ℕ | π1 * 3 + 1 == π2)(std::pair{finite_cardinality(7),
                                                    finite_cardinality(21)}),
              "3·7+1 != 21: (7,21) ∉ the affine graph.");

// dom / cod are the projections π_A / π_B (Table 3), recovered from the graph's
// STRUCTURE: they return the DECLARED domain/codomain, not the effective image.
// The residue graph has no axis cylinder, so both recover the declared universe
// ℕ (unqualified so ADL finds the order-layer recovery).  cod is thus the
// declared ℕ --- its EFFECTIVE second projection is only the residue class
// {0,…,16}, but reading that off is the ∃-elimination behind the Rice wall
// (§3.3), so cod does not claim surjectivity here.
static_assert(dom(ℕ* ℕ | π1 % 17 == π2)(finite_cardinality(100)),
              "π_A of the residue graph = declared ℕ (entire), contains 100.");
static_assert(cod(ℕ* ℕ | π1 % 17 == π2)(finite_cardinality(3)),
              "π_B = declared codomain ℕ, contains 3 (NOT the effective "
              "{0,…,16} residue image, which would need the ∃-projection).");

// With a RESTRICTED factor, dom recovers the halfspace off the π1 cylinder:
// π_A((ℕ|≤5)*ℕ) = {a ≤ 5}, while cod stays the unrestricted ℕ.
static_assert(dom((ℕ | (π <= 5)) * ℕ)(finite_cardinality(4)),
              "π_A recovers {a ≤ 5}: 4 ≤ 5.");
static_assert(!dom((ℕ | (π <= 5)) * ℕ)(finite_cardinality(7)),
              "π_A recovers {a ≤ 5}: 7 ≰ 5.");
static_assert(cod((ℕ | (π <= 5)) * ℕ)(finite_cardinality(99)),
              "π_B is unrestricted ℕ: contains 99.");
// The restriction survives a graph refinement (dom digs through
// ProductRestrict).
static_assert(dom((ℕ | (π <= 5)) * ℕ | π1 + 1 == π2)(finite_cardinality(4)),
              "π_A of the restricted successor still recovers {a ≤ 5}.");

// TWO bounds on the SAME axis: dom recovers the TIGHTER halfspace (their meet),
// not either bound alone.  Guards axis_factor's both-branch: the factors are
// recovered Halfspace SETS, so their meet is the set-level & collapse; the
// predicate-level && would build a categorical Morphism and lose dom/cod.
static_assert(dom(ℕ* ℕ | (π1 <= 5 && π1 <= 3))(finite_cardinality(3)),
              "π_A of {a ≤ 5 ∧ a ≤ 3} recovers the tighter {a ≤ 3}: 3 ≤ 3.");
static_assert(!dom(ℕ * ℕ | (π1 <= 5 && π1 <= 3))(finite_cardinality(4)),
              "π_A recovers the TIGHTER bound: 4 ≤ 5 but 4 ≰ 3, so excluded.");
// Three same-axis bounds reduce to one factor whatever the association: the
// meet lattice is closed over its own outputs (#872).
static_assert(dom(ℕ* ℕ | ((π1 >= 1 && π1 <= 5) && π1 >= 2)) ==
                  Interval<Cardinality>::bounded(finite_cardinality(2),
                                                 Strictness::NonStrict,
                                                 finite_cardinality(5),
                                                 Strictness::NonStrict),
              "π_A of {1 ≤ a ≤ 5 ∧ a ≥ 2} is the interval [2, 5].");
static_assert(
    dom(ℕ* ℕ | ((π1 >= 1 && π1 <= 5) && π1 >= 2)) ==
        dom(ℕ * ℕ | (π1 >= 1 && (π1 <= 5 && π1 >= 2))),
    "the factor is independent of the association of the three bounds.");

// relational application: apply(R, a) is the fibre {b | (a,b) ∈ R}.  For the
// residue graph (a function) it is the singleton {a % 17}: apply(R,20) = {3}.
static_assert(
    dedekind::relational::apply(ℕ* ℕ | π1 % 17 == π2,
                                finite_cardinality(20))(finite_cardinality(3)),
    "apply(R,20) = {3}: (20,3) ∈ R since 20 % 17 == 3.");
static_assert(
    !dedekind::relational::apply(ℕ * ℕ | π1 % 17 == π2,
                                 finite_cardinality(20))(finite_cardinality(4)),
    "apply(R,20) does not contain 4.");

/** @brief @c upperbounds(S) --- the @f$\forall@f$-projection @f$R/\ni@f$: the
 *  region dominating all of @c S, and the pluggable point of the extremum
 *  (Bird \& de~Moor @cite birddemoor1997aop).
 *
 *  @details @c max is one GENERIC definition, the @f$\forall@f$-projection of
 * the order relation onto its second coordinate:
 *  @f[ \max S \;=\; \{\pi_2 \in S \mid \forall \pi_1 \in S.\; \pi_1 \le \pi_2\}
 *               \;=\; S \cap \mathrm{upperbounds}(S) \;=\; (\in) \cap (R/\ni).
 * @f]
 *  @c upperbounds is the pluggable point (as @f$\forall@f$ / @f$\exists@f$ plug
 *  into @c Ø::operator==), decided SYMBOLICALLY per structure so no candidate
 * is enumerated: a halfspace bounded ABOVE (@c {x≤p}) is dominated exactly by
 *  @c {x≥p}; one unbounded above (@c {x≥p}) has no upper bound (@c Ø).
 *  @c lowerbounds is the dual.  The generic @c max/min then meet @c S with
 * them, and the meet @c structured_and collapses the interval to the attained
 * pivot
 *  (@c {x≤p} ∩ @c {x≥p} = @c {p}) or, when the sup is unattained (strict) or
 *  absent (unbounded), to @c Ø. */
/** @brief Upper bounds of an interval, @c {x ≥ sup}: a strict upper bound is
 *  attained at its predecessor on a discrete carrier (so @c max @c {x<p} is
 *  @c p−1), and kept on a dense one (so the meet is empty, no max); an interval
 *  unbounded above has none; everything bounds @c Ø. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L>
constexpr Interval<T, L> upperbounds(const Interval<T, L>& s) {
  using S = Interval<T, L>;
  if (!s.span()) return S::universe();
  if (!s.span()->upper) return S::empty();
  const Bound<T>& b = *s.span()->upper;
  T sup = b.at;
  if constexpr (HasCoveringStep<T>)
    if (b.strictness == Strictness::Strict) sup = predecessor(b.at);
  return S::half(sup, Direction::Upward, Strictness::NonStrict);
}
/** @brief The dual: lower bounds, @c {x ≤ inf}. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L>
constexpr Interval<T, L> lowerbounds(const Interval<T, L>& s) {
  using S = Interval<T, L>;
  if (!s.span()) return S::universe();
  if (!s.span()->lower) return S::empty();
  const Bound<T>& b = *s.span()->lower;
  T inf = b.at;
  if constexpr (HasCoveringStep<T>)
    if (b.strictness == Strictness::Strict) inf = successor(b.at);
  return S::half(inf, Direction::Downward, Strictness::NonStrict);
}
// 𝔹: the whole carrier is bounded --- ⊤ dominates it, ⊥ is dominated by it.
export template <IsOckhamAlgebra L, IsCardinality C>
constexpr auto upperbounds(const 𝔸<bool, L, C>&) {
  return singleton<L>(true);
}
export template <IsOckhamAlgebra L, IsCardinality C>
constexpr auto lowerbounds(const 𝔸<bool, L, C>&) {
  return singleton<L>(false);
}

// #895/#932: a disjoint bare halfspace-meet collapses to the empty set.
static_assert(((ℕ | (π > 5)) & (ℕ | (π < 3))) == Ø<Cardinality>{},
              "bare {x>5} ∩ {x<3} collapses to the empty set (#895 / #932).");

/** @brief @c max(S) --- the generic extremum: @c S met (@c ∩) with its own
 *  @f$\forall@f$-dominators, @f$S \cap \mathrm{upperbounds}(S)@f$.  @c min is
 * the dual (@c S met with its minorants).
 *
 *  @details One definition for any ordered @c S whose @c upperbounds and meet
 * are defined; the structural collapse lives entirely in @c upperbounds and the
 *  meet, so there is no generic search.  Gated on the SEMANTIC order:
 *  greatest/least element is a @b partial-order notion, so the domain must
 *  certify @c IsPartiallyOrdered (dedekind's reflexive/transitive/antisymmetric
 *  axioms, which subsume @c std::totally_ordered one level up in
 *  @c IsTotallyOrdered).  A carrier without a certified order --- a raw float,
 *  whose NaN breaks reflexivity --- is honestly rejected: you cannot take the
 *  max of a set that may contain a NaN. */
export template <typename S>
  requires IsPartiallyOrdered<typename S::Domain> &&
           requires(const S& s) { s & upperbounds(s); }
constexpr auto max(const S& s) {
  return s & upperbounds(s);
}
export template <typename S>
  requires IsPartiallyOrdered<typename S::Domain> &&
           requires(const S& s) { s & lowerbounds(s); }
constexpr auto min(const S& s) {
  return s & lowerbounds(s);
}

inline constexpr auto ℤ =
    𝔸<SignedCardinality>{};  // local alias (:integer is downstream)
// Exhibit (intensional, infinite case) over ℕ = @c 𝔸<Cardinality> and ℤ =
// @c 𝔸<SignedCardinality>, both registered chains (ℤ under the posture that
// its sentinels ±ℵ_0 / NaZ are the memory boundary, not points of ℤ).
inline constexpr auto le5 = ℕ | (π <= 5);  // {x ∈ ℕ | x ≤ 5}
inline constexpr auto ge5 = ℕ | (π >= 5);  // {x ∈ ℕ | x ≥ 5}
static_assert(max(le5)(5), "5 = max {x ≤ 5} (read off the pivot).");
static_assert(!max(le5)(3), "3 is not the greatest element of {x ≤ 5}.");
static_assert(min(ge5)(5), "5 = min {x ≥ 5}.");
static_assert(!min(ge5)(7), "7 is not the least element of {x ≥ 5}.");
// DISCRETE strict: {x<5} on ℕ has attained max 4 (the predecessor), NOT ∅; the
// bound is reached through the NNO's step on the Cardinality proxy, exactly as
// on a machine integer.
static_assert(max(ℕ | (π < 5))(4), "4 = max {x < 5} on ℕ.");
static_assert(!max(ℕ | (π < 5))(5), "5 ∉ {x < 5}, so not its max.");
static_assert(min(ℕ | (π > 5))(6), "6 = min {x > 5} on ℕ.");
// ℤ: the same extrema, and the negative side ℕ cannot spell.
static_assert(max(ℤ | (π <= 5))(5), "5 = max {x ≤ 5} on ℤ.");
static_assert(!max(ℤ | (π <= 5))(3), "3 is not the greatest element.");
static_assert(min(ℤ | (π >= -5))(-5), "−5 = min {x ≥ −5} on ℤ.");
static_assert(max(ℤ | (π < 5))(4), "4 = max {x < 5} on ℤ (the step).");
static_assert(min(ℤ | (π > -5))(-4), "−4 = min {x > −5} on ℤ.");

/** @brief The translation graph @c π1 @c + @c k @c == @c π2 and the scaling
 *  graph @c π1 @c * @c k @c == @c π2, as the atoms the grammar builds for them.
 *  @tparam K the constant's type. */
export template <typename K>
using Translation =
    Cmp<Rel::Eq, BinOp<std::plus<>, Projection<1>, Const<K>>, Projection<2>>;
export template <typename K>
using Scaling = Cmp<Rel::Eq, BinOp<std::multiplies<>, Projection<1>, Const<K>>,
                    Projection<2>>;

/** @brief Two translation graphs are the same relation iff they carry the same
 *  shift, read in the carrier.  Gated on @c IsSaturating: the shift arithmetic
 *  is faithful only where the carrier's @c + is (on @c bool, @c +1 and @c +2
 *  are the same graph, so a comparison of shifts would separate them). */
export template <typename T, typename K1, typename K2, typename L, typename C,
                 typename CU>
  requires dedekind::category::IsSaturating<T>
constexpr bool operator==(
    const Comprehension<𝔸<std::pair<T, T>, L, C>, Translation<K1>>& a,
    const Comprehension<𝔸<std::pair<T, T>, L, CU>, Translation<K2>>& b) {
  return carrier_cast<T>(a.predicate.a.b.value) ==
         carrier_cast<T>(b.predicate.a.b.value);
}

/** @brief @b Symbolic composition of translation graphs, @f$T_a \circ T_b =
 *  T_{a+b}@f$: the shifts added in the carrier, with @b no @f$\exists@f$ over
 *  the intermediate.  Gated on @c IsSaturating, where the symbolic @c a+b is
 * the actual composite. */
export template <typename T, typename K1, typename K2, typename L, typename C,
                 typename CU>
  requires dedekind::category::IsSaturating<T>
constexpr auto operator>>(
    const Comprehension<𝔸<std::pair<T, T>, L, C>, Translation<K1>>& a,
    const Comprehension<𝔸<std::pair<T, T>, L, CU>, Translation<K2>>& b) {
  const T shift = carrier_cast<T>(a.predicate.a.b.value) +
                  carrier_cast<T>(b.predicate.a.b.value);
  return 𝔸<std::pair<T, T>, L>{} | (π1 + shift == π2);
}

/** @section halfspace__Cover_And_Star  The successor is the cover; the order
 *  is its star.  On a discrete chain nothing lies strictly between @c x and
 *  @c S(x): the bounded meet @c (x, S(x)) is empty and @c (x, S(S(x))) is the
 *  point @c S(x), both read off the one @c bounded law.  And the order is the
 *  Kleene star of the cover relation @f$\Gamma_S = \mathrm{graph}(S)@f$:
 *  @f$a < b \iff (a, b) \in \Gamma_S^{+}@f$ --- the one relation whose star is
 *  point-free here, because the chain's own @c < decides it. */
namespace detail_cover_witness {
consteval bool order_is_cover_star(int k) {
  for (int a = 0; a < k; ++a)
    for (int b = 0; b < k; ++b) {
      bool reached = false;
      int x = a;
      for (int n = 1; n < k; ++n) {
        x = Successor<int>{}(x);
        reached = reached || x == b;
      }
      if ((a < b) != reached) return false;
    }
  return true;
}
}  // namespace detail_cover_witness
static_assert(
    Interval<int>::bounded(3, Strictness::Strict, 4, Strictness::Strict) ==
        Interval<int>::empty(),
    "nothing lies strictly between 3 and S(3): the successor is the cover.");
static_assert(Interval<int>::bounded(3, Strictness::Strict, 5,
                                     Strictness::Strict) ==
                  Interval<int>::point(4),
              "(3, S(S(3))) is the point S(3) = 4.");
static_assert(Interval<Cardinality>::bounded(finite_cardinality(3),
                                             Strictness::Strict,
                                             finite_cardinality(4),
                                             Strictness::Strict) ==
                  Interval<Cardinality>::empty(),
              "…and on the ℕ proxy, through its own successor.");
static_assert(Interval<Ternary>::bounded(Ternary::False, Strictness::Strict,
                                         Ternary::True, Strictness::Strict) ==
                  Interval<Ternary>::point(Ternary::Unknown),
              "{x : ⊥ < x < ⊤} = {Unknown}: on K₃ the cover isolates the "
              "middle truth value.");
static_assert(graph(Successor<int>{})(std::pair{3, 4}) &&
                  !graph(Successor<int>{})(std::pair{3, 5}),
              "Γ_S = graph(S) is the cover relation.");
static_assert(detail_cover_witness::order_is_cover_star(8),
              "on [0, 8): a < b ⟺ (a, b) ∈ Γ_S⁺ --- the order is the star of "
              "the cover.");

// Modelling witness ("Theorems for Free", type-checked): the SPECIFIC pivot
// overload AGREES with the ABSTRACT definition max R = (∈) ∩ (R/∋) at the
// pivot.  5 is the max because 5 ∈ {x≤5} AND ∀a∈{x≤5}. a ≤ 5 --- the latter
// (the R/∋ division) decided by the counterexample set {x≤5} ∩ {x>5} collapsing
// to ∅ via the complement-pair meet.  So the specialisation is checked against
// the general law, not merely trusted (the Wadler free theorem, mechanised).
static_assert(max(le5)(5) ==
                  (le5(5) && (le5 & (ℕ | (π > 5))) == Ø<Cardinality>{}),
              "specific max(le5) models (∈) ∩ (R/∋) at the pivot.");
static_assert(min(ge5)(5) ==
                  (ge5(5) && (ge5 & (ℕ | (π < 5))) == Ø<Cardinality>{}),
              "specific min(ge5) models (∈) ∩ (R/∋) at the pivot.");

// Exhibit (finite case): max 𝔹 = {true}, min 𝔹 = {false} --- the SAME generic
// max/min above, its ∀-projection settled by 𝔹's upperbounds/lowerbounds ({⊤} /
// {⊥}) and the universe-identity meet 𝔹 ∩ {⊤} = {⊤}.
static_assert(max(𝔹)(true), "max 𝔹 = {true}.");
static_assert(!max(𝔹)(false), "false is not the greatest element of 𝔹.");
static_assert(min(𝔹)(false), "min 𝔹 = {false}.");
static_assert(!min(𝔹)(true), "true is not the least element of 𝔹.");
// Modelling witness ("Theorems for Free", type-checked) with a STRUCTURAL
// IsPredicate --- not an opaque lambda, which cannot feed the collapse.  The
// specific max(𝔹) models the abstract (∈) ∩ (R/∋): at false the dominance
// ∀a∈𝔹. a ≤ false FAILS (true ⋠ false), spelled as the halfspace {a ≤ false},
// so false is correctly NOT the max.
static_assert(max(𝔹)(false) == (𝔹(false) && forall(𝔹, π <= false)),
              "specific max(𝔹) models (∈) ∩ (R/∋), structurally.");

// (The image of a halfspace under a translation --- the pivot shifted by K ---
// is now the affine pushforward on the GRAPH surface: image(graph | π1 ⋈ p) =
// {y ⋈ p+K}, below.  The earlier Translation<T,K>-arrow version is superseded.)

// ── Relative product: composition of relations (Tarski) ────────────────────
// ComposePred and the Boolean-middle relative product operator>> moved DOWN to
// dedekind.relational:dyadic (#792).  The functional-propagation trait for
// ComposePred (is_right_unique_v, §3.2 Table 3) stays here in :halfspace's
// registry block below (it registers a dedekind::category trait), qualified as
// dedekind::sets::ComposePred; the >> witnesses reach the operator by ADL.

// ≤ ∘ ≤ = ≤ (transitivity), decidable because the intermediate is Boolean.
static_assert(static_cast<bool>(((𝔹 * 𝔹 | π1 <= π2) >>
                                 (𝔹 * 𝔹 | π1 <= π2))(std::pair{false, true})),
              "≤ ∘ ≤ contains (false, true).");
static_assert(!static_cast<bool>(((𝔹 * 𝔹 | π1 <= π2) >>
                                  (𝔹 * 𝔹 | π1 <= π2))(std::pair{true, false})),
              "≤ ∘ ≤ excludes (true, false): transitivity recovers ≤.");

// ── Relation algebra: union (the set-grammar |), the diagonal, and the
// reflexive / symmetric closures.  The relation | / operator& (union / meet),
// the
// diagonal Δ (a plain equality DiagPred), and the
// reflexive / symmetric closures moved DOWN to dedekind.relational:dyadic
// (#792) --- pure Set<pair> algebra.  The ordered witnesses below stay and
// reach them by ADL on their dedekind::sets::Set arguments.

// reflexive(<) = < ∪ Δ = ≤ on 𝔹; symmetric(<) = < ∪ <° = ≠ on 𝔹.
static_assert(static_cast<bool>(reflexive(𝔹* 𝔹 |
                                          π1 < π2)(std::pair{false, false})),
              "reflexive(<) adds the diagonal: (false,false) ∈ < ∪ Δ = ≤.");
static_assert(!static_cast<bool>(reflexive(𝔹 * 𝔹 |
                                           π1 < π2)(std::pair{true, false})),
              "reflexive(<) is still ≤: (true,false) ∉ ≤.");
static_assert(static_cast<bool>(symmetric(𝔹* 𝔹 |
                                          π1 < π2)(std::pair{true, false})),
              "symmetric(<) adds the converse: (true,false) ∈ < ∪ <° = ≠.");
static_assert(!static_cast<bool>(symmetric(𝔹 * 𝔹 |
                                           π1 < π2)(std::pair{false, false})),
              "symmetric(<) excludes the diagonal: (false,false) ∉ ≠.");

// The converse is the DAGGER of Rel: an INVOLUTION (R°° = R) that REVERSES
// composition ((R;S)° = S°;R°) --- the two laws that make it a dagger functor,
// and the reason a bijective relation's converse IS its inverse (Component A).
static_assert(static_cast<bool>(converse(converse(𝔹* 𝔹 | π1 < π2))(std::pair{
                  false, true})) ==
                  static_cast<bool>((𝔹 * 𝔹 | π1 < π2)(std::pair{false, true})),
              "R°° = R at (false,true): the converse is an involution.");
static_assert(static_cast<bool>(converse(converse(𝔹* 𝔹 | π1 < π2))(std::pair{
                  true, false})) ==
                  static_cast<bool>((𝔹 * 𝔹 | π1 < π2)(std::pair{true, false})),
              "R°° = R at (true,false): agrees on the excluded pair too.");
static_assert(
    static_cast<bool>(converse((𝔹 * 𝔹 | π1 < π2) >>
                               (𝔹 * 𝔹 | π1 <= π2))(std::pair{true, false})) ==
        static_cast<bool>((converse(𝔹 * 𝔹 | π1 <= π2) >>
                           converse(𝔹 * 𝔹 | π1 < π2))(std::pair{true, false})),
    "(R;S)° = S°;R° at (true,false): the converse reverses composition "
    "(dagger contravariance).");

// Kleene / relation-algebra laws on the DSL, witnessed on 𝔹: Δ is the
// composition unit (R;Δ = R, the algebra's 1); composition distributes over
// union (R;(S∪T) = R;S ∪ R;T); and --- the Schröder property-gated rewrite ---
// a FUNCTIONAL relation's composition distributes over MEET too (R;(S∩T) =
// R;S ∩ R;T), which fails for a non-functional relation.
static_assert(static_cast<bool>(((𝔹 * 𝔹 | π1 < π2) >>
                                 dedekind::relational::diag<bool>())(std::pair{
                  false, true})) ==
                  static_cast<bool>((𝔹 * 𝔹 | π1 < π2)(std::pair{false, true})),
              "R;Δ = R: the diagonal is the composition unit (the 1).");
static_assert(
    static_cast<bool>(((𝔹 * 𝔹 | π1 <= π2) >>
                       ((𝔹 * 𝔹 | π1 < π2) | (𝔹 * 𝔹 | π1 == π2)))(std::pair{
        false, true})) ==
        static_cast<bool>((((𝔹 * 𝔹 | π1 <= π2) >> (𝔹 * 𝔹 | π1 < π2)) |
                           ((𝔹 * 𝔹 | π1 <= π2) >>
                            (𝔹 * 𝔹 | π1 == π2)))(std::pair{false, true})),
    "R;(S∪T) = R;S ∪ R;T: composition distributes over union.");
static_assert(
    static_cast<bool>(((𝔹 * 𝔹 | π1 != π2) >>
                       ((𝔹 * 𝔹 | π1 <= π2) & (𝔹 * 𝔹 | π1 == π2)))(std::pair{
        true, false})) ==
        static_cast<bool>((((𝔹 * 𝔹 | π1 != π2) >> (𝔹 * 𝔹 | π1 <= π2)) &
                           ((𝔹 * 𝔹 | π1 != π2) >>
                            (𝔹 * 𝔹 | π1 == π2)))(std::pair{true, false})),
    "functional R ⟹ R;(S∩T) = R;S ∩ R;T: the property-gated Schröder rewrite.");

// ── FIXME(#786): the reflexive-TRANSITIVE closure (the Kleene star R*) is the
// remaining brick that turns §7's CPM exhibit into a one-liner (R* at a
// tropical semiring).  R* = Δ + R⁺ = Δ + R + R² + ... needs powers Rⁿ = the
// relative product iterated, but @c operator>> above is BOOLEAN-MIDDLE ONLY
// (requires std::same_as<B, bool>), so on 𝔹 the star degenerates to Δ + R (two
// nodes reach in ≤1 step) — no genuine iteration.  Two bricks, in order:
//   1. a FINITE-CARRIER relative product: enumerate a finite middle carrier
//      (the finite-quotient handle, §3.1) so Rⁿ is computable for >2 nodes, and
//      bound the star by the carrier size (Rⁿ stabilises at n = |carrier|).
//   2. a SEMIRING-PARAMETRIC >> and star: (R;S)(a,c) = ⊕_b R(a,b)⊗S(b,c), so
//      R* over (∨,∧) = reachability and over Tropical(max,+) = the critical
//      path (CPM) — the SAME closure, one semiring apart, re-expressing
//      showcase_13 through the DSL star and retiring the net/DSL duality.
// Brick 2's semiring choice + the carrier-enumeration protocol are design
// decisions best made with the author (not guessed); this seed lands the union
// (∪, the set-grammar |), the diagonal (1), and the reflexive/symmetric
// closures (half the Kleene algebra), leaving the star for the next pass.

}  // namespace dedekind::order

// ── Structural inference of the relation properties (Table 3) ───────────────
// The four properties are opt-in traits (trust at the leaves).  Here they are
// INFERRED for the DSL's OWN graph relations, so the concepts are no longer
// restrictive when structure decides.  is_right_unique_v (functional) and
// is_left_total_v (entire) are the pluggable points; IsFunction reads off them.
// This is Table 3's point-free reading realised: a LEAF carries the property by
// its shape, a NODE (a relative product @c >>) inherits it because the property
// composes.
namespace dedekind::category {

// The graph atoms are certified functional (and, where their term is total,
// entire) in sets:expressions.  This block carries restriction: functionality
// survives it, entireness is not certified through it (a restriction may drop
// the domain).

// RESTRICTION preserves single-valuedness (it removes pairs, never adds), so
// FUNCTIONALITY propagates through ProductRestrict: a restricted graph is still
// functional (a PARTIAL function).
template <typename A, typename B, typename L, typename P, typename RP,
          typename C>
inline constexpr bool is_right_unique_v<
    dedekind::sets::Comprehension<dedekind::sets::𝔸<std::pair<A, B>, L, C>,
                                  dedekind::order::ProductRestrict<P, RP>>> =
    is_right_unique_v<dedekind::sets::Comprehension<
        dedekind::sets::𝔸<std::pair<A, B>, L, C>, P>>;

// NODE (the compositional closure) for ComposePred's FUNCTIONALITY moved to
// dedekind.relational:dyadic (PR #797, Copilot review) --- it is
// structure-independent relation-algebra (R>>S functional iff both factors
// are), so it lives with ComposePred so a client importing only :relational
// sees it.

}  // namespace dedekind::category

namespace dedekind::order {
// A RESTRICTED translation graph is still FUNCTIONAL (a partial function),
// inferred through ProductRestrict from the underlying translation's
// certificate
// -- but NOT entire (the restriction drops the domain), so it is not a total
// function, matching the paper's partial-function reading.
static_assert(
    dedekind::relational::IsFunctional<decltype((ℤ * ℤ | π1 + 3 == π2) |
                                                π1 <= 5)> &&
        !dedekind::relational::IsEntire<decltype((ℤ * ℤ | π1 + 3 == π2) |
                                                 π1 <= 5)>,
    "a restricted translation graph is a functional-but-not-entire partial "
    "function, inferred through ProductRestrict.");
}  // namespace dedekind::order
