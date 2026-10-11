/**
 * @file dedekind/order/halfspace.cppm
 * @partition :halfspace
 * @brief Compile-time halfspace predicates on ordered carriers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section halfspace__Halfspaces_As_Types
 * Structured compile-time halfspace predicates over an ordered carrier. The
 * pivot is carried in the predicate's TYPE as a non-type template parameter,
 * which is what lets `(n > 5) && (n < 3)` collapse structurally
 * to an empty predicate at compile time. Contrast with the lambda-returning
 * scout operators in `dedekind.sets`, which erase the pivot into a closure.
 *
 * @section halfspace__DSL_Surface
 *
 *     inline constexpr auto n = element<𝔸<ℕ>>;
 *     inline constexpr auto big   = Set{n | (n > 5)};
 *     inline constexpr auto small = Set{n | (n < 3)};
 *     // (big ∩ small) = ∅  — witnessed at compile time via structured_and
 *
 * Wikipedia: Half-space (geometry), Separating hyperplane theorem,
 * Non-type template parameter
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
#include <stdexcept>   // std::domain_error (reduce_join: no normal form)
#include <type_traits>
#include <utility>

export module dedekind.order:halfspace;

import dedekind.category;
import dedekind.sets;
import dedekind.relational; // IsFunctional/IsEntire (:graph), used by the order
                            // relation witnesses below (#792)
import :poset;  // IsPartiallyOrdered — the SEMANTIC order certificate for
                // max/min
import :total;  // IsTotallyOrdered — the carrier gate of Halfspace / SetVal:
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

/** @brief A side of a cut: no bound, or a bound of strictness @c S.  Typed, so
 *  direction and strictness stay concept-visible (the order topology, @c ~ and
 *  @c upperbounds read them); only the bound values ride in the instance.
 *  @tparam S the strictness of the bound. */
export struct Unbounded {};
export template <Strictness S>
struct Bounded {
  static constexpr Strictness strictness = S;
};
/** @brief Whether @c Side is a bound.  @tparam Side a side type. */
export template <typename Side>
inline constexpr bool is_bounded_side_v = false;
export template <Strictness S>
inline constexpr bool is_bounded_side_v<Bounded<S>> = true;
/** @brief A side of a cut.  @tparam Side @c Unbounded or a @c Bounded. */
export template <typename Side>
concept IsSide = std::same_as<Side, Unbounded> || is_bounded_side_v<Side>;
/** @brief The complement's side: a bound flips its strictness.
 *  @tparam Side the side to flip. */
template <IsSide Side>
struct flipped_side {
  using type = Unbounded;
};
template <Strictness S>
struct flipped_side<Bounded<S>> {
  using type = Bounded<flip(S)>;
};
export template <IsSide Side>
using flipped_side_t = typename flipped_side<Side>::type;

/** @brief The cut, the datum of the order theory on a chain: a lower and an
 *  upper side in the type, the bound values @c lo / @c hi in the instance.  A
 *  ray has one side, an interval both; the point (@c lo == hi) and the empty
 *  set (crossed bounds) are value facts the reducer discharges; the universe
 *  (no side) is @c 𝔸.
 *  @tparam Lo the lower side.
 *  @tparam Hi the upper side.
 *  @tparam T the carrier, a registered chain. */
export template <IsSide Lo, IsSide Hi, IsTotallyOrdered T>
  requires(is_bounded_side_v<Lo> || is_bounded_side_v<Hi>)
struct Bounds {
  using Domain = T;
  using Codomain = bool;
  using cardinality_type = carrier_cardinality_t<T>;
  static constexpr bool has_lo = is_bounded_side_v<Lo>;
  static constexpr bool has_hi = is_bounded_side_v<Hi>;
  T lo{};
  T hi{};

  constexpr Bounds() = default;
  /** @brief A ray from its one bound; implicit and converting, so a literal of
   *  another type is one user-defined conversion, the carrier's own. */
  template <std::convertible_to<T> U>
    requires(has_lo != has_hi)
  constexpr Bounds(U&& bound) {  // NOLINT(google-explicit-constructor)
    if constexpr (has_lo)
      lo = static_cast<T>(std::forward<U>(bound));
    else
      hi = static_cast<T>(std::forward<U>(bound));
  }
  /** @brief An interval from both bounds. */
  constexpr Bounds(T lower, T upper)
    requires(has_lo && has_hi)
      : lo(lower), hi(upper) {}

  /** @brief The foreign carriers this datum admits: any @c U the order compares
   *  against @c T.  Read by the comprehension's heterogeneous χ. */
  template <typename U>
  static constexpr bool admits = !std::same_as<std::remove_cvref_t<U>, T> &&
                                 requires(const U& x, const T& p) {
                                   { x < p } -> std::convertible_to<bool>;
                                   { x > p } -> std::convertible_to<bool>;
                                   { x <= p } -> std::convertible_to<bool>;
                                   { x >= p } -> std::convertible_to<bool>;
                                 };

  /** @brief χ(x): the present sides' comparisons, conjoined. */
  template <typename U>
    requires std::same_as<std::remove_cvref_t<U>, T> || admits<U>
  constexpr bool operator()(const U& x) const {
    bool ok = true;
    if constexpr (has_lo)
      ok = ok && (Lo::strictness == Strictness::Strict ? (x > lo) : (x >= lo));
    if constexpr (has_hi)
      ok = ok && (Hi::strictness == Strictness::Strict ? (x < hi) : (x <= hi));
    return ok;
  }
};

/** @brief Which side a ray bounds, and that side's strictness. */
export template <IsSide Lo, IsSide Hi>
  requires(is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
consteval Direction direction_of() {
  return is_bounded_side_v<Lo> ? Direction::Upward : Direction::Downward;
}
export template <IsSide Lo, IsSide Hi>
  requires(is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
consteval Strictness ray_strictness() {
  if constexpr (is_bounded_side_v<Lo>)
    return Lo::strictness;
  else
    return Hi::strictness;
}

/** @brief The rays @c {x > p} (lower-bounded) and @c {x < p} (upper-bounded)
 *  as comprehensions of the datum over the universe of @c T.
 *  @tparam T the carrier, a registered chain.
 *  @tparam S the strictness of the bound.
 *  @tparam L the species the membership answer is valued in. */
export template <IsTotallyOrdered T, Strictness S, IsOckhamAlgebra L = Boole>
using UpRay = Comprehension<𝔸<T, L>, Bounds<Bounded<S>, Unbounded, T>>;
export template <IsTotallyOrdered T, Strictness S, IsOckhamAlgebra L = Boole>
using DownRay = Comprehension<𝔸<T, L>, Bounds<Unbounded, Bounded<S>, T>>;
/** @brief The ray at @c v by direction, for the sites that compute the
 *  direction (@c dir_of(R), a reflection).
 *  @tparam D the direction.
 *  @tparam S the strictness.
 *  @tparam L the species.
 *  @tparam T the carrier, a registered chain. */
export template <Direction D, Strictness S, IsOckhamAlgebra L = Boole,
                 IsTotallyOrdered T>
constexpr auto make_ray(T v) {
  if constexpr (D == Direction::Upward)
    return 𝔸<T, L>{} | Bounds<Bounded<S>, Unbounded, T>{std::move(v)};
  else
    return 𝔸<T, L>{} | Bounds<Unbounded, Bounded<S>, T>{std::move(v)};
}
/** @brief The interval @c {lo ⋈ x ⋈ hi}: both sides bounded.
 *  @tparam T the carrier, a registered chain.
 *  @tparam SL the strictness of the lower bound.
 *  @tparam SU the strictness of the upper bound.
 *  @tparam L the species. */
export template <IsTotallyOrdered T, Strictness SL, Strictness SU,
                 IsOckhamAlgebra L = Boole>
using Interval = Comprehension<𝔸<T, L>, Bounds<Bounded<SL>, Bounded<SU>, T>>;

/** @brief The bound of a ray.
 *  @tparam T the carrier.  @tparam L the species.  @tparam C the cardinality.
 *  @tparam Lo / @tparam Hi the sides, exactly one bounded. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
  requires(is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
constexpr T pivot(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& r) {
  if constexpr (is_bounded_side_v<Lo>)
    return r.predicate.lo;
  else
    return r.predicate.hi;
}

/** @section halfspace__Leaf_Surface
 *
 * The ray's own surface: the complement (a ray's complement is the opposite ray
 * at the same bound; an interval's is not a cut and stays the generic @c Not
 * node), the telling aliases.  Every other operation is a comprehension's.
 */

/** @brief A cut written with a literal of another type, @c ℕ @c | @c (π @c > @c
 * 5): the bound meets the carrier exactly, or the binding raises (a compile
 * error in @c constexpr code).  @tparam T the carrier.  @tparam K the bound's
 * type. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi, IsTotallyOrdered K>
  requires(!std::same_as<T, K>)
constexpr auto operator|(const 𝔸<T, L, C>& u, const Bounds<Lo, Hi, K>& b) {
  if constexpr (is_bounded_side_v<Lo> && is_bounded_side_v<Hi>)
    return u | Bounds<Lo, Hi, T>{carrier_cast<T>(b.lo), carrier_cast<T>(b.hi)};
  else if constexpr (is_bounded_side_v<Lo>)
    return u | Bounds<Lo, Hi, T>{carrier_cast<T>(b.lo)};
  else
    return u | Bounds<Lo, Hi, T>{carrier_cast<T>(b.hi)};
}

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

/** @brief An order atom @c π @c ⋈ @c k on a chain is the cut it names, whatever
 *  the constant's type: the constant meets the carrier exactly or the binding
 *  raises.  @tparam T the carrier.  @tparam R the order relation.
 *  @tparam K the constant's type. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C, Rel R,
                 typename K>
  requires(is_order_rel(R))
constexpr auto operator|(const 𝔸<T, L, C>& u,
                         const Cmp<R, Projection<0>, Const<K>>& c) {
  const T k = carrier_cast<T>(c.b.value);
  if constexpr (dir_of(R) == Direction::Upward)
    return u | Bounds<Bounded<strict_of(R)>, Unbounded, T>{k};
  else
    return u | Bounds<Unbounded, Bounded<strict_of(R)>, T>{k};
}

/** @brief Complement of a halfspace: the opposite halfspace with the SAME pivot
 *  value, flipped direction + strictness.  (value-carrying, so the
 *  boundary-collapse to Ø/𝔸 that the NTTP factory did at compile time is now a
 *  value-level concern of @c reduce_meet, not a type-level branch here.) */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
  requires(is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
constexpr auto operator~(
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& r) {
  using B = Bounds<flipped_side_t<Hi>, flipped_side_t<Lo>, T>;
  return 𝔸<T, L, C>{} | B{pivot(r)};
}

// The same-pivot complement-pair @c operator| / @c operator& (union → 𝔸,
// meet → Ø) are DELETED --- they gated on a shared NTTP pivot, which a value
// pivot cannot express at the type level.  The meet is subsumed by
// @c structured_and → @c reduce_meet (a strict complement pair reduces to
// empty, a closed one to the boundary point); the join by @c structured_or.

/** @brief Telling aliases for the two ℕ halfspaces the §3 listing uses
 *  (value-carrying: the pivot rides in the instance): @c Above<>{N} = {x>N},
 *  @c AtMost<>{N} = {x<=N}.  The alias fixes carrier / direction / strictness;
 *  @c Above<> is the type, @c Above<>{N} the value (was @c Above<N>{}). */
export template <IsOckhamAlgebra L = Boole>
using Above = UpRay<Cardinality, Strictness::Strict, L>;
export template <IsOckhamAlgebra L = Boole>
using AtMost = DownRay<Cardinality, Strictness::NonStrict, L>;

// A bare Halfspace / Singleton is a first-class @c IsSubobject (ι: S ↣ A plus
// its own χ), though NOT a full ETCS @c IsSet: @c IsSet additionally demands
// the ETCS-axiom surface (@c HasETCSAxioms + the CCC witness) that only the
// ambient universe @c 𝔸<T> carries.  Subobject-hood is the right membership —
// it is what the complement-lattice operators above operate on.
static_assert(IsSubobject<Above<>, dedekind::sets::Cardinality>,
              "a Halfspace is a first-class subobject ι: S ↣ ℕ.");
static_assert(IsSubobject<Singleton<bool>, bool>,
              "a static Singleton is a first-class subobject.");
// ... and both are set OBJECTS in the (reified universe, χ) sense: a structured
// normal form over its ambient, χ into the named logic species.
static_assert(dedekind::sets::IsSetObject<Above<>>,
              "a Halfspace is a set object: (universe ℕ, χ = x > p).");
static_assert(dedekind::sets::IsSetObject<Singleton<bool>>,
              "a Singleton is a set object: (universe 𝔹, χ = x == v).");

/** @brief The interval from its two endpoints.
 *  @tparam SL the strictness of the lower bound.
 *  @tparam SU the strictness of the upper bound.
 *  @tparam L the species.
 *  @tparam T the carrier, a registered chain. */
export template <Strictness SL, Strictness SU, IsOckhamAlgebra L = Boole,
                 IsTotallyOrdered T>
constexpr Interval<T, SL, SU, L> make_interval(T lo, T hi) {
  return 𝔸<T, L>{} | Bounds<Bounded<SL>, Bounded<SU>, T>{lo, hi};
}

/** @brief The endpoints of an interval. */
export template <IsTotallyOrdered T, Strictness SL, Strictness SU,
                 IsOckhamAlgebra L>
constexpr T lower_pivot(const Interval<T, SL, SU, L>& iv) {
  return iv.predicate.lo;
}
export template <IsTotallyOrdered T, Strictness SL, Strictness SU,
                 IsOckhamAlgebra L>
constexpr T upper_pivot(const Interval<T, SL, SU, L>& iv) {
  return iv.predicate.hi;
}

/** @brief Effective integer bounds on a built-in integral carrier: the
 *  tightest admitted integers after the strictness offset (an integral endpoint
 *  carries no fractional part to floor / ceil).  Decided on the values, so the
 *  same normalisation serves emptiness, cardinality and the subset test, and
 *  distinct endpoints denoting the same set agree (@c (1,4) and @c [2,3] over
 *  @c int).  The variant ℕ-/ℤ-proxies await the heterogeneous-pivot
 *  restoration (#970). */
export template <std::integral T, Strictness SL, Strictness SU, typename L>
constexpr long long eff_lower(const Interval<T, SL, SU, L>& iv) {
  const auto lo = static_cast<long long>(lower_pivot(iv));
  return SL == Strictness::Strict ? lo + 1 : lo;
}
export template <std::integral T, Strictness SL, Strictness SU, typename L>
constexpr long long eff_upper(const Interval<T, SL, SU, L>& iv) {
  const auto hi = static_cast<long long>(upper_pivot(iv));
  return SU == Strictness::Strict ? hi - 1 : hi;
}

/** @brief Whether the interval denotes the empty set (χ ≡ False).  A built-in
 *  integral carrier decides on the effective bounds, so the open gap @c (5,6)
 *  is empty; any other ordered carrier on endpoint degeneracy (@c hi<lo, or
 *  @c lo==hi with an open end --- @c [5,5] is the singleton).  Empty intervals
 *  are representable, so @c :inclusion recognises @f$∅ ⊆ X@f$. */
export template <IsTotallyOrdered T, Strictness SL, Strictness SU,
                 IsOckhamAlgebra L>
constexpr bool is_empty(const Interval<T, SL, SU, L>& iv) {
  if constexpr (std::integral<T>) {
    return eff_lower(iv) > eff_upper(iv);
  } else {
    const T lo = lower_pivot(iv);
    const T hi = upper_pivot(iv);
    return hi < lo ||
           (lo == hi && (SL == Strictness::Strict || SU == Strictness::Strict));
  }
}

/** @brief The finite cardinality of an interval on a built-in integral
 *  carrier (0 when empty). */
export template <std::integral T, Strictness SL, Strictness SU, typename L>
constexpr std::size_t size(const Interval<T, SL, SU, L>& iv) {
  const long long lo = eff_lower(iv);
  const long long hi = eff_upper(iv);
  return hi < lo ? 0u : static_cast<std::size_t>(hi - lo + 1);
}

/** @section halfspace__Value_First_Meet — the ONE meet law, value-first.
 *
 *  @details The meet is written ONCE here, over a @c SetVal whose pivot is a
 *  @b value.  Being @c constexpr it serves BOTH phases (the
 * compile-time/runtime optionality): the type-level @c structured_and below @b
 * delegates to it (it lifts its NTTP pivots to values, calls @c reduce_meet,
 * and lifts the result kind back to @c Singleton / @c Interval / @c
 * EmptyPredicate), and the Python @c dedekind.lwv surface calls the @b same
 * function on runtime pivots. No NTTP is read off at runtime; the pivot never
 * needs to live in a type.
 *
 *  Value-oriented relational grounding: a halfspace is a point plus a
 * direction,
 *  @f$\{x \mid x \bowtie p\} = \pi_1(\,S \times \eta(p) \mid \pi_1 \bowtie
 *  \pi_2\,)@f$, the pivot in the value @f$\eta(p)@f$ (a @c Singleton); the
 *  singleton is the degenerate point @c SetVal::point.  House spelling @c χ /
 *  @c π₁,π₂. */
export enum class SetKind { Empty, Universe, Halfspace, Singleton, Interval };

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

/** @brief A set as a VALUE: kind + pivot(s) + direction/strictness, with a
 *  runtime-evaluable membership χ.  The pivot rides in @c lo (Halfspace /
 *  Singleton) or @c lo/@c hi (Interval), as a value --- never an NTTP.
 *
 *  @note PROVISIONAL REPRESENTATION (FIXME #970): the kind-flag + shared-fields
 *  layout is runtime type-dispatch by flag, a first-iteration stand-in for a
 *  proper coproduct (@c std::variant / @c IsCoproduct, tied to the variance
 *  architecture).  Consume sets through the stable INTERFACE --- @c operator&,
 *  @c operator|, @c operator==, @c operator<=, membership @c operator() --- and
 *  through the @c half / @c point / @c empty / @c universe factories; do @b not
 *  build downstream on the @c kind / @c lo / @c hi fields, which will change.
 *
 *  The carrier is gated on the registered chain (@c IsTotallyOrdered) like
 *  @c Halfspace: the interval arms are a normal form only on a chain.
 */
export template <typename V = long long, typename L = Boole>
  requires IsTotallyOrdered<V>
struct SetVal : dedekind::sets::SetExpr<SetVal<V, L>, V, L> {
  using Domain = V;
  SetKind kind = SetKind::Universe;
  V lo{};
  V hi{};
  Direction dir = Direction::Upward;
  Strictness sl = Strictness::NonStrict;
  Strictness su = Strictness::NonStrict;

  /** @brief χ(x): runtime membership, decided from the value fields.
   *
   *  @details The two DECIDED arms (⊥ / ⊤) need no carrier comparison, so they
   *  are answered in this small front and the carrier comparisons live in
   *  @c contains_bounded behind it.  The split is load-bearing for the IR
   *  exhibits: on a variant carrier (ℕ = @c Cardinality) the comparisons are
   *  heavy enough that the inliner declines a single fat function, and a
   *  constant @c SetVal then stays a call instead of folding to @c ret @c i1
   *  @c false.  A small front inlines, the load of @c kind from the constant
   *  folds, and the bounded call is dead code. */
  constexpr bool contains(const V& x) const {
    if (kind == SetKind::Empty) return false;
    if (kind == SetKind::Universe) return true;
    return contains_bounded(x);
  }

  /** @brief The bounded arms of χ: point, halfspace, interval --- the arms that
   *  compare against the carrier. */
  constexpr bool contains_bounded(const V& x) const {
    switch (kind) {
      case SetKind::Empty:
        return false;
      case SetKind::Universe:
        return true;
      case SetKind::Singleton:
        return x == lo;
      case SetKind::Halfspace:
        return dir == Direction::Upward
                   ? (sl == Strictness::Strict ? x > lo : x >= lo)
                   : (sl == Strictness::Strict ? x < lo : x <= lo);
      case SetKind::Interval: {
        const bool lo_ok = (sl == Strictness::Strict) ? x > lo : x >= lo;
        const bool hi_ok = (su == Strictness::Strict) ? x < hi : x <= hi;
        return lo_ok && hi_ok;
      }
    }
    return false;
  }

  /** @brief ETCS χ: @c contains lifted to the logic species @c L. */
  constexpr typename L::Ω operator()(const V& x) const {
    return contains(x) ? L::True : L::False;
  }

  static constexpr SetVal half(V pivot, Direction d, Strictness s) {
    return {{}, SetKind::Halfspace, pivot, V{}, d, s, Strictness::NonStrict};
  }
  static constexpr SetVal point(V pivot) {
    return {{},
            SetKind::Singleton,
            pivot,
            pivot,
            Direction::Upward,
            Strictness::NonStrict,
            Strictness::NonStrict};
  }
  /** @brief The two boundary sets as values --- the meet's annihilator (@c Ø)
   *  and unit (@c 𝔸).  Factories (not raw aggregate init) so callers never
   * spell the empty-base brace of the @c SetExpr aggregate. */
  static constexpr SetVal empty() { return {{}, SetKind::Empty}; }
  static constexpr SetVal universe() { return {{}, SetKind::Universe}; }
  /** @brief The bounded meet @c [lo, hi] with its strictness pair, collapsed:
   *  empty if the bounds cross, the single point on a built-in integral carrier
   *  admitting exactly one integer, else the interval.  The ONE collapse law
   *  the crossing-halfspace meet and the interval meet both ride.  The point
   *  collapse gates on the NNO steps (@c HasNNOStep: successor / predecessor,
   *  an axiom of the category), so the ℕ proxy @c Cardinality collapses to its
   *  point exactly as a machine integer does. */
  static constexpr SetVal bounded(V lo, Strictness sl, V hi, Strictness su) {
    const bool either_strict =
        sl == Strictness::Strict || su == Strictness::Strict;
    if (either_strict ? (lo >= hi) : (lo > hi)) return empty();
    // The point collapse gates on the NNO's successor / predecessor (an axiom
    // of the category), not on std::integral: the ℕ proxy Cardinality steps
    // too.  With lo < hi already established, pred(hi) is defined (hi ≥ 1).
    if constexpr (HasNNOStep<V>) {
      const V el = sl == Strictness::Strict ? successor(lo) : lo;
      const V eu = su == Strictness::Strict ? predecessor(hi) : hi;
      if (el > eu) return empty();
      if (el == eu) return point(el);
    }
    return {{}, SetKind::Interval, lo, hi, Direction::Upward, sl, su};
  }
};

// The value set is a set OBJECT --- (reified universe, χ) --- so whatever the
// Python surface exports (it exports SetVal) conforms to IsSetObject by
// construction.  A MUST for the exported type, pinned here at its definition.
static_assert(
    dedekind::sets::IsSetObject<SetVal<int>>,
    "SetVal is a set object: the value normal form over its universe.");
static_assert(dedekind::sets::IsSetObject<SetVal<long long>>,
              "the Python surface's Set (SetVal<long long>) is a set object.");

/** @brief The ONE meet law: @c ↑a∩↑b=↑(a∨b) / @c ↓a∩↓b=↓(a∧b) (same direction,
 *  the tighter pivot wins) and @c ↑a∩↓b=[a,b] (crossing), the interval
 *  collapsing by integer cardinality to a @c point or empty.  @c constexpr, so
 *  evaluable at compile time or runtime. */
export template <typename V, typename L>
constexpr SetVal<V, L> reduce_meet(const SetVal<V, L>& a,
                                   const SetVal<V, L>& b) {
  using K = SetKind;
  using S = SetVal<V, L>;
  if (a.kind == K::Empty || b.kind == K::Empty) return {{}, K::Empty};
  if (a.kind == K::Universe) return b;
  if (b.kind == K::Universe) return a;
  // A singleton meet is the point iff it lies in the other set (discharged ∃).
  if (a.kind == K::Singleton) return b.contains(a.lo) ? a : S{{}, K::Empty};
  if (b.kind == K::Singleton) return a.contains(b.lo) ? b : S{{}, K::Empty};
  if (a.kind == K::Halfspace && b.kind == K::Halfspace) {
    if (a.dir == b.dir) {
      const bool up = a.dir == Direction::Upward;
      if (a.lo == b.lo)
        return S::half(
            a.lo, a.dir,
            (a.sl == Strictness::Strict || b.sl == Strictness::Strict)
                ? Strictness::Strict
                : Strictness::NonStrict);
      const bool a_wins = up ? (a.lo > b.lo) : (a.lo < b.lo);
      return a_wins ? a : b;
    }
    // Crossing ↑lo ∩ ↓hi = [lo, hi]; normalise lower = Upward, upper =
    // Downward.
    const S lower = (a.dir == Direction::Upward) ? a : b;
    const S upper = (a.dir == Direction::Upward) ? b : a;
    const V lo = lower.lo;
    const V hi = upper.lo;
    const Strictness sl = lower.sl;
    const Strictness su = upper.sl;
    return S::bounded(lo, sl, hi, su);
  }
  // An Interval operand (against an Interval or a halfspace): each operand
  // contributes a lower bound iff it is an Interval or an Upward halfspace, and
  // an upper bound iff an Interval or a Downward halfspace (a halfspace keeps
  // its pivot in @c lo whatever its direction).  The tighter lower / tighter
  // upper wins, the strictest strictness at a tie; the bounded result then
  // collapses through the one @c bounded law.  One operand being an Interval,
  // both sides are always present.
  if (a.kind == K::Interval || b.kind == K::Interval) {
    const bool a_iv = a.kind == K::Interval;
    const bool b_iv = b.kind == K::Interval;
    const bool a_has_lo = a_iv || a.dir == Direction::Upward;
    const bool a_has_hi = a_iv || a.dir == Direction::Downward;
    const bool b_has_lo = b_iv || b.dir == Direction::Upward;
    const bool b_has_hi = b_iv || b.dir == Direction::Downward;
    const V a_hi = a_iv ? a.hi : a.lo;
    const V b_hi = b_iv ? b.hi : b.lo;
    const Strictness a_su = a_iv ? a.su : a.sl;
    const Strictness b_su = b_iv ? b.su : b.sl;
    V lo = a_has_lo ? a.lo : b.lo;
    Strictness sl = a_has_lo ? a.sl : b.sl;
    if (a_has_lo && b_has_lo) {
      if (b.lo > a.lo) {
        lo = b.lo;
        sl = b.sl;
      } else if (a.lo == b.lo) {
        sl = (a.sl == Strictness::Strict || b.sl == Strictness::Strict)
                 ? Strictness::Strict
                 : Strictness::NonStrict;
      }
    }
    V hi = a_has_hi ? a_hi : b_hi;
    Strictness su = a_has_hi ? a_su : b_su;
    if (a_has_hi && b_has_hi) {
      if (b_hi < a_hi) {
        hi = b_hi;
        su = b_su;
      } else if (a_hi == b_hi) {
        su = (a_su == Strictness::Strict || b_su == Strictness::Strict)
                 ? Strictness::Strict
                 : Strictness::NonStrict;
      }
    }
    return S::bounded(lo, sl, hi, su);
  }
  return {{}, K::Universe};
}

/** @brief A cut as a @c SetVal: a ray to the @c Halfspace kind, an interval to
 *  the raw @c Interval kind that @c reduce_meet normalises. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
constexpr SetVal<T, L> to_setval(
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& c) {
  if constexpr (is_bounded_side_v<Lo> && is_bounded_side_v<Hi>)
    return {{},
            SetKind::Interval,
            c.predicate.lo,
            c.predicate.hi,
            Direction::Upward,
            Lo::strictness,
            Hi::strictness};
  else if constexpr (is_bounded_side_v<Lo>)
    return SetVal<T, L>::half(c.predicate.lo, Direction::Upward,
                              Lo::strictness);
  else
    return SetVal<T, L>::half(c.predicate.hi, Direction::Downward,
                              Hi::strictness);
}

/** @brief Structural equality of two value sets: same kind and the fields that
 *  kind uses.  The value-first counterpart of the old type-level set equality;
 *  it is what the subset identity @f$A \subseteq B \iff (A \cap B) = A@f$ rides
 *  on now that the meet returns a @c SetVal.
 *
 *  @details The verdict is returned in the ambient logic @c L::Ω (so it
 * composes with the @c L::Ω subset ladder and stays Kleene under a Kleene
 * ambient rather than silently narrowing a @c bool).  The comparison is gated
 * on @c V having decidable equality (@c std::equality_comparable): an
 * undecidable carrier --- where value equality is not answerable --- is ruled
 * out at the type level, so the operator is simply absent rather than vouching
 * for a verdict it cannot compute.
 *
 *  On a carrier with the NNO step the comparison is @b extensional: a strict
 *  bound at @c a is the non-strict bound at @c S(a) (above: @c P(a)), so
 *  @c (0, 3) and @c [1, 3) are the same set of ℤ and compare equal, by their
 *  closed bounds.  On a dense carrier they are different sets, and the
 *  comparison stays structural. */
namespace detail_setval_eq {
/** @brief Close a strict lower bound at the next element, where the step moves;
 *  at a fixpoint of @c S (a saturating end: ⊤ on K₃, ±ℵ₀) there is no next
 *  element to close at and the bound stays strict --- @c {x > ⊤} is not
 *  @c {x ≥ ⊤}.  @tparam V the carrier. */
template <HasCoveringStep V>
constexpr void close_lower(V& v, Strictness& s) {
  if (s != Strictness::Strict) return;
  const V next = successor(v);
  if (next == v) return;
  v = next;
  s = Strictness::NonStrict;
}
/** @brief The dual: close a strict upper bound at the previous element where
 *  @c P moves; @c {x < ℵ₀}, the finite fragment, is not @c {x ≤ ℵ₀}.
 *  @tparam V the carrier. */
template <HasCoveringStep V>
constexpr void close_upper(V& v, Strictness& s) {
  if (s != Strictness::Strict) return;
  const V prev = predecessor(v);
  if (prev == v) return;
  v = prev;
  s = Strictness::NonStrict;
}
/** @brief The form compared: on a discrete chain every strict bound that can
 *  close is closed, so @c (0, 3) and @c [1, 3) coincide; on a dense carrier
 *  every bound stays as written.  @tparam V the carrier.  @tparam L the logic.
 */
template <typename V, typename L>
constexpr SetVal<V, L> canonical(SetVal<V, L> s) {
  if constexpr (HasCoveringStep<V>) {
    switch (s.kind) {
      case SetKind::Halfspace:
        if (s.dir == Direction::Upward)
          close_lower(s.lo, s.sl);
        else
          close_upper(s.lo, s.sl);
        break;
      case SetKind::Interval:
        close_lower(s.lo, s.sl);
        close_upper(s.hi, s.su);
        break;
      default:
        break;
    }
  }
  return s;
}
}  // namespace detail_setval_eq
export template <typename V, typename L>
  requires std::equality_comparable<V>
constexpr typename L::Ω operator==(const SetVal<V, L>& a,
                                   const SetVal<V, L>& b) {
  const SetVal<V, L> x = detail_setval_eq::canonical(a);
  const SetVal<V, L> y = detail_setval_eq::canonical(b);
  if (x.kind != y.kind) return L::False;
  bool eq = true;
  switch (x.kind) {
    case SetKind::Empty:
    case SetKind::Universe:
      break;  // kind alone decides
    case SetKind::Singleton:
      eq = x.lo == y.lo;
      break;
    case SetKind::Halfspace:
      eq = x.dir == y.dir && x.lo == y.lo && x.sl == y.sl;
      break;
    case SetKind::Interval:
      eq = x.lo == y.lo && x.hi == y.hi && x.sl == y.sl && x.su == y.su;
      break;
  }
  return eq ? L::True : L::False;
}
namespace detail_setval_eq {
using dedekind::category::Ternary;
using dedekind::sets::Cardinality;
using dedekind::sets::ℵ_0;
static_assert(
    SetVal<int>::bounded(0, Strictness::Strict, 3, Strictness::Strict) ==
        SetVal<int>::bounded(1, Strictness::NonStrict, 3, Strictness::Strict),
    "(0, 3) = [1, 3) on ℤ: the strict bound closes at the next element.");
static_assert(
    !(SetVal<Ternary>::half(Ternary::True, Direction::Upward,
                            Strictness::Strict) ==
      SetVal<Ternary>::half(Ternary::True, Direction::Upward,
                            Strictness::NonStrict)),
    "{x > ⊤} is not {x ≥ ⊤}: at a saturating end a strict bound has no closed "
    "form.");
static_assert(
    !(SetVal<Cardinality>::half(Cardinality{ℵ_0{}}, Direction::Downward,
                                Strictness::Strict) ==
      SetVal<Cardinality>::half(Cardinality{ℵ_0{}}, Direction::Downward,
                                Strictness::NonStrict)),
    "{x < ℵ₀}, the finite fragment, is not {x ≤ ℵ₀} = 𝔸.");
}  // namespace detail_setval_eq

/** @brief Bridge: compare a value set to a halfspace by the halfspace's own
 *  @c SetVal form, so the generic meet-subset identity accepts a bare
 *  @c Halfspace operand (@c (a & b) == a with @c a a halfspace). */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
  requires std::equality_comparable<T>
constexpr typename L::Ω operator==(
    const SetVal<T, L>& s,
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h) {
  return s == to_setval(h);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
  requires std::equality_comparable<T>
constexpr typename L::Ω operator==(
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h,
    const SetVal<T, L>& s) {
  return s == to_setval(h);
}

/** @section halfspace__Halfspace_Structural_Algebra — ADL hooks for operator&&.
 */

/** @brief The halfspace meet: ONE overload, delegating to the one @c
 * reduce_meet on the pivot VALUES.  Value-carrying pivots mean the
 * result KIND (empty / point / interval / halfspace) depends on runtime values,
 * so a function cannot pick a distinct return TYPE --- the meet returns the
 * unified value @c SetVal (kind-tagged), and the type-directed collapse becomes
 *  value-directed (a @c constexpr @c SetVal still folds at compile time). */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C1,
                 IsCardinality C2, IsSide Lo1, IsSide Hi1, IsSide Lo2,
                 IsSide Hi2>
constexpr auto structured_and(
    const Comprehension<𝔸<T, L, C1>, Bounds<Lo1, Hi1, T>>& a,
    const Comprehension<𝔸<T, L, C2>, Bounds<Lo2, Hi2, T>>& b) {
  return reduce_meet(to_setval(a), to_setval(b));
}
/** @brief The meet of a cut with a value leaf, and of two value leaves: the one
 *  @c reduce_meet.  Declared here, before @c axis_factor, so the recursion
 *  over a @c RelAnd tree is closed over its own outputs whatever the
 *  association (#872). */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
constexpr SetVal<T, L> structured_and(
    const SetVal<T, L>& s,
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h) {
  return reduce_meet(s, to_setval(h));
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
constexpr SetVal<T, L> structured_and(
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h,
    const SetVal<T, L>& s) {
  return reduce_meet(to_setval(h), s);
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr SetVal<V, L> structured_and(const SetVal<V, L>& a,
                                      const SetVal<V, L>& b) {
  return reduce_meet(a, b);
}

/** @section halfspace__Halfspace_Structural_Join — @c structured_or, the JOIN
 *  (∪) dual of @c structured_and: it makes the union COLLAPSE symmetrically to
 *  the meet, so the join is no longer a declared-but-unimplemented hook.
 *  Same-direction rays union to the WEAKER bound (the smaller pivot up /
 *  larger pivot down, non-strict winning at an equal pivot).  A crossing union
 *  (a cover or a gap) has no cut normal form, so no overload matches and @c |
 *  falls to the reducer's honest @c Join node, decided by membership.  This is
 *  why @c image(abs) = @c image(x↦x on x≥0) ∪
 *  @c image(x↦−x on x<0) = @c {y≥0} ∪ @c {y>0} collapses to @c {y≥0}. */

/** @brief The value-first same-direction JOIN law: ↑a∪↑b = ↑min(a,b),
 *  ↓a∪↓b = ↓max(a,b) --- the WEAKER bound wins (non-strict on a tie).  Only the
 *  same-direction union always collapses to a halfspace; a crossing union
 *  either covers the line or leaves a gap (no @c SetVal kind), so it is left to
 *  the generic point-wise @c operator|| (value-carrying can't dispatch
 *  cover-vs-gap on a runtime pivot). */
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr SetVal<V, L> reduce_join(const SetVal<V, L>& a,
                                   const SetVal<V, L>& b) {
  using K = SetKind;
  using S = SetVal<V, L>;
  if (a.kind == K::Empty) return b;
  if (b.kind == K::Empty) return a;
  if (a.kind == K::Universe || b.kind == K::Universe) return S::universe();
  // A point already inside the other set adds nothing.
  if (a.kind == K::Singleton && b.contains(a.lo)) return b;
  if (b.kind == K::Singleton && a.contains(b.lo)) return a;
  if (a.kind == K::Halfspace && b.kind == K::Halfspace && a.dir == b.dir) {
    const bool up = a.dir == Direction::Upward;
    if (a.lo == b.lo)
      return S::half(
          a.lo, a.dir,
          (a.sl == Strictness::NonStrict || b.sl == Strictness::NonStrict)
              ? Strictness::NonStrict
              : Strictness::Strict);
    const bool a_wins = up ? (a.lo < b.lo) : (a.lo > b.lo);  // weaker bound
    return a_wins ? a : b;
  }
  // No interval normal form (a crossing union, a point outside, two
  // intervals): refused, not guessed.
  throw std::domain_error("reduce_join: the union has no interval normal form");
}

/** @brief Same-direction halfspace union, through the one @c reduce_join. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C1,
                 IsCardinality C2, IsSide Lo1, IsSide Hi1, IsSide Lo2,
                 IsSide Hi2>
  requires(is_bounded_side_v<Lo1> != is_bounded_side_v<Hi1>) &&
          (is_bounded_side_v<Lo1> == is_bounded_side_v<Lo2>) &&
          (is_bounded_side_v<Hi1> == is_bounded_side_v<Hi2>)
constexpr auto structured_or(
    const Comprehension<𝔸<T, L, C1>, Bounds<Lo1, Hi1, T>>& a,
    const Comprehension<𝔸<T, L, C2>, Bounds<Lo2, Hi2, T>>& b) {
  return reduce_join(to_setval(a), to_setval(b));
}

/** @section halfspace__Interval_Cartesian_Product — 2D structural products. */

/** @brief @c π @c ⋈ @c v with a carrier @b value: the @c Bounds datum, which
 *  the set former binds (@c 𝔸<Q>{} | (π < p) is @c {q < p}).  A degenerate cut
 * is a value fact, decided by the value laws.  Declared beside @c π so ADL
 * finds them through it.
 *  @tparam T the carrier, a registered chain. */
}  // namespace dedekind::order
namespace dedekind::sets {
using dedekind::order::Bounded;
using dedekind::order::Bounds;
using dedekind::order::IsTotallyOrdered;
using dedekind::order::Strictness;
using dedekind::order::Unbounded;
export template <IsTotallyOrdered T>
constexpr Bounds<Bounded<Strictness::Strict>, Unbounded, T> operator>(
    Projection<0>, T v) {
  return {std::move(v)};
}
export template <IsTotallyOrdered T>
constexpr Bounds<Bounded<Strictness::NonStrict>, Unbounded, T> operator>=(
    Projection<0>, T v) {
  return {std::move(v)};
}
export template <IsTotallyOrdered T>
constexpr Bounds<Unbounded, Bounded<Strictness::Strict>, T> operator<(
    Projection<0>, T v) {
  return {std::move(v)};
}
export template <IsTotallyOrdered T>
constexpr Bounds<Unbounded, Bounded<Strictness::NonStrict>, T> operator<=(
    Projection<0>, T v) {
  return {std::move(v)};
}
}  // namespace dedekind::sets
namespace dedekind::order {

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

/** @brief @f$\pi_I^{-1}@f$ of a halfspace factor: the cylinder
 *  @f$\pi_I \bowtie \text{pivot}@f$ on the product, an atom of the grammar. */
export template <IsRingIntegral auto I, IsTotallyOrdered T, IsOckhamAlgebra L,
                 IsCardinality C, IsSide Lo, IsSide Hi>
  requires(is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
constexpr auto cylinder(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h) {
  return Cmp<rel_of(direction_of<Lo, Hi>(), ray_strictness<Lo, Hi>()),
             Projection<I>, Const<T>>{{}, {pivot(h)}};
}

// restricted × total:  {x ⋈ p} × 𝔸  =  𝔸<pair> | (π1 ⋈ p).
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi, typename T2, IsOckhamAlgebra L2,
                 IsCardinality C2>
  requires std::same_as<L, L2> &&
           (is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
constexpr auto operator*(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& a,
                         const 𝔸<T2, L2, C2>&) {
  return 𝔸<std::pair<T, T2>, L>{} | cylinder<1>(a);
}

// total × restricted:  𝔸 × {y ⋈ q}  =  𝔸<pair> | (π2 ⋈ q).
export template <typename T1, IsOckhamAlgebra L1, IsCardinality C1,
                 IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
  requires std::same_as<L1, L> &&
           (is_bounded_side_v<Lo> != is_bounded_side_v<Hi>)
constexpr auto operator*(
    const 𝔸<T1, L1, C1>&,
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& b) {
  return 𝔸<std::pair<T1, T>, L>{} | cylinder<2>(b);
}

// restricted × restricted:  𝔸<pair> | (π1 ⋈ p) && (π2 ⋈ q).
export template <IsTotallyOrdered Ta, IsOckhamAlgebra La, IsCardinality Ca,
                 IsSide LoA, IsSide HiA, IsTotallyOrdered Tb,
                 IsOckhamAlgebra Lb, IsCardinality Cb, IsSide LoB, IsSide HiB>
  requires std::same_as<La, Lb> &&
           (is_bounded_side_v<LoA> != is_bounded_side_v<HiA>) &&
           (is_bounded_side_v<LoB> != is_bounded_side_v<HiB>)
constexpr auto operator*(
    const Comprehension<𝔸<Ta, La, Ca>, Bounds<LoA, HiA, Ta>>& a,
    const Comprehension<𝔸<Tb, Lb, Cb>, Bounds<LoB, HiB, Tb>>& b) {
  return 𝔸<std::pair<Ta, Tb>, La>{} | (cylinder<1>(a) && cylinder<2>(b));
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

/** @brief A cylinder on axis @c I, @c π_I @c ⋈ @c c: the halfspace it lifted
 *  from (only an order comparison is a halfspace; Eq/Ne fall to the default).
 *  The slot is matched to the axis by value, so two integral spellings of one
 *  axis agree. */
export template <IsRingIntegral auto I, IsTotallyOrdered TI, IsOckhamAlgebra L,
                 IsRingIntegral auto Slot, Rel R, typename K>
  requires(is_order_rel(R) && Slot == I)
constexpr auto axis_factor(const Cmp<R, Projection<Slot>, Const<K>>& c) {
  return make_ray<dir_of(R), strict_of(R), L>(carrier_cast<TI>(c.b.value));
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
    // cut or, from a deeper meet, a SetVal), through structured_and directly
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

// divides: {(a,b) | b % a == 0 ∧ a != 0}.  The guard comes FIRST so the
// conjunction short-circuits before the %, and a == 0 never reaches it.
static_assert((ℕ * ℕ | (π1 != 0 && π2 % π1 == 0))(std::pair{
                  finite_cardinality(2), finite_cardinality(6)}) &&
                  !(ℕ * ℕ | (π1 != 0 && π2 % π1 == 0))(std::pair{
                      finite_cardinality(4), finite_cardinality(6)}),
              "6 % 2 == 0, 6 % 4 != 0: divides, with its guard first.");
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
static_assert(dom(ℕ* ℕ | ((π1 >= 1 && π1 <= 5) && π1 >= 2)).kind ==
                  SetKind::Interval,
              "π_A of {1 ≤ a ≤ 5 ∧ a ≥ 2} is the interval [2, 5].");
static_assert(dom(ℕ* ℕ | ((π1 >= 1 && π1 <= 5) && π1 >= 2)).lo ==
                      finite_cardinality(2) &&
                  dom(ℕ * ℕ | (π1 >= 1 && (π1 <= 5 && π1 >= 2))).hi ==
                      finite_cardinality(5),
              "…with the tighter lower bound 2 and the upper bound 5.");
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
/** @brief Upper bounds of a downward halfspace {x ⋈ p} = {x ≥ sup}
 *  (value-carrying, returns SetVal).  Discrete strict: sup = predecessor(p)
 *  (attained), so the meet {x<p} ∩ {x≥p−1} = {p−1}; else sup = p (dense strict
 *  → the meet is empty, no max).  (The machine-boundary empty-cut → 𝔸 edge is
 *  simplified out; FIXME(#970) restore it with the coproduct arms.) */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, Strictness SU>
constexpr SetVal<T, L> upperbounds(
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Bounded<SU>, T>>& c) {
  T sup = c.predicate.hi;
  // Discrete carriers attain the strict bound at the predecessor --- gated on
  // the NNO's step (an axiom of the category, which ℕ's proxy witnesses), not
  // on std::integral.
  if constexpr (HasCoveringStep<T>)
    if (SU == Strictness::Strict) sup = predecessor(c.predicate.hi);
  return SetVal<T, L>::half(sup, Direction::Upward, Strictness::NonStrict);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo>
constexpr SetVal<T, L> upperbounds(
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Unbounded, T>>&) {
  return {{}, SetKind::Empty};  // unbounded above: no upper bound
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 Strictness SL, IsSide Hi>
constexpr SetVal<T, L> lowerbounds(
    const Comprehension<𝔸<T, L, C>, Bounds<Bounded<SL>, Hi, T>>& c) {
  T inf = c.predicate.lo;
  if constexpr (HasCoveringStep<T>)
    if (SL == Strictness::Strict) inf = successor(c.predicate.lo);
  return SetVal<T, L>::half(inf, Direction::Downward, Strictness::NonStrict);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Hi>
constexpr SetVal<T, L> lowerbounds(
    const Comprehension<𝔸<T, L, C>, Bounds<Unbounded, Hi, T>>&) {
  return {{}, SetKind::Empty};  // unbounded below: no lower bound
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

/** @brief @c & IS the meet on bare order operands: it forwards to the
 *  @c structured_and customization point, exactly as @c operator& does for
 *  wrapped predicates, so no @c Set{} wrapping is needed.  The complement-pair
 *  @c operator& above (opposite direction AND flipped strictness → @c Ø) is
 * more specialized and still claims its case; every other halfspace pair
 *  (overlapping, same-direction) routes here.
 *
 *  @details A @b disjoint pair (@c structured_and yields @c EmptyPredicate<T>)
 *  canonicalises to the empty set @c Ø<T,L> here, mirroring the sets-layer
 *  @c elevate_meet and the complement-pair meet above.  Before #895 this bare
 *  meet returned the raw @c EmptyPredicate<T>, which has no @c == against
 *  @c Ø<T,L>, so @c A @c & @c B on bare halfspaces disagreed with the
 *  @c Set{A} @c & @c Set{B} spelling (that reaches @c elevate_meet) and forced
 * a defensive @c Set{} re-wrap at the disjoint-emptiness call sites.  Routing
 * the empty result through the SAME @c Ø<T,L> the sets layer produces makes the
 * two spellings type-identical.  Non-empty results (an @c Interval, a
 *  @c Singleton) are returned exactly as @c structured_and shapes them. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C1,
                 IsCardinality C2, IsSide Lo1, IsSide Hi1, IsSide Lo2,
                 IsSide Hi2>
constexpr auto operator&(
    const Comprehension<𝔸<T, L, C1>, Bounds<Lo1, Hi1, T>>& a,
    const Comprehension<𝔸<T, L, C2>, Bounds<Lo2, Hi2, T>>& b) {
  // The meet is the one value law; the disjoint case is the empty SetVal
  // kind (no separate Ø<T,L> canonicalisation needed).
  return structured_and(a, b);
}

// #895/#932: a disjoint bare halfspace-meet collapses to the empty SetVal kind.
static_assert(((ℕ | (π > 5)) & (ℕ | (π < 3))).kind == SetKind::Empty,
              "bare {x>5} ∩ {x<3} collapses to the empty set (#895 / #932).");

/** @brief @c | IS the join on bare order operands, dual to the @c & meet: a
 *  same-direction union of rays collapses via @c structured_or.  A crossing
 *  union has no interval normal form, so this overload is not viable there and
 *  the generic set @c | (the reducer's @c Join node) decides by membership. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C1,
                 IsCardinality C2, IsSide Lo1, IsSide Hi1, IsSide Lo2,
                 IsSide Hi2>
constexpr auto operator|(
    const Comprehension<𝔸<T, L, C1>, Bounds<Lo1, Hi1, T>>& a,
    const Comprehension<𝔸<T, L, C2>, Bounds<Lo2, Hi2, T>>& b)
  requires requires { structured_or(a, b); }
{
  return structured_or(a, b);
}
/** @brief Meets involving the value @c SetVal route through the one
 *  @c reduce_meet --- this is how @c max/min's @c s @c & @c
 *  upperbounds(s) collapses, since @c upperbounds now yields a @c SetVal. */
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr SetVal<V, L> operator&(const SetVal<V, L>& a, const SetVal<V, L>& b) {
  return reduce_meet(a, b);
}
/** @brief The join law under the reducer's customization-point name, for two
 *  @b value leaves (the meet's sibling sits beside the cut meet above): a term
 *  whose leaves are already @c SetVal --- the leaf type the Python surface
 *  builds --- collapses through the same reducer as a term of bare cuts. */
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr SetVal<V, L> structured_or(const SetVal<V, L>& a,
                                     const SetVal<V, L>& b) {
  return reduce_join(a, b);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
constexpr auto operator&(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h,
                         const SetVal<T, L>& s) {
  return reduce_meet(to_setval(h), s);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
constexpr auto operator&(
    const SetVal<T, L>& s,
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h) {
  return reduce_meet(s, to_setval(h));
}

/** @brief @c Ø absorbs the meet (no upper bound ⟹ no max). */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi, IsOckhamAlgebra LZ>
constexpr auto operator&(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>&,
                         Ø<T, LZ>) {
  return dedekind::sets::codomain_reduce_t<Ø<T, L>>{};
}
/** @brief @c 𝔸 is the meet IDENTITY: @c {x⋈p} ∩ 𝔸 = @c {x⋈p}. */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi, IsOckhamAlgebra LU, IsCardinality CU>
constexpr auto operator&(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h,
                         const 𝔸<T, LU, CU>&) {
  return h;
}

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
    Cmp<Rel::Eq, BinOp<Plus, Projection<1>, Const<K>>, Projection<2>>;
export template <typename K>
using Scaling =
    Cmp<Rel::Eq, BinOp<Times, Projection<1>, Const<K>>, Projection<2>>;

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
    SetVal<int>::bounded(3, Strictness::Strict, 4, Strictness::Strict).kind ==
        SetKind::Empty,
    "nothing lies strictly between 3 and S(3): the successor is the cover.");
static_assert(
    SetVal<int>::bounded(3, Strictness::Strict, 5, Strictness::Strict).kind ==
            SetKind::Singleton &&
        SetVal<int>::bounded(3, Strictness::Strict, 5, Strictness::Strict).lo ==
            4,
    "(3, S(S(3))) is the point S(3) = 4.");
static_assert(SetVal<Cardinality>::bounded(finite_cardinality(3),
                                           Strictness::Strict,
                                           finite_cardinality(4),
                                           Strictness::Strict)
                      .kind == SetKind::Empty,
              "…and on the ℕ proxy, through its own successor.");
static_assert(SetVal<Ternary>::bounded(Ternary::False, Strictness::Strict,
                                       Ternary::True, Strictness::Strict)
                          .kind == SetKind::Singleton &&
                  SetVal<Ternary>::bounded(Ternary::False, Strictness::Strict,
                                           Ternary::True, Strictness::Strict)
                          .lo == Ternary::Unknown,
              "{x : ⊥ < x < ⊤} = {Unknown}: on K₃ the cover isolates the "
              "middle truth value.");
static_assert(graph(Successor<int>{})(std::pair{3, 4}) &&
                  !graph(Successor<int>{})(std::pair{3, 5}),
              "Γ_S = graph(S) is the cover relation.");
static_assert(detail_cover_witness::order_is_cover_star(8),
              "on [0, 8): a < b ⟺ (a, b) ∈ Γ_S⁺ --- the order is the star of "
              "the cover.");

/** @brief Two cuts are the same set iff their value forms agree: through
 *  @c to_setval and the extensional @c SetVal equality, which on a discrete
 *  carrier closes a strict bound where the step moves (@c {x > ⊥} is
 *  @c {x ≥ U} on K₃) and stays structural on a dense one.  Answers in @c L.
 *  On a truth chain the boundaries' exhaustion decides instead (it also sees
 *  that @c {x > ⊤} and @c {x < ⊥} are both empty, which no cut law does). */
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C1,
                 IsCardinality C2, IsSide Lo1, IsSide Hi1, IsSide Lo2,
                 IsSide Hi2>
  requires std::equality_comparable<T> && (!IsPst<T>)
constexpr typename L::Ω operator==(
    const Comprehension<𝔸<T, L, C1>, Bounds<Lo1, Hi1, T>>& a,
    const Comprehension<𝔸<T, L, C2>, Bounds<Lo2, Hi2, T>>& b) {
  return to_setval(a) == to_setval(b);
}

/** @brief Whether the ray @c {x ⋈ p} on @c T is empty: a strict ray at the
 *  carrier's end (@c {x > max}, @c {x < min}, @c {x < 0} on a carrier whose
 *  floor is 0), or a closed one beyond it.  A pivot is a value, so this is a
 *  value fact.  @tparam T the carrier.  @param p the pivot.  @param d the
 *  direction.  @param s the strictness. */
template <IsTotallyOrdered T>
constexpr bool ray_is_empty(const T& p, Direction d, Strictness s) {
  if (d == Direction::Upward) {
    if constexpr (std::integral<T>)
      return s == Strictness::Strict && p == std::numeric_limits<T>::max();
    else
      return false;
  }
  if constexpr (std::unsigned_integral<T> || std::same_as<T, Cardinality>)
    return s == Strictness::Strict && p == T{};
  else if constexpr (std::signed_integral<T>)
    return s == Strictness::Strict && p == std::numeric_limits<T>::min();
  else
    return false;
}

/** @brief Whether a cut denotes the empty set: a ray at the carrier's end, an
 *  interval whose bounds cross. */
template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C, IsSide Lo,
          IsSide Hi>
constexpr bool cut_is_empty(
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& c) {
  if constexpr (is_bounded_side_v<Lo> && is_bounded_side_v<Hi>)
    return SetVal<T, L>::bounded(c.predicate.lo, Lo::strictness, c.predicate.hi,
                                 Hi::strictness)
               .kind == SetKind::Empty;
  else
    return ray_is_empty(pivot(c), direction_of<Lo, Hi>(),
                        ray_strictness<Lo, Hi>());
}
/** @brief Whether a cut is all of its carrier: a ray whose complement is
 *  empty (@c {x ≥ 0} on ℕ).  An interval never is. */
template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C, IsSide Lo,
          IsSide Hi>
constexpr bool cut_is_universe(
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& c) {
  if constexpr (is_bounded_side_v<Lo> && is_bounded_side_v<Hi>)
    return false;
  else
    return ray_is_empty(pivot(c), flip(direction_of<Lo, Hi>()),
                        flip(ray_strictness<Lo, Hi>()));
}

// On a truth chain (𝔹, K₃) the boundaries decide these by exhausting the chain
// (sets:boundaries); the structural cut law below is for the other chains.
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
  requires(!IsPst<T>)
constexpr bool operator==(
    const Ø<T, L>&, const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h) {
  return cut_is_empty(h);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
  requires(!IsPst<T>)
constexpr bool operator==(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h,
                          const Ø<T, L>&) {
  return cut_is_empty(h);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
constexpr bool operator==(
    const dedekind::sets::EmptyPredicate<T>&,
    const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h) {
  return cut_is_empty(h);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi>
constexpr bool operator==(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h,
                          const dedekind::sets::EmptyPredicate<T>&) {
  return cut_is_empty(h);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi, IsCardinality CU>
  requires(!IsPst<T>)
constexpr bool operator==(
    const 𝔸<T, L, CU>&, const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h) {
  return cut_is_universe(h);
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality C,
                 IsSide Lo, IsSide Hi, IsCardinality CU>
  requires(!IsPst<T>)
constexpr bool operator==(const Comprehension<𝔸<T, L, C>, Bounds<Lo, Hi, T>>& h,
                          const 𝔸<T, L, CU>&) {
  return cut_is_universe(h);
}

// Modelling witness ("Theorems for Free", type-checked): the SPECIFIC pivot
// overload AGREES with the ABSTRACT definition max R = (∈) ∩ (R/∋) at the
// pivot.  5 is the max because 5 ∈ {x≤5} AND ∀a∈{x≤5}. a ≤ 5 --- the latter
// (the R/∋ division) decided by the counterexample set {x≤5} ∩ {x>5} collapsing
// to ∅ via the complement-pair meet.  So the specialisation is checked against
// the general law, not merely trusted (the Wadler free theorem, mechanised).
static_assert(max(le5)(5) ==
                  (le5(5) && (le5 & (ℕ | (π > 5))).kind == SetKind::Empty),
              "specific max(le5) models (∈) ∩ (R/∋) at the pivot.");
static_assert(min(ge5)(5) ==
                  (ge5(5) && (ge5 & (ℕ | (π < 5))).kind == SetKind::Empty),
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

// This block carries only FUNCTIONALITY (@c is_right_unique_v), the STRUCTURAL
// half: a graph is single-valued by its shape, decidable in @c order alone.
// ENTIRENESS (@c is_left_total_v) is the ALGEBRAIC half --- a translation is
// total iff the carrier is an ordered additive group --- so those
// specialisations live in @c dedekind.algebra:halfspace_transport, alongside
// the transport operations.  (The @c ProductRestrict entireness is left at its
// primary false there and in the primary: a restriction MAY drop the domain, so
// entireness is conservatively not certified through a joint.)

// LEAF: π2 == T, or T == π2, with T mentioning only the first slot is the
// graph of a function of the first coordinate, whatever T is: a translation, a
// scaling, the diagonal π1 == π2, an arrow applied to π1.
template <typename A, typename B, typename L, typename C, typename T>
  requires(dedekind::sets::only_slot(dedekind::sets::slots_v<T>, 1))
inline constexpr bool is_right_unique_v<dedekind::sets::Comprehension<
    dedekind::sets::𝔸<std::pair<A, B>, L, C>,
    dedekind::sets::Cmp<dedekind::sets::Rel::Eq, T,
                        dedekind::sets::Projection<2>>>> = true;
template <typename A, typename B, typename L, typename C, typename T>
  requires(dedekind::sets::only_slot(dedekind::sets::slots_v<T>, 1))
inline constexpr bool is_right_unique_v<dedekind::sets::Comprehension<
    dedekind::sets::𝔸<std::pair<A, B>, L, C>,
    dedekind::sets::Cmp<dedekind::sets::Rel::Eq, dedekind::sets::Projection<2>,
                        T>>> = true;

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
