/**
 * @file dedekind/order/runs.cppm
 * @partition :runs
 * @brief Run lists: the normal form of the sets of a chain built from
 *        intervals, valued in a species.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section runs__Normal_Form
 * A set of a chain valued in a species @c L, built from intervals with the
 * species' @c ∧, @c ∨ and @c ¬, is a @b step function: a value before the
 * first step, and finitely many steps, each a position on the chain where the
 * value changes.  In canonical form the positions are closed where the chain
 * has the step and no step repeats the value before it, so each set has one
 * run list, and @c &, @c |, @c ~ are total on the one type.  It is the
 * canonical form of the Boolean (De Morgan) algebra the intervals generate,
 * quotiented by the chain's order; @c Interval (@c :halfspace) is its
 * generator.  A step at the carrier's own end is kept (@c {x ≤ max} on a
 * machine word read as a window of ℤ stays bounded above); equality reads the
 * carrier's ends.
 */
module;

#include <concepts>
#include <cstddef>
#include <limits>
#include <optional>
#include <type_traits>
#include <utility>
#include <vector>

export module dedekind.order:runs;

import dedekind.category;
import dedekind.sets;
import :total;  // IsTotallyOrdered: positions live on a certified chain

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::category;

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

/** @brief One side of an interval as a value: the bound and whether it is
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

/** @section runs__Positions
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
/** @brief The element @c x covers: none at the least element or on a carrier
 *  without the step.  @tparam V the carrier. */
template <typename V>
constexpr std::optional<V> next_below(const V& x) {
  if constexpr (std::integral<V>) {
    if (x == std::numeric_limits<V>::min()) return std::nullopt;
    return predecessor(x);
  } else if constexpr (HasNNOStep<V>) {
    const V p = predecessor(x);
    if (p == x) return std::nullopt;
    return p;
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

namespace detail_runs {
/** @brief The species' operations as named functions on @c L::Ω. */
template <typename L>
struct And {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::AND(a, b);
  }
};
template <typename L>
struct Or {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::OR(a, b);
  }
};
template <typename L>
struct Rfl {
  constexpr auto operator()(typename L::Ω a) const { return L::RFL(a); }
};
/** @brief @c a ⇔ @c b in the species: both, or neither. */
template <typename L>
struct Iff {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::OR(L::AND(a, b), L::AND(L::RFL(a), L::RFL(b)));
  }
};
}  // namespace detail_runs

/** @brief A set of the chain @c V valued in @c L, as a step function: the
 *  value before every step, and the steps.  Built only through the factories
 *  and the operations, so always in canonical form.  @tparam V the carrier.
 *  @tparam L the species. */
export template <typename V = long long, typename L = Boole>
  requires IsTotallyOrdered<V> && IsOckhamAlgebra<L>
struct Runs : dedekind::sets::SetExpr<Runs<V, L>, V, L> {
  using Domain = V;
  using Codomain = typename L::Ω;
  using cardinality_type = carrier_cardinality_t<V>;
  using Ω = typename L::Ω;

  /** @brief Where the value changes, and to what. */
  struct Step {
    Position<V> at;
    Ω value;
    friend constexpr bool operator==(const Step&, const Step&) = default;
  };

  /** @brief The universe, 𝔸. */
  constexpr Runs() = default;
  /** @brief The constant set: @c Ø at ⊥, @c 𝔸 at ⊤. */
  static constexpr Runs constant(Ω v) { return Runs{v, {}}; }
  static constexpr Runs empty() { return constant(L::False); }
  static constexpr Runs universe() { return constant(L::True); }
  /** @brief An interval's sides (@c nullopt for Ø), valued @c v on it and ⊥
   *  elsewhere.  @param s the sides.  @param v the value. */
  static constexpr Runs of(const std::optional<Span<V>>& s, Ω v = L::True) {
    if (!s) return empty();
    std::vector<Step> steps;
    if (s->lower) steps.push_back({detail_position::start_of(*s->lower), v});
    if (s->upper)
      steps.push_back({detail_position::end_of(*s->upper), L::False});
    return normal(s->lower ? L::False : v, std::move(steps));
  }
  /** @brief The canonical run list of a step function: each position closed
   *  where the chain has the step, a later step at the same position replacing
   *  an earlier one, and no step repeating the value before it.
   *  @param first the value before every step.  @param steps the steps, in
   *  non-decreasing order of position. */
  static constexpr Runs normal(Ω first, std::vector<Step> steps) {
    Runs r{first, {}};
    for (Step s : steps) {
      s.at = detail_position::canonical(s.at);
      if (!r.steps_.empty() && r.steps_.back().at == s.at) r.steps_.pop_back();
      const Ω before = r.steps_.empty() ? r.first_ : r.steps_.back().value;
      if (s.value != before) r.steps_.push_back(s);
    }
    return r;
  }

  /** @brief The value before every step. */
  constexpr Ω first() const { return first_; }
  /** @brief The steps, in order along the chain. */
  constexpr const std::vector<Step>& steps() const { return steps_; }

  /** @brief χ(x): the value of the last step at or before @c x. */
  constexpr Ω operator()(const V& x) const {
    Ω v = first_;
    for (const Step& s : steps_) {
      if (s.at.side == Side::Below ? x < s.at.at : !(s.at.at < x)) break;
      v = s.value;
    }
    return v;
  }
  /** @brief Whether @c x is a member: χ(x) = ⊤. */
  constexpr bool contains(const V& x) const { return (*this)(x) == L::True; }

  /** @name The algebra, pointwise in the species
   *  @brief Meet and join under the reducer's customization-point names
   *  (@c SetCombine's leaf legs), so the set-level @c & and @c | on run lists
   *  and intervals fold to a run list; the complement; and equality,
   *  @f$\bigwedge_x (\chi_a(x) \Leftrightarrow \chi_b(x))@f$ over the
   *  carrier's own elements, so over K₃ two sets agree to degree U where
   *  either is U.  Found by ADL; an interval converts.
   *  @{ */
  friend constexpr Runs structured_and(const Runs& a, const Runs& b) {
    return pointwise(a, b, detail_runs::And<L>{});
  }
  friend constexpr Runs structured_or(const Runs& a, const Runs& b) {
    return pointwise(a, b, detail_runs::Or<L>{});
  }
  friend constexpr Runs operator~(const Runs& a) {
    return relabel<L>(a, detail_runs::Rfl<L>{});
  }
  friend constexpr Ω operator==(const Runs& a, const Runs& b)
    requires std::equality_comparable<V>
  {
    return forall(pointwise(a, b, detail_runs::Iff<L>{}));
  }
  /** @} */

 private:
  constexpr Runs(Ω first, std::vector<Step> steps)
      : first_(first), steps_(std::move(steps)) {}
  Ω first_ = L::True;
  std::vector<Step> steps_;
};

static_assert(dedekind::sets::IsSetObject<Runs<int>> &&
                  dedekind::sets::IsSetObject<Runs<Ternary, Kleene>>,
              "a run list is a set object over its universe.");

/** @brief The run list of @c f applied at every point: the steps of both
 *  operands, merged along the chain.  @param a,b the operands.  @param f the
 *  pointwise operation on @c L::Ω. */
export template <typename V, typename L, typename F>
constexpr Runs<V, L> pointwise(const Runs<V, L>& a, const Runs<V, L>& b, F f) {
  const auto& x = a.steps();
  const auto& y = b.steps();
  std::vector<typename Runs<V, L>::Step> out;
  auto va = a.first();
  auto vb = b.first();
  std::size_t i = 0;
  std::size_t j = 0;
  while (i < x.size() || j < y.size()) {
    const Position<V> at =
        (j == y.size() || (i < x.size() && x[i].at < y[j].at)) ? x[i].at
                                                               : y[j].at;
    if (i < x.size() && x[i].at == at) va = x[i++].value;
    if (j < y.size() && y[j].at == at) vb = y[j++].value;
    out.push_back({at, f(va, vb)});
  }
  return Runs<V, L>::normal(f(a.first(), b.first()), std::move(out));
}
/** @brief The run list of @c f applied to every value, valued in @c L2: a
 *  complement, a lift into a wider species, an α-cut.  @param a the operand.
 *  @param f the operation @c L::Ω → @c L2::Ω. */
export template <typename L2, typename V, typename L, typename F>
constexpr Runs<V, L2> relabel(const Runs<V, L>& a, F f) {
  std::vector<typename Runs<V, L2>::Step> out;
  for (const auto& s : a.steps()) out.push_back({s.at, f(s.value)});
  return Runs<V, L2>::normal(f(a.first()), std::move(out));
}

namespace detail_runs {
/** @brief The run list as the carrier sees it: a step before every element
 *  sets the first value, a step after every element is dropped. */
template <typename V, typename L>
constexpr Runs<V, L> within_carrier(const Runs<V, L>& r) {
  auto first = r.first();
  std::vector<typename Runs<V, L>::Step> out;
  for (const auto& s : r.steps()) {
    if (detail_position::before_all(s.at))
      first = s.value;
    else if (!detail_position::after_all(s.at))
      out.push_back(s);
  }
  return Runs<V, L>::normal(first, std::move(out));
}
}  // namespace detail_runs

/** @name The quantifiers of a set over its carrier
 *  @brief @f$\bigwedge_x \chi(x)@f$ and @f$\bigvee_x \chi(x)@f$ in the
 *  species over the carrier's own elements, a fold over the run list's values:
 *  on @c Ø both are ⊥, on @c 𝔸 both ⊤.
 *  @{ */
export template <typename V, typename L>
constexpr typename L::Ω forall(const Runs<V, L>& r) {
  const auto c = detail_runs::within_carrier(r);
  auto v = c.first();
  for (const auto& s : c.steps()) v = L::AND(v, s.value);
  return v;
}
export template <typename V, typename L>
constexpr typename L::Ω exists(const Runs<V, L>& r) {
  const auto c = detail_runs::within_carrier(r);
  auto v = c.first();
  for (const auto& s : c.steps()) v = L::OR(v, s.value);
  return v;
}
/** @} */

/** @brief A maximal run of membership as its closed ends, @c [lo, hi].
 *  @tparam C the chain. */
export template <typename C>
struct Run {
  C lo;
  C hi;
  friend constexpr bool operator==(const Run&, const Run&) = default;
};
/** @brief The runs of a two-valued set of a discrete chain with ends, as
 *  closed pairs in order, read over the carrier: an unbounded side at the
 *  carrier's end.  @param r the run list. */
export template <IsTotallyOrdered C>
  requires(detail_position::least<C>().has_value())
constexpr std::vector<Run<C>> runs(const Runs<C, Boole>& r) {
  using namespace detail_position;
  const auto c = detail_runs::within_carrier(r);
  std::vector<Run<C>> out;
  std::optional<C> lo = c.first() ? least<C>() : std::nullopt;
  for (const auto& s : c.steps()) {
    if (s.value)
      lo = s.at.at;  // a canonical position inside the carrier is below
    else
      out.push_back({*lo, *next_below(s.at.at)});
  }
  if (c.steps().empty() ? c.first() : c.steps().back().value)
    out.push_back({*lo, *greatest<C>()});
  return out;
}

/** @section runs__Formal_Verification */
namespace detail_runs_witness {
using R = Runs<int>;
constexpr auto closed = Strictness::NonStrict;
constexpr std::optional<Span<int>> span(int lo, int hi) {
  return Span<int>{Bound<int>{lo, closed}, Bound<int>{hi, closed}};
}
static_assert(structured_and(R::of(span(0, 10)), ~R::of(span(5, 5))) ==
                  structured_or(R::of(span(0, 4)), R::of(span(6, 10))),
              "[0,10] ∩ ¬{5} = [0,4] ∪ [6,10]: one run list");
static_assert((~~R::of(span(0, 10))).steps() == R::of(span(0, 10)).steps(),
              "¬¬A = A, as the same steps");
static_assert(runs(~R::of(span(5, 5))).size() == 2 && runs(R::empty()).empty(),
              "¬{5} is two runs, Ø none");
}  // namespace detail_runs_witness

}  // namespace dedekind::order
