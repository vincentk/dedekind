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
 *
 * A set of a chain valued in a species @c L is a function from the chain to
 * @c L::Ω.  Built from intervals with the species' @c ∧, @c ∨ and @c ¬, it is
 * a @b step function: a value before the first step, and finitely many steps,
 * each a position on the chain (just below or just above a value) where the
 * value changes.  In canonical form the positions are closed where the chain
 * has the step, ordered, and no step repeats the value before it, so each set
 * has exactly one run list.  The variation is in the value (how many steps),
 * never in the type: a union of intervals, a complement, a three-valued set
 * over K₃ are all one type, and @c &, @c |, @c ~ are total on it.
 *
 * This is the canonical form of the Boolean (or De Morgan) algebra generated
 * by the intervals of a chain, quotiented by the chain's order: the reducer
 * (@c category::Reduce) folds a set term into it through @c SetCombine, and
 * @c sets::normalise names that step.  An @c Interval is its generator, a run
 * list of at most one run.
 *
 * The representation keeps a step at the carrier's own end (@c {x ≤ max} on a
 * machine word read as a window of ℤ stays bounded above); equality reads the
 * carrier's ends, so it is extensional.
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
import :total;      // IsTotallyOrdered
import :halfspace;  // Interval, Position, Bound, Span

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::category;

/** @brief A set of the chain @c V valued in @c L, as a step function: the
 *  value before every step, and the steps.  Built only through the factories
 *  and the operations, so always in canonical form.
 *  @tparam V the carrier.  @tparam L the species. */
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
  /** @brief The constant set: @c Ø at ⊥, @c 𝔸 at ⊤.  @param v the value. */
  static constexpr Runs constant(Ω v) { return Runs{v, {}}; }
  static constexpr Runs empty() { return constant(L::False); }
  static constexpr Runs universe() { return constant(L::True); }
  /** @brief An interval, valued @c v on it and ⊥ elsewhere.
   *  @param s the interval.  @param v its value. */
  static constexpr Runs of(const Interval<V, L>& s, Ω v = L::True) {
    if (!s.span()) return empty();
    const auto& [lo, hi] = *s.span();
    std::vector<Step> steps;
    if (lo) steps.push_back({detail_position::start_of(*lo), v});
    if (hi) steps.push_back({detail_position::end_of(*hi), L::False});
    return normal(lo ? L::False : v, std::move(steps));
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
      if (!past(x, s.at)) break;
      v = s.value;
    }
    return v;
  }
  /** @brief Whether @c x is a member: χ(x) = ⊤. */
  constexpr bool contains(const V& x) const { return (*this)(x) == L::True; }

 private:
  constexpr Runs(Ω first, std::vector<Step> steps)
      : first_(first), steps_(std::move(steps)) {}
  /** @brief Whether @c x lies past the position @c p. */
  static constexpr bool past(const V& x, const Position<V>& p) {
    return p.side == Side::Below ? !(x < p.at) : p.at < x;
  }
  Ω first_ = L::True;
  std::vector<Step> steps_;
};

static_assert(dedekind::sets::IsSetObject<Runs<int>> &&
                  dedekind::sets::IsSetObject<Runs<Ternary, Kleene>>,
              "a run list is a set object: the value normal form over its "
              "universe.");

/** @brief The run list of @c f applied at every point of the chain: the steps
 *  of both operands, merged along it.  @param a,b the operands.  @param f the
 *  pointwise operation on @c L::Ω. */
export template <typename V, typename L, typename F>
constexpr Runs<V, L> pointwise(const Runs<V, L>& a, const Runs<V, L>& b, F f) {
  using Step = typename Runs<V, L>::Step;
  const auto& x = a.steps();
  const auto& y = b.steps();
  std::vector<Step> out;
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
/** @brief The run list as the carrier sees it: a step before every element
 *  sets the first value, a step after every element is dropped.  @param r the
 *  run list. */
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

/** @name The algebra on run lists
 *  @brief Meet, join and complement pointwise in the species, under the
 *  reducer's customization-point names (@c SetCombine's leaf legs), so the
 *  set-level @c & and @c | on intervals and run lists fold to a run list.  The
 *  meet of two intervals stays an interval (@c :halfspace).
 *  @{ */
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> structured_and(const Runs<V, L>& a, const Runs<V, L>& b) {
  return pointwise(a, b, detail_runs::And<L>{});
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> structured_and(const Runs<V, L>& a,
                                    const Interval<V, L>& b) {
  return structured_and(a, Runs<V, L>::of(b));
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> structured_and(const Interval<V, L>& a,
                                    const Runs<V, L>& b) {
  return structured_and(Runs<V, L>::of(a), b);
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> structured_or(const Runs<V, L>& a, const Runs<V, L>& b) {
  return pointwise(a, b, detail_runs::Or<L>{});
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> structured_or(const Runs<V, L>& a,
                                   const Interval<V, L>& b) {
  return structured_or(a, Runs<V, L>::of(b));
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> structured_or(const Interval<V, L>& a,
                                   const Runs<V, L>& b) {
  return structured_or(Runs<V, L>::of(a), b);
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> structured_or(const Interval<V, L>& a,
                                   const Interval<V, L>& b) {
  return structured_or(Runs<V, L>::of(a), Runs<V, L>::of(b));
}
/** @brief The complement: the species' reflection at every point. */
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> operator~(const Runs<V, L>& a) {
  return relabel<L>(a, detail_runs::Rfl<L>{});
}
/** @brief The complement of an interval: the parts below and above it. */
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
constexpr Runs<V, L> operator~(const Interval<V, L>& a) {
  return ~Runs<V, L>::of(a);
}
/** @} */

/** @brief Equality of two run lists as sets of the carrier, in the species:
 *  @f$\bigwedge_x (\chi_a(x) \Leftrightarrow \chi_b(x))@f$, read over the
 *  carrier's own elements.  For a two-valued set it is the equality of the
 *  normal forms; over K₃ two sets agree to degree U where either is U. */
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
  requires std::equality_comparable<V>
constexpr typename L::Ω operator==(const Runs<V, L>& a, const Runs<V, L>& b) {
  const auto e =
      pointwise(detail_runs::within_carrier(a), detail_runs::within_carrier(b),
                detail_runs::Iff<L>{});
  auto v = e.first();
  for (const auto& s : e.steps()) v = L::AND(v, s.value);
  return v;
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
  requires std::equality_comparable<V>
constexpr typename L::Ω operator==(const Runs<V, L>& a,
                                   const Interval<V, L>& b) {
  return a == Runs<V, L>::of(b);
}
export template <IsTotallyOrdered V, IsOckhamAlgebra L>
  requires std::equality_comparable<V>
constexpr typename L::Ω operator==(const Interval<V, L>& a,
                                   const Runs<V, L>& b) {
  return Runs<V, L>::of(a) == b;
}
/** @brief A run list against the boundaries.  On a truth chain the boundaries
 *  decide by exhausting the chain instead (sets:boundaries). */
export template <IsTotallyOrdered T, IsOckhamAlgebra L>
  requires(!IsPst<T>)
constexpr bool operator==(const Ø<T, L>&, const Runs<T, L>& r) {
  return static_cast<bool>(r == Runs<T, L>::empty());
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L>
  requires(!IsPst<T>)
constexpr bool operator==(const Runs<T, L>& r, const Ø<T, L>& e) {
  return e == r;
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality CU>
  requires(!IsPst<T>)
constexpr bool operator==(const 𝔸<T, L, CU>&, const Runs<T, L>& r) {
  return static_cast<bool>(r == Runs<T, L>::universe());
}
export template <IsTotallyOrdered T, IsOckhamAlgebra L, IsCardinality CU>
  requires(!IsPst<T>)
constexpr bool operator==(const Runs<T, L>& r, const 𝔸<T, L, CU>& u) {
  return u == r;
}

/** @brief The runs of a two-valued set as intervals, in order along the
 *  chain: the maximal pieces on which it holds.  @param r the run list. */
export template <IsTotallyOrdered V>
constexpr std::vector<Interval<V, Boole>> intervals(const Runs<V, Boole>& r) {
  using I = Interval<V, Boole>;
  std::vector<I> out;
  std::optional<Bound<V>> lo;
  bool inside = r.first();
  for (const auto& s : r.steps()) {
    if (s.value) {
      lo = detail_position::lower_at(s.at);
    } else {
      out.push_back(I::of_sides(inside ? lo : std::nullopt,
                                detail_position::upper_at(s.at)));
      lo.reset();
    }
    inside = s.value;
  }
  if (inside) out.push_back(I::of_sides(lo, std::nullopt));
  return out;
}

/** @section runs__Formal_Verification */
namespace detail_runs_witness {
using I = Interval<int>;
using R = Runs<int>;
constexpr auto closed = Strictness::NonStrict;
// One normal form, however the set is spelled: [0, 10] without 5.
static_assert(structured_and(I::bounded(0, closed, 10, closed), ~I::point(5)) ==
                  structured_or(I::bounded(0, closed, 4, closed),
                                I::bounded(6, closed, 10, closed)),
              "[0,10] ∩ ¬{5} = [0,4] ∪ [6,10]: one run list");
static_assert(R::of(I::bounded(0, closed, 10, closed)).steps() ==
                  (~~R::of(I::bounded(0, closed, 10, closed))).steps(),
              "¬¬A = A, as the same steps");
static_assert(intervals(~I::point(5)).size() == 2 &&
                  intervals(R::empty()).empty() &&
                  intervals(R::universe()).size() == 1,
              "¬{5} is two runs, Ø none, 𝔸 one");
static_assert(Ø<int>{} == structured_and(~I::point(5), I::point(5)) &&
                  𝔸<int>{} == structured_or(~I::point(5), I::point(5)),
              "a ∧ ¬a = Ø and a ∨ ¬a = 𝔸 on the normal form");
}  // namespace detail_runs_witness

}  // namespace dedekind::order
