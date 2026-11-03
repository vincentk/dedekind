/**
 * @file dedekind/category/runs.cppm
 * @partition :runs
 * @brief Run lists over a finite chain: a subset of @f$[0, n)@f$ as the
 *        positions where its membership changes, the canonical form of a
 *        decidable set on a chain, bounded by its type and on the stack.
 *
 * A run list is a predicate on the counts, an arrow @f$N \to \mathbb{B}@f$,
 * held as its steps: the strictly increasing positions where its value
 * flips, from outside at 0.  The steps @c {2, 5, 7, 9} are the set
 * @f$[2, 5) \cup [7, 9)@f$.  Maximal runs are what makes the form canonical:
 * two run lists are equal exactly when they hold the same elements, so
 * @c == is the sets' equality, at compile time and at run time alike.
 *
 * The lattice operations are @c :lattice's own vocabulary: @c a @c | @c b
 * and @c a @c & @c b build @c Join and @c Meet nodes, and @c :lattice_term's
 * reducer folds a term over run lists to one run list.  Its induced laws
 * fire first; the residual pair of leaves takes the run lists' own sweep,
 * injected as the reducer's leaf-combine (@c RunCombine).  Precomposition
 * with a translation, @c shift(d) @c >> @c s, is the preimage of @c s along
 * it, again a run list.
 *
 * Over a finite chain only, by the type's bound @c K on the steps: Pst's
 * run list.  The order module's @c Runs, valued in any species over chains
 * that may be infinite, stored in a map, is the general case; there a run
 * list is a step source (@c order::RunListSteps), so the order's @c ext,
 * merges, cuts and extensional equality read it, and a run list and an
 * interval term are compared as sets.  The step-source vocabulary they
 * share is to move here, over finite chains (#1138).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <functional>
#include <ranges>
#include <type_traits>
#include <utility>

export module dedekind.category:runs;

import :morphism;      // IsArrow
import :chain;         // IsCountWord
import :lattice;       // Meet, Join, builds_lattice_nodes: | and &
import :lattice_term;  // Reduce, canonical_order: the one reducer
import :f_algebra;     // cata: the one fold
import :periodic;      // AtMost, Shift
import :functor;       // Maybe: where a bound is claimed, not proved
import :logic;         // Boole: a run list's truth values

namespace dedekind::category {

/** @brief A subset of the counts held as its steps: the positions where
 *  membership flips, strictly increasing, from outside at 0.  At most @c K
 *  steps, so at most @c K / 2 runs.  @tparam N the counts.  @tparam K the
 *  bound on the steps. */
export template <IsCountWord N, std::size_t K>
struct RunList {
  using Domain = N;
  using Codomain = bool;
  using logic_species = Boole;  // Pst's: decided, two-valued
  AtMost<N, K> steps;
  /** @param x a count.  @return whether it lies in the set: an odd number
   *  of steps at or before it. */
  constexpr bool operator()(N x) const {
    return (std::ranges::upper_bound(steps, x) - steps.begin()) % 2 == 1;
  }
  /** @brief The sets' equality, whatever the two bounds: the same steps. */
  template <std::size_t J>
  constexpr bool operator==(const RunList<N, J>& o) const {
    return std::ranges::equal(steps, o.steps);
  }
};

/** @brief Run lists opt in: their @c | and @c & are the lattice's nodes
 *  (@c :lattice), a union and an intersection once reduced. */
template <IsCountWord N, std::size_t K>
struct builds_lattice_nodes<RunList<N, K>> : std::true_type {};

/** @tparam K the bound on the steps.  @param lo @param hi the ends.
 *  @return the run @f$[lo, hi)@f$, empty where @c hi is not past @c lo. */
export template <std::size_t K, IsCountWord N>
  requires(K >= 2)
constexpr RunList<N, K> run(N lo, std::type_identity_t<N> hi) {
  if (hi <= lo) return {};
  return {AtMost<N, K>{}.with(lo).with(hi)};
}

namespace detail_runs {
/** @brief One position of a sweep: the operation's value there, kept as a
 *  step where it differs from the value before. */
template <IsCountWord N, std::size_t J, std::size_t K, typename Op>
struct Sweep {
  RunList<N, J> a;
  RunList<N, K> b;
  Op op;
  struct State {
    bool inside{false};
    AtMost<N, J + K> steps;
  };
  constexpr State operator()(const State& s, N p) const {
    const bool v = op(a(p), b(p));
    if (v == s.inside) return s;
    return {v, s.steps.with(p)};
  }
};
/** @param a @param b two run lists.  @param op a pointwise operation on
 *  truth values.  @return the run list of @c op on them: the steps of both,
 *  in order, kept where the value changes, so the runs are maximal.  Its
 *  steps are among theirs, so its bound is the sum of theirs: total. */
template <IsCountWord N, std::size_t J, std::size_t K, typename Op>
constexpr RunList<N, J + K> swept(const RunList<N, J>& a,
                                  const RunList<N, K>& b, Op op) {
  AtMost<N, J + K> at = detail_periodic::joined(a.steps, b.steps);
  std::ranges::sort(at.items.begin(), at.items.begin() + at.length);
  const Sweep<N, J, K, Op> sweep{a, b, op};
  return {
      std::ranges::fold_left(at, typename Sweep<N, J, K, Op>::State{}, sweep)
          .steps};
}
/** @param r a run list.  @param w a single run @f$[lo, hi)@f$, or none.
 *  @return @c r restricted to it: its steps strictly inside, opened at
 *  @c lo where @c lo is in @c r and closed at @c hi where @c hi - 1 is.  Two
 *  bisections and a copy of what is kept, where the sweep tests every
 *  position of both. */
template <IsCountWord N, std::size_t J>
constexpr RunList<N, J + 2> restricted(const RunList<N, J>& r,
                                       const RunList<N, 2>& w) {
  if (w.steps.size() < 2) return {};
  const N lo = w.steps.items[0];
  const N hi = w.steps.items[1];
  const AtMost<N, J + 2> opened =
      r(lo) ? AtMost<N, J + 2>{}.with(lo) : AtMost<N, J + 2>{};
  const AtMost<N, J + 2> inside = std::ranges::fold_left(
      std::ranges::subrange(std::ranges::upper_bound(r.steps, lo),
                            std::ranges::lower_bound(r.steps, hi)),
      opened, detail_periodic::With{});
  return {r(static_cast<N>(hi - 1)) ? inside.with(hi) : inside};
}
/** @brief No order on run lists for the reducer to canonicalise by: the
 *  residual of two keeps its authored order, then combines. */
struct Unordered {};
/** @brief One step of a preimage along a translation: a step past it moves
 *  down by it; one at or before it is absorbed into the value at 0. */
template <IsCountWord N, std::size_t K>
struct Translated {
  N by;
  constexpr AtMost<N, K> operator()(const AtMost<N, K>& out, N p) const {
    return p > by ? out.with(static_cast<N>(p - by)) : out;
  }
};
}  // namespace detail_runs

/** @brief The run lists' domain meet and join, the reducer's leaf-combine:
 *  where two leaves are run lists on one chain, the sweep, its bound the sum
 *  of theirs. */
export struct RunCombine {
  template <IsCountWord N, std::size_t J, std::size_t K>
  static constexpr RunList<N, J + K> meet_value(const RunList<N, J>& a,
                                                const RunList<N, K>& b) {
    return detail_runs::swept(a, b, std::logical_and<bool>{});
  }
  /** @brief The meet with a single run, by law a restriction: the same
   *  set as the sweep's, by bisection. */
  template <IsCountWord N, std::size_t J>
  static constexpr RunList<N, J + 2> meet_value(const RunList<N, J>& a,
                                                const RunList<N, 2>& w) {
    return detail_runs::restricted(a, w);
  }
  template <IsCountWord N, std::size_t J, std::size_t K>
  static constexpr RunList<N, J + K> join_value(const RunList<N, J>& a,
                                                const RunList<N, K>& b) {
    return detail_runs::swept(a, b, std::logical_or<bool>{});
  }
};

/** @param t a lattice term over run lists.  @return its normal form: the
 *  reducer's laws, then the run lists' sweep on what remains. */
export template <typename T>
constexpr auto reduced(const T& t) {
  return cata(Reduce<detail_runs::Unordered, canonical_order, RunCombine>{}, t);
}

/** @brief A run list under a bound claimed, not proved by the types: the
 *  same set where its steps fit, nothing where they do not.  @tparam K the
 *  claimed bound.  @param r the run list.  @return it, bounded by @c K. */
export template <std::size_t K, IsCountWord N, std::size_t J>
constexpr Maybe<RunList<N, K>> narrowed(const RunList<N, J>& r) {
  if (r.steps.size() > K) return {};
  return RunList<N, K>{
      std::ranges::fold_left(r.steps, AtMost<N, K>{}, detail_periodic::With{})};
}

/** @brief @c shift(d) @c >> @c s: @f$x \mapsto s(x + d)@f$, the preimage
 *  of @c s along the translation, a run list again.  @param t the
 *  translation.  @param s the run list.  @return the preimage. */
export template <IsCountWord N, std::size_t K>
constexpr RunList<N, K> operator>>(const Shift<N>& t, const RunList<N, K>& s) {
  const AtMost<N, K> start =
      s(t.by) ? AtMost<N, K>{}.with(N{0}) : AtMost<N, K>{};
  return {std::ranges::fold_left(s.steps, start,
                                 detail_runs::Translated<N, K>{t.by})};
}

// The canonical form, witnessed.  A run is its two ends; two runs that
// touch merge into one, so the form is unique and == is the sets' equality.
static_assert(run<4>(2uz, 5uz)(2) && run<4>(2uz, 5uz)(4) &&
              !run<4>(2uz, 5uz)(5) && !run<4>(2uz, 5uz)(1) &&
              run<4>(3uz, 3uz) == RunList<std::size_t, 4>{});
static_assert(
    reduced(run<4>(0uz, 2uz) | run<4>(2uz, 4uz)) == run<4>(0uz, 4uz) &&
    reduced(run<4>(0uz, 3uz) & run<4>(2uz, 6uz)) == run<4>(2uz, 3uz) &&
    reduced(run<4>(0uz, 2uz) & run<4>(2uz, 4uz)) == RunList<std::size_t, 4>{});
// A term of three folds to one run list: [0, 2) ∪ [5, 7), then cut to [1, 6).
static_assert(
    reduced((run<4>(0uz, 2uz) | run<4>(5uz, 7uz)) & run<4>(1uz, 6uz)) ==
    RunList<std::size_t, 4>{
        AtMost<std::size_t, 4>{}.with(1uz).with(2uz).with(5uz).with(6uz)});
// Totality by type, not by abort: a union or an intersection of two run
// lists has its steps among theirs, so its bound is the sum of theirs (the
// run lists are not closed under either, but force the wider carrier, as a
// fixed-width word does); a preimage along a translation keeps its bound,
// closed.
using Runs4 = RunList<std::size_t, 4>;
static_assert(std::same_as<decltype(reduced(Runs4{} | Runs4{})),
                           RunList<std::size_t, 8>> &&
              std::same_as<decltype(reduced(Runs4{} & Runs4{})),
                           RunList<std::size_t, 8>> &&
              std::same_as<decltype(shift(1uz) >> Runs4{}), Runs4>);
// The union that needs its wider bound: two runs apart are four steps, and
// their intersection with a third that spans both cuts each, four again.
static_assert(reduced(run<2>(0uz, 2uz) | run<2>(4uz, 6uz)).steps.size() == 4 &&
              reduced(reduced(run<2>(0uz, 2uz) | run<2>(4uz, 6uz)) &
                      run<4>(1uz, 5uz))
                      .steps.size() == 4);
// The restriction is the sweep's meet: [0, 2) ∪ [4, 6) ∪ [8, 9) cut to
// [1, 8) is [1, 2) ∪ [4, 6), and cut to [5, 9) is [5, 6) ∪ [8, 9).
static_assert(detail_runs::restricted(reduced(reduced(run<2>(0uz, 2uz) |
                                                      run<2>(4uz, 6uz)) |
                                              run<4>(8uz, 9uz)),
                                      run<2>(1uz, 8uz)) ==
                  detail_runs::swept(
                      reduced(reduced(run<2>(0uz, 2uz) | run<2>(4uz, 6uz)) |
                              run<4>(8uz, 9uz)),
                      run<2>(1uz, 8uz), std::logical_and<bool>{}) &&
              detail_runs::restricted(reduced(reduced(run<2>(0uz, 2uz) |
                                                      run<2>(4uz, 6uz)) |
                                              run<4>(8uz, 9uz)),
                                      run<2>(5uz, 9uz)) ==
                  reduced(run<2>(5uz, 6uz) | run<2>(8uz, 9uz)));
// A bound claimed: the union of two points fits four steps, not two.
static_assert(
    narrowed<4>(reduced(run<2>(0uz, 1uz) | run<2>(5uz, 6uz))).has_value() &&
    !narrowed<2>(reduced(run<2>(0uz, 1uz) | run<2>(5uz, 6uz))).has_value());
// The preimage along a translation: [2, 5) seen from 3 on is [0, 2), and
// from 1 on is [1, 4).
static_assert((shift(3uz) >> run<4>(2uz, 5uz)) == run<4>(0uz, 2uz) &&
              (shift(1uz) >> run<4>(2uz, 5uz)) == run<4>(1uz, 4uz));

}  // namespace dedekind::category
