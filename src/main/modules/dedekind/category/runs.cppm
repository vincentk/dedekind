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
 * Over a finite chain only, by the type's bound @c K on the steps; the
 * order module's @c Runs, over chains that may be infinite and stored in a
 * map, agrees with it there (#1135).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <algorithm>
#include <cstddef>
#include <functional>
#include <ranges>

export module dedekind.category:runs;

import :morphism;      // IsArrow
import :chain;         // IsCountWord
import :lattice;       // Meet, Join: the nodes | and & build
import :lattice_term;  // Reduce, canonical_order: the one reducer
import :f_algebra;     // cata: the one fold
import :periodic;      // AtMost, Shift

namespace dedekind::category {

/** @brief A subset of the counts held as its steps: the positions where
 *  membership flips, strictly increasing, from outside at 0.  At most @c K
 *  steps, so at most @c K / 2 runs.  @tparam N the counts.  @tparam K the
 *  bound on the steps. */
export template <IsCountWord N, std::size_t K>
struct RunList {
  using Domain = N;
  using Codomain = bool;
  AtMost<N, K> steps;
  /** @param x a count.  @return whether it lies in the set: an odd number
   *  of steps at or before it. */
  constexpr bool operator()(N x) const {
    return (std::ranges::upper_bound(steps, x) - steps.begin()) % 2 == 1;
  }
  constexpr bool operator==(const RunList&) const = default;
};

/** @tparam K the bound on the steps.  @param lo @param hi the ends.
 *  @return the run @f$[lo, hi)@f$, empty where @c hi is not past @c lo. */
export template <std::size_t K, IsCountWord N>
constexpr RunList<N, K> run(N lo, std::type_identity_t<N> hi) {
  if (hi <= lo) return {};
  return {AtMost<N, K>{}.with(lo).with(hi)};
}

namespace detail_runs {
/** @brief One position of a sweep: the operation's value there, kept as a
 *  step where it differs from the value before. */
template <IsCountWord N, std::size_t K, typename Op>
struct Sweep {
  RunList<N, K> a;
  RunList<N, K> b;
  Op op;
  struct State {
    bool inside{false};
    AtMost<N, K> steps;
  };
  constexpr State operator()(const State& s, N p) const {
    const bool v = op(a(p), b(p));
    if (v == s.inside) return s;
    return {v, s.steps.with(p)};
  }
};
/** @param a @param b two run lists.  @param op a pointwise operation on
 *  truth values.  @return the run list of @c op on them: the steps of both,
 *  in order, kept where the value changes, so the runs are maximal. */
template <IsCountWord N, std::size_t K, typename Op>
constexpr RunList<N, K> swept(const RunList<N, K>& a, const RunList<N, K>& b,
                              Op op) {
  AtMost<N, 2 * K> at = detail_periodic::joined(a.steps, b.steps);
  std::ranges::sort(at.items.begin(), at.items.begin() + at.length);
  const Sweep<N, K, Op> sweep{a, b, op};
  return {std::ranges::fold_left(at, typename Sweep<N, K, Op>::State{}, sweep)
              .steps};
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
 *  where two leaves are run lists of one type, the sweep. */
export struct RunCombine {
  template <IsCountWord N, std::size_t K>
  static constexpr RunList<N, K> meet_value(const RunList<N, K>& a,
                                            const RunList<N, K>& b) {
    return detail_runs::swept(a, b, std::logical_and<bool>{});
  }
  template <IsCountWord N, std::size_t K>
  static constexpr RunList<N, K> join_value(const RunList<N, K>& a,
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
// The preimage along a translation: [2, 5) seen from 3 on is [0, 2), and
// from 1 on is [1, 4).
static_assert((shift(3uz) >> run<4>(2uz, 5uz)) == run<4>(0uz, 2uz) &&
              (shift(1uz) >> run<4>(2uz, 5uz)) == run<4>(1uz, 4uz));

}  // namespace dedekind::category
