/**
 * @file dedekind/sequences/fold.cppm
 * @partition :fold
 * @brief The left fold (catamorphism) over an enumerable sequence.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section fold__Overview
 * @c fold(xs, init, op) reduces a sequence to a single value by threading an
 * accumulator, @c op(acc, x) left to right.  It is the point-free primitive
 * behind the graph closure of @c dedekind.optimization:closure --- a graph
 * algorithm becomes a fold over its edge sequence rather than nested index
 * loops.  A fold is defined only for an @b enumerable (extensional) sequence:
 * an intensional predicate has no elements to thread (Rice), so its relation
 * is materialised extensionally first, then folded.
 */
module;

#include <type_traits>  // the two shapes of a step: value-returning or in place
#include <utility>

export module dedekind.sequences:fold;

namespace dedekind::sequences {

/**
 * @brief Strict left fold over an enumerable range @c xs.
 *
 * Threads the accumulator through @c op(acc, x) left to right, in place, and
 * returns it.  A scalar accumulator recovers the familiar reduction (sum,
 * product); a structured accumulator (e.g.\ a potential vector) recovers a
 * stateful pass, without copying the accumulator at each step.
 *
 * The step has two shapes.  A @b monoid step returns the new accumulator,
 * @c acc @c = @c op(acc, x), the textbook @f$\mathrm{fold}(\oplus, e)@f$ ---
 * the quantifiers over a finite chain (@c :pst) are this shape, with
 * @f$\oplus = \vee, \wedge@f$ in @f$L@f$.  A @b stateful step updates @c acc
 * in place and returns @c void, so a structured accumulator (a potential
 * vector) is not copied per step.  Which shape is meant is read off the step's
 * return type.
 *
 * @param xs   any range with @c begin / @c end (e.g.\ a @c constexpr array,
 *             which folds to a constant at translation time).
 * @param acc  the seed accumulator (returned, threaded).
 * @param op   @c op(acc, x) returning the new accumulator, or @c op(acc&, x)
 *             returning @c void; applied left to right.
 */
export template <typename Range, typename Acc, typename Op>
constexpr Acc fold(const Range& xs, Acc acc, Op op) {
  for (const auto& x : xs) {
    if constexpr (std::is_void_v<std::invoke_result_t<Op&, Acc&, decltype(x)>>)
      op(acc, x);
    else
      acc = op(std::move(acc), x);
  }
  return acc;
}

}  // namespace dedekind::sequences
