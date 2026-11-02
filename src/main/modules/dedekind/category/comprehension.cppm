/**
 * @file dedekind/category/comprehension.cppm
 * @partition :comprehension
 * @brief Monad comprehensions over a finite chain: the queries of SQL and
 *        LINQ, @c where, @c select, @c order_by and @c limit, and the folds
 *        that end them.
 *
 * @section comprehension__Rhythm One carrier, one rhythm
 * Every stage takes a query and returns a query, composed by @c >>, as
 * Python's @c [f(x) for x in xs if p(x)] and SQL's @c SELECT ... WHERE do.
 *
 * | LINQ        | SQL        | here                        |
 * |-------------|------------|-----------------------------|
 * | @c from     | @c FROM    | @c n @c >> @c chain<N>()    |
 * | @c Select   | @c SELECT  | @c >> @c f                  |
 * | @c Where    | @c WHERE   | @c >> @c where(p)           |
 * | @c Count    | @c COUNT   | @c count                    |
 * | @c Sum      | @c SUM     | @c sum                      |
 *
 * The carrier is a finite chain sequence holding at most one element per
 * position: the list monad whose Kleisli arrows yield none or one, so a
 * query is a chain read through a Kleisli arrow into @c Maybe.  @c where is
 * that arrow; @c select after it is @c Maybe's action on arrows; a fold ends
 * the query, its step lifted once through @c Maybe's eliminator, so no
 * consumer cases on a hole.  A plain sequence has no holes and stays
 * unlifted, by its type.  The rewrites a query optimiser applies are laws
 * of the monad and of the algebra: Wadler's comprehensions over any monad
 * (1990), the comprehension syntax of Buneman, Libkin, Suciu, Tannen and
 * Wong as a query language (1994), Grust's optimiser on its laws (#1133).
 * A join yielding several rows per input leaves the carrier, a bag's; its
 * count is the tensor products' (@c :periodic), never materialised.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <functional>
#include <optional>
#include <ranges>
#include <type_traits>

export module dedekind.category:comprehension;

import :morphism;  // IsArrow, Dom, Cod
import :nno;       // Successor: a witness's arrow
import :chain;     // the chain, FiniteChainSequence, IsCountWord
import :functor;   // Maybe, MaybeMap: the functor's action on the holes
import :periodic;  // is_even, the count of a characteristic sequence

namespace dedekind::category {

namespace detail_comprehension {
/** @brief A fold step with its accumulator bound, @f$a \mapsto
 *  \mathit{op}(\mathit{acc}, a)@f$: the present case of the eliminator. */
template <std::semiregular Op, std::semiregular R>
struct StepFrom {
  Op op;
  R acc;
  template <typename A>
    requires std::convertible_to<
        std::invoke_result_t<const Op&, const R&, const A&>, R>
  constexpr R operator()(const A& a) const {
    return std::invoke(op, acc, a);
  }
};
/** @brief One more element into a count. */
template <IsCountWord N>
struct Tally {
  template <typename A>
  constexpr N operator()(N acc, const A&) const {
    return static_cast<N>(acc + 1);
  }
};
/** @brief Whether @c T is a @c Maybe: an element of a query, or none. */
template <typename T>
inline constexpr bool is_maybe_v = false;
template <typename T>
inline constexpr bool is_maybe_v<Maybe<T>> = true;
}  // namespace detail_comprehension

/** @brief A query: a range whose elements are @c Maybe, each present or a
 *  hole. */
export template <typename R>
concept IsQuery =
    std::ranges::input_range<R> &&
    detail_comprehension::is_maybe_v<std::ranges::range_value_t<R>>;

/** @brief A fold step lifted through @c Maybe: @c Maybe's eliminator with
 *  the cases @f$[\mathit{acc}, \mathit{op}(\mathit{acc}, \cdot)]@f$, a hole
 *  leaving the accumulator as it is.  Written once, so the folds that end a
 *  query take their plain steps and none cases on a hole.
 *  @tparam Op the step. */
export template <std::semiregular Op>
struct Lifted {
  Op op;
  template <std::semiregular R, typename A>
    requires std::convertible_to<
        std::invoke_result_t<const Op&, const R&, const A&>, R>
  constexpr R operator()(const R& acc, const Maybe<A>& mx) const {
    return mx.transform(detail_comprehension::StepFrom<Op, R>{op, acc})
        .value_or(acc);
  }
};
/** @param op a fold step.  @return @c op through @c Maybe. */
export template <std::semiregular Op>
constexpr Lifted<Op> lifted(Op op) {
  return {std::move(op)};
}

/** @brief @c where(p), SQL's @c WHERE: the Kleisli arrow into @c Maybe that
 *  keeps an element where @c p holds, a hole where it does not.  The walk
 *  keeps its length, so a filtered chain is a chain in the same order.
 *  @tparam P the predicate. */
export template <IsArrow P>
  requires std::same_as<Cod<P>, bool>
struct Where {
  using Domain = Dom<P>;
  using Codomain = Maybe<Dom<P>>;
  P p;
  constexpr Codomain operator()(const Domain& x) const {
    return p(x) ? Codomain{x} : Codomain{};
  }
};
/** @param p the predicate.  @return the elements where it holds. */
export template <IsArrow P>
  requires std::same_as<Cod<P>, bool>
constexpr Where<P> where(P p) {
  return {std::move(p)};
}

/** @brief @c >> @c f on a query, SQL's @c SELECT: @c f on the present
 *  elements, the holes passed through, @c Maybe's action on the arrow
 *  (@c maybe_functor).  @param q the query.  @param f the arrow. */
export template <std::regular S, std::semiregular Coalg, IsArrow F>
  requires std::same_as<typename unfold_view<S, Coalg>::element_type,
                        Maybe<Dom<F>>>
constexpr auto operator>>(const unfold_view<S, Coalg>& q, F f) {
  return q >> MaybeMap<Dom<F>, F>{std::move(f)};
}

/** @brief The count of a query, SQL's @c COUNT: its present elements, the
 *  tally lifted through @c Maybe.  @tparam N the counts.  @param q the
 *  query.  @return how many elements it holds. */
export template <IsCountWord N = std::size_t, IsQuery Q>
constexpr N count(Q q) {
  return std::ranges::fold_left(std::move(q), N{0},
                                lifted(detail_comprehension::Tally<N>{}));
}
/** @brief The sum of a query of counts, SQL's @c SUM: @c + lifted through
 *  @c Maybe.  @param q the query.  @return the sum of its present
 *  elements, modulo the word. */
export template <IsQuery Q, IsCountWord N = typename std::ranges::range_value_t<
                                Q>::value_type>
constexpr N sum(Q q) {
  return std::ranges::fold_left(std::move(q), N{0}, lifted(std::plus<N>{}));
}

// The laws, witnessed.  A filter keeps exactly what the characteristic
// sequence counts; SELECT after WHERE maps the kept elements only: of
// 0, ..., 9 the even ones, each one more, 1 + 3 + 5 + 7 + 9.
static_assert(count(10uz >> chain<std::size_t>() >> where(is_even)) ==
                  count(10uz >> chain<std::size_t>() >> is_even) &&
              count(10uz >> chain<std::size_t>() >> where(is_even)) == 5);
static_assert(sum(10uz >> chain<std::size_t>() >> where(is_even) >>
                  Successor<std::size_t>{}) == 25);

}  // namespace dedekind::category
