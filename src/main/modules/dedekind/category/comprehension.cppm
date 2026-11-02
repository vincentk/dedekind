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
#include <array>
#include <concepts>
#include <cstddef>
#include <functional>
#include <optional>
#include <ranges>
#include <type_traits>
#include <utility>

export module dedekind.category:comprehension;

import :morphism;  // IsArrow, Dom, Cod
import :nno;       // Successor: a witness's arrow
import :chain;     // the chain, FiniteChainSequence, IsCountWord
import :functor;   // Maybe, MaybeMap: the functor's action on the holes
import :periodic;  // is_even, the count of a characteristic sequence, AtMost
import :posetal;   // IsMonotone, IsAntiMonotone, ~: the order laws a rank has

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

/** @brief @c join(p, s), SQL's @c JOIN on a window: the tensor of the
 *  windows of the words of @c p that land in @c s, at every offset where a
 *  window fits.  A constructor, not a materialisation: the join is a
 *  tensor, counted, never enumerated, so it stays inside the counted world
 *  and never meets the bag a join yielding rows would need.
 *  @details Read three ways.  As a @b pullback: the cylinder at offset
 *  @c i is the preimage of @c s under the window @f$\pi_I : \Sigma^N \to
 *  \Sigma^n@f$, the pullback of @f$\pi_I@f$ along the inclusion
 *  @f$s \hookrightarrow \Sigma^n@f$ (@c :pullback); two words agreeing on
 *  the window are related by its @b kernel @b pair (@c IsKernelPair), the
 *  self-join of @c p on @f$\pi_I@f$.  As a @b correlation: the count
 *  slides @c s's indicator, the @b kernel of the convolution, over every
 *  offset of every word, @f$\sum_w \sum_i \chi_s(\pi_{I+i}(w))@f$.  As
 *  a @b query: @c COUNT(*) over the join, its aggregate pushed through it
 *  by R2, Yan and Larson's eager aggregation (VLDB 1995), the window's
 *  count times the free legs', and the offsets summed by translation
 *  invariance (@c HasUnitPeriod).  So @c count(join(p, s)) is
 *  @c occurrences(p, s), and @c exact_count the checked one.
 *  @tparam P the tensor power.  @tparam S the set of words. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
struct WindowJoin {
  P power;
  S inner;
};
/** @param p a tensor power.  @param s a set of words over the same legs.
 *  @return the join of @c p and @c s on the window. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
constexpr WindowJoin<P, S> join(P p, S s) {
  return {std::move(p), std::move(s)};
}
/** @param j a join on the window.  @return its count, @c COUNT(*), modulo
 *  the word: @c occurrences. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
constexpr auto count(const WindowJoin<P, S>& j) {
  return occurrences(j.power, j.inner);
}
/** @param j a join on the window.  @return its count exactly, or none
 *  where the word cannot hold it: @c exact_occurrences. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
constexpr auto exact_count(const WindowJoin<P, S>& j) {
  return exact_occurrences(j.power, j.inner);
}

/** @brief The orders a query is sorted by, the standard's comparisons named
 *  as SQL's @c ASC and @c DESC: the defaults of @c order_by's parameter,
 *  which takes any comparison with sufficient structure, a total preorder on
 *  the rank's values. */
export using ascending = std::less<>;
export using descending = std::greater<>;

/** @brief @c order_by(rank), SQL's @c ORDER @c BY: the stage that sorts a
 *  query by @c rank under @c Order.  @tparam Rank the rank.  @tparam Order
 *  the comparison. */
export template <IsArrow Rank, typename Order>
struct OrderBy {
  Rank rank;
  Order order;
};
/** @tparam Order the comparison, @c ascending by default.  @param rank the
 *  rank.  @return the stage. */
export template <typename Order = ascending, IsArrow Rank>
constexpr OrderBy<Rank, Order> order_by(Rank rank, Order order = {}) {
  return {std::move(rank), std::move(order)};
}
/** @brief @c limit<K>(), SQL's @c LIMIT: the first @c K present elements.
 *  @tparam K the bound. */
export template <std::size_t K>
struct Limit {};
/** @tparam K the bound.  @return the stage. */
export template <std::size_t K>
constexpr Limit<K> limit() {
  return {};
}

namespace detail_comprehension {
/** @brief The trait behind @c AscendsByPosition: an arrow over the
 *  positions whose present elements rise with the position, so that a
 *  query read through it is sorted by construction.  A struct, so that a
 *  family registers by one partial specialisation.  @tparam F the arrow. */
template <typename F>
struct ascends_by_position : std::false_type {};
/** @brief @c where keeps the positions it keeps: a filtered chain is a chain
 *  in the same order. */
template <typename P>
struct ascends_by_position<Where<P>> : std::true_type {};
template <typename F>
concept AscendsByPosition = ascends_by_position<F>::value;

/** @brief The walk's own order already is @c Order's under @c Rank: the
 *  chain descends by @c prior, so its elements fall along it, and a
 *  monotone rank under @c descending, or an antitone one under
 *  @c ascending, keeps them in order.  @c order_by is then the identity. */
template <typename Rank, typename Order>
concept KeptByTheWalk =
    (IsMonotone<Rank> && std::same_as<Order, descending>) ||
    (IsAntiMonotone<Rank> && std::same_as<Order, ascending>);
/** @brief The walk's order is @c Order's reversed: @c order_by is the query
 *  re-rooted on the ascending slice, read upwards. */
template <typename Rank, typename Order>
concept ReversedByTheWalk =
    (IsAntiMonotone<Rank> && std::same_as<Order, descending>) ||
    (IsMonotone<Rank> && std::same_as<Order, ascending>);

/** @brief An element's share of a limit: one if present, none for a hole,
 *  read through @c Maybe's eliminator. */
struct One {
  template <typename A>
  constexpr std::size_t operator()(const A&) const {
    return 1;
  }
};
template <typename T>
constexpr std::size_t present(const Maybe<T>& e) {
  return e.transform(One{}).value_or(0);
}
template <typename T>
constexpr std::size_t present(const T&) {
  return 1;
}

/** @brief The coalgebra of @c limit<K>: the walk, until @c K elements are
 *  present.  One more state, the count so far; O(1), no storage. */
template <std::regular S, std::semiregular Coalg, std::size_t K>
struct Taken {
  Coalg coalg;
  using Yield = std::invoke_result_t<const Coalg&, const S&>;
  using Element = std::remove_cvref_t<decltype(std::declval<Yield>()->first)>;
  constexpr std::optional<std::pair<Element, std::pair<S, std::size_t>>>
  operator()(const std::pair<S, std::size_t>& s) const {
    if (s.second == K) return std::nullopt;
    const Yield y = std::invoke(coalg, s.first);
    if (!y.has_value()) return std::nullopt;
    return std::pair<Element, std::pair<S, std::size_t>>{
        y->first, {y->second, s.second + present(y->first)}};
  }
};

/** @brief @c order_by without a law to read, pending its @c limit: it needs
 *  the whole query, so only the fused @c order_by @c >> @c limit<K> is
 *  offered, never the sort alone (it would need the caller's buffer). */
template <typename Q, typename Rank, typename Order>
struct Pending {
  Q query;
  OrderBy<Rank, Order> by;
};
/** @brief Whether @c a ranks before @c b. */
template <typename Rank, typename Order>
struct Before {
  Rank rank;
  Order order;
  template <typename A>
  constexpr bool operator()(const A& a, const A& b) const {
    return std::invoke(order, std::invoke(rank, a), std::invoke(rank, b));
  }
};
/** @brief One more element into the best @c K so far: the list with it,
 *  sorted by the rank, its last cut where it overflows.  @c K + 1 values on
 *  the stack, sorted by the standard's sort. */
template <typename T, std::size_t K, typename Rank, typename Order>
struct KeepBest {
  Before<Rank, Order> before;
  constexpr AtMost<T, K> operator()(const AtMost<T, K>& best,
                                    const T& x) const {
    AtMost<T, K + 1> all =
        detail_periodic::joined(best, AtMost<T, 1>{}.with(x));
    std::ranges::sort(all.items.begin(), all.items.begin() + all.length,
                      before);
    return std::ranges::fold_left(all | std::views::take(K), AtMost<T, K>{},
                                  detail_periodic::With{});
  }
};
}  // namespace detail_comprehension

/** @brief @c order_by on a filtered chain whose rank keeps the walk's order:
 *  the identity, the query sorted by construction (rewrite 3).
 *  @param q the query.  @return it. */
export template <IsCountWord N, typename F, IsArrow Rank, typename Order>
  requires detail_comprehension::AscendsByPosition<F> &&
           detail_comprehension::KeptByTheWalk<Rank, Order>
constexpr FiniteChainSequence<N, F> operator>>(
    const FiniteChainSequence<N, F>& q, const OrderBy<Rank, Order>&) {
  return q;
}
/** @brief @c order_by on a filtered chain whose rank reverses the walk's
 *  order: the query re-rooted on the ascending slice @c [0, n), read
 *  upwards, the positions where the walk read them downwards (rewrite 3).
 *  @param q the query.  @return it, ascending. */
export template <IsCountWord N, typename F, IsArrow Rank, typename Order>
  requires detail_comprehension::AscendsByPosition<F> &&
           detail_comprehension::ReversedByTheWalk<Rank, Order>
constexpr auto operator>>(const FiniteChainSequence<N, F>& q,
                          const OrderBy<Rank, Order>&) {
  return slice(N{0}, q.seed()) >> q.coalgebra().f;
}
/** @brief @c order_by with no law to read: pending its @c limit.
 *  @param q the query.  @param by the stage.  @return the pending sort. */
export template <std::ranges::input_range Q, IsArrow Rank, typename Order>
constexpr detail_comprehension::Pending<Q, Rank, Order> operator>>(
    Q q, OrderBy<Rank, Order> by) {
  return {std::move(q), std::move(by)};
}

/** @brief @c limit<K> on a query or a sequence: the walk, until @c K
 *  elements are present, lazily (rewrite 1; on a sorted query, rewrite 3's
 *  first @c K).  @param q the query.  @return its first @c K. */
export template <std::regular S, std::semiregular Coalg, std::size_t K>
constexpr auto operator>>(const unfold_view<S, Coalg>& q, Limit<K>) {
  return unfold(std::pair<S, std::size_t>{q.seed(), 0},
                detail_comprehension::Taken<S, Coalg, K>{q.coalgebra()});
}
/** @brief @c order_by @c >> @c limit<K> with no law: fused, one pass keeping
 *  the best @c K in an @c AtMost, never sorting the whole (rewrite 2).
 *  @param p the pending sort.  @return the best @c K, best first. */
export template <typename Q, typename Rank, typename Order, std::size_t K>
constexpr auto operator>>(
    const detail_comprehension::Pending<Q, Rank, Order>& p, Limit<K>) {
  using T = typename std::ranges::range_value_t<Q>::value_type;
  return std::ranges::fold_left(
      p.query, AtMost<T, K>{},
      lifted(detail_comprehension::KeepBest<T, K, Rank, Order>{
          {p.by.rank, p.by.order}}));
}

/** @brief A query's present elements as a value, bounded by its @c limit:
 *  the @c ext boundary, where a list is asked for (#1133).  @param q a
 *  limited query.  @return its elements, on the stack. */
export template <std::regular S, std::semiregular Coalg, std::size_t K>
constexpr auto collect(
    const unfold_view<std::pair<S, std::size_t>,
                      detail_comprehension::Taken<S, Coalg, K>>& q) {
  using E = typename unfold_view<
      std::pair<S, std::size_t>,
      detail_comprehension::Taken<S, Coalg, K>>::element_type;
  using T = typename E::value_type;
  return std::ranges::fold_left(q, AtMost<T, K>{},
                                lifted(detail_periodic::With{}));
}
/** @param best the best @c K, already a value.  @return them. */
export template <typename T, std::size_t K>
constexpr AtMost<T, K> collect(const AtMost<T, K>& best) {
  return best;
}

// The laws, witnessed.  A filter keeps exactly what the characteristic
// sequence counts; SELECT after WHERE maps the kept elements only: of
// 0, ..., 9 the even ones, each one more, 1 + 3 + 5 + 7 + 9.
static_assert(count(10uz >> chain<std::size_t>() >> where(is_even)) ==
                  count(10uz >> chain<std::size_t>() >> is_even) &&
              count(10uz >> chain<std::size_t>() >> where(is_even)) == 5);
static_assert(sum(10uz >> chain<std::size_t>() >> where(is_even) >>
                  Successor<std::size_t>{}) == 25);
// ORDER BY ... LIMIT three ways.  Sorted by construction: a monotone rank
// descending is the walk's own order, an antitone one the walk reversed;
// with no law the fused pass keeps the best K.  Of 0, ..., 9 the even ones:
// the largest two by the identity, by the successor (no law here), and the
// smallest two by the reflection, which reverses the walk.
static_assert(std::ranges::equal(
    collect(10uz >> chain<std::size_t>() >> where(is_even) >>
            order_by<descending>(Identity<std::size_t>{}) >> limit<2>()),
    std::array{8uz, 6uz}));
static_assert(std::ranges::equal(
    collect(10uz >> chain<std::size_t>() >> where(is_even) >>
            order_by<descending>(Successor<std::size_t>{}) >> limit<2>()),
    std::array{8uz, 6uz}));
static_assert(std::ranges::equal(
    collect(10uz >> chain<std::size_t>() >> where(is_even) >>
            order_by<descending>(~Identity<std::size_t>{}) >> limit<2>()),
    std::array{0uz, 2uz}));

}  // namespace dedekind::category
