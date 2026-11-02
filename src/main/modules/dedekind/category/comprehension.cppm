/**
 * @file dedekind/category/comprehension.cppm
 * @partition :comprehension
 * @brief Monad comprehensions over a finite chain: the queries of SQL and
 *        LINQ, @c where, @c select, @c order_by and @c take, and the folds
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
 * | @c Join     | @c JOIN    | @c join(p, s)               |
 * | @c OrderBy  | @c ORDER BY| @c >> @c order_by(rank)     |
 * | @c Take     | @c LIMIT   | @c >> @c take<K>()          |
 * | @c Count    | @c COUNT   | @c count                    |
 * | @c Sum      | @c SUM     | @c sum                      |
 *
 * The names are the common ones; where a name is the category theory's,
 * the comprehension takes another: @c take, Haskell's and LINQ's, for SQL's
 * @c LIMIT, since a limit here is the universal construction (@c :limit),
 * and the monad's multiplication is @c μ, so that @c join is SQL's.
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
import :discrete;  // constant: a witness's rank
import :lattice;   // Meet: a filter's predicate as a lattice term
import :periodic;  // is_even, the count of a characteristic sequence, AtMost
import :posetal;   // IsMonotone, IsAntiMonotone, ~: the order laws a rank has
import :kleisli;   // κ, maybe_functor's Kleisli triple: the holes' monad

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
template <typename T>
concept IsMaybe = is_maybe_v<T>;
/** @brief A stage that reads a query's elements: an arrow out of the type
 *  its present elements have. */
template <typename Q, typename F>
concept ReadsItsElements =
    std::same_as<typename Q::element_type, Maybe<Dom<F>>>;
/** @brief A Kleisli arrow into @c Maybe extended to the elements of a
 *  query, @f$\kappa(-, f) : \mathit{Maybe}\,A \to \mathit{Maybe}\,B@f$.
 *  @tparam F the Kleisli arrow. */
template <IsArrow F>
struct Extended {
  using Domain = Maybe<Dom<F>>;
  using Codomain = Cod<F>;
  F f;
  constexpr Codomain operator()(const Domain& ma) const { return κ(ma, f); }
};
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
  requires detail_comprehension::ReadsItsElements<unfold_view<S, Coalg>, F>
constexpr auto operator>>(const unfold_view<S, Coalg>& q, F f) {
  return q >> MaybeMap<Dom<F>, F>{std::move(f)};
}
/** @brief @c >> @c f on a query with @c f a Kleisli arrow into @c Maybe, a
 *  second @c where among them: Kleisli composition, @c f extended by
 *  @c Maybe's @c κ (@c :kleisli), so the holes do not nest.  Filter fusion,
 *  @c where(p) @c >> @c where(q) @c = @c where(p @c ∧ @c q), is then the
 *  monad's associativity, not a rewrite of this partition's.
 *  @param q the query.  @param f the Kleisli arrow. */
export template <std::regular S, std::semiregular Coalg, IsArrow F>
  requires detail_comprehension::ReadsItsElements<unfold_view<S, Coalg>, F> &&
           detail_comprehension::IsMaybe<Cod<F>>
constexpr auto operator>>(const unfold_view<S, Coalg>& q, F f) {
  return q >> detail_comprehension::Extended<F>{std::move(f)};
}

/** @brief @c where(p) @c >> @c where(q) on a chain, fused by type: one
 *  @c where whose predicate is the lattice term @c p @c & @c q, the meet in the
 *  Boolean algebra of the chain's subobjects (@c :lattice's @c Meet).  The
 *  filter is then a term the lattice's laws and the periodic counts read,
 *  not two walks; where @c p @c & @c q is not formed, the Kleisli composite
 *  above takes the query.  @param q the filtered chain.  @param w the second
 *  filter.  @return the chain under one filter. */
export template <IsCountWord N, typename P, typename Q>
  requires requires(const P& p, const Q& q) {
    { p & q } -> IsArrow;
    requires std::same_as<Cod<decltype(p& q)>, bool>;
  }
constexpr auto operator>>(const FiniteChainSequence<N, Where<P>>& q,
                          const Where<Q>& w) {
  return q.seed() >> chain<N>() >> where(q.coalgebra().f.p & w.p);
}
/** @brief The count of a chain under a periodic filter: the count of the
 *  filter's characteristic sequence, which @c :periodic takes period by
 *  period (the lcm for a meet), no walk of the query.  The law
 *  @c count(where(p)) @c = @c count(χ_p), used as the rewrite.
 *  @param q the filtered chain.  @return how many elements it keeps. */
export template <IsCountWord N, HasPeriod P>
  requires std::same_as<Cod<P>, bool>
constexpr N count(const FiniteChainSequence<N, Where<P>>& q) {
  return count(q.seed() >> chain<N>() >> q.coalgebra().f.p);
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
/** @brief The offsets a window takes, as a tensor: the chain of @c m, its
 *  count @c m, one factor @f$m^1@f$.  Fubini's sum over the offsets, every
 *  summand the same (period 1, translation invariance), is this factor of
 *  the product.  @tparam N the counts. */
export template <IsCountWord N>
struct Offsets {
  N count;
};
/** @param o the offsets.  @return their count, one factor. */
export template <IsCountWord N>
constexpr AtMost<Factor<N>, 1> count_factors(const Offsets<N>& o) {
  return AtMost<Factor<N>, 1>{}.with(Factor<N>{o.count, N{1}});
}
/** @brief The plan of a join on the window, @c EXPLAIN: the offsets times
 *  the cylinder at the first, an outer product, the cylinder in turn its
 *  selection pushed down (@c pushed_down).  Fubini, periodicity and the box
 *  law compose into it; none of the three is stated for joins.  Its type is
 *  the plan, so a test asserts it.  Defined where a window fits.
 *  @param j the join.  @return its plan. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
constexpr auto explain(const WindowJoin<P, S>& j) {
  const auto n = detail_periodic::power_of(j.inner).length;
  using N = std::remove_cvref_t<decltype(n)>;
  return Offsets<N>{static_cast<N>(j.power.length - n + 1)} *
         cylinder(j.power, window<N>(N{0}, n), j.inner);
}
/** @brief The factors of a join's count: its plan's, the box law throughout,
 *  with no sum over the offsets left; none where no window fits.
 *  @param j the join.  @return its count, factored. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
constexpr auto count_factors(const WindowJoin<P, S>& j) {
  using L = decltype(count_factors(explain(j)));
  using N = detail_periodic::word_of_t<L>;
  if (detail_periodic::power_of(j.inner).length > j.power.length) {
    return L{}.with(Factor<N>{N{0}, N{1}});
  }
  return count_factors(explain(j));
}
/** @param j a join on the window.  @return its count, @c COUNT(*), modulo
 *  the word: @c occurrences. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
constexpr auto count(const WindowJoin<P, S>& j) {
  return count(count_factors(j));
}
/** @param j a join on the window.  @return its count exactly, or none
 *  where the word cannot hold it: @c exact_occurrences. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
constexpr auto exact_count(const WindowJoin<P, S>& j) {
  return exact_occurrences(j.power, j.inner);
}

/** @brief A rank of the lengths: @f$n \mapsto@f$ how often the family's set
 *  of length @c n occurs in the words of @c p, @c COUNT(*) over the join on
 *  the window, grouped by the length.  @tparam P the tensor power.
 *  @tparam Fam the family, a set of words per length. */
export template <typename P, IsArrow Fam>
struct OccurrencesOfLength {
  using Domain = Dom<Fam>;
  using Codomain = Dom<Fam>;
  P power;
  Fam family;
  constexpr Codomain operator()(Domain n) const {
    return count(join(power, family(n)));
  }
};
/** @param p a tensor power.  @param fam a family of sets of words, by
 *  length.  @return the rank of the lengths by their occurrences. */
export template <typename P, IsArrow Fam>
constexpr OccurrencesOfLength<P, Fam> occurrences_of_length(P p, Fam fam) {
  return {std::move(p), std::move(fam)};
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
/** @brief @c take<K>(), SQL's @c LIMIT: the first @c K present elements.
 *  @tparam K the bound. */
export template <std::size_t K>
struct Take {};
/** @tparam K the bound.  @return the stage. */
export template <std::size_t K>
constexpr Take<K> take() {
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
    ((IsAntiMonotone<Rank> && std::same_as<Order, descending>) ||
     (IsMonotone<Rank> && std::same_as<Order, ascending>)) &&
    !KeptByTheWalk<Rank, Order>;  // a constant rank keeps the walk

/** @brief An element's share of a @c take: one if present, none for a hole,
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

/** @brief The coalgebra of @c take<K>: the walk, until @c K elements are
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

/** @brief One more element into a bounded query, present. */
struct WithPresent {
  template <std::semiregular T, std::size_t K>
  constexpr AtMost<Maybe<T>, K> operator()(const AtMost<Maybe<T>, K>& q,
                                           const T& x) const {
    return q.with(Maybe<T>{x});
  }
};
/** @brief @c order_by without a law to read, pending its @c take: it needs
 *  the whole query, so only the fused @c order_by @c >> @c take<K> is
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
/** @brief @c order_by with no law to read: pending its @c take.
 *  @param q the query.  @param by the stage.  @return the pending sort. */
export template <std::ranges::input_range Q, IsArrow Rank, typename Order>
constexpr detail_comprehension::Pending<Q, Rank, Order> operator>>(
    Q q, OrderBy<Rank, Order> by) {
  return {std::move(q), std::move(by)};
}

/** @brief @c take<K> on a query or a sequence: the walk, until @c K
 *  elements are present, lazily (rewrite 1; on a sorted query, rewrite 3's
 *  first @c K).  @param q the query.  @return its first @c K. */
export template <std::regular S, std::semiregular Coalg, std::size_t K>
constexpr auto operator>>(const unfold_view<S, Coalg>& q, Take<K>) {
  return unfold(std::pair<S, std::size_t>{q.seed(), 0},
                detail_comprehension::Taken<S, Coalg, K>{q.coalgebra()});
}
namespace detail_comprehension {
/** @brief The fused plan of @c order_by @c >> @c take<K> with no law: one
 *  pass keeping the best @c K, the elements present.  @param p the pending
 *  sort.  @return the best @c K, best first, as a query. */
template <std::size_t K, typename Q, typename Rank, typename Order>
constexpr auto fused_take(const Pending<Q, Rank, Order>& p) {
  using T = typename std::ranges::range_value_t<Q>::value_type;
  return std::ranges::fold_left(
      std::ranges::fold_left(
          p.query, AtMost<T, K>{},
          lifted(detail_comprehension::KeepBest<T, K, Rank, Order>{
              {p.by.rank, p.by.order}})),
      AtMost<Maybe<T>, K>{}, detail_comprehension::WithPresent{});
}
/** @brief The progression @f$j \mapsto m j + r@f$, a residue class's members
 *  in order: a congruence filter on a chain read as a chain of its own. */
template <IsCountWord N>
struct Progression {
  using Domain = N;
  using Codomain = N;
  N step;
  N first;
  constexpr N operator()(N j) const { return static_cast<N>(first + step * j); }
};
}  // namespace detail_comprehension
/** @brief @c order_by @c >> @c take<K> with no law: fused, one pass keeping
 *  the best @c K in an @c AtMost, never sorting the whole (rewrite 2).  A
 *  query still, each element present, so its count is its rows' and
 *  @c collect makes the plain list.  @param p the pending sort.
 *  @return the best @c K, best first. */
export template <typename Q, typename Rank, typename Order, std::size_t K>
constexpr auto operator>>(
    const detail_comprehension::Pending<Q, Rank, Order>& p, Take<K>) {
  return detail_comprehension::fused_take<K>(p);
}

/** @brief The lengths in a residue class, ranked by their occurrences, the
 *  first @c K: one plan compiled, the even case, and the run time only
 *  reindexing into it.  A family whose density falls along the even lengths
 *  ranks them by construction, the shortest first (the walk read upwards,
 *  rewrite 3), so the first @c K need no rank evaluated at all.  A residue
 *  class of even modulus holds lengths of one parity: even ones are the
 *  case; odd ones occur nowhere (the reversal pairs the letters, and no
 *  even alphabet fixes them all), all ranked alike.  Either way the class's
 *  members in order are the plan, a progression: padded into the class by
 *  @f$j \mapsto m j + r@f$, unpadded by reading the lengths back.  An odd
 *  modulus mixes the parities and takes the fused pass.
 *  @param p the pending sort.  @return the first @c K lengths. */
export template <IsCountWord N, typename P, typename Fam, std::size_t K>
  requires antitone_on_even_lengths<Fam>::value
constexpr AtMost<Maybe<N>, K> operator>>(
    const detail_comprehension::Pending<
        unfold_view<std::optional<N>,
                    detail_chain::SliceStep<N, Where<Congruent<N>>>>,
        OccurrencesOfLength<P, Fam>, descending>& p,
    Take<K>) {
  const Congruent<N> c = p.query.coalgebra().f.p;
  if (c.modulus % 2 != 0) return detail_comprehension::fused_take<K>(p);
  const N hi = p.query.coalgebra().hi;
  const N lo = p.query.seed().value_or(hi);
  const N first = static_cast<N>(
      lo + (c.modulus + c.residue % c.modulus - lo % c.modulus) % c.modulus);
  const N members =
      first < hi ? static_cast<N>((hi - first - 1) / c.modulus + 1) : N{0};
  const N taken = members < N{K} ? members : N{K};
  return std::ranges::fold_left(
      slice(N{0}, taken) >>
          detail_comprehension::Progression<N>{c.modulus, first},
      AtMost<Maybe<N>, K>{}, detail_comprehension::WithPresent{});
}

/** @brief A query's present elements as a value, bounded by its @c take:
 *  the @c ext boundary, where a list is asked for (#1133).  @param q a
 *  taken query.  @return its elements, on the stack. */
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
/** @param best the best @c K, a query already on the stack.  @return its
 *  elements as a plain list. */
export template <typename T, std::size_t K>
constexpr AtMost<T, K> collect(const AtMost<Maybe<T>, K>& best) {
  return std::ranges::fold_left(best, AtMost<T, K>{},
                                lifted(detail_periodic::With{}));
}

// The laws, witnessed.  A filter keeps exactly what the characteristic
// sequence counts; SELECT after WHERE maps the kept elements only: of
// 0, ..., 9 the even ones, each one more, 1 + 3 + 5 + 7 + 9.
static_assert(count(10uz >> chain<std::size_t>() >> where(is_even)) ==
                  count(10uz >> chain<std::size_t>() >> is_even) &&
              count(10uz >> chain<std::size_t>() >> where(is_even)) == 5);
static_assert(sum(10uz >> chain<std::size_t>() >> where(is_even) >>
                  Successor<std::size_t>{}) == 25);
// The holes' monad is :kleisli's Maybe, and WHERE after WHERE its Kleisli
// composition: of 0, ..., 29 the even multiples of three, as the periodic
// meet counts them.
static_assert(
    IsKleisliTriple<maybe_functor<std::size_t>, std::size_t, std::size_t>);
// Two filters on a chain are one, their predicate the lattice's meet, and
// its count the periodic one (the lcm of 2 and 3), the walk never taken.
static_assert(
    std::same_as<decltype(30uz >> chain<std::size_t>() >> where(is_even) >>
                          where(Congruent<>{3, 0})),
                 FiniteChainSequence<std::size_t,
                                     Where<Meet<Congruent<>, Congruent<>>>>>);
static_assert(count(30uz >> chain<std::size_t>() >> where(is_even) >>
                    where(Congruent<>{3, 0})) ==
                  count(30uz >> chain<std::size_t>() >>
                        (is_even & Congruent<>{3, 0})) &&
              count(30uz >> chain<std::size_t>() >> where(is_even) >>
                    where(Congruent<>{3, 0})) == 5);
// A constant rank is monotone and antitone at once: it keeps the walk, the
// identity rewrite, not its reversal.  And the fused take is still a query,
// counted by its rows, as the lazy one is.
static_assert(std::ranges::equal(
    collect(10uz >> chain<std::size_t>() >> where(is_even) >>
            order_by<descending>(constant<std::size_t>(0uz)) >> take<2>()),
    std::array{8uz, 6uz}));
static_assert(count(3uz >> chain<std::size_t>() >> where(is_even) >>
                    order_by<descending>(Successor<std::size_t>{}) >>
                    take<2>()) ==
              count(3uz >> chain<std::size_t>() >> where(is_even) >>
                    order_by<descending>(Identity<std::size_t>{}) >>
                    take<2>()));
// ORDER BY ... LIMIT three ways.  Sorted by construction: a monotone rank
// descending is the walk's own order, an antitone one the walk reversed;
// with no law the fused pass keeps the best K.  Of 0, ..., 9 the even ones:
// the largest two by the identity, by the successor (no law here), and the
// smallest two by the reflection, which reverses the walk.
static_assert(std::ranges::equal(
    collect(10uz >> chain<std::size_t>() >> where(is_even) >>
            order_by<descending>(Identity<std::size_t>{}) >> take<2>()),
    std::array{8uz, 6uz}));
static_assert(std::ranges::equal(
    collect(10uz >> chain<std::size_t>() >> where(is_even) >>
            order_by<descending>(Successor<std::size_t>{}) >> take<2>()),
    std::array{8uz, 6uz}));
static_assert(std::ranges::equal(
    collect(10uz >> chain<std::size_t>() >> where(is_even) >>
            order_by<descending>(~Identity<std::size_t>{}) >> take<2>()),
    std::array{0uz, 2uz}));

}  // namespace dedekind::category
