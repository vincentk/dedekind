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
 * | @c Join     | @c JOIN    | @c p @c >> @c s               |
 * | @c OrderBy  | @c ORDER BY| @c >> @c order_by(rank)     |
 * | @c Take     | @c LIMIT   | @c >> @c take<K>()          |
 * | -           | @c OVER    | @c >> @c windows(n)         |
 * | @c Count    | @c COUNT   | @c count, @c >> @c counting   |
 * | @c Sum      | @c SUM     | @c sum                      |
 *
 * The names are the common ones; where a name is the category theory's,
 * the comprehension takes another: @c take, Haskell's and LINQ's, for SQL's
 * @c LIMIT, since a limit here is the universal construction (@c :limit),
 * and the monad's multiplication is @c μ; SQL's @c JOIN keeps no name of its
 * own, being a power read through a set of words, the bind.
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
#include <cstdint>
#include <cstdlib>
#include <functional>
#include <limits>
#include <optional>
#include <ranges>
#include <span>
#include <type_traits>
#include <utility>
#include <vector>
#include <version>

export module dedekind.category:comprehension;

import :morphism;  // IsArrow, Dom, Cod
import :nno;       // Successor: a witness's arrow
import :chain;     // the chain, FiniteChainSequence, IsCountWord
import :functor;   // Maybe, MaybeMap: the functor's action on the holes
import :discrete;  // constant: a witness's rank
import :lattice;   // Meet: a filter's predicate as a lattice term
import :periodic;  // is_even, the count of a characteristic sequence, AtMost
import :posetal;   // IsMonotone, IsAntiMonotone, ~: the order laws a rank has
import :topoi;     // Subobject: what a predicate classifies
import :pullback;  // as_classified: the classifier's square
import :limit;     // One: the terminal object
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

/** @section comprehension__Foreign_Ranges The caller's ranges
 *  A range of the caller's, a genome's bytes or a series of prices, is read
 *  where it is, never copied, and a stage on it is the standard's own view:
 *  @c r @c >> @c f is @c std::views::transform, @c r @c >> @c windows(n)
 *  is @c std::views::slide.  A spelling in the arrow vocabulary, not a new
 *  type: the types are the standard's, as the witnesses below check.  A
 *  view is taken by value, as Rule 1 asks; a container is read through a
 *  @c std::span or @c std::views::all of it, by the caller, so no stage
 *  borrows a temporary.  The library's own walks keep their @c >>. */
namespace detail_comprehension {
template <typename R>
inline constexpr bool is_walk_v = false;
template <std::regular S, std::semiregular Coalg>
inline constexpr bool is_walk_v<unfold_view<S, Coalg>> = true;
/** @brief A view of the caller's, not one of the library's walks. */
template <typename R>
concept IsForeignView = std::ranges::view<R> && !is_walk_v<R>;

}  // namespace detail_comprehension
/** @brief @c r @c >> @c f on a range of the caller's: @c f on each
 *  element, which is @c std::views::transform.  @param r the view.
 *  @param f the arrow.  @return the standard's transform view. */
export template <detail_comprehension::IsForeignView R, IsArrow F>
  requires std::invocable<const F&, std::ranges::range_reference_t<R>>
constexpr auto operator>>(R r, F f) {
  return std::views::transform(std::move(r), std::move(f));
}

namespace detail_comprehension {
/** @brief Refuses a window of no letters, or wider than a range's distance
 *  type counts: the standard's @c slide has no such view. */
[[noreturn]] constexpr void refuse_window_width() { std::abort(); }
#ifdef __cpp_lib_ranges_slide
/** @brief The windows of @c n: the standard's @c slide. */
template <std::ranges::view R>
  requires std::ranges::forward_range<R>
constexpr auto sliding(R r, std::size_t n) {
  using D = std::ranges::range_difference_t<R>;
  if (n == 0 || n > static_cast<std::size_t>(std::numeric_limits<D>::max()))
    refuse_window_width();
  return std::views::slide(std::move(r), static_cast<D>(n));
}
#else
// libc++ (Homebrew LLVM 23, Apple) ships no std::views::slide yet, where
// libstdc++ (CI) does: the same windows, each the subrange of n from its
// position, spelled with the views libc++ has.  To delete once libc++ has
// slide; the pinning witness below is skipped meanwhile.
template <std::ranges::view R>
struct SubrangeAt {
  R r;
  std::ranges::range_difference_t<R> n;
  constexpr auto operator()(std::ranges::range_difference_t<R> i) const {
    return std::ranges::subrange(std::ranges::begin(r) + i,
                                 std::ranges::begin(r) + i + n);
  }
};
template <std::ranges::view R>
  requires std::ranges::random_access_range<R> && std::ranges::sized_range<R>
constexpr auto sliding(R r, std::size_t n) {
  using D = std::ranges::range_difference_t<R>;
  if (n == 0 || n > static_cast<std::size_t>(std::numeric_limits<D>::max()))
    refuse_window_width();
  const D size = static_cast<D>(std::ranges::size(r));
  const D w = static_cast<D>(n);
  const D windows = w <= size ? size - w + 1 : D{0};
  return std::views::transform(std::views::iota(D{0}, windows),
                               SubrangeAt<R>{std::move(r), w});
}
#endif
}  // namespace detail_comprehension
/** @brief @c windows(n), the stage that reads each position's window of
 *  @c n elements, a position with its context, as a comonad's
 *  @f$\delta@f$ would on an infinite stream; on a finite range the last
 *  @c n - 1 positions have no full window, so it is not a comonad here.
 *  It is @c std::views::slide(n), spelled as a stage. */
export struct Windows {
  std::size_t n;
};
/** @param n the window's length, at least one.  @return the stage. */
export constexpr Windows windows(std::size_t n) {
  if (n == 0) detail_comprehension::refuse_window_width();
  return {n};
}
/** @param r a view of the caller's.  @param w the stage.  @return the
 *  standard's slide view over it. */
export template <detail_comprehension::IsForeignView R>
  requires std::ranges::forward_range<R>
constexpr auto operator>>(R r, Windows w) {
  return detail_comprehension::sliding(std::move(r), w.n);
}

/** @concept IsPrefixKernel
 *  @brief A kernel on the non-empty prefixes of a text: an arrow out of them,
 *  a coKleisli arrow of the history comonad, whose answer at a position may
 *  read all that came before it and nothing after.  @tparam K the kernel.
 *  @tparam T the letters. */
export template <typename K, typename T>
concept IsPrefixKernel = IsArrow<K> && std::same_as<Dom<K>, std::span<const T>>;
namespace detail_comprehension {
/** @brief The kernel at one more prefix. */
template <typename T, IsPrefixKernel<T> K>
struct KernelAt {
  std::span<const T> text;
  K kernel;
  constexpr Cod<K> operator()(std::size_t n) const {
    return kernel(text.first(n));
  }
};
}  // namespace detail_comprehension
/** @brief @c scan(k, s), the extension of the history comonad: a kernel asked
 *  at every non-empty prefix, SQL's window function over @c ROWS @c UNBOUNDED
 *  @c PRECEDING, as @c scan on a @c Path (@c :sequences).  Every position has
 *  a prefix, so unlike a window of @c n this is a comonad on a finite range;
 *  each answer may read the whole prefix, so a kernel that factors through
 *  finitely many states has an overload of its own (@c :automaton).
 *  @param k the kernel.  @param s the text.  @return the answers, lazily. */
export template <typename T, IsPrefixKernel<T> K>
constexpr auto scan(K k, std::span<const T> s) {
  return std::views::iota(1uz, s.size() + 1) |
         std::views::transform(
             detail_comprehension::KernelAt<T, K>{s, std::move(k)});
}
/** @brief The history comonad's counit: a prefix's last letter.  @tparam T the
 *  letters. */
export template <typename T>
struct LastOf {
  using Domain = std::span<const T>;
  using Codomain = T;
  constexpr T operator()(std::span<const T> p) const { return p.back(); }
};
namespace detail_comprehension {
constexpr std::array<std::size_t, 3> history_witness{7, 8, 9};
/** @brief A kernel for the laws: a prefix's length. */
struct LengthOf {
  using Domain = std::span<const std::size_t>;
  using Codomain = std::size_t;
  constexpr std::size_t operator()(std::span<const std::size_t> p) const {
    return p.size();
  }
};
}  // namespace detail_comprehension
// Extending by the counit is the identity, and the counit of an extension is
// the kernel.  Coassociativity is not stated: the extension is a view and its
// kernel takes a span.
static_assert(
    std::ranges::equal(scan(LastOf<std::size_t>{},
                            std::span<const std::size_t>{
                                detail_comprehension::history_witness}),
                       detail_comprehension::history_witness) &&
    std::ranges::equal(scan(detail_comprehension::LengthOf{},
                            std::span<const std::size_t>{
                                detail_comprehension::history_witness}),
                       std::array{1uz, 2uz, 3uz}));

// The functor's laws, read on a range of the caller's: the identity maps
// it to itself, and mapping twice is mapping the composite once.
namespace detail_comprehension {
constexpr std::array<std::size_t, 5> functor_witness{0, 1, 2, 3, 4};
}  // namespace detail_comprehension
static_assert(
    std::ranges::equal(
        std::span<const std::size_t>{detail_comprehension::functor_witness} >>
            Identity<std::size_t>{},
        detail_comprehension::functor_witness) &&
    std::ranges::equal(
        std::span<const std::size_t>{detail_comprehension::functor_witness} >>
            Successor<std::size_t>{} >> is_even,
        std::span<const std::size_t>{detail_comprehension::functor_witness} >>
            (Successor<std::size_t>{} >> is_even)));
// The lazy map is the list functor's action, read extensionally: a letter
// map over the caller's bytes yields what the certified PowerFunctor's
// power(tau) does to the word (an eager functor; the lazy pipeline has no
// single target type, so it is the functor's action by value, not by
// concept).
namespace detail_comprehension {
constexpr std::array<std::byte, 6> gaattc{std::byte{'G'}, std::byte{'A'},
                                          std::byte{'A'}, std::byte{'T'},
                                          std::byte{'T'}, std::byte{'C'}};
constexpr bool lazy_is_the_functor() {
  const LetterMap complement = letter_map("ACGT", "TGCA");
  const std::vector<std::byte> word(gaattc.begin(), gaattc.end());
  return std::ranges::equal(std::span<const std::byte>{gaattc} >> complement,
                            power(complement)(word));
}
}  // namespace detail_comprehension
static_assert(IsFunctor<PowerFunctor> &&
              detail_comprehension::lazy_is_the_functor());
// The spellings are the standard's views, by type.
#ifdef __cpp_lib_ranges_slide
static_assert(std::same_as<
              decltype(std::span<const std::size_t>{} >> windows(6)),
              decltype(std::span<const std::size_t>{} | std::views::slide(6))>);
#endif
static_assert(std::same_as<decltype(std::span<const std::size_t>{} >> is_even),
                           decltype(std::span<const std::size_t>{} |
                                    std::views::transform(is_even))>);

/** @brief @c where(section): the windows a section holds, its membership
 *  as the filter, @c where(sites).  The section stays a tensor (its @c *
 *  is the outer product); only here is it read as a predicate, its
 *  characteristic map, an arrow from a window of its letters: a
 *  @c std::span, since an arrow has one domain and a text is contiguous (a
 *  genome's bytes, Listing 5's carrier).  The membership itself reads any
 *  random-access range; a window of a non-contiguous container is tested
 *  by it directly, not through this stage.  @param s the section.
 *  @return the stage. */
export template <typename P, typename S, IsArrow T>
constexpr auto where(const Symmetric<P, S, T>& s) {
  return where(arrow<std::span<const Dom<T>>, bool>(s));
}
/** @brief @c r @c >> @c s: where the words of a section occur in a range of
 *  the caller's: its windows of the section's length, each kept where the
 *  section holds it.  The section knows its length, so no window of another
 *  arises.  Listing 5's @c dna @c >> @c pali6, the same expression as the join
 *  below read at one sequence.  @param r the view.  @param s the section.
 *  @return the query. */
export template <detail_comprehension::IsForeignView R, typename P, typename S,
                 IsArrow T>
  requires requires(R r, const Symmetric<P, S, T>& s) {
    std::move(r) >> windows(1uz) >> where(s);
  }
constexpr auto operator>>(R r, const Symmetric<P, S, T>& s) {
  return std::move(r) >> windows(static_cast<std::size_t>(s.power.length)) >>
         where(s);
}

// where(p) keeps what p classifies: the subobject {x | p(x)} is the pullback
// of the truth 1 -> Omega along p, by shape (the concept, ⊤ the constant
// arrow), and the filter keeps an element exactly where it is a member.
namespace detail_comprehension {
using Evens = Subobject<std::size_t, Congruent<>>;
struct KeepsWhatItClassifies {
  Evens classified;
  constexpr bool operator()(bool ok, std::size_t x) const {
    return ok && where(is_even)(x).has_value() == classified(x);
  }
};
}  // namespace detail_comprehension
static_assert(
    IsPullback<decltype(as_classified(detail_comprehension::Evens{is_even})),
               Congruent<>, decltype(constant<One>(true))>);
static_assert(
    std::ranges::fold_left(std::views::iota(0uz, 10uz), true,
                           detail_comprehension::KeepsWhatItClassifies{
                               detail_comprehension::Evens{is_even}}));

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

/** @brief @c p @c >> @c s, SQL's @c JOIN on a window:
 *  the tensor of the windows of the words of @c p that land in @c s, at
 *  every offset where a window fits.  A constructor, not a materialisation: the
 * join is a tensor, counted, never enumerated, so it stays inside the counted
 * world and never meets the bag a join yielding rows would need.
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
 *  invariance (@c HasUnitPeriod).  So its @c count is
 *  @c occurrences(p, s), and @c exact_count the checked one.
 *  @tparam P the tensor power.  @tparam S the set of words. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
struct WindowJoin {
  P power;
  S inner;
};
/** @param p a tensor power.  @param s a section over the same legs.
 *  @return the join of @c p and @c s on the window: @c s's words at every
 *  offset of every word of @c p, counted by its laws, never enumerated. */
export template <HasNNOStep C, IsArrow F, IsCountWord N, typename P, typename S,
                 IsArrow T>
  requires requires(const Power<C, F, N>& p, const Symmetric<P, S, T>& s) {
    occurrences(p, s);
  }
constexpr WindowJoin<Power<C, F, N>, Symmetric<P, S, T>> operator>>(
    const Power<C, F, N>& p, const Symmetric<P, S, T>& s) {
  return {p, s};
}
/** @brief The offsets a window takes, as a tensor: the chain of @c m, its
 *  count @c m, one factor @f$m^1@f$.  Its outer product with a tensor is the
 *  copower @f$m \cdot X@f$, @c m copies, the action of @f$\mathbb{N}@f$ on the
 *  counts (@c :action; Listing 4's @c concat on copies).  Fubini's sum over the
 * offsets, every summand the same (period 1, translation invariance), is this
 * factor of the product.  @tparam N the counts. */
export template <IsCountWord N>
struct Offsets {
  Factor<N> factor;  ///< m as m^1, or as 2^w where m is 2^w, past the word
};
/** @param o the offsets.  @return their count, one factor. */
export template <IsCountWord N>
constexpr AtMost<Factor<N>, 1> count_factors(const Offsets<N>& o) {
  return AtMost<Factor<N>, 1>{}.with(o.factor);
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
  // N - n + 1 offsets, kept symbolic where they are 2^w and leave the word
  const N below = static_cast<N>(j.power.length - n);
  const Factor<N> m =
      below == std::numeric_limits<N>::max()
          ? Factor<N>{N{2}, static_cast<N>(std::numeric_limits<N>::digits)}
          : Factor<N>{static_cast<N>(below + 1), N{1}};
  return Offsets<N>{m} * cylinder(j.power, window<N>(N{0}, n), j.inner);
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

/** @brief @c join_with(p), a set of words to its join with @c p on the
 *  window: the join curried in the set, an arrow between tensors, its
 *  exponential transpose (@c curry, @c :cartesian); each join the pullback
 *  along the window (@c IsPullback, @c :pullback).
 *  @tparam P the tensor power.  @tparam S the sets. */
export template <typename P, typename S>
  requires requires(const P& p, const S& s) { occurrences(p, s); }
struct JoinWith {
  using Domain = S;
  using Codomain = WindowJoin<P, S>;
  P power;
  constexpr Codomain operator()(const S& s) const { return {power, s}; }
};
/** @brief @c counting, a counted tensor to its count: the reading that ends
 *  a pipeline of arrows, as @c count ends a query.  The cardinality, strong
 *  monoidal from @f$\otimes@f$ to @f$\times@f$ (the box law read on counts),
 *  modulo the word: a ranking by it is a ranking of the exact counts only
 *  while they fit, which the law's plan, evaluating no count, does not need
 *  and the fused pass does (#1134).  @tparam J the tensor. */
export template <typename J>
struct Counting {
  using Domain = J;
  using Codomain = decltype(count(std::declval<const J&>()));
  constexpr Codomain operator()(const J& j) const { return count(j); }
};
namespace detail_comprehension {
template <typename P>
struct JoinWithStage {
  P power;
};
struct CountingStage {};
}  // namespace detail_comprehension
/** @param p a tensor power.  @return the stage joining a set with it. */
export template <typename P>
constexpr detail_comprehension::JoinWithStage<P> join_with(P p) {
  return {std::move(p)};
}
/** @brief The stage counting what precedes it. */
export inline constexpr detail_comprehension::CountingStage counting{};
/** @param fam a family of sets of words.  @param st the stage.  @return the
 *  family, each set joined with the power. */
export template <IsArrow Fam, typename P>
  requires requires(const P& p, const Cod<Fam>& s) { occurrences(p, s); }
constexpr auto operator>>(const Fam& fam,
                          const detail_comprehension::JoinWithStage<P>& st) {
  return fam >> JoinWith<P, Cod<Fam>>{st.power};
}
/** @param f an arrow into counted tensors.  @return it, then the count. */
export template <IsArrow F>
  requires requires(const Cod<F>& j) { count(j); }
constexpr auto operator>>(const F& f, detail_comprehension::CountingStage) {
  return f >> Counting<Cod<F>>{};
}
/** @brief @c q @c >> @c counting, the stage form of @c count(q): the fold
 *  that ends a query, in the query's own rhythm.  @param q the query.
 *  @return its count. */
export template <IsQuery Q>
constexpr std::size_t operator>>(Q q, detail_comprehension::CountingStage) {
  return count(std::move(q));
}
namespace detail_comprehension {
/** @brief The count of a query, as @c Maybe's action: a query that may not
 *  be, counted where it is. */
struct Counted {
  template <IsQuery Q>
  constexpr std::size_t operator()(Q q) const {
    return count(std::move(q));
  }
};
}  // namespace detail_comprehension
/** @brief A query that may not be (a plan past its capacity), counted: the
 *  count lifted through @c Maybe, nothing where the query is nothing.  The
 *  monad carries the uncertainty to the end, so no caller cases on it.
 *  @param q the query, or nothing.  @return its count, or nothing. */
export template <IsQuery Q>
constexpr Maybe<std::size_t> count(const Maybe<Q>& q) {
  return q.transform(detail_comprehension::Counted{});
}
/** @brief The stage form of the same. */
export template <IsQuery Q>
constexpr Maybe<std::size_t> operator>>(const Maybe<Q>& q,
                                        detail_comprehension::CountingStage) {
  return count(q);
}
/** @brief A rank whose values do not grow along the even lengths, so that
 *  ranking the even lengths descending reads them upwards.  Derived, not
 *  registered per rank: a family's occurrences in the words of length
 *  @c N are @f$(N - n + 1)@f$ offsets times its density times
 *  @f$|\Sigma|^N@f$, a product of non-negative sequences that do not grow,
 *  which does not grow either.  The product law, stated here for the join's
 *  exact count; a general one belongs in @c :posetal, on an exact carrier or
 *  on an extent, since a word's @c × wraps and an antitone product past it
 *  is not antitone (#1134).
 *  @tparam Rank the rank. */
export template <typename Rank>
struct antitone_on_even_lengths : std::false_type {};
template <typename Fam, typename P, typename S, typename J>
  requires density_falls_on_even_lengths<Fam>::value
struct antitone_on_even_lengths<
    Compose<Compose<Fam, JoinWith<P, S>>, Counting<J>>> : std::true_type {};

/** @brief The orders a query is sorted by, the standard's comparisons named
 *  as SQL's @c ASC and @c DESC: the defaults of @c order_by's parameter,
 *  which takes any comparison with sufficient structure, a total preorder on
 *  the rank's values. */
export using ascending = std::less<>;
export using descending = std::greater<>;

/** @brief @c order_by(rank), SQL's @c ORDER @c BY: the stage that sorts a
 *  query by @c rank under @c Order, a strict weak order on the ranks.  Ties
 *  are SQL's: which of the tied elements a @c take keeps, and in which
 *  order, is the plan's (a law's rewrite reads them in the walk's order or
 *  its reversal, the fused pass in the walk's); a rank without ties orders
 *  alike under every plan.  @tparam Rank the rank.  @tparam Order the
 *  comparison. */
export template <IsArrow Rank, typename Order>
  requires std::strict_weak_order<const Order&, Cod<Rank>, Cod<Rank>>
struct OrderBy {
  Rank rank;
  Order order;
};
/** @tparam Order the comparison, @c ascending by default.  @param rank the
 *  rank.  @return the stage. */
export template <typename Order = ascending, IsArrow Rank>
  requires std::strict_weak_order<const Order&, Cod<Rank>, Cod<Rank>>
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

/** @brief The value a query's element carries: the @c Maybe's, or the
 *  element itself in a plain sequence. */
template <typename E>
struct element_of {
  using type = E;
};
template <typename T>
struct element_of<Maybe<T>> {
  using type = T;
};
/** @brief The coalgebra reading a bounded list position by position, each
 *  element present: a value on the stack walked as any query, so every
 *  stage composes after it. */
template <std::semiregular T, std::size_t K>
struct Read {
  AtMost<T, K> list;
  constexpr std::optional<std::pair<Maybe<T>, std::size_t>> operator()(
      std::size_t i) const {
    if (i >= list.size()) return std::nullopt;
    return std::pair<Maybe<T>, std::size_t>{Maybe<T>{list.items[i]}, i + 1};
  }
};
/** @param list a bounded list.  @return it as a query. */
template <std::semiregular T, std::size_t K>
constexpr unfold_view<std::size_t, Read<T, K>> walk(const AtMost<T, K>& list) {
  return unfold(std::size_t{0}, Read<T, K>{list});
}
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
/** @brief One more element into the best @c K so far, sorted by the rank:
 *  inserted after those it does not rank before, so ties keep the walk's
 *  order, and the last cut where the list overflows.  O(K) on the stack. */
template <typename T, std::size_t K, typename Rank, typename Order>
struct KeepBest {
  Before<Rank, Order> before;
  constexpr AtMost<T, K> operator()(const AtMost<T, K>& best,
                                    const T& x) const {
    const auto at = static_cast<std::size_t>(
        std::ranges::upper_bound(best, x, before) - best.begin());
    if (at == K) return best;
    const AtMost<T, K> head = std::ranges::fold_left(
        best | std::views::take(at), AtMost<T, K>{}, detail_periodic::With{});
    return std::ranges::fold_left(
        best | std::views::drop(at) | std::views::take(K - at - 1),
        head.with(x), detail_periodic::With{});
  }
};
}  // namespace detail_comprehension

/** @brief @c order_by on a filtered chain whose rank keeps the walk's order:
 *  the identity, the query sorted by construction (rewrite 3).
 *  @param q the query.  @return it. */
export template <IsCountWord N, typename F, IsArrow Rank, typename Order>
  requires std::same_as<Dom<Rank>, N> &&
           detail_comprehension::AscendsByPosition<F> &&
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
  requires std::same_as<Dom<Rank>, N> &&
           detail_comprehension::AscendsByPosition<F> &&
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
 *  pass keeping the best @c K, the elements present.  The pending query is
 *  owned, so a range iterable only when mutable folds too.  @param p the
 *  pending sort.  @return the best @c K, best first, as a query. */
template <std::size_t K, typename Q, typename Rank, typename Order>
constexpr auto fused_take(Pending<Q, Rank, Order> p) {
  using E = std::ranges::range_value_t<Q>;
  using T = typename element_of<E>::type;
  const KeepBest<T, K, Rank, Order> keep{{p.by.rank, p.by.order}};
  if constexpr (IsMaybe<E>) {
    return walk(std::ranges::fold_left(p.query, AtMost<T, K>{}, lifted(keep)));
  } else {
    return walk(std::ranges::fold_left(p.query, AtMost<T, K>{}, keep));
  }
}
/** @brief Whether a rank of lengths stays exact from length @c l on: its
 *  values do not grow there, so the largest is at @c l, and it fits the
 *  rank's word, the count's, where its factors say so (@c bits_needed): the
 *  product law's extent, decided by one count, before any is compared. */
template <IsArrow F, typename J, IsCountWord N>
constexpr bool fits_the_word(const Compose<F, Counting<J>>& rank, N l) {
  return bits_needed(count_factors(rank.f(l))) <=
         static_cast<std::size_t>(
             std::numeric_limits<Cod<Counting<J>>>::digits);
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
constexpr auto operator>>(detail_comprehension::Pending<Q, Rank, Order> p,
                          Take<K>) {
  return detail_comprehension::fused_take<K>(std::move(p));
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
export template <IsCountWord N, typename Rank, std::size_t K>
  requires std::same_as<Dom<Rank>, N> && antitone_on_even_lengths<Rank>::value
constexpr auto operator>>(
    const detail_comprehension::Pending<
        unfold_view<std::optional<N>,
                    detail_chain::SliceStep<N, Where<Congruent<N>>>>,
        Rank, descending>& p,
    Take<K>) {
  const Congruent<N> c = p.query.coalgebra().f.p;
  const N hi = p.query.coalgebra().hi;
  const N lo = p.query.seed().value_or(hi);
  const N m = c.modulus;
  const N r = c.residue;
  // no member: a residue at or past the modulus, or an empty slice
  if (m == 0 || r >= m || lo >= hi) {
    return detail_comprehension::walk(AtMost<N, K>{});
  }
  if (m % 2 != 0) return detail_comprehension::fused_take<K>(p);
  // the first member, by subtraction, so nothing wraps near the word's top
  const N below = static_cast<N>(lo % m);
  const N d =
      r >= below ? static_cast<N>(r - below) : static_cast<N>(m - (below - r));
  if (d >= hi - lo) return detail_comprehension::walk(AtMost<N, K>{});
  const N first = static_cast<N>(lo + d);
  // the law is the exact counts': it holds while they fit the word, which
  // the largest, the first member's, decides (the product law's extent)
  if (!detail_comprehension::fits_the_word(p.by.rank, first)) {
    return detail_comprehension::fused_take<K>(p);
  }
  const N members = static_cast<N>((hi - first - 1) / m + 1);
  const N taken = members < N{K} ? members : N{K};
  return detail_comprehension::walk(std::ranges::fold_left(
      slice(N{0}, taken) >> detail_comprehension::Progression<N>{m, first},
      AtMost<N, K>{}, detail_periodic::With{}));
}

namespace detail_comprehension {
/** @brief The most elements a walk presents: a @c take's @c K, or a bounded
 *  list's, kept by each @c SELECT and @c WHERE after it (a map presents no
 *  more than it reads).  No member where nothing bounds the walk. */
template <typename Coalg>
struct bound_of {};
template <std::regular S, std::semiregular Coalg, std::size_t K>
struct bound_of<Taken<S, Coalg, K>> : std::integral_constant<std::size_t, K> {};
template <std::semiregular T, std::size_t K>
struct bound_of<Read<T, K>> : std::integral_constant<std::size_t, K> {};
template <std::regular S, std::semiregular Coalg, IsArrow F>
struct bound_of<Mapped<S, Coalg, F>> : bound_of<Coalg> {};
template <typename Coalg>
concept IsBounded = requires { bound_of<Coalg>::value; };
}  // namespace detail_comprehension
/** @brief A query's present elements as a value, bounded by its @c take:
 *  the @c ext boundary, where a list is asked for (#1133).  A plain
 *  sequence's elements as they are, a query's unwrapped.  @param q a
 *  bounded query or sequence.  @return its elements, on the stack. */
export template <std::regular S, std::semiregular Coalg>
  requires detail_comprehension::IsBounded<Coalg>
constexpr auto collect(const unfold_view<S, Coalg>& q) {
  using E = typename unfold_view<S, Coalg>::element_type;
  using T = typename detail_comprehension::element_of<E>::type;
  constexpr std::size_t K = detail_comprehension::bound_of<Coalg>::value;
  if constexpr (detail_comprehension::IsMaybe<E>) {
    return std::ranges::fold_left(q, AtMost<T, K>{},
                                  lifted(detail_periodic::With{}));
  } else {
    return std::ranges::fold_left(q, AtMost<T, K>{}, detail_periodic::With{});
  }
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
// A plain chain ranks and takes as a query does; a ranked, taken query takes
// again (its result is still a walk); and a plain take collects as itself.
static_assert(std::ranges::equal(collect(3uz >> chain<std::size_t>() >>
                                         order_by(Identity<std::size_t>{}) >>
                                         take<2>()),
                                 std::array{0uz, 1uz}));
static_assert(
    std::ranges::equal(collect(10uz >> chain<std::size_t>() >> where(is_even) >>
                               order_by<descending>(Successor<std::size_t>{}) >>
                               take<2>() >> take<1>()),
                       std::array{8uz}));
static_assert(std::ranges::equal(
    collect(3uz >> chain<std::size_t>() >> take<2>()), std::array{2uz, 1uz}));
// Ties under the fused pass keep the walk's order: of 9, ..., 0 ranked by
// evenness, the even ones tie, and the first three read are kept.
static_assert(std::ranges::equal(collect(10uz >> chain<std::size_t>() >>
                                         order_by<descending>(is_even) >>
                                         take<3>()),
                                 std::array{8uz, 6uz, 4uz}));
// The bound survives SELECT and WHERE after the take: each presents no more.
static_assert(std::ranges::equal(collect(3uz >> chain<std::size_t>() >>
                                         take<2>() >> Successor<std::size_t>{}),
                                 std::array{3uz, 2uz}));
static_assert(
    std::ranges::equal(collect(10uz >> chain<std::size_t>() >> where(is_even) >>
                               order_by<descending>(Successor<std::size_t>{}) >>
                               take<2>() >> where(Congruent<>{4, 0})),
                       std::array{8uz}));
// A rank on another word than the query's would read it converted, so its
// order law, stated on its own domain, rewrites nothing: the identity on
// uint8_t leaves the query pending its comparisons, as one with no law.
static_assert(
    !std::same_as<decltype(10uz >> chain<std::size_t>() >> where(is_even) >>
                           order_by<descending>(Identity<std::uint8_t>{})),
                  decltype(10uz >> chain<std::size_t>() >> where(is_even))>);
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
