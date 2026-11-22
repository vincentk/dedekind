// Negative control for rules/rule3-bounded-fold.query: the gate must flag
// ten standard folds inside the jlt_listing namespace: three at namespace
// scope, one with no bound stated, one folding a range other than the bounded
// parameter, one under a negation, one under a disjunction, one over an iota
// without a bound, an iota and a repeat bounded by unreachable_sentinel; and
// none of the three that fold a bounded parameter (type
// constraint, requires clause, trailing clause), nor one over a sized_range
// parameter, nor one over a counted iota, nor the one outside the namespace.
// Standalone, no imports, so it is checked without the build.
#include <algorithm>
#include <array>
#include <functional>
#include <ranges>
#include <utility>

namespace jlt_listing {

constexpr std::array<int, 3> xs{1, 2, 3};

constexpr int left = std::ranges::fold_left(xs, 0, std::plus<>{});  // finding 1
constexpr int twice =  // fold_right would do, but libc++ has none yet
    std::ranges::fold_left(xs, 1, std::multiplies<>{});  // finding 2
constexpr int pair_of_iterators = std::ranges::fold_left(
    xs.begin(), xs.end(), 0, std::plus<>{});  // finding 3

namespace category {
extern "C++" {  // a transparent context, as a module's export is
template <typename R>
concept IsBoundedRange = std::ranges::sized_range<const R>;
}
template <IsBoundedRange R, typename M, typename H>
constexpr M fold(const R& r, const M v, const H h) {
  return std::ranges::fold_left(r, v, h);  // exempt: the bound is stated
}
template <std::ranges::input_range R, typename M, typename H>
constexpr M loose(const R& r, const M v, const H h) {
  return std::ranges::fold_left(r, v, h);  // finding 4: no bound stated
}
template <typename R, typename M, typename H>
  requires IsBoundedRange<R> && std::ranges::input_range<R>
constexpr M by_clause(const R& r, const M v, const H h) {
  return std::ranges::fold_left(r, v, h);  // exempt: a conjunct states it
}
template <typename R, typename M, typename H>
constexpr M by_trailing(R r, const M v, const H h)
  requires IsBoundedRange<R>
{
  return std::ranges::fold_left(std::move(r), v, h);  // exempt: trailing
}
template <IsBoundedRange B, std::ranges::input_range R, typename M, typename H>
constexpr M other_range(const B&, const R& r, const M v, const H h) {
  return std::ranges::fold_left(r, v, h);  // finding 5: R is not bounded
}
template <typename R, typename M, typename H>
  requires(!IsBoundedRange<R>)
constexpr M negated(const R& r, const M v, const H h) {
  return std::ranges::fold_left(r, v, h);  // finding 6: under a negation
}
template <typename R, typename M, typename H>
  requires IsBoundedRange<R> || std::ranges::input_range<R>
constexpr M disjoint(const R& r, const M v, const H h) {
  return std::ranges::fold_left(r, v, h);  // finding 7: under a disjunction
}
constexpr int through_fold = fold(xs, 0, std::plus<>{});
template <std::ranges::sized_range R, typename M, typename H>
constexpr M sized(const R& r, const M v, const H h) {
  return std::ranges::fold_left(r, v, h);  // exempt: sized_range states it
}
constexpr int counted =
    std::ranges::fold_left(std::views::iota(0, 4), 0, std::plus<>{});  // exempt
template <typename M, typename H>
constexpr M from_zero(const M v, const H h) {
  return std::ranges::fold_left(std::views::iota(0), v, h);  // finding 8
}
template <typename M, typename H>
constexpr M to_nowhere(const M v, const H h) {
  return std::ranges::fold_left(std::views::iota(0u, std::unreachable_sentinel),
                                v, h);  // finding 9
}
template <typename M, typename H>
constexpr M forever(const M v, const H h) {
  return std::ranges::fold_left(
      std::views::repeat(1, std::unreachable_sentinel), v, h);  // finding 10
}
}  // namespace category

}  // namespace jlt_listing

constexpr int outside =
    std::ranges::fold_left(jlt_listing::xs, 0, std::plus<>{});  // outside scope
