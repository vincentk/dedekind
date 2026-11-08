// Negative control for rules/rule3-bounded-fold.query: the gate must flag
// seven standard folds inside the jlt_listing namespace: three at namespace
// scope, one with no bound stated, one folding a range other than the bounded
// parameter, one under a negation, one under a disjunction; and none of the
// three that fold a bounded parameter (type constraint, requires clause,
// trailing clause), nor the one outside the namespace.
// Standalone, no imports, so it is checked without the build.
#include <algorithm>
#include <array>
#include <functional>
#include <ranges>
#include <utility>

namespace jlt_listing {

constexpr std::array<int, 3> xs{1, 2, 3};

constexpr int left = std::ranges::fold_left(xs, 0, std::plus<>{});  // finding 1
constexpr int right =
    std::ranges::fold_right(xs, 0, std::plus<>{});  // finding 2
constexpr int pair_of_iterators =
    std::ranges::fold_left(xs.begin(), xs.end(), 0, std::plus<>{});  // finding 3

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
template <IsBoundedRange B, std::ranges::input_range R, typename M,
          typename H>
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
}  // namespace category

}  // namespace jlt_listing

constexpr int outside =
    std::ranges::fold_left(jlt_listing::xs, 0, std::plus<>{});  // outside scope
