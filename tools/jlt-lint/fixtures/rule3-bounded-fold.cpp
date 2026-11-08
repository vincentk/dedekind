// Negative control for rules/rule3-bounded-fold.query: the gate must flag the
// four standard folds called directly inside the jlt_listing namespace, one
// of them in a function whose range is not IsBoundedRange, and neither the
// one in a function whose range is, nor the one outside the namespace.
// Standalone, no imports, so it is checked without the build.
#include <algorithm>
#include <array>
#include <functional>
#include <ranges>

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
constexpr int through_fold = fold(xs, 0, std::plus<>{});
}  // namespace category

}  // namespace jlt_listing

constexpr int outside =
    std::ranges::fold_left(jlt_listing::xs, 0, std::plus<>{});  // outside scope
