// Negative control for rules/rule3-bounded-fold.query: the gate must flag the
// three standard folds called directly inside the jlt_listing namespace, and
// neither the one inside the library's fold nor the one outside the namespace.
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
template <typename R, typename M, typename H>
constexpr M fold(const R& r, const M v, const H h) {
  return std::ranges::fold_left(r, v, h);  // exempt: the library's fold
}
constexpr int through_fold = fold(xs, 0, std::plus<>{});
}  // namespace category

}  // namespace jlt_listing

constexpr int outside =
    std::ranges::fold_left(jlt_listing::xs, 0, std::plus<>{});  // outside scope
