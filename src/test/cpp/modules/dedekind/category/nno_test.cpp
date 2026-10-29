/** @file test/cpp/modules/dedekind/category/nno_test.cpp
 *
 * The chain of counts of the Jlt paper's Figure 2 (#981): unfold, the
 * anamorphism; chain, Dedekind's Kette under a partial step; chain_view, the
 * bounded chain as an instance of unfold; and the constant arrow, the figure's
 * vertical arrow where the fold is n copies of one value.
 */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>
#include <optional>
#include <ranges>
#include <utility>
#include <vector>

import dedekind.category;

namespace {
using namespace dedekind::category;
using std::size_t;

// The chain of n under Prior is what Prior reaches from n, {n - 1, ..., 0}:
// n elements, n itself not among them, and nothing below 0 (Figure 2 of the
// Jlt paper, after Lambek).
static_assert(std::ranges::fold_left(chain(size_t{4}, Prior<size_t>{}),
                                     size_t{0}, std::plus<>{}) == 6);
static_assert(std::ranges::fold_left(chain(size_t{0}, Prior<size_t>{}),
                                     size_t{0}, std::plus<>{}) == 0);
static_assert(std::ranges::distance(chain(size_t{6}, Prior<size_t>{})) == 6);
// bool counts: the chain of true is {false}, of false nothing.
static_assert(std::ranges::distance(chain(true, Prior<bool>{})) == 1);
static_assert(std::ranges::distance(chain(false, Prior<bool>{})) == 0);
// chain_view is unfold over 1 + C: [lo, hi] inclusive, and a saturating top.
static_assert(std::ranges::fold_left(chain_view{size_t{1}, size_t{4}},
                                     size_t{0}, std::plus<>{}) == 10);
static_assert(std::ranges::distance(chain_view{false, true}) == 2);
static_assert(std::ranges::distance(chain_view{size_t{3}, size_t{2}}) == 0);
// The constant arrow: every count to one value, so the fold is n copies.
static_assert(IsConstantArrow<Constant<size_t>> &&
              IsMonotone<Constant<size_t>> && IsAntiMonotone<Constant<size_t>>);
static_assert(std::ranges::fold_left(chain(size_t{3}, Prior<size_t>{}) |
                                         std::views::transform(Constant<size_t>{
                                             7}),
                                     size_t{0}, std::plus<>{}) == 21);

// A coalgebra whose element is not its state: the parity of each count on
// the way down.  The iterator compares states, so the element need not.
struct Parity {
  constexpr std::optional<std::pair<bool, size_t>> operator()(
      const size_t& n) const {
    if (n == 0) return std::nullopt;
    return std::pair<bool, size_t>{n % 2 == 1, n - 1};
  }
};
static_assert(std::ranges::distance(unfold(size_t{4}, Parity{})) == 4);
constexpr size_t count_true(size_t a, bool b) { return a + (b ? 1 : 0); }
static_assert(std::ranges::fold_left(unfold(size_t{5}, Parity{}), size_t{0},
                                     count_true) == 3);

std::vector<size_t> collect(auto&& view) {
  std::vector<size_t> out;
  for (size_t x : view) out.push_back(x);
  return out;
}
}  // namespace

TEST_CASE(
    "the chain of a count under Prior is the counts below it, walked down",
    "[nno][chain]") {
  size_t n = 5;
  CHECK(collect(chain(n, Prior<size_t>{})) ==
        std::vector<size_t>{4, 3, 2, 1, 0});
  CHECK(collect(chain(size_t{0}, Prior<size_t>{})).empty());
  CHECK(chain(n, Prior<size_t>{}).seed() == 5);
}

TEST_CASE("chain_view is the bounded chain, an unfold over 1 + C",
          "[nno][chain]") {
  size_t lo = 2, hi = 5;
  CHECK(collect(chain_view{lo, hi}) == std::vector<size_t>{2, 3, 4, 5});
  CHECK(collect(chain_view{hi, lo}).empty());
  std::vector<bool> bs;
  for (bool b : chain_view{false, true}) bs.push_back(b);
  CHECK(bs == std::vector<bool>{false, true});
}

TEST_CASE("the constant arrow makes the fold n copies", "[nno][constant]") {
  size_t n = 4, x = 7;
  CHECK(std::ranges::fold_left(chain(n, Prior<size_t>{}) |
                                   std::views::transform(Constant<size_t>{x}),
                               size_t{0}, std::plus<>{}) == 28);
  CHECK(Constant<size_t>{x}(n) == 7);
}
