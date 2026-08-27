/** @file test/cpp/modules/dedekind/sets/graph_test.cpp
 *
 * Runtime exercises for :graph — graph(f), the relation/function lattice,
 * and is_graph_of.  The partition's witnesses are static_asserts (invisible
 * to coverage), so the constructor and the finite witness are driven at run
 * time here too.
 */
#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <utility>

import dedekind.sets;
import dedekind.category;

using namespace dedekind::sets;

namespace {
// A distinct arrow to separate from the identity: succ(x) = x + 1.
struct Succ {
  using Domain = int;
  using Codomain = int;
  constexpr int operator()(int x) const { return x + 1; }
};
}  // namespace

TEST_CASE("graph: graph(f) is the diagonal; membership is b == f(a)",
          "[sets][graph]") {
  const auto g = graph(dedekind::category::Identity<int>{});
  CHECK(g(std::pair{5, 5}));
  CHECK_FALSE(g(std::pair{5, 6}));
}

TEST_CASE("graph: is_graph_of decides equality on a finite domain",
          "[sets][graph]") {
  const auto id = dedekind::category::Identity<int>{};
  const auto dom = std::views::iota(0, 4);

  // graph(id) IS the graph of id... (int -> int, so codomain range == dom)
  CHECK(is_graph_of(graph(id), id, dom, dom));
  // ...and graph(succ) is NOT: the witness distinguishes the two functions.
  CHECK_FALSE(is_graph_of(graph(Succ{}), id, dom, dom));
}
