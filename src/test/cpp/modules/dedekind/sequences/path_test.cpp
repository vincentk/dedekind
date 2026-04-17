#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <vector>

import dedekind.sequences;
import dedekind.topology;
import dedekind.category;

using namespace dedekind::sequences;
using namespace dedekind::topology;
using namespace dedekind::category;

TEST_CASE("Sequences: The Path to Continuity",
          "[sequences][topology][limits]") {
  using ℤ = int;

  // A divergent integer path: s_n = n + 42
  auto s_n = [](std::size_t n) -> ℤ { return static_cast<ℤ>(n) + 42; };
  Path<ℤ> path{s_n};

  SECTION("Axiomatic Proofs") { static_assert(IsSequence<decltype(path)>); }

  SECTION("Sampling") {
    REQUIRE(path.at(0) == 42);
    REQUIRE(path.at(5) == 47);
  }

  SECTION("Comonadic Extension (Contextual Sampling)") {
    auto diffs = path <<=
        [](const Path<ℤ>& ctx) { return ctx.at(0) - ctx.at(1); };

    REQUIRE(diffs.at(0) == -1);
  }

  SECTION("Finite prefixes are first-class sequences") {
    const auto first_four = prefix(path, 4);

    static_assert(IsFiniteSequence<decltype(first_four)>);
    REQUIRE(first_four.size() == 4u);
    REQUIRE(first_four.at(0) == 42);
    REQUIRE(first_four.at(3) == 45);
  }

  SECTION("Drop yields an infinite shifted tail") {
    const auto tail = drop(path, 5);

    static_assert(IsSequence<decltype(tail)>);
    static_assert(!IsFiniteSequence<decltype(tail)>);
    REQUIRE(tail.at(0) == path.at(5));
    REQUIRE(tail.at(3) == path.at(8));
  }

  SECTION("Drop identity at zero offset") {
    const auto same = drop(path, 0);

    REQUIRE(same.at(0) == path.at(0));
    REQUIRE(same.at(7) == path.at(7));
  }

  SECTION("Prefix of drop forms a shifted finite window") {
    const auto window = prefix(drop(path, 2), 4);

    static_assert(IsFiniteSequence<decltype(window)>);
    REQUIRE(window.size() == 4u);
    REQUIRE(window.at(0) == 44);
    REQUIRE(window.at(3) == 47);
  }

  SECTION("Iterative rules support finite orbit counting") {
    const auto orbit = iterate(1, [](int x) { return x + 1; }, 5);

    static_assert(IsFiniteSequence<decltype(orbit)>);
    REQUIRE(orbit.size() == 5u);
    REQUIRE(orbit.at(0) == 1);
    REQUIRE(orbit.at(4) == 5);
    REQUIRE(count_if(orbit, [](int x) { return x > 3; }) == 2u);
  }

  SECTION("Finite iterate materializes once and avoids replayed stepping") {
    std::size_t step_calls = 0;
    const auto orbit = iterate(
        1,
        [&step_calls](int x) {
          ++step_calls;
          return x + 1;
        },
        6);

    // Finite iterate should precompute exactly length-1 transitions.
    REQUIRE(step_calls == 5u);

    // Accessing/counted scans over the finite path must not trigger new steps.
    REQUIRE(orbit.at(0) == 1);
    REQUIRE(orbit.at(5) == 6);
    REQUIRE(count_if(orbit, [](int x) { return x >= 4; }) == 3u);
    REQUIRE(step_calls == 5u);
  }

  SECTION("Finite paths interoperate with std::ranges algorithms") {
    const auto orbit = iterate(2, [](int x) { return x + 2; }, 5);

    static_assert(std::ranges::random_access_range<decltype(orbit)>);

    std::vector<int> visited;
    for (const int value : orbit) visited.push_back(value);

    REQUIRE(visited == std::vector<int>{2, 4, 6, 8, 10});
    REQUIRE(std::ranges::count_if(orbit, [](int x) { return x >= 6; }) == 3);
  }

  SECTION("from_range and as_range adapt finite paths") {
    const std::vector<int> source{1, 2, 3, 4};
    const auto doubled =
        from_range(source | std::views::transform([](int x) { return x * 2; }));

    REQUIRE(doubled.size() == source.size());
    REQUIRE(
        std::ranges::equal(as_range(doubled), std::vector<int>{2, 4, 6, 8}));
  }
}
