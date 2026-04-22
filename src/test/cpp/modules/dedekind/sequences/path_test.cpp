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

  SECTION("scan: running size equals index + 1") {
    // scan(size, path)(i) == prefix(path, i+1).size() == i+1
    const auto sizes =
        scan([](const FinitePath<ℤ>& p) { return p.size(); }, path);

    static_assert(IsSequence<decltype(sizes)>);
    REQUIRE(sizes.at(0) == 1u);
    REQUIRE(sizes.at(4) == 5u);
    REQUIRE(sizes.at(9) == 10u);
  }

  SECTION("scan: running sum of natural numbers") {
    // path(i) = i  =>  scan(sum)(i) = 0 + 1 + ... + i = i*(i+1)/2
    Path<ℤ> naturals{[](std::size_t n) { return static_cast<ℤ>(n); }};
    const auto running_sum = scan(
        [](const FinitePath<ℤ>& p) {
          ℤ s = 0;
          for (std::size_t k = 0; k < p.size(); ++k) s += p.at(k);
          return s;
        },
        naturals);

    REQUIRE(running_sum.at(0) == 0);   // prefix [0]: sum = 0
    REQUIRE(running_sum.at(3) == 6);   // prefix [0,1,2,3]: sum = 6
    REQUIRE(running_sum.at(4) == 10);  // prefix [0..4]: sum = 10
  }

  SECTION("scan: exists() over a threshold — finds absorbing element") {
    // path(i) = i+42; threshold > 45 first holds at i=4 (path(4)=46)
    const auto hit = scan(
        [](const FinitePath<ℤ>& p) {
          return exists(p, [](ℤ x) { return x > 45; });
        },
        path);

    REQUIRE(hit.at(3) == false);  // prefix [42,43,44,45]: none > 45
    REQUIRE(hit.at(4) == true);   // prefix [42..46]: 46 > 45
    REQUIRE(hit.at(9) == true);   // absorbing: remains true
  }
}
