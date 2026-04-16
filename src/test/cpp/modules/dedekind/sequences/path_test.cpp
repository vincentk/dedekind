#include <catch2/catch_test_macros.hpp>

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
}
