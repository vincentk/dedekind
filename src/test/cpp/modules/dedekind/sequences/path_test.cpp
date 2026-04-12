#include <catch2/catch_test_macros.hpp>
#include <cmath>

import dedekind.sequences;
import dedekind.topology;
import dedekind.category;

using namespace dedekind::sequences;
using namespace dedekind::topology;
using namespace dedekind::category;

TEST_CASE("Sequences: The Path to Continuity",
          "[sequences][topology][limits]") {
  using ℤ = int;

  // A convergent path: s_n = 1/n + 42
  auto s_n = [](std::size_t n) -> ℤ {
    return static_cast<ℤ>(n) + 42;
  };
  Path<ℤ> path{s_n};

  SECTION("Axiomatic Proofs") {
    static_assert(IsSequence<decltype(path)>);
  }

  SECTION("Sampling") {
    REQUIRE(path.at(0) == 42);
    REQUIRE(path.at(5) == 47);
  }

  SECTION("Comonadic Extension (Contextual Sampling)") {
    auto diffs = path <<= [](const Path<ℤ>& ctx) {
      return ctx.at(0) - ctx.at(1);
    };

    REQUIRE(diffs.at(0) == -1);
  }
}
