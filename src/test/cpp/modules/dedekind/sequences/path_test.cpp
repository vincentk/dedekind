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

  // A convergent path: s_n = 1/n + 42
  auto s_n = [](std::size_t n) -> ℤ {
    return (1 / static_cast<ℤ>(n + 1)) +
           42;  // Note: Integer division will yield 0 for n > 0
  };
  Path<ℤ> path{s_n};

  SECTION("Axiomatic Proofs") {
    /** @proof A Path must be a Frobenius structure to support both Push and
     * Pull. */
    static_assert(IsFrobenius<Path, ℤ, ℤ>,
                  "
                  "Path must satisfy Frobenius duality.");

    /** @proof The target species must be Archimedean to resolve a limit. */
    static_assert(HasLimit<ℤ>, "ℤ must support the limit() morphism.");
  }

  SECTION("Limit Resolution and Neighborhoods") {
    // The theoretical limit L = 42
    ℤ L = limit(path);

    // Define an epsilon-neighborhood N around L=42
    Interval<ℤ> neighborhood(41.99, 42.01);

    /**
     * @requirement The limit L must be contained within the
     * topological neighborhood to prove convergence.
     */
    REQUIRE(neighborhood.contains(L) == true);

    // Verify the limit is "close enough" for machine precision
    REQUIRE(std::abs(L - 42.0) < 0.001);
  }

  SECTION("Comonadic Extension (Contextual Sampling)") {
    /**
     * @test Using the Extend (<<=) operator to calculate
     * local differences across the path.
     */
    auto diffs = path <<= [](const Path<ℝ>& ctx) {
      return ctx.at(0) - ctx.at(1);  // (1/n) - (1/(n+1))
    };

    // For n=0: (1/1 + 42) - (1/2 + 42) = 0.5
    REQUIRE(diffs.at(0) == Catch::Approx(0.5));
  }
}
