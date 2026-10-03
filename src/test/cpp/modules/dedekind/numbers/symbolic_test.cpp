#include <catch2/catch_test_macros.hpp>
#include <limits>

import dedekind.numbers;
import dedekind.category;

using namespace dedekind::numbers;
using namespace dedekind::category;

TEST_CASE("Numbers: Symbolic Checkpoint", "[numbers][symbolic]") {
  SECTION("Sqrt2 symbolic anchor") {
    const auto root2 = Sqrt2_Symbolic<double>();
    // The lower cut is Kleene-valued (NaN ↦ Unknown): an L-set, not an ETCS
    // set (IsSet needs Ω = 𝔹).
    STATIC_CHECK(dedekind::category::IsLSet<decltype(root2)>);
    STATIC_CHECK_FALSE(dedekind::category::IsSet<decltype(root2)>);
    STATIC_CHECK(dedekind::category::HasTernarySupport<decltype(root2)>);
    REQUIRE(root2.χ(1.4) == Ternary::True);
    REQUIRE(root2.χ(1.5) == Ternary::False);
    REQUIRE(root2.χ(std::numeric_limits<double>::quiet_NaN()) ==
            Ternary::Unknown);
  }

  SECTION("Transcendental-set marker (no rational point in the set)") {
    // The `TranscendentalSet<double>` marker: the @b set of transcendentals
    // over $\mathbb{R}$.
    const auto T = TranscendentalSet<double>();
    STATIC_CHECK(dedekind::category::IsSet<decltype(T)>);
    REQUIRE(T.χ(0.0) == false);  // 0 is rational, not transcendental
  }
}
