#include <catch2/catch_test_macros.hpp>
#include <limits>

import dedekind.numbers;
import dedekind.category;

using namespace dedekind::numbers;
using namespace dedekind::category;

TEST_CASE("Numbers: Symbolic Checkpoint", "[numbers][symbolic]") {
  using RealValue = Real<double>;
  using ComplexValue = Complex<RealValue>;

  SECTION("Sqrt2 symbolic anchor") {
    const auto root2 = Sqrt2_Symbolic<double>();
    STATIC_CHECK(dedekind::category::IsSet<decltype(root2)>);
    STATIC_CHECK(dedekind::category::HasTernarySupport<decltype(root2)>);
    REQUIRE(root2.χ(1.4) == Ternary::True);
    REQUIRE(root2.χ(1.5) == Ternary::False);
    REQUIRE(root2.χ(std::numeric_limits<double>::quiet_NaN()) ==
            Ternary::Unknown);
  }

  SECTION("Complex arithmetic over Real wrapper") {
    const ComplexValue a{RealValue{1.0}, RealValue{2.0}};
    const ComplexValue b{RealValue{3.0}, RealValue{4.0}};
    const ComplexValue s = a + b;
    REQUIRE(s.real().resolve() == 4.0);
    REQUIRE(s.imag().resolve() == 6.0);
  }

  SECTION("Constants and transcendental marker") {
    REQUIRE(Pi().resolve() > 3.14);
    REQUIRE(E().resolve() > 2.71);
    const auto T = TranscendentalSet<double>();
    STATIC_CHECK(dedekind::category::IsSet<decltype(T)>);
    REQUIRE(T.χ(0.0) == false);
  }
}
