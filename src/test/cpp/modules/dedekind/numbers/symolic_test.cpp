#include <catch2/catch_test_macros.hpp>

import dedekind.numbers;

using namespace dedekind::numbers;

TEST_CASE("Numbers: Symbolic Checkpoint", "[numbers][symbolic]") {
  using R = Real<double>;
  using C = Complex<R>;

  SECTION("Sqrt2 symbolic anchor") {
    const auto root2 = Sqrt2_Symbolic<double>();
    REQUIRE(root2.resolve() > 1.4);
    REQUIRE(root2.resolve() < 1.5);
  }

  SECTION("Complex arithmetic over Real wrapper") {
    const C a{R{1.0}, R{2.0}};
    const C b{R{3.0}, R{4.0}};
    const C s = a + b;
    REQUIRE(s.real().resolve() == 4.0);
    REQUIRE(s.imag().resolve() == 6.0);
  }

  SECTION("Constants and transcendental marker") {
    REQUIRE(Pi().resolve() > 3.14);
    REQUIRE(E().resolve() > 2.71);
    const auto T = TranscendentalSet<double>();
    REQUIRE(T(0.0) == false);
  }
}
