TEST_CASE("Analysis: Dual Numbers and Differentiation", "[numbers][dual]") {
  using ℝ = double;
  using 𝔻 = Dual<ℝ>;

  SECTION("Automatic Differentiation: f(x) = x²") {
    // Seed: x = 3, dx = 1
    𝔻 x{3.0, 1.0};

    // Compute f(x) = x * x
    𝔻 res = x * x;

    // f(3) = 9
    REQUIRE(res.value() == 9.0);
    // f'(3) = 2*x = 6
    REQUIRE(res.derivative() == 6.0);
  }

  SECTION("Exterior Calculus: The 1-Form") {
    using Vec2 = Vector<ℝ, 2>;
    // ω = 2dx + 3dy
    OneForm<ℝ, 2> omega{{2.0, 3.0}};

    // Evaluate on vector v = (4, 5) -> 2(4) + 3(5) = 23
    REQUIRE(omega(Vec2{4.0, 5.0}) == 23.0);
  }
}
