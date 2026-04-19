#include <catch2/catch_test_macros.hpp>
import dedekind.algebra;
import dedekind.category;

using namespace dedekind::algebra;

TEST_CASE("Algebra: Polynomial Morphisms", "[algebra][polynomial]") {
  using ℤ = int;

  // Coefficients are stored constant-first: p = -2 + 0·x + 1·x²
  Polynomial<ℤ> p({-2, 0, 1});

  SECTION("Basic Structural Shape") {
    REQUIRE(p.degree() == 2);
    REQUIRE(!p.is_zero());
  }

  SECTION("Additive and Multiplicative Identities") {
    auto z = Polynomial<ℤ>::zero();
    auto o = Polynomial<ℤ>::one();
    REQUIRE(z.is_zero());
    REQUIRE(o.degree() == 0);
  }

  SECTION("Canonicalization strips trailing zeros") {
    // 1 + 0·x should be the same as the scalar 1
    Polynomial<ℤ> q({1, 0});
    REQUIRE(q.degree() == 0);
    REQUIRE(q == Polynomial<ℤ>::one());
  }

  SECTION("Addition (coefficient-wise)") {
    // p = -2 + x²,  q = 3 + 5·x
    Polynomial<ℤ> q({3, 5});
    auto r = p + q;  // 1 + 5·x + x²
    REQUIRE(r.degree() == 2);
    REQUIRE(r == Polynomial<ℤ>({1, 5, 1}));

    // Adding zero is the identity.
    REQUIRE((p + Polynomial<ℤ>::zero()) == p);
    REQUIRE((Polynomial<ℤ>::zero() + p) == p);
  }

  SECTION("Subtraction (ring coefficients)") {
    // p - p == 0
    REQUIRE((p - p).is_zero());

    // p - zero == p
    REQUIRE((p - Polynomial<ℤ>::zero()) == p);
  }

  SECTION("Formal differentiation") {
    // d/dx (-2 + 0·x + x²) = 0 + 2·x  =>  [0, 2]
    auto dp = p.derive();
    REQUIRE(dp.degree() == 1);
    REQUIRE(dp == Polynomial<ℤ>({0, 2}));

    // Derivative of a constant is zero.
    REQUIRE(Polynomial<ℤ>::one().derive().is_zero());
    REQUIRE(Polynomial<ℤ>::zero().derive().is_zero());

    // d/dx (x) = 1.  Polynomial for x has coefficients [0, 1].
    Polynomial<ℤ> x_poly({0, 1});
    auto dx = x_poly.derive();
    REQUIRE(dx.degree() == 0);
    REQUIRE(dx == Polynomial<ℤ>::one());
  }

  SECTION("Multiplication (Cauchy product)") {
    // (1 + x) * (1 + x) = 1 + 2x + x²
    Polynomial<ℤ> one_plus_x({1, 1});
    auto sq = one_plus_x * one_plus_x;
    REQUIRE(sq.degree() == 2);
    REQUIRE(sq == Polynomial<ℤ>({1, 2, 1}));
  }

  SECTION("Compile-time polynomial law metadata") {
    using P = Polynomial<ℤ>;

    static_assert(dedekind::category::is_associative_v<P, std::plus<>>);
    static_assert(dedekind::category::is_associative_v<P, std::multiplies<>>);
    static_assert(dedekind::category::is_commutative_v<P, std::plus<>>);

    static_assert(requires(P a, P b) {
      { a + b } -> std::same_as<P>;
      { a * b } -> std::same_as<P>;
      { a - b } -> std::same_as<P>;
    });
  }

  SECTION("Compile-time polynomial calculus interface checks") {
    using P = Polynomial<ℤ>;

    static_assert(P{}.is_zero());
    static_assert(P{}.degree() == 0);

    static_assert(requires(P a, P b) {
      { a + b } -> std::same_as<P>;
      { a - b } -> std::same_as<P>;
      { a * b } -> std::same_as<P>;
      { a.derive() } -> std::same_as<P>;
    });
  }
}
