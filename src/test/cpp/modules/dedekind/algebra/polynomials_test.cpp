#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <complex>
#include <concepts>
import dedekind.algebra;
import dedekind.category;

using namespace dedekind::algebra;

namespace {

template <typename Coeff, typename Target, typename Embed>
constexpr Target evaluate_polynomial_in(const Polynomial<Coeff>& p,
                                        const Target& x, Embed&& embed) {
  Target acc = embed(Coeff{0});
  for (auto it = p.coeffs().rbegin(); it != p.coeffs().rend(); ++it) {
    acc = (acc * x) + embed(*it);
  }
  return acc;
}

struct DualWitness {
  double primal{};
  double tangent{};

  friend constexpr DualWitness operator+(const DualWitness& a,
                                         const DualWitness& b) {
    return {a.primal + b.primal, a.tangent + b.tangent};
  }

  friend constexpr DualWitness operator*(const DualWitness& a,
                                         const DualWitness& b) {
    return {a.primal * b.primal,
            (a.primal * b.tangent) + (a.tangent * b.primal)};
  }
};

}  // namespace

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

  SECTION("Showcase~10: polynomial derivative as a compile-time identity") {
    // p(x) = 3x^3 + 2x^2 + x + 1 (coefficients constant-first);
    // p'(x) = 9x^2 + 4x + 1. Exact, at translation time.
    using P = Polynomial<ℤ>;
    static_assert(P({1, 1, 2, 3}).derive() == P({1, 4, 9}));
    static_assert(P({0, 1}).derive() == P::one());
    static_assert(P::one().derive().is_zero());
  }

  SECTION("Coefficient species can evaluate into richer codomains") {
    // Boolean coefficients: p(x) = 1 + x^2, evaluated in Complex<double>.
    const Polynomial<bool> p_bool({true, false, true});
    const std::complex<double> z{2.0, 1.0};
    const auto as_complex = [](bool b) {
      return std::complex<double>{b ? 1.0 : 0.0, 0.0};
    };
    const auto pz = evaluate_polynomial_in<bool>(p_bool, z, as_complex);
    REQUIRE(pz.real() == 4.0);
    REQUIRE(pz.imag() == 4.0);

    // Natural coefficients: p(x) = x^2 + 3x + 2, evaluated in a dual witness.
    const Polynomial<unsigned int> p_nat({2u, 3u, 1u});
    const DualWitness seed{5.0, 1.0};
    const auto as_dual = [](unsigned int n) {
      return DualWitness{static_cast<double>(n), 0.0};
    };
    const auto pd = evaluate_polynomial_in<unsigned int>(p_nat, seed, as_dual);
    REQUIRE(pd.primal == 42.0);
    REQUIRE(pd.tangent == 13.0);
  }

  SECTION("Finite Taylor truncations are expressible as polynomials") {
    // exp(x) around 0 truncated at order 3: 1 + x + x^2/2 + x^3/6
    const Polynomial<double> exp_t3({1.0, 1.0, 0.5, 1.0 / 6.0});
    const double approx_at_1 =
        evaluate_polynomial_in<double>(exp_t3, 1.0, [](double c) { return c; });
    REQUIRE(std::abs(approx_at_1 - (8.0 / 3.0)) < 1e-12);
  }

  SECTION("Polynomial model currently indexes only non-negative exponents") {
    using P = Polynomial<ℤ>;
    static_assert(std::unsigned_integral<decltype(P{}.degree())>);
  }
}
