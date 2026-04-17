#include <catch2/catch_test_macros.hpp>

import dedekind.geometry;

using namespace dedekind::geometry;

static_assert(IsMatrixScalar<float>);
static_assert(HasMatrixScalarEmbedding<float>);
#if DEDEKIND_ENABLE_DOUBLE_REAL_PROXY
static_assert(IsMatrixScalar<double>);
static_assert(HasMatrixScalarEmbedding<double>);
#else
static_assert(!IsMatrixScalar<double>);
#endif

TEST_CASE("Geometry: Linear Map MVP", "[geometry][linear-map]") {
  using R = double;
  using Vec2 = Vector<R, 2>;
  using Map22 = LinearMap<R, 2, 2>;
  using Map32 = LinearMap<R, 3, 2>;

  SECTION("Identity map acts as identity") {
    constexpr auto identity = identity_linear_map<R, 2>();
    Vec2 v{2.0, -1.0};

    auto result = identity(v);

    REQUIRE(result == v);
  }

  SECTION("Dense linear map applies to vectors") {
    Map22 a{{{2.0, 1.0}, {0.0, 3.0}}};
    Vec2 v{1.0, 2.0};

    auto result = a * v;

    REQUIRE(result[0] == 4.0);
    REQUIRE(result[1] == 6.0);
  }

  SECTION("Rectangular maps are supported") {
    Map32 a{{{1.0, 0.0}, {0.0, 1.0}, {1.0, 1.0}}};
    Vec2 v{2.0, 3.0};

    auto result = a(v);

    REQUIRE(result[0] == 2.0);
    REQUIRE(result[1] == 3.0);
    REQUIRE(result[2] == 5.0);
  }

  SECTION("Composition matches repeated application") {
    Map22 a{{{1.0, 2.0}, {0.0, 1.0}}};
    Map22 b{{{2.0, 0.0}, {1.0, 3.0}}};
    Vec2 v{1.0, 2.0};

    auto composed = (a * b)(v);
    auto staged = a(b(v));

    REQUIRE(composed == staged);
  }

  SECTION("Linearity holds over addition and scalar multiplication") {
    Map22 a{{{2.0, -1.0}, {1.0, 3.0}}};
    Vec2 x{1.0, 2.0};
    Vec2 y{3.0, -1.0};
    R s = 2.0;

    REQUIRE(a(x + y) == a(x) + a(y));
    REQUIRE(a(s * x) == s * a(x));
  }

  SECTION("Linear maps form an additive and scalar-multiplicative carrier") {
    Map22 a{{{1.0, 2.0}, {3.0, 4.0}}};
    Map22 b{{{0.5, -1.0}, {2.0, 1.0}}};
    Vec2 v{2.0, 1.0};

    REQUIRE(((a + b)(v)) == (a(v) + b(v)));
    REQUIRE(((a - b)(v)) == (a(v) - b(v)));
    REQUIRE(((2.0 * a)(v)) == (a(v) + a(v)));
    REQUIRE((zero_linear_map<R, 2, 2>()(v)) == Vec2{});
  }
}

/**
 * @test Linear Action Axioms: The Four Laws of Module Harmony
 *
 * This test suite verifies that LinearMap<double, R, C> satisfies the
 * operational vector-space-like laws under explicit machine-real proxy opt-in.
 *
 * The four axioms are:
 * 1. **Vector Additivity**: s * (m1 + m2) = s*m1 + s*m2
 * 2. **Scalar Additivity**: (s1 + s2) * m = s1*m + s2*m
 * 3. **Associativity**: (s1 * s2) * m = s1 * (s2 * m)
 * 4. **Identity**: 1.0 * m = m
 *
 * Verification proceeds by:
 * - Testing specific matrix operations
 * - Checking coefficients match expected values
 * - Verifying closure and compatibility with the scalar field
 */
TEST_CASE("Linear Action Axioms: Vector Space Structure",
          "[geometry][linear-map][axioms][vector-space]") {
  using R = double;
  using Map22 = LinearMap<R, 2, 2>;

  SECTION("Axiom 1: Vector Additivity (s * (m1 + m2) = s*m1 + s*m2)") {
    Map22 m1{{{1.0, 2.0}, {3.0, 4.0}}};
    Map22 m2{{{0.5, 1.0}, {1.5, 2.0}}};
    R s = 3.0;

    auto lhs = s * (m1 + m2);
    auto rhs = s * m1 + s * m2;

    // Check all coefficients match
    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(lhs.coefficient(i, j) == rhs.coefficient(i, j));
      }
    }
  }

  SECTION("Axiom 2: Scalar Additivity ((s1 + s2) * m = s1*m + s2*m)") {
    Map22 m{{{1.0, 2.0}, {3.0, 4.0}}};
    R s1 = 2.0;
    R s2 = 3.0;

    auto lhs = (s1 + s2) * m;
    auto rhs = s1 * m + s2 * m;

    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(lhs.coefficient(i, j) == rhs.coefficient(i, j));
      }
    }
  }

  SECTION(
      "Axiom 3: Associativity of Scalar Multiplication "
      "((s1 * s2) * m = s1 * (s2 * m))") {
    Map22 m{{{1.0, 2.0}, {3.0, 4.0}}};
    R s1 = 2.0;
    R s2 = 3.0;

    auto lhs = (s1 * s2) * m;
    auto rhs = s1 * (s2 * m);

    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(lhs.coefficient(i, j) == rhs.coefficient(i, j));
      }
    }
  }

  SECTION("Axiom 4: Identity of Scalar Multiplication (1.0 * m = m)") {
    Map22 m{{{1.0, 2.0}, {3.0, 4.0}}};
    R one = 1.0;

    auto identity_scaled = one * m;

    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(identity_scaled.coefficient(i, j) == m.coefficient(i, j));
      }
    }
  }
}

/**
 * @test Matrix Composition Properties: Non-Commutative Ring Structure
 *
 * Square matrices form a non-commutative ring under + and *:
 * - (M^n×n, +) is an abelian group
 * - (M^n×n, *) is a monoid
 * - Multiplication distributes over addition
 * - Multiplication is associative but NOT commutative
 */
TEST_CASE("Matrix Composition: Non-Commutative Ring",
          "[geometry][linear-map][composition][ring]") {
  using R = double;
  using Map22 = LinearMap<R, 2, 2>;

  SECTION("Composition is associative: (A*B)*C = A*(B*C)") {
    Map22 a{{{1.0, 2.0}, {3.0, 4.0}}};
    Map22 b{{{2.0, 0.0}, {1.0, 3.0}}};
    Map22 c{{{1.0, 1.0}, {0.0, 2.0}}};

    auto left_assoc = (a * b) * c;
    auto right_assoc = a * (b * c);

    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(left_assoc.coefficient(i, j) == right_assoc.coefficient(i, j));
      }
    }
  }

  SECTION("Composition is generally non-commutative: A*B ≠ B*A") {
    Map22 a{{{1.0, 2.0}, {0.0, 1.0}}};
    Map22 b{{{2.0, 0.0}, {1.0, 3.0}}};

    auto ab = a * b;
    auto ba = b * a;

    // For generic matrices, they should differ
    bool all_equal = true;
    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        if (ab.coefficient(i, j) != ba.coefficient(i, j)) {
          all_equal = false;
        }
      }
    }

    REQUIRE(!all_equal);  // Composition is non-commutative
  }

  SECTION("Left distributivity: A*(B+C) = A*B + A*C") {
    Map22 a{{{1.0, 2.0}, {3.0, 4.0}}};
    Map22 b{{{0.5, -1.0}, {2.0, 1.0}}};
    Map22 c{{{1.5, 2.0}, {-1.0, 0.5}}};

    auto lhs = a * (b + c);
    auto rhs = a * b + a * c;

    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(lhs.coefficient(i, j) == rhs.coefficient(i, j));
      }
    }
  }

  SECTION("Right distributivity: (A+B)*C = A*C + B*C") {
    Map22 a{{{1.0, 2.0}, {3.0, 4.0}}};
    Map22 b{{{0.5, -1.0}, {2.0, 1.0}}};
    Map22 c{{{1.5, 2.0}, {-1.0, 0.5}}};

    auto lhs = (a + b) * c;
    auto rhs = a * c + b * c;

    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(lhs.coefficient(i, j) == rhs.coefficient(i, j));
      }
    }
  }

  SECTION("Identity matrix is left and right neutral for multiplication") {
    Map22 a{{{1.0, 2.0}, {3.0, 4.0}}};
    auto I = identity_linear_map<R, 2>();

    auto left_identity = I * a;
    auto right_identity = a * I;

    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(left_identity.coefficient(i, j) == a.coefficient(i, j));
        REQUIRE(right_identity.coefficient(i, j) == a.coefficient(i, j));
      }
    }
  }

  SECTION("Zero matrix is left and right absorbing for multiplication") {
    Map22 a{{{1.0, 2.0}, {3.0, 4.0}}};
    auto Z = zero_linear_map<R, 2, 2>();

    auto left_zero = Z * a;
    auto right_zero = a * Z;

    for (std::size_t i = 0; i < 2; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(left_zero.coefficient(i, j) == 0.0);
        REQUIRE(right_zero.coefficient(i, j) == 0.0);
      }
    }
  }
}

TEST_CASE("Geometry: Covectors and Outer Products",
          "[geometry][covector][outer]") {
  using R = double;
  using Vec2 = Vector<R, 2>;
  using Vec3 = Vector<R, 3>;

  SECTION("Covector alias is LinearMap<F,1,N>") {
    static_assert(std::same_as<Covector<R, 2>, LinearMap<R, 1, 2>>);
    Covector<R, 2> cov{{{3.0, -1.0}}};
    Vec2 v{2.0, 4.0};
    // cov(v)[0] = 3*2 + (-1)*4 = 2.0
    REQUIRE(cov(v)[0] == 2.0);
  }

  SECTION("Outer product has rank-1 coefficient formula u[i]*v[j]") {
    Vec3 u{1.0, 2.0, 3.0};
    Vec2 v{4.0, 5.0};
    auto p = outer(u, v);
    static_assert(std::same_as<decltype(p), LinearMap<R, 3, 2>>);
    REQUIRE(p.coefficient(0, 0) == 4.0);   // u[0]*v[0]
    REQUIRE(p.coefficient(0, 1) == 5.0);   // u[0]*v[1]
    REQUIRE(p.coefficient(1, 0) == 8.0);   // u[1]*v[0]
    REQUIRE(p.coefficient(2, 1) == 15.0);  // u[2]*v[1]
  }

  SECTION("Outer product satisfies (u⊗v)(w) = dot(v,w)·u") {
    Vec2 u{1.0, 2.0};
    Vec2 v{3.0, 4.0};
    Vec2 w{1.0, 1.0};
    auto p = outer(u, v);
    // dot(v, w) = 3 + 4 = 7; result should be 7*u
    REQUIRE(p(w) == 7.0 * u);
  }

  SECTION("Outer product is bilinear: scalar factors pull out") {
    Vec3 u{1.0, 2.0, 3.0};
    Vec2 v{4.0, 5.0};
    R a = 2.0;

    // (a*u) ⊗ v = a * (u ⊗ v)
    auto outer_scaled_left = outer(a * u, v);
    auto scaled_outer = a * outer(u, v);

    for (std::size_t i = 0; i < 3; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(outer_scaled_left.coefficient(i, j) ==
                scaled_outer.coefficient(i, j));
      }
    }

    // u ⊗ (a*v) = a * (u ⊗ v)
    auto outer_scaled_right = outer(u, a * v);

    for (std::size_t i = 0; i < 3; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(outer_scaled_right.coefficient(i, j) ==
                scaled_outer.coefficient(i, j));
      }
    }
  }

  SECTION("Outer product distributes over addition in first argument") {
    Vec3 u1{1.0, 2.0, 3.0};
    Vec3 u2{0.5, 1.0, 1.5};
    Vec2 v{4.0, 5.0};

    // (u1 + u2) ⊗ v = (u1 ⊗ v) + (u2 ⊗ v)
    auto outer_sum = outer(u1 + u2, v);
    auto sum_outer = outer(u1, v) + outer(u2, v);

    for (std::size_t i = 0; i < 3; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(outer_sum.coefficient(i, j) == sum_outer.coefficient(i, j));
      }
    }
  }

  SECTION("Outer product distributes over addition in second argument") {
    Vec3 u{1.0, 2.0, 3.0};
    Vec2 v1{4.0, 5.0};
    Vec2 v2{2.0, 3.0};

    // u ⊗ (v1 + v2) = (u ⊗ v1) + (u ⊗ v2)
    auto outer_sum = outer(u, v1 + v2);
    auto sum_outer = outer(u, v1) + outer(u, v2);

    for (std::size_t i = 0; i < 3; ++i) {
      for (std::size_t j = 0; j < 2; ++j) {
        REQUIRE(outer_sum.coefficient(i, j) == sum_outer.coefficient(i, j));
      }
    }
  }
}

TEST_CASE("Geometry: Finite-Dimensional Jacobian Contract",
          "[geometry][jacobian][derivative]") {
  using R = double;
  using Vec2 = Vector<R, 2>;
  using Vec1 = Vector<R, 1>;
  using Map22 = LinearMap<R, 2, 2>;

  SECTION("Affine maps carry a constant Jacobian witness") {
    const Map22 linear_part{{{2.0, 1.0}, {0.0, 3.0}}};
    const Vec2 translation{1.0, -2.0};

    const auto affine = make_differentiable_map<R, 2, 2>(
        [linear_part, translation](const Vec2& x) {
          return linear_part(x) + translation;
        },
        [linear_part](const Vec2&) { return linear_part; });

    static_assert(HasJacobianAt<decltype(affine)>);

    const Vec2 point_a{1.0, 2.0};
    const Vec2 point_b{-3.0, 0.5};

    REQUIRE(affine(point_a) == (linear_part(point_a) + translation));
    REQUIRE(jacobian_at(affine, point_a) == linear_part);
    REQUIRE(frechet_derivative_at(affine, point_b) == linear_part);
  }

  SECTION("Scalar-valued maps expose their differential as a covector") {
    const auto quadratic = make_differentiable_map<R, 2, 1>(
        [](const Vec2& x) { return Vec1{x[0] * x[0] + 3.0 * x[1]}; },
        [](const Vec2& x) {
          Covector<R, 2> cov;
          cov.set_coefficient(0, 0, 2.0 * x[0]);
          cov.set_coefficient(0, 1, 3.0);
          return cov;
        });

    const Vec2 point{2.0, -1.0};
    const Vec2 direction{0.5, 2.0};
    const auto cov = differential_at(quadratic, point);

    REQUIRE(quadratic(point)[0] == 1.0);
    REQUIRE(cov.coefficient(0, 0) == 4.0);
    REQUIRE(cov.coefficient(0, 1) == 3.0);
    REQUIRE(cov(direction)[0] == 8.0);
  }

  SECTION("Piecewise constant Heaviside map is differentiable away from 0") {
    const auto heaviside = make_differentiable_map<R, 1, 1>(
        [](const Vector<R, 1>& x) { return Vec1{x[0] < 0.0 ? 0.0 : 1.0}; },
        [](const Vector<R, 1>& x) {
          Covector<R, 1> cov;
          // Classical derivative is 0 for x != 0; x = 0 is a non-smooth point.
          cov.set_coefficient(0, 0, x[0] == 0.0 ? 0.0 : 0.0);
          return cov;
        });

    const Vector<R, 1> left_point{-2.0};
    const Vector<R, 1> right_point{3.0};

    REQUIRE(heaviside(left_point)[0] == 0.0);
    REQUIRE(heaviside(right_point)[0] == 1.0);
    REQUIRE(differential_at(heaviside, left_point).coefficient(0, 0) == 0.0);
    REQUIRE(differential_at(heaviside, right_point).coefficient(0, 0) == 0.0);
  }

  SECTION("Option payoff (max(S-K,0)) is piecewise linear") {
    const R strike = 100.0;
    const auto call_payoff = make_differentiable_map<R, 1, 1>(
        [strike](const Vector<R, 1>& x) {
          const R intrinsic = x[0] - strike;
          return Vec1{intrinsic > 0.0 ? intrinsic : 0.0};
        },
        [strike](const Vector<R, 1>& x) {
          Covector<R, 1> cov;
          // Classical derivative: 0 below strike, 1 above strike.
          // At-the-money (x = strike) is non-differentiable and left for
          // future generalized-derivative abstractions.
          cov.set_coefficient(0, 0, x[0] > strike ? 1.0 : 0.0);
          return cov;
        });

    const Vector<R, 1> out_of_money{80.0};
    const Vector<R, 1> in_the_money{130.0};

    REQUIRE(call_payoff(out_of_money)[0] == 0.0);
    REQUIRE(call_payoff(in_the_money)[0] == 30.0);
    REQUIRE(differential_at(call_payoff, out_of_money).coefficient(0, 0) ==
            0.0);
    REQUIRE(differential_at(call_payoff, in_the_money).coefficient(0, 0) ==
            1.0);
  }
}

TEST_CASE("Geometry: Flat-Space Tangent/Cotangent Bundle Structures",
          "[geometry][tangent][cotangent][bundle]") {
  using R = double;
  using Vec3 = Vector<R, 3>;

  // Static alias checks — TangentVector and CotangentVector are the expected
  // types for the flat manifold ℝ^3.
  static_assert(std::same_as<TangentVector<R, 3>, Vector<R, 3>>);
  static_assert(std::same_as<CotangentVector<R, 3>, Covector<R, 3>>);

  SECTION("TangentBundlePoint stores base and fiber independently") {
    const Vec3 base{1.0, 2.0, 3.0};
    const TangentVector<R, 3> fiber{0.1, 0.2, 0.3};
    const TangentBundlePoint<R, 3> pt{base, fiber};

    REQUIRE(pt.base == base);
    REQUIRE(pt.fiber == fiber);
  }

  SECTION("CotangentBundlePoint stores base and covector fiber") {
    const Vec3 base{-1.0, 0.0, 1.0};
    CotangentVector<R, 3> fiber;
    fiber.set_coefficient(0, 0, 1.0);
    fiber.set_coefficient(0, 1, 2.0);
    fiber.set_coefficient(0, 2, 3.0);
    const CotangentBundlePoint<R, 3> pt{base, fiber};

    REQUIRE(pt.base == base);
    REQUIRE(pt.fiber == fiber);
  }

  SECTION(
      "Jacobian of differentiable map maps TangentVector to TangentVector") {
    // The Jacobian J at p maps tangent vectors at p to tangent vectors at f(p).
    // For the identity map, J = Id.
    const auto identity = make_differentiable_map<R, 3, 3>(
        [](const Vec3& x) { return x; },
        [](const Vec3&) { return identity_linear_map<R, 3>(); });

    const TangentBundlePoint<R, 3> source{Vec3{1.0, 2.0, 3.0},
                                          TangentVector<R, 3>{1.0, 0.0, 0.0}};
    const auto jac = jacobian_at(identity, source.base);
    const TangentVector<R, 3> pushed = jac(source.fiber);

    REQUIRE(pushed == source.fiber);  // identity pushforward
  }
}