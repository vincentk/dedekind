#include <catch2/catch_test_macros.hpp>

import dedekind.geometry;

using namespace dedekind::geometry;

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
}