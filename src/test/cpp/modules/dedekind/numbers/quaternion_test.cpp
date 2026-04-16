#include <catch2/catch_test_macros.hpp>

import dedekind.numbers;
import dedekind.geometry;

using namespace dedekind::numbers;
using namespace dedekind::geometry;

// -----------------------------------------------------------------------
// Quaternion arithmetic
// -----------------------------------------------------------------------

TEST_CASE("Quaternion: basic construction and accessors",
          "[numbers][quaternion]") {
  Quaternion<double> q{1.0, 2.0, 3.0, 4.0};
  REQUIRE(q.w() == 1.0);
  REQUIRE(q.x() == 2.0);
  REQUIRE(q.y() == 3.0);
  REQUIRE(q.z() == 4.0);
}

TEST_CASE("Quaternion: additive structure", "[numbers][quaternion]") {
  Quaternion<double> q{1.0, 2.0, 3.0, 4.0};
  Quaternion<double> p{0.5, 1.0, 1.5, 2.0};

  SECTION("Addition is component-wise") {
    auto s = q + p;
    REQUIRE(s.w() == 1.5);
    REQUIRE(s.x() == 3.0);
    REQUIRE(s.y() == 4.5);
    REQUIRE(s.z() == 6.0);
  }

  SECTION("Subtraction is component-wise") {
    auto d = q - p;
    REQUIRE(d.w() == 0.5);
    REQUIRE(d.x() == 1.0);
  }

  SECTION("Scalar multiplication is component-wise") {
    auto s = 2.0 * q;
    REQUIRE(s.w() == 2.0);
    REQUIRE(s.x() == 4.0);
    REQUIRE(s.y() == 6.0);
    REQUIRE(s.z() == 8.0);
  }
}

TEST_CASE("Quaternion: Hamilton product rules", "[numbers][quaternion]") {
  // Basis units: i, j, k
  Quaternion<double> i{0, 1, 0, 0};
  Quaternion<double> j{0, 0, 1, 0};
  Quaternion<double> k{0, 0, 0, 1};

  SECTION("i^2 = -1") {
    auto ii = i * i;
    REQUIRE(ii == Quaternion<double>{-1, 0, 0, 0});
  }

  SECTION("j^2 = -1") {
    auto jj = j * j;
    REQUIRE(jj == Quaternion<double>{-1, 0, 0, 0});
  }

  SECTION("k^2 = -1") {
    auto kk = k * k;
    REQUIRE(kk == Quaternion<double>{-1, 0, 0, 0});
  }

  SECTION("ij = k") { REQUIRE(i * j == k); }

  SECTION("jk = i") { REQUIRE(j * k == i); }

  SECTION("ki = j") { REQUIRE(k * i == j); }

  SECTION("ji = -k  (non-commutative)") { REQUIRE(j * i == -k); }

  SECTION("ijk = -1") {
    REQUIRE((i * j) * k == Quaternion<double>{-1, 0, 0, 0});
  }
}

TEST_CASE("Quaternion: conjugate and norm", "[numbers][quaternion]") {
  Quaternion<double> q{1.0, 2.0, 3.0, 4.0};

  SECTION("conj negates imaginary parts") {
    auto c = q.conj();
    REQUIRE(c.w() == 1.0);
    REQUIRE(c.x() == -2.0);
    REQUIRE(c.y() == -3.0);
    REQUIRE(c.z() == -4.0);
  }

  SECTION("q * conj(q) = |q|^2 * 1") {
    auto product = q * q.conj();
    REQUIRE(product.w() == q.norm_squared());
    REQUIRE(product.x() == 0.0);
    REQUIRE(product.y() == 0.0);
    REQUIRE(product.z() == 0.0);
  }

  SECTION("norm_squared = a^2 + b^2 + c^2 + d^2") {
    REQUIRE(q.norm_squared() == 1.0 + 4.0 + 9.0 + 16.0);
  }
}

// -----------------------------------------------------------------------
// Dimension concepts
// -----------------------------------------------------------------------

TEST_CASE("Dimension concepts: compile-time checks",
          "[geometry][dimension][concepts]") {
  using Vec1 = Vector<double, 1>;
  using Vec2 = Vector<double, 2>;
  using Vec4 = Vector<double, 4>;

  // Finite-dimension concepts
  static_assert(HasFiniteDimension<Vec1>);
  static_assert(HasFiniteDimension<Vec2>);
  static_assert(HasFiniteDimension<Vec4>);

  // Specific-dimension concept
  static_assert(HasDimension<Vec1, 1>);
  static_assert(HasDimension<Vec2, 2>);
  static_assert(HasDimension<Vec4, 4>);
  static_assert(!HasDimension<Vec2, 3>);

  // Dimension cardinality tags
  static_assert(Vec1::dimension_cardinality::is_finite);
  static_assert(Vec2::dimension_cardinality::is_countable);

  SUCCEED("All dimension concept static asserts pass");
}

// -----------------------------------------------------------------------
// Scalar → Vector<F,1>
// -----------------------------------------------------------------------

TEST_CASE("as_vector: scalar to 1D vector", "[geometry][affine][embedding]") {
  SECTION("double scalar embeds to Vector<double,1>") {
    auto v = as_vector(3.14);
    static_assert(std::same_as<decltype(v), Vector<double, 1>>);
    REQUIRE(v[0] == 3.14);
  }
}

// -----------------------------------------------------------------------
// Complex → Vector<R,2> and LinearMap<R,2,2>
// -----------------------------------------------------------------------

TEST_CASE("as_vector(Complex): 2D embedding", "[numbers][complex][embedding]") {
  Complex<double> z{3.0, 4.0};
  auto v = as_vector(z);

  static_assert(std::same_as<decltype(v), Vector<double, 2>>);
  static_assert(HasDimension<decltype(v), 2>);

  REQUIRE(v[0] == 3.0);
  REQUIRE(v[1] == 4.0);
}

TEST_CASE("as_matrix(Complex): 2x2 rotation-matrix embedding",
          "[numbers][complex][embedding]") {
  Complex<double> z{3.0, 4.0};
  auto m = as_matrix(z);

  static_assert(std::same_as<decltype(m), LinearMap<double, 2, 2>>);

  // [[3, -4], [4, 3]]
  REQUIRE(m.coefficient(0, 0) == 3.0);
  REQUIRE(m.coefficient(0, 1) == -4.0);
  REQUIRE(m.coefficient(1, 0) == 4.0);
  REQUIRE(m.coefficient(1, 1) == 3.0);
}

TEST_CASE("as_matrix(Complex) is compatible with multiplication",
          "[numbers][complex][embedding]") {
  // Verify: as_matrix(z) * as_vector(w) == as_vector(z * w)
  Complex<double> z{1.0, 2.0};
  Complex<double> w{3.0, 1.0};

  auto zw = z * w;  // (1+2i)(3+i) = 3 + i + 6i + 2i^2 = 1 + 7i
  REQUIRE(zw.real() == 1.0);
  REQUIRE(zw.imag() == 7.0);

  auto m = as_matrix(z);
  auto v = as_vector(w);
  auto result = m * v;  // Matrix-vector product

  REQUIRE(result[0] == zw.real());
  REQUIRE(result[1] == zw.imag());
}

// -----------------------------------------------------------------------
// Quaternion → Vector<R,4> and LinearMap<R,4,4>
// -----------------------------------------------------------------------

TEST_CASE("as_vector(Quaternion): 4D embedding",
          "[numbers][quaternion][embedding]") {
  Quaternion<double> q{1.0, 2.0, 3.0, 4.0};
  auto v = as_vector(q);

  static_assert(std::same_as<decltype(v), Vector<double, 4>>);
  static_assert(HasDimension<decltype(v), 4>);

  REQUIRE(v[0] == 1.0);  // w
  REQUIRE(v[1] == 2.0);  // x
  REQUIRE(v[2] == 3.0);  // y
  REQUIRE(v[3] == 4.0);  // z
}

TEST_CASE("as_matrix(Quaternion): left-multiplication matrix",
          "[numbers][quaternion][embedding]") {
  Quaternion<double> q{1.0, 2.0, 3.0, 4.0};
  auto m = as_matrix(q);

  static_assert(std::same_as<decltype(m), LinearMap<double, 4, 4>>);

  // Row 0: [ w, -x, -y, -z] = [1, -2, -3, -4]
  REQUIRE(m.coefficient(0, 0) == 1.0);
  REQUIRE(m.coefficient(0, 1) == -2.0);
  REQUIRE(m.coefficient(0, 2) == -3.0);
  REQUIRE(m.coefficient(0, 3) == -4.0);

  // Row 1: [ x,  w, -z,  y] = [2,  1, -4,  3]
  REQUIRE(m.coefficient(1, 0) == 2.0);
  REQUIRE(m.coefficient(1, 1) == 1.0);
  REQUIRE(m.coefficient(1, 2) == -4.0);
  REQUIRE(m.coefficient(1, 3) == 3.0);
}

TEST_CASE("as_matrix(Quaternion) is compatible with multiplication",
          "[numbers][quaternion][embedding]") {
  // Verify: as_matrix(q) * as_vector(p) == as_vector(q * p)
  Quaternion<double> q{1.0, 0.0, 1.0, 0.0};  // 1 + 0i + 1j + 0k
  Quaternion<double> p{1.0, 0.5, 0.0, 0.5};  // 1 + 0.5i + 0j + 0.5k

  auto qp = q * p;

  auto m = as_matrix(q);
  auto vp = as_vector(p);
  auto result = m * vp;

  REQUIRE(result[0] == qp.w());
  REQUIRE(result[1] == qp.x());
  REQUIRE(result[2] == qp.y());
  REQUIRE(result[3] == qp.z());
}

TEST_CASE("Quaternion multiplication is non-commutative",
          "[numbers][quaternion]") {
  Quaternion<double> q{1.0, 2.0, 0.0, 0.0};  // 1 + 2i
  Quaternion<double> p{1.0, 0.0, 3.0, 0.0};  // 1 + 3j

  auto qp = q * p;
  auto pq = p * q;

  // q*p != p*q in general
  REQUIRE(qp != pq);
}
