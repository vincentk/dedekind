#include <catch2/catch_test_macros.hpp>

import dedekind.numbers;
import dedekind.geometry;

using namespace dedekind::numbers;
using namespace dedekind::geometry;

// -----------------------------------------------------------------------
// Quaternion arithmetic over the exact ℚ
// -----------------------------------------------------------------------

namespace {
using Q = Rational<>;
using H = Quaternion<Q>;
constexpr H h(Q w, Q x, Q y, Q z) { return H{w, x, y, z}; }
}  // namespace

TEST_CASE("Quaternion: basic construction and accessors",
          "[numbers][quaternion]") {
  const H q = h(Q{1}, Q{2}, Q{3}, Q{4});
  REQUIRE(q.w() == Q{1});
  REQUIRE(q.x() == Q{2});
  REQUIRE(q.y() == Q{3});
  REQUIRE(q.z() == Q{4});
}

TEST_CASE("Quaternion: additive structure", "[numbers][quaternion]") {
  const H q = h(Q{1}, Q{2}, Q{3}, Q{4});
  const H p = h(Q{1, 2}, Q{1}, Q{3, 2}, Q{2});

  SECTION("Addition is component-wise") {
    REQUIRE(q + p == h(Q{3, 2}, Q{3}, Q{9, 2}, Q{6}));
  }

  SECTION("Subtraction is component-wise") {
    REQUIRE(q - p == h(Q{1, 2}, Q{1}, Q{3, 2}, Q{2}));
  }

  SECTION("Scalar multiplication is component-wise") {
    REQUIRE(Q{2} * q == h(Q{2}, Q{4}, Q{6}, Q{8}));
  }
}

TEST_CASE("Quaternion: Hamilton product rules", "[numbers][quaternion]") {
  const H one = h(Q{1}, Q{0}, Q{0}, Q{0});
  const H i = h(Q{0}, Q{1}, Q{0}, Q{0});
  const H j = h(Q{0}, Q{0}, Q{1}, Q{0});
  const H k = h(Q{0}, Q{0}, Q{0}, Q{1});

  REQUIRE(i * i == -one);
  REQUIRE(j * j == -one);
  REQUIRE(k * k == -one);
  REQUIRE(i * j == k);
  REQUIRE(j * k == i);
  REQUIRE(k * i == j);
  REQUIRE(j * i == -k);
  REQUIRE((i * j) * k == -one);
}

TEST_CASE("Quaternion: conjugate and norm", "[numbers][quaternion]") {
  const H q = h(Q{1}, Q{2}, Q{3}, Q{4});

  SECTION("conj negates imaginary parts") {
    REQUIRE(q.conj() == h(Q{1}, Q{-2}, Q{-3}, Q{-4}));
  }

  SECTION("q * conj(q) = |q|^2 * 1") {
    REQUIRE(q * q.conj() == h(q.norm_squared(), Q{0}, Q{0}, Q{0}));
  }

  SECTION("norm_squared = a^2 + b^2 + c^2 + d^2") {
    REQUIRE(q.norm_squared() == Q{30});
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

TEST_CASE("Quaternion multiplication is non-commutative",
          "[numbers][quaternion]") {
  const H q = h(Q{1}, Q{2}, Q{0}, Q{0});  // 1 + 2i
  const H p = h(Q{1}, Q{0}, Q{3}, Q{0});  // 1 + 3j
  REQUIRE(q * p != p * q);
}
