#include <catch2/catch_test_macros.hpp>
#include <type_traits>

// Import your module
import dedekind.sets;

TEST_CASE("Cardinality Arithmetic (Compile-time Theorems)",
          "[cardinality][static]") {
  using namespace dedekind::sets;

  SECTION("Finite Logic") {
    // Exact values for tractable finite sets
    STATIC_REQUIRE((Extensional(5) | Extensional(5)) == Extensional(10));
    STATIC_REQUIRE(Extensional(12) * Extensional(3) == Extensional(36));
    STATIC_REQUIRE((Extensional(7) & Extensional(15)) == Extensional(7));

    // Zero/Identity Laws
    STATIC_REQUIRE((Extensional(42) | Zero{}) == Extensional(42));
    STATIC_REQUIRE((Extensional(42) & Zero{}) == Zero{});
  }

  SECTION("Transfinite Lattice Laws (Estimates)") {
    // Countable (ℵ₀) | Finite = ℵ₀ (The absorbing law)
    STATIC_REQUIRE(std::is_same_v<decltype(ℵ_0{} | Extensional(10)), ℵ_0>);

    // Continuum (ℶ₁) & Countable (ℵ₀) = ℵ₀ (The restriction law)
    STATIC_REQUIRE(std::is_same_v<decltype(ℶ_1{} & ℵ_0{}), ℵ_0>);

    // Transfinite Multiplication
    STATIC_REQUIRE(std::is_same_v<decltype(ℶ_1{} * ℵ_0{}), ℶ_1>);
  }

  SECTION("Power Set Jumps") {
    STATIC_REQUIRE(power(Zero{}) == Extensional(1));
    STATIC_REQUIRE(power(Extensional(4)) == Extensional(16));

    // 2^ℵ₀ = ℶ₁ (The jump from Countable to Continuum)
    STATIC_REQUIRE(std::is_same_v<decltype(power(ℵ_0{})), ℶ_1>);
  }

  SECTION("Relational Proofs") {
    STATIC_REQUIRE(Zero{} < Extensional(1));
    STATIC_REQUIRE(Extensional(100) < ℵ_0{});
    STATIC_REQUIRE(ℵ_0{} < ℵ_1{});
    STATIC_REQUIRE(ℵ_0{} < ℶ_1{});
  }
}

#include <catch2/catch_test_macros.hpp>

// Import your module
import dedekind.sets;

TEST_CASE("Extensional Cardinality Arithmetic (Runtime Bounds)",
          "[cardinality][runtime]") {
  using namespace dedekind::sets;

  // We create these as runtime variables so the compiler doesn't
  // necessarily "constant-fold" them away, ensuring our logic works
  // even for values discovered at runtime.
  auto a = Extensional(10);
  auto b = Extensional(20);
  auto e = Empty{};  // Zero

  SECTION("Addition (Union Estimate)") {
    // |A ∪ B| ≤ |A| + |B|
    // Our operator| provides the upper-bound estimate
    auto result = a | b;
    REQUIRE(result.bound == 30);
    REQUIRE(std::is_same_v<decltype(result), Extensional>);
  }

  SECTION("Minimization (Intersection Estimate)") {
    // |A ∩ B| ≤ min(|A|, |B|)
    auto result = a & b;
    REQUIRE(result.bound == 10);
    REQUIRE(std::is_same_v<decltype(result), Extensional>);
  }

  SECTION("Multiplication (Cartesian Product)") {
    // |A × B| = |A| * |B|
    auto result = a * b;
    REQUIRE(result.bound == 200);
    REQUIRE(std::is_same_v<decltype(result), Extensional>);
  }

  SECTION("Identity and Zero Behavior") {
    REQUIRE((a | e).bound == 10);  // A ∪ ∅ = A
    REQUIRE((a & e).bound == 0);   // A ∩ ∅ = ∅
    REQUIRE((a * e).bound == 0);   // A × ∅ = ∅
  }

  SECTION("Comparison Operators") {
    REQUIRE(a < b);
    REQUIRE(a <= b);
    REQUIRE(b > a);
    REQUIRE(a != b);
    REQUIRE(e < a);
  }
  SECTION("The Pathological Frontier (Symbolic Explosion)") {
    using namespace dedekind::sets;

    // 1. The Power Set Jump: P(N64)
    // Mathematically: A set of size 2^(2^64).
    // Symbolic Result: LargeFinite (Finite, but unmeasurable).
    using PowerSetOfUniverse = decltype(power(ℕ64{}));
    STATIC_REQUIRE(std::is_same_v<PowerSetOfUniverse, LargeFinite>);

    // 2. The Cartesian Product: N64 x N64
    // Mathematically: A set of size 2^64 * 2^64 = 2^128.
    // Symbolic Result: LargeFinite.
    using ProductOfUniverse = decltype(ℕ64{} * ℕ64{});
    STATIC_REQUIRE(std::is_same_v<ProductOfUniverse, LargeFinite>);

    // 3. Chain Reaction
    // P(N64 x N64) -> Still LargeFinite (The Finite Ceiling)
    using ExplodedSet = decltype(power(ℕ64{} * ℕ64{}));
    STATIC_REQUIRE(std::is_same_v<ExplodedSet, LargeFinite>);

    // 4. Comparison Proof
    // Even the largest machine space is smaller than the smallest "Large" set
    STATIC_REQUIRE(ℕ64{} < LargeFinite{});
  }
}
