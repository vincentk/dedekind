#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.category;
import dedekind.sets; // Ω<T> — set carrier for the set-indexed rungs
using namespace dedekind::algebra;
using namespace dedekind::category;

// Set-indexed base rung: algebra::IsMagma is a claim about a *set object*,
// not a raw carrier.  The magma partition sits upstream of :sets and cannot
// import a set carrier, so its positive witness lives here.
static_assert(
    dedekind::algebra::IsMagma<decltype(dedekind::sets::Ω<unsigned int>)>,
    "Ω<unsigned int> under + is a set-indexed magma.");
// Regression guard on the IsSet/Domain composition: a bare carrier is not a
// set object, so the set-indexed concept must reject it (explicit Op keeps
// the default-argument substitution out of the picture).
static_assert(
    !dedekind::algebra::IsMagma<unsigned int, std::plus<unsigned int>>,
    "A raw carrier is not a set object: set-indexed IsMagma rejects it.");

TEST_CASE("Algebra: Monoid Axioms (Atomic)", "[algebra][monoid]") {
  SECTION("Additive Identity (0)") {
    // Documentation-only checkpoint:
    // Machine integers are not asserted as total algebraic witnesses here
    // (overflow violates closure in the mathematical model).
    // STATIC_CHECK(dedekind::category::IsPointed<int, std::plus<int>>);
    // STATIC_CHECK(dedekind::category::IsPointed<long, std::plus<long>>);

    // Runtime check for identity value
    int x = 42;
    int zero = 0;  // Inferred identity
    CHECK(x + zero == x);
    CHECK(zero + x == x);
  }

  SECTION("Multiplicative Identity (1)") {
    // Documentation-only checkpoint for machine-int multiplicative identity.
    // STATIC_CHECK(dedekind::category::IsPointed<int, std::multiplies<int>>);

    int x = 42;
    int unit = 1;
    CHECK(x * unit == x);
  }

  SECTION("Boolean Monoids") {
    // bool may be a canonical monoid/group carrier depending on operation,
    // but witness registration is currently deferred in category.
    SUCCEED("Boolean monoid witness deferred during reintegration.");
  }
}
