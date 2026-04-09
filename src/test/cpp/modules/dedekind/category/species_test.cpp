/** @file dedekind/category/species_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

template <typename T, typename Op = std::plus<T>>
void audit_feature_cube() {
  // --- Plane 1: Algebraic Invariants (The "Manner" of Action) ---
  STATIC_CHECK(is_associative_v<T, Op>);
  STATIC_CHECK(is_commutative_v<T, Op>);

  if constexpr (is_idempotent_v<T, Op>) {
    STATIC_CHECK(IsIdempotent<T, Op>);
  }

  /*     if constexpr (is_invertible_v<T, Op>) {
          STATIC_CHECK(IsGroupoid<T, Op>); // Proof: Inverse exists
      } */

  // --- Plane 2: Relational Invariants (The "Structure" of Order) ---
  // Note: Usually checked against std::less_equal or S::operator<=
  using Rel = std::less_equal<T>;
  STATIC_CHECK(is_reflexive_v<T, Rel>);
  STATIC_CHECK(is_transitive_v<T, Rel>);
  STATIC_CHECK(is_antisymmetric_v<T, Rel>);

  // --- Plane 3: Identity & Mapping (The "Boundary" constants) ---
  if constexpr (IsPointed<T, Op>) {
    STATIC_CHECK(requires { identity_v<T, Op>; });
    // We strip the cv-qualifiers (like 'const') from the constexpr storage
    // so that it matches the raw species type T.
    STATIC_CHECK(
        std::same_as<std::remove_cvref_t<decltype(identity_v<T, Op>)>, T>);
  }

  // if constexpr (IsSet<T>) {
  //     STATIC_CHECK(requires { characteristic_v<T>; });
  // }
}

TEST_CASE("Category: The Species Feature Cube", "[category][species][cube]") {
  SECTION("The Logical Species (bool)") {
    using T = bool;
    // Logic is the most "Perfect" species in the cube
    audit_feature_cube<T, std::logical_and<T>>();
    audit_feature_cube<T, std::logical_or<T>>();
  }

  SECTION("The Integral Species (int)") {
    using T = unsigned int;
    // Arithmetic Plane
    audit_feature_cube<T, std::plus<T>>();
    // Lattice Plane (The bitwise join/meet we just fixed)
    audit_feature_cube<T, std::bit_or<T>>();
  }

  // SECTION("The Floating Species (double)") {
  //   using T = double;
  //  This is where the Auditor (Clang) gets pedantic:
  //  is_reflexive_v should fail due to NaN
  //  is_associative_v should fail due to precision loss
  //}
}
