#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;

using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Algebra:Boolean starter symbols", "[algebra][boolean][starter]") {
  auto b = var<𝔹>;

  auto truthy = Set{b % B | (b == true)};
  auto falsy = Set{b % B | (b == false)};

  STATIC_CHECK(std::same_as<decltype(B), const BooleanSet>);
  STATIC_CHECK(std::same_as<𝔹, BooleanSet>);

  CHECK(truthy(true));
  CHECK_FALSE(truthy(false));
  CHECK_FALSE(falsy(true));
  CHECK(falsy(false));

  CHECK((truthy | falsy)(true));
  CHECK((truthy | falsy)(false));
  CHECK_FALSE((truthy & falsy)(true));
  CHECK_FALSE((truthy & falsy)(false));
}

TEST_CASE("Algebra:Boolean paper alignment (logical vs bitwise)",
          "[algebra][boolean][paper]") {
  // Paper alignment target: Feature Cube bool row (logical ||/&& and bitwise
  // |/&).
  for (bool a : {false, true}) {
    for (bool b : {false, true}) {
      for (bool c : {false, true}) {
        // Logical lattice distributivity.
        CHECK((a && (b || c)) == ((a && b) || (a && c)));
        CHECK((a || (b && c)) == ((a || b) && (a || c)));

        // Bitwise lattice distributivity over bool.
        CHECK((a & (b | c)) == ((a & b) | (a & c)));
        CHECK((a | (b & c)) == ((a | b) & (a | c)));
      }

      // Identity and absorber match the paper's bool table for logical ops.
      CHECK((a || false) == a);      // identity for join
      CHECK((a && true) == a);       // identity for meet
      CHECK((a || true) == true);    // absorber for join
      CHECK((a && false) == false);  // absorber for meet

      // Bitwise operators coincide extensionally with logical operators on
      // bool.
      CHECK((a | b) == (a || b));
      CHECK((a & b) == (a && b));
    }
  }
}

TEST_CASE("Algebra:Boolean set laws", "[algebra][boolean][sets][laws]") {
  auto b = var<𝔹>;

  const auto truthy = Set{b % B | (b == true)};
  const auto falsy = Set{b % B | (b == false)};
  const auto empty = Set{b % B | ((b == true) && (b == false))};
  const auto universe = Set{b % B};

  const auto same_set = [](const auto& lhs, const auto& rhs) {
    return lhs(false) == rhs(false) && lhs(true) == rhs(true);
  };

  // Partition laws for the Boolean ambient species.
  CHECK(same_set(truthy | falsy, universe));
  CHECK(same_set(truthy & falsy, empty));

  // Join/meet identities and absorbers.
  CHECK(same_set(truthy | empty, truthy));
  CHECK(same_set(truthy & universe, truthy));
  CHECK(same_set(truthy & empty, empty));
  CHECK(same_set(truthy | universe, universe));

  // Symmetric checks for falsy.
  CHECK(same_set(falsy | empty, falsy));
  CHECK(same_set(falsy & universe, falsy));
  CHECK(same_set(falsy & empty, empty));
  CHECK(same_set(falsy | universe, universe));

  // Lattice shape laws on concrete Boolean sets.
  CHECK(same_set(truthy | falsy, falsy | truthy));
  CHECK(same_set(truthy & falsy, falsy & truthy));
  CHECK(same_set(truthy | truthy, truthy));
  CHECK(same_set(truthy & truthy, truthy));
  CHECK(same_set(truthy | (truthy & falsy), truthy));
  CHECK(same_set(truthy & (truthy | falsy), truthy));
}

TEST_CASE("Algebra:Boolean contradiction is compile-time empty",
          "[algebra][boolean][showcase][constexpr]") {
  constexpr auto b = var<𝔹>;

  // 1) Unfiltered Boolean universe.
  constexpr auto universe = Set{b % B};
  static_assert(universe(true));
  static_assert(universe(false));

  // 2) Two half-spaces over {false, true}.
  constexpr auto truthy = Set{b % B | (b == true)};
  constexpr auto falsy = Set{b % B | (b == false)};

  static_assert(truthy(true));
  static_assert(!truthy(false));
  static_assert(!falsy(true));
  static_assert(falsy(false));

  // 3) Their meet is the contradictory set (empty).
  constexpr auto contradiction = truthy & falsy;
  static_assert(!contradiction(true));
  static_assert(!contradiction(false));

  CHECK(universe(true));
  CHECK(universe(false));
  CHECK(truthy(true));
  CHECK_FALSE(truthy(false));
  CHECK_FALSE(falsy(true));
  CHECK(falsy(false));
  CHECK_FALSE(contradiction(true));
  CHECK_FALSE(contradiction(false));
}

TEST_CASE("Algebra:Boolean paper alignment (logical vs bitwise)",
          "[algebra][boolean][paper]") {
  // Paper alignment target: Feature Cube bool row (logical ||/&& and bitwise
  // |/&).
  for (bool a : {false, true}) {
    for (bool b : {false, true}) {
      for (bool c : {false, true}) {
        // Logical lattice distributivity.
        CHECK((a && (b || c)) == ((a && b) || (a && c)));
        CHECK((a || (b && c)) == ((a || b) && (a || c)));

        // Bitwise lattice distributivity over bool.
        CHECK((a & (b | c)) == ((a & b) | (a & c)));
        CHECK((a | (b & c)) == ((a | b) & (a | c)));
      }

      // Identity and absorber match the paper's bool table for logical ops.
      CHECK((a || false) == a);      // identity for join
      CHECK((a && true) == a);       // identity for meet
      CHECK((a || true) == true);    // absorber for join
      CHECK((a && false) == false);  // absorber for meet

      // Bitwise operators coincide extensionally with logical operators on
      // bool.
      CHECK((a | b) == (a || b));
      CHECK((a & b) == (a && b));
    }
  }
}

TEST_CASE("Algebra:Boolean set laws", "[algebra][boolean][sets][laws]") {
  auto b = var<BooleanSet>;

  const auto truthy = Set{b % B | (b == true)};
  const auto falsy = Set{b % B | (b == false)};
  const auto empty = Set{b % B | ((b == true) && (b == false))};
  const auto universe = Set{b % B | ((b == true) || (b == false))};

  const auto same_set = [](const auto& lhs, const auto& rhs) {
    return lhs(false) == rhs(false) && lhs(true) == rhs(true);
  };

  // Partition laws for the Boolean ambient species.
  CHECK(same_set(truthy | falsy, universe));
  CHECK(same_set(truthy & falsy, empty));

  // Join/meet identities and absorbers.
  CHECK(same_set(truthy | empty, truthy));
  CHECK(same_set(truthy & universe, truthy));
  CHECK(same_set(truthy & empty, empty));
  CHECK(same_set(truthy | universe, universe));

  // Symmetric checks for falsy.
  CHECK(same_set(falsy | empty, falsy));
  CHECK(same_set(falsy & universe, falsy));
  CHECK(same_set(falsy & empty, empty));
  CHECK(same_set(falsy | universe, universe));

  // Lattice shape laws on concrete Boolean sets.
  CHECK(same_set(truthy | falsy, falsy | truthy));
  CHECK(same_set(truthy & falsy, falsy & truthy));
  CHECK(same_set(truthy | truthy, truthy));
  CHECK(same_set(truthy & truthy, truthy));
  CHECK(same_set(truthy | (truthy & falsy), truthy));
  CHECK(same_set(truthy & (truthy | falsy), truthy));
}
