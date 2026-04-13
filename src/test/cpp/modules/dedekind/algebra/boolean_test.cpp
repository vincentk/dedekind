#include <catch2/catch_test_macros.hpp>

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;

using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Algebra:Boolean starter symbols", "[algebra][boolean][starter]") {
  auto b = var<BooleanSet>;

  auto truthy = Set{b % B | (b == true)};
  auto falsy = Set{b % B | (b == false)};

  STATIC_CHECK(std::same_as<decltype(B), const BooleanSet>);
  STATIC_CHECK(std::same_as<decltype(𝔹), const BooleanSet>);

  CHECK(truthy(true) == Ternary::True);
  CHECK(truthy(false) == Ternary::False);
  CHECK(falsy(true) == Ternary::False);
  CHECK(falsy(false) == Ternary::True);

  CHECK((truthy | falsy)(true) == Ternary::True);
  CHECK((truthy | falsy)(false) == Ternary::True);
  CHECK((truthy & falsy)(true) == Ternary::False);
  CHECK((truthy & falsy)(false) == Ternary::False);
}
