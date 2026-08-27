/** @file test/cpp/modules/dedekind/numbers/materialise_composition_test.cpp
 *
 * Source for the §3.1 exhibit (lst:materialise, right panel): the extensional
 * and intensional readings COMPOSE.  An extensional set (a std::set) bounds an
 * intensional one (a halfspace), making the universe finite, so their
 * intersection always materialises — even though the halfspace alone, over ℕ,
 * does not.  {2,3,5} ∩ {x>3} = {5}; {2,3,5} ∩ {x>5} = {}.
 */
#include <catch2/catch_test_macros.hpp>
#include <set>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::sets;
using namespace dedekind::order;
using namespace dedekind::numbers;

TEST_CASE("extensional ∩ intensional composes and always materialises",
          "[sets][materialise][composition]") {
  std::set<int> ext{2, 3, 5};
  auto gt3 =
      in<Ω<Cardinality>> > bound<3>;  // {x > 3}, a halfspace (intensional)
  auto gt5 = in<Ω<Cardinality>> > bound<5>;          // {x > 5}
  CHECK(materialise(ext, gt3) == std::set<int>{5});  // {2,3,5} ∩ {x>3}
  CHECK(materialise(ext, gt5) == std::set<int>{});   // {2,3,5} ∩ {x>5}
}
