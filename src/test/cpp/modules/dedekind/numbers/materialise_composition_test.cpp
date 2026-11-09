/** @file test/cpp/modules/dedekind/numbers/materialise_composition_test.cpp
 *
 * Source for the §3.1 exhibit (lst:materialise, right panel): the extensional
 * and intensional readings COMPOSE.  An extensional set (a std::set) bounds an
 * intensional one (a halfspace), making the universe finite, so their
 * intersection always materialises — even though the halfspace alone, over
 * the counts, does not.  {2,3,5} ∩ {x>3} = {5}; {2,3,5} ∩ {x>5} = {}.
 */
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <set>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::sets;
using namespace dedekind::order;
using namespace dedekind::numbers;

TEST_CASE("extensional ∩ intensional composes and always materialises",
          "[sets][ext][composition]") {
  std::set<std::size_t> xs{2, 3, 5};
  auto gt3 = 𝔸<std::size_t>{} & π > 3;  // {x > 3}, a halfspace (intensional)
  auto gt5 = 𝔸<std::size_t>{} & π > 5;  // {x > 5}
  CHECK(ext(xs, gt3) == std::set<std::size_t>{5});  // {2,3,5} ∩ {x>3}
  CHECK(ext(xs, gt5) == std::set<std::size_t>{});   // {2,3,5} ∩ {x>5}
}
