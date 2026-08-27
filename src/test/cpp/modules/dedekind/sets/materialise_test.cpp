/** @file test/cpp/modules/dedekind/sets/materialise_test.cpp
 *
 * Source for the §3.1 exhibit (lst:materialise): Trsk is STRICTLY MORE
 * EXPRESSIVE than std::set.  The embedding ι = ambient_set∘from_std (any
 * container IS a Trsk set, its contains() the characteristic predicate) is
 * total; the retraction μ = materialise (enumerate a finite universe, keep the
 * χ-matches) is partial — the naturals have no finite materialisation.  The
 * price of the intensional regime is proof, not enumeration.
 */
#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <set>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Trsk is strictly more expressive than std::set (ι total, μ partial)",
          "[sets][materialise][extensional-intensional]") {
  // ι : std::set ↪ Trsk (total) — contains() IS the characteristic predicate.
  std::set<int> c{2, 3, 5};
  static_assert(IsSet<decltype(ambient_set<int>(from_std(c)))>,
                "any std::set lifts to a Trsk (IsSet) object");

  // μ : Trsk ⇀ std::set (partial) — enumerate a finite universe, keep
  // χ-matches.
  auto isEven = [](int x) { return x % 2 == 0; };
  auto evens = materialise(std::views::iota(0, 10), isEven);
  CHECK(evens == std::set<int>{0, 2, 4, 6, 8});
}
