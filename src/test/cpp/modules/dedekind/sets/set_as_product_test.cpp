/** @file test/cpp/modules/dedekind/sets/set_as_product_test.cpp
 *
 *  Witness tests for the @c SetAsProduct seam (#573 slice 2 / #644).
 *
 *  The concept itself lives in @c :category:concrete next to
 *  @c IsSetObject (predicate reading), but its witness uses
 *  @c classify<T>(...) from @c :sets:expressions, which is downstream of
 *  @c :category in the module DAG.  Per the project's DAG-tests rule
 *  (@c feedback_module_dag_tests), the witness test belongs at the
 *  @c :sets layer where both the concept and the witness are visible.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Concrete: SetAsProduct seam — Set := (Underlying, Classifier)",
          "[category][concrete][sets][product][parthood]") {
  // A canonical set object materialised through :sets:expressions:
  // classify<T>(predicate) builds a Set<T, L, P> whose Ambient is T and
  // whose χ is the predicate.  The natural witness for the
  // (Underlying, Classifier) reading.
  const auto s_even = classify<int>([](const int& x) { return x % 2 == 0; });

  SECTION("set object witnesses both readings (predicate and product)") {
    // Predicate reading (sibling): S is a subobject of int.
    STATIC_CHECK(IsSetObject<decltype(s_even), int>);
    // Product reading (this slice): S decomposes as (int, decltype(χ)).
    STATIC_CHECK(SetAsProduct<decltype(s_even), int, decltype(s_even.χ)>);
  }

  SECTION("Underlying must match S::Ambient") {
    // A wrong Underlying must not satisfy the concept.
    STATIC_CHECK_FALSE(
        SetAsProduct<decltype(s_even), bool, decltype(s_even.χ)>);
  }
}
