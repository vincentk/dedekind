/** @file test/cpp/modules/dedekind/sets/set_as_product_test.cpp
 *
 *  Witness tests for the @c SetAsProduct seam (#573 slice 2 / #644).
 *
 *  The concept itself lives in @c :category:concrete next to
 *  @c IsSetObject (predicate reading).  The witness uses
 *  @c classify<T>(...) from @c :category:topoi (available through the
 *  @c dedekind.category umbrella) to materialise a @c Subobject<A, χ>
 *  test target; the @c :sets layer is the natural home for the test
 *  fork so future witnesses using the @c :sets:expressions DSL
 *  (@c element<...>, @c in<...>, named carriers) can sit alongside.
 */
#include <catch2/catch_test_macros.hpp>
#include <type_traits>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Concrete: SetAsProduct seam — Set := (Underlying, Classifier)",
          "[category][concrete][sets][product][parthood]") {
  // classify<T>(predicate) (from :category:topoi) materialises a
  // Subobject<A, χ> whose Ambient is T and whose χ is the rule-arrow
  // built from the predicate.  Post-#681 structural refactor:
  // @c SetAsProduct's Classifier dimension is the codomain of the
  // carrier-as-predicate (L::Ω), not the predicate-type wrapper.  For
  // a bool-returning lambda under ClassicalLogic the Classifier is bool.
  const auto s_even = classify<int>([](const int& x) { return x % 2 == 0; });

  // Classifier = codomain of S(a) — i.e.\ @c ClassicalLogic::Ω = bool.
  using ClassifierΩ = bool;

  SECTION("set object witnesses both readings (predicate and product)") {
    // Predicate reading (sibling): S is a subobject of int.
    STATIC_CHECK(IsSetObject<decltype(s_even), int>);
    // Product reading (this slice): S decomposes as (int, classifier-Ω).
    STATIC_CHECK(SetAsProduct<decltype(s_even), int, ClassifierΩ>);
  }

  SECTION("Underlying must match S::Ambient") {
    // A wrong Underlying must not satisfy the concept (fails on the
    // IsSetObject prerequisite, not on the Classifier match).
    STATIC_CHECK_FALSE(SetAsProduct<decltype(s_even), bool, ClassifierΩ>);
  }
}
