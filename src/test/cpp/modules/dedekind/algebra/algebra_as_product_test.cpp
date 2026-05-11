/** @file test/cpp/modules/dedekind/algebra/algebra_as_product_test.cpp
 *
 *  Witness test for the @c AlgebraAsProduct seam (#573 slice 3 / #645).
 *
 *  The concept lives in @c :algebra:universal next to element-level
 *  @c HasCarrier; the witness composes a thin algebra-view wrapper
 *  (real-shaped, not mock-shaped) with a @c :sets:expressions set object
 *  via @c classify<T>(...).  Per @c feedback_module_dag_tests, this test
 *  layer (@c :algebra) imports both @c :sets and @c :algebra, so the
 *  composition is visible here.
 */
#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;
import dedekind.category;
import dedekind.sets;

using namespace dedekind::algebra;
using namespace dedekind::category;

// File-scope thin algebra-view wrapper.  Wraps a single bool value and
// exposes both mereology arms HasCarrier asks for: predicate-style
// part-whole (whole(part) iff equal) and operator->() drill-down.  Same
// shape as the internal _has_carrier_witness::algebra_view in
// universal.cppm, but lives here so the test does not poke at the main
// module's internal namespace.
namespace algebra_as_product_test {
struct BoolAlgebraView {
  bool value;
  constexpr bool operator()(const bool& part) const noexcept {
    return part == value;
  }
  constexpr const bool* operator->() const noexcept { return &value; }
  friend constexpr bool operator==(const BoolAlgebraView&,
                                   const BoolAlgebraView&) = default;
};
}  // namespace algebra_as_product_test

TEST_CASE("Universal: AlgebraAsProduct seam — Algebra := (Set, Operations)",
          "[algebra][universal][product][parthood][sets]") {
  // F_2 = (𝔹, ⊕, ∧) algebra view.  The set object is the universal bool
  // set materialised via classify<bool>; its Ambient is bool, so the
  // (Set, Operations) decomposition holds with the algebra-view as the
  // whole and bool as the underlying carrier.
  const auto bool_set = classify<bool>([](const bool&) { return true; });

  SECTION("F_2 wrapped in a thin view witnesses AlgebraAsProduct") {
    STATIC_CHECK(AlgebraAsProduct<algebra_as_product_test::BoolAlgebraView,
                                  decltype(bool_set), std::bit_xor<bool>,
                                  std::bit_and<bool>>);
  }

  SECTION("closure tier (no Ops): set-level shape without operation closure") {
    // With no Ops, AlgebraAsProduct reduces to the closure-tier shape:
    // IsSetObject<Set, Set::Ambient> still holds (Set is a set object) AND
    // HasCarrier<A, Set::Ambient> still holds at its no-Ops form ---
    // std::regular<Set::Ambient> plus the IsPartOfRelation parthood witness
    // and the arrow_drill_down projector.  Only the per-operation closure
    // requirements on Ops simplify away; the mereology / parthood arms of
    // HasCarrier do not.
    STATIC_CHECK(AlgebraAsProduct<algebra_as_product_test::BoolAlgebraView,
                                  decltype(bool_set)>);
  }
}
