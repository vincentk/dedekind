/** @file dedekind/sequences/materialize_test.cpp
 *
 * The last plank of the intensional → intensional-finite → materialize →
 * extensional bridge: `materialize` realises a finite (IsExtensional) interval
 * domain into its `ExtensionalSet`, via the halfspace→iota_view bridge and the
 * existing `sets::materialise`.  An unbounded domain has no `to_iota_view`, so
 * it cannot reach `materialize` at all — the Rice wall made structural.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.sequences; // materialize, to_iota_view
import dedekind.order;     // OrderInterval, Strictness
import dedekind.category;  // ClassicalLogic

using namespace dedekind::sequences;
using dedekind::category::ClassicalLogic;
using dedekind::order::OrderInterval;
using dedekind::order::Strictness;

namespace {
// [0, 4] — a closed integer interval, the finite prefix {0,1,2,3,4} of ℕ.
using Prefix5 = OrderInterval<int, 0, 4, Strictness::NonStrict,
                              Strictness::NonStrict, ClassicalLogic>;
}  // namespace

TEST_CASE("materialize: a closed interval becomes its ExtensionalSet",
          "[sequences][ranges][materialize]") {
  constexpr Prefix5 oi{};
  const auto ext = materialize(oi);

  CHECK(ext.size() == 5);
  for (int x = 0; x <= 4; ++x) CHECK(ext.contains(x));
  CHECK(!ext.contains(5));
  CHECK(!ext.contains(-1));
}

TEST_CASE("materialize: the filtered form realises argmax over a finite domain",
          "[sequences][ranges][materialize]") {
  constexpr Prefix5 oi{};

  // The two-argument form keeps only the members satisfying the predicate —
  // exactly how an argmax/tie-set over a finite domain is realised.
  const auto evens = materialize(oi, [](int x) { return x % 2 == 0; });
  CHECK(evens.size() == 3);  // {0, 2, 4}
  CHECK(evens.contains(0));
  CHECK(evens.contains(2));
  CHECK(evens.contains(4));
  CHECK(!evens.contains(1));
  CHECK(!evens.contains(3));

  // A unique optimum materialises to a singleton; an empty predicate to ∅.
  const auto sole = materialize(oi, [](int x) { return x == 3; });
  CHECK(sole.size() == 1);
  CHECK(sole.contains(3));

  const auto empty = materialize(oi, [](int) { return false; });
  CHECK(empty.size() == 0);
}
