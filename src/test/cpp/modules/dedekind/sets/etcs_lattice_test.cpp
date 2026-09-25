/** @file test/cpp/modules/dedekind/sets/etcs_lattice_test.cpp
 *  ETCS set-LATTICE behaviour: intersection / union / complement, the
 *  meet / join aliases, ternary support, and the Boolean-algebra laws.
 *
 *  Relocated from the :category ETCS tests (#834): the set-lattice operations
 *  now live in dedekind.sets, so the tests that exercise them belong in the
 *  :sets test layer (which imports both :category, for classify / ambient_set /
 *  IsSet, and :sets, for the ops).  Membership (in / in_via) stayed in the
 *  :category ETCS tests, as it is χ-evaluation, not a lattice op. */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("ETCS: set lattice operations", "[sets][etcs][lattice]") {
  const auto s_even = classify<int>([](const int& x) { return x % 2 == 0; });
  const auto s_positive = classify<int>([](const int& x) { return x > 0; });

  SECTION("Classical intersection/union/complement") {
    const auto both = set_intersection(s_even, s_positive);
    const auto either = set_union(s_even, s_positive);
    const auto not_even = set_complement(s_even);

    STATIC_CHECK(IsSubobject<decltype(both), int>);
    STATIC_CHECK(IsSubobject<decltype(either), int>);
    STATIC_CHECK(IsSubobject<decltype(not_even), int>);

    CHECK(both.χ(2) == true);
    CHECK(both.χ(-2) == false);
    CHECK(either.χ(-2) == true);
    CHECK(either.χ(-3) == false);
    CHECK(not_even.χ(3) == true);
    CHECK(not_even.χ(2) == false);
  }

  SECTION("Lattice aliases meet/join map to intersection/union") {
    const auto m = meet(s_even, s_positive);
    const auto j = join(s_even, s_positive);

    CHECK(m.χ(4) == true);
    CHECK(m.χ(-4) == false);
    CHECK(j.χ(-4) == true);
    CHECK(j.χ(-3) == false);
  }
}

TEST_CASE("ETCS: ternary support lattice", "[sets][etcs][support]") {
  const auto bounded = classify<int>([](const int& x) {
    if (x < -10 || x > 10) return Ternary::Unknown;
    return Ternary::True;
  });

  const auto non_negative = classify<int>(
      [](const int& x) { return x >= 0 ? Ternary::True : Ternary::False; });

  SECTION("Support intersection propagates unknown honestly") {
    const auto support = set_intersection(bounded, non_negative);

    STATIC_CHECK(HasTernarySupport<decltype(support)>);

    CHECK(support.χ(5) == Ternary::True);
    CHECK(support.χ(-5) == Ternary::False);
    CHECK(support.χ(50) == Ternary::Unknown);
  }

  SECTION("Support union and complement preserve ternary semantics") {
    const auto support_union = set_union(bounded, non_negative);
    const auto support_not_non_negative = set_complement(non_negative);

    STATIC_CHECK(HasTernarySupport<decltype(support_union)>);
    STATIC_CHECK(HasTernarySupport<decltype(support_not_non_negative)>);

    CHECK(support_union.χ(5) == Ternary::True);
    CHECK(support_union.χ(-5) == Ternary::True);
    CHECK(support_union.χ(-50) == Ternary::Unknown);

    CHECK(support_not_non_negative.χ(5) == Ternary::False);
    CHECK(support_not_non_negative.χ(-5) == Ternary::True);
  }
}

TEST_CASE("ETCS: Boolean algebra over bool ambient",
          "[sets][etcs][boolean-algebra]") {
  const auto p = ambient_set<bool>([](const bool& x) { return x; });
  const auto q = ambient_set<bool>([](const bool& x) { return !x; });
  const auto top = ambient_set<bool>([](const bool&) { return true; });
  const auto bottom = ambient_set<bool>([](const bool&) { return false; });

  STATIC_CHECK(IsSet<decltype(p)>);
  STATIC_CHECK(IsSet<decltype(q)>);

  const auto p_or_q = set_union(p, q);
  const auto p_and_q = set_intersection(p, q);
  const auto not_p = set_complement(p);
  const auto not_q = set_complement(q);

  for (bool x : {false, true}) {
    // Complements and involution
    CHECK(not_p.χ(x) == (!p.χ(x)));
    CHECK(set_complement(not_p).χ(x) == p.χ(x));

    // Excluded middle and non-contradiction
    CHECK(p_or_q.χ(x) == top.χ(x));
    CHECK(p_and_q.χ(x) == bottom.χ(x));

    // De Morgan over lifted set operations
    CHECK(set_complement(set_intersection(p, q)).χ(x) ==
          set_union(not_p, not_q).χ(x));
    CHECK(set_complement(set_union(p, q)).χ(x) ==
          set_intersection(not_p, not_q).χ(x));

    // Absorption
    CHECK(set_union(p, set_intersection(p, q)).χ(x) == p.χ(x));
    CHECK(set_intersection(p, set_union(p, q)).χ(x) == p.χ(x));
  }
}
