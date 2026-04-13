/** @file test/cpp/modules/dedekind/category/etcs_test.cpp */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("ETCS: primitive ambient species can be materialized as sets",
          "[category][etcs][sets][primitives]") {
  const auto int_non_negative =
      ambient_set<int>([](const int& x) { return x >= 0; });
  const auto uint_even =
      ambient_set<unsigned>([](const unsigned& x) { return (x % 2u) == 0u; });
  const auto bool_true = ambient_set<bool>([](const bool& x) { return x; });
  const auto unit_interval =
      ambient_set<double>([](const double& x) { return x >= 0.0 && x <= 1.0; });

  STATIC_CHECK(IsSet<decltype(int_non_negative)>);
  STATIC_CHECK(IsSet<decltype(uint_even)>);
  STATIC_CHECK(IsSet<decltype(bool_true)>);
  STATIC_CHECK(IsSet<decltype(unit_interval)>);

  CHECK(int_non_negative.χ(0));
  CHECK_FALSE(int_non_negative.χ(-1));
  CHECK(uint_even.χ(2u));
  CHECK_FALSE(uint_even.χ(3u));
  CHECK(bool_true.χ(true));
  CHECK_FALSE(bool_true.χ(false));
  CHECK(unit_interval.χ(0.5));
  CHECK_FALSE(unit_interval.χ(-0.1));
}

TEST_CASE("ETCS: set lattice operations", "[category][etcs][sets]") {
  const auto s_even = classify<int>([](const int& x) { return x % 2 == 0; });
  const auto s_positive = classify<int>([](const int& x) { return x > 0; });

  SECTION("Classical intersection/union/complement") {
    const auto both = set_intersection(s_even, s_positive);
    const auto either = set_union(s_even, s_positive);
    const auto not_even = set_complement(s_even);

    STATIC_CHECK(IsSetObject<decltype(both), int>);
    STATIC_CHECK(IsSetObject<decltype(either), int>);
    STATIC_CHECK(IsSetObject<decltype(not_even), int>);

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

TEST_CASE("ETCS: ternary support lattice", "[category][etcs][support]") {
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
          "[category][etcs][boolean-algebra]") {
  const auto p = ambient_set<bool>([](const bool& x) { return x; });
  const auto q = ambient_set<bool>([](const bool& x) { return !x; });
  const auto top = ambient_set<bool>([](const bool&) { return true; });
  const auto bottom = ambient_set<bool>([](const bool&) { return false; });

  STATIC_CHECK(IsSetInCanonicalCCC<decltype(p)>);
  STATIC_CHECK(IsSetInCanonicalCCC<decltype(q)>);

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
