/** @file test/cpp/modules/dedekind/sets/etcs_lattice_test.cpp
 *  ETCS set-LATTICE behaviour: intersection (@c operator&), union
 *  (@c operator|), complement (@c operator!), ternary support, and the
 *  Boolean-algebra laws.  This test imports both @c :category (for classify /
 *  ambient_set / IsSet) and @c :sets (for the operators).
 *
 *  Membership reads through @c operator() (@c X(v)), the @c IsSubobject
 *  classifier call shape, rather than a named @c .χ member (which the collapsed
 *  set-node results of @c operator& / @c operator| do not carry). */
#include <catch2/catch_test_macros.hpp>
#include <concepts>     // std::same_as
#include <type_traits>  // std::invoke_result_t

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("ETCS: set lattice operations", "[sets][etcs][lattice]") {
  const auto s_even = classify<int>([](const int& x) { return x % 2 == 0; });
  const auto s_positive = classify<int>([](const int& x) { return x > 0; });

  SECTION("Intersection/union/complement via operator&/|/~") {
    const auto both = s_even & s_positive;
    const auto either = s_even | s_positive;
    const auto not_even = ~s_even;

    STATIC_CHECK(
        IsSetObject<decltype(both)>);  // a node over subobjects: a set object
    STATIC_CHECK(
        IsSetObject<decltype(either)>);  // a node over subobjects: a set object
    STATIC_CHECK(IsSetObject<decltype(not_even)>);  // a node over subobjects: a
                                                    // set object

    CHECK(both(2) == true);
    CHECK(both(-2) == false);
    CHECK(either(-2) == true);
    CHECK(either(-3) == false);
    CHECK(not_even(3) == true);
    CHECK(not_even(2) == false);
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
    const auto support = bounded & non_negative;

    STATIC_CHECK(
        IsSetObject<decltype(support)> &&
        std::same_as<std::invoke_result_t<decltype(support) const&, const int&>,
                     Ternary>);

    CHECK(support(5) == Ternary::True);
    CHECK(support(-5) == Ternary::False);
    CHECK(support(50) == Ternary::Unknown);
  }

  SECTION("Support union and complement preserve ternary semantics") {
    const auto support_union = bounded | non_negative;
    const auto support_not_non_negative = ~non_negative;

    STATIC_CHECK(
        IsSetObject<decltype(support_union)> &&
        std::same_as<
            std::invoke_result_t<decltype(support_union) const&, const int&>,
            Ternary>);
    STATIC_CHECK(
        IsSetObject<decltype(support_not_non_negative)> &&
        std::same_as<std::invoke_result_t<
                         decltype(support_not_non_negative) const&, const int&>,
                     Ternary>);

    CHECK(support_union(5) == Ternary::True);
    CHECK(support_union(-5) == Ternary::True);
    CHECK(support_union(-50) == Ternary::Unknown);

    CHECK(support_not_non_negative(5) == Ternary::False);
    CHECK(support_not_non_negative(-5) == Ternary::True);
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

  const auto p_or_q = p | q;
  const auto p_and_q = p & q;
  const auto not_p = ~p;
  const auto not_q = ~q;

  for (bool x : {false, true}) {
    // Complements and involution
    CHECK(not_p(x) == (!p(x)));
    CHECK((~not_p)(x) == p(x));

    // Excluded middle and non-contradiction
    CHECK(p_or_q(x) == top(x));
    CHECK(p_and_q(x) == bottom(x));

    // De Morgan over lifted set operations
    CHECK((~(p & q))(x) == (not_p | not_q)(x));
    CHECK((~(p | q))(x) == (not_p & not_q)(x));

    // Absorption
    CHECK((p | (p & q))(x) == p(x));
    CHECK((p & (p | q))(x) == p(x));
  }
}
