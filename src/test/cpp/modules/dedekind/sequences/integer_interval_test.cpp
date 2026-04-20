#include <catch2/catch_test_macros.hpp>

import dedekind.sequences;
import dedekind.topology;
import dedekind.category;
import dedekind.sets;

using namespace dedekind::sequences;
using namespace dedekind::topology;
using namespace dedekind::category;

// Default: closed–open [lo, hi)
using II = IntegerInterval<int>;
// Fully closed [lo, hi]
using CI = IntegerInterval<int, Boundary::Closed, Boundary::Closed>;

TEST_CASE("IntegerInterval: the serendipitous bridge",
          "[sequences][topology]") {
  /**
   * The serendipitous finding: an integer interval is simultaneously
   *   1. A convex set    (IsConvex)
   *   2. A set with unique elements  (injectivity of as_sequence())
   *   3. A finite enumerable set     (IsTerminalSet)
   * All three combine into IsConvexEnumerable.
   */

  SECTION("Axiomatic: the triple property") {
    // 1. Convex set
    static_assert(IsConvex<II>);
    // 2. Finite enumerable set
    static_assert(IsTerminalSet<II>);
    // 3. The serendipitous combination
    static_assert(IsConvexEnumerable<II>);

    // Closed variant satisfies the same triple
    static_assert(IsConvexEnumerable<CI>);

    // IntegerInterval exposes supremum()/infimum() as its stored bounds,
    // satisfying HasExtrema — the order-theoretic prerequisite for
    // IsDedekindComplete (IsTotallyOrdered && IsDense && HasExtrema).
    static_assert(dedekind::category::HasExtrema<II>);
    static_assert(dedekind::category::HasExtrema<CI>);
  }

  SECTION("Predicate view: set membership χ: ℤ → bool") {
    II interval{1, 5};  // [1, 5): contains 1,2,3,4

    REQUIRE(interval(1));   // lower bound included (Closed)
    REQUIRE(interval(4));   // last element included
    REQUIRE(!interval(5));  // upper bound excluded (Open)
    REQUIRE(!interval(0));  // below lower bound
  }

  SECTION("Size: finite cardinality as a set") {
    REQUIRE(II{1, 5}.size() == 4u);  // {1,2,3,4}
    REQUIRE(II{0, 1}.size() == 1u);  // {0}
    REQUIRE(II{3, 3}.size() == 0u);  // empty (lo == hi in half-open)
    REQUIRE(II{5, 3}.size() == 0u);  // empty (inverted)

    REQUIRE(CI{1, 5}.size() == 5u);  // {1,2,3,4,5}
    REQUIRE(CI{3, 3}.size() == 1u);  // {3} — single element
  }

  SECTION("Sequence view: injective enumeration f(i) = lo + i") {
    II interval{1, 5};
    auto seq = interval.as_sequence();

    static_assert(IsSequence<decltype(seq)>);

    REQUIRE(seq.at(0) == 1);  // f(0) = 1
    REQUIRE(seq.at(3) == 4);  // f(3) = 4
  }

  SECTION("Uniqueness: injective generator witnesses the set axiom") {
    II interval{1, 5};
    auto seq = interval.as_sequence();

    // i ≠ j implies f(i) ≠ f(j) for all i,j < size()
    for (std::size_t i = 0; i < interval.size(); ++i)
      for (std::size_t j = i + 1; j < interval.size(); ++j)
        REQUIRE(seq.at(i) != seq.at(j));
  }

  SECTION("Closed interval [1, 5]: all boundary policies") {
    CI interval{1, 5};  // [1, 5]

    REQUIRE(interval(1));   // lower bound included
    REQUIRE(interval(5));   // upper bound included (Closed)
    REQUIRE(!interval(6));  // above upper bound
    REQUIRE(!interval(0));  // below lower bound

    auto seq = interval.as_sequence();
    REQUIRE(seq.at(0) == 1);
    REQUIRE(seq.at(4) == 5);  // 5th element (0-indexed) of {1,2,3,4,5}
  }

  SECTION("Boundary accessors") {
    II interval{2, 7};
    REQUIRE(interval.lower_bound() == 2);
    REQUIRE(interval.upper_bound() == 7);
    static_assert(II::lower_boundary == Boundary::Closed);
    static_assert(II::upper_boundary == Boundary::Open);
  }
}
