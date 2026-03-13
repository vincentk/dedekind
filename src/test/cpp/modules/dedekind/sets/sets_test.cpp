#include <catch2/catch_test_macros.hpp>

// Import our Dedekind modules
import dedekind.sets;

using namespace dedekind::sets;
using namespace dedekind::sets::universes;

TEST_CASE("Dedekind MVP: Basic Membership and Symbols", "[sets]") {
  SECTION("Integer Universe Membership") {
    // Create a handle for the default integer universe
    auto Z = SetHandle<int>(std::make_shared<IntegerUniverse<int>>());

    // Test our 'mathy' operator[]
    CHECK(Z[42] == true);
    CHECK(Z[-7] == true);

    // Verify metadata
    CHECK(Z.cardinality() == Cardinality::CountablyInfinite);
  }

  SECTION("Symbolic Intersection with Empty Set") {
    auto Z = SetHandle<int>(std::make_shared<IntegerUniverse<int>>());
    auto Empty = SetHandle<int>(std::make_shared<EmptySetNode<int>>());

    // Symbolic: Anything & Empty should return an EmptySetNode (optimized)
    auto result = Z & Empty;

    CHECK(result[42] == false);
    CHECK(result.cardinality() == Cardinality::Empty);
  }
}

TEST_CASE("Dedekind MVP: Real Numbers and Intervals", "[sets]") {
  SECTION("Real Universe (Double) Finite Check") {
    auto R = SetHandle<double>(std::make_shared<RealUniverse<double>>());

    CHECK(R[3.14] == true);
    CHECK(R[std::numeric_limits<double>::infinity()] ==
          false);  // RealUniverse excludes non-finite
    CHECK(R.cardinality() == Cardinality::Uncountable);
  }
}

TEST_CASE("Dedekind MVP: Set Builder Syntax", "[sets]") {
  SECTION("Filtering the Integer Universe") {
    // Universe ^ Predicate
    auto Evens =
        SetHandle<int>(std::make_shared<IntegerUniverse<int>>()) ^ is_even;

    CHECK(Evens[2] == true);
    CHECK(Evens[3] == false);
    CHECK(Evens[-4] == true);
  }

  SECTION("Chaining Filters (Subsets of Subsets)") {
    auto R = SetHandle<double>(std::make_shared<RealUniverse<double>>());

    // { x in R | x > 0 } ^ { x | x is roughly 1.0 }
    auto PositiveUnit = R ^ is_positive ^ is_unit;

    CHECK(PositiveUnit[1.0] == true);
    CHECK(PositiveUnit[-1.0] == false);  // Fails is_positive
    CHECK(PositiveUnit[0.5] == false);   // Fails is_unit
  }
}

TEST_CASE("Dedekind: Generic Predicates", "[sets]") {
  using namespace dedekind::sets::universes;
  using namespace dedekind::sets::predicates;

  SECTION("Integer Context") {
    auto Z = SetHandle<int>(std::make_shared<IntegerUniverse<int>>());
    auto PositiveZ = Z ^ is_positive;
    CHECK(PositiveZ[5] == true);
    CHECK(PositiveZ[-5] == false);
  }

  SECTION("Double Context") {
    auto R = SetHandle<double>(std::make_shared<RealUniverse<double>>());
    auto PositiveR = R ^ is_positive;
    CHECK(PositiveR[5.5] == true);
    CHECK(PositiveR[-0.1] == false);
  }
}

TEST_CASE("Dedekind: Extensional and Negated Logic", "[sets]") {
  using namespace dedekind::sets::universes;
  using namespace dedekind::sets::predicates;

  SECTION("Extensional Set vs Symbolic Filter") {
    // A concrete set of numbers
    auto SmallSet =
        std::make_shared<ExtensionalSet<int>>(std::set{1, 2, 3, 4, 5});
    auto S = SetHandle<int>(SmallSet);

    // Filter for odd numbers using our negated predicate
    auto Odds = S ^ is_odd;

    CHECK(Odds[1] == true);
    CHECK(Odds[2] == false);
    CHECK(Odds.cardinality() == Cardinality::Finite);
  }

  SECTION("Universal Complement") {
    auto Z = SetHandle<int>(std::make_shared<IntegerUniverse<int>>());
    auto NonPositive = Z ^ !is_positive;  // { x in Z | x <= 0 }

    CHECK(NonPositive[0] == true);
    CHECK(NonPositive[5] == false);
  }
}

TEST_CASE("Dedekind: Final MVP Showcase", "[sets]") {
  using namespace dedekind::sets::universes;
  using namespace dedekind::sets::predicates;

  SECTION("Predicate Composition") {
    auto PositiveOdds = Z ^ (is_odd && is_positive);

    CHECK(PositiveOdds[3] == true);
    CHECK(PositiveOdds[-3] == false);  // Fails is_positive
    CHECK(PositiveOdds[2] == false);   // Fails is_odd
  }

  SECTION("Set Difference and Negation") {
    // Evens that are not positive: { ..., -4, -2, 0 }
    auto S = (Z ^ is_even) - (Z ^ is_positive);

    CHECK(S[-2] == true);
    CHECK(S[2] == false);
    CHECK(S[0] == true);
  }

  SECTION("Primitive Universes") {
    CHECK(Chars['a'] == true);
    CHECK(Floats[3.14f] == true);
    CHECK(Bools[true] == true);
  }
}

TEST_CASE("Dedekind: Symbolic Empty Set Difference", "[sets][meta]") {
  using namespace dedekind::sets;
  using namespace dedekind::sets::universes;

  // 1. Define our sets
  auto E = SetHandle<double, Empty>(std::make_shared<EmptySetNode<double>>(),
                                    Empty{});
  auto Reals = R;  // The pre-defined Uncountable universe

  // 2. Perform the math: E \ R
  auto result = E - Reals;

  // 3. The "Mathy" Checks
  SECTION("Membership Logic") {
    CHECK(result[0.0] == false);  // Membership test via operator[]
    CHECK(result[42.0] == false);
  }

  SECTION("Metadata Logic (The Switchboard)") {
    auto res_card = result.cardinality();  // Returns an AnyCardinality variant

    // This uses the std::visit + operator<= "Switchboard" we just built
    CHECK(res_card <= Empty{});
    CHECK(Empty{} <= res_card);  // Proves it's exactly Empty

    // If it's truly Empty, it's also smaller than the Reals
    CHECK(res_card <= Reals.cardinality());
  }
}

TEST_CASE("Cardinality Monotonicity Fuzz Test", "[meta][fuzz]") {
  // Generate random cardinalities from our sealed hierarchy
  // Note: In a real build, you'd use a helper to pick these types
  auto c1 = GENERATE(
      values<CardinalityBase*>({new Empty(), new Extensional(10), new Finite(),
                                new Countable(), new Uncountable()}));

  auto c2 = GENERATE(
      values<CardinalityBase*>({new Empty(), new Extensional(5), new Finite(),
                                new Countable(), new Uncountable()}));

  SECTION("Intersection never grows") {
    auto result = (*c1) & (*c2);
    // The result must be 'smaller or equal' to both parents in the hierarchy
    CHECK(is_smaller_or_equal(result, *c1));
    CHECK(is_smaller_or_equal(result, *c2));
  }

  SECTION("Union never shrinks") {
    auto result = (*c1) | (*c2);
    // The result must be 'larger or equal' to both parents
    CHECK(is_larger_or_equal(result, *c1));
    CHECK(is_larger_or_equal(result, *c2));
  }
}
