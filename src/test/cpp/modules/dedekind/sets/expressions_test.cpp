#include <catch2/catch_test_macros.hpp>

// Import our Dedekind modules
import dedekind.sets;

using namespace dedekind::sets;
using namespace dedekind::sets::universes;

TEST_CASE("Dedekind MVP: Basic Membership and Symbols", "[sets]") {
  SECTION("Integer Universe Membership") {
    // 1. Declare our symbolic scout for the Integer Species
    constexpr auto x = var<Integers>;

    /**
     * 2. Build a Set Expression:
     * { x \in \mathbb{Z} | x + 5 < 10 }
     *
     * Mechanics:
     * - 'x % Integers' creates the MembershipConstraint (The Base).
     * - 'x + 5' creates a SymbolicExpr (The Transformation).
     * - '... < 10' creates the final Predicate (The Logic).
     */
    auto MySet = IntentionalSet{x % Integers | (x + 5 < 10)};

    // 3. Verification
    CHECK(MySet.contains(2));
    CHECK(MySet.contains(10);
    
    /**
     * @note Ontological Safety:
     * If we tried: var<Naturals> - 10, the compiler would error 
     * because Naturals (Level 0a) do not satisfy IsGroup (Level 3).
     */

    SECTION("Boolean species with Logical Predicates") {
      // A concrete singleton
      auto Unit = Singleton{1.0};

      // The Symbolic DSL over the Booleans species
      constexpr auto b = var<Booleans>;

      /**
       * Terse Expression:
       * "The set of booleans such that they are true."
       */
      auto OnlyTrue = IntentionalSet{b % Booleans{} | (b == true)};

      static_assert(IsSet<decltype(OnlyTrue)>);
      assert(OnlyTrue.contains(true) == true);
      assert(OnlyTrue.contains(false) == false);
  }
  }
