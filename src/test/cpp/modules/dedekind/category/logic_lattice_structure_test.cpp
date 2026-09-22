/** @file test/cpp/modules/dedekind/category/logic_lattice_structure_test.cpp
 *
 * Downstream witnesses that the shipped logic species' De Morgan-tower position
 * (their :logic concepts) is BACKED by their carriers' position in the :total
 * lattice-ladder variety concepts.  The two partitions meet in @c category;
 * these asserts pin the bridge so the species cannot drift from the algebra
 * their carriers actually realise.
 *
 * Scope note: @c Kleene's carrier (the @c Ternary enum, i.e. @c Kleene::Ω) is
 * not yet registered in @c :total / @c :posetal (it needs its order +
 * identities), so its distributive-lattice witness is deferred; and the full
 * @c IsAlgebraOnSet "palace" is further down the build chain.  Both tracked
 * separately.
 */
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE(
    "Logic species realise the :total lattice ladder (downstream witness)",
    "[category][logic][lattice][witness]") {
  SECTION("Boole is Boolean: bool is a Boolean algebra under (or, and, not)") {
    // The :logic species concept ...
    STATIC_REQUIRE(IsBooleanLogic<Boole>);
    // ... is backed by the :total carrier-level variety concept.
    STATIC_REQUIRE(
        IsBooleanAlgebra<Boole::Ω, std::logical_or<bool>,
                         std::logical_and<bool>, std::logical_not<bool>>);
  }

  SECTION(
      "Chain<int> is a bounded De Morgan chain: int is a distributive "
      "lattice under (max, min)") {
    STATIC_REQUIRE(IsBoundedDeMorganChain<Chain<int>>);
    STATIC_REQUIRE(IsDistributiveLattice<int, decltype(std::ranges::max),
                                         decltype(std::ranges::min)>);
    // A chain wider than two grades is NOT complemented, matching the species.
    STATIC_REQUIRE(!IsBooleanLogic<Chain<int>>);
  }

  SECTION("value-level bridge: the species ops ARE the carrier's lattice ops") {
    // Exercises actual operations (not just concept constants): each species'
    // declared AND / OR / RFL computes exactly the carrier's :total lattice op,
    // which is what makes the concept-level bridge above load-bearing.
    CHECK(Boole::AND(true, false) == std::logical_and<bool>{}(true, false));
    CHECK(Boole::OR(true, false) == std::logical_or<bool>{}(true, false));
    CHECK(Boole::RFL(true) == std::logical_not<bool>{}(true));
    CHECK(Chain<int>::AND(7, 3) == std::ranges::min(7, 3));
    CHECK(Chain<int>::OR(7, 3) == std::ranges::max(7, 3));
    // The tower split, exercised at the value level: bool's ¬ is a genuine
    // complement (a ∧ ¬a = ⊥), int's is not (interior stays interior).
    CHECK((Boole::AND(true, Boole::RFL(true)) == Boole::False));
    CHECK((Chain<int>::AND(5, Chain<int>::RFL(5)) != Chain<int>::False));
  }
}
