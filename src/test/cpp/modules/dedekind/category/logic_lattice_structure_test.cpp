/** @file test/cpp/modules/dedekind/category/logic_lattice_structure_test.cpp
 *
 * Downstream witnesses that the shipped logic species' De Morgan-tower position
 * (their :logic concepts) is BACKED by their carriers' position in the :total
 * lattice-ladder variety concepts.  The two partitions meet in @c category;
 * these asserts pin the bridge so the species cannot drift from the algebra
 * their carriers actually realise.
 *
 * #923 op-type bridge: the carrier-level witnesses below are routed through
 * each species' declared op-@b types --- @c L::JoinOp (∨), @c L::MeetOp (∧),
 * @c L::RflOp (¬) --- rather than hard-coding @c std::logical_or / @c Sup /
 * @c Inf per species.  So a species that mis-declares its op-types (e.g. Kleene
 * claiming @c std::logical_or instead of @c Sup) fails HERE, and the mapping
 * "the static method @c L::OR @b is the function-object @c L::JoinOp" is pinned
 * both at the concept level and (in the value-level section) at the value
 * level.
 *
 * Scope (#935): the sound core is @c Boole and @c Kleene, each certified
 * end-to-end (distributive + @b bounded --- Kleene via the merged #912/#933
 * @c Ternary @c :total registration).  @c Chain<int> is asserted
 * @b distributive only; #935 makes no boundedness claim about it (the
 * machine-int @c numeric_limits poles vs unbounded ℤ decision is #941).
 * @c Percent is @b excluded entirely: its @c [0,100] invariant is not enforced
 * for a publicly-mutable @c Percentage::v, so certifying it as a bounded
 * lattice via generic @c Sup / @c Inf would be unsound (#940: enforce the
 * invariant first).  Consuming this bridge to gate the full Ockham laws is
 * #907.
 */
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <utility>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE(
    "Logic species realise the :total lattice ladder via their op-types "
    "(#923 bridge)",
    "[category][logic][lattice][witness]") {
  SECTION("Boole is Boolean: bool is a Boolean algebra under (OR, AND, RFL)") {
    // The :logic species concept ...
    STATIC_REQUIRE(IsBooleanLogic<Boole>);
    // ... is backed by the :total carrier-level variety concepts, reached
    // through the declared op-types (Boole::JoinOp = std::logical_or<bool>,
    // Boole::MeetOp = std::logical_and<bool>).
    STATIC_REQUIRE(
        IsDistributiveLattice<Boole::Ω, Boole::JoinOp, Boole::MeetOp>);
    STATIC_REQUIRE(IsBoundedLattice<Boole::Ω, Boole::JoinOp, Boole::MeetOp>);
    // The complement law is registered on std::logical_not<bool> (the negation
    // op-type); Boole::RflOp = logic_complement<Boole> computes the same value
    // (pinned in the value-level section) but is the :involution witness, not
    // the is_complemented_v key --- so IsBooleanAlgebra uses logical_not here.
    STATIC_REQUIRE(IsBooleanAlgebra<Boole::Ω, Boole::JoinOp, Boole::MeetOp,
                                    std::logical_not<bool>>);
  }

  SECTION(
      "Kleene is a bounded De Morgan chain: Ternary is a distributive "
      "lattice under (JoinOp, MeetOp) = (Sup, Inf), and --- being finite --- "
      "a bounded one") {
    STATIC_REQUIRE(IsBoundedDeMorganChain<Kleene>);
    // Value-returning Sup/Inf (not the std::ranges::max/min niebloids, which
    // return const T& and so fail IsClosedUnder's same_as<T>): the honest
    // T×T→T ops that reach the whole ladder (#912; niebloid migration #934).
    STATIC_REQUIRE(
        IsDistributiveLattice<Kleene::Ω, Kleene::JoinOp, Kleene::MeetOp>);
    // K₃ is finite, so its carrier is a BOUNDED lattice: ⊥ = False
    // (∨-identity), ⊤ = True (∧-identity), via the merged #912/#933 regs.
    STATIC_REQUIRE(IsBoundedLattice<Kleene::Ω, Kleene::JoinOp, Kleene::MeetOp>);
    // Three grades, so NOT complemented --- matching the species (not Boolean).
    STATIC_REQUIRE(!IsBooleanLogic<Kleene>);
  }

  SECTION(
      "Chain<int> is a De Morgan chain: int is a distributive lattice under "
      "(JoinOp, MeetOp) = (Sup, Inf); boundedness is out of scope for #935") {
    STATIC_REQUIRE(IsBoundedDeMorganChain<Chain<int>>);
    STATIC_REQUIRE(IsDistributiveLattice<Chain<int>::Ω, Chain<int>::JoinOp,
                                         Chain<int>::MeetOp>);
    // #935 asserts NO :total boundedness for Chain<int> either way: whether the
    // carrier's numeric_limits poles should register as Sup/Inf identities (to
    // reach IsBoundedLattice, matching IsBoundedDeMorganChain) or it stays the
    // unbounded ℤ reading is a deliberate design decision left to the
    // follow-up. A chain wider than two grades is NOT complemented, matching
    // the species.
    STATIC_REQUIRE(!IsBooleanLogic<Chain<int>>);
  }

  SECTION("value-level bridge: the declared op-types ARE the species' ops") {
    // Exercises the ACTUAL op-types (not just concept constants): each species'
    // declared JoinOp / MeetOp / RflOp function object computes exactly what
    // the static method L::OR / L::AND / L::RFL computes.  Both operand orders
    // and every carrier value, so a wrong alias (e.g. Kleene::JoinOp = Inf, or
    // a projection AND{return b;}) would fail rather than slip through.
    for (bool a : {false, true}) {
      for (bool b : {false, true}) {
        CHECK(Boole::OR(a, b) == Boole::JoinOp{}(a, b));
        CHECK(Boole::AND(a, b) == Boole::MeetOp{}(a, b));
      }
      CHECK(Boole::RFL(a) == Boole::RflOp{}(a));
    }
    for (Ternary a : {Ternary::False, Ternary::Unknown, Ternary::True}) {
      for (Ternary b : {Ternary::False, Ternary::Unknown, Ternary::True}) {
        CHECK(Kleene::OR(a, b) == Kleene::JoinOp{}(a, b));
        CHECK(Kleene::AND(a, b) == Kleene::MeetOp{}(a, b));
      }
      CHECK(Kleene::RFL(a) == Kleene::RflOp{}(a));
    }
    for (auto [x, y] : {std::pair{7, 3}, std::pair{3, 7}, std::pair{-5, 5}}) {
      CHECK(Chain<int>::OR(x, y) == Chain<int>::JoinOp{}(x, y));
      CHECK(Chain<int>::AND(x, y) == Chain<int>::MeetOp{}(x, y));
    }
    for (int x : {-5, 0, 5}) {
      CHECK(Chain<int>::RFL(x) == Chain<int>::RflOp{}(x));
    }
    // The tower split, exercised at the value level: bool's ¬ is a genuine
    // complement (a ∧ ¬a = ⊥), int's is not (interior stays interior).
    CHECK((Boole::AND(true, Boole::RFL(true)) == Boole::False));
    CHECK((Chain<int>::AND(5, Chain<int>::RFL(5)) != Chain<int>::False));
  }
}
