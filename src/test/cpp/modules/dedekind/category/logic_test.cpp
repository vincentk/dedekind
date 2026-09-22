/** @file test/cpp/modules/dedekind/category/logic_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Logic: The Binary Prime (Classical)", "[category][logic][boolean]") {
  SECTION("Classical Invariants") {
    CHECK((true && true) == true);
    CHECK((true && false) == false);
    CHECK(!true == false);
    CHECK((true || false) == true);
  }

  SECTION("Textbook operator symmetries (Boolean algebra)") {
    constexpr bool values[] = {false, true};
    for (bool a : values) {
      for (bool b : values) {
        CHECK((a && b) == (b && a));
        CHECK((a || b) == (b || a));
      }
    }

    for (bool a : values) {
      for (bool b : values) {
        for (bool c : values) {
          CHECK(((a && b) && c) == (a && (b && c)));
          CHECK(((a || b) || c) == (a || (b || c)));
          CHECK((a && (b || c)) == ((a && b) || (a && c)));
          CHECK((a || (b && c)) == ((a || b) && (a || c)));
        }
      }
    }
  }

  SECTION("Species Promotion (Boolean Wrapper)") {
    Truth<Boole> t{true};
    Truth<Boole> f{false};

    // De Morgan's laws for the Boolean wrapper (via contextual bool).
    CHECK(!(t && f) == (!t || !f));
    CHECK(!(t || f) == (!t && !f));
  }
}

TEST_CASE("Logic: The Indeterminacy (Kleene)", "[category][logic][kleene]") {
  using enum Ternary;

  SECTION("Kleene Truth Tables") {
    // Conjunction (AND)
    CHECK((True && Unknown) == Unknown);
    CHECK((False && Unknown) == False);

    // Disjunction (OR)
    CHECK((True || Unknown) == True);
    CHECK((False || Unknown) == Unknown);

    // Negation (NOT)
    CHECK(!Unknown == Unknown);
    CHECK(!True == False);
    CHECK(!False == True);
  }

  SECTION("Textbook operator symmetries (K3 lattice laws)") {
    constexpr Ternary values[] = {False, Unknown, True};
    for (auto a : values) {
      for (auto b : values) {
        CHECK((a && b) == (b && a));
        CHECK((a || b) == (b || a));
      }
    }

    for (auto a : values) {
      for (auto b : values) {
        for (auto c : values) {
          CHECK(((a && b) && c) == (a && (b && c)));
          CHECK(((a || b) || c) == (a || (b || c)));
          CHECK((a && (b || c)) == ((a && b) || (a && c)));
          CHECK((a || (b && c)) == ((a || b) && (a || c)));
        }
      }
    }
  }

  SECTION("Structural Identities (De Morgan's Laws)") {
    constexpr Ternary values[] = {False, Unknown, True};
    // !(A && B) == !A || !B
    for (auto a : values) {
      for (auto b : values) {
        CHECK(!(a && b) == (!a || !b));
        CHECK(!(a || b) == (!a && !b));
      }
    }
  }

  SECTION("Morphism Lifting") {
    // Verifying the lift_logic bridge from Boolean to Ternary
    CHECK(lift_logic<Kleene>(true) == True);
    CHECK(lift_logic<Kleene>(false) == False);

    // Identity lifting
    CHECK(lift_logic<Kleene>(Unknown) == Unknown);

    // Consistency: lifting preserves order
    CHECK(lift_logic<Kleene>(false) <= lift_logic<Kleene>(true));
  }
}

/** @file test/cpp/modules/dedekind/category/logic_test.cpp */

TEST_CASE("Logic: The Lattice Order (Relational Honesty)",
          "[category][logic][order]") {
  SECTION("Boolean Lattice Order") {
    using B = Truth<Boole>;
    B t{true}, f{false};

    // Axiom: a <= b iff (a + b) == b
    CHECK(holds(f <= t));    // (false || true) == true
    CHECK(holds(f <= f));    // (false || false) == false, so equality holds
    CHECK(holds(t <= t));    // (true || true) == true
    CHECK(refutes(t <= f));  // (true || false) != false
  }

  SECTION("Kleene Information/Truth Order") {
    using K = Truth<Kleene>;
    using enum Ternary;
    K T{True}, F{False}, U{Unknown};

    // Verifying the Linear Truth Chain: False < Unknown < True
    CHECK(holds(F <= U));  // (False || Unknown) == Unknown, hence <= is True
    CHECK(holds(U <= T));  // (Unknown || True) == True
    CHECK(holds(F <= T));  // Transitivity

    // Reflexivity
    CHECK(holds(U <= U));

    // Antisymmetry (Strictly different values cannot be <= each other both
    // ways)
    CHECK(refutes(T <= U));
  }
}

TEST_CASE("Logic: the finite Kleene chain Chain<int> (De Morgan, #901)",
          "[category][logic][demorgan][chain]") {
  using C = Chain<int>;

  SECTION("meet/join/reflection = min/max/~; ~ swaps the poles") {
    CHECK(C::AND(7, 3) == 3);            // meet = min
    CHECK(C::OR(7, 3) == 7);             // join = max
    CHECK(C::RFL(0) == ~0);              // reflection = bitwise NOT
    CHECK(C::RFL(C::False) == C::True);  // ⊥ ↦ ⊤ (INT_MIN ↦ INT_MAX)
    CHECK(C::RFL(C::True) == C::False);  // ⊤ ↦ ⊥
  }

  SECTION("involution, bound-absorption (decidability collapse), De Morgan") {
    CHECK(C::RFL(C::RFL(42)) == 42);         // ~~x = x
    CHECK(C::AND(7, C::False) == C::False);  // x ∧ ⊥ = ⊥
    CHECK(C::OR(7, C::True) == C::True);     // x ∨ ⊤ = ⊤
    CHECK(C::RFL(C::AND(3, 8)) ==
          C::OR(C::RFL(3), C::RFL(8)));  // De Morgan ~(a∧b) = ~a ∨ ~b
  }

  SECTION("Kleene, not Boolean: interior values are uncomplemented") {
    CHECK(C::AND(0, C::RFL(0)) != C::False);  // 0 ∧ ~0 = min(0,-1) = -1 ≠ ⊥
  }

  SECTION("concept tower (runtime witnesses, for coverage)") {
    CHECK(IsDeMorganAlgebra<C>);
    CHECK(IsBoundedDeMorganChain<C>);  // ⟹ Kleene (chain-normality)
    CHECK(!IsBooleanLogic<C>);         // not Boolean (interior uncomplemented)
    CHECK(IsBooleanLogic<Boole>);      // 𝔹 is the Boolean core
    CHECK(IsBoundedDeMorganChain<Kleene>);
    CHECK(!IsBooleanLogic<Kleene>);
  }

  SECTION("unsigned full-range chain") {
    using U = Chain<unsigned>;
    CHECK(U::False == 0u);
    CHECK(U::RFL(0u) == U::True);  // ¬0 = UMAX
    CHECK(U::RFL(U::RFL(123u)) == 123u);
    CHECK(IsBoundedDeMorganChain<U>);
    CHECK(!IsBooleanLogic<U>);
  }

  SECTION("𝔹 ↪ Chain<int>: lift_logic + Truth order land on the poles") {
    // A decided bool verdict embeds at the chain's poles, not the interior 0/1.
    CHECK(lift_logic<C>(true) == C::True);    // ⊤ = INT_MAX
    CHECK(lift_logic<C>(false) == C::False);  // ⊥ = INT_MIN

    // Truth<Chain<int>>::operator<= lifts (OR==b) through lift_logic, so its
    // answer is a pole, never 1/0 (which are interior chain values).
    CHECK((Truth<C>{3} <= Truth<C>{7}).value == C::True);
    CHECK((Truth<C>{7} <= Truth<C>{3}).value == C::False);
  }
}

TEST_CASE("Logic: the Percentage confidence chain (bounded, #906)",
          "[category][logic][demorgan][percentage]") {
  using P = Percent;

  SECTION("meet/join/reflection on [0,100]; self-dual at 50") {
    CHECK(P::AND(Percentage{70}, Percentage{30}) == Percentage{30});  // min
    CHECK(P::OR(Percentage{70}, Percentage{30}) == Percentage{70});   // max
    CHECK(P::RFL(Percentage{30}) == Percentage{70});                  // 100-30
    CHECK(P::RFL(Percentage{50}) == Percentage{50});  // self-dual midpoint
    CHECK(P::RFL(P::True) == P::False);               // 100 ↦ 0
    CHECK(P::RFL(P::False) == P::True);               // 0 ↦ 100
  }

  SECTION("range enforcement: out-of-range saturates to the top pole") {
    CHECK(Percentage{200}.v == 100);
    CHECK(P::True.v == 100);
    CHECK(P::False.v == 0);
  }

  SECTION("involution + De Morgan") {
    CHECK(P::RFL(P::RFL(Percentage{25})) == Percentage{25});  // ~~p = p
    CHECK(P::RFL(P::RFL(Percentage{60})) == Percentage{60});
    CHECK(P::RFL(P::AND(Percentage{30}, Percentage{80})) ==
          P::OR(P::RFL(Percentage{30}),
                P::RFL(Percentage{80})));  // ¬(a∧b) = ¬a ∨ ¬b
  }

  SECTION("Kleene, not Boolean: an interior grade is uncomplemented") {
    // 30 ∧ ¬30 = min(30,70) = 30 ≠ ⊥
    CHECK(P::AND(Percentage{30}, P::RFL(Percentage{30})) != P::False);
  }

  SECTION("concept tower (runtime witnesses, for coverage)") {
    CHECK(IsDeMorganAlgebra<P>);
    CHECK(IsBoundedDeMorganChain<P>);  // ⟹ Kleene
    CHECK(!IsBooleanLogic<P>);         // 101 grades, not Boolean
  }
}