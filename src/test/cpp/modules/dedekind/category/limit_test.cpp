/** @file test/cpp/modules/dedekind/category/limit_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <exception>
#include <functional>
#include <variant>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Discrete: Terminal Object (1) - Pure Existence",
          "[category][discrete][terminal]") {
  SECTION("Existence Axiom") {
    STATIC_CHECK(IsTerminalObject<One>);

    // Every species T must have exactly one morphism to the Terminal Object (T
    // -> 1)
    STATIC_CHECK(HasUniqueMorphismTo<int, One>);
    STATIC_CHECK(HasUniqueMorphismTo<bool, One>);
  }

  SECTION("Global Elements (Morphisms 1 -> X)") {
    // A constant value '5' is a mapping from the state of pure existence to
    // 'int'. We use the arrow factory to deduce the lambda type.
    auto five = arrow<One, int>([](One) { return 5; });

    STATIC_CHECK(IsArrow<decltype(five)>);
    CHECK(five(One{}) == 5);
  }
}

TEST_CASE("Discrete: Initial Object (0) - The Annihilator",
          "[category][discrete][initial]") {
  SECTION("Unreachability Axiom") {
    STATIC_CHECK(IsInitialObject<Zero>);
    // Dually, there is a unique morphism from the Initial Object to any species
    // (0 -> T)
    STATIC_CHECK(HasUniqueMorphismFrom<Zero, int>);
    STATIC_CHECK(HasUniqueMorphismFrom<Zero, double>);
  }

  SECTION("Annihilation Logic") {
    // A morphism out of Zero represents an unreachable code path.
    // Explicit return type is provided to assist the factory deduction.
    auto unreachable = arrow<Zero, int>([](Zero) -> int {
      // Logically never entered in a valid category of sets
      std::terminate();
    });

    STATIC_CHECK(IsArrow<decltype(unreachable)>);
  }
}

TEST_CASE("Discrete: Projected Boundary Semantics",
          "[category][discrete][boundary][projection]") {
  SECTION("Projected terminal object witnesses") {
    STATIC_CHECK(IsBoundaryProjection<std::identity, One, One>);
    STATIC_CHECK(IsProjectedTerminalObject<One>);

    struct TerminalEnvelope {
      One value{};
      constexpr const One* operator->() const { return &value; }
    };
    struct Drill {
      constexpr const One& operator()(const TerminalEnvelope& whole) const {
        return *whole.operator->();
      }
    };
    STATIC_CHECK(IsProjectedTerminalObject<TerminalEnvelope, Drill>);
  }

  SECTION("Projected initial object witnesses") {
    STATIC_CHECK(IsBoundaryProjection<std::identity, Zero, Zero>);
    STATIC_CHECK(IsProjectedInitialObject<Zero>);

    struct InitialEnvelope {
      Zero value{nullptr};
      constexpr const Zero* operator->() const { return &value; }
    };
    struct Drill {
      constexpr Zero operator()(const InitialEnvelope& whole) const {
        return *whole.operator->();
      }
    };
    STATIC_CHECK(IsProjectedInitialObject<InitialEnvelope, Drill>);
  }
}
