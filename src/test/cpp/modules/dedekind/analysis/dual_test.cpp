#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

// Dual<F> relocated from dedekind.numbers:dual to dedekind.analysis:dual at
// PR #513 — its structural meaning is differential (forward-mode AD); its
// natural neighbours are :ftc (numerical bridge), :forms, :hamilton.
// This file aggregates Dual-specific test coverage that previously lived
// under src/test/cpp/modules/dedekind/numbers/ (dual_test.cpp itself,
// plus the 𝔻-related sections of starters_test.cpp and tower_test.cpp).
#include <functional>  // std::plus / std::multiplies in IsAlgebra mirrors

import dedekind.algebra; // HasRingOperators, IsAlgebra (witness mirrors)
import dedekind.analysis;
import dedekind.category; // Boole, Ternary, var, ...
import dedekind.geometry; // IsTangentBundle (flat-case tangent-bundle concept)
import dedekind.numbers;  // Rational<>, Complex<F>, IEEE<F>
import dedekind.sets;     // Set, 𝔸, predicate-set DSL

using namespace dedekind::analysis;
using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE("Analysis: Dual Numbers and Differentiation", "[analysis][dual]") {
  using Q = Rational<>;
  using DualValue = Dual<Q>;

  SECTION("Automatic Differentiation: f(x) = x²") {
    const DualValue x{Q{3}, Q{1}};  // seed x = 3, dx = 1
    const DualValue res = x * x;
    REQUIRE(res.value() == Q{9});
    REQUIRE(res.derivative() == Q{6});  // f'(3) = 2x = 6
  }

  SECTION("Subtraction: (a + bε) - (c + dε)") {
    const DualValue res = DualValue{Q{5}, Q{3}} - DualValue{Q{2}, Q{1}};
    REQUIRE(res.value() == Q{3});
    REQUIRE(res.derivative() == Q{2});
  }

  SECTION("Unary negation: -(a + bε) = -a - bε") {
    const DualValue res = -DualValue{Q{4}, Q{-1}};
    REQUIRE(res.value() == Q{-4});
    REQUIRE(res.derivative() == Q{1});
  }

  SECTION("Inverse: (a + bε)⁻¹ = 1/a - (b/a²)ε") {
    const DualValue inv = DualValue{Q{2}, Q{1}}.inverse();
    REQUIRE(inv.value() == Q{1, 2});
    REQUIRE(inv.derivative() == Q{-1, 4});
  }

  SECTION("Division: AD rule d/dx(1/x)|_{x=2} = -1/4") {
    const DualValue res = DualValue{Q{1}, Q{0}} / DualValue{Q{2}, Q{1}};
    REQUIRE(res.value() == Q{1, 2});
    REQUIRE(res.derivative() == Q{-1, 4});
  }
}

TEST_CASE(
    "Analysis: Dual carrier-generality + IsTangentBundle witnesses (read-side)",
    "[analysis][dual][carrier-generality][universal]") {
  // Mirrors of the static_assert witnesses pinned upstream in
  // analysis/dual.cppm.  STATIC_CHECK runs both at compile time AND
  // records as a passing Catch2 assertion at runtime, so coverage
  // tooling can observe the same claim that the upstream
  // static_assert exercises mechanically.

  // Dual(R) = R[ε]/(ε²) closes the ring-operator surface for any
  // commutative ring R (Hartshorne Ex. II.2.8 reading; see #504).
  STATIC_CHECK(dedekind::algebra::HasRingOperators<Dual<int>>);
  STATIC_CHECK(dedekind::algebra::IsAlgebra<Dual<int>, std::plus<Dual<int>>,
                                            std::multiplies<Dual<int>>>);
  STATIC_CHECK(dedekind::algebra::HasRingOperators<Dual<unsigned int>>);

  // Nilpotent axiom ε² = 0 on Dual<int> (defining relation independent
  // of F; see analysis/dual.cppm @section Carrier_Generality).
  constexpr Dual<int> eps_int{0, 1};
  STATIC_CHECK(eps_int * eps_int == Dual<int>{0, 0});

  // IsTangentBundle structural identification — Dual<F> IS the
  // first-order tangent-bundle carrier over F (flat case;
  // Spec(R[ε]/(ε²)) reading).  Bundle-structure on non-flat manifolds
  // is the #185 follow-up.
  STATIC_CHECK(dedekind::geometry::IsTangentBundle<Dual<Rational<>>>);
  STATIC_CHECK(dedekind::geometry::IsTangentBundle<Dual<int>>);
}

// ---------------------------------------------------------------------------
