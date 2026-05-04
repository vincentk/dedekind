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
import dedekind.category; // ClassicalLogic, Ternary, var, ...
import dedekind.geometry; // IsTangentBundle (flat-case tangent-bundle concept)
import dedekind.numbers;  // Complex<F>, machine_real_scalar, IEEE<F>
import dedekind.sets;     // Set, Ω, predicate-set DSL

using namespace dedekind::analysis;
using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE("Analysis: Dual Numbers and Differentiation", "[analysis][dual]") {
  using Scalar = double;
  using DualValue = Dual<Scalar>;

  SECTION("Automatic Differentiation: f(x) = x²") {
    // Seed: x = 3, dx = 1
    DualValue x{3.0, 1.0};

    // Compute f(x) = x * x
    DualValue res = x * x;

    // f(3) = 9
    REQUIRE(res.value() == 9.0);
    // f'(3) = 2*x = 6
    REQUIRE(res.derivative() == 6.0);
  }

  SECTION("Subtraction: (a + bε) - (c + dε)") {
    DualValue a{5.0, 3.0};
    DualValue b{2.0, 1.0};
    DualValue res = a - b;
    REQUIRE(res.value() == 3.0);
    REQUIRE(res.derivative() == 2.0);
  }

  SECTION("Unary negation: -(a + bε) = -a - bε") {
    DualValue a{4.0, -1.0};
    DualValue res = -a;
    REQUIRE(res.value() == -4.0);
    REQUIRE(res.derivative() == 1.0);
  }

  SECTION("Inverse: (a + bε)⁻¹ = 1/a - (b/a²)ε") {
    // (2 + 1ε)⁻¹ = 0.5 - 0.25ε
    DualValue a{2.0, 1.0};
    DualValue inv = a.inverse();
    REQUIRE(inv.value() == 0.5);
    REQUIRE(inv.derivative() == -0.25);
  }

  SECTION("Division: AD rule d/dx(1/x)|_{x=2} = -1/4") {
    // f(x) = 1/x, f'(x) = -1/x²
    // Seed x = 2, dx = 1: (2 + 1ε) / (2 + 1ε) would give 1, test 1/(x):
    DualValue one{1.0, 0.0};
    DualValue x{2.0, 1.0};
    DualValue res = one / x;
    REQUIRE(res.value() == 0.5);
    REQUIRE(res.derivative() == -0.25);
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
  STATIC_CHECK(dedekind::geometry::IsTangentBundle<Dual<double>>);
  STATIC_CHECK(dedekind::geometry::IsTangentBundle<Dual<int>>);
}

// ---------------------------------------------------------------------------
// Starter-universe coverage moved from numbers/starters_test.cpp at PR #513
// (:dual relocation).  The 𝔻 / D / DualSet starter aliases now live in
// dedekind::analysis.
// ---------------------------------------------------------------------------

TEST_CASE("Analysis: 𝔻 / D / DualSet starter aliases",
          "[analysis][dual][starter]") {
  // Post-#559: 𝔻 is the universe value Ω<Dual<machine_real_scalar>,
  // ClassicalLogic, ℶ_1>, and D is the classifier instance DualSet{}
  // (= DualSetOf<>{}).  The pair has the same shape as ℝ/ℂ.
  STATIC_CHECK(std::same_as<
               std::remove_cvref_t<decltype(𝔻)>,
               UniversalSet<Dual<machine_real_scalar>, ClassicalLogic, ℶ_1>>);
  STATIC_CHECK(std::same_as<typename std::remove_cvref_t<decltype(𝔻)>::Domain,
                            Dual<machine_real_scalar>>);
  STATIC_CHECK(std::same_as<decltype(D), const DualSet>);

  constexpr auto d = element<𝔻>;
  constexpr auto duals = Set{d};
  static_assert(duals(Dual<double>{1.0, 1.0}) == Ternary::True);
}

TEST_CASE("Analysis: 𝔻 lattice identity (U ∪ ¬U = top, U ∩ ¬U = bottom)",
          "[analysis][dual][starter][lattice]") {
  constexpr auto d = element<𝔻>;
  const auto U = Set{d};
  const auto O = !U;
  CHECK((U | O)(Dual<double>{3.0, 1.0}) == Ternary::True);
  CHECK((U & O)(Dual<double>{3.0, 1.0}) == Ternary::False);
}

// ---------------------------------------------------------------------------
// Tower coverage moved from numbers/tower_test.cpp at PR #513 (:dual
// relocation).  ℂ ↪ Dual seeding and Set-membership-over-Dual<double>
// belong with the Dual carrier rather than under :numbers tower tests.
// ---------------------------------------------------------------------------

TEST_CASE("Analysis: ℂ -> Dual (forward-mode AD seed)",
          "[analysis][dual][tower]") {
  // A complex number c = (a + 0i) can be seeded into Dual as (a + 0ε).
  const auto c = Complex<machine_real_scalar>{3.0, 0.0};
  const Dual<machine_real_scalar> d{c.real(), machine_real_scalar{}};

  CHECK(d.value() == 3.0);
  CHECK(d.derivative() == 0.0);

  // Dual arithmetic: (3 + 0ε) * (2 + 1ε) = 6 + 3ε
  const Dual<machine_real_scalar> seed{2.0, 1.0};
  const auto product = d * seed;
  CHECK(product.value() == 6.0);
  CHECK(product.derivative() == 3.0);
}

TEST_CASE("Analysis: Set membership over Dual<double> domain",
          "[analysis][dual][tower][sets]") {
  using F = machine_real_scalar;
  // Sets over Dual: membership based on the primal value component.
  const auto positive_pred = [](const Dual<F>& d) { return d.value() > 0.0; };
  const Set<Dual<F>, ClassicalLogic, decltype(positive_pred)> positive_primal{
      positive_pred};

  CHECK(positive_primal(Dual<F>{1.0, 0.5}) == true);
  CHECK(positive_primal(Dual<F>{-1.0, 0.5}) == false);

  // Derivative component does not affect set membership.
  CHECK(positive_primal(Dual<F>{0.5, -99.0}) == true);
}
