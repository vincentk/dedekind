#include <catch2/catch_test_macros.hpp>
#include <complex>

import dedekind.algebra;
import dedekind.category;
import dedekind.ieee;
import dedekind.ieee.approx;
import dedekind.sets;

using namespace dedekind::algebra;

TEST_CASE("Modules: Integer Polynomial Action", "[algebra][modules]") {
  SECTION("Polynomial Carrier over Z") {
    // p(x) = 3x^2 + 2x + 5
    // Coeffs in vector: {5, 2, 3} (constant first)
    Polynomial<int> p({5, 2, 3});
    CHECK(p.degree() == 2);
    CHECK_FALSE(p.is_zero());
  }

  SECTION("Reified 1D real line supports vector operations") {
    using RealScalar = decltype(RealLine{}.coordinate());
    RealLine x(2.0);
    RealLine y(3.5);

    CHECK((x + y).coordinate().resolve() == 5.5);
    CHECK((RealScalar{2.0} * x).coordinate().resolve() == 4.0);
    CHECK((x * RealScalar{2.0}).coordinate().resolve() == 4.0);
    CHECK((y - x).coordinate().resolve() == 1.5);
    CHECK((-x).coordinate().resolve() == -2.0);
  }

  SECTION("Other 1D spaces: complex line") {
    using C = std::complex<double>;
    using ComplexLine = OneDimensionalVector<C, struct ComplexTag>;

    ComplexLine v(C{1.0, 2.0});
    ComplexLine w(C{0.5, -1.0});

    CHECK((v + w).coordinate() == C{1.5, 1.0});
    CHECK((C{2.0, 0.0} * v).coordinate() == C{2.0, 4.0});
  }

  SECTION("Semimodule and vector-space baseline concepts are grounded") {
    CHECK(IsSemimodule<unsigned int, unsigned int>);
    using RealScalar = decltype(RealLine{}.coordinate());
    CHECK(HasVectorSpaceOperators<RealLine, RealScalar>);
    CHECK(SatisfiesVectorSpaceAxioms<RealLine, RealScalar>);
  }

  SECTION("RealLine uses IEEE fast-lane policy by default") {
    using RealScalar = decltype(RealLine{}.coordinate());
    CHECK(std::same_as<RealScalar, dedekind::ieee::IEEE<double>>);

    RealLine x(2.0);
    RealLine y(3.5);
    const auto lane_result = (x + y).coordinate();

    CHECK(dedekind::ieee::demo_add_ignore(x.coordinate(), y.coordinate()) ==
          lane_result);
    CHECK(dedekind::ieee::IgnoreErrorPolicy<double>{}(lane_result) ==
          lane_result);
  }

  SECTION("Boolean 1D vectors behave like a semimodule over the Boolean rig") {
    BoolLine a(true);
    BoolLine b(false);

    CHECK(BoolLineJoin{}(a, b).coordinate());
    CHECK_FALSE(BoolLineJoin{}(b, b).coordinate());
    CHECK(BoolLineMeetAction{}(true, a).coordinate());
    CHECK_FALSE(BoolLineMeetAction{}(false, a).coordinate());

    // The four value-level CHECKs above already exercise BoolLine's
    // semimodule semantics under (BoolLineJoin, BoolLineMeetAction) at
    // concrete inputs.  A meta-level signature-alignment claim
    // previously named HasSemimoduleOperators<BoolLine, bool, ...> was
    // retired in PR #510 / #374 (over-generic; redundant with the
    // strict IsSemimodule for carriers that earn the trait registry,
    // and with the value-level CHECKs for those that don't).  The
    // strict IsSemimodule does not fire here because the axiom-variable
    // templates are not specialised for (logical_or, logical_and) on
    // bool under the active numeric policy.
  }

  SECTION("Unsigned 1D scaling supports strength reduction") {
    UnsignedLine u(7u);

    CHECK(scale_strength_reduced(u, 8u).coordinate() == 56u);
    CHECK(scale_strength_reduced(u, 3u).coordinate() == 21u);
    CHECK(scale_strength_reduced(u, 0u).coordinate() == 0u);
  }

  SECTION("Vector-space and semimodule carriers lift to ETCS IsSet") {
    auto real_space =
        dedekind::category::ambient_set<RealLine>([](const RealLine&) {
          return dedekind::category::ClassicalLogic::True;
        });
    CHECK(dedekind::category::IsSet<decltype(real_space)>);
    CHECK(dedekind::category::in(RealLine(0.0), real_space) ==
          dedekind::category::ClassicalLogic::True);
    CHECK(dedekind::category::in(RealLine(1.25), real_space) ==
          dedekind::category::ClassicalLogic::True);

    auto bool_space =
        dedekind::category::ambient_set<BoolLine>([](const BoolLine&) {
          return dedekind::category::ClassicalLogic::True;
        });
    CHECK(dedekind::category::IsSet<decltype(bool_space)>);
    CHECK(dedekind::category::in(BoolLine(true), bool_space) ==
          dedekind::category::ClassicalLogic::True);
    CHECK(dedekind::category::in(BoolLine(false), bool_space) ==
          dedekind::category::ClassicalLogic::True);
  }

  SECTION("Set comprehension syntax works for vector carriers") {
    using namespace dedekind::sets;

    auto v = element<Ω<RealLine>>;
    auto zero_line = Set{v % UniversalSet<RealLine>{} | [](const RealLine& r) {
      return r.coordinate().resolve() == 0.0;
    }};
    CHECK(zero_line(RealLine(0.0)));
    CHECK_FALSE(zero_line(RealLine(1.0)));

    auto b = element<Ω<BoolLine>>;
    auto true_line = Set{b % UniversalSet<BoolLine>{} |
                         [](const BoolLine& x) { return x.coordinate(); }};
    CHECK(true_line(BoolLine(true)));
    CHECK_FALSE(true_line(BoolLine(false)));
  }

  SECTION("Vector-space notion exists as an ETCS set and vectors are members") {
    auto vector_space =
        dedekind::category::ambient_set<RealLine>([](const RealLine&) {
          return dedekind::category::ClassicalLogic::True;
        });

    CHECK(dedekind::category::IsSet<decltype(vector_space)>);
    CHECK(dedekind::category::in(RealLine(0.0), vector_space) ==
          dedekind::category::ClassicalLogic::True);
    CHECK(dedekind::category::in(RealLine(3.25), vector_space) ==
          dedekind::category::ClassicalLogic::True);
  }

  SECTION(
      "Boolean semimodule notion exists as an ETCS set and vectors are "
      "members") {
    auto bool_semimodule =
        dedekind::category::ambient_set<BoolLine>([](const BoolLine&) {
          return dedekind::category::ClassicalLogic::True;
        });

    CHECK(dedekind::category::IsSet<decltype(bool_semimodule)>);
    CHECK(dedekind::category::in(BoolLine(true), bool_semimodule) ==
          dedekind::category::ClassicalLogic::True);
    CHECK(dedekind::category::in(BoolLine(false), bool_semimodule) ==
          dedekind::category::ClassicalLogic::True);
  }
}
