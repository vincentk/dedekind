#include <array>
#include <catch2/catch_test_macros.hpp>
import dedekind.category;
import dedekind.numbers;

using namespace dedekind::numbers;
using namespace dedekind::category;

TEST_CASE("Numbers: embed_z2_c is monic", "[numbers][lattice]") {
  static_assert(IsMonicArrow<std::decay_t<decltype(embed_z2_c)>>,
                "embed_z2_c must be declared monic.");

  const auto z = embed_z2_c({2, 5});
  REQUIRE(z.real() == 2.0);
  REQUIRE(z.imag() == 5.0);
}

TEST_CASE("Numbers: complex_lattice", "[numbers][lattice]") {
  SECTION("Contains integral grid points") {
    const auto grid = complex_lattice(4);
    using Logic = typename decltype(grid)::logic_species;
    REQUIRE(grid(Complex<double>{0, 0}) == Logic::True);
    REQUIRE(grid(Complex<double>{3, 3}) == Logic::True);
    REQUIRE(grid(Complex<double>{0, 3}) == Logic::True);
  }

  SECTION("Excludes non-integral points") {
    const auto grid = complex_lattice(4);
    using Logic = typename decltype(grid)::logic_species;
    REQUIRE(grid(Complex<double>{0.5, 0}) == Logic::False);
    REQUIRE(grid(Complex<double>{1, 1.5}) == Logic::False);
  }

  SECTION("Excludes out-of-bounds integral points") {
    const auto grid = complex_lattice(4);
    using Logic = typename decltype(grid)::logic_species;
    REQUIRE(grid(Complex<double>{4, 0}) == Logic::False);
    REQUIRE(grid(Complex<double>{-1, 0}) == Logic::False);
  }
}

TEST_CASE("Numbers: lattice factory API", "[numbers][lattice][api]") {
  SECTION("lattice<ℂ_d> supports unbounded and bounded forms") {
    const auto all = lattice<ℂ_d>;
    using LogicAll = typename decltype(all)::logic_species;
    REQUIRE(all(Complex<double>{2, 5}) == LogicAll::True);
    REQUIRE(all(Complex<double>{2.25, 5}) == LogicAll::False);

    const auto bounded = lattice<ℂ_d>.bounded(4);
    using LogicBounded = typename decltype(bounded)::logic_species;
    REQUIRE(bounded(Complex<double>{3, 3}) == LogicBounded::True);
    REQUIRE(bounded(Complex<double>{4, 0}) == LogicBounded::False);
  }

  SECTION("lattice<ℝ_d> supports unbounded and bounded forms") {
    const auto all = lattice<ℝ_d>;
    using LogicAll = typename decltype(all)::logic_species;
    using Rd = typename decltype(ℝ_d)::Domain;
    REQUIRE(all(Rd{2.0}) == LogicAll::True);
    REQUIRE(all(Rd{2.25}) == LogicAll::False);

    const auto bounded = lattice<ℝ_d>.bounded(4);
    using LogicBounded = typename decltype(bounded)::logic_species;
    REQUIRE(bounded(Rd{0.0}) == LogicBounded::True);
    REQUIRE(bounded(Rd{3.0}) == LogicBounded::True);
    REQUIRE(bounded(Rd{4.0}) == LogicBounded::False);
  }

  SECTION("lattice<ℝ_d,3> models integer points in ℝ^3") {
    const auto x = lattice<ℝ_d, 3>;
    using Rd = typename decltype(ℝ_d)::Domain;
    using V3 = std::array<Rd, 3>;
    REQUIRE(x(V3{Rd{1.0}, Rd{2.0}, Rd{3.0}}));
    REQUIRE(!x(V3{Rd{1.0}, Rd{2.5}, Rd{3.0}}));
  }

  SECTION("lattice<ℂ_d,3> models Gaussian integer points in ℂ^3") {
    const auto y = lattice<ℂ_d, 3>;
    using C3 = std::array<Complex<double>, 3>;
    REQUIRE(y(C3{Complex<double>{1, 0}, Complex<double>{2, 3},
                 Complex<double>{4, 5}}));
    REQUIRE(!y(C3{Complex<double>{1, 0.5}, Complex<double>{2, 3},
                  Complex<double>{4, 5}}));
  }
}
