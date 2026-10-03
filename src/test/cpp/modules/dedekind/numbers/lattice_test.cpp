#include <array>
#include <catch2/catch_test_macros.hpp>
import dedekind.category;
import dedekind.numbers;

using namespace dedekind::numbers;
using namespace dedekind::category;

TEST_CASE("Numbers: lattice factory API", "[numbers][lattice][api]") {
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
}
