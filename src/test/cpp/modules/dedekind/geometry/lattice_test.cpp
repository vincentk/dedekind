#include <catch2/catch_test_macros.hpp>
import dedekind.category;
import dedekind.geometry;
import dedekind.sets;

using namespace dedekind::geometry;
using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Geometry: square_natural_grid", "[geometry][lattice]") {
  SECTION("Contains expected interior points") {
    const auto grid = square_natural_grid(4);
    using Logic = typename decltype(grid)::logic_species;
    REQUIRE(grid({0, 0}) == Logic::True);
    REQUIRE(grid({3, 3}) == Logic::True);
    REQUIRE(grid({0, 3}) == Logic::True);
    REQUIRE(grid({2, 1}) == Logic::True);
  }

  SECTION("Excludes out-of-bounds points") {
    const auto grid = square_natural_grid(4);
    using Logic = typename decltype(grid)::logic_species;
    REQUIRE(grid({4, 0}) == Logic::False);
    REQUIRE(grid({-1, 0}) == Logic::False);
    REQUIRE(grid({0, 4}) == Logic::False);
  }

  SECTION("Empty grid contains no points") {
    const auto grid = square_natural_grid(0);
    using Logic = typename decltype(grid)::logic_species;
    REQUIRE(grid({0, 0}) == Logic::False);
  }
}

TEST_CASE("Geometry: square_integer_grid", "[geometry][lattice]") {
  SECTION("Supports negative bounds") {
    const auto grid = square_integer_grid(-2, 2);
    using Logic = typename decltype(grid)::logic_species;
    REQUIRE(grid({-2, -2}) == Logic::True);
    REQUIRE(grid({-1, 1}) == Logic::True);
    REQUIRE(grid({1, 1}) == Logic::True);
    REQUIRE(grid({2, 0}) == Logic::False);
    REQUIRE(grid({0, -3}) == Logic::False);
  }

  SECTION("Natural grid is a subset of corresponding integer grid") {
    const int n = 4;
    const auto nat_grid = square_natural_grid(n);
    const auto int_grid = square_integer_grid(0, n);
    using NatLogic = typename decltype(nat_grid)::logic_species;
    using IntLogic = typename decltype(int_grid)::logic_species;

    for (int x = -1; x <= n; ++x) {
      for (int y = -1; y <= n; ++y) {
        if (nat_grid({x, y}) == NatLogic::True)
          REQUIRE(int_grid({x, y}) == IntLogic::True);
      }
    }
  }
}

TEST_CASE("Geometry: unbounded lattice relations", "[geometry][lattice]") {
  SECTION("Factory lattice<S> exposes 1D and 2D discretizations") {
    const auto n_line = lattice<NaturalLatticeSet>.line();
    const auto n_plane = lattice<NaturalLatticeSet>.plane();
    const auto z_line = lattice<IntegerLatticeSet>.line();
    const auto z_plane = lattice<IntegerLatticeSet>.plane();

    using NLineLogic = typename decltype(n_line)::logic_species;
    using NPlaneLogic = typename decltype(n_plane)::logic_species;
    using ZLineLogic = typename decltype(z_line)::logic_species;
    using ZPlaneLogic = typename decltype(z_plane)::logic_species;

    REQUIRE(n_line(0) == NLineLogic::True);
    REQUIRE(n_line(-1) == NLineLogic::False);
    REQUIRE(n_plane({1, 2}) == NPlaneLogic::True);
    REQUIRE(n_plane({-1, 2}) == NPlaneLogic::False);

    REQUIRE(z_line(-7) == ZLineLogic::True);
    REQUIRE(z_line(9) == ZLineLogic::True);
    REQUIRE(z_plane({-3, 4}) == ZPlaneLogic::True);
  }

  SECTION(
      "Unbounded natural lattice is a subset of unbounded integer lattice") {
    const auto nat = natural_lattice_2d();
    const auto integers = integer_lattice_2d();
    using NatLogic = typename decltype(nat)::logic_species;
    using IntLogic = typename decltype(integers)::logic_species;

    // Finite witness window for subset sanity checks.
    for (int x = -3; x <= 3; ++x) {
      for (int y = -3; y <= 3; ++y) {
        if (nat({x, y}) == NatLogic::True)
          REQUIRE(integers({x, y}) == IntLogic::True);
      }
    }
  }

  SECTION(
      "Bounded natural grid can be derived from unbounded natural lattice") {
    auto p = var_for_type<std::pair<int, int>>;
    const auto bounded =
        Set{p % natural_lattice_2d() | [](const std::pair<int, int>& q) {
          return (q.first >= 0) && (q.first < 4) && (q.second >= 0) &&
                 (q.second < 4);
        }};
    using Logic = typename decltype(bounded)::logic_species;
    REQUIRE(bounded({0, 0}) == Logic::True);
    REQUIRE(bounded({3, 3}) == Logic::True);
    REQUIRE(bounded({4, 0}) == Logic::False);
    REQUIRE(bounded({-1, 0}) == Logic::False);
  }

  SECTION("Half-space and interval restrictions from integer lattice") {
    auto p = var_for_type<std::pair<int, int>>;
    const auto half_space =
        Set{p % integer_lattice_2d() |
            [](const std::pair<int, int>& q) { return q.first >= 0; }};
    const auto interval_box =
        Set{p % integer_lattice_2d() | [](const std::pair<int, int>& q) {
          return (q.first >= -2) && (q.first < 2) && (q.second >= -2) &&
                 (q.second < 2);
        }};

    using HalfLogic = typename decltype(half_space)::logic_species;
    using BoxLogic = typename decltype(interval_box)::logic_species;

    REQUIRE(half_space({0, -100}) == HalfLogic::True);
    REQUIRE(half_space({-1, 5}) == HalfLogic::False);

    REQUIRE(interval_box({-2, -2}) == BoxLogic::True);
    REQUIRE(interval_box({1, 1}) == BoxLogic::True);
    REQUIRE(interval_box({2, 0}) == BoxLogic::False);
    REQUIRE(interval_box({0, -3}) == BoxLogic::False);
  }
}

TEST_CASE("Geometry: embed_z2_r2 is monic", "[geometry][lattice]") {
  static_assert(IsMonicArrow<std::decay_t<decltype(embed_z2_r2)>>,
                "embed_z2_r2 must be declared monic.");

  const auto v = embed_z2_r2({3, 7});
  REQUIRE(v[0] == 3.0);
  REQUIRE(v[1] == 7.0);
}

TEST_CASE("Geometry: IsBijectiveArrow", "[geometry][lattice][category]") {
  static_assert(IsBijectiveArrow<Identity<int>>,
                "Identity<int> must be bijective.");
  static_assert(IsMonicArrow<std::decay_t<decltype(embed_z2_r2)>>,
                "embed_z2_r2 is monic (injective).");
  // The embedding is not surjective onto all of ℝ², so it is not bijective.
  static_assert(!IsEpicArrow<std::decay_t<decltype(embed_z2_r2)>>,
                "embed_z2_r2 is not epic (not surjective onto ℝ²).");
}
