#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <complex>
#include <cstdint>
#include <string>

import dedekind.sets;
import dedekind.sequences;
import dedekind.numbers;

using namespace dedekind::sets;
using namespace dedekind::sequences;
using namespace dedekind::numbers;

namespace {

using LatticePoint = std::complex<int>;
using ComplexPoint = Complex<double>;

ComplexPoint parameter_of(const LatticePoint& p, int size) {
  return ComplexPoint{
      (2.0 * static_cast<double>(p.real()) / static_cast<double>(size)) - 1.5,
      (2.0 * static_cast<double>(p.imag()) / static_cast<double>(size)) - 1.0};
}

std::string render_ascii_art(int size, std::size_t max_iter, double cutoff) {
  const double cutoff_sq = cutoff * cutoff;
  std::string ascii;
  ascii.reserve(static_cast<std::size_t>(size) *
                static_cast<std::size_t>(size + 1));
  for (int y = 0; y < size; ++y) {
    for (int x = 0; x < size; ++x) {
      const auto c = parameter_of(LatticePoint{x, y}, size);
      ascii.push_back(
          orbit_escapes(mandelbrot_orbit(c), max_iter, cutoff_sq) ? '.' : '#');
    }
    ascii.push_back('\n');
  }
  return ascii;
}

std::uint64_t fnv1a64(const std::string& text) {
  std::uint64_t hash = 1469598103934665603ULL;
  for (const char ch : text) {
    hash ^= static_cast<std::uint64_t>(static_cast<unsigned char>(ch));
    hash *= 1099511628211ULL;
  }
  return hash;
}

}  // namespace

TEST_CASE("Sets: Mandelbrot set-builder stress test", "[sets][mandelbrot]") {
  SECTION("Orbit starts at zero") {
    const auto orbit = mandelbrot_orbit(ComplexPoint{-0.75, 0.1});

    static_assert(IsSequence<decltype(orbit)>);
    REQUIRE(orbit.at(0).real() == 0.0);
    REQUIRE(orbit.at(0).imag() == 0.0);
  }

  SECTION("Truncated orbit prefix is a finite sequence") {
    const auto orbit = prefix(mandelbrot_orbit(ComplexPoint{-0.75, 0.1}), 9u);

    static_assert(IsFiniteSequence<decltype(orbit)>);
    REQUIRE(orbit.size() == 9u);
  }

  SECTION("orbit_escapes: bounded and escaping points") {
    // c = 0: orbit is identically 0, never escapes
    REQUIRE(!orbit_escapes(mandelbrot_orbit(ComplexPoint{0.0, 0.0}), 50u, 4.0));
    // c = -0.5: inside the main cardioid, bounded
    REQUIRE(
        !orbit_escapes(mandelbrot_orbit(ComplexPoint{-0.5, 0.0}), 50u, 4.0));
    // c = 2: z_1 = 2, z_2 = 6, escapes at step 1
    REQUIRE(orbit_escapes(mandelbrot_orbit(ComplexPoint{2.0, 0.0}), 50u, 4.0));
    // c = -2.5: clearly outside
    REQUIRE(orbit_escapes(mandelbrot_orbit(ComplexPoint{-2.5, 0.0}), 50u, 4.0));
  }

  SECTION("euclidean_escape_radius_squared: parametric threshold") {
    const auto default_criterion = euclidean_escape_radius_squared<double>();
    const auto large_criterion =
        euclidean_escape_radius_squared<double>(9.0);  // radius 3

    // |2.5| = 2.5: exceeds radius 2 but not radius 3
    const ComplexPoint test_point{2.5, 0.0};
    REQUIRE(default_criterion(test_point) == true);
    REQUIRE(large_criterion(test_point) == false);
  }

  SECTION("orbit_escapes with custom escape criterion") {
    auto custom_criterion = euclidean_escape_radius_squared<double>(16.0);

    // c = 0.25 is bounded under both default and radius-4 criteria
    const auto orbit_bounded = mandelbrot_orbit(ComplexPoint{0.25, 0.0});
    REQUIRE(!orbit_escapes(orbit_bounded, 10u, custom_criterion));
    REQUIRE(!orbit_escapes(orbit_bounded, 10u, 4.0));

    // c = 2 escapes quickly
    REQUIRE(orbit_escapes(mandelbrot_orbit(ComplexPoint{2.0, 0.0}), 10u, 4.0));
  }

  SECTION("prefix_bounded_N: fixed-depth criterion") {
    const auto in_50 = prefix_bounded_N<double>(50u);

    REQUIRE(in_50(mandelbrot_orbit(ComplexPoint{0.0, 0.0})));
    REQUIRE(in_50(mandelbrot_orbit(ComplexPoint{-0.5, 0.0})));
    REQUIRE(!in_50(mandelbrot_orbit(ComplexPoint{2.0, 0.0})));
  }

  SECTION("M_N: set membership for known interior and exterior points") {
    const auto m50 = M_N<double>(50u);
    using Logic = typename decltype(m50)::logic_species;

    REQUIRE(m50(ComplexPoint{0.0, 0.0}) == Logic::True);    // origin is in M
    REQUIRE(m50(ComplexPoint{-0.5, 0.0}) == Logic::True);   // main cardioid
    REQUIRE(m50(ComplexPoint{2.0, 0.0}) == Logic::False);   // clearly outside
    REQUIRE(m50(ComplexPoint{-2.5, 0.0}) == Logic::False);  // clearly outside
  }

  SECTION("ASCII rendering: structural checks") {
    const auto ascii = render_ascii_art(24, 20u, 2.0);

    REQUIRE(ascii.size() == 24u * 25u);

    // Grid centre (x=12, y=12) maps to c=(-0.5, 0) which is inside M
    const auto centre_offset = 12u * 25u + 12u;
    REQUIRE(ascii[centre_offset] == '#');

    // Top-left corner maps to c=(-1.5, -1.0), outside M
    REQUIRE(ascii[0] == '.');
  }
}

TEST_CASE("Sets: Mandelbrot benchmark-style rendering",
          "[sets][mandelbrot][.stress][.benchmark]") {
  const int size = 512;
  const std::size_t max_iter = 50u;
  const double cutoff = 2.0;
  const auto t0 = std::chrono::steady_clock::now();
  const auto ascii = render_ascii_art(size, max_iter, cutoff);
  const auto t1 = std::chrono::steady_clock::now();

  const auto elapsed_ms =
      std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count();
  const auto checksum = fnv1a64(ascii);

  INFO("mandelbrot_size=" << size << ", elapsed_ms=" << elapsed_ms
                          << ", checksum=" << checksum);

  REQUIRE(ascii.size() ==
          static_cast<std::size_t>(size) * static_cast<std::size_t>(size + 1));
  // To add a regression hash: run once, read checksum from INFO output above,
  // then add: REQUIRE(checksum == <value>ULL);
}
