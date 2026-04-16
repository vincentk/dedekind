#include <array>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <complex>
#include <cstdint>
#include <string>

import dedekind.sets;
import dedekind.category;
import dedekind.sequences;
import dedekind.topology;
import dedekind.numbers;

using namespace dedekind::sets;
using namespace dedekind::sequences;
using namespace dedekind::numbers;

namespace {

using LatticePoint = std::complex<int>;
using ComplexPoint = Complex<double>;

double norm_squared(const ComplexPoint& z) {
  const double re = z.real();
  const double im = z.imag();
  return (re * re) + (im * im);
}

ComplexPoint parameter_of(const LatticePoint& p, int size) {
  return ComplexPoint{
      (2.0 * static_cast<double>(p.real()) / static_cast<double>(size)) - 1.5,
      (2.0 * static_cast<double>(p.imag()) / static_cast<double>(size)) - 1.0};
}

auto truncated_mandelbrot_orbit(const ComplexPoint& c, int max_iter) {
  return prefix(mandelbrot_orbit(c), static_cast<std::size_t>(max_iter) + 1u);
}

std::size_t escape_count(const ComplexPoint& c, int max_iter, double cutoff) {
  const auto orbit = truncated_mandelbrot_orbit(c, max_iter);
  const double cutoff_sq = cutoff * cutoff;

  return count_if(orbit, [cutoff_sq](const ComplexPoint& z) {
    return norm_squared(z) > cutoff_sq;
  });
}

bool adapted_mandelbrot_member(const LatticePoint& p, int size, int max_iter,
                               double cutoff) {
  return escape_count(parameter_of(p, size), max_iter, cutoff) <= 1u;
}

constexpr auto adapted_mandelbrot_plane(int size, int max_iter, double cutoff) {
  auto c = var<ℂ>;

  return Set{c % C | [size, max_iter, cutoff](const Complex<double>& z) {
    int x = static_cast<int>(z.real());
    int y = static_cast<int>(z.imag());
    return adapted_mandelbrot_member(LatticePoint{x, y}, size, max_iter,
                                     cutoff);
  }};
}

constexpr auto benchmark_lattice(int size) { return lattice<C>.bounded(size); }

constexpr auto adapted_mandelbrot_sample(int size, int max_iter,
                                         double cutoff) {
  return adapted_mandelbrot_plane(size, max_iter, cutoff) &
         benchmark_lattice(size);
}

std::string render_ascii_art(int size, int max_iter, double cutoff) {
  const auto mandelbrot = adapted_mandelbrot_sample(size, max_iter, cutoff);
  using Logic = typename decltype(mandelbrot)::logic_species;

  std::string ascii;
  ascii.reserve(static_cast<std::size_t>(size) *
                static_cast<std::size_t>(size + 1));

  for (int y = 0; y < size; ++y) {
    for (int x = 0; x < size; ++x) {
      ascii.push_back(mandelbrot(Complex<double>{static_cast<double>(x),
                                                 static_cast<double>(y)}) ==
                              Logic::True
                          ? '#'
                          : '.');
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

TEST_CASE("Sets: adapted Mandelbrot set-builder stress test",
          "[sets][mandelbrot]") {
  SECTION("Truncated orbit prefixes are finite sequences") {
    const auto orbit = truncated_mandelbrot_orbit(ComplexPoint{-0.75, 0.1}, 8);

    static_assert(IsFiniteSequence<decltype(orbit)>);
    REQUIRE(orbit.size() == 9u);
    REQUIRE(orbit.at(0).real() == 0.0);
    REQUIRE(orbit.at(0).imag() == 0.0);
  }

  SECTION("Known points agree with the adapted benchmark predicate") {
    const int size = 256;
    const int max_iter = 50;
    const double cutoff = 2.0;
    const auto mandelbrot = adapted_mandelbrot_sample(size, max_iter, cutoff);
    using Logic = typename decltype(mandelbrot)::logic_species;

    REQUIRE(mandelbrot(Complex<double>{size / 2, size / 2}) == Logic::True);
    REQUIRE(mandelbrot(Complex<double>{0, 0}) == Logic::False);
    REQUIRE(mandelbrot(Complex<double>{size - 1, size - 1}) == Logic::False);
  }

  SECTION("ASCII rendering matches expected adapted silhouette") {
    const int size = 24;
    const int max_iter = 20;
    const double cutoff = 2.0;
    const auto ascii = render_ascii_art(size, max_iter, cutoff);
    const std::array<std::string, 24> expected_rows{
        "..................#.....", "........................",
        "................###.....", "...............####.....",
        "................##......", "............###########.",
        "............##########..", "...........############.",
        "..........##############", "....####..#############.",
        "...####################.", "...####################.",
        "######################..", "...####################.",
        "...####################.", "....####..#############.",
        "..........##############", "...........############.",
        "............##########..", "............###########.",
        "................##......", "...............####.....",
        "................###.....", "........................"};

    std::string expected;
    for (const auto& row : expected_rows) {
      expected += row;
      expected.push_back('\n');
    }

    INFO("mandelbrot_size=" << size << "\n" << ascii);

    REQUIRE(ascii.size() == static_cast<std::size_t>(size) *
                                static_cast<std::size_t>(size + 1));
    REQUIRE(ascii == expected);
  }

  SECTION("Parametrized escape criterion: custom radius") {
    // Test with a custom escape radius (3 instead of 2)
    const auto large_radius_criterion =
        euclidean_escape_radius_squared<double>(9.0);  // 3^2 = 9

    const ComplexPoint test_point{2.5, 0.0};

    // Point with magnitude 2.5 > 2 (default) but < 3
    // With default criterion (radius 2): should escape
    // With custom criterion (radius 3): should not escape
    REQUIRE(euclidean_escape_radius_squared<double>()(test_point) == true);
    REQUIRE(large_radius_criterion(test_point) == false);
  }

  SECTION("orbit_escapes with custom escape criterion") {
    // Use a larger escape radius (4^2 = 16)
    auto custom_criterion = euclidean_escape_radius_squared<double>(16.0);

    const ComplexPoint c{0.25, 0.0};  // A point in the Mandelbrot set
    const auto orbit = mandelbrot_orbit(c);

    // With radius 4, orbit at c=0.25 should not escape in first 10 iterations
    REQUIRE(orbit_escapes(orbit, 10u, custom_criterion) == false);

    // With standard radius (2^2 = 4), orbit at c=0.25 is also bounded
    REQUIRE(orbit_escapes(orbit, 10u, 4.0) == false);

    // A point clearly outside the set should escape quickly
    const auto orbit_outside = mandelbrot_orbit(ComplexPoint{2.0, 0.0});
    REQUIRE(orbit_escapes(orbit_outside, 10u, 4.0) == true);
  }
}

TEST_CASE("Sets: Mandelbrot benchmark-style rendering",
          "[sets][mandelbrot][.stress][.benchmark]") {
  const int size = 512;
  const int max_iter = 50;
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
  REQUIRE(checksum != 0ULL);
}
