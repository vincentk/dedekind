#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <complex>
#include <cstdint>
#include <string>

import dedekind.category;
import dedekind.sets;
import dedekind.sequences;
import dedekind.numbers;

using namespace dedekind::category;
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

// Map escape time to a shading character.
//   nullopt     → '#'  (inside M: no escape witnessed)
//   fast escape → ' '  (far outside)
//   slow escape → 'O'  (near the boundary)
char shade(const std::optional<std::size_t>& escape_time,
           std::size_t max_iter) {
  if (!escape_time.has_value()) return '#';
  constexpr std::string_view palette = " .:-=+*oO";
  const auto idx = (*escape_time * (palette.size() - 1)) / max_iter;
  return palette[idx];
}

std::string render_ascii_art(int size, std::size_t max_iter, double cutoff) {
  const double cutoff_sq = cutoff * cutoff;
  std::string ascii;
  ascii.reserve(static_cast<std::size_t>(size) *
                static_cast<std::size_t>(size + 1));
  for (int y = 0; y < size; ++y) {
    for (int x = 0; x < size; ++x) {
      const auto c = parameter_of(LatticePoint{x, y}, size);
      const auto et =
          orbit_escape_time(mandelbrot_orbit(c), max_iter, cutoff_sq);
      ascii.push_back(shade(et, max_iter));
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

  SECTION("orbit_escape_time: bounded and escaping points") {
    const auto criterion = euclidean_escape_radius_squared<double>();

    // c = 0: orbit is identically 0, never escapes
    REQUIRE(!orbit_escape_time(mandelbrot_orbit(ComplexPoint{0.0, 0.0}), 50u,
                               criterion)
                 .has_value());

    // c = -0.5: inside the main cardioid, bounded
    REQUIRE(!orbit_escape_time(mandelbrot_orbit(ComplexPoint{-0.5, 0.0}), 50u,
                               criterion)
                 .has_value());

    // c = 2: z_0=0, z_1=2 (|2|²=4, not >4), z_2=6 (|6|²=36>4) → escapes at 2
    const auto et_2 = orbit_escape_time(
        mandelbrot_orbit(ComplexPoint{2.0, 0.0}), 50u, criterion);
    REQUIRE(et_2.has_value());
    REQUIRE(*et_2 == 2u);

    // Points farther out escape sooner
    const auto et_10 = orbit_escape_time(
        mandelbrot_orbit(ComplexPoint{10.0, 0.0}), 50u, criterion);
    REQUIRE(et_10.has_value());
    REQUIRE(*et_10 < *et_2);
  }

  SECTION("orbit_divergence_path: Kleene running state") {
    const auto criterion = euclidean_escape_radius_squared<double>();
    const auto divergence = orbit_divergence_path(
        mandelbrot_orbit(ComplexPoint{2.0, 0.0}), criterion);

    static_assert(IsSequence<decltype(divergence)>);
    REQUIRE(divergence.at(0) == Ternary::Unknown);  // z_0=0, inside ball
    REQUIRE(divergence.at(1) == Ternary::Unknown);  // z_1=2, |2|²=4 not >4
    REQUIRE(divergence.at(2) == Ternary::True);     // z_2=6, |6|²=36>4
  }

  SECTION("orbit_divergence_path is monotone: True is absorbing") {
    const auto criterion = euclidean_escape_radius_squared<double>();
    const auto divergence = orbit_divergence_path(
        mandelbrot_orbit(ComplexPoint{2.0, 0.0}), criterion);

    // True once escape is witnessed; stays True at all later depths
    REQUIRE(divergence.at(2) == Ternary::True);
    REQUIRE(divergence.at(3) == Ternary::True);
    REQUIRE(divergence.at(10) == Ternary::True);

    // Bounded orbit stays Unknown for all queried depths
    const auto bounded_divergence = orbit_divergence_path(
        mandelbrot_orbit(ComplexPoint{0.0, 0.0}), criterion);
    REQUIRE(bounded_divergence.at(0) == Ternary::Unknown);
    REQUIRE(bounded_divergence.at(50) == Ternary::Unknown);
  }

  SECTION("orbit_divergence_path and orbit_escape_time are consistent") {
    const auto criterion = euclidean_escape_radius_squared<double>();
    const auto orbit = mandelbrot_orbit(ComplexPoint{2.0, 0.0});
    const auto divergence = orbit_divergence_path(orbit, criterion);
    const auto et = orbit_escape_time(orbit, 50u, criterion);

    REQUIRE(et.has_value());
    // Unknown strictly before escape time, True from escape time onward
    REQUIRE(divergence.at(*et - 1) == Ternary::Unknown);
    REQUIRE(divergence.at(*et) == Ternary::True);
  }

  SECTION("orbit_escape_time: boolean collapse via has_value()") {
    const auto criterion = euclidean_escape_radius_squared<double>();
    REQUIRE(!orbit_escape_time(mandelbrot_orbit(ComplexPoint{0.0, 0.0}), 50u,
                               criterion)
                 .has_value());
    REQUIRE(!orbit_escape_time(mandelbrot_orbit(ComplexPoint{-0.5, 0.0}), 50u,
                               criterion)
                 .has_value());
    REQUIRE(orbit_escape_time(mandelbrot_orbit(ComplexPoint{2.0, 0.0}), 50u,
                              criterion)
                .has_value());
    REQUIRE(orbit_escape_time(mandelbrot_orbit(ComplexPoint{-2.5, 0.0}), 50u,
                              criterion)
                .has_value());
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

  SECTION("M_kleene_N: Layer 2 Ternary membership before Boolean collapse") {
    const auto criterion = euclidean_escape_radius_squared<double>();
    const auto kleene = M_kleene_N<double>(50u, criterion);

    // In-set points: orbit never escapes → Unknown (open question)
    REQUIRE(kleene(ComplexPoint{0.0, 0.0}) == Ternary::Unknown);
    REQUIRE(kleene(ComplexPoint{-0.5, 0.0}) == Ternary::Unknown);

    // Out-of-set points: escape witnessed → True
    REQUIRE(kleene(ComplexPoint{2.0, 0.0}) == Ternary::True);
    REQUIRE(kleene(ComplexPoint{-2.5, 0.0}) == Ternary::True);
  }

  SECTION("M_N: Inclusive policy (outer approximation, M_N ⊇ M_true)") {
    const auto criterion = euclidean_escape_radius_squared<double>();
    const auto m50 = M_N<double>(50u, criterion);
    using Logic = typename decltype(m50)::logic_species;

    // Unknown (undecided) → True (in M): standard Mandelbrot rendering
    REQUIRE(m50(ComplexPoint{0.0, 0.0}) == Logic::True);
    REQUIRE(m50(ComplexPoint{-0.5, 0.0}) == Logic::True);
    // Escaped → False (not in M)
    REQUIRE(m50(ComplexPoint{2.0, 0.0}) == Logic::False);
    REQUIRE(m50(ComplexPoint{-2.5, 0.0}) == Logic::False);
  }

  SECTION("M_N: Exclusive policy (inner approximation, M_N ⊆ M_true)") {
    const auto criterion = euclidean_escape_radius_squared<double>();
    const auto m50_excl = M_N<double>(50u, criterion, KleenePolicy::Exclusive);
    using Logic = typename decltype(m50_excl)::logic_species;

    // Unknown (undecided) → False: finite computation cannot witness
    // boundedness
    REQUIRE(m50_excl(ComplexPoint{0.0, 0.0}) == Logic::False);
    REQUIRE(m50_excl(ComplexPoint{-0.5, 0.0}) == Logic::False);
    // Escaped → also False (not in M)
    REQUIRE(m50_excl(ComplexPoint{2.0, 0.0}) == Logic::False);
    REQUIRE(m50_excl(ComplexPoint{-2.5, 0.0}) == Logic::False);
  }

  SECTION("ASCII rendering: escape-time shading") {
    const auto ascii = render_ascii_art(24, 20u, 2.0);

    REQUIRE(ascii.size() == 24u * 25u);

    // Grid centre (x=12, y=12) → c=(-0.5, 0): deep inside M, shaded '#'
    REQUIRE(ascii[12u * 25u + 12u] == '#');

    // Top-left (x=0, y=0) → c=(-1.5, -1.0): outside M, not '#'
    REQUIRE(ascii[0] != '#');
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
