#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <complex>
#include <cstdint>
#include <vector>

import dedekind.sets;
import dedekind.category;

using namespace dedekind::sets;

namespace {

struct Pixel {
  int x;
  int y;
};

using ComplexPoint = std::complex<double>;

ComplexPoint parameter_of(const Pixel& p, int size) {
  return ComplexPoint{
      (2.0 * static_cast<double>(p.x) / static_cast<double>(size)) - 1.5,
      (2.0 * static_cast<double>(p.y) / static_cast<double>(size)) - 1.0};
}

ComplexPoint orbit_step(const ComplexPoint& z, const ComplexPoint& c) {
  return z * z + c;
}

bool orbit_bounded_prefix(const ComplexPoint& c, int max_iter) {
  ComplexPoint z{0.0, 0.0};

  for (int n = 0; n < max_iter; ++n) {
    if (std::norm(z) > 4.0) return false;
    // Recurrence: z_{n+1} = z_n^2 + c
    z = orbit_step(z, c);
  }

  return true;
}

constexpr bool in_half_open_interval(int x, int lo, int hi) {
  return x >= lo && x < hi;
}

bool mandelbrot_member(const Pixel& p, int size, int max_iter) {
  const auto c = parameter_of(p, size);
  return orbit_bounded_prefix(c, max_iter);
}

constexpr auto mandelbrot_set_n(int size, int max_iter) {
  auto p = var<Ω<Pixel>>;

  const auto in_grid = Set{p % Ω<Pixel>{} | [size](const Pixel& px) {
    return in_half_open_interval(px.x, 0, size) &&
           in_half_open_interval(px.y, 0, size);
  }};

  const auto bounded_orbit =
      Set{p % Ω<Pixel>{} | [size, max_iter](const Pixel& px) {
        return mandelbrot_member(px, size, max_iter);
      }};

  // Finite approximation M_N = {p in grid | orbit(parameter_of(p)) bounded up
  // to N iterations}.
  return in_grid & bounded_orbit;
}

template <typename MandelbrotSet>
constexpr std::uint8_t pack_pbm_byte(const MandelbrotSet& mandelbrot, int y,
                                     int x_start, int size) {
  using Logic = typename MandelbrotSet::logic_species;

  std::uint8_t acc = 0;
  int bit_count = 0;

  for (int dx = 0; dx < 8 && (x_start + dx) < size; ++dx) {
    acc <<= 1;
    if (mandelbrot(Pixel{x_start + dx, y}) == Logic::True) acc |= 1U;
    ++bit_count;
  }

  // PBM rows are padded with trailing zero bits to byte alignment.
  if (bit_count < 8) acc <<= (8 - bit_count);
  return acc;
}

std::vector<std::uint8_t> render_mandelbrot_pbm_bits(int size, int max_iter) {
  const auto mandelbrot = mandelbrot_set_n(size, max_iter);

  std::vector<std::uint8_t> bytes;
  bytes.reserve(static_cast<std::size_t>(size) *
                static_cast<std::size_t>((size + 7) / 8));

  for (int y = 0; y < size; ++y) {
    for (int x = 0; x < size; x += 8)
      bytes.push_back(pack_pbm_byte(mandelbrot, y, x, size));
  }

  return bytes;
}

std::uint64_t fnv1a64(const std::vector<std::uint8_t>& bytes) {
  std::uint64_t hash = 1469598103934665603ULL;
  for (std::uint8_t b : bytes) {
    hash ^= static_cast<std::uint64_t>(b);
    hash *= 1099511628211ULL;
  }
  return hash;
}

}  // namespace

TEST_CASE("Sets: Mandelbrot set-builder stress test", "[sets][mandelbrot]") {
  SECTION("Known points agree with Mandelbrot membership") {
    const int size = 256;
    const int max_iter = 50;
    const auto mandelbrot = mandelbrot_set_n(size, max_iter);
    using Logic = typename decltype(mandelbrot)::logic_species;

    REQUIRE(mandelbrot(Pixel{size / 2, size / 2}) == Logic::True);
    REQUIRE(mandelbrot(Pixel{0, 0}) == Logic::False);
    REQUIRE(mandelbrot(Pixel{size - 1, size - 1}) == Logic::False);
  }

  SECTION("Finite approximation checksum is stable") {
    const int size = 64;
    const int max_iter = 50;
    const auto bytes = render_mandelbrot_pbm_bits(size, max_iter);
    const auto checksum = fnv1a64(bytes);

    INFO("mandelbrot_size=" << size << ", checksum=" << checksum);

    REQUIRE(bytes.size() == static_cast<std::size_t>(size) *
                                static_cast<std::size_t>((size + 7) / 8));
    REQUIRE(checksum == 1850133184385998530ULL);
  }
}

TEST_CASE("Sets: Mandelbrot benchmark-style rendering",
          "[sets][mandelbrot][.stress][.benchmark]") {
  const int size = 512;
  const int max_iter = 50;
  const auto t0 = std::chrono::steady_clock::now();
  const auto bytes = render_mandelbrot_pbm_bits(size, max_iter);
  const auto t1 = std::chrono::steady_clock::now();

  const auto elapsed_ms =
      std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count();
  const auto checksum = fnv1a64(bytes);

  INFO("mandelbrot_size=" << size << ", elapsed_ms=" << elapsed_ms
                          << ", checksum=" << checksum);

  REQUIRE(bytes.size() == static_cast<std::size_t>(size) *
                              static_cast<std::size_t>((size + 7) / 8));
  REQUIRE(checksum == 16923083633697550095ULL);
}
