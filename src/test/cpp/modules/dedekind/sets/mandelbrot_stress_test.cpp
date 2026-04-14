#include <catch2/catch_test_macros.hpp>
#include <chrono>
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

struct ComplexPoint {
  double re;
  double im;
};

constexpr ComplexPoint parameter_of(const Pixel& p, int size) {
  return ComplexPoint{
      (2.0 * static_cast<double>(p.x) / static_cast<double>(size)) - 1.5,
      (2.0 * static_cast<double>(p.y) / static_cast<double>(size)) - 1.0};
}

constexpr bool orbit_is_bounded(const ComplexPoint& c, int max_iter) {
  double zr = 0.0;
  double zi = 0.0;

  for (int n = 0; n < max_iter; ++n) {
    const double zr2 = zr * zr;
    const double zi2 = zi * zi;
    if (zr2 + zi2 > 4.0) return false;

    // z_{n+1} = z_n^2 + c
    zi = 2.0 * zr * zi + c.im;
    zr = zr2 - zi2 + c.re;
  }

  return true;
}

constexpr bool mandelbrot_member(const Pixel& p, int size, int max_iter) {
  const auto c = parameter_of(p, size);
  return orbit_is_bounded(c, max_iter);
}

constexpr auto mandelbrot_set_n(int size, int max_iter) {
  auto p = var<Ω<Pixel>>;

  const auto in_grid = Set{p % Ω<Pixel>{} | [size](const Pixel& px) {
    return px.x >= 0 && px.x < size && px.y >= 0 && px.y < size;
  }};

  const auto bounded_orbit =
      Set{p % Ω<Pixel>{} | [size, max_iter](const Pixel& px) {
            return mandelbrot_member(px, size, max_iter);
  }};

  // Finite approximation M_N = {p in grid | orbit(parameter_of(p)) bounded up
  // to N iterations}.
  return in_grid & bounded_orbit;
}

std::vector<std::uint8_t> render_mandelbrot_pbm_bits(int size, int max_iter) {
  const auto mandelbrot = mandelbrot_set_n(size, max_iter);

  std::vector<std::uint8_t> bytes;
  bytes.reserve(static_cast<std::size_t>(size) *
                static_cast<std::size_t>((size + 7) / 8));

  for (int y = 0; y < size; ++y) {
    std::uint8_t acc = 0;
    int bit_count = 0;

    for (int x = 0; x < size; ++x) {
      acc <<= 1;
      if (mandelbrot(Pixel{x, y}) == dedekind::category::Ternary::True)
        acc |= 1U;
      ++bit_count;

      if (bit_count == 8) {
        bytes.push_back(acc);
        acc = 0;
        bit_count = 0;
      }
    }

    if (bit_count != 0) {
      acc <<= (8 - bit_count);
      bytes.push_back(acc);
    }
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

TEST_CASE("Sets: Mandelbrot set-builder stress test",
          "[sets][stress][mandelbrot]") {
  SECTION("Known points agree with Mandelbrot membership") {
    const int size = 1024;
    const int max_iter = 50;
    const auto mandelbrot = mandelbrot_set_n(size, max_iter);

    REQUIRE(mandelbrot(Pixel{size / 2, size / 2}) ==
            dedekind::category::Ternary::True);
    REQUIRE(mandelbrot(Pixel{0, 0}) == dedekind::category::Ternary::False);
    REQUIRE(mandelbrot(Pixel{size - 1, size - 1}) ==
            dedekind::category::Ternary::False);
  }

  SECTION("Benchmark-style rendering under load") {
    const int size = 512;
    const int max_iter = 50;
    const auto t0 = std::chrono::steady_clock::now();
    const auto bytes = render_mandelbrot_pbm_bits(size, max_iter);
    const auto t1 = std::chrono::steady_clock::now();

    const auto elapsed_ms =
        std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count();
    const auto checksum = fnv1a64(bytes);

    WARN("mandelbrot_size=" << size << ", elapsed_ms=" << elapsed_ms
                            << ", checksum=" << checksum);

    REQUIRE(bytes.size() == static_cast<std::size_t>(size) *
                                static_cast<std::size_t>((size + 7) / 8));
    REQUIRE(checksum != 0ULL);
  }
}
