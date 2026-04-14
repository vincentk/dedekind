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

constexpr bool mandelbrot_member(const Pixel& p, int size) {
  const double cr =
      (2.0 * static_cast<double>(p.x) / static_cast<double>(size)) - 1.5;
  const double ci =
      (2.0 * static_cast<double>(p.y) / static_cast<double>(size)) - 1.0;

  double zr = 0.0;
  double zi = 0.0;

  for (int iter = 0; iter < 50; ++iter) {
    const double zr2 = zr * zr;
    const double zi2 = zi * zi;
    if (zr2 + zi2 > 4.0) return false;

    zi = 2.0 * zr * zi + ci;
    zr = zr2 - zi2 + cr;
  }

  return true;
}

constexpr auto mandelbrot_set(int size) {
  auto pixel = var<Ω<Pixel>>;

  const auto in_grid = Set{pixel % Ω<Pixel>{} | [size](const Pixel& p) {
    return p.x >= 0 && p.x < size && p.y >= 0 && p.y < size;
  }};

  const auto in_mandelbrot = Set{pixel % Ω<Pixel>{} | [size](const Pixel& p) {
    return mandelbrot_member(p, size);
  }};

  // Idiomatic set-builder composition: support set ∩ Mandelbrot predicate set.
  return in_grid & in_mandelbrot;
}

std::vector<std::uint8_t> render_mandelbrot_pbm_bits(int size) {
  const auto mandelbrot = mandelbrot_set(size);

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
    const auto mandelbrot = mandelbrot_set(size);

    REQUIRE(mandelbrot(Pixel{size / 2, size / 2}) ==
            dedekind::category::Ternary::True);
    REQUIRE(mandelbrot(Pixel{0, 0}) == dedekind::category::Ternary::False);
    REQUIRE(mandelbrot(Pixel{size - 1, size - 1}) ==
            dedekind::category::Ternary::False);
  }

  SECTION("Benchmark-style rendering under load") {
    const int size = 512;
    const auto t0 = std::chrono::steady_clock::now();
    const auto bytes = render_mandelbrot_pbm_bits(size);
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
