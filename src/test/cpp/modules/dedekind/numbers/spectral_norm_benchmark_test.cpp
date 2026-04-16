#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <span>
#include <vector>

namespace {

// Benchmarks Game spectral-norm matrix entry:
// A(i,j) = 1 / ( ((i+j)(i+j+1)/2) + i + 1 )
constexpr double spectral_entry(std::size_t i, std::size_t j) {
  const std::size_t ij = i + j;
  const double denom =
      (static_cast<double>(ij) * static_cast<double>(ij + 1) * 0.5) +
      static_cast<double>(i) + 1.0;
  return 1.0 / denom;
}

void apply_A(std::span<const double> in, std::span<double> out) {
  const std::size_t n = in.size();
  for (std::size_t i = 0; i < n; ++i) {
    double sum = 0.0;
    for (std::size_t j = 0; j < n; ++j) {
      sum += spectral_entry(i, j) * in[j];
    }
    out[i] = sum;
  }
}

void apply_At(std::span<const double> in, std::span<double> out) {
  const std::size_t n = in.size();
  for (std::size_t i = 0; i < n; ++i) {
    double sum = 0.0;
    for (std::size_t j = 0; j < n; ++j) {
      sum += spectral_entry(j, i) * in[j];
    }
    out[i] = sum;
  }
}

// Extraction seam for #164: operator composition is isolated here and receives
// explicit workspace storage rather than allocating internally.
void apply_AtA(std::span<const double> in, std::span<double> out,
               std::span<double> workspace) {
  apply_A(in, workspace);
  apply_At(workspace, out);
}

double dot(std::span<const double> lhs, std::span<const double> rhs) {
  const std::size_t n = lhs.size();
  double sum = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    sum += lhs[i] * rhs[i];
  }
  return sum;
}

double spectral_norm(std::size_t n, std::size_t power_iterations = 10u) {
  std::vector<double> u(n, 1.0);
  std::vector<double> v(n, 0.0);
  std::vector<double> tmp(n, 0.0);

  for (std::size_t i = 0; i < power_iterations; ++i) {
    apply_AtA(u, v, tmp);
    apply_AtA(v, u, tmp);
  }

  const double vBv = dot(u, v);
  const double vv = dot(v, v);
  return std::sqrt(vBv / vv);
}

}  // namespace

TEST_CASE("Numbers: spectral norm benchmark kernels", "[numbers][spectral]") {
  SECTION("Matrix entry formula matches canonical values") {
    REQUIRE(spectral_entry(0u, 0u) == 1.0);
    REQUIRE(spectral_entry(0u, 1u) == 0.5);
    REQUIRE(spectral_entry(1u, 0u) == (1.0 / 3.0));
    REQUIRE(spectral_entry(1u, 1u) == 0.2);
  }

  SECTION("Power iteration tracks known benchmark values") {
    REQUIRE_THAT(spectral_norm(10u),
                 Catch::Matchers::WithinAbs(1.271844019, 1e-9));
    REQUIRE_THAT(spectral_norm(100u),
                 Catch::Matchers::WithinAbs(1.274219991, 1e-9));
  }
}

TEST_CASE("Numbers: spectral norm benchmark-style run",
          "[numbers][spectral][.stress][.benchmark]") {
  const std::size_t n = 500u;
  const auto t0 = std::chrono::steady_clock::now();
  const double result = spectral_norm(n);
  const auto t1 = std::chrono::steady_clock::now();

  const auto elapsed_ms =
      std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count();

  INFO("n=" << n << ", elapsed_ms=" << elapsed_ms << ", value=" << result);

  REQUIRE_THAT(result, Catch::Matchers::WithinAbs(1.274224116, 1e-9));
}
