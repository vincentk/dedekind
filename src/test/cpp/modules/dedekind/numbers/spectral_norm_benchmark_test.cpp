#include <cassert>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>
#include <chrono>
#include <cmath>
#include <concepts>
#include <cstddef>
#include <vector>

import dedekind.numbers;
import dedekind.sequences;

using namespace dedekind::numbers;
using namespace dedekind::sequences;

namespace {

template <typename>
inline constexpr bool dependent_false_v = false;

template <typename S>
using DenseVector = std::vector<S>;

template <typename S>
concept RationalLike = requires(S x) {
  typename S::Domain;
  { x.num() } -> std::same_as<typename S::Domain>;
  { x.den() } -> std::same_as<typename S::Domain>;
};

template <typename S>
constexpr auto lift_real(double x) {
  if constexpr (std::floating_point<S>) {
    return static_cast<S>(x);
  } else if constexpr (requires {
                         typename S::scalar_type;
                         S{typename S::scalar_type{},
                           typename S::scalar_type{}};
                       }) {
    using R = typename S::scalar_type;
    return S{static_cast<R>(x), R{}};
  } else {
    static_assert(dependent_false_v<S>,
                  "lift_real<S> requires either a floating-point scalar or a"
                  " complex-like scalar with `scalar_type` and two-argument"
                  " construction");
  }
}

template <typename S>
constexpr auto scalar_zero() {
  if constexpr (RationalLike<S>) {
    return S{typename S::Domain{0}, typename S::Domain{1}};
  } else {
    return lift_real<S>(0.0);
  }
}

template <typename S>
constexpr auto scalar_one() {
  if constexpr (RationalLike<S>) {
    return S{typename S::Domain{1}, typename S::Domain{1}};
  } else {
    return lift_real<S>(1.0);
  }
}

template <typename S>
double project_real(const S& x) {
  // FIXME(https://github.com/vincentk/dedekind/issues/138): Replace this
  // bridge with a production scalar-to-machine-real projection morphism.
  if constexpr (std::floating_point<S>) {
    return static_cast<double>(x);
  } else if constexpr (requires { x.real(); }) {
    return static_cast<double>(x.real());
  } else if constexpr (RationalLike<S>) {
    return static_cast<double>(x.num()) / static_cast<double>(x.den());
  } else {
    return static_cast<double>(x);
  }
}

template <typename S>
auto as_sequence(const DenseVector<S>& data) {
  return FinitePath<S>{[&data](std::size_t i) { return data.at(i); },
                       data.size()};
}

// Benchmarks Game spectral-norm matrix entry:
// A(i,j) = 1 / ( ((i+j)(i+j+1)/2) + i + 1 )
template <typename S>
constexpr auto spectral_entry(std::size_t i, std::size_t j) {
  const std::size_t ij = i + j;
  const std::size_t denom = ((ij * (ij + 1u)) / 2u) + i + 1u;

  if constexpr (RationalLike<S>) {
    return S{typename S::Domain{1}, static_cast<typename S::Domain>(denom)};
  } else {
    return lift_real<S>(1.0 / static_cast<double>(denom));
  }
}

template <typename S>
void apply_A(const FinitePath<S>& in, DenseVector<S>& out) {
  const std::size_t n = in.size();
  assert(out.size() == n && "apply_A: output vector size must match input");
  for (std::size_t i = 0; i < n; ++i) {
    auto sum = scalar_zero<S>();
    for (std::size_t j = 0; j < n; ++j) {
      sum = sum + (spectral_entry<S>(i, j) * in.at(j));
    }
    out[i] = sum;
  }
}

template <typename S>
void apply_At(const FinitePath<S>& in, DenseVector<S>& out) {
  const std::size_t n = in.size();
  assert(out.size() == n && "apply_At: output vector size must match input");
  for (std::size_t i = 0; i < n; ++i) {
    auto sum = scalar_zero<S>();
    for (std::size_t j = 0; j < n; ++j) {
      sum = sum + (spectral_entry<S>(j, i) * in.at(j));
    }
    out[i] = sum;
  }
}

// Extraction seam for #164: operator composition is isolated here and receives
// explicit workspace storage rather than allocating internally.
template <typename S>
void apply_AtA(const DenseVector<S>& in, DenseVector<S>& out,
               DenseVector<S>& workspace) {
  assert(out.size() == in.size() &&
         "apply_AtA: output vector size must match input");
  assert(workspace.size() == in.size() &&
         "apply_AtA: workspace vector size must match input");
  apply_A(as_sequence(in), workspace);
  apply_At(as_sequence(workspace), out);
}

template <typename S>
double inner_product_energy(const DenseVector<S>& lhs,
                            const DenseVector<S>& rhs) {
  assert(lhs.size() == rhs.size() &&
         "inner_product_energy: input vector sizes must match");
  const std::size_t n = lhs.size();
  double sum = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    sum += project_real(lhs[i] * rhs[i]);
  }
  return sum;
}

template <typename S>
double spectral_norm(std::size_t n, std::size_t power_iterations = 10u) {
  if (n == 0u) return 0.0;
  if (power_iterations == 0u) return 0.0;

  DenseVector<S> u(n, scalar_one<S>());
  DenseVector<S> v(n, scalar_zero<S>());
  DenseVector<S> tmp(n, scalar_zero<S>());

  for (std::size_t i = 0; i < power_iterations; ++i) {
    apply_AtA(u, v, tmp);
    apply_AtA(v, u, tmp);
  }

  const double vBv = inner_product_energy(u, v);
  const double vv = inner_product_energy(v, v);
  return std::sqrt(vBv / vv);
}

}  // namespace

TEST_CASE("Numbers: spectral norm benchmark kernels", "[numbers][spectral]") {
  SECTION("Matrix entry formula matches canonical values") {
    REQUIRE_THAT(project_real(spectral_entry<double>(0u, 0u)),
                 Catch::Matchers::WithinRel(1.0, 1e-12));
    REQUIRE_THAT(project_real(spectral_entry<double>(0u, 1u)),
                 Catch::Matchers::WithinRel(0.5, 1e-12));
    REQUIRE_THAT(project_real(spectral_entry<double>(1u, 0u)),
                 Catch::Matchers::WithinRel(1.0 / 3.0, 1e-12));
    REQUIRE_THAT(project_real(spectral_entry<double>(1u, 1u)),
                 Catch::Matchers::WithinRel(0.2, 1e-12));
  }

  SECTION("Power iteration tracks known benchmark values") {
    REQUIRE_THAT(spectral_norm<double>(10u),
                 Catch::Matchers::WithinRel(1.271844019, 1e-8));
    REQUIRE_THAT(spectral_norm<double>(100u),
                 Catch::Matchers::WithinRel(1.274219991, 1e-8));
  }

  SECTION("Scalar-generic pipeline also supports complex carriers") {
    REQUIRE_THAT(spectral_norm<Complex<double>>(10u),
                 Catch::Matchers::WithinRel(1.271844019, 1e-8));
  }
}

TEST_CASE("Numbers: spectral norm benchmark-style run",
          "[numbers][spectral][.stress][.benchmark]") {
  const std::size_t n = 500u;
  const auto t0 = std::chrono::steady_clock::now();
  const double result = spectral_norm<double>(n);
  const auto t1 = std::chrono::steady_clock::now();

  const auto elapsed_ms =
      std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count();

  INFO("n=" << n << ", elapsed_ms=" << elapsed_ms << ", value=" << result);

  REQUIRE_THAT(result, Catch::Matchers::WithinRel(1.274224116, 1e-8));
}
