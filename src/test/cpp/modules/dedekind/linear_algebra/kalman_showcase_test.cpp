/** @file dedekind/linear_algebra/kalman_showcase_test.cpp
 *
 * Showcase~13 (paper §5.X) — a scalar Kalman filter as a compile-time
 * specialised primitive, with its certificates as types.  The noises are
 * positive by type (ℚ₊), the covariance stays positive by the closure laws of
 * ℚ₊ (the Joseph form P' = P·R/S), and the innovation covariance
 * S = h²P + R is positive by the same laws, so the gain K = hP/S divides by a
 * certified divisor: no check at run time, none at compile time either.  The
 * predict/update pair is `constexpr`, and a measurement trajectory is carried
 * from initial state to posterior estimate by the compiler, in exact
 * rationals.  Embedded sensor fusion under the Axiomatic Systems Programming
 * discipline: fixed topology, exact arithmetic, deterministic, no runtime
 * dependency.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;

using dedekind::category::NonZero;
using dedekind::numbers::Positive;
using dedekind::numbers::square;
using dedekind::sets::RatExpr;

namespace {

using Rat = RatExpr<>;

/**
 * @brief A stationary scalar Kalman filter.  State transition F = 1 (a
 *        constant-position model); the measurement gain @ref h is non-zero
 *        by type.  Runtime state: the estimate @ref x_hat and its covariance
 *        @ref P, positive by type.
 */
struct KalmanFilter1D {
  Positive Q;      // process noise
  Positive R;      // measurement noise
  NonZero<Rat> h;  // measurement gain
  Rat x_hat;
  Positive P;

  constexpr void predict() {
    // F = 1: x_hat unchanged, the covariance grows by the process noise.
    P = P + Q;
  }

  constexpr void update(const Rat& z) {
    const Rat innovation = z - h.value() * x_hat;
    const Positive S = square(h) * P + R;     // ℚ₊ · ℚ₊ + ℚ₊: positive by law
    const Rat K = h.value() * P.value() / S;  // divides by a certified divisor
    x_hat = x_hat + K * innovation;
    P = P * R / S;  // (1 − K h) P = P R / S, in ℚ₊
  }
};

/** @return the filter of the hand-check: Q = 1/100, R = 1/10, h = 1, x̂ = 0,
 *  P = 1. */
constexpr KalmanFilter1D filter() {
  return {Positive::literal<1>() / Positive::literal<100>(),
          Positive::literal<1>() / Positive::literal<10>(),
          NonZero<Rat>::literal<1>(), Rat{0}, Positive::literal<1>()};
}

}  // namespace

TEST_CASE(
    "linear_algebra:kalman — scalar filter, one step, compile-time trajectory",
    "[linear_algebra][kalman][showcase]") {
  // Hand-check, a single measurement z = 1:
  //   after predict():   P' = 1 + 1/100 = 101/100
  //   update(z = 1):     y = 1 - 0 = 1
  //                      S = 101/100 + 1/10 = 111/100
  //                      K = (101/100) / (111/100) = 101/111
  //                      x_hat' = 0 + (101/111) * 1 = 101/111
  //                      P''    = (101/100) (1/10) / (111/100) = 101/1110
  constexpr auto filtered = [] {
    KalmanFilter1D f = filter();
    f.predict();
    f.update(Rat{1});
    return f;
  }();

  static_assert(filtered.x_hat == Rat{101, 111},
                "posterior estimate must be exactly 101/111");
  static_assert(filtered.P.value() == Rat{101, 1110},
                "posterior covariance must be exactly 101/1110");

  CHECK(filtered.x_hat == Rat{101, 111});
  SUCCEED("Showcase 13: compile-time-specialised scalar Kalman step.");
}

TEST_CASE(
    "linear_algebra:kalman — two steps, compile-time trajectory converges",
    "[linear_algebra][kalman][showcase]") {
  // Two identical measurements z = 1.  The covariance strictly decreases (the
  // filter gains confidence); exact rationals make that a compile-time fact.
  constexpr auto after_one = [] {
    KalmanFilter1D f = filter();
    f.predict();
    f.update(Rat{1});
    return f;
  }();
  constexpr auto after_two = [] {
    KalmanFilter1D f = filter();
    f.predict();
    f.update(Rat{1});
    f.predict();
    f.update(Rat{1});
    return f;
  }();

  static_assert(after_two.P.value() < after_one.P.value());
  CHECK(after_two.P.value() < after_one.P.value());

  SUCCEED(
      "Showcase 13: two-step covariance contraction as a compile-time fact.");
}
