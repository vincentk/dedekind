/** @file dedekind/linear_algebra/kalman_showcase_test.cpp
 *
 * Showcase~13 (paper §5.X) — a scalar Kalman filter as a compile-time
 * specialised primitive. Filter coefficients (process noise Q,
 * measurement noise R) are NTTPs; predict/update are `constexpr`.
 * The entire update trajectory is exercised as a `static_assert`
 * chain, so the compiler carries the filter from initial state to
 * posterior estimate without emitting runtime code for the reduction
 * itself. This is the embedded-sensor-fusion application of the
 * Axiomatic Systems Programming discipline: fixed filter topology,
 * exact-rational arithmetic, deterministic behaviour, zero runtime
 * dependency.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.numbers;

using dedekind::numbers::Rational;

namespace {

using Rat = Rational<long>;

/**
 * @brief A stationary scalar Kalman filter with compile-time-fixed
 *        process noise @p Q and measurement noise @p Rnoise.
 *
 * State transition matrix F = 1 (constant-position model); measurement
 * matrix H = 1 (direct observation). Runtime state: the current
 * estimate @ref x_hat and its covariance @ref P. The predict / update
 * pair is `constexpr`, so a fully compile-time measurement trajectory
 * collapses to literal rationals; a runtime trajectory specialises to
 * a branch-free arithmetic inner loop with no allocations, no
 * dispatch, no external calls.
 */
template <typename R, R Q, R Rnoise>
struct KalmanFilter1D {
  R x_hat;
  R P;

  constexpr void predict() {
    // F = 1:  x_hat unchanged, covariance grows by process noise.
    P = P + Q;
  }

  constexpr void update(R z) {
    const R innovation = z - x_hat;
    const R S = P + Rnoise;
    const R K = P / S;
    x_hat = x_hat + K * innovation;
    P = (R{1L} - K) * P;
  }
};

}  // namespace

TEST_CASE(
    "linear_algebra:kalman — scalar filter, one step, compile-time trajectory",
    "[linear_algebra][kalman][showcase]") {
  // Filter: Q = 1/100, R = 1/10  (typical "noisy but reasonable" sensor).
  // Initial: x_hat = 0, P = 1.  Single measurement z = 1.
  //
  // Hand-check:
  //   after predict():   P' = 1 + 1/100 = 101/100
  //   update(z = 1):     y = 1 - 0 = 1
  //                      S = 101/100 + 1/10 = 111/100
  //                      K = (101/100) / (111/100) = 101/111
  //                      x_hat' = 0 + (101/111) * 1 = 101/111
  //                      P''    = (1 - 101/111) * 101/100
  //                             = (10/111) * (101/100)
  //                             = 1010/11100 = 101/1110
  constexpr auto filtered = [] {
    KalmanFilter1D<Rat, Rat{1L, 100L}, Rat{1L, 10L}> f{Rat{0L}, Rat{1L}};
    f.predict();
    f.update(Rat{1L});
    return f;
  }();

  static_assert(filtered.x_hat == Rat{101L, 111L},
                "posterior estimate must be exactly 101/111");
  static_assert(filtered.P == Rat{101L, 1110L},
                "posterior covariance must be exactly 101/1110");

  SUCCEED("Showcase 13: compile-time-specialised scalar Kalman step.");
}

TEST_CASE(
    "linear_algebra:kalman — two steps, compile-time trajectory converges",
    "[linear_algebra][kalman][showcase]") {
  // Same filter, two identical measurements z = 1.  The covariance must
  // strictly decrease (the filter gains confidence); exact rationals let
  // us assert that mechanically.
  constexpr auto after_one = [] {
    KalmanFilter1D<Rat, Rat{1L, 100L}, Rat{1L, 10L}> f{Rat{0L}, Rat{1L}};
    f.predict();
    f.update(Rat{1L});
    return f;
  }();
  constexpr auto after_two = [] {
    KalmanFilter1D<Rat, Rat{1L, 100L}, Rat{1L, 10L}> f{Rat{0L}, Rat{1L}};
    f.predict();
    f.update(Rat{1L});
    f.predict();
    f.update(Rat{1L});
    return f;
  }();

  // Covariance strictly decreases step over step (innovations reduce
  // uncertainty).  Both values are known to the compiler as exact
  // rationals, so the ordering is a compile-time fact.
  static_assert(after_two.P < after_one.P);

  SUCCEED("Showcase 13: two-step covariance contraction as a compile-time fact.");
}
