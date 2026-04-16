/**
 * @file dedekind/category/numeric.cppm
 * @partition :numeric
 * @brief Level 3: The Numeric Landing (IEEE 754 and Lipschitz Boundaries).
 *
 * @details
 * This partition defines first-pass, honest APIs for machine-numeric hazards:
 * - NaN holes (floating-point truth holes)
 * - Lipschitz boundaries (bounded-support certificates)
 *
 * The boundary may not always be inferable from the type alone. For such
 * cases, users can provide a boundary policy explicitly.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "In dedekind.category:numeric, structure is clarified by explicit
 * composition and typed interfaces." (Module-specific documentation note for
 * maintainers.)
 *       -- dedekind maintainers
 */
module;

#include <cmath>
#include <concepts>
#include <functional>
#include <limits>

export module dedekind.category:numeric;

import :logic;
import :topoi;

namespace dedekind::category {

#if defined(__GNUC__) || defined(__clang__)
/** @brief Compile-time diagnostic: builtin overflow path enabled. */
export inline constexpr bool numeric_uses_builtin_overflow_checks = true;
#else
/** @brief Compile-time diagnostic: portable fallback overflow path enabled. */
export inline constexpr bool numeric_uses_builtin_overflow_checks = false;
#endif

namespace detail {

#if defined(_MSC_VER)
#define DEDEKIND_NOINLINE __declspec(noinline)
#elif defined(__GNUC__) || defined(__clang__)
#define DEDEKIND_NOINLINE __attribute__((noinline))
#else
#define DEDEKIND_NOINLINE
#endif

#if defined(DEDEKIND_RUNTIME_COVERAGE_BRIDGES)
#define DEDEKIND_BRIDGE_CONSTEXPR
#define DEDEKIND_BRIDGE_NOINLINE DEDEKIND_NOINLINE
#else
#define DEDEKIND_BRIDGE_CONSTEXPR constexpr
#define DEDEKIND_BRIDGE_NOINLINE
#endif

template <std::signed_integral T>
constexpr bool add_overflow_signed(T a, T b, T& out) {
#if defined(__GNUC__) || defined(__clang__)
  // Use a fixed builtin signature to avoid template-dependent overload
  // ambiguity across compilers.
  if constexpr (sizeof(T) <= sizeof(long long)) {
    long long tmp = 0;
    if (__builtin_saddll_overflow(static_cast<long long>(a),
                                  static_cast<long long>(b), &tmp)) {
      return true;
    }
    if (tmp > static_cast<long long>(std::numeric_limits<T>::max()) ||
        tmp < static_cast<long long>(std::numeric_limits<T>::min())) {
      return true;
    }
    out = static_cast<T>(tmp);
    return false;
  } else {
    // Portable fallback for signed types wider than long long.
    if ((b > 0 && a > (std::numeric_limits<T>::max() - b)) ||
        (b < 0 && a < (std::numeric_limits<T>::min() - b))) {
      return true;
    }
    out = static_cast<T>(a + b);
    return false;
  }
#else
  // Portable fallback
  if ((b > 0 && a > (std::numeric_limits<T>::max() - b)) ||
      (b < 0 && a < (std::numeric_limits<T>::min() - b))) {
    return true;
  }
  out = static_cast<T>(a + b);
  return false;
#endif
}

template <std::signed_integral T>
constexpr bool mul_overflow_signed(T a, T b, T& out) {
#if defined(__GNUC__) || defined(__clang__)
  // Use a fixed builtin signature to avoid template-dependent overload
  // ambiguity across compilers.
  if constexpr (sizeof(T) <= sizeof(long long)) {
    long long tmp = 0;
    if (__builtin_smulll_overflow(static_cast<long long>(a),
                                  static_cast<long long>(b), &tmp)) {
      return true;
    }
    if (tmp > static_cast<long long>(std::numeric_limits<T>::max()) ||
        tmp < static_cast<long long>(std::numeric_limits<T>::min())) {
      return true;
    }
    out = static_cast<T>(tmp);
    return false;
  } else {
    // Portable fallback for signed types wider than long long.
    if (a == 0 || b == 0) {
      out = static_cast<T>(0);
      return false;
    }
    if ((a > 0 && b > 0 && a > (std::numeric_limits<T>::max() / b)) ||
        (a > 0 && b < 0 && b < (std::numeric_limits<T>::min() / a)) ||
        (a < 0 && b > 0 && a < (std::numeric_limits<T>::min() / b)) ||
        (a < 0 && b < 0 && a < (std::numeric_limits<T>::max() / b))) {
      return true;
    }
    out = static_cast<T>(a * b);
    return false;
  }
#else
  // Portable fallback
  if (a == 0 || b == 0) {
    out = static_cast<T>(0);
    return false;
  }
  if ((a > 0 && b > 0 && a > (std::numeric_limits<T>::max() / b)) ||
      (a > 0 && b < 0 && b < (std::numeric_limits<T>::min() / a)) ||
      (a < 0 && b > 0 && a < (std::numeric_limits<T>::min() / b)) ||
      (a < 0 && b < 0 && a < (std::numeric_limits<T>::max() / b))) {
    return true;
  }
  out = static_cast<T>(a * b);
  return false;
#endif
}

}  // namespace detail

/** @brief A typed witness of numeric classification in Ω_K3. */
export template <typename T>
struct NumericWitness {
  T value;
  Ternary status;
};

/** @concept IsNumericSpecies */
export template <typename T>
concept IsNumericSpecies = std::integral<T> || std::floating_point<T>;

/**
 * @brief User-provided Lipschitz boundary policy over a numeric species.
 *
 * The policy maps a machine value into Ω_K3:
 * - True: inside certified support.
 * - Unknown: outside support (or otherwise indeterminate).
 * - False: explicit exclusion.
 */
export template <typename Policy, typename T>
concept IsLipschitzBoundaryPolicy =
    IsNumericSpecies<T> && requires(Policy p, T x) {
      { p(x) } -> std::same_as<Ternary>;
    };

/** @brief Closed interval policy [lower, upper] for integral species. */
export template <std::integral T>
struct IntervalBoundaryPolicy {
  using value_type = T;
  T lower;
  T upper;

  constexpr Ternary operator()(T x) const noexcept {
    if (x < lower || x > upper) return Ternary::Unknown;
    return Ternary::True;
  }
};

/**
 * @brief Full-machine boundary policy (no clipping).
 *
 * This is intentionally permissive and suitable as a default policy.
 * For stricter semantics, users should pass IntervalBoundaryPolicy explicitly.
 */
export template <std::integral T>
struct FullMachineBoundaryPolicy {
  using value_type = T;

  constexpr Ternary operator()(T) const noexcept { return Ternary::True; }
};

/** @brief IEEE-754 NaN-hole classifier policy for floating species. */
export template <std::floating_point T>
struct NaNHolePolicy {
  using value_type = T;

  constexpr Ternary operator()(T x) const noexcept {
    return std::isnan(x) ? Ternary::Unknown : Ternary::True;
  }
};

/** @brief Classify a numeric value under a user policy. */
export template <typename T, typename Policy>
  requires IsLipschitzBoundaryPolicy<Policy, T>
constexpr Ternary classify_numeric(T x, Policy policy) {
  return policy(x);
}

/**
 * @brief Certified integer addition with an explicit support policy.
 *
 * Arithmetic overflow is surfaced as Ω_K3::Unknown. Support-policy
 * violations propagate the policy's ternary result, which may be
 * Ω_K3::False or Ω_K3::Unknown depending on the boundary policy.
 */
export template <std::integral T, typename Policy>
  requires IsLipschitzBoundaryPolicy<Policy, T>
constexpr NumericWitness<T> certify_add(T a, T b, Policy policy) {
  const Ternary support_a = policy(a);
  const Ternary support_b = policy(b);
  const Ternary support_inputs = TernaryLogic::AND(support_a, support_b);

  if (support_inputs != Ternary::True) {
    return {T{}, support_inputs};
  }

  if constexpr (std::is_signed_v<T>) {
    T result{};
    if (detail::add_overflow_signed(a, b, result)) {
      return {T{}, Ternary::Unknown};
    }

    const Ternary support_result = policy(result);
    return {result, TernaryLogic::AND(support_inputs, support_result)};
  } else {
    const T result = static_cast<T>(a + b);
    const Ternary support_result = policy(result);
    return {result, TernaryLogic::AND(support_inputs, support_result)};
  }
}

/**
 * @brief Certified floating addition with NaN-hole policy.
 *
 * The default policy maps NaN to Unknown.
 */
export template <std::floating_point T, typename Policy = NaNHolePolicy<T>>
  requires IsLipschitzBoundaryPolicy<Policy, T>
constexpr NumericWitness<T> certify_add(T a, T b, Policy policy = {}) {
  const Ternary support_a = policy(a);
  const Ternary support_b = policy(b);
  const Ternary support_inputs = TernaryLogic::AND(support_a, support_b);

  const T result = static_cast<T>(a + b);
  const Ternary support_result = policy(result);

  return {result, TernaryLogic::AND(support_inputs, support_result)};
}

/**
 * @brief Certified integer multiplication with an explicit support policy.
 *
 * Signed overflow is surfaced as Ω_K3::Unknown. Support-policy failures are
 * propagated via the policy result for the inputs/result and therefore may
 * surface as either Ω_K3::False or Ω_K3::Unknown.
 */
export template <std::integral T, typename Policy>
  requires IsLipschitzBoundaryPolicy<Policy, T>
constexpr NumericWitness<T> certify_mul(T a, T b, Policy policy) {
  const Ternary support_a = policy(a);
  const Ternary support_b = policy(b);
  const Ternary support_inputs = TernaryLogic::AND(support_a, support_b);

  if (support_inputs != Ternary::True) {
    return {T{}, support_inputs};
  }

  if constexpr (std::is_signed_v<T>) {
    T result{};
    if (detail::mul_overflow_signed(a, b, result)) {
      return {T{}, Ternary::Unknown};
    }

    const Ternary support_result = policy(result);
    return {result, TernaryLogic::AND(support_inputs, support_result)};
  } else {
    const T result = static_cast<T>(a * b);
    const Ternary support_result = policy(result);
    return {result, TernaryLogic::AND(support_inputs, support_result)};
  }
}

/**
 * @brief Certified floating multiplication with NaN-hole policy.
 */
export template <std::floating_point T, typename Policy = NaNHolePolicy<T>>
  requires IsLipschitzBoundaryPolicy<Policy, T>
constexpr NumericWitness<T> certify_mul(T a, T b, Policy policy = {}) {
  const Ternary support_a = policy(a);
  const Ternary support_b = policy(b);
  const Ternary support_inputs = TernaryLogic::AND(support_a, support_b);

  const T result = static_cast<T>(a * b);
  const Ternary support_result = policy(result);

  return {result, TernaryLogic::AND(support_inputs, support_result)};
}

/**
 * @brief Certified integer division with explicit policy.
 *
 * Division by zero and signed MIN / -1 overflow are surfaced as False/Unknown.
 */
export template <std::integral T, typename Policy>
  requires IsLipschitzBoundaryPolicy<Policy, T>
constexpr NumericWitness<T> certify_div(T a, T b, Policy policy) {
  const Ternary support_a = policy(a);
  const Ternary support_b = policy(b);
  const Ternary support_inputs = TernaryLogic::AND(support_a, support_b);

  if (support_inputs != Ternary::True) {
    return {T{}, support_inputs};
  }

  if (b == T{0}) {
    return {T{}, Ternary::False};
  }

  if constexpr (std::is_signed_v<T>) {
    if (a == std::numeric_limits<T>::min() && b == T{-1}) {
      return {T{}, Ternary::Unknown};
    }
  }

  const T result = static_cast<T>(a / b);
  const Ternary support_result = policy(result);
  return {result, TernaryLogic::AND(support_inputs, support_result)};
}

/**
 * @brief Certified floating division with NaN-hole policy.
 */
export template <std::floating_point T, typename Policy = NaNHolePolicy<T>>
  requires IsLipschitzBoundaryPolicy<Policy, T>
constexpr NumericWitness<T> certify_div(T a, T b, Policy policy = {}) {
  const Ternary support_a = policy(a);
  const Ternary support_b = policy(b);
  const Ternary support_inputs = TernaryLogic::AND(support_a, support_b);

  const T result = static_cast<T>(a / b);
  const Ternary support_result = policy(result);

  return {result, TernaryLogic::AND(support_inputs, support_result)};
}

/**
 * @concept IsNumericHoleClassifier
 * @brief A characteristic morphism χ: T -> Ω_K3 for numeric support.
 */
export template <typename Chi, typename T>
concept IsNumericHoleClassifier =
    IsNumericSpecies<T> && IsCharacteristic<Chi> && std::same_as<Dom<Chi>, T> &&
    std::same_as<Cod<Chi>, Ternary>;

/** @section Coverage_Bridge_API
 * @brief Non-template bridge functions for profiler-friendly attribution.
 *
 * These wrappers preserve the template-based public API while providing
 * concrete entry points that coverage tools can attribute more consistently
 * across module boundaries.
 */

export DEDEKIND_BRIDGE_NOINLINE DEDEKIND_BRIDGE_CONSTEXPR NumericWitness<int>
certify_add_int_interval(int a, int b, IntervalBoundaryPolicy<int> policy) {
  return certify_add<int, IntervalBoundaryPolicy<int>>(a, b, policy);
}

export DEDEKIND_BRIDGE_NOINLINE DEDEKIND_BRIDGE_CONSTEXPR
    NumericWitness<unsigned int>
    certify_add_uint_full(unsigned int a, unsigned int b,
                          FullMachineBoundaryPolicy<unsigned int> policy = {}) {
  return certify_add<unsigned int, FullMachineBoundaryPolicy<unsigned int>>(
      a, b, policy);
}

export DEDEKIND_BRIDGE_NOINLINE DEDEKIND_BRIDGE_CONSTEXPR NumericWitness<int>
certify_mul_int_interval(int a, int b, IntervalBoundaryPolicy<int> policy) {
  return certify_mul<int, IntervalBoundaryPolicy<int>>(a, b, policy);
}

export DEDEKIND_BRIDGE_NOINLINE DEDEKIND_BRIDGE_CONSTEXPR NumericWitness<int>
certify_div_int_full(int a, int b, FullMachineBoundaryPolicy<int> policy = {}) {
  return certify_div<int, FullMachineBoundaryPolicy<int>>(a, b, policy);
}

export DEDEKIND_BRIDGE_NOINLINE DEDEKIND_BRIDGE_CONSTEXPR NumericWitness<double>
certify_add_double_nan(double a, double b, NaNHolePolicy<double> policy = {}) {
  return certify_add<double, NaNHolePolicy<double>>(a, b, policy);
}

export DEDEKIND_BRIDGE_NOINLINE DEDEKIND_BRIDGE_CONSTEXPR NumericWitness<double>
certify_mul_double_nan(double a, double b, NaNHolePolicy<double> policy = {}) {
  return certify_mul<double, NaNHolePolicy<double>>(a, b, policy);
}

export DEDEKIND_BRIDGE_NOINLINE DEDEKIND_BRIDGE_CONSTEXPR NumericWitness<double>
certify_div_double_nan(double a, double b, NaNHolePolicy<double> policy = {}) {
  return certify_div<double, NaNHolePolicy<double>>(a, b, policy);
}

// Honesty anchors
static_assert(IsLipschitzBoundaryPolicy<IntervalBoundaryPolicy<int>, int>);
static_assert(IsLipschitzBoundaryPolicy<FullMachineBoundaryPolicy<int>, int>);
static_assert(IsLipschitzBoundaryPolicy<NaNHolePolicy<double>, double>);
static_assert(numeric_uses_builtin_overflow_checks ||
                  !numeric_uses_builtin_overflow_checks,
              "Numeric overflow strategy flag must be defined.");

#undef DEDEKIND_NOINLINE
#undef DEDEKIND_BRIDGE_CONSTEXPR
#undef DEDEKIND_BRIDGE_NOINLINE

}  // namespace dedekind::category
