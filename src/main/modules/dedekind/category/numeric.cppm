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
 * Overflow or support breach is surfaced as Ω_K3::Unknown.
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
		if (__builtin_add_overflow(a, b, &result)) {
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
 * @concept IsNumericHoleClassifier
 * @brief A characteristic morphism χ: T -> Ω_K3 for numeric support.
 */
export template <typename Chi, typename T>
concept IsNumericHoleClassifier =
		IsNumericSpecies<T> && IsCharacteristic<Chi> &&
		std::same_as<Dom<Chi>, T> && std::same_as<Cod<Chi>, Ternary>;

// Honesty anchors
static_assert(IsLipschitzBoundaryPolicy<IntervalBoundaryPolicy<int>, int>);
static_assert(IsLipschitzBoundaryPolicy<FullMachineBoundaryPolicy<int>, int>);
static_assert(IsLipschitzBoundaryPolicy<NaNHolePolicy<double>, double>);

}  // namespace dedekind::category
