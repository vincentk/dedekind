/**
 * @file dedekind/numbers/real.cppm
 * @partition :real
 * @module dedekind.numbers:real
 * @brief Level 9: Minimal real wrapper for experimental reintegration.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Die Zahlen sind freie Schöpfungen des menschlichen Geistes."
 *       ("Numbers are free creations of the human mind.")
 *       -- Richard Dedekind, Was sind und was sollen die Zahlen? (1888)
 */
module;

#include <concepts>
#include <cstddef>
#include <type_traits>
#include <utility>

export module dedekind.numbers:real;

import dedekind.category;
import dedekind.sets;
import :rational;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

// Canonical machine realization for the real scalar carrier.
export using machine_real_scalar = double;

export template <typename S>
concept IsRealCarrier = requires(S a, S b) {
  S{};
  { a + b } -> std::same_as<S>;
  { a - b } -> std::same_as<S>;
  { a * b } -> std::same_as<S>;
  { a / b } -> std::same_as<S>;
  { a < b } -> std::convertible_to<bool>;
};

export template <IsRealCarrier Q>
class Real {
 public:
  using Domain = Q;
  using path_type = Q;

  constexpr Real() = default;
  constexpr explicit Real(Q value) : value_(value) {}

  constexpr Q resolve() const { return value_; }
  constexpr Q path() const { return value_; }

  // Compatibility with prior symbolic-cut style usage.
  constexpr bool operator()(const Q& x) const { return x < value_; }

  friend constexpr bool operator==(const Real&, const Real&) = default;

  friend constexpr Real operator+(const Real& a, const Real& b) {
    return Real{a.value_ + b.value_};
  }

  friend constexpr Real operator*(const Real& a, const Real& b) {
    return Real{a.value_ * b.value_};
  }

  friend constexpr Real operator-(const Real& a, const Real& b) {
    return Real{a.value_ - b.value_};
  }

  friend constexpr Real operator/(const Real& a, const Real& b) {
    return Real{a.value_ / b.value_};
  }

 private:
  Q value_{};
};

/** @section Partial_Arithmetic_with_Ternary_Logic */

/**
 * @brief Partial addition transform for Real<S>.
 *
 * Real arithmetic uses a numeric carrier S (typically double/IEEE 754).
 * Operations succeed but may lose precision due to rounding.
 * We acknowledge this by always returning Ternary::True (the operation
 * completed) but document that the result is approximate.
 */
export template <IsRealCarrier S>
struct PartialAddReal {
  using value_type = Real<S>;
  using logic_species = TernaryLogic;

  TernaryResult<Real<S>> operator()(
      std::pair<const Real<S>&, const Real<S>&> p) const noexcept {
    auto [a, b] = p;
    return {Ternary::True, a + b};
  }
};

/**
 * @brief Partial multiplication transform for Real<S>.
 *
 * Real multiplication succeeds but may lose precision due to rounding.
 */
export template <IsRealCarrier S>
struct PartialMulReal {
  using value_type = Real<S>;
  using logic_species = TernaryLogic;

  TernaryResult<Real<S>> operator()(
      std::pair<const Real<S>&, const Real<S>&> p) const noexcept {
    auto [a, b] = p;
    return {Ternary::True, a * b};
  }
};

/**
 * @brief Partial division for Real<S> with zero-check.
 *
 * Returns Ternary::False if divisor is zero (for double), reflecting
 * the IEEE 754 behavior of producing non-finite values (±∞ or NaN).
 */
export template <IsRealCarrier S>
struct PartialDivReal {
  using value_type = Real<S>;
  using logic_species = TernaryLogic;

  TernaryResult<Real<S>> operator()(
      std::pair<const Real<S>&, const Real<S>&> p) const noexcept {
    auto [a, b] = p;
    // For floating-point types, division by zero produces inf/nan.
    // We flag this as False because non-finite output lies outside
    // the intended domain of this partial operation.
    if constexpr (std::is_floating_point_v<S>) {
      if (b.resolve() == S{0}) {
        return {Ternary::False, a / b};  // Produces ±∞ or NaN per IEEE 754
      }
    }
    return {Ternary::True, a / b};
  }
};

/**
 * @brief Identity and Associativity traits for Real arithmetic.
 *
 * Real<S> arithmetic (e.g., with S = double) is commutative but NOT
 * associative: floating-point rounding means (a+b)+c ≠ a+(b+c) in general.
 * Commutativity holds for all IEEE 754 operations (barring no special cases
 * that differ by order). Associativity-by-fiat is reserved for the
 * explicit dedekind::ieee::IEEE<F> opt-in wrapper.
 *
 * Partial identities: 0 for addition, 1 for multiplication.
 *
 * Specializations are declared in the dedekind::category namespace (see below).
 */

/**
 * @brief Embedding transform: ℚ ↪ ℝ with Ternary acknowledgment.
 *
 * The embedding of a rational Q = p/q into the reals (via IEEE 754 scalar S)
 * is **lossy**: the result is the closest representable value, not necessarily
 * exact. This transform returns Ternary::Unknown to signal potential
 * information loss due to floating-point rounding.
 *
 * In a more sophisticated model, we could check if the rational is exactly
 * representable and return True, but conservatively, we flag all embeddings
 * as Unknown.
 */
export template <IsInteger I = default_integer,
                 IsRealCarrier S = machine_real_scalar>
struct PartialEmbedRationalToReal {
  using value_type = Real<S>;
  using logic_species = TernaryLogic;

  TernaryResult<Real<S>> operator()(const Rational<I>& q) const noexcept {
    // The embedding is lossy due to IEEE 754 approximation — inline to avoid
    // forward ref
    return {Ternary::Unknown,
            Real<S>{static_cast<S>(q.num()) / static_cast<S>(q.den())}};
  }
};

/**
 * @brief Kleene traits for rational→real embedding (lossy).
 *
 * The embedding is not associative/commutative in the Kleene sense because
 * different computation orders might yield different rounded results.
 */
// Note: We intentionally do NOT declare associativity for this embedding
// because floating-point rounding violates Kleene associativity.

}  // namespace dedekind::numbers

namespace dedekind::category {

/** @brief Kleene traits for real arithmetic.
 *
 * Commutativity holds for IEEE 754 addition and multiplication.
 * Associativity does NOT hold for floating-point — that opt-in belongs
 * exclusively to dedekind::ieee::IEEE<F>.
 */
template <dedekind::numbers::IsRealCarrier S>
inline constexpr bool is_kleene_commutative_v<
    dedekind::numbers::Real<S>, dedekind::numbers::PartialAddReal<S>> = true;

template <dedekind::numbers::IsRealCarrier S>
inline constexpr dedekind::numbers::Real<S> partial_identity_v<
    dedekind::numbers::Real<S>, dedekind::numbers::PartialAddReal<S>> =
    dedekind::numbers::Real<S>{S{0}};

template <dedekind::numbers::IsRealCarrier S>
inline constexpr bool is_kleene_commutative_v<
    dedekind::numbers::Real<S>, dedekind::numbers::PartialMulReal<S>> = true;

template <dedekind::numbers::IsRealCarrier S>
inline constexpr dedekind::numbers::Real<S> partial_identity_v<
    dedekind::numbers::Real<S>, dedekind::numbers::PartialMulReal<S>> =
    dedekind::numbers::Real<S>{S{1}};

}  // namespace dedekind::category

namespace dedekind::numbers {

/**
 * @brief Machine realization arrow ℚ ↪ ℝ: Rational<I> → Real<S>.
 * @details Converts a rational p/q to the closest IEEE 754 value of type S.
 */
export template <IsInteger I = default_integer,
                 IsRealCarrier S = machine_real_scalar>
inline constexpr auto embed_ℚ_ℝ =
    arrow<Rational<I>, Real<S>>([](const Rational<I>& q) noexcept {
      return Real<S>{static_cast<S>(q.num()) / static_cast<S>(q.den())};
    });

/**
 * @brief Characteristic morphism for ℝ: the real numbers.
 * Accepts native Real<S> and delegates predecessor checks through ℚ.
 */
export template <IsRealCarrier S = machine_real_scalar,
                 IsInteger I = default_integer, typename L = ClassicalLogic,
                 typename C = ℶ_1>
struct RealsOf {
  using Domain = Real<S>;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native Real<S>: always a member of ℝ
  constexpr typename L::Ω operator()(const Real<S>&) const { return L::True; }

  // Direct parent: embed Rational<I> into ℝ via the canonical arrow.
  constexpr typename L::Ω operator()(const Rational<I>& q) const {
    return operator()(embed_ℚ_ℝ<I, S>(q));
  }

  // Delegate non-parent ancestors to ambient ℚ.
  template <typename T>
    requires(!std::same_as<T, Real<S>> && !std::same_as<T, Rational<I>>)
  constexpr typename L::Ω operator()(const T& x) const {
    return dedekind::numbers::RationalsOf<I>{}(x);
  }
};

export using RealSet = RealsOf<>;
export using ℝ = RealSet;

export inline constexpr ℝ R{};

}  // namespace dedekind::numbers

namespace dedekind::category {
template <typename Q>
struct SpeciesTraits<dedekind::numbers::Real<Q>> {
  using Domain = dedekind::numbers::Real<Q>;
  using machine_type = dedekind::numbers::Real<Q>;
};

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℚ_ℝ<>)>> =
        true;
}  // namespace dedekind::category

namespace dedekind::numbers {

static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<
                                       Real<machine_real_scalar>>(R))>,
    "RealsOf must be the canonical IsSet anchor for dedekind.numbers:real.");

}  // namespace dedekind::numbers
