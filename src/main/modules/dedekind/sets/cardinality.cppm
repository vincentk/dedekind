/**
 * @file sets/cardinality.cppm
 * @partition :cardinality
 * @brief Cardinality types and the Extensional N-limb cardinal carrier.
 *
 * @details
 * This partition owns all concrete cardinality types:
 *   - `Finite`, `ℵ<N>`, `ℵ_0`, `ℶ_1` — the cardinality ladder (moved from
 *     `:mereology`, where only the structural concepts now remain).
 *   - `ExtensionalCardinal<N>` — the machine-total finite cardinal carrier
 *     with wrapping arithmetic, compatible with `dedekind.category:total`.
 *   - `Cardinality` — the `finite | ℵ_0` sum type and its utility functions.
 *
 * Cardinality is a property of sets — how many elements a set contains —
 * so it belongs in the `sets` layer, below `numbers` and `algebra`.
 * The `numbers:cardinality` partition re-exports this and adds the
 * `IsNatural` and `IsTotallyOrdered` proofs that require the `numbers`
 * and `order` layers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Das Wesen der Mathematik liegt in ihrer Freiheit."
 *       ("The essence of mathematics lies entirely in its freedom.")
 *       -- Georg Cantor, Grundlagen einer allgemeinen
 *          Mannigfaltigkeitslehre (1883)
 */
module;

#include <array>
#include <climits>
#include <compare>
#include <concepts>
#include <cstddef>
#include <functional>
#include <limits>
#include <utility>
#include <variant>

export module dedekind.sets:cardinality;

import dedekind.category;

namespace dedekind::sets {

using namespace dedekind::category;

// ---------------------------------------------------------------------------
// Cardinality ladder (moved from :mereology)
// ---------------------------------------------------------------------------

/** @struct Finite: Hardware-bound magnitude. */
export struct Finite {
  static constexpr bool is_finite = true;
  static constexpr bool is_countable = true;

  auto operator<=>(const Finite&) const = default;

  using power_type = Finite;  // Finite sets always jump to other Finite sets.
};

/** @struct ℵ: The Transfinite Ladder. */
export template <std::size_t N>
struct ℵ {
  static constexpr bool is_finite = false;
  static constexpr bool is_countable = (N == 0);
  using power_type = ℵ<N + 1>;

  constexpr friend bool operator==(const ℵ&, const ℵ&) = default;
};

export using ℵ_0 = ℵ<0>;  ///< Countable Infinity
export using ℶ_1 = ℵ<1>;  ///< The Continuum (assuming GCH)

// ---------------------------------------------------------------------------
// Extensional cardinal carrier
// ---------------------------------------------------------------------------

/**
 * @brief Finite extensional cardinality carrier using `N` machine limbs.
 * @tparam N Number of `std::size_t` limbs stored in little-endian order.
 */
export template <std::size_t N = 1>
struct ExtensionalCardinal {
  static_assert(N > 0, "ExtensionalCardinal<N> requires N >= 1.");

  using cardinality_type = Finite;
  using limb_type = std::size_t;

  struct CheckedResult {
    ExtensionalCardinal value;
    bool overflowed;
  };

  std::array<limb_type, N> limbs{};

  static constexpr std::size_t limb_bits = CHAR_BIT * sizeof(limb_type);

  constexpr ExtensionalCardinal() = default;

  constexpr ExtensionalCardinal(limb_type value) noexcept { limbs[0] = value; }

  /** @brief Construction from any integral type via two's-complement wrapping.
   *  The explicit static_cast suppresses -Wsign-conversion at the call site. */
  template <std::integral S>
    requires(!std::same_as<S, limb_type>)
  constexpr explicit ExtensionalCardinal(S value) noexcept {
    limbs[0] = static_cast<limb_type>(value);
  }

  /** @brief Explicit conversion to floating-point (single-limb only).
   *  Constrained to N==1 to prevent silent truncation of multi-limb values. */
  template <std::floating_point F>
    requires(N == 1)
  constexpr explicit operator F() const noexcept {
    return static_cast<F>(limbs[0]);
  }

  constexpr friend bool operator==(const ExtensionalCardinal&,
                                   const ExtensionalCardinal&) = default;

  constexpr friend std::strong_ordering operator<=>(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    for (std::size_t index = N; index-- > 0;) {
      if (lhs.limbs[index] < rhs.limbs[index]) {
        return std::strong_ordering::less;
      }
      if (lhs.limbs[index] > rhs.limbs[index]) {
        return std::strong_ordering::greater;
      }
    }
    return std::strong_ordering::equal;
  }

  constexpr friend ExtensionalCardinal operator+(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    return checked_add(lhs, rhs).value;
  }

  constexpr friend ExtensionalCardinal operator*(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    return checked_mul(lhs, rhs).value;
  }

  constexpr friend ExtensionalCardinal operator-(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    return lhs + (-rhs);
  }

  /**
   * @brief Euclidean division (single-limb exact; multi-limb truncates to
   *        first limb). Division by zero yields zero by convention (total).
   */
  constexpr friend ExtensionalCardinal operator/(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    if (rhs.limbs[0] == 0) return ExtensionalCardinal{};
    ExtensionalCardinal result{};
    result.limbs[0] = lhs.limbs[0] / rhs.limbs[0];
    return result;
  }

  /** @brief Euclidean modulo (single-limb exact). Division by zero yields
   *         lhs by convention (total). */
  constexpr friend ExtensionalCardinal operator%(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    if (rhs.limbs[0] == 0) return lhs;
    ExtensionalCardinal result{};
    result.limbs[0] = lhs.limbs[0] % rhs.limbs[0];
    return result;
  }

  constexpr ExtensionalCardinal operator-() const noexcept {
    ExtensionalCardinal result{};
    for (std::size_t index = 0; index < N; ++index) {
      result.limbs[index] = ~limbs[index];
    }
    return result + ExtensionalCardinal{1};
  }

  constexpr std::size_t realize_to_size_t(
      std::size_t sentinel =
          std::numeric_limits<std::size_t>::max()) const noexcept {
    for (std::size_t index = 1; index < N; ++index) {
      if (limbs[index] != 0) {
        return sentinel;
      }
    }
    return limbs[0];
  }

  static constexpr CheckedResult checked_add(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    ExtensionalCardinal result{};
    unsigned __int128 carry = 0;

    for (std::size_t index = 0; index < N; ++index) {
      const unsigned __int128 sum =
          static_cast<unsigned __int128>(lhs.limbs[index]) +
          static_cast<unsigned __int128>(rhs.limbs[index]) + carry;
      result.limbs[index] = static_cast<limb_type>(sum);
      carry = sum >> limb_bits;
    }

    return CheckedResult{result, carry != 0};
  }

  static constexpr CheckedResult checked_mul(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    ExtensionalCardinal result{};
    bool overflowed = false;

    for (std::size_t left_index = 0; left_index < N; ++left_index) {
      for (std::size_t right_index = 0; right_index < N; ++right_index) {
        const std::size_t target_index = left_index + right_index;
        const unsigned __int128 product =
            static_cast<unsigned __int128>(lhs.limbs[left_index]) *
            static_cast<unsigned __int128>(rhs.limbs[right_index]);

        if (target_index >= N) {
          overflowed = overflowed || (product != 0);
          continue;
        }

        overflowed =
            accumulate_word(result.limbs, target_index, product) || overflowed;
      }
    }

    return CheckedResult{result, overflowed};
  }

 private:
  static constexpr bool accumulate_word(std::array<limb_type, N>& target,
                                        std::size_t index,
                                        unsigned __int128 word) noexcept {
    while (word != 0 && index < N) {
      const unsigned __int128 sum =
          static_cast<unsigned __int128>(target[index]) +
          static_cast<limb_type>(word);
      target[index] = static_cast<limb_type>(sum);
      word = (word >> limb_bits) + (sum >> limb_bits);
      ++index;
    }
    return word != 0;
  }
};

// ---------------------------------------------------------------------------
// Overflow-aware arithmetic returning ℵ_0 on overflow
// ---------------------------------------------------------------------------

export template <std::size_t N>
constexpr std::variant<ExtensionalCardinal<N>, ℵ_0> add_or_ℵ_0(
    const ExtensionalCardinal<N>& lhs, const ExtensionalCardinal<N>& rhs) {
  const auto checked = ExtensionalCardinal<N>::checked_add(lhs, rhs);
  if (checked.overflowed) {
    return std::variant<ExtensionalCardinal<N>, ℵ_0>{ℵ_0{}};
  }
  return std::variant<ExtensionalCardinal<N>, ℵ_0>{checked.value};
}

export template <std::size_t N>
constexpr std::variant<ExtensionalCardinal<N>, ℵ_0> mul_or_ℵ_0(
    const ExtensionalCardinal<N>& lhs, const ExtensionalCardinal<N>& rhs) {
  const auto checked = ExtensionalCardinal<N>::checked_mul(lhs, rhs);
  if (checked.overflowed) {
    return std::variant<ExtensionalCardinal<N>, ℵ_0>{ℵ_0{}};
  }
  return std::variant<ExtensionalCardinal<N>, ℵ_0>{checked.value};
}

/** @brief Draft sum type for finite vs countably infinite cardinalities. */
export using Cardinality = std::variant<ExtensionalCardinal<>, ℵ_0>;

/** @brief Convenience constructor for finite cardinal values. */
export constexpr Cardinality finite_cardinality(std::size_t n) {
  return Cardinality{ExtensionalCardinal<>{n}};
}

/**
 * @brief Product witness for Lipschitz-boundary crossings.
 * @details Structured as `std::pair` to stay aligned with `IsProduct`.
 */
export using LipschitzBoundaryWitness = std::pair<Cardinality, Cardinality>;

/** @brief Cardinal addition under the finite/ℵ_0 policy. */
export constexpr Cardinality add(const Cardinality& lhs,
                                 const Cardinality& rhs) {
  if (std::holds_alternative<ℵ_0>(lhs) || std::holds_alternative<ℵ_0>(rhs)) {
    return Cardinality{ℵ_0{}};
  }
  return add_or_ℵ_0(std::get<ExtensionalCardinal<>>(lhs),
                    std::get<ExtensionalCardinal<>>(rhs));
}

/**
 * @brief Cardinal multiplication under the finite/ℵ_0 policy.
 * @details `x * ℵ_0 = ℵ_0` for any x.
 */
export constexpr Cardinality mul(const Cardinality& lhs,
                                 const Cardinality& rhs) {
  if (std::holds_alternative<ℵ_0>(lhs) || std::holds_alternative<ℵ_0>(rhs)) {
    return Cardinality{ℵ_0{}};
  }
  return mul_or_ℵ_0(std::get<ExtensionalCardinal<>>(lhs),
                    std::get<ExtensionalCardinal<>>(rhs));
}

/**
 * @brief Total comparison over the finite/ℵ_0 fragment.
 * @details finite < ℵ_0, and ℵ_0 == ℵ_0.
 */
export constexpr std::strong_ordering compare(const Cardinality& lhs,
                                              const Cardinality& rhs) {
  const bool lhs_inf = std::holds_alternative<ℵ_0>(lhs);
  const bool rhs_inf = std::holds_alternative<ℵ_0>(rhs);

  if (lhs_inf && rhs_inf) return std::strong_ordering::equal;
  if (lhs_inf) return std::strong_ordering::greater;
  if (rhs_inf) return std::strong_ordering::less;

  return std::get<ExtensionalCardinal<>>(lhs) <=>
         std::get<ExtensionalCardinal<>>(rhs);
}

/**
 * @brief Realize a cardinality value to `std::size_t` with explicit boundary.
 * @param transfinite_sentinel Value used for non-finite cardinalities.
 */
export constexpr std::size_t realize_to_size_t(
    const Cardinality& value, std::size_t transfinite_sentinel =
                                  std::numeric_limits<std::size_t>::max()) {
  if (std::holds_alternative<ExtensionalCardinal<>>(value)) {
    return std::get<ExtensionalCardinal<>>(value).realize_to_size_t(
        transfinite_sentinel);
  }
  return transfinite_sentinel;
}

/** @brief Draft writer-like cardinality effect carrier. */
export struct CardinalityEffect {
  Cardinality value;
  Cardinality trace;
};

/** @brief Unit for cardinality effect composition. */
export constexpr CardinalityEffect unit_effect(Cardinality value) {
  return CardinalityEffect{value, finite_cardinality(0)};
}

/** @brief Sequential composition of cardinality effects. */
export template <typename F>
constexpr CardinalityEffect bind_effect(const CardinalityEffect& effect,
                                        F&& f) {
  const CardinalityEffect next = f(effect.value);
  return CardinalityEffect{next.value, add(effect.trace, next.trace)};
}

/** @brief Additive inverse of ExtensionalCardinal<> (two's-complement). */
export constexpr ExtensionalCardinal<> inverse(
    ExtensionalCardinal<> value, std::plus<ExtensionalCardinal<>>) {
  return -value;
}

}  // namespace dedekind::sets

// ---------------------------------------------------------------------------
// Category trait registrations for ExtensionalCardinal<1>
// ---------------------------------------------------------------------------

namespace dedekind::category {

using C1 = dedekind::sets::ExtensionalCardinal<>;

template <>
struct identity_trait<C1, std::plus<C1>> {
  using value_type = C1;
  static constexpr value_type value = value_type{0};
};

template <>
struct identity_trait<C1, std::multiplies<C1>> {
  using value_type = C1;
  static constexpr value_type value = value_type{1};
};

template <>
inline constexpr bool is_associative_v<C1, std::plus<C1>> = true;

template <>
inline constexpr bool is_associative_v<C1, std::multiplies<C1>> = true;

template <>
inline constexpr bool is_commutative_v<C1, std::plus<C1>> = true;

template <>
inline constexpr bool is_commutative_v<C1, std::multiplies<C1>> = true;

template <>
inline constexpr bool
    is_distributive_v<C1, std::multiplies<C1>, std::plus<C1>> = true;

template <>
struct is_periodic<C1, std::plus<C1>> : std::true_type {};

template <>
struct is_periodic<C1, std::multiplies<C1>> : std::true_type {};

template <std::size_t N>
inline constexpr bool
    is_reflexive_v<dedekind::sets::ExtensionalCardinal<N>, std::less_equal<>> =
        true;

template <std::size_t N>
inline constexpr bool
    is_transitive_v<dedekind::sets::ExtensionalCardinal<N>, std::less_equal<>> =
        true;

template <std::size_t N>
inline constexpr bool is_antisymmetric_v<dedekind::sets::ExtensionalCardinal<N>,
                                         std::less_equal<>> = true;

static_assert(IsRing<C1, std::plus<C1>, std::multiplies<C1>>,
              "ExtensionalCardinal<1> must certify as a total ring.");

static_assert(
    IsProduct<dedekind::sets::LipschitzBoundaryWitness,
              dedekind::sets::Cardinality, dedekind::sets::Cardinality>,
    "Cardinality boundary witness must realize a categorical product.");

}  // namespace dedekind::category
