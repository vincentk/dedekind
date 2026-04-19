/**
 * @file dedekind/numbers/cardinality.cppm
 * @partition :cardinality
 * @brief Extensional N-limb cardinality carrier with explicit overflow into
 *        `Aleph0`.
 *
 * @details
 * `ExtensionalCardinal<N>` is the machine-total, finite, extensional carrier
 * for cardinal quantities in this draft partition. Values are represented as
 * `N` little-endian `std::size_t` limbs with wrapping arithmetic, so the
 * carrier is compatible with the total-algebra checks in
 * `dedekind.category:total`.
 *
 * Checked arithmetic detects carry beyond the last limb and reifies that event
 * as the `Aleph0` sentinel at the variant layer. This keeps the intensional /
 * extensional boundary explicit: embed machine naturals into
 * `ExtensionalCardinal<N>`, compute internally, and realize back to
 * `std::size_t` only through an explicit boundary policy.
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
#include <cstddef>
#include <functional>
#include <limits>
#include <utility>
#include <variant>

export module dedekind.numbers:cardinality;

import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :naturals;

namespace dedekind::numbers {

using namespace dedekind::category;
using namespace dedekind::sets;

/** @brief Canonical draft witness for countable infinity. */
export struct Aleph0 {
  using cardinality_type = ℵ_0;

  constexpr friend bool operator==(Aleph0, Aleph0) = default;
};

/** @brief Public singleton witness for countable infinity. */
export inline constexpr Aleph0 aleph0{};

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

export template <std::size_t N>
constexpr std::variant<ExtensionalCardinal<N>, Aleph0> add_or_aleph0(
    const ExtensionalCardinal<N>& lhs, const ExtensionalCardinal<N>& rhs) {
  const auto checked = ExtensionalCardinal<N>::checked_add(lhs, rhs);
  if (checked.overflowed) {
    return std::variant<ExtensionalCardinal<N>, Aleph0>{aleph0};
  }
  return std::variant<ExtensionalCardinal<N>, Aleph0>{checked.value};
}

export template <std::size_t N>
constexpr std::variant<ExtensionalCardinal<N>, Aleph0> mul_or_aleph0(
    const ExtensionalCardinal<N>& lhs, const ExtensionalCardinal<N>& rhs) {
  const auto checked = ExtensionalCardinal<N>::checked_mul(lhs, rhs);
  if (checked.overflowed) {
    return std::variant<ExtensionalCardinal<N>, Aleph0>{aleph0};
  }
  return std::variant<ExtensionalCardinal<N>, Aleph0>{checked.value};
}

/** @brief Draft sum type for finite vs countably infinite cardinalities. */
export using Cardinality = std::variant<ExtensionalCardinal<>, Aleph0>;

/** @brief Convenience constructor for finite cardinal values. */
export constexpr Cardinality finite_cardinality(std::size_t n) {
  return Cardinality{ExtensionalCardinal<>{n}};
}

/**
 * @brief Product witness for Lipschitz-boundary crossings.
 * @details Structured as `std::pair` to stay aligned with `IsProduct`.
 */
export using LipschitzBoundaryWitness = std::pair<Cardinality, Cardinality>;

static_assert(
    IsProduct<LipschitzBoundaryWitness, Cardinality, Cardinality>,
    "Cardinality boundary witness must realize a categorical product.");

/** @brief Cardinal addition under the draft finite/aleph0 policy. */
export constexpr Cardinality add(const Cardinality& lhs,
                                 const Cardinality& rhs) {
  if (std::holds_alternative<Aleph0>(lhs) ||
      std::holds_alternative<Aleph0>(rhs)) {
    return Cardinality{aleph0};
  }

  return add_or_aleph0(std::get<ExtensionalCardinal<>>(lhs),
                       std::get<ExtensionalCardinal<>>(rhs));
}

/**
 * @brief Cardinal multiplication under the draft finite/aleph0 policy.
 * @details This intentionally follows the issue policy: `x * aleph0 = aleph0`.
 */
export constexpr Cardinality mul(const Cardinality& lhs,
                                 const Cardinality& rhs) {
  if (std::holds_alternative<Aleph0>(lhs) ||
      std::holds_alternative<Aleph0>(rhs)) {
    return Cardinality{aleph0};
  }

  return mul_or_aleph0(std::get<ExtensionalCardinal<>>(lhs),
                       std::get<ExtensionalCardinal<>>(rhs));
}

/**
 * @brief Total comparison over the draft finite/aleph0 fragment.
 * @details finite `<` aleph0 and aleph0 `==` aleph0.
 */
export constexpr std::strong_ordering compare(const Cardinality& lhs,
                                              const Cardinality& rhs) {
  const bool lhs_aleph0 = std::holds_alternative<Aleph0>(lhs);
  const bool rhs_aleph0 = std::holds_alternative<Aleph0>(rhs);

  if (lhs_aleph0 && rhs_aleph0) {
    return std::strong_ordering::equal;
  }
  if (lhs_aleph0) {
    return std::strong_ordering::greater;
  }
  if (rhs_aleph0) {
    return std::strong_ordering::less;
  }

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

constexpr ExtensionalCardinal<> inverse(ExtensionalCardinal<> value,
                                        std::plus<ExtensionalCardinal<>>) {
  return -value;
}

}  // namespace dedekind::numbers

namespace dedekind::category {

using C1 = dedekind::numbers::ExtensionalCardinal<>;

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
inline constexpr bool is_reflexive_v<dedekind::numbers::ExtensionalCardinal<N>,
                                     std::less_equal<>> = true;

template <std::size_t N>
inline constexpr bool is_transitive_v<dedekind::numbers::ExtensionalCardinal<N>,
                                      std::less_equal<>> = true;

template <std::size_t N>
inline constexpr bool is_antisymmetric_v<
    dedekind::numbers::ExtensionalCardinal<N>, std::less_equal<>> = true;

static_assert(IsRing<C1, std::plus<C1>, std::multiplies<C1>>,
              "ExtensionalCardinal<1> must certify as a total ring.");

static_assert(dedekind::numbers::IsNatural<C1>,
              "ExtensionalCardinal<1> must satisfy IsNatural.");

static_assert(dedekind::order::IsTotallyOrdered<C1>,
              "ExtensionalCardinal<1> must satisfy IsTotallyOrdered.");

}  // namespace dedekind::category
