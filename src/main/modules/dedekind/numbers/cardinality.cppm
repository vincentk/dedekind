e.g./**
     * @file dedekind/numbers/cardinality.cppm
     * @partition :cardinality
     * @brief Draft intensional cardinality carrier anchored in finite machine
     * size.
     *
     * @copyright 2026 The Dedekind Authors
     * Licensed under the Apache License, Version 2.0.
     *
     * @note "Rigor is not a prison, but the architecture of freedom."
     *       -- Emmy Noether, paraphrase
     */
    module;

#include <compare>
#include <cstddef>
#include <functional>
#include <limits>
#include <utility>
#include <variant>

export module dedekind.numbers:cardinality;

import dedekind.category;
import dedekind.sets;

namespace dedekind::numbers {

using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @brief Finite cardinality carrier anchored in std::size_t.
 * @details Arithmetic is intentionally machine-total (wrapping semantics),
 * preserving compatibility with `dedekind.category:total` ring checks.
 */
export struct FiniteCardinal {
  using cardinality_type = Finite;
  std::size_t value{};

  constexpr FiniteCardinal() = default;
  constexpr explicit FiniteCardinal(std::size_t v) : value(v) {}

  constexpr explicit operator std::size_t() const { return value; }

  constexpr friend bool operator==(FiniteCardinal, FiniteCardinal) = default;

  constexpr friend FiniteCardinal operator+(FiniteCardinal lhs,
                                            FiniteCardinal rhs) {
    return FiniteCardinal{lhs.value + rhs.value};
  }

  constexpr friend FiniteCardinal operator*(FiniteCardinal lhs,
                                            FiniteCardinal rhs) {
    return FiniteCardinal{lhs.value * rhs.value};
  }
};

/** @brief Canonical draft witness for countable infinity. */
export struct Aleph0 {
  using cardinality_type = ℵ_0;

  constexpr friend bool operator==(Aleph0, Aleph0) = default;
};

/** @brief Public singleton witness for countable infinity. */
export inline constexpr Aleph0 aleph0{};

/** @brief Draft sum type for finite vs countably infinite cardinalities. */
export using Cardinality = std::variant<FiniteCardinal, Aleph0>;

/** @brief Convenience constructor for finite cardinal values. */
export constexpr Cardinality finite_cardinality(std::size_t n) {
  return Cardinality{FiniteCardinal{n}};
}

/**
 * @brief Product witness for Lipschitz-boundary crossings.
 * @details Structured as std::pair to stay aligned with IsProduct in
 * `dedekind.category:cartesian`.
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

  return Cardinality{std::get<FiniteCardinal>(lhs) +
                     std::get<FiniteCardinal>(rhs)};
}

/**
 * @brief Cardinal multiplication under the draft finite/aleph0 policy.
 * @details This intentionally follows issue policy: x * aleph0 = aleph0.
 */
export constexpr Cardinality mul(const Cardinality& lhs,
                                 const Cardinality& rhs) {
  if (std::holds_alternative<Aleph0>(lhs) ||
      std::holds_alternative<Aleph0>(rhs)) {
    return Cardinality{aleph0};
  }

  return Cardinality{std::get<FiniteCardinal>(lhs) *
                     std::get<FiniteCardinal>(rhs)};
}

/**
 * @brief Total comparison over the draft finite/aleph0 fragment.
 * @details finite < aleph0 and aleph0 == aleph0.
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

  const auto a = std::get<FiniteCardinal>(lhs).value;
  const auto b = std::get<FiniteCardinal>(rhs).value;
  if (a < b) {
    return std::strong_ordering::less;
  }
  if (a > b) {
    return std::strong_ordering::greater;
  }
  return std::strong_ordering::equal;
}

/**
 * @brief Realize a cardinality value to std::size_t with explicit boundary.
 * @param transfinite_sentinel Value used for non-finite cardinalities.
 */
export constexpr std::size_t realize_to_size_t(
    const Cardinality& c, std::size_t transfinite_sentinel =
                              std::numeric_limits<std::size_t>::max()) {
  if (std::holds_alternative<FiniteCardinal>(c)) {
    return std::get<FiniteCardinal>(c).value;
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

}  // namespace dedekind::numbers

namespace dedekind::category {

template <>
struct identity_trait<dedekind::numbers::FiniteCardinal,
                      std::plus<dedekind::numbers::FiniteCardinal>> {
  using value_type = dedekind::numbers::FiniteCardinal;
  static constexpr value_type value = value_type{0};
};

template <>
struct identity_trait<dedekind::numbers::FiniteCardinal,
                      std::multiplies<dedekind::numbers::FiniteCardinal>> {
  using value_type = dedekind::numbers::FiniteCardinal;
  static constexpr value_type value = value_type{1};
};

template <>
inline constexpr bool
    is_associative_v<dedekind::numbers::FiniteCardinal,
                     std::plus<dedekind::numbers::FiniteCardinal>> = true;

template <>
inline constexpr bool
    is_associative_v<dedekind::numbers::FiniteCardinal,
                     std::multiplies<dedekind::numbers::FiniteCardinal>> = true;

template <>
inline constexpr bool
    is_commutative_v<dedekind::numbers::FiniteCardinal,
                     std::plus<dedekind::numbers::FiniteCardinal>> = true;

template <>
inline constexpr bool
    is_commutative_v<dedekind::numbers::FiniteCardinal,
                     std::multiplies<dedekind::numbers::FiniteCardinal>> = true;

template <>
inline constexpr bool
    is_distributive_v<dedekind::numbers::FiniteCardinal,
                      std::multiplies<dedekind::numbers::FiniteCardinal>,
                      std::plus<dedekind::numbers::FiniteCardinal>> = true;

template <>
struct is_periodic<dedekind::numbers::FiniteCardinal,
                   std::plus<dedekind::numbers::FiniteCardinal>>
    : std::true_type {};

template <>
struct is_periodic<dedekind::numbers::FiniteCardinal,
                   std::multiplies<dedekind::numbers::FiniteCardinal>>
    : std::true_type {};

constexpr dedekind::numbers::FiniteCardinal inverse(
    dedekind::numbers::FiniteCardinal a,
    std::plus<dedekind::numbers::FiniteCardinal>) {
  return dedekind::numbers::FiniteCardinal{static_cast<std::size_t>(0) -
                                           a.value};
}

static_assert(IsRing<dedekind::numbers::FiniteCardinal,
                     std::plus<dedekind::numbers::FiniteCardinal>,
                     std::multiplies<dedekind::numbers::FiniteCardinal>>,
              "FiniteCardinal must certify as a total ring.");

}  // namespace dedekind::category