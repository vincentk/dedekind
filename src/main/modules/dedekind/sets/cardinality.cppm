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
   *  The explicit @c static_cast keeps the sign-conversion at this boundary
   *  rather than at each call site.
   *
   *  Intentionally non-explicit (post-#402) so integral values can convert
   *  implicitly when initialising or passing a @c Cardinality — the
   *  load-bearing path for the @c ℕ-as-Cardinality carrier reading.  The
   *  @c std::variant alternative @c ExtensionalCardinal<> is selected by
   *  the variant's per-alternative-ctor lookup regardless of @c explicit;
   *  what the @c explicit drop changes is whether implicit conversions are
   *  permitted (e.g., copy-initialisation, argument passing, return
   *  conversion) from @c std::integral source values. */
  template <std::integral S>
    requires(!std::same_as<S, limb_type>)
  constexpr ExtensionalCardinal(S value) noexcept {
    // NOLINTNEXTLINE(google-explicit-constructor)
    // Intentionally implicit so @c std::integral values convert to
    // @c Cardinality (= @c std::variant<ExtensionalCardinal<>, ℵ_0>)
    // without the call site spelling @c ExtensionalCardinal<>{...}.
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
   * @brief Euclidean division.  Division by zero yields zero by
   *        convention (total).  When the divisor fits in a single
   *        limb, performs schoolbook long division across the full
   *        @c N-limb dividend (multi-limb-correct).  When the divisor
   *        itself is multi-limb, falls back to single-limb truncation
   *        --- multi-limb-by-multi-limb division is out of scope.
   */
  constexpr friend ExtensionalCardinal operator/(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    if (rhs.limbs[0] == 0) return ExtensionalCardinal{};
    // Detect a multi-limb divisor; fall back to truncated single-limb
    // division if the rhs has any non-zero high limbs.
    bool divisor_multi_limb = false;
    for (std::size_t i = 1; i < N; ++i) {
      if (rhs.limbs[i] != 0) {
        divisor_multi_limb = true;
        break;
      }
    }
    if (divisor_multi_limb) {
      ExtensionalCardinal result{};
      result.limbs[0] = lhs.limbs[0] / rhs.limbs[0];
      return result;
    }
    // Single-limb divisor: schoolbook long division from the high limb
    // down.  Use unsigned __int128 to carry the running remainder
    // shifted into the next limb without overflowing intermediate
    // arithmetic.
    ExtensionalCardinal result{};
    unsigned __int128 remainder = 0;
    const unsigned __int128 divisor =
        static_cast<unsigned __int128>(rhs.limbs[0]);
    for (std::size_t i = N; i-- > 0;) {
      const unsigned __int128 cur =
          (remainder << limb_bits) |
          static_cast<unsigned __int128>(lhs.limbs[i]);
      result.limbs[i] = static_cast<limb_type>(cur / divisor);
      remainder = cur % divisor;
    }
    return result;
  }

  /** @brief Euclidean modulo.  Division by zero yields @c lhs by
   *         convention (total).  Single-limb divisor: full multi-limb
   *         remainder via the same long-division pass as @c operator/;
   *         the result is therefore representable in a single limb (it
   *         is bounded above by the divisor) and lives in @c limbs[0].
   *         Multi-limb divisor: falls back to single-limb truncation. */
  constexpr friend ExtensionalCardinal operator%(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    if (rhs.limbs[0] == 0) return lhs;
    bool divisor_multi_limb = false;
    for (std::size_t i = 1; i < N; ++i) {
      if (rhs.limbs[i] != 0) {
        divisor_multi_limb = true;
        break;
      }
    }
    if (divisor_multi_limb) {
      ExtensionalCardinal result{};
      result.limbs[0] = lhs.limbs[0] % rhs.limbs[0];
      return result;
    }
    unsigned __int128 remainder = 0;
    const unsigned __int128 divisor =
        static_cast<unsigned __int128>(rhs.limbs[0]);
    for (std::size_t i = N; i-- > 0;) {
      const unsigned __int128 cur =
          (remainder << limb_bits) |
          static_cast<unsigned __int128>(lhs.limbs[i]);
      remainder = cur % divisor;
    }
    ExtensionalCardinal result{};
    result.limbs[0] = static_cast<limb_type>(remainder);
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

/** @brief Sum type for finite vs countably infinite cardinalities; the
 *         library's bona-fide proxy for @f$\mathbb{N}@f$ modulo
 *         physical limits.
 *
 *  @details The variant @c (ExtensionalCardinal<>, ℵ_0) is @b not a
 *  cyclic carrier (it does not wrap modulo capacity); on overflow it
 *  @b escalates --- saturates --- into @c ℵ_0 via the @c add /
 *  @c mul helpers below.  The explicit overflow story is "you reached
 *  countable infinity" rather than the C++ default of silent wrap or
 *  UB.  The finite fragment is @c ExtensionalCardinal<N>, a
 *  @b fixed-precision @c N-limb unsigned carrier; the default
 *  @c ExtensionalCardinal<> uses a single @c std::size_t limb (so
 *  the variant is @b not arbitrary-precision bigint --- choosing a
 *  larger @c N at the type level widens the finite range but every
 *  instantiation is still fixed-precision).  The @c ℵ_0 alternative
 *  marks the saturation regime that takes over once that fixed
 *  capacity cannot hold the result.
 *
 *  Strictly mathematically, @f$\mathbb{N} \cup \{\aleph_0\}@f$ is
 *  not a monoid in the textbook sense (e.g.\ left-cancellation
 *  fails on the saturating element).  The library's pragmatic
 *  certificate accepts it as a proxy for @f$\mathbb{N}@f$ because
 *  the finite fragment is @f$\mathbb{N}@f$ exactly and the
 *  saturation is the @b explicit overflow contract: "if you compute
 *  beyond memory limits, you saturate at @c ℵ_0".  Downstream code
 *  that requires strict-ℕ behaviour should test the finite-fragment
 *  alternative (@c std::holds_alternative<ExtensionalCardinal<>>).
 *  See the sibling @c SignedCardinality below for the ℤ proxy on
 *  the same pattern.
 */
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

// ---------------------------------------------------------------------------
// Cardinality homogeneous operator surface (closes #424; prerequisite for
// #416 / #402 retarget).  Mirrors the SignedCardinality pattern (PR #396)
// --- @c std::plus / @c std::multiplies / @c std::three_way_comparable
// can now find operators on @c Cardinality directly, lifting the @c IsRig
// and order witnesses out of "free functions only" land.
//
// No subtraction: ℕ is closed under @c + but not under @c - (would
// produce ℤ).  The math-wins reading is that @c Cardinality is a
// @b commutative @b semiring (rig), not a ring; downstream code that
// needs additive inverses uses @c SignedCardinality.
//
// @c / and @c % @b are included: Euclidean division on ℕ is closed
// (@c 7 @c / @c 3 @c = @c 2 with remainder @c 1, both naturals --- the
// quotient/remainder pair is the textbook construction).  This is what
// lifts @c IsDividableChain<Cardinality> downstream in @c :completeness,
// matching the existing claim on @c unsigned int.
//
// @c noexcept @b justification: the operators below dispatch through
// @c std::get<>, which throws @c std::bad_variant_access if the variant
// is @c valueless_by_exception.  @c std::variant becomes valueless only
// if an alternative's move/copy ctor throws during reassignment; both
// of @c Cardinality's alternatives are nothrow-move-constructible
// (@c ExtensionalCardinal<> wraps a @c std::array of integral limbs;
// @c ℵ_0 is empty), so @c Cardinality is provably never valueless and
// the @c noexcept on these operators is sound.  The static_asserts
// below make the guarantee explicit at compile time.
// ---------------------------------------------------------------------------

static_assert(std::is_nothrow_move_constructible_v<ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must be nothrow-move-constructible "
              "so Cardinality is provably never valueless_by_exception "
              "(and the noexcept on the homogeneous operators below is "
              "sound, even though they dispatch through std::get<>).");
static_assert(std::is_nothrow_move_constructible_v<ℵ_0>,
              "ℵ_0 must be nothrow-move-constructible (it's empty, so "
              "this is trivially true; pinned for the same valueless-"
              "by-exception guarantee on Cardinality).");

/** @brief Explicit @c == on @c Cardinality.  @c std::variant supplies a
 *         defaulted @c operator== inside the module's purview, but it
 *         is @b not reachable across the @c dedekind.sets module
 *         boundary (the @c <variant> header lives in the global module
 *         fragment, so its operator templates are not exported).
 *         Defining the operator explicitly in @c dedekind::sets ---
 *         where ADL can find it from importers like @c
 *         dedekind.numbers --- closes that gap so @c
 *         IsPartiallyOrdered<Cardinality> / @c
 *         std::three_way_comparable<Cardinality> fire downstream. */
export constexpr bool operator==(const Cardinality& lhs,
                                 const Cardinality& rhs) noexcept {
  const bool lhs_inf = std::holds_alternative<ℵ_0>(lhs);
  const bool rhs_inf = std::holds_alternative<ℵ_0>(rhs);
  if (lhs_inf != rhs_inf) return false;
  if (lhs_inf) return true;
  return std::get<ExtensionalCardinal<>>(lhs) ==
         std::get<ExtensionalCardinal<>>(rhs);
}

/** @brief @c + on @c Cardinality wraps the existing @c add() policy
 *         (saturating to @c ℵ_0 on overflow). */
export constexpr Cardinality operator+(const Cardinality& lhs,
                                       const Cardinality& rhs) noexcept {
  return add(lhs, rhs);
}

/** @brief @c * on @c Cardinality wraps the existing @c mul() policy. */
export constexpr Cardinality operator*(const Cardinality& lhs,
                                       const Cardinality& rhs) noexcept {
  return mul(lhs, rhs);
}

/** @brief Euclidean division on @c Cardinality.  Convention follows
 *         the @c ExtensionalCardinal<> totalisation: division by zero
 *         yields zero; @c finite @c / @c ℵ_0 yields zero (the limit);
 *         @c ℵ_0 @c / @c finite-non-zero yields @c ℵ_0; @c ℵ_0 @c /
 *         @c ℵ_0 yields @c ℵ_0 (saturation rather than indeterminate ---
 *         @c Cardinality has no @c NaZ-equivalent sentinel; the ℕ proxy
 *         keeps the policy total). */
export constexpr Cardinality operator/(const Cardinality& lhs,
                                       const Cardinality& rhs) noexcept {
  const bool lhs_inf = std::holds_alternative<ℵ_0>(lhs);
  const bool rhs_inf = std::holds_alternative<ℵ_0>(rhs);
  if (lhs_inf && rhs_inf) return Cardinality{ℵ_0{}};
  if (lhs_inf) {
    // ℵ_0 / 0 collapses to 0 by ExtensionalCardinal<>'s convention; ℵ_0 /
    // non-zero stays ℵ_0.
    if (std::get<ExtensionalCardinal<>>(rhs) == ExtensionalCardinal<>{}) {
      return finite_cardinality(0);
    }
    return Cardinality{ℵ_0{}};
  }
  if (rhs_inf) return finite_cardinality(0);  // finite / ℵ_0 → 0
  return Cardinality{std::get<ExtensionalCardinal<>>(lhs) /
                     std::get<ExtensionalCardinal<>>(rhs)};
}

/** @brief Euclidean remainder on @c Cardinality.  Mirrors the @c
 *         operator/ convention: @c finite @c % @c finite delegates to
 *         the underlying @c ExtensionalCardinal<>'s @c %; @c finite @c
 *         % @c ℵ_0 returns the @c lhs (the limit-style "remainder is
 *         the original" reading); operations with @c ℵ_0 on the @c lhs
 *         saturate to @c ℵ_0. */
export constexpr Cardinality operator%(const Cardinality& lhs,
                                       const Cardinality& rhs) noexcept {
  const bool lhs_inf = std::holds_alternative<ℵ_0>(lhs);
  const bool rhs_inf = std::holds_alternative<ℵ_0>(rhs);
  if (lhs_inf) return Cardinality{ℵ_0{}};
  if (rhs_inf) return lhs;
  return Cardinality{std::get<ExtensionalCardinal<>>(lhs) %
                     std::get<ExtensionalCardinal<>>(rhs)};
}

/** @brief Spaceship on @c Cardinality wraps @c compare() (finite values
 *         compare via the underlying @c ExtensionalCardinal<> spaceship;
 *         @c ℵ_0 dominates every finite value).  Returns
 *         @c std::strong_ordering --- there is no NaN-equivalent in the
 *         ℕ proxy. */
export constexpr std::strong_ordering operator<=>(
    const Cardinality& lhs, const Cardinality& rhs) noexcept {
  return compare(lhs, rhs);
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

/**
 * @brief Signed fixed-precision N-limb integer with a sign-magnitude layout.
 *
 * @details Two structural fields — @ref negative (the sign bit) and @ref
 * magnitude (an N-limb unsigned natural delegated to ExtensionalCardinal<N>).
 * Canonical zero has `negative == false` and `magnitude == 0`; the
 * arithmetic operators preserve that canonical form, so `+0` and `-0` compare
 * equal and never appear in results of operators on this type.
 *
 * Satisfies IsInteger (see registrations after the numbers:integer module
 * pulls this file), making it the intended signed backing carrier for
 * `Rational<Z>` when fixed-precision rationals with negative coefficients
 * are required — overflow happens at 2^{N*64-1} and not before.  The
 * `SignedCardinality` variant downstream wraps this carrier with ±ℵ_0
 * sentinels so callers that need an unbounded ℤ proxy escalate
 * (saturate) rather than wrap; this carrier on its own is the bounded
 * (cyclic, mod 2^{N*64}) ℤ proxy.
 *
 * Structural on purpose: the two public fields make the type usable as a
 * non-type template parameter wherever `Rational<Z>` is.
 */
export template <std::size_t N = 1>
struct SignedExtensionalCardinal {
  static_assert(N > 0, "SignedExtensionalCardinal<N> requires N >= 1.");

  using magnitude_type = ExtensionalCardinal<N>;

  bool negative;
  magnitude_type magnitude;

  constexpr SignedExtensionalCardinal() noexcept
      : negative(false), magnitude() {}

  /** @brief Construction from any integral source, sign-correctly. */
  template <std::integral S>
  constexpr SignedExtensionalCardinal(S value) noexcept  // NOLINT
      : negative(false), magnitude() {
    if constexpr (std::signed_integral<S>) {
      if (value < 0) {
        negative = true;
        using U = std::make_unsigned_t<S>;
        // `0u - static_cast<U>(value)` in unsigned arithmetic yields the
        // magnitude without overflow even at the minimum value of S.
        const U magnitude_bits = static_cast<U>(0) - static_cast<U>(value);
        magnitude = magnitude_type{
            static_cast<typename magnitude_type::limb_type>(magnitude_bits)};
        return;
      }
    }
    magnitude =
        magnitude_type{static_cast<typename magnitude_type::limb_type>(value)};
  }

  constexpr friend bool operator==(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    // Zero is canonical: ignore sign when magnitude is zero.
    const bool lhs_zero = lhs.magnitude == magnitude_type{};
    const bool rhs_zero = rhs.magnitude == magnitude_type{};
    if (lhs_zero && rhs_zero) return true;
    return lhs.negative == rhs.negative && lhs.magnitude == rhs.magnitude;
  }

  constexpr friend std::strong_ordering operator<=>(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    const bool lhs_zero = lhs.magnitude == magnitude_type{};
    const bool rhs_zero = rhs.magnitude == magnitude_type{};
    if (lhs_zero && rhs_zero) return std::strong_ordering::equal;
    if (lhs.negative && !rhs.negative) return std::strong_ordering::less;
    if (!lhs.negative && rhs.negative) return std::strong_ordering::greater;
    // Same sign (non-zero on at least one side).
    if (lhs.negative) {
      // Both negative: larger magnitude is the smaller value.
      return rhs.magnitude <=> lhs.magnitude;
    }
    return lhs.magnitude <=> rhs.magnitude;
  }

  constexpr SignedExtensionalCardinal operator-() const noexcept {
    // Canonicalise zero: if the magnitude is zero we always return +0,
    // regardless of the input sign. The public-field layout means
    // callers can construct non-canonical {negative = true, magnitude = 0}
    // values directly; this operator normalises them on the way out.
    if (magnitude == magnitude_type{}) {
      return SignedExtensionalCardinal{};  // canonical +0
    }
    SignedExtensionalCardinal result{*this};
    result.negative = !negative;
    return result;
  }

  constexpr friend SignedExtensionalCardinal operator+(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    SignedExtensionalCardinal result;
    if (lhs.negative == rhs.negative) {
      // Same sign: magnitudes add, sign preserved.
      result.negative = lhs.negative;
      result.magnitude = lhs.magnitude + rhs.magnitude;
    } else if (lhs.magnitude >= rhs.magnitude) {
      // Opposite signs, |lhs| >= |rhs|: result takes sign of lhs.
      result.magnitude = lhs.magnitude - rhs.magnitude;
      result.negative = lhs.negative;
    } else {
      // Opposite signs, |lhs| <  |rhs|: result takes sign of rhs.
      result.magnitude = rhs.magnitude - lhs.magnitude;
      result.negative = rhs.negative;
    }
    if (result.magnitude == magnitude_type{}) result.negative = false;
    return result;
  }

  constexpr friend SignedExtensionalCardinal operator-(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    return lhs + (-rhs);
  }

  constexpr friend SignedExtensionalCardinal operator*(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    SignedExtensionalCardinal result;
    result.magnitude = lhs.magnitude * rhs.magnitude;
    result.negative = (lhs.negative != rhs.negative) &&
                      (result.magnitude != magnitude_type{});
    return result;
  }

  /** @brief Euclidean division; result truncates toward zero (C++ semantics).
   */
  constexpr friend SignedExtensionalCardinal operator/(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    SignedExtensionalCardinal result;
    result.magnitude = lhs.magnitude / rhs.magnitude;
    result.negative = (lhs.negative != rhs.negative) &&
                      (result.magnitude != magnitude_type{});
    return result;
  }

  /** @brief Euclidean remainder; result takes the sign of the dividend
   *         (C++ semantics: `a == (a / b) * b + a % b`). */
  constexpr friend SignedExtensionalCardinal operator%(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    SignedExtensionalCardinal result;
    result.magnitude = lhs.magnitude % rhs.magnitude;
    result.negative = lhs.negative && (result.magnitude != magnitude_type{});
    return result;
  }

  /** @brief Explicit conversion to a signed integral type (single-limb only).
   *  Constrained to N == 1 to prevent silent truncation of multi-limb values.
   *  Used by the IR-fixture showcases to extract a compile-time-known
   *  rational's numerator as a concrete machine integer for the emitted
   *  `ret i64 ...` witness. */
  template <std::signed_integral S>
    requires(N == 1)
  constexpr explicit operator S() const noexcept {
    // Negate in unsigned space: `-signed_min` is UB in the target type
    // when magnitude equals |signed_min|.  Casting the unsigned magnitude
    // back to the signed type after modular negation yields the correct
    // two's-complement value (including the signed-min edge case).
    using U = std::make_unsigned_t<S>;
    const U u_mag = static_cast<U>(magnitude.limbs[0]);
    const U u_signed =
        negative ? static_cast<U>(static_cast<U>(0) - u_mag) : u_mag;
    return static_cast<S>(u_signed);
  }
};

// ---------------------------------------------------------------------------
// SignedCardinality: extended-integer carrier with ±ℵ_0 escalation (#377)
// ---------------------------------------------------------------------------
//
// `SignedExtensionalCardinal<N>` shipped in #373 is a *finite* signed
// carrier --- ℤ/2^{64 N}ℤ under sign-magnitude arithmetic.  Useful for
// showcase arithmetic, but not ℤ.  `SignedCardinality` is the signed
// counterpart of `Cardinality` (the ℕ ∪ {ℵ_0} variant): an extended-
// integer carrier whose addition / subtraction / multiplication
// escalate to ±ℵ_0 (or `NaZ` for indeterminate forms) rather than wrap.
//
// Design choices:
//   - Mixed-infinity arithmetic (e.g.\ +ℵ_0 + (−ℵ_0)) returns the
//     `NaZ` sentinel rather than throwing, mirroring IEEE NaN
//     propagation: NaZ + anything = NaZ, NaZ * anything = NaZ.  Keeps
//     the type usable in constexpr / NTTP-like contexts that cannot
//     observe exceptions.
//   - The variant alternative ordering (finite first) makes
//     `SignedCardinality{}` the canonical zero, satisfying the
//     `IsReflectiveSpecies`/`IsInteger` `T{}` requirement.
//   - The total `+` / `*` are saturating, not periodic --- the
//     `is_saturating_v` species trait (introduced under this issue)
//     is the load-bearing certificate that lifts `IsTotal` and the
//     downstream `IsAbelianGroup<SignedCardinality, std::plus<...>>`.

/** @struct PositiveInfinity: the @f$+\aleph_0@f$ sentinel for
 * SignedCardinality. */
export struct PositiveInfinity {
  constexpr friend bool operator==(const PositiveInfinity&,
                                   const PositiveInfinity&) = default;
};

/** @struct NegativeInfinity: the @f$-\aleph_0@f$ sentinel. */
export struct NegativeInfinity {
  constexpr friend bool operator==(const NegativeInfinity&,
                                   const NegativeInfinity&) = default;
};

/** @struct NaZ: Not-a-ℤ-element.  Result of indeterminate forms
 *  (+ℵ_0 + (−ℵ_0), 0 * ±ℵ_0, ±ℵ_0 / ±ℵ_0, etc.).  Propagates through
 *  arithmetic in the IEEE-NaN style.
 */
export struct NaZ {
  constexpr friend bool operator==(const NaZ&, const NaZ&) = default;
};

/** @brief Signed counterpart of @c Cardinality with ±ℵ_0 escalation;
 *         the library's bona-fide proxy for @f$\mathbb{Z}@f$ modulo
 *         physical limits.
 *
 *  @details Variant of @c (SignedExtensionalCardinal<>, +ℵ_0, −ℵ_0, NaZ).
 *  The finite alternative is the default-constructed first slot, so
 *  @c SignedCardinality{} is canonical zero.
 *
 *  Same picture as @c Cardinality (the ℕ proxy), with a sign added:
 *    - @b Not @b cyclic --- arithmetic does not wrap modulo capacity.
 *    - On signed overflow, @b escalates (saturates) into @c ±ℵ_0
 *      according to the sign of the operands.
 *    - Indeterminate forms (@c +ℵ_0 + (−ℵ_0), @c 0 * ±ℵ_0,
 *      @c ±ℵ_0 / ±ℵ_0, @c x / 0) propagate the @c NaZ sentinel
 *      IEEE-NaN-style rather than throwing, keeping the carrier
 *      usable in @c constexpr / NTTP-shaped contexts that cannot
 *      observe exceptions.
 *    - The finite fragment @c SignedExtensionalCardinal<N> is
 *      @b fixed-precision signed-magnitude with @c N limbs; the
 *      shorthand @c SignedExtensionalCardinal<> uses the default
 *      single-limb instantiation (so the variant is @b not
 *      arbitrary-precision bigint --- choosing a larger @c N widens
 *      the finite range but every instantiation is still
 *      fixed-precision).  The explicit overflow story is escalation
 *      to @f$\pm \aleph_0@f$, not bigint growth: signed-overflow UB
 *      and silent wrap are replaced by the saturation behaviour.
 *
 *  Strictly mathematically, @f$\mathbb{Z} \cup \{\pm \aleph_0,
 *  \mathit{NaZ}\}@f$ is not a group in the textbook sense
 *  (@c +ℵ_0 + (−ℵ_0) = NaZ breaks the inverse law for the
 *  saturating elements).  The library's pragmatic certificate
 *  accepts it as a proxy for @f$\mathbb{Z}@f$ because the finite
 *  fragment is @f$\mathbb{Z}@f$ exactly and the saturation /
 *  NaZ behaviour is the @b explicit overflow contract: "if you
 *  compute beyond memory limits, you saturate at @c ±ℵ_0; if you
 *  compute an indeterminate form, you get @c NaZ".  Downstream code
 *  that requires strict-ℤ behaviour should test the finite-fragment
 *  alternative (@c std::holds_alternative<SignedExtensionalCardinal<>>).
 *
 *  Same shape as how @c unsigned int's @c IsRing claim is "modulo
 *  wrap" rather than literal @f$\mathbb{Z}@f$ --- the project's
 *  stance is that the truthful name for the carrier includes the
 *  overflow / saturation behaviour, and the trait registry pins
 *  what the carrier actually does at the limits.
 */
export using SignedCardinality =
    std::variant<SignedExtensionalCardinal<>, PositiveInfinity,
                 NegativeInfinity, NaZ>;

// noexcept justification for SignedCardinality's operators (parallel
// to the Cardinality block above; pinned per Copilot review on PR #425
// to make the std::get-can't-throw guarantee compile-time explicit).
// All four variant alternatives are nothrow-move-constructible
// (SignedExtensionalCardinal<> wraps an integral magnitude + bool;
// PositiveInfinity / NegativeInfinity / NaZ are empty sentinel
// structs), so SignedCardinality is provably never valueless and the
// noexcept on its operators (in use since PR #396) is sound.
static_assert(std::is_nothrow_move_constructible_v<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> must be nothrow-move-"
              "constructible so SignedCardinality is provably never "
              "valueless_by_exception.");
static_assert(std::is_nothrow_move_constructible_v<PositiveInfinity> &&
                  std::is_nothrow_move_constructible_v<NegativeInfinity> &&
                  std::is_nothrow_move_constructible_v<NaZ>,
              "All SignedCardinality sentinel alternatives (±ℵ_0, NaZ) "
              "must be nothrow-move-constructible (empty structs --- "
              "trivially true; pinned for the noexcept guarantee).");

namespace detail {
constexpr bool sc_is_finite(const SignedCardinality& v) noexcept {
  return std::holds_alternative<SignedExtensionalCardinal<>>(v);
}
constexpr bool sc_is_pos_inf(const SignedCardinality& v) noexcept {
  return std::holds_alternative<PositiveInfinity>(v);
}
constexpr bool sc_is_neg_inf(const SignedCardinality& v) noexcept {
  return std::holds_alternative<NegativeInfinity>(v);
}
constexpr bool sc_is_naz(const SignedCardinality& v) noexcept {
  return std::holds_alternative<NaZ>(v);
}
constexpr bool sc_is_zero(const SignedCardinality& v) noexcept {
  return sc_is_finite(v) && std::get<SignedExtensionalCardinal<>>(v) ==
                                SignedExtensionalCardinal<>{};
}
constexpr int sc_sign(const SignedCardinality& v) noexcept {
  // Returns +1 for positive (incl. +ℵ_0), -1 for negative (incl. -ℵ_0),
  // 0 for zero or NaZ.  Used by escalation logic.
  if (sc_is_naz(v)) return 0;
  if (sc_is_pos_inf(v)) return +1;
  if (sc_is_neg_inf(v)) return -1;
  const auto& z = std::get<SignedExtensionalCardinal<>>(v);
  if (z == SignedExtensionalCardinal<>{}) return 0;
  return z.negative ? -1 : +1;
}
}  // namespace detail

/** @brief Convenience constructor for finite signed values. */
export template <std::integral S>
constexpr SignedCardinality finite_signed_cardinality(S value) noexcept {
  return SignedCardinality{SignedExtensionalCardinal<>{value}};
}

/** @brief Unary negation: swaps signs and ±ℵ_0; preserves NaZ. */
export constexpr SignedCardinality operator-(
    const SignedCardinality& v) noexcept {
  if (detail::sc_is_naz(v)) return SignedCardinality{NaZ{}};
  if (detail::sc_is_pos_inf(v)) return SignedCardinality{NegativeInfinity{}};
  if (detail::sc_is_neg_inf(v)) return SignedCardinality{PositiveInfinity{}};
  return SignedCardinality{-std::get<SignedExtensionalCardinal<>>(v)};
}

/** @brief Addition with ±ℵ_0 escalation on overflow. */
export constexpr SignedCardinality operator+(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  // Mixed infinities: indeterminate.
  if (sc_is_pos_inf(lhs) && sc_is_neg_inf(rhs)) return SignedCardinality{NaZ{}};
  if (sc_is_neg_inf(lhs) && sc_is_pos_inf(rhs)) return SignedCardinality{NaZ{}};
  // Same-sign infinities: absorb.
  if (sc_is_pos_inf(lhs) || sc_is_pos_inf(rhs))
    return SignedCardinality{PositiveInfinity{}};
  if (sc_is_neg_inf(lhs) || sc_is_neg_inf(rhs))
    return SignedCardinality{NegativeInfinity{}};
  // Both finite: sign-magnitude with overflow detection on same-sign
  // adds (opposite-sign reduces magnitude, never overflows).
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  if (a.negative == b.negative) {
    const auto checked =
        SignedExtensionalCardinal<>::magnitude_type::checked_add(a.magnitude,
                                                                 b.magnitude);
    if (checked.overflowed) {
      return a.negative ? SignedCardinality{NegativeInfinity{}}
                        : SignedCardinality{PositiveInfinity{}};
    }
    SignedExtensionalCardinal<> result;
    result.negative = a.negative;
    result.magnitude = checked.value;
    if (result.magnitude == SignedExtensionalCardinal<>::magnitude_type{}) {
      result.negative = false;  // canonicalise zero
    }
    return SignedCardinality{result};
  }
  return SignedCardinality{a + b};
}

/** @brief Subtraction: @c a - b @c == a + (-b). */
export constexpr SignedCardinality operator-(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  return lhs + (-rhs);
}

/** @brief Multiplication with ±ℵ_0 escalation on overflow.  @c 0 * ±ℵ_0
 *         is indeterminate (returns @c NaZ).
 */
export constexpr SignedCardinality operator*(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  const bool lhs_inf = sc_is_pos_inf(lhs) || sc_is_neg_inf(lhs);
  const bool rhs_inf = sc_is_pos_inf(rhs) || sc_is_neg_inf(rhs);
  if (lhs_inf || rhs_inf) {
    if (sc_is_zero(lhs) || sc_is_zero(rhs))
      return SignedCardinality{NaZ{}};  // 0 * ±ℵ_0
    const int s = sc_sign(lhs) * sc_sign(rhs);
    return s > 0 ? SignedCardinality{PositiveInfinity{}}
                 : SignedCardinality{NegativeInfinity{}};
  }
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  const auto checked = SignedExtensionalCardinal<>::magnitude_type::checked_mul(
      a.magnitude, b.magnitude);
  if (checked.overflowed) {
    const bool result_negative = (a.negative != b.negative);
    return result_negative ? SignedCardinality{NegativeInfinity{}}
                           : SignedCardinality{PositiveInfinity{}};
  }
  SignedExtensionalCardinal<> result;
  result.magnitude = checked.value;
  result.negative =
      (a.negative != b.negative) &&
      (result.magnitude != SignedExtensionalCardinal<>::magnitude_type{});
  return SignedCardinality{result};
}

/** @brief Truncating division.  Division by zero and @c ±ℵ_0/±ℵ_0
 *         return @c NaZ; @c finite/±ℵ_0 returns @c 0 (the limit);
 *         @c ±ℵ_0 divided by a non-zero finite value saturates to
 *         signed infinity according to the operand signs.
 */
export constexpr SignedCardinality operator/(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  if (sc_is_zero(rhs)) return SignedCardinality{NaZ{}};  // x / 0
  const bool lhs_inf = sc_is_pos_inf(lhs) || sc_is_neg_inf(lhs);
  const bool rhs_inf = sc_is_pos_inf(rhs) || sc_is_neg_inf(rhs);
  if (lhs_inf && rhs_inf) return SignedCardinality{NaZ{}};
  if (lhs_inf) {
    const int s = sc_sign(lhs) * sc_sign(rhs);
    return s > 0 ? SignedCardinality{PositiveInfinity{}}
                 : SignedCardinality{NegativeInfinity{}};
  }
  if (rhs_inf) return finite_signed_cardinality(0);  // finite / ±ℵ_0 → 0
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  return SignedCardinality{a / b};
}

/** @brief Truncating remainder.  Mirrors C++ semantics on the finite
 *         alternative; non-finite operands give @c NaZ.
 */
export constexpr SignedCardinality operator%(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  if (!sc_is_finite(lhs) || !sc_is_finite(rhs)) return SignedCardinality{NaZ{}};
  if (sc_is_zero(rhs)) return SignedCardinality{NaZ{}};
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  return SignedCardinality{a % b};
}

/** @brief Partial ordering on the extended integers: −ℵ_0 < finite <
 *         +ℵ_0; @c NaZ is @b unordered with respect to everything
 *         (including itself), mirroring IEEE-NaN's unordered
 *         semantics.  Returning @c std::partial_ordering rather than
 *         @c std::strong_ordering is the principled choice: @c NaZ
 *         compares @c unordered, which keeps ordering and equality
 *         (variant's defaulted @c ==, which finds @c NaZ == @c NaZ
 *         true) consistent without forcing a fake @c equal answer.
 */
export constexpr std::partial_ordering compare_signed(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return std::partial_ordering::unordered;
  // -ℵ_0 < everything except itself.
  if (sc_is_neg_inf(lhs) && sc_is_neg_inf(rhs))
    return std::partial_ordering::equivalent;
  if (sc_is_neg_inf(lhs)) return std::partial_ordering::less;
  if (sc_is_neg_inf(rhs)) return std::partial_ordering::greater;
  // +ℵ_0 > everything except itself.
  if (sc_is_pos_inf(lhs) && sc_is_pos_inf(rhs))
    return std::partial_ordering::equivalent;
  if (sc_is_pos_inf(lhs)) return std::partial_ordering::greater;
  if (sc_is_pos_inf(rhs)) return std::partial_ordering::less;
  return std::get<SignedExtensionalCardinal<>>(lhs) <=>
         std::get<SignedExtensionalCardinal<>>(rhs);
}

/** @brief @c < derived from @c compare_signed; required by @c IsInteger.
 *         For @c NaZ operands the comparison is @c unordered so this
 *         returns @c false, mirroring IEEE-NaN's @c < behaviour.
 */
export constexpr bool operator<(const SignedCardinality& lhs,
                                const SignedCardinality& rhs) noexcept {
  return compare_signed(lhs, rhs) == std::partial_ordering::less;
}

// ---------------------------------------------------------------------------
// Heterogeneous comparison: variant ↔ std::integral (closes #415)
// ---------------------------------------------------------------------------
//
// Cross-type relational operators between the variant ℕ-/ℤ-proxy carriers
// (@c Cardinality, @c SignedCardinality) and built-in @c std::integral
// values.  The load-bearing path is the halfspace machinery's
// @c (x @c > @c Pivot) substitution where @c x is a variant ℤ-proxy and
// @c Pivot is the @c int NTTP from @c bound<-21>: previously this
// required an explicit lift of the literal to the variant alternative.
//
// Defining @c operator<=> together with @c operator== lets the C++20
// rewrite rules synthesise the four partial-order operators (@c <,
// @c <=, @c >, @c >=) and the symmetric (rhs-first) direction
// uniformly --- the witness section in @c numbers/cardinality.cppm
// pins both directions via @c HasPartialOrderOperatorsWith.

/** @brief @c Cardinality @c <=> @c std::integral.  ℵ_0 is greater than
 *         any finite integer; negative signed values are below the
 *         entire ℕ proxy (no element of @c Cardinality is negative),
 *         which is encoded as @c lhs @c > @c rhs.  Otherwise the
 *         comparison reduces to the @c ExtensionalCardinal<> spaceship.
 *         Delegates the integral lift through @c ExtensionalCardinal<>'s
 *         existing @c std::integral ctor, which subsumes @c bool /
 *         @c size_t / signed-non-negative paths uniformly --- avoiding
 *         @c std::make_unsigned_t (undefined for @c bool).
 */
export template <std::integral T>
constexpr std::strong_ordering operator<=>(const Cardinality& lhs,
                                           T rhs) noexcept {
  if (std::holds_alternative<ℵ_0>(lhs)) return std::strong_ordering::greater;
  if constexpr (std::signed_integral<T>) {
    if (rhs < 0) return std::strong_ordering::greater;
  }
  return std::get<ExtensionalCardinal<>>(lhs) <=> ExtensionalCardinal<>{rhs};
}

/** @brief @c Cardinality @c == @c std::integral.  Negative signed values
 *         and ℵ_0 never equal a finite integer; the finite alternative
 *         delegates to @c ExtensionalCardinal<>'s defaulted @c ==. */
export template <std::integral T>
constexpr bool operator==(const Cardinality& lhs, T rhs) noexcept {
  if (std::holds_alternative<ℵ_0>(lhs)) return false;
  if constexpr (std::signed_integral<T>) {
    if (rhs < 0) return false;
  }
  return std::get<ExtensionalCardinal<>>(lhs) == ExtensionalCardinal<>{rhs};
}

/** @brief @c SignedCardinality @c <=> @c std::integral.  Lifts @c rhs
 *         to a finite @c SignedCardinality and routes through
 *         @c compare_signed; @c NaZ on @c lhs propagates as
 *         @c std::partial_ordering::unordered, mirroring IEEE-NaN's
 *         unordered semantics. */
export template <std::integral T>
constexpr std::partial_ordering operator<=>(const SignedCardinality& lhs,
                                            T rhs) noexcept {
  return compare_signed(lhs, finite_signed_cardinality(rhs));
}

/** @brief @c SignedCardinality @c == @c std::integral.  @c NaZ and
 *         @c ±ℵ_0 never equal a finite integer; the finite
 *         alternative delegates to @c SignedExtensionalCardinal<>'s
 *         canonical-zero-aware equality. */
export template <std::integral T>
constexpr bool operator==(const SignedCardinality& lhs, T rhs) noexcept {
  if (!detail::sc_is_finite(lhs)) return false;
  return std::get<SignedExtensionalCardinal<>>(lhs) ==
         SignedExtensionalCardinal<>{rhs};
}

// ---------------------------------------------------------------------------
// Heterogeneous comparison: variant ↔ std::floating_point (closes #428)
// ---------------------------------------------------------------------------
//
// Cross-type relational operators between the variant ℕ-/ℤ-proxy
// carriers and built-in @c std::floating_point values.  Mirrors the
// @c std::integral overloads above; needed for showcase callsites like
// @c (var<ℤ> @c > @c bound<-21.0>) where @c bound<V>'s NTTP type is
// @c double and the carrier is the variant.  Returns @c
// std::partial_ordering throughout — a NaN @c rhs (or @c NaZ on @c
// lhs) propagates as @c unordered, IEEE-NaN-style.
//
// Float extraction for the variant carriers:
//   * @c ExtensionalCardinal<>'s @c explicit @c operator @c F() (in
//     this same file) covers the @c Cardinality finite path.
//   * @c SignedExtensionalCardinal<> doesn't carry an @c operator
//     @c F(); we lift via @c (negative @c ? @c -1 @c : @c 1) @c *
//     @c static_cast<F>(magnitude) on the single-limb instantiation.

namespace detail {
/** @brief Convert the SignedCardinality finite fragment to a
 *         floating-point value (single-limb only by virtue of the
 *         underlying @c ExtensionalCardinal<>'s @c operator @c F()
 *         constraint). */
template <std::floating_point F>
constexpr F sc_finite_to_floating(const SignedExtensionalCardinal<>& z) noexcept {
  const F mag = static_cast<F>(z.magnitude);
  return z.negative ? -mag : mag;
}
}  // namespace detail

/** @brief @c Cardinality @c <=> @c std::floating_point.  @c ℵ_0 is
 *         greater than every finite double (and equivalent to
 *         @c +inf); negative finite floats and @c -inf land strictly
 *         below the ℕ proxy.  A @c NaN @c rhs returns
 *         @c std::partial_ordering::unordered. */
export template <std::floating_point F>
constexpr std::partial_ordering operator<=>(const Cardinality& lhs,
                                            F rhs) noexcept {
  // NaN propagates as unordered.
  if (rhs != rhs) return std::partial_ordering::unordered;
  if (std::holds_alternative<ℵ_0>(lhs)) {
    // +inf is the only finite-double equivalent of ℵ_0.
    if (rhs == std::numeric_limits<F>::infinity())
      return std::partial_ordering::equivalent;
    return std::partial_ordering::greater;
  }
  if (rhs == -std::numeric_limits<F>::infinity())
    return std::partial_ordering::greater;
  if (rhs == std::numeric_limits<F>::infinity())
    return std::partial_ordering::less;
  if (rhs < F{0}) return std::partial_ordering::greater;  // ℕ ≥ 0 > rhs
  const F lhs_value = static_cast<F>(std::get<ExtensionalCardinal<>>(lhs));
  if (lhs_value < rhs) return std::partial_ordering::less;
  if (lhs_value > rhs) return std::partial_ordering::greater;
  return std::partial_ordering::equivalent;
}

/** @brief @c Cardinality @c == @c std::floating_point.  Equality
 *         requires an exact finite-vs-finite match, or @c ℵ_0 vs
 *         @c +inf.  @c NaN never equals anything. */
export template <std::floating_point F>
constexpr bool operator==(const Cardinality& lhs, F rhs) noexcept {
  if (rhs != rhs) return false;  // NaN
  if (std::holds_alternative<ℵ_0>(lhs))
    return rhs == std::numeric_limits<F>::infinity();
  if (rhs < F{0}) return false;
  return static_cast<F>(std::get<ExtensionalCardinal<>>(lhs)) == rhs;
}

/** @brief @c SignedCardinality @c <=> @c std::floating_point.
 *         @c NaZ propagates as @c unordered (mirrors compare_signed's
 *         NaZ semantics); @c ±ℵ_0 maps to @c ±inf-equivalent;
 *         @c NaN @c rhs returns @c unordered.  Finite-vs-finite uses
 *         the magnitude lift @c sc_finite_to_floating. */
export template <std::floating_point F>
constexpr std::partial_ordering operator<=>(const SignedCardinality& lhs,
                                            F rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs)) return std::partial_ordering::unordered;
  if (rhs != rhs) return std::partial_ordering::unordered;  // NaN
  if (sc_is_pos_inf(lhs)) {
    return rhs == std::numeric_limits<F>::infinity()
               ? std::partial_ordering::equivalent
               : std::partial_ordering::greater;
  }
  if (sc_is_neg_inf(lhs)) {
    return rhs == -std::numeric_limits<F>::infinity()
               ? std::partial_ordering::equivalent
               : std::partial_ordering::less;
  }
  if (rhs == std::numeric_limits<F>::infinity())
    return std::partial_ordering::less;
  if (rhs == -std::numeric_limits<F>::infinity())
    return std::partial_ordering::greater;
  const F lhs_value =
      sc_finite_to_floating<F>(std::get<SignedExtensionalCardinal<>>(lhs));
  if (lhs_value < rhs) return std::partial_ordering::less;
  if (lhs_value > rhs) return std::partial_ordering::greater;
  return std::partial_ordering::equivalent;
}

/** @brief @c SignedCardinality @c == @c std::floating_point.  @c NaZ /
 *         @c NaN never equal anything; @c ±ℵ_0 equals only @c ±inf;
 *         finite delegates through the magnitude lift. */
export template <std::floating_point F>
constexpr bool operator==(const SignedCardinality& lhs, F rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs)) return false;
  if (rhs != rhs) return false;
  if (sc_is_pos_inf(lhs)) return rhs == std::numeric_limits<F>::infinity();
  if (sc_is_neg_inf(lhs)) return rhs == -std::numeric_limits<F>::infinity();
  return sc_finite_to_floating<F>(
             std::get<SignedExtensionalCardinal<>>(lhs)) == rhs;
}

// ---------------------------------------------------------------------------
// Heterogeneous comparison: Cardinality ↔ SignedCardinality (closes #428,
// extended scope from the user's "ℕ ⊂ ℤ should be comparable" pushback)
// ---------------------------------------------------------------------------
//
// The canonical embedding @c ℕ ↪ @c ℤ is structural: every natural is an
// integer.  At the variant level @c Cardinality (ℕ-proxy) embeds into
// @c SignedCardinality (ℤ-proxy) via @c embed_ℕ_ℤ (in @c numbers/integer.cppm).
// That makes @c Cardinality @c < @c SignedCardinality a well-defined
// question; pre-#428 it didn't compile, forcing call sites to explicitly
// lift through the embedding before comparing.
//
// The implementation lifts @c Cardinality on the fly to its
// @c SignedCardinality image and routes through @c compare_signed:
//   * @c finite Cardinality @c c → positive @c SignedExtensionalCardinal<>
//     with @c magnitude @c = @c c, @c negative @c = @c false.
//   * @c ℵ_0 → @c PositiveInfinity (= @c +ℵ_0).
// Returns @c std::partial_ordering throughout — a @c NaZ on the
// @c SignedCardinality side propagates as @c unordered.

namespace detail {
/** @brief Lift @c Cardinality into @c SignedCardinality via the canonical
 *         ℕ ↪ ℤ embedding.  Mirrors @c embed_ℕ_ℤ in @c numbers/integer.cppm
 *         but lives here so the cross-variant operators below can reach
 *         it without crossing the @c sets → @c numbers module boundary. */
constexpr SignedCardinality lift_cardinality_to_signed(
    const Cardinality& c) noexcept {
  if (std::holds_alternative<ℵ_0>(c)) {
    return SignedCardinality{PositiveInfinity{}};
  }
  SignedExtensionalCardinal<> result;
  result.magnitude = std::get<ExtensionalCardinal<>>(c);
  result.negative = false;
  return SignedCardinality{result};
}
}  // namespace detail

/** @brief @c Cardinality @c <=> @c SignedCardinality.  Routes through
 *         the canonical ℕ ↪ ℤ lift and @c compare_signed; @c NaZ on
 *         the @c rhs propagates as @c unordered. */
export constexpr std::partial_ordering operator<=>(
    const Cardinality& lhs, const SignedCardinality& rhs) noexcept {
  return compare_signed(detail::lift_cardinality_to_signed(lhs), rhs);
}

/** @brief @c Cardinality @c == @c SignedCardinality.  @c NaZ never
 *         equals anything; otherwise the canonical lift settles the
 *         comparison. */
export constexpr bool operator==(const Cardinality& lhs,
                                 const SignedCardinality& rhs) noexcept {
  if (detail::sc_is_naz(rhs)) return false;
  // The lift produces a non-negative SignedCardinality, so any negative
  // rhs is never equal.  Otherwise compare_signed returns equivalent.
  return compare_signed(detail::lift_cardinality_to_signed(lhs), rhs) ==
         std::partial_ordering::equivalent;
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

// ---------------------------------------------------------------------------
// Category trait registrations for SignedExtensionalCardinal<1>
// ---------------------------------------------------------------------------
//
// Mirrors the ExtensionalCardinal<1> registrations above, with the crucial
// addition of `is_invertible_v` under + (every integer has an additive
// inverse — this is what lifts ℤ from a monoid to a group, and is the
// semantic difference between `Monoid_ℕ` and `Group_ℤ`).

using Z1 = dedekind::sets::SignedExtensionalCardinal<>;

template <>
struct identity_trait<Z1, std::plus<Z1>> {
  using value_type = Z1;
  static constexpr value_type value = value_type{0};
};

template <>
struct identity_trait<Z1, std::multiplies<Z1>> {
  using value_type = Z1;
  static constexpr value_type value = value_type{1};
};

template <>
inline constexpr bool is_associative_v<Z1, std::plus<Z1>> = true;

template <>
inline constexpr bool is_associative_v<Z1, std::multiplies<Z1>> = true;

template <>
inline constexpr bool is_commutative_v<Z1, std::plus<Z1>> = true;

template <>
inline constexpr bool is_commutative_v<Z1, std::multiplies<Z1>> = true;

template <>
inline constexpr bool
    is_distributive_v<Z1, std::multiplies<Z1>, std::plus<Z1>> = true;

// Periodicity: the operation is total on the finite carrier, wrapping at
// the N-limb capacity boundary. Required for IsTotal / IsMagma dispatch.
template <>
struct is_periodic<Z1, std::plus<Z1>> : std::true_type {};

template <>
struct is_periodic<Z1, std::multiplies<Z1>> : std::true_type {};

// The defining trait of ℤ over ℕ: every element has an additive inverse
// (unary minus is total on SignedExtensionalCardinal, canonical zero is
// preserved under negation).
template <>
inline constexpr bool is_invertible_v<Z1, std::plus<Z1>> = true;

template <>
struct inverse_trait<Z1, std::plus<Z1>> {
  static constexpr bool exists = true;
  using value_type = Z1;
  static constexpr value_type compute(const Z1& value) noexcept {
    return -value;
  }
};

template <std::size_t N>
inline constexpr bool is_reflexive_v<
    dedekind::sets::SignedExtensionalCardinal<N>, std::less_equal<>> = true;

template <std::size_t N>
inline constexpr bool is_transitive_v<
    dedekind::sets::SignedExtensionalCardinal<N>, std::less_equal<>> = true;

template <std::size_t N>
inline constexpr bool is_antisymmetric_v<
    dedekind::sets::SignedExtensionalCardinal<N>, std::less_equal<>> = true;

static_assert(IsRing<Z1, std::plus<Z1>, std::multiplies<Z1>>,
              "SignedExtensionalCardinal<1> must certify as a total ring.");

static_assert(IsAbelianGroup<Z1, std::plus<Z1>>,
              "SignedExtensionalCardinal<1> must certify as an abelian group "
              "under addition (the defining ℤ-versus-ℕ distinction).");

// ---------------------------------------------------------------------------
// IsCyclicGroup witnesses for the cardinal carriers (#378).
// ---------------------------------------------------------------------------
//
// `ExtensionalCardinal<N>` is the bounded ℕ carrier; under addition
// it wraps modulo its limb capacity, forming a finite cyclic group.
// `SignedExtensionalCardinal<N>` is its signed counterpart, also
// cyclic under addition (same capacity, signed representation).
// Order is exposed via `cyclic_order_v`; here we report 0
// ("order not representable as std::size_t") rather than the exact
// 2^(64 N)-class bound --- the concept-level claim is cyclicity,
// not the cardinality value.

template <std::size_t N>
struct is_cyclic_group<dedekind::sets::ExtensionalCardinal<N>,
                       std::plus<dedekind::sets::ExtensionalCardinal<N>>>
    : std::true_type {};

template <std::size_t N>
struct is_cyclic_group<dedekind::sets::SignedExtensionalCardinal<N>,
                       std::plus<dedekind::sets::SignedExtensionalCardinal<N>>>
    : std::true_type {};

static_assert(IsCyclicGroup<C1, std::plus<C1>>,
              "ExtensionalCardinal<1> under + must satisfy IsCyclicGroup "
              "(bounded ℕ, wraps modulo capacity).");

static_assert(IsCyclicGroup<Z1, std::plus<Z1>>,
              "SignedExtensionalCardinal<1> under + must satisfy IsCyclicGroup "
              "(bounded ℤ, wraps modulo capacity).");

/** @section CCC_Inheritance_389 CCC inheritance (#389)
 *
 * Both cardinal carriers can serve as the ambient species of an
 * ETCS-style set object; the canonical CCC over each carrier is
 * therefore Cartesian-closed and any @c IsSet built over these
 * ambients inherits the CCC guarantee structurally per #389.
 */
static_assert(HasCanonicalSetCCC<C1>,
              "ExtensionalCardinal<1> hosts a canonical Cartesian-closed "
              "Set ambient.");
static_assert(HasCanonicalSetCCC<Z1>,
              "SignedExtensionalCardinal<1> hosts a canonical "
              "Cartesian-closed Set ambient.");

// ---------------------------------------------------------------------------
// Category trait registrations for SignedCardinality (#377)
// ---------------------------------------------------------------------------
//
// The extended-integer carrier is a saturating abelian group under +
// (escalating to ±ℵ_0 rather than wrapping or being undefined).  The
// `is_saturating_v` trait is the load-bearing certificate that lifts
// IsTotal -> IsMagma -> IsMonoid -> IsGroup -> IsAbelianGroup, the
// chain `Group_ℤ` requires.  It is *not* periodic (unbounded), nor
// idempotent (a+a != a in general), nor cyclic (no finite generator).

using SC = dedekind::sets::SignedCardinality;

template <>
struct identity_trait<SC, std::plus<SC>> {
  using value_type = SC;
  static constexpr value_type value =
      SC{dedekind::sets::SignedExtensionalCardinal<>{0}};
};

template <>
struct identity_trait<SC, std::multiplies<SC>> {
  using value_type = SC;
  static constexpr value_type value =
      SC{dedekind::sets::SignedExtensionalCardinal<>{1}};
};

template <>
inline constexpr bool is_associative_v<SC, std::plus<SC>> = true;

template <>
inline constexpr bool is_associative_v<SC, std::multiplies<SC>> = true;

template <>
inline constexpr bool is_commutative_v<SC, std::plus<SC>> = true;

template <>
inline constexpr bool is_commutative_v<SC, std::multiplies<SC>> = true;

template <>
inline constexpr bool
    is_distributive_v<SC, std::multiplies<SC>, std::plus<SC>> = true;

// Saturation rather than periodicity: SignedCardinality is unbounded
// and escalates to ±ℵ_0 on overflow.  This is the load-bearing
// trait that satisfies the IsTotal pragmatic certificate.  See
// `is_saturating` in `category:species`.
template <>
struct is_saturating<SC, std::plus<SC>> : std::true_type {};

template <>
struct is_saturating<SC, std::multiplies<SC>> : std::true_type {};

// The defining ℤ trait exposes unary `-` as the operation used by
// the additive-inverse machinery.  -ℵ_0 maps to +ℵ_0 and vice versa;
// for finite values this is ordinary sign-flip; for NaZ, unary `-`
// propagates NaZ (mirroring how IEEE -NaN behaves), without
// asserting that NaZ satisfies an additive-inverse law.  The
// pragmatic certificate is "the finite fragment is ℤ exactly; the
// saturating elements satisfy the law mutually; NaZ propagates",
// which is enough to lift IsAbelianGroup on the carrier.
template <>
inline constexpr bool is_invertible_v<SC, std::plus<SC>> = true;

template <>
struct inverse_trait<SC, std::plus<SC>> {
  static constexpr bool exists = true;
  using value_type = SC;
  static constexpr value_type compute(const SC& value) noexcept {
    return -value;
  }
};

// Note: order-axiom trait specialisations (is_reflexive_v /
// is_transitive_v / is_antisymmetric_v) are deliberately *not*
// registered for SignedCardinality with std::less_equal<>: the
// carrier does not define operator<= (only operator< via the
// partial-ordering compare_signed), and antisymmetry would not be
// justified given NaZ's unordered comparison semantics.  Downstream
// code that needs the order traits should specialise them on a
// well-formed relation functor (e.g.\ one based on compare_signed)
// rather than on std::less_equal<>.

static_assert(IsAbelianGroup<SC, std::plus<SC>>,
              "SignedCardinality must certify as an abelian group under "
              "addition --- the canonical exact-ℤ extended-integer carrier "
              "with ±ℵ_0 escalation (closes #377's Group_ℤ acceptance).");

static_assert(IsRing<SC, std::plus<SC>, std::multiplies<SC>>,
              "SignedCardinality must certify as a total ring (the "
              "saturation certificate satisfies IsTotal on the "
              "multiplicative side too).");

static_assert(IsCommutativeRing<SC, std::plus<SC>, std::multiplies<SC>>,
              "SignedCardinality must certify as a commutative ring.");

// ---------------------------------------------------------------------------
// Category trait registrations for Cardinality (#424; prerequisite for
// the #402 retarget).  The unsigned variant ℕ-proxy now carries the same
// trait surface as its signed sibling SC --- modulo the missing additive
// inverse: Cardinality is a commutative @b semiring (rig), not a ring.
// ℕ has no negatives.
//
// Saturation (not periodicity) on both ops mirrors SignedCardinality:
// overflow escalates to ℵ_0 rather than wrapping.  is_saturating is the
// load-bearing IsTotal certificate (alongside is_periodic / is_idempotent).
//
// Order-axiom specialisations (is_reflexive_v / is_transitive_v /
// is_antisymmetric_v under std::less_equal<>): registered here because
// Cardinality's homogeneous operator<=> (added in #424 above) lifts the
// underlying ExtensionalCardinal<>'s strict total order onto the variant
// in the well-formed direction (finite ≤ ℵ_0; ℵ_0 ≤ ℵ_0).  Unlike
// SignedCardinality (which carries NaZ's unordered semantics and
// therefore can't justify std::less_equal<>-based axioms), Cardinality
// is a clean total order.
// ---------------------------------------------------------------------------

using Card = dedekind::sets::Cardinality;

template <>
struct identity_trait<Card, std::plus<Card>> {
  using value_type = Card;
  static constexpr value_type value = dedekind::sets::finite_cardinality(0);
};

template <>
struct identity_trait<Card, std::multiplies<Card>> {
  using value_type = Card;
  static constexpr value_type value = dedekind::sets::finite_cardinality(1);
};

template <>
inline constexpr bool is_associative_v<Card, std::plus<Card>> = true;

template <>
inline constexpr bool is_associative_v<Card, std::multiplies<Card>> = true;

template <>
inline constexpr bool is_commutative_v<Card, std::plus<Card>> = true;

template <>
inline constexpr bool is_commutative_v<Card, std::multiplies<Card>> = true;

template <>
inline constexpr bool
    is_distributive_v<Card, std::multiplies<Card>, std::plus<Card>> = true;

// Saturating (escalates to ℵ_0 on overflow), not periodic.  Lifts
// IsTotal -> IsMagma -> IsCommutativeMonoid -> IsRig (no additive
// inverse, so the chain stops here; SignedCardinality is what extends
// to IsRing / IsAbelianGroup).
template <>
struct is_saturating<Card, std::plus<Card>> : std::true_type {};

template <>
struct is_saturating<Card, std::multiplies<Card>> : std::true_type {};

// Order-axiom traits under std::less_equal<>: the variant's homogeneous
// <=> (compare()) is a strict total order on (finite ∪ {ℵ_0}).
template <>
inline constexpr bool is_reflexive_v<Card, std::less_equal<>> = true;

template <>
inline constexpr bool is_transitive_v<Card, std::less_equal<>> = true;

template <>
inline constexpr bool is_antisymmetric_v<Card, std::less_equal<>> = true;

static_assert(IsCommutativeMonoid<Card, std::plus<Card>>,
              "Cardinality must certify as a commutative monoid under "
              "addition (the ℕ-proxy with ℵ_0 escalation; saturating, "
              "not cyclic).");

static_assert(IsCommutativeMonoid<Card, std::multiplies<Card>>,
              "Cardinality must certify as a commutative monoid under "
              "multiplication.");

static_assert(IsRig<Card, std::plus<Card>, std::multiplies<Card>>,
              "Cardinality must certify as a rig (commutative semiring): "
              "+ and * are total, identities present, distributivity holds, "
              "and there are no additive inverses (ℕ has no negatives). "
              "The bona-fide ℕ proxy modulo physical limits.");

// ---------------------------------------------------------------------------
// SpeciesTraits specialisations: lift the variant ℕ-/ℤ-proxy carriers
// to first-class @c IsSpecies status (per #413; prerequisite for #402).
//
// Without these, @c std::variant<...> doesn't satisfy
// @c IsSpecies (the primary @c SpeciesTraits<T> registration covers
// @c std::integral / @c std::floating_point / @c bool only), so
// @c ambient_set<Cardinality>(...) and @c ambient_set<SignedCardinality>(...)
// can't anchor IsSet witnesses on the variant carriers — which blocks
// the @c ℕ @c = @c Cardinality / @c ℤ @c = @c SignedCardinality
// retarget under #402.
//
// Pattern follows @c SpeciesTraits<Rational<Z>> in @c numbers/rational.cppm:
// @c Domain @c = the carrier itself (variants are their own elements),
// @c machine_type @c = the carrier (no separate machine sibling — the
// variants ARE the project's canonical exact ℕ-/ℤ-proxy carriers).
// ---------------------------------------------------------------------------

template <>
struct SpeciesTraits<dedekind::sets::Cardinality> {
  using Domain = dedekind::sets::Cardinality;
  using machine_type = dedekind::sets::Cardinality;
};

template <>
struct SpeciesTraits<dedekind::sets::SignedCardinality> {
  using Domain = dedekind::sets::SignedCardinality;
  using machine_type = dedekind::sets::SignedCardinality;
};

// IsSpecies witnesses: the variant carriers are now first-class
// species and can flow into @c ambient_set<...> / ETCS object
// construction in downstream partitions (cf.\ @c sets:boundaries,
// where @c Ω<Cardinality> / @c Ω<SignedCardinality> instantiations
// will land as part of the #402 retarget PR).
static_assert(IsSpecies<dedekind::sets::Cardinality>,
              "Cardinality must be a recognised Species (post-#413).");
static_assert(IsSpecies<dedekind::sets::SignedCardinality>,
              "SignedCardinality must be a recognised Species (post-#413).");

}  // namespace dedekind::category
