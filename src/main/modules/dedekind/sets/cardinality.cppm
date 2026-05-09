/**
 * @file dedekind/sets/cardinality.cppm
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
#include <cmath>
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

export template <typename C>
// Terminology note: this concept validates cardinality metadata tags
// (`is_finite`, `is_countable`, `power_type`) rather than proving
// cardinal arithmetic laws. Name kept for compatibility during taxonomy pass.
concept IsCardinality = requires {
  { C::is_finite } -> std::convertible_to<bool>;
  { C::is_countable } -> std::convertible_to<bool>;
  typename C::power_type;
};

/** @concept IsCountable: Magnitude is at most Aleph_0. */
export template <typename C>
concept IsCountable = IsCardinality<C> && (C::is_countable == true);

/** @concept IsUncountable: Magnitude is strictly greater than Aleph_0. */
export template <typename C>
concept IsUncountable = IsCardinality<C> && !IsCountable<C>;

/** @concept IsFinite: Strictly terminating. */
export template <typename C>
concept IsFinite = IsCountable<C> && (C::is_finite == true);

/** @struct Finite: Hardware-bound magnitude. */
export struct Finite {
  static constexpr bool is_finite = true;
  static constexpr bool is_countable = true;

  auto operator<=>(const Finite&) const = default;

  using power_type = Finite;  // Finite sets always jump to other Finite sets.
};

static_assert(IsCardinality<Finite>);
static_assert(IsFinite<Finite>);

/** @brief Primary trait: Is a species defined by its members? */
export template <typename T>
struct is_extensional : std::false_type {};

// Atomic Proof: Integrals are always extensional.
template <std::integral T>
struct is_extensional<T> : std::true_type {};

/** @concept IsExtensional (The Proof) */
export template <typename S>
concept IsExtensional =
    std::is_same_v<decltype(std::declval<const S&>().size()), std::size_t> ||
    is_extensional<S>::value;

/**
 * @concept IsEnumerated
 * @brief A set whose members are materialized or bounded in memory (The
 * "Bucket").
 *
 * @details In the structuralist ontology, Extensionality implies that
 *          membership is not merely a rule (λx. P(x)) but is constrained
 *          by a physical container with a terminable address space.
 *
 * @tparam S A set species.
 * @tparam L The Subobject Classifier (Ω). Defaults to ClassicalLogic.
 */
export template <typename S, typename L = ClassicalLogic>
concept IsEnumerated = IsExtensional<S> && requires(const S s) {
  typename S::Domain;
  requires dedekind::category::IsSet<
      decltype(dedekind::category::ambient_set<typename S::Domain>(s))>;

  /** @section mereology__Magnitude: The Physical Proof */
  // An extensional set MUST claim a Finite cardinality type.
  requires(S::cardinality_type::is_finite == true);

  /** @section mereology__Termination: The Boundedness Proof */
  // Every extensional set must define a maximum capacity (upper_bound)
  // to ensure memory-safe allocations and finite iteration.
  { s.upper_bound() } -> std::convertible_to<std::size_t>;
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

  /** @brief Heterogeneous equality with built-in integrals.
   *
   *  @details Without this overload the comparison @c SEC<> @c == @c int
   *  is ambiguous: both @c operator==(SEC, SEC) (after the implicit
   *  @c int @c → @c SEC constructor) and @c operator==(SignedCardinality,
   *  int) (after the implicit @c SEC @c → @c SignedCardinality variant
   *  conversion) match in one user-defined conversion.  Providing the
   *  heterogeneous form directly on @c SEC makes it a strictly better
   *  match (no UDC required on the @c SEC side), resolving the ambiguity
   *  without forcing call-sites to write @c SEC<>{N} just to compare
   *  against an @c int literal.  Symmetric overload below covers
   *  @c int @c == @c SEC.  Required by the @c default_integer retarget
   *  to @c SEC<> (#499 / paper-side ℚ-as-IsField unblock).
   */
  template <std::integral T>
  constexpr friend bool operator==(const SignedExtensionalCardinal& lhs,
                                   T rhs) noexcept {
    return lhs == SignedExtensionalCardinal{rhs};
  }
  template <std::integral T>
  constexpr friend bool operator==(
      T lhs, const SignedExtensionalCardinal& rhs) noexcept {
    return SignedExtensionalCardinal{lhs} == rhs;
  }

  /** @brief Heterogeneous ordering with built-in integrals.
   *
   *  @details Mirrors the heterogeneous @c operator== above for the
   *  ordering side (@c <, @c <=, @c >, @c >=).  Without these
   *  overloads, comparisons like @c q.num() @c > @c 0 (where
   *  @c q.num() returns @c SEC<> after the @c default_integer
   *  retarget, and @c 0 is an @c int literal) trigger the same
   *  ambiguity the equality side resolved: @c operator<=>(SEC, SEC)
   *  via @c int @c → @c SEC versus @c operator<=>(SignedCardinality,
   *  int) via @c SEC @c → @c SignedCardinality.  Providing the
   *  heterogeneous form directly on @c SEC makes it a strictly better
   *  match (no UDC on the @c SEC side), letting callers write
   *  @c q.num() @c > @c 0 and similar without explicit
   *  @c default_integer{0} lifts.  Mirrors the heterogeneous
   *  @c operator== above for the ordering side.
   */
  template <std::integral T>
  constexpr friend std::strong_ordering operator<=>(
      const SignedExtensionalCardinal& lhs, T rhs) noexcept {
    return lhs <=> SignedExtensionalCardinal{rhs};
  }
  template <std::integral T>
  constexpr friend std::strong_ordering operator<=>(
      T lhs, const SignedExtensionalCardinal& rhs) noexcept {
    return SignedExtensionalCardinal{lhs} <=> rhs;
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
/** @brief Compare a non-negative integer @c lhs with a finite
 *         floating-point value @c rhs in @b integer @b domain — the
 *         comparison is precision-correct even when @c lhs exceeds @c
 *         2^digits<F> (where the cast @c LT @c → @c F is no longer
 *         exact and consecutive integers collapse to the same float).
 *
 *  Pre: @c rhs is finite, @b not NaN.  Sign of @c rhs is handled.
 *
 *  Strategy:
 *    * @c rhs negative @c → @c lhs (≥ 0) is strictly greater.
 *    * @c rhs out of @c [0, 2^digits<LT>) range @c → @c lhs ≤ LT_max
 *      < rhs, so @c lhs is strictly less.
 *    * @c rhs in range @c → split @c rhs into integer part
 *      @c trunc(rhs) and a fractional indicator; compare @c lhs to the
 *      integer part in @c LT domain (exact); if rhs has no fractional
 *      part, @c equivalent at integer-equality, else @c lhs sits below
 *      @c rhs at integer-equality (since @c rhs > floor in that case).
 */
template <std::floating_point F, std::unsigned_integral LT>
constexpr std::partial_ordering compare_unsigned_to_floating(LT lhs,
                                                             F rhs) noexcept {
  if (rhs < F{0}) return std::partial_ordering::greater;
  // 2^digits<LT> as an exact F (powers of two are representable until
  // they overflow the F exponent range; for LT = uint64_t and F =
  // double, this is 2^64 — exact).
  // 2^digits<LT> via constexpr arithmetic: the doublings are exact in
  // F since 2^k is representable until k overflows F's exponent range
  // (well above any plausible LT::digits).  std::ldexp would be cleaner
  // but isn't constexpr until C++26.
  constexpr F two_to_lt_bits = []() {
    F result = F{1};
    for (int i = 0; i < std::numeric_limits<LT>::digits; ++i) result *= F{2};
    return result;
  }();
  if (rhs >= two_to_lt_bits) return std::partial_ordering::less;
  const F rhs_trunc = std::trunc(rhs);
  const bool rhs_is_integer = (rhs == rhs_trunc);
  const LT rhs_floor = static_cast<LT>(rhs_trunc);
  if (lhs < rhs_floor) return std::partial_ordering::less;
  if (lhs > rhs_floor) return std::partial_ordering::greater;
  return rhs_is_integer ? std::partial_ordering::equivalent
                        : std::partial_ordering::less;
}

/** @brief Equality counterpart of @c compare_unsigned_to_floating:
 *         requires @c rhs to be finite, non-negative, integer-valued,
 *         in @c [0, 2^digits<LT>) range, and equal to @c lhs in @c LT
 *         domain.  Anything else is @c false. */
template <std::floating_point F, std::unsigned_integral LT>
constexpr bool eq_unsigned_to_floating(LT lhs, F rhs) noexcept {
  if (!std::isfinite(rhs)) return false;
  if (rhs < F{0}) return false;
  const F rhs_trunc = std::trunc(rhs);
  if (rhs != rhs_trunc) return false;
  // 2^digits<LT> via constexpr arithmetic: the doublings are exact in
  // F since 2^k is representable until k overflows F's exponent range
  // (well above any plausible LT::digits).  std::ldexp would be cleaner
  // but isn't constexpr until C++26.
  constexpr F two_to_lt_bits = []() {
    F result = F{1};
    for (int i = 0; i < std::numeric_limits<LT>::digits; ++i) result *= F{2};
    return result;
  }();
  if (rhs >= two_to_lt_bits) return false;
  return lhs == static_cast<LT>(rhs_trunc);
}
}  // namespace detail

/** @brief @c Cardinality @c <=> @c std::floating_point.  @c ℵ_0 is
 *         greater than every finite double (and equivalent to
 *         @c +inf); negative finite floats and @c -inf land strictly
 *         below the ℕ proxy.  A @c NaN @c rhs returns
 *         @c std::partial_ordering::unordered.  Finite-vs-finite is
 *         settled in @b integer @b domain via @c
 *         compare_unsigned_to_floating, which stays precision-correct
 *         even when the underlying limb exceeds @c 2^digits<F>. */
export template <std::floating_point F>
constexpr std::partial_ordering operator<=>(const Cardinality& lhs,
                                            F rhs) noexcept {
  if (rhs != rhs) return std::partial_ordering::unordered;
  if (std::holds_alternative<ℵ_0>(lhs)) {
    if (rhs == std::numeric_limits<F>::infinity())
      return std::partial_ordering::equivalent;
    return std::partial_ordering::greater;
  }
  if (rhs == -std::numeric_limits<F>::infinity())
    return std::partial_ordering::greater;
  if (rhs == std::numeric_limits<F>::infinity())
    return std::partial_ordering::less;
  using LT = ExtensionalCardinal<>::limb_type;
  return detail::compare_unsigned_to_floating<F, LT>(
      std::get<ExtensionalCardinal<>>(lhs).limbs[0], rhs);
}

/** @brief @c Cardinality @c == @c std::floating_point.  Equality
 *         requires either @c ℵ_0 vs @c +inf, or finite @c lhs equal to
 *         a finite, non-negative, integer-valued @c rhs in the carrier's
 *         representable range.  Float rounding cannot fake equality
 *         (@c eq_unsigned_to_floating compares in integer domain). */
export template <std::floating_point F>
constexpr bool operator==(const Cardinality& lhs, F rhs) noexcept {
  if (rhs != rhs) return false;
  if (std::holds_alternative<ℵ_0>(lhs))
    return rhs == std::numeric_limits<F>::infinity();
  using LT = ExtensionalCardinal<>::limb_type;
  return detail::eq_unsigned_to_floating<F, LT>(
      std::get<ExtensionalCardinal<>>(lhs).limbs[0], rhs);
}

/** @brief @c SignedCardinality @c <=> @c std::floating_point.
 *         @c NaZ propagates as @c unordered (mirrors compare_signed's
 *         NaZ semantics); @c ±ℵ_0 maps to @c ±inf-equivalent;
 *         @c NaN @c rhs returns @c unordered.  Finite-vs-finite is
 *         settled by sign-aware delegation to the unsigned integer-
 *         domain helper, so the comparison stays precision-correct
 *         even when the underlying magnitude exceeds @c 2^digits<F>. */
export template <std::floating_point F>
constexpr std::partial_ordering operator<=>(const SignedCardinality& lhs,
                                            F rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs)) return std::partial_ordering::unordered;
  if (rhs != rhs) return std::partial_ordering::unordered;
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
  // Both finite.  Sign first, then magnitude comparison via the unsigned
  // helper (with order reversed for negative-vs-negative).
  const auto& z = std::get<SignedExtensionalCardinal<>>(lhs);
  using LT = ExtensionalCardinal<>::limb_type;
  const LT magnitude_value = z.magnitude.limbs[0];
  const bool z_is_zero = (magnitude_value == 0);
  const bool z_is_neg = z.negative && !z_is_zero;
  const bool rhs_is_neg = rhs < F{0};
  if (z_is_neg && !rhs_is_neg) return std::partial_ordering::less;
  if (!z_is_neg && rhs_is_neg) return std::partial_ordering::greater;
  if (!z_is_neg) {
    return compare_unsigned_to_floating<F, LT>(magnitude_value, rhs);
  }
  // Both negative; compare |lhs| with |rhs|, then reverse the order.
  const auto magnitude_cmp =
      compare_unsigned_to_floating<F, LT>(magnitude_value, -rhs);
  if (magnitude_cmp == std::partial_ordering::less)
    return std::partial_ordering::greater;
  if (magnitude_cmp == std::partial_ordering::greater)
    return std::partial_ordering::less;
  return magnitude_cmp;  // equivalent stays equivalent
}

/** @brief @c SignedCardinality @c == @c std::floating_point.  @c NaZ /
 *         @c NaN never equal anything; @c ±ℵ_0 equals only @c ±inf;
 *         finite delegates to the integer-domain @c
 *         eq_unsigned_to_floating on the magnitude with sign
 *         reconciled, so float rounding cannot fake equality on
 *         large-magnitude integers. */
export template <std::floating_point F>
constexpr bool operator==(const SignedCardinality& lhs, F rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs)) return false;
  if (rhs != rhs) return false;
  if (sc_is_pos_inf(lhs)) return rhs == std::numeric_limits<F>::infinity();
  if (sc_is_neg_inf(lhs)) return rhs == -std::numeric_limits<F>::infinity();
  if (!std::isfinite(rhs)) return false;
  const auto& z = std::get<SignedExtensionalCardinal<>>(lhs);
  using LT = ExtensionalCardinal<>::limb_type;
  const LT magnitude_value = z.magnitude.limbs[0];
  const bool z_is_zero = (magnitude_value == 0);
  if (z_is_zero) return rhs == F{0};  // ±0 collapse
  const bool z_is_neg = z.negative;
  const bool rhs_is_neg = rhs < F{0};
  if (z_is_neg != rhs_is_neg) return false;
  return eq_unsigned_to_floating<F, LT>(magnitude_value,
                                        rhs_is_neg ? -rhs : rhs);
}

// ---------------------------------------------------------------------------
// Heterogeneous comparison: Cardinality ↔ SignedCardinality (closes #428,
// extended scope from the user's "ℕ ⊂ ℤ should be comparable" pushback)
// ---------------------------------------------------------------------------
//
// The canonical embedding @c ℕ ↪ @c ℤ is structural: every natural is an
// integer.  At the @b variant level the corresponding map is
// @c Cardinality (ℕ-proxy) → @c SignedCardinality (ℤ-proxy); the lift
// helper below realises it.  (The same direction at the machine /
// downstream-numbers layer is named @c embed_uint_sint_ in @c
// numbers/integer.cppm — pre-#402 that arrow is @c arrow<unsigned, int>;
// post-#402 it retargets to the variant pair, at which point the local
// helper here and @c embed_uint_sint_ become two names for the same lift.)
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

/** @brief Lift @c Cardinality into @c SignedCardinality via the canonical
 *         ℕ ↪ ℤ embedding direction.  Public, exported: this is the
 *         canonical variant-level @c ℕ @c ↪ @c ℤ embedding that the
 *         carrier-lattice diagram (Figure 1 in @c paper.tex) labels at
 *         the top row.  The machine-layer counterpart is @c embed_uint_sint_
 *         in @c numbers:integer (an @c arrow<unsigned, @c int> sign
 *         reinterpretation, structurally distinct from the variant-
 *         layer canonical embedding here).  Lives in @c sets:cardinality
 *         so the cross-variant operators below reach it without crossing
 *         the @c sets @c → @c numbers module boundary; an exported
 *         @c arrow form @c lift_ℕ_ℤ_ wrapping this function lives in
 *         @c numbers:integer for monic-arrow registration in the
 *         carrier lattice. */
export constexpr SignedCardinality lift_cardinality_to_signed(
    const Cardinality& c) noexcept {
  if (std::holds_alternative<ℵ_0>(c)) {
    return SignedCardinality{PositiveInfinity{}};
  }
  SignedExtensionalCardinal<> result;
  result.magnitude = std::get<ExtensionalCardinal<>>(c);
  result.negative = false;
  return SignedCardinality{result};
}

/** @brief @c Cardinality @c <=> @c SignedCardinality.  Routes through
 *         the canonical ℕ ↪ ℤ lift and @c compare_signed; @c NaZ on
 *         the @c rhs propagates as @c unordered. */
export constexpr std::partial_ordering operator<=>(
    const Cardinality& lhs, const SignedCardinality& rhs) noexcept {
  return compare_signed(lift_cardinality_to_signed(lhs), rhs);
}

/** @brief @c Cardinality @c == @c SignedCardinality.  @c NaZ never
 *         equals anything; otherwise the canonical lift settles the
 *         comparison. */
export constexpr bool operator==(const Cardinality& lhs,
                                 const SignedCardinality& rhs) noexcept {
  if (detail::sc_is_naz(rhs)) return false;
  // The lift produces a non-negative SignedCardinality, so any negative
  // rhs is never equal.  Otherwise compare_signed returns equivalent.
  return compare_signed(lift_cardinality_to_signed(lhs), rhs) ==
         std::partial_ordering::equivalent;
}

// ---------------------------------------------------------------------------
// Elementwise cross-carrier arithmetic (#432; paper-3 sibling of #362)
// ---------------------------------------------------------------------------
//
// The carrier-aware analog of the Set-level strength-reduction in PR
// #431: at the @b element level, arithmetic between values of different
// carriers should compose along the canonical embeddings, with the
// result type announcing the smallest carrier in which the operation
// closes.  Two distinct cases:
//
//  (a) @b Closed @b cross-carrier @b ops — both operands have the
//      operator and the result lives in the larger carrier.  Mechanical
//      promotion via @c lift_cardinality_to_signed and dispatch to
//      @c SignedCardinality's operator.
//
//  (b) @b Closure-forcing @b ops — the operator is well-defined on the
//      smaller carrier as a function into the larger, but the smaller
//      carrier isn't @b closed under it.  The operator's @b existence
//      with a wider return type is the @b structure-forcing @b axiom
//      expressed in code: this is exactly the Grothendieck construction
//      "ℤ is what one obtains by adjoining @c operator- to @c (ℕ, +)",
//      reified at the type level.  Phrasing "ℕ has no subtraction" is
//      the slight oversimplification we used in PR #425; the honest
//      reading is that subtraction is well-defined on ℕ × ℕ → ℤ; ℕ just
//      isn't closed under it.  See @c docs/design/carrier-lattice.md.

/** @brief Closure-forcing unary minus on @c Cardinality: well-defined
 *         on every natural, with codomain widened to @c
 *         SignedCardinality.  Delegates to the canonical lift @c
 *         lift_cardinality_to_signed and @c SignedCardinality's
 *         existing unary @c -, so canonicalisation rules (canonical
 *         @c +0 for negation-of-zero; @c +ℵ_0 → @c -ℵ_0 sentinel
 *         flip) flow from a single source of truth.  This is the
 *         smallest possible elementwise statement of the Grothendieck
 *         construction in code. */
export constexpr SignedCardinality operator-(const Cardinality& v) noexcept {
  return -lift_cardinality_to_signed(v);
}

/** @brief Retraction direction: @c abs : @c ℤ @c → @c ℕ.  The math-
 *         honest signature for absolute value: every element of
 *         @c SignedCardinality has a non-negative magnitude, so the
 *         result type @b is @c Cardinality (not @c
 *         SignedCardinality), announcing the non-negativity guarantee
 *         at the type level.
 *
 *  Categorically, @c abs is a @b retraction (left inverse) of the
 *  canonical embedding @c embed_uint_sint_ on the non-negative fragment of
 *  ℤ — i.e., @c abs ∘ embed_uint_sint_ @c = @c id_ℕ — extended by sign-
 *  folding on the negative fragment.  This is the @b downward
 *  complement of the closure-forcing @c -Cardinality direction
 *  above: where unary @c - @b widens @c ℕ @c → @c ℤ along the
 *  Grothendieck embedding, @c abs @b narrows @c ℤ @c → @c ℕ along
 *  the same embedding's retraction.  Closes #436.
 *
 *  Sentinel handling:
 *    * @c finite_signed_cardinality(n) (any sign) → finite
 *      Cardinality with magnitude @c |n|.
 *    * @c PositiveInfinity → @c ℵ_0 (the bound on the magnitude
 *      escalates to ℵ_0).
 *    * @c NegativeInfinity → @c ℵ_0 (sign is folded; magnitude
 *      escalates).
 *    * @c NaZ → @c ℵ_0 by analogy with the IEEE-NaN-style propagation
 *      that #396 chose for the saturating semantics; NaZ has no
 *      well-defined magnitude, but the saturated answer is the
 *      conservative bound.
 *
 *  @section cardinality__Relationship_to_std_abs
 *  Conceptually the same operation as @c std::abs at a different
 *  carrier layer, with two load-bearing differences:
 *
 *  @b 1. @b Type-honest @b codomain.  The natural categorical
 *  signature for absolute value is @c int @c → @c unsigned (the image
 *  is exactly the non-negative ints, which canonically embed into
 *  @c unsigned).  C++'s @c std::abs(int) @c → @c int is type-
 *  @b dishonest about this: the result is always non-negative but the
 *  return type doesn't say so, and the caller has to know.  This
 *  @c abs's variant-layer signature @c SignedCardinality @c → @c
 *  Cardinality (i.e. @c ℤ @c → @c ℕ) is the @b honest version of the
 *  same operation: the type announces non-negativity.  In other
 *  words, the variant @c abs is to the (type-honest) @c int @c → @c
 *  unsigned absolute value what @c lift_ℕ_ℤ_ is to the unsigned-to-
 *  signed widening — both honour at the variant layer what the
 *  machine layer signs implicitly.
 *
 *  @b 2. @b Total. @c std::abs(int) has @b undefined @b behaviour at
 *  @c INT_MIN (since @c -INT_MIN overflows the signed range).  The
 *  variant carrier @c SignedCardinality has no such pathology — the
 *  magnitude of the most-negative finite value is representable as a
 *  non-negative @c Cardinality, and the saturating @c ±ℵ_0 / @c NaZ
 *  cases extend the operation totally.
 *
 *  The commuting square (@c std::abs vs this @c abs across the
 *  machine ↔ variant lifts, with the post-@c std::abs side coerced to
 *  @c unsigned to make the codomain match) holds on the non-@c
 *  INT_MIN fragment of @c int and is partial at @c INT_MIN — @c
 *  std::abs(INT_MIN) is UB, while @c
 *  abs(embed_sint_ℤ_(INT_MIN)) is a finite Cardinality
 *  with magnitude @c |INT_MIN| (computable via the unsigned-modular
 *  expression @c 0u @c - @c static_cast<unsigned>(INT_MIN) — well-
 *  defined for any @c int width per @c
 *  std::numeric_limits<int>::digits, no 32-bit ABI assumption).  This
 *  is the same Honest-Rejection / saturating-extension pattern the
 *  project runs for @c int / @c unsigned vs the variant carriers.
 *
 *  @see std::abs (machine-layer counterpart; type-dishonest @c int →
 *       @c int signature; partial under UB at @c INT_MIN).
 *  @see lift_ℕ_ℤ_ in @c numbers:integer (the injective half of the
 *       split-mono Figure 1 pair).
 */
export constexpr Cardinality abs(const SignedCardinality& z) noexcept {
  if (detail::sc_is_naz(z)) return Cardinality{ℵ_0{}};
  if (detail::sc_is_pos_inf(z) || detail::sc_is_neg_inf(z))
    return Cardinality{ℵ_0{}};
  // Finite: extract the magnitude (an ExtensionalCardinal<>) directly.
  return Cardinality{std::get<SignedExtensionalCardinal<>>(z).magnitude};
}

/** @brief Arrow form of @c abs: an exported @c arrow<SignedCardinality,
 *         Cardinality> object.  Registered @b epic (surjective) below: every
 *         @c Cardinality is the image of itself ( @c abs(lift_ℕ_ℤ_(c)) @c =
 *         @c c) and of its negation; the map is sign-folding, conflating
 *         @c +n and @c -n.  Companion to the injective @c lift_ℕ_ℤ_ in
 *         @c numbers:integer — together they exhibit the split-mono pair
 *         from the carrier-lattice diagram (Figure 1). */
export inline constexpr auto abs_ =
    dedekind::category::arrow<SignedCardinality, Cardinality>(
        [](const SignedCardinality& z) noexcept { return abs(z); });

/** @brief Closure-forcing binary minus on @c Cardinality: well-defined
 *         as a function @c ℕ × ℕ → ℤ; ℕ isn't closed under it.  The
 *         operator's existence with the wider return type @b is the
 *         Grothendieck embedding made operational at the binary level.
 *         Routes through @c lift_cardinality_to_signed on both
 *         operands and dispatches to @c SignedCardinality's @c -. */
export constexpr SignedCardinality operator-(const Cardinality& a,
                                             const Cardinality& b) noexcept {
  return lift_cardinality_to_signed(a) - lift_cardinality_to_signed(b);
}

/** @brief Closed cross-carrier addition: @c Cardinality @c + @c
 *         SignedCardinality lifts the @c Cardinality through the
 *         canonical embedding and dispatches to @c SignedCardinality's
 *         @c +.  Closes in @c ℤ.  Carrier-promotion in the
 *         math-correct direction (larger carrier wins). */
export constexpr SignedCardinality operator+(
    const Cardinality& a, const SignedCardinality& b) noexcept {
  return lift_cardinality_to_signed(a) + b;
}

/** @brief Symmetric: @c SignedCardinality @c + @c Cardinality.
 *         Delegates to the canonical direction; addition is
 *         commutative on ℤ. */
export constexpr SignedCardinality operator+(const SignedCardinality& a,
                                             const Cardinality& b) noexcept {
  return a + lift_cardinality_to_signed(b);
}

/** @brief Closure-forcing cross-carrier subtraction: @c Cardinality
 *         @c - @c SignedCardinality.  Result is in @c ℤ — well-defined
 *         on the @c (ℕ, ℤ) pair as a function into ℤ. */
export constexpr SignedCardinality operator-(
    const Cardinality& a, const SignedCardinality& b) noexcept {
  return lift_cardinality_to_signed(a) - b;
}

/** @brief Symmetric: @c SignedCardinality @c - @c Cardinality.  Lifts
 *         @c b through the embedding; result is in @c ℤ.  Subtraction
 *         is @b not commutative, so this is a separate overload, not
 *         a delegation. */
export constexpr SignedCardinality operator-(const SignedCardinality& a,
                                             const Cardinality& b) noexcept {
  return a - lift_cardinality_to_signed(b);
}

/** @brief Closed cross-carrier multiplication: @c Cardinality @c * @c
 *         SignedCardinality lifts and dispatches.  Closes in @c ℤ. */
export constexpr SignedCardinality operator*(
    const Cardinality& a, const SignedCardinality& b) noexcept {
  return lift_cardinality_to_signed(a) * b;
}

/** @brief Symmetric: @c SignedCardinality @c * @c Cardinality.
 *         Multiplication is commutative; delegates to the canonical
 *         direction. */
export constexpr SignedCardinality operator*(const SignedCardinality& a,
                                             const Cardinality& b) noexcept {
  return a * lift_cardinality_to_signed(b);
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

/** @section cardinality__CCC_Inheritance_389 CCC inheritance (#389)
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

// Lift the bounded exact ℤ carrier @c SignedExtensionalCardinal<N>
// to IsSpecies status as well, parallel to @c SignedCardinality
// above.  Required for @c ℤ @c = @c SignedExtensionalCardinal<>
// to flow into @c ambient_set<...> as the canonical carrier per
// #399 slice 3 (the value-level @c Z constant in
// @c numbers/integer.cppm anchors @c IsSet on this carrier).  N is
// kept parametric to mirror the per-N variability already exposed
// at the carrier level.
template <std::size_t N>
struct SpeciesTraits<dedekind::sets::SignedExtensionalCardinal<N>> {
  using Domain = dedekind::sets::SignedExtensionalCardinal<N>;
  using machine_type = dedekind::sets::SignedExtensionalCardinal<N>;
};

// IsSpecies witnesses: the variant carriers are now first-class
// species and can flow into @c ambient_set<...> / ETCS object
// construction in downstream partitions (cf.\ @c sets:boundaries,
// where @c UniversalSet<Cardinality> / @c UniversalSet<SignedCardinality>
// instantiations will land as part of the #402 retarget PR).
static_assert(IsSpecies<dedekind::sets::Cardinality>,
              "Cardinality must be a recognised Species (post-#413).");
static_assert(IsSpecies<dedekind::sets::SignedCardinality>,
              "SignedCardinality must be a recognised Species (post-#413).");
static_assert(
    IsSpecies<dedekind::sets::SignedExtensionalCardinal<>>,
    "SignedExtensionalCardinal<> must be a recognised Species — anchors "
    "the @c ℤ alias's IsSet witness in @c numbers:integer (#399 slice 3).");

// ---------------------------------------------------------------------------
// Closure-forcing trait specialisations (slice of #432)
// ---------------------------------------------------------------------------
//
// Engineer's honesty obligation: assert that @c SignedCardinality is
// the @b canonical (smallest) recipient of the closure-forcing
// operations on @c Cardinality.  Justification: @c SignedCardinality
// is (the operational realisation of) the Grothendieck group of
// @c (Cardinality, +); the unary and binary @c − overloads in @c
// dedekind::sets are the universal map ℕ → Forget(ℤ) / its binary
// shadow.  See @c docs/design/carrier-lattice.md for the textbook
// dependency graph.

template <>
inline constexpr bool
    is_closure_forcing_v<std::negate<>, dedekind::sets::Cardinality,
                         dedekind::sets::SignedCardinality> = true;

template <>
inline constexpr bool
    is_closure_forcing_v<std::minus<>, dedekind::sets::Cardinality,
                         dedekind::sets::SignedCardinality> = true;

static_assert(
    IsClosureForcing<std::negate<>, dedekind::sets::Cardinality,
                     dedekind::sets::SignedCardinality>,
    "Unary @c -Cardinality is closure-forcing into @c SignedCardinality "
    "— the Grothendieck construction at the operator level.");

static_assert(IsClosureForcing<std::minus<>, dedekind::sets::Cardinality,
                               dedekind::sets::SignedCardinality>,
              "Binary @c Cardinality - Cardinality is closure-forcing into "
              "@c SignedCardinality — well-defined as a function ℕ × ℕ → ℤ.");

// And the negative-control witness: ℕ is @b not closed under unary or
// binary @c -, which is exactly what makes those ops closure-forcing.
static_assert(!IsClosedUnderUnary<dedekind::sets::Cardinality, std::negate<>>,
              "Cardinality must @b not satisfy IsClosedUnderUnary under "
              "@c std::negate<> — that's precisely what makes unary @c - "
              "closure-forcing.");
static_assert(!IsClosedUnder<dedekind::sets::Cardinality, std::minus<>>,
              "Cardinality must @b not satisfy IsClosedUnder under "
              "@c std::minus<> — that's precisely what makes binary @c - "
              "closure-forcing.");

// ---------------------------------------------------------------------------
// Surjective-arrow witness: @c abs_ is registered @b epic.
//
// Every @c Cardinality @c c is the image of itself ( @c abs_(lift_ℕ_ℤ_(c))
// @c = @c c) under @c abs_, so @c abs_ is surjective.  This is the project's
// first downstream surjective arrow registration (closes part of #459's
// downstream-witness goal): the @c IsSurjective synonym now mechanically
// fires on a real domain arrow, not just on @c Identity.  The arrow is
// @b not monic: distinct signed cardinalities (e.g. @c +3, @c -3) map to
// the same @c Cardinality, so sign is folded.
// ---------------------------------------------------------------------------
template <>
inline constexpr bool
    is_epic_arrow_v<std::decay_t<decltype(dedekind::sets::abs_)>> = true;
static_assert(IsSurjective<std::decay_t<decltype(dedekind::sets::abs_)>>,
              "abs_ : SignedCardinality → Cardinality is registered "
              "surjective; the IsSurjective synonym fires on a real domain "
              "arrow.");
static_assert(!IsInjective<std::decay_t<decltype(dedekind::sets::abs_)>>,
              "abs_ is NOT injective — it folds sign (abs(3) == abs(-3)).");

}  // namespace dedekind::category
