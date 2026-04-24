/**
 * @file dedekind/algebra/galois.cppm
 * @partition :galois
 * @brief Level 3.5: Galois fields (finite fields).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Scope
 * A \emph{Galois field} (or finite field) is a field of finite
 * cardinality.  Every such field has order @f$q = p^n@f$ for some
 * prime @f$p@f$ and positive integer @f$n@f$; the prime @f$p@f$ is the
 * field's characteristic, and the field is a @f$n@f$-dimensional
 * vector space over its prime subfield @f$\mathbb{F}_p@f$.  The
 * canonical examples are @f$\mathbb{F}_p@f$ (prime-order, $n = 1$)
 * and @f$\mathrm{GF}(p^n)@f$ (prime-power order, realised as
 * @f$\mathbb{F}_p[x] / (f(x))@f$ for an irreducible @f$f@f$ of
 * degree @f$n@f$).
 *
 * This partition hosts:
 *   - @c IsGaloisField --- the concept (field + finite-cardinality
 *     opt-in);
 *   - @c is_galois_field_v, @c galois_order_v --- the carrier-
 *     authored opt-in traits;
 *   - concrete Galois-field carriers: @c bool under
 *     @c (std::bit_xor, std::bit_and) as @f$\mathbb{F}_2@f$, and
 *     @c 𝔽64 as @f$\mathrm{GF}(2^6) = \mathbb{F}_2[x]/(x^6 + x + 1)@f$.
 *
 * The generic (possibly-infinite) @c IsField concept lives in
 * @c algebra:field; vector-space structure of Galois fields over
 * their prime subfield (e.g.\ @c IsVectorSpace<𝔽64, bool>) is
 * witnessed in @c algebra:vectorspace, which imports this
 * partition to reach the carriers.
 *
 * @note "Mathematics knows no races or geographic boundaries; for
 *        mathematics, the cultural world is one country."
 *       — David Hilbert, address to the Zürich ICM (1932).
 */
module;

#include <cstddef>  // for std::size_t (galois_order_v)
#include <cstdint>  // for std::uint8_t / std::uint16_t (𝔽64 storage)
#include <functional>  // for std::plus, std::multiplies, std::bit_xor, std::bit_and
#include <stdexcept>  // for std::domain_error (𝔽64 division-by-zero)

export module dedekind.algebra:galois;

import dedekind.category;
import :field;

namespace dedekind::algebra {
using namespace dedekind::category;

/**
 * @brief Opt-in trait: is this carrier (under the given operator
 *        witnesses) a Galois field, i.e.\ a field of finite
 *        cardinality?
 *
 * @details Default @c false.  Carrier authors specialise this
 *          alongside the rest of the field-trait chain to lift a
 *          generic @c IsField claim to @c IsGaloisField.  The opt-in
 *          is required because finiteness is not structurally
 *          derivable from a C++ type signature (integer types are
 *          bounded on a machine but unbounded mathematically, dually
 *          for symbolic carriers); the author declares the
 *          mathematical claim.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
inline constexpr bool is_galois_field_v = false;

/**
 * @brief The order @f$q@f$ of a Galois field: the cardinality
 *        @f$|T|@f$ (always a prime power @f$p^n@f$).  Zero signals
 *        "not a Galois field" (the default).
 *
 * @details Useful downstream for algorithms keyed on the order,
 *          e.g.\ Fermat's @f$a^{-1} = a^{q-2}@f$ on the cyclic
 *          multiplicative group.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
inline constexpr std::size_t galois_order_v = 0;

/**
 * @concept IsGaloisField
 * @brief A finite field: axiomatic @c category::IsField plus the
 *        carrier-author finite-cardinality opt-in.
 *
 * @details Uses the axiomatic @c dedekind::category::IsField so the
 *          concept holds on carriers without the division-operator
 *          surface (e.g.\ @c bool under @c (std::bit_xor,
 *          std::bit_and)), which genuinely is a Galois field but
 *          does not expose @c operator/ or @c .inverse().  Downstream
 *          code that needs division on a Galois field should
 *          separately compose with @c IsDivisionRing; the two
 *          concerns are kept orthogonal.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
concept IsGaloisField = dedekind::category::IsField<T, Add, Mult> &&
                        is_galois_field_v<T, Add, Mult>;

/**
 * @struct 𝔽64
 * @brief The Galois field of order 64, @f$\mathrm{GF}(2^6) =
 *        \mathbb{F}_2[x] / (x^6 + x + 1)@f$.
 *
 * @details
 * The smallest non-prime finite field: 64 elements of characteristic 2,
 * cyclic multiplicative group @f$\mathbb{F}_{64}^{\times}@f$ of order
 * 63.  Each element is a polynomial of degree @f$< 6@f$ in
 * @f$\mathbb{F}_2[x]@f$, packed bit-wise into a single
 * @c std::uint8_t (bit @c i holds the coefficient of @f$x^i@f$; the
 * top two bits are always zero).
 *
 * - Addition is XOR (characteristic two; subtraction coincides, so
 *   @f$-x = x@f$).
 * - Multiplication is polynomial multiplication modulo the
 *   irreducible polynomial @f$f(x) = x^6 + x + 1@f$ (bit pattern
 *   @c 0b1000011 @c = @c 0x43).  This @f$f@f$ is both irreducible and
 *   primitive over @f$\mathbb{F}_2@f$.
 * - The multiplicative inverse is computed by Fermat's little
 *   theorem: @f$a^{-1} = a^{62}@f$ on @f$\mathbb{F}_{64}^{\times}@f$,
 *   via square-and-multiply.
 *
 * The struct wrapper exists to canonicalise the value range to
 * @f${0, \ldots, 63}@f$ (C++ concepts cannot restrict types to a
 * subset of @c uint8_t values); addition and multiplication both
 * preserve this invariant.
 *
 * As a carrier, @c 𝔽64 witnesses @c IsGaloisField (here) and
 * @c IsVectorSpace<𝔽64, bool> (in @c algebra:vectorspace).  The
 * vector-space claim uses the natural embedding
 * @f$\mathbb{F}_2 \hookrightarrow \mathbb{F}_{64}@f$ as the scalar
 * action on coordinates.
 *
 * Reference polynomial: Lidl & Niederreiter, *Finite Fields*, 2nd
 * ed. (Cambridge University Press, 1997), Table C.1.
 */
export struct 𝔽64 {
  // Canonical value 0..63; constructor masks with 0x3F so out-of-range
  // bits are reduced to zero rather than leaking into the polynomial.
  std::uint8_t value;

  constexpr 𝔽64() noexcept : value(0) {}
  explicit constexpr 𝔽64(std::uint8_t v) noexcept
      : value(static_cast<std::uint8_t>(v & 0x3Fu)) {}

  // Characteristic two: addition and subtraction coincide (bitwise XOR).
  constexpr friend 𝔽64 operator+(𝔽64 a, 𝔽64 b) noexcept {
    return 𝔽64(static_cast<std::uint8_t>(a.value ^ b.value));
  }
  constexpr friend 𝔽64 operator-(𝔽64 a, 𝔽64 b) noexcept {
    return 𝔽64(static_cast<std::uint8_t>(a.value ^ b.value));
  }
  // Unary negation: -x = x, since 2x = 0 in characteristic 2.
  constexpr friend 𝔽64 operator-(𝔽64 a) noexcept { return a; }

  // Polynomial product modulo f(x) = x^6 + x + 1.
  //   1. Carryless multiply: 6-bit × 6-bit → 11-bit intermediate.
  //   2. Reduce: for each high bit k ≥ 6 that is set, XOR f(x) << (k-6)
  //      into `prod`.  This clears bit k (the x^6·x^(k-6) term) and
  //      propagates the low (x + 1) part into lower positions.
  constexpr friend 𝔽64 operator*(𝔽64 a, 𝔽64 b) noexcept {
    std::uint16_t prod = 0;
    const std::uint16_t wide_a = a.value;
    for (int i = 0; i < 6; ++i) {
      if (b.value & (1u << i)) {
        prod ^= static_cast<std::uint16_t>(wide_a << i);
      }
    }
    constexpr std::uint16_t F = 0x43u;  // x^6 + x + 1
    for (int k = 10; k >= 6; --k) {
      if (prod & (1u << k)) {
        prod ^= static_cast<std::uint16_t>(F << (k - 6));
      }
    }
    return 𝔽64(static_cast<std::uint8_t>(prod & 0x3Fu));
  }

  // Multiplicative inverse via Fermat's little theorem on the cyclic
  // group of order 63: a^(-1) = a^62.  Square-and-multiply over the
  // exponent 62 (= 0b111110) uses 6 squarings and 5 multiplies.
  constexpr 𝔽64 inverse() const {
    if (value == 0) throw std::domain_error("𝔽64: inverse of zero.");
    𝔽64 result(static_cast<std::uint8_t>(1));
    𝔽64 base = *this;
    int exp = 62;
    while (exp > 0) {
      if (exp & 1) result = result * base;
      base = base * base;
      exp >>= 1;
    }
    return result;
  }

  constexpr friend 𝔽64 operator/(𝔽64 a, 𝔽64 b) {
    if (b.value == 0) throw std::domain_error("𝔽64: division by zero.");
    return a * b.inverse();
  }

  constexpr friend bool operator==(𝔽64 a, 𝔽64 b) noexcept {
    return a.value == b.value;
  }
};

/**
 * @brief Scalar action of @f$\mathbb{F}_2@f$ (witnessed on @c bool)
 *        on @c 𝔽64, via the natural embedding
 *        @f$\mathbb{F}_2 \hookrightarrow \mathbb{F}_{64}@f$
 *        (@f$0 \mapsto 0@f$, @f$1 \mapsto 1@f$).
 *
 * @details
 * Required by @c IsVectorSpace<𝔽64, bool> (witnessed in
 * @c algebra:vectorspace): the scalar side is the prime subfield
 * @f$\mathbb{F}_2@f$; its action on the ambient field is "multiply
 * every coordinate of @c v by the scalar bit", i.e.\
 * @c false·v @c = @c 0 and @c true·v @c = @c v.  The one-operand
 * @c std::multiplies functor dispatches here via ADL.
 */
export constexpr 𝔽64 operator*(bool s, 𝔽64 v) noexcept { return s ? v : 𝔽64{}; }

}  // namespace dedekind::algebra

namespace dedekind::category {

// --- 𝔽64 atlas registration ---
// FIXME(#382): every trait spec below is written per-(carrier, op).  A
// library helper `FieldRegistration<T, Zero, One>` would collapse the
// whole block into a single base-class inheritance on
// `SpeciesTraits<𝔽64>`.

template <>
struct SpeciesTraits<dedekind::algebra::𝔽64> {
  using Domain = dedekind::algebra::𝔽64;
  using machine_type = std::uint8_t;
};

// --- Identities ---
template <>
struct identity_trait<dedekind::algebra::𝔽64,
                      std::plus<dedekind::algebra::𝔽64>> {
  using value_type = dedekind::algebra::𝔽64;
  static constexpr value_type value{};  // 𝔽64(0)
};

template <>
struct identity_trait<dedekind::algebra::𝔽64,
                      std::multiplies<dedekind::algebra::𝔽64>> {
  using value_type = dedekind::algebra::𝔽64;
  static constexpr value_type value{static_cast<std::uint8_t>(1)};
};

// --- Algebraic facts ---
template <>
inline constexpr bool is_associative_v<dedekind::algebra::𝔽64,
                                       std::plus<dedekind::algebra::𝔽64>> =
    true;
template <>
inline constexpr bool is_associative_v<
    dedekind::algebra::𝔽64, std::multiplies<dedekind::algebra::𝔽64>> = true;

template <>
inline constexpr bool is_commutative_v<dedekind::algebra::𝔽64,
                                       std::plus<dedekind::algebra::𝔽64>> =
    true;
template <>
inline constexpr bool is_commutative_v<
    dedekind::algebra::𝔽64, std::multiplies<dedekind::algebra::𝔽64>> = true;

template <>
inline constexpr bool is_distributive_v<dedekind::algebra::𝔽64,
                                        std::multiplies<dedekind::algebra::𝔽64>,
                                        std::plus<dedekind::algebra::𝔽64>> =
    true;

// Totality via periodicity: 𝔽64 wraps modulo the irreducible f(x)
// under both operations.
template <>
struct is_periodic<dedekind::algebra::𝔽64, std::plus<dedekind::algebra::𝔽64>>
    : std::true_type {};
template <>
struct is_periodic<dedekind::algebra::𝔽64,
                   std::multiplies<dedekind::algebra::𝔽64>> : std::true_type {};

// --- Inverses ---
template <>
inline constexpr bool
    is_invertible_v<dedekind::algebra::𝔽64, std::plus<dedekind::algebra::𝔽64>> =
        true;
template <>
inline constexpr bool is_invertible_v<dedekind::algebra::𝔽64,
                                      std::multiplies<dedekind::algebra::𝔽64>> =
    true;

}  // namespace dedekind::category

namespace dedekind::algebra {

// --- Galois-field opt-ins ---

// bool under (XOR, AND) is the Galois field 𝔽2 (order 2).  Ring-,
// field-, and distributivity-trait specialisations for the bitwise
// operators on bool live in :species; the Galois-specific claim is
// the finite-cardinality opt-in below.
template <>
inline constexpr bool
    is_galois_field_v<bool, std::bit_xor<bool>, std::bit_and<bool>> = true;
template <>
inline constexpr std::size_t
    galois_order_v<bool, std::bit_xor<bool>, std::bit_and<bool>> = 2;

// 𝔽64 is the Galois field of order 64 = 2^6.
template <>
inline constexpr bool
    is_galois_field_v<𝔽64, std::plus<𝔽64>, std::multiplies<𝔽64>> = true;
template <>
inline constexpr std::size_t
    galois_order_v<𝔽64, std::plus<𝔽64>, std::multiplies<𝔽64>> = 64;

/** @section Formal_Verification */

// bool is the Galois field 𝔽2 under (XOR, AND).
static_assert(IsGaloisField<bool, std::bit_xor<bool>, std::bit_and<bool>>,
              "bool must satisfy IsGaloisField under (XOR, AND): it is "
              "the Galois field 𝔽2 (order 2).");

// 𝔽64 is the Galois field of order 64 under its polynomial arithmetic.
static_assert(dedekind::category::IsCommutativeRing<𝔽64, std::plus<𝔽64>,
                                                    std::multiplies<𝔽64>>,
              "𝔽64 must be a commutative ring (GF(2^6)).");

static_assert(
    dedekind::category::IsField<𝔽64, std::plus<𝔽64>, std::multiplies<𝔽64>>,
    "𝔽64 must satisfy the axiomatic category::IsField.");

static_assert(IsField<𝔽64, std::plus<𝔽64>, std::multiplies<𝔽64>>,
              "𝔽64 must satisfy algebra::IsField (division ring + "
              "axiomatic field).");

static_assert(IsGaloisField<𝔽64, std::plus<𝔽64>, std::multiplies<𝔽64>>,
              "𝔽64 must satisfy IsGaloisField (order 64 = 2^6).");

}  // namespace dedekind::algebra
