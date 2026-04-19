/**
 * @file dedekind/numbers/integer.cppm
 * @partition :integer
 * @brief Minimal number taxonomy concepts for reintegration.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Die ganzen Zahlen hat der liebe Gott gemacht, alles andere ist
 * Menschenwerk."
 *       ("God made the integers; all else is the work of man.")
 *       -- Leopold Kronecker, Jahresbericht der DMV 2 (1891, reported)
 */
module;

#include <concepts>

export module dedekind.numbers:integer;

import dedekind.category;
import dedekind.sets;
import :naturals;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename T>
concept IsReflectiveSpecies = std::regular<T> && requires(T a) {
  { -a } -> std::same_as<T>;
  { T{} } -> std::same_as<T>;
};

/**
 * @concept IsInteger
 * @brief Structural concept for an Euclidean integer domain.
 *
 * @details Deliberately *not* restricted to `std::signed_integral<T>` so that
 * user-defined multi-precision integer types (e.g. a sign-augmented
 * `ExtensionalCardinal<N>`) can satisfy this concept without being built-in
 * C++ types.  The required operations are exactly those used by
 * `Rational<Z>::simplify()` and the ring/field machinery:
 *
 *  - Additive group: `+`, `-` (binary and unary), `T{0}`.
 *  - Multiplicative monoid: `*`, `T{1}`.
 *  - Euclidean pair: `/` and `%` (needed by `std::gcd` and `simplify()`).
 *  - Total order: `<` (needed for canonical-sign normalisation).
 *
 * **Embedding from `std::signed_integral`:** every built-in signed integer
 * type satisfies this concept unchanged — the blanket `std::signed_integral`
 * constraint is now expressed as a static proof rather than a gating
 * condition.  Use `embed_signed_integral<Z>(v)` to inject a
 * `std::signed_integral` value into an arbitrary `IsInteger` type `Z`.
 */
export template <typename T>
concept IsInteger = IsReflectiveSpecies<T> && requires(T a, T b) {
  // Additive group
  { a + b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
  // Multiplicative monoid
  { a * b } -> std::same_as<T>;
  // Euclidean domain (needed for std::gcd in Rational::simplify)
  { a / b } -> std::same_as<T>;
  { a % b } -> std::same_as<T>;
  // Total order (needed for canonical-sign normalisation)
  { a < b } -> std::convertible_to<bool>;
};

/**
 * @brief Canonical injection from `std::signed_integral` into any `IsInteger`
 *        domain `Z` via its single-argument constructor.
 *
 * @details `std::signed_integral` types (e.g. `int`) are *not* certified as
 * `IsInteger` because their addition has undefined-behaviour overflow (see the
 * `!IsMagma<int, std::plus<int>>` rejection in `dedekind.category:total`).
 * This function is the *embedding arrow* that injects a built-in signed value
 * into a well-behaved `IsInteger` domain (e.g. a future
 * `SignedExtensionalCardinal<N>`), without claiming `int` itself is such a
 * domain.
 *
 * @tparam Z  The target `IsInteger` type.
 * @tparam S  A `std::signed_integral` source type (deduced).
 */
export template <IsInteger Z, std::signed_integral S>
constexpr Z embed_signed_integral(S v) {
  return Z{v};
}

export template <typename T>
concept IsNaturalNumber = std::unsigned_integral<T>;

export template <typename T>
concept IsRationalLike = std::regular<T> && requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
  { a / b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
};

export template <typename T>
concept IsFieldLike = IsRationalLike<T>;

export template <typename Q, typename Z>
concept IsRational = IsFieldLike<Q> && IsInteger<Z> && requires(Q q) {
  { q } -> std::same_as<Q>;
};

export template <typename T>
concept IsRealLike = std::floating_point<T> && IsReflectiveSpecies<T>;

export template <typename T>
concept IsReal = IsRealLike<T> || IsRationalLike<T>;

export template <typename S>
concept IsContinuous = std::regular<S> && !std::integral<S>;

export template <typename S>
concept IsDiscrete = std::regular<S> && std::integral<S>;

export template <typename C, typename R>
concept IsComplex = requires(C z) {
  { z.real() } -> std::same_as<R>;
  { z.imag() } -> std::same_as<R>;
};

export template <typename M, typename E = M>
concept Group_ℤ = IsInteger<E> && requires(E a, E b) {
  { a + b } -> std::same_as<E>;
  { a - b } -> std::same_as<E>;
};

export template <typename M, typename E, typename Z>
concept Field_ℚ = IsRational<E, Z>;

export template <typename M, typename E, typename Q>
concept Continuum_ℝ = IsReal<E> && IsContinuous<E>;

export template <typename M, typename E, typename R>
concept Algebra_ℂ = IsComplex<E, R>;

/**
 * @brief Characteristic morphism for ℤ: the integers.
 * Accepts native int and all embedded predecessors (unsigned, Ternary).
 */
export template <typename L = ClassicalLogic, typename C = ℵ_0>
struct IntegersOf {
  using Domain = int;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = C;

  // Native int: always a member of ℤ
  constexpr typename L::Ω operator()(int) const { return L::True; }

  // Embedded unsigned (via embed_ℕ_ℤ)
  constexpr typename L::Ω operator()(unsigned n) const {
    return operator()(static_cast<int>(n));
  }

  // Embedded Ternary (via embed_K3_ℤ)
  constexpr typename L::Ω operator()(Ternary t) const {
    switch (t) {
      case Ternary::False:
        return operator()(-1);
      case Ternary::Unknown:
        return operator()(0);
      case Ternary::True:
        return operator()(1);
    }
    return L::False;
  }

  // Embedded bool (via embed_𝔹_ℕ → embed_ℕ_ℤ)
  constexpr typename L::Ω operator()(bool b) const {
    return operator()(embed_𝔹_ℕ(b));
  }
};

export using IntegerSet = IntegersOf<>;
export using ℤ = IntegerSet;

export inline constexpr ℤ Z{};

/**
 * @brief Canonical embedding ℕ ↪ ℤ: unsigned int → int.
 * @details The natural numbers embed into the integers via the unsigned→signed
 *          widening conversion. This is injective for values that fit in int;
 *          large unsigned values may overflow, so the domain is conventionally
 *          restricted to values ≤ INT_MAX when used with certified arithmetic.
 */
export inline constexpr auto embed_ℕ_ℤ = arrow<unsigned, int>(
    [](const unsigned& x) noexcept { return static_cast<int>(x); });

/**
 * @brief Canonical embedding K3 ↪ ℤ: Ternary → int.
 * @details Maps False -> -1, Unknown -> 0, True -> 1.
 */
export inline constexpr auto embed_K3_ℤ =
    arrow<Ternary, int>([](const Ternary& t) noexcept {
      switch (t) {
        case Ternary::False:
          return -1;
        case Ternary::Unknown:
          return 0;
        case Ternary::True:
          return 1;
      }
      return 0;
    });

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_ℕ_ℤ)>> =
        true;

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_K3_ℤ)>> =
        true;
}  // namespace dedekind::category
