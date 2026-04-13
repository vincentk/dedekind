/**
 * @file ontology:numbers.cppm
 * @brief Minimal number taxonomy concepts for reintegration.
 */
module;

#include <concepts>

export module dedekind.numbers:integer;

import dedekind.category;
import dedekind.sets;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

export template <typename T>
concept IsReflectiveSpecies = std::regular<T> && requires(T a) {
  { -a } -> std::same_as<T>;
  { T{} } -> std::same_as<T>;
};

export template <typename T>
concept IsInteger = std::signed_integral<T> && IsReflectiveSpecies<T>;

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

export template <typename L = ClassicalLogic, typename C = ℵ_0>
using IntegerSetOf = Ω<int, L, C>;

export using IntegerSet = IntegerSetOf<>;
export using ℤ = IntegerSet;

export inline constexpr ℤ Z{};

/**
 * @brief Canonical embedding ℕ ↪ ℤ: unsigned int → int.
 * @details The natural numbers embed into the integers via the unsigned→signed
 *          widening conversion. This is injective for values that fit in int;
 *          large unsigned values may overflow, so the domain is conventionally
 *          restricted to values ≤ INT_MAX when used with certified arithmetic.
 */
export inline constexpr auto embed_N_Z = arrow<unsigned, int>(
    [](const unsigned& x) noexcept { return static_cast<int>(x); });

/**
 * @brief Canonical embedding K3 ↪ ℤ: Ternary → int.
 * @details Maps False -> -1, Unknown -> 0, True -> 1.
 */
export inline constexpr auto embed_K3_Z =
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
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_N_Z)>> =
        true;

template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_K3_Z)>> =
        true;
}  // namespace dedekind::category
