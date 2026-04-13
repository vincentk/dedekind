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

}  // namespace dedekind::numbers
