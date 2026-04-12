/**
 * @file ontology:numbers.cppm
 * @brief Minimal number taxonomy concepts for reintegration.
 */
module;

#include <concepts>

export module dedekind.numbers:integers;

namespace dedekind::numbers {

export template <typename T>
concept IsReflectiveSpecies = std::regular<T> && requires(T a) {
  { -a } -> std::same_as<T>;
};

export template <typename T>
concept IsInteger = std::signed_integral<T>;

export template <typename T>
concept IsNaturalNumber = std::unsigned_integral<T>;

export template <typename T>
concept IsRationalLike = std::regular<T> && requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a * b } -> std::same_as<T>;
  { a / b } -> std::same_as<T>;
};

export template <typename T>
concept IsFieldSpecies = IsRationalLike<T>;

export template <typename Q, typename Z>
concept IsRational = IsFieldSpecies<Q> && IsInteger<Z>;

export template <typename T>
concept IsRealLike = std::floating_point<T>;

export template <typename T>
concept IsReal = IsRealLike<T> || IsRationalLike<T>;

export template <typename S>
concept IsContinuous = std::regular<S>;

export template <typename S>
concept IsDiscrete = std::regular<S>;

export template <typename C, typename R>
concept IsComplex = requires(C z) {
  { z.real() } -> std::same_as<R>;
  { z.imag() } -> std::same_as<R>;
};

export template <typename M, typename E = M>
concept Group_ℤ = IsInteger<E>;

export template <typename M, typename E, typename Z>
concept Field_ℚ = IsRational<E, Z>;

export template <typename M, typename E, typename Q>
concept Continuum_ℝ = IsReal<E>;

export template <typename M, typename E, typename R>
concept Algebra_ℂ = IsComplex<E, R>;

}  // namespace dedekind::numbers
