module;

#include <concepts>
#include <functional>

import dedekind.sets;
export module dedekind.algebra:abstract;

namespace dedekind::algebra {

// --- 1. THE CLAIMS (Traits) ---
export template <typename T, typename Op>
inline constexpr bool is_commutative_v = false;
export template <typename T, typename Op>
inline constexpr bool is_associative_v = false;
export template <typename T, typename Op>
inline constexpr auto identity_v = T(0);

// --- 2. THE THEOREMS (Standard Type Proofs) ---
// These MUST be declared here so Concepts can see them immediately.

// Addition (ints)
template <std::integral T>
inline constexpr bool is_associative_v<T, std::plus<T>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;

// Multiplication (ints)
template <std::integral T>
inline constexpr bool is_associative_v<T, std::multiplies<T>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::multiplies<T>> = true;
template <std::integral T>
inline constexpr auto identity_v<T, std::multiplies<T>> = T(1);

/** @brief Axiom: Does every non-zero element have a multiplicative inverse? */
export template <typename T, typename Mul>
inline constexpr bool is_invertible_v = false;

// Theorem: Integers are NOT invertible under multiplication (1/2 is not an int)
template <std::integral T>
inline constexpr bool is_invertible_v<T, std::multiplies<T>> = false;

// Theorem: Floating point numbers ARE invertible
template <std::floating_point T>
inline constexpr bool is_invertible_v<T, std::multiplies<T>> = true;

// For Field Proofs
template <std::floating_point T>
inline constexpr bool is_associative_v<T, std::plus<T>> = true;
template <std::floating_point T>
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;
template <std::floating_point T>
inline constexpr bool is_associative_v<T, std::multiplies<T>> = true;
template <std::floating_point T>
inline constexpr bool is_commutative_v<T, std::multiplies<T>> = true;
template <std::floating_point T>
inline constexpr auto identity_v<T, std::multiplies<T>> = T(1.0);

// Strings
template <>
inline constexpr bool is_associative_v<std::string, std::plus<std::string>> =
    true;
template <>
inline constexpr auto identity_v<std::string, std::plus<std::string>> =
    std::string("");

// --- 3. THE MORPHISMS ---

export template <typename T, typename Op>
constexpr auto inverse(T a) {
  if constexpr (std::is_same_v<Op, std::plus<T>>) {
    // "Naked" check: only try to negate if T supports it
    if constexpr (requires { -a; }) return -a;
  } else if constexpr (std::is_same_v<Op, std::multiplies<T>>) {
    return T(1) / a;
  } else {
    // Only call a.inverse() if it actually exists!
    if constexpr (requires { a.inverse(); }) return a.inverse();
  }
}

// --- 4. THE CONCEPTS ---
// Now they pick up the true values of the traits above.

export template <typename T, typename Op>
concept IsMagma = requires(T a, T b) {
  { Op{}(a, b) } -> std::convertible_to<T>;
};

export template <typename T, typename Op>
concept IsSemigroup = IsMagma<T, Op> && is_associative_v<T, Op>;

export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && requires {
  { identity_v<T, Op> } -> std::convertible_to<T>;
};

export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && requires(T a) {
  { inverse<T, Op>(a) } -> std::convertible_to<T>;
};

export template <typename T, typename Op>
concept IsAbelianGroup = IsGroup<T, Op> && is_commutative_v<T, Op>;

export template <typename T>
concept IsAdditiveGroup = IsAbelianGroup<T, std::plus<T>>;

export template <typename T, typename Add = std::plus<T>,
                 typename Mul = std::multiplies<T>>
concept IsRing =
    IsAbelianGroup<T, Add> && IsMonoid<T, Mul> && requires(T a, T b, T c) {
      { Mul{}(a, Add{}(b, c)) } -> std::convertible_to<T>;
    };

/**
 * @brief A Set that explicitly belongs to an Algebraic Structure.
 * @tparam S The Set type.
 * @tparam Op The Operation (e.g. std::plus<int>).
 */
export template <typename S, typename Op>
concept IsAlgebraicSet = sets::IsSet<S, typename S::element_type> &&
                         IsMonoid<typename S::element_type, Op>;

/**
 * @brief Axiom of a Field.
 *
 * Theorem: A Ring where multiplication is commutative and invertible.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mul = std::multiplies<T>>
concept IsField = IsRing<T, Add, Mul> && is_commutative_v<T, Mul> &&
                  is_invertible_v<T, Mul> &&  // <--- ADD THIS LINE
                  requires(T a) {
                    { inverse<T, Mul>(a) } -> std::convertible_to<T>;
                  };
};  // namespace dedekind::algebra
