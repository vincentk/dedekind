/**
 * @file ontology:category.cppm
 * @brief The Categorical "Skeletal" Foundations.
 *
 * Wikipedia: Category theory, Morphism, Functor
 */
module;

#include <concepts>
#include <functional>

export module dedekind.ontology:category;

namespace dedekind::ontology {

/** @section The Traits (The categorical invariants) */
export template <typename T, typename Op>
inline constexpr bool is_associative_v = false;

export template <typename T, typename Op>
inline constexpr T identity_v = [] {
  static_assert(sizeof(T) == 0, "No identity_v defined for this Type/Op pair.");
  return T{};
}();

/** @concept IsMagmoid - Binary Composition. */
export template <typename T, typename Op>
concept IsMagmoid = requires(T a, T b) {
  { Op{}(a, b) } -> std::convertible_to<T>;
};

/** @concept IsSemigroupoid - Associativity: (f ∘ g) ∘ h = f ∘ (g ∘ h) */
export template <typename T, typename Op>
concept IsSemigroupoid =
    IsMagmoid<T, Op> && requires { requires is_associative_v<T, Op>; };

/**
 * @concept IsSmallCategory
 * @brief Every object has an Identity Morphism (id).
 */
export template <typename T, typename Op>
concept IsSmallCategory = IsSemigroupoid<T, Op> && requires {
  { identity_v<T, Op> } -> std::convertible_to<T>;
};

/** @concept IsGroupoid - Every arrow is reversible (Isomorphism). */
export template <typename T, typename Op>
concept IsGroupoid = IsSmallCategory<T, Op> && requires(T a) {
  { inverse<T, Op>(a) } -> std::convertible_to<T>;
};

/** @section Primitive Specializations */

// --- Booleans (The Monoid of Choice) ---
template <>
inline constexpr bool is_associative_v<bool, std::logical_or<>> = true;
template <>
inline constexpr bool identity_v<bool, std::logical_or<>> = false;

template <>
inline constexpr bool is_associative_v<bool, std::logical_and<>> = true;
template <>
inline constexpr bool identity_v<bool, std::logical_and<>> = true;

// --- Integers (The Additive Group) ---
template <>
inline constexpr bool is_associative_v<int, std::plus<>> = true;
template <>
inline constexpr int identity_v<int, std::plus<>> = 0;

/**
 * @concept IsFunctor
 * @brief A mapping between categories that preserves structure.
 *
 * @details F: C ↣ D such that F(f ∘ g) = F(f) ∘ F(g).
 *          In C++, this is a Type-Morphism (template) that preserves
 *          the IsSmallCategory "Soul".
 */
export template <template <typename> typename F, typename T, typename Op>
concept IsFunctor =
    IsSmallCategory<T, Op> &&
    IsSmallCategory<F<T>, Op>;  // Simplification: assuming Op maps over F

}  // namespace dedekind::ontology
