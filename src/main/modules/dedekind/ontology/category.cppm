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

/** 
 * @brief Trait to mark an operation as associative: (a ∘ b) ∘ c = a ∘ (b ∘ c)
 **/
export template <typename T, typename Op>
inline constexpr bool is_associative_v = false;

/**
 * @brief Trait to mark an operation as commutative: a ∘ b = b ∘ a
 **/
export template <typename T, typename Op>
inline constexpr bool is_commutative_v = false;

/**
 * @brief The Identity Morphism for a given Type and Operation.
 * @note Must be specialized for each Category/Monoid.
 */
export template <typename T, typename Op>
inline constexpr T identity_v = [] {
  // Hard-coding 'false' here usually breaks the build immediately.
  // We use 'sizeof(T) == 0' as a trick to make it dependent on T.
  static_assert(sizeof(T) == 0,
                "Dedekind: No identity_v defined for this Type/Op pair.");
  return T{};
}();

/**
 * @brief The Inverse Morphism for Groupoids.
 * @note Must be implemented for types satisfying IsGroupoid.
 */
export template <typename T, typename Op>
T inverse(T a);

/**
 * @concept IsMagmoid
 * @brief Basic binary composition: T × T → T.
 */
export template <typename T, typename Op>
concept IsMagmoid = requires(T a, T b) {
  { Op{}(a, b) } -> std::convertible_to<T>;
};

/**
 * @concept IsSemigroupoid
 * @brief : (f ∘ g) ∘ h = f ∘ (g ∘ h)
 * */
export template <typename T, typename Op>
concept IsSemigroupoid =
    IsMagmoid<T, Op> && requires { requires is_associative_v<T, Op>; };

/**
 * @concept IsSmallCategory
 * @brief A Semigroupoid where every object has an identity morphism.
 */
export template <typename T, typename Op>
concept IsSmallCategory = IsSemigroupoid<T, Op> && requires {
  { identity_v<T, Op> } -> std::convertible_to<T>;
};

/**
 * @concept IsGroupoid
 * @brief Every arrow is reversible (Isomorphism).
 **/
export template <typename T, typename Op>
concept IsGroupoid = IsSmallCategory<T, Op> && requires(T a) {
  { inverse<T, Op>(a) } -> std::convertible_to<T>;
};

/** 
 * @concept IsAbelian 
 * @brief A Category where the binary operation is commutative.
 */
export template <typename T, typename Op>
concept IsAbelian = IsSmallCategory<T, Op> && is_commutative_v<T, Op>;

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

}  // namespace dedekind::ontology
