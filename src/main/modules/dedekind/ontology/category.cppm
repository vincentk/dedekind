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

/** 
 * @brief A Natural Transformation between Functors F and G.
 * 
 * @details In Category Theory, η: F ⟹ G is a family of morphisms η_t: F(t) → G(t).
 *          In C++, this is a polymorphic mapping that preserves the "Soul" 
 *          of the underlying category regardless of the coordinate type T.
 * 
 * @tparam F The source Functor (e.g., a "species" like Single<T>).
 * @tparam G The target Functor (e.g., a "species" like PowerSet<T>).
 * @tparam T The object type being mapped.
 */
export template <
    template <typename> typename F, 
    template <typename> typename G, 
    typename T
>
// We assume Op is implicitly handled or void for this structural mapping
requires IsFunctor<F, T, void> && IsFunctor<G, T, void>
struct NaturalTransformation {
    /**
     * @brief The component of the natural transformation at type T.
     * @param x An object in the image of functor F.
     * @return The corresponding object in the image of functor G.
     */
    G<T> operator()(F<T> x) const;
};

/** @section Primitive Specializations */

// --- Booleans: An Abelian Monoid (Lattice) ---
template <> inline constexpr bool is_associative_v<bool, std::logical_or<>> = true;
template <> inline constexpr bool is_commutative_v<bool, std::logical_or<>> = true;
template <> inline constexpr bool identity_v<bool, std::logical_or<>> = false;

template <> inline constexpr bool is_associative_v<bool, std::logical_and<>> = true;
template <> inline constexpr bool is_commutative_v<bool, std::logical_and<>> = true;
template <> inline constexpr bool identity_v<bool, std::logical_and<>> = true;

// --- Integers: An Abelian Group (Z, +) ---
template <> inline constexpr bool is_associative_v<int, std::plus<>> = true;
template <> inline constexpr bool is_commutative_v<int, std::plus<>> = true;
template <> inline constexpr int identity_v<int, std::plus<>> = 0;

// --- Characters: A Commutative Species ---
template <> inline constexpr bool is_associative_v<char, std::plus<>> = true;
template <> inline constexpr bool is_commutative_v<char, std::plus<>> = true;
template <> inline constexpr char identity_v<char, std::plus<>> = 0;

/** @section Verification */
static_assert(IsAbelian<int, std::plus<>>);
static_assert(IsAbelian<bool, std::logical_or<>>);

}  // namespace dedekind::ontology
