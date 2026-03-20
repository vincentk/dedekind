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

// --- Booleans: An Abelian Monoid (Lattice) ---
template <>
inline constexpr bool is_associative_v<bool, std::logical_or<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::logical_or<bool>> = true;
template <>
inline constexpr bool identity_v<bool, std::logical_or<bool>> = false;

template <>
inline constexpr bool is_associative_v<bool, std::logical_and<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::logical_and<bool>> = true;
template <>
inline constexpr bool identity_v<bool, std::logical_and<bool>> = true;

// --- Integers: An Abelian Group (Z, +) ---
template <>
inline constexpr bool is_associative_v<int, std::plus<int>> = true;
template <>
inline constexpr bool is_commutative_v<int, std::plus<int>> = true;
template <>
inline constexpr int identity_v<int, std::plus<int>> = 0;

// Integers: A Multiplicative Monoid (Z, *)
template <>
inline constexpr bool is_associative_v<int, std::multiplies<int>> = true;
template <>
inline constexpr bool is_commutative_v<int, std::multiplies<int>> = true;
template <>
inline constexpr int identity_v<int, std::multiplies<int>> = 1;

// --- Characters: A Commutative Species ---
template <>
inline constexpr bool is_associative_v<char, std::plus<char>> = true;
template <>
inline constexpr bool is_commutative_v<char, std::plus<char>> = true;
template <>
inline constexpr char identity_v<char, std::plus<char>> = 0;

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
 *          the IsSmallCategory concept.
 */
export template <template <typename> typename F, typename T, typename Op>
concept IsFunctor =
    IsSmallCategory<T, Op> &&
    IsSmallCategory<F<T>, Op>;  // Simplification: assuming Op maps over F

/** @section Verification */
static_assert(IsAbelian<int, std::plus<int>>);
static_assert(IsAbelian<bool, std::logical_or<bool>>);

/** @section Identity Functor */
export template <typename T>
struct Identity {
  T value;
  constexpr Identity(T v) : value(v) {}
  constexpr operator T() const { return value; }
};

/** @section Lifting Traits to the Identity Functor */

// 1. If T is associative, Identity<T> is associative.
template <typename T, typename Op>
inline constexpr bool is_associative_v<Identity<T>, Op> =
    is_associative_v<T, Op>;

// 2. The identity of Identity<T> is just the identity of T, wrapped.
template <typename T, typename Op>
inline constexpr Identity<T> identity_v<Identity<T>, Op> =
    Identity<T>{identity_v<T, Op>};

/** @section The Natural Transformation Factory */
export template <template <typename> typename F, template <typename> typename G,
                 typename T,    // The Source Type (e.g., bool)
                 typename OpF,  // The Source Operation (e.g., logical_and)
                 typename OpG,  // The Target Operation (e.g., multiplies)
                 auto Morphism  // The "Secret Sauce"
                 >
  requires IsFunctor<F, T, OpF> && IsFunctor<G, decltype(Morphism(T{})), OpG>
struct lift_natural_transformation {
  /** @brief The Original: Only for items ALREADY in the box F<T> */
  constexpr auto operator()(F<T> x) const {
    return G{Morphism(static_cast<T>(x))};
  }

  /** @brief THE RECOVERY: Only for "Loose" items NOT in the box F */
  template <typename U>
    requires(!std::same_as<U, F<T>>)
  constexpr auto operator()(T x) const {
    return (*this)(F<T>{x});
  }

  static constexpr bool preserves_identity() {
    // Does the promoted identity of the source match the identity of the
    // target?
    return Morphism(identity_v<T, OpF>) ==
           identity_v<decltype(Morphism(T{})), OpG>;
  }
};

/**
 * @section The Unit of the Species (The Entry Gate)
 * @details
 *   - PhD: η (eta) - The Unit Transformation Id ⟹ G.
 *   - Haskell: pure/return - Lifts a value into a context.
 *   - Dedekind: unit - The structural "one" of the category.
 */
export template <template <typename> typename G, typename T, typename OpT,
                 typename OpG, auto Morphism>
using unit_5 = lift_natural_transformation<Identity, G, T, OpT, OpG, Morphism>;

/** @brief Extracts the argument type from a function pointer. */
template <typename T>
struct morphism_traits;

template <typename R, typename A>
struct morphism_traits<R (*)(A)> {
  using argument_type = A;
};

/** @section The 4-Parameter Unit */
export template <template <typename> typename G, typename OpF, typename OpG,
                 auto Morphism>
using unit = unit_5<
    G,
    typename morphism_traits<decltype(Morphism)>::argument_type,  // Auto-T!
    OpF, OpG, Morphism>;

// The Haskell-style alias
export template <template <typename> typename G, typename T, typename OpT,
                 typename OpG, auto Morphism>
using pure = unit<G, OpT, OpG, Morphism>;

// The Greek (Category Theory) alias
export template <template <typename> typename G, typename T, typename OpT,
                 typename OpG, auto Morphism>
using eta = unit<G, OpT, OpG, Morphism>;

constexpr int my_promotion_sauce(bool b) { return b ? 1 : 0; }

/** @section THE FIX: Supply all 6 parameters to the type alias */
export using BoolToInt = unit<Identity,
                              std::logical_and<bool>,  // OpF
                              std::multiplies<int>,    // OpG
                              my_promotion_sauce       // Morphism
                              >;

/** @section The Usage */
export constexpr BoolToInt transform{};

// This works because 'transform' is now a concrete instance of that specific
// bridge
static_assert(transform(true) == 1);

// This works because the struct already 'knows' T, OpF, and OpG!
static_assert(BoolToInt::preserves_identity());

}  // namespace dedekind::ontology
