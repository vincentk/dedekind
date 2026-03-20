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
 *          the IsSmallCategory "Soul".
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

/** @section The Natural Transformation Factory */
export template <template <typename> typename F, template <typename> typename G,
                 auto Morphism>
struct lift_natural_transformation {
  /** @brief The Original: Handles the "Boxed" version (F<T>) */
  template <typename T>
  constexpr auto operator()(F<T> x) const {
    return G{Morphism(static_cast<T>(x))};
  }

  /** @brief THE RECOVERY: Handles the "Naked" version (T)
   *  It wraps the raw type into F<T> automatically.
   */
  template <typename T>
  constexpr auto operator()(T x) const {
    return (*this)(F<T>{x});
  }

  template <typename T, typename OpF, typename OpG>
  static constexpr bool preserves_identity() {
    // Does the promoted identity of the source match the identity of the
    // target?
    return Morphism(identity_v<T, OpF>) ==
           identity_v<decltype(Morphism(T{})), OpG>;
  }
};

// The "Secret Sauce"
constexpr int my_promotion_sauce(bool b) { return b ? 1 : 0; }

// WE SUPPLY THEM HERE: <SourceFunctor, TargetFunctor, TheSauce>
export using BoolToInt =
    lift_natural_transformation<Identity, Identity, my_promotion_sauce>;

/** @section The "Nice Looking" Usage */

// Now BoolToInt is a real type, so we can instantiate it!
export constexpr BoolToInt transform{};

static_assert(transform(true) == 1);
static_assert(transform(false) == 0);

// 4. Verify the Category Law (The Identity Check)
static_assert(BoolToInt::preserves_identity<bool, std::logical_and<bool>,
                                            std::multiplies<int>>());


}  // namespace dedekind::ontology
