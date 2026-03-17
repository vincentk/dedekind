export module dedekind.ontology:algebra;

import :mereology;

namespace dedekind::ontology {


// --- 1. THE TRAITS (The "Naked" Axioms) ---
export template <typename T, typename Op> inline constexpr bool is_associative_v = false;
export template <typename T, typename Op> inline constexpr bool is_commutative_v = false;
export template <typename T, typename Op> inline constexpr T identity_v = T(0);
/** @brief Proof Assistant: Integers and Floats are Commutative under Addition. */
template <std::integral T> 
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;

template <std::floating_point T> 
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;

/** 
 * @section Algebra: The study of operations and structures.
 * 
 * @concept IsMagma
 * @brief The most primitive operation: a set closed under a binary operator.
 * Wikipedia: Magma (algebra)
 */
export template <typename T, typename Op>
concept IsMagma = requires(T a, T b) {
    { Op{}(a, b) } -> std::same_as<T>;
};

/**
 * @concept IsMonoid
 * @brief A Magma that is associative and has an identity element (Zero/Empty).
 * Wikipedia: Monoid, Identity element
 */
export template <typename T, typename Op>
concept IsMonoid = IsMagma<T, Op> && requires {
    { identity_v<T, Op> } -> std::same_as<T>;
    requires is_associative_v<T, Op>;
};

/**
 * @concept IsGroup
 * @brief A Monoid where every element has an inverse.
 * Wikipedia: Group (mathematics), Additive inverse
 */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && requires(T a) {
    { inverse<T, Op>(a) } -> std::same_as<T>;
};

/**
 * @section Algebra: The Hierarchy of Operations.
 * 
 * @concept IsAbelianGroup
 * @brief A Group where the operator (usually +) is commutative.
 * Wikipedia: Abelian group
 */
export template <typename T, typename Op = std::plus<T>>
concept IsAbelianGroup = IsGroup<T, Op> && is_commutative_v<T, Op>;


/**
 * @concept IsOrderedAbelianGroup
 * @brief An Abelian Group where the order is preserved by addition.
 *        If a < b, then a + c < b + c.
 * Wikipedia: Ordered abelian group
 */
export template <typename T>
concept IsOrderedAbelianGroup = IsAbelianGroup<T, std::plus<T>> && 
                                IsTotallyOrdered<T>;

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is commutative.
 */
export template <typename T>
concept IsCommutativeRing = IsRing<T> && is_commutative_v<T, std::multiplies<T>>;

/** @concept IsAbelianGroup: A Group where the order of operations doesn't matter. */
export template <typename T, typename Op = std::plus<T>>
concept IsAbelianGroup = IsGroup<T, Op> && is_commutative_v<T, Op>;

/**
 * @concept IsRing
 * @brief A set with two binary operations (Addition and Multiplication).
 * Wikipedia: Ring (mathematics)
 */
export template <typename T, typename Add = std::plus<T>, typename Mul = std::multiplies<T>>
concept IsRing = IsAbelianGroup<T, Add> && 
                 IsMonoid<T, Mul> && 
                 requires(T a, T b, T c) {
    // Axiom: Multiplication distributes over addition
    { Mul{}(a, Add{}(b, c)) } -> std::same_as<T>;
};

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is also commutative.
 * Wikipedia: Commutative ring
 */
export template <typename T>
concept IsCommutativeRing = IsRing<T> && is_commutative_v<T, std::multiplies<T>>;

/**
 * @concept IsField
 * @brief The "Painless" Field: A Commutative Ring where every non-zero element 
 *        has a multiplicative inverse (Division).
 * Wikipedia: Field (mathematics)
 */
export template <typename T>
concept IsField = IsCommutativeRing<T> && requires(T a, T b) {
    // The Inverse Morphism for Multiplication: Division
    { a / b } -> std::same_as<T>;
};

} // namespace dedekind::ontology
