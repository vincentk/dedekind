export module dedekind.ontology:algebra;

import :mereology;

namespace dedekind::ontology {

// --- 1. THE TRAITS (The "Naked" Axioms) ---
export template <typename T, typename Op>
inline constexpr bool is_associative_v = false;
export template <typename T, typename Op>
inline constexpr bool is_commutative_v = false;
export template <typename T, typename Op>
inline constexpr T identity_v = T(0);
/** @brief Proof Assistant: Integers and Floats are Commutative under Addition.
 */
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;

template <std::floating_point T>
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;

/**
 * @concept IsExtensional
 * @brief A set whose identity is determined solely by its enumerated members.
 * Wikipedia: Extensionality, Axiom of extensionality
 */
export template <typename S>
concept IsExtensional = IsSet<S, typename S::element_type> && requires {
  typename S::is_extensional_tag;
  // An extensional set must be able to report its size or iterate its members.
  { s.size() } -> std::integral;
};

/**
 * @concept IsBounded
 * @brief Theorem: Every Extensional species is Bounded (in our finite
 * universe).
 */
export template <typename S>
concept IsBounded = IsExtensional<S> || requires {
  { std::numeric_limits<typename S::element_type>::max() };
};

/**
 * @section Algebra: Actions and Scaling.
 * @concept IsScalableBy
 * @brief An additive species T that can be "stepped" by an index N.
 * @note This is the "Naked" engine of the Archimedean property.
 */
export template <typename T, typename N>
concept IsScalableBy = requires(T x, N n) {
  { x * n } -> std::same_as<T>;
};

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
 * @concept IsSemigroup
 * @brief An associative Magma (No identity required).
 */
export template <typename T, typename Op>
concept IsSemigroup =
    IsMagma<T, Op> && requires { requires is_associative_v<T, Op>; };

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
concept IsOrderedAbelianGroup =
    IsAbelianGroup<T, std::plus<T>> && IsTotallyOrdered<T>;

/**
 * @concept IsSemiring
 * @brief A set with two Monoids (Add, Mul) where Mul distributes over Add.
 * @note This is the home of 'bool' and 'Natural Numbers'.
 */
export template <typename T>
concept IsSemiring =
    IsMonoid<T, std::plus<T>> && IsMonoid<T, std::multiplies<T>> &&
    requires(T a, T b, T c) {
      { a * (b + c) } -> std::same_as<T>;  // Distributivity
    };

/**
 * @concept IsRing
 * @brief A set that is both a Semiring AND an Abelian Group under addition.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mul = std::multiplies<T>>
concept IsRing = IsSemiring<T> && IsAbelianGroup<T, Add>;

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is commutative.
 */
export template <typename T>
concept IsCommutativeRing =
    IsRing<T> && is_commutative_v<T, std::multiplies<T>>;

/**
 * @concept IsModular
 * @brief An algebraic structure that wraps around a modulus (n).
 * @details x + y = (x + y) mod n.
 * Wikipedia: Modular arithmetic, Cyclic group
 */
export template <typename T>
concept IsModular = IsRing<T> && requires(T a) {
  { T::modulus() } -> std::convertible_to<T>;
  typename T::is_modular_tag;
};

/**
 * @concept IsDivisionRing
 * @brief A Ring where every non-zero element has a multiplicative inverse.
 * @details This is the formal "Roadblock" for division by zero.
 */
export template <typename T>
concept IsDivisionRing = IsRing<T> && requires(T a, T b) {
  // Axiom: b must not be the Additive Identity (Zero).
  requires(b != identity_v<T, std::plus<T>>);
  { a / b } -> std::same_as<T>;
};

/**
 * @concept IsField
 * @brief The "Painless" Field: A Commutative Ring where every non-zero element
 *        has a multiplicative inverse (Division).
 * Wikipedia: Field (mathematics)
 */
export template <typename T>
concept IsField =
    IsCommutativeRing<T> && IsDivisionRing<T> && requires(T a, T b) {
      // The Inverse Morphism for Multiplication: Division
      { a / b } -> std::same_as<T>;
    };

/**
 * @concept IsOrderedField
 * @brief A Field where the algebraic operations preserve the Total Order.
 * @details Axiom 1: If a < b, then a + c < b + c.
 *          Axiom 2: If 0 < a and 0 < b, then 0 < ab.
 * Wikipedia: Ordered field
 */
export template <typename T>
concept IsOrderedField =
    IsField<T> && IsTotallyOrdered<T> && requires(T a, T b, T c) {
      // Structural Proof: The order is invariant under translation and scaling.
      // In our Naked Ontology, we trust the species to satisfy these
      // internal laws if it claims the 'IsOrderedField' tag.
    };

/**
 * @concept IsSemimodule
 * @brief A Monoid (V) acted upon by a Semiring (S).
 */
export template <typename V, typename S>
concept IsSemimodule =
    IsMonoid<V, std::plus<V>> && IsSemiring<S> && requires(V v, S s) {
      { v * s } -> std::same_as<V>;
    };

/**
 * @section Algebra: The Linear Shelf.
 * @concept IsModule
 * @brief An Abelian Group V acted upon by a Ring S.
 * Wikipedia: Module (mathematics)
 */
export template <typename V, typename S>
concept IsModule = IsAbelianGroup<V> && IsRing<S> && requires(V v, S s) {
  { v * s } -> std::same_as<V>;
  { s * v } -> std::same_as<V>;
};

/**
 * @concept IsVectorSpace
 * @brief A Module where the Scalars form a Field.
 * Wikipedia: Vector space
 */
export template <typename V, typename S>
concept IsVectorSpace = IsModule<V, S> && IsField<S>;

}  // namespace dedekind::ontology
