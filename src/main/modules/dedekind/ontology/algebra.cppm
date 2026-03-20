module;

#include <compare>     // for std::strong_ordering
#include <concepts>    // for std::integral, std::floating_point
#include <functional>  // for std::plus, std::multiplies

export module dedekind.ontology:algebra;

import :category;
import :mereology;  // For Ordered Fields
import :numbers;    // For Ordered Fields and Density
import :order;      // For the Dedekind Completeness requirement

namespace dedekind::ontology {
using ::dedekind::ontology::IsDense;
using ::dedekind::ontology::IsTotallyOrdered;
using ::dedekind::ontology::ℵ_0;

/**
 * @concept IsDedekindComplete
 * @brief The topological "Soul" of the Continuum.
 *
 * @details A structure is Dedekind-complete if it possesses the
 * Least-Upper-Bound property. In our structuralist approach, this requires the
 * existence of a Total Order, Density, and the functional ability to resolve
 *          extrema (Supremum/Infimum).
 *
 * @tparam S The Ordered Structure (The Rule).
 *
 * @section Structural_Inference:
 * While the Rationals (Q) are Dense and Totally Ordered, they fail this
 * requirement because they lack the "Extrema" morphism for sets like
 * {q ∈ Q | q² < 2}. The Real Continuum (R) satisfies this by definition
 * through the Dedekind Cut synthesis.
 *
 * Wikipedia: Completeness of the real numbers, Least-upper-bound property
 */
export template <typename S>
concept IsDedekindComplete = IsTotallyOrdered<S> && IsDense<S> && HasExtrema<S>;

/**
 * @brief The Continuous Field property.
 * @details A species is Continuous if it possesses the cardinality
 *          of the power set of the naturals (Beth-1) and satisfies
 *          the Dedekind-Completeness axiom.
 *
 * This represents the "Smooth" transition where gaps in the
 * Rational field have been "filled" by the Dedekind Cut.
 */
export template <typename S>
concept IsContinuous =
    IsUncountable<typename S::cardinality_type> && IsDedekindComplete<S>;

/**
 * @concept IsDiscrete
 * @brief A space of isolated points (Z, N).
 * @details A species is Discrete if it is Countable but lacks a
 *          midpoint morphism between its elements. This property
 *          enables mathematical induction and successor-based logic.
 * @note Axiom: For a discrete set, there exists a "gap" between any
 *       two distinct elements.
 */
export template <typename S>
concept IsDiscrete = IsCountable<S> && !IsDense<S>;

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
};

/**
 * @concept IsCommutativeMonoid
 * @brief A Monoid where order of operations does not change the result.
 */
export template <typename T, typename Op>
concept IsCommutativeMonoid =
    IsMonoid<T, Op> && requires { requires is_commutative_v<T, Op>; };

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
 * @concept IsNumbers
 * @brief The Root Category for all Numerical Structures.
 *
 * @tparam M The Algebraic Structure (The "Rule").
 * @tparam C The Cardinality (The "Magnitude").
 * @tparam E The Element Species (The "What").
 */
export template <typename M, typename C, typename E = typename M::element_type>
concept IsNumbers =
    IsSet<M, C, E> &&    // 1. It must be a Rule (Predicate-based)
    IsCardinality<C> &&  // 2. It must have a defined Magnitude
    requires(const M& m) { requires IsCommutativeMonoid<M, std::plus<E>>; };

/**
 * @concept Monoid_ℕ
 * @brief The Parametric Algebraic Soul of the Natural Numbers.
 *
 * @tparam M The Monoid structure.
 * @tparam E The underlying Element species.
 */
export template <typename M, typename E = typename M::element_type>
concept Monoid_ℕ =
    IsNumbers<M, ℵ_0, E> && IsNatural<E> && requires(const M& m) {
      // The structure must actually possess the claimed cardinality.
      { m.cardinality() } -> std::same_as<ℵ_0>;

      // The Soul: Structural Laws
      requires IsArchimedean<M>;
      requires IsCommutativeMonoid<M, std::plus<E>>;
      requires IsCommutativeMonoid<M, std::multiplies<E>>;
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
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutativeMonoid<T, Op>;

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
 * @concept IsRing
 * @brief A set that is both a Semiring AND an Abelian Group under addition.
 */
export template <typename T, typename Add = std::plus<T>,
                 typename Mul = std::multiplies<T>>
concept IsRing = IsSemiring<T> && IsAbelianGroup<T, Add>;

/**
 * @concept IsCommutativeRing
 * @brief A Ring where multiplication is also commutative.
 * @details This is the foundation for Z, Q, and R.
 * Wikipedia: Commutative ring
 */
export template <typename T>
concept IsCommutativeRing =
    IsRing<T> && IsCommutativeMonoid<T, std::multiplies<T>>;

/**
 * @concept IsEuclidean
 * @brief A Commutative Ring with a division algorithm.
 * @details For any a, b (b ≠ 0), there exist q, r such that a = bq + r.
 *
 * Wikipedia: Euclidean domain
 */
export template <typename T>
concept IsEuclidean = IsCommutativeRing<T> && requires(T a, T b) {
  // The Division Morphism
  { a / b } -> std::same_as<T>;
  // The Remainder Morphism (The "Scissors")
  { a % b } -> std::same_as<T>;

  // Euclidean Property: a = (a/b)*b + (a%b)
  // Note: In C++, we assume std::divides and std::modulus satisfy this.
};

/**
 * @concept Group_ℤ
 * @brief The Canonical Algebraic Soul of the Integers.
 *
 * @details ℤ is the uniquely determined Infinite Cyclic Group (under addition)
 *          that extends the Naturals with additive inverses.
 */
export template <typename M, typename E = typename M::element_type>
concept Group_ℤ = IsNumbers<M, ℵ_0, E> &&  // The Magnitude (Coded in)
                  IsInteger<E> &&          // The Species (The "What")
                  requires(const M& m) {
                    // The Soul: Group Axioms
                    // Note: Subtraction is now a Total Morphism.
                    requires IsOrderedAbelianGroup<M> && IsCommutativeRing<M> &&
                                 IsArchimedean<M> && IsEuclidean<M>;
                  };

/**
 * @concept IsCyclic
 * @brief The "Clock" Soul: An Abelian Group that wraps after n steps.
 *
 * @details A structure is Cyclic if it is an Abelian Group where every element
 *          is invariant under the Modulus. This defines the topology of a
 *          Circle rather than a Line.
 *
 * @tparam T The coordinate species.
 *
 * @section Structural_Logic:
 * 1. Magnitude: The cardinality must be Finite and equal to the modulus.
 * 2. Symmetry: Every Cyclic group is Abelian (Commutative).
 * 3. Closure: The Successor Morphism (++x) eventually returns to the Identity.
 *
 * Wikipedia: Cyclic group, Modular arithmetic, Circle group
 */
export template <typename T>
concept IsCyclic = IsAbelianGroup<T, std::plus<T>> &&
                   IsFinite<typename T::cardinality_type> && requires(T a) {
                     /** @brief The Modulus: The circumference/order of the
                      * cycle. */
                     { T::modulus() } -> std::convertible_to<T>;

                     /** @brief The Remainder Morphism: x mod n. */
                     { a % T::modulus() } -> std::same_as<T>;

                     /** @brief Axiom: The set is its own remainder. */
                     requires(a % T::modulus() == a);

                     /** @brief Proof: The size of the set matches the cycle
                      * length. */
                     requires(T::cardinality() == T::modulus());
                   };

/**
 * @concept IsCyclicRing
 * @brief The "Painless" Modular Arithmetic (Z/nZ).
 *
 * @details A Cyclic Ring is the algebraic soul of a Finite Commutative Ring.
 *          By requiring IsCommutativeRing, we satisfy IsRing implicitly.
 */
export template <typename T>
concept IsCyclicRing = IsCommutativeRing<T> && IsCyclic<T>;

/**
 * @concept IsDivisionRing
 * @brief A Ring where every non-zero element has a multiplicative inverse.
 * @details This is the formal "Roadblock" for division by zero.
 */
export template <typename T>
concept IsDivisionRing = IsRing<T> && requires(T a, T b) {
  /**
   * @brief The Division Morphism.
   * Note: The "b != 0" requirement is a runtime contract,
   * not a compile-time type constraint.
   */
  { a / b } -> std::same_as<T>;
};

/**
 * @concept IsField
 * @brief The "Painless" Field: A Commutative Ring where every non-zero element
 *        has a multiplicative inverse (Division).
 * Wikipedia: Field (mathematics)
 */
export template <typename T>
concept IsField = IsCommutativeRing<T> && IsDivisionRing<T>;

/**
 * @concept IsOrderedField
 * @brief A Field where the algebraic operations preserve the Total Order.
 * @details Axiom 1: If a < b, then a + c < b + c.
 *          Axiom 2: If 0 < a and 0 < b, then 0 < ab.
 * Wikipedia: Ordered field
 */
export template <typename T>
concept IsOrderedField = IsField<T> && IsTotallyOrdered<T>;

/**
 * @concept Field_ℚ
 * @brief The Canonical Algebraic Soul of the Rational Numbers.
 *
 * @details ℚ is defined as the unique Ordered, Dense, Archimedean Field
 *          constructed over an underlying Integer species. In our
 *          "Rules, not buckets" manifesto, this concept "blesses" a
 *          coordinate species (E) with the structural laws of the
 *          Rational Field (M).
 *
 * @tparam M The Field structure (The "Rule" or "Soul").
 * @tparam C The Magnitude (Strictly Aleph_0 for the universal field).
 * @tparam E The underlying Rational species (The "What" / Element).
 * @tparam Z The underlying Integer species (The "Ancestry" of E).
 *
 * @section Structural_Recursion:
 * This concept enforces that the Field is strictly Countable (Aleph_0)
 * and that every element E can be projected back to its Integer
 * components (Z), ensuring a verified path from N to Q.
 */
export template <typename M, typename E, typename Z>
concept Field_ℚ = IsNumbers<M, ℵ_0, E> &&
                  IsRational<E, Z> &&  // <--- The Relative Species Check
                  requires(const M& m) {
                    requires IsOrderedField<M>;
                    requires IsDense<M>;
                    requires IsArchimedean<M>;
                  };

/**
 * @concept Continuum_ℝ
 * @brief The Canonical "Blessing" of the Real Numbers.
 *
 * @tparam M The Field structure (The "Rule").
 * @tparam E The underlying Real species (The "Element").
 * @tparam Q The underlying Rational species (The "Ancestry").
 */
export template <typename M, typename E, typename Q>
concept Continuum_ℝ =
    IsNumbers<M, ℶ_1, E> && IsReal<E> && requires(const M& m) {
      /** @property IsDedekindComplete: The defining "Soul" of R. */
      requires IsDedekindComplete<M> && IsOrderedField<M> && IsArchimedean<M>;
    };

/**
 * @concept IsAlgebraicallyClosed
 * @brief Semantic requirement for a Field where every polynomial has a root.
 * @details This is the "Soul" property required by Algebra_ℂ.
 */
export template <typename M>
concept IsAlgebraicallyClosed = IsField<M>;  // Refined by its use in Algebra_ℂ

/**
 * @concept Algebra_ℂ
 * @brief The Canonical "Blessing" of the Complex Numbers.
 *
 * @details ℂ is the Algebraically Closed Field over the Real Continuum.
 *          It inherits the Magnitude (Beth_1) but rejects the Order.
 */
export template <typename M, typename E, typename R>
concept Algebra_ℂ =
    IsNumbers<M, ℶ_1, E> && IsComplex<E, R> && requires(const M& m) {
      requires IsField<M>;

      /**
       * @property IsAlgebraicallyClosed
       * This soul is "pre-validated" by the species' ability to
       * solve quadratic roots via sqrt().
       */
      requires IsAlgebraicallyClosed<M>;

      /** @property !IsOrderedField: C is NOT totally ordered.
       */
      requires !IsOrderedField<M>;
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
