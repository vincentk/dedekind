/**
 * @file dedekind/linear_algebra/contracts.cppm
 * @partition :contracts
 * @brief Rank-nullity and matrix-structural compile-time contracts.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @details
 * Two families of contracts live in this partition:
 *
 *  1. Rank-nullity — historical scope: `RankNullityWitness`,
 *     `StaticRankNullity`, `LinearOperatorContract`.
 *
 *  2. Matrix-structural concepts encoding the slogans:
 *     - a matrix column is a vector;
 *     - a matrix row is a covector (row vector);
 *     - vectors and covectors have a dimensionality, parallel to set
 *       cardinality (the `dedekind.sets:cardinality` ladder);
 *     - a matrix is both a horizontal concatenation of column vectors
 *       and a vertical concatenation of row vectors;
 *     - a matrix therefore carries two dimensionalities: row count and
 *       column count;
 *     - matrices over a ring form at least a submodule, and under full
 *       module axioms a module;
 *     - matrices over a field form at least a ring; invertible matrices
 *       additionally carry the inverse operation (loosely, "sometimes a
 *       field" — GLₙ(F) is a multiplicative group, and in the 1×1 case
 *       the full ring structure collapses to the field F itself);
 *     - matrix multiplication is associative but not commutative in
 *       general, and composes as a linear map;
 *     - matrices are equipped with all the ring operations plus
 *       transpose.
 *
 * @note "Nullity is not nothingness; it is the complement of reach."
 *       -- Anonymous lecture note, Linear Algebra II, 1990s
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>
#include <utility>

export module dedekind.linear_algebra:contracts;

import dedekind.algebra;

export namespace dedekind::linear_algebra {

/** @section contracts__Rank_Nullity_Contracts (historical scope) */

/**
 * @concept RankNullityWitness
 * @brief Static witness for the rank-nullity identity.
 */
template <typename W>
concept RankNullityWitness = requires {
  { W::ambient_dimension } -> std::convertible_to<std::size_t>;
  { W::rank } -> std::convertible_to<std::size_t>;
  { W::nullity } -> std::convertible_to<std::size_t>;
  requires(W::rank <= W::ambient_dimension);
  requires(W::nullity <= W::ambient_dimension);
  requires(W::rank + W::nullity == W::ambient_dimension);
};

/**
 * @struct StaticRankNullity
 * @brief Convenience compile-time carrier for finite-dimensional witnesses.
 */
template <std::size_t AmbientDimension, std::size_t Rank>
  requires(Rank <= AmbientDimension)
struct StaticRankNullity {
  static constexpr std::size_t ambient_dimension = AmbientDimension;
  static constexpr std::size_t rank = Rank;
  static constexpr std::size_t nullity = AmbientDimension - Rank;
};

/**
 * @concept LinearOperatorContract
 * @brief Minimal contract for finite-dimensional linear operator carriers.
 */
template <typename Op>
concept LinearOperatorContract = requires {
  typename Op::scalar_type;
  typename Op::rank_nullity;
  requires RankNullityWitness<typename Op::rank_nullity>;
};

/** @section contracts__Dimensionality A finite cardinality of coordinate axes.
 *
 *  `HasDimensionCount<V>` asks a carrier to expose its coordinate count as
 *  a compile-time `std::size_t`. This is the linear-algebraic specialisation
 *  of the set-theoretic `ExtensionalCardinal<>` carrier in
 *  `dedekind.sets:cardinality`: both count a finite index set, but the
 *  ambient structure (axes vs. elements) is what distinguishes a dimension
 *  from a generic cardinality.
 */

/**
 * @concept HasDimensionCount
 * @brief A carrier exposing a compile-time `dimension` count (axes or slots).
 */
template <typename V>
concept HasDimensionCount = requires {
  { V::dimension } -> std::convertible_to<std::size_t>;
};

/** @section contracts__Orientation The column/row distinction for 1-tensors. */

/** @brief Tag for column-oriented 1-tensors (vectors in the usual sense). */
struct ColumnOrientation {};

/** @brief Tag for row-oriented 1-tensors (covectors, linear functionals). */
struct RowOrientation {};

/**
 * @concept HasOrientation
 * @brief A carrier that declares whether it is a column or a row.
 */
template <typename V>
concept HasOrientation =
    requires { typename V::orientation; } &&
    (std::same_as<typename V::orientation, ColumnOrientation> ||
     std::same_as<typename V::orientation, RowOrientation>);

/** @section contracts__Vectors and covectors.
 *
 *  A column/row vector is not a new operational predicate --- it @b is the
 *  canonical @c dedekind::algebra::IsSemimodule (an additive commutative monoid
 *  under a linear scalar action), plus a @c ColumnOrientation / @c
 * RowOrientation tag and a finite dimension.  The @b default is the semimodule
 * (over a
 *  @b semiring), because that is the structure a max-plus / tropical vector
 *  actually has --- a dioid has no additive inverse.  The ring/field carriers
 *  (@c Vec2V<Field>) refine it to @c dedekind::algebra::IsModule /
 *  @c IsVectorSpace, the concepts that @b add the inverse; there is no
 *  bespoke @c *Like duplicate here (that operational family was retired in
 *  @c algebra:modules).
 *
 *  The scalar's @c ⊕/⊗ come straight from its semiring structure --- if @c S
 *  @c IsSemiring, it @b has the ops, so we use them.  @ref semiring_ops_of
 *  reads @c dedekind::algebra::semiring_ops<S> where @c S declares one
 *  (tropical, bool, @c Mat(S)) and falls back to the standard @c +/@c · for an
 *  ordinary ring; it @b registers nothing (a ring scalar needs no
 *  @c semiring_ops entry).
 */
template <typename S>
struct semiring_ops_of {
  using add = std::plus<S>;
  using mult = std::multiplies<S>;
};
template <typename S>
  requires requires { typename dedekind::algebra::semiring_ops<S>::add; }
struct semiring_ops_of<S> {
  using add = typename dedekind::algebra::semiring_ops<S>::add;
  using mult = typename dedekind::algebra::semiring_ops<S>::mult;
};

/**
 * @concept IsColumnVector
 * @brief A column vector: an @c IsSemimodule with @c ColumnOrientation and a
 *        finite dimension.  "A matrix column is a (semimodule) vector" ---
 * every
 *        @c column_type on an @c IsMatrix carrier models this.  Over a ring the
 *        carrier further satisfies @c dedekind::algebra::IsModule; over a dioid
 *        (@c MaxPlus) it does not, and none is asked.
 */
template <typename V>
concept IsColumnVector =
    HasDimensionCount<V> && HasOrientation<V> &&
    std::same_as<typename V::orientation, ColumnOrientation> &&
    dedekind::algebra::IsSemimodule<
        V, typename V::scalar_type, std::plus<V>,
        typename semiring_ops_of<typename V::scalar_type>::add,
        typename semiring_ops_of<typename V::scalar_type>::mult>;

/**
 * @concept IsCovector
 * @brief A row vector / covector: the @c RowOrientation twin of
 *        @ref IsColumnVector --- an @c IsSemimodule row.
 */
template <typename V>
concept IsCovector =
    HasDimensionCount<V> && HasOrientation<V> &&
    std::same_as<typename V::orientation, RowOrientation> &&
    dedekind::algebra::IsSemimodule<
        V, typename V::scalar_type, std::plus<V>,
        typename semiring_ops_of<typename V::scalar_type>::add,
        typename semiring_ops_of<typename V::scalar_type>::mult>;

/** @section contracts__Matrix shape and decomposition. */

/**
 * @concept HasMatrixShape
 * @brief A matrix carries two dimensionalities: row count and column count.
 *
 *  This is the joint-shape claim — a direct generalisation of
 *  `HasDimensionCount` from one axis to two. Row count is the dimension of
 *  the column space (number of entries per column); column count is the
 *  dimension of the row space (number of entries per row).
 */
template <typename M>
concept HasMatrixShape = requires {
  { M::row_count } -> std::convertible_to<std::size_t>;
  { M::column_count } -> std::convertible_to<std::size_t>;
};

/**
 * @concept HasColumnType
 * @brief A matrix exposes its columns as a concrete vector carrier.
 */
template <typename M>
concept HasColumnType = requires {
  typename M::column_type;
  requires IsColumnVector<typename M::column_type>;
  requires HasDimensionCount<typename M::column_type>;
};

/**
 * @concept HasRowType
 * @brief A matrix exposes its rows as a concrete covector carrier.
 */
template <typename M>
concept HasRowType = requires {
  typename M::row_type;
  requires IsCovector<typename M::row_type>;
  requires HasDimensionCount<typename M::row_type>;
};

/**
 * @concept HasColumnDecomposition
 * @brief A matrix can be viewed as the horizontal concatenation of its
 *        column vectors.
 *
 *  Requires an indexed accessor `m.column(i)` returning the i-th column. The
 *  horizontal-concatenation view is established by: iterating the indices
 *  `0 .. column_count-1` recovers the full matrix when the entries are fixed
 *  by the columns.
 */
template <typename M>
concept HasColumnDecomposition =
    HasColumnType<M> && requires(const M& m, std::size_t i) {
      { m.column(i) } -> std::same_as<typename M::column_type>;
    };

/**
 * @concept HasRowDecomposition
 * @brief A matrix can be viewed as the vertical concatenation of its
 *        row vectors.
 */
template <typename M>
concept HasRowDecomposition =
    HasRowType<M> && requires(const M& m, std::size_t i) {
      { m.row(i) } -> std::same_as<typename M::row_type>;
    };

/**
 * @concept IsMatrix
 * @brief The structural-matrix slogan-pack:
 *        shape + column/row types + both decompositions.
 *
 *  A type satisfies `IsMatrix` iff it
 *    - carries a `scalar_type`,
 *    - exposes both `row_count` and `column_count`,
 *    - exposes `column_type` as an `IsColumnVector`,
 *    - exposes `row_type` as an `IsCovector`,
 *    - admits both the horizontal and the vertical decompositions.
 */
template <typename M>
concept IsMatrix = requires { typename M::scalar_type; } && HasMatrixShape<M> &&
                   HasColumnDecomposition<M> && HasRowDecomposition<M>;

/** @section contracts__Algebraic_Contracts Ring, module, field structure on
 * matrices. */

/**
 * @concept IsMatrixSubmoduleLike
 * @brief Matrices over a ring form at least a submodule-like carrier.
 *
 *  Operational witness: additive closure, unary negation, and a left scalar
 *  action. This is the "at least a submodule" half of the user's claim; the
 *  full module proof is the strict `IsModule<M, R>` specialisation below.
 */
template <typename M, typename R>
concept IsMatrixSubmoduleLike = requires(M a, M b, R s) {
  { a + b } -> std::same_as<M>;
  { a - b } -> std::same_as<M>;
  { -a } -> std::same_as<M>;
  { s * a } -> std::same_as<M>;
  { a * s } -> std::same_as<M>;
};

/**
 * @concept IsMatrixAsModule
 * @brief The strict "sometimes a module" witness.
 *
 *  Delegates to the categorical `dedekind.algebra:modules::IsModule<M, R>`
 *  proof. The gap between `IsMatrixSubmoduleLike` and `IsMatrixAsModule` is
 *  exactly the gap between operational closure and the full axiomatic
 *  witnesses (associativity, distributivity, ring structure on R).
 */
template <typename M, typename R>
concept IsMatrixAsModule = dedekind::algebra::IsModule<M, R>;

/**
 * @concept HasMatrixMultiplication
 * @brief Square-matrix multiplication: `M × M → M`.
 *
 *  Associative by contract (matrices represent linear maps, and composition
 *  of linear maps is associative), but **not commutative** in general. The
 *  concept therefore only asserts closure of the product; callers must not
 *  assume `a * b == b * a`. Documented as a non-commutative linear
 *  composition, per the user-stated contract.
 */
template <typename M>
concept HasMatrixMultiplication = requires(M a, M b) {
  { a * b } -> std::same_as<M>;
};

/**
 * @concept HasTranspose
 * @brief A matrix carrier equipped with a transpose operation.
 *
 *  The transposed matrix type is not required to equal `M`; for a general
 *  `m × n` matrix the transpose lives in an `n × m` carrier. Square matrix
 *  carriers can and typically do return `M` from `transpose()`.
 */
template <typename M>
concept HasTranspose = requires(const M& a) {
  { a.transpose() };
};

/**
 * @concept HasInvolutiveTranspose
 * @brief Transpose is its own inverse at the type level:
 *        `decltype((a.transpose()).transpose())` is `M`.
 *
 *  This is the structural half of the involution law `(Mᵀ)ᵀ = M`. The
 *  *value-level* equality `(a.transpose()).transpose() == a` is a per-type
 *  theorem, witnessed via `static_assert` on concrete instances; the concept
 *  just pins that the types round-trip.
 *
 *  Involutions generate a ℤ/2 action on matrix carriers: the two-element
 *  group {identity, transpose} under composition. `Matrix2x2V<T>` is a
 *  degenerate witness (the orbit `{M, Mᵀ}` lives entirely inside
 *  `Matrix2x2V<T>`); the non-degenerate action is the
 *  `Vec2V<T> ↔ Covec2V<T>` exchange captured by `IsTransposeDualPair`.
 */
template <typename M>
concept HasInvolutiveTranspose =
    HasTranspose<M> &&
    std::same_as<std::remove_cvref_t<
                     decltype(std::declval<M>().transpose().transpose())>,
                 std::remove_cvref_t<M>>;

/**
 * @concept IsTransposeDualPair
 * @brief `V` and `Covec` are dual under transpose: `V → Covec` and
 *        `Covec → V`, with both round-trips returning the original type.
 *
 *  Formalises the column/row duality. `transpose` is the canonical
 *  isomorphism `V ↔ V*` in finite dimension; double-transpose returns to
 *  the original carrier on each side. Together the pair `(V, Covec)`
 *  carries the same ℤ/2 action as `HasInvolutiveTranspose`, but spelled
 *  across two distinct orientations.
 */
template <typename V, typename Covec>
concept IsTransposeDualPair =
    HasTranspose<V> && HasTranspose<Covec> &&
    std::same_as<std::remove_cvref_t<decltype(std::declval<V>().transpose())>,
                 std::remove_cvref_t<Covec>> &&
    std::same_as<
        std::remove_cvref_t<decltype(std::declval<Covec>().transpose())>,
        std::remove_cvref_t<V>>;

/**
 * @concept IsMatrixOverFieldRingLike
 * @brief Matrices over a field carry at least a ring structure plus transpose.
 *
 *  Encodes the slogan "matrix over a field is at least a ring, equipped with
 *  all the ring operations plus transpose". The concept does not require
 *  commutativity of multiplication; the ring is non-commutative in general.
 */
template <typename M, typename F>
concept IsMatrixOverFieldRingLike =
    IsMatrixSubmoduleLike<M, F> && HasMatrixMultiplication<M> &&
    HasTranspose<M>;

/**
 * @concept HasMultiplicativeInverse
 * @brief A matrix carrier with a closed-form inverse operation.
 *
 *  Weaker than `dedekind.algebra:IsDivisionRing<M>`, which demands full ring
 *  closure (+, -, *) alongside `a.inverse()`. For `n ≥ 2` square-matrix
 *  carriers over a field, `IsDivisionRing<M>` does **not** hold: Mₙ(F) has
 *  zero divisors (e.g. `[[1,0],[0,0]]·[[0,0],[0,1]] = 0`). The only
 *  matrix-flavoured carriers for which `IsDivisionRing` fires cleanly are
 *  `M₁(F) ≅ F` and genuine division algebras (quaternions, ...).
 */
template <typename M>
concept HasMultiplicativeInverse = requires(const M& a) {
  { a.inverse() };
};

/**
 * @concept IsInvertibleMatrixOverField
 * @brief A matrix over a field that carries ring-like structure plus an
 *        `.inverse()` operation.
 *
 *  Honest taxonomy (spelled out because the user-level slogan "matrix over
 *  a field is sometimes a field if invertible" is deliberately loose):
 *    - Mₙ(F) for n ≥ 2:      ring, not division ring, not field.
 *    - GLₙ(F):               multiplicative group, not a ring (no additive
 *                             closure — sum of invertibles can be singular).
 *    - M₁(F) ≅ F:            the only collapse where ring = division ring
 *                             = field all coincide.
 *    - Orthogonal O(n, F):   multiplicative group (see `IsOrthogonalMatrix`).
 *    - Quaternions ℍ:        a non-commutative division ring, but not a
 *                             matrix type in this hierarchy.
 *
 *  This concept therefore captures an honest structural subset: "has
 *  ring-like closure and exposes an inverse". It does not claim division-
 *  ring status for the carrier.
 */
template <typename M, typename F>
concept IsInvertibleMatrixOverField =
    IsMatrixOverFieldRingLike<M, F> && HasMultiplicativeInverse<M>;

/** @section contracts__Orthogonal Orthogonal matrices: the cleanest
 * group-under-multiplication.
 *
 *  An orthogonal matrix satisfies `Mᵀ · M = M · Mᵀ = I`, so its inverse is
 *  its transpose. Orthogonal matrices are **not** a ring (not additively
 *  closed: `I + I = 2I` fails orthogonality), so `IsDivisionRing` does not
 *  fire here. They form a multiplicative group `O(n, F)`:
 *    - closed under multiplication: `(AB)ᵀ(AB) = BᵀAᵀAB = BᵀIB = I`,
 *    - identity `I ∈ O(n)`,
 *    - inverse `M⁻¹ = Mᵀ ∈ O(n)`.
 *
 *  Structurally the concept asks only for matrix multiplication plus an
 *  involutive transpose; the orthogonality relation `MᵀM = I` itself is a
 *  value-level law, witnessed per-instance on concrete matrices.
 */

/**
 * @concept IsOrthogonalMatrixCarrier
 * @brief Carries the operations under which the orthogonality law
 *        `Mᵀ · M = I` makes sense.
 *
 *  Orthogonality is a per-value predicate (concretely witnessed via
 *  `static_assert(m.transpose() * m == Identity{})`). The concept only pins
 *  the structural surface; the law is a theorem about specific carriers
 *  and specific values.
 */
template <typename M>
concept IsOrthogonalMatrixCarrier =
    HasMatrixMultiplication<M> && HasInvolutiveTranspose<M>;

/** @section contracts__Shape-conformant operations.
 *
 *  Matrix addition requires the two operands to share row count, column
 *  count, and scalar type. Matrix multiplication requires the inner
 *  dimensions to agree: an `m × k` matrix composed with a `k × n` matrix
 *  produces an `m × n` result. These predicates express the ideal type-
 *  checking rule: you cannot add or multiply two matrices whose shapes are
 *  incompatible — the expression should not even compile.
 *
 *  For the current `Matrix2x2V<T>` the rule is enforced structurally (the
 *  operators are signed with a fixed shape), but expressing it as a concept
 *  generalises cleanly to future `MatrixMxN<T, M, N>` carriers and lets
 *  the rule be asserted at use sites.
 */

/**
 * @concept MatchesAdditiveShape
 * @brief Two matrix-like carriers can be added iff they share shape and
 *        scalar type.
 */
template <typename A, typename B>
concept MatchesAdditiveShape =
    HasMatrixShape<A> && HasMatrixShape<B> && requires {
      typename A::scalar_type;
      typename B::scalar_type;
      requires std::same_as<typename A::scalar_type, typename B::scalar_type>;
      requires A::row_count == B::row_count;
      requires A::column_count == B::column_count;
    };

/**
 * @concept MatchesMultiplicativeShape
 * @brief Two matrix-like carriers can be composed iff the inner dimensions
 *        agree: `A::column_count == B::row_count` and the scalar types
 *        match.
 */
template <typename A, typename B>
concept MatchesMultiplicativeShape =
    HasMatrixShape<A> && HasMatrixShape<B> && requires {
      typename A::scalar_type;
      typename B::scalar_type;
      requires std::same_as<typename A::scalar_type, typename B::scalar_type>;
      requires A::column_count == B::row_count;
    };

/**
 * @concept HasConformingMatrixAddition
 * @brief `A` and `B` support shape-conformant addition, and the result has
 *        the same shape and scalar type.
 */
template <typename A, typename B>
concept HasConformingMatrixAddition =
    MatchesAdditiveShape<A, B> && requires(const A& a, const B& b) {
      { a + b } -> HasMatrixShape;
    };

/**
 * @concept HasConformingMatrixMultiplication
 * @brief `A` and `B` support shape-conformant multiplication, and the
 *        result has shape `A::row_count × B::column_count`.
 */
template <typename A, typename B>
concept HasConformingMatrixMultiplication =
    MatchesMultiplicativeShape<A, B> && requires(const A& a, const B& b) {
      { a * b } -> HasMatrixShape;
    };

/**
 * @concept IsMatrixAlgebra
 * @brief Umbrella concept bundling all 9 matrix slogans for square matrices
 *        over a field.
 *
 *  Stacking: `IsMatrix<M>` (shape + column/row types + decompositions) plus
 *  `IsMatrixOverFieldRingLike<M, F>` (ring-like operations plus transpose).
 *  Invertibility is *not* required — an individual square matrix can be
 *  singular and still belong to the ring of matrices.
 */
template <typename M, typename F>
concept IsMatrixAlgebra = IsMatrix<M> && IsMatrixOverFieldRingLike<M, F>;

}  // namespace dedekind::linear_algebra
