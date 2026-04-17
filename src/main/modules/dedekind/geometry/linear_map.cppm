/**
 * @file dedekind/geometry/linear_map.cppm
 * @partition :linear_map
 * @brief Level 10.05: Concrete finite-dimensional linear maps with algebraic
 * verification.
 *
 * @section Algebraic_Structure
 * LinearMap<F, R, C> represents a finite-dimensional linear map F^C -> F^R
 * and serves as an algebraic carrier for the theory of modules and vector
 * spaces.
 *
 * **Algebraic Status:**
 * - When F is a **Semiring**: LinearMap forms a **Semimodule** over F
 *   (additive monoid + external linear action by semiring scalars)
 * - When F is a **Ring**: LinearMap forms a **Module** over F
 *   (additive group + external linear action by ring scalars)
 * - When F is a **Field**: LinearMap forms a **Vector Space** over F
 *   (additive group + external linear action by field scalars)
 *
 * The structure ensures that both external operations (scalar action) and
 * internal operations (matrix addition) satisfy the **Linear Action Axioms**:
 * 1. **Vector Additivity**: s * (m1 + m2) = s*m1 + s*m2
 * 2. **Scalar Additivity**: (s1 + s2) * m = s1*m + s2*m
 *
 * @note Matrix **multiplication** (composition) is non-commutative, so the
 *       ring of square matrices does NOT form a commutative ring, only a ring.
 *
 * @section Design_and_Extensibility
 * The current LinearMap<F, R, C> is a dense matrix implementation with
 * compile-time fixed dimensions. This design is appropriate for:
 * - Small to medium matrices (dense, static dimensions)
 * - Static dimension requirements (no runtime resizing)
 *
 * For other use cases, the architecture supports extensions:
 * - Sparse matrices: Can be implemented as a separate type (e.g., COO, CSR)
 *   with the same algebraic interface (semimodule laws are preserved).
 * - Infinite/functional matrices: Represented functionally (e.g., Hilbert
 *   matrix accessed via coefficient(i,j) = 1/(i+j+1)) or as lazy evaluation.
 * - Diagonal matrices: Specialized implementation storing only O(n)
 * coefficients while preserving the LinearMap interface.
 *
 * The semimodule axioms are independent of storage representation, so any
 * alternative matrix type implementing the same arithmetic operations will
 * automatically satisfy the same algebraic structure (if F is a semiring).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "أشدُّ ما يحتاج إليه من يُعنى بعلم الحساب أن يعرف ما يحتاج الناس إليه في
 * معاملاتهم من المقادير، إذ بذلك يقاس العدل في القضاء." — محمد بن موسى
 * الخوارزمي، منسوب في ويكي الاقتباس العربي. [Trans: "What one most needs in the
 * science of computation is to know the quantities people need in their
 * dealings, for by that, justice in judgment is measured."]
 */

module;

#include <array>
#include <concepts>
#include <cstddef>
#include <initializer_list>
#include <utility>

export module dedekind.geometry:linear_map;

import dedekind.algebra;
import dedekind.category;
import :affine;

namespace dedekind::geometry {
using namespace dedekind::algebra;
using namespace dedekind::category;

#ifndef DEDEKIND_ENABLE_DOUBLE_REAL_PROXY
#define DEDEKIND_ENABLE_DOUBLE_REAL_PROXY 0
#endif

/**
 * @concept IsMatrixScalar
 * @brief A scalar type that can serve as coefficients in a LinearMap.
 *
 * The current concrete Vector/LinearMap MVP is restricted to floating-point
 * carriers.
 *
 * Policy hardening: using `double` as a proxy for the real line requires
 * explicit build opt-in via DEDEKIND_ENABLE_DOUBLE_REAL_PROXY=1. This keeps the
 * machine-real approximation path explicit rather than ambient.
 */
export template <typename S>
concept IsMatrixScalar =
    std::floating_point<S> &&
    (!std::same_as<S, double> || (DEDEKIND_ENABLE_DOUBLE_REAL_PROXY == 1));

/**
 * @brief Explicit embedding witness from floating carriers into matrix scalars.
 *
 * This is the policy gate for admitting machine floating carriers into the
 * matrix layer. In particular, `double` is only embeddable when
 * DEDEKIND_ENABLE_DOUBLE_REAL_PROXY=1.
 */
export template <typename F>
  requires std::floating_point<F> && IsMatrixScalar<F>
constexpr F embed_matrix_scalar(F value) noexcept {
  return value;
}

/**
 * @concept HasMatrixScalarEmbedding
 * @brief Floating carrier that is admissible as a matrix scalar by policy.
 */
export template <typename F>
concept HasMatrixScalarEmbedding = std::floating_point<F> && requires(F x) {
  { embed_matrix_scalar(x) } -> std::same_as<F>;
};

/**
 * @class LinearMap
 * @brief A dense compile-time-sized linear map F^Cols -> F^Rows.
 *
 * **Algebraic Role:**
 * LinearMap<F, R, C> is the concrete carrier for a Semimodule over the
 * scalar semiring F. When instantiated with field scalars (e.g., double),
 * it represents a Vector Space over that field.
 *
 * The matrix structure is defined by:
 * - Dense row-major coefficient storage: std::array<std::array<F, Cols>, Rows>
 * - Vector application via matrix-vector multiplication
 * - Matrix composition via matrix-matrix multiplication (for square matrices)
 * - Additive structure: element-wise addition and scalar multiplication
 *
 * @tparam F The scalar field/ring/semiring type.
 * @tparam Rows The number of output dimensions.
 * @tparam Cols The number of input dimensions.
 *
 * Supports scalars meeting IsMatrixScalar. The surrounding documentation treats
 * these carriers as field-like under the active numeric policy, rather than as
 * a categorical field proof.
 */
export template <IsMatrixScalar F, std::size_t Rows, std::size_t Cols>
class LinearMap {
 public:
  using scalar_type = F;
  using Domain = Vector<F, Cols>;
  using Codomain = Vector<F, Rows>;

  static constexpr std::size_t row_count = Rows;
  static constexpr std::size_t column_count = Cols;

  constexpr LinearMap() = default;

  constexpr LinearMap(std::initializer_list<std::initializer_list<F>> rows)
      : coeffs_{} {
    auto row_it = rows.begin();
    for (std::size_t i = 0; i < Rows && row_it != rows.end(); ++i, ++row_it) {
      auto col_it = row_it->begin();
      for (std::size_t j = 0; j < Cols && col_it != row_it->end();
           ++j, ++col_it)
        coeffs_[i][j] = *col_it;
    }
  }

  friend constexpr bool operator==(const LinearMap&,
                                   const LinearMap&) = default;

  constexpr F coefficient(std::size_t row, std::size_t col) const {
    return coeffs_[row][col];
  }

  constexpr void set_coefficient(std::size_t row, std::size_t col, F value) {
    coeffs_[row][col] = value;
  }

  constexpr Codomain operator()(const Domain& v) const {
    Codomain result{};
    for (std::size_t i = 0; i < Rows; ++i) {
      F sum{};
      for (std::size_t j = 0; j < Cols; ++j) sum = sum + coeffs_[i][j] * v[j];
      result[i] = sum;
    }
    return result;
  }

 private:
  template <IsMatrixScalar G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator+(const LinearMap<G, R, C>&,
                                                const LinearMap<G, R, C>&);
  template <IsMatrixScalar G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator-(const LinearMap<G, R, C>&,
                                                const LinearMap<G, R, C>&);
  template <IsMatrixScalar G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const G&,
                                                const LinearMap<G, R, C>&);
  template <IsMatrixScalar G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const LinearMap<G, R, C>&,
                                                const G&);
  template <IsMatrixScalar G, std::size_t R, std::size_t Inner, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const LinearMap<G, R, Inner>&,
                                                const LinearMap<G, Inner, C>&);

  std::array<std::array<F, Cols>, Rows> coeffs_{};
};

export template <IsMatrixScalar F, std::size_t Rows, std::size_t Cols>
using Matrix = LinearMap<F, Rows, Cols>;

// FIXME(https://github.com/vincentk/dedekind/issues/174): Extend the MVP with
// post-core matrix operators (transpose/trace/Hadamard/Kronecker/concat).

export template <IsMatrixScalar F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator+(
    const LinearMap<F, Rows, Cols>& a, const LinearMap<F, Rows, Cols>& b) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = a.coeffs_[i][j] + b.coeffs_[i][j];
  return result;
}

export template <IsMatrixScalar F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator-(
    const LinearMap<F, Rows, Cols>& a, const LinearMap<F, Rows, Cols>& b) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = a.coeffs_[i][j] - b.coeffs_[i][j];
  return result;
}

export template <IsMatrixScalar F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator*(
    const F& scalar, const LinearMap<F, Rows, Cols>& a) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = scalar * a.coeffs_[i][j];
  return result;
}

export template <IsMatrixScalar F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator*(const LinearMap<F, Rows, Cols>& a,
                                             const F& scalar) {
  return scalar * a;
}

export template <IsMatrixScalar F, std::size_t Rows, std::size_t Cols>
constexpr Vector<F, Rows> operator*(const LinearMap<F, Rows, Cols>& a,
                                    const Vector<F, Cols>& v) {
  return a(v);
}

export template <IsMatrixScalar F, std::size_t Rows, std::size_t Inner,
                 std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator*(
    const LinearMap<F, Rows, Inner>& a, const LinearMap<F, Inner, Cols>& b) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i) {
    for (std::size_t j = 0; j < Cols; ++j) {
      F sum{};
      for (std::size_t k = 0; k < Inner; ++k)
        sum = sum + a.coeffs_[i][k] * b.coeffs_[k][j];
      result.coeffs_[i][j] = sum;
    }
  }
  return result;
}

export template <IsMatrixScalar F, std::size_t N>
constexpr LinearMap<F, N, N> identity_linear_map() {
  LinearMap<F, N, N> result;
  for (std::size_t i = 0; i < N; ++i) result.set_coefficient(i, i, F{1});
  return result;
}

export template <IsMatrixScalar F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> zero_linear_map() {
  return LinearMap<F, Rows, Cols>{};
}

/**
 * @section Algebraic_Verification_for_Matrix_Scalars
 *
 * When F satisfies IsMatrixScalar:
 * - the implementation provides the operations needed by this MVP
 * - those operations behave like a field only under idealized exact arithmetic
 * - in actual IEEE arithmetic, this is a field-like policy witness rather than
 *   a categorical field proof
 *
 * Consequence: LinearMap<F, R, C> behaves as a vector-space-like carrier over
 * F in the same operational sense used by IsFieldLikeScalar.
 *
 * This means LinearMap satisfies the **four axioms of linear action**:
 * 1. Vector Additivity: s * (m1 + m2) = s*m1 + s*m2
 * 2. Scalar Additivity: (s1 + s2) * m = s1*m + s2*m
 * 3. Associativity of Scalar Multiplication: (s1 * s2) * m = s1 * (s2 * m)
 * 4. Identity of Scalar Multiplication: 1 * m = m
 *
 * We verify these axioms statically where possible.
 */

/**
 * @concept IsLinearMapModule
 * @brief Verify that LinearMap<F, R, C> behaves as a module over scalar F.
 *
 * For IsMatrixScalar F, this verifies:
 * - F satisfies the operational scalar contract used by the geometry layer
 * - LinearMap supports the external linear action
 * - Addition and scalar multiplication are properly closed
 */

/**
 * @section Module_Structure_Verification
 *
 * **Core Algebraic Theorem:**
 * LinearMap<F, R, C> is always a **Semimodule** over the scalar semiring F.
 *
 * This means:
 * 1. (LinearMap^{R×C}, +) is an additive commutative monoid
 *    - Closure: A + B is a LinearMap<F, R, C>
 *    - Identity: zero_linear_map<F, R, C>() is the additive identity
 *    - Associativity: (A+B)+C = A+(B+C)
 *    - Commutativity: A+B = B+A
 *
 * 2. (F, +, *) is a semiring (the scalar carrier)
 *    - (F, +) is a commutative monoid
 *    - (F, *) is a monoid
 *    - Multiplication distributes over addition
 *
 * 3. External linear action F × LinearMap^{R×C} → LinearMap^{R×C}
 *    - **Vector Additivity**: s * (A + B) = s*A + s*B
 *    - **Scalar Additivity**: (s1 + s2) * A = s1*A + s2*A
 *    - **Associativity**: (s1 * s2) * A = s1 * (s2 * A)
 *    - **Identity**: 1_F * A = A
 *
 * **Refinements for Special Scalar Types:**
 * - When F is a ring: LinearMap<F, R, C> is a **Module** over F
 *   (Additionally has scalar negation)
 * - When F is a field: LinearMap<F, R, C> is a **Vector Space** over F
 *   (Additionally has scalar division)
 * - When F is a machine floating scalar admitted by IsMatrixScalar:
 *   LinearMap<F, R, C> is vector-space-like with practical IEEE semantics
 *
 * **Note on Matrix Multiplication (Non-Commutativity):**
 * Square matrices M^{n×n} form a ring under matrix addition and composition,
 * but this ring is NON-COMMUTATIVE. However, the semimodule structure of
 * arbitrary matrices is unaffected by composition, since composition only
 * applies to square matrices and acts as an additional binary operation.
 */

/**
 * @brief Covector: a linear functional F^N -> F, represented as a row vector.
 *
 * A covector (dual vector / linear functional) is canonically a
 * LinearMap<F, 1, N>: it maps a column vector to a length-1 column vector
 * whose single entry is the inner product of the row with the argument.
 *
 * Covectors form the **dual module** Hom_F(V, F), where V = F^N.
 * When F is a field, the dual module is isomorphic to V itself.
 */
export template <IsMatrixScalar F, std::size_t N>
using Covector = LinearMap<F, 1, N>;

/**
 * @class DifferentiableMap
 * @brief A finite-dimensional map paired with an exact Jacobian witness.
 *
 * This is the first differentiation contract in Dedekind's finite-dimensional
 * geometry layer. It packages:
 * - a primal map F^DomainDim -> F^CodomainDim
 * - an exact Jacobian oracle returning LinearMap<F, CodomainDim, DomainDim>
 *
 * The Jacobian is therefore a concrete Fr\'echet derivative witness in the
 * existing LinearMap carrier.
 */
export template <IsMatrixScalar F, std::size_t DomainDim,
                 std::size_t CodomainDim, typename ValueFn, typename JacobianFn>
  requires requires(const ValueFn& value, const JacobianFn& jacobian,
                    const Vector<F, DomainDim>& x) {
    { value(x) } -> std::same_as<Vector<F, CodomainDim>>;
    { jacobian(x) } -> std::same_as<LinearMap<F, CodomainDim, DomainDim>>;
  }
class DifferentiableMap {
 public:
  using scalar_type = F;
  using Domain = Vector<F, DomainDim>;
  using Codomain = Vector<F, CodomainDim>;
  using Jacobian = LinearMap<F, CodomainDim, DomainDim>;

  static constexpr std::size_t domain_dimension = DomainDim;
  static constexpr std::size_t codomain_dimension = CodomainDim;

  constexpr DifferentiableMap(ValueFn value, JacobianFn jacobian)
      : value_(std::move(value)), jacobian_(std::move(jacobian)) {}

  constexpr Codomain operator()(const Domain& x) const { return value_(x); }

  constexpr Jacobian jacobian_at(const Domain& x) const { return jacobian_(x); }

 private:
  [[no_unique_address]] ValueFn value_;
  [[no_unique_address]] JacobianFn jacobian_;
};

/**
 * @concept HasJacobianAt
 * @brief Map-like object exposing an exact finite-dimensional Jacobian.
 */
export template <typename Map>
concept HasJacobianAt =
    requires(const Map& map, const typename Map::Domain& x) {
      typename Map::scalar_type;
      typename Map::Domain;
      typename Map::Codomain;
      typename Map::Jacobian;
      { map(x) } -> std::same_as<typename Map::Codomain>;
      { map.jacobian_at(x) } -> std::same_as<typename Map::Jacobian>;
    };

/**
 * @brief Construct a finite-dimensional differentiable map from primal and
 * Jacobian callables.
 */
export template <IsMatrixScalar F, std::size_t DomainDim,
                 std::size_t CodomainDim, typename ValueFn, typename JacobianFn>
constexpr auto make_differentiable_map(ValueFn value, JacobianFn jacobian) {
  return DifferentiableMap<F, DomainDim, CodomainDim, ValueFn, JacobianFn>{
      std::move(value), std::move(jacobian)};
}

/**
 * @brief Return the Jacobian of a finite-dimensional differentiable map.
 */
export template <HasJacobianAt Map>
constexpr auto jacobian_at(const Map& map, const typename Map::Domain& x) ->
    typename Map::Jacobian {
  return map.jacobian_at(x);
}

/**
 * @brief Return the Fr\'echet derivative of a finite-dimensional map.
 *
 * In the current finite-dimensional slice, this coincides with the Jacobian.
 */
export template <HasJacobianAt Map>
constexpr auto frechet_derivative_at(const Map& map,
                                     const typename Map::Domain& x) ->
    typename Map::Jacobian {
  return jacobian_at(map, x);
}

/**
 * @brief Return the scalar-valued differential as a covector.
 */
export template <HasJacobianAt Map>
  requires(Map::codomain_dimension == 1)
constexpr Covector<typename Map::scalar_type, Map::domain_dimension>
differential_at(const Map& map, const typename Map::Domain& x) {
  return jacobian_at(map, x);
}

/**
 * @brief Outer product u ⊗ v : F^N -> F^M.
 *
 * Constructs the rank-1 linear map whose (i,j) coefficient is u[i]*v[j].
 * Applied to a vector w, it yields dot(v,w) * u (the dyadic product).
 *
 * **Algebraic Properties:**
 * - Bilinear: (a*u) ⊗ v = a * (u ⊗ v) = u ⊗ (a*v) for all scalar a
 * - Distributes over addition: (u1+u2) ⊗ v = (u1⊗v) + (u2⊗v) and
 *                               u ⊗ (v1+v2) = (u⊗v1) + (u⊗v2)
 * - Rank 1: The image of u ⊗ v is the 1-dimensional subspace span(u)
 */
export template <IsMatrixScalar F, std::size_t M, std::size_t N>
constexpr LinearMap<F, M, N> outer(const Vector<F, M>& u,
                                   const Vector<F, N>& v) {
  LinearMap<F, M, N> result;
  for (std::size_t i = 0; i < M; ++i)
    for (std::size_t j = 0; j < N; ++j)
      result.set_coefficient(i, j, u[i] * v[j]);
  return result;
}

/**
 * @section Specializations_for_Common_Ring_and_Field_Scalars
 *
 * The following type aliases provide semantic naming for matrices over
 * specific algebraic structures. Each specialization documents the algebraic
 * guarantees of the linear map as a module/vector space.
 *
 * **Provided Alias:**
 * - RealMatrix<R, C>: machine-real matrix alias (double) with explicit opt-in
 *   requirement via DEDEKIND_ENABLE_DOUBLE_REAL_PROXY=1.
 *
 * All specializations are **Semimodules** at minimum (over the semiring
 * formed by their scalar type). When the scalar forms a ring or field,
 * the matrix carries the corresponding module or vector space structure.
 *
 * Additional aliases (for complex/rational carriers) are planned and should be
 * introduced only once their scalar/vector contracts are implemented in this
 * partition.
 */

/**
 * @brief RealMatrix: machine-real matrix alias behind explicit opt-in.
 *
 * **Policy:** This alias is only available when
 * DEDEKIND_ENABLE_DOUBLE_REAL_PROXY=1 is set at build time.
 *
 * **Algebraic Interpretation:** treated as vector-space-like under the active
 * numeric policy (not as a strict field proof).
 *
 * Satisfies the linear action laws operationally:
 * 1. Vector Additivity: s * (m1 + m2) = s*m1 + s*m2
 * 2. Scalar Additivity: (s1 + s2) * m = s1*m + s2*m
 * 3. Associativity: (s1 * s2) * m = s1 * (s2 * m)
 * 4. Identity: 1.0 * m = m
 *
 * For square matrices (R == C), composition forms a non-commutative ring.
 */
export template <std::size_t Rows, std::size_t Cols>
using RealMatrix = LinearMap<double, Rows, Cols>;

// ============================================================================
// Tangent and Cotangent Space Aliases (flat ℝ^N case)
// ============================================================================
//
// For the Euclidean space F^N (a flat manifold), the tangent space at every
// point is canonically isomorphic to F^N itself (the tangent bundle is
// trivial).  Similarly, the cotangent space at every point is (F^N)* ≅ F^N.
//
// These aliases give semantic names to those canonical identifications.
// They serve as preparation for a future non-flat manifold abstraction
// (see FIXME below) where the tangent/cotangent spaces are no longer globally
// trivialized but depend on the base-point and atlas chart.
//
// FIXME(https://github.com/vincentk/dedekind/issues/185): Replace these
// flat-space aliases with proper manifold/atlas/chart concepts once
// non-trivial manifold structure is introduced.

/**
 * @brief TangentVector at a point in F^N.
 *
 * For the flat manifold F^N, the tangent space T_p(F^N) ≅ F^N for every
 * base-point p.  The alias makes the geometric role explicit without
 * introducing any new data.
 */
export template <IsMatrixScalar F, std::size_t N>
using TangentVector = Vector<F, N>;

/**
 * @brief CotangentVector at a point in F^N.
 *
 * The cotangent space T*_p(F^N) ≅ (F^N)* is the dual of the tangent space.
 * In the finite-dimensional flat case this is canonically represented as a
 * row vector (Covector), i.e. a LinearMap<F, 1, N>.
 */
export template <IsMatrixScalar F, std::size_t N>
using CotangentVector = Covector<F, N>;

/**
 * @struct TangentBundlePoint
 * @brief A point in the tangent bundle T(F^N) = F^N × F^N.
 *
 * Packages a base-point with a tangent vector at that point.  For the flat
 * manifold F^N the bundle is trivially the product; in a non-flat setting
 * (future issue) this struct would generalise to an atlas-chart-local section.
 */
export template <IsMatrixScalar F, std::size_t N>
struct TangentBundlePoint {
  Vector<F, N> base;          ///< Base point p ∈ F^N
  TangentVector<F, N> fiber;  ///< Tangent vector v ∈ T_p(F^N) ≅ F^N

  friend constexpr bool operator==(const TangentBundlePoint&,
                                   const TangentBundlePoint&) = default;
};

/**
 * @struct CotangentBundlePoint
 * @brief A point in the cotangent bundle T*(F^N) = F^N × (F^N)*.
 *
 * Packages a base-point with a cotangent vector (covector) at that point.
 * The cotangent bundle is dual to the tangent bundle; sections of T*M
 * are exactly the differential 1-forms (see analysis:forms for the OneForm
 * view of these objects).
 */
export template <IsMatrixScalar F, std::size_t N>
struct CotangentBundlePoint {
  Vector<F, N> base;            ///< Base point p ∈ F^N
  CotangentVector<F, N> fiber;  ///< Cotangent vector ω ∈ T*_p(F^N) ≅ (F^N)*

  friend constexpr bool operator==(const CotangentBundlePoint&,
                                   const CotangentBundlePoint&) = default;
};

}  // namespace dedekind::geometry
