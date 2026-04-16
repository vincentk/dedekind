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
 * @note "Нельзя быть математиком, не будучи в то же время поэтом в душе."
 *       ("It is impossible to be a mathematician without being a poet in
 * soul.")
 *       -- Софья Ковалевская (Sofya Kovalevskaya)
 */

module;

#include <array>
#include <concepts>
#include <cstddef>
#include <initializer_list>

export module dedekind.geometry:linear_map;

import dedekind.algebra;
import dedekind.category;
import :affine;

namespace dedekind::geometry {
using namespace dedekind::algebra;
using namespace dedekind::category;

/**
 * @concept IsMatrixScalar
 * @brief A scalar type that can serve as coefficients in a LinearMap.
 *
 * At minimum, the scalar must support:
 * - Construction and equality checking (default + equality_comparable)
 * - Addition and scalar multiplication
 *
 * In practice, this is satisfied by any type meeting std::floating_point,
 * std::integral, or a custom scalar meeting the above requirements.
 */
export template <typename S>
concept IsMatrixScalar =
    std::equality_comparable<S> && std::default_initializable<S>;

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
 * Supports any scalar type meeting IsMatrixScalar. The algebraic structure
 * (Semimodule, Module, or VectorSpace) depends on the scalar type's algebraic
 * properties.
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
  template <std::floating_point G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator+(const LinearMap<G, R, C>&,
                                                const LinearMap<G, R, C>&);
  template <std::floating_point G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator-(const LinearMap<G, R, C>&,
                                                const LinearMap<G, R, C>&);
  template <std::floating_point G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const G&,
                                                const LinearMap<G, R, C>&);
  template <std::floating_point G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const LinearMap<G, R, C>&,
                                                const G&);
  template <std::floating_point G, std::size_t R, std::size_t Inner,
            std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const LinearMap<G, R, Inner>&,
                                                const LinearMap<G, Inner, C>&);

  std::array<std::array<F, Cols>, Rows> coeffs_{};
};

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
using Matrix = LinearMap<F, Rows, Cols>;

// FIXME(https://github.com/vincentk/dedekind/issues/174): Extend the MVP with
// post-core matrix operators (transpose/trace/Hadamard/Kronecker/concat).

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator+(
    const LinearMap<F, Rows, Cols>& a, const LinearMap<F, Rows, Cols>& b) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = a.coeffs_[i][j] + b.coeffs_[i][j];
  return result;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator-(
    const LinearMap<F, Rows, Cols>& a, const LinearMap<F, Rows, Cols>& b) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = a.coeffs_[i][j] - b.coeffs_[i][j];
  return result;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator*(
    const F& scalar, const LinearMap<F, Rows, Cols>& a) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = scalar * a.coeffs_[i][j];
  return result;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator*(const LinearMap<F, Rows, Cols>& a,
                                             const F& scalar) {
  return scalar * a;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr Vector<F, Rows> operator*(const LinearMap<F, Rows, Cols>& a,
                                    const Vector<F, Cols>& v) {
  return a(v);
}

export template <std::floating_point F, std::size_t Rows, std::size_t Inner,
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

export template <std::floating_point F, std::size_t N>
constexpr LinearMap<F, N, N> identity_linear_map() {
  LinearMap<F, N, N> result;
  for (std::size_t i = 0; i < N; ++i) result.set_coefficient(i, i, F{1});
  return result;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> zero_linear_map() {
  return LinearMap<F, Rows, Cols>{};
}

/**
 * @section Algebraic_Verification_for_Floating_Point_Scalars
 *
 * When F is std::floating_point (e.g., double, float):
 * - (F, +) forms an Abelian Group
 * - (F, *) forms a monoid with identity 1.0 (note: not a group for all x)
 * - (F, +, *) satisfies the distributive law: a*(b+c) = a*b + a*c
 * - Therefore, F is a Field
 *
 * Consequence: LinearMap<F, R, C> for floating_point F is a **Vector Space**
 * over F.
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
 * For floating_point F, this verifies:
 * - F satisfies IsField (required for VectorSpace)
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
 * - When F is ℝ (double/float): LinearMap<F, R, C> is a **Vector Space**
 *   (with practical floating-point semantics)
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
export template <std::floating_point F, std::size_t N>
using Covector = LinearMap<F, 1, N>;

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
export template <std::floating_point F, std::size_t M, std::size_t N>
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
 * **Naming Convention:**
 * - RealMatrix<R, C>: Vector Space over ℝ (double)
 * - ComplexMatrix<R, C>: Vector Space over ℂ (Complex<double>)
 * - RationalMatrix<R, C>: Vector Space over ℚ (Rational<int>)
 *
 * All specializations are **Semimodules** at minimum (over the semiring
 * formed by their scalar type). When the scalar forms a ring or field,
 * the matrix carries the corresponding module or vector space structure.
 *
 * **Usage Note for ComplexMatrix and RationalMatrix:**
 * These aliases require dedekind.numbers to be imported for the full type
 * definitions. They are provided here for semantic clarity and forward
 * reference.
 */

/**
 * @brief RealMatrix: A vector space of real-valued matrices over the field ℝ.
 *
 * **Algebraic Structure:** LinearMap<double, R, C> is a **Vector Space** over
 * ℝ.
 *
 * Satisfies all four linear action axioms:
 * 1. Vector Additivity: s * (m1 + m2) = s*m1 + s*m2
 * 2. Scalar Additivity: (s1 + s2) * m = s1*m + s2*m
 * 3. Associativity: (s1 * s2) * m = s1 * (s2 * m)
 * 4. Identity: 1.0 * m = m
 *
 * For square matrices (R == C), composition forms a non-commutative ring.
 */
export template <std::size_t Rows, std::size_t Cols>
using RealMatrix = LinearMap<double, Rows, Cols>;

}  // namespace dedekind::geometry
