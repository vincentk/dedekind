/**
 * @file dedekind/linear_algebra/matnxn.cppm
 * @partition :matnxn
 * @brief The N×N matrix over a @b semiring — @c Mat(S), the higher-rank,
 *        semiring-generic generalisation of @c :mat2x2's @c Matrix2x2V<T>.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section matnxn__Scope
 *
 * @c Matrix2x2V<T> (@c :mat2x2) is @b ring-native: it is fixed at 2×2 and
 * consumes @c T's own @c operator+ / @c operator* as @c ⊕ / @c ⊗.  That
 * shape does not survive the tropical carriers: in @c
 * dedekind::algebra::Tropical
 * @c operator+ is @b ⊗ (honest addition) and @c ⊕ is a @b separate functor
 * (@c TropicalPlus), so native operators route the wrong monoids.  The
 * fix is the same one @c :transfer already uses — read @c ⊕ / @c ⊗ off
 * @c dedekind::algebra::semiring_ops<S> — lifted to a first-class
 * carrier.
 *
 * @c MatNxNV<S,N> is therefore the double generalisation
 * (2×2 → N×N, ring → semiring) that @c :matrix reserves the slot for:
 * @c Mat(S), the @b Kleene-algebra-of-matrices (Conway/Kozen).  Its two
 * monoids @ref MatPlus (elementwise @c ⊕) and @ref MatTimes (the @c ⊕-⊗
 * contraction) are registered so that @c Mat(S) is itself an
 * @c IsSemiring whenever @c S is — the existential proof that the functor
 * @c Mat : Semiring → Semiring is inhabited.  @c :transfer's @c star is
 * then just this semiring's @c *-closure.
 */
module;

#include <array>
#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>

export module dedekind.linear_algebra:matnxn;

import dedekind.algebra;  // semiring_ops, IsSemiring, MaxPlus (witness)
import dedekind.category; // IsSemiring, identity_v, identity_registry, traits
import dedekind.sets;     // Finite (dimension tag)
import :contracts;        // ColumnOrientation/RowOrientation, IsMatrix,
                          // IsColumnVector

namespace dedekind::linear_algebra {

template <typename S, std::size_t N>
struct MatPlus;
template <typename S, std::size_t N>
struct MatTimes;

/**
 * @brief An N-entry @b semimodule vector over the semiring @c S, read as an
 *        index→scalar arrow.  @c IsColumnVector / @c IsCovector by the default
 *        (semimodule) contract: @c ⊕ and the two-sided scalar @c ⊗ action, but
 *        @b no negation (a dioid has none).  @ref Ket (@c ColumnOrientation,
 *        @c |v⟩) and @ref Bra (@c RowOrientation, @c ⟨w|) are its two
 *        orientations --- the bra·ket language of @c :transfer made first-class
 *        carriers, the "matrix column/row @b is a vector" slogan for a
 * semiring. One carrier, orientation as a template parameter (the tag is the @b
 *        only difference between a column and a row).
 */
export template <typename S, std::size_t N, typename Orientation>
struct SemimoduleVec {
  using scalar_type = S;
  using orientation = Orientation;
  using dimension_type = dedekind::sets::Finite;
  static constexpr std::size_t dimension = N;
  /// @brief A vector @b is an index→scalar arrow, so @c Ket / @c Bra are
  ///        @c IsArrow --- they feed @c :transfer's @c inner_product /
  ///        @c OuterProduct directly, retiring the ad-hoc bra/ket structs.
  using Domain = std::size_t;
  using Codomain = S;

  std::array<S, N> c{};

  /// @brief The additive monoid @c (·, ⊕) @b inherits the scalar's laws (§4
  ///        property distribution): the elementwise @c ⊕ is associative /
  ///        commutative exactly when the base @c ⊕ is, and @b only for the
  ///        vector's own @c std::plus --- an arbitrary @c Op is not certified.
  ///        Totality distributes separately (@c is_saturating below).
  template <typename Op>
  static constexpr bool is_associative_v =
      std::same_as<Op, std::plus<SemimoduleVec>> &&
      dedekind::category::is_associative_v<
          S, typename dedekind::algebra::semiring_ops<S>::add>;
  template <typename Op>
  static constexpr bool is_commutative_v =
      std::same_as<Op, std::plus<SemimoduleVec>> &&
      dedekind::category::is_commutative_v<
          S, typename dedekind::algebra::semiring_ops<S>::add>;

  constexpr S operator()(std::size_t i) const {
    return c[i];
  }  // index → scalar
  constexpr S operator[](std::size_t i) const { return c[i]; }
  friend constexpr bool operator==(const SemimoduleVec&,
                                   const SemimoduleVec&) = default;

  friend constexpr SemimoduleVec operator+(const SemimoduleVec& a,
                                           const SemimoduleVec& b) {
    using Add = typename dedekind::algebra::semiring_ops<S>::add;
    SemimoduleVec r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Add{}(a.c[i], b.c[i]);
    return r;
  }
  friend constexpr SemimoduleVec operator*(const S& s, const SemimoduleVec& a) {
    using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
    SemimoduleVec r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Mult{}(s, a.c[i]);
    return r;
  }
  friend constexpr SemimoduleVec operator*(const SemimoduleVec& a, const S& s) {
    using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
    SemimoduleVec r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Mult{}(a.c[i], s);
    return r;
  }
};

/** @brief @c |v⟩: a column of @c Mat(S) (a @c ColumnOrientation vector). */
export template <typename S, std::size_t N>
using Ket = SemimoduleVec<S, N, ColumnOrientation>;
/** @brief @c ⟨w|: a row of @c Mat(S) (a @c RowOrientation covector). */
export template <typename S, std::size_t N>
using Bra = SemimoduleVec<S, N, RowOrientation>;

/**
 * @brief The @b dagger of a 1-tensor: transpose flips the orientation
 *        (@c Ket ↔ @c Bra), keeping the components --- so
 *        @f$\mathrm{dagger}(|v\rangle)=\langle v|@f$ and
 *        @f$\mathrm{dagger}(\langle w|)=|w\rangle@f$
 *        (@f$(n\times1)^{\top}=1\times n@f$).
 *
 * @details The vector face of the one dagger documented on
 * @c MatNxNV::transpose (see @c dedekind::category::IsDagger).  It aligns with
 * the matrix transpose: @f$(M|v\rangle)^{\top}=\langle v|M^{\top}@f$ --- the
 * dagger reverses every arrow, turning the operator @c Mat and its @c Ket into
 * a @c Bra and the transposed operator.  Over a general semiring there is no
 * conjugation, so this transpose @b is the dagger (real / Boolean / tropical);
 * over @c ℂ the true adjoint additionally conjugates the components
 * (FIXME(#787): a @c conj-aware overload).  Involutive:
 * @c transpose(transpose(v)) @c == @c v.
 */
export template <typename S, std::size_t N, typename O>
  requires(IsColumnVector<SemimoduleVec<S, N, O>> ||
           IsCovector<SemimoduleVec<S, N, O>>)
constexpr SemimoduleVec<S, N, dual_orientation_t<O>> transpose(
    const SemimoduleVec<S, N, O>& v) {
  return SemimoduleVec<S, N, dual_orientation_t<O>>{v.c};
}

// @note NO bare @c dagger(v) alias.  A @b dagger is relative to a @b (type,
//       operation): a carrier can carry several (over @c ℂ: unary negation,
//       reciprocal, complex conjugation; and, reading a scalar as a 1×1 matrix,
//       the trivial transpose).  So "the dagger" is @c Dagger{}(x) for a @b
//       chosen involution functor (@c category::IsDagger), not a single global
//       name.  @c transpose above IS the coordinate-swap dagger --- certified
//       @c is_involutive as @c TransposeF (the real / semiring adjoint, no
//       conjugation) --- but it is one dagger among several, so it keeps its
//       honest linear-algebra name.  ℂ's conjugate-transpose is a @b different
//       @c Dagger functor (FIXME(#787)).

/**
 * @brief @c Mat(S): the N×N matrix over a semiring @c S.  Entries are stored
 *        row-major; @c ⊕ / @c ⊗ are @c S's semiring operations, read off
 *        @c dedekind::algebra::semiring_ops<S> (never native @c
 * operator+/@c *, which the tropical carriers skew).
 *
 * @details The semiring trait opt-ins (@c is_associative_v, @c
 * is_commutative_v,
 *          @c is_idempotent_v) certify the two matrix monoids so that
 *          @c IsSemiring<MatNxNV<S,N>, MatPlus, MatTimes> holds.  Idempotence
 *          of @c ⊕ is inherited from the base (a dioid @c S ⟹ a dioid
 *          @c Mat(S)), which is what makes the @c *-closure terminate.
 */
export template <typename S, std::size_t N>
struct MatNxNV {
  using scalar_type = S;
  using dimension_type = dedekind::sets::Finite;
  static constexpr std::size_t row_count = N;
  static constexpr std::size_t column_count = N;
  /// @brief A column is a @ref Ket, a row a @ref Bra --- the bra·ket @b are the
  ///        matrix's column/row vectors, so @c Mat(S) is an @c IsMatrix.
  using column_type = Ket<S, N>;
  using row_type = Bra<S, N>;

  /**
   * @brief @c Mat(S) @b is the linear operator @f$|v\rangle \mapsto
   *        M|v\rangle@f$ --- a callable @c IsArrow with
   *        @c Domain @c = @c Codomain @c = @c Ket<S,N>.
   *
   * @details The @b callable reading of the arrow chain
   * @c IsLinearOperator ⟹ @c IsFunction ⟹ @c IsRelation ⟹ @c IsArrow
   * (see @c dedekind::relational::IsRelation for the three-hats note):
   * matrix-vector application is a total, single-valued map, so the matrix
   * @b is a function @b is an arrow.  The binary @c operator()(i,j) entry
   * accessor and this unary @c operator()(Ket) apply differ in arity, so
   * they do not collide.
   */
  using Domain = Ket<S, N>;
  using Codomain = Ket<S, N>;

  std::array<std::array<S, N>, N> e{};

  /// @brief §4 property distribution: matrix @c ⊕ (@ref MatPlus) is associative
  ///        exactly when the base @c ⊕ is; matrix @c ⊗ (@ref MatTimes) is
  ///        associative exactly when @c S is a @b semiring (the @c ⊕/⊗
  ///        contraction's associativity needs @c S's associativity AND
  ///        distributivity), so a non-semiring @c S no longer certifies
  ///        @c IsSemiring<Mat(S)>.
  template <typename Op>
  static constexpr bool is_associative_v =
      (std::same_as<Op, MatPlus<S, N>> &&
       dedekind::category::is_associative_v<
           S, typename dedekind::algebra::semiring_ops<S>::add>) ||
      (std::same_as<Op, MatTimes<S, N>> &&
       dedekind::category::IsSemiring<
           S, typename dedekind::algebra::semiring_ops<S>::add,
           typename dedekind::algebra::semiring_ops<S>::mult>);
  /// @brief Only @c ⊕ (@ref MatPlus) is commutative (@c ⊗ is not), and only
  ///        when the base @c ⊕ is.
  template <typename Op>
  static constexpr bool is_commutative_v =
      std::same_as<Op, MatPlus<S, N>> &&
      dedekind::category::is_commutative_v<
          S, typename dedekind::algebra::semiring_ops<S>::add>;
  /// @brief @c ⊕ is idempotent exactly when the base @c ⊕ is (dioid lift).
  template <typename Op>
  static constexpr bool is_idempotent_v =
      std::same_as<Op, MatPlus<S, N>> &&
      dedekind::category::is_idempotent_v<
          S, typename dedekind::algebra::semiring_ops<S>::add>;

  constexpr S operator()(std::size_t i, std::size_t j) const { return e[i][j]; }

  /**
   * @brief Matrix-vector application @f$(M|v\rangle)_i = \bigoplus_j M_{ij}
   *        \otimes v_j@f$ over @c S's semiring ops (never native @c +/@c *,
   *        which the tropical carriers skew).  The @c IsArrow call operator:
   *        @c Ket → @c Ket.
   *
   * @details Takes the extensional (array-backed) @c Ket.  Accepting an
   * @b intensional (rule / function-backed) index→scalar vector too is a
   * follow-up: the sound gate is that @c V is indexable at the finite index
   * (an @c IsRingIntegral domain), and that concept is not in scope here
   * without an awkward @c dedekind.order dependency --- so it lands with the
   * function-space image work, not on the bare @c Mat call operator.
   */
  constexpr Ket<S, N> operator()(const Ket<S, N>& v) const {
    using Add = typename dedekind::algebra::semiring_ops<S>::add;
    using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
    const S zero = dedekind::category::identity_v<S, Add>;
    Ket<S, N> r{};
    for (std::size_t i = 0; i < N; ++i) {
      S acc = zero;
      for (std::size_t j = 0; j < N; ++j)
        acc = Add{}(acc, Mult{}(e[i][j], v.c[j]));
      r.c[i] = acc;
    }
    return r;
  }

  constexpr const std::array<S, N>& operator[](std::size_t i) const {
    return e[i];
  }
  constexpr std::array<S, N>& operator[](std::size_t i) { return e[i]; }

  friend constexpr bool operator==(const MatNxNV&, const MatNxNV&) = default;

  /// @brief Terse SYNTAX over the semiring SEMANTICS (#796): @c A+B is the
  ///        elementwise @c ⊕ (@ref MatPlus), @c A*B the @c ⊕-⊗ matrix product
  ///        (@ref MatTimes) --- the @b same ops @c :transfer reads off
  ///        @c semiring_ops<Mat(S)>, never native operators.  The glyph derives
  ///        from the Form, so it cannot skew the way a native @c + does on the
  ///        tropical carrier (there @c + is @c ⊗); like @ref SemimoduleVec's
  ///        @c +/@c *, these are sugar over the one defaulted seam.
  friend constexpr MatNxNV operator+(const MatNxNV& a, const MatNxNV& b) {
    return MatPlus<S, N>{}(a, b);
  }
  friend constexpr MatNxNV operator*(const MatNxNV& a, const MatNxNV& b) {
    return MatTimes<S, N>{}(a, b);
  }

  /**
   * @brief Transpose @f$M^{\top}@f$ --- reflect across the main diagonal.
   *
   * @details @b The @b dagger @b of @b this @b arrow, one operation across the
   * three surfaces (see @c dedekind::category::IsDagger and
   * @c dedekind::relational::converse):
   *   @li on a @b relation (over @c 𝔹) it is the @b converse @f$R^{\circ}@f$
   *       (@c relational::converse / @c SwapPred):
   *       @f$(M^{\top})_{ij}=M_{ji}@f$ @b is @f$R^{\circ}(j,i)=R(i,j)@f$ ---
   *       transpose of the Boolean matrix @b is the swap of the relation's
   *       coordinates;
   *   @li on a @b real space it is the transpose @f$M^{\top}@f$ (here);
   *   @li on a complex/Hilbert space the @b adjoint @f$M^{*}@f$
   *       (conjugate-transpose).
   * It reverses every arrow (contravariant:
   * @f$(AB)^{\top}=B^{\top}A^{\top}@f$), exactly as @c converse does
   * (@f$(R;S)^{\circ}=S^{\circ};R^{\circ}@f$).  Now that @c Mat(S) is a
   * callable @c IsArrow, this transpose IS its @c † --- so
   * FIXME(#787): register @c inverse @c = @c transpose for the @b orthogonal /
   * @b unitary case (@f$M^{\top}=M^{-1}@f$), where "the converse is the
   * inverse" (bijective relation) and "the transpose is the inverse"
   * (orthogonal matrix) are @b one theorem.  @c :transfer's @c converse is this
   * dagger's intensional (rule-level) twin; this transpose is its extensional
   * (materialised) form.
   */
  constexpr MatNxNV transpose() const {
    MatNxNV t{};
    for (std::size_t i = 0; i < N; ++i)
      for (std::size_t j = 0; j < N; ++j) t.e[j][i] = e[i][j];
    return t;
  }

  /// @brief The @c j-th column as a @ref Ket (the horizontal decomposition).
  constexpr column_type column(std::size_t j) const {
    column_type k{};
    for (std::size_t i = 0; i < N; ++i) k.c[i] = e[i][j];
    return k;
  }
  /// @brief The @c i-th row as a @ref Bra (the vertical decomposition).
  constexpr row_type row(std::size_t i) const {
    row_type b{};
    b.c = e[i];
    return b;
  }
};

/**
 * @brief Opt-in: @c F is a @b linear @b operator --- a structure-preserving
 *        arrow @f$f(s\otimes x \oplus y) = s\otimes f(x) \oplus f(y)@f$.
 *
 * @details Like @c is_monic_arrow_v (@c :morphism), linearity quantifies over
 * all inputs, so it @b cannot be verified at compile time; the carrier declares
 * it and the public review is the audit trail.  @c Mat(S) is registered below.
 */
export template <typename F>
inline constexpr bool is_linear_operator_v = false;

/**
 * @brief A @b linear @b operator: a (callable) @c IsArrow that additionally
 *        preserves the semimodule structure.
 *
 * @details @b What @b this @b concept @b reifies (and what it does @b not).  As
 * @b code, @c IsLinearOperator<F> is exactly @c IsArrow<F> plus the opt-in
 * linearity trait --- a @b callable arrow (@c Domain/@c Codomain + a call
 * operator, which is what makes @c Mat(S) an @c IsArrow) that is declared
 * linear.  The wider reading
 * @c IsLinearOperator ⟹ @c IsFunction ⟹ @c IsRelation ⟹ @c IsArrow is the
 * @b conceptual chain, @b not a direct concept subsumption: the relational
 * @c IsFunction<R,A,B> / @c IsRelation<S,T1,T2> are predicates over a
 * @c Set<pair> carrier, a different shape from this unary callable arrow.  The
 * bridge is the @b graph: @c graph(f) (@c :relational, via
 * @c arrow_as_relation) IS the @c IsFunction / @c IsRelation, so the chain is
 * reified @b through that adapter, not by making @c IsLinearOperator require
 * the relational concepts directly.  (Linearity itself is a law over all
 * inputs --- uncheckable --- hence the opt-in trait, not a computed
 * refinement.)
 *
 * FIXME(#787): once @c Mat(S) carries a dagger @c inverse, @c IsUnitary ⟹
 * @c IsIsomorphism becomes real; FIXME(#301): invertibility via
 * determinant/adjugate; FIXME(#442): the law set that survives when @c S is not
 * a field.
 */
export template <typename F>
concept IsLinearOperator =
    dedekind::category::IsArrow<F> && is_linear_operator_v<F>;

/// @brief @c Mat(S) is the linear operator @f$|v\rangle \mapsto M|v\rangle@f$.
export template <typename S, std::size_t N>
inline constexpr bool is_linear_operator_v<MatNxNV<S, N>> = true;

// @note NO bare @c dagger(M) alias either (same reason as the vector case
//       above): a matrix over @c ℂ has several daggers, so "the dagger" is a
//       @b chosen involution functor.  @c MatNxNV::transpose is the
//       coordinate-swap one, certified @c is_involutive as @c TransposeF; ℂ's
//       conjugate-transpose is a different @c Dagger (FIXME(#787)).  Spell the
//       chosen dagger as @c TransposeF{}(M) (or the future conjugate one), not
//       a name that pretends the dagger is unique.

/** @brief The zero matrix — every entry the base @c ⊕-identity (0̄). */
export template <typename S, std::size_t N>
constexpr MatNxNV<S, N> zero_matrix() {
  using Add = typename dedekind::algebra::semiring_ops<S>::add;
  MatNxNV<S, N> z{};
  for (std::size_t i = 0; i < N; ++i)
    for (std::size_t j = 0; j < N; ++j)
      z.e[i][j] = dedekind::category::identity_v<S, Add>;
  return z;
}

/** @brief The identity matrix — @c ⊗-identity (1̄) on the diagonal, 0̄ off it. */
export template <typename S, std::size_t N>
constexpr MatNxNV<S, N> identity_matrix() {
  using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
  MatNxNV<S, N> id = zero_matrix<S, N>();
  for (std::size_t i = 0; i < N; ++i)
    id.e[i][i] = dedekind::category::identity_v<S, Mult>;
  return id;
}

/** @brief @c Mat(S)'s additive monoid @c ⊕: elementwise base-@c ⊕. */
template <typename S, std::size_t N>
struct MatPlus {
  constexpr MatNxNV<S, N> operator()(const MatNxNV<S, N>& a,
                                     const MatNxNV<S, N>& b) const {
    using Add = typename dedekind::algebra::semiring_ops<S>::add;
    MatNxNV<S, N> c{};
    for (std::size_t i = 0; i < N; ++i)
      for (std::size_t j = 0; j < N; ++j)
        c.e[i][j] = Add{}(a.e[i][j], b.e[i][j]);
    return c;
  }
};

/** @brief @c Mat(S)'s multiplicative monoid @c ⊗: the @c ⊕-@c ⊗ contraction
 *         @c c_{ij} = @c ⊕_k a_{ik} @c ⊗ @c b_{kj} (matrix multiply). */
template <typename S, std::size_t N>
struct MatTimes {
  constexpr MatNxNV<S, N> operator()(const MatNxNV<S, N>& a,
                                     const MatNxNV<S, N>& b) const {
    using Add = typename dedekind::algebra::semiring_ops<S>::add;
    using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
    const S zero = dedekind::category::identity_v<S, Add>;
    MatNxNV<S, N> c{};
    for (std::size_t i = 0; i < N; ++i)
      for (std::size_t j = 0; j < N; ++j) {
        S acc = zero;
        for (std::size_t k = 0; k < N; ++k)
          acc = Add{}(acc, Mult{}(a.e[i][k], b.e[k][j]));
        c.e[i][j] = acc;
      }
    return c;
  }
};

/** @brief The transpose reified as a @b dagger functor: @c TransposeF{}(A) =
 *  @c Aᵀ.  A value/type (not a free call), so the point-free
 *  @c dedekind::category dagger predicates and concepts (@c is_unitary,
 *  @c IsDagger, @c IsUnitary) can take it as their @c Dagger argument.  On
 *  @c Mat(S) the dagger is the transpose; on @c Rel it is @c converse; on a
 *  Hilbert space the adjoint --- @c :involution unifies the three. */
export template <typename S, std::size_t N>
struct TransposeF {
  constexpr MatNxNV<S, N> operator()(const MatNxNV<S, N>& a) const {
    return a.transpose();
  }
};

}  // namespace dedekind::linear_algebra

namespace dedekind::category {

/** @brief @c ⊕-identity of @c Mat(S) is the zero matrix.  Registered on the
 *         exported @c identity_trait extension point (the internal
 *         @c identity_registry box is not reachable across modules), exactly
 *         as @c dedekind::algebra::Tropical registers its own @c ∞ / @c 0. */
template <typename S, std::size_t N>
struct identity_trait<dedekind::linear_algebra::MatNxNV<S, N>,
                      dedekind::linear_algebra::MatPlus<S, N>> {
  static constexpr auto value = dedekind::linear_algebra::zero_matrix<S, N>();
};

/** @brief @c ⊗-identity of @c Mat(S) is the identity matrix. */
template <typename S, std::size_t N>
struct identity_trait<dedekind::linear_algebra::MatNxNV<S, N>,
                      dedekind::linear_algebra::MatTimes<S, N>> {
  static constexpr auto value =
      dedekind::linear_algebra::identity_matrix<S, N>();
};

/** @brief The transpose is an @b involution (@c Aᵀᵀ = @c A), so @c TransposeF
 * is a certified @c IsDagger on @c Mat(S) --- what the dagger predicates
 *  (@c is_unitary) require.  A @b structural fact about the operation, @b not a
 *  per-arrow unitarity claim (unitarity is a @b value property; see
 *  @c :involution). */
template <typename S, std::size_t N>
struct is_involutive<dedekind::linear_algebra::TransposeF<S, N>,
                     dedekind::linear_algebra::MatNxNV<S, N>> : std::true_type {
};

/** @brief The @c ⊕-identity of a @ref dedekind::linear_algebra::SemimoduleVec
 *         (a @ref Ket or @ref Bra) is the zero vector (every entry the base
 *         @c ⊕-identity) --- what makes it an @c IsCommutativeMonoid, hence an
 *         @c IsSemimodule, hence an @c IsColumnVector / @c IsCovector.  One
 *         registration for both orientations. */
template <typename S, std::size_t N, typename O>
struct identity_trait<
    dedekind::linear_algebra::SemimoduleVec<S, N, O>,
    std::plus<dedekind::linear_algebra::SemimoduleVec<S, N, O>>> {
  static constexpr auto value = [] {
    dedekind::linear_algebra::SemimoduleVec<S, N, O> z{};
    for (std::size_t i = 0; i < N; ++i)
      z.c[i] = identity_v<S, typename dedekind::algebra::semiring_ops<S>::add>;
    return z;
  }();
};

/** @brief @c ⊗ distributes over @c ⊕ in @c Mat(S) exactly when it does in @c S
 *         (§4 property distribution) --- not unconditionally. */
template <typename S, std::size_t N>
inline constexpr bool
    is_distributive_v<dedekind::linear_algebra::MatNxNV<S, N>,
                      dedekind::linear_algebra::MatTimes<S, N>,
                      dedekind::linear_algebra::MatPlus<S, N>> =
        is_distributive_v<S, typename dedekind::algebra::semiring_ops<S>::mult,
                          typename dedekind::algebra::semiring_ops<S>::add>;

/** @brief Totality of @c Mat(S)'s monoids is @b inherited, honestly: matrix
 *         @c ⊕ is total exactly when the base @c ⊕ is a magma, and matrix
 *         @c ⊗ (built from both base ops) when both are.  Same discipline as
 *         @c Tropical's @c ⊗ saturation --- @c true for a periodic/saturating
 *         base such as the necklace's saturating-unsigned dioid, @c false
 *         where the base op is a hazard (e.g. signed @c +). */
template <typename S, std::size_t N>
struct is_saturating<dedekind::linear_algebra::MatNxNV<S, N>,
                     dedekind::linear_algebra::MatPlus<S, N>>
    : std::bool_constant<
          IsMagma<S, typename dedekind::algebra::semiring_ops<S>::add>> {};
template <typename S, std::size_t N>
struct is_saturating<dedekind::linear_algebra::MatNxNV<S, N>,
                     dedekind::linear_algebra::MatTimes<S, N>>
    : std::bool_constant<
          IsMagma<S, typename dedekind::algebra::semiring_ops<S>::add> &&
          IsMagma<S, typename dedekind::algebra::semiring_ops<S>::mult>> {};

/** @brief Totality @b distributes over the vector construction (§4 property
 *  distribution): a @ref dedekind::linear_algebra::Ket /
 *  @ref dedekind::linear_algebra::Bra is a total magma under its elementwise
 *  @c ⊕ exactly when the @b scalar @c ⊕ is.  Parametric on @c S --- the
 * decision is settled at the call site by which scalar the exhibit uses (a
 * total field such as @c Rational<default_integer> or the max-plus dioid: yes;
 * a signed
 *  @c long-backed @c Rational: no), never asserted near the carrier. */
template <typename S, std::size_t N, typename O>
struct is_saturating<
    dedekind::linear_algebra::SemimoduleVec<S, N, O>,
    std::plus<dedekind::linear_algebra::SemimoduleVec<S, N, O>>>
    : std::bool_constant<
          IsMagma<S, typename dedekind::algebra::semiring_ops<S>::add>> {};

}  // namespace dedekind::category

namespace dedekind::algebra {

/** @brief @c Mat(S) is a semiring: register its @c ⊕ / @c ⊗ so downstream
 *         (e.g. @c :transfer's @c star = the @c *-closure) reads them
 *         generically off @c semiring_ops, exactly as for the base @c S. */
template <typename S, std::size_t N>
struct semiring_ops<dedekind::linear_algebra::MatNxNV<S, N>> {
  using add = dedekind::linear_algebra::MatPlus<S, N>;
  using mult = dedekind::linear_algebra::MatTimes<S, N>;
};

}  // namespace dedekind::algebra

namespace dedekind::linear_algebra {

/**
 * @section matnxn__Existential_Proof
 * @brief @c Mat : Semiring → Semiring is inhabited.  With @c S a semiring,
 *        @c Mat(S) = @c MatNxNV<S,N> is a semiring under (@ref MatPlus,
 *        @ref MatTimes).  Witnessed at the max-plus dioid, the carrier the
 *        necklace/CPM transfer machinery runs on.
 */
using MPll = dedekind::algebra::MaxPlus<unsigned long long>;
static_assert(
    dedekind::category::IsSemiring<MatNxNV<MPll, 3>, MatPlus<MPll, 3>,
                                   MatTimes<MPll, 3>>,
    "Mat(S) over a semiring S is itself a semiring (Kleene algebra of "
    "matrices).");

// The bra·ket ARE the row/column vectors: a Ket is the canonical IsSemimodule
// (an IsColumnVector), a Bra an IsCovector --- MaxPlus has no negation, and the
// semimodule contract asks none.  But it is NOT a module: IsModule needs the
// scalar to be a ring (an additive group), which the dioid MaxPlus is not.
static_assert(IsColumnVector<Ket<MPll, 3>>, "a Ket is a (semimodule) column.");
static_assert(IsCovector<Bra<MPll, 3>>, "a Bra is a (semimodule) row.");
static_assert(
    !dedekind::algebra::IsModule<
        Ket<MPll, 3>, MPll, std::plus<Ket<MPll, 3>>,
        typename dedekind::algebra::semiring_ops<MPll>::add,
        typename dedekind::algebra::semiring_ops<MPll>::mult>,
    "a MaxPlus Ket is a semimodule but NOT a module: the dioid has no −a.");
static_assert(IsMatrix<MatNxNV<MPll, 3>>,
              "Mat(S) is a matrix: shape + Ket columns + Bra rows + both "
              "decompositions, all over a semiring.");

// ── Mat(S) as a callable arrow: the IsLinearOperator chain (#787 / #301) ────
static_assert(
    dedekind::category::IsArrow<MatNxNV<MPll, 3>>,
    "Mat(S) is a callable arrow |v⟩ ↦ M|v⟩ (Domain = Codomain = Ket), "
    "the callable reading of IsLinearOperator ⟹ … ⟹ IsArrow.");
static_assert(IsLinearOperator<MatNxNV<MPll, 3>>,
              "Mat(S) is a linear operator (a callable arrow + declared "
              "linearity).");
// Matrix-vector application over the MaxPlus semiring: the identity operator
// I|v⟩ = |v⟩ (exercises operator()(Ket) end to end).
static_assert(identity_matrix<MPll, 3>()(Ket<MPll, 3>{}) == Ket<MPll, 3>{},
              "I|v⟩ = |v⟩ for the semiring identity matrix.");

// A NON-identity matvec, hand-checked, so a wrong-indexing / wrong-⊕⊗ impl
// cannot pass: M = [[0,1],[2,0]], v = [10,20] over max-plus (⊕=max, ⊗=+), so
// (Mv)_i = max_j(M_ij + v_j) = [max(0+10,1+20), max(2+10,0+20)] = [21, 20].
constexpr bool nontrivial_maxplus_matvec() {
  MatNxNV<MPll, 2> m{};
  m.e[0][0] = MPll{true, 0ull};
  m.e[0][1] = MPll{true, 1ull};
  m.e[1][0] = MPll{true, 2ull};
  m.e[1][1] = MPll{true, 0ull};
  Ket<MPll, 2> v{};
  v.c[0] = MPll{true, 10ull};
  v.c[1] = MPll{true, 20ull};
  const Ket<MPll, 2> r = m(v);
  return r.c[0] == MPll{true, 21ull} && r.c[1] == MPll{true, 20ull};
}
static_assert(nontrivial_maxplus_matvec(),
              "non-identity max-plus matvec M·v = [21, 20] "
              "(catches mis-indexing / wrong ⊕⊗).");

// ── The bra-ket dagger: transpose flips Ket ↔ Bra (aligns with Mat transpose)
// ─
static_assert(std::same_as<decltype(transpose(Ket<MPll, 3>{})), Bra<MPll, 3>>,
              "dagger(|v⟩) = ⟨v|: the transpose of a Ket is a Bra.");
static_assert(std::same_as<decltype(transpose(Bra<MPll, 3>{})), Ket<MPll, 3>>,
              "dagger(⟨w|) = |w⟩: and of a Bra is a Ket.");
static_assert(transpose(Ket<MPll, 3>{}) == Bra<MPll, 3>{},
              "the dagger keeps the components (Ket ↦ Bra, same entries).");
static_assert(
    transpose(transpose(Ket<MPll, 3>{})) == Ket<MPll, 3>{},
    "the transpose (coordinate-swap dagger) is an involution: v†† = v.");

// ── dagger ↔ involution ↔ (group) ↔ isomorphism, where applicable ───────────
// (1) INVOLUTION: the coordinate-swap dagger on a matrix IS the CERTIFIED
//     involution TransposeF (Aᵀᵀ = A, the order-2 / ℤ2 fact).  ("The" dagger is
//     relative to a chosen involution functor --- TransposeF here; ℂ's
//     conjugate-transpose is a different one, #787.)
static_assert(
    dedekind::category::IsDagger<TransposeF<MPll, 3>, MatNxNV<MPll, 3>>,
    "transpose is a certified dagger / involution on Mat(S): Aᵀᵀ = A.");
static_assert(TransposeF<MPll, 3>{}(TransposeF<MPll, 3>{}(
                  identity_matrix<MPll, 3>())) == identity_matrix<MPll, 3>(),
              "the TransposeF dagger is an involution: M†† = M.");
// The vector dagger's involution is the transpose(transpose(v)) == v witness
// above (Ket ↔ Bra is order-2); the IsInvolution CONCEPT is N/A there, since a
// single dagger flips the TYPE (Ket → Bra), not an endomap on one carrier.

// (2) ISOMORPHISM, where applicable: for a UNITARY matrix the dagger IS the
//     inverse (M† = M⁻¹).  The identity is the trivial unitary (I† = I = I⁻¹),
//     read off the generic dagger surface.  Concept-level IsUnitary ⟹
//     IsIsomorphism (registering inverse = f†) is FIXME(#787); a matrix TYPE
//     holds many arrows, so unitarity stays value-level here.
static_assert(
    dedekind::category::is_unitary<
        TransposeF<MPll, 3>,
        typename dedekind::algebra::semiring_ops<MatNxNV<MPll, 3>>::mult>(
        identity_matrix<MPll, 3>()),
    "the identity is unitary: I†;I = I;I† = I (dagger = inverse, applicable).");

}  // namespace dedekind::linear_algebra
