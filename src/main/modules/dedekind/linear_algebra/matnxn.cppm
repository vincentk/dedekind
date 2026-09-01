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
 * @c dedekind::algebra::semiring_ops<S> — lifted to a first-class carrier.
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
 * @brief @c |v⟩: a column of @c Mat(S) --- an N-entry @b semimodule vector over
 *        the semiring @c S, read as an index→scalar @b ket.  It is
 *        @c IsColumnVector by the default (semimodule) contract: @c ⊕ and the
 *        two-sided scalar @c ⊗ action, but @b no negation (a dioid has none).
 *        The bra·ket language of @c :transfer, made a first-class carrier: this
 *        @b is the "matrix column is a vector" slogan for a semiring.
 */
export template <typename S, std::size_t N>
struct Ket {
  using scalar_type = S;
  using orientation = ColumnOrientation;
  using dimension_type = dedekind::sets::Finite;
  static constexpr std::size_t dimension = N;

  std::array<S, N> c{};

  /// @brief The additive monoid @c (Ket, ⊕) inherits the scalar's laws: since
  ///        @c S is a semiring, its @c ⊕ is an associative + commutative
  ///        monoid, and so is the elementwise Ket-@c ⊕.  Totality distributes
  ///        separately (see the @c is_saturating registration below).
  template <typename Op>
  static constexpr bool is_associative_v = true;
  template <typename Op>
  static constexpr bool is_commutative_v = true;

  constexpr S operator()(std::size_t i) const {
    return c[i];
  }  // index → scalar
  constexpr S operator[](std::size_t i) const { return c[i]; }
  friend constexpr bool operator==(const Ket&, const Ket&) = default;

  friend constexpr Ket operator+(const Ket& a, const Ket& b) {
    using Add = typename dedekind::algebra::semiring_ops<S>::add;
    Ket r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Add{}(a.c[i], b.c[i]);
    return r;
  }
  friend constexpr Ket operator*(const S& s, const Ket& a) {
    using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
    Ket r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Mult{}(s, a.c[i]);
    return r;
  }
  friend constexpr Ket operator*(const Ket& a, const S& s) {
    using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
    Ket r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Mult{}(a.c[i], s);
    return r;
  }
};

/** @brief @c ⟨w|: a row of @c Mat(S) --- the @c RowOrientation twin of @ref
 * Ket, an @c IsCovector semimodule vector (the bra). */
export template <typename S, std::size_t N>
struct Bra {
  using scalar_type = S;
  using orientation = RowOrientation;
  using dimension_type = dedekind::sets::Finite;
  static constexpr std::size_t dimension = N;

  std::array<S, N> c{};

  template <typename Op>
  static constexpr bool is_associative_v = true;
  template <typename Op>
  static constexpr bool is_commutative_v = true;

  constexpr S operator()(std::size_t i) const { return c[i]; }
  constexpr S operator[](std::size_t i) const { return c[i]; }
  friend constexpr bool operator==(const Bra&, const Bra&) = default;

  friend constexpr Bra operator+(const Bra& a, const Bra& b) {
    using Add = typename dedekind::algebra::semiring_ops<S>::add;
    Bra r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Add{}(a.c[i], b.c[i]);
    return r;
  }
  friend constexpr Bra operator*(const S& s, const Bra& a) {
    using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
    Bra r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Mult{}(s, a.c[i]);
    return r;
  }
  friend constexpr Bra operator*(const Bra& a, const S& s) {
    using Mult = typename dedekind::algebra::semiring_ops<S>::mult;
    Bra r{};
    for (std::size_t i = 0; i < N; ++i) r.c[i] = Mult{}(a.c[i], s);
    return r;
  }
};

/**
 * @brief @c Mat(S): the N×N matrix over a semiring @c S.  Entries are stored
 *        row-major; @c ⊕ / @c ⊗ are @c S's semiring operations, read off
 *        @c dedekind::algebra::semiring_ops<S> (never native @c operator+/@c *,
 *        which the tropical carriers skew).
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

  std::array<std::array<S, N>, N> e{};

  /// @brief Matrix @c ⊕ and @c ⊗ are both associative.
  template <typename Op>
  static constexpr bool is_associative_v = true;
  /// @brief Only @c ⊕ (@ref MatPlus) is commutative; @c ⊗ is not.
  template <typename Op>
  static constexpr bool is_commutative_v = std::same_as<Op, MatPlus<S, N>>;
  /// @brief @c ⊕ is idempotent exactly when the base @c ⊕ is (dioid lift).
  template <typename Op>
  static constexpr bool is_idempotent_v =
      std::same_as<Op, MatPlus<S, N>> &&
      dedekind::category::is_idempotent_v<
          S, typename dedekind::algebra::semiring_ops<S>::add>;

  constexpr S operator()(std::size_t i, std::size_t j) const { return e[i][j]; }
  constexpr const std::array<S, N>& operator[](std::size_t i) const {
    return e[i];
  }
  constexpr std::array<S, N>& operator[](std::size_t i) { return e[i]; }

  friend constexpr bool operator==(const MatNxNV&, const MatNxNV&) = default;

  /// @brief Transpose — reflect across the main diagonal (the extensional
  ///        dagger; @c :transfer's @c converse is its intensional twin).
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

/** @brief The @c ⊕-identity of a @ref dedekind::linear_algebra::Ket /
 *         @ref dedekind::linear_algebra::Bra is the zero vector (every entry
 *         the base @c ⊕-identity) --- what makes it an @c IsCommutativeMonoid,
 *         hence an @c IsSemimodule, hence an @c IsColumnVector / @c IsCovector.
 */
template <typename S, std::size_t N>
struct identity_trait<dedekind::linear_algebra::Ket<S, N>,
                      std::plus<dedekind::linear_algebra::Ket<S, N>>> {
  static constexpr auto value = [] {
    dedekind::linear_algebra::Ket<S, N> z{};
    for (std::size_t i = 0; i < N; ++i)
      z.c[i] = identity_v<S, typename dedekind::algebra::semiring_ops<S>::add>;
    return z;
  }();
};
template <typename S, std::size_t N>
struct identity_trait<dedekind::linear_algebra::Bra<S, N>,
                      std::plus<dedekind::linear_algebra::Bra<S, N>>> {
  static constexpr auto value = [] {
    dedekind::linear_algebra::Bra<S, N> z{};
    for (std::size_t i = 0; i < N; ++i)
      z.c[i] = identity_v<S, typename dedekind::algebra::semiring_ops<S>::add>;
    return z;
  }();
};

/** @brief @c ⊗ distributes over @c ⊕ in @c Mat(S) (inherited from @c S). */
template <typename S, std::size_t N>
inline constexpr bool
    is_distributive_v<dedekind::linear_algebra::MatNxNV<S, N>,
                      dedekind::linear_algebra::MatTimes<S, N>,
                      dedekind::linear_algebra::MatPlus<S, N>> = true;

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
template <typename S, std::size_t N>
struct is_saturating<dedekind::linear_algebra::Ket<S, N>,
                     std::plus<dedekind::linear_algebra::Ket<S, N>>>
    : std::bool_constant<
          IsMagma<S, typename dedekind::algebra::semiring_ops<S>::add>> {};
template <typename S, std::size_t N>
struct is_saturating<dedekind::linear_algebra::Bra<S, N>,
                     std::plus<dedekind::linear_algebra::Bra<S, N>>>
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

}  // namespace dedekind::linear_algebra
