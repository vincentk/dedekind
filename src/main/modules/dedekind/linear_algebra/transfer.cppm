/**
 * @file dedekind/linear_algebra/transfer.cppm
 * @partition :transfer
 * @brief Semiring-generic low-rank transfer operators: the bra·ket inner
 *        product, the @c ⊕/⊗ matrix-product kernel, and the rank-1 collapse
 *        @c D² @c = @c λ@c ·@c D that makes a transfer @b power analytic.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section transfer__Motivation
 * The dense @c :mat2x2 and the intensional @c :diagonal carriers multiply
 * entries with the built-in ring @c · — so a max-plus dioid, which has no
 * C++ @c operator*, cannot be an entry type.  This partition supplies the
 * missing @b semiring surface, routing every scalar product through an
 * @c Mult @c = @c ⊗ functor and every scalar sum through @c Add @c = @c ⊕,
 * defaulted (like @ref dedekind::optimization::semiring_closure) from
 * @c dedekind::algebra::semiring_ops<S>.  The one gate is
 * @c dedekind::category::IsSemiring — never @c HasRingOperators.
 *
 * @section transfer__The_Rank_1_Collapse
 * A rank-1 dyad @c M @c = @c |v⟩⟨w| (an @ref OuterProduct with a semiring
 * @c Mult) composes with itself in closed form:
 * @f[
 *   (M \cdot M)(i,j) \;=\; \bigoplus_k v_i \otimes w_k \otimes v_k \otimes w_j
 *     \;=\; v_i \otimes \Big(\underbrace{\bigoplus_k w_k \otimes v_k}_{\lambda
 *     \,=\, \langle w | v \rangle}\Big) \otimes w_j \;=\; \lambda \otimes
 * M(i,j).
 * @f]
 * So @c M^n @c = @c λ^{n-1}@c M and the transfer power never iterates: the
 * @b whole memo is the one scalar @c λ.  This is @c rank @c = @c 1 of the
 * Fliess / Carlyle–Paz law @c rank(Hankel) @c = @c dim(minimal @c memo).  For
 * a max-plus carrier @c λ @c = @c ⟨w|v⟩ @c = @c max_k(w_k @c + @c v_k) is the
 * tropical eigenvalue; the choice of semiring is the choice of problem
 * (@c bool @c ⟹ reachability, @c MaxPlus @c ⟹ critical path).
 *
 * @note "Alles Gescheite ist schon gedacht worden, man muß nur versuchen, es
 *        noch einmal zu denken." — J. W. von Goethe.  Here: the transfer
 *        matrix of statistical mechanics, met again as a rank-1 dyad.
 */
module;

#include <array>
#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>
#include <utility>

export module dedekind.linear_algebra:transfer;

import dedekind.algebra; // IsSemiring, semiring_ops, Tropical/MaxPlus, ⊕/⊗
import dedekind.category; // IsArrow, IsSemiring, identity_v, closure, ClassicalLogic
import dedekind.order; // IsDirectedSet; converse / is_relation (Rel dagger)
import dedekind.sets;  // Set<pair,L,P> — the DSL relation carrier
import :diagonal;      // OuterProduct — the rank-1 dyad carrier
import :matnxn;        // Mat(S) = MatNxNV<S,N>: the certified matrix semiring

namespace dedekind::linear_algebra {

/**
 * @brief The one reduction the transfer surface keeps: a structured @c ⊕-fold
 *        @c ⊕_{k<N} @c term(k) over the (low, fixed) rank index, the empty fold
 *        being the @c ⊕-identity.  A fold-expression, not an imperative loop —
 *        the DSL folds @b structurally, so the reduction stays intensional
 *        until an optimum "means it".
 *
 * @tparam Add  The @c ⊕ functor.
 * @param  zero The @c ⊕-identity (0-bar), the value of the empty (@c N=0) fold.
 * @param  term The intensional summand @c k @c ↦ @c term(k).
 */
template <typename Add, typename S, typename Term, std::size_t... K>
constexpr S add_fold(std::index_sequence<K...>, S zero, Term term) {
  S acc = zero;
  ((acc = Add{}(acc, term(K))), ...);  // structured ⊕-fold over the rank pack
  return acc;
}

/**
 * @brief The bra·ket inner product @c ⟨w|v⟩ @c = @c ⊕_{k<N} @c w(k) @c ⊗
 *        @c v(k) over a semiring.  Bra @c w and ket @c v are @c IsArrow
 *        factors (index → scalar); @c N is the shared dimension.
 *
 * @details For a tropical carrier this is @c max_k(w_k @c + @c v_k) — the
 *          max-plus eigenvalue that governs the rank-1 transfer power.  The
 *          @c ⊕-fold over the (low) rank dimension is the matrix-contraction
 *          kernel, evaluated as a bounded compile-time reduction.
 */
export template <
    std::size_t N, typename Bra, typename Ket,
    typename S = typename std::remove_cvref_t<Bra>::Codomain,
    typename Add = typename dedekind::algebra::semiring_ops<S>::add,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult>
  requires dedekind::category::IsArrow<Bra> &&
           dedekind::category::IsArrow<Ket> &&
           dedekind::category::IsSemiring<S, Add, Mult>
constexpr S inner_product(const Bra& w, const Ket& v) {
  using WD = typename std::remove_cvref_t<Bra>::Domain;
  using VD = typename std::remove_cvref_t<Ket>::Domain;
  return add_fold<Add>(
      std::make_index_sequence<N>{}, dedekind::category::identity_v<S, Add>,
      [&](std::size_t k) {
        return Mult{}(w(static_cast<WD>(k)), v(static_cast<VD>(k)));
      });
}

/**
 * @brief The semiring matrix-product entry @c (A@c ⊗@c B)(i,j) @c = @c
 *        @c ⊕_{k<N} @c A(i,k) @c ⊗ @c B(k,j).  @c A and @c B are any
 *        @c (i,j)@c →@c S entry maps (e.g. an @ref OuterProduct); @c N is
 *        the contracted (middle) dimension.
 *
 * @details This is the @c ⊕/⊗ kernel the ring-wired @c :mat2x2 lacked.  It
 *          is rank-agnostic; the @ref inner_product above is its 1×1 face.
 */
export template <
    std::size_t N, typename A, typename B,
    typename S =
        std::remove_cvref_t<std::invoke_result_t<A, std::size_t, std::size_t>>,
    typename Add = typename dedekind::algebra::semiring_ops<S>::add,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult>
  requires dedekind::category::IsSemiring<S, Add, Mult>
constexpr S matmul_entry(const A& a, const B& b, std::size_t i, std::size_t j) {
  return add_fold<Add>(std::make_index_sequence<N>{},
                       dedekind::category::identity_v<S, Add>,
                       [&](std::size_t k) { return Mult{}(a(i, k), b(k, j)); });
}

/**
 * @brief The rank-1 eigenvalue of a dyad @c M @c = @c |v⟩⟨w|: @c λ @c =
 *        @c ⟨w|v⟩, the scalar for which @c M² @c = @c λ@c ·@c M.  Reads the
 *        dyad's factors straight off the @ref OuterProduct.
 */
export template <std::size_t N, typename U, typename V, typename Mult>
constexpr auto eigenvalue(const OuterProduct<U, V, Mult>& m) {
  // ⟨w|v⟩ contracts the RIGHT factor (bra w = m.v) against the LEFT factor
  // (ket v = m.u) over the shared middle dimension.
  return inner_product<N>(m.v, m.u);
}

/**
 * @brief The closure of a length-@c L chain of transfers: the @c ⊗-fold of
 *        their per-bead eigenvalues @c ⊗_{m<L} @c bead(m).  A structured fold
 *        whose accumulator is a @b single scalar — because each transfer is
 *        rank-1, that scalar is the @b entire memo (@c dim @c = @c rank @c =
 *        @c 1).  Intensional: @c bead is an @c index→λ map, and nothing
 *        materialises until the value is forced ("realize it when you mean
 * it").
 */
export template <
    std::size_t L, typename Bead,
    typename S = std::remove_cvref_t<std::invoke_result_t<Bead, std::size_t>>,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult>
constexpr S transfer_chain(Bead bead) {
  return add_fold<Mult>(std::make_index_sequence<L>{},
                        dedekind::category::identity_v<S, Mult>,
                        [&](std::size_t m) { return bead(m); });
}

// The intensional argmax over a finite branch domain is NOT reimplemented here:
// it is `dedekind::sequences::argmax(interval, cost)` (the §3.3 forall-filter),
// applied over the branch interval [0,N) with the bra·ket cost.  See the
// necklace exhibit's `diamond_argmax`.

/** @section transfer__The_Intensional_Extensional_Joint
 *
 *  The secret sauce that turns the general theorem @c R* @c = @c closure(Δ, @c
 *  λS.@c S;R) into an @b executable corollary: the map from an intensional
 *  relation (a predicate over a possibly-infinite carrier, whose @b type grows
 *  under @c ; and @c ∪) to its extensional adjacency matrix (a @b fixed,
 *  type-stable carrier), on which a fixpoint can run.  The extensionality
 *  constraint is spelled out @b in @b the @b type: @ref materialise demands the
 *  finite bound @c N (the @c IsExtensional licence), and @ref star only accepts
 *  the resulting @ref SquareMatrix.  So @c R* @c = @c star(materialise<N>(R))
 *  type-checks exactly when the carrier is finite; the infinite case is a type
 *  error, not a runtime check --- the Rice wall, made structural.
 */

/** @brief A dense @c N×N semiring matrix --- the extensional adjacency carrier
 *  a relation materialises to.  Finite @c N by construction, hence the
 *  @c IsExtensional witness the star's fixpoint needs.  This @b is @c Mat(S)
 *  (@ref MatNxNV, @c :matnxn), the certified matrix semiring; the alias keeps
 *  the transfer-local spelling while @c ⊕/⊗ and the @c *-closure come from
 *  @c semiring_ops<Mat(S)>, not hand-rolled here. */
export template <typename S, std::size_t N>
using SquareMatrix = MatNxNV<S, N>;

/** @brief @b THE @b JOINT: materialise a @b binary @b endorelation @b on @b a
 *  @b semiring into its dense extensional adjacency @ref SquareMatrix --- the
 *  generic (black-box) fiber of the @c relation→matrix map (§ the paper's
 *  classification table).  @c rel is @c (i,j)@c →@c S over the @b same finite
 *  index carrier @c [0,N) on both sides (hence @b endo, hence @b square), with
 *  the codomain @c S an @c IsSemiring (so @c ⊕/⊗ and thus @ref star are
 *  defined).  The compile-time bound @c N @b is the extensionality constraint:
 *  an infinite relation has no such @c N and cannot form a matrix, so the Rice
 *  wall lives in this signature.  Structured relations (functional, bijective,
 *  rank-1) admit specialised fibers (sparse, orthogonal, low-rank); this is the
 *  base case. */
/** @brief Read a relation's @c (i,j) entry, accepting EITHER the two-index
 *  weighted call @c rel(i,j) (a semiring-adjacency arrow) OR the point-free DSL
 *  call @c rel({i,j}) (a @c Set<pair> relation).  This one seam is what lets a
 *  @b typed Ddk relation --- not a raw lambda --- feed @ref materialise. */
template <typename Rel>
constexpr auto rel_entry(const Rel& rel, std::size_t i, std::size_t j) {
  if constexpr (std::invocable<const Rel&, std::size_t, std::size_t>)
    return rel(i, j);
  else
    return rel(std::pair<std::size_t, std::size_t>{i, j});
}
template <typename Rel>
using rel_codomain_t = std::remove_cvref_t<decltype(rel_entry(
    std::declval<const Rel&>(), std::size_t{0}, std::size_t{0}))>;

export template <
    std::size_t N, typename Rel, typename S = rel_codomain_t<Rel>,
    typename Add = typename dedekind::algebra::semiring_ops<S>::add,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult>
  requires dedekind::category::IsSemiring<S, Add, Mult>
constexpr SquareMatrix<S, N> materialise(const Rel& rel) {
  SquareMatrix<S, N> m{};
  for (std::size_t i = 0; i < N; ++i)
    for (std::size_t j = 0; j < N; ++j)
      m[i][j] = static_cast<S>(rel_entry(rel, i, j));
  return m;
}

/** @brief The Kleene star @c A* @c = @c ⨆ₙ @c Aⁿ @c = @c Δ @c ⊕ @c A @c ⊕ @c A²
 *  @c ⊕ @c … --- @c R* as a @b running corollary of the generic @c closure,
 *  iterating @c M @c ↦ @c Δ @c ⊕ @c M⊗A to its fixpoint on the @b extensional
 *  matrix.  Over @c Bool it is reachability; over @c MaxPlus the longest path
 *  (the algebraic path problem).  Discharges @c FIXME(#786) for @c R* as a
 *  matrix corollary; the @ref SquareMatrix argument is the extensionality the
 *  intensional relation lacked.
 *
 *  @c ⊕/⊗ and @c Δ are @b not hand-rolled: they are @c Mat(S)'s own semiring
 *  operations (@c semiring_ops<Mat(S)> = @c MatPlus / @c MatTimes) and unit
 *  (@ref identity_matrix), so @c star is literally the @c *-closure of the
 *  certified matrix semiring --- the same generic @c closure that computes any
 *  semiring's star, instantiated at @c Mat(S). */
export template <
    std::size_t N, typename S,
    typename Add = typename dedekind::algebra::semiring_ops<S>::add,
    typename Mult = typename dedekind::algebra::semiring_ops<S>::mult>
  requires dedekind::category::IsSemiring<S, Add, Mult>
constexpr SquareMatrix<S, N> star(const SquareMatrix<S, N>& A) {
  using Mat = MatNxNV<S, N>;
  using MatAdd = typename dedekind::algebra::semiring_ops<Mat>::add;
  using MatMult = typename dedekind::algebra::semiring_ops<Mat>::mult;
  return dedekind::category::closure(
      identity_matrix<S, N>(),  // Δ = ⊗-identity of Mat(S)
      [A](const Mat& M) { return MatMult{}(M, A); },               // M ↦ M ⊗ A
      [](const Mat& x, const Mat& y) { return MatAdd{}(x, y); });  // ⊕
}

/** @brief The @b extensional dagger: the transpose @c Aᵀ (swap @c i,j) of an
 *  already-materialised matrix.  The dagger @b itself is @c converse (the
 *  intensional predicate swap @c R°(i,j) @c = @c R(j,i), see the witnesses);
 * this transpose is only its materialised realisation, worth forming when a
 * physical layout is wanted (cache locality) rather than as the operation.  On
 * @c Rel the dagger is @c converse, on a real space the transpose, on a Hilbert
 * space the adjoint; it reverses every arrow, so @c (R°)* @c = @c (R*)° is
 * CPM's
 *  @b backward pass (the latest-start times). */
export template <std::size_t N, typename S>
constexpr SquareMatrix<S, N> transpose(const SquareMatrix<S, N>& A) {
  return A
      .transpose();  // Mat(S) owns the reflection; this is the free spelling
}

/** @section transfer__Witnesses */
namespace detail_transfer {

using dedekind::algebra::MaxPlus;
using MP = MaxPlus<unsigned long long>;

// A diamond bead: two parallel branches with entry→mid weights (a1,b1) as the
// ket, and mid→exit weights (a2,b2) as the bra.  The tropical inner product is
// the bead gain max(a1+a2, b1+b2) — the diamond's parallelism as a bra·ket.
struct entry_to_mid {  // ket v: 0 ↦ a1 = 3, 1 ↦ b1 = 5
  using Domain = std::size_t;
  using Codomain = MP;
  constexpr MP operator()(std::size_t k) const {
    return k == 0 ? MP::of(3) : MP::of(5);
  }
};
struct mid_to_exit {  // bra w: 0 ↦ a2 = 5, 1 ↦ b2 = 2
  using Domain = std::size_t;
  using Codomain = MP;
  constexpr MP operator()(std::size_t k) const {
    return k == 0 ? MP::of(5) : MP::of(2);
  }
};
static_assert(dedekind::category::IsArrow<entry_to_mid>);
static_assert(dedekind::category::IsArrow<mid_to_exit>);

// λ = ⟨w|v⟩ = max(3+5, 5+2) = max(8, 7) = 8 — the long branch wins.
inline constexpr MP lambda = inner_product<2>(mid_to_exit{}, entry_to_mid{});
static_assert(lambda == MP::of(8),
              "tropical bra·ket eigenvalue max(3+5, 5+2) = 8.");

// The rank-1 dyad M = |v⟩⟨w| over the tropical ⊗ (= saturating +).
using TropMult = typename dedekind::algebra::semiring_ops<MP>::mult;
inline constexpr OuterProduct<entry_to_mid, mid_to_exit, TropMult> bead{};

// Entry law: M(i,j) = v_i ⊗ w_j (tropical + ).  M(0,0) = 3+5 = 8.
static_assert(bead(0, 0) == MP::of(8), "dyad entry (0,0) = 3 ⊗ 5 = 8.");
static_assert(bead(1, 0) == MP::of(10), "dyad entry (1,0) = 5 ⊗ 5 = 10.");

// The rank-1 collapse, witnessed mechanically: (M·M)(i,j) = λ ⊗ M(i,j).
// M²(0,0) = ⊕_k M(0,k)⊗M(k,0) = max((3+5)+(3+5), (3+2)+(5+5)) = max(16,15) = 16
//         = λ ⊗ M(0,0) = 8 + 8 = 16.  The transfer power never iterates.
static_assert(matmul_entry<2>(bead, bead, 0, 0) ==
                  TropMult{}(lambda, bead(0, 0)),
              "rank-1 collapse M² = λ·M at (0,0): 16 = 8 ⊗ 8.");
static_assert(matmul_entry<2>(bead, bead, 1, 1) ==
                  TropMult{}(lambda, bead(1, 1)),
              "rank-1 collapse M² = λ·M at (1,1).");
static_assert(matmul_entry<2>(bead, bead, 0, 1) ==
                  TropMult{}(lambda, bead(0, 1)),
              "rank-1 collapse M² = λ·M at (0,1) — off-diagonal.");
static_assert(eigenvalue<2>(bead) == lambda,
              "eigenvalue(M) reads λ = ⟨w|v⟩ off the dyad's factors.");

// (The intensional, IsSet-valued, tie-honest argmax over the branches is
// `dedekind::sequences::argmax`; it is exercised in materialise_test and the
// necklace exhibit, not reimplemented here.)

// transfer_chain: a length-3 chain of the uniform bead gain λ=8 closes to
// λ⊗λ⊗λ = 8+8+8 = 24 — a structured ⊗-fold, one scalar of memo.
static_assert(transfer_chain<3>([](std::size_t) { return lambda; }) ==
                  MP::of(24),
              "transfer_chain ⊗-folds three λ=8 beads to 24.");

// The choice of semiring is the choice of problem.  Over 𝔹 the SAME bra·ket
// is reachability: ⊕_k (w_k ∧ v_k) — is there a branch present end to end?
struct branch_in {  // v: both branches present
  using Domain = std::size_t;
  using Codomain = bool;
  constexpr bool operator()(std::size_t) const { return true; }
};
struct branch_out {  // w: only the k=0 branch present
  using Domain = std::size_t;
  using Codomain = bool;
  constexpr bool operator()(std::size_t k) const { return k == 0; }
};
static_assert(inner_product<2>(branch_out{}, branch_in{}),
              "𝔹 bra·ket = reachability: some branch is present end to end.");

// ── R* as a MATRIX corollary of closure: the intensional→extensional joint
// made executable.  The relations here are @b typed Ddk carriers, not raw
// lambdas: a bool edge relation is a @c Set<pair> (the DSL @c converse and the
// four-property traits attach to it), and its adjacency is minted by
// @ref materialise.  R* = star(materialise<N>(R)).
using Idx2 = std::pair<std::size_t, std::size_t>;

/** @brief The path DAG's edge relation @c i → i+1, a NAMED predicate carrier
 *  (un-lambda'd, so it is a typed relation the DSL converse and relation traits
 *  attach to). */
struct EdgeSucc {
  template <typename Pair>
  constexpr bool operator()(const Pair& p) const {
    return static_cast<std::size_t>(p.second) ==
           static_cast<std::size_t>(p.first) + 1;
  }
};
inline constexpr auto path_rel =
    dedekind::sets::Set<Idx2, dedekind::category::ClassicalLogic, EdgeSucc>{
        EdgeSucc{}};
static_assert(dedekind::order::is_relation(path_rel),
              "path_rel is a Ddk relation: an IsSet on a product domain.");

// Over 𝔹 the star is REACHABILITY, R*[i][j] = (i ≤ j).
inline constexpr auto Bstar = star<4>(materialise<4>(path_rel));
static_assert(Bstar[0][0], "reflexive: Δ ⊆ R* (0 reaches 0).");
static_assert(Bstar[0][3], "transitive: 0 reaches 3 along the path.");
static_assert(!Bstar[3][0], "acyclic: 3 does not reach 0.");

// Over MaxPlus with unit edges the SAME star is the LONGEST path (the algebraic
// path problem) — one semiring apart.  A WEIGHTED (semiring-valued) relation is
// not a bool Set<pair>; it is Mat(S)'s vocabulary, so a named @c (i,j)→MP
// adjacency carrier.
using TropAdd = typename dedekind::algebra::semiring_ops<MP>::add;
inline constexpr MP mp_bot = dedekind::category::identity_v<MP, TropAdd>;
struct WeightedUnitPath {
  constexpr MP operator()(std::size_t i, std::size_t j) const {
    return (j == i + 1) ? MP::of(1) : mp_bot;
  }
};
inline constexpr WeightedUnitPath wpath{};
inline constexpr auto Tstar = star<4>(materialise<4>(wpath));
static_assert(Tstar[0][3] == MP::of(3),
              "MaxPlus star = longest path: 0→1→2→3 costs 3.");
static_assert(Tstar[3][0] == mp_bot, "unreachable = the ⊕-identity.");

// ── The DAGGER, two vocabularies.  On a WEIGHTED relation it is the matrix
// TRANSPOSE (the real-space form of the converse): CPM's BACKWARD pass is the
// closure of the transposed adjacency, and equals the transposed forward star,
// (R°)* = (R*)°.  A fact of Mat(S): the *-closure commutes with transpose (over
// MaxPlus these are the latest-start times to the sink).
static_assert(star<4>(materialise<4>(wpath).transpose()) == transpose(Tstar),
              "backward pass = the dagger's closure: (R°)* = (R*)°.");

// ── UNITARY via the Rel DAGGER.  On a BOOL relation the dagger IS the DSL
// @c converse (the intensional coordinate swap, @ref
// dedekind::order::converse), no transfer-local lambda.  A permutation's
// converse is its inverse: P° ; P = Δ.  perm is the cyclic shift, a genuine Ddk
// relation; the relative product is read at the 𝔹 matrix level (materialise
// both, ⊕/⊗ over 𝔹).  Component A's "converse = transpose = adjoint = inverse"
// as one fact; the critical path (a unique optimum) is exactly such a
// permutation.
struct CyclicShift {
  template <typename Pair>
  constexpr bool operator()(const Pair& p) const {
    return static_cast<std::size_t>(p.second) ==
           (static_cast<std::size_t>(p.first) + 1) % 4;
  }
};
inline constexpr auto perm_rel =
    dedekind::sets::Set<Idx2, dedekind::category::ClassicalLogic, CyclicShift>{
        CyclicShift{}};
static_assert(dedekind::order::is_relation(perm_rel),
              "perm_rel is a Ddk relation: the cyclic-shift permutation.");
inline constexpr auto Pmat = materialise<4>(perm_rel);
inline constexpr auto PdaggerMat =
    materialise<4>(dedekind::order::converse(perm_rel));
// P° ; P = Δ over 𝔹, all 16 entries (the relative product via ⊕/⊗): the full
// certificate the is_unitary trait attaches to below, not merely a spot check.
constexpr bool perm_dagger_is_inverse() {
  for (std::size_t i = 0; i < 4; ++i)
    for (std::size_t j = 0; j < 4; ++j)
      if (matmul_entry<4>(PdaggerMat, Pmat, i, j) != (i == j)) return false;
  return true;
}
static_assert(perm_dagger_is_inverse(),
              "a permutation is UNITARY: P° ; P = Δ (converse IS inverse).");

}  // namespace detail_transfer

}  // namespace dedekind::linear_algebra

// ── Wire the @c is_unitary SEED (Component A): the cyclic-shift permutation is
// the first per-carrier witness the seed anticipated "with the linear-algebra
// consumer".  In @c Rel the dagger is the converse, and a permutation's
// converse IS its inverse (P° ; P = Δ), so @c CyclicShift is @b unitary.  This
// collapses "converse = transpose = adjoint = inverse" onto one certificate for
// the bijective case.  (The categorical @c IsUnitary CONCEPT --- dagger +
// inverse + composition, which vary per category --- stays the deferred design
// @c :involution flags; this is its first per-carrier certificate.)
namespace dedekind::category {
// The certificate is DERIVED from the proof, not asserted beside it: it is
// exactly the outcome of the ⊕/⊗ relative product P° ; P compared to Δ.  Change
// CyclicShift to a non-permutation and the trait goes false, honestly.
template <>
struct is_unitary<dedekind::linear_algebra::detail_transfer::CyclicShift,
                  std::size_t>
    : std::bool_constant<
          dedekind::linear_algebra::detail_transfer::perm_dagger_is_inverse()> {
};
static_assert(
    is_unitary_v<dedekind::linear_algebra::detail_transfer::CyclicShift,
                 std::size_t>,
    "Component A: the cyclic-shift permutation's dagger (the converse) is its "
    "inverse --- a unitary arrow of Rel (certificate = the P°;P=Δ "
    "computation).");

// Seed the COMPOSITION law: unitaries are closed under the relative product ; .
// The dagger is contravariant, (A;B)° = B°;A°, so if A° = A⁻¹ and B° = B⁻¹ then
// (A;B)° = (A;B)⁻¹ --- the composite is unitary.  Derived from the factors (the
// relation-algebra ComposePred carries them by TYPE), so it is the structural
// GROUP law, not a per-carrier assertion; it holds regardless of whether the ;
// itself evaluates (the DSL >> is boolean-middle-only, a separate FIXME(#786)).
template <typename PA, typename PB, typename Mid, typename T>
struct is_unitary<dedekind::order::ComposePred<PA, PB, Mid>, T>
    : std::bool_constant<is_unitary_v<PA, T> && is_unitary_v<PB, T>> {};
static_assert(
    is_unitary_v<dedekind::order::ComposePred<
                     dedekind::linear_algebra::detail_transfer::CyclicShift,
                     dedekind::linear_algebra::detail_transfer::CyclicShift>,
                 std::size_t>,
    "unitary is closed under composition (the unitary group): P ; P is unitary "
    "because both factors are.");
}  // namespace dedekind::category
