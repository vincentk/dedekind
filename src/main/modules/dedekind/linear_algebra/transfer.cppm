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

#include <concepts>
#include <cstddef>
#include <functional>
#include <type_traits>

export module dedekind.linear_algebra:transfer;

import dedekind.algebra;  // IsSemiring, semiring_ops, Tropical/MaxPlus, ⊕/⊗
import dedekind.category; // IsArrow, IsSemiring, identity_v
import dedekind.order;    // IsDirectedSet
import :diagonal;         // OuterProduct — the rank-1 dyad carrier

namespace dedekind::linear_algebra {

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
  S acc = dedekind::category::identity_v<S, Add>;  // ⊕-identity (0-bar)
  for (std::size_t k = 0; k < N; ++k)
    acc = Add{}(acc, Mult{}(w(static_cast<WD>(k)), v(static_cast<VD>(k))));
  return acc;
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
  S acc = dedekind::category::identity_v<S, Add>;
  for (std::size_t k = 0; k < N; ++k)
    acc = Add{}(acc, Mult{}(a(i, k), b(k, j)));
  return acc;
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

}  // namespace detail_transfer

}  // namespace dedekind::linear_algebra
