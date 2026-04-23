/**
 * @file dedekind/linear_algebra/embeddings.cppm
 * @partition :embeddings
 * @brief Level 12.5c: Canonical regular representations ℂ ↪ M₂, 𝔻 ↪ M₂.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Overview
 * Two faithful ring homomorphisms, both spelled over the same 2×2 carrier
 * `Matrix2x2V<T>` from `:matrix`:
 *
 *   ℂ ↪ M₂(T):   a + b·i  ↦  [[ a, -b ],
 *                              [ b,  a ]]
 *
 *   𝔻 ↪ M₂(T):   a + b·ε  ↦  [[ a,  b ],
 *                              [ 0,  a ]]
 *
 * The complex embedding is the classical regular representation of ℂ as
 * rotation-and-scaling matrices; the dual embedding is the upper-triangular
 * regular representation of 𝔻 = T[ε]/(ε²), with the nilpotent ε ↦ strict
 * upper-triangular block.
 *
 * Paper-facing carrier is `T = Rational<long>` (ℚ as an exact proxy for ℝ
 * per the #364 / #366 narrowing). The two embeddings are witnessed by
 * `static_assert`s that each map preserves the additive and multiplicative
 * units and commutes with + and ·.
 *
 * Wikipedia: Regular representation, Dual numbers, Automatic differentiation
 *
 * @note "Tout est nombre." — Pythagoras (attrib., via Aristotle,
 *       Metaphysics A.5) [Trans: "All is number."]
 */
module;

#include <concepts>

export module dedekind.linear_algebra:embeddings;

import dedekind.algebra;  // IsRingLike + IsRingLikeHomomorphism (constraint / witness)
import dedekind.numbers;  // Complex<T>, Dual<T>, Rational<Z>, IsComplexScalar
import :matrix;  // Matrix2x2V<T>, identity / zero constants for the target

namespace dedekind::linear_algebra {

using dedekind::numbers::Complex;
using dedekind::numbers::Dual;

/** @section Canonical_Embedding_ℂ_↪_M₂ */

/**
 * @brief Regular representation of ℂ inside M₂(T):
 *        `a + b·i ↦ [[a, -b], [b, a]]`.
 *
 * An injective ring homomorphism: preserves +, ·, 0, and 1. The image is
 * exactly the subring of `rotation-and-scaling` 2×2 matrices — matrices
 * commuting with the canonical `J = [[0, -1], [1, 0]]` and closed under
 * matrix addition and multiplication. Over `T = Rational<Z>` the map is
 * exact and invertible on its image via `complex_from_matrix2x2`.
 *
 * @tparam T A carrier supporting `+`, `-` (unary and binary), and `*` in the
 *           sense of `IsComplexScalar`.
 */
export template <typename T>
  requires dedekind::numbers::IsComplexScalar<T> &&
           dedekind::algebra::IsRingLike<T>
constexpr Matrix2x2V<T> as_matrix2x2(const Complex<T>& z) {
  // `IsRingLike<T>` guarantees unary negation on `T`, and the target
  // `Matrix2x2V<T>` is itself bounded on `IsRingLike<T>`, so the natural
  // spelling `-z.imag()` is exactly in the required surface.
  return {z.real(), -z.imag(), z.imag(), z.real()};
}

/**
 * @brief Left inverse of `as_matrix2x2` on matrices of complex form.
 * @details Recovers `(a, b)` from `[[a, -b], [b, a]]` by reading the first
 *          column. `complex_from_matrix2x2(as_matrix2x2(z)) == z` for every
 *          `z`; the opposite direction is only true when `M` is in the image
 *          of ℂ ↪ M₂(T). The caller is responsible for that precondition.
 */
export template <typename T>
  requires dedekind::numbers::IsComplexScalar<T> &&
           dedekind::algebra::IsRingLike<T>
constexpr Complex<T> complex_from_matrix2x2(const Matrix2x2V<T>& M) {
  return Complex<T>{M.m11, M.m21};
}

/** @section Canonical_Embedding_𝔻_↪_M₂ */

/**
 * @brief Regular representation of 𝔻 inside M₂(T):
 *        `a + b·ε ↦ [[a, b], [0, a]]`.
 *
 * An injective ring homomorphism from the dual numbers 𝔻 = T[ε]/(ε²) into
 * upper-triangular 2×2 matrices with equal diagonal. The nilpotent relation
 * `ε² = 0` lifts exactly to `[[0, 1], [0, 0]]² = [[0, 0], [0, 0]]`.
 * Preserves +, ·, 0, and 1; witnessed below.
 *
 * Forward-mode automatic differentiation (already reified on `Dual<F>`) is
 * structurally the action of this embedding: applying a polynomial to
 * `Dual` equals applying the polynomial to its matrix image.
 *
 * @tparam T A carrier with ring-like arithmetic compatible with `Dual<T>`.
 */
export template <typename T>
  requires std::regular<T> && dedekind::algebra::IsRingLike<T>
constexpr Matrix2x2V<T> as_matrix2x2(const Dual<T>& d) {
  // `std::regular<T>` matches `Dual<F>`'s own constraint; `IsRingLike<T>`
  // is additionally required because the target `Matrix2x2V<T>` carries
  // that bound.
  return {d.value(), d.derivative(), T{0}, d.value()};
}

/**
 * @brief Left inverse of `as_matrix2x2` on matrices of dual form.
 * @details Recovers `(a, b)` from `[[a, b], [0, a]]`. Precondition: `M` is
 *          in the image of 𝔻 ↪ M₂(T) — i.e. `m11 == m22` and `m21 == 0`.
 *          Not enforced; see the `complex_from_matrix2x2` note.
 */
export template <typename T>
  requires std::regular<T> && dedekind::algebra::IsRingLike<T>
constexpr Dual<T> dual_from_matrix2x2(const Matrix2x2V<T>& M) {
  return Dual<T>{M.m11, M.m12};
}

/** @section Ring_Homomorphism_Witnesses_over_ℚ
 *
 *  The paper-facing existential proofs that both embeddings are ring
 *  homomorphisms — exact at compile time over `Rational<long>`.
 */
namespace detail {

using Rat = dedekind::numbers::Rational<long>;

// Two concrete ℚ-valued complex numbers with non-trivial real and imaginary
// parts, chosen so that z*w has distinct real and imaginary components and
// the homomorphism property is not satisfied by a symmetry coincidence.
inline constexpr Complex<Rat> z_probe{Rat{1L}, Rat{2L}};
inline constexpr Complex<Rat> w_probe{Rat{3L}, Rat{-5L}};

static_assert(as_matrix2x2(Complex<Rat>{Rat{0L}, Rat{0L}}) ==
                  zero_matrix2x2_v<Rat>,
              "ℂ ↪ M₂(ℚ): the complex zero maps to the matrix zero.");
static_assert(as_matrix2x2(Complex<Rat>{Rat{1L}, Rat{0L}}) ==
                  identity_matrix2x2_v<Rat>,
              "ℂ ↪ M₂(ℚ): the complex unit maps to the matrix identity.");
static_assert(as_matrix2x2(z_probe + w_probe) ==
                  as_matrix2x2(z_probe) + as_matrix2x2(w_probe),
              "ℂ ↪ M₂(ℚ) preserves addition.");
static_assert(as_matrix2x2(z_probe* w_probe) ==
                  as_matrix2x2(z_probe) * as_matrix2x2(w_probe),
              "ℂ ↪ M₂(ℚ) preserves multiplication.");
static_assert(complex_from_matrix2x2(as_matrix2x2(z_probe)) == z_probe,
              "ℂ ↪ M₂(ℚ) admits a left inverse on its image.");

// Two concrete ℚ-valued dual numbers. `d_probe` has a non-zero derivative
// so the matrix is strictly upper-triangular (not diagonal), and the
// product mixes the nilpotent part non-trivially.
inline constexpr Dual<Rat> d_probe{Rat{2L}, Rat{3L}};
inline constexpr Dual<Rat> e_probe{Rat{-1L}, Rat{5L}};

static_assert(as_matrix2x2(Dual<Rat>{Rat{0L}, Rat{0L}}) ==
                  zero_matrix2x2_v<Rat>,
              "𝔻 ↪ M₂(ℚ): the dual zero maps to the matrix zero.");
static_assert(as_matrix2x2(Dual<Rat>{Rat{1L}, Rat{0L}}) ==
                  identity_matrix2x2_v<Rat>,
              "𝔻 ↪ M₂(ℚ): the dual unit maps to the matrix identity.");
static_assert(as_matrix2x2(d_probe + e_probe) ==
                  as_matrix2x2(d_probe) + as_matrix2x2(e_probe),
              "𝔻 ↪ M₂(ℚ) preserves addition.");
static_assert(as_matrix2x2(d_probe* e_probe) ==
                  as_matrix2x2(d_probe) * as_matrix2x2(e_probe),
              "𝔻 ↪ M₂(ℚ) preserves multiplication.");
static_assert(dual_from_matrix2x2(as_matrix2x2(d_probe)) == d_probe,
              "𝔻 ↪ M₂(ℚ) admits a left inverse on its image.");

// Nilpotency of ε lifts exactly to the matrix world.
inline constexpr Dual<Rat> eps_q{Rat{0L}, Rat{1L}};
static_assert(as_matrix2x2(eps_q* eps_q) == zero_matrix2x2_v<Rat>,
              "ε² = 0 in 𝔻 lifts to the zero matrix under the embedding.");

/** @section Ring_Homomorphism_Concept_Witnesses
 *
 *  The two embeddings witness `dedekind.algebra::IsRingLikeHomomorphism`
 *  at the type level, folding the four preservation static_asserts above
 *  (0, 1, +, ·) into a single structural claim. Each wrapper struct
 *  promotes the function template to a callable object so the concept can
 *  bind `Phi` to a concrete type.
 */

struct ComplexToMatrix2x2Hom {
  constexpr Matrix2x2V<Rat> operator()(const Complex<Rat>& z) const {
    return as_matrix2x2(z);
  }
};

struct DualToMatrix2x2Hom {
  constexpr Matrix2x2V<Rat> operator()(const Dual<Rat>& d) const {
    return as_matrix2x2(d);
  }
};

static_assert(dedekind::algebra::IsRingLikeHomomorphism<
                  ComplexToMatrix2x2Hom, Complex<Rat>, Matrix2x2V<Rat>>,
              "ℂ ↪ M₂(ℚ) is structurally a ring-like homomorphism — the "
              "four preservation laws above pin its value-level content.");

static_assert(dedekind::algebra::IsRingLikeHomomorphism<
                  DualToMatrix2x2Hom, Dual<Rat>, Matrix2x2V<Rat>>,
              "𝔻 ↪ M₂(ℚ) is structurally a ring-like homomorphism — the "
              "four preservation laws above pin its value-level content.");

}  // namespace detail

}  // namespace dedekind::linear_algebra
