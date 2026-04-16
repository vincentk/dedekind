/**
 * @file numbers.cppm
 * @module dedekind.numbers
 * @brief Level -1 to 8: The Numerical Tower (ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Structural_Numerics
 * "Liczba nie jest tylko wielkością, lecz punktem w sieci powiązań.
 *  Zrozumieć liczbę to zrozumieć strukturę, w której ona występuje."
 *  (A number is not merely a magnitude, but a point in a network of relations.
 *  To understand a number is to understand the structure in which it occurs.)
 *  — Zdzisław Pawlak
 *
 * @details
 * This module reifies the standard numerical species as formal Categorical
 * objects. Following the Dedekind tradition, we do not treat numbers as
 * "built-in" primitives, but as specific positions within an algebraic
 * and order-theoretic hierarchy.
 *
 * @note "In dedekind.numbers, structure is clarified by explicit composition
 * and typed interfaces." (Module-specific documentation note for maintainers.)
 *       -- dedekind maintainers
 */

export module dedekind.numbers;

/** @section Discrete_Foundations (Level -1 & 0) */
export import :booleans;  // Truth<L> and Ω
export import :naturals;  // ℕ (Unsigned species)
export import :integer;   // ℤ (Signed species)

/** @section Algebraic_Extensions (Level 3 & 8) */
export import :rational;    // ℚ (The Quotient Field)
export import :complex;     // ℂ (The Cayley-Dickson construction)
export import :quaternion;  // ℍ (Hamilton's division ring)
export import :scalars;     // Floating-point anchors
export import :lattice;     // Lattices over ℝ and ℂ
export import :mandelbrot;  // Mandelbrot recurrence/orbit helpers

/** @section Non_Standard_Analysis */
export import :dual;      // Automatic Differentiation / Infinitesimals
export import :symbolic;  // Formal symbols / Indeterminates
export import :real;      // ℝ (The Dedekind Cut / Cauchy continuum)

/** @section Metadata */
export import :constants;  // π, e, γ, etc.
