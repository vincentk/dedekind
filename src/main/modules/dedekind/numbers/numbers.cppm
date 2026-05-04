/**
 * @file numbers.cppm
 * @brief Level -1 to 8: The Numerical Tower (ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section numbers__Structural_Numerics
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
 * @note "Meine Methoden sind wirklich Methoden des Arbeitens und Denkens;
 * deshalb sind sie überall anonym eingedrungen." — Emmy Noether, letter to
 * Helmut Hasse (1931), cited in Auguste Dick, *Emmy Noether, 1882-1935* (1981).
 * [Trans: "My methods are really methods of working and thinking; this is why
 * they have crept in everywhere anonymously."]
 */

export module dedekind.numbers;

/** @section numbers__Ordered_And_Convex_Structures */
export import dedekind.order;  // Ordered predicate concepts for numeric species
export import dedekind.topology;  // Rays, half-spaces, intervals, convex shapes

/** @section numbers__Discrete_Foundations */
export import :boolean;   // Truth<L> and Ω
export import :uint;      // std::unsigned_integral as ℤ/2^wℤ
                          // (machine layer below ℕ; closes part of #417)
export import :natural;   // Cardinality partition (carrier:
                          // dedekind::sets::Cardinality)
export import :integer;   // SignedExtensionalCardinal<> partition (variant
                          // ℤ-proxy / IntegersOf<>)
export import :sint;      // std::signed_integral: operator surface ✓,
                          // axiomatic ring ✗ (Honest Rejection;
                          // closes part of #418)
export import :integral;  // std::integral umbrella note over the three
                          // siblings (unsigned_integral / signed_integral
                          // / bool); closes #419
export import :floating_point;  // std::floating_point: operator surface +
                                // operational ordered-field ✓; axiomatic ring /
                                // field / total order ✗ (IEEE 754 Honest
                                // Rejection); closes #420

/** @section numbers__Algebraic_Extensions */
export import :rational;    // ℚ (The Quotient Field)
export import :complex;     // ℂ (The Cayley-Dickson construction)
export import :quaternion;  // ℍ (Hamilton's division ring)
export import :scalars;     // Floating-point anchors
export import :lattice;     // Lattices over RealsOf<> and ℂ
export import :mandelbrot;  // Mandelbrot recurrence/orbit helpers

/** @section numbers__Non_Standard_Analysis */
// :dual moved to dedekind.analysis:dual at PR #513 --- the carrier is
// fundamentally differential (forward-mode AD) and its natural neighbours
// (:ftc, :forms, :hamilton) live in :analysis.  Consumers should
// `import dedekind.analysis;` (downstream of :numbers in the build graph)
// to reach Dual<F> and the 𝔻 alias.
export import :symbolic;  // Formal symbols / Indeterminates
export import :real;      // ℝ (universe value Ω<Real<machine_real_scalar>,
                          // ClassicalLogic, ℶ_1>); RealsOf<> / RealSet (the
                      // cross-carrier classifier); R (classifier instance) —
                      // Dedekind Cut / Cauchy continuum
export import :cardinality;  // Draft finite/aleph0 carrier and policies

/** @section numbers__Metadata */
export import :constants;  // π, e, γ, etc.
