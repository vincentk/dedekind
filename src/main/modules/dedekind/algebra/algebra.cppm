/**
 * @file algebra.cppm
 * @module dedekind.algebra
 * @brief Level 3: The Algebra of Restoration (Groups ⊂ Rings ⊂ Modules ⊂
 * Polynomials).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section The_Etymology_of_Balance
 * "الكتاب المختصر في حساب الجبر والمقابلة"
 * (The Compendious Book on Calculation by Completion and Balancing)
 * — Muḥammad ibn Mūsā al-Khwārizmī
 *
 * @subsection The_Restoration
 * "...قصدت به الجبر والمقابلة، ومسائل من الحساب يحتاج الناس إليها في مواريثهم
 * ووصاياهم..."
 * ("...I have intended it [the book] for the restoration and the balancing,
 *  and for those problems of calculation which men require in their
 *  inheritances and legacies...")
 *
 * @details
 * Derived from 'al-jabr' (الجبر), meaning "the restoration" or "reunion."
 * Algebra in the Dedekind topos is the study of operations that preserve
 * structural symmetry.
 *
 * This module reifies the fundamental algebraic species, ensuring that
 * every mapping (Morphism) respects the laws of composition, identity,
 * and inversion. By 'restoring' these laws to C++ primitives, we enable
 * the compiler to perform symbolic reductions—the automated 'balancing'
 * of the mathematical equation.
 *
 * @section The_Algebraic_Stack
 * - **Group (@ref group)**: The logic of Inversion and Symmetry.
 * - **Rings (@ref ring)**: The dual structure of Addition and Multiplication.
 * - **Division (@ref division)**: The Euclidean Algorithm and the GCD Morphism.
 * - **Modules (@ref modules)**: Linear actions of Rings on additive Groups.
 * - **Polynomials (@ref polynomials)**: Formal power series R[x] and
 * extensions.
 */
export module dedekind.algebra;

export import :boolean;
export import :division;
export import :field;
export import :group;
export import :modules;
export import :polynomial;
export import :ring;
