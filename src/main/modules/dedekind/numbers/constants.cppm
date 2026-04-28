/**
 * @file dedekind/numbers/constants.cppm
 * @partition :constants
 * @brief Reserved partition for genuine Dedekind-cut realisations of
 *        named real constants ($\sqrt{2}$, $e$, $\pi$, $\gamma$, …).
 *        Currently empty (no exports).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Aus dem Paradies, das Cantor uns geschaffen, soll uns niemand
 * vertreiben konnen."
 *       ("No one shall expel us from the paradise that Cantor has created
 * for us.")
 *       -- David Hilbert, Ueber das Unendliche (1926)
 */

module;

/**
 * @file dedekind/numbers/constants.cppm
 * @partition :constants
 *
 * @section Description
 * This partition is reserved for cut-based realisations of $\sqrt{2}$, $e$,
 * $\pi$, $\gamma$, $\zeta(3)$, and similar named real constants. The
 * earlier @c Sqrt2() / @c E() / @c Pi() entries returning hardcoded
 * @c Real<double>{1.41421356237} etc. were @b not Dedekind cuts --- they
 * were @c double literals dressed in the @c Real<double> type and
 * mislabelled as "transcendental constants" (note that $\sqrt{2}$ is
 * algebraic, not transcendental). Removed during the \#379 alignment
 * sweep; see the @c FIXME(\#379) breadcrumb in the body for the
 * reinstatement plan.
 *
 * @quote
 * "Ce que nous connaissons est peu de chose; ce que nous ignorons est
 * immense."
 * ("What we know is little; what we do not know is immense.")
 * -- Pierre-Simon Laplace
 */
export module dedekind.numbers:constants;

import :real;

namespace dedekind::numbers {

// FIXME(#379): partition reserved for genuine Dedekind-cut realisations
// of named real constants ($\sqrt{2}$, $e$, $\pi$, $\gamma$, $\zeta(3)$,
// ...).  The earlier `Sqrt2()` / `E()` / `Pi()` returning hardcoded
// `Real<double>{1.41421356237}` etc. were @b not Dedekind cuts ---
// they were `double` literals dressed in the `Real<double>` type and
// labelled misleadingly as "transcendental constants" (note that
// $\sqrt{2}$ is algebraic, not transcendental).  Removed during the
// #379 alignment sweep.  Genuine cut realisations would build each
// constant as a predicate set over $\mathbb{Q}$ (e.g.\ Leibniz /
// Euler series anchors for $\pi$ and $e$, the algebraic predicate
// $\{q \in \mathbb{Q} : q \cdot q < 2\}$ for $\sqrt{2}$); the
// machinery for such cuts lives in `:real`.

}  // namespace dedekind::numbers
