/**
 * @file dedekind/numbers/constants.cppm
 * @partition :constants
 * @brief Module interface in the dedekind hierarchy.
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
 * @brief Level 8.3: Named numeric constants exposed as Real<double> values.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * This partition currently exports pragmatic literal approximations for
 * common constants used throughout tests and examples.
 *
 * - Sqrt2() -> sqrt(2) approximation
 * - E()     -> Euler's number approximation
 * - Pi()    -> pi approximation
 *
 * Exact constructive encodings (e.g. via cuts or convergent symbolic
 * sequences) are tracked separately as follow-up work.
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
