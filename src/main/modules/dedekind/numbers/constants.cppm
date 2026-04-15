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
 */
export module dedekind.numbers:constants;

import :real;

namespace dedekind::numbers {

export constexpr auto Sqrt2() { return Real<double>{1.41421356237}; }
export constexpr auto E() { return Real<double>{2.71828182846}; }
export constexpr auto Pi() { return Real<double>{3.14159265359}; }

}  // namespace dedekind::numbers
