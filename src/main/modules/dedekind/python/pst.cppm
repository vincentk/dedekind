/**
 * @file dedekind/python/pst.cppm
 * @partition :pst
 * @brief Python's @c int as a bounded chain: the 64-bit window and its ends.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

module;

#include <stdexcept>  // std::overflow_error at the window's end
#include <string>

export module dedekind.python:pst;

export namespace dedekind::python {

/** @brief The integer carrier the Python surface shares between the arrows
 *  and the sets: @c long @c long, what a Python @c int crosses the boundary
 *  as, a bounded chain.  One carrier, so an arrow's image of a set is
 *  well-typed. */
using Int = long long;

/** @brief The window's end.  Python's @c int is ℤ; @c long @c long is its
 *  64-bit window, on which the step is not closed.  Where ℤ continues and the
 *  window cannot, the surface raises (@c std::overflow_error, Python's
 *  @c OverflowError) rather than wrap --- ℕ's proxy saturates to ℵ₀ instead,
 *  the total posture.  FIXME(#1008): an erased composite (@c succ @c >>
 *  @c succ) steps unchecked; the carrier that closes this is
 *  @c SignedCardinality.
 *  @param what the operation that reached the end. */
[[noreturn]] inline void window_end(const char* what) {
  throw std::overflow_error(std::string(what) +
                            ": the 64-bit window ends here; ℤ does not");
}
/** @brief @c a @c + @c b inside the window, or @c window_end.
 *  @param a a value in the window.  @param b the offset.
 *  @param what the operation, for the message.  @return the sum. */
constexpr Int add_in_window(Int a, Int b, const char* what) {
  Int sum{};
  if (__builtin_add_overflow(a, b, &sum)) window_end(what);
  return sum;
}

}  // namespace dedekind::python
