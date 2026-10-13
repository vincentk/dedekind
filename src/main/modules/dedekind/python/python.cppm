/**
 * @file dedekind/python/python.cppm
 * @brief Curated binding facade for external runtimes.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section python__Description
 * The last layer of the build chain: what more than one Python extension
 * reads, and nothing the library does not already say.  That is the integer
 * carrier and its 64-bit window's ends, the erased arrow, and @c materialise,
 * the one stored form of a term.  Each fragment lives in its extension
 * (@c jlt: @c nanobind_jlt.cpp, @c lwv: @c nanobind_lwv.cpp, @c pst:
 * @c nanobind_pst.cpp).  Reduction stays in C++.
 *
 * @note "Le vrai n'est pas le tout, mais le tout dans sa structure."
 *       -- Gaston Bachelard, paraphrase
 *       [Trans: "Truth is not the whole, but the whole in its structure."]
 */

module;

#include <functional>
#include <stdexcept>  // std::overflow_error at the window's end
#include <string>

export module dedekind.python;

import dedekind.category;
import dedekind.numbers; // the carriers' order registrations
import dedekind.order;
import dedekind.sets;

export namespace dedekind::python {

// What crossing the boundary needs, and more than one extension reads: the
// integer carrier and its window, the erased arrow, the stored form of a term.
using dedekind::category::Morphism;
using dedekind::order::Runs;

/** @brief The type-erased arrow @c T→T every extension holds: the real
 *  @c :morphism @c Morphism with a @c std::function transform.  This IS a
 *  category arrow (@c IsArrow / @c IsEndomorphism hold), not a bespoke wrapper.
 *  @tparam T the carrier. */
template <typename T>
using Arrow = Morphism<T, T, std::function<T(T)>>;

/** @brief The integer carrier the Python surface shares between the arrows
 *  (@c jlt) and the sets (@c lwv): @c long @c long, what a
 *  Python @c int crosses the boundary as.  One carrier, so an arrow's image of
 *  a set is well-typed. */
using Int = long long;

/** @brief The window's end.  Python's @c int is ℤ; @c long @c long is its
 *  64-bit window, on which the step is not closed.  Where ℤ continues and the
 *  window cannot, the surface raises (@c std::overflow_error, Python's
 *  @c OverflowError) rather than wrap --- ℕ's proxy saturates to ℵ₀ instead,
 *  the total posture (@c pst).  FIXME(#1008): an erased composite
 *  (@c succ @c >> @c succ) steps unchecked; the carrier that closes this is
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

/** @brief A term's normal form, materialised for a handle: the one stored
 *  form, a run list over the term's carrier and species.  @param t the term.
 */
template <typename T>
auto materialise(const T& t) {
  using S = decltype(dedekind::order::steps(t));
  return Runs<typename S::Domain, typename S::logic_species>(
      dedekind::order::steps(t));
}

}  // namespace dedekind::python
