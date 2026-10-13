/**
 * @file dedekind/python/python.cppm
 * @brief Curated binding facade for external runtimes.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section python__Description
 * The last layer of the build chain: what the Python bindings hold, and nothing
 * the library does not already say: @c materialise, the one stored form of a
 * term, and @c jlt, the arrows every extension reads, with the 64-bit window's
 * ends.  A fragment only one extension reads lives in that extension (@c lwv:
 * @c nanobind_lwv.cpp, @c pst: @c nanobind_pst.cpp).  Reduction stays in C++.
 *
 * @note "Le vrai n'est pas le tout, mais le tout dans sa structure."
 *       -- Gaston Bachelard, paraphrase
 *       [Trans: "Truth is not the whole, but the whole in its structure."]
 */

module;

#include <concepts>
#include <functional>
#include <limits>     // the integer window's ends
#include <stdexcept>  // std::overflow_error at the window's end
#include <string>

export module dedekind.python;

import dedekind.category;
import dedekind.numbers; // the carriers' order registrations
import dedekind.order;
import dedekind.sets;

export namespace dedekind::python {

// The library's names, said once for the partition; the fragments below add
// only the handles' erasure and the window.
using dedekind::category::cata;
using dedekind::category::Dom;
using dedekind::category::HasNNOStep;
using dedekind::category::IsEndomorphism;
using dedekind::category::IsOckhamAlgebra;
using dedekind::category::logic_complement;
using dedekind::category::Morphism;
using dedekind::category::Predecessor;
using dedekind::category::Successor;
using dedekind::order::Runs;

// ── Jlt: a fluent DSL over the real category arrows (#961) ────────────────
//
// The Jlt exhibit exposes the ACTUAL @c :morphism arrows over TWO objects,
// @c bool and @c int, as @b duck-typed handles: compose with @c >>, apply with
// @c (), inspect @c dom / @c cod.  There is no bespoke term class; an "arrow"
// is the @b protocol (@c dom / @c cod / call / @c >>), witnessed by @c IsArrow
// on the C++ side.
//
// The generators are @b involutive @b endomorphisms: @c id and @c refl.  @c
// refl is @c :logic's @b pre-configured reflection @c logic_complement<L> (@c
// L::RFL, already witnessed an involution there): @c ¬ on @c bool (@c Boole)
// and the order-reversing @c ~ on @c int (@c Chain<int>).  Under @c >> they
// generate
// @c ℤ/2 per object --- every element is self-inverse (@c IsInvolution).
//
// Python arrows are @b extensional (functions), so a composition type-erases to
// a @c Morphism<T,T>.  But it is NOT hand-rolled: @c compose builds @c
// :morphism's
// @c Compose (@c operator>>) and runs it through @c :f_algebra's @c cata ---
// the ONE reducer engine --- then type-erases the reduced arrow.  So a Python
// composition @b dispatches to the F-algebra reducer (@c id>>refl folds to
// @c refl); the result is extensional, but structurally reduced.  Composability
// (@c cod(f) @c = @c dom(g)) is enforced @b structurally --- a @c bool-arrow
// cannot compose with an @c int-arrow (their types do not match).

namespace jlt {

/** @brief The exhibit's type-erased arrow @c T→T: the real @c :morphism
 *  @c Morphism with a @c std::function transform.  This IS a category arrow
 *  (@c IsArrow / @c IsEndomorphism hold), not a bespoke wrapper.
 *  @tparam T the carrier. */
template <typename T>
using Arrow = Morphism<T, T, std::function<T(T)>>;

/** @brief @c refl on a chain: @c :logic's pre-configured reflection
 *  @c logic_complement<L> (@c L::RFL: @c ¬ on @c Boole, the order-reversing
 *  @c ~ on @c Chain<Int>), already witnessed an involution there, erased.
 *  @tparam L the species whose @c Ω is the carrier. */
template <IsOckhamAlgebra L>
Arrow<typename L::Ω> refl() {
  using T = typename L::Ω;
  return Arrow<T>{std::function<T(T)>{logic_complement<L>{}}};
}

/** @brief The integer carrier the Python surface shares between the arrows
 *  (@c jlt), the sets (@c lwv) and the chains (@c pst): @c long @c long, what a
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
/** @brief A step arrow applied inside the window: total on @c bool, and on
 *  the integer window raising at the end it would cross rather than
 *  overflowing.  @tparam T the carrier.  @tparam Step the successor or the
 *  predecessor on it.  @param f the arrow.  @param x the argument.
 *  @return @c f(x). */
template <HasNNOStep T, typename Step>
  requires std::same_as<Step, Successor<T>> ||
           std::same_as<Step, Predecessor<T>>
T step_in_window(const Step& f, T x) {
  if constexpr (std::same_as<T, Int>) {
    constexpr bool up = std::same_as<Step, Successor<T>>;
    if (x == (up ? std::numeric_limits<Int>::max()
                 : std::numeric_limits<Int>::min()))
      window_end(up ? "succ" : "pred");
  }
  return f(x);
}

/** @brief Composition @c f @c >> @c g (apply @c f, then @c g) of two
 * same-object endomorphisms, as a type-erased arrow.  It REUSES the real
 * machinery --- no hand-rolled @c g(f(x)): it builds @c :morphism's @c Compose
 * via @c operator>> and runs it through @c :f_algebra's @c cata (the ONE
 * reducer engine), then type-erases the reduced arrow.  So a Python composition
 * @b dispatches to the F-algebra reducer; the result is extensional (a
 * function), but structurally reduced (e.g. @c id>>refl folds to @c refl via @c
 * cata's unit law).  The
 *  @c same_as<Dom<F>,Dom<G>> constraint IS the composability law (@c cod(f) @c
 * =
 *  @c dom(g) for endomorphisms): a cross-object compose does not type-check. */
template <IsEndomorphism F, IsEndomorphism G>
  requires std::same_as<Dom<F>, Dom<G>>
inline Arrow<Dom<F>> compose(const F& f, const G& g) {
  using T = Dom<F>;
  return Arrow<T>{std::function<T(T)>{cata(f >> g)}};
}

}  // namespace jlt

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
