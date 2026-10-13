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
 * term, and the two fragments that more than one extension reads, @c jlt
 * (arrows) and @c lwv (sets over the integer window).  Each adds only what
 * crossing the boundary needs: the type erasure to one handle type per
 * fragment, and the 64-bit window's ends.  A fragment only one extension reads
 * lives in that extension (@c pst: @c nanobind_pst.cpp).  Reduction stays in
 * C++.
 *
 * @note "Le vrai n'est pas le tout, mais le tout dans sa structure."
 *       -- Gaston Bachelard, paraphrase
 *       [Trans: "Truth is not the whole, but the whole in its structure."]
 */

module;

#include <concepts>
#include <functional>
#include <limits>     // the integer window's ends
#include <optional>   // the cover, the first element of a set
#include <ranges>     // a translated set's steps, as a view
#include <stdexcept>  // std::overflow_error at the window's end
#include <string>
#include <utility>

export module dedekind.python;

import dedekind.category;
import dedekind.numbers; // the carriers' order registrations
import dedekind.order;
import dedekind.sets;

export namespace dedekind::python {

// The library's names, said once for the partition; the fragments below add
// only the handles' erasure and the window.
using dedekind::category::Boole;
using dedekind::category::cata;
using dedekind::category::Dom;
using dedekind::category::HasNNOStep;
using dedekind::category::Identity;
using dedekind::category::IsEndomorphism;
using dedekind::category::IsOckhamAlgebra;
using dedekind::category::logic_complement;
using dedekind::category::Morphism;
using dedekind::category::Predecessor;
using dedekind::category::Successor;
using dedekind::order::Direction;
using dedekind::order::Interval;
using dedekind::order::Position;
using dedekind::order::Runs;
using dedekind::order::Side;
using dedekind::order::Strictness;
using dedekind::sets::IsSetObject;

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

// ── Lwv: the scalar set-comprehension language (#965, paper §3)
// ───────────────
//
// Lwv := Jlt ∩ Set (paper.tex:158): intensional sets over a Jlt carrier, a set
// being a membership test @c {x ∈ S | P(x)} rather than a listing.  The
// canonical README exhibit is the two overlapping halfspaces
// @c {x>3} ∩ {x<5} = {4}: a halfspace @c ℕ|(χ ⋈ k) is the principal
// filter/ideal @c ↑k / @c ↓k of the carrier order, and its meet routes through
// the @b real lattice reducer: the meet of two intervals is the value-first
// crossing law @c ↑a ∩ ↓b = [a,b] (collapsing to a point over an integral
// carrier), and unions and complements are run lists (@c order:runs), the
// normal form of the sets of a chain built from intervals.  This exhibit is the
// SET twin of the @c jlt ARROW exhibit above: it binds the real reducer, NOT a
// Python-side reimplementation (handle-only).

/** @brief A term's normal form, materialised for a handle: the one stored
 *  form, a run list over the term's carrier and species.  @param t the term.
 */
template <typename T>
auto materialise(const T& t) {
  using S = decltype(dedekind::order::steps(t));
  return Runs<typename S::Domain, typename S::logic_species>(
      dedekind::order::steps(t));
}

namespace lwv {

/** @brief A value-based set handle (#965): a materialised run list over
 *  @c long @c long (Python's @c int, ℤ read through the 64-bit window), its
 *  runs by start, so one constructor covers all pivots at runtime.  Its @c &,
 *  @c | and @c ~ build the library's term and materialise its normal form. */
using Set = Runs<long long>;
/** @brief One run of a set: an interval. */
using Leaf = Interval<long long>;
// Whatever crosses the Python boundary is a set OBJECT --- (reified universe,
// χ) --- and this is the compile-time MUST for it (RFC 2119).
static_assert(IsSetObject<Set>,
              "the Python surface's Set must be a set object (IsSetObject).");

/** @brief The rays @c {x ⋈ k}: the principal filter ↑k and ideal ↓k, open or
 *  closed.  @tparam D the direction.  @tparam S the strictness.  @param k the
 *  pivot. */
template <Direction D, Strictness S>
Set ray(long long k) {
  return materialise(Leaf::half(k, D, S));
}
/** @brief The point @c {k}.  @param k the point. */
inline Set point(long long k) { return materialise(Leaf::point(k)); }
/** @brief The lattice on handles: the library's term, materialised. */
inline Set meet(const Set& a, const Set& b) { return materialise(a & b); }
inline Set join(const Set& a, const Set& b) { return materialise(a | b); }
inline Set complement(const Set& a) { return materialise(~a); }

/** @brief A set translated by @c k, as a source: every step moved, the image
 *  under the order automorphism @f$x \mapsto x + k@f$ of ℤ.  Where the image
 *  leaves the window --- a step past its last value, or members moved below
 *  its first --- ℤ continues and the window cannot, so it raises rather than
 *  truncate. */
struct Shifted {
  using Domain = long long;
  using logic_species = Boole;
  using Step = dedekind::order::Step<long long, Boole>;
  /** @brief One step moved by @c k, inside the window or raising. */
  struct By {
    long long k;
    Step operator()(Step st) const {
      using W = std::numeric_limits<long long>;
      st.at.at = jlt::add_in_window(st.at.at, k, "image");
      if (st.value ? st.at == Position<long long>{W::max(), Side::Above}
                   : st.at == Position<long long>{W::min(), Side::Below})
        jlt::window_end("image");
      st.at = dedekind::order::canonical(st.at);
      return st;
    }
  };
  const Set* s;
  long long k;
  bool first() const { return s->first(); }
  auto steps() const { return std::views::transform(s->steps(), By{k}); }
};
/** @brief The arrows whose image of a set has a closed form: the identity, the
 *  successor and the predecessor on the integer window.  An opaque composed
 *  arrow has no such normal form; its image is intensional (Kleene-valued) and
 *  is refused at the boundary rather than guessed.  @tparam F the arrow. */
template <typename F>
concept IsStructuralArrow = std::same_as<F, Identity<jlt::Int>> ||
                            std::same_as<F, Successor<jlt::Int>> ||
                            std::same_as<F, Predecessor<jlt::Int>>;
/** @brief How far the arrow moves a step: 0, +1, −1.  @tparam F the arrow. */
template <IsStructuralArrow F>
consteval long long offset() {
  if constexpr (std::same_as<F, Successor<jlt::Int>>)
    return 1;
  else if constexpr (std::same_as<F, Predecessor<jlt::Int>>)
    return -1;
  else
    return 0;
}
/** @brief @f$f(S)@f$ and @f$f^{-1}(S)@f$ for a structural arrow, in closed
 *  form (paper §4: the image of @c {n > 5} under the successor is @c {n > 6},
 *  decided with no search of the domain).  @tparam F the arrow. */
template <IsStructuralArrow F>
Set image(const F&, const Set& s) {
  return Set(Shifted{&s, offset<F>()});
}
template <IsStructuralArrow F>
Set preimage(const F&, const Set& s) {
  return Set(Shifted{&s, -offset<F>()});
}

/** @brief Slicing by @b value: @f$S \cap [a, b)@f$, the meet with the
 *  half-open interval (Python's own convention), an absent bound meaning the
 *  ray.  Not positional: a set has no enumeration to index into; that reading
 *  belongs to a sequence. */
inline Set restrict(const Set& s, std::optional<long long> lo,
                    std::optional<long long> hi) {
  Leaf window = Leaf::universe();
  if (lo)
    window = window & Leaf::half(*lo, Direction::Upward, Strictness::NonStrict);
  if (hi)
    window = window & Leaf::half(*hi, Direction::Downward, Strictness::Strict);
  return materialise(s & window);
}

/** @brief Whether the set is bounded (on the discrete chain ℤ, equivalently
 *  finite): no run from below the window, and the last run ends. */
inline bool is_bounded(const Set& s) {
  return !s.first() &&
         (s.breakpoints().empty() || !s.breakpoints().rbegin()->second);
}
/** @brief The least element, the unfold's seed: none for @c Ø, and none for a
 *  set unbounded below (@c ↓k, @c 𝔸) --- ℤ has no bottom. */
inline std::optional<long long> least(const Set& s) {
  auto rs = dedekind::order::runs(s);
  const auto it = rs.begin();
  if (s.first() || it == rs.end()) return std::nullopt;
  return (*it).lo;
}
/** @brief The greatest element, where the set is bounded; @c nullopt on a
 *  set unbounded above: the unfold does not stop. */
inline std::optional<long long> greatest(const Set& s) {
  std::optional<long long> hi;
  if (is_bounded(s))
    for (const auto& r : dedekind::order::runs(s)) hi = r.hi;
  return hi;
}
/** @brief The least member above @c x: @c x + 1 inside a run, else the start
 *  of the next run; none past the last.  @param x a member below the window's
 *  end. */
inline std::optional<long long> next_member(const Set& s, long long x) {
  for (const auto& r : dedekind::order::runs(s)) {
    if (x < r.lo) return r.lo;
    if (x < r.hi) return x + 1;
  }
  return std::nullopt;
}

}  // namespace lwv

}  // namespace dedekind::python
