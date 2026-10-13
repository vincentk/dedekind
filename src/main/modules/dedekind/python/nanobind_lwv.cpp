/**
 * @file dedekind/python/nanobind_lwv.cpp
 * @brief Nanobind extension module for the Lwv set-comprehension DSL (#965).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * The SET twin of the @c jlt arrow exhibit, iteration 2: a @b value-based,
 * scalable surface bound as the private native extension @c dedekind._lwv
 * (re-exported by @c dedekind.lwv).  A set is a duck-typed handle (@c __call__
 * /
 * @c __contains__ membership, @c & meet) whose pivot rides as a @b value, so a
 * single constructor --- @c above(k) / @c below(k) / @c at_least(k) /
 * @c at_most(k) / @c singleton(k) --- covers every pivot at runtime.  This
 * unifies the iteration-1 one-constant-per-pivot stopgap (@c gt_3 / @c lt_5,
 * now removed).
 *
 * The headline still holds and now at genuine runtime: @c above(3) @c &
 * @c below(5) collapses to @c singleton(4), routed through the @b same
 * @c constexpr lattice reducer the compile-time @c static_assert exhibits fold
 * (no Python-side reducer).  A set is a run list (@c order:runs), so unions
 * and complements are sets of the same type.  Value-oriented relational
 * reading: a halfspace is a point plus a direction, @c {x|x⋈p} = π₁(S × η(p) |
 * π₁⋈π₂), the pivot in the value @c η(p); the @c singleton is the degenerate
 * point.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/string.h>

#include <concepts>
#include <limits>
#include <optional>
#include <ranges>
#include <stdexcept>
#include <string>

import dedekind.category; // Identity, Successor, Predecessor
import dedekind.order;    // Direction / Strictness, intervals
import dedekind.sets;     // Ø, IsSetObject

namespace nb = nanobind;

namespace {
using Int = long long;  // what a Python int crosses the boundary as
namespace ord = dedekind::order;
using dedekind::category::Boole;
using dedekind::category::Identity;
using dedekind::category::Predecessor;
using dedekind::category::Successor;
using dedekind::order::Direction;
using dedekind::order::ext;
using dedekind::order::Interval;
using dedekind::order::Position;
using dedekind::order::Runs;
using dedekind::order::Side;
using dedekind::order::Strictness;
using dedekind::sets::IsSetObject;

// ── Lwv: the scalar set-comprehension language (#965, paper §3) ───────────
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
// SET twin of the @c jlt ARROW exhibit: it binds the real reducer, NOT a
// Python-side reimplementation (handle-only).

/** @brief Where ℤ continues and @c long @c long cannot: raise (Python's
 *  @c OverflowError) rather than wrap.  FIXME(#1019): the algebraic carriers
 *  (the unsigned ring, the modular integers) replace the window.
 *  @param what the operation that reached the end. */
[[noreturn]] void window_end(const char* what) {
  throw std::overflow_error(std::string(what) +
                            ": the 64-bit window ends here; ℤ does not");
}
/** @brief @c a @c + @c b inside the window, or @c window_end. */
Int add_in_window(Int a, Int b, const char* what) {
  Int sum{};
  if (__builtin_add_overflow(a, b, &sum)) window_end(what);
  return sum;
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
  return ext(Leaf::half(k, D, S));
}
/** @brief The point @c {k}.  @param k the point. */
Set point(long long k) { return ext(Leaf::point(k)); }

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
      st.at.at = add_in_window(st.at.at, k, "image");
      if (st.value ? st.at == Position<long long>{W::max(), Side::Above}
                   : st.at == Position<long long>{W::min(), Side::Below})
        window_end("image");
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
concept IsStructuralArrow =
    std::same_as<F, Identity<Int>> || std::same_as<F, Successor<Int>> ||
    std::same_as<F, Predecessor<Int>>;
/** @brief How far the arrow moves a step: 0, +1, −1.  @tparam F the arrow. */
template <IsStructuralArrow F>
consteval long long offset() {
  if constexpr (std::same_as<F, Successor<Int>>)
    return 1;
  else if constexpr (std::same_as<F, Predecessor<Int>>)
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
Set restrict(const Set& s, std::optional<long long> lo,
             std::optional<long long> hi) {
  Leaf window = Leaf::universe();
  if (lo)
    window = window & Leaf::half(*lo, Direction::Upward, Strictness::NonStrict);
  if (hi)
    window = window & Leaf::half(*hi, Direction::Downward, Strictness::Strict);
  return ext(s & window);
}

/** @brief Whether the set is bounded (on the discrete chain ℤ, equivalently
 *  finite): no run from below the window, and the last run ends. */
bool is_bounded(const Set& s) {
  return !s.first() &&
         (s.breakpoints().empty() || !s.breakpoints().rbegin()->second);
}
/** @brief The least element, the unfold's seed: none for @c Ø, and none for a
 *  set unbounded below (@c ↓k, @c 𝔸) --- ℤ has no bottom. */
std::optional<long long> least(const Set& s) {
  auto rs = dedekind::order::runs(s);
  const auto it = rs.begin();
  if (s.first() || it == rs.end()) return std::nullopt;
  return (*it).lo;
}
/** @brief The greatest element, where the set is bounded; @c nullopt on a
 *  set unbounded above: the unfold does not stop. */
std::optional<long long> greatest(const Set& s) {
  std::optional<long long> hi;
  if (is_bounded(s))
    for (const auto& r : dedekind::order::runs(s)) hi = r.hi;
  return hi;
}
/** @brief The least member above @c x: @c x + 1 inside a run, else the start
 *  of the next run; none past the last.  @param x a member below the window's
 *  end. */
std::optional<long long> next_member(const Set& s, long long x) {
  for (const auto& r : dedekind::order::runs(s)) {
    if (x < r.lo) return r.lo;
    if (x < r.hi) return x + 1;
  }
  return std::nullopt;
}

}  // namespace lwv
using Set = lwv::Set;

/** @brief @c image / @c preimage for one structural arrow type @c F. */
template <typename F>
void bind_images(nb::module_& m, const char* image_doc,
                 const char* preimage_doc) {
  m.def(
      "image", [](const F& f, const Set& s) { return lwv::image(f, s); },
      nb::arg("f"), nb::arg("s"), image_doc);
  m.def(
      "preimage", [](const F& f, const Set& s) { return lwv::preimage(f, s); },
      nb::arg("f"), nb::arg("s"), preimage_doc);
}

/** @brief The unfold of a set of the discrete chain from its least element by
 *  the successor, stopping after its greatest if it has one: a bounded set
 *  iterates like @c range, a ray like @c itertools.count --- until the 64-bit
 *  window ends, where the ray's unfold raises rather than wraps.  @f$O(1)@f$
 *  space. */
struct SetIter {
  Set set;
  std::optional<long long> current;  // nullopt once the unfold has ended
  std::optional<long long> last;     // the greatest element, if bounded above
  bool at_window_end = false;        // a ray reached the window's last value
};

/** @brief One run, written: @c {4}, @c [3, 8), @c {x | x > 5}, @c 𝔸. */
std::string repr(const lwv::Leaf& s) {
  const auto& [lower, upper] = *s.span();
  if (!lower && !upper) return "𝔸";
  if (lower && upper && lower->at == upper->at)
    return "{" + std::to_string(lower->at) + "}";
  if (lower && upper) {
    const char* lb = lower->strictness == ord::Strictness::Strict ? "(" : "[";
    const char* rb = upper->strictness == ord::Strictness::Strict ? ")" : "]";
    return lb + std::to_string(lower->at) + ", " + std::to_string(upper->at) +
           rb;
  }
  const bool up = lower.has_value();
  const auto& b = up ? *lower : *upper;
  const bool strict = b.strictness == ord::Strictness::Strict;
  const char* op = up ? (strict ? ">" : ">=") : (strict ? "<" : "<=");
  return std::string("{x | x ") + op + " " + std::to_string(b.at) + "}";
}
/** @brief A set, written as its runs joined by @c ∪; @c Ø without any. */
std::string repr(const Set& s) {
  std::string out;
  for (const auto& r : ord::intervals(s)) {
    if (!out.empty()) out += " ∪ ";
    out += repr(r);
  }
  return out.empty() ? "Ø" : out;
}

// The FINITE cardinality where it is decidable (a bounded set); std::nullopt
// for a set unbounded at either end.
std::optional<unsigned long long> cardinality(const Set& s) {
  if (!lwv::is_bounded(s)) return std::nullopt;
  unsigned long long n = 0;
  for (const auto& r : ord::runs(s)) {
    // Exact in the unsigned word for every run short of the whole window; the
    // whole window has 2^64 elements, one more than it holds.
    const auto span = static_cast<unsigned long long>(r.hi) -
                      static_cast<unsigned long long>(r.lo);
    if (span == std::numeric_limits<unsigned long long>::max() ||
        __builtin_add_overflow(n, span + 1, &n))
      window_end("len");
  }
  return n;
}
}  // namespace

NB_MODULE(_lwv, m) {
  m.doc() =
      "The Lwv set-comprehension DSL (#965, paper §3), value-based iteration "
      "2: "
      "halfspaces / singletons over int as duck-typed handles whose pivot is a "
      "runtime value.  Construct with above(k)/at_least(k)/below(k)/at_most(k)/"
      "singleton(k) (and everything()/nothing()); membership via `s(x)` / "
      "`x in s`; meet, join and complement via `a & b`, `a | b`, `~a`.  "
      "`above(3) & below(5)` collapses to `singleton(4)` through the same "
      "constexpr lattice reducer the compile-time exhibits fold -- runtime "
      "reduction, no Python-side reducer.  A set is a run list, the normal "
      "form of unions of intervals: `~singleton(5)` is two runs.";

  nb::class_<Set>(
      m, "Set",
      "A value-based Lwv set: a run list, finitely many disjoint intervals "
      "whose bounds are runtime values.  Membership `s(x)` / `x in s`; "
      "`a & b`, `a | b`, `~a` route through the lattice reducer.")
      .def(
          "__call__", [](const Set& s, long long x) { return s.contains(x); },
          nb::arg("x"), "Membership χ(x): whether x is in the set.")
      .def(
          "__contains__",
          [](const Set& s, long long x) { return s.contains(x); }, nb::arg("x"),
          "x in s: the same membership test.")
      .def(
          "__and__", [](const Set& a, const Set& b) { return ext(a & b); },
          "a & b: the meet, folded by the lattice reducer at runtime (its "
          "leaf-combine is the interval meet).")
      .def(
          "__or__", [](const Set& a, const Set& b) { return ext(a | b); },
          "a | b: the join, a run list again.")
      .def(
          "__invert__", [](const Set& s) { return ext(~s); },
          "~s: the complement, the parts below, between and above s's runs.")
      .def("__repr__", [](const Set& s) { return repr(s); })
      .def(
          "__eq__", [](const Set& a, const Set& b) { return a == b; },
          nb::is_operator(), "a == b: the same set.")
      .def(
          "__bool__",
          [](const Set& s) { return !(dedekind::sets::Ø<long long>{} == s); },
          "A set is truthy iff it is inhabited.")
      .def(
          "runs",
          [](const Set& s) {
            nb::list out;
            for (const auto& r : ord::intervals(s))
              out.append(nb::cast(ext(r)));
            return out;
          },
          "The maximal runs, in order along the chain, each a set of one "
          "interval: the normal form, read off.")
      .def(
          "__len__",
          [](const Set& s) -> std::size_t {
            const auto c = cardinality(s);
            if (!c)
              throw nb::type_error(
                  "ℵ₀ is not a Python int: the set is unbounded (see "
                  "is_bounded)");
            return static_cast<std::size_t>(*c);
          },
          "The number of elements of a bounded set; a ray or 𝔸 has none to "
          "give.")
      .def(
          "__iter__",
          [](const Set& s) {
            const auto first = lwv::least(s);
            if (!first && !(dedekind::sets::Ø<long long>{} == s))
              throw nb::type_error(
                  "no least element: a set unbounded below ({x < k}, 𝔸) has "
                  "no bottom on ℤ to unfold from");
            return SetIter{s, first, lwv::greatest(s)};
          },
          "Unfold the set from its least element by the successor: a bounded "
          "set iterates like range(a, b), a ray like itertools.count(a).")
      .def_prop_ro(
          "is_bounded", [](const Set& s) { return lwv::is_bounded(s); },
          "Whether the set is bounded (has a least and a greatest element).")
      .def_prop_ro(
          "is_finite", [](const Set& s) { return lwv::is_bounded(s); },
          "Whether the set is finite: on the discrete chain ℤ, exactly when it "
          "is bounded.")
      .def(
          "__getitem__",
          [](const Set& s, nb::handle key) -> Set {
            if (!nb::isinstance<nb::slice>(key))
              throw nb::type_error(
                  "a set is sliced by value, s[a:b] = s ∩ [a, b); positional "
                  "indexing presupposes an enumeration, which is a sequence's");
            if (!key.attr("step").is_none())
              throw nb::type_error(
                  "a stepped slice is the meet with a residue class, which "
                  "awaits the congruence sets");
            const auto bound = [](nb::handle v) -> std::optional<long long> {
              if (v.is_none()) return std::nullopt;
              return nb::cast<long long>(v);
            };
            return lwv::restrict(s, bound(key.attr("start")),
                                 bound(key.attr("stop")));
          },
          nb::arg("key"),
          "s[a:b] = s ∩ [a, b), slicing by VALUE (pandas' .loc, not .iloc): "
          "the "
          "meet with the half-open interval; s[:b] is the lower cut at b, "
          "s[a:] the upper ray.")
      .def_prop_ro(
          "cardinality",
          [](const Set& s) -> nb::object {
            const auto c = cardinality(s);
            return c ? nb::cast(*c) : nb::none();
          },
          "The finite cardinality where decidable (a bounded set); None for a "
          "set unbounded at either end.");

  m.def("above", &lwv::ray<ord::Direction::Upward, ord::Strictness::Strict>,
        nb::arg("k"), "above(k): the open upper halfspace {x | x > k} = ↑k.");
  m.def("at_least",
        &lwv::ray<ord::Direction::Upward, ord::Strictness::NonStrict>,
        nb::arg("k"), "at_least(k): the closed upper halfspace {x | x >= k}.");
  m.def("below", &lwv::ray<ord::Direction::Downward, ord::Strictness::Strict>,
        nb::arg("k"), "below(k): the open lower halfspace {x | x < k} = ↓k.");
  m.def("at_most",
        &lwv::ray<ord::Direction::Downward, ord::Strictness::NonStrict>,
        nb::arg("k"), "at_most(k): the closed lower halfspace {x | x <= k}.");
  m.def("singleton", &lwv::point, nb::arg("k"),
        "singleton(k): the point {k} = η(k), the value-based atom.");
  m.def("everything", &Set::universe, "everything(): the universe 𝔸.");
  m.def("nothing", &Set::empty, "nothing(): the empty set Ø.");

  nb::class_<SetIter>(m, "SetIter", "The unfold of a set by the successor.")
      .def("__iter__", [](SetIter& it) -> SetIter& { return it; })
      .def("__next__", [](SetIter& it) {
        if (it.at_window_end) window_end("iter");
        if (!it.current) throw nb::stop_iteration();
        const long long value = *it.current;
        if (it.last && value >= *it.last)
          it.current = std::nullopt;
        else if (value == std::numeric_limits<long long>::max())
          it.at_window_end = true;
        else
          it.current = lwv::next_member(it.set, value);
        return value;
      });

  // image / preimage (paper §4): the function algebra on sets, for the arrows
  // whose action on a value leaf has a normal form.  An erased composite is
  // intensional and has no overload: refused (TypeError), not guessed.
  // FIXME(#1005): refl gets a typed arrow.
  const char* image_doc =
      "image(f, S) = {f(x) | x ∈ S} for a structural arrow (id, succ, pred): "
      "the bounds move, the kind stays.  image(succ(int), above(5)) == "
      "above(6).";
  const char* preimage_doc =
      "preimage(f, S) = {x | f(x) ∈ S} for a structural arrow (id, succ, "
      "pred).  preimage(succ(int), above(6)) == above(5).";
  bind_images<Identity<Int>>(m, image_doc, preimage_doc);
  bind_images<Successor<Int>>(m, image_doc, preimage_doc);
  bind_images<Predecessor<Int>>(m, image_doc, preimage_doc);
}
