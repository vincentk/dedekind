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
 * @c constexpr @c :order @c reduce_meet the compile-time @c static_assert
 * exhibit folds (no Python-side reducer).  Value-oriented relational reading: a
 * halfspace is a point plus a direction, @c {x|x⋈p} = π₁(S × η(p) | π₁⋈π₂),
 * the pivot in the value @c η(p); the @c singleton is the degenerate point.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/string.h>

#include <optional>
#include <string>

import dedekind.python; // dedekind::python::lwv: Set, above/below/..., meet
import dedekind.order;  // dedekind::order::SetKind / Direction / Strictness

namespace nb = nanobind;

namespace {
namespace lwv = dedekind::python::lwv;
namespace jlt = dedekind::python::jlt;
namespace ord = dedekind::order;
using Set = lwv::Set;

/** @brief The unfold of a set of the discrete chain from its least element by
 *  the successor, stopping one past its greatest if it has one: a bounded set
 *  iterates like @c range, a ray like @c itertools.count.  @f$O(1)@f$ space. */
struct SetIter {
  std::optional<long long> current;
  std::optional<long long> stop;
};

const char* kind_name(ord::SetKind k) {
  switch (k) {
    case ord::SetKind::Empty:
      return "empty";
    case ord::SetKind::Universe:
      return "universe";
    case ord::SetKind::Halfspace:
      return "halfspace";
    case ord::SetKind::Singleton:
      return "singleton";
    case ord::SetKind::Interval:
      return "interval";
  }
  return "?";
}

std::string repr(const Set& s) {
  switch (s.kind) {
    case ord::SetKind::Empty:
      return "Ø";
    case ord::SetKind::Universe:
      return "𝔸";
    case ord::SetKind::Singleton:
      return "{" + std::to_string(s.lo) + "}";
    case ord::SetKind::Halfspace: {
      const char* op = s.dir == ord::Direction::Upward
                           ? (s.sl == ord::Strictness::Strict ? ">" : ">=")
                           : (s.sl == ord::Strictness::Strict ? "<" : "<=");
      return std::string("{x | x ") + op + " " + std::to_string(s.lo) + "}";
    }
    case ord::SetKind::Interval: {
      const char* lb = s.sl == ord::Strictness::Strict ? "(" : "[";
      const char* rb = s.su == ord::Strictness::Strict ? ")" : "]";
      return lb + std::to_string(s.lo) + ", " + std::to_string(s.hi) + rb;
    }
  }
  return "?";
}

// The FINITE cardinality where it is decidable (empty / singleton / integer
// interval); std::nullopt for the infinite kinds (halfspace / universe).
std::optional<long long> cardinality(const Set& s) {
  switch (s.kind) {
    case ord::SetKind::Empty:
      return 0;
    case ord::SetKind::Singleton:
      return 1;
    case ord::SetKind::Interval: {
      const long long el = (s.sl == ord::Strictness::Strict) ? s.lo + 1 : s.lo;
      const long long eu = (s.su == ord::Strictness::Strict) ? s.hi - 1 : s.hi;
      return eu >= el ? (eu - el + 1) : 0;
    }
    default:
      return std::nullopt;
  }
}
}  // namespace

NB_MODULE(_lwv, m) {
  m.doc() =
      "The Lwv set-comprehension DSL (#965, paper §3), value-based iteration "
      "2: "
      "halfspaces / singletons over int as duck-typed handles whose pivot is a "
      "runtime value.  Construct with above(k)/at_least(k)/below(k)/at_most(k)/"
      "singleton(k) (and everything()/nothing()); membership via `s(x)` / "
      "`x in s`; meet via `a & b`.  `above(3) & below(5)` collapses to "
      "`singleton(4)` through the same constexpr :order reduce_meet the "
      "compile-time exhibit folds -- runtime reduction, no Python-side "
      "reducer.";

  nb::class_<Set>(
      m, "Set",
      "A value-based Lwv set: a halfspace / singleton / interval / boundary "
      "whose pivot(s) are runtime values.  Membership `s(x)` / `x in s`; meet "
      "`a & b` routes through the value-first :order reducer.")
      .def(
          "__call__", [](const Set& s, long long x) { return s.contains(x); },
          nb::arg("x"), "Membership χ(x): whether x is in the set.")
      .def(
          "__contains__",
          [](const Set& s, long long x) { return s.contains(x); }, nb::arg("x"),
          "x in s: the same membership test.")
      .def(
          "__and__", [](const Set& a, const Set& b) { return lwv::meet(a, b); },
          "a & b: intersection through the value-first reduce_meet (runtime).")
      .def("__repr__", [](const Set& s) { return repr(s); })
      .def(
          "__eq__", [](const Set& a, const Set& b) { return a == b; },
          nb::is_operator(), "a == b: the same set (kind and bounds).")
      .def(
          "__bool__",
          [](const Set& s) { return s.kind != ord::SetKind::Empty; },
          "A set is truthy iff it is inhabited.")
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
            if (!first && s.kind != ord::SetKind::Empty)
              throw nb::type_error(
                  "no least element: a set unbounded below ({x < k}, 𝔸) has "
                  "no bottom on ℤ to unfold from");
            return SetIter{first, lwv::past_end(s)};
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
          "s[a:] the upper ray.  One reduce_meet.")
      .def_prop_ro(
          "kind", [](const Set& s) { return kind_name(s.kind); },
          "The structural kind: empty / universe / halfspace / singleton / "
          "interval.")
      .def_prop_ro(
          "cardinality",
          [](const Set& s) -> nb::object {
            const auto c = cardinality(s);
            return c ? nb::cast(*c) : nb::none();
          },
          "The finite cardinality where decidable (0 / 1 / interval count); "
          "None for the infinite kinds (halfspace, universe).");

  m.def("above", &lwv::above, nb::arg("k"),
        "above(k): the open upper halfspace {x | x > k} = ↑k.");
  m.def("at_least", &lwv::at_least, nb::arg("k"),
        "at_least(k): the closed upper halfspace {x | x >= k}.");
  m.def("below", &lwv::below, nb::arg("k"),
        "below(k): the open lower halfspace {x | x < k} = ↓k.");
  m.def("at_most", &lwv::at_most, nb::arg("k"),
        "at_most(k): the closed lower halfspace {x | x <= k}.");
  m.def("singleton", &lwv::singleton, nb::arg("k"),
        "singleton(k): the point {k} = η(k), the value-based atom.");
  m.def("everything", &lwv::everything, "everything(): the universe 𝔸.");
  m.def("nothing", &lwv::nothing, "nothing(): the empty set Ø.");

  nb::class_<SetIter>(m, "SetIter", "The unfold of a set by the successor.")
      .def("__iter__", [](SetIter& it) -> SetIter& { return it; })
      .def("__next__", [](SetIter& it) {
        if (!it.current || (it.stop && *it.current >= *it.stop))
          throw nb::stop_iteration();
        const long long value = *it.current;
        ++*it.current;
        return value;
      });

  // image / preimage (paper §4): the function algebra on sets, for the arrows
  // whose action on a value leaf has a normal form.  An erased composite is
  // intensional and refused, not guessed.  FIXME(#1005): refl gets a typed
  // arrow.
  const char* image_doc =
      "image(f, S) = {f(x) | x ∈ S} for a structural arrow (id, succ, pred): "
      "the bounds move, the kind stays.  image(succ(int), above(5)) == "
      "above(6).";
  const char* preimage_doc =
      "preimage(f, S) = {x | f(x) ∈ S} for a structural arrow (id, succ, "
      "pred).  preimage(succ(int), above(6)) == above(5).";
  m.def(
      "image",
      [](const jlt::Id<jlt::Int>& f, const Set& s) { return lwv::image(f, s); },
      nb::arg("f"), nb::arg("s"), image_doc);
  m.def(
      "image",
      [](const jlt::Succ<jlt::Int>& f, const Set& s) {
        return lwv::image(f, s);
      },
      nb::arg("f"), nb::arg("s"), image_doc);
  m.def(
      "image",
      [](const jlt::Pred<jlt::Int>& f, const Set& s) {
        return lwv::image(f, s);
      },
      nb::arg("f"), nb::arg("s"), image_doc);
  m.def(
      "image",
      [](const jlt::Arrow<jlt::Int>&, const Set&) -> Set {
        throw nb::type_error(
            "image of an erased composite arrow is intensional (Kleene-valued) "
            "and has no value normal form; only id / succ / pred do");
      },
      nb::arg("f"), nb::arg("s"), image_doc);
  m.def(
      "preimage",
      [](const jlt::Id<jlt::Int>& f, const Set& s) {
        return lwv::preimage(f, s);
      },
      nb::arg("f"), nb::arg("s"), preimage_doc);
  m.def(
      "preimage",
      [](const jlt::Succ<jlt::Int>& f, const Set& s) {
        return lwv::preimage(f, s);
      },
      nb::arg("f"), nb::arg("s"), preimage_doc);
  m.def(
      "preimage",
      [](const jlt::Pred<jlt::Int>& f, const Set& s) {
        return lwv::preimage(f, s);
      },
      nb::arg("f"), nb::arg("s"), preimage_doc);
  m.def(
      "preimage",
      [](const jlt::Arrow<jlt::Int>&, const Set&) -> Set {
        throw nb::type_error(
            "preimage of an erased composite arrow is intensional "
            "(Kleene-valued) and has no value normal form; only id / succ / "
            "pred do");
      },
      nb::arg("f"), nb::arg("s"), preimage_doc);
}
