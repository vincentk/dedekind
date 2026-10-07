/**
 * @file dedekind/python/nanobind_collatz.cpp
 * @brief Nanobind extension module for the bounded Collatz exhibit (#861):
 * the orbit as a handle, the budgeted verdict in K₃, and the window ∀.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * Private native module @c dedekind._collatz, re-exported by the pure-Python
 * @c dedekind.collatz facade.  @c collatz(n) is the orbit, a lazy Path indexed
 * and sliced by position and iterated without end; @c reaches_within(B) is the
 * predicate "reaches 1 within B steps", answering ⊤ or U and never ⊥; and
 * @c forall(S, P) is the bounded ∀ over an @c lwv window, in K₃.  A set without
 * a window (a ray, 𝔸) is refused: that ∀ is the conjecture.  Reduction stays in
 * C++.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/vector.h>

#include <cstddef>
#include <optional>
#include <string>
#include <vector>

import dedekind.category;  // Ternary
import dedekind.numbers;   // collatz_reach_time
import dedekind.python;    // dedekind::python::collatz, ::lwv
import dedekind.sequences; // Path

namespace nb = nanobind;

namespace {
namespace collatz = dedekind::python::collatz;
namespace lwv = dedekind::python::lwv;
using dedekind::category::Ternary;
using dedekind::sequences::Path;

/** @brief The orbit handle: the seed and its lazy path. */
struct Orbit {
  std::size_t seed;
  Path<std::size_t> path;
};
/** @brief The orbit's unfold, without end: the attractor 1 → 4 → 2 → 1 repeats.
 */
struct OrbitIter {
  Path<std::size_t> path;
  std::size_t index = 0;
};

std::size_t seed_of(long long n) {
  if (n < 0)
    throw nb::value_error(
        "Collatz is a relation on ℕ: the seed is non-negative");
  return static_cast<std::size_t>(n);
}

std::string repr(const Orbit& o) {
  std::string s = "⟨";
  for (std::size_t i = 0; i < 6; ++i) s += std::to_string(o.path.at(i)) + ", ";
  return s + "…⟩";
}
}  // namespace

NB_MODULE(_collatz, m) {
  m.doc() =
      "The bounded Collatz exhibit (#861).  collatz(n) is the orbit of n under "
      "T (n/2 if even, 3n+1 if odd): o[i], o[a:b], iter(o), o.reach_time(B).  "
      "reaches_within(B) is the verdict 'reaches 1 within B steps', valued in "
      "K₃: TRUE once the orbit is seen to arrive, UNKNOWN when the budget runs "
      "out, never FALSE (nothing refutes; that is the open problem).  "
      "forall(S, reaches_within(B)) is the bounded ∀ over an lwv window, in "
      "K₃; "
      "a set without a window is refused.";

  nb::class_<OrbitIter>(m, "OrbitIter", "The unfold of an orbit, without end.")
      .def("__iter__", [](OrbitIter& it) -> OrbitIter& { return it; })
      .def("__next__", [](OrbitIter& it) { return it.path.at(it.index++); });

  nb::class_<Orbit>(m, "Orbit",
                    "The orbit of a seed under the Collatz rule: a lazy path.")
      .def_prop_ro(
          "seed", [](const Orbit& o) { return o.seed; }, "The seed n.")
      .def(
          "__getitem__",
          [](const Orbit& o, nb::handle key) -> nb::object {
            if (nb::isinstance<nb::slice>(key)) {
              const auto stop = key.attr("stop");
              if (stop.is_none())
                throw nb::type_error(
                    "an orbit has no end: give the slice a stop, o[:k]");
              const auto start = key.attr("start");
              const auto step = key.attr("step");
              const long long a =
                  start.is_none() ? 0 : nb::cast<long long>(start);
              const long long b = nb::cast<long long>(stop);
              const long long d =
                  step.is_none() ? 1 : nb::cast<long long>(step);
              if (a < 0 || b < 0 || d <= 0)
                throw nb::index_error(
                    "an orbit is indexed from its seed forward: non-negative "
                    "positions, positive step");
              std::vector<std::size_t> out;
              for (long long i = a; i < b; i += d)
                out.push_back(o.path.at(static_cast<std::size_t>(i)));
              return nb::cast(out);
            }
            const auto i = nb::cast<long long>(key);
            if (i < 0)
              throw nb::index_error("an orbit has no end to count from");
            return nb::cast(o.path.at(static_cast<std::size_t>(i)));
          },
          nb::arg("key"),
          "o[i]: the i-th term, T^i(n); o[a:b]: the terms at positions [a, b) "
          "(a stop is required: the orbit does not end).")
      .def(
          "__iter__", [](const Orbit& o) { return OrbitIter{o.path, 0}; },
          "Unfold the orbit from the seed; it never stops (use islice).")
      .def(
          "reach_time",
          [](const Orbit& o, std::size_t budget) {
            return dedekind::numbers::collatz_reach_time(o.seed, budget);
          },
          nb::arg("budget"),
          "The first index at which the orbit is 1, if within the budget; "
          "None otherwise (not yet: never 'never').")
      .def("__repr__", [](const Orbit& o) { return repr(o); });

  nb::class_<collatz::ReachesWithin>(
      m, "ReachesWithin",
      "The predicate 'reaches 1 within the budget', valued in K₃: TRUE or "
      "UNKNOWN, never FALSE.")
      .def_prop_ro(
          "budget", [](const collatz::ReachesWithin& p) { return p.budget; },
          "The budget B.")
      .def(
          "__call__",
          [](const collatz::ReachesWithin& p, long long n) { return p(n); },
          nb::arg("n"),
          "The verdict for n: TRUE once seen to arrive, else UNKNOWN.")
      .def("__repr__", [](const collatz::ReachesWithin& p) {
        return "{n | n reaches 1 within " + std::to_string(p.budget) + "}";
      });

  m.def(
      "collatz",
      [](long long n) { return Orbit{seed_of(n), collatz::orbit(seed_of(n))}; },
      nb::arg("n"), "collatz(n): the orbit of n under the rule, a lazy path.");
  m.def(
      "reaches_within",
      [](std::size_t budget) { return collatz::ReachesWithin{budget}; },
      nb::arg("budget"),
      "reaches_within(B): the predicate 'reaches 1 within B steps', in K₃.");
  m.def(
      "forall",
      [](const lwv::Set& s, const collatz::ReachesWithin& p) -> Ternary {
        if (!lwv::is_bounded(s))
          throw nb::type_error(
              "∀ over a set without a window ({x > k}, 𝔸) is the conjecture "
              "itself: no budget bounds a search over all of ℕ; slice the set, "
              "s[a:b]");
        return lwv::forall(s, p);
      },
      nb::arg("s"), nb::arg("p"),
      "forall(S, P): the bounded ∀ over the window S spans, in K₃: TRUE when "
      "every n in S reaches 1 within P's budget, UNKNOWN otherwise.  A ray or "
      "𝔸 has no window and is refused.");
}
