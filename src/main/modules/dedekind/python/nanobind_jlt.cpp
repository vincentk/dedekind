/**
 * @file dedekind/python/nanobind_jlt.cpp
 * @brief Nanobind extension module for the Jlt composition-term exhibit (#961).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * The category-concept surface of the Jlt exhibit (first iteration: the unary
 * boolean operations) --- the arrows @c Arrow, the primitives @c id and @c not,
 * composition @c >>, and the value-first reducer @c simplify --- bound as its
 * own private native extension @c dedekind._jlt (NumPy-style: one private
 * compiled module per cohesive component, re-exported by the pure-Python
 * @c dedekind.jlt facade).  This TU owns only the @b handle surface; the term
 * and its reduction live in the C++ core (@c dedekind::python::ArrowTerm), per
 * the package's handle-only contract: Python composes handles and asks C++ to
 * normalise, it does not implement the law.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/string.h>

import dedekind.python; // ArrowTerm: the runtime composition term + reducer

namespace nb = nanobind;

NB_MODULE(_jlt, m) {
  using dedekind::python::ArrowTerm;

  m.doc() =
      "The Jlt composition-term exhibit (#961), first iteration: a point-free "
      "calculus of the unary boolean operations.  Objects are true | false; "
      "the primitive arrows are `id` and `not`.  Compose with `f >> g` (apply "
      "f, then g); normalise with `simplify` (the value-first cata, run in "
      "C++); apply to a bool with `a(x)`.";

  nb::class_<ArrowTerm>(
      m, "Arrow",
      "A composition term over the unary boolean operations {id, not}.  "
      "Reduction runs in C++ (value-first); Python holds the handle.")
      .def(
          "__rshift__",
          [](const ArrowTerm& f, const ArrowTerm& g) { return f >> g; },
          "f >> g: diagrammatic composition (apply f, then g).")
      .def(
          "__call__", [](const ArrowTerm& a, bool x) { return a(x); },
          "Apply the arrow to a bool.")
      .def(
          "__eq__",
          [](const ArrowTerm& a, const ArrowTerm& b) { return a == b; },
          "Structural equality of terms.")
      .def(
          "__repr__", [](const ArrowTerm& a) { return a.sexpr(); },
          "S-expression rendering: id | not | (>> l r).");

  // `id` and `not` are singleton Arrow VALUES (the two unary boolean ops), so
  // the exhibit reads `id >> not` / `simplify(id >> id) == id` without call
  // syntax.  `not` is a Python keyword, so it is exposed as `not_`.
  m.attr("id") = ArrowTerm::id();
  m.attr("not_") = ArrowTerm::lnot();
  m.def(
      "simplify",
      [](const ArrowTerm& t) { return dedekind::python::simplify(t); },
      nb::arg("term"),
      "Value-first cata: normalise a term via the monoid unit law "
      "(id >> f == f == f >> id).  The reduction runs in C++.");
}
