/**
 * @file dedekind/python/nanobind_jlt.cpp
 * @brief Nanobind extension module for the Jlt arrow DSL (#961).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * A @b fluent DSL over the @b real category arrows (first iteration: the unary
 * boolean operations), bound as the private native extension @c dedekind._jlt
 * (NumPy-style, re-exported by the pure-Python @c dedekind.jlt facade).  It
 * exposes the actual @c :morphism arrows --- the identity @c Identity<bool> and
 * the type-erased @c Morphism<bool,bool> --- as @b duck-typed handles: an
 * "arrow" is the protocol @c __call__ / @c __rshift__ / @c dom / @c cod, not a
 * base class.  @c IsArrow is witnessed on the C++ side; the Python side tests
 * the protocol and the monoid laws @b extensionally.
 *
 * Composition (@c >>) type-erases to a @c Morphism (Python arrows are
 * extensional --- functions), so no unbounded family of @c Compose<F,G> types
 * and no captured Python callable (hence no refleak).  Structural REDUCTION
 * (@c simplify / @c cata) is intensional and lives in C++, not here.
 */

#include <nanobind/nanobind.h>

import dedekind.python; // dedekind::python::jlt: Id, Arrow, id/lnot/compose

namespace nb = nanobind;

namespace {
namespace jlt = dedekind::python::jlt;

/** @brief @c dom / @c cod as a Python @b type object (numpy/pandas style: a
 *  dtype-like handle).  Every arrow here is @c bool→bool, so both are the
 *  builtin @c bool type --- @c arrow.dom() @c is @c bool. */
nb::object bool_type() {
  return nb::borrow<nb::object>(reinterpret_cast<PyObject*>(&PyBool_Type));
}

/** @brief Attach the shared duck-typed arrow protocol to a bound arrow class:
 *  apply @c (), diagrammatic composition @c >> (→ type-erased @c Arrow),
 *  @c dom / @c cod (→ @c bool).  Templated on the C++ arrow type @c T so the
 *  same protocol binds to both @c Identity<bool> and the type-erased
 *  @c Morphism. */
template <typename T>
void bind_arrow_protocol(nb::class_<T>& cls) {
  cls.def(
         "__call__", [](const T& f, bool x) { return f(x); },
         "Apply the arrow to a boolean object.")
      .def(
          "__rshift__",
          [](const T& f, const jlt::Id& g) { return jlt::compose(f, g); },
          "f >> g (apply f, then g); composes to a type-erased Morphism.")
      .def(
          "__rshift__",
          [](const T& f, const jlt::Arrow& g) { return jlt::compose(f, g); },
          "f >> g (apply f, then g); composes to a type-erased Morphism.")
      .def(
          "dom", [](const T&) { return bool_type(); },
          "The domain object (the boolean type).")
      .def(
          "cod", [](const T&) { return bool_type(); },
          "The codomain object (the boolean type).");
}

}  // namespace

NB_MODULE(_jlt, m) {
  m.doc() =
      "The Jlt arrow DSL (#961), first iteration: a fluent, point-free "
      "calculus "
      "of the unary boolean operations.  Objects are the booleans; the "
      "primitive arrows are `id` and `not_`.  Compose with `f >> g` (apply f, "
      "then g); apply to a bool with `a(x)`; inspect `a.dom()` / `a.cod()`.  "
      "Arrows are extensional (functions): structural `simplify` is "
      "intensional "
      "and lives on the C++ side, not here.";

  auto identity =
      nb::class_<jlt::Id>(m, "Identity",
                          "The identity arrow id: bool -> bool (the monoid "
                          "unit).  A real :morphism Identity<bool>.");
  bind_arrow_protocol(identity);
  identity.def("__repr__", [](const jlt::Id&) { return "id"; });

  auto morphism = nb::class_<jlt::Arrow>(
      m, "Morphism",
      "A type-erased arrow bool -> bool (the real :morphism Morphism).  "
      "Composition lands here.");
  bind_arrow_protocol(morphism);
  morphism.def("__repr__",
               [](const jlt::Arrow&) { return "<arrow bool -> bool>"; });

  // The two primitive arrows, as singleton VALUES so the DSL reads `id >>
  // not_`. `not` is a Python keyword, so negation is exposed as `not_`.
  m.attr("id") = jlt::id();
  m.attr("not_") = jlt::lnot();
}
