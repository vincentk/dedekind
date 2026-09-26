/**
 * @file dedekind/python/nanobind_jlt.cpp
 * @brief Nanobind extension module for the Jlt arrow DSL (#961).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * A @b fluent DSL over the @b real category arrows, bound as the private native
 * extension @c dedekind._jlt (NumPy-style, re-exported by the pure-Python
 * @c dedekind.jlt facade).  It exposes the actual @c :morphism arrows over TWO
 * objects, @c bool and @c int --- the identity @c Identity<T> and the
 * type-erased @c Morphism<T,T> --- as @b duck-typed handles: an "arrow" is the
 * protocol @c __call__ / @c __rshift__ + the free functions @c dom / @c cod,
 * not a base class.  The generators are @c id and @c refl (the reflection
 * involution: @c ¬ on @c bool, negation on @c int).
 *
 * Composability is @b structural: @c __rshift__ is bound only for same-object
 * operands, so @c id(bool) @c >> @c refl(int) finds no overload and raises ---
 * the runtime image of the C++ @c operator>>'s static @c same_as<Cod,Dom>.
 * Composition type-erases to a @c Morphism (Python arrows are extensional); no
 * captured Python callable, so no refleak.
 */

#include <nanobind/nanobind.h>

import dedekind.python; // dedekind::python::jlt: Id<T>, Arrow<T>, id/refl/compose

namespace nb = nanobind;

namespace {
namespace jlt = dedekind::python::jlt;

/** @brief @c dom / @c cod as a Python @b type object (numpy/pandas dtype
 * style):
 *  @c bool → the builtin @c bool, @c int → the builtin @c int. */
template <typename T>
nb::object type_object();
template <>
nb::object type_object<bool>() {
  return nb::borrow<nb::object>(reinterpret_cast<PyObject*>(&PyBool_Type));
}
template <>
nb::object type_object<int>() {
  return nb::borrow<nb::object>(reinterpret_cast<PyObject*>(&PyLong_Type));
}

/** @brief Bind the arrow @b operators (apply @c (), same-object composition
 *  @c >>) to a bound arrow class @c S (an @c Id<T> or an @c Arrow<T>).  @c >>
 *  accepts only same-object operands, so cross-object composition raises
 *  automatically (no matching overload) --- the composability guard. */
template <typename S>
void bind_endo_operators(nb::class_<S>& cls) {
  using T = typename S::Domain;
  cls.def(
         "__call__", [](const S& f, T x) { return f(x); },
         "Apply the arrow to an object of its carrier.")
      .def(
          "__rshift__",
          [](const S& f, const jlt::Id<T>& g) { return jlt::compose(f, g); },
          "f >> g (apply f, then g); same-object composition → Morphism.")
      .def(
          "__rshift__",
          [](const S& f, const jlt::Arrow<T>& g) { return jlt::compose(f, g); },
          "f >> g (apply f, then g); same-object composition → Morphism.");
}

/** @brief Bind one carrier @c T: the real @c Identity<T> and the type-erased
 *  @c Morphism<T,T>, their operators, and the free accessors @c dom / @c cod.
 */
template <typename T>
void bind_carrier(nb::module_& m, const char* identity_name,
                  const char* morphism_name) {
  auto identity = nb::class_<jlt::Id<T>>(
      m, identity_name,
      "The identity arrow id: T -> T (the monoid unit).  A real :morphism "
      "Identity<T>.");
  bind_endo_operators(identity);
  identity.def("__repr__", [](const jlt::Id<T>&) { return "id"; });

  auto morphism = nb::class_<jlt::Arrow<T>>(
      m, morphism_name,
      "A type-erased endo-arrow T -> T (the real :morphism Morphism).  "
      "Composition and refl land here.");
  bind_endo_operators(morphism);
  morphism.def("__repr__", [](const jlt::Arrow<T>&) { return "<arrow>"; });

  m.def(
      "dom", [](const jlt::Id<T>&) { return type_object<T>(); },
      "dom(f): the domain object (a type).");
  m.def(
      "dom", [](const jlt::Arrow<T>&) { return type_object<T>(); },
      "dom(f): the domain object (a type).");
  m.def(
      "cod", [](const jlt::Id<T>&) { return type_object<T>(); },
      "cod(f): the codomain object (a type).");
  m.def(
      "cod", [](const jlt::Arrow<T>&) { return type_object<T>(); },
      "cod(f): the codomain object (a type).");
}

}  // namespace

NB_MODULE(_jlt, m) {
  m.doc() =
      "The Jlt arrow DSL (#961): a fluent, point-free calculus of involutive "
      "endomorphisms over two objects, bool and int.  Generators are `id(T)` "
      "and `refl(T)` (reflection: not on bool, negation on int).  Compose with "
      "`f >> g` (apply f, then g); apply with `a(x)`; inspect with `dom(a)` / "
      "`cod(a)`.  Arrows are extensional (functions); composing across objects "
      "(bool vs int) is not defined and raises.";

  bind_carrier<bool>(m, "IdentityBool", "MorphismBool");
  bind_carrier<int>(m, "IdentityInt", "MorphismInt");

  // id(T) / refl(T): the primitive arrows on carrier T (a Python type object,
  // bool or int) -- mirroring :morphism's id<T>().  refl is the reflection
  // involution: `not` on bool, negation on int.  `not` is a Python keyword, so
  // the reflection is exposed as `refl` (uniform across carriers).
  m.def(
      "id",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(jlt::id<bool>());
        if (t.is(type_object<int>())) return nb::cast(jlt::id<int>());
        throw nb::type_error("id(T): T must be bool or int");
      },
      nb::arg("carrier"), "id(T): the identity arrow on carrier T.");
  m.def(
      "refl",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(jlt::refl_bool());
        if (t.is(type_object<int>())) return nb::cast(jlt::refl_int());
        throw nb::type_error("refl(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "refl(T): the reflection involution on carrier T (not on bool, negation "
      "on int).");
}
