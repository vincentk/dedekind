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
nb::object type_object<jlt::Int>() {
  return nb::borrow<nb::object>(reinterpret_cast<PyObject*>(&PyLong_Type));
}

/** @brief Bind @c f @c >> @c g for one right-operand type @c G.  The C++
 *  @c operator>> itself returns a @c Compose node, one type per composition
 *  shape, which the reducer wants and Python cannot hold; so the binding goes
 *  through @c jlt::compose, which is @c operator>>, then @c cata, then the
 *  erasure to the one handle type @c Arrow<T>.  Hence a lambda, not
 *  @c nb::self @c >> @c nb::other. */
template <typename S, typename G>
void bind_rshift(nb::class_<S>& cls) {
  cls.def(
      "__rshift__", [](const S& f, const G& g) { return jlt::compose(f, g); },
      "f >> g (apply f, then g): the reducer's composition, erased to an "
      "arrow; same-object operands only.");
}

/** @brief Bind same-object composition @c >> to a bound arrow class @c S, one
 *  overload per right operand.  @c >> accepts only same-object operands, so
 *  cross-object composition raises automatically (no matching overload) ---
 *  the composability guard. */
template <typename S>
void bind_compositions(nb::class_<S>& cls) {
  using T = typename S::Domain;
  bind_rshift<S, jlt::Id<T>>(cls);
  bind_rshift<S, jlt::Arrow<T>>(cls);
  bind_rshift<S, jlt::Succ<T>>(cls);
  bind_rshift<S, jlt::Pred<T>>(cls);
}

/** @brief The arrow @b operators (apply @c (), composition @c >>) of a bound
 *  arrow class @c S. */
template <typename S>
void bind_endo_operators(nb::class_<S>& cls) {
  using T = typename S::Domain;
  cls.def(
      "__call__", [](const S& f, T x) { return f(x); },
      "Apply the arrow to an object of its carrier.");
  bind_compositions(cls);
}

/** @brief The arrow operators of a typed step @c S: @c __call__ through
 *  @c jlt::step_in_window (total on @c bool; at the integer window's end it
 *  raises @c OverflowError instead of overflowing), and @c >> as for every
 *  endo-arrow. */
template <typename S>
void bind_step_operators(nb::class_<S>& cls) {
  using T = typename S::Domain;
  cls.def(
      "__call__", [](const S& f, T x) { return jlt::step_in_window(f, x); },
      "Apply the step; at the end of the 64-bit window it raises "
      "OverflowError (ℤ continues, the window does not).");
  bind_compositions(cls);
}

/** @brief Bind the step arrows on carrier @c T as their own classes, typed:
 *  the real @c :nno @c Successor / @c Predecessor, composable like @c id, and
 *  readable by @c lwv.image. */
template <typename T>
void bind_steps(nb::module_& m, const char* succ_name, const char* pred_name) {
  auto succ = nb::class_<jlt::Succ<T>>(
      m, succ_name,
      "The successor arrow S : T -> T (the real :nno Successor), total and "
      "saturating at the chain's top.");
  bind_step_operators(succ);
  succ.def("__repr__", [](const jlt::Succ<T>&) { return "succ"; });
  auto pred = nb::class_<jlt::Pred<T>>(
      m, pred_name,
      "The predecessor arrow P : T -> T (the real :nno Predecessor), total and "
      "saturating at the chain's bottom (the monus on ℕ).");
  bind_step_operators(pred);
  pred.def("__repr__", [](const jlt::Pred<T>&) { return "pred"; });
  m.def("dom", [](const jlt::Succ<T>&) { return type_object<T>(); });
  m.def("cod", [](const jlt::Succ<T>&) { return type_object<T>(); });
  m.def("dom", [](const jlt::Pred<T>&) { return type_object<T>(); });
  m.def("cod", [](const jlt::Pred<T>&) { return type_object<T>(); });
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
      "and `refl(T)`, the reflection involution (:logic's logic_complement: "
      "not "
      "on bool, the order-reversing ~ on the int chain).  Compose with `f >> "
      "g` "
      "(apply f, then g); apply with `a(x)`; inspect with `dom(a)` / `cod(a)`. "
      " "
      "Arrows are extensional (functions); composing across objects (bool vs "
      "int) is not defined and raises.";

  bind_carrier<bool>(m, "IdentityBool", "MorphismBool");
  bind_carrier<jlt::Int>(m, "IdentityInt", "MorphismInt");
  bind_steps<bool>(m, "SuccessorBool", "PredecessorBool");
  bind_steps<jlt::Int>(m, "SuccessorInt", "PredecessorInt");

  // id(T) / refl(T): the primitive arrows on carrier T (a Python type object,
  // bool or int) -- mirroring :morphism's id<T>().  refl is :logic's reflection
  // involution logic_complement<L>: `not` (¬) on bool, the order-reversing `~`
  // on the int chain.  `not` is a Python keyword, so it is exposed as `refl`.
  m.def(
      "id",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(jlt::id<bool>());
        if (t.is(type_object<jlt::Int>())) return nb::cast(jlt::id<jlt::Int>());
        throw nb::type_error("id(T): T must be bool or int");
      },
      nb::arg("carrier"), "id(T): the identity arrow on carrier T.");
  m.def(
      "refl",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(jlt::refl_bool());
        if (t.is(type_object<jlt::Int>())) return nb::cast(jlt::refl_int());
        throw nb::type_error("refl(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "refl(T): the reflection involution on carrier T (not on bool, negation "
      "on int).");
  m.def(
      "succ",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(jlt::Succ<bool>{});
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(jlt::Succ<jlt::Int>{});
        throw nb::type_error("succ(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "succ(T): the successor arrow on carrier T, the NNO step (saturating: "
      "succ(bool)(True) is True; on int the 64-bit window ends at 2**63 - 1 "
      "and succ raises OverflowError there).");
  m.def(
      "pred",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(jlt::Pred<bool>{});
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(jlt::Pred<jlt::Int>{});
        throw nb::type_error("pred(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "pred(T): the predecessor arrow on carrier T (saturating at the bottom: "
      "pred(bool)(False) is False).");
}
