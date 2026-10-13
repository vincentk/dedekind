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

#include <concepts>
#include <cstdint>
#include <functional>
#include <limits>
#include <stdexcept>
#include <string>

import dedekind.category; // Identity, Successor, Predecessor, the species

namespace nb = nanobind;

namespace {
using dedekind::category::Boole;
using dedekind::category::cata;
using dedekind::category::Chain;
using dedekind::category::Dom;
using dedekind::category::HasNNOStep;
using dedekind::category::Identity;
using dedekind::category::IsEndomorphism;
using dedekind::category::IsOckhamAlgebra;
using dedekind::category::logic_complement;
using dedekind::category::Morphism;
using dedekind::category::Predecessor;
using dedekind::category::Successor;

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
/** @brief The type-erased arrow @c T→T a Python handle holds: the real
 *  @c :morphism @c Morphism with a @c std::function transform, so a category
 *  arrow (@c IsArrow / @c IsEndomorphism hold).  @tparam T the carrier. */
template <typename T>
using Arrow = Morphism<T, T, std::function<T(T)>>;
/** @brief What a Python @c int crosses the boundary as: the canonical ring
 *  u64 = ℤ/2⁶⁴ (@c dedekind.pst), whose step wraps. */
using Int = std::uint64_t;

/** @brief @c refl on a chain: @c :logic's pre-configured reflection
 *  @c logic_complement<L> (@c L::RFL: @c ¬ on @c Boole, the order-reversing
 *  @c ~ on @c Chain<Int>), already witnessed an involution there, erased.
 *  @tparam L the species whose @c Ω is the carrier. */
template <IsOckhamAlgebra L>
Arrow<typename L::Ω> refl() {
  using T = typename L::Ω;
  return Arrow<T>{std::function<T(T)>{logic_complement<L>{}}};
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
Arrow<Dom<F>> compose(const F& f, const G& g) {
  using T = Dom<F>;
  return Arrow<T>{std::function<T(T)>{cata(f >> g)}};
}

}  // namespace jlt

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
  bind_rshift<S, Identity<T>>(cls);
  bind_rshift<S, jlt::Arrow<T>>(cls);
  bind_rshift<S, Successor<T>>(cls);
  bind_rshift<S, Predecessor<T>>(cls);
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
 *  the arrow applied: total on @c bool (saturating) and on the word (wrapping).
 *  endo-arrow. */
template <typename S>
void bind_step_operators(nb::class_<S>& cls) {
  using T = typename S::Domain;
  cls.def(
      "__call__", [](const S& f, T x) { return f(x); },
      "Apply the step: saturating on bool, wrapping on the word u64.");
  bind_compositions(cls);
}

/** @brief Bind the step arrows on carrier @c T as their own classes, typed:
 *  the real @c :nno @c Successor / @c Predecessor, composable like @c id, and
 *  read by @c dedekind.pst.image. */
template <typename T>
void bind_steps(nb::module_& m, const char* succ_name, const char* pred_name) {
  auto succ = nb::class_<Successor<T>>(
      m, succ_name,
      "The successor arrow S : T -> T (the real :nno Successor), total and "
      "saturating at the chain's top.");
  bind_step_operators(succ);
  succ.def("__repr__", [](const Successor<T>&) { return "succ"; });
  auto pred = nb::class_<Predecessor<T>>(
      m, pred_name,
      "The predecessor arrow P : T -> T (the real :nno Predecessor), total and "
      "saturating at the chain's bottom (the monus on ℕ).");
  bind_step_operators(pred);
  pred.def("__repr__", [](const Predecessor<T>&) { return "pred"; });
  m.def("dom", [](const Successor<T>&) { return type_object<T>(); });
  m.def("cod", [](const Successor<T>&) { return type_object<T>(); });
  m.def("dom", [](const Predecessor<T>&) { return type_object<T>(); });
  m.def("cod", [](const Predecessor<T>&) { return type_object<T>(); });
}

/** @brief Bind one carrier @c T: the real @c Identity<T> and the type-erased
 *  @c Morphism<T,T>, their operators, and the free accessors @c dom / @c cod.
 */
template <typename T>
void bind_carrier(nb::module_& m, const char* identity_name,
                  const char* morphism_name) {
  auto identity = nb::class_<Identity<T>>(
      m, identity_name,
      "The identity arrow id: T -> T (the monoid unit).  A real :morphism "
      "Identity<T>.");
  bind_endo_operators(identity);
  identity.def("__repr__", [](const Identity<T>&) { return "id"; });

  auto morphism = nb::class_<jlt::Arrow<T>>(
      m, morphism_name,
      "A type-erased endo-arrow T -> T (the real :morphism Morphism).  "
      "Composition and refl land here.");
  bind_endo_operators(morphism);
  morphism.def("__repr__", [](const jlt::Arrow<T>&) { return "<arrow>"; });

  m.def(
      "dom", [](const Identity<T>&) { return type_object<T>(); },
      "dom(f): the domain object (a type).");
  m.def(
      "dom", [](const jlt::Arrow<T>&) { return type_object<T>(); },
      "dom(f): the domain object (a type).");
  m.def(
      "cod", [](const Identity<T>&) { return type_object<T>(); },
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
        if (t.is(type_object<bool>())) return nb::cast(Identity<bool>{});
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(Identity<jlt::Int>{});
        throw nb::type_error("id(T): T must be bool or int");
      },
      nb::arg("carrier"), "id(T): the identity arrow on carrier T.");
  m.def(
      "refl",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(jlt::refl<Boole>());
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(jlt::refl<Chain<jlt::Int>>());
        throw nb::type_error("refl(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "refl(T): the reflection involution on carrier T (not on bool, negation "
      "on int).");
  m.def(
      "succ",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(Successor<bool>{});
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(Successor<jlt::Int>{});
        throw nb::type_error("succ(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "succ(T): the successor arrow on carrier T, the NNO step (saturating: "
      "succ(bool)(True) is True; on int, the word u64, it wraps: succ(2**64 - "
      "1) is 0).");
  m.def(
      "pred",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(Predecessor<bool>{});
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(Predecessor<jlt::Int>{});
        throw nb::type_error("pred(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "pred(T): the predecessor arrow on carrier T (saturating at the bottom: "
      "pred(bool)(False) is False).");
}
