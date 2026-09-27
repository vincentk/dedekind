/**
 * @file dedekind/python/nanobind_lwv.cpp
 * @brief Nanobind extension module for the Lwv set-comprehension DSL (#965).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * The SET twin of the @c jlt arrow exhibit: a first iteration of the @b Lwv
 * language (paper §3, @c Lwv @c := @c Jlt @c ∩ @c Set), bound as the private
 * native extension @c dedekind._lwv (NumPy-style, re-exported by the
 * pure-Python
 * @c dedekind.lwv facade).  It exposes the @b real @c :order halfspaces --- the
 * README exhibit @c gt_3 @c = @c {x>3} and @c lt_5 @c = @c {x<5} over @c int
 * --- as @b duck-typed handles: a "set" is the protocol @c __call__ /
 * @c __contains__ (membership χ) plus the algebra operators.
 *
 * The headline: @c gt_3 @c & @c lt_5 routes through the @b real @c :order
 * reducer (@c structured_and, the value-first crossing law
 * @c ↑a @c ∩ @c ↓b @c = @c [a,b]) and COLLAPSES to the singleton @c {4} --- the
 * README's compile-time @c static_assert collapse, now observed at Python
 * runtime.  There is no Python-side reducer; the handle dispatches to the C++
 * one (as @c jlt's @c >> dispatches to @c cata).
 *
 * First iteration: the pivots are compile-time NTTPs, so this binds the curated
 * README sets exactly as @c jlt binds the fixed generators @c id / @c refl.  A
 * general fluent constructor over @b runtime pivots awaits the value-first
 * subobject reducer's leaf-combine leg (#922 slice 2).
 */

#include <nanobind/nanobind.h>

import dedekind.python; // dedekind::python::lwv: Above / Below / meet

namespace nb = nanobind;

namespace {
namespace lwv = dedekind::python::lwv;

using Gt3 = lwv::Above<3>;  // {x ∈ int | x > 3} = ↑3 (open)
using Lt5 = lwv::Below<5>;  // {x ∈ int | x < 5} = ↓5 (open)
using Collapse =
    lwv::ReadmeCollapse;  // = decltype(meet(Gt3, Lt5)) = Singleton{4}

/** @brief Bind the membership protocol (@c __call__ / @c __contains__) on a
 *  bound set class @c S: @c s(x) / @c x @c in @c s is the characteristic map χ,
 *  decided to a Python @c bool. */
template <typename S, typename X>
void bind_membership(nb::class_<S>& cls) {
  cls.def(
         "__call__", [](const S& s, X x) { return static_cast<bool>(s(x)); },
         nb::arg("x"), "Membership χ(x): whether x is in the set.")
      .def(
          "__contains__",
          [](const S& s, X x) { return static_cast<bool>(s(x)); }, nb::arg("x"),
          "x in s: the same membership test, Python-idiomatic.");
}

}  // namespace

NB_MODULE(_lwv, m) {
  m.doc() =
      "The Lwv set-comprehension DSL (#965, paper §3: Lwv := Jlt ∩ Set): "
      "intensional halfspaces over int, bound as duck-typed handles "
      "(membership "
      "via `s(x)` / `x in s`).  The README exhibit `gt_3 = {x>3}` and "
      "`lt_5 = {x<5}` meet through the real :order reducer: `gt_3 & lt_5` "
      "collapses to the singleton {4}.  Pivots are compile-time (this first "
      "iteration binds the curated README sets, as jlt binds id/refl); runtime "
      "pivots await #922 slice 2.";

  auto gt3 = nb::class_<Gt3>(m, "UpperHalfspace",
                             "The strict upper halfspace {x ∈ int | x > 3} = "
                             "the principal filter ↑3.");
  bind_membership<Gt3, int>(gt3);
  gt3.def("__repr__", [](const Gt3&) { return "{x | x > 3}"; });

  auto lt5 = nb::class_<Lt5>(
      m, "LowerHalfspace",
      "The strict lower halfspace {x ∈ int | x < 5} = the principal ideal ↓5.");
  bind_membership<Lt5, int>(lt5);
  lt5.def("__repr__", [](const Lt5&) { return "{x | x < 5}"; });

  auto collapse = nb::class_<Collapse>(
      m, "Singleton",
      "The singleton {4}: the reduced meet {x>3} ∩ {x<5}, collapsed by the "
      ":order reducer (cardinality analysis on an integral carrier).");
  bind_membership<Collapse, typename Collapse::Domain>(collapse);
  collapse.def("__repr__", [](const Collapse&) { return "{4}"; });
  collapse.def_prop_ro(
      "cardinality", [](const Collapse&) { return 1; },
      "The finite cardinality of the collapsed set: |{4}| = 1.");

  // gt_3 & lt_5: the meet routes through the real :order reducer
  // (dedekind::python::lwv::meet -> :order structured_and) and collapses to the
  // singleton {4}.  The result TYPE is read off the reducer (Collapse), never
  // restated here -- the README exhibit at Python runtime.
  gt3.def(
      "__and__", [](const Gt3& a, const Lt5& b) { return lwv::meet(a, b); },
      "gt_3 & lt_5: intersection through the :order reducer -> the singleton "
      "{4} (the README collapse).");

  // The two README exhibit sets, as module-level handles (fixed generators, as
  // jlt exposes id / refl).
  //
  // FIXME(#965): one module constant per pivot does NOT scale (would need
  // gt_0, gt_1, ...).  Iteration 2 (its own PR) is the scalable, value-oriented
  // surface: the halfspace as preimage_1(S * eta(p) | (chi/pi_1 <= pi_2)) with
  // the pivot p a VALUE in the singleton eta(p) and the relation fixed and
  // pivot-free, so a single `chi <= fix(p)` constructor covers all pivots; the
  // meet collapse routes through the value-first reduce_meet.  Design on #965.
  m.attr("gt_3") = Gt3{};
  m.attr("lt_5") = Lt5{};
}
