/**
 * @file dedekind/python/nanobind_pst.cpp
 * @brief Nanobind extension module for the Pst chains (#1001).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * @f$\mathbf{Pst} = \mathbf{Jlt} \cap \mathbf{Chain}@f$ (paper §3): the bounded
 * chains the sets are valued in, bound as the private native extension
 * @c dedekind._pst (NumPy-style, re-exported by @c dedekind.pst).  Three chains
 * are bound, by carrier: @c B (𝔹, the two truth values), @c K3 (Kleene's
 * three), and @c N (ℕ's proxy @c Cardinality, a bounded chain in the same shape
 * with ⊤ = ℵ₀).  A chain is a handle exposing its endpoints (@c bottom / @c
 * top), its step in both readings --- the total, saturating @c succ / @c pred
 * (the algebra side) and the partial @c cover (the coalgebra side, @c None at
 * ⊤) --- its order (@c le), and its classification as the C++ concepts decide
 * it
 * (@c is_truth_object, @c is_dense, @c saturates).  Iterating a chain unfolds
 * it from @c bottom by the cover and stops where the cover stops: at ⊤ for 𝔹
 * and K₃, never in finite time for ℕ, whose ⊤ is a limit, not a successor.
 * Reduction stays in C++; Python holds handles.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/string.h>

#include <cstddef>
#include <optional>
#include <string>
#include <variant>

import dedekind.category; // Ternary, HasNNOStep, IsPst
import dedekind.python;   // dedekind::python::pst
import dedekind.sets;     // Cardinality, ℵ_0, finite_cardinality

namespace nb = nanobind;

namespace {
namespace pst = dedekind::python::pst;
using dedekind::category::Ternary;
using dedekind::sets::Cardinality;
using dedekind::sets::ℵ_0;

/** @brief ℕ's elements cross the boundary as Python @c int, its top as the one
 *  @c aleph0 object. */
nb::object to_py_cardinality(const Cardinality& c) {
  if (std::holds_alternative<ℵ_0>(c)) return nb::cast(ℵ_0{});
  return nb::int_(std::get<dedekind::sets::ExtensionalCardinal<>>(c).value);
}
Cardinality from_py_cardinality(nb::handle h) {
  if (nb::isinstance<ℵ_0>(h)) return Cardinality{ℵ_0{}};
  if (nb::isinstance<nb::int_>(h)) {
    const auto v = nb::cast<long long>(h);
    if (v < 0) throw nb::value_error("ℕ has no negative elements");
    return dedekind::sets::finite_cardinality(static_cast<std::size_t>(v));
  }
  throw nb::type_error("an element of ℕ is an int or aleph0");
}
template <typename C>
nb::object to_py(const C& x) {
  if constexpr (std::same_as<C, Cardinality>)
    return to_py_cardinality(x);
  else
    return nb::cast(x);
}
template <typename C>
C from_py_as(nb::handle h) {
  if constexpr (std::same_as<C, Cardinality>)
    return from_py_cardinality(h);
  else
    return nb::cast<C>(h);
}

/** @brief The unfold of a chain from its bottom by the cover: @c StopIteration
 *  is the Python spelling of the cover's @c nullopt at ⊤. */
template <typename C>
struct ChainIter {
  std::optional<C> current;
};

template <typename C>
void bind_chain(nb::module_& m, const char* cls, const char* iter_cls,
                const char* doc) {
  using Ch = pst::Chain<C>;
  nb::class_<ChainIter<C>>(m, iter_cls)
      .def("__iter__", [](ChainIter<C>& it) -> ChainIter<C>& { return it; })
      .def("__next__", [](ChainIter<C>& it) {
        if (!it.current) throw nb::stop_iteration();
        const C value = *it.current;
        it.current = pst::cover<C>(value);
        return to_py<C>(value);
      });
  nb::class_<Ch>(m, cls, doc)
      .def_prop_ro(
          "name", [](const Ch&) { return std::string(Ch::name); },
          "The chain's name.")
      .def_prop_ro(
          "bottom", [](const Ch&) { return to_py<C>(Ch::bottom); },
          "⊥, the least element.")
      .def_prop_ro(
          "top", [](const Ch&) { return to_py<C>(Ch::top); },
          "⊤, the greatest element (ℵ₀ on ℕ, a limit).")
      .def(
          "succ",
          [](const Ch&, nb::handle x) {
            return to_py<C>(pst::succ<C>(from_py_as<C>(x)));
          },
          nb::arg("x"),
          "S(x): the successor, total and saturating at ⊤ (the algebra side).")
      .def(
          "pred",
          [](const Ch&, nb::handle x) {
            return to_py<C>(pst::pred<C>(from_py_as<C>(x)));
          },
          nb::arg("x"),
          "P(x): the predecessor, total and saturating at ⊥ (the monus on ℕ).")
      .def(
          "cover",
          [](const Ch&, nb::handle x) -> nb::object {
            const auto c = pst::cover<C>(from_py_as<C>(x));
            return c ? to_py<C>(*c) : nb::none();
          },
          nb::arg("x"),
          "The element covering x, or None at ⊤: the successor read partially, "
          "N → 1 + N (the coalgebra side).")
      .def(
          "le",
          [](const Ch&, nb::handle x, nb::handle y) {
            return from_py_as<C>(x) <= from_py_as<C>(y);
          },
          nb::arg("x"), nb::arg("y"), "x ≤ y in the chain's order.")
      .def(
          "__iter__", [](const Ch&) { return ChainIter<C>{Ch::bottom}; },
          "Unfold the chain from ⊥ by the cover; stops where the cover stops.")
      .def_prop_ro(
          "is_bounded", [](const Ch&) { return true; },
          "Every Pst chain is bounded: it has ⊥ and ⊤.")
      .def_prop_ro(
          "is_truth_object", [](const Ch&) { return pst::is_truth_object<C>; },
          "IsPst<C>: whether the chain is a truth object (carries ∧, ∨, ¬).")
      .def_prop_ro(
          "is_dense", [](const Ch&) { return pst::is_dense<C>; },
          "IsDense<C>: whether a midpoint always exists (never, on a chain "
          "with the step).")
      .def_prop_ro(
          "saturates", [](const Ch&) { return pst::saturates<C>(); },
          "Whether S(⊤) = ⊤: the successor's posture at the top.")
      .def_prop_ro(
          "cardinality",
          [](const Ch&) { return to_py_cardinality(Ch::cardinality); },
          "The number of elements: 2, 3, or ℵ₀.")
      .def("__repr__", [](const Ch&) { return std::string(Ch::name); });
}
}  // namespace

NB_MODULE(_pst, m) {
  m.doc() =
      "The Pst chains (#1001, paper §3): Pst = Jlt ∩ Chain, the bounded chains "
      "the sets are valued in.  B (𝔹), K3 (Kleene's three truth values) and N "
      "(ℕ's proxy, ⊤ = ℵ₀) expose bottom / top, the total saturating succ / "
      "pred, the partial cover (None at ⊤), the order le, their classification "
      "by the C++ concepts, and iterate from ⊥ by the cover.";

  nb::enum_<Ternary>(m, "Ternary", "Kleene's three truth values, a chain.")
      .value("FALSE", Ternary::False)
      .value("UNKNOWN", Ternary::Unknown)
      .value("TRUE", Ternary::True);
  nb::class_<ℵ_0>(m, "Aleph0",
                  "ℵ₀, the top of ℕ's proxy: the countable "
                  "cardinal, the memory boundary.")
      .def("__repr__", [](const ℵ_0&) { return std::string("ℵ₀"); })
      .def(
          "__eq__", [](const ℵ_0&, const ℵ_0&) { return true; },
          nb::is_operator())
      .def("__hash__", [](const ℵ_0&) { return 0; });
  m.attr("aleph0") = ℵ_0{};

  bind_chain<bool>(m, "ChainB", "ChainBIter",
                   "𝔹 = {False < True}: the two-element truth chain.");
  bind_chain<Ternary>(m, "ChainK3", "ChainK3Iter",
                      "K₃ = {FALSE < UNKNOWN < TRUE}: Kleene's truth chain.");
  bind_chain<Cardinality>(
      m, "ChainN", "ChainNIter",
      "ℕ's proxy: 0 < 1 < 2 < … < ℵ₀, a bounded chain whose top is a limit.");
  m.attr("B") = pst::Chain<bool>{};
  m.attr("K3") = pst::Chain<Ternary>{};
  m.attr("N") = pst::Chain<Cardinality>{};
}
