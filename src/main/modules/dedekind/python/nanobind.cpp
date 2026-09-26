/**
 * @file dedekind/python/nanobind.cpp
 * @brief Nanobind MVP entrypoint for the curated Python facade.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * This translation unit binds a deliberately small, reviewable subset of the
 * wrapper-facing `dedekind.python` facade for Python smoke tests and notebook
 * oriented experiments.
 *
 * Binding design:
 *  - `dedekind.sets`      (extensional, finite) ↔ Python `set`  (std::set<T>)
 *  - `dedekind.sequences` (finite paths)         ↔ Python `list`
 * (std::vector<T>)
 *
 * Set operations (`set_union`, `set_intersection`, `set_difference`,
 * `set_cardinality`) are overloaded for Python's standard scalar types:
 * `bool`, `int`, `float` (C++ `double`), and `str` (C++ `std::string`).
 * Nanobind performs overload resolution at call time; passing a set of mixed
 * or unsupported element types raises `TypeError` automatically.
 *
 * @note "La science est faite de données comme une maison de pierres."
 *       -- Henri Poincare, paraphrase
 *       [Trans: "Science is built from facts as a house is built from stones."]
 */

#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>
#include <nanobind/stl/pair.h>
#include <nanobind/stl/set.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/tuple.h>
#include <nanobind/stl/vector.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <set>
#include <span>
#include <string>
#include <tuple>
#include <unordered_set>
#include <utility>
#include <vector>

import dedekind.analysis; // Dual<F> (relocated from :numbers at PR #513)
import dedekind.numbers;
import dedekind.optimization; // maximize_with_values, HalfspaceTriple
import dedekind.python;
import dedekind.sets; // SignedExtensionalCardinal (carrier for paper-faithful Rat)

namespace nb = nanobind;

namespace {

// ── sequences: Python list ↔ dedekind.sequences ──────────────────────────
// These functions accept and return std::vector<int> (Python list) because
// they exercise the FinitePath / range adapters in dedekind.sequences.

auto ordered_set_roundtrip(const std::vector<int>& values) -> std::vector<int> {
  const std::set<int> ordered(values.begin(), values.end());
  const auto ext = dedekind::python::from_std(ordered);
  const auto back = dedekind::python::to_std<std::set<int>>(ext);
  return {back.begin(), back.end()};
}

auto unordered_set_roundtrip(const std::vector<int>& values)
    -> std::vector<int> {
  const std::unordered_set<int> unordered(values.begin(), values.end());
  const auto ext = dedekind::python::from_std(unordered);
  const auto back = dedekind::python::to_std<std::unordered_set<int>>(ext);
  std::vector<int> materialized(back.begin(), back.end());
  std::sort(materialized.begin(), materialized.end());
  return materialized;
}

auto path_from_range(const std::vector<int>& values) -> std::vector<int> {
  const auto path = dedekind::python::from_range(values);
  const auto& view = dedekind::python::as_range(path);
  return {view.begin(), view.end()};
}

// ── arrays: NumPy ndarray ↔ dedekind.sequences ──────────────────────────
// Typed bindings for NumPy arrays to std::vector via the buffer protocol.
// Supports bool, int64, double; falls back to Python sequence for str.

template <typename T>
auto path_from_array(nb::ndarray<T, nb::ndim<1>, nb::c_contig> arr)
    -> std::vector<T> {
  const auto size = static_cast<std::size_t>(arr.shape(0));
  std::vector<T> result;
  result.reserve(size);
  for (std::size_t i = 0; i < size; ++i) {
    result.push_back(arr(static_cast<int64_t>(i)));
  }

  return result;
}

auto path_from_str_seq(nb::sequence seq) -> std::vector<std::string> {
  std::vector<std::string> result;
  for (nb::handle h : seq) result.push_back(nb::cast<std::string>(h));
  return result;
}

// ── extensional sets: Python set ↔ dedekind.sets ─────────────────────────
// These functions accept and return std::set<T> (Python set) and route
// through ExtensionalSet<T> via the dedekind.python facade.
// Overloaded for bool, int, double (float), std::string (str).

template <typename T>
auto ext_union(const std::set<T>& a, const std::set<T>& b) -> std::set<T> {
  std::set<T> merged(a);
  merged.insert(b.begin(), b.end());
  const auto ext = dedekind::python::from_std(merged);
  return dedekind::python::to_std<std::set<T>>(ext);
}

template <typename T>
auto ext_intersection(const std::set<T>& a, const std::set<T>& b)
    -> std::set<T> {
  const auto ext_a = dedekind::python::from_std(a);
  const auto ext_b = dedekind::python::from_std(b);
  std::set<T> result;
  for (const auto& v : ext_a) {
    if (ext_b.contains(v)) result.insert(v);
  }
  return result;
}

template <typename T>
auto ext_difference(const std::set<T>& a, const std::set<T>& b) -> std::set<T> {
  const auto ext_a = dedekind::python::from_std(a);
  const auto ext_b = dedekind::python::from_std(b);
  std::set<T> result;
  for (const auto& v : ext_a) {
    if (!ext_b.contains(v)) result.insert(v);
  }
  return result;
}

template <typename T>
auto ext_cardinality(const std::set<T>& s) -> std::size_t {
  return dedekind::python::from_std(s).size();
}

// Convenience: register all four set-algebra overloads for one element type.
//
// FIXME(#886): these bind only EXTENSIONAL std::set.  The intensional sets
// (boundary Ø / 𝔸, Singleton, predicate Sets) are not yet Python objects; the
// agreed shape is to model Ø / 𝔸 ~1:1, expose membership as Python `x in s`
// (__contains__ over the C++ operator()), and `|` as the set-builder filter --
// with the C++ core shaping the bindings, not the reverse.  Tracked in #886;
// deliberately out of scope for the #892 reducer-sets removal.
template <typename T>
void register_set_ops(nb::module_& m) {
  m.def(
      "set_union",
      [](const std::set<T>& a, const std::set<T>& b) {
        return ext_union(a, b);
      },
      "A ∪ B — union of two sets (dedekind.sets).");
  m.def(
      "set_intersection",
      [](const std::set<T>& a, const std::set<T>& b) {
        return ext_intersection(a, b);
      },
      "A ∩ B — intersection of two sets (dedekind.sets).");
  m.def(
      "set_difference",
      [](const std::set<T>& a, const std::set<T>& b) {
        return ext_difference(a, b);
      },
      "A ∖ B — set difference (dedekind.sets).");
  m.def(
      "set_cardinality",
      [](const std::set<T>& s) { return ext_cardinality(s); },
      "|S| — cardinality of a finite extensional set (dedekind.sets).");
}

}  // namespace

// ── Complex and Dual number bindings ──────────────────────────────────────
// These bindings expose algebraic extensions (Complex<double>, Dual<double>)
// for experimental numeric computations from Python.

void bind_complex(nb::module_& m) {
  using Complex = dedekind::numbers::Complex<double>;
  nb::class_<Complex>(m, "Complex",
                      "Represents a complex number z = re + im*i.")
      .def(nb::init<double, double>(), nb::arg("re") = 0.0, nb::arg("im") = 0.0,
           "Construct a complex number from real and imaginary parts.")
      .def("real", &Complex::real, "Extract the real part.")
      .def("imag", &Complex::imag, "Extract the imaginary part.")
      .def(
          "__add__", [](const Complex& a, const Complex& b) { return a + b; },
          "Addition of complex numbers.")
      .def(
          "__mul__", [](const Complex& a, const Complex& b) { return a * b; },
          "Multiplication of complex numbers.")
      .def("__repr__", [](const Complex& z) {
        return std::string("Complex(") + std::to_string(z.real()) + "+" +
               std::to_string(z.imag()) + "i)";
      });
}

// ── 2D LP across the bridge on a Dual<Rational> carrier ────────────────
// The single LP binding exposed to Python.  Exact, paper-faithful carrier:
// `Rational<SignedExtensionalCardinal<>>` is the same ℚ the compile-time
// NTTP showcase uses; `Dual<F>` lifts the active-set enumeration into the
// forward-mode AD product (val, der).  Result: optimum primal AND its
// first-order sensitivity, ℚ-exact, from one runtime call to the same
// `constexpr` kernel the compile-time exhibit folds.
//
// Carrier-pluggability lemma (witnessed by the C++ test suite, not
// re-bound here): the kernel is `T`-generic.  Approximate carriers
// (`double`, `Dual<double>`) subsume to this one as ε → 0 or as the
// rationals' denominators → 1, so a separate Python binding is redundant.

using Z = dedekind::sets::SignedExtensionalCardinal<>;
using Rat = dedekind::numbers::Rational<Z>;
using DualRat = dedekind::analysis::Dual<Rat>;

auto maximize_lp_dual_rational(
    std::pair<DualRat, DualRat> objective,
    const std::vector<std::tuple<DualRat, DualRat, DualRat>>& halfspaces)
    -> std::tuple<DualRat, DualRat, bool> {
  std::vector<dedekind::optimization::HalfspaceTriple<DualRat>> coeffs;
  coeffs.reserve(halfspaces.size());
  for (const auto& [a, b, c] : halfspaces) {
    coeffs.push_back({a, b, c});
  }
  const auto result = dedekind::optimization::maximize_with_values<DualRat>(
      std::span<const dedekind::optimization::HalfspaceTriple<DualRat>>(coeffs),
      objective.first, objective.second);
  const auto& pred = result.predicate();
  return {pred.point.x, pred.point.y, pred.feasible};
}

// ── Rational<SignedExtensionalCardinal<>> ── exact ℚ for paper-faithful inputs
void bind_rational(nb::module_& m) {
  nb::class_<Rat>(
      m, "Rational",
      "Exact rational `num/den` over the project's paper-facing integer "
      "carrier (SignedExtensionalCardinal).  Same `Rat` type used by the "
      "compile-time LP showcase.")
      .def("__init__", [](Rat* self) { new (self) Rat{}; })
      .def(
          "__init__", [](Rat* self, long n) { new (self) Rat{Z{n}}; },
          nb::arg("n"), "Integer embedding `n / 1`.")
      .def(
          "__init__",
          [](Rat* self, long num, long den) { new (self) Rat{Z{num}, Z{den}}; },
          nb::arg("num"), nb::arg("den"),
          "Construct `num / den`.  Denominator must be non-zero.")
      .def(
          "num", [](const Rat& r) { return static_cast<long>(r.num()); },
          "Numerator as a Python int (single-limb carrier).")
      .def(
          "den", [](const Rat& r) { return static_cast<long>(r.den()); },
          "Denominator as a Python int (single-limb carrier).")
      .def(
          "__add__", [](const Rat& a, const Rat& b) { return a + b; },
          "Rational addition.")
      .def(
          "__sub__", [](const Rat& a, const Rat& b) { return a - b; },
          "Rational subtraction.")
      .def(
          "__mul__", [](const Rat& a, const Rat& b) { return a * b; },
          "Rational multiplication.")
      .def(
          "__truediv__", [](const Rat& a, const Rat& b) { return a / b; },
          "Rational division.")
      .def(
          "__neg__", [](const Rat& a) { return -a; }, "Unary negation.")
      .def(
          "__eq__", [](const Rat& a, const Rat& b) { return a == b; },
          "Equality.")
      .def("__repr__", [](const Rat& r) {
        return std::string("Rational(") +
               std::to_string(static_cast<long>(r.num())) + ", " +
               std::to_string(static_cast<long>(r.den())) + ")";
      });
}

// ── Dual<Rational> ── forward-mode AD product over exact ℚ
void bind_dual_rational(nb::module_& m) {
  nb::class_<DualRat>(
      m, "DualRational",
      "Forward-mode AD product `Dual<Rational>` = `val + der·ε`, with both "
      "components exact rationals.  Lifting LP into this carrier gives "
      "optimum AND first-order sensitivity in a single solve, ℚ-exact.")
      .def("__init__", [](DualRat* self) { new (self) DualRat{}; })
      .def(
          "__init__",
          [](DualRat* self, const Rat& val) { new (self) DualRat{val}; },
          nb::arg("val"), "Dual with primal `val` and zero tangent.")
      .def(
          "__init__",
          [](DualRat* self, const Rat& val, const Rat& der) {
            new (self) DualRat{val, der};
          },
          nb::arg("val"), nb::arg("der"),
          "Dual with primal `val` and tangent `der`.")
      .def(
          "__init__",
          [](DualRat* self, long val, long der) {
            new (self) DualRat{Rat{Z{val}}, Rat{Z{der}}};
          },
          nb::arg("val"), nb::arg("der"),
          "Convenience: build `Dual<Rational>` from two Python integers.")
      .def(
          "value", [](const DualRat& d) { return d.val; },
          "Primal value as a Rational.")
      .def(
          "derivative", [](const DualRat& d) { return d.der; },
          "Tangent as a Rational.")
      .def(
          "__add__", [](const DualRat& a, const DualRat& b) { return a + b; },
          "Dual addition.")
      .def(
          "__sub__", [](const DualRat& a, const DualRat& b) { return a - b; },
          "Dual subtraction.")
      .def(
          "__mul__", [](const DualRat& a, const DualRat& b) { return a * b; },
          "Dual multiplication (chain rule: ε² = 0).")
      .def(
          "__neg__", [](const DualRat& a) { return -a; }, "Unary negation.")
      .def(
          "__eq__", [](const DualRat& a, const DualRat& b) { return a == b; },
          "Equality.")
      .def("__repr__", [](const DualRat& d) {
        return std::string("DualRational(val=Rational(") +
               std::to_string(static_cast<long>(d.val.num())) + ", " +
               std::to_string(static_cast<long>(d.val.den())) +
               "), der=Rational(" +
               std::to_string(static_cast<long>(d.der.num())) + ", " +
               std::to_string(static_cast<long>(d.der.den())) + "))";
      });
}

// ── Jlt: composition term + value-first reducer (#961) ──────────────────────
// Handle-only binding: `Arrow` is a handle over the C++ `ArrowTerm`; `simplify`
// delegates to the C++ value-first reducer (`ArrowTerm::reduce`).  Python does
// NOT implement the law --- it composes handles and asks C++ to normalise.
void bind_jlt(nb::module_& m) {
  using dedekind::python::ArrowTerm;

  nb::class_<ArrowTerm>(
      m, "Arrow",
      "A composition term over int endo-maps (the Jlt exhibit, #961).  Compose "
      "with `f >> g` (apply f, then g); normalise with `simplify`; apply with "
      "`a(x)`.  Reduction runs in C++ (value-first); Python holds the handle.")
      .def(
          "__rshift__",
          [](const ArrowTerm& f, const ArrowTerm& g) { return f >> g; },
          "f >> g: diagrammatic composition (apply f, then g).")
      .def(
          "__call__", [](const ArrowTerm& a, int x) { return a(x); },
          "Apply the arrow to an int.")
      .def(
          "__eq__",
          [](const ArrowTerm& a, const ArrowTerm& b) { return a == b; },
          "Structural equality of terms.")
      .def(
          "__repr__", [](const ArrowTerm& a) { return a.sexpr(); },
          "S-expression rendering: id | <sym> | (>> l r).");

  // `id` is a singleton Arrow VALUE (there is one identity on int), so the
  // exhibit reads `id >> id` / `simplify(id >> id) == id` without call syntax.
  m.attr("id") = ArrowTerm::id();
  m.def(
      "atom",
      [](const std::string& name, nb::callable fn) {
        return ArrowTerm::atom(name,
                               [fn](int x) { return nb::cast<int>(fn(x)); });
      },
      nb::arg("name"), nb::arg("fn"),
      "An opaque named atom wrapping a Python int->int map.");
  m.def(
      "simplify",
      [](const ArrowTerm& t) { return dedekind::python::simplify(t); },
      nb::arg("term"),
      "Value-first cata: normalise a term via the monoid unit law "
      "(id >> f == f == f >> id).  The reduction runs in C++.");
}

void bind_dual(nb::module_& m) {
  using Dual = dedekind::analysis::Dual<double>;
  nb::class_<Dual>(m, "Dual",
                   "Represents a dual number d = val + der*ε (ε² = 0).")
      .def(nb::init<double, double>(), nb::arg("val") = 0.0,
           nb::arg("der") = 0.0,
           "Construct a dual number from value and derivative parts.")
      .def("value", &Dual::value, "Extract the function value f(x).")
      .def("derivative", &Dual::derivative,
           "Extract the derivative f'(x) (forward-mode AD).")
      .def(
          "__add__", [](const Dual& a, const Dual& b) { return a + b; },
          "Addition of dual numbers.")
      .def(
          "__mul__", [](const Dual& a, const Dual& b) { return a * b; },
          "Multiplication of dual numbers (respects ε² = 0).")
      .def("__repr__", [](const Dual& d) {
        return std::string("Dual(") + std::to_string(d.value()) + "+" +
               std::to_string(d.derivative()) + "ε)";
      });
}

NB_MODULE(_dedekind, module) {
  module.doc() = "Dedekind Python MVP facade";

  // ── sequences (Python list ↔ dedekind.sequences) ─────────────────────
  module.def("ordered_set_roundtrip", &ordered_set_roundtrip,
             "Round-trip a Python list through the ordered finite-set facade "
             "(dedekind.sets).");
  module.def("unordered_set_roundtrip", &unordered_set_roundtrip,
             "Round-trip a Python list through the unordered finite-set facade "
             "(dedekind.sets).");
  module.def("path_from_range", &path_from_range,
             "Materialize a finite path from a Python list "
             "(dedekind.sequences).");

  // ── arrays (NumPy ndarray ↔ dedekind.sequences) ─────────────────────────
  // Typed NumPy array bindings (bool, int64, double) + sequence fallback.
  module.def("path_from_array", &path_from_array<bool>,
             "Materialize a finite path from a NumPy bool array "
             "(via buffer protocol).");
  module.def("path_from_array", &path_from_array<int64_t>,
             "Materialize a finite path from a NumPy int64 array "
             "(via buffer protocol).");
  module.def("path_from_array", &path_from_array<double>,
             "Materialize a finite path from a NumPy float64 array "
             "(via buffer protocol).");
  module.def("path_from_str_seq", &path_from_str_seq,
             "Materialize a finite path from a Python sequence of strings.");
  module.def("path_from_bool_array", &path_from_array<bool>,
             "Materialize a finite path from a NumPy bool array "
             "(typed helper).");
  module.def("path_from_int64_array", &path_from_array<int64_t>,
             "Materialize a finite path from a NumPy int64 array "
             "(typed helper).");
  module.def("path_from_float64_array", &path_from_array<double>,
             "Materialize a finite path from a NumPy float64 array "
             "(typed helper).");

  // ── extensional set algebra (Python set ↔ dedekind.sets) ─────────────
  // Overloads are tried in registration order: bool, int, double, str.
  register_set_ops<bool>(module);
  register_set_ops<int>(module);
  register_set_ops<double>(module);
  register_set_ops<std::string>(module);

  // ── algebraic extensions (dedekind.numbers) ─────────────────────────────
  bind_complex(module);
  bind_dual(module);
  bind_rational(module);
  bind_dual_rational(module);

  // ── Jlt: composition term + value-first reducer (#961) ──────────────────
  bind_jlt(module);

  // ── canonical sets across the bridge (#886, vertical prototype) ─────────
  // Expose the C++ universe/subobject SETS themselves as Python objects whose
  // `x in s` runs the NATIVE characteristic morphism χ.  This proves the core
  // reaches Python.
  //
  // NFKC note: Python normalises identifiers, so the double-struck source
  // names collapse to ASCII attr keys (`𝔹` → "B", `ℕ` → "N") while the
  // repr stays mathy.  The ambient universe `ℕ` and the discriminating ℕ⊂ℤ
  // classifier `N` would BOTH normalise to "N", so we resolve the collision by
  // keying the ambient universe under "N" (reached from Python as `ℕ`) and the
  // classifier under the distinct ASCII key "Nat".
  {
    using BoolUniverse = std::decay_t<decltype(dedekind::sets::𝔹)>;
    nb::class_<BoolUniverse>(module, "BooleanUniverse",
                             "The Boolean universe 𝔹 = 𝔸<bool>.")
        .def(
            "__contains__",
            [](const BoolUniverse& s, bool x) {
              return static_cast<bool>(s(x));
            },
            "x ∈ 𝔹 via the native characteristic morphism χ_𝔹.")
        .def("__repr__", [](const BoolUniverse&) { return std::string("𝔹"); });
    module.attr("B") = dedekind::sets::𝔹;

    // ℕ = 𝔸<Cardinality>, the ambient natural-numbers universe.  Membership is
    // universally true by the UniversalSet axiom (Total Presence): `4 in ℕ` is
    // True, exactly as `True in 𝔹` is.  ℕ is the universe, not a discriminator;
    // discrimination lives in the `Nat` classifier below.
    using NatUniverse = std::decay_t<decltype(dedekind::sets::ℕ)>;
    nb::class_<NatUniverse>(
        module, "NaturalUniverse",
        "The natural-numbers universe ℕ = 𝔸<Cardinality> (the ambient "
        "universe; χ_ℕ is universally true).")
        .def(
            "__contains__",
            [](const NatUniverse& s, int x) {
              // ℕ here is the AMBIENT universe 𝔸<Cardinality>: χ_ℕ is
              // universally true, so the verdict is True for every x and the
              // embedding of a negative x into the unsigned Cardinality (which
              // wraps) does not affect it.  Discrimination x ≥ 0 is the job of
              // the `Nat` classifier below, which sees the raw signed int.
              const dedekind::sets::Cardinality v = x;
              return static_cast<bool>(s(v));
            },
            "x ∈ ℕ via the native characteristic morphism χ_ℕ (universally "
            "true; ℕ is the ambient universe).")
        .def("__repr__", [](const NatUniverse&) { return std::string("ℕ"); });
    module.attr("N") = dedekind::sets::ℕ;  // source `ℕ` NFKC-normalises to "N"

    // N = NaturalNumbersOf<>, the discriminating ℤ-subobject classifier
    // (χ: x ↦ x ≥ 0).  The money shot: `-7 not in Nat` is decided in C++.
    // Keyed under "Nat" to avoid the NFKC collision with `ℕ` → "N".
    using Naturals = dedekind::sets::NaturalNumbersOf<>;
    nb::class_<Naturals>(module, "NaturalClassifier",
                         "The naturals as a subobject of ℤ (χ: x ↦ x ≥ 0): "
                         "the discriminating native classifier.")
        .def(
            "__contains__",
            [](const Naturals& s, int x) { return static_cast<bool>(s(x)); },
            "x ∈ ℕ⊂ℤ via the native classifier (Nat(-7) == False).")
        .def("__repr__", [](const Naturals&) { return std::string("ℕ⊂ℤ"); });
    module.attr("Nat") = Naturals{};  // ASCII key, distinct from `ℕ` → "N"
  }

  // ── ext: the native retraction μ: Int ⇀ Ext (#886) ─────────────────────
  // Honest minimal slice of the crossing: extensionalise a FINITE native
  // universe through a NATIVE characteristic morphism into a Python `set`.
  // Here χ is the native ℕ⊂ℤ classifier, so `ext([-2, -1, 0, 1, 2]) ==
  // {0, 1, 2}` is decided entirely in C++.  Binds the C++ `sets::ext`
  // (renamed from `materialise` in #919).  Passing a *Python* predicate into
  // the native core is deferred to the value-first reducer (#922); only the
  // value-level feasible form (native universe + native χ) is bound here.
  module.def(
      "ext",
      [](const std::vector<int>& universe) {
        return dedekind::sets::ext(universe, dedekind::sets::N);
      },
      "Extensionalise a finite universe through the native ℕ⊂ℤ classifier χ "
      "into a Python set: ext([-2, -1, 0, 1, 2]) == {0, 1, 2}.  The retraction "
      "μ: Int ⇀ Ext; χ runs in C++, not Python.");

  // ── 2D LP across the bridge on a Dual<Rational> carrier ────────────────
  module.def(
      "maximize_lp", &maximize_lp_dual_rational,
      "2D LP across the bridge on a Dual<Rational> carrier — the same "
      "carrier the compile-time NTTP showcase uses, instantiated in its "
      "Dual<F> AD lifting.  Inputs are tuples of DualRational; result is "
      "(x_star, y_star, feasible) with the coordinates ℚ-exact in both "
      "primal and tangent.  Same active-set kernel the compile-time "
      "exhibit folds; running here at runtime with Python values.");

  // ── linear_algebra / graphblas middleware ────────────────────────────────
  module.def("graphblas_backend_stub_available",
             &dedekind::python::graphblas_backend_stub_available,
             "Return whether the middleware advertises GraphBLAS backend "
             "capability for future validation/prototyping.");
}
