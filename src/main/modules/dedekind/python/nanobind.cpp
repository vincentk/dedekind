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
#include <nanobind/stl/set.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/vector.h>

#include <cstddef>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

import dedekind.python;

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
  std::ranges::sort(materialized);
  return materialized;
}

auto path_from_range(const std::vector<int>& values) -> std::vector<int> {
  const auto path = dedekind::python::from_range(values);
  const auto& view = dedekind::python::as_range(path);
  return {view.begin(), view.end()};
}

// ── extensional sets: Python set ↔ dedekind.sets ─────────────────────────
// These functions accept and return std::set<T> (Python set) and route
// through FiniteExtensionalSet<T> via the dedekind.python facade.
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

NB_MODULE(_dedekind, module) {
  module.doc() = "Dedekind Python MVP facade";

  // ── sequences (Python list ↔ dedekind.sequences) ─────────────────────
  module.def("ordered_set_roundtrip", &ordered_set_roundtrip,
             "Round-trip a Python list through the ordered finite-set facade "
             "(dedekind.sequences).");
  module.def("unordered_set_roundtrip", &unordered_set_roundtrip,
             "Round-trip a Python list through the unordered finite-set facade "
             "(dedekind.sequences).");
  module.def("path_from_range", &path_from_range,
             "Materialize a finite path from a Python list "
             "(dedekind.sequences).");

  // ── extensional set algebra (Python set ↔ dedekind.sets) ─────────────
  // Overloads are tried in registration order: bool, int, double, str.
  register_set_ops<bool>(module);
  register_set_ops<int>(module);
  register_set_ops<double>(module);
  register_set_ops<std::string>(module);
}
