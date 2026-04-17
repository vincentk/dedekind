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
 * @note "La science est faite de données comme une maison de pierres."
 *       -- Henri Poincare, paraphrase
 *       [Trans: "Science is built from facts as a house is built from stones."]
 */

module;

#include <nanobind/nanobind.h>
#include <nanobind/stl/vector.h>

#include <algorithm>
#include <set>
#include <unordered_set>
#include <vector>

import dedekind.python;

namespace nb = nanobind;

namespace {

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

}  // namespace

NB_MODULE(_dedekind, module) {
  module.doc() = "Dedekind Python MVP facade";

  module.def(
      "ordered_set_roundtrip", &ordered_set_roundtrip,
      "Round-trip a Python sequence through the ordered finite-set facade.");
  module.def(
      "unordered_set_roundtrip", &unordered_set_roundtrip,
      "Round-trip a Python sequence through the unordered finite-set facade.");
  module.def("path_from_range", &path_from_range,
             "Materialize a finite path from a Python sequence and expose it "
             "back as a list.");
}