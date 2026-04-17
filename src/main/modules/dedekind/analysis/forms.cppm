/**
 * @file dedekind/analysis/forms.cppm
 * @partition :forms
 * @brief Level 11.5: The Exterior Algebra (Differential Forms).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "To new concepts correspond, necessarily, new signs."
 *       -- David Hilbert, Mathematical Problems (1900)
 */

module;

#include <concepts>
#include <cstddef>

export module dedekind.analysis:forms;

import dedekind.geometry;

namespace dedekind::analysis {

using namespace dedekind::geometry;

/**
 * @concept IsDifferentialForm
 * @brief A mapping from a Tangent Space to a Scalar.
 */
export template <typename W, typename V, typename F>
concept IsDifferentialForm = requires(W w, V v) {
  { w(v) } -> std::same_as<F>;  // Evaluation (Interior Product)
};

/**
 * @class OneForm
 * @brief The cotangent vector (The 'Gradient' components).
 */
export template <std::floating_point F, std::size_t N>
struct OneForm {
  Vector<F, N> components;

  // Evaluation: ω(v) = dot(components, v)
  constexpr F operator()(const Vector<F, N>& v) const {
    return dot(components, v);
  }
};

/**
 * @brief Convert a OneForm to its Covector (row-vector) representation.
 *
 * A OneForm<F,N> and a Covector<F,N> (= LinearMap<F,1,N>) are two
 * representations of the same linear functional F^N -> F.  This function
 * constructs the Covector whose single row holds the OneForm's components,
 * making the bijection explicit.
 */
export template <std::floating_point F, std::size_t N>
constexpr Covector<F, N> to_covector(const OneForm<F, N>& w) {
  Covector<F, N> result;
  for (std::size_t j = 0; j < N; ++j)
    result.set_coefficient(0, j, w.components[j]);
  return result;
}

}  // namespace dedekind::analysis
