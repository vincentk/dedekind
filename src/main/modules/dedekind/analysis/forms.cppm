/**
 * @file dedekind/analysis/forms.cppm
 * @partition :forms
 * @brief Level 11.5: The Exterior Algebra (Differential Forms).
 */

module;

#include <concepts>
#include <functional>

export module dedekind.analysis:forms;

import dedekind.geometry;
import dedekind.numbers;

namespace dedekind::analysis {

using namespace dedekind::geometry;
using namespace dedekind::numbers;

/**
 * @concept IsDifferentialForm
 * @brief A mapping from a Tangent Space to a Scalar.
 */
export template <typename W, typename V, typename F>
concept IsDifferentialForm = IsVectorSpace<V, F> && requires(W w, V v) {
  { w(v) } -> std::same_as<F>;  // Evaluation (Interior Product)
};

/**
 * @class OneForm
 * @brief The cotangent vector (The 'Gradient' components).
 */
export template <IsField F, std::size_t N>
struct OneForm {
  Vector<F, N> components;

  // Evaluation: ω(v) = dot(components, v)
  constexpr F operator()(const Vector<F, N>& v) const {
    return dot(components, v);
  }
};

}  // namespace dedekind::analysis
