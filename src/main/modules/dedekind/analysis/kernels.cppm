/**
 * @file ontology:analysis:kernels.cppm
 * @partition :kernels
 * @brief Level 3 Analysis: The Reproducing Kernel as a Morphism.
 *
 * @section Kernels: The Geometry of Similarity
 * This partition defines the Reproducing Kernel \( K: \Omega \times \Omega \to
 * \mathbb{F} \). In the Dedekind Category, a Kernel is both a Symmetric
 * Bilinear Form and a generator of Paths (Sequences).
 *
 * @details
 * Following the RKHS (Reproducing Kernel Hilbert Space) formalism:
 * - IsKernel: A symmetric, positive-definite mapping.
 * - IsSequence: By fixing one coordinate \( K(x, \cdot) \), the kernel becomes
 *   a Path over the Domain \(\Omega\).
 * - IsMorphism: The kernel acts as a morphism between the Index Set and the
 * Scalar Field.
 *
 * @build_order 7
 * @dependency :sequences, :category, :mereology
 *
 * Wikipedia: Reproducing kernel Hilbert space, Radial basis function, Morphism
 */
module;

#include <concepts>
#include <functional>

export module dedekind.analysis:kernels;

import dedekind.sets;      // IsSet, IsCardinality, RealLine
import dedekind.sequences; // IsSequence, IsFiniteSequence
import dedekind.category;  // IsMorphism, IsSymmetric, IsEndomorphism

namespace dedekind::analysis {
using namespace dedekind::sets;
using namespace dedekind::category;

/**
 * @brief The Gaussian (RBF) Kernel.
 * In the Category of Hilbert Spaces, this is the Representer of Evaluation.
 */
template <typename T = double>
struct GaussianKernel {
  using value_type = T;
  using Domain = RealLine;
  using Codomain = T;

  T sigma = 1.0;

  // 1. The Binary Form (IsKernel)
  T operator()(T a, T b) const noexcept {
    const T diff = a - b;
    return std::exp(-(diff * diff) / (static_cast<T>(2) * sigma * sigma));
  }

  // 2. The Morphism Projection (IsSequence)
  // Maps a point to its representer in the feature space.
  T operator[](T x) const noexcept { return (*this)(static_cast<T>(0), x); }

  // 3. Cardinality for the Path logic
  auto cardinality() const noexcept { return Domain::cardinality(); }
};

/**
 * @section Static Formal Proofs
 * These assertions verify that our Physics/Geometry objects satisfy
 * the high-level Categorical and Ontological requirements.
 */

// A. Sequence Requirement: It is a functional mapping from a Set (R).
static_assert(
    sequences::IsSequence<GaussianKernel<double>>,
    "GaussianKernel must satisfy the Level 2.5 Sequence (Path) concept.");

// B. Morphism Requirement: It maps a Domain to a Codomain.
static_assert(
    IsMorphism<GaussianKernel<double>, RealLine, double>,
    "GaussianKernel must be a valid Morphism in the Dedekind Category.");

// C. Categorical Symmetry: The kernel is a symmetric endomorphism on its
// domain. (Assuming IsSymmetric checks binary operator(a, b) == operator(b, a))
static_assert(IsSymmetric<GaussianKernel<double>>,
              "A Reproducing Kernel must satisfy Categorical Symmetry.");

// D. Infinite Support: It is NOT a Finite Sequence (it's a path over the
// continuum).
static_assert(!sequences::IsFiniteSequence<GaussianKernel<double>>,
              "GaussianKernel over RealLine must be an infinite Path.");

/**
 * @brief The Reproducing Kernel for a specific point 'y'.
 * Satisfies IsSequence: Domain Ω -> Codomain F.
 */
template <typename DomainSet, typename T = double>
struct ReproducingKernel {
  using value_type = T;
  using Domain = DomainSet;

  DomainSet domain_v;
  value_type y_point;

  // The 'Morphism' aspect: evaluation at x
  T operator[](const typename DomainSet::Domain& x) const {
    return inner_product_logic(x, y_point);
  }

  auto cardinality() const { return domain_v.cardinality(); }
};

// This now satisfies your new IsSequence concept!
static_assert(sequences::IsSequence<ReproducingKernel<sets::Interval<int>>>);

/**
 * @concept IsKernel
 * @brief Symmetry and Positive Definiteness (Level 3 Analysis).
 */
export template <typename K, typename Domain, typename T>
concept IsKernel = requires(K k, Domain a, Domain b) {
  { k(a, b) } -> std::convertible_to<T>;
  // Symmetry is a semantic requirement, but we can check the signature
};
