/**
 * @file dedekind/sequences/cauchy.cppm
 * @partition :cauchy
 * @brief Level 2.5c: The Logic of Closeness (Cauchy Convergence).
 *
 * @section The_Cauchy_Axiom
 * A Path is Cauchy if its elements become "arbitrarily close" as the index
 * increases. In the Dedekind structuralist framework, this is the internal
 * proof of convergence before a limit is formally resolved.
 *
 * @details
 * Following the Cantor-Dedekind completion, we define a Cauchy Path as the
 * prerequisite for the "Seamless" continuum. If every Cauchy sequence in a
 * species converges to a limit within that species, the species is
 * Dedekind-Complete.
 *
 * Wikipedia: Cauchy sequence, Completeness of the real numbers, Metric space
 */

module;

#include <cmath>
#include <concepts>
#include <functional>

export module dedekind.sequences:cauchy;

import dedekind.category;
import dedekind.order;
import :path;
import :limits;

namespace dedekind::sequences {

using namespace dedekind::category;
using namespace dedekind::order;

/**
 * @concept IsCauchy
 * @brief A path where the metric distance between elements vanishes at the
 * horizon.
 *
 * @details
 * A species is Cauchy if its elements (Codomain) can be measured against 
 * an Archimedean scale. The metric distance must be representable within 
 * the species' own logical universe.
 *
 * @tparam Seq A species fulfilling the IsSequence requirement.
 * @axiom For every ε > 0, there exists N such that for all n, m > N, |s_n - s_m| < ε.
 */
export template <IsSequence Seq>
concept IsCauchy = IsArchimedeanField<typename Seq::Codomain> &&
                   requires(Seq s, std::size_t n, std::size_t m) {
                     /** 
                      * @brief The Metric Morphism.
                      * The distance between points must resolve to the 
                      * Codomain's internal representation of magnitude.
                      */
                     {
                       std::abs(s.at(n) - s.at(m))
                     } -> std::convertible_to<typename Seq::Codomain>;
                   };
/**
 * @concept IsConvergent
 * @brief A Cauchy path that possesses a limit within its own species.
 * 
 * @details 
 * For a sequence to be convergent, it must first satisfy the Cauchy 
 * property (internal coherence) and its Codomain (the species of its 
 * values, e.g., ℝ or ℚ) must admit a limit point.
 * 
 * @tparam Seq A species fulfilling the IsCauchy requirement.
 */
export template <IsCauchy Seq>
concept IsConvergent = HasLimit<typename Seq::Codomain>;

/**
 * @struct CauchyPath
 * @brief A Path formally reified as a Cauchy-compliant sequence.
 */
export template <typename T>
  requires IsArchimedean<T>
struct CauchyPath : public Path<T> {
  using Path<T>::Path;  // Inherit the Frobenius generator

  /** @section Mereological_Tagging */
  using is_cauchy_tag = void;
};

/** @section Formal_Verification */

/** @proof Archimedean paths over double satisfy the Cauchy logic. */
static_assert(IsCauchy<Path<double>>,
              "Axiom Failure: Paths over ℝ must support Cauchy metric logic.");

/** @proof Archimedean paths over double are Convergent (possess a limit). */
static_assert(IsConvergent<Path<double>>,
              "Topology Failure: Cauchy paths over ℝ must resolve to a limit.");

/** @proof Negative Proof: Integers are Archimedean but NOT convergent. */
static_assert(
    !IsConvergent<Path<int>>,
    "Structural Safety: Discrete species (ℤ) cannot claim convergence.");

}  // namespace dedekind::sequences
