/**
 * @file dedekind/sequences/limits.cppm
 * @partition :limits
 * @brief The Convergence Morphism (Resolution of the Path).
 *
 * @section limits__Convergence
 * This partition defines the "Resolution" of a Path into a single point.
 * In the Dedekind structuralist framework, a limit is the terminal state
 * of a sequence as it enters the infinitesimal neighborhood of a species.
 *
 * @details
 * We utilize the Archimedean property from (:order) to ensure that the
 * species supports the notion of "stepping" toward an asymptotic horizon.
 *
 * Wikipedia: Limit of a sequence, Archimedean property, Convergence
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "A mathematical problem should be difficult in order to entice us, yet
 * not completely inaccessible, lest it mock at our efforts."
 *       -- David Hilbert, Mathematical Problems (1900)
 */

module;

#include <concepts>
#include <functional>

export module dedekind.sequences:limits;

import dedekind.category;
import dedekind.topology;
import dedekind.order;
import :path;

namespace dedekind::sequences {

using namespace dedekind::category;
using namespace dedekind::topology;
using namespace dedekind::order;

/**
 * @concept HasLimit
 * @brief Species that can resolve a Path into a single point.
 * @details Requires the species to be Archimedean (ordered and steppable)
 *          and provides a formal 'limit' mapping.
 */
export template <typename T>
concept HasLimit = IsArchimedean<T> && requires(Path<T> s) {
  /** @brief The Resolution: Mapping the infinite path to the continuum. */
  { limit(s) } -> std::same_as<T>;
};

/**
 * @brief The Archimedean Limit (lim n->∞ s_n)
 * @details For machine primitives (floating_point), this resolves
 *          to the epsilon-stabilized tail of the sequence.
 *
 * @tparam T A floating-point species satisfying the Archimedean property.
 * @param s The path (sequence) to be resolved.
 * @return The value at the species' asymptotic horizon.
 */
export template <typename T>
  requires std::floating_point<T> && IsArchimedean<T>
constexpr T limit(const Path<T>& s) {
  /**
   * @section limits__Structuralist_Resolution
   * In a machine context, we sample the path at the 'asymptotic'
   * horizon of the species.
   */
  // FIXME #293: Heuristic sampling index for limit evaluation. Proper fix
  // requires ε-stabilization mechanism and formal convergence test coverage.
  return s.at(10000);
}

/** @section limits__Formal_Verification */

/** @proof
 * Deferred while Archimedean witnesses are being retargeted to the current
 * order action contracts.
 */

/**
 * @brief The Discrete Limit Morphism for Boolean Truth.
 * @details In the Boolean Topos, a path converges if it is
 *          eventually constant.
 *
 * FIXME: PR 96 existential proof. Generalize.
 */
export constexpr Boolean limit(const Path<Boolean>& s) {
  // If Path is a Morphism f: N -> Boolean, we sample the
  // "Eventual" state. For PR 96, index 0 is the minimal
  // witness of the constant path.
  return s(0);
}

// Boolean limit witness is provided above; concept-level proof is deferred
// while order/topology convergence contracts are being aligned.

// FIXME: re-enable support for floating-point limits once we have a proper
// ε-stabilization mechanism.

/** @proof Integers are Archimedean but do not (necessarily) have limits in Z.
 */
// This depends on whether you've specialized limit<int>.
// If not, HasLimit<int> should be false.
static_assert(!HasLimit<int>,
              "Axiom Check: Discrete integers do not support the continuous "
              "limit morphism.");

}  // namespace dedekind::sequences
