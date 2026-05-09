/**
 * @file dedekind/sets/computability.cppm
 * @partition :computability
 * @brief Computability as a compile-time observable.
 *
 * @section computability__Computability_Tiers
 *
 * Three concepts expose how much of a set's computational content is
 * available to the compiler. Two of them are strictly nested by their
 * definitions; the third is orthogonal.
 *
 *                     IsCompileTimeEnumerable
 *                                ↓  (definitionally, via `IsExtensional<S> &&
 * …`) IsExtensional
 *
 *                    HasDecidableMembership    (orthogonal — logic-species
 * axis)
 *
 * - `HasDecidableMembership<S>` — membership `x ∈ S` is answered in
 *   two-valued logic (i.e. no Unknown). Equivalent to `logic_species` being
 *   `ClassicalLogic`. This is independent of finiteness: a finite extensional
 *   object can be declared with `TernaryLogic` (e.g. `Ø<int, TernaryLogic>`,
 *   `Singleton<42, TernaryLogic>`), in which case it satisfies `IsExtensional`
 *   but NOT `HasDecidableMembership`.
 *
 * - `IsExtensional<S>` — the set has a finite cardinality and an observable
 *   `size()`. Does not by itself imply decidable membership.
 *
 * - `IsCompileTimeEnumerable<S>` — the set's elements are known to the
 *   *compiler*, not merely to the runtime. In practice: the values live in
 *   the type system (NTTPs), so the compiler can reason about individual
 *   elements without ever running the program. Definitionally implies
 *   `IsExtensional` (it is spelled as a refinement of it).
 *
 * @section computability__Reduction_Restores_Computability
 *
 * The `Set::operator&` / `operator|` machinery monotonically tightens this
 * classification as compile-time reductions succeed. An intensional Set over
 * a transfinite carrier starts at the bottom (Ternary, no observable size,
 * opaque predicate); a structural reduction to `Ø` or `Singleton<V>` lifts it
 * to the top (Classical, size 1-or-0, elements in types).
 *
 * A paper-facing `static_assert` at each tier witnesses the lift in the type
 * system itself — no runtime test needed, and no explanatory prose either.
 *
 * Wikipedia: Computability theory, Decidability (logic), Concepts (C++20)
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "We can only see a short distance ahead, but we can see plenty
 *       there that needs to be done."
 *       — Alan Turing, *Computing Machinery and Intelligence*,
 *         Mind LIX, 236 (October 1950), closing line.
 */
module;
#include <concepts>
#include <cstddef>
#include <type_traits>

export module dedekind.sets:computability;

import dedekind.category;
import :cardinality;  // For Finite

namespace dedekind::sets {
using namespace dedekind::category;

/**
 * @concept HasDecidableMembership
 * @brief S's membership map ranges into two-valued (classical) logic.
 *
 * The presence of `Unknown` in the codomain would forbid this — so Ternary-
 * classified Sets fail. This is the library's observable proxy for
 * "membership is decidable": the compiler knows there's no third answer.
 */
export template <typename S>
concept HasDecidableMembership =
    requires { typename std::remove_cvref_t<S>::logic_species; } &&
    std::same_as<typename std::remove_cvref_t<S>::logic_species,
                 ClassicalLogic>;
}  // namespace dedekind::sets
