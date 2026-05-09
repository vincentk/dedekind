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
 *                                ↓  (definitionally, via `IsFiniteSet<S> && …`)
 *                         IsFiniteSet
 *
 *                    HasDecidableMembership    (orthogonal — logic-species
 * axis)
 *
 * - `HasDecidableMembership<S>` — membership `x ∈ S` is answered in
 *   two-valued logic (i.e. no Unknown). Equivalent to `logic_species` being
 *   `ClassicalLogic`. This is independent of finiteness: a finite extensional
 *   object can be declared with `TernaryLogic` (e.g. `Ø<int, TernaryLogic>`,
 *   `Singleton<42, TernaryLogic>`), in which case it satisfies `IsFiniteSet`
 *   but NOT `HasDecidableMembership`.
 *
 * - `IsFiniteSet<S>` — the set has a finite cardinality and an observable
 *   `size()`. Does not by itself imply decidable membership.
 *
 * - `IsCompileTimeEnumerable<S>` — the set's elements are known to the
 *   *compiler*, not merely to the runtime. In practice: the values live in
 *   the type system (NTTPs), so the compiler can reason about individual
 *   elements without ever running the program. Definitionally implies
 *   `IsFiniteSet` (it is spelled as a refinement of it).
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

/**
 * @concept IsFiniteSet
 * @brief S has finite cardinality and exposes an observable `size()`.
 *
 * The cardinality is carried in the type (`cardinality_type = Finite`), so
 * this is a purely compile-time check — not a runtime claim that `size()`
 * happens to be small.
 */
export template <typename S>
concept IsFiniteSet =
    requires { typename std::remove_cvref_t<S>::cardinality_type; } &&
    std::same_as<typename std::remove_cvref_t<S>::cardinality_type, Finite> &&
    requires(const std::remove_cvref_t<S>& s) {
      { s.size() } -> std::convertible_to<std::size_t>;
    };

/**
 * @concept IsCompileTimeEnumerable
 * @brief S's elements are knowable to the compiler.
 *
 * Stronger than `IsFiniteSet`: routes through `IsExtensional` (in
 * @c :sets:cardinality) — the canonical authority on extensionality
 * vocabulary post-2026-05-09.  The previous tag-based marker
 * (@c is_compile_time_extensional_tag) was retired in favour of the
 * concept gate; new code consumes @c :sets:cardinality, doesn't
 * duplicate it.
 */
export template <typename S>
concept IsCompileTimeEnumerable = IsFiniteSet<S> && IsExtensional<S>;

}  // namespace dedekind::sets
