/**
 * @file dedekind/sets/computability.cppm
 * @partition :computability
 * @brief Decidable membership as a compile-time observable.
 *
 * @section computability__Decidability_Axis
 *
 * Post-2026-05-09 consolidation, this partition exports a single concept
 * along the @b decidability axis:
 *
 *   - `HasDecidableMembership<S>` — membership `x ∈ S` is answered in
 *     two-valued logic (i.e. no Unknown).  Equivalent to `logic_species`
 *     being `ClassicalLogic`.  Independent of finiteness: a finite
 *     extensional object can be declared with `TernaryLogic`
 *     (e.g. `Ø<int, TernaryLogic>`, `Singleton<42, TernaryLogic>`), in
 *     which case it satisfies @c :sets:cardinality::IsExtensional but
 *     NOT `HasDecidableMembership`.
 *
 * The orthogonal @b extensionality axis lives upstream in
 * @c :sets:cardinality (the canonical home for extensionality /
 * cardinality / enumerability vocabulary post-2026-05-09).  Use
 * @c IsExtensional from there for the size-observable check;
 * @c IsFinite / @c IsCountable / @c IsUncountable / @c IsCardinality
 * are the cardinality-tier concepts.  The partition header was
 * trimmed in the same pass: the previous tag-based
 * @c IsCompileTimeEnumerable concept was retired as redundant — the
 * @c IsExtensional gate captures the same observable.
 *
 * @section computability__Reduction_Restores_Decidability
 *
 * The @c Set::operator& / @c operator| machinery monotonically tightens
 * the decidability classification as compile-time reductions succeed.
 * An intensional Set over a transfinite carrier starts at the bottom
 * (TernaryLogic, opaque predicate); a structural reduction to @c Ø or
 * @c Singleton<V> lifts it to the top (ClassicalLogic, decidable
 * membership).
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
 * @section mereology__Structural_Inference
 * @brief Deduce the governing logic species from the nature of the Base.
 *
 * Theorem (carrier-axis decidability resolver, post-#622):
 * If a Species's @c cardinality_type is @b Countable (@c Finite or @c ℵ_0),
 * its membership predicates are recognised as Classical (Δ⁰₁ on the carrier
 * axis).  If the cardinality is @b Uncountable (@c ℶ_1, …), the species is
 * a Kleene Topos (Ternary) — two independent ceilings stack to justify the
 * Ternary verdict:
 *   (a) Rice's theorem forbids recognising non-trivial semantic properties
 *       of arbitrary code, so opaque predicates over uncountable carriers
 *       cannot be promoted by inspection;
 *   (b) the float↔ℝ gap means a @c double-typed witness does not faithfully
 *       denote a real number, so even a structurally-Δ⁰₁ comparison
 *       (e.g.\ @c x @c > @c pivot) on ℝ-as-@c double is exact-as-@c double
 *       but unknown-as-ℝ.
 * Both ceilings agree on the verdict (Ternary) for independent reasons.
 *
 * Decidability is the load-bearing property; cardinality is its cheap
 * structural witness on the carrier axis.  Set-level and predicate-level
 * promotion axes (#692, #693) are tracked separately and compose by OR.
 */
// Primary template: when @c Base exposes no @c cardinality_type the
// resolver cannot make a structural claim on the carrier axis — fall
// back to the honest default of @c TernaryLogic.  This branch keeps the
// resolver SFINAE-friendly for callers that probe @c NaturalLogic in a
// @c requires-clause (e.g.\ the cartesian-product @c operator* overload
// gate in @c :sets:expressions): non-Set carriers like @c
// dedekind::ieee::IEEE<double> degrade gracefully rather than producing
// a hard error.
export template <typename Base, typename = void>
struct NaturalLogic {
  using species = TernaryLogic;
  using type = species;
};

// Specialisation: when @c Base exposes @c cardinality_type, read the
// carrier-axis verdict per #622 — Countable (@c Finite, @c ℵ_0) →
// @c ClassicalLogic, Uncountable (@c ℶ_1, …) → @c TernaryLogic.
export template <typename Base>
struct NaturalLogic<Base, std::void_t<typename Base::cardinality_type>> {
  using species =
      std::conditional_t<IsCountable<typename Base::cardinality_type>,
                         ClassicalLogic, TernaryLogic>;
  using type = species;
};

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
