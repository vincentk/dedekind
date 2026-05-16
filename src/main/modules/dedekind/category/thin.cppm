/**
 * @file dedekind/category/thin.cppm
 * @partition :thin
 * @brief Thin categories — categories whose hom-sets contain at most one
 *        morphism.
 *
 * @section thin__Categorical_Definition
 * A @b thin (or @em posetal-shaped, in the textbook's looser usage) category
 * is one in which @c hom(a, @c b) has @b at @b most one morphism for any
 * pair of objects.  Equivalently: any two parallel morphisms are equal.
 *
 * In the order-theoretic encoding the codebase uses across @c :mereology
 * and @c :posetal, this corresponds to a @b preorder over @c T with relation
 * @c Rel and truth-value codomain @c L::Ω:
 *
 * - Objects are elements of @c T.
 * - The unique morphism @c a → b (when it exists) is the witness of
 *   @c Rel(a, @c b) @c = @c L::True.
 * - Identity morphism @c id_a corresponds to @b reflexivity
 *   (@c Rel(a, @c a) @c = @c L::True).
 * - Composition corresponds to @b transitivity
 *   (@c Rel(a, @c b) @c ∧ @c Rel(b, @c c) @c ⇒ @c Rel(a, @c c)).
 *
 * @section thin__Distinction_from_Posetal
 * @c IsThinCategory does @b not require @b antisymmetry.  A thin category
 * is a @b preorder, not necessarily a partial order; isomorphic objects
 * need not be identical.  Adding antisymmetry yields @c :posetal::IsPosetal
 * — a thin category that is also @b skeletal.
 *
 * @c IsPosetal is a strict refinement of @c IsThinCategory; the inclusion
 * is encoded definitionally per the project's @em "faithful specialization
 * in the type signature from day 1" posture (#698).
 *
 * @section thin__Sollbruchstelle_From_Small
 * @c :small's docstring deliberately defers thin-category vocabulary:
 * @em "thin, discrete, locally-finite, intensional / extensional, lazy /
 * strict --- are also intentionally out of scope here; they are named
 * (or named-able) at downstream partitions where they have semantic
 * content."  This partition closes that seam directly.
 *
 * @section thin__Boolean_Witness
 * @c bool with @c std::less_equal participates: the 2-element poset
 * @c (false @c ≤ @c true) is the smallest non-trivial thin category and
 * a canonical entry point to the Form-chain (#698).
 *
 * @see https://en.wikipedia.org/wiki/Preorder
 * @see https://ncatlab.org/nlab/show/thin+category
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Two parallel arrows in a thin category are equal --- the relation
 *        IS the morphism."
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:thin;

import :logic;
import :mereology;  // is_reflexive_v, is_transitive_v traits

namespace dedekind::category {

/**
 * @concept IsThinCategory
 * @brief A category in which every hom-set has at most one morphism.
 *
 * @details
 * Equivalently, a @b preorder over @c T with relation @c Rel returning
 * @c L::Ω.  The axioms enforced:
 *
 * - @b Closure: @c rel(a, @c b) yields a truth value in @c L::Ω.
 * - @b Reflexivity: @c ∀a. @c rel(a, @c a) @c = @c L::True
 *   (the identity morphism).
 * - @b Transitivity: @c ∀a,b,c. @c rel(a, @c b) @c ∧ @c rel(b, @c c) @c ⇒
 *   @c rel(a, @c c) (composition).
 *
 * Antisymmetry is @b not required; @c IsPosetal in @c :posetal is the
 * strict refinement that adds it.
 *
 * @tparam T   The Domain (Objects).
 * @tparam Rel The Relation (the unique-morphism witness).
 * @tparam L   The Logic Species (the Subobject Classifier Ω).
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename L = ClassicalLogic>
concept IsThinCategory = requires(Rel rel, T a, T b) {
  /** @brief The relation must yield a result in the logical classifier. */
  { rel(a, b) } -> std::same_as<typename L::Ω>;

  /** @brief Reflexivity (identity arrow). */
  requires is_reflexive_v<T, Rel>;

  /** @brief Transitivity (composition). */
  requires is_transitive_v<T, Rel>;
};

/** @section thin__Canonical_Witnesses
 *
 *  @brief Boolean is the canonical 2-element thin category.
 *
 *  @details @c (false @c ≤ @c true) under @c std::less_equal is the
 *  smallest non-trivial thin category — and the smallest non-trivial
 *  lattice / subobject classifier / Boolean algebra.  Pinning it here
 *  anchors the Form-chain at its base.
 */
static_assert(IsThinCategory<bool>,
              "bool with std::less_equal is the canonical 2-element thin "
              "category (preorder over {false, true}).");

static_assert(IsThinCategory<int>,
              "int with std::less_equal is thin (the canonical total order).");

}  // namespace dedekind::category
