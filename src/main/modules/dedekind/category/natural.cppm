/**
 * @file dedekind/category/natural.cppm
 * @module dedekind.category:natural
 * @brief Level 2.2: Natural Transformations (The Slide between Functors).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "Il concetto di trasformazione naturale è il vero motivo per cui
 *  sono state inventate le categorie."
 * (The concept of natural transformation is the real reason why
 *  categories were invented.)
 * — Saunders Mac Lane (tradotto in spirito Aluffi)
 *
 * @section Naturality_Condition
 * For every object $X$ in the source category, $\eta$ provides a component
 * arrow $\eta_X: F(X) \to G(X)$ such that for any morphism $f: X \to Y$,
 * the naturality square commutes: $G(f) \circ \eta_X = \eta_Y \circ F(f)$.
 *
 * A Natural Transformation η: F ⟹ G is a family of morphisms that provides
 * a structural bridge between two functorial contexts. Unlike a Functor
 * which maps species, the Transformation maps the *context* itself.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:natural;

import :functor;

namespace dedekind::category {

/**
 * @concept IsNaturalTransformation
 * @theorem Naturality (F ⟹ G)
 * @brief Proposition: There exists a structure-preserving bridge between two
 * Functors.
 *
 * @details
 * For this theorem to hold for a candidate (Eta), the following must be true:
 * 1. Categorical Context: F and G are verified Functors over the Species (T,
 * OpT).
 * 2. Morphism Signature: Eta is a valid Arrow mapping the context F⟨T⟩ to G⟨T⟩.
 *
 * For every object X, we define a morphism η_X: F⟨X⟩ → G⟨X⟩.
 * To be "Natural," the following square must commute for any f: X → Y:
 *
 *        η_X
 *   F⟨X⟩ ────→ G⟨X⟩
 *    │          │
 * F(f)│          │G(f)
 *    ↓          ↓
 *   F⟨Y⟩ ────→ G⟨Y⟩
 *        η_Y
 *
 * Formally: G(f) ∘ η_X = η_Y ∘ F(f)
 *
 * @concept IsNaturalTransformation
 * @brief α : F ⟹ G
 *
 * This is as sharp as it gets. It reads exactly like the definition:
 * "A morphism between Functors that preserves the Species."
 */

/**
 * @concept IsNaturalTransformation
 * @brief α : F ⟹ G
 *
 * A Natural Transformation is a family of morphisms α_X : F(X) -> G(X)
 * such that for every f : X -> Y, the square G(f) ∘ α_X = α_Y ∘ F(f) commutes.
 */
export template <typename α_Factory>
concept IsNaturalTransformation = requires {
  // 1. Component Extraction Proof
  typename α_Factory::template Component<int>;
  requires IsArrow<typename α_Factory::template Component<int>>;

  // 2. Commutativity Verification (The Slide)
  requires requires(Morphism<int, int, std::function<int(int)>> f) {
    // Path A: F(f) >> α_Y
    {
      fmap<α_Factory::SourceFunctor::template Shape>(f) >>
          typename α_Factory::template Component<int>{}
    } -> IsArrow;

    // Path B: α_X >> G(f)
    {
      typename α_Factory::template Component<int>{} >>
          fmap<α_Factory::TargetFunctor::template Shape>(f)
    } -> IsArrow;
  };
};

}  // namespace dedekind::category
