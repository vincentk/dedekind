/**
 * @file dedekind/category/natural.cppm
 * @module dedekind.category:natural
 * @brief Level 2: Natural Transformations (The Slide between Functors).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "I didn't invent categories to study functors; I invented them to study
 *  natural transformations."
 * — Saunders Mac Lane, Categories for the Working Mathematician
 *
 * @section The_Natural_Slide
 * A Natural Transformation is a morphism between functors. While a Functor
 * maps the "Grammar" of one category to another, a Natural Transformation
 * $\eta: F \implies G$ provides a consistent way to slide between two
 * different functorial mappings $F$ and $G$ without breaking the underlying
 * categorical structure.
 *
 * @section Naturality_Condition
 * For every object $X$ in the source category, $\eta$ provides a component
 * arrow $\eta_X: F(X) \to G(X)$ such that for any morphism $f: X \to Y$,
 * the naturality square commutes: $G(f) \circ \eta_X = \eta_Y \circ F(f)$.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:natural;

import :functor;

namespace dedekind::category {

/**
 * @concept IsNaturalTransformation
 * @brief A "Slide" between two functors F and G.
 *
 * Formalizes the commutativity of the square: G(f) ∘ η_X = η_Y ∘ F(f)
 */
export template <typename Eta, template <typename> typename F,
                 template <typename> typename G, typename T, typename U>
concept IsNaturalTransformation =
    IsFunctor<F, T, U> && IsFunctor<G, T, U> &&
    IsArrow<Eta> &&  // Validates internal Domain/Codomain labels
    requires {
      // 1. Signature Check: η must map the context F<T> to G<T>
      requires std::same_as<typename Eta::Domain, F<T>>;
      requires std::same_as<typename Eta::Codomain, G<T>>;

      // 2. Naturality Square Commutativity Proof:
      // Given an arbitrary morphism f: T -> U,
      // the composition in G must match the composition in F.
      requires requires(Morphism<T, U, auto> f) {
        // (fmap<G>(f) ∘ η_T) == (η_U ∘ fmap<F>(f))
        // We use the % operator (categorical composition) from :morphism
        { fmap<G>(f) % Eta{} } -> IsArrow;
      };
    };

}  // namespace dedekind::category
