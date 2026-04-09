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
import :small;
import :morphism;

namespace dedekind::category {

/**
 * @concept IsPreTransformation
 * @brief Level 2.1: The raw family of components.
 */
export template <typename Alpha, typename CatS, typename CatT>
concept IsPreTransformation =
    IsCategory<CatS> && IsCategory<CatT> &&
    requires(Alpha alpha, typename CatS::Arrow::Domain c) {
      // The machine: takes an object in S, returns an arrow in T
      { alpha(c) } -> std::same_as<typename CatT::Arrow>;
    };

/**
 * @concept IsTwoMorphism
 * @brief Level 2.15: The bridge between two specific Functors.
 */
export template <typename Alpha, typename F, typename G>
concept IsTwoMorphism =
    IsFunctor<F> && IsFunctor<G> &&
    // Symmetry check: Both functors must connect the same Categories
    std::same_as<typename F::SourceCategory, typename G::SourceCategory> &&
    std::same_as<typename F::TargetCategory, typename G::TargetCategory> &&
    requires(Alpha alpha, typename F::SourceCategory::Arrow::Domain c) {
      // Component at c connects F(c) to G(c) in the Target Hub
      // Note: We use identity lifting to find the objects F(c) and G(c)
      requires std::same_as<typename decltype(alpha(c))::Domain,
                            typename decltype(F{}.fmap(
                                F::SourceCategory::id_c(c)))::Domain>;
      requires std::same_as<typename decltype(alpha(c))::Codomain,
                            typename decltype(G{}.fmap(
                                G::SourceCategory::id_c(c)))::Domain>;
    };

/**
 * @concept IsNaturalTransformation
 * @theorem Naturality (F ⟹ G)
 */
export template <typename Alpha, typename F, typename G>
concept IsNaturalTransformation =
    IsTwoMorphism<Alpha, F, G> &&
    requires(Alpha alpha, F f_map, G g_map,
             typename F::SourceCategory::Arrow f_spoke) {
      // The Slide: F(f) >> alpha(Y) == alpha(X) >> G(f)
      // Path A: Lift f through F and then move across alpha at Codomain(f)
      {
        (f_map >> f_spoke) >> alpha(f_spoke.Codomain)
      } -> std::same_as<decltype(alpha(f_spoke.Domain) >> (g_map >> f_spoke))>;
    };

/**
 * @brief The Identity Natural Transformation for Functor F.
 * @details alpha(c) = id_{F(c)}.
 */
export template <IsFunctor F>
struct identity_transformation {
  using SourceFunctor = F;
  using TargetFunctor = F;

  F f_map;

  auto operator()(const typename F::SourceCategory::Arrow::Domain& c) const {
    // We find the object F(c) by lifting the identity of c
    auto id_Fc = f_map.fmap(F::SourceCategory::id_c(c));
    // The Domain of F(id_c) is the object F(c)
    return F::TargetCategory::id_c(id_Fc.vertex);
  }
};

/**
 * @brief Vertical composition of two 2-morphisms (beta . alpha).
 * @details alpha: F => G, beta: G => H. Result: F => H.
 */
export template <typename Alpha, typename Beta, IsFunctor F, IsFunctor G,
                 IsFunctor H>
  requires IsNaturalTransformation<Alpha, F, G> &&
           IsNaturalTransformation<Beta, G, H>
struct vertical_composition {
  Alpha alpha;
  Beta beta;

  auto operator()(const typename F::SourceCategory::Arrow::Domain& c) const {
    return alpha(c) >> beta(c);
  }
};

/**
 * @brief Horizontal composition (Godement product).
 * @details F: C->D, G: D->E and alpha: F=>F', beta: G=>G'.
 */
export template <typename Alpha, typename Beta, IsFunctor F, IsFunctor F_prime,
                 IsFunctor G, IsFunctor G_prime>
  requires IsNaturalTransformation<Alpha, F, F_prime> &&
           IsNaturalTransformation<Beta, G, G_prime>
struct horizontal_composition {
  Alpha alpha;
  Beta beta;
  G g_functor;  // Required for mapping the components of alpha through G

  auto operator()(const typename F::SourceCategory::Arrow::Domain& c) const {
    // (beta * alpha)_c = G'(alpha_c) >> beta_{F(c)}
    // or equivalent: beta_{F'(c)} >> G(alpha_c)
    return G_prime{}.fmap(alpha(c)) >>
           beta(F_prime{}.fmap(F::SourceCategory::id_c(c)).vertex);
  }
};

}  // namespace dedekind::category
