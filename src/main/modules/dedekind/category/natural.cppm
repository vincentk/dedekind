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
 *
 * In this library's single-species categories, object labels are recovered via
 * the identity spoke `id_c(x)`. As a result, component arrows such as `η_x`
 * are checked by lifting identity arrows and reading off the resulting domain
 * object, rather than by ranging over a separate textbook object class.
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
    std::same_as<typename F::Σ_cat, typename G::Σ_cat> &&
    std::same_as<typename F::Τ_cat, typename G::Τ_cat> &&
    requires(Alpha alpha, typename F::Σ_cat::Arrow::Domain c) {
      // Component at c connects F(c) to G(c) in the Target Hub
      // In the single-species setting, we recover F(c) and G(c) by lifting
      // the identity spoke on c and reading the resulting spoke's domain.
      requires std::same_as<typename decltype(alpha(c))::Domain,
                            typename decltype(F{}.fmap(
                                F::Σ_cat::id_c(c)))::Domain>;
      requires std::same_as<typename decltype(alpha(c))::Codomain,
                            typename decltype(G{}.fmap(
                                G::Σ_cat::id_c(c)))::Domain>;
    };

/**
 * @concept IsNaturalTransformation
 * @theorem Naturality (F ⟹ G)
 */
export template <typename Alpha, typename F, typename G>
concept IsNaturalTransformation =
    IsTwoMorphism<Alpha, F, G> && requires(Alpha alpha, F f_map, G g_map,
                                           typename F::Σ_cat::Arrow::Domain c) {
      // The Slide is witnessed through the identity spoke at c.
      // This keeps the condition structural for categories whose object
      // labels are recovered via their identity arrows.
      {
        (f_map >> F::Σ_cat::id_c(c)) >> alpha(c)
      } -> std::same_as<decltype(alpha(c) >> (g_map >> F::Σ_cat::id_c(c)))>;
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

  auto operator()(const typename F::Σ_cat::Arrow::Domain& c) const {
    // In the common single-species case, the source object label can be reused
    // directly as the target witness. Otherwise, return the lifted identity
    // arrow itself: by the functor identity law, fmap(id_c(c)) = id_{F(c)}.
    if constexpr (std::convertible_to<typename F::Σ_cat::Arrow::Domain,
                                      typename F::Τ_cat::Arrow::Domain>) {
      return F::Τ_cat::id_c(static_cast<typename F::Τ_cat::Arrow::Domain>(c));
    } else {
      return f_map.fmap(F::Σ_cat::id_c(c));
    }
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

  auto operator()(const typename F::Σ_cat::Arrow::Domain& c) const {
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

  auto operator()(const typename F::Σ_cat::Arrow::Domain& c) const {
    // (beta * alpha)_c = G'(alpha_c) >> beta_{F(c)}
    // or equivalent: beta_{F'(c)} >> G(alpha_c)
    if constexpr (std::convertible_to<typename F::Σ_cat::Arrow::Domain,
                                      typename G::Σ_cat::Arrow::Domain>) {
      return G_prime{}.fmap(alpha(c)) >>
             beta(static_cast<typename G::Σ_cat::Arrow::Domain>(c));
    } else {
      return G_prime{}.fmap(alpha(c)) >>
             beta(F_prime{}.fmap(F::Σ_cat::id_c(c)).vertex);
    }
  }
};

}  // namespace dedekind::category
