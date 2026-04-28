/**
 * @file dedekind/category/natural.cppm
 * @partition :natural
 * @module dedekind.category:natural
 * @brief Level 2.2: Natural Transformations (The Slide between Functors).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Il concetto di trasformazione naturale è il vero motivo per cui
 *        sono state inventate le categorie."
 *       [Trans: "The concept of natural transformation is the real reason
 *        why categories were invented."]
 *       — Saunders Mac Lane (paraphrased in the spirit of Paolo Aluffi,
 *         *Algebra: Chapter 0*, AMS 2009).
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
 * @details Defines Natural Transformations as 2-cells, acting as morphisms
 * between Functors. For any two functors F, G: C -> D, a natural transformation
 * provides a family of morphisms in the target category D that satisfies
 * the naturality square, enabling structural translation between different
 * species-mappings.
 *
 * In this library's single-species categories, object labels are recovered via
 * the identity spoke `id_c(x)`. As a result, component arrows such as `η_x`
 * are checked by lifting identity arrows and reading off the resulting domain
 * object, rather than by ranging over a separate textbook object class.
 *
 * @note "Logic teaches us that on such and such a road we are sure of not
 * meeting an obstacle; it does not tell us which is the road that leads to the
 * desired end."
 *       -- Henri Poincare, Science and Method (1908)
 */
module;

#include <concepts>
#include <functional>
#include <optional>

export module dedekind.category:natural;

import :functor;
import :small;
import :morphism;

namespace dedekind::category {

// Hub tags for textbook operator defaults without template-template dispatch.
export struct maybe_hub_tag final {};
export struct identity_hub_tag final {};
export struct box_hub_tag final {};

export inline constexpr maybe_hub_tag maybe_hub{};
export inline constexpr identity_hub_tag identity_hub{};
export inline constexpr box_hub_tag box_hub{};

export template <typename Tag>
concept IsDefaultHubTag =
    std::same_as<std::remove_cvref_t<Tag>, maybe_hub_tag> ||
    std::same_as<std::remove_cvref_t<Tag>, identity_hub_tag> ||
    std::same_as<std::remove_cvref_t<Tag>, box_hub_tag>;

/**
 * @brief The Maybe endofunctor T, implemented via std::optional.
 */
// η (Unit): a -> Maybe a
template <typename A>
constexpr Maybe<std::decay_t<A>> η(A&& value) {
  return std::make_optional(std::forward<A>(value));
}

// Tagged defaults for Maybe hub.
export template <typename A>
  requires IsSpecies<std::decay_t<A>>
constexpr Maybe<std::decay_t<A>> η(maybe_hub_tag, A&& value) {
  return std::make_optional(std::forward<A>(value));
}

/**
 * @brief μ for Maybe: flattens nested optional context.
 * @details Maybe<Maybe<A>> -> Maybe<A>
 */
template <typename A>
  requires IsSpecies<A>
constexpr Maybe<A> μ(Maybe<Maybe<A>> const& mma) {
  return mma.has_value() ? *mma : std::nullopt;
}

export template <typename A>
  requires IsSpecies<A>
constexpr Maybe<A> μ(maybe_hub_tag, Maybe<Maybe<A>> const& mma) {
  return mma.has_value() ? *mma : std::nullopt;
}

/**
 * @brief The Identity Functor Id_C.
 * Reified 1-morphism that preserves both objects and arrows exactly.
 */

// Monadic
template <typename A>
constexpr Identity<std::decay_t<A>> η(A&& value) {
  (void)value;
  return id<std::decay_t<A>>();
}

// Tagged defaults for Identity hub.
export template <typename A>
  requires IsSpecies<std::decay_t<A>>
constexpr Identity<std::decay_t<A>> η(identity_hub_tag, A&& value) {
  (void)value;
  return id<std::decay_t<A>>();
}

/**
 * @brief Monadic join (μ): Identity<Identity<A>> → Identity<A>.
 * In hub/spoke terms: unwraps a doubly-wrapped spoke.
 */
template <typename A>
  requires IsSpecies<A>
constexpr Identity<A> μ(Identity<Identity<A>> const& iia) {
  (void)iia;
  return id<A>();
}

export template <typename A>
  requires IsSpecies<A>
constexpr Identity<A> μ(identity_hub_tag, Identity<Identity<A>> const& iia) {
  (void)iia;
  return id<A>();
}

// Comonadic
template <typename A>
constexpr Identity<A> ε(Identity<A> const& ia) {
  (void)ia;
  return id<A>();
}

export template <typename A>
  requires IsSpecies<A>
constexpr Identity<A> ε(identity_hub_tag, Identity<A> const& ia) {
  (void)ia;
  return id<A>();
}

/**
 * @brief Comonadic duplicate (δ): Identity<A> → Identity<Identity<A>>.
 * In hub/spoke terms: wraps a spoke into a doubly-wrapped spoke.
 */
template <typename A>
constexpr Identity<Identity<A>> δ(Identity<A> const& ia) {
  (void)ia;
  return id<Identity<A>>();
}

export template <typename A>
  requires IsSpecies<A>
constexpr Identity<Identity<A>> δ(identity_hub_tag, Identity<A> const& ia) {
  (void)ia;
  return id<Identity<A>>();
}

/**
 * @brief The Box endofunctor.
 * Reified 1-morphism that preserves both objects and arrows exactly.
 */
// Monadic
template <typename A>
constexpr Box<std::decay_t<A>> η(A&& value) {
  return {std::forward<A>(value)};
}

// Tagged defaults for Box hub.
export template <typename A>
  requires IsSpecies<std::decay_t<A>>
constexpr Box<std::decay_t<A>> η(box_hub_tag, A&& value) {
  return {std::forward<A>(value)};
}

/**
 * @brief μ for Box: flattens nested box context.
 * @details Box<Box<A>> -> Box<A>
 */
template <typename A>
  requires IsSpecies<A>
constexpr Box<A> μ(Box<Box<A>> const& bba) {
  return {bba.value.value};
}

export template <typename A>
  requires IsSpecies<A>
constexpr Box<A> μ(box_hub_tag, Box<Box<A>> const& bba) {
  return {bba.value.value};
}

// Comonadic
template <typename A>
constexpr A ε(Box<A> const& ba) {
  return ba.value;
}

export template <typename A>
  requires IsSpecies<A>
constexpr A ε(box_hub_tag, Box<A> const& ba) {
  return ba.value;
}

template <typename A>
constexpr Box<Box<A>> δ(Box<A> const& ba) {
  return {ba};
}

export template <typename A>
  requires IsSpecies<A>
constexpr Box<Box<A>> δ(box_hub_tag, Box<A> const& ba) {
  return {ba};
}

/**
 * @concept IsPreTransformation
 * @brief Level 2.1: The raw family of components.
 */
export template <typename Α, typename CatS, typename CatT>
concept IsPreTransformation = IsCategory<CatS> && IsCategory<CatT> &&
                              requires(Α α, typename CatS::Arrow::Domain c) {
                                // The machine: takes an object in S, returns an
                                // arrow in T
                                { α(c) } -> std::same_as<typename CatT::Arrow>;
                              };

/**
 * @concept IsTwoMorphism
 * @brief Level 2.15: The bridge between two specific Functors.
 */
export template <typename Α, typename F, typename G>
concept IsTwoMorphism =
    IsFunctor<F> && IsFunctor<G> &&
    // Symmetry check: Both functors must connect the same Categories
    std::same_as<typename F::Σ_cat, typename G::Σ_cat> &&
    std::same_as<typename F::Τ_cat, typename G::Τ_cat> &&
    requires(Α α, typename F::Σ_cat::Arrow::Domain c) {
      // Component at c connects F(c) to G(c) in the Target Hub
      // In the single-species setting, we recover F(c) and G(c) by lifting
      // the identity spoke on c and reading the resulting spoke's domain.
      requires std::same_as<typename decltype(α(c))::Domain,
                            typename decltype(F{}.φ(
                                F::Σ_cat::id_c(c)))::Domain>;
      requires std::same_as<typename decltype(α(c))::Codomain,
                            typename decltype(G{}.φ(
                                G::Σ_cat::id_c(c)))::Domain>;
    };

/**
 * @concept IsNaturalTransformation
 * @theorem Naturality (F ⟹ G)
 */
export template <typename Α, typename F, typename G>
concept IsNaturalTransformation =
    IsTwoMorphism<Α, F, G> &&
    requires(Α α, F f_map, G g_map, typename F::Σ_cat::Arrow::Domain c) {
      // The Slide is witnessed through the identity spoke at c.
      // This keeps the condition structural for categories whose object
      // labels are recovered via their identity arrows.
      {
        (f_map >> F::Σ_cat::id_c(c)) >> α(c)
      } -> std::same_as<decltype(α(c) >> (g_map >> F::Σ_cat::id_c(c)))>;
    };

/**
 * @brief The Identity Natural Transformation for Functor F.
 * @details α(c) = id_{F(c)}.
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
      return f_map.φ(F::Σ_cat::id_c(c));
    }
  }
};

/**
 * @brief Vertical composition of two 2-morphisms (β . α).
 * @details α: F => G, β: G => H. Result: F => H.
 */
export template <typename Α, typename Β, IsFunctor F, IsFunctor G, IsFunctor H>
  requires IsNaturalTransformation<Α, F, G> && IsNaturalTransformation<Β, G, H>
struct vertical_composition {
  Α α;
  Β β;

  auto operator()(const typename F::Σ_cat::Arrow::Domain& c) const {
    return α(c) >> β(c);
  }
};

/**
 * @brief Horizontal composition (Godement product).
 * @details F: C->D, G: D->E and α: F=>F', β: G=>G'.
 */
export template <typename Α, typename Β, IsFunctor F, IsFunctor F_prime,
                 IsFunctor G, IsFunctor G_prime>
  requires IsNaturalTransformation<Α, F, F_prime> &&
           IsNaturalTransformation<Β, G, G_prime>
struct horizontal_composition {
  Α α;
  Β β;
  G g_functor;  // Required for mapping the components of α through G

  auto operator()(const typename F::Σ_cat::Arrow::Domain& c) const {
    // (β * α)_c = G'(α_c) >> β_{F(c)}
    // or equivalent: β_{F'(c)} >> G(α_c)
    return G_prime{}.φ(α(c)) >>
           β(static_cast<typename G::Σ_cat::Arrow::Domain>(c));
  }
};

}  // namespace dedekind::category
