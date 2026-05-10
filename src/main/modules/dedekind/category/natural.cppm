/**
 * @file dedekind/category/natural.cppm
 * @partition :natural
 * @brief Natural Transformations (The Slide between Functors).
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
 * @section natural__Naturality_Condition
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
#include <tuple>  // std::tuple<T> — bona-fide Frobenius carrier (#632)

export module dedekind.category:natural;

import :discrete;  // IsDiscreteCategory — constraint on the trivial-self-
                   // adjunction aliases below (#583 review).
import :functor;
import :small;
import :morphism;

namespace dedekind::category {

// Hub tags for textbook operator defaults without template-template dispatch.
export struct maybe_hub_tag final {};
export struct identity_hub_tag final {};
export struct tuple_hub_tag final {};

export inline constexpr maybe_hub_tag maybe_hub{};
export inline constexpr identity_hub_tag identity_hub{};
export inline constexpr tuple_hub_tag tuple_hub{};

export template <typename Tag>
concept IsDefaultHubTag =
    std::same_as<std::remove_cvref_t<Tag>, maybe_hub_tag> ||
    std::same_as<std::remove_cvref_t<Tag>, identity_hub_tag> ||
    std::same_as<std::remove_cvref_t<Tag>, tuple_hub_tag>;

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

// (Maybe is a monad, not a comonad — counit on @c std::nullopt has no
// honest answer.  The bona-fide Frobenius carrier in this codebase is
// @c std::tuple<T> below; see the @c tuple_hub_tag block.)

/**
 * @brief The std::tuple<T> Frobenius carrier.
 *
 * @details @c std::tuple<T> is a 1-tuple — it always has a single
 * element, so the comonadic counit @c std::get<0> is total (no
 * Some-fragment caveat) and the duplicate wraps once more cleanly.
 * Combined with the textbook monadic structure (η as @c
 * std::make_tuple, μ via @c std::get<0> on the inner tuple), this
 * makes @c std::tuple<T> the project's bona-fide Frobenius witness
 * — both Kleisli and co-Kleisli laws hold without concession.
 *
 * Replaces the vestigial @c Box-as-trivially-comonadic carrier
 * retired in #632.
 */
// η: a → std::tuple<a>
template <typename A>
constexpr std::tuple<std::decay_t<A>> η(tuple_hub_tag, A&& value) {
  return std::tuple<std::decay_t<A>>{std::forward<A>(value)};
}

// μ: std::tuple<std::tuple<A>> → std::tuple<A>
export template <typename A>
  requires IsSpecies<A>
constexpr std::tuple<A> μ(tuple_hub_tag, std::tuple<std::tuple<A>> const& tta) {
  return std::get<0>(tta);
}

// ε: std::tuple<A> → A
template <typename A>
constexpr A ε(std::tuple<A> const& ta) {
  return std::get<0>(ta);
}

export template <typename A>
  requires IsSpecies<A>
constexpr A ε(tuple_hub_tag, std::tuple<A> const& ta) {
  return std::get<0>(ta);
}

// δ: std::tuple<A> → std::tuple<std::tuple<A>>
template <typename A>
  requires IsSpecies<A>
constexpr std::tuple<std::tuple<A>> δ(std::tuple<A> const& ta) {
  return std::tuple<std::tuple<A>>{ta};
}

export template <typename A>
  requires IsSpecies<A>
constexpr std::tuple<std::tuple<A>> δ(tuple_hub_tag, std::tuple<A> const& ta) {
  return std::tuple<std::tuple<A>>{ta};
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
 * @concept IsPreTransformation
 * @brief The raw family of components.
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
 * @brief The bridge between two specific Functors.
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

// =============================================================================
// Discrete-restriction representatives of the textbook Disc ⊣ U adjunction.
// =============================================================================
//
// For any @c IsDiscreteCategory @c C, the meta-categorical @c Disc and @c U
// functors collapse to the identity endofunctor on @c C, and their unit /
// counit collapse to the identity natural transformation on that endofunctor.
// The aliases below pin those collapsed entities under their textbook role
// names so call sites reading @c Disc @c ⊣ @c U as a @b natural-transformation
// -anchored adjunction can refer to bona fide named types in @c :natural
// rather than to documentary commentary.
//
// The full meta-categorical @c Disc @c ⊣ @c U with cross-category source /
// target is left as future work; it requires meta-categories of sets and
// categories that the project does not yet model.  Tracked under #587.
// (The discrete-restriction representatives below were filed and addressed
// under #572.)

/**
 * @brief Discrete-restriction representative of the textbook @c Disc / @c U
 *        functor: the identity endofunctor on a discrete category @c C.
 *
 * @details In the meta-categorical @c Disc @c ⊣ @c U adjunction, both
 * @c Disc and @c U restrict to identity-on-objects-and-arrows when their
 * source / target collapses to a single discrete category @c C.  This
 * alias pins the identity endofunctor under that textbook role name so
 * the adjunction-machinery static_asserts can refer to it directly.
 */
export template <typename C>
  requires IsDiscreteCategory<C>
using disc_self_endofunctor_t = identity_functor<C>;

/**
 * @brief Discrete-restriction representative of the textbook unit @c η /
 *        counit @c ε of the @c Disc @c ⊣ @c U adjunction: the identity
 *        natural transformation on @c disc_self_endofunctor_t<C>.
 *
 * @details In the meta-categorical adjunction the unit @c η_S sends each
 * element of @c S to its embedding in @c U(F(S)); under the discrete
 * restriction @c F = U = Id and @c η = @c ε is the identity natural
 * transformation on the identity endofunctor.  Pinned under the
 * textbook role name so the adjunction machinery sees a bona fide named
 * type rather than documentary commentary.
 */
export template <typename C>
  requires IsDiscreteCategory<C>
using disc_self_unit_t = identity_transformation<disc_self_endofunctor_t<C>>;

}  // namespace dedekind::category
