/**
 * @file dedekind/category/functor.cppm
 * @module dedekind.category:functor
 * @brief Level 2: The Functorial Spine (Structure Preservation).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "एकं सद् विप्रा बहुधा वदन्ति"
 * (Truth is One, though the sages speak of it as many.)
 * — ऋग्वेद (Rig Veda)
 *
 * @section The_Functorial_Mapping
 * A Functor is a bridge between categories that preserves the "Grammar" of
 * the source. In our skeletal framework, a Functor F: T -> U ensures that:
 * 1. Objects are mapped: Every species T has a corresponding F<T>.
 * 2. Morphisms are lifted: Every arrow f: T -> U has a corresponding F(f).
 * 3. Identities are preserved: F(id_T) = id_{F<T>}.
 * 4. Composition is preserved: F(f >> g) = F(f) >> F(g).
 * 
 * @details Represents a Functor as a morphism between categories (Σ_cat -> Τ_cat). 
 * In this library's hierarchy, a Functor is an "arrow between species" that 
 * maps morphisms from the source category to the target category while 
 * strictly preserving identity and composition laws.
 *
 *
 * @section The_Hub_Spoke_Architecture
 * In our Category-First formalism, we distinguish between the **Hub** (the
 * category-level structure that owns arrow factories such as @ref id_c) and
 * the **Spoke** (an ordinary arrow inside such a category).
 *
 * While a Functor is technically an @ref IsArrow mapping spokes to spokes,
 * it is also a hub arrow in the sense of @ref IsHubArrow: its Domain and
 * Codomain are themselves arrow spaces. The Categorical Laws
 * (Identity/Composition preservation) therefore require access to the Hub's
 * static factories (e.g., @ref id_c).
 *
 * Therefore, every @ref IsFunctor must carry handles to its @ref SourceCategory
 * and @ref TargetCategory. Without these "Hub" handles, a mapping of
 * types would be a mere function; with them, it becomes a verified
 * structure-preserving Functor.
 */

module;

#include <concepts>
#include <functional>

export module dedekind.category:functor;

import :action;
import :morphism;
import :small;

namespace dedekind::category {

/**
 * @brief φ (phi): Morphism lift (fmap).
 * Constraint: (a -> b) -> (T a -> T b)
 */
template <template <typename> typename T, typename A, typename F>
constexpr auto φ(T<A> const&, F&&) -> T<std::invoke_result_t<F, A>> = delete;

/**
 * @brief an alias for φ (phi) to match the standard "fmap" terminology.
 */
template <template <typename> typename T, typename A, typename F>
constexpr auto fmap(T<A> const& m, F&& f) {
  return φ(m, std::forward<F>(f));
}

// ma >> f  =>  φ(ma, f)  [The downstram fish]
template <template <typename> typename T, typename A, typename F>
  requires Functor<T>
constexpr auto operator>>(T<A> const& ma, F&& f) {
  return φ(ma, std::forward<F>(f));
}

// f << wa  =>  φ(wa, f)  [The upstream fish]
template <template <typename> typename T, typename A, typename F>
  requires Functor<T>
constexpr auto operator<<(F&& f, T<A> const& wa) {
  return φ(wa, std::forward<F>(f));
}

// f >> g  =>  g ∘ f      [The Functorial "Fish"]
// At this level, it's just standard composition.
template <typename F, typename G>
constexpr auto operator>>(F&& f, G&& g) {
  return [f = std::forward<F>(f), g = std::forward<G>(g)](auto&& x) {
    return std::invoke(g, std::invoke(f, std::forward<decltype(x)>(x)));
  };
}

/**
 * @brief The "Upstream Fish" (Standard Composition)
 * g << f  =>  g ∘ f
 */
template <typename G, typename F>
constexpr auto operator<<(G&& g, F&& f) {
  return [g = std::forward<G>(g), f = std::forward<F>(f)](auto&& x) {
    return std::invoke(g, std::invoke(f, std::forward<decltype(x)>(x)));
  };
}

/**
 * @brief The Maybe endofunctor T, implemented via std::optional.
 */
template <typename T>
using Maybe = std::optional<T>;

/**
 * @brief φ for Maybe (std::optional).
 * If ma has a value, applies f and wraps the result.
 */
template <typename A, typename F>
constexpr auto φ(Maybe<A> const& ma, F&& f)
    -> Maybe<std::invoke_result_t<F, A>> {
  if (ma) {
    return std::make_optional(std::invoke(std::forward<F>(f), *ma));
  }
  return std::nullopt;
}

/**
 * @brief φ for Identity.
 * Simply applies the function to the underlying value.
 */
template <typename A, typename F>
constexpr auto φ(Identity<A> const& id, F&& f)
    -> Identity<std::invoke_result_t<F, A>> {
  return {std::invoke(std::forward<F>(f), id.value)};
}

/**
 * @brief φ for Box.
 */
template <typename A, typename F>
constexpr auto φ(Box<A> const& box, F&& f) -> Box<std::invoke_result_t<F, A>> {
  return {std::invoke(std::forward<F>(f), box.value)};
}

/**
 * @concept IsFunctor
 * @brief A 1-morphism mapping CatS -> CatT.
 *
 * @details
 * 1. Mapping: F(f: A->B) -> F(f): F(A)->F(B)
 * 2. Identity Law: F(id_c) = id_{F(c)}
 * 3. Composition Law: F(f >> g) = F(f) >> F(g)
 *
 * Because `IsCategory` in this project is single-species, the object witness
 * for `c` is always recovered by first forming the identity arrow `id_c(c)`.
 * Functorial object mapping is therefore observed indirectly through the spoke
 * `F(id_c(c))`, whose domain recovers the image object `F(c)`.
 */
export template <typename F>
concept IsFunctor = IsArrow<F> && requires {
  typename F::Σ_cat;
  typename F::Τ_cat;
  requires IsCategory<typename F::Σ_cat>;
  requires IsCategory<typename F::Τ_cat>;
} && requires(F f, typename F::Σ_cat::Arrow f_c) {
  // 1. Morphism mapping check
  { φ(f_c >> f_c) } -> std::convertible_to<typename F::Τ_cat::Arrow>;

  // 2. Identity Preservation check (The Textbook Embedding)
  // We introduce 'c' to represent an arbitrary object in the source category
  requires requires(typename F::Σ_cat::Arrow::Domain c) {
    /**
     * The image of the identity on object c in the source category
     * must be exactly the identity morphism in the target category.
     */
    requires std::same_as<decltype(φ(F::Σ_cat::id_c(c))),
                          typename F::Τ_cat::Id>;
  };
};

template <typename F, typename G, typename H>
  requires IsFunctor<F>
void verify_functor_composition(typename F::Σ_cat::Arrow f,
                                typename F::Σ_cat::Arrow g) {
  // Path 1: φ(g >> f)
  using Path1 = decltype(φ(g >> f));

  // Path 2: φ(g) >> φ(f)
  // (This assumes your target category Τ_cat supports >> for its arrows)
  using Path2 = decltype(φ(g) >> φ(f));

  static_assert(std::same_as<Path1, Path2>,
                "Functor violates type-level composition preservation!");
}

/**
 * @brief Composite Functor G . F (Maps CatS -> CatT -> CatU).
 *
 * Verifies that the target of the first functor matches the source
 * of the second, maintaining the structural spine.
 */
export template <IsFunctor F, IsFunctor G>
  requires std::same_as<typename F::Τ_cat, typename G::Σ_cat>
struct composite_functor {
  using Σ_cat = typename F::Σ_cat;
  using Τ_cat = typename G::Τ_cat;

  using Domain = typename F::Domain;
  using Codomain = typename G::Codomain;

  /**
   * @brief φ(f) = G.φ(F.φ(f))
   * Chaining the functorial lift.
   */
  template <typename A>
    requires IsArrow<std::remove_cvref_t<A>>
  constexpr auto φ(A&& f) const {
    // First lift through F, then through G
    return G{}.φ(F{}.φ(std::forward<A>(f)));
  }

  /** @brief Morphic Action: (G . F)(f) */
  constexpr Codomain operator()(const Domain& f) const { return φ(f); }
};

/**
 * @concept IsEndofunctor
 * @brief A structure-preserving mapping from a Category back to itself (F : 𝒞 →
 * 𝒞).
 *
 * @concept IsEndofunctor
 * @brief F : C -> C
 * The reflexive case where the species-space remains invariant.
 */
export template <typename Context>
concept IsEndofunctor =
    IsFunctor<Context> &&
    std::same_as<typename Context::Σ_cat, typename Context::Τ_cat>;

static_assert(IsEndofunctor<std::optional>);
static_assert(IsEndofunctor<Identity>);
static_assert(IsEndofunctor<Box>);

/**
 * @section The_Identity_Functor
 *
 * @brief The Identity Functor Id_C.
 * Reified 1-morphism that preserves both objects and arrows exactly.
 */
export template <IsCategory CatC>
struct identity_functor {
  // Hub Handles
  using Σ_cat = CatC;
  using Τ_cat = CatC;

  // Arrow Mapping Labels
  using Domain = typename Σ_cat::Arrow;
  using Codomain = typename Τ_cat::Arrow;

  /**
   * @section Functorial_Action
   * @brief fmap(f) = f.
   * Implementation of the identity lift for any arrow in the category.
   */
  template <typename T>
    requires IsArrow<std::remove_cvref_t<T>>
  constexpr auto φ(T&& f) const noexcept {
    return std::forward<T>(f);
  }

  /** @brief Morphic Action: Allows the functor itself to act as an Arrow. */
  constexpr Codomain operator()(const Domain& f) const noexcept {
    return this->φ(f);
  }
};

/**
 * @brief Categorical Composition for Functors (F: C->D, G: D->E).
 * @details This is the hub-level composition law: it composes arrows whose
 *          objects are themselves spokes.
 */
export template <IsFunctor F, IsFunctor G>
  requires std::same_as<typename F::Τ_cat, typename G::Σ_cat>
constexpr auto operator>>(F, G) -> composite_functor<F, G> {
  return {};
}

/**
 * @section Functorial_Lifting_Syntax
 * @brief F >> f = F(f)
 * We use forwarding references (F&&) to ensure this matches better
 * than the "Action" overloads in :morphism.
 * This is the bridge from a hub arrow to a spoke arrow: a functor acts on a
 * spoke and returns the corresponding spoke in the target hub.
 */
export template <typename F, typename Arrow>
  requires IsFunctor<std::remove_cvref_t<F>> &&
           IsArrow<std::remove_cvref_t<Arrow>> &&  // Concepts must be
                                                   // reference-safe
           std::same_as<typename std::remove_cvref_t<Arrow>::Domain,
                        typename std::remove_cvref_t<F>::Σ_cat::Arrow::Domain>
constexpr auto operator>>(F&& functor, Arrow&& f) {
  return functor.φ(std::forward<Arrow>(f));
}

/**
 * @brief The Boxed Species (The F<T> Context).
 *
 * Reifies the "Box" as a categorical object that announces its
 * own species and shape, enabling 1-parameter functorial discovery.
 */
export template <typename T>
struct Box final {
  /** @brief The underlying species (The Object T). */
  using Species = T;

  /** @brief The Functorial Shape (The Type Constructor F). */
  template <typename U>
  using Shape = Box<U>;

  /** @brief The physical payload. */
  T value;

  /** @brief Equality for structural verification in static_asserts. */
  constexpr bool operator==(const Box&) const = default;
};

export template <typename T>
struct box_functor {
  // 1. Hub Handles (Must be these exact names for IsFunctor)
  using Σ_cat = DiscreteCategory<T>;
  using Τ_cat = DiscreteCategory<Box<T>>;

  // 2. Spoke Handles (Must be these exact names for IsArrow)
  using Domain = typename Σ_cat::Arrow;
  using Codomain = typename Τ_cat::Arrow;

  // 3. Identity Case: Strictly returns the Target Hub's Id type
  constexpr auto φ(const Identity<T>&) const noexcept {
    return Τ_cat::id_c(Box<T>{});
  }

  // 4. General Case: Lifts any Morphism
  template <typename A>
    requires IsArrow<std::remove_cvref_t<A>> &&
             (!std::same_as<std::remove_cvref_t<A>, Identity<T>>)
  constexpr auto φ(A&& f) const {
    using PureA = std::remove_cvref_t<A>;
    return arrow([f = std::forward<A>(f)](Box<T> b) {
      return Box<typename PureA::Codomain>{f(std::move(b.value))};
    });
  }

  // 5. Arrow Action (Required to satisfy IsArrow)
  constexpr Codomain operator()(const Domain& f) const noexcept { return φ(f); }
};

using IntCat = DiscreteCategory<int>;
identity_functor<IntCat> Id;
box_functor<int> B;

auto f_functor_test_FIXME = arrow([](int x) { return x + 1; });

// 1. Lifting via Identity Functor
// Syntax: Id >> f == f
static_assert(IsArrow<decltype(Id >> f_functor_test_FIXME)>);

// 2. Lifting via Box Functor
// Syntax: B >> f == Boxed version of f
auto boxed_f = B >> f_functor_test_FIXME;
static_assert(IsArrow<decltype(boxed_f)>);

}  // namespace dedekind::category
