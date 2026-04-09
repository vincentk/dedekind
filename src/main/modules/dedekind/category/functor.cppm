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
 * @section The_Hub_Spoke_Architecture
 * In our Category-First formalism, we distinguish between the **Hub** (the
 * Category struct) and the **Spoke** (the Arrow type).
 *
 * While a Functor is technically an @ref IsArrow mapping spokes to spokes,
 * the Categorical Laws (Identity/Composition preservation) require access
 * to the Hub's static factories (e.g., @ref id_c).
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
 * @concept IsFunctor
 * @brief A 1-morphism mapping CatS -> CatT.
 *
 * @details
 * 1. Mapping: F(f: A->B) -> F(f): F(A)->F(B)
 * 2. Identity Law: F(id_c) = id_{F(c)}
 * 3. Composition Law: F(f >> g) = F(f) >> F(g)
 */
export template <typename F>
concept IsFunctor = IsArrow<F> && requires {
  typename F::SourceCategory;
  typename F::TargetCategory;
  requires IsCategory<typename F::SourceCategory>;
  requires IsCategory<typename F::TargetCategory>;
} && requires(F f, typename F::SourceCategory::Arrow f_c) {
  // The Functor must provide an fmap that preserves composition
  {
    f.fmap(f_c >> f_c)
  } -> std::convertible_to<typename F::TargetCategory::Arrow>;

  // Identity Preservation check using fmap
  requires requires(typename F::SourceCategory::Arrow::Domain c) {
    {
      f.fmap(F::SourceCategory::id_c(c))
    } -> std::same_as<typename F::TargetCategory::Id>;
  };
};

/**
 * @brief Composite Functor G . F (Maps CatS -> CatT -> CatU).
 *
 * Verifies that the target of the first functor matches the source
 * of the second, maintaining the structural spine.
 */
export template <IsFunctor F, IsFunctor G>
  requires std::same_as<typename F::TargetCategory, typename G::SourceCategory>
struct composite_functor {
  using SourceCategory = typename F::SourceCategory;
  using TargetCategory = typename G::TargetCategory;

  using Domain = typename F::Domain;
  using Codomain = typename G::Codomain;

  /**
   * @brief fmap(f) = G.fmap(F.fmap(f))
   * Chaining the functorial lift.
   */
  template <typename A>
    requires IsArrow<std::remove_cvref_t<A>>
  constexpr auto fmap(A&& f) const {
    // First lift through F, then through G
    return G{}.fmap(F{}.fmap(std::forward<A>(f)));
  }

  /** @brief Morphic Action: (G . F)(f) */
  constexpr Codomain operator()(const Domain& f) const { return fmap(f); }
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
    IsFunctor<Context> && std::same_as<typename Context::SourceCategory,
                                       typename Context::TargetCategory>;

/**
 * @section The_Identity_Functor
 *
 * @brief The Identity Functor Id_C.
 * Reified 1-morphism that preserves both objects and arrows exactly.
 */
export template <IsCategory CatC>
struct identity_functor {
  // Hub Handles
  using SourceCategory = CatC;
  using TargetCategory = CatC;

  // Arrow Mapping Labels
  using Domain = typename CatC::Arrow;
  using Codomain = typename CatC::Arrow;

  /**
   * @section Functorial_Action
   * @brief fmap(f) = f.
   * Implementation of the identity lift for any arrow in the category.
   */
  template <typename T>
    requires IsArrow<std::remove_cvref_t<T>>
  constexpr auto fmap(T&& f) const noexcept {
    return std::forward<T>(f);
  }

  /** @brief Morphic Action: Allows the functor itself to act as an Arrow. */
  constexpr Codomain operator()(const Domain& f) const noexcept {
    return this->fmap(f);
  }
};

/** @brief Categorical Composition for Functors (F: C->D, G: D->E) */
export template <IsFunctor F, IsFunctor G>
  requires std::same_as<typename F::TargetCategory, typename G::SourceCategory>
constexpr auto operator>>(F f, G g) {
  // We wrap the base arrow composition but re-attach the Hub handles
  struct Composite
      : decltype(static_cast<const F&>(f) >> static_cast<const G&>(g)) {
    using SourceCategory = typename F::SourceCategory;
    using TargetCategory = typename G::TargetCategory;

    // Inherit the constructor/call operator from the arrow composition
    using Base = decltype(static_cast<const F&>(f) >> static_cast<const G&>(g));
    using Base::Base;
  };
  return Composite{f >> g};
}

/**
 * @section Functorial_Lifting_Syntax
 * @brief F >> f = F(f)
 * We use forwarding references (F&&) to ensure this matches better
 * than the "Action" overloads in :morphism.
 */
export template <typename F, typename Arrow>
  requires IsFunctor<std::remove_cvref_t<F>> &&
           IsArrow<std::remove_cvref_t<Arrow>> &&  // Concepts must be
                                                   // reference-safe
           std::same_as<
               typename std::remove_cvref_t<Arrow>::Domain,
               typename std::remove_cvref_t<F>::SourceCategory::Arrow::Domain>
constexpr auto operator>>(F&& functor, Arrow&& f) {
  return functor.fmap(std::forward<Arrow>(f));
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
  using SourceCategory = DiscreteCategory<T>;
  using TargetCategory = DiscreteCategory<Box<T>>;

  // 2. Spoke Handles (Must be these exact names for IsArrow)
  using Domain = typename SourceCategory::Arrow;
  using Codomain = typename TargetCategory::Arrow;

  // 3. Identity Case: Strictly returns the Target Hub's Id type
  constexpr auto fmap(const Identity<T>&) const noexcept {
    return TargetCategory::id_c(Box<T>{});
  }

  // 4. General Case: Lifts any Morphism
  template <typename A>
    requires IsArrow<std::remove_cvref_t<A>> &&
             (!std::same_as<std::remove_cvref_t<A>, Identity<T>>)
  constexpr auto fmap(A&& f) const {
    using PureA = std::remove_cvref_t<A>;
    return arrow([f = std::forward<A>(f)](Box<T> b) {
      return Box<typename PureA::Codomain>{f(std::move(b.value))};
    });
  }

  // 5. Arrow Action (Required to satisfy IsArrow)
  constexpr Codomain operator()(const Domain& f) const noexcept {
    return fmap(f);
  }
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
