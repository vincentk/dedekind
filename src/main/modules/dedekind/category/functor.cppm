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
 *
 * @section Skeletal_Discovery
 * By utilizing the "Canonical Discovery" engine, Dedekind functors do not
 * require explicit operator passing. The species T announces its own
 * category structure, allowing the Functor to verify the mapping properties
 * through introspection.
 */

module;

#include <concepts>
#include <functional>

export module dedekind.category:functor;

import :action;
import :morphism;

namespace dedekind::category {

/** @section Monadic_Bootstrap */
export template <template <typename> typename F, typename T>
struct η;
export template <template <typename> typename F, typename T>
struct μ;

/** @brief fmap derived from Kleisli Triple: fmap(f) = (f >> η) >> μ */
export template <template <typename> typename F, IsArrow Arrow>
constexpr auto fmap(Arrow f) {
  using U = typename Arrow::Codomain;
  return (f >> η<F, U>{}) >> μ<F, U>{};
}

/**
 * @concept IsFunctor
 * @brief F: C -> D
 * Formalises the Law: F(id_C) = id_{F(C)}
 */
export template <typename F>
concept IsFunctor = IsArrow<F> && IsSmallCategory<typename F::Domain> &&
                    IsSmallCategory<typename F::Codomain> &&
                    requires(Identity<typename F::Domain> id_C) {
                      // The Identity of the source category slides through the
                      // Functor and emerges as the Identity of the target
                      // context.
                      {
                        F{}(id_C)
                      } -> std::same_as<Identity<typename F::Codomain>>;
                    };

/** @section The_Identity_Functor */

/** @section Verification */
// The Identity Functor on Integers (Naked)
static_assert(IsFunctor<Identity<int>>,
              "Identity<int> must be a valid Functor.");

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
    std::same_as<typename Context::Domain, typename Context::Codomain>;

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

static_assert(IsArrow<Identity<Box<int>>>, "Box is an identity arrow.");

/** @brief The Functorial Bridge Tag. */
template <template <typename> typename F>
struct fmap_tag {};

/** @brief Global witness for Box mapping. */
export template <typename T = void>  // Template to keep it header-friendly
constexpr auto Boxed = fmap_tag<Box>{};

/** @brief Postfix Operator: arrow >> Boxed */
export template <typename Arrow, template <typename> typename F>
  requires IsArrow<Arrow>
constexpr auto operator>>(Arrow f, fmap_tag<F>) {
  return fmap<F>(f);
}

}  // namespace dedekind::category
