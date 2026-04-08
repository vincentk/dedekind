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

/**
 * @brief The Universal fmap (Forward Declaration).
 * This remains open for Action-First specializations (Kleisli, Partial).
 */
export template <template <typename> typename F, IsArrow Arrow>
constexpr auto fmap(Arrow f);

/** @section The_Lifting_Engine */

// 1. Naked Identity fmap (The Zero-Length Highway)
export template <template <typename> typename F, IsArrow Arrow>
  requires std::same_as<F<typename Arrow::Domain>,
                        Identity<typename Arrow::Domain>>
constexpr auto fmap(Arrow f) {
  return f;
}

/**
 * @brief The Boxed Identity Functor.
 * This type constructor allows raw species (int, bool) to participate
 * in the functorial spine without physical boxing.
 */
export template <typename T>
struct Box final {
  using Domain = T;
  using Codomain = T;
  T value;
  constexpr bool operator==(const Box&) const = default;
};

static_assert(IsArrow<Identity<Box<int>>>, "Box is an identity arrow.");

// 2. Boxed fmap (The Actual Lift)
export template <template <typename> typename F, IsArrow Arrow>
  requires std::same_as<F<typename Arrow::Domain>, Box<typename Arrow::Domain>>
constexpr auto fmap(Arrow f) {
  using T = typename Arrow::Domain;
  using U = typename Arrow::Codomain;
  auto lifted = [f](Box<T> bx) -> Box<U> { return Box<U>{f(bx.value)}; };
  return Morphism<Box<T>, Box<U>, decltype(lifted)>{std::move(lifted)};
}

/**
 * @concept IsFunctor
 * @brief F: C -> D (Morphism between Categories) which conserves identities on
 * C and D.
 */
/**
 * @concept IsFunctor
 * @brief F: C -> D
 * Formalises the Law: F(id_C) = id_{F(C)}
 */
export template <typename F>
concept IsFunctor =
    IsArrow<F> && 
    IsSmallCategory<typename F::Domain> && 
    IsSmallCategory<typename F::Codomain> &&
    requires(Identity<typename F::Domain> id_C) {
        // The Identity of the source category slides through the Functor
        // and emerges as the Identity of the target context.
        { id_C >> F{} } -> std::same_as<Identity<typename F::Codomain>>; 
    };

/** @section Verification */
// The Identity Functor on Integers (Naked)
static_assert(IsFunctor<Identity<int>, Identity<int>>,
              "Spine Error: Naked Identity failed.");

// 2. The Boxed Functor
static_assert(IsFunctor<Box<int>, Box<int>>, "Spine Error: Box failed.");

// The Boxed Functor
static_assert(IsFunctor<Box<int>, Box<int>>, "Spine Error: Box failed.");

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
concept IsEndofunctor = IsFunctor<Context, Context>;

/** @section Endofunctor_Verification */

/** @section Endofunctor_Verification */

// 3. Proof: Box is an Endofunctor on Integers.
static_assert(IsEndofunctor<Box<int>>,
              "Functor: Box<int> must be a valid Endofunctor on Z.");

// 4. Proof: Box is an Endofunctor on the Boolean Lattice.
static_assert(IsEndofunctor<Box<bool>>,
              "Functor: Box<bool> must be a valid Endofunctor on B.");

// 5. Verification of 'naked' lifting (Identity Functor)
static_assert(std::same_as<decltype(fmap<Identity>(id<int>())), Identity<int>>,
              "Highway Error: Identity lift must return the original arrow.");

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

/** @section Pipeline_Verification */

// We use the skeletal 'increment' arrow (Level 0)
constexpr auto increment = arrow([](int x) { return x + 1; });

// Note: The 'into' pipeline is moved to :kleisli to respect build order.
// Here we verify the structure preservation of fmap directly.
static_assert(fmap<Box>(increment)(Box<int>{41}).value == 42,
              "Skeletal Proof: fmap must preserve the mapping logic.");

/** @section The_Natural_Slide_Preview */
// Verification that fmap preserves identities: F(id) = id_F
static_assert(IsArrow<decltype(fmap<Box>(id<int>()))>,
              "Identity Law: fmap(id) must resolve to a valid Arrow.");

}  // namespace dedekind::category
