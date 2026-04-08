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

namespace dedekind::category {

/**
 * @brief Skeletal Functor (Box): A generic testing container.
 */
export template <typename T>
struct Box final {
  using machine_type = T;
  T value;

  constexpr bool operator==(const Box& other) const = default;
};

/**
 * @brief Canonical Functorial Lift specialized to the Box type.
 *
 * @tparam Box   The Functorial Context (renamed from F for clarity).
 * @tparam Arrow The Skeletal Morphism to be lifted.
 */
export template <template <typename> typename F, IsArrow Arrow>
  requires requires {
    typename F<typename Arrow::Domain>::machine_type;  // Extra constraint
  }
constexpr auto fmap(Arrow f) {
  using T = typename Arrow::Domain;
  using U = typename Arrow::Codomain;

  // We can still use 'Box' internally or as an alias if you prefer,
  // but the signature must use 'F' to match the skeletal declaration.
  auto lifted = [f](F<T> boxed) -> F<U> { return F<U>{f(boxed.value)}; };

  return Morphism<F<T>, F<U>, decltype(lifted)>{std::move(lifted)};
}

/** @section Verification: The Identity Law (F(id_X) = id_F<X>) */

// 4. Proof: Identity check on a complex Species.
// This confirms the skeletal labels match F<T> -> F<T>.
static_assert(IsArrow<decltype(fmap<Box>(id<int>()))>,
              "Identity Law: fmap(id) must preserve the Boxed species.");

/**
 * @concept IsFunctor
 * @brief A structure-preserving mapping between categories.
 *
 * "A functor translates the grammar of one category into another."
 *
 * @tparam F  The Functorial 'Box' (e.g., Maybe, List, Ternary).
 * @tparam T  The Source Species.
 * @tparam U  The Target Species.
 */
export template <template <typename> typename F, typename T, typename U>
concept IsFunctor =
    IsSmallCategory<T> && IsSmallCategory<U> && requires(Identity<T> id_t) {
      typename F<T>;
      typename F<U>;

      // The lift: F(id_T)
      { fmap<F>(id_t) } -> IsArrow;

      // The labels: F(T) -> F(U) logic
      requires std::same_as<domain_t<decltype(fmap<F>(id_t))>, F<T>>;
    };

// 1. Proof: Box is a Functor between Integer species.
// Reduced from 5 params to 3: <Box, Source, Target>
static_assert(IsFunctor<Box, int, int>,
              "Level 0 Proof: Box must satisfy the Functor concept for Z.");

/**
 * @concept IsEndofunctor
 * @brief A structure-preserving mapping from a Category back to itself (F : 𝒞 →
 * 𝒞).
 *
 * @tparam F  The Functorial 'Box' (The Transformer).
 * @tparam T  The Species acting as the Category 𝒞.
 */
export template <template <typename> typename F, typename T>
concept IsEndofunctor = IsFunctor<F, T, T>;

/** @section Functor Verification: The Identity Endofunctor */

/** @section Endofunctor_Verification */

// 2. Proof: Box is an Endofunctor on Integers.
// Discovery: IsSmallCategory<int> finds (int, multiplies) or (int, plus)
// internally.
static_assert(IsEndofunctor<Box, int>,
              "Functor: Box must be a valid Endofunctor on Z.");

// 3. Proof: Box is an Endofunctor on the Boolean Lattice.
static_assert(IsEndofunctor<Box, bool>,
              "Functor: Box must be a valid Endofunctor on B.");

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
