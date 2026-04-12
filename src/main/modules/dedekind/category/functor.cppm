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
 * @details Represents a Functor as a morphism between categories (Σ_cat ->
 * Τ_cat). In this library's hierarchy, a Functor is an "arrow between species"
 * that maps morphisms from the source category to the target category while
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

  /**
   * 2. The Type Constructor (The Recipe)
   * This verifies that F::Shape exists and is a valid template.
   * We test it with 'int' as a dummy species.
   */
  typename F::template Shape<int>;
} && requires(F f, typename F::Σ_cat::Arrow f_c) {
  // 1. Morphism mapping check
  { f.φ(f_c >> f_c) } -> IsArrow;

  requires std::same_as<
      typename decltype(f.φ(f_c))::Domain,
      typename F::template Shape<typename F::Σ_cat::Arrow::Domain>>;

  requires std::same_as<
      typename decltype(f.φ(f_c))::Codomain,
      typename F::template Shape<typename F::Σ_cat::Arrow::Codomain>>;

  // 2. Identity Preservation check (The Textbook Embedding)
  // We introduce 'c' to represent an arbitrary object in the source category
  // 2. Identity Preservation check
  requires requires(typename F::Σ_cat::Arrow::Domain c) {
    /**
     * The image of the identity on object c in the source category
     * must be an arrow in the target category.
     *
     * Note: We use 'f.φ' because φ is a member function.
     */
    { f.φ(F::Σ_cat::id_c(c)) } -> IsArrow;

    requires std::same_as<
        typename decltype(f.φ(F::Σ_cat::id_c(c)))::Domain,
        typename F::template Shape<typename F::Σ_cat::Arrow::Domain>>;

    requires std::same_as<
        typename decltype(f.φ(F::Σ_cat::id_c(c)))::Codomain,
        typename F::template Shape<typename F::Σ_cat::Arrow::Domain>>;
  };
};

template <typename F, typename G, typename H>
  requires IsFunctor<F>
void verify_functor_composition(typename F::Σ_cat::Arrow f,
                                typename F::Σ_cat::Arrow g) {
  // Path 1: φ(g >> f)
  using Path1 = decltype(F::φ(g >> f));

  // Path 2: φ(g) >> φ(f)
  // (This assumes your target category Τ_cat supports >> for its arrows)
  using Path2 = decltype(F::φ(g) >> F::φ(f));

  static_assert(std::same_as<Path1, Path2>,
                "Functor violates type-level composition preservation!");
}

/**
 * @brief φ (phi): The Functorial Morphism Lift (fmap).
 *
 * Constraint: (a -> b) -> (T a -> T b)
 *
 * @details This is the core lifting engine of the library. It upgrades a
 * "homeless" function (a raw morphism) into a bona fide arrow within the
 * Functorial context.
 *
 * @tparam 𝗙 The Functorial type (The Box/Shape). Must satisfy @ref IsFunctor.
 * @tparam 𝗳 The raw morphism type. Must be invocable with the source category's
 * species.
 *
 * @param ma The functorial witness (the "Boxed" value) representing an object
 * in Σ_cat.
 * @param f  The transformation A -> B to be lifted into the functor.
 *
 * @return A new functorial instance of the same @ref Shape, containing the
 * result of f.
 * @note This base overload is deleted to force specialization for concrete
 * functors.
 */
template <typename 𝗙, typename 𝗳>
  requires IsFunctor<𝗙> && std::invocable<𝗳, typename 𝗙::Σ_cat::Species>
[[nodiscard]]
constexpr auto φ(𝗙 const&, 𝗳&&) -> typename 𝗙::template Shape<
    std::invoke_result_t<𝗳, typename 𝗙::Σ_cat::Species>> = delete;

/**
 * @brief an alias for φ (phi) to match the standard "fmap" terminology.
 */
template <typename... Args>
[[nodiscard]]
constexpr auto fmap(Args&&... args)
    -> decltype(φ(std::forward<Args>(args)...)) {
  return φ(std::forward<Args>(args)...);
}

/**
 * @brief The "Downstream Fish" Operator (ma >> f).
 *
 * @details Provides a pipe-like syntax for functorial mapping (fmap).
 * It bridges the Value World and the Category World by applying a
 * raw function to the contents of a verified Categorical Functor.
 *
 * @example
 * auto result = maybe_value >> [](int i){ return i + 1; };
 *
 * @tparam 𝗙 The type of the Functorial container.
 * @tparam 𝗳 The type of the function to be lifted.
 *
 * @param ma The "Boxed" value (the Object) to operate upon.
 * @param f  The raw function (The Morphism) to apply.
 *
 * @return The result of φ(ma, f).
 */
template <typename 𝗙, typename 𝗳>
  requires IsFunctor<𝗙> && std::invocable<𝗳, typename 𝗙::Σ_cat::Species>
[[nodiscard]]
constexpr auto operator>>(𝗙 const& ma, 𝗳&& f) {
  return φ(ma, std::forward<𝗳>(f));
}

// f << wa  =>  φ(wa, f)  [The upstream fish]
template <typename 𝗙, typename 𝗳>
  requires IsFunctor<𝗙> && std::invocable<𝗳, typename 𝗙::Σ_cat::Species>
[[nodiscard]]
constexpr auto operator<<(𝗳&& f, 𝗙 const& wa) {
  return φ(wa, std::forward<𝗳>(f));
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

// f >> g  =>  g ∘ f      [The Functorial "Fish"]
// At this level, it's just standard composition.
template <typename 𝗙, typename 𝗚>
  requires IsFunctor<𝗙> && IsFunctor<𝗚> &&
           std::same_as<typename 𝗙::Σ_cat, typename 𝗚::Σ_cat> &&
           std::same_as<typename 𝗙::Τ_cat, typename 𝗚::Τ_cat>
[[nodiscard]]
constexpr auto operator>>(𝗙&& f, 𝗚&& g) {
  return composite_functor<std::remove_cvref_t<𝗙>, std::remove_cvref_t<𝗚>>{
      std::forward<𝗙>(f), std::forward<𝗚>(g)};
}

/**
 * @brief The "Upstream Fish" (Standard Composition)
 * g << f  =>  g ∘ f
 */
template <typename 𝗙, typename 𝗚>
  requires IsFunctor<𝗙> && IsFunctor<𝗚> &&
           std::same_as<typename 𝗙::Σ_cat, typename 𝗚::Σ_cat> &&
           std::same_as<typename 𝗙::Τ_cat, typename 𝗚::Τ_cat>
[[nodiscard]]
constexpr auto operator<<(𝗙&& f, 𝗚&& g) {
  return composite_functor<std::remove_cvref_t<𝗙>, std::remove_cvref_t<𝗚>>{
      std::forward<𝗙>(f), std::forward<𝗚>(g)};
}

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

/**
 * @brief The Boxed Species (The F<T> Context).
 *
 * Reifies the "Box" as a categorical object that announces its
 * own species and shape, enabling 1-parameter functorial discovery.
 */
export template <typename T>
struct Box final {
  /** @brief The physical payload. */
  const T value;

  /** @brief Equality for structural verification in static_asserts. */
  constexpr bool operator==(const Box&) const = default;
};

/**
 * @brief TraceHub: Maps Set -> StringCategory.
 *
 * Instead of 'boxing' a value, this functor 'describes' the mapping.
 * It's perfect for verifying that fmap is called exactly when it should be.
 */
template <typename T>
struct trace_hub {
  using Σ_cat = Set<T>;
  using Τ_cat = StringCategory;

  using Domain = typename Σ_cat::Arrow;
  using Codomain = typename Τ_cat::Arrow;

  template <typename 𝗳>
  constexpr auto φ([[maybe_unused]] 𝗳&& f) const {
    // Use operator+= or explicit construction for every part
    std::string label = "lifted(";
    label += typeid(𝗳).name();
    label += ")";

    return StringArrow{
        .label = std::move(label), .domain_id = 0, .codomain_id = 0};
  }

  template <typename U>
  using Shape = std::string;
};

/**
 * The "Identity Hub" for any category, which simply returns the input arrow
 * as-is. This serves as the canonical identity functor for any category,
 * ensuring that the identity morphism is preserved without alteration.
 *
 * I.e. this is presumably the only functor which is truly *generic* across all
 * categories, since it doesn't rely on any specific structure of the category.
 */
template <typename Cat>
  requires IsCategory<Cat>
struct identity_hub {
  using Σ_cat = Cat;
  using Τ_cat = Cat;

  using Domain = Cat;
  using Codomain = Cat;

  /**
   * @brief Object Mapping: Id(C) = C
   * Satisfies the IsArrow requirement for Hub Arrows.
   */
  constexpr Codomain operator()(const Domain& c) const noexcept { return c; }

  /**
   * @brief φ for Identity: Just return the arrow.
   * We loosen the constraint to accept any valid arrow of the category,
   * satisfying the "Honest" requirement that F(f) = f.
   */
  template <typename 𝗳>
    requires IsArrow<std::remove_cvref_t<𝗳>> &&
             std::same_as<typename std::remove_cvref_t<𝗳>::Domain,
                          typename Cat::Species>
  constexpr auto φ(𝗳&& f) const {
    return std::forward<𝗳>(f);
  }

  // The shape is also generic: Id(A) = A
  template <typename U>
  using Shape = U;
};

static_assert(IsEndofunctor<identity_hub<Set<int>>>,
              "Verification Failed: identity_hub must satisfy IsEndofunctor.");

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

}  // namespace dedekind::category
