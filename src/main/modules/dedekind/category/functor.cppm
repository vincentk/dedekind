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
 * @section The_Honest_Spine Hub (Intensional Law) vs. Spoke (Extensional
 * Matter)
 *
 * With a view to interoperability with primitive types and the std::library,
 * Dedekind distinguishes between the mathematical rule of transformation
 * and the data instances that undergo it. This separation ensures that
 * categorical laws are verified at the structural level before they are
 * applied to values.
 *
 * 1. The Hub (Intensional Hub / Law):
 *    Represented by @ref IsFunctor, this is a stateless blueprint acting
 *    as a 1-morphism between categories. It owns the "Morphic Action" (φ)—the
 *    mathematical recipe for lifting arrows. It requires handles to its
 *    @ref Σ_cat and @ref Τ_cat to verify structural integrity (Identity and
 *    Composition preservation). Without these handles, a mapping is a
 *    mere function; with them, it becomes a verified Functor.
 *
 * 2. The Spoke (Extensional Spoke / Matter):
 *    This is the concrete instance—an ordinary arrow (@ref IsArrow) or a data
 *    container (e.g., Box<T>). While the Hub provides the "How," the Spoke
 *    provides the "What." The spoke is a resident of a category, but it
 *    does not own the mapping logic itself.
 *
 * 3. The Applicator (The Bridge):
 *    To bridge the Intensional and the Extensional, we use the @ref fmap
 *    factory. This binds a Hub to a Spoke, creating a **Functorial
 * Applicator**. This witness allows the "Handy" fish operators (>> and <<) to
 * resolve unambiguously by utilizing the Hub's categorical authority.
 *
 * @section IsFunctor_Models Concrete IsFunctor Models In This Partition
 *
 * The following types in this partition model @ref IsFunctor:
 * - @ref identity_functor: Cat -> Cat.
 * - @ref box_functor: Set<T> -> Set<Box<T>>.
 * - @ref maybe_functor: Set<T> -> Set<Maybe<T>>.
 * - @ref trace_functor: Set<T> -> StringCategory.
 * - @ref composite_functor: composition G . F for any composable functors.
 *
 * Notes:
 * - @ref maybe_functor is a concrete @ref IsFunctor model in this partition,
 *   but under the current category choices it is not an @ref IsEndofunctor
 *   witness for @ref IsMonad (see `:monad` for the textbook constraint).
 * - Value-level overloads of φ for Maybe/Identity/Box at the end of this file
 *   are lifting utilities, not IsFunctor hub models by themselves.
 *
 */

module;

#include <concepts>
#include <functional>
#include <optional>
#include <string>

export module dedekind.category:functor;

import :action;
import :cartesian;
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
 *
 * Canonical hub models provided in this partition are listed in
 * @ref IsFunctor_Models.
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
 * @concept IsMorphicApplicator
 * @brief The stage-2 engine that has the data and waits for the action.
 */
template <typename T, typename Hub, typename Spoke>
concept IsMorphicApplicator = requires(T engine, typename Hub::Σ_cat::Arrow f) {
  /**
   * The engine accepts a "Spoke-Arrow" and returns the result.
   * We check that the result type is exactly the Target Category's Species.
   */
  { engine(f) } -> std::same_as<typename Hub::Τ_cat::Species>;
};

/**
 * @concept IsFunctorialApplicator
 * @brief The interface of the "Contextual Wrapper" returned by fmap(Hub).
 */
export template <typename T, typename Hub>
concept IsFunctorialApplicator =
    requires(T applicator, typename Hub::Σ_cat::Species ma) {
      /**
       * 1. Data Binding
       * The applicator must accept a Spoke (ma) from the Hub's source category.
       * This returns the "Morphic Applicator" (Stage 2).
       */
      { applicator(ma) } -> IsMorphicApplicator<Hub, decltype(ma)>;
    };

/**
 * @brief Stage 2: The Morphic Engine.
 * It has the Law and the Matter; it just needs the Action.
 */
template <typename Hub, typename Spoke>
struct morphic_engine {
  Hub h;
  Spoke ma;

  template <typename Func>
  constexpr auto operator()(Func&& f) const {
    // 1. Ensure we have an Arrow (lifts lambdas if necessary)
    auto lifted_arrow = h.φ(arrow(std::forward<Func>(f)));

    /**
     * 2. The Crucial Step:
     * Your Hub knows the Target Category (Τ_cat).
     * We wrap the raw data 'ma' into that category's Species (e.g., Box<int>).
     */
    using TargetSpecies = typename Hub::Τ_cat::Species;

    // Pass the 'Boxed' version of ma to the lifted morphism
    return lifted_arrow(TargetSpecies{ma});
  }
};

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
 * @brief The Intensional Hub for Boxed values.
 * @details Concrete @ref IsFunctor model. Acts as an endofunctor
 * Set<T> -> Set<Box<T>>.
 */
export template <typename T>
struct box_functor {
  using ArrowKind = hub_arrow_tag;
  using Σ_cat = Set<T>;
  using Τ_cat = Set<Box<T>>;

  // Requirement for IsArrow (Hub as 1-morphism in Cat)
  using Domain = Σ_cat;
  using Codomain = Τ_cat;

  /** @brief F_obj: The Type Constructor */
  template <typename U>
  using Shape = Box<U>;

  /** @brief F_mor (φ): The Morphic Lift */
  template <typename 𝗳>
    requires IsArrow<std::remove_cvref_t<𝗳>>
  constexpr auto φ(𝗳&& f) const {
    // We return an Arrow in the Target Category (Set<Box<T>>)
    return arrow([f = std::forward<𝗳>(f)](Box<T> const& b) {
      return Box{std::invoke(f, b.value)};
    });
  }

  // Action on Objects
  constexpr Τ_cat operator()(const Σ_cat&) const noexcept { return {}; }
};

/**
 * @brief The Intensional Functor for Optional (Maybe) values.
 * @details Concrete @ref IsFunctor model implementing
 * Set<T> -> Set<std::optional<T>>.
 */
export template <typename T>
struct maybe_functor {
  using ArrowKind = hub_arrow_tag;
  using Σ_cat = Set<T>;
  using Τ_cat = Set<std::optional<T>>;

  using Domain = Σ_cat;
  using Codomain = Τ_cat;

  template <typename U>
  using Shape = std::optional<U>;

  /** @brief φ: The Morphic Lift with Short-Circuiting Law */
  template <typename 𝗳>
    requires IsArrow<std::remove_cvref_t<𝗳>>
  constexpr auto φ(𝗳&& f) const {
    return arrow([f = std::forward<𝗳>(f)](std::optional<T> const& m) {
      // The Law: If the input is empty, the result remains empty.
      return m.has_value() ? std::optional{std::invoke(f, *m)} : std::nullopt;
    });
  }

  constexpr Τ_cat operator()(const Σ_cat&) const noexcept { return {}; }
};

static_assert(IsFunctor<maybe_functor<int>>,
              "Verification Failed: maybe_functor must satisfy IsFunctor.");

static_assert(IsMorphicApplicator<morphic_engine<box_functor<int>, int>,
                                  box_functor<int>, int>,
              "Structural Integrity Failed: morphic_engine must satisfy "
              "IsMorphicApplicator.");

/**
 * @brief Stage 1: The Functorial Applicator.
 * It has the Law; it just needs the Matter.
 */
template <typename Hub>
struct functor_applicator {
  Hub h;

  template <typename Spoke>
  constexpr auto operator()(Spoke&& ma) const {
    return morphic_engine<Hub, std::decay_t<Spoke>>{h, std::forward<Spoke>(ma)};
  }
};

static_assert(IsFunctorialApplicator<functor_applicator<box_functor<int>>,
                                     box_functor<int>>,
              "Structural Integrity Failed: functor_applicator must satisfy "
              "IsFunctorialApplicator.");

// Now fmap is just a factory for the Applicator
template <typename Hub>
  requires IsFunctor<Hub>
[[nodiscard]] constexpr auto fmap(Hub const& h) {
  return functor_applicator<Hub>{h};
}

/**
 * @brief A Functorial Context (The "Fishy" Spoke).
 * Holds both the Law (Hub) and the Matter (Spoke).
 */
template <typename Hub, typename Spoke>
struct context final {
  Hub hub;      // The "Instruction Manual" (can have state!)
  Spoke value;  // The "Matter"

  template <typename Func>
  constexpr auto operator>>(Func&& f) const {
    // We use the specific instance of the hub, preserving its state
    return fmap(hub)(value)(std::forward<Func>(f));
  }
};

/**
 * @brief 1-Argument Factory: Bind data to a specific Hub instance.
 * Creates a @ref context that pairs a functor hub with its data spoke.
 */
export template <typename Hub, typename Spoke>
constexpr auto immerse(Hub&& h, Spoke&& s) {
  return context<std::decay_t<Hub>, std::decay_t<Spoke>>{
      std::forward<Hub>(h), std::forward<Spoke>(s)};
}

/**
 * @brief Hub Action Fish: apply a functor hub to a spoke arrow.
 *
 * @details This keeps fish syntax at the hub level for general functors:
 *   F >> f
 * where F is a verified functor and f is an arrow in F::Σ_cat.
 */
export template <typename Hub, typename Arrow>
  requires IsFunctor<Hub> && IsSpokeArrow<std::remove_cvref_t<Arrow>>
[[nodiscard]]
constexpr auto operator>>(Hub const& hub, Arrow&& f) {
  return hub.φ(std::forward<Arrow>(f));
}

/**
 * @brief Composite Functor G . F (Maps CatS -> CatT -> CatU).
 *
 * Verifies that the target of the first functor matches the source
 * of the second, maintaining the structural spine.
 * This is a concrete @ref IsFunctor model whenever F and G are composable
 * functors.
 */
export template <IsFunctor F, IsFunctor G>
  requires std::same_as<typename F::Τ_cat, typename G::Σ_cat>
struct composite_functor {
  using ArrowKind = hub_arrow_tag;
  F first{};
  G second{};

  using Σ_cat = typename F::Σ_cat;
  using Τ_cat = typename G::Τ_cat;

  using Domain = typename F::Domain;
  using Codomain = typename G::Codomain;

  template <typename U>
  using Shape = typename G::template Shape<typename F::template Shape<U>>;

  /**
   * @brief φ(f) = G.φ(F.φ(f))
   * Chaining the functorial lift.
   */
  template <typename A>
    requires IsArrow<std::remove_cvref_t<A>>
  constexpr auto φ(A&& f) const {
    // First lift through F, then through G.
    return second.φ(first.φ(std::forward<A>(f)));
  }

  /** @brief Object Mapping: (G . F)(c) = G(F(c)) */
  constexpr Codomain operator()(const Domain& c) const {
    return second(first(c));
  }
};

// f >> g  =>  g ∘ f      [Canonical Functorial Fish]
export template <typename 𝗙, typename 𝗚>
  requires IsFunctor<𝗙> && IsFunctor<𝗚> &&
           std::same_as<typename std::remove_cvref_t<𝗙>::Τ_cat,
                        typename std::remove_cvref_t<𝗚>::Σ_cat>
[[nodiscard]]
constexpr auto operator>>(𝗙&& f, 𝗚&& g) {
  return composite_functor<std::remove_cvref_t<𝗙>, std::remove_cvref_t<𝗚>>{
      std::forward<𝗙>(f), std::forward<𝗚>(g)};
}

/**
 * @brief The "Upstream Fish" (Optional Composition Sugar)
 * g << f  =>  g ∘ f
 */
export template <typename 𝗙, typename 𝗚>
  requires IsFunctor<𝗙> && IsFunctor<𝗚> &&
           std::same_as<typename std::remove_cvref_t<𝗚>::Τ_cat,
                        typename std::remove_cvref_t<𝗙>::Σ_cat>
[[nodiscard]]
constexpr auto operator<<(𝗙&& f, 𝗚&& g) {
  return composite_functor<std::remove_cvref_t<𝗚>, std::remove_cvref_t<𝗙>>{
      std::forward<𝗚>(g), std::forward<𝗙>(f)};
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
 * @brief TraceFunctor: Maps Set -> StringCategory.
 *
 * Instead of 'boxing' a value, this functor 'describes' the mapping.
 * It's perfect for verifying that fmap is called exactly when it should be.
 * Concrete @ref IsFunctor model.
 */
export template <typename T>
struct trace_functor {
  using ArrowKind = hub_arrow_tag;
  using Σ_cat = Set<T>;
  using Τ_cat = StringCategory;

  using Domain = Σ_cat;
  using Codomain = Τ_cat;

  template <typename 𝗳>
    requires IsArrow<std::remove_cvref_t<𝗳>>
  constexpr auto φ([[maybe_unused]] 𝗳&& f) const {
    return StringArrow{.label = "lifted", .domain_id = 0, .codomain_id = 0};
  }

  template <typename U>
  using Shape = int;

  constexpr Τ_cat operator()(const Σ_cat&) const noexcept { return {}; }
};

static_assert(IsFunctor<trace_functor<int>>,
              "Verification Failed: trace_functor must satisfy IsFunctor.");

/**
 * The "Identity Hub" for any category, which simply returns the input arrow
 * as-is. This serves as the canonical identity functor for any category,
 * ensuring that the identity morphism is preserved without alteration.
 *
 * I.e. this is presumably the only functor which is truly *generic* across all
 * categories, since it doesn't rely on any specific structure of the category.
 * Concrete @ref IsFunctor model.
 */
export template <typename Cat>
  requires IsCategory<Cat>
struct identity_functor {
  using ArrowKind = hub_arrow_tag;
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

static_assert(
    IsEndofunctor<identity_functor<Set<int>>>,
    "Verification Failed: identity_functor must satisfy IsEndofunctor.");

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
