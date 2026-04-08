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

/** @section Morphism_Lifting_Proof */
using Negate = std::negate<int>;
using TaggedNegate = Morphism<int, int, Negate>;

// This triggers the 'fmap' discovery via the Monadic Bridge
static_assert(
    IsArrow<decltype(fmap<Box>(TaggedNegate{Negate{}}))>,
    "Skeletal Failure: Failed to derive fmap for Box from its Kleisli Triple.");

/** @section Verification: The Identity Law (F(id_X) = id_F<X>) */

// Proof: Lifting the identity morphism on 'int' gives us an arrow on
// 'Box<int>'.
static_assert(IsArrow<decltype(fmap<Box>(id<int>()))>,
              "Identity Law: fmap(id) must preserve the Boxed species.");

// Proof: The lifted Negate morphism correctly transforms a Boxed value.
static_assert(fmap<Box>(endo<int>(std::negate<int>{}))(Box<int>{42}).value ==
                  -42,
              "Action: fmap(f) must preserve the underlying machine logic.");

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

// Proof: Box is a Functor between the Additive Category of Integers and itself.
static_assert(
    IsFunctor<Box, int, std::plus<int>, int, std::plus<int>>,
    "Level 0 Proof: Box must satisfy the Functor concept for (Z, +).");

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

// 1. Proof: Identity is an Endofunctor on (int, +).
// F=Identity, T=int, Op=plus<int>
static_assert(IsEndofunctor<Box, int, std::plus<int>>,
              "Functor: Identity must be a valid Endofunctor on Z.");

// 2. Proof: Identity is an Endofunctor on the Boolean Lattice (bool, &&).
static_assert(IsEndofunctor<Box, bool, std::logical_and<bool>>,
              "Functor: Identity must be a valid Endofunctor on B.");

// 3. Proof: Identity is an Endofunctor on XOR logic (bool, ^).
static_assert(
    IsEndofunctor<Box, bool, std::bit_xor<bool>>,
    "Functor: Identity must be a valid Endofunctor on the XOR Group.");

/** @brief The Functorial Bridge Tag. */
template <template <typename> typename F>
struct fmap_tag {};

/** @brief Global witness for Box mapping. */
export template <typename T = void>  // Template to keep it header-friendly
constexpr auto Boxed = fmap_tag<Box>{};

/** @brief Postfix Operator: arrow >> Boxed */
export template <typename Arrow, template <typename> typename F>
  requires IsArrow<Arrow, typename Arrow::Domain, typename Arrow::Codomain>
constexpr auto operator>>(Arrow f, fmap_tag<F>) {
  return fmap<F>(f);
}

/** @section The_Into_Tag */
export template <template <typename...> typename F>
struct into_tag {};

export template <template <typename...> typename F>
inline constexpr into_tag<F> into{};

/** @section The_Push_Operator (η) */
export template <typename T, template <typename...> typename F>
constexpr auto operator>>(T&& x, into_tag<F>) {
  // We use the η witness from :species to lift the value
  return η<F, std::decay_t<T>>{}(std::forward<T>(x));
}

/** @section Pipeline_Verification: From Brick to Box */

// The Logic (Brick)
constexpr auto increment = endo<int>([](int x) { return x + 1; });

// The Flow: Value -> Box -> Morphism-on-Box
// Reading: "Take 41, put it INTO a Box, then apply increment BOXED."
static_assert((41 >> into<Box> >> (increment >> Boxed<>)).value == 42,
              "Pipeline: The linear flow from value to boxed result failed.");

// The Composition Flow:
// Reading: "Take 10, put it INTO a Box, then increment it, then negate it."
constexpr auto negate = endo<int>(std::negate<int>{});

static_assert((10 >> into<Box> >> (increment >> Boxed<>) >> (negate >> Boxed<>))
                      .value == -11,
              "Pipeline: Multi-stage functorial composition failed.");

/** @section The_Join_Tag */
export template <template <typename...> typename F>
struct join_tag {};

export template <template <typename...> typename F>
inline constexpr join_tag<F> join{};

/** @section The_Collapse_Operator (μ) */
export template <typename T, template <typename...> typename F>
constexpr auto operator>>(const F<F<T>>& nested, join_tag<F>) {
  // μ(m) = m >>= id
  return nested >>= [](const F<T>& inner) { return inner; };
}

/** @brief Rvalue overload for "High-Speed" Move semantics */
export template <typename T>
constexpr auto operator>>(Box<Box<T>>&& nested_box, join_tag<Box>) {
  return std::move(nested_box.value);
}

// A "Double-Entry" Pipeline:
// 42 >> into >> into -> Box<Box<int>>
// ... then >> join -> Box<int>
static_assert((42 >> into<Box> >> into<Box> >> join<Box>).value == 42,
              "Monad Law: Join must collapse the double-context.");

/**
 * @concept IsNaturalTransformation
 * @theorem Naturality (F ⟹ G)
 * @brief Proposition: There exists a structure-preserving bridge between two
 * Functors.
 *
 * @details
 * For this theorem to hold for a candidate (Eta), the following must be true:
 * 1. Categorical Context: F and G are verified Functors over the Species (T,
 * OpT).
 * 2. Morphism Signature: Eta is a valid Arrow mapping the context F⟨T⟩ to G⟨T⟩.
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
 * @tparam Eta  The candidate proof-object (The transformation type).
 * @tparam F    The source Functor (The origin context).
 * @tparam G    The target Functor (The destination context).
 * @tparam T    The source Species (The object).
 * @tparam U    The target Species (The object).
 * @tparam OpT   The Operation of the Source Category 𝒞.
 * @tparam OpU   The Operation of the Target Category 𝒟.
 */
export template <typename Alpha, template <typename> typename F,
                 template <typename> typename G, typename T, typename U,
                 typename OpT, typename OpU>
concept IsNaturalTransformation =
    IsFunctor<F, T, OpT, T, OpT> && IsFunctor<G, U, OpU, U, OpU> &&
    IsArrow<Alpha, F<T>, G<U>>;

/**
 * @concept IsNaturalEndoTransformation
 * @brief The "Highway" Theorem: F ⟹ G where F, G : 𝒞 → 𝒞.
 * @details Reduces the 7-parameter boilerplate to 5 for the 99% case.
 */
export template <typename Alpha, template <typename> typename F,
                 template <typename> typename G, typename T, typename Op>
concept IsNaturalEndoTransformation =
    IsNaturalTransformation<Alpha, F, G, T, T, Op, Op>;

/**
 * @section Monad_as_Monoid (Explicit Definition)
 * We bridge the gap:
 *   η (Unit)           <--> identity_v (Monoid Unit)
 *   μ (Multiplication) <--> Op         (Monoid Operation)
 */
export template <template <typename> typename F, typename T, typename OpT>
concept IsMonad =
    // 1. Requirement: Must be a Functor (Derived via our Dispatcher)
    IsEndofunctor<F, T, OpT> &&

    // 2. Requirement: Must satisfy the Kleisli Extension System
    IsKleisliExtension<F, T, T> &&

    requires(F<F<T>> nested, T x, F<T> box, std::function<F<T>(T)> f) {
      // 3. Axiomatic Verification:
      // Verification of η (Unit): T -> F<T>
      { η<F, T>{}(x) } -> std::same_as<F<T>>;

      // Verification of >>= (Bind): F<T> -> (T -> F<T>) -> F<T>
      { box >>= f } -> std::same_as<F<T>>;

      // 4. Structural Emergence:
      // μ (Join) is now an observable property of the Kleisli Action.
      // We verify that the "Self-Bind" correctly collapses layers.
      {
        box >>= [](T val) { return η<F, T>{}(val); }
      } -> std::same_as<F<T>>;
    };

/** @section Level_0_Final_Proof: The Box Monad */

// 1. Proof: Box satisfies the formal IsMonad concept for (Z, +)
static_assert(IsMonad<Box, int, std::plus<int>>,
              "Ontology: Box must be recognized as a formal Monad.");

// 2. Action Proof: Join (μ) must collapse the context via the pipe
static_assert((42 >> into<Box> >> into<Box> >> join<Box>) == 42 >> into<Box>,
              "Ontology: The Monadic Join (μ) failed the Action Proof.");

// 3. Action Proof: Unit (η) must lift the species
static_assert((42 >> into<Box>) == Box{42},
              "Ontology: The Monadic Unit (η) failed the Action Proof.");

/**
 * @section Comonadic_Morphisms: Extract (ε) and Duplicate (δ)
 */

/**
 * @concept IsComonad
 * @brief The Unified Comonadic Proof: Co-Kleisli Action as Contextual Being.
 *
 * @details
 * This concept bridges the dual formal definitions:
 * 1. Co-Kleisli Triple (Action): Existence of ε (Extract) and <<= (Extend).
 * 2. Comonoid in Endofunctors (Structure): F is a Functor with δ (Duplicate).
 *
 * @section The_Mereological_Pull
 * Following the Dedekind posture, we do not require δ (Duplicate/Coreturn)
 * to be explicitly defined if <<= (Extend) is present, as δ is the
 * "Self-Extend": δ(w) = w <<= id.
 */
export template <template <typename...> typename F, typename T, typename OpT>
concept IsComonad =
    // 1. Requirement: Must be a Functor (Derived via Discovery)
    IsEndofunctor<F, T, OpT> &&

    // 2. Requirement: Must satisfy the Co-Kleisli Extension System
    IsCoKleisliExtension<F, T, T> &&

    requires(F<T> box, std::function<T(F<T>)> f) {
      // 3. Axiomatic Verification:
      // Verification of ε (Extract): F<T> -> T
      { ε<F, T>{}(box) } -> std::same_as<T>;

      // Verification of <<= (Extend): F<T> -> (F<T> -> T) -> F<T>
      { box <<= f } -> std::same_as<F<T>>;

      // 4. Structural Emergence:
      // δ (Duplicate) is an observable property of the Co-Kleisli Action.
      // We verify that the "Self-Extend" correctly nests the context.
      {
        box <<= [](auto&& w) { return w; }
      } -> std::same_as<F<F<T>>>;
    };

/** @section The_Counit_Tag (ε) */
export template <template <typename...> typename F>
struct extract_tag {};

export template <template <typename...> typename F>
inline constexpr extract_tag<F> extract{};

/** @section The_Co_Multiplication_Tag (δ) */
export template <template <typename...> typename F>
struct duplicate_tag {};

export template <template <typename...> typename F>
inline constexpr duplicate_tag<F> duplicate{};

/** @section The_Pull_Operator (ε) */
export template <typename T, template <typename...> typename F>
constexpr T operator<<(const F<T>& box, extract_tag<F>) {
  // Routes to the ε witness in :species
  return ε<F, T>{}(box);
}

/** @section The_Duplicate_Operator (δ) */
export template <typename T, template <typename...> typename F>
  requires IsCoKleisliExtension<F, T, T>
constexpr auto operator<<(const F<T>& box, duplicate_tag<F>) {
  // δ(w) = w <<= id
  return box <<= [](const F<T>& w) { return w; };
}

/** @section Comonad_Verification: The Slick Highway Proofs */

// 1. The Extract Law (ε): Getting the car out of the Box.
static_assert((42 >> into<Box> << extract<Box>) == 42,
              "Comonad Law: Extract (ε) must recover the raw Species.");

// 2. The Duplicate Law (δ): Making a 'Shadow' Box.
// Instead of that decltype(arrow) mess, we just pipe it.
static_assert((42 >> into<Box> << duplicate<Box>).value.value == 42,
              "Comonad Law: Duplicate (δ) must yield a nested Context.");

// 3. The Co-Unit Law: ext(dup(x)) == x
// Reading: "Take a box, duplicate it, then extract the outer layer."
static_assert((42 >> into<Box> << duplicate<Box> << extract<Box>) ==
                  42 >> into<Box>,
              "Comonad Law: Extract ∘ Duplicate must be an Identity on Boxes.");

}  // namespace dedekind::category
