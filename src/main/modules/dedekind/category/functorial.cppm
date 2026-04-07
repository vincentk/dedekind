/**
 * @file ontology:category.cppm
 * @brief Level 0: The Skeletal Foundation (Algebraic Bricks and Categorical
 * Cement).

 * @subsection The_Cement: Categorical Morphisms
 * Primitives are bound by morphisms that preserve structural invariants:
 * - IsFunctor     : A structure-preserving mapping between categories.
 * - IsMonad       : A mechanism for lifting and chaining species (The Push).
 * - IsComonad     : A mechanism for sampling and extending contexts (The Pull).
 * - IsEmbedding   : A proof of injective (1:1) type promotion.
 *
 * @section The_Bootstrapping_Strategy: Action-First Derivation
 * To resolve the circular dependency between Functors and Monads, Dedekind
 * implements an "Action-First" bootstrapping strategy.
 *
 * @note Technical Implementation Constraints:
 * In textbook Category Theory, a Monad is a Monoid in the category of
 * Endofunctors. However, C++ language constraints necessitate a divergence to
 * Kleisli Triples:
 * 1. Partial Specialization: C++ forbids partial specialization of function
 *    templates (e.g., fmap<Box>), preventing a centralized definition.
 * 2. ADL Resolution: Function templates called via name-lookup (fmap<F>) cannot
 *    trigger Argument-Dependent Lookup.
 *
 * By defining Monads/Comonads as Extension Systems (η/ε + >>= / <<=), we
 * utilize operator overloading on concrete objects. This triggers ADL, allowing
 * Level 0 to instantiate a derived 'fmap' without prior knowledge of Level 1
 * species.
 *
 * - Monadic Discovery   : fmap(f) is derived as: m >>= (η ∘ f).
 * - Comonadic Discovery : fmap(f) is derived as: w <<= (f ∘ ε).
 *
 * @section The_Highway_Notation: Directional Vectors
 * Operators represent the flow of data across the ontology:
 * - value >> into<>    : η (Unit) - Lifting a species into a context.
 * - box   << extract<> : ε (Counit) - Sampling a species from a context.
 * - box   >>= f        : Bind - Monadic composition (Left-to-Right).
 * - box   <<= f        : Extend - Comonadic composition (Right-to-Left /
 * Contextual).
 *
 * @section Structural_Inference
 * Compile-time exhaustive proofs (e.g., Bool-to-Int embedding) guarantee
 * the security of the skeletal layer before higher-level complex species
 * are initialized.
 */

module;

#include <concepts>
#include <functional>

export module dedekind.category:functorial;

import :kleisli;
import :morphism;
import :species;

namespace dedekind::category {

/**
 * @section The_Unified_Highway_Bridge (The Discovery Dispatcher)
 * @brief Automates the derivation of fmap from the species' extension system.
 *
 * @details
 * In the Dedekind ontology, a Functor is not a "primitive" but an
 * "epi-phenomenon" derived from the underlying Monadic or Comonadic
 * structure. This dispatcher resolves the Bootstrapping Paradox:
 * 1. It prioritises the Monadic Push (Kleisli: m >>= η ∘ f).
 * 2. It falls back to the Comonadic Pull (Co-Kleisli: w <<= f ∘ ε).
 * 3. It provides a formal Proof of Absence if neither structure is present.
 *
 * @note Architectural Choice:
 * We utilize 'if constexpr' dispatch instead of multiple overloads to:
 * - Eliminate overload ambiguity in the C++ template system.
 * - Centralise the diagnostic logic for structural failures.
 * - Ensure zero-overhead selection of the most efficient highway.
 */
export template <template <typename...> typename F, typename Arrow,
                 typename T = typename Arrow::Domain,
                 typename U = typename Arrow::Codomain>
  requires IsArrow<Arrow, T, U>
constexpr IsArrow<F<T>, F<U>> auto fmap(Arrow f) {
  // 1. Priority: Identity Functor (The Invisible Box)
  if constexpr (std::same_as<F<T>, T>) {
    return f;
  } else if constexpr (IsKleisliExtension<F, T, U>) {
    /** @theorem fmap(f) = m >>= (η ∘ f) */
    return arrow<F<T>, F<U>>([f](const F<T>& m) {
      return m >>= [f](const T& x) { return η<F, U>{}(f(x)); };
    });
  } else if constexpr (IsCoKleisliExtension<F, T, U>) {
    /** @theorem fmap(f) = w <<= (f ∘ ε) */
    return arrow<F<T>, F<U>>([f](const F<T>& w) {
      return w <<= [f](const F<T>& ctx) { return f(ε<F, T>{}(ctx)); };
    });
  } else {
    /** @section The_Controlled_Explosion */
    struct Discovery_Failure {
      static_assert(sizeof(T) == 0,
                    "Ontology Error: Species lacks both Kleisli (>>=) and "
                    "Co-Kleisli (<<=) structures. "
                    "A Functorial mapping cannot be derived for this context.");
    };
    return Discovery_Failure{};
  }
}

/** @section Morphism_Lifting_Proof */
using Negate = std::negate<int>;
using TaggedNegate = Morphism<int, int, Negate>;

// This triggers the 'fmap' discovery via the Monadic Bridge
static_assert(
    IsArrow<decltype(fmap<Box>(TaggedNegate{Negate{}})), Box<int>, Box<int>>,
    "Skeletal Failure: Failed to derive fmap for Box from its Kleisli Triple.");

/** @section Verification: The Identity Law (F(id_X) = id_F<X>) */

// Proof: Lifting the identity morphism on 'int' gives us an arrow on
// 'Box<int>'.
static_assert(IsArrow<decltype(fmap<Box>(id<int>())), Box<int>, Box<int>>,
              "Identity Law: fmap(id) must preserve the Boxed species.");

// Proof: The lifted Negate morphism correctly transforms a Boxed value.
static_assert(fmap<Box>(endo<int>(std::negate<int>{}))(Box<int>{42}).value ==
                  -42,
              "Action: fmap(f) must preserve the underlying machine logic.");

/**
 * @concept IsFunctor
 * @brief A structure-preserving mapping between two Categories 𝒞 and 𝒟.
 *
 * @details
 * Formally, a Functor F : 𝒞 → 𝒟 consists of:
 *   - Object Mapping: For every object X ∈ 𝒞, an object F⟨X⟩ ∈ 𝒟.
 *   - Morphism Mapping: For every arrow f : X → Y, an arrow F(f) : F⟨X⟩ → F⟨Y⟩.
 *
 * It must satisfy the Functor Laws:
 *   1. Identity Preservation: F(id_X) = id_F⟨X⟩
 *   2. Composition Preservation: F(g ∘ f) = F(g) ∘ F(f)
 *
 * @tparam F   The Type-Morphism (The "Box" or "Transformer").
 * @tparam 𝒯   The Object in the Source Category 𝒞.
 * @tparam Op𝒯 The Composition Rule (Morphism) in 𝒞.
 * @tparam 𝒰   The Object in the Target Category 𝒟.
 * @tparam Op𝒰 The Composition Rule (Morphism) in 𝒟.
 */
export template <template <typename> typename F, typename T, typename OpT,
                 typename U, typename OpU>
concept IsFunctor = IsSmallCategory<T, OpT> && IsSmallCategory<U, OpU> &&
                    requires(Identity<T> id_t) {
                      // 1. Object Mapping: Does the Species T exist in the Box
                      // F?
                      typename F<T>;

                      // 2. Morphism Mapping: F(f: T -> T) must yield an arrow
                      // F<T> -> F<T>. This 'lift' stays within the Functor's
                      // context F.
                      { fmap<F>(id_t) } -> IsArrow<F<T>, F<T>>;
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
 * @tparam F   The Type-Morphism (The "Box" or "Transformer").
 * @tparam 𝒯   The Object in the Category 𝒞.
 * @tparam Op𝒯 The Composition Rule (Morphism) in 𝒞.
 */
export template <template <typename> typename F, typename 𝒯, typename Op𝒯>
concept IsEndofunctor = IsFunctor<F, 𝒯, Op𝒯, 𝒯, Op𝒯>;

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
