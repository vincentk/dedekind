/**
 * @file ontology:category.morphisms
 * @brief Level 0: The Algebraic Core (Morphisms, Categories, and Structural
 * Invariants).
 *
 * @section The_Structuralist_Framework
 * This partition defines the categorical primitives that govern the interaction
 * between species. By encoding mathematical structures into C++ concepts, we
 * treat the compiler as a formal verifier for structural integrity.
 *
 * @subsection Categories_and_Monoids
 * In the Dedekind ontology, we primarily interface with 'Small'
 * Categories—those where objects and morphisms are representable as discrete
 * types and functions.
 * - IsSmallCategory : A Monoid viewed as a category with a single object.
 * - IsAbelian       : A category where the composition (Op) is commutative,
 *                     permitting symbolic reordering and optimization.
 *
 * @subsection Morphisms_and_Arrows
 * Morphisms are the "cement" of the ontology, defining how data flows between
 * species while preserving their underlying algebraic laws.
 * - Morphism (Arrow) : A tagged mapping from a Domain (A) to a Codomain (B).
 * - IsInitial       : The "Zero" object (0); the source of the unique morphism
 * to any X.
 * - IsTerminal      : The "One" object (1); the target of the unique morphism
 * from any X.
 *
 * @section The_Action_Axiom
 * To bypass C++ template limitations regarding ADL and partial specialization,
 * morphisms are treated as "Actions." Rather than defining a global 'compose'
 * function, we utilize the internal structure of the species (Kleisli or
 * Co-Kleisli extension systems) to derive higher-order behavior like functors.
 *
 * @section Universal_Zero
 * The ontology provides a 'Zero Morphism' (\ref zero), which represents
 * information absorption. It maps any element of the domain to the identity
 * element of the codomain's underlying monoid.
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:morphism;

import :species;

namespace dedekind::category {

/**
 * @concept IsArrow
 * @brief Verified mapping f: A -> B.
 *
 * Supports three paths:
 * 1. The Morphic Registry (Formal SpeciesTraits)
 * 2. Functional Deduction (Lambdas/Callables via invoke_result)
 * 3. The Universal Bridge (Primitives acting as Identity Arrows)
 */
export template <typename F, typename A, typename B>
concept IsArrow =
    (requires { typename SpeciesTraits<F, A>::Codomain; } &&
     std::convertible_to<typename SpeciesTraits<F, A>::Codomain, B>) ||
    ((std::integral<F> || std::floating_point<F> || std::same_as<F, bool>) &&
     std::same_as<F, A> && std::same_as<A, B>);

/**
 * @section The_Skeletal_Morphism
 * @brief The formal structure of an Arrow f: A -> B.
 */
export template <typename A, typename B, typename Func>
struct Morphism {
  using Domain = A;
  using Codomain = B;
  Func transform;

  // We provide the call operator, but NOT the composition (f ∘ g).
  constexpr B operator()(const A& x) const { return transform(x); }
};

/** @brief Universal inference for any Morphism signature f: Args... -> Codomain
 */
template <typename F, typename... Args>
struct infer_morphism {
  // For binary operators, Domain is the first argument type
  using Domain = std::tuple_element_t<0, std::tuple<Args...>>;
  using Codomain = std::invoke_result_t<F, Args...>;
};

// 1. Unary Discovery (e.g. std::identity)
template <typename F, typename A>
  requires requires(F f, A x) { f(x); }
struct SpeciesTraits<F, A> : infer_morphism<F, A> {};

// 2. Binary Discovery (e.g. std::plus)
template <typename F, typename A>
  requires requires(F f, A x) { f(x, x); }
struct SpeciesTraits<F, A> : infer_morphism<F, A, A> {};

/** @section Logical_Species (bool) */
template <>
struct SpeciesTraits<std::logical_and<bool>>
    : infer_morphism<std::logical_and<bool>, bool, bool> {};
template <>
struct SpeciesTraits<std::logical_or<bool>>
    : infer_morphism<std::logical_or<bool>, bool, bool> {};
template <>
struct SpeciesTraits<std::equal_to<bool>>
    : infer_morphism<std::equal_to<bool>, bool, bool> {};

/** @section Integral_Species (uint/int) */
template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::plus<T>> : infer_morphism<std::plus<T>, T, T> {};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::minus<T>> : infer_morphism<std::minus<T>, T, T> {};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::multiplies<T>>
    : infer_morphism<std::multiplies<T>, T, T> {};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::divides<T>> : infer_morphism<std::divides<T>, T, T> {
};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::bit_and<T>> : infer_morphism<std::bit_and<T>, T, T> {
};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::bit_or<T>> : infer_morphism<std::bit_or<T>, T, T> {};

/** @section Relational_Morphisms (Subobject Classifiers) */
template <typename T>
struct SpeciesTraits<std::less_equal<T>>
    : infer_morphism<std::less_equal<T>, T, T> {};

template <typename T>
struct SpeciesTraits<std::less<T>> : infer_morphism<std::less<T>, T, T> {};

static_assert(
    IsArrow<Morphism<int, bool, std::function<bool(int)>>, int, bool>,
    "Taxonomy Error: Morphism must satisfy the skeletal IsArrow concept.");

// 2. Verify "Inferred Arrows" (Inference Engine)
// Testing std::plus<int>: int x int -> int
static_assert(
    IsArrow<std::plus<int>, int, int>,
    "Inference Error: std::plus<int> should be an Arrow from int to int.");

// Testing Relational Morphisms: double x double -> bool
static_assert(IsArrow<std::less_equal<double>, double, bool>,
              "Inference Error: std::less_equal<double> should be an Arrow "
              "from double to bool.");

// 3. Verify "Lambdas" (Functional Auto-Discovery)
auto logic_gate = [](int x) -> bool { return x > 0; };
static_assert(
    IsArrow<decltype(logic_gate), int, bool>,
    "Inference Error: Lambda was not automatically discovered as an Arrow.");

// 4. Verify "The Universal Bridge" (Primitives as Identity Arrows)
// Allows using a raw '5' as an identity mapping in algebraic expressions
static_assert(
    IsArrow<int, int, int>,
    "Bridge Error: Primitive int should act as an identity Arrow for itself.");
static_assert(IsArrow<double, double, double>,
              "Bridge Error: Primitive double should act as an identity Arrow "
              "for itself.");

// 5. Verify "Morphism Struct" (Reification)
using IntToBool = Morphism<int, bool, std::function<bool(int)>>;
static_assert(IsArrow<IntToBool, int, bool>,
              "Taxonomy Error: Formal Morphism struct failed IsArrow check.");

/** @section Morphism_Proof */
static_assert(
    IsArrow<Morphism<int, bool, std::function<bool(int)>>, int, bool>,
    "Taxonomy Error: Morphism must satisfy the skeletal IsArrow concept.");

/**
 * @concept IsEndomorphism
 * @brief Proposition: A Morphism where Domain ≡ Codomain (f: A -> A).
 */
export template <typename F, typename T>
concept IsEndomorphism = IsArrow<F, T, T>;

static_assert(
    IsEndomorphism<std::plus<int>, int>,
    "Taxonomy Error: std::plus<int> must be recognized as an Endomorphism.");

/**
 * @struct Rule
 * @brief A type-erased Morphism A -> B between Species.
 * @details This wrapper reifies a functional rule into a formal
 *          Categorical Arrow, preventing C-style pointer decay.
 *
 * @tparam A The Domain Species (The Source).
 * @tparam B The Codomain Species (The Target).
 */
export template <IsSpecies A, IsSpecies B>
struct Rule {
  using Domain = A;
  using Codomain = B;

  // We use std::function to erase the lambda's unique type
  // and provide a stable "Morphic Identity."
  std::function<B(const A&)> apply;

  /** @brief The Morphic Application (The Arrow's Action) */
  constexpr B operator()(const A& x) const { return apply(x); }
};

/** @section Rule_Overloads */
// Specifically target Rule to kill the 'bool' ambiguity and fix deduction.
export template <typename A, typename B>
auto operator&&(const Rule<A, B>& p1, const Rule<A, B>& p2) {
  return Rule<A, B>{[p1, p2](const A& x) {
    using S = typename SpeciesTraits<B>::species;
    return B{S::AND(p1(x).value, p2(x).value)};
  }};
}

export template <typename A, typename B>
auto operator||(const Rule<A, B>& p1, const Rule<A, B>& p2) {
  return Rule<A, B>{[p1, p2](const A& x) {
    using S = typename SpeciesTraits<B>::species;
    return B{S::OR(p1(x).value, p2(x).value)};
  }};
}

/** @section The Morphism Factory: arrow <A, B> (f) */
export template <typename A, typename B, typename F>
constexpr auto arrow(F&& f) {
  return Morphism<A, B, std::decay_t<F>>{std::forward<F>(f)};
}

/** @brief The Endomorphism Factory: endo<A>(f) */
export template <typename A, typename F>
constexpr auto endo(F&& f) {
  return arrow<A, A>(std::forward<F>(f));
}

/** @section Arrow Factory Verification: Tagging & Species Integrity */

// 4. Action Proof: The tagged arrow preserves the underlying action.
// We verify that the factory-produced morphism actually executes.
static_assert(endo<int>([](int x) { return x * 2; })(21) == 42,
              "Arrow Factory: Action check failed for anonymous lambda.");

/** @section The_Universal_Functor_Interface */

/** @brief The Identity Morphism: The "Zero-Length Highway" for Species T. */
export template <typename T>
struct Identity final {
  using Domain = T;
  using Codomain = T;

  // The logic is baked into the type itself.
  constexpr T operator()(const T& x) const noexcept { return x; }
};

/** @brief The Identity Factory: Returns the neutral arrow for Species A. */
export template <typename A>
constexpr auto id() {
  return Identity<A>{};
}

/** @section Morphism_Lifting_Proof */
using Negate = std::negate<int>;
using TaggedNegate = Morphism<int, int, Negate>;

constexpr auto f_neg = endo<int>(std::negate<int>{});
constexpr auto identity_int = id<int>();

/** @section Identity Verification */
static_assert(IsArrow<decltype(id<bool>()), bool, bool>,
              "The id<A>() factory must produce a valid Arrow.");

// 1. Right Identity: f(id(x)) == f(x)
static_assert(f_neg(identity_int(42)) == f_neg(42),
              "Unit Law: f ∘ id_A must equal f.");

// 2. Left Identity: id(f(x)) == f(x)
static_assert(identity_int(f_neg(42)) == f_neg(42),
              "Unit Law: id_B ∘ f must equal f.");

/** @section Lifting Traits to the Identity Functor */

// 1. If T is associative under Op, Identity<T> is associative.
template <typename T, typename Op>
inline constexpr bool is_associative_v<Identity<T>, Op> =
    is_associative_v<T, Op>;

// 2. If T is commutative under Op, Identity<T> is commutative.
template <typename T, typename Op>
inline constexpr bool is_commutative_v<Identity<T>, Op> =
    is_commutative_v<T, Op>;

/**
 * @section Lifting Traits: Morphism Identity
 * The identity element of the Identity Morphism under composition
 * is the Identity Morphism itself (id ∘ id = id).
 */
template <typename T, typename Op>
inline constexpr Identity<T> identity_v<Identity<T>, Op> = Identity<T>{};

/**
 * @section Categorical Composition (Explicitly Typed)
 * @brief Synthesizes an arrow A -> C from A -> B and B -> C.
 * @details By requiring A, B, and C as template parameters, we ensure
 *          the composition is a statically verified bridge.
 */
export template <typename F, typename G>
  requires requires {
    typename std::decay_t<F>::Domain;
    typename std::decay_t<F>::Codomain;
    typename std::decay_t<G>::Domain;
    typename std::decay_t<G>::Codomain;
    // The strict Categorical Identity:
    requires std::same_as<typename std::decay_t<F>::Codomain,
                          typename std::decay_t<G>::Domain>;
  }
constexpr auto operator>>(F&& f, G&& g) {
  using F_pure = std::decay_t<F>;
  using G_pure = std::decay_t<G>;

  using A = typename F_pure::Domain;
  using C = typename G_pure::Codomain;

  // Note: The lambda is implicitly constexpr in C++23 if possible
  return arrow<A, C>([f = std::forward<F>(f), g = std::forward<G>(g)](
                         A x) constexpr { return g(f(std::move(x))); });
}

/** @section Categorical Verification: The Unit Laws (f ∘ id = f = id ∘ f) */

// 1. Proof: Morphism Identity (Existence & Tagging)
// We verify that id exists and chains with tagged morphisms.
static_assert(
    IsArrow<decltype(id<int>() >> arrow<int, int>(std::negate<int>{})), int,
            int>,
    "Unit Law: id_A must be a left-identity for morphisms out of A.");

static_assert(
    IsArrow<decltype(endo<int>(std::negate<int>{}) >> id<int>()), int, int>,
    "Unit Law: id_B must be a right-identity for morphisms into B.");

// 2. Proof: Cross-Species Identity
// id_Z combined with a Z -> B bridge must result in a Z -> B bridge
/** @section Cross_Species_Proof: Z -> B */
static_assert(
    IsArrow<decltype(arrow<int, bool>([](int x) { return x > 0; })), int, bool>,
    "Skeletal Failure: Failed to construct an anonymous cross-species "
    "Morphism.");

// 3. Proof: Extensional Equality (The Action)
// The composite morphism (f ∘ id) must yield the same value as f.
static_assert((endo<int>(std::negate<int>{}) >> id<int>())(42) == -42,
              "Action Proof: Composition with id must be value-invariant.");

// 4. Proof: Categorical Unity (id ∘ id = id)
static_assert(
    IsArrow<decltype(id<int>() >> id<int>()), int, int>,
    "Unity: id composed with itself must remain the Identity Morphism.");

// 5. Negative Proof: Species Safety (The Broken Bridge)
// This should fail to compile if you uncomment it, because bool != int.
// static_assert(!IsArrow<decltype(id<bool>() >> arrow<int,
// int>(std::negate<int>{})), int, int>);

/**
 * @concept IsIsomorphism
 * @brief An Arrow f: A -> B with a guaranteed Inverse g: B -> A.
 * @details Represents a reversible morphism. In Level 0, we verify
 *          the structural existence of the 'Undo' path.
 */
export template <typename F, typename A, typename B>
concept IsIsomorphism = IsArrow<F, A, B> && requires(F f) {
  // We probe the global/partition 'inverse' bridge for this specific Morphism.
  { inverse(f) } -> IsArrow<B, A>;
};

/** @brief The structural inverse of an Identity is itself. */
template <typename T>
[[nodiscard]] constexpr auto inverse(Identity<T> id) noexcept {
  return id;
}

// Proof: id_int is an isomorphism from int to int.
static_assert(IsIsomorphism<Identity<int>, int, int>,
              "Identity must be a self-inverse isomorphism.");

// Proof: id_bool is an isomorphism from bool to bool.
static_assert(IsIsomorphism<Identity<bool>, bool, bool>,
              "Identity must be a self-inverse isomorphism.");

/** @brief The structural inverse of Negation is itself (Involutive). */
template <typename A, typename B, typename Impl>
  requires std::same_as<Impl, std::negate<A>>
[[nodiscard]] constexpr auto inverse(Morphism<A, B, Impl> f) noexcept {
  return f;  // Negate is its own inverse
}

// Proof: Tagged Negation is a formal Isomorphism.
static_assert(IsIsomorphism<TaggedNegate, int, int>,
              "Negation must be recognized as a reversible Morphism.");
/**
 * @section The Action Bridge (Value >> Arrow)
 * @brief Proposition: A Value x can be piped into a Morphism f: A -> B.
 * @details This is the terminal step of a Highway pipeline. It maps the
 *          Species-level data into the Codomain result.
 *
 * @tparam T     The Input Value type (The 'Car').
 * @tparam Arrow The Morphism type (The 'Highway').
 *
 * @note Syntactic Sugar: x >> f ≡ f(x).
 */
export template <typename T, typename Arrow>
  requires IsArrow<Arrow, typename Arrow::Domain, typename Arrow::Codomain> &&
           std::convertible_to<T, typename Arrow::Domain>
constexpr auto operator>>(T&& value, const Arrow& f) ->
    typename Arrow::Codomain {
  return f(std::forward<T>(value));
}

}  // namespace dedekind::category
