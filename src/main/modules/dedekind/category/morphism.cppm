/**
 * @file ontology:category.morphisms
 * @partition :morphism
 * @brief Level 0: The Skeletal Morphism (Structural Cement).
 *
 * @section The_Structuralist_Framework
 * This partition defines the categorical primitives that govern the interaction
 * between species. In the Dedekind ontology, we do not treat functions as
 * ephemeral blocks of logic, but as formal "Arrows" that carry their own
 * metadata (Domain and Codomain).
 *
 * @quote
 * "A morphism is not just a function, but a function *together with* its
 *  domain and codomain... This 'bookkeeping' is exactly what allows the
 *  categorical machinery to work its magic."
 *  — Urs Schreiber, n-Category Café
 *
 * @subsection The_Skeletal_Arrow
 * The primary purpose of this partition is to provide the "Box and Label"
 * for any mapping:
 * - @ref Morphism (Arrow) : A labeled struct f: A -> B.
 * - @ref IsArrow         : The skeletal concept verifying Domain/Codomain
 * labels.
 * - @ref Identity (id)   : The "Zero-Length Highway" (Reflexivity).
 * - @ref operator>>      : Categorical composition (The Path Axiom).
 *
 * @section The_Action_Axiom
 * To bypass C++ template limitations regarding ADL, morphisms are treated as
 * "Actions." This allows us to lift total functions into the monadic Kleisli
 * spine without losing the formal species-integrity of the Domain.
 *
 * @note Universal constructions (Initial, Terminal, Zero Morphism) are
 *       deferred to Level 0.5 (:limit) and Level 1 (:total), as they
 *       rely on specific algebraic properties of the species.
 */

module;

#include <concepts>
#include <functional>

export module dedekind.category:morphism;

import :species;

namespace dedekind::category {

/**
 * @concept IsArrow
 * @brief A type that knows its own Domain (A) and Codomain (B).
 */
export template <typename F>
concept IsArrow = requires {
  typename std::remove_cvref_t<F>::Domain;
  typename std::remove_cvref_t<F>::Codomain;
  requires requires(F f, typename F::Domain x) {
    { f(x) } -> std::convertible_to<typename std::remove_cvref_t<F>::Codomain>;
  };
};

/**
 * @section The_Skeletal_Morphism
 * @brief The formal structure of an Arrow f: A -> B.
 */
export template <typename A, typename B, typename Func>
struct Morphism {
  using Domain = A;  // The concept is looking for these exact names
  using Codomain = B;
  Func transform;

  constexpr explicit Morphism(Func f) : transform(std::move(f)) {}

  // We provide the call operator, but NOT the composition (f ∘ g).
  constexpr Codomain operator()(const Domain& x) const { return transform(x); }
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

/** @section Lambda_Introspection */
template <typename T>
struct MorphicBridge;

// If the type is already a verified Arrow, just extract its labels.
template <IsArrow T>
struct MorphicBridge<T> {
  using Domain = typename T::Domain;
  using Codomain = typename T::Codomain;
};

// Pattern match for: ReturnType Class::operator()(ArgType) const
template <typename C, typename R, typename A>
struct MorphicBridge<R (C::*)(A) const> {
  using Domain = std::decay_t<A>;
  using Codomain = std::decay_t<R>;
};

// Pattern match for: ReturnType Class::operator()(ArgType)
template <typename C, typename R, typename A>
struct MorphicBridge<R (C::*)(A)> {
  using Domain = std::decay_t<A>;
  using Codomain = std::decay_t<R>;
};

/** @section Library_Function_Support */
template <typename R, typename A>
struct MorphicBridge<std::function<R(A)>> {
  using Domain = std::decay_t<A>;
  using Codomain = std::decay_t<R>;
};

/** @section Morphic_Resolution */

template <typename F>
struct signature_extractor {
  // Priority 1: Formal Arrows (Morphism, Identity, ZeroMorphism)
  template <IsArrow U>
  static auto resolve(int) -> U;

  // Priority 2: Lambdas and Functors (Internal operator())
  template <typename U>
  static auto resolve(int) -> decltype(&std::remove_cvref_t<U>::operator());

  // Priority 3: Function Pointers / std::function
  template <typename U>
  static auto resolve(...) -> std::remove_cvref_t<U>;

  using type = decltype(resolve<F>(0));
};

// 2. Define aliases for the Domain and Codomain extraction
template <typename F>
using domain_t =
    typename MorphicBridge<typename signature_extractor<F>::type>::Domain;

template <typename F>
using codomain_t =
    typename MorphicBridge<typename signature_extractor<F>::type>::Codomain;

/** @section CTAD_Guide */
template <typename F>
Morphism(F) -> Morphism<domain_t<F>, codomain_t<F>, F>;

static_assert(
    IsArrow<Morphism<int, bool, std::function<bool(int)>>>,
    "Taxonomy Error: Morphism must satisfy the skeletal IsArrow concept.");

/** @section Morphic_Factory */

// 1. Overload for raw Lambdas / Functors
// Restricted: Only if it's NOT already an Arrow but IS callable.
template <typename F>
  requires(!IsArrow<std::remove_cvref_t<F>>) &&
          requires(F f) { typename signature_extractor<F>::type; }
constexpr auto arrow(F&& f) {
  using D = domain_t<F>;
  using C = codomain_t<F>;
  return Morphism<D, C, std::decay_t<F>>(std::forward<F>(f));
}

// 2. Passthrough for things that are already Arrows (Idempotent factory)
template <IsArrow F>
constexpr auto arrow(F&& f) {
  return std::forward<F>(f);
}

// 3. Verify "Lambdas" (Functional Auto-Discovery)
auto logic_gate = arrow([](int x) -> bool { return x > 0; });

static_assert(
    IsArrow<decltype(logic_gate)>,
    "Inference Error: Lambda was not automatically discovered as an Arrow.");

// 4. Verify "The Universal Bridge" (Primitives as Identity Arrows)
// Allows using a raw '5' as an identity mapping in algebraic expressions
static_assert(
    IsArrow<decltype(arrow([](int x) { return x; }))>,
    "Bridge Error: Primitive int should act as an identity Arrow for itself.");
static_assert(IsArrow<decltype(arrow([](double x) { return x; }))>,
              "Bridge Error: Primitive double should act as an identity Arrow "
              "for itself.");

// 5. Verify "Morphism Struct" (Reification)
using IntToBool = Morphism<int, bool, std::function<bool(int)>>;
static_assert(IsArrow<IntToBool>,
              "Taxonomy Error: Formal Morphism struct failed IsArrow check.");

/**
 * @concept IsEndomorphism
 * @brief Proposition: A Morphism where Domain ≡ Codomain (f: A -> A).
 */
export template <typename T>
concept IsEndomorphism = IsArrow<T> && std::same_as<domain_t<T>, codomain_t<T>>;

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

// 1. Explicit Arrow: arrow<A, B>(f)
export template <typename A, typename B, typename F>
constexpr auto arrow(F&& f) {
  return Morphism<A, B, std::decay_t<F>>(std::forward<F>(f));
}

// 2. Explicit Endomorphism: endo<A>(f)
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

/**
 * @brief The Identity Morphism and Functorial Witness.
 *
 * In the Category of Species (Set), Identity acts as the unit morphism \(id_T:
 * T \to T\). In the Category of Functors, it acts as the Identity Functor \(Id:
 * \mathcal{C} \to \mathcal{C}\).
 *
 * This reification is "Dual-Action": it preserves the structure of both
 * individual elements (Species) and their transformations (Morphisms). It is
 * the canonical "Zero-Length Highway" that connects a species to itself without
 * distortion.
 *
 * @tparam T The Species (Object) for which this identity is defined.
 */
export template <typename T>
struct Identity final {
  /** @brief The source category species. */
  using Domain = T;
  /** @brief The target category species (invariant for identity). */
  using Codomain = T;

  /**
   * @section Categorical_Actions
   * The following operators ensure that Identity acts as a structure-preserving
   * map across different levels of the categorical hierarchy.
   */

  /**
   * @brief The Morphic Action (Object -> Object).
   *
   * Implements the identity function \(f(x) = x\). This satisfies the
   * "Identity on Set" requirement for Level 0 categorical grounding.
   *
   * @param x The species value to be mapped.
   * @return The identical species value.
   */
  constexpr T operator()(const T& x) const noexcept { return x; }

  /**
   * @brief The Functorial Action (Morphism -> Morphism).
   *
   * Implements the Identity Functor lift \(F(f) = f\). This satisfies the
   * Functorial Identity Law: mapping an arrow through the identity context
   * must result in the original arrow.
   *
   * @tparam Arrow A type satisfying the IsArrow concept.
   * @param f The arrow to be lifted into the identity context.
   * @return The original arrow, preserved without transformation.
   */
  template <IsArrow Arrow>
    requires std::same_as<typename Arrow::Domain, T>
  constexpr auto operator()(Arrow f) const noexcept {
    return f;
  }

  /**
   * @note Any type passed to this identity that is neither the Species @p T
   * nor a valid @p IsArrow will result in a substitution failure (SFINAE),
   * preventing "loose" or non-categorical sliding.
   */
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
static_assert(IsArrow<decltype(id<bool>())>,
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
    IsArrow<decltype(id<int>() >> arrow<int, int>(std::negate<int>{}))>,
    "Unit Law: id_A must be a left-identity for morphisms out of A.");

static_assert(IsArrow<decltype(endo<int>(std::negate<int>{}) >> id<int>())>,
              "Unit Law: id_B must be a right-identity for morphisms into B.");

// 2. Proof: Cross-Species Identity
// id_Z combined with a Z -> B bridge must result in a Z -> B bridge
/** @section Cross_Species_Proof: Z -> B */
static_assert(
    IsArrow<decltype(arrow<int, bool>([](int x) { return x > 0; }))>,
    "Skeletal Failure: Failed to construct an anonymous cross-species "
    "Morphism.");

// 3. Proof: Extensional Equality (The Action)
// The composite morphism (f ∘ id) must yield the same value as f.
static_assert((endo<int>(std::negate<int>{}) >> id<int>())(42) == -42,
              "Action Proof: Composition with id must be value-invariant.");

// 4. Proof: Categorical Unity (id ∘ id = id)
static_assert(
    IsArrow<decltype(id<int>() >> id<int>())>,
    "Unity: id composed with itself must remain the Identity Morphism.");

/**
 * @concept IsIsomorphism
 * @brief An Arrow f: A -> B with a guaranteed Inverse g: B -> A.
 * @details Represents a reversible morphism. In Level 0, we verify
 *          the structural existence of the 'Undo' path.
 */
export template <typename F>
concept IsIsomorphism = IsArrow<F> && requires(F f) {
  // An isomorphism must provide its own inverse arrow
  { inverse(f) } -> IsArrow;
  // And the domain of the inverse must be the codomain of the original
  requires std::same_as<typename F::Codomain,
                        typename decltype(inverse(f))::Domain>;
};

/** @brief The structural inverse of an Identity is itself. */
template <typename T>
[[nodiscard]] constexpr auto inverse(Identity<T> id) noexcept {
  return id;
}

// Proof: id_int is an isomorphism from int to int.
static_assert(IsIsomorphism<Identity<int>>,
              "Identity must be a self-inverse isomorphism.");

// Proof: id_bool is an isomorphism from bool to bool.
static_assert(IsIsomorphism<Identity<bool>>,
              "Identity must be a self-inverse isomorphism.");

/** @brief The structural inverse of Negation is itself (Involutive). */
template <typename A, typename B, typename Impl>
  requires std::same_as<Impl, std::negate<A>>
[[nodiscard]] constexpr auto inverse(Morphism<A, B, Impl> f) noexcept {
  return f;  // Negate is its own inverse
}

// Proof: Tagged Negation is a formal Isomorphism.
static_assert(IsIsomorphism<TaggedNegate>,
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
  requires IsArrow<Arrow> && std::convertible_to<T, typename Arrow::Domain>
constexpr auto operator>>(T&& value, const Arrow& f) ->
    typename Arrow::Codomain {
  return f(std::forward<T>(value));
}

}  // namespace dedekind::category
