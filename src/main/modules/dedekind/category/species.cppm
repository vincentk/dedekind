/**
 * @file ontology:species.cppm
 * @brief Level 0a: The Reified Machine (The Taxonomic Bricks).
 *
 * @section The_Taxonomy_of_Species
 * This partition performs the formal reification of C++ machine types.
 * Following Joyal’s Theory of Species, we treat each fundamental type
 * not as a set of values, but as a rule for generating structure.
 *
 * @subsection Structuralist_Elevation
 * Raw types (bool, int, double) are elevated to "Species" by attaching
 * algebraic metadata. This ensures that a species carries its own
 * "laws of nature" (associativity, identity) as static constants.
 *
 * @subsection Zero_Overhead_Requirements
 * To satisfy the Stroustrupian ideal, all species definitions must
 * resolve to their underlying machine primitives at compile-time.
 * Categorification here is an act of reasoning, not an act of
 * allocation.
 *
 * @section Taxonomic_Traits: The Skeletal Constants
 * - identity_v<T, Op>  : The neutral element for a specific operation.
 * - is_associative_v   : Static proof of grouping independence.
 * - is_commutative_v   : Static proof of order independence.
 *
 * @section The_Proto_Morphism
 * Defines the skeletal signature of an Arrow (Domain -> Codomain).
 * This provides the mapping metadata required for the Functorial
 * discovery in Level 0b (:category).
 *
 * Wikipedia: Combinatorial species, Type theory, Generic programming
 */

module;

#include <algorithm>
#include <concepts>
#include <functional>

export module dedekind.category:species;

namespace dedekind::category {

/**
 * @tparam T The type (Species) or the Function Object (Arrow).
 * @tparam Args Optional Domain for inference.
 */
export template <typename T, typename... Args>
struct SpeciesTraits;

/** @section Primary_Registration_for_Atoms */
// We register the "Species" themselves so they satisfy IsSpecies
template <typename T>
  requires std::integral<T> || std::floating_point<T> || std::same_as<T, bool>
struct SpeciesTraits<T> {
  using Domain = T;
  using machine_type = T;
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

/**
 * @concept IsSpecies
 * @brief Formal verification that a type has been registered in the Atlas.
 */
export template <typename T>
concept IsSpecies = requires { typename SpeciesTraits<T>::Domain; };

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

static_assert(
    IsArrow<Morphism<int, bool, std::function<bool(int)>>, int, bool>,
    "Taxonomy Error: Morphism must satisfy the skeletal IsArrow concept.");

/**
 * @concept IsEndomorphism
 * @brief Proposition: A Morphism where Domain ≡ Codomain (f: A -> A).
 */
export template <typename F, typename T>
concept IsEndomorphism = IsArrow<F, T, T>;

/** @section Morphism_Proof */
static_assert(
    IsArrow<Morphism<int, bool, std::function<bool(int)>>, int, bool>,
    "Taxonomy Error: Morphism must satisfy the skeletal IsArrow concept.");

/** @section Verification_of_the_Algebraic_Atlas */

// 1. Verify "Atoms" (IsSpecies)
static_assert(IsSpecies<int>, "Atlas Error: int must be a recognized Species.");
static_assert(IsSpecies<double>,
              "Atlas Error: double must be a recognized Species.");
static_assert(IsSpecies<bool>,
              "Atlas Error: bool must be a recognized Species.");

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

static_assert(
    IsEndomorphism<std::plus<int>, int>,
    "Taxonomy Error: std::plus<int> must be recognized as an Endomorphism.");

/** @section The Traits (The categorical invariants) */

/**
 * @brief Trait to mark an operation as associative: (a ∘ b) ∘ c = a ∘ (b ∘ c)
 **/
export template <typename T, typename Op>
struct is_associative : std::false_type {};

// This "Discovery" specialization looks for a member variable
// named 'is_associative_v' that is parameterized by the Op.
template <typename T, typename Op>
  requires requires { T::template is_associative_v<Op>; }
struct is_associative<T, Op>
    : std::bool_constant<T::template is_associative_v<Op>> {};

export template <typename T, typename Op>
inline constexpr bool is_associative_v = is_associative<T, Op>::value;

/**
 * @brief Trait to mark an operation as commutative: a ∘ b = b ∘ a
 **/
export template <typename T, typename Op>
struct is_commutative : std::false_type {};

template <typename T, typename Op>
  requires requires { T::template is_commutative_v<Op>; }
struct is_commutative<T, Op>
    : std::bool_constant<T::template is_commutative_v<Op>> {};

export template <typename T, typename Op>
inline constexpr bool is_commutative_v = is_commutative<T, Op>::value;

/** @section The_Bitwise_Commutativity_Axiom */
template <typename T>
  requires std::is_integral_v<T>
struct is_commutative<T, std::bit_or<T>> : std::true_type {};

template <typename T>
  requires std::is_integral_v<T>
struct is_commutative<T, std::bit_and<T>> : std::true_type {};

/**
 * @brief Trait to mark an operation as idempotent: a ∘ a = a
 **/
export template <typename T, typename Op>
struct is_idempotent : std::false_type {};

template <typename T, typename Op>
  requires requires { T::template is_idempotent_v<Op>; }
struct is_idempotent<T, Op>
    : std::bool_constant<T::template is_idempotent_v<Op>> {};

/** @brief Helper for shorthand access in concepts. */
export template <typename T, typename Op>
inline constexpr bool is_idempotent_v = is_idempotent<T, Op>::value;

// 1. Bitwise is Idempotent
template <std::integral T>
struct is_idempotent<T, std::bit_and<T>> : std::true_type {};

template <std::integral T>
struct is_idempotent<T, std::bit_or<T>> : std::true_type {};

// 2. Min/Max is Idempotent

// 1. Get the types of the range-based function objects
using MinOp = decltype(std::ranges::min);
using MaxOp = decltype(std::ranges::max);

template <std::integral T>
struct is_idempotent<T, MinOp> : std::true_type {};

template <std::integral T>
struct is_idempotent<T, MaxOp> : std::true_type {};

/**
 * @section Identity_Discovery_Engine
 * This internal bridge allows Level 0 concepts to discover algebraic
 * identities defined in higher-level modules (like :polynomials).
 */
template <typename T, typename Op>
struct identity_discovery {
  // We use a lambda-based decltype to check if the member exists.
  // If T::identity_v exists for this Op, this resolves to the identity value.
  static constexpr bool has_member = requires { T::template identity_v<Op>; };
};
/**
 * @section Identity_Discovery_Engine
 * This internal bridge allows Level 0 concepts to discover algebraic
 * identities defined in higher-level modules (like :polynomials).
 */

// 1. The "Missing" Case: No value member here.
template <typename T, typename Op, typename = void>
struct identity_base {};

// 2. The "Found" Case: Only active if T::identity_v exists.
// FIX: Added the missing template argument list and std::void_t check
template <typename T, typename Op>
struct identity_base<T, Op, std::void_t<decltype(T::template identity_v<Op>)>> {
  using value_type = T;
  static constexpr T value = T::template identity_v<Op>;
};

/**
 * @brief Primary identity_trait
 */
export template <typename T, typename Op>
struct identity_trait : identity_base<T, Op> {};

/** @brief Helper to access the identity value. */
export template <typename T, typename Op>
  requires requires { typename identity_trait<T, Op>::value_type; }
inline constexpr T identity_v = identity_trait<T, Op>::value;

/** @brief Shorthand verification. */
export template <typename T, typename Op>
inline constexpr bool has_identity_v =
    requires { typename identity_trait<T, Op>::value_type; };

/**
 * @brief The Characteristic of the Species.
 * @details For a finite ring (like char or int), this is the modulus n
 *          where n * 1 = 0. For infinite fields (Q, R), it is 0.
 */
export template <typename T>
inline constexpr size_t characteristic_v =
    0;  // Default to infinite/characteristic 0

template <>
inline constexpr size_t characteristic_v<unsigned char> = 256;

// --- Booleans: An Abelian Monoid (Lattice) ---
template <>
inline constexpr bool is_associative_v<bool, std::logical_or<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::logical_or<bool>> = true;
template <>
struct identity_trait<bool, std::logical_or<bool>> {
  using value_type = bool;
  static constexpr bool value = false;  // ⊥ (Null/False)
};

template <>
inline constexpr bool is_associative_v<bool, std::logical_and<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::logical_and<bool>> = true;
template <>
struct identity_trait<bool, std::logical_and<bool>> {
  using value_type = bool;
  static constexpr bool value = true;  // ⊤ (Whole/True)
};

/**
 * @brief Trait to mark a relation as reflexive: a ~ a = True
 **/
export template <typename T, typename Rel>
struct is_reflexive : std::false_type {};

// Discovery: Look for a member variable 'is_reflexive_v'
template <typename T, typename Rel>
  requires requires { T::template is_reflexive_v<Rel>; }
struct is_reflexive<T, Rel>
    : std::bool_constant<T::template is_reflexive_v<Rel>> {};

// The Bridge (The "v" helper)
export template <typename T, typename Rel>
inline constexpr bool is_reflexive_v = is_reflexive<T, Rel>::value;

/** @section The_Reflexivity_Axiom */
// General case: Any type T is reflexive under std::less_equal
// if it satisfies basic totally_ordered requirements.
template <typename T>
  requires std::totally_ordered<T>
struct is_reflexive<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsReflexive
 * @brief Formal verification of the identity relation.
 */
export template <typename T, typename Rel>
concept IsReflexive = requires(T a) {
  { Rel{}(a, a) };
} && requires { requires is_reflexive_v<T, Rel>; };

/** @section Transitivity: (a <= b && b <= c) => a <= c */
export template <typename T, typename Rel>
struct is_transitive : std::false_type {};

template <typename T, typename Rel>
  requires requires { T::template is_transitive_v<Rel>; }
struct is_transitive<T, Rel>
    : std::bool_constant<T::template is_transitive_v<Rel>> {};

export template <typename T, typename Rel>
inline constexpr bool is_transitive_v = is_transitive<T, Rel>::value;

template <typename T>
  requires std::is_integral_v<T> || std::is_same_v<T, bool>
struct is_transitive<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsTransitive
 * @brief Formal verification: (a ≤ b ∧ b ≤ c) ⇒ a ≤ c.
 */
export template <typename T, typename Rel>
concept IsTransitive = requires(T a, T b) {
  { Rel{}(a, b) } -> std::convertible_to<bool>;
} && requires { requires is_transitive_v<T, Rel>; };

/** @section Antisymmetry: (a <= b && b <= a) => a == b */
export template <typename T, typename Rel>
struct is_antisymmetric : std::false_type {};

template <typename T, typename Rel>
  requires requires { T::template is_antisymmetric_v<Rel>; }
struct is_antisymmetric<T, Rel>
    : std::bool_constant<T::template is_antisymmetric_v<Rel>> {};

export template <typename T, typename Rel>
inline constexpr bool is_antisymmetric_v = is_antisymmetric<T, Rel>::value;

template <typename T>
  requires std::is_integral_v<T> || std::is_same_v<T, bool>
struct is_antisymmetric<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsAntisymmetric
 * @brief Formal verification: (a ≤ b ∧ b ≤ a) ⇒ a = b.
 */
export template <typename T, typename Rel>
concept IsAntisymmetric = requires(T a, T b) {
  { Rel{}(a, b) } -> std::convertible_to<bool>;
} && requires { requires is_antisymmetric_v<T, Rel>; };

/** @section Categorical_Inverses: The 'Undo' Bricks */

/** @brief In XOR, every element is its own inverse (Involutive). */
template <std::integral 𝒯>
inline constexpr 𝒯 inverse(𝒯 a, std::bit_xor<𝒯>) {
  return a;
}

/** @brief In Addition, negation is the inverse (Two's Complement wrapping). */
template <std::integral 𝒯>
inline constexpr 𝒯 inverse(𝒯 a, std::plus<𝒯>) {
  return static_cast<𝒯>(-static_cast<std::make_unsigned_t<𝒯>>(a));
}

/** @brief The Master Bridge: Routes the request to specific implementations. */
export template <typename T, typename Op>
  requires requires(T a) {
    { inverse(a, Op{}) } -> std::same_as<T>;
  }
inline constexpr T inverse_v(T a) {
  return inverse(a, Op{});
}

/** @brief The Formal Trait: Hooks the bridge into the Concept system. */
export template <typename T, typename Op>
struct inverse_trait {
  static constexpr bool exists = requires(T a) {
    { inverse(a, Op{}) } -> std::same_as<T>;
  };
};

/** @brief The Shorthand for the Registry. */
export template <typename T, typename Op>
inline constexpr bool is_invertible_v = inverse_trait<T, Op>::exists;

// --- Integers: The finite ring (Z, +, *) ---

/**
 * @brief Addition is associative and commutative, with identity 0.
 * We use std::plus<> (transparent) to catch both explicit and implicit calls.
 */
template <std::integral T>
inline constexpr bool is_associative_v<T, std::plus<>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::plus<>> = true;

/**
 * @brief Integers possess additive inverses (negatives).
 * This promotes the Monoid (N) to the Group (Z).
 */
template <std::integral T>
inline constexpr bool is_invertible_v<T, std::plus<>> = true;

template <std::integral T>
inline constexpr bool is_invertible_v<T, std::plus<T>> = true;

template <std::integral T>
inline constexpr bool is_associative_v<T, std::plus<T>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;

template <std::integral T>
struct identity_trait<T, std::plus<>> {
  using value_type = T;
  static constexpr T value = 0;
};

template <std::integral T>
struct identity_trait<T, std::plus<T>> {
  using value_type = T;
  static constexpr T value = 0;
};

// Also for multiplication
template <std::integral T>
inline constexpr bool is_associative_v<T, std::multiplies<>> = true;

template <std::integral T>
inline constexpr bool is_commutative_v<T, std::multiplies<>> = true;

template <std::integral T>
struct identity_trait<T, std::multiplies<>> {
  using value_type = T;
  static constexpr T value = 1;
};

/**
 * @brief Multiplication is associative and commutative, with identity 1.
 */
template <std::integral T>
inline constexpr bool is_associative_v<T, std::multiplies<T>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::multiplies<T>> = true;
template <std::integral T>
struct identity_trait<T, std::multiplies<T>> {
  using value_type = T;
  static constexpr T value = 1;
};

/**
 * @brief Modulus is NOT associative nor commutative, and has no identity.
 * @details This is because (a mod n) mod n = a mod n, but (a mod n) mod m != a
 * mod m in general. Also, a mod n != n mod a in general.
 */
template <std::integral T>
inline constexpr bool is_associative_v<T, std::modulus<T>> = false;

template <std::integral T>
inline constexpr bool is_commutative_v<T, std::modulus<T>> = false;

/** @section Bitwise_Certifications: (Z, ^) and (Z, &) */

// 1. Bitwise XOR (^) is Associative, Commutative, and has Identity 0.
template <std::integral T>
inline constexpr bool is_associative_v<T, std::bit_xor<T>> = true;

template <std::integral T>
inline constexpr bool is_commutative_v<T, std::bit_xor<T>> = true;

template <std::integral T>
struct identity_trait<T, std::bit_xor<T>> {
  using value_type = T;
  static constexpr T value = 0;
};

// 2. Bitwise AND: Associative and Commutative.
// Note: We intentionally omit identity_v for (int, &).
template <std::integral T>
inline constexpr bool is_associative_v<T, std::bit_and<T>> = true;

template <std::integral T>
inline constexpr bool is_commutative_v<T, std::bit_and<T>> = true;

/**
 * @brief Proof: Integral types satisfy the Total Order axioms.
 * @details [Architectural Decision 2026-04-01]:
 * We specifically whitelist std::integral types here because they satisfy
 * Reflexivity (x <= x) and Antisymmetry (a <= b && b <= a => a == b)
 * without exception.
 *
 * @note [Future Work]: Floating-point types (double/float) are currently
 * excluded from this blanket proof due to IEEE 754 'NaN' violating
 * Reflexivity. To support them, we will require a 'Safe' wrapper or a
 * NaN-aware comparison morphism.
 */
template <std::integral T>
inline constexpr bool is_reflexive_v<T, std::less_equal<>> = true;

template <std::integral T>
inline constexpr bool is_transitive_v<T, std::less_equal<>> = true;

template <std::integral T>
inline constexpr bool is_antisymmetric_v<T, std::less_equal<>> = true;

/** @section Boolean_Bitwise_Certifications: (B, ^) and (B, &) */

// 1. Boolean XOR (Exclusive OR)
template <>
inline constexpr bool is_associative_v<bool, std::bit_xor<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::bit_xor<bool>> = true;
template <>
struct identity_trait<bool, std::bit_xor<bool>> {
  using value_type = bool;
  static constexpr bool value = false;
};

// 2. Boolean AND (Conjunction)
template <>
inline constexpr bool is_associative_v<bool, std::bit_and<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::bit_and<bool>> = true;
template <>
struct identity_trait<bool, std::bit_and<bool>> {
  using value_type = bool;
  static constexpr bool value = true;
};

/**
 * @section Taxonomic_Validation
 * @brief Self-verifying the structural integrity of the reified species.
 */
static_assert(identity_v<bool, std::logical_or<bool>> == false,
              "Taxonomy Error: Boolean OR identity must be False (⊥).");

static_assert(identity_v<bool, std::logical_and<bool>> == true,
              "Taxonomy Error: Boolean AND identity must be True (⊤).");

static_assert(identity_v<int, std::plus<int>> == 0,
              "Taxonomy Error: Integer additive identity must be 0.");

static_assert(identity_v<size_t, std::multiplies<size_t>> == 1,
              "Taxonomy Error: Unsigned multiplicative identity must be 1.");

static_assert(is_associative_v<int, std::plus<int>>,
              "Taxonomy Error: Integer addition must be associative.");

/** @section Property_Ledger: Periodicity
 *  A type is periodic if it defines a circular topology where
 *  "stepping off the edge" results in a valid, defined wrap.
 */
export template <typename T, typename Op>
struct is_periodic : std::false_type {};

export template <typename T, typename Op>
inline constexpr bool is_periodic_v = is_periodic<T, Op>::value;

/** @section totality  */
export template <typename T, typename Op>
struct is_total
    : std::bool_constant<
          is_periodic_v<T, Op> ||  // Path A: It wraps (Groups/Rings)
          is_idempotent_v<T, Op>   // Path B: It's stable (Lattices/Extrema)
          > {};

export template <typename T, typename Op>
inline constexpr bool is_total_v = is_total<T, Op>::value;

/**
 * Unsigned integers are natively periodic under
 * addition/subtraction/multiplication.
 **/
template <typename T>
  requires std::unsigned_integral<T>
struct is_periodic<T, std::plus<T>> : std::true_type {};

template <typename T>
  requires std::unsigned_integral<T>
struct is_periodic<T, std::minus<T>> : std::true_type {};

template <typename T>
  requires std::unsigned_integral<T>
struct is_periodic<T, std::multiplies<T>> : std::true_type {};

// XOR is Periodic (Order 2), ensuring its totality
template <std::integral T>
struct is_periodic<T, std::bit_xor<T>> : std::true_type {};

/**
 * @section The_Modular_Species (Z/nZ)
 * A total species representing a Finite Cyclic Group.
 */
export template <auto N>
struct Modular {
  static_assert(N > 0, "Modulus must be positive.");
  using machine_type = decltype(N);
  machine_type value;

  // We keep the constructor explicit to maintain structuralist integrity
  explicit constexpr Modular(machine_type v) : value(v % N) {}

  // Total Addition: (a + b) mod N
  constexpr friend Modular operator+(Modular a, Modular b) {
    return Modular((a.value + b.value) % N);
  }

  // Total Multiplication: (a * b) mod N
  constexpr friend Modular operator*(Modular a, Modular b) {
    return Modular((a.value * b.value) % N);
  }

  // Equality as a Subobject Classifier
  constexpr friend bool operator==(Modular a, Modular b) {
    return a.value == b.value;
  }
};

template <auto N>
struct is_periodic<Modular<N>, std::plus<Modular<N>>> : std::true_type {};

template <auto N>
struct is_periodic<Modular<N>, std::multiplies<Modular<N>>> : std::true_type {};

/** @section Atlas_Registration: Modular<N> */
export template <auto N>
struct SpeciesTraits<Modular<N>> {
  using Domain = Modular<N>;
  using machine_type = decltype(N);

  /** @section Algebraic_Facts */
  template <typename Op>
  static constexpr bool is_associative_v = true;

  template <typename Op>
  static constexpr bool is_commutative_v = true;

  // Addition and Multiplication are both idempotent ONLY if N=1 (Trivial Ring)
  template <typename Op>
  static constexpr bool is_idempotent_v = (N == 1);

  /** @section Identity_Discovery */
  template <typename Op>
  static constexpr auto identity_v = []() {
    if constexpr (std::is_same_v<Op, std::plus<Modular<N>>>) {
      return Modular<N>{0};
    } else if constexpr (std::is_same_v<Op, std::multiplies<Modular<N>>>) {
      return Modular<N>{1};
    }
  }();
};

static_assert(
    characteristic_v<unsigned char> == 256,
    "Taxonomy Error: 8-bit unsigned species must have characteristic 256.");

/** @section Logic_Species_Specializations */

// Theorem: Truth is Idempotent. (True ∧ True = True)
template <>
struct is_idempotent<bool, std::logical_and<bool>> : std::true_type {};

// Theorem: Presence is Idempotent. (True ∨ True = True)
template <>
struct is_idempotent<bool, std::logical_or<bool>> : std::true_type {};

// Theorem: Bitwise Logic is Idempotent.
template <typename T>
  requires std::is_integral_v<T>
struct is_idempotent<T, std::bit_and<T>> : std::true_type {};

template <typename T>
  requires std::is_integral_v<T>
struct is_idempotent<T, std::bit_or<T>> : std::true_type {};

template <typename T>
  requires std::is_integral_v<T>
struct is_associative<T, std::bit_or<T>> : std::true_type {};

/**
 * @section The_Distributive_Axiom
 * @brief Trait to mark the structural glue between two operators.
 * axiom: a * (b + c) = (a * b) + (a * c)
 */
export template <typename T, typename Add, typename Mul>
inline constexpr bool is_distributive_v = false;

/** @section Machine_Distributivity: Integers */
template <std::integral T>
inline constexpr bool is_distributive_v<T, std::plus<>, std::multiplies<>> =
    true;

template <std::integral T>
inline constexpr bool is_distributive_v<T, std::plus<T>, std::multiplies<T>> =
    true;

// Finite and transfinite species are mutually exclusive.

/** @brief Primary trait for Transfiniteness. */
export template <typename T>
struct is_transfinite : std::false_type {};  // Default: The world is Finite.

/** @brief Shorthand helper. */
export template <typename T>
inline constexpr bool is_transfinite_v = is_transfinite<T>::value;

/** @brief Primary trait: The distinguished 'Point' of a species. */
export template <typename T>
struct origin_trait {
  // Empty by default.
};

/** @brief Atomic Proof: Integrals are pointed at 0. */
template <std::integral T>
struct origin_trait<T> {
  static constexpr T value = 0;
};

/** @brief Atomic Proof: Floating-point types are pointed at 0. */
template <std::floating_point T>
struct origin_trait<T> {
  static constexpr T value = 0.0;
};

/** @section The_Decorator_Concepts */

/** @concept IsTransfinite: A species that exceeds any terminal ordinal. */
export template <typename T>
concept IsTransfinite =
    is_transfinite_v<T> || requires { typename T::is_transfinite_tag; };

/** @concept IsFinite: The "Pedestrian" reality of terminating sets. */
export template <typename T>
concept IsFinite = !IsTransfinite<T>;

export template <typename T, typename Op>
concept IsAssociative = is_associative_v<T, Op>;

export template <typename T, typename Op>
concept IsCommutative = is_commutative_v<T, Op>;

/** @section Commutative Verification: The Symmetry Law */

// Proof: (int, &) is Commutative (even though it's not a SmallCategory).
// This allows a compiler to reorder bitwise AND masks.
static_assert(IsCommutative<int, std::bit_and<int>>,
              "Commutative: Integer AND must allow operand swapping.");

// Proof: (int, +) is Commutative.
static_assert(IsCommutative<int, std::plus<int>>,
              "Commutative: Integer addition is symmetric.");

// Proof: (bool, ||) is Commutative.
static_assert(IsCommutative<bool, std::logical_or<bool>>,
              "Commutative: Boolean OR is symmetric.");

// Negative Proof: (int, -) is NOT Commutative: 5 - 2 != 2 - 5.
static_assert(!IsCommutative<int, std::minus<int>>,
              "Commutative: Subtraction must fail the symmetry proof.");

// Negative Proof: (int, /) is NOT Commutative.
static_assert(!IsCommutative<int, std::divides<int>>,
              "Commutative: Division must fail the symmetry proof.");

/**
 * @concept IsDistributive
 * @brief The "Glue" of the Ring: a * (b + c) = a*b + a*c.
 */
export template <typename T, typename Add, typename Mul>
concept IsDistributive = requires(T a, T b, T c) {
  // We check the semantic presence of the law (usually via a trait)
  requires is_distributive_v<T, Add, Mul>;
};

export template <typename T, typename Op>
concept IsIdempotent = is_idempotent_v<T, Op>;

/**
 * @concept IsPointed
 * Replaces the missing 'HasIdentity' for Level 0.1
 */
export template <typename T, typename Op>
concept IsPointed = requires {
  { identity_v<T, Op> } -> std::convertible_to<T>;
};

export template <typename T, typename Op>
concept IsInvertible = IsPointed<T, Op> && is_invertible_v<T, Op>;

/**
 * @concept IsPeriodic
 * @brief Taxonomic Decorator: Certifies that an operation is circular (wraps).
 * This is the "Safety Certificate" for machine totality.
 */
export template <typename T, typename Op>
concept IsPeriodic = is_periodic_v<T, Op>;

/**
 * @concept IsTotal
 * @brief The Master Safety Certificate for Level 0.
 * A morphism is total if it is either Periodic (Circular) or Idempotent
 * (Stable).
 */
export template <typename T, typename Op>
concept IsTotal = IsPeriodic<T, Op> || IsIdempotent<T, Op>;

/** @section The_Box_Species (The Standard Model) */
export template <typename T>
struct Box final {
  using machine_type = T;
  T value;

  constexpr bool operator==(const Box& other) const = default;
};

// >>= (Bind): Chaining the Box to a Kleisli Arrow
export template <typename T, typename Func>
constexpr auto operator>>=(const Box<T>& b, Func&& f) {
  // We sample the part and hand it to the factory.
  return std::forward<Func>(f)(b.value);
}

/** @section The_Extend_Operator (<<=) */
export template <typename T, typename Func>
constexpr auto operator<<=(const Box<T>& b, Func&& f) {
  using U = std::invoke_result_t<Func, Box<T>>;
  // Co-Kleisli Extend: apply 'f' to the whole box,
  // and re-wrap the result in a new Box.
  return Box<U>{std::forward<Func>(f)(b)};
}

}  // namespace dedekind::category
