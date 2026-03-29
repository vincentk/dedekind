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

#include <concepts>
#include <functional>

export module dedekind.category:species;

namespace dedekind::category {

/**
 * @concept IsSpecies
 * @brief Ensures a type has been formally reified with algebraic traits.
 */
export template <typename T>
concept IsSpecies = requires {
  typename T::machine_type;  // Maps back to the Stroustrupian primitive
};

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

/**
 * @concept IsArrow
 * @brief Structural verification of a Morphism signature.
 * @details This is the 'Static Blueprint' of a transformation.
 *          It ensures the type can act as a mapping between species.
 */
export template <typename F, typename A, typename B>
concept IsArrow = requires(F f, A x) {
  typename F::Domain;
  typename F::Codomain;
  requires std::same_as<typename F::Domain, A>;
  requires std::same_as<typename F::Codomain, B>;
  { f(x) } -> std::same_as<B>;
};

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

/**
 * @concept IsMagmoid
 * @brief Represents a Magma-like structure where a binary operation is defined.
 *
 * @details In Category Theory, this corresponds to a 'Quiver' or 'Graph' with
 *          a composition rule that is closed: T × T → T. At this level, we
 *          only guarantee that two elements can be combined; we do not yet
 *          enforce associativity or identity.
 *
 * @tparam T The coordinate species (The "Objects" or "Elements").
 * @tparam Op The binary operation (The "Morphism" or "Composition Rule").
 */
export template <typename T, typename Op>
concept IsMagmoid = requires(T a, T b) {
  { Op{}(a, b) } -> std::convertible_to<T>;
};

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
 * @concept IsAssociative
 * @brief Formal verification that (a ∘ b) ∘ c = a ∘ (b ∘ c).
 */
export template <typename T, typename Op>
concept IsAssociative =
    IsMagmoid<T, Op> && requires { requires is_associative_v<T, Op>; };

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

/**
 * @concept IsCommutative
 * @brief Formal verification that a ∘ b = b ∘ a.
 */
export template <typename T, typename Op>
concept IsCommutative =
    IsMagmoid<T, Op> && requires { requires is_commutative_v<T, Op>; };

export template <typename T, typename Op>
struct is_idempotent : std::false_type {};

template <typename T, typename Op>
  requires requires { T::template is_idempotent_v<Op>; }
struct is_idempotent<T, Op>
    : std::bool_constant<T::template is_idempotent_v<Op>> {};

/** @brief Helper for shorthand access in concepts. */
export template <typename T, typename Op>
inline constexpr bool is_idempotent_v = is_idempotent<T, Op>::value;

/**
 * @concept IsIdempotent
 * @brief Formal verification that x ∘ x = x.
 */
export template <typename T, typename Op>
concept IsIdempotent =
    IsMagmoid<T, Op> && requires { requires is_idempotent_v<T, Op>; };

/** @brief Primary trait: Identity does not exist by default. */
export template <typename T, typename Op>
struct identity_trait {};  // Empty by default

/** @brief Helper to access the value if it exists. */
export template <typename T, typename Op>
inline constexpr T identity_v = identity_trait<T, Op>::value;

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
  static constexpr bool value = false;  // ⊥ (Null/False)
};

template <>
inline constexpr bool is_associative_v<bool, std::logical_and<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::logical_and<bool>> = true;
template <>
struct identity_trait<bool, std::logical_and<bool>> {
  static constexpr bool value = true;  // ⊤ (Whole/True)
};

/**
 * @brief Trait to mark a relation as Reflexive: a ∘ a is always True.
 **/
export template <typename T, typename Rel>
inline constexpr bool is_reflexive_v = false;

/**
 * @brief Trait to mark a relation as Transitive: (a ∘ b) && (b ∘ c) => (a ∘ c)
 **/
export template <typename T, typename Rel>
inline constexpr bool is_transitive_v = false;

/**
 * @brief Trait to mark a relation as Antisymmetric: (a ∘ b) && (b ∘ a) => (a ==
 * b)
 **/
export template <typename T, typename Rel>
inline constexpr bool is_antisymmetric_v = false;

// --- Integers: The finite ring (Z, +, *) ---

/**
 * @brief Addition is associative and commutative, with identity 0.
 */
template <std::integral T>
inline constexpr bool is_associative_v<T, std::plus<T>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;
template <std::integral T>
struct identity_trait<T, std::plus<T>> {
  static constexpr T value = 0;
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
  static constexpr T value = 0;
};

// 2. Bitwise AND: Associative and Commutative.
// Note: We intentionally omit identity_v for (int, &).
template <std::integral T>
inline constexpr bool is_associative_v<T, std::bit_and<T>> = true;

template <std::integral T>
inline constexpr bool is_commutative_v<T, std::bit_and<T>> = true;

/** @section Boolean_Bitwise_Certifications: (B, ^) and (B, &) */

// 1. Boolean XOR (Exclusive OR)
template <>
inline constexpr bool is_associative_v<bool, std::bit_xor<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::bit_xor<bool>> = true;
template <>
struct identity_trait<bool, std::bit_xor<bool>> {
  static constexpr bool value = false;
};

// 2. Boolean AND (Conjunction)
template <>
inline constexpr bool is_associative_v<bool, std::bit_and<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::bit_and<bool>> = true;
template <>
struct identity_trait<bool, std::bit_and<bool>> {
  static constexpr bool value = true;
};

/** @section Categorical_Inverses: The 'Undo' Bricks */

/**
 * @brief In XOR, every element is its own inverse (Involutive).
 * @details This satisfies the Group axiom `a ∘ a = e` where e is 0.
 *          In bitwise logic, this is a perfectly symmetric, non-overflowing
 *          action across the entire Species range.
 */
template <std::integral 𝒯>
inline constexpr 𝒯 inverse(𝒯 a, std::bit_xor<𝒯>) {
  return a;
}

/**
 * @brief In Addition, negation is the inverse.
 * @note [Mathematical Authority]: For signed types, this assumes Two's
 * Complement arithmetic. Note that for the minimum value (e.g., INT_MIN), the
 *       inverse overflows back to itself, satisfying `a + inverse(a) = 0`
 *       via machine wrapping (Modular Arithmetic in Z/2^nZ).
 */
template <std::integral 𝒯>
inline constexpr 𝒯 inverse(𝒯 a, std::plus<𝒯>) {
  // In C++20/23, signed overflow for negation is defined behavior
  // as Two's Complement wrapping.
  return static_cast<𝒯>(-static_cast<std::make_unsigned_t<𝒯>>(a));
}

/**
 * @brief The Master Bridge: The entry point for IsGroupoid.
 * @details This bridge 'routes' the request to the specific implementation
 *          above. If no implementation exists, the concept fails (Skeletal
 * Safety).
 */
export template <typename T, typename Op>  // Note: Op first or deduced
  requires requires(T a) {
    { inverse(a, Op{}) } -> std::same_as<T>;
  }
inline constexpr T inverse(T a) {
  return inverse(a, Op{});
}

/** @section Magmoid Verification: The Atomic Bricks */

// Proof: Boolean AND is a Magmoid.
static_assert(IsMagmoid<bool, std::logical_and<bool>>,
              "Magmoid: bool must be closed under logical conjunction.");

// Proof: Integer Addition is a Magmoid.
static_assert(IsMagmoid<int, std::plus<int>>,
              "Magmoid: int must be closed under addition.");

// Proof: std::modulus is a Magmoid (even if it's not a Monoid).
static_assert(IsMagmoid<int, std::modulus<int>>,
              "Magmoid: int must be closed under remainder.");

/**
 * @concept IsSemigroupoid
 * @brief A Magmoid that satisfies the Associative Law.
 *
 * @details This is a Category without a guaranteed identity. It enforces:
 *          (f ∘ g) ∘ h = f ∘ (g ∘ h). In our structuralist approach, we
 *          verify this by checking the 'is_associative_v' trait, which
 *          acts as a compile-time proof of the associative property.
 *
 * @note Many high-performance algorithms (like parallel reductions)
 *       only require this level of structure.
 */
export template <typename T, typename Op>
concept IsSemigroupoid = IsMagmoid<T, Op> && IsAssociative<T, Op>;

/** @section Semigroupoid Verification: The Grouping Law */

// Proof: (int, +) is associative: (a + b) + c = a + (b + c).
static_assert(IsSemigroupoid<int, std::plus<int>>,
              "Semigroupoid: Integer addition must allow re-grouping.");

// Proof: (int, -) is NOT associative: (10 - 5) - 2 != 10 - (5 - 2).
static_assert(!IsSemigroupoid<int, std::minus<int>>,
              "Semigroupoid: Subtraction must fail the grouping proof.");

// 2. Proof: (int, &) is a Semigroupoid (Associative).
static_assert(IsSemigroupoid<int, std::bit_and<int>>,
              "Bitwise: AND is associative.");

// Proof: (int, *) is a Semigroupoid.
static_assert(IsSemigroupoid<int, std::multiplies<int>>,
              "Semigroupoid: Integer multiplication is associative.");

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

/**
 * @concept IsPointed
 * @brief The Law of the Origin: A species with a distinguished structural
 * identity.
 *
 * @details
 * In the structuralist registry, a Pointed Species is a type T that designates
 * a unique "Structural Origin" (0) via a static factory. This is the
 * algebraic prerequisite for higher-level structures:
 * - Level 3: Monoids (Identity element e).
 * - Level 3: Groups (Neutral element 0).
 * - Level 4: Vector Spaces (The Zero Vector).
 *
 * @section Ontological_Role
 * While a singleton set provides a "Basepoint" for a specific body, an
 * IsPointed Species defines an "Absolute Zero" for its entire domain.
 * This allows for zero-overhead symbolic evaluation of identities
 * across the Dedekind universe.
 *
 * @tparam T The Species being verified (e.g., Integer, Matrix, Complex).
 *
 * Wikipedia: Pointed space, Origin (mathematics), Zero element
 */
export template <typename T>
concept IsPointed = requires {
  { T::origin() } -> std::same_as<T>;
};

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
