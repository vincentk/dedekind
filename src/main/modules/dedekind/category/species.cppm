/**
 * @file dedekind/category/species.cppm
 * @partition :species
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
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "We now come to a decisive step of mathematical abstraction: we forget
 * about what the symbols stand for."
 *       -- Hermann Weyl, The Mathematical Way of Thinking (1941)
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

/**
 * @concept IsSpecies
 * @brief Formal verification that a type has been registered in the Atlas.
 */
export template <typename T>
concept IsSpecies = requires { typename SpeciesTraits<T>::Domain; };

/** @section Verification_of_the_Algebraic_Atlas */

// 1. Verify "Atoms" (IsSpecies)
static_assert(IsSpecies<int>, "Atlas Error: int must be a recognized Species.");
static_assert(IsSpecies<double>,
              "Atlas Error: double must be a recognized Species.");
static_assert(IsSpecies<bool>,
              "Atlas Error: bool must be a recognized Species.");

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

// 2. Min/Max is Idempotent

// 1. Get the types of the range-based function objects
using MinOp = decltype(std::ranges::min);
using MaxOp = decltype(std::ranges::max);

template <std::integral T>
struct is_idempotent<T, MinOp> : std::true_type {};

template <std::integral T>
struct is_idempotent<T, MaxOp> : std::true_type {};

// --- THE STORAGE (Facts) ---
template <typename T, typename Op>
struct identity_registry {};  // Empty box

/** @section Lattice Identities */

// Signed/Floating: The Lattice (Max) identity is the absolute floor.
template <typename T, typename Op>
  requires(std::signed_integral<T> || std::floating_point<T>) &&
          (std::same_as<Op, std::ranges::greater> ||
           std::same_as<Op, std::ranges::greater_equal>)
struct identity_registry<T, Op> {
  static constexpr T value = std::numeric_limits<T>::lowest();
};

// Signed/Floating: The Lattice (Min) identity is the absolute floor.
template <typename T, typename Op>
  requires(std::signed_integral<T> || std::floating_point<T>) &&
          (std::same_as<Op, std::ranges::less> ||
           std::same_as<Op, std::ranges::less_equal>)
struct identity_registry<T, Op> {
  static constexpr T value = std::numeric_limits<T>::max();
};

/** @section Integral_Pointed_Species */

// The Modulo Group identity is 0.
template <typename T, typename Op>
  requires std::integral<T> &&
           (std::same_as<Op, std::plus<T>> || std::same_as<Op, std::plus<void>>)
struct identity_registry<T, Op> {
  static constexpr T value = T{0};
};

// The Modulo Group identity is 0.
template <typename T, typename Op>
  requires std::integral<T> && (std::same_as<Op, std::multiplies<T>> ||
                                std::same_as<Op, std::multiplies<void>>)
struct identity_registry<T, Op> {
  static constexpr T value = T{1};
};

/** @section Bitwise_Pointed_Species */

// 1. Bitwise OR/XOR Identity is 0
template <typename T>
  requires std::integral<T>
struct identity_registry<T, std::bit_or<T>> {
  static constexpr T value = T(0);
};

template <typename T>
  requires std::integral<T>
struct identity_registry<T, std::bit_xor<T>> {
  static constexpr T value = T(0);
};

// 2. Bitwise AND Identity is All-Ones (~0)
template <typename T>
  requires std::integral<T>
struct identity_registry<T, std::bit_and<T>> {
  static constexpr T value = ~T(0);
};

// 3. Floats under Addition (Zero)
template <typename T>
  requires std::floating_point<T>
struct identity_registry<T, std::plus<T>> {
  static constexpr T value = T(0.0);
};

// 3. Floats under Multiplication (One)
template <typename T>
  requires std::floating_point<T>
struct identity_registry<T, std::multiplies<T>> {
  static constexpr T value = T(1.0);
};

// 1. Boolean 'Addition' (OR) has identity FALSE
// a ∨ false = a
template <typename Op>
  requires std::same_as<Op, std::logical_or<bool>> ||
           std::same_as<Op, std::logical_or<void>>
struct identity_registry<bool, Op> {
  static constexpr bool value = false;
};

// 2. Boolean 'Multiplication' (AND) has identity TRUE
// a ∧ true = a
template <typename Op>
  requires std::same_as<Op, std::logical_and<bool>> ||
           std::same_as<Op, std::logical_and<void>>
struct identity_registry<bool, Op> {
  static constexpr bool value = true;
};

// 3. Boolean 'XOR' (Ring Addition) has identity FALSE
// a ⊕ false = a
template <typename Op>
  requires std::same_as<Op, std::bit_xor<bool>> ||
           std::same_as<Op, std::bit_xor<void>>
struct identity_registry<bool, Op> {
  static constexpr bool value = false;
};

// --- THE LIBRARIAN (Discovery Logic) ---
// Exported so that types defined in other modules (e.g. dedekind.numbers)
// can provide explicit specializations to register their identity elements.
export template <typename T, typename Op, typename = void>
struct identity_trait : identity_registry<T, Op> {};

// Special "Discovery" track for types that define their own identity internally
template <typename T, typename Op>
struct identity_trait<T, Op,
                      std::void_t<decltype(T::template identity_v<Op>)>> {
  static constexpr T value = T::template identity_v<Op>;
};

// --- THE SERVICE DESK (Public API) ---
export template <typename T, typename Op>
  requires requires { identity_trait<T, Op>::value; }
inline constexpr T identity_v = identity_trait<T, Op>::value;

/**
 * @concept IsPointed
 * Replaces the missing 'HasIdentity' for Level 0.1
 */
export template <typename T, typename Op>
concept IsPointed = requires {
  { identity_v<T, Op> } -> std::convertible_to<T>;
};

static_assert(IsPointed<bool, std::logical_or<bool>>,
              "Pointed: Booleans must have an additive identity (false).");

static_assert(IsPointed<bool, std::logical_and<bool>>,
              "Pointed: Booleans must have a multiplicative identity (true).");

static_assert(IsPointed<int, std::plus<int>>,
              "Pointed: Integers must have an additive identity (0).");

static_assert(IsPointed<int, std::multiplies<int>>,
              "Pointed: Integers must have a multiplicative identity (1).");

static_assert(IsPointed<double, std::plus<double>>,
              "Pointed: Doubles must have an additive identity (0).");

static_assert(IsPointed<double, std::multiplies<double>>,
              "Pointed: Doubles must have a multiplicative identity (1).");

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

// --- Booleans: A commutative Abelian Monoid (Lattice) ---
template <typename Op>
  requires std::same_as<Op, std::logical_or<bool>> ||
               std::same_as<Op, std::logical_or<void>>
inline constexpr bool is_commutative_v<bool, Op> = true;

template <typename Op>
  requires std::same_as<Op, std::logical_and<bool>> ||
               std::same_as<Op, std::logical_and<void>>
inline constexpr bool is_commutative_v<bool, Op> = true;

// Dijkstra would appreciate the exhaustiveness here:
// Whether using std::logical_and<bool> or the transparent std::logical_and<>,
// the operation on the species 'bool' is strictly associative.
template <typename Op>
  requires std::same_as<Op, std::logical_and<bool>> ||
               std::same_as<Op, std::logical_and<void>>
inline constexpr bool is_associative_v<bool, Op> = true;

template <typename Op>
  requires std::same_as<Op, std::logical_or<bool>> ||
               std::same_as<Op, std::logical_or<void>>
inline constexpr bool is_associative_v<bool, Op> = true;

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

/**
 * @section The_Distributive_Axiom
 * @brief Trait to mark the structural glue between two operators.
 * axiom: a * (b + c) = (a * b) + (a * c)
 */
export template <typename T, typename Mult, typename Add>
inline constexpr bool is_distributive_v = false;

/** @brief The Shorthand for the Registry. */
export template <typename T, typename Op>
inline constexpr bool is_invertible_v = inverse_trait<T, Op>::exists;

// --- Booleans: The Perfect Symmetry ---

/**
 * @brief XOR on bool is the additive composition of the Boolean Ring.
 * It is bit-perfect, associative, and total.
 */
template <>
inline constexpr bool is_associative_v<bool, std::bit_xor<bool>> = true;

/**
 * @brief Every boolean is its own inverse (x ^ x = false).
 * There is no Lipschitz boundary breach here.
 */
template <>
inline constexpr bool is_invertible_v<bool, std::bit_xor<bool>> = true;

template <>
struct identity_trait<bool, std::bit_xor<bool>> {
  using value_type = bool;
  static constexpr bool value = false;
};

// --- Integers: The finite ring (Z, +, *) ---

/**
 * @brief Associativity is only total for unsigned types (modular arithmetic).
 * For signed types, it is REJECTED here due to the re-ordering hazard (UB).
 */
template <std::unsigned_integral T>
inline constexpr bool is_associative_v<T, std::plus<>> = true;
template <std::unsigned_integral T>
inline constexpr bool is_associative_v<T, std::plus<T>> = true;

template <std::signed_integral T>
inline constexpr bool is_associative_v<T, std::plus<>> = false;
template <std::signed_integral T>
inline constexpr bool is_associative_v<T, std::plus<T>> = false;

/**
 * @brief Commutativity remains a total property (order-independence).
 */
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::plus<>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;

template <std::unsigned_integral T>
struct identity_trait<T, std::plus<>> {
  using value_type = T;
  static constexpr T value = 0;
};

template <std::integral T>
struct identity_trait<T, std::plus<T>> {
  using value_type = T;
  static constexpr T value = 0;
};

template <std::integral T>
struct identity_trait<T, std::multiplies<>> {
  using value_type = T;
  static constexpr T value = 1;
};

/**
 * @brief Multiplication is ONLY associative for types with defined (modular)
 * overflow. Signed integers fail this at Level 0 due to the UB hazard.
 */
template <std::unsigned_integral T>
inline constexpr bool is_associative_v<T, std::multiplies<T>> = true;

template <std::unsigned_integral T>
inline constexpr bool is_commutative_v<T, std::multiplies<T>> = true;

template <std::signed_integral T>
inline constexpr bool is_associative_v<T, std::multiplies<T>> = false;

template <std::unsigned_integral T>
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
 * @brief On the multiplicative group @f$\mathbb{F}_2^\times =
 * \{\mathtt{true}\}@f$, the only non-zero element is self-inverse: @c true @c
 * & @c true @c = @c true (the registered operator is @c std::bit_and<bool>,
 * the bitwise AND).  Zero (i.e.\ @c false) is excluded by convention on
 * @c is_invertible_v, so the trait can be declared at the type level without
 * quantifying over values. Together with the XOR additive group, this witnesses
 * @c dedekind::category::IsField<bool, std::bit_xor<bool>, std::bit_and<bool>>:
 * @c bool @em is the Galois field @f$\mathbb{F}_2@f$ under its natural bitwise
 * operators.
 */
template <>
inline constexpr bool is_invertible_v<bool, std::bit_and<bool>> = true;

/** @brief AND distributes over XOR on bool: the field distributivity axiom. */
template <>
inline constexpr bool
    is_distributive_v<bool, std::bit_and<bool>, std::bit_xor<bool>> = true;

/** @section Transparent_Bridges */

// 1. Bitwise OR (|)
template <std::integral T>
inline constexpr bool is_idempotent_v<T, std::bit_or<>> = true;

template <std::integral T>
inline constexpr bool is_commutative_v<T, std::bit_or<>> = true;

template <std::integral T>
struct identity_trait<T, std::bit_or<>> {
  using value_type = T;
  static constexpr T value = 0;
};

// 2. Bitwise AND (&)
template <std::integral T>
inline constexpr bool is_idempotent_v<T, std::bit_and<>> = true;

template <std::integral T>
inline constexpr bool is_associative_v<T, std::bit_and<>> = true;

// 3. Multiplication (*)
template <std::unsigned_integral T>
inline constexpr bool is_associative_v<T, std::multiplies<>> = true;

template <std::signed_integral T>
inline constexpr bool is_associative_v<T, std::multiplies<>> = false;

template <typename T>
inline constexpr bool is_distributive_v<T, std::multiplies<T>, std::plus<T>> =
    std::unsigned_integral<T>;

// Certification: Bitwise AND distributes over Bitwise OR (Boolean Algebra)
template <typename T>
inline constexpr bool is_distributive_v<T, std::bit_and<>, std::bit_or<>> =
    std::integral<T>;

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

static_assert(
    !is_associative_v<int, std::plus<int>>,
    "Honesty Check: Signed addition is NOT associative due to UB hazards.");

/** @section Property_Ledger: Periodicity
 *  A type is periodic if it defines a circular topology where
 *  "stepping off the edge" results in a valid, defined wrap.
 */
export template <typename T, typename Op>
struct is_periodic : std::false_type {};

export template <typename T, typename Op>
inline constexpr bool is_periodic_v = is_periodic<T, Op>::value;

/** @section Property_Ledger: Saturation
 *  A type is saturating under @c Op if it defines an extended-range
 *  topology where "stepping off the edge" escalates to a saturating
 *  value (e.g.\ @f$\pm \aleph_0@f$) rather than wrapping.  The
 *  archetypal carrier is @c sets::SignedCardinality (#377) ---
 *  the ℤ ∪ {±ℵ_0, NaZ} extended-integer carrier whose @c + is total
 *  by escalation, not by wrap.
 *
 *  Distinct from @c is_idempotent_v, which says the operation is
 *  globally stable (a*a = a for all a).  Saturation is the milder
 *  claim that the operation is total because any out-of-range
 *  result has a defined saturating image.
 */
export template <typename T, typename Op>
struct is_saturating : std::false_type {};

export template <typename T, typename Op>
inline constexpr bool is_saturating_v = is_saturating<T, Op>::value;

/** @section totality
 *  Three pragmatic paths to totality, each a sufficient (not
 *  necessary) condition: periodicity (modular wrap), idempotence
 *  (globally stable), or saturation (escalation to an extended-range
 *  sentinel).  See the textbook note on @c IsTotal below.
 */
export template <typename T, typename Op>
struct is_total
    : std::bool_constant<
          is_periodic_v<T, Op> ||    // Path A: It wraps (Groups/Rings)
          is_idempotent_v<T, Op> ||  // Path B: It's stable (Lattices/Extrema)
          is_saturating_v<T, Op>     // Path C: It escalates (extended ℤ, ±ℵ_0)
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
 * @section Ring_like_distributivity_for_unsigned_integrals
 * The @c Modular<N> carrier and its trait specialisations moved to
 * @c dedekind.morphologies:cyclic as part of #378; see that partition
 * for the finite-cyclic-ring story.  The generic distributivity
 * claim for @c std::unsigned_integral carriers (which covers
 * @c unsigned int / @c unsigned long / @c size_t) stays here.
 */
template <typename T>
inline constexpr bool is_distributive_v<T, std::multiplies<>, std::plus<>> =
    std::unsigned_integral<T>;

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

// Boolean uniqueness: (a ∨ b) ∧ (a ∨ c) = a ∨ (b ∧ c)
template <>
inline constexpr bool
    is_distributive_v<bool, std::logical_or<bool>, std::logical_and<bool>> =
        true;

template <>
inline constexpr bool
    is_distributive_v<bool, std::logical_and<bool>, std::logical_or<bool>> =
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
export template <typename T, typename Mul, typename Add>
concept IsDistributive = requires(T a, T b, T c) {
  // We check the semantic presence of the law (usually via a trait)
  requires is_distributive_v<T, Mul, Add>;
};

export template <typename T, typename Op>
concept IsIdempotent = is_idempotent_v<T, Op>;

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
 * A morphism is total if it is Periodic (Circular), Idempotent
 * (Stable), or Saturating (escalating to an extended-range sentinel).
 *
 * Textbook note:
 * The three paths are orthogonal algebraic properties.  This concept
 * is a pragmatic implementation certificate and not a canonical
 * algebraic taxonomy boundary; carriers opt in to whichever path
 * matches their machine-totality story.  The named concept @c
 * IsSaturating that wraps @c is_saturating_v lives in @c
 * morphologies:absorption (the saturation home), per #387's lift;
 * here we reach for the underlying trait variable directly so this
 * upstream-foundational layer does not depend on a downstream
 * partition.
 */
export template <typename T, typename Op>
concept IsTotal =
    IsPeriodic<T, Op> || IsIdempotent<T, Op> || is_saturating_v<T, Op>;

/** @section Lattice_Morphisms (std::ranges) */

// 1. Join (max) is Idempotent, Associative, and Commutative
template <typename T>
inline constexpr bool is_idempotent_v<T, decltype(std::ranges::max)> = true;

template <typename T>
inline constexpr bool is_associative_v<T, decltype(std::ranges::max)> = true;

template <typename T>
inline constexpr bool is_commutative_v<T, decltype(std::ranges::max)> = true;

// 2. Meet (min) is Idempotent, Associative, and Commutative
template <typename T>
inline constexpr bool is_idempotent_v<T, decltype(std::ranges::min)> = true;

template <typename T>
inline constexpr bool is_associative_v<T, decltype(std::ranges::min)> = true;

template <typename T>
inline constexpr bool is_commutative_v<T, decltype(std::ranges::min)> = true;

/** @section Distributive_Lattice_Laws (std::ranges) */

// 1. Max distributes over Min
template <typename T>
inline constexpr bool is_distributive_v<T, decltype(std::ranges::max),
                                        decltype(std::ranges::min)> = true;

// 2. Min distributes over Max
template <typename T>
inline constexpr bool is_distributive_v<T, decltype(std::ranges::min),
                                        decltype(std::ranges::max)> = true;

/**
 * @brief The Absorber Trait (Axiom: a ∨ (a ∧ b) = a).
 * Represents the structural tethering between two dual operations.
 *
 * @note (#387 lift) The named concept @c IsAbsorptive that wraps
 * this trait, plus the audit witnesses, live in the downstream
 * partition @c morphologies:absorption.  The opt-in specialisations
 * for the canonical operator pairs ((max, min), (logical_or,
 * logical_and), the (bit_xor, bit_and) Boolean-ring non-witness)
 * stay here, alongside the trait-variable template, so that upstream
 * concepts in @c category (e.g.\ @c IsLattice in @c :total,
 * @c IsMereologicalSkewLattice in @c :mereology,
 * @c IsOrderLatticeOperations in @c :posetal) can reach them via the
 * direct trait check without inverting the dependency on
 * @c morphologies.
 */
template <typename T, typename Op1, typename Op2>
inline constexpr bool is_absorptive_v = false;

/** @section Lattice_Absorber_Registration */

// 1. Integers (and any totally-ordered carrier): max/min mutual absorption.
template <typename T>
inline constexpr bool
    is_absorptive_v<T, decltype(std::ranges::max), decltype(std::ranges::min)> =
        true;
template <typename T>
inline constexpr bool
    is_absorptive_v<T, decltype(std::ranges::min), decltype(std::ranges::max)> =
        true;

// 2. Boolean / Kleene logic: OR/AND mutual absorption.
template <typename T>
inline constexpr bool
    is_absorptive_v<T, std::logical_or<T>, std::logical_and<T>> = true;
template <typename T>
inline constexpr bool
    is_absorptive_v<T, std::logical_and<T>, std::logical_or<T>> = true;

/** @section Boolean_Ring_Morphisms (XOR, AND) */

// 1. XOR (std::bit_xor) is NOT idempotent (a ^ a = 0)
template <typename T>
inline constexpr bool is_idempotent_v<T, std::bit_xor<T>> = false;

// 2. AND (std::bit_and) IS idempotent
template <typename T>
inline constexpr bool is_idempotent_v<T, std::bit_and<T>> = true;

// 3. The Absorption Failure (the "Boolean Ring is not a Lattice"
//    pin); explicit specialisations against rogue downstream
//    re-specialisation.
template <typename T>
inline constexpr bool is_absorptive_v<T, std::bit_xor<T>, std::bit_and<T>> =
    false;

template <typename T>
inline constexpr bool is_absorptive_v<T, std::bit_and<T>, std::bit_xor<T>> =
    false;

/** @section Atomic_Floor_Verification */

static_assert(!is_associative_v<int, std::plus<int>>,
              "Honesty Check: Signed addition is NOT associative due to UB.");

// 1. Verify "Species" Registration (Section 2.2.1)
static_assert(IsSpecies<int>,
              "Taxonomy Error: int must be a registered Species.");
static_assert(IsSpecies<bool>,
              "Taxonomy Error: bool must be a registered Species.");

// 2. Verify Algebraic Invariants (The "Feature Cube", Table 2)
static_assert(is_idempotent_v<bool, std::logical_or<bool>>,
              "Axiom Error: Boolean 'OR' must be idempotent.");

static_assert(is_idempotent_v<bool, std::logical_and<bool>>,
              "Axiom Error: Boolean 'AND' must be idempotent.");

static_assert(is_idempotent_v<unsigned int, std::bit_and<unsigned int>>,
              "Axiom Error: Bitwise 'AND' must be idempotent for integrals.");

static_assert(is_commutative_v<int, std::bit_or<int>>,
              "Axiom Error: Bitwise 'OR' must be commutative.");

static_assert(identity_v<bool, std::logical_or<bool>> == false,
              "Logic Error: Boolean Or-Monoid identity must be False.");

static_assert(identity_v<bool, std::logical_and<bool>> == true,
              "Logic Error: Boolean And-Monoid identity must be True.");

// 5. Verify Material Constants
static_assert(characteristic_v<unsigned char> == 256,
              "Modulus Error: unsigned char must have characteristic 256.");
static_assert(
    characteristic_v<double> == 0,
    "Field Error: Continuous species (double) must have characteristic 0.");

// Unsigned is modular and safe (Associative)
static_assert(is_associative_v<unsigned int, std::multiplies<>>,
              "Axiom Error: Unsigned multiplication must be associative.");

// Signed is rejected due to UB overflow hazard
static_assert(
    !is_associative_v<int, std::multiplies<>>,
    "Honesty Check: Signed multiplication is NOT associative due to UB.");

static_assert(identity_v<int, std::multiplies<>> == 1,
              "Axiom Error: Multiplicative identity must be 1.");

// Bitwise OR
static_assert(is_idempotent_v<int, std::bit_or<>>,
              "Axiom Error: Bitwise OR must be idempotent.");
static_assert(is_commutative_v<int, std::bit_or<>>,
              "Axiom Error: Bitwise OR must be commutative.");
static_assert(identity_v<int, std::bit_or<>> == 0,
              "Logic Error: Bitwise OR identity must be 0.");

// Bitwise AND
static_assert(is_idempotent_v<int, std::bit_and<>>,
              "Axiom Error: Bitwise AND must be idempotent.");
static_assert(is_associative_v<int, std::bit_and<>>,
              "Axiom Error: Bitwise AND must be associative.");

static_assert(!is_associative_v<int, std::modulus<>>,
              "Axiom Error: Modulus is not associative.");
static_assert(!is_commutative_v<int, std::modulus<>>,
              "Axiom Error: Modulus is not commutative.");

/** @section Distributive_Certifications */

// 1. Unsigned: Multiplicative Distribution over Addition is Total.
static_assert(is_distributive_v<unsigned int, std::multiplies<>, std::plus<>>,
              "Axiom Error: Unsigned * must distribute over +.");

// 2. Signed: Multiplicative Distribution over Addition is REJECTED.
static_assert(!is_distributive_v<int, std::multiplies<>, std::plus<>>,
              "Honesty Check: Signed * does NOT distribute over + due to UB.");

// Modular<N>'s distributivity static_assert lives in morphologies:cyclic
// (the carrier moved there as part of #378).

// 3. Bitwise Distributivity: AND distributes over OR (Boolean Ring properties).
static_assert(is_distributive_v<unsigned int, std::bit_and<>, std::bit_or<>>,
              "Axiom Error: Bitwise AND must distribute over bitwise OR.");

// 4. Verification: Multiplication does NOT distribute over bitwise OR.
// (a * (b | c) != (a * b) | (a * c) in general).
static_assert(
    !is_distributive_v<int, std::multiplies<>, std::bit_or<>>,
    "Logic Error: Multiplication should not distribute over bitwise OR.");

// The `is_cyclic_group` / `cyclic_order` specialisations for
// `std::unsigned_integral` carriers live in `dedekind.category:total`
// alongside the `IsCyclicGroup` concept itself (the struct templates
// are defined there; `:species` is below `:total` in the DAG).

}  // namespace dedekind::category
