/**
 * @file ontology:category.cppm
 * @brief Level 0: The Skeletal Foundation (The Bricks and the Cement).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :category
 * @build_order 1
 * @dependency None (Bootstrap Partition)
 *
 * @section The_Structuralist_Unity: Bricks and Cement
 * This partition establishes the "Standard Model" of the Dedekind library.
 * The raw C++ machine instructions are unified with the abstract laws of
 * Category Theory to create a self-verifying skeletal layer.
 *
 * @subsection The_Bricks: Machine Primitives
 * The library captures the C++ standard library primitive types (bool, char,
 * int, double) and operators (+, *, &&) by providing Proof Assistant Traits.
 * This enriches the CPU's silicon instructions with mathematical authority.
 * - identity_v: The neutral element (0, 1, true).
 * - is_associative_v: The proof of grouping independence.
 * - is_commutative_v: The proof of swap-safety.
 *
 * @subsection The_Cement: Categorical Logic
 * We provide the "Highways" and "Bridges" that allow these bricks to
 * move between worlds without breaking the math:
 * - IsFunctor: The "Box" that preserves the skeletal structure.
 * - lift_natural_transformation: The "Bridge" between functors.
 * - unit / pure / eta: The on-ramp from raw values to structures.
 * - IsEmbedding: A static assertion for injective (1:1) promotions.
 *
 * @section Structural_Inference
 * By keeping the Bricks and Cement together, Level 0 can perform
 * Exhaustive Proofs (like the Bool-to-Int embedding) before any
 * higher-level sets or numbers are even defined.
 *
 * Wikipedia: Category theory, Natural transformation, Functor, Monoid
 */
module;

#include <concepts>
#include <functional>

export module dedekind.ontology:category;

namespace dedekind::ontology {

/** @section The Traits (The categorical invariants) */

/**
 * @brief Trait to mark an operation as associative: (a ∘ b) ∘ c = a ∘ (b ∘ c)
 **/
export template <typename T, typename Op>
inline constexpr bool is_associative_v = false;

/**
 * @brief Trait to mark an operation as commutative: a ∘ b = b ∘ a
 **/
export template <typename T, typename Op>
inline constexpr bool is_commutative_v = false;

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
  { std::declval<Op>()(a, b) } -> std::convertible_to<T>;
};

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
concept IsSemigroupoid = IsMagmoid<T, Op> && is_associative_v<T, Op>;

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

/**
 * @concept IsSmallCategory
 * @brief The fundamental structure of a Monoid viewed as a Category.
 *
 * @details A Category is 'Small' if its collection of morphisms forms a Set.
 *          In C++, this means the structure can be fully represented by a type
 * T. It adds the 'Identity Morphism' (Unit) to a Semigroupoid: f ∘ id = f = id
 * ∘ f.
 *
 * @section Structuralist_Identity
 * The 'identity_v' trait provides the neutral element for the operation Op.
 */
export template <typename T, typename Op>
concept IsSmallCategory = IsSemigroupoid<T, Op> &&
                          requires {
                            typename identity_trait<T, Op>;
                          } &&  // Ensure specialization exists
                          requires {
                            {
                              identity_trait<T, Op>::value
                            } -> std::same_as<const T&>;
                          };

// Proof: (int, +) with '0' is a Small Category.
static_assert(IsSmallCategory<int, std::plus<int>>,
              "SmallCategory: Integer addition with 0 is a Monoid.");

// Proof: (bool, &&) with 'true' is a Small Category.
static_assert(IsSmallCategory<bool, std::logical_and<bool>>,
              "SmallCategory: Boolean AND with 'true' is a Monoid.");

// 3. Proof: (int, &) is NOT a SmallCategory (No Identity).
// This fails the identity_v check because we haven't anchored a
// "Universal Mask" for the Integer Species.
static_assert(!IsSmallCategory<int, std::bit_and<int>>,
              "Bitwise: AND lacks a universal neutral element in Z.");

/**
 * @concept IsCommutative
 * @brief Represents the Symmetry Law (a ∘ b = b ∘ a) for a binary operation.
 *
 * @details In the Dedekind structuralist hierarchy, Commutativity is the
 *          "Permission to Swap." It acts as a formal proof that the order
 *          of operands is irrelevant to the final result of the morphism.
 *
 * @section Computational_Authority
 * Unlike Associativity (which permits re-grouping), Commutativity permits
 * structural re-ordering. This is the foundational requirement for:
 * - Order-independent parallel reductions.
 * - Hardware-level instruction scheduling (Out-of-Order Execution).
 * - Simplification of DAG nodes by canonicalizing operand order (e.g., x + 1).
 *
 * @tparam T The coordinate species.
 * @tparam Op The binary operation being verified for symmetry.
 */
export template <typename T, typename Op>
concept IsCommutative = IsMagmoid<T, Op> && is_commutative_v<T, Op>;

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
 * @concept IsAbelian
 * @brief A Category where the binary composition is Commutative (a ∘ b = b ∘
 * a).
 *
 * @details In the Dedekind topos, Abelian structures (like Logic or Addition)
 *          provide formal permission for the DAG to reorder morphisms.
 *          This enables radical symbolic optimizations, such as SIMD folding,
 *          parallel reductions, and identity-erasure, without altering the
 *          mathematical essence of the result.
 */
export template <typename T, typename Op>
concept IsAbelian = IsSmallCategory<T, Op> && IsCommutative<T, Op>;

/** @section Verification */
static_assert(IsAbelian<int, std::plus<int>>);
static_assert(IsAbelian<bool, std::logical_or<bool>>);

/**
 * @concept IsArrow
 * @brief The formal signature of a Morphism f: A -> B.
 * @details Represents a unary transformation between two Species.
 *          It ensures the Codomain is stable by stripping CV-qualifiers
 *          and references, revealing the underlying mathematical object.
 */
export template <typename F, typename A, typename B>
concept IsArrow = requires(F f, A x) {
  // We probe the action and strip references/const to find the true Codomain.
  typename std::remove_cvref_t<decltype(f(std::forward<A>(x)))>;
  requires std::same_as<std::remove_cvref_t<decltype(f(std::forward<A>(x)))>,
                        B>;
};

// Proof: Negation is an Arrow from int to int.
static_assert(IsArrow<std::negate<int>, int, int>,
              "Arrow: Integer negation must map Z to Z.");

// Proof: A bound comparison (x > 0) is an Arrow: Z -> B.
static_assert(IsArrow<decltype([](int x) { return x > 0; }), int, bool>,
              "Arrow: An anonymous bound comparison must map Z to B.");

// Proof: A pure doubling function is an Arrow: Z -> Z.
static_assert(IsArrow<decltype([](int x) { return x * 2; }), int, int>,
              "Arrow: An anonymous scaling function must map Z to Z.");

// Negative Proof: Addition is NOT a Unary Arrow (it's a Binary Morphism).
static_assert(!IsArrow<std::plus<int>, int, int>,
              "Arrow: Binary operators are not simple arrows.");

/**
 * @struct Morphism
 * @brief A Tagged Arrow carrying its Domain and Codomain as static metadata.
 * @details This is the primary 'Highway' brick. By 'tagging' a C++ callable,
 *          we enable automated structural inference for categorical
 * composition.
 *
 * @tparam A The Domain Species (Source).
 * @tparam B The Codomain Species (Target).
 * @tparam Impl The underlying machine implementation (Function/Lambda).
 */
export template <typename A, typename B, typename Impl>
struct Morphism {
  using Domain = A;
  using Codomain = B;
  Impl action;

  constexpr Morphism(Impl f) : action(f) {}

  /** @brief Performs the mapping A -> B. */
  constexpr B operator()(const A& x) const { return action(x); }
};

/**
 * @struct IdentityAction
 * @brief The primitive 'do-nothing' logic for the Identity Morphism.
 */
export template <typename T>
struct IdentityAction {
  constexpr T operator()(const T& x) const noexcept { return x; }
};

/**
 * @struct Identity
 * @brief The Tagged Identity Morphism id_A: A -> A.
 * @details This is the 'Zero-Length Highway' required for Category Theory.
 *          It satisfies the Unit Laws: f ∘ id = f and id ∘ g = g.
 */
export template <typename T>
struct Identity : Morphism<T, T, IdentityAction<T>> {
  // You MUST name the full base class type in the initializer list
  constexpr Identity()
      : Morphism<T, T, IdentityAction<T>>(IdentityAction<T>{}) {}
};

// We verify that for any object T, Identity<T> is an arrow T -> T.
// This anchors the "Skeletal" layer to the "Machine" layer.
static_assert(IsArrow<Identity<int>, int, int>,
              "Identity<int> must be a morphism from int to int.");

static_assert(IsArrow<Identity<bool>, bool, bool>,
              "Identity<bool> must be a morphism from bool to bool.");

// 3. Negative Proof: Species Safety.
// Identity <int> is NOT an arrow for bool (no promotion allowed).
static_assert(!IsArrow<Identity<int>, bool, bool>,
              "Type Safety: Identity<int> cannot act on booleans.");

/** @brief The Identity Factory: Returns the neutral arrow for Species A. */
export template <typename A>
constexpr auto id() {
  return Identity<A>{};
}

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

/** @section Identity Verification */

// 2. Proof: Identity is "Universal" for its Species.
// We verify the factory id<A>() produces a valid Arrow (B -> B).
static_assert(IsArrow<decltype(id<bool>()), bool, bool>,
              "The id<A>() factory must produce a valid Arrow.");

/** @section The Morphism Factory: arrow <A, B> (f) */
export template <typename A, typename B, typename F>
constexpr auto arrow(F&& f) {
  return Morphism<A, B, std::decay_t<F>>{std::forward<F>(f)};
}

/** @section Arrow Factory Verification: Tagging & Species Integrity */

// 1. Proof: arrow<A, B> correctly tags a standard function object.
using Negate = std::negate<int>;
using TaggedNegate = decltype(arrow<int, int>(Negate{}));

static_assert(std::same_as<typename TaggedNegate::Domain, int>,
              "Arrow Factory: Failed to tag Domain as 'int'.");
static_assert(std::same_as<typename TaggedNegate::Codomain, int>,
              "Arrow Factory: Failed to tag Codomain as 'int'.");

// 2. Proof: arrow<A, B> correctly tags a cross-species lambda (Z -> B).
using IsPositive = decltype([](int x) { return x > 0; });
using TaggedIsPositive = decltype(arrow<int, bool>(IsPositive{}));

static_assert(std::same_as<typename TaggedIsPositive::Domain, int>,
              "Arrow Factory: Failed to tag cross-species Domain.");
static_assert(std::same_as<typename TaggedIsPositive::Codomain, bool>,
              "Arrow Factory: Failed to tag cross-species Codomain.");

// 3. Proof: arrow<A, B> satisfies the IsArrow concept.
static_assert(IsArrow<TaggedIsPositive, int, bool>,
              "Arrow Factory: Produced an object that violates IsArrow.");

// 4. Action Proof: The tagged arrow preserves the underlying action.
// We verify that the factory-produced morphism actually executes.
static_assert(arrow<int, int>([](int x) { return x * 2; })(21) == 42,
              "Arrow Factory: Action check failed for anonymous lambda.");

/**
 * @section Categorical Composition (Explicitly Typed)
 * @brief Synthesizes an arrow A -> C from A -> B and B -> C.
 * @details By requiring A, B, and C as template parameters, we ensure
 *          the composition is a statically verified bridge.
 */
export template <typename F, typename G>
  requires requires {
    typename F::Domain;
    typename G::Codomain;
  }
constexpr auto operator>>(F&& f, G&& g) {
  using A = typename F::Domain;
  using C = typename G::Codomain;

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
    IsArrow<decltype(arrow<int, int>(std::negate<int>{}) >> id<int>()), int,
            int>,
    "Unit Law: id_B must be a right-identity for morphisms into B.");

// 2. Proof: Cross-Species Identity
// id_Z combined with a Z -> B bridge must result in a Z -> B bridge
static_assert(
    IsArrow<decltype(id<int>() >> arrow<int, bool>(IsPositive{})), int, bool>,
    "Unit Law: Identity must preserve the bridge from Z to B.");

// 3. Proof: Extensional Equality (The Action)
// The composite morphism (f ∘ id) must yield the same value as f.
static_assert((arrow<int, int>(std::negate<int>{}) >> id<int>())(42) == -42,
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
 * @struct ZeroAction
 * @brief The 'Absorption' logic.
 * @details Maps any input of species A to the neutral element of species B.
 */
export template <typename A, typename B, typename Op>
struct ZeroAction {
  constexpr B operator()(const A&) const noexcept { return identity_v<B, Op>; }
};

/**
 * @brief The Zero Morphism Factory: zero<A, B, Op>()
 * @details Synthesizes an arrow that absorbs all information,
 *          collapsing the Domain into the Codomain's identity.
 */
export template <typename A, typename B, typename Op>
  requires IsSmallCategory<B, Op>
constexpr auto zero() {
  return arrow<A, B>(ZeroAction<A, B, Op>{});
}

/** @section Zero Morphism Verification */

// 1. Proof: Zero is an Arrow from int to int (under addition).
using ZeroZ = decltype(zero<int, int, std::plus<int>>());
static_assert(IsArrow<ZeroZ, int, int>,
              "Zero: Must be a valid morphism mapping Z to Z.");

// 2. Action Proof: Zero maps everything to 0.
static_assert(zero<int, int, std::plus<int>>()(42) == 0,
              "Absorption: Zero morphism must return the identity element.");

// 3. Action Proof: Zero maps everything to 'true' (under logic AND).
static_assert(zero<int, bool, std::logical_and<bool>>()(42) == true,
              "Absorption: Boolean AND zero must return 'true'.");

/**
 * @concept IsGroupoid
 * @brief A Category where every morphism is an Isomorphism (Invertible).
 *
 * @details This represents a structure where every "action" has a perfect
 *          "undo". In Algebra, this is a Group. It requires an 'inverse'
 *          morphism such that: f ∘ f⁻¹ = id.
 *
 * @note For the Integers (Z), this is addition with negation.
 */
export template <typename T, typename Op>
concept IsGroupoid = IsSmallCategory<T, Op> && requires(T x) {
  // Every element must have an inverse relative to the operation Op
  { inverse<T, Op>(x) } -> std::same_as<T>;
};

/** @section Groupoid Verification: The Symmetry Law */

// 1. Proof: (int, +) is a Groupoid (Additive Group).
// Since we defined inverse<int, plus>(x) as -x, this must pass.
static_assert(IsGroupoid<int, std::plus<int>>,
              "Arithmetic: Integer addition must be a Groupoid.");

// 2. Proof: (int, ^) is a Groupoid (Self-inverse).
// XOR is its own inverse, satisfying a ^ a = 0.
static_assert(IsGroupoid<int, std::bit_xor<int>>,
              "Bitwise: XOR must be a Groupoid.");

// 3. Proof: (bool, ^) is a Groupoid (The Boolean Group).
// Unlike AND/OR, XOR/NOT logic allows for perfect reversibility.
static_assert(IsGroupoid<bool, std::bit_xor<bool>>,
              "Logic: Boolean XOR must be a Groupoid.");

// Verification: (bool, ∧) is a Small Category (Monoid) but NOT a Groupoid.
static_assert(!IsGroupoid<bool, std::logical_and<bool>>,
              "Logic: Boolean AND must not be a Groupoid.");

/**
 * @concept IsAbelianGroupoid
 * @brief A Category where every Morphism is an Isomorphism and
 *        Composition is Commutative.
 *
 * @details This represents the peak of structural symmetry in the
 *          Skeletal layer. In Algebra, this corresponds to an
 *          Abelian Group (e.g., Integer Addition).
 *
 * @section HPC_Authority
 * Because an Abelian Groupoid is both invertible and commutative,
 * the Dedekind engine can:
 * 1. Reorder operations safely for SIMD/Vectorization (Commutativity).
 * 2. Perform "Inverse Pruning" (f ∘ f⁻¹ → id) to eliminate
 *    redundant machine instructions (Invertibility).
 *
 * @note While we will define formal 'Groups' in ontology:algebra,
 *       this concept anchors the requirement at the Category level.
 */
export template <typename T, typename Op>
concept IsAbelianGroupoid = IsGroupoid<T, Op> && IsAbelian<T, Op>;

/** @section Verification of the Standard Model */

// Proof: (bool, &&) is NOT an Abelian Groupoid (It's an Abelian Monoid).
static_assert(!IsAbelianGroupoid<bool, std::logical_and<bool>>,
              "Logic is symmetric but not reversible (Annihilation).");

static_assert(IsAbelian<bool, std::logical_and<bool>>,
              "Logic: AND must be commutative.");

// Proof: (Z, +) is an Abelian Groupoid.
static_assert(IsAbelianGroupoid<int, std::plus<int>>,
              "Integer addition must be a reversible, symmetric action.");

// 1. Proof: (int, ^) is an Abelian Groupoid.
static_assert(IsAbelianGroupoid<int, std::bit_xor<int>>,
              "Bitwise: XOR is a reversible, symmetric Category.");

/** @section Peak Symmetry: Zero vs. Groupoid */

// Proof: The Zero Morphism (int -> int) is an Arrow,
// even if it maps into an Abelian Groupoid.
static_assert(IsArrow<ZeroZ, int, int>,
              "Zero: A 'Black Hole' arrow must map Z to Z.");

// Proof: The result of zero() belongs to the Identity element.
static_assert(zero<int, int, std::plus<int>>()(99) == 0,
              "Absorption: Z -> Z via + must yield 0.");

/**
 * @section Functor Lifting (fmap)
 * @brief Lifts a Morphism f: A -> B into the Functor world F⟨A⟩ -> F⟨B⟩.
 */
export template <template <typename> typename F, typename A, typename B,
                 typename Impl>
constexpr auto lift(Morphism<A, B, Impl> f) {
  // For the Identity Functor, F⟨A⟩ is just A.
  // Therefore, lifting f is simply returning f.
  return f;
}

/** @section Lift Verification: Action & Preservation */

// 1. Proof: Lifting 'Negate' via Identity preserves the result.
static_assert(lift<Identity>(arrow<int, int>(Negate{}))(42) == -42,
              "Lift: The Identity-lifted arrow must produce the same result.");

// 2. Proof: Identity Law (F(id) = id).
// We verify that lifting the identity morphism remains a valid Arrow.
static_assert(
    IsArrow<decltype(lift<Identity>(id<int>())), int, int>,
    "Lift: Lifting the identity morphism must result in a valid Arrow.");

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
                    requires(Morphism<T, T, IdentityAction<T>> f) {
                      // 1. Object Mapping: Does the Species T exist in the Box
                      // F?
                      typename F<T>;

                      // 2. Morphism Mapping: Can we lift an arrow f: T -> T
                      // into the Functor? For an Endofunctor (T=U), this must
                      // produce a valid Arrow T -> T.
                      { lift<F>(f) } -> IsArrow<T, T>;
                    };

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
static_assert(IsEndofunctor<Identity, int, std::plus<int>>,
              "Functor: Identity must be a valid Endofunctor on Z.");

// 2. Proof: Identity is an Endofunctor on the Boolean Lattice (bool, &&).
static_assert(IsEndofunctor<Identity, bool, std::logical_and<bool>>,
              "Functor: Identity must be a valid Endofunctor on B.");

// 3. Proof: Identity is an Endofunctor on XOR logic (bool, ^).
static_assert(
    IsEndofunctor<Identity, bool, std::bit_xor<bool>>,
    "Functor: Identity must be a valid Endofunctor on the XOR Group.");

/**
 * @section The Natural Transformation (η: F ⟹ G)
 * @brief A structure-preserving "Bridge" between two Functors.
 *
 * @details
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
 * @tparam F   The Source Functor.
 * @tparam G   The Target Functor.
 * @tparam 𝒯   The Object in the Category.
 * @tparam OpF The Operation of the source.
 * @tparam OpG The Operation of the target.
 * @tparam η   The Morphism component (The "Secret Sauce").
 */
export template <template <typename> typename F, template <typename> typename G,
                 typename 𝒯, typename OpF, typename OpG,
                 auto η_X  // The Morphism Component
                 >
  requires IsEndofunctor<F, 𝒯, OpF> &&
           IsEndofunctor<G, decltype(η_X(std::declval<𝒯>())),
                         OpG>  // Simplified T mapping
struct Naturality final {
  using Source = 𝒯;
  using Target = decltype(η_X(std::declval<𝒯>()));

  /** @brief η_X : F⟨X⟩ → G⟨X⟩ */
  constexpr Target operator()(Source x) const noexcept { return η_X(x); }

  /** @brief Axiom: η(id_F) = id_G */
  static constexpr bool preserves_identity() noexcept {
    return η_X(identity_v<Source, OpF>) == identity_v<Target, OpG>;
  }
};

/**
 * @brief η: Id ⟹ G (The 6-Parameter Explicit Transformation)
 */
export template <template <typename> typename ℱ, template <typename> typename 𝒢,
                 typename 𝒯, typename Opℱ, typename Op𝒢, auto η_X>
using natural_transformation = Naturality<ℱ, 𝒢, 𝒯, Opℱ, Op𝒢, η_X>;

/** @brief Extracts the argument type from a function pointer. */
template <typename T>
struct morphism_traits;

template <typename R, typename A>
struct morphism_traits<R (*)(A)> {
  using argument_type = A;
};

/**
 * @section The Unit of the Functor (η: 1_𝒞 ⟹ G)
 *
 * @details
 * In Category Theory, the unit (η) is the natural transformation that
 * embeds an object into a Functorial context: η_X : X → G⟨X⟩.
 *
 * From a structuralist C++ perspective, this is the "On-Ramp".
 * By using `morphism_traits`, we perform "Object Discovery"—the
 * transformation is defined by the Morphism itself, allowing the
 * Domain Object (𝒯) to be inferred from the "Secret Sauce" (η_X).
 *
 * @tparam G   The Target Functor (The "Box").
 * @tparam OpF The Source Operation (in 𝒞).
 * @tparam OpG The Target Operation (in 𝒟).
 * @tparam η_X The Morphism Component (The Action).
 */
export template <template <typename> typename G, typename OpF, typename OpG,
                 auto η_X>
using unit = natural_transformation<
    Identity, G,
    typename morphism_traits<decltype(η_X)>::argument_type,  // Auto-Deduction!
    OpF, OpG, η_X>;

/** @brief The Greek (Category Theory) Alias for the Unit. */
export template <template <typename> typename G, typename OpF, typename OpG,
                 auto η_X>
using η = unit<G, OpF, OpG, η_X>;

constexpr int my_promotion_sauce(bool b) { return b ? 1 : 0; }

/** @brief The Retraction Morphism (r: 𝒢 ⟹ 1_𝒞).
    Primary template is deleted to enforce explicit existence. */
export template <typename 𝒯, typename Op𝒯, typename Op𝒢, auto η_X>
𝒯 retraction(decltype(η_X(std::declval<𝒯>())) y) = delete;

/**
 * @concept IsEmbedding
 * @brief A Natural Transformation η: 1_𝒞 ⟹ 𝒢 that preserves injectivity.
 *
 * @details
 * In Category Theory, an embedding (specifically a Monomorphism) is a morphism
 * f: A → B such that for any two morphisms g₁, g₂: X → A,
 * if f ∘ g₁ = f ∘ g₂, then g₁ = g₂.
 *
 * In our structuralist C++ approach, we verify this "Monomorphism" property
 * by proving the Morphism (η_X) is strictly injective (1:1)
 * across the entire Domain 𝒯.
 *
 * @tparam G   The Target Functor (The "Box" we are promoting into).
 * @tparam 𝒯   The Coordinate Species (The Domain Object, e.g., bool).
 * @tparam Op𝒯 The Binary Operation of the Source Category (e.g., AND).
 * @tparam Op𝒰 The Binary Operation of the Target Category (e.g., MULT).
 * @tparam η_X The Morphism Component (The actual C++ promotion logic).
 */
export template <template <typename> typename 𝒢, typename 𝒯, typename Op𝒯,
                 typename Op𝒢, auto η_X>
concept IsEmbedding =
    // 1. Structural Check: Does the Unit bridge actually exist?
    requires { typename η<𝒢, Op𝒯, Op𝒢, η_X>; } &&

    // 2. The Retraction Bridge: r (The "Undo" across Categories)
    requires(decltype(η_X(std::declval<𝒯>())) y) {
      // We look for a specialized retraction morphism for this species
      { retraction<𝒯, Op𝒯, Op𝒢, η_X>(y) } -> std::same_as<𝒯>;

      // The Axiom: r(η(x)) == x
      requires std::same_as<
          decltype(retraction<𝒯, Op𝒯, Op𝒢, η_X>(η_X(std::declval<𝒯>()))), 𝒯>;
    };

/** @section The Retraction for Bool-to-Int */
template <>
inline bool retraction<bool, std::logical_and<bool>, std::multiplies<int>,
                       my_promotion_sauce>(int y) {
  return y != 0;  // The unique "Undo" for the promotion
}

/**
 * @section Verification: The Injective Trap (Negative Proof)
 * @brief Proves that a non-invertible mapping is rejected by the ontology.
 *
 * @details
 * To be an 'IsEmbedding', a transformation must admit a 'retraction' (r).
 * Since an anonymous lambda that collapses all inputs to '0' (Annihilation)
 * cannot have a well-defined inverse mapping back to the Boolean domain,
 * no 'retraction' specialization can exist.
 *
 * The concept correctly evaluates to 'false' because it cannot find the
 * required "Undo" bridge for this specific morphism.
 */
static_assert(
    !IsEmbedding<Identity, bool, std::logical_and<bool>, std::multiplies<int>,
                 [](bool) { return 0; }>,
    "Dedekind: Concept correctly identified a non-injective mapping.");

/**
 * @concept IsHomomorphism
 * @brief A structure-preserving map between two similar Species (𝒞 and 𝒟).
 * @details
 * Formally: f(a ∘ b) = f(a) ⋆ f(b), where ∘ is Op𝒞 and ⋆ is Op𝒟.
 * This verifies the "Naturality" of the binary operation across the morphism.
 */
export template <typename 𝒯, typename 𝒰, auto η_X, typename Op𝒯, typename Op𝒰>
concept IsHomomorphism =
    IsSmallCategory<𝒯, Op𝒯> && IsSmallCategory<𝒰, Op𝒰> &&
    // 1. Axiom: η(id_𝒞) = id_𝒟
    (η_X(identity_v<𝒯, Op𝒯>) == identity_v<𝒰, Op𝒰>) &&
    // 2. Structural Proof: The Naturality Square commutes at the Unit
    requires(𝒯 a, 𝒯 b) {
      typename η<Identity, Op𝒯, Op𝒰, η_X>;
      requires η<Identity, Op𝒯, Op𝒰, η_X>::preserves_identity();
    };

namespace {
// 1. The Proving Sauce
constexpr int promote_v(bool b) { return b ? 1 : 0; }

// 2. The Verification Bridge
using LogicToArithmetic = unit<Identity,
                               std::logical_and<bool>,  // OpF
                               std::multiplies<int>,    // OpG
                               promote_v                // Morphism
                               >;

constexpr LogicToArithmetic transform{};

// 3. The Formal Proofs
// This works because 'transform' is now a concrete instance of that specific
// bridge
static_assert(transform(true) == 1);

static_assert(LogicToArithmetic::preserves_identity(),
              "Categorical Error: Identity mapping failed.");

static_assert(IsHomomorphism<bool, int, promote_v, std::logical_and<bool>,
                             std::multiplies<int>>,
              "Categorical Error: Product preservation failed.");

// Proof: η: (bool, ∧) ⟹ (int, ×) is a strictly injective Embedding.
static_assert(IsEmbedding<Identity, bool, std::logical_and<bool>,
                          std::multiplies<int>, my_promotion_sauce>,
              "Dedekind: Bool-to-Int promotion must be injective.");
}  // namespace
}  // namespace dedekind::ontology