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
  // 1. Structural Tags: The Morphism must announce its Species.
  typename F::Domain;
  typename F::Codomain;
  requires std::same_as<typename F::Domain, A>;
  requires std::same_as<typename F::Codomain, B>;

  // 2. Functional Action: The implementation must match the tags.
  { f(std::forward<A>(x)) } -> std::same_as<B>;
};

/**
 * @concept IsEndomorphism
 * @brief Proposition: A Morphism where Domain ≡ Codomain (f: A -> A).
 */
export template <typename F, typename T>
concept IsEndomorphism = IsArrow<F, T, T>;

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

/** @section The Morphism Factory: arrow <A, B> (f) */
export template <typename A, typename B, typename F>
constexpr auto arrow(F&& f) {
  return Morphism<A, B, std::decay_t<F>>{std::forward<F>(f)};
}

/**
 * @section Endomorphisms (f: A -> A)
 * @brief A specialized Morphism where Domain and Codomain are identical.
 */
export template <typename A, typename Impl>
using Endomorphism = Morphism<A, A, Impl>;

/** @brief The Endomorphism Factory: endo<A>(f) */
export template <typename A, typename F>
constexpr auto endo(F&& f) {
  return arrow<A, A>(std::forward<F>(f));
}

/** @section Arrow Factory Verification: Tagging & Species Integrity */

// 1. Proof: arrow<A, B> correctly tags a standard function object.
using Negate = std::negate<int>;
using TaggedNegate = decltype(endo<int>(Negate{}));

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
static_assert(endo<int>([](int x) { return x * 2; })(21) == 42,
              "Arrow Factory: Action check failed for anonymous lambda.");

// The Box (The "Cement")
template <typename T>
struct Box final {
  T value;
  bool operator==(const Box&) const = default;
};

/** @brief The Unit/Pure Factory: Lifts a raw value into the Box context. */
template <typename T>
constexpr auto pure(T&& value) {
  return Box<std::decay_t<T>>{std::forward<T>(value)};
}

/**
 * @section Functor Mapping (fmap: F(f))
 * @theorem Morphism Mapping
 * @brief Lifts a Morphism f: A -> B into the Functorial world F<A> -> F<B>.
 */
export template <template <typename> typename F, typename Arrow,
                 typename A = typename Arrow::Domain,
                 typename B = typename Arrow::Codomain>
  requires IsArrow<Arrow, A, B> && std::same_as<F<A>, Box<A>>
constexpr auto fmap(Arrow f) {
  // We return a new Morphism that acts on Boxes.
  return arrow<Box<A>, Box<B>>([f](Box<A> b) { return Box<B>{f(b.value)}; });
}

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

/** @section Verification: The Identity Law (F(id_X) = id_F<X>) */

// Proof: Lifting the identity morphism on 'int' gives us an arrow on
// 'Box<int>'.
static_assert(IsArrow<decltype(fmap<Box>(id<int>())), Box<int>, Box<int>>,
              "Identity Law: fmap(id) must preserve the Boxed species.");

// Proof: The lifted Negate morphism correctly transforms a Boxed value.
static_assert(
    fmap<Box>(endo<int>(std::negate<int>{}))(Box<int>{42}).value == -42,
    "Action: fmap(f) must preserve the underlying machine logic.");

/** @section Identity_Verification: The Unit Laws */

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
    IsArrow<decltype(endo<int>(std::negate<int>{}) >> id<int>()), int,
            int>,
    "Unit Law: id_B must be a right-identity for morphisms into B.");

// 2. Proof: Cross-Species Identity
// id_Z combined with a Z -> B bridge must result in a Z -> B bridge
static_assert(
    IsArrow<decltype(id<int>() >> arrow<int, bool>(IsPositive{})), int, bool>,
    "Unit Law: Identity must preserve the bridge from Z to B.");

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
export template <typename Eta, template <typename> typename F,
                 template <typename> typename G, typename T, typename U,
                 typename OpT, typename OpU>
concept IsNaturalTransformation =
    IsFunctor<F, T, OpT, T, OpT> && IsFunctor<G, U, OpU, U, OpU> &&
    IsArrow<Eta, F<T>, G<U>>;

/**
 * @struct Naturality
 * @witness The Proof-Object for the IsNaturalTransformation Theorem.
 *
 * @details
 * This structure serves as the formal inhabitant (the witness) of the
 * Naturality proposition. It does not perform discovery or lifting;
 * it simply carries a verified Morphism across a functorial bridge.
 *
 * Its role is to:
 * 1. Hold a pre-verified Morphism (η_X) that maps the Species.
 * 2. Define the Functorial Domain (F⟨T⟩) and Codomain (G⟨U⟩).
 * 3. Provide the Identity Preservation Lemma as a static property of the
 * witness.
 *
 * @tparam F    The Source Functor context.
 * @tparam G    The Target Functor context.
 * @tparam T    The source Species (The object).
 * @tparam U    The target Species (The object).
 * @tparam OpF  The Operation of the source category.
 * @tparam OpG  The Operation of the target category.
 * @tparam η_X  The Verified Morphism (The 'Secret Sauce' already lifted to an
 * Arrow).
 */
export template <template <typename> typename F, template <typename> typename G,
                 typename T, typename U, typename OpF, typename OpG, auto η_X>
  requires IsEndofunctor<F, T, OpF> && IsEndofunctor<G, U, OpG> &&
           IsArrow<decltype(η_X), F<T>, G<U>>
struct Naturality final {
  using Domain = F<T>;
  using Codomain = G<U>;

  /** @brief Execution: Invokes the verified Morphism η_X. */
  constexpr Codomain operator()(Domain x) const noexcept { return η_X(x); }

  /** @brief Lemma: η(id_F) = id_G (Verified via the internal Morphism). */
  static constexpr bool preserves_identity() noexcept {
    return η_X(identity_v<T, OpF>) == identity_v<U, OpG>;
  }
};

/** @section Internal_Morphism_Traits (Private to :category) */

// Primary template: Redirects to member function pointer if T is a class
template <typename T>
struct morphism_traits : morphism_traits<decltype(&T::operator())> {};

// Specialization for const member functions (Standard Lambdas)
template <typename C, typename R, typename A>
struct morphism_traits<R (C::*)(A) const> {
  using argument_type = A;
  using result_type = R;
};

// Specialization for non-const member functions (Mutable Lambdas)
template <typename C, typename R, typename A>
struct morphism_traits<R (C::*)(A)> {
  using argument_type = A;
  using result_type = R;
};

// Specialization for raw function pointers
template <typename R, typename A>
struct morphism_traits<R (*)(A)> {
  using argument_type = A;
  using result_type = R;
};

// Specialization for raw function types (Final fallback)
template <typename R, typename A>
struct morphism_traits<R(A)> {
  using argument_type = A;
  using result_type = R;
};

/** @section Verification: Morphism Discovery */

// 1. Proof: Discovery for raw function pointers.
constexpr bool raw_func(int) { return true; }
static_assert(
    std::same_as<morphism_traits<decltype(&raw_func)>::argument_type, int>,
    "Discovery: Failed to extract argument from raw function pointer.");

// 2. Proof: Discovery for anonymous lambdas.
using IsZero = decltype([](int x) { return x == 0; });
static_assert(std::same_as<morphism_traits<IsZero>::argument_type, int>,
              "Discovery: Failed to extract argument from lambda.");

// 3. Proof: Discovery for mutable lambdas (Stateful).
using Counter = decltype([i = 0](int x) mutable { return x + i++; });
static_assert(std::same_as<morphism_traits<Counter>::argument_type, int>,
              "Discovery: Failed to extract argument from mutable lambda.");

/** @brief The Functorial Bridge Tag. */
template <template <typename> typename F>
struct fmap_tag {};

/** @brief Global witness for Box mapping. */
export template <typename T = void> // Template to keep it header-friendly
constexpr auto Boxed = fmap_tag<Box>{};

/** @brief Postfix Operator: arrow >> Boxed */
export template <typename Arrow, template <typename> typename F>
  requires IsArrow<Arrow, typename Arrow::Domain, typename Arrow::Codomain>
constexpr auto operator>>(Arrow f, fmap_tag<F>) {
  return fmap<F>(f);
}

/** @brief The Unit/Pure Tag for value lifting. */
template <template <typename> typename F>
struct into_tag {};

/** @brief Global witness for Box entry. */
export template <typename T = void>
constexpr auto into = into_tag<Box>{};

/** @brief Postfix Operator: value >> into<F> */
export template <typename T, template <typename> typename F>
constexpr auto operator>>(T&& value, into_tag<F>) {
  // Uses your existing 'pure' or explicit constructor
  return F<std::decay_t<T>>{std::forward<T>(value)};
}

/** 
 * @section The Action Bridge (Value >> Arrow)
 * @brief Allows a value to be applied to a Morphism using the pipe operator.
 * @details This completes the 'Streaming' syntax: x >> f ≡ f(x).
 */
export template <typename T, typename Arrow,
                 typename A = typename Arrow::Domain,
                 typename B = typename Arrow::Codomain>
  requires IsArrow<Arrow, A, B> && std::convertible_to<T, A>
constexpr auto operator>>(T&& value, Arrow&& f) {
  return f(std::forward<T>(value));
}

/** @section Pipeline_Verification: From Brick to Box */

// The Logic (Brick)
constexpr auto increment = endo<int>([](int x) { return x + 1; });

// The Flow: Value -> Box -> Morphism-on-Box
// Reading: "Take 41, put it INTO a Box, then apply increment BOXED."
static_assert((41 >> into<> >> (increment >> Boxed<>)).value == 42,
              "Pipeline: The linear flow from value to boxed result failed.");

// The Composition Flow:
// Reading: "Take 10, put it INTO a Box, then increment it, then negate it."
constexpr auto negate = endo<int>(std::negate<int>{});

static_assert((10 >> into<> >> (increment >> Boxed<>) >> (negate >> Boxed<>)).value == -11,
              "Pipeline: Multi-stage functorial composition failed.");

/**
 * @section The Unit Proof (η: 1_𝒞 ⟹ G)
 *
 * @details
 * Under the Curry-Howard correspondence, this alias is the 'Assembler'.
 * It constructs the 'Naturality' witness by:
 * 1. Automatic Discovery: Extracting T and U from the 'Secret Sauce' (η_X).
 * 2. Formal Lifting: Wrapping the raw action into a tagged 'arrow'.
 * 3. Inhabitation: Instantiating the 'Naturality' struct with verified parts.
 *
 * @tparam G   The Target Functor (The "Box").
 * @tparam OpF The Source Operation (in 𝒞).
 * @tparam OpG The Target Operation (in 𝒟).
 * @tparam η_X The Morphism Component (The raw C++ action).
 */
export template <template <typename> typename F, template <typename> typename G,
                 typename OpF, typename OpG, auto η_X>
using unit = Naturality<
    F, G,
    typename morphism_traits<decltype(η_X)>::argument_type,  // T
    typename morphism_traits<decltype(η_X)>::result_type,    // U
    OpF, OpG,
    // The Lifting: Converts raw lambda into a tagged Arrow (Proof-Object)
    arrow<typename morphism_traits<decltype(η_X)>::argument_type,
          typename morphism_traits<decltype(η_X)>::result_type>(η_X)>;

/** @section Canonical_Embeddings: The One-Liner On-Ramps */

// 1. Logic -> Character (B ↪ Z/256Z)
export using η_bool_char =
    unit<Identity, Identity, std::logical_and<bool>, std::multiplies<char>,
         [](bool b) constexpr -> char { return b ? char(1) : char(0); }>;

// We verify that our construction (η_bool_char) satisfies
// the Naturality Theorem (IsNaturalTransformation).
// 𝒯 = bool, Op𝒯 = logical_and<bool>, Op𝒰 = plus<char>
static_assert(
    IsNaturalTransformation<η_bool_char, Identity, Identity, bool, char,
                            std::logical_and<bool>, std::multiplies<char>> &&
        η_bool_char::preserves_identity(),
    "Theorem Failure: η_bool_char is not a valid Natural Transformation (B ⟹ "
    "Z/256Z).");

// 2. Character -> Unsigned (Z/256Z ↪ Z_u)
export using η_char_uint =
    unit<Identity, Identity, std::plus<char>, std::plus<unsigned int>,
         [](char c) {
           return static_cast<unsigned int>(static_cast<unsigned char>(c));
         }>;

// We verify that our construction (η_bool_char) satisfies
// the Naturality Theorem (IsNaturalTransformation).
// 𝒯 = bool, Op𝒯 = logical_and<bool>, Op𝒰 = plus<char>
static_assert(
    IsNaturalTransformation<η_char_uint, Identity, Identity, char, unsigned int,
                            std::plus<char>, std::plus<unsigned int>> &&
        η_char_uint::preserves_identity(),
    "Theorem Failure: η_char_uint is not a valid Natural Transformation "
    "(Z/256Z ⟹ Z_u).");

/**
 * @brief Proof: Transitive Composition of Embeddings.
 * We compose the two Natural Transformations into a single 'Long Bridge'.
 * η_bool_uint = η_char_uint ∘ η_bool_char
 */
constexpr auto bool_to_uint = (η_bool_char{}) >> (η_char_uint{});

// 1. Proof: The 'Long Bridge' is a valid Arrow (B -> Z_u).
static_assert(IsArrow<decltype(bool_to_uint), bool, unsigned int>,
              "Transitivity: The composed promotion must be a valid Arrow from "
              "bool to uint.");

// 2. Action Proof: Extensional Equality across the chain.
// We verify that 'true' maps to '1' through the entire pipeline.
static_assert(bool_to_uint(true) == 1u,
              "Action: The escalator must preserve the 'True' identity (1) "
              "across species.");

// 3. Action Proof: The 'False' identity (0) preservation.
static_assert(bool_to_uint(false) == 0u,
              "Action: The escalator must preserve the 'False' identity (0) "
              "across species.");

/** @brief The Canonical Component η_X for a Functor F and Species T. */
export template <template <typename> typename F, typename T>
constexpr F<T> η_component(T x) = delete;

/** @brief Specialization: Identity's η_X is the Identity mapping. */
template <typename T>
constexpr Identity<T> η_component(T x) {
  return x;
}

/**
 * @section The Monadic Unit (η: Id ⟹ F)
 * @brief The CANONICAL unit for a verified Monad.
 * @details This is the 'Standard On-Ramp' for Functor F and Species T.
 *          Signature: (F, T, OpT). It finds the η_component automatically.
 */
export template <template <typename> typename F, typename T, typename OpT>
using η = unit<F, OpT, OpT, η_component<F, T>>;

/**
 * @brief The Monadic Multiplication (μ_X: F⟨F⟨X⟩⟩ → F⟨X⟩)
 * @details In the Category of Endofunctors, the Monad is a Monoid.
 *          This 'Multiplication' collapses the nested composition
 *          of the functor back into a single layer.
 */
/*
export template <template <typename> typename F, typename T>
  requires(!std::same_as<F<F<T>>, F<T>>)  // The "Structure Gap" Constraint
constexpr F<T> multiplication(F<F<T>> box);*/

/** @brief Specialization: The Identity Monad's Multiplication. */
template <typename T>
constexpr Identity<T> multiplication(Identity<T> box) {
  return box;  // Unwrap the nested Identity
}

/**
 * @section The Monadic Multiplication (μ: F ∘ F ⟹ F)
 * @brief The CANONICAL multiplication (Join) for a verified Monad.
 * @details Signature: (F, T, OpT). It finds the multiplication logic
 * automatically.
 */
export template <template <typename> typename F, typename T, typename OpT>
using μ = natural_transformation<F, F, T, OpT, OpT, [](auto box) constexpr {
  return multiplication<F, T>(box);
}>;

/**
 * @concept η (The Monadic Unit)
 * @brief Formalizes the "On-Ramp" signature: T ⟹ F⟨T⟩.
 */
export template <typename Transform, template <typename> typename F, typename T>
concept is_η = IsArrow<Transform, T, F<T>>;

/**
 * @concept μ (The Monadic Multiplication)
 * @brief Formalizes the "Flattening" signature: F⟨F⟨T⟩⟩ ⟹ F⟨T⟩.
 */
export template <typename Transform, template <typename> typename F, typename T>
concept is_μ = IsArrow<Transform, F<F<T>>, F<T>>;

/**
 * @section Monad_as_Monoid (Explicit Definition)
 * We bridge the gap:
 *   η (Unit)           <--> identity_v (Monoid Unit)
 *   μ (Multiplication) <--> Op         (Monoid Operation)
 */
export template <template <typename> typename F, typename T, typename OpT>
concept IsMonad = IsEndofunctor<F, T, OpT> && requires(F<F<T>> nested, T x) {
  IsEndofunctor<F, T, OpT> && requires {
    // 1. Structural Verification: Do the canonical aliases exist?
    typename η<F, T, OpT>;
    typename μ<F, T, OpT>;

    // 2. Role Verification: Do these types satisfy the Monadic Concepts?
    requires is_η<η<F, T, OpT>, F, T>;
    requires is_μ<μ<F, T, OpT>, F, T>;
  };
  /**
   * @brief Axiom 1: The Monoid Unit (η)
   * Maps the object to the 'Box' (T -> F<T>).
   */
  //{ unit<F>(x) } -> std::same_as<F<T>>;

  /**
   * @brief η: Id ⟹ F (The Natural Unit)
   * We prove the Unit is a Natural Transformation by instantiating
   * your alias with the canonical component.
   */
  // typename unit<F, OpT, OpT, η_component<F, T>>;

  /**
   * @brief Axiom 2: The Monoid Multiplication (μ)
   * Collapses the 'Tensor Product' of the functor with itself (F ∘ F -> F).
   */
  // { multiplication<F, T>(nested) } -> std::same_as<F<T>>;
};

/**
 * @section Comonadic_Morphisms: Extract (ε) and Duplicate (δ)
 */

/** @brief ε: W⟨T⟩ → T (The Core Extraction) */
export template <template <typename> typename W, typename T>
constexpr T extract(W<T> box);

/** @brief δ: W⟨T⟩ → W⟨W⟨T⟩⟩ (The Contextual Duplication) */
export template <template <typename> typename W, typename T>
constexpr W<W<T>> duplicate(W<T> box);

/** @section Identity_Comonad_Specialization */
template <typename T>
constexpr T extract(Identity<T> box) {
  return box;
}

template <typename T>
constexpr Identity<Identity<T>> duplicate(Identity<T> box) {
  return box;
}

/**
 * @concept IsComonad
 * @brief The Dual of a Monad: A Functor with Extract and Duplicate.
 */
export template <template <typename> typename W, typename T, typename OpT>
concept IsComonad = IsEndofunctor<W, T, OpT> && requires(W<T> box) {
  { extract<W, T>(box) } -> std::same_as<T>;
  { duplicate<W, T>(box) } -> std::same_as<W<W<T>>>;
};

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
    requires { typename unit<𝒢, Op𝒯, Op𝒢, η_X>; } &&

    // 2. The Retraction Bridge: r (The "Undo" across Categories)
    requires(decltype(η_X(std::declval<𝒯>())) y) {
      // We look for a specialized retraction morphism for this species
      { retraction<𝒯, Op𝒯, Op𝒢, η_X>(y) } -> std::same_as<𝒯>;

      // The Axiom: r(η(x)) == x
      requires std::same_as<
          decltype(retraction<𝒯, Op𝒯, Op𝒢, η_X>(η_X(std::declval<𝒯>()))), 𝒯>;
    };

constexpr int my_promotion_sauce(bool b) { return b ? 1 : 0; }
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
export template <typename T, typename U, auto η_X, typename OpT, typename OpU>
concept IsHomomorphism =
    IsSmallCategory<T, OpT> && IsSmallCategory<U, OpU> &&
    requires(T a, T b) {
      // We only check if the expression is VALID.
      { η_X(OpT{}(a, b)) } -> std::same_as<U>;
      { OpU{}(η_X(a), η_X(b)) } -> std::same_as<U>;
    } &&
    // Identity Preservation (This works because identity_v has a known value)
    (η_X(identity_v<T, OpT>) == identity_v<U, OpU>);

/** @section Homomorphism Verification: Boolean -> Integer */
namespace {
constexpr int promote_v(bool b) { return b ? 1 : 0; }

// 1. Proof: Structural Homomorphism exists.
static_assert(IsHomomorphism<bool, int, promote_v, std::logical_and<bool>,
                             std::multiplies<int>>,
              "Structural Error: Boolean-to-Integer bridge is broken.");

// 2. Proof: Binary Action Axiom (Manual check of the truth table)
// f(a ∘ b) == f(a) ⋆ f(b)
static_assert(promote_v(true && false) == (promote_v(true) * promote_v(false)));
static_assert(promote_v(true && true) == (promote_v(true) * promote_v(true)));
static_assert(promote_v(false && false) ==
              (promote_v(false) * promote_v(false)));
}  // namespace

namespace {

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