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

export module dedekind.category:morphisms;

import :species;

namespace dedekind::category {

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

/** @brief The Unit/Pure Factory: Lifts a raw value into the Box context. */
export template <typename T>
constexpr auto pure(T&& value) {
  return Box<std::decay_t<T>>{std::forward<T>(value)};
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
concept IsGroupoid = IsSmallCategory<T, Op> && IsInvertible<T, Op>;

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
