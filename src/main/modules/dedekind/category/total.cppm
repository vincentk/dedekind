/**
 * @file dedekind/category/total.cppm
 * @partition :total
 * @brief Level 1: The Laws of Total Algebra (The Algebraic Hierarchy).
 *
 * @section The_Categorical_Foundation
 * « Język jest aparatem wyznaczającym obraz świata. Struktura kategorii
 *   jest fundamentem wszelkiego poznania naukowego. »
 *  (Language is an apparatus that determines the image of the world.
 *   The structure of categories is the foundation of all scientific knowledge.)
 *  — Kazimierz Ajdukiewicz, 'Język i Poznanie' (Language and Cognition)
 *
 * @details
 * This partition defines the "Laws of Harmony" for a single species T under
 * a binary operation Op. Following the Polish School of Logic, we treat
 * these structures as species that "mature" as they gain axioms—moving
 * from the primal Magma to the perfect symmetry of an Abelian Group.
 *
 * @section Std_Namespace_Mappings
 * This partition asserts bidirectional mappings between algebraic hierarchy
 * concepts and standard C++ types:
 *
 * | Std type        | Highest algebraic structure                    |
 * |-----------------|------------------------------------------------|
 * | `bool`          | Rig (OR/AND), Distributive Lattice (OR/AND)    |
 * | `unsigned int`  | Abelian Group (+), Ring (+,*), Ring (XOR,AND)  |
 * | `int`           | Distributive Lattice (max,min)                 |
 * | `double`        | Distributive Lattice (max,min)                 |
 *
 * @copyright 2026 The Dedekind Authors
 *
 * @note "In dedekind.category:total, structure is clarified by explicit
 * composition and typed interfaces." (Module-specific documentation note for
 * maintainers.)
 *       -- dedekind maintainers
 */
module;

#include <algorithm>
#include <concepts>
#include <functional>

export module dedekind.category:total;

import :discrete;
import :pullback;
import :morphism;
import :species;

namespace dedekind::category {
/**
 * @concept IsTotalArrow
 * @brief A Morphism whose domain species admits no undefined (hazard) values.
 * @details ∀x ∈ Domain: f(x) is valid (not a hazard).
 *
 * A morphism is "Total" when it is defined for every element of its domain.
 * This corresponds to functions that cannot produce undefined behaviour (e.g.
 * overflow, division by zero). The criterion is structural: the domain species
 * must use `bool` as its logic type, which is only satisfied by species whose
 * arithmetic wraps predictably (e.g. `unsigned int`, `bool`).
 */
export template <typename F>
concept IsTotalArrow =
    IsArrow<F> && std::same_as<typename GetLogic<domain_t<F>>::type::Ω, bool>;

/**
 * @struct TotalMorphism
 * @brief A concrete wrapper asserting that a Morphism A → B is total.
 * @details Extends `Morphism<A, B, Func>` with a compile-time assertion that
 * the underlying function satisfies `IsTotalArrow`. This makes totality an
 * explicit, type-level guarantee rather than a convention.
 *
 * @tparam A    The Domain species (must be a total species, e.g. unsigned int).
 * @tparam B    The Codomain species.
 * @tparam Func The callable implementing the mapping.
 */
export template <IsSpecies A, IsSpecies B, typename Func>
struct TotalMorphism : Morphism<A, B, Func> {
  // Fix: IsTotalArrow now only takes the function type
  static_assert(
      IsTotalArrow<Morphism<A, B, Func>>,
      "Totality Error: The provided function is not total over the domain.");
};

// 3. Verify it is a Total Arrow (defined for all x in Domain)
static_assert(IsTotalArrow<decltype(zero<int, int>())>,
              "Totality Error: zero() must be total constant over its domain.");

// 3. Verify it is a Total Arrow (defined for all x in Domain)
static_assert(IsTotalArrow<decltype(unit<int, int>())>,
              "Totality Error: unit() must be total constant over its domain.");

/**
 * @concept IsMagma
 * @brief Level 1.0: T × T → T (The Base Total Species).
 * @details A Magma is the most primitive algebraic structure: a set T together
 * with a binary operation Op that is closed (the result stays in T) and total
 * (defined for every pair of inputs). No further axioms are required.
 *
 * In the Dedekind taxonomy, `unsigned int` with wrapping addition is a Magma,
 * while `int` with standard addition is not (signed overflow is undefined
 * behaviour and therefore a "hazard").
 */
export template <typename T, typename Op>
concept IsMagma = IsTotal<T, Op> && requires(T a, T b) {
  { Op{}(a, b) } -> std::convertible_to<T>;
};

// REJECTION: Signed addition is NOT a Magma (No Periodicity, No Idempotency).
// It is correctly identified as a Hazard/Partial function.
static_assert(!IsMagma<int, std::plus<int>>);

/**
 * @concept IsUnitalMagma
 * @brief Level 1.1: A Magma with a global Identity (0 or 1).
 */
export template <typename T, typename Op>
concept IsUnitalMagma = IsMagma<T, Op> && IsPointed<T, Op>;

/**
 * @concept IsLoop
 * @brief Level 1.2: A UnitalMagma with Inverses (-x).
 * @note "A Group without the Associativity Axiom."
 */
export template <typename T, typename Op>
concept IsLoop = IsUnitalMagma<T, Op> && IsInvertible<T, Op>;

/**
 * @concept IsSemigroup
 * @brief Level 1.3: An Associative Magma.
 * @details Adds associativity to the Magma: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c).
 * This is the foundational property that allows operations to be "chained"
 * without ambiguity, enabling sequential composition.
 */
export template <typename T, typename Op>
concept IsSemigroup = IsMagma<T, Op> && IsAssociative<T, Op>;

/**
 * @concept IsCommutativeSemigroup
 * @brief An Associative Magma where the operation is also commutative.
 * @details Extends `IsSemigroup` with the symmetry axiom: a ⊕ b = b ⊕ a.
 */
export template <typename T, typename Op>
concept IsCommutativeSemigroup = IsSemigroup<T, Op> && IsCommutative<T, Op>;

/**
 * @concept IsMonoid
 * @brief Level 1.4: A Semigroup with a two-sided Identity element.
 * @details Adds a neutral element `e` such that e ⊕ a = a = a ⊕ e for all a.
 * `bool` with `logical_or` is a Monoid (identity = `false`);
 * `unsigned int` with `plus` is a Monoid (identity = 0).
 */
export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && IsUnitalMagma<T, Op>;

/**
 * @concept IsCommutativeMonoid
 * @brief Level 1.5: A Monoid where the operation is commutative.
 * @details Both `bool` (OR, AND) and `unsigned int` (multiplication) satisfy
 * this level. Importantly, `unsigned int` with multiplication is a
 * CommutativeMonoid but not a Group (no multiplicative inverse for 2).
 */
export template <typename T, typename Op>
concept IsCommutativeMonoid = IsMonoid<T, Op> && IsCommutative<T, Op>;

static_assert(IsCommutativeMonoid<bool, std::logical_or<bool>>);
static_assert(IsCommutativeMonoid<bool, std::logical_and<bool>>);

// Strictly True: Multiplication on integers is a Total Commutative Monoid.
// It is NOT a group (no integer inverse for 2).
static_assert(!IsCommutativeMonoid<int, std::multiplies<int>>);
static_assert(IsCommutativeMonoid<unsigned int, std::multiplies<unsigned int>>);

/**
 * @concept IsGroup
 * @brief Level 1.6: A Monoid with two-sided Inverses (The Perfect Symmetry).
 * @details Every element a has a unique inverse a⁻¹ such that a ⊕ a⁻¹ = e.
 * `unsigned int` with addition is a Group (modular inverse exists for every
 * element in Z/2ⁿZ). Signed `int` with addition is not, as overflow is
 * undefined behaviour.
 */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && IsLoop<T, Op>;

// REJECTION: subtraction on uint is NOT a Group (no negative results).
// While it is periodic (wraps), it lacks the inverse axiom in the positive
// domain.
static_assert(!IsGroup<unsigned int, std::minus<unsigned int>>);

/**
 * @concept IsAbelianGroup
 * @brief Level 1.7: A Commutative Group.
 * @details `unsigned int` with addition (Z/2ⁿZ) is the canonical example:
 * addition wraps, every element has a modular inverse, and a + b = b + a.
 */
export template <typename T, typename Op>
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutative<T, Op>;

// Strictly True: Unsigned addition is a Total Abelian Group (Z/2^nZ).
static_assert(IsAbelianGroup<unsigned int, std::plus<unsigned int>>);

/**
 * @concept IsRig
 * @brief Level 2.1: A Semiring without Negatives (Addition is a Monoid).
 * @details A Rig ("Ring without negatives") provides two monoidal operations
 * that distribute over each other. `bool` with (OR, AND) and `unsigned int`
 * with (+, *) are both Rigs. Neither has an additive inverse, so they stop
 * short of a Ring.
 *
 * Textbook note: In standard algebra literature, `semiring` is often used for
 * this structure, while `rig` is the explicit mnemonic "ring without
 * negatives".
 */
export template <typename T, typename Add, typename Mult>
concept IsRig = IsCommutativeMonoid<T, Add> && IsMonoid<T, Mult> &&
                IsDistributive<T, Mult, Add>;

// Strictly True: bool with OR/AND is a Rig (but not a Ring, no subtraction).
static_assert(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>);
// Strictly True: unsigned int is a Rig (no negative integers).
static_assert(IsRig<unsigned int, std::plus<unsigned int>,
                    std::multiplies<unsigned int>>);

/**
 * @concept IsRng
 * @brief Level 2.2: A Ring without Identity (Multiplication is a Semigroup).
 * @details Provides an Abelian Group for addition and a Semigroup for
 * multiplication, with distribution. Called "Rng" (Ring without the
 * multiplicative identity "i").
 *
 * Textbook note: This is also commonly described as a "ring without unity".
 */
export template <typename T, typename Add, typename Mult>
concept IsRng = IsAbelianGroup<T, Add> && IsSemigroup<T, Mult> &&
                IsDistributive<T, Mult, Add>;

/**
 * @concept IsSemiring
 * @brief Alias for `IsRig`: a general multi-operation species.
 * @details The term "semiring" is used in some literature as a synonym for
 * "rig." In this library it is an exact alias for `IsRig`.
 */
export template <typename T, typename Add, typename Mult>
concept IsSemiring = IsRig<T, Add, Mult>;

/**
 * @concept IsRing
 * @brief Level 2.3: The Perfect Species (Rig ∩ Rng).
 * @details A Ring combines an Abelian Group for addition and a Monoid for
 * multiplication, with mutual distributivity. `unsigned int` with (+, *)
 * forms a Ring (arithmetic in Z/2ⁿZ). Signed `int` does not satisfy this
 * partition's totality requirements due to undefined overflow.
 */
export template <typename T, typename Add, typename Mult>
concept IsRing = IsRig<T, Add, Mult> && IsRng<T, Add, Mult>;

// Strictly True: int with modular addition/multiplication is a Ring.
// (Assuming we use a modular_plus to stay in the :total partition).
static_assert(IsRing<unsigned int, std::plus<unsigned int>,
                     std::multiplies<unsigned int>>);

// SUCCESS: Modular<N> is a Total Ring (Axiomatic Periodicity).
static_assert(IsRing<Modular<256>, std::plus<Modular<256>>,
                     std::multiplies<Modular<256>>>);

/**
 * @concept IsSemilattice
 * @brief Level 3.1: A commutative, idempotent Semigroup.
 * @details An operation is idempotent when a ⊕ a = a. Combined with
 * commutativity and associativity, this gives the structure of a semilattice.
 * `bool` with OR (or AND), and `int`/`double` with `max` (or `min`) are
 * canonical examples.
 */
export template <typename T, typename Op>
concept IsSemilattice = IsCommutativeSemigroup<T, Op> && IsIdempotent<T, Op>;

/**
 * @concept IsJoinSemilattice
 * @brief A Semilattice used as the "join" (least upper bound / ∨) half of a
 * Lattice. In `bool` this is `logical_or`; in `int`/`double` it is `max`.
 */
export template <typename T, typename Op>
concept IsJoinSemilattice = IsSemilattice<T, Op>;

/**
 * @concept IsMeetSemilattice
 * @brief A Semilattice used as the "meet" (greatest lower bound / ∧) half of a
 * Lattice. In `bool` this is `logical_and`; in `int`/`double` it is `min`.
 */
export template <typename T, typename Op>
concept IsMeetSemilattice = IsSemilattice<T, Op>;

/**
 * @concept IsLattice
 * @brief Level 3.2: A pair of semilattices satisfying the Absorption Laws.
 * @details Combines a JoinSemilattice and a MeetSemilattice with the
 * absorption identities: a ∨ (a ∧ b) = a and a ∧ (a ∨ b) = a.
 * `bool` with (OR, AND) and `int`/`double` with (max, min) are Lattices.
 */
export template <typename T, typename Join, typename Meet>
concept IsLattice = IsJoinSemilattice<T, Join> && IsMeetSemilattice<T, Meet> &&
                    IsAbsorptive<T, Join, Meet>;

/**
 * @concept IsDistributiveLattice
 * @brief Level 3.3: A Lattice where Join and Meet mutually distribute.
 * @details
 * Formal Laws:
 * 1. a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)  [Join over Meet]
 * 2. a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)  [Meet over Join]
 *
 * Std namespace mappings:
 * - `bool`   with (`std::logical_or`, `std::logical_and`) is a Distributive
 *            Lattice. This corresponds to the classical two-element Boolean
 *            lattice {false < true}.
 * - `int`    with (`std::ranges::max`, `std::ranges::min`) is a Distributive
 *            Lattice.  This corresponds to the total order on integers.
 * - `double` with (`std::ranges::max`, `std::ranges::min`) is similarly a
 *            Distributive Lattice.
 *
 * Note: (unsigned int, XOR, AND) is a Ring, not a Lattice, because XOR is not
 * idempotent.
 */
export template <typename T, typename Join, typename Meet>
concept IsDistributiveLattice =
    IsLattice<T, Join, Meet> && IsDistributive<T, Join, Meet> &&
    IsDistributive<T, Meet, Join>;

// bool is a Distributive Lattice (AND/OR are both idempotent)
static_assert(
    IsDistributiveLattice<bool, std::logical_or<bool>, std::logical_and<bool>>);

// Lattice laws for integers under max/min (Total Order).
static_assert(IsDistributiveLattice<int, decltype(std::ranges::max),
                                    decltype(std::ranges::min)>);

/** @section Boolean_Ring_Negative_Proof */

using XOR = std::bit_xor<unsigned int>;
using AND = std::bit_and<unsigned int>;

// This MUST be false because XOR is not idempotent.
static_assert(!IsIdempotent<unsigned int, XOR>);

// This MUST be false because a ^ (a & b) != a.
static_assert(
    !IsAbsorptive<unsigned int, XOR, AND>,
    "Taxonomy Error: Boolean Ring operations must NOT be absorptive.");

// The Final Verdict:
static_assert(!IsLattice<unsigned int, XOR, AND>,
              "Taxonomy Error: A Boolean Ring is a Ring, not a Lattice.");
}  // namespace dedekind::category
