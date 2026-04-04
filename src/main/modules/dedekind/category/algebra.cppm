/**
 * @file category:total.cppm
 * @partition :total
 * @brief Level 0.2: The Laws of Composition (The Algebraic Hierarchy).
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
 * @copyright 2026 The Dedekind Authors
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:algebra;

import :species;

namespace dedekind::category {

/** @concept IsMagma: T × T → T (The Base Total Species) */
export template <typename T, typename Op>
concept IsMagma = requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
};

/**
 * @concept IsUnitalMagma
 * @brief Level 1.1: A Magma with a global Identity (0 or 1).
 */
export template <typename T, typename Op>
concept IsUnitalMagma = IsMagma<T, Op> && HasIdentity<T, Op>;

/**
 * @concept IsLoop
 * @brief Level 1.2: A UnitalMagma with Inverses (-x).
 * @note "A Group without the Associativity Axiom."
 */
export template <typename T, typename Op>
concept IsLoop = IsUnitalMagma<T, Op> && IsInvertible<T, Op>;

/** @concept IsSemigroup: Associative Magma */
export template <typename T, typename Op>
concept IsSemigroup = IsMagma<T, Op> && IsAssociative<T, Op>;

/** @concept IsMonoid: Semigroup + Identity */
export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && IsUnitalMagma<T, Op>;

/** @concept IsCommutativeMonoid: Monoid + Symmetry */
export template <typename T, typename Op>
concept IsCommutativeMonoid = IsMonoid<T, Op> && IsCommutative<T, Op>;

/** @concept IsGroup: Monoid + Inverse (The Perfect Symmetry) */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && IsLoop<T, Op>;

/** @concept IsAbelianGroup: Commutative Group */
export template <typename T, typename Op>
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutative<T, Op>;

/** @section Truth_Anchors */

// Strictly True: Logical operators on bool are Total Commutative Monoids.
static_assert(IsCommutativeMonoid<bool, std::logical_or<bool>>);
static_assert(IsCommutativeMonoid<bool, std::logical_and<bool>>);

// Strictly True: Unsigned addition is a Total Abelian Group (Z/2^nZ).
static_assert(IsAbelianGroup<unsigned int, std::plus<unsigned int>>);

// Strictly True: Multiplication on integers is a Total Commutative Monoid.
// It is NOT a group (no integer inverse for 2).
static_assert(IsCommutativeMonoid<int, std::multiplies<int>>);

// The "Honest" Rejection: Signed addition is NOT a Total Magma
// in this partition because of UB/Overflow (Partiality).
static_assert(!IsMagma<int, std::plus<int>>);

/** @concept IsRig: Semiring without Negatives (Addition is a Monoid) */
export template <typename T, typename Add, typename Mult>
concept IsRig = IsCommutativeMonoid<T, Add> && IsMonoid<T, Mult> &&
                IsDistributive<T, Add, Mult>;

/** @concept IsRng: Ring without Identity (Multiplication is a Semigroup) */
export template <typename T, typename Add, typename Mult>
concept IsRng = IsAbelianGroup<T, Add> && IsSemigroup<T, Mult> &&
                IsDistributive<T, Add, Mult>;

/** @concept IsSemiring: General Multi-operation Species */
export template <typename T, typename Add, typename Mult>
concept IsSemiring = IsRig<T, Add, Mult>;

/** @concept IsRing: The Perfect Species (Rng + Rig) */
export template <typename T, typename Add, typename Mult>
concept IsRing = IsRig<T, Add, Mult> && IsRng<T, Add, Mult>;

/** @section Multi_Operation_Anchors */

// Strictly True: bool with OR/AND is a Rig (but not a Ring, no subtraction).
static_assert(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>);

// Strictly True: unsigned int is a Rig (no negative integers).
static_assert(IsRig<unsigned int, std::plus<unsigned int>,
                    std::multiplies<unsigned int>>);

// Strictly True: int with modular addition/multiplication is a Ring.
// (Assuming we use a modular_plus to stay in the :total partition).
static_assert(IsRing<unsigned int, std::plus<unsigned int>,
                     std::multiplies<unsigned int>>);

/** @concept IsJoinSemilattice */
export template <typename T, typename Op>
concept IsJoinSemilattice = IsCommutativeMonoid<T, Op> && IsIdempotent<T, Op>;

/** @concept IsMeetSemilattice */
export template <typename T, typename Op>
concept IsMeetSemilattice = IsCommutativeMonoid<T, Op> && IsIdempotent<T, Op>;

/** @concept IsLattice */
export template <typename T, typename Join, typename Meet>
concept IsLattice = IsJoinSemilattice<T, Join> && IsMeetSemilattice<T, Meet>;

/** @section Idempotency_Anchors */

// bool is a Distributive Lattice (AND/OR are both idempotent)
static_assert(IsLattice<bool, std::logical_or<bool>, std::logical_and<bool>>);

// extrema as Lattices
static_assert(IsJoinSemilattice<int, decltype([](int a, int b) {
                                  return std::max(a, b);
                                })>);

}  // namespace dedekind::category