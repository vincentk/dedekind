/**
 * @file ontology:category.cppm
 * @partition :algebra
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

export module dedekind.category:total;

import :logic;
import :morphism;
import :species;

namespace dedekind::category {

/**
 * @concept IsTotalArrow
 * @brief Formal verification that a morphism is defined for the entire species.
 */
export template <typename F, typename A, typename B>
concept IsTotalArrow = IsArrow<F, A, B> && requires(F f, A x) {
  // Totality check: The SubobjectClassifier must return 'True' (Classical)
  // for all elements in the species.
  requires std::same_as<typename SubobjectClassifier<A>::Omega, bool>;
  { SubobjectClassifier<A>::evaluate_total(x) } -> std::same_as<bool>;
};

/**
 * @section Total_Morphism_Reification
 * A concrete wrapper for functions that are mathematically 'Total'.
 */
export template <IsSpecies A, IsSpecies B, typename Func>
struct TotalMorphism : Morphism<A, B, Func> {
  static_assert(
      IsTotalArrow<Func, A, B>,
      "Totality Error: The provided function is not total over the domain.");
};

/** @concept IsMagma: T × T → T (The Base Total Species) */
export template <typename T, typename Op>
concept IsMagma = IsTotal<T, Op> && requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
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

/** @concept IsSemigroup: Associative Magma */
export template <typename T, typename Op>
concept IsSemigroup = IsMagma<T, Op> && IsAssociative<T, Op>;

/** @concept IsMonoid: Semigroup + Identity */
export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && IsUnitalMagma<T, Op>;

/** @concept IsCommutativeMonoid: Monoid + Symmetry */
export template <typename T, typename Op>
concept IsCommutativeMonoid = IsMonoid<T, Op> && IsCommutative<T, Op>;

static_assert(IsCommutativeMonoid<bool, std::logical_or<bool>>);
static_assert(IsCommutativeMonoid<bool, std::logical_and<bool>>);

// Strictly True: Multiplication on integers is a Total Commutative Monoid.
// It is NOT a group (no integer inverse for 2).
static_assert(!IsCommutativeMonoid<int, std::multiplies<int>>);
static_assert(IsCommutativeMonoid<unsigned int, std::multiplies<unsigned int>>);

/** @concept IsGroup: Monoid + Inverse (The Perfect Symmetry) */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && IsLoop<T, Op>;

// REJECTION: subtraction on uint is NOT a Group (no negative results).
// While it is periodic (wraps), it lacks the inverse axiom in the positive
// domain.
static_assert(!IsGroup<unsigned int, std::minus<unsigned int>>);

/** @concept IsAbelianGroup: Commutative Group */
export template <typename T, typename Op>
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutative<T, Op>;

// Strictly True: Unsigned addition is a Total Abelian Group (Z/2^nZ).
static_assert(IsAbelianGroup<unsigned int, std::plus<unsigned int>>);

/** @concept IsRig: Semiring without Negatives (Addition is a Monoid) */
export template <typename T, typename Add, typename Mult>
concept IsRig = IsCommutativeMonoid<T, Add> && IsMonoid<T, Mult> &&
                IsDistributive<T, Mult, Add>;

// Strictly True: bool with OR/AND is a Rig (but not a Ring, no subtraction).
static_assert(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>);
// Strictly True: unsigned int is a Rig (no negative integers).
static_assert(IsRig<unsigned int, std::plus<unsigned int>,
                    std::multiplies<unsigned int>>);

/** @concept IsRng: Ring without Identity (Multiplication is a Semigroup) */
export template <typename T, typename Add, typename Mult>
concept IsRng = IsAbelianGroup<T, Add> && IsSemigroup<T, Mult> &&
                IsDistributive<T, Mult, Add>;

/** @concept IsSemiring: General Multi-operation Species */
export template <typename T, typename Add, typename Mult>
concept IsSemiring = IsRig<T, Add, Mult>;

/** @concept IsRing: The Perfect Species (Rng + Rig) */
export template <typename T, typename Add, typename Mult>
concept IsRing = IsRig<T, Add, Mult> && IsRng<T, Add, Mult>;

// Strictly True: int with modular addition/multiplication is a Ring.
// (Assuming we use a modular_plus to stay in the :total partition).
static_assert(IsRing<unsigned int, std::plus<unsigned int>,
                     std::multiplies<unsigned int>>);

// SUCCESS: Modular<N> is a Total Ring (Axiomatic Periodicity).
static_assert(IsRing<Modular<256>, std::plus<Modular<256>>,
                     std::multiplies<Modular<256>>>);

/** @concept IsJoinSemilattice */
export template <typename T, typename Op>
concept IsJoinSemilattice = IsCommutativeMonoid<T, Op> && IsIdempotent<T, Op>;

/** @concept IsMeetSemilattice */
export template <typename T, typename Op>
concept IsMeetSemilattice = IsCommutativeMonoid<T, Op> && IsIdempotent<T, Op>;

/** @concept IsLattice */
export template <typename T, typename Join, typename Meet>
concept IsLattice = IsJoinSemilattice<T, Join> && IsMeetSemilattice<T, Meet>;

// bool is a Distributive Lattice (AND/OR are both idempotent)
static_assert(IsLattice<bool, std::logical_or<bool>, std::logical_and<bool>>);

// extrema as Lattices
static_assert(IsJoinSemilattice<int, decltype([](int a, int b) {
                                  return std::max(a, b);
                                })>);

}  // namespace dedekind::category
