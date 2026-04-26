/**
 * @file ontology:numbers.cppm
 * @brief Level 4: The Dictionary of Species (The Registry).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :numbers
 * @build_order 7
 * @dependency :algebra, :topology, :cardinalities
 *
 * @section Numbers: The Realization of the Soul
 * This partition is the final "Registry" of the ontology. It maps concrete
 * C++ types to their formal algebraic and topological identities.
 *
 * @details
 * We "Bless" the coordinate species by verifying their rungs on the ladder:
 * - IsNatural  : N (ℕ) - The Discrete Monoid.
 * - IsInteger  : Z (ℤ) - The Euclidean Group.
 * - IsRational : Q (ℚ) - The Countable Dense Field.
 * - IsReal     : R (ℝ) - The Continuous Dedekind-Complete Field.
 *
 * @section Structural_Mapping
 * This is where we perform the final 'Lifting'. We prove that 'int'
 * satisfies 'Group_ℤ' and that 'double' is a hardware-constrained
 * approximation of 'Field_ℝ'.
 *
 * @anchors C++ Fundamental Types: bool, char, int, long, float, double.
 *
 * Wikipedia: Number, Natural number, Integer, Rational number, Real number
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Having been introduced there to this art with an amazing method of
 * teaching by means of the nine figures of the Indians, I loved the knowledge
 * of such an art to such an extent above all other arts...that I learned with
 * very earnest application...anything to be studied concerning it and its
 * various methods used in Egypt, in Syria, in Greece, in Sicily, and in
 * Provence." — Leonardo Fibonacci, *Liber Abaci*, Prologus (~1202).
 */
module;

#include <concepts>
#include <functional>

export module dedekind.numbers:naturals;

import dedekind.algebra; // HasRingOperators / HasSemiringOperators / IsArithmeticRing (canonical-spine witnesses)
import dedekind.category;
import dedekind.order; // HasLatticeOperators (canonical-spine witnesses)
import dedekind.sets;
import :scalars;
import :booleans;

namespace dedekind::numbers {
using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @concept IsNatural
 * @brief Structural concept for a commutative semiring with total order
 *        (the intensional ℕ).
 *
 * @details Deliberately *not* restricted to `std::unsigned_integral<N>` so
 * that user-defined certified natural-number types (e.g.
 * `ExtensionalCardinal<N>`) can satisfy the concept without being built-in C++
 * types.  The required operations are exactly those that characterise ℕ as a
 * commutative semiring with a total order:
 *
 *  - Additive monoid: `+`.
 *  - Multiplicative monoid: `*`.
 *  - Total order: `<=`.
 *  - No subtraction required — that is what distinguishes ℕ from ℤ.
 *
 * **Embedding from `std::unsigned_integral`:** machine unsigned types are the
 * extensional/IEEE-policy approximation of ℕ.  Use
 * `embed_unsigned_integral<N>(v)` to inject a machine value into a certified
 * `IsNatural` domain, and `realize_to_size_t(sentinel)` to project back.
 *
 * Wikipedia: Semiring, Peano axioms
 */
export template <typename N>
concept IsNatural = std::regular<N> && requires(N a, N b) {
  { a + b } -> std::same_as<N>;
  { a * b } -> std::same_as<N>;
  { a <= b } -> std::convertible_to<bool>;
};

/**
 * @concept IsNaturalNumber
 * @brief Alias for the machine/extensional natural-number species.
 *
 * @details `std::unsigned_integral` types are the IEEE-policy realisation of
 * ℕ — they satisfy `IsNatural` structurally (unsigned arithmetic wraps, so
 * the semiring laws hold), but they are identified separately here because
 * they are the *output* of `realize_to_size_t` and the *input* of
 * `embed_unsigned_integral`, not the preferred certified domain for new code.
 */
export template <typename T>
concept IsNaturalNumber = std::unsigned_integral<T>;

/**
 * @concept Monoid_ℕ
 * @brief ℕ as the commutative monoid of naturals under addition.
 *
 * @details A carrier @c T satisfies @c Monoid_ℕ iff
 *   - it is @c IsNatural (the structural commutative-semiring-with-order
 *     witness for ℕ), and
 *   - @c (T, +, 0) is certified as an @c IsCommutativeMonoid by the species-
 *     trait registry (associativity, identity, commutativity of @c
 *     std::plus<T>).
 *
 * This is the concept the downstream library is meant to program against:
 * writing @c template @c <Monoid_ℕ T> binds the generic code to *any* carrier
 * that can prove itself a natural-number monoid, so a concrete choice
 * (@c ExtensionalCardinal<>, @c unsigned @c int, or a user-supplied carrier
 * carrying a species-trait proof) plugs in without rewriting the algorithm.
 *
 * The safety side-effect is deliberate: carriers whose @c + is *not* a
 * law-abiding monoid (signed @c int under @c std::plus, because overflow is
 * UB) fail this concept at the gate, so generic code that depends on the
 * monoid laws never instantiates on an unsafe carrier in the first place.
 */
export template <typename T>
concept Monoid_ℕ =
    IsNatural<T> && dedekind::category::IsCommutativeMonoid<T, std::plus<T>>;

/**
 * @brief Canonical embedding 𝔹 ↪ ℕ: bool → unsigned.
 * @details False maps to 0, True maps to 1.
 */
export inline constexpr auto embed_𝔹_ℕ =
    arrow<bool, unsigned>([](const bool& b) noexcept { return b ? 1u : 0u; });

/**
 * @brief Canonical injection from `std::unsigned_integral` into any
 *        `IsNatural` domain `N` via its single-argument constructor.
 *
 * @details `std::unsigned_integral` types are the machine/extensional
 * approximation of ℕ.  This arrow is the Liskov injection into a certified
 * `IsNatural` domain (e.g. `ExtensionalCardinal<K>`).  The reverse direction —
 * projecting a certified natural back to a machine width — is
 * `realize_to_size_t(sentinel)`.
 *
 * @tparam N  The target `IsNatural` type.
 * @tparam U  A `std::unsigned_integral` source type (deduced).
 */
export template <IsNatural N, std::unsigned_integral U>
constexpr N embed_unsigned_integral(U v) {
  return N{v};
}

/** @section Canonical_Species_Spine
 *
 * The canonical natural-numbers species ℕ is defined upstream in
 * @c dedekind.sets:boundaries (as @c NaturalNumbersOf<L, C> with
 * alias @c ℕ = @c NaturalNumbers and value-level constant
 * @c inline @c constexpr @c ℕ @c N{}).  This partition adds the
 * @c numbers-/@c order-/@c algebra-layer witnesses that pin the
 * canonical species against drift, and provides the embedding chain
 * arrows @c 𝔹 ↪ ℕ ↪ ℤ that the upstream sets layer cannot reach.
 */

using ::dedekind::sets::N;
using ::dedekind::sets::ℕ;

/** @section Formal_Verification */

// (0) Carrier-type witness: ℕ names the carrier itself (post-#401).
static_assert(std::same_as<ℕ, unsigned int>,
              "ℕ is the unsigned-int carrier (post-#401; modular ring "
              "ℤ/2^N, with ExtensionalCardinal<> as the exact saturating-"
              "to-ℵ_0 sibling).");

// (0a) Relationship between ℕ (the carrier) and NaturalNumbersOf<>
//      (the predicate-set / classifier).  The predicate-set's @c Domain
//      @b is the carrier — same shape as the 𝔹 ↔ Ω<bool> relationship
//      from #400.  IsSet<ℕ> itself does @b not fire (carrier types
//      carry no predicate-set surface); to participate as a set, lift
//      through the predicate-set.
static_assert(std::same_as<typename NaturalNumbersOf<>::Domain, ℕ>,
              "NaturalNumbersOf<>::Domain is the unsigned-int carrier "
              "ℕ — predicate-set's underlying element type IS the "
              "carrier.");

// (1) IsSet anchor: the predicate-set NaturalNumbersOf<> is a bona-fide
//     set.  Witnesses the set-builder DSL entry point that survives the
//     carrier migration.
static_assert(
    dedekind::category::IsSet<decltype(dedekind::category::ambient_set<
                                       unsigned int>(NaturalNumbersOf<>{}))>,
    "NaturalNumbersOf<> is the canonical IsSet anchor for ℕ.");

// (2) Syntax (the C++ operator surface that maps to ℕ's algebra).
//   - HasSemiringOperators<unsigned int>: +, * close, with T{} and T{1}.
//   - HasRingOperators<unsigned int>: +, -, unary -, * close (modular wrap
//     gives ℕ-flavoured behaviour without true negatives, but the literal
//     operators close on the carrier).
//   - HasLatticeOperators<unsigned int>: bitwise &, |, ^, ~ close.
static_assert(
    dedekind::algebra::HasSemiringOperators<unsigned int>,
    "ℕ's machine carrier (unsigned) closes the semiring operator surface "
    "(+, *, T{}, T{1}).");
static_assert(dedekind::algebra::HasRingOperators<unsigned int>,
              "ℕ's machine carrier (unsigned) also closes the literal "
              "ring operator surface (+, binary -, unary -, *) "
              "modulo wrap.");
static_assert(dedekind::order::HasLatticeOperators<unsigned int>,
              "ℕ's machine carrier (unsigned) closes the bitwise lattice "
              "operator surface (&, |, ^, ~).");

// (3) Semantics (the algebraic structures unsigned int actually carries).
//   - Self-documenting: IsNatural / IsNaturalNumber on the canonical
//     machine carrier (asserted earlier in this partition).
//   - Strict abelian-group / ring witnesses on `unsigned int` and on the
//     exact ℕ carrier (`ExtensionalCardinal<>`) live in their respective
//     trait-registration partitions; cited here as a lookup chain.
//   - The seal `IsArithmeticRing<unsigned int>` (PR #394) certifies that
//     the strict ring proof and the literal C++ operators agree on the
//     canonical machine carrier.
static_assert(IsNatural<unsigned int>,
              "unsigned int satisfies IsNatural (commutative semiring "
              "with order; +,*,<= close on the carrier).");
static_assert(IsNaturalNumber<unsigned int>,
              "unsigned int is the canonical IsNaturalNumber.");
static_assert(
    dedekind::algebra::IsArithmeticRing<unsigned int>,
    "unsigned int is the seal where strict ℕ-flavoured ring proof and "
    "the literal C++ operators (+, binary -, unary -, *) agree --- the "
    "canonical machine arithmetic ring under modular wrap.  Under the "
    "math-wins-over-C++ stance, this is the closest strict-ring carrier "
    "ℕ has at the machine level (the unbounded ℕ proxy lives in "
    "`Cardinality` from sets:cardinality, with ℵ_0 escalation).");
// Commutative-semiring witness (explicit, for documentation purposes).
// ℕ under (+, *) is a commutative idempotent semiring without additive
// inverse — what the math textbook calls a @b rig.  Pinned at the
// canonical machine carrier so the @b commutative-semiring claim that
// Monoid_ℕ implies (via IsCommutativeMonoid<T, std::plus> + the
// multiplicative monoid) is also visible as a single static_assert.
static_assert(
    dedekind::algebra::IsRig<unsigned int, std::plus<unsigned int>,
                             std::multiplies<unsigned int>>,
    "ℕ under (+, *) is a commutative idempotent semiring (rig) — "
    "no additive inverse on the carrier; modular wrap is the "
    "rig-flavoured closure rather than a ring inverse.");
// Order witnesses (explicit, for documentation purposes).  ℕ is the
// canonical totally-ordered chain 0 ≤ 1 ≤ 2 ≤ ... at the literal
// level; the spaceship and the four partial-order operators all
// fire on the carrier.  Mirrors the @b shape vs.\ @b axiom split of
// HasRingOperators / IsRing from PR #394.
static_assert(dedekind::order::HasPartialOrderOperators<ℕ>,
              "ℕ carries the partial-order operator surface "
              "(<, <=, >, >=).");
static_assert(dedekind::order::HasTotalOrderOperators<ℕ>,
              "ℕ carries the total-order operator surface "
              "(spaceship + the four partial-order operators).");
static_assert(dedekind::order::IsTotallyOrdered<ℕ>,
              "ℕ is axiomatically totally ordered (the chain "
              "0 ≤ 1 ≤ 2 ≤ ...).");

// (4) Primitive-type arrows.  ℕ *is* @c unsigned @c int (post-#401), so
// the predicate-set membership question reduces to direct calls on the
// classifier @c N (the namespace-level @c NaturalNumbersOf<> constant
// from sets:boundaries):
//   - Forward (unsigned → ℕ): trivially total (every unsigned is a
//     natural).
//   - Forward into a certified IsNatural domain (e.g.\ ExtensionalCardinal<>):
//     `embed_unsigned_integral<N>(v)`.
//   - Reverse (ℕ → unsigned): for the certified domain, project via
//     `realize_to_size_t(sentinel)` (lives in sets:cardinality).
static_assert(N(0u) == ClassicalLogic::True, "0 ∈ ℕ.");
static_assert(N(42u) == ClassicalLogic::True, "42 ∈ ℕ.");
static_assert(N(-7) == ClassicalLogic::False,
              "Direct N(int) call is the ℕ-as-subset-of-ℤ classifier; "
              "rejects negatives.");

// (5) Adjacent-set arrow: 𝔹 ↪ ℕ via @c embed_𝔹_ℕ above; registered
// monic at the bottom of this partition.  The forward arrow ℕ ↪ ℤ
// lives in @c :integer (downstream), as @c embed_ℕ_ℤ.

}  // namespace dedekind::numbers

namespace dedekind::category {
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::numbers::embed_𝔹_ℕ)>> =
        true;
}  // namespace dedekind::category
