/**
 * @file dedekind/order/lattice.cppm
 * @partition :lattice
 * @brief Order-theoretic meet / join / lattice structures.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section order_lattice__Scope
 * Species carrying binary meet and/or join operations that respect
 * the underlying order, reaching up to distributive lattices.  The
 * actual axiomatic certification (associativity, commutativity,
 * idempotence, absorption, distributivity) lives in
 * `dedekind.category:posetal` as
 * `IsCertifiedOrder{Meet,Join}Semilattice` etc.; this partition
 * re-exports those witnesses under the order-theoretic names a
 * mathematician expects to find alongside posets and chains.
 *
 * Wikipedia: Semilattice, Lattice (order), Distributive lattice.
 *
 * @note "Lattice theory provides the appropriate generality for the
 *        algebraic study of order, just as group theory does for
 *        symmetries."
 *       [Trans: original is English; the gloss is that meet (∧) and
 *        join (∨) play the same role for partial orders that the
 *        group operation plays for symmetry groups.]
 *       — Garrett Birkhoff, *Lattice Theory* (3rd ed.,
 *         AMS Colloquium Publications vol. 25, 1967), Preface.
 *         Birkhoff's monograph is the canonical reference for the
 *         meet/join axiomatics this partition re-exports under the
 *         names mathematicians expect to find next to chains and
 *         posets.
 */
module;
#include <algorithm>
#include <concepts>  // std::convertible_to (HasLatticeOperators)
#include <cstddef>   // std::size_t — bitwise lattice witness (#710)
#include <functional>
#include <type_traits>  // std::is_integral_v — bitwise lattice (#710)

export module dedekind.order:lattice;

import dedekind.category;

namespace dedekind::order {
using namespace dedekind::category;

/**
 * @concept HasLatticeOperators
 * @brief @b Pure @b syntactic @b shape: T supports the bitwise / lattice
 *        operators @c &, @c |, @c ^, @c ~ with results convertible to T.
 *
 * @details
 * Use this concept where the callsite needs the lattice operators
 * @c {meet, join, symmetric-difference, complement} to compile and
 * yield a value coercible back into @c T --- e.g.\ Boolean carriers,
 * integer bit-twiddle types, sublattices of a Heyting algebra, the
 * additive @f$\mathbb{F}_2@f$ surface.  No axiomatic claim about
 * idempotency, absorption, distributivity, or De Morgan duality is
 * made here; for those, use the certified lattice concepts below or
 * the bundled @c IsOrderLattice further down.
 *
 * The return-type is @c convertible_to<T> rather than @c same_as<T>
 * (cf.\ the strict @c HasRingOperators) because integer promotion
 * makes @c bool & bool @c -> int in C++; relaxing to convertibility
 * lets the canonical Boolean carrier (@c bool) satisfy the concept
 * via promotion-and-back.
 *
 * Sibling of @c dedekind::algebra::HasRingOperators (in @c
 * algebra:ring) and @c dedekind::category::HasLogicalOperators (in
 * @c category:logic) in the shape-concept family --- introduced
 * under #393.
 */
export template <typename T>
concept HasLatticeOperators = requires(T a, T b) {
  { a & b } -> std::convertible_to<T>;
  { a | b } -> std::convertible_to<T>;
  { a ^ b } -> std::convertible_to<T>;
  { ~a } -> std::convertible_to<T>;
};

/**
 * @concept IsOrderMeetSemilattice
 * @brief Re-export the certified meet-semilattice stage from `:posetal`.
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsOrderMeetSemilattice =
    dedekind::category::IsCertifiedOrderMeetSemilattice<T, Meet>;

/**
 * @concept IsOrderJoinSemilattice
 * @brief Re-export the certified join-semilattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsOrderJoinSemilattice =
    dedekind::category::IsCertifiedOrderJoinSemilattice<T, Join>;

/**
 * @concept IsOrderLattice
 * @brief @b Bundled @b lattice @b witness: the strict commutative-ring
 *        proof under @c (std::bit_xor<T>, std::bit_and<T>) AND the
 *        bitwise/lattice operator surface on @c T, by definition.
 *
 * @details
 * The Boolean-ring reading of "lattice": a commutative ring where
 * the additive operation plays the role of symmetric difference
 * (join modulo absorption) and the multiplicative operation plays
 * the role of meet.  This bundle locks the @c (Add, Mult) functors
 * to @c (std::bit_xor<T>, std::bit_and<T>) --- the operator pair
 * the @c HasLatticeOperators shape concept describes --- so the two
 * halves of the bundle (categorical proof + operator surface) refer
 * to the @b same algebraic structure.
 *
 * Earlier drafts parameterised the bundle by @c (Add, Mult) and
 * defaulted them to @c (std::bit_xor<T>, std::bit_and<T>); the
 * parameters were unused by the operator-surface clause (which is
 * hard-wired to the bitwise operators), so a caller could pass
 * non-bitwise @c (Add, Mult) and still satisfy the concept on a
 * carrier with bitwise ops.  The current form pins the bundle to
 * the Boolean-ring reading; callers wanting other @c (Add, Mult)
 * pairs should reach for the underlying
 * @c category::IsCommutativeRing<T, Add, Mult> directly.
 *
 * @b Meaning @b shift @b (#393): this concept previously re-exported
 * @c category::IsCertifiedOrderLatticeOperations<T, Join, Meet> with
 * @c std::ranges::min / @c std::ranges::max as the order-theoretic
 * Join/Meet defaults.  The order-theoretic certified lattice machinery
 * still lives at @c category::IsCertifiedOrderLatticeOperations and
 * is reachable directly; @c IsOrderLattice now bundles the
 * Boolean-ring lattice reading instead --- which is the canonical
 * @c HasLatticeOperators shape↔concept mapping the audit recorded.
 * The Boolean ring and Boolean lattice are equivalent categories, so
 * either reading recovers the other on Boolean-flavoured carriers.
 */
export template <typename T>
concept IsOrderLattice =
    dedekind::category::IsCommutativeRing<T, std::bit_xor<T>,
                                          std::bit_and<T>> &&
    HasLatticeOperators<T>;

/**
 * @concept IsOrderDistributiveLattice
 * @brief Re-export the certified distributive lattice stage from `:posetal`.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsOrderDistributiveLattice =
    dedekind::category::IsCertifiedOrderDistributiveLatticeOperations<T, Join,
                                                                      Meet>;

/** @section order_lattice__Formal_Verification */

// Pure-syntactic-shape witnesses: integer bit-twiddle types and
// bool itself satisfy the four lattice operators with results
// convertible back to the carrier (bool's & under integer promotion
// returns int but is convertible to bool, hence the deliberately
// loose return-type spec).
static_assert(HasLatticeOperators<unsigned int>,
              "unsigned int has the syntactic lattice-operator surface "
              "(bitwise &, |, ^, ~).");
static_assert(HasLatticeOperators<bool>,
              "bool has the syntactic lattice-operator surface "
              "(bitwise operators yield int via promotion, "
              "convertible back to bool).");

// IsOrderLattice (bundled, Boolean-ring reading) on bool: bool
// satisfies IsCommutativeRing<bool, std::bit_xor, std::bit_and> per
// the Galois-field witness shipped in #378, and now also satisfies
// HasLatticeOperators<bool>; the bundled IsOrderLattice<bool> fires.
static_assert(IsOrderLattice<bool>,
              "bool is the canonical Boolean-ring lattice "
              "(under bit_xor / bit_and; both halves of the bundle "
              "fire today).");

/** @section order_lattice__Bitwise_Boolean_Lattice
 *
 *  @brief Integral carriers under the @b bitwise Boolean algebra (#710).
 *
 *  @details
 *  An integral carrier @c T (@c int, @c unsigned, @c std::size_t, …)
 *  has TWO distinct lattice readings on the same underlying set:
 *
 *    - The @b Boolean-ring reading (@c IsOrderLattice<bool> above
 *      already pins this for @c bool): @c (XOR, AND) — additive group
 *      under XOR, multiplicative monoid under AND.  Algebraic / field-
 *      flavoured.
 *    - The @b Boolean-lattice reading (this section, #710): the
 *      power-set lattice of the bit positions.  @c a @c ⊆_bit @c b
 *      @c ⟺ @c (a @c & @c b) @c == @c a; meet = @c &; join = @c |;
 *      complement = @c ~; bottom = @c 0; top = @c ~T(0).  Order-theoretic
 *      / Form-chain-row-7 flavoured.
 *
 *  The two readings are equivalent on Boolean algebras (XOR is the
 *  symmetric difference @c (a @c | @c b) @c & @c ~(a @c & @c b)), but
 *  they expose different concept surfaces — @c IsOrderLattice
 *  (Boolean-ring) above vs.\ @c :category::IsBooleanLatticeCategory
 *  (Form-chain row 7) here.  Naming both makes the equivalence
 *  navigable rather than implicit.
 *
 *  @section order_lattice__Bitwise_GF_Cross_Reference
 *  @c :algebra::galois.cppm carries the Galois field machinery
 *  (@c bool @c = @c 𝔽_2 under @c (XOR, AND); @c 𝔽64 @c = @c GF(2^6)
 *  with polynomial multiplication mod @c x^6+x+1; @c IsGaloisField
 *  concept; @c GaloisFieldRegistration CRTP).  For wider integral
 *  @c T (@c size_t etc.), the field claim requires a specific primitive
 *  polynomial — the project does not witness @c GF(2^N) for arbitrary
 *  @c N as field-on-the-primitive type without a struct wrapper.
 *
 *  Cross-reading: the bitwise Boolean @b lattice on @c size_t here is
 *  the lattice of subsets of @c {0, …, 63}; the (would-be) Galois
 *  @b field @c GF(2^64) on the same carrier is a different algebraic
 *  structure on the same underlying set.  Don't conflate them.
 */

/** @brief Bit-subset relation: @c a @c ⊆_bit @c b @c ⟺ @c (a @c &
 *         @c b) @c == @c a.  Form-chain @c Rel slot for the bitwise
 *         Boolean lattice on integral carriers (#710). */
export template <typename T>
  requires std::is_integral_v<T>
struct bit_subset_eq {
  constexpr bool operator()(T a, T b) const noexcept { return (a & b) == a; }
};

}  // namespace dedekind::order

// Trait specialisations sit in the @c dedekind::category namespace
// where their primary templates live; re-opening here keeps the
// bitwise-lattice witnesses self-contained at this site.
namespace dedekind::category {

template <typename T>
  requires std::is_integral_v<T>
struct is_reflexive<T, dedekind::order::bit_subset_eq<T>> : std::true_type {};

template <typename T>
  requires std::is_integral_v<T>
struct is_transitive<T, dedekind::order::bit_subset_eq<T>> : std::true_type {};

template <typename T>
  requires std::is_integral_v<T>
struct is_antisymmetric<T, dedekind::order::bit_subset_eq<T>> : std::true_type {
};

template <typename T>
  requires std::is_integral_v<T>
struct is_directed<T, dedekind::order::bit_subset_eq<T>> : std::true_type {};

template <typename T>
  requires std::is_integral_v<T>
struct is_codirected<T, dedekind::order::bit_subset_eq<T>> : std::true_type {};

/** @section order_lattice__Bitwise_Algebra_Trait_Registration
 *
 *  @brief Algebra-trait specialisations for @c std::bit_or<T> /
 *         @c std::bit_and<T> on specific integral @c T (#710).
 *
 *  @details The project's existing specialisations cover @c bool +
 *  the transparent @c std::bit_or<> / @c std::bit_and<> forms, plus
 *  @c std::bit_and<T> + idempotent on @c std::bit_or<T> for specific
 *  @c T.  The pieces this slice still needs for the bitwise Boolean
 *  lattice's @c IsLatticeCategory chain to fire on generic integral
 *  @c T:
 *
 *    - @c is_associative<T, std::bit_or<T>>     (struct spec)
 *    - @c is_absorptive_v<T, std::bit_or<T>, std::bit_and<T>>     (variable spec)
 *    - @c is_absorptive_v<T, std::bit_and<T>, std::bit_or<T>>     (variable spec)
 *    - @c is_distributive_v<T, std::bit_or<T>, std::bit_and<T>>   (variable spec)
 *    - @c is_distributive_v<T, std::bit_and<T>, std::bit_or<T>>   (variable spec)
 *
 *  Note: @c is_associative_v / @c is_idempotent_v are derived from
 *  struct traits, so specialisations go on the @b struct (matching
 *  the pattern used at species.cppm for @c is_idempotent<T,
 *  std::bit_or<T>>).  @c is_absorptive_v and @c is_distributive_v
 *  are @b direct variable templates with primary @c = false, so
 *  specialisations go on the variable. */
template <typename T>
  requires std::is_integral_v<T>
struct is_associative<T, std::bit_or<T>> : std::true_type {};

template <typename T>
  requires std::is_integral_v<T>
inline constexpr bool is_absorptive_v<T, std::bit_or<T>, std::bit_and<T>> =
    true;

template <typename T>
  requires std::is_integral_v<T>
inline constexpr bool is_absorptive_v<T, std::bit_and<T>, std::bit_or<T>> =
    true;

template <typename T>
  requires std::is_integral_v<T>
inline constexpr bool is_distributive_v<T, std::bit_or<T>, std::bit_and<T>> =
    true;

template <typename T>
  requires std::is_integral_v<T>
inline constexpr bool is_distributive_v<T, std::bit_and<T>, std::bit_or<T>> =
    true;

/** @brief @c HeytingExponential specialisation for the bitwise Boolean
 *         lattice (#710): on integral @c T under @c bit_subset_eq with
 *         @c std::bit_and as meet, the relative complement
 *         @c a @c → @c b @c = @c ~a @c | @c b (Boolean Heyting), and
 *         the @c eval morphism @c e(x) @c = @c e @c & @c x (meet).
 *         Required so @c IsHeytingLatticeCategory fires at row 6 of
 *         the Form-chain for this Rel / Meet pairing. */
template <typename T>
  requires std::is_integral_v<T>
struct HeytingExponential<T, dedekind::order::bit_subset_eq<T>,
                          std::bit_and<T>> {
  using Domain = T;
  using Codomain = T;
  T value;
  constexpr T operator()(T x) const noexcept { return value & x; }
};

/** @brief Bitwise-lattice bottom: the all-zeros bitmask. */
template <typename T>
  requires std::is_integral_v<T>
struct LatticeBottom<T, dedekind::order::bit_subset_eq<T>> {
  using is_initial_object_tag = void;
  static constexpr T value = T{0};
};

/** @brief Bitwise-lattice top: the all-ones bitmask. */
template <typename T>
  requires std::is_integral_v<T>
struct LatticeTop<T, dedekind::order::bit_subset_eq<T>> {
  using is_terminal_object_tag = void;
  static constexpr T value = static_cast<T>(~T{0});
};

/** @brief Canonical specialisation: @c std::bit_not<T> is the
 *         complement for the bitwise Boolean lattice on integral @c T.
 *         Complement laws hold structurally: @c a @c & @c ~a @c = @c 0
 *         (bottom) and @c a @c | @c ~a @c = @c ~T(0) (top). */
template <typename T>
  requires std::is_integral_v<T>
struct is_complement<std::bit_not<T>, T, dedekind::order::bit_subset_eq<T>,
                     std::bit_or<T>, std::bit_and<T>> : std::true_type {};

}  // namespace dedekind::category

namespace dedekind::order {

/** @section order_lattice__Bitwise_Canonical_Witnesses */

static_assert(
    dedekind::category::IsBooleanLatticeCategory<
        std::size_t, bit_subset_eq<std::size_t>, std::bit_or<std::size_t>,
        std::bit_and<std::size_t>, std::bit_not<std::size_t>>,
    "size_t under (bit_subset_eq, |, &, ~) is the bitwise Boolean "
    "lattice — the power-set lattice of {0, …, 63}.  Distinct from "
    "the totally-ordered Heyting chain on size_t under std::less_equal "
    "(which honestly fails row 7 — see :category::lattice's static_assert "
    "documenting the Honest Rejection).");

static_assert(dedekind::category::IsBooleanLatticeCategory<
                  unsigned, bit_subset_eq<unsigned>, std::bit_or<unsigned>,
                  std::bit_and<unsigned>, std::bit_not<unsigned>>,
              "unsigned under (bit_subset_eq, |, &, ~) is the bitwise Boolean "
              "lattice on its bit-width.");

static_assert(dedekind::category::LatticeBottom<
                  std::size_t, bit_subset_eq<std::size_t>>::value ==
                  std::size_t{0},
              "Bitwise lattice bottom on size_t is the all-zeros bitmask.");
static_assert(dedekind::category::LatticeTop<
                  std::size_t, bit_subset_eq<std::size_t>>::value ==
                  ~std::size_t{0},
              "Bitwise lattice top on size_t is the all-ones bitmask.");

// Set<T, L, P> structurally cannot satisfy @c HasLatticeOperators (#469
// design note).  After the @c operator^ symmetric-difference slice
// landed alongside @c operator~ (predicate-level complement), Set
// supports all four lattice operator @c symbols (|, &, ^, ~).  But
// @c HasLatticeOperators<T> additionally requires the result to be
// @c convertible_to<T> — i.e.\ it asks for a homogeneous-result
// surface, where @c a @c & @c b returns something castable back to
// @c T.  This works for primitive carriers (@c bool, @c unsigned int)
// where the operators stay within the carrier (modulo integer
// promotion, which IS convertible back).  It does @b not work for
// Set, whose operators are @b type-parameterised: @c |, @c &, @c ^
// each return @c Set<T, L, lambda-OR/AND/XOR-predicate> with a fresh
// @c Predicate template parameter, and @c ~A returns @c Set<T, L,
// NegatedPredicate<P>>.  None of these are convertible back to the
// input @c Set<T, L, P>, because the Predicate is a structural part
// of the type.
//
// The "Set is a lattice" claim therefore lives one level less strict
// than @c HasLatticeOperators: Set has the @b lattice @b operator
// @b shape (the four operators all compile and return some Set), but
// not the convertible-result shape the existing concept requires.
// A follow-on slice (filed under #524) could introduce a weaker
// @c HasLatticeOperatorShape<T> concept that drops the
// @c convertible_to<T> clause and pin Set against that.  For this
// PR's scope, the documentation of the operator surface in
// @c :sets:expressions and the operational tests in
// @c expressions_test.cpp suffice.

}  // namespace dedekind::order
