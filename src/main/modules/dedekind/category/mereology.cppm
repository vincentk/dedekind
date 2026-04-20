/**
 * @file dedekind/category/mereology.cppm
 * @partition :mereology
 * @brief Level 0c: Embryonic Mereology (Part-Whole and Skew Algebraic Core).
 *
 * Following the formal axiomatization of Stanisław Leśniewski, this partition
 * introduces the "Part-Whole" relation as a foundational structural primitive.
 * In this stage we define:
 * 1) the primitive part-whole relation (and its order-style axioms), and
 * 2) the non-commutative (skew) maturation ladder for meet/join operations.
 *
 * Commutative, order-theoretic semilattice/lattice refinements are layered in
 * `:posetal` to keep order-theory nomenclature and laws localized there.
 *
 * @section Axioms
 * 1. Reflexivity: Everything is a part of itself.
 * 2. Transitivity: A part of a part is a part of the whole.
 * 3. Antisymmetry: Two distinct things cannot be parts of each other.
 *
 * @see Leśniewski-style mereology overview:
 * https://plato.stanford.edu/entries/mereology/
 * @see Skew lattice background (non-commutative generalization):
 * https://en.wikipedia.org/wiki/Skew_lattice
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "It remains to discuss briefly what general requirements may be justly
 * laid down for the solution of a mathematical problem. I should say first of
 * all, this: that it shall be possible to establish the correctness of the
 * solution by means of a finite number of steps."
 *       -- David Hilbert, Mathematical Problems (1900)
 */
module;

#include <algorithm>
#include <concepts>
#include <functional>

export module dedekind.category:mereology;

import :species;
import :morphism;

namespace dedekind::category {

/**
 * @concept IsPartRelation
 * @brief Verifies that a binary operator satisfies Leśniewski's mereological
 * axioms.
 *
 * @details
 * Textbook term: this is the axiomatic core of a partial-order style
 * parthood relation used in formal mereology.
 *
 * @tparam Op The parthood operator (defaults to `std::less_equal<T>`).
 * @tparam T The type being reasoned about.
 * @tparam Ω The truth-value codomain (default: classical two-valued logic).
 *
 * @note Textbook alignment:
 * - Parthood/partial-order axioms: reflexive, transitive, antisymmetric.
 * - Default comparator `std::less_equal<T>` corresponds to the usual
 *   order-theoretic relation symbol `<=`.
 *
 * @see Structural mereology overview (Stanford Encyclopedia):
 * https://plato.stanford.edu/entries/mereology/
 * @see Lambek & Scott (1988), Introduction to Higher-Order Categorical Logic.
 * @see McLarty (1992), Elementary Categories, Elementary Toposes.
 */
export template <typename T, typename Op = std::less_equal<T>,
                 typename Ω = bool>
concept IsPartRelation = requires(Op op, T a, T b, T c) {
  { op(a, b) } -> std::same_as<Ω>;  // The operator must return a truth value

  // Mereological Axiom 1: Reflexivity (∀x, x ≤ x)
  // Axiom 2: Transitivity (∀x,y,z: x ≤ y ∧ y ≤ z ⇒ x ≤ z)
  // Axiom 3: Antisymmetry (∀x,y: x ≤ y ∧ y ≤ x ⇒ x = y)
  requires is_reflexive_v<T, Op>;
  requires is_transitive_v<T, Op>;
  requires is_antisymmetric_v<T, Op>;
};

/**
 * @concept IsPartialOrder
 * @brief Named stage for the partial-order maturity level of parthood.
 *
 * @details
 * This is an explicit taxonomy rung in the order ladder:
 * parthood relation -> partial order -> total order.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename Ω = bool>
concept IsPartialOrder = IsPartRelation<T, Rel, Ω>;

/**
 * @concept IsTotalOrder
 * @brief Linear/total refinement of `IsPartialOrder`.
 *
 * @details
 * A total order is a partial order where any two elements are comparable.
 * In this library we certify that comparability via `std::totally_ordered<T>`
 * while preserving the configurable relation witness and Ω codomain.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename Ω = bool>
concept IsTotalOrder = IsPartialOrder<T, Rel, Ω> && std::totally_ordered<T> &&
                       requires(Rel rel, T a, T b) {
                         { rel(a, b) } -> std::same_as<Ω>;
                         { rel(b, a) } -> std::same_as<Ω>;
                       };

/**
 * @concept IsPartOfRelation
 * @brief Minimal core part-whole relation yielding Omega.
 *
 * @details
 * Textbook term: primitive binary parthood predicate (often written
 * `part <= whole` or `part ⊑ whole` depending on formalism).
 *
 * This concept accepts three equivalent encodings used across partitions:
 * - Order-style: `part <= whole`
 * - Predicate-style: `whole(part)`
 * - Indexer-style: `whole[part]`
 *
 * The canonical default in `IsPartRelation` remains order-style (`<=`),
 * while topoi/set layers may present parthood through characteristic
 * predicates (`operator()`) or lookup/index syntax (`operator[]`).
 *
 * Richer mereological structure (species routing, ambient checks, lattice
 * refinements) should be layered by downstream partitions.
 *
 * @see https://en.wikipedia.org/wiki/Mereology
 */
export template <typename Part, typename Whole, typename Ω = bool>
concept IsPartOfRelation = requires(const Part& part, const Whole& whole) {
  requires(
      requires {
        { part <= whole } -> std::same_as<Ω>;
      } ||
      requires {
        { whole(part) } -> std::same_as<Ω>;
      } ||
      requires {
        { whole[part] } -> std::same_as<Ω>;
      });
};

/**
 * @concept IsMereologicalJoinMagma
 * @brief Stage 0 for join-like algebra: closure only.
 *
 * @details
 * Default witness is `std::ranges::max`, matching the canonical join witness
 * on totally ordered carriers.
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsMereologicalJoinMagma = requires(Join join, T a, T b) {
  { join(a, b) } -> std::convertible_to<T>;
};

/**
 * @concept IsMereologicalMeetMagma
 * @brief Stage 0 for meet-like algebra: closure only.
 *
 * @details
 * Default witness is `std::ranges::min`, matching the canonical meet witness
 * on totally ordered carriers.
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsMereologicalMeetMagma = requires(Meet meet, T a, T b) {
  { meet(a, b) } -> std::convertible_to<T>;
};

/**
 * @concept IsMereologicalJoinSemigroup
 * @brief Stage 1 for join-like algebra: magma + associativity.
 *
 * @details
 * Maturation ladder in this partition:
 * magma -> semigroup -> band -> skew lattice.
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsMereologicalJoinSemigroup =
    IsMereologicalJoinMagma<T, Join> && IsAssociative<T, Join>;

/**
 * @concept IsMereologicalMeetSemigroup
 * @brief Stage 1 for meet-like algebra: magma + associativity.
 *
 * @details
 * Maturation ladder in this partition:
 * magma -> semigroup -> band -> skew lattice.
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsMereologicalMeetSemigroup =
    IsMereologicalMeetMagma<T, Meet> && IsAssociative<T, Meet>;

/**
 * @concept IsMereologicalMeetBand
 * @brief Non-commutative meet-like operation (associative + idempotent).
 *
 * @details
 * Textbook term: this is the band-level (possibly non-commutative) fragment
 * underlying skew lattice formulations.
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsMereologicalMeetBand =
    IsMereologicalMeetSemigroup<T, Meet> && IsIdempotent<T, Meet>;

/**
 * @concept IsMereologicalJoinBand
 * @brief Non-commutative join-like operation (associative + idempotent).
 *
 * @details
 * Textbook term: this is the dual band-level fragment used in skew lattice
 * formulations.
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsMereologicalJoinBand =
    IsMereologicalJoinSemigroup<T, Join> && IsIdempotent<T, Join>;

/**
 * @concept IsMereologicalSkewLatticeOperations
 * @brief Non-commutative lattice-style operations (skew lattice fragment).
 *
 * @details
 * Requires associative + idempotent join/meet operations connected by
 * absorption. Commutativity is intentionally not required.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsMereologicalSkewLatticeOperations =
    IsMereologicalJoinBand<T, Join> && IsMereologicalMeetBand<T, Meet> &&
    IsAbsorptive<T, Join, Meet>;

/**
 * @concept IsMereologicalMeetSemilattice
 * @brief Compatibility alias for a meet-band stage.
 *
 * @note Textbook term:
 * commutative meet-semilattice refinements are modeled in `:posetal`.
 *
 * @see https://en.wikipedia.org/wiki/Semilattice
 */
export template <typename T, typename Meet = decltype(std::ranges::min)>
concept IsMereologicalMeetSemilattice = IsMereologicalMeetBand<T, Meet>;

/**
 * @concept IsMereologicalJoinSemilattice
 * @brief Compatibility alias for a join-band stage.
 *
 * @note Textbook term:
 * commutative join-semilattice refinements are modeled in `:posetal`.
 *
 * @see https://en.wikipedia.org/wiki/Semilattice
 */
export template <typename T, typename Join = decltype(std::ranges::max)>
concept IsMereologicalJoinSemilattice = IsMereologicalJoinBand<T, Join>;

/**
 * @concept IsMereologicalLatticeOperations
 * @brief Compatibility lattice alias over the skew core + absorption.
 *
 * @details
 * This compatibility alias only requires paired join/meet semilattice stages.
 * Strict absorption-based lattice certification is hosted in `:posetal`
 * (`IsOrderLatticeOperations`) so order-theoretic laws and references live in
 * the order partition.
 *
 * Non-commutative variants are captured by
 * `IsMereologicalSkewLatticeOperations`.
 *
 * @see https://en.wikipedia.org/wiki/Skew_lattice
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsMereologicalLatticeOperations =
    IsMereologicalJoinSemilattice<T, Join> &&
    IsMereologicalMeetSemilattice<T, Meet>;

/**
 * @concept IsMereologicalDistributiveLatticeOperations
 * @brief Mature distributive stage: lattice operations with mutual
 * distribution.
 */
export template <typename T, typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min)>
concept IsMereologicalDistributiveLatticeOperations =
    IsMereologicalLatticeOperations<T, Join, Meet> &&
    IsDistributive<T, Join, Meet> && IsDistributive<T, Meet, Join>;

/**
 * @concept IsSkewMeetSemilattice
 * @brief Non-commutative meet-like idempotent semigroup with set-theoretic
 * default operator (`std::bit_and`).
 *
 * @details
 * This is the canonical set-theoretic naming of the meet-band stage, binding
 * the bit-AND operator as the conventional "intersection" for set-like types.
 * The general algebraic concept is `IsMereologicalMeetBand`; this alias fixes
 * the set-theoretic default.
 *
 * @tparam S   The set-like carrier type.
 * @tparam Meet The meet operator (default: `std::bit_and<S>` for sets).
 */
export template <typename S, typename Meet = std::bit_and<S>>
concept IsSkewMeetSemilattice = IsMereologicalMeetBand<S, Meet>;

/**
 * @concept IsSkewJoinSemilattice
 * @brief Non-commutative join-like idempotent semigroup with set-theoretic
 * default operator (`std::bit_or`).
 *
 * @details
 * Set-theoretic naming of the join-band stage, binding bit-OR as the
 * conventional "union" for set-like types.
 *
 * @tparam S   The set-like carrier type.
 * @tparam Join The join operator (default: `std::bit_or<S>` for sets).
 */
export template <typename S, typename Join = std::bit_or<S>>
concept IsSkewJoinSemilattice = IsMereologicalJoinBand<S, Join>;

/**
 * @concept IsSkewLattice
 * @brief Non-commutative lattice with set-theoretic default operators.
 *
 * @details
 * Set-theoretic naming alias that fixes `bit_or`/`bit_and` as the canonical
 * join/meet operators, reflecting the convention that union = OR and
 * intersection = AND for bit-representable set-like types.
 *
 * @tparam S    The set-like carrier type.
 * @tparam Join The join operator (default: `std::bit_or<S>`).
 * @tparam Meet The meet operator (default: `std::bit_and<S>`).
 */
export template <typename S, typename Join = std::bit_or<S>,
                 typename Meet = std::bit_and<S>>
concept IsSkewLattice = IsMereologicalSkewLatticeOperations<S, Join, Meet>;

/**
 * @concept IsSetMeetSemilattice
 * @brief Commutative meet-semilattice with set-theoretic default operator.
 *
 * @tparam S    The set-like carrier type.
 * @tparam Meet The meet operator (default: `std::bit_and<S>`).
 */
export template <typename S, typename Meet = std::bit_and<S>>
concept IsSetMeetSemilattice = IsMereologicalMeetSemilattice<S, Meet>;

/**
 * @concept IsSetJoinSemilattice
 * @brief Commutative join-semilattice with set-theoretic default operator.
 *
 * @tparam S    The set-like carrier type.
 * @tparam Join The join operator (default: `std::bit_or<S>`).
 */
export template <typename S, typename Join = std::bit_or<S>>
concept IsSetJoinSemilattice = IsMereologicalJoinSemilattice<S, Join>;

/**
 * @concept IsSetLattice
 * @brief Paired commutative join/meet semilattice with set-theoretic defaults.
 *
 * @details
 * Set-theoretic naming alias: binds `bit_or` as join (union) and `bit_and`
 * as meet (intersection). The general algebraic concept is
 * `IsMereologicalLatticeOperations`.
 *
 * @tparam S    The set-like carrier type.
 * @tparam Join The join operator (default: `std::bit_or<S>`).
 * @tparam Meet The meet operator (default: `std::bit_and<S>`).
 */
export template <typename S, typename Join = std::bit_or<S>,
                 typename Meet = std::bit_and<S>>
concept IsSetLattice = IsMereologicalLatticeOperations<S, Join, Meet>;

/**
 * @concept HasExtrema
 * @brief Structural requirement for Dedekind completeness witnesses.
 *
 * @details
 * A type satisfies `HasExtrema` when it exposes both a supremum (least upper
 * bound) and an infimum (greatest lower bound) operation. This is the
 * structural prerequisite for `IsDedekindComplete` in the order partition.
 *
 * @tparam S A carrier type (set or interval species).
 *
 * @see Dedekind (1872), Stetigkeit und irrationale Zahlen.
 * @see McLarty (1992), Elementary Categories, Elementary Toposes, ch. 4.
 */
export template <typename S>
concept HasExtrema = requires(S s) {
  /** @brief Computes the least upper bound of a bounded subset. */
  { s.supremum() };
  /** @brief Computes the greatest lower bound of a bounded subset. */
  { s.infimum() };
};

/**
 * @concept IsMereologicalCutCandidate
 * @brief Transitional bridge contract for the soft-to-hard structural cut.
 *
 * @details
 * Models the structural pruning semantics of Leśniewski-style mereological
 * cuts: a `Shell` filters a `Manifold` to a `FilteredSpace` that is a
 * mereological part of the manifold (verified via `IsPartOfRelation`). This
 * concept captures the "soft" half of the full `IsMereologicalCut` described
 * in `nn_notes/mereological_cut.cppm`, without introducing a dependency on
 * `:pullback`.
 *
 * The full `IsMereologicalCut` additionally requires the filtered inclusion to
 * be a pullback limit (unique factorization). That Axiom 7 / Issue #195
 * constraint is intentionally deferred here.
 *
 * @note TODO(Issue #195): strengthen to `IsSubobject<FilteredSpace, Manifold>`
 *       and add `IsPullbackLimit` uniqueness once the `:pullback` partition
 *       naming is stable.
 *
 * @tparam Shell    The filtering agent (e.g. a learned sieve or pruning mask).
 * @tparam Manifold The ambient space being partitioned.
 * @tparam Ω        The truth-value codomain (default: `bool`).
 *
 * @see https://plato.stanford.edu/entries/mereology/
 * @see Lawvere & Schanuel (2009), Conceptual Mathematics, ch. 4 (subobjects).
 */
export template <typename Shell, typename Manifold, typename Ω = bool>
concept IsMereologicalCutCandidate = requires(Shell s, Manifold m) {
  // The filtered subspace must be a declared type alias.
  typename Shell::FilteredSpace;

  // The filtered subspace must be a mereological part of the manifold.
  requires IsPartOfRelation<typename Shell::FilteredSpace, Manifold, Ω>;

  // The shell must expose a filter operation producing the FilteredSpace.
  { s.filter(m) } -> std::same_as<typename Shell::FilteredSpace>;
};

/**
 * @concept Parthood
 * @brief Skeletal part-whole contract at the concept level.
 *
 * @details
 * This concept is the concept-only replacement for the former `Parthood`
 * witness struct. At this embryonic stage, we encode parthood directly as the
 * self-carrier primitive relation contract (`T` as both part and whole).
 */
export template <typename T>
concept Parthood = IsPartOfRelation<T, T, bool>;

/**
 * @brief Generic opt-in drill-down projector for part-whole wrappers.
 *
 * @details
 * This is the upstream projector primitive used by downstream partitions
 * (`:posetal`, `:limit`, and others) to express functional part access via
 * `operator->` while preserving explicit opt-in semantics.
 *
 * Overloads support both wrapper types exposing a member `operator->` and
 * raw pointers.
 */
export template <typename Whole>
constexpr decltype(auto) arrow_drill_down(const Whole& whole)
  requires requires { whole.operator->(); }
{
  return *whole.operator->();
}

/** @brief Pointer overload for generic drill-down projection. */
export template <typename T>
constexpr const T& arrow_drill_down(const T* whole) {
  return *whole;
}

// Compiler-validated documentation witnesses for the mereological ladder.
static_assert(
    IsPartRelation<int>,
    "Parthood axioms must hold for the canonical integer order witness.");
static_assert(Parthood<int>,
              "Parthood concept must hold for the canonical integer witness.");
static_assert(
    requires(const int* p) {
      { arrow_drill_down(p) } -> std::same_as<const int&>;
    },
    "Pointer drill-down overload must return a const reference to the "
    "pointed-to value.");
static_assert(IsPartialOrder<int>,
              "Partial-order stage must refine the parthood axioms.");
static_assert(IsTotalOrder<int>,
              "Total-order stage must refine partial-order parthood.");

static_assert(IsMereologicalMeetMagma<int, decltype(std::ranges::min)>);
static_assert(IsMereologicalJoinMagma<int, decltype(std::ranges::max)>);
static_assert(IsMereologicalMeetSemigroup<int, decltype(std::ranges::min)>);
static_assert(IsMereologicalJoinSemigroup<int, decltype(std::ranges::max)>);
static_assert(IsMereologicalMeetBand<int, decltype(std::ranges::min)>);
static_assert(IsMereologicalJoinBand<int, decltype(std::ranges::max)>);
static_assert(IsMereologicalSkewLatticeOperations<
              int, decltype(std::ranges::max), decltype(std::ranges::min)>);

namespace detail {
// Minimal positive witness for IsMereologicalCutCandidate.
struct merc_filtered {
  constexpr bool operator<=(const int&) const { return true; }
};
struct merc_shell {
  using FilteredSpace = merc_filtered;
  constexpr merc_filtered filter(const int&) const { return {}; }
};
}  // namespace detail

static_assert(
    IsMereologicalCutCandidate<detail::merc_shell, int>,
    "Transitional cut candidate must hold for the canonical demo witness "
    "(Issue #195).");

}  // namespace dedekind::category
