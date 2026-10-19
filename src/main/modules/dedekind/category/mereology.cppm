/**
 * @file dedekind/category/mereology.cppm
 * @partition :mereology
 * @brief Embryonic Mereology (Part-Whole and Skew Algebraic Core).
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
 * @section mereology__Axioms
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
#include <optional>  // negative witness: std::optional<double> (NaN-containing order)
#include <type_traits>  // std::remove_cvref_t: cv-normalise op keys (#934)

export module dedekind.category:mereology;

import :species;
import :morphism;

namespace dedekind::category {

/**
 * @concept IsTruthObject
 * @brief @c Ω may carry a relation's answer in the laws below: it has a top
 *        @c ⊤, the value a law asserts (reflexivity is @c rel(a, @c a) @c = @c
 *        ⊤).  @c bool (top @c true) is the default; @c Ternary names its
 *        @c Ω::True, a value type with a species (@c Percentage, the bounded
 *        chains) finds it on the species.  A statement of
 *        intent, not a certificate: the laws come before logic, so the truth
 *        objects are only certified downstream (@c :logic, @c IsΩ).
 */
export template <typename Ω>
concept IsTruthObject = std::same_as<Ω, bool> || requires { Ω::True; } ||
                        requires { Ω::logic_species::True; };

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

/** @section mereology__The_Reflexivity_Axiom */
// The built-in integral / @c bool @c <= is reflexive; the same rule as
// transitivity and antisymmetry below (#1043).  @c std::totally_ordered<T> is
// no certificate: it is syntactic, and @c double satisfies it although
// @c NaN @c <= @c NaN is false.  Any other carrier registers its own order.
template <typename T>
  requires std::is_integral_v<T> || std::is_same_v<T, bool>
struct is_reflexive<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsReflexive
 * @brief Formal verification of the identity relation.
 */
export template <typename T, typename Rel, typename Ω = bool>
concept IsReflexive = IsTruthObject<Ω> && requires(const Rel& rel, const T& a) {
  { rel(a, a) } -> std::convertible_to<Ω>;
} && requires { requires is_reflexive_v<T, Rel>; };

/** @section mereology__Transitivity: (a <= b && b <= c) => a <= c */
export template <typename T, typename Rel>
struct is_transitive : std::false_type {};

template <typename T, typename Rel>
  requires requires { T::template is_transitive_v<Rel>; }
struct is_transitive<T, Rel>
    : std::bool_constant<T::template is_transitive_v<Rel>> {};

export template <typename T, typename Rel>
inline constexpr bool is_transitive_v = is_transitive<T, Rel>::value;

// The built-in integral / @c bool @c <= is a genuine total order, hence
// transitive (and antisymmetric, below).  Enums are DELIBERATELY excluded: a
// scoped enum may supply an ADL @c operator<= / @c operator<=> that is neither
// transitive nor antisymmetric, so a blanket over @c is_enum_v<T> would
// unsoundly certify ARBITRARY enums as partial orders.  A specific enum carrier
// whose order is known (e.g. @c Ternary) hand-registers both legs at its home
// partition (@c :logic).  @c double is neither integral nor @c bool, so the
// raw-float rejection is preserved (#933 / #934).
template <typename T>
  requires std::is_integral_v<T> || std::is_same_v<T, bool>
struct is_transitive<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsTransitive
 * @brief Formal verification: (a ≤ b ∧ b ≤ c) ⇒ a ≤ c.
 */
export template <typename T, typename Rel, typename Ω = bool>
concept IsTransitive =
    IsTruthObject<Ω> && requires(const Rel& rel, const T& a, const T& b) {
      { rel(a, b) } -> std::convertible_to<Ω>;
    } && requires { requires is_transitive_v<T, Rel>; };

/** @section mereology__Antisymmetry: (a <= b && b <= a) => a == b */
export template <typename T, typename Rel>
struct is_antisymmetric : std::false_type {};

template <typename T, typename Rel>
  requires requires { T::template is_antisymmetric_v<Rel>; }
struct is_antisymmetric<T, Rel>
    : std::bool_constant<T::template is_antisymmetric_v<Rel>> {};

export template <typename T, typename Rel>
inline constexpr bool is_antisymmetric_v = is_antisymmetric<T, Rel>::value;

// Enums are excluded for the same reason as transitivity: an enum's ADL @c <=
// may fail antisymmetry, so blanketing @c is_enum_v<T> is unsound.  A known
// enum carrier (@c Ternary) hand-registers this leg at @c :logic; @c double is
// neither integral nor @c bool, preserving the raw-float rejection (#933/#934).
template <typename T>
  requires std::is_integral_v<T> || std::is_same_v<T, bool>
struct is_antisymmetric<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsAntisymmetric
 * @brief Formal verification: (a ≤ b ∧ b ≤ a) ⇒ a = b.
 */
export template <typename T, typename Rel, typename Ω = bool>
concept IsAntisymmetric =
    IsTruthObject<Ω> && requires(const Rel& rel, const T& a, const T& b) {
      { rel(a, b) } -> std::convertible_to<Ω>;
    } && requires { requires is_antisymmetric_v<T, Rel>; };

/** @section mereology__Connexity: a <= b || b <= a
 *  Any two elements are comparable: the law that makes a partial order a
 *  chain.  In Rel it reads @c ⊤ @c ≤ @c R @c ∪ @c R°; its disjunction is why
 *  it is not inherited by products (a product of chains is no chain), so it is
 *  certified per carrier, never derived componentwise.  Distinct from
 *  @c is_total_v (@c :total), the totality of an @b operation. */
export template <typename T, typename Rel>
struct is_connex : std::false_type {};

template <typename T, typename Rel>
  requires requires { T::template is_connex_v<Rel>; }
struct is_connex<T, Rel> : std::bool_constant<T::template is_connex_v<Rel>> {};

export template <typename T, typename Rel>
inline constexpr bool is_connex_v = is_connex<T, Rel>::value;

// The built-in integral / @c bool @c <= is a chain.
template <typename T>
  requires std::is_integral_v<T> || std::is_same_v<T, bool>
struct is_connex<T, std::less_equal<T>> : std::true_type {};

/**
 * @concept IsConnex
 * @brief Formal verification: a ≤ b ∨ b ≤ a.
 */
export template <typename T, typename Rel, typename Ω = bool>
concept IsConnex =
    IsTruthObject<Ω> && requires(const Rel& rel, const T& a, const T& b) {
      { rel(a, b) } -> std::convertible_to<Ω>;
    } && requires { requires is_connex_v<T, Rel>; };

/**
 * @brief Proof: @c <= on the integral types is a total order (reflexive,
 * transitive, antisymmetric, connex), under the transparent spelling as under
 * the typed one above.
 * @note Floating-point types are excluded: NaN breaks reflexivity and
 * connexity.  The NaN-free carrier is @c safe_float (@c :morphologies).
 */
template <std::integral T>
inline constexpr bool is_reflexive_v<T, std::less_equal<>> = true;

template <std::integral T>
inline constexpr bool is_transitive_v<T, std::less_equal<>> = true;

template <std::integral T>
inline constexpr bool is_antisymmetric_v<T, std::less_equal<>> = true;

template <std::integral T>
inline constexpr bool is_connex_v<T, std::less_equal<>> = true;

/** @section mereology__Lattice_Op_Gate (Sup / Inf law gate)
 *
 * @details The value-returning @c Sup / @c Inf order-lattice law blanket below
 * is gated on @c SupInfLattice<T> so it never over-certifies a carrier on which
 * the ops are not honest lattice ops.  The gate is defined here, upstream of
 * the blanket.  (#934 retired the parallel @c std::ranges::max / @c
 * std::ranges::min niebloid blanket that this gate used to guard as well.)
 *
 * @par Either @c std::less_equal spelling, complete per relation.
 * Carriers register their order traits under one of two conventions: the typed
 * @c std::less_equal<T> (@c int / @c bool via the integral blanket, @c Ternary)
 * or the transparent @c std::less_equal<> (@c safe_float<F>, @c Rational<I>,
 * @c Cut<Q>, cardinals).  Both are @c :species traits (no DAG dependency on
 * @c :order, which is downstream), so the gate accepts a certificate under
 * @b either spelling.  The three laws are grouped @b per @b relation and the
 * two @b complete certificates are OR'd, @b not OR'd law-by-law: a mixed
 * carrier (antisymmetry certified typed, transitivity only transparent) has all
 * four laws under @b neither single relation, so it must not pass.
 *
 * @par A chain, not a poset (#1043).
 * The certificate is a @b total order: reflexive, transitive, antisymmetric
 * @b and connex.  @c Sup / @c Inf are @c max / @c min, honest lattice ops only
 * on a chain: on a poset, @c Sup of incomparable @c a, @c b is order-dependent,
 * so @c is_commutative_v<T, Sup> would be false.  Connexity is certified per
 * carrier (@c is_connex_v), not read off the syntactic
 * @c std::totally_ordered<T>, which @c double satisfies. */

/**
 * @concept SupInfOperand
 * @brief A carrier on which @c Sup / @c Inf (and @c max / @c min) are a
 * genuine, usable value op: totally ordered (for @c <) and copy-constructible
 * (the winner is @b returned @b by @b value).  This is the CALL gate: @c
 * Sup{}(1.0, 2.0) is a valid call that returns a value, so raw floats are @b
 * not excluded here; only the lattice @b laws fail on them (see @c
 * SupInfLattice).
 * @tparam T The candidate carrier type.
 */
export template <typename T>
concept SupInfOperand = std::totally_ordered<T> && std::copy_constructible<T>;

/**
 * @concept SupInfLattice
 * @brief A carrier on which @c max / @c min (and @c Sup / @c Inf) are honest
 *        @b lattice ops: usable (@c SupInfOperand) @b and carrying a certified
 *        order.
 * @details Requires a @b complete total-order certificate (reflexive,
 * transitive, antisymmetric, connex) under @b one @c std::less_equal spelling
 * --- typed @c std::less_equal<T> @b or transparent @c std::less_equal<> (see
 * @c mereology__Lattice_Op_Gate for why the legs are grouped per relation).
 * @tparam T The candidate carrier type.
 */
export template <typename T>
concept SupInfLattice =
    SupInfOperand<T> && ((is_reflexive_v<T, std::less_equal<T>> &&
                          is_transitive_v<T, std::less_equal<T>> &&
                          is_antisymmetric_v<T, std::less_equal<T>> &&
                          is_connex_v<T, std::less_equal<T>>) ||
                         (is_reflexive_v<T, std::less_equal<>> &&
                          is_transitive_v<T, std::less_equal<>> &&
                          is_antisymmetric_v<T, std::less_equal<>> &&
                          is_connex_v<T, std::less_equal<>>));

// #934: the ref-returning @c std::ranges::max / @c std::ranges::min niebloid
// lattice-law blanket was retired here.  Its semilattice / distributive laws
// now live solely on the value-returning @c Sup / @c Inf ops (see
// @c mereology__Value_Lattice_Ops), still gated on @c SupInfLattice<T>.

/** @section mereology__Value_Lattice_Ops (Sup / Inf)
 *
 * @brief Value-returning chain lattice ops --- the honest @c T @c × @c T @c →
 *        @c T meet / join.
 *
 * @details The @c std::ranges::max / @c std::ranges::min niebloids @b select
 * and @b alias an argument: they return @c const @c T&, not a fresh @c T.  So
 * @c IsClosedUnder<T, Op> (which demands @c { @c Op{}(a,b) @c } @c -> @c
 * same_as<T>) rejects them, and the strict monoid / bounded-lattice ladder
 * (@c IsMonoid @c → @c IsMagma @c → @c IsClosedUnder in @c :total) is
 * unreachable through the niebloid --- only the lenient @c
 * IsOrderLatticeOperations path (which checks @c convertible_to<T>) admits it.
 * @c Sup / @c Inf return a @b copy of the winner, so they are honest @c
 * T @c × @c T @c → @c T ops that reach the whole ladder and never dangle on
 * temporaries.  They compute the ternary directly (no niebloid inside).
 *
 * @b Sole @b lattice @b ops (#934): @c Sup / @c Inf are now the @b only
 * carriers of the order-lattice laws (idempotence, associativity,
 * commutativity, distributivity, absorption).  The ref-returning @c
 * std::ranges::max /
 * @c std::ranges::min niebloids no longer carry any lattice-law registration
 * --- every carrier lattice-op site (defaults + concept template args across
 * @c :lattice / @c :mereology / @c :posetal / @c :total / @c :order) was
 * migrated to @c Sup / @c Inf, retiring the pre-existing niebloid blanket.
 * Reaching the strict monoid / bounded-lattice rung (@c IsClosedUnder needs @c
 * same_as<T>) may flip a carrier's bounded-lattice status (see #941 for @c
 * Chain<int>). The law blanket stays gated on the @c SupInfLattice certificate
 * (see
 * @c mereology__Lattice_Op_Gate above). */

/** @brief Join @c ∨: the least upper bound on a chain (@c max), returned
 *  @b by value as an honest @c T @c × @c T @c → @c T. */
export struct Sup {
  /** @brief Value-returning join: returns @c max(a,b) by value (a copy of the
   *  winner), never a reference into an argument. */
  template <SupInfOperand T>
  constexpr T operator()(const T& a, const T& b) const {
    return a < b ? b : a;
  }
};
/** @brief Meet @c ∧: the greatest lower bound on a chain (@c min), returned
 *  @b by value.  Value-returning sibling of @c Sup. */
export struct Inf {
  /** @brief Value-returning meet: returns @c min(a,b) by value (a copy of the
   *  winner), never a reference into an argument. */
  template <SupInfOperand T>
  constexpr T operator()(const T& a, const T& b) const {
    return a < b ? a : b;
  }
};

// Sup (∨) / Inf (∧) are the SOLE carriers of the order-lattice laws (#934
// retired the parallel std::ranges::max / min niebloid blanket); registered
// here since the concepts are trait-gated (IsIdempotent / IsCommutative / ...
// look up is_*_v, they are not structural).
//
// The SupInfLattice<T> gate is load-bearing, NOT decoration.  The trait check
// never instantiates Sup/Inf, so an unconstrained blanket would certify laws
// for types where the op is not even a usable value function or where the laws
// fail:
//   * non-totally_ordered / non-copyable T: Sup/Inf can't be called at all
//     (e.g. is_commutative_v<void, Sup> would spuriously be true);
//   * any carrier whose order contains NaN: raw double, but ALSO wrappers like
//     std::optional<double> (totally_ordered + copyable + NOT floating_point),
//     where max/min are non-commutative (Sup{}(NaN,x)=NaN vs Sup{}(x,NaN)=x).
// SupInfLattice certifies POSITIVELY via the repo's total-order traits
// (reflexive/transitive/antisymmetric under std::less_equal<T>) rather than a
// negative !floating_point proxy: a genuine total order cannot contain NaN, so
// double AND optional<double> --- which never register those traits --- are
// both excluded, whereas integrals / scoped-enum chains register them and pass.
// Without the gate this blanket would falsely certify
// IsDistributiveLattice<double, ...> / <optional<double>, ...>, contradicting
// the library's raw-float rejection.
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_idempotent_v<T, Sup> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_associative_v<T, Sup> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_commutative_v<T, Sup> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_idempotent_v<T, Inf> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_associative_v<T, Inf> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_commutative_v<T, Inf> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_distributive_v<T, Sup, Inf> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_distributive_v<T, Inf, Sup> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_absorptive_v<T, Sup, Inf> = true;
template <typename T>
  requires SupInfLattice<T>
inline constexpr bool is_absorptive_v<T, Inf, Sup> = true;

// Negative witnesses (co-located; :species is upstream of :total, so assert the
// trait directly, not IsDistributiveLattice).
// (1) Raw floats: NaN breaks reflexivity and connexity of <= (NaN <= NaN and
// NaN <= x are false), so double carries no order law under either spelling
// (#1043: std::totally_ordered<double> holds, and certifies nothing).
static_assert(!IsReflexive<double, std::less_equal<double>> &&
                  !IsReflexive<double, std::less_equal<>> &&
                  !IsConnex<double, std::less_equal<double>> &&
                  !IsConnex<double, std::less_equal<>> &&
                  !SupInfLattice<double>,
              "<= on double is not reflexive (NaN <= NaN is false), so double "
              "carries no order certificate under either spelling");
static_assert(!is_commutative_v<double, Sup>,
              "raw floats rejected: NaN breaks max/min commutativity (no "
              "double lattice carrier)");
static_assert(!is_commutative_v<double, Inf>,
              "raw floats rejected: NaN breaks max/min commutativity (no "
              "double lattice carrier)");
// (2) A float-CONTAINING wrapper: std::optional<double> is totally_ordered +
// copyable + NOT floating_point, so a !floating_point proxy would have leaked
// it in.  The positive certified-order gate excludes it (it registers the
// transitive/antisymmetric order traits under NEITHER std::less_equal spelling)
// --- this pins that the ORDER cert, not a type-category float check, is doing
// the work.
static_assert(SupInfOperand<std::optional<double>> &&
                  !SupInfLattice<std::optional<double>>,
              "optional<double> IS a usable Sup/Inf operand (ordered + "
              "copyable) yet fails the certified-order gate --- so the ORDER "
              "cert, not a !floating_point proxy, is what excludes it");
static_assert(!is_commutative_v<std::optional<double>, Sup>,
              "float-containing wrappers rejected: optional<double>'s order "
              "contains NaN, so it is not a lattice carrier");
// (3) Non-usable carriers: the trait check never instantiates Sup/Inf, so
// without SupInfLattice a type on which the op cannot even be called (not
// totally_ordered / not copy_constructible --- here void) would be spuriously
// certified.
static_assert(!is_commutative_v<void, Sup>,
              "SupInfLattice gate: a non-usable carrier (void: neither ordered "
              "nor copyable) is not a Sup/Inf lattice carrier");
// Positive-preserved witness: every certified chain keeps the laws (int and
// the scoped-enum Ternary carrier alike).
static_assert(SupInfLattice<int> && is_commutative_v<int, Sup> &&
                  is_idempotent_v<int, Inf>,
              "int (and every SupInfLattice carrier) keeps the Sup/Inf lattice "
              "laws");

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
 * A total order is a partial order where any two elements are comparable:
 * @c Rel on @c T is connex (@c is_connex_v), certified per carrier like the
 * other three laws, never read off the syntactic @c std::totally_ordered<T>
 * (#1043).
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename Ω = bool>
concept IsTotalOrder = IsPartialOrder<T, Rel, Ω> && is_connex_v<T, Rel> &&
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
 * Default witness is `Sup`, matching the canonical join witness
 * on totally ordered carriers.
 */
export template <typename T, typename Join = Sup>
concept IsMereologicalJoinMagma = requires(Join join, T a, T b) {
  { join(a, b) } -> std::convertible_to<T>;
};

/**
 * @concept IsMereologicalMeetMagma
 * @brief Stage 0 for meet-like algebra: closure only.
 *
 * @details
 * Default witness is `Inf`, matching the canonical meet witness
 * on totally ordered carriers.
 */
export template <typename T, typename Meet = Inf>
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
export template <typename T, typename Join = Sup>
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
export template <typename T, typename Meet = Inf>
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
export template <typename T, typename Meet = Inf>
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
export template <typename T, typename Join = Sup>
concept IsMereologicalJoinBand =
    IsMereologicalJoinSemigroup<T, Join> && IsIdempotent<T, Join>;

/**
 * @concept IsAbsorptive
 * @brief Mutual absorption and internal idempotency for a pair of
 *        operations.
 *
 * @details Verifies that @c Op1 and @c Op2 are dual partners:
 *   1. @c Op1 absorbs @c Op2: @f$a \vee (a \wedge b) = a@f$
 *   2. @c Op2 absorbs @c Op1: @f$a \wedge (a \vee b) = a@f$
 *   3. Both are idempotent (inherent in absorption, but verified for
 *      rigor).
 *
 * Hosted here in @c :mereology (lifted from @c :species under #387)
 * because absorption is a lattice-shaped fact about pairs of
 * operations, not a tier-zero trait of single operations --- the
 * downstream lattice concepts in @c :mereology / @c :posetal /
 * @c :total are the natural consumers.  The underlying trait
 * variable @c is_absorptive_v stays in @c :species (where the rest
 * of the trait registry lives, and where carriers opt in).
 */
export template <typename T, typename Op1, typename Op2>
concept IsAbsorptive =
    IsIdempotent<T, Op1> && IsIdempotent<T, Op2> &&
    // cv-strip the op types so a const-qualified op (decltype of a constexpr
    // object) keys the same is_absorptive_v registration as the bare type
    // (#934); the IsIdempotent wrappers already normalise internally.
    is_absorptive_v<T, std::remove_cvref_t<Op1>, std::remove_cvref_t<Op2>> &&
    is_absorptive_v<T, std::remove_cvref_t<Op2>, std::remove_cvref_t<Op1>>;

/**
 * @concept IsMereologicalSkewLattice
 * @brief Non-commutative lattice-style operations (skew lattice fragment).
 *
 * @details
 * Requires associative + idempotent join/meet operations connected by
 * absorption. Commutativity is intentionally not required.
 */
export template <typename T, typename Join = Sup, typename Meet = Inf>
concept IsMereologicalSkewLattice =
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
export template <typename T, typename Meet = Inf>
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
export template <typename T, typename Join = Sup>
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
 * `IsMereologicalSkewLattice`.
 *
 * @see https://en.wikipedia.org/wiki/Skew_lattice
 */
export template <typename T, typename Join = Sup, typename Meet = Inf>
concept IsMereologicalLatticeOperations =
    IsMereologicalJoinSemilattice<T, Join> &&
    IsMereologicalMeetSemilattice<T, Meet>;

/**
 * @concept IsMereologicalDistributiveLatticeOperations
 * @brief Mature distributive stage: lattice operations with mutual
 * distribution.
 */
export template <typename T, typename Join = Sup, typename Meet = Inf>
concept IsMereologicalDistributiveLatticeOperations =
    IsMereologicalLatticeOperations<T, Join, Meet> &&
    IsDistributive<T, Join, Meet> && IsDistributive<T, Meet, Join>;

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
static_assert(!IsTotalOrder<double>,
              "<= on double is no total order: NaN is comparable with nothing "
              "(#1043; std::totally_ordered<double> certifies nothing)");

static_assert(IsMereologicalMeetMagma<int, Inf>);
static_assert(IsMereologicalJoinMagma<int, Sup>);
static_assert(IsMereologicalMeetSemigroup<int, Inf>);
static_assert(IsMereologicalJoinSemigroup<int, Sup>);
static_assert(IsMereologicalMeetBand<int, Inf>);
static_assert(IsMereologicalJoinBand<int, Sup>);
static_assert(IsMereologicalSkewLattice<int, Sup, Inf>);

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
