/**
 * @file dedekind/category/equivalence.cppm
 * @partition :equivalence
 * @brief Equivalence relations: reflexive + symmetric + transitive predicates,
 *        composing the existing axiom traits in @c :species with a new
 *        symmetry trait.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section equivalence__Motivation
 *
 * The textbook equivalence relation @c R on a carrier @c T is reflexive
 * (@c R(x, x)), symmetric (@c R(x, y) @c ⇔ @c R(y, x)), and transitive
 * (@c R(x, y) @c ∧ @c R(y, z) @c ⇒ @c R(x, z)).  These three properties
 * are universal-property territory: the type system can name the
 * predicate @b shape (a binary relation on @c T returning a logical
 * value), but cannot quantify over all @c x, @c y, @c z to verify the
 * axioms.  Per the project's @b honesty @b obligation pattern, each axiom
 * is an engineer-supplied trait specialisation; the compiler trusts the
 * declaration; the public review process is the audit trail.
 *
 * Reflexivity and transitivity already live in @c :species
 * (@c is_reflexive_v<T, Rel> and @c is_transitive_v<T, Rel>, with the
 * canonical specialisations on @c std::less_equal that the partial-order
 * machinery in @c :posetal builds on).  This partition adds the missing
 * @b symmetry trait and bundles all three into a single
 * @c IsEquivalence<T, Rel> concept.
 *
 * @section equivalence__The_Mazur_Reading
 *
 * Per Barry Mazur's @em When @em is @em one @em thing @em equal @em to
 * @em some @em other @em thing? (in Gold \& Simons (eds.), @em Proof
 * @em and @em Other @em Dilemmas, MAA 2008), an equivalence relation
 * is the principled way to relax strict equality without abandoning
 * the algebraic gate: a carrier @c T can be treated as inhabiting a
 * stricter algebraic concept when the laws hold @b up @b to a named
 * equivalence relation, even if they don't hold under @c == in the
 * bit-perfect sense.  This partition lays the structural foundation
 * for that escape hatch (filed under #591); the first concrete pilot
 * is @c std::unsigned_integral as @c ℕ for ``sufficiently small''
 * inputs, wired in @c numbers:uint.
 *
 * @section equivalence__Use_cases
 *
 *   - @b Bounded equivalence: @c uint64_t under modular arithmetic
 *     equates to @c ℕ for inputs strictly less than @c 2^64; the
 *     equivalence captures the ``sufficiently small'' qualification at
 *     the type level.  Pilot in @c numbers:uint.
 *   - @b ε-equivalence on IEEE doubles: @c |a @c - @c b| @c < @c eps;
 *     lifts @c double's non-strict-field arithmetic into the algebraic
 *     gate.  Future work; depends on the unsigned pilot landing first.
 *   - @b Quotient algebras: @c A/~ as a carrier requires the
 *     equivalence relation @c ~ on @c A that defines the quotient
 *     (sister motivation to @c IsQuotientAlgebra in
 *     @c algebra:quotient).
 *
 * @see Mazur, B. (2008), @em When @em is @em one @em thing @em equal
 *      @em to @em some @em other @em thing?, in Gold \& Simons (eds.),
 *      @em Proof @em and @em Other @em Dilemmas: @em Mathematics @em
 *      and @em Philosophy, MAA, pp.\ 221--242.
 *
 * @note "No entity without identity."
 *       — Willard Van Orman Quine, @em Theories @em and @em Things
 *         (Harvard University Press, 1981), Ch.\ "Things and Their
 *         Place in Theories".
 */
module;

#include <concepts>
#include <functional>
#include <type_traits>

export module dedekind.category:equivalence;

import :morphism;
import :species;  // is_reflexive_v / is_transitive_v / IsReflexive /
                  // IsTransitive

namespace dedekind::category {

// The heterogeneous-shape concept @c IsBinaryRelation<R, @c A, @c B> lives
// in @c :cartesian (Level 0.5 of the DAG).  Equivalence relations are
// the homogeneous case @c A @c = @c B @c = @c T, but we don't reach for
// @c :cartesian from here so this partition stays at Level 0 alongside
// @c :mereology / @c :logic.  The structural-shape requirement (Rel{}
// is callable on @c T @c × @c T → @c bool) is carried by the
// @c IsReflexive / @c IsSymmetric / @c IsTransitive concepts directly,
// each of which already includes the @c Rel{}(a, b) callable check.

// ---------------------------------------------------------------------------
// is_symmetric: the missing axiom trait, completing the reflexive /
// symmetric / transitive triad already started in :species.
// ---------------------------------------------------------------------------

/**
 * @brief Engineer-supplied trait: relation @c Rel on @c T is symmetric
 *        (∀x, y: @c Rel(x, @c y) @c ⇔ @c Rel(y, @c x)).
 *
 * @details Default @c false; specialise to @c true to declare symmetry.
 * Follows the same shape as @c :species's @c is_reflexive /
 * @c is_transitive: a class template specialised by partial
 * specialisation, with a member-detection bridge from @c T's nested
 * @c is_symmetric_v static.
 */
export template <typename T, typename Rel>
struct is_symmetric : std::false_type {};

// Discovery: Look for a member variable 'is_symmetric_v' on T.
template <typename T, typename Rel>
  requires requires { T::template is_symmetric_v<Rel>; }
struct is_symmetric<T, Rel>
    : std::bool_constant<T::template is_symmetric_v<Rel>> {};

/** @brief The "v" helper following the @c :species convention. */
export template <typename T, typename Rel>
inline constexpr bool is_symmetric_v = is_symmetric<T, Rel>::value;

/**
 * @concept IsSymmetric
 * @brief @b Formal verification: @c Rel(a, @c b) @c ⇔ @c Rel(b, @c a).
 *
 * Mirrors @c IsReflexive / @c IsTransitive in @c :species: structural
 * shape (@c Rel{} is callable on the symmetric pair) plus the
 * engineer-supplied trait declaration.
 */
export template <typename T, typename Rel>
concept IsSymmetric = requires(T a, T b) {
  { Rel{}(a, b) } -> std::convertible_to<bool>;
  { Rel{}(b, a) } -> std::convertible_to<bool>;
} && requires { requires is_symmetric_v<T, Rel>; };

// ---------------------------------------------------------------------------
// std::equal_to<T> as the canonical equivalence relation on integral
// (non-floating-point) carriers.
// ---------------------------------------------------------------------------
//
// std::equal_to<T> is the project's canonical equivalence relation on any
// std::regular T whose == is bit-perfect: reflexive, symmetric, and
// transitive by the std::regular contract.  We deliberately exclude
// floating-point carriers here because IEEE-754 NaN breaks reflexivity
// (NaN == NaN is false), so std::equal_to<double> is NOT a textbook
// equivalence relation despite double being std::regular.  The Mazur-
// equivalence escape hatch (#591) is the right home for the floating-
// point case; ε-equivalence rather than == is the operative relation
// there.
//
// The integral constraint matches the policy in @c category:cartesian
// (which anchors integral-only registrations on the same axis).  Pin
// all three traits via partial specialisation so the canonical integral
// case fires without per-carrier opt-in.

template <typename T>
  requires std::regular<T> && (!std::floating_point<T>)
struct is_reflexive<T, std::equal_to<T>> : std::true_type {};

template <typename T>
  requires std::regular<T> && (!std::floating_point<T>)
struct is_symmetric<T, std::equal_to<T>> : std::true_type {};

template <typename T>
  requires std::regular<T> && (!std::floating_point<T>)
struct is_transitive<T, std::equal_to<T>> : std::true_type {};

// ---------------------------------------------------------------------------
// IsEquivalence: the textbook equivalence-relation concept.
// ---------------------------------------------------------------------------

/**
 * @concept IsEquivalence
 * @brief A binary relation on @c T that is reflexive, symmetric, and
 *        transitive --- the textbook equivalence-relation axioms.
 *
 * @details Composes the structural shape (@c IsBinaryRelation) with the
 * three engineer-supplied axiom concepts (@c IsReflexive @b /
 * @c IsSymmetric @b / @c IsTransitive).  The runtime laws stay the
 * engineer's honesty obligation; the type system pins the @b shape
 * mechanically and the @b combination of axiom claims structurally.
 *
 * The Mazur @em up @em to @em equivalence reading (see partition
 * docstring) treats this concept as the principled relaxation of strict
 * equality: stating @c IsEquivalence<T, Rel> is a structural declaration
 * that @c Rel is the equivalence relation under which algebraic concepts
 * on @c T are to be read.
 *
 * @tparam T   The carrier on which @c Rel is a relation.
 * @tparam Rel The relation type.
 */
export template <typename T, typename Rel>
concept IsEquivalence =
    IsReflexive<T, Rel> && IsSymmetric<T, Rel> && IsTransitive<T, Rel>;

// Witnesses: std::equal_to<T> on std::regular T satisfies IsEquivalence
// for the canonical primitive carriers.

static_assert(IsEquivalence<int, std::equal_to<int>>,
              "std::equal_to<int> must satisfy IsEquivalence on int (the "
              "canonical witness over std::regular<T>).");
static_assert(IsEquivalence<bool, std::equal_to<bool>>,
              "std::equal_to<bool> must satisfy IsEquivalence on bool.");

// ---------------------------------------------------------------------------
// IsHashFunction: hash arrows as first-class citizens (#598/#605 scout).
// ---------------------------------------------------------------------------

/**
 * @concept IsHashFunction
 * @brief A callable @c h on @c T whose codomain is @c std::size_t —
 *        i.e. an arrow @c T @c → @c size_t.
 *
 * @details A hash function induces a canonical equivalence relation on
 * its domain:
 * @f[ x \,\sim_h\, y \iff h(x) \,=\, h(y), @f]
 * the @b kernel of @c h read as a binary relation.  This relation is
 * automatically reflexive / symmetric / transitive as a consequence of
 * @c size_t equality being so on @c std::size_t — i.e. every
 * @c IsHashFunction structurally @b induces an @c IsEquivalence.  The
 * partition is the fibres @f$h^{-1}(s)@f$ for @f$s \in \mathrm{im}(h)@f$,
 * grouping inputs that collide under @c h.
 *
 * The concept lives next to @c IsEquivalence because the equivalence
 * relation @b is the hash's mathematical content; the @c size_t codomain
 * is a representation choice (the arithmetic-friendly fibre index, used
 * by @c std::unordered_set / @c std::unordered_map for bucket dispatch).
 *
 * @tparam H The hash callable (e.g.\ @c std::hash<T>).
 * @tparam T The carrier the hash is defined on.
 *
 * @b Recommendation (per Gemini correspondence, 2026-05-06): the home
 * of the base concept is the equivalence-relation layer; algebra-
 * specific refinements (rolling hashes as Monoid → Ring homomorphisms,
 * etc.) belong further downstream once they materialise.
 */
export template <typename H, typename T>
concept IsHashFunction = requires(const H& h, const T& x) {
  { h(x) } -> std::convertible_to<std::size_t>;
};

// Witness: the canonical hash on a primitive carrier satisfies the
// concept (and so does the std::regular equality next to it — the
// pair (std::hash<int>, std::equal_to<int>) is the textbook
// fibre-partition witness for `int`).
static_assert(IsHashFunction<std::hash<int>, int>,
              "std::hash<int> is the canonical hash arrow int → size_t.");
static_assert(IsHashFunction<std::hash<bool>, bool>,
              "std::hash<bool> is the canonical hash arrow bool → size_t.");

}  // namespace dedekind::category
