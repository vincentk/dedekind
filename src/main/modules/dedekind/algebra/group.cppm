/**
 * @file dedekind/algebra/group.cppm
 * @partition :group
 * @brief The Symmetry of Numbers (ℤ, ℚ*).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.

 *
 * @section group__Taxonomy_of_Symmetry
 * This partition establishes the "Rules of Reflection" (Inverses).
 * It promotes Monoids to Groups by verifying the existence of
 * symmetric elements: the negative (-x) for addition and the
 * reciprocal (1/x) for multiplication.
 *
 * @note "Det er utvilsomt at gruppeteorien er den mest fundamentale del av
 *       den moderne matematikken."
 *       ("It is beyond doubt that group theory is the most fundamental part
 *       of modern mathematics.")
 *       -- Sophus Lie, Gesammelte Abhandlungen
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:group;

import :monoid;
import :universal;  // IsAlgebra (universal-algebra closure tier; #517)
import dedekind.category;

namespace dedekind::algebra {

/**
 * @concept HasGroupOperatorsAdd
 * @brief @b Pure @b syntactic @b shape: T closes strictly under the
 *        additive-group operator surface @c +, binary @c -, unary @c -.
 *
 * @details
 * Sibling of @c HasRingOperators (in @c :ring) restricted to the
 * additive-group surface --- the operators a callsite needs to spell
 * group-theoretic translations of `a + b`, `a - b`, `-a`.  Strict
 * closure: each result is exactly @c T, not just convertible to
 * @c T.  Carriers that fail strict closure under literal @c + (e.g.\
 * @c bool, @c unsigned char, @c unsigned short --- integer promotion
 * to @c int) correctly refuse, matching the math-textbook stance the
 * project adopts (#393).
 *
 * Introduced under #394 as the additive-group companion of
 * @c HasRingOperators / @c HasFieldOperators.
 */
export template <typename T>
concept HasGroupOperatorsAdd = requires(T a, T b) {
  { a + b } -> std::same_as<T>;
  { a - b } -> std::same_as<T>;
  { -a } -> std::same_as<T>;
};

/**
 * @concept HasGroupOperatorsMul
 * @brief @b Pure @b syntactic @b shape: T closes strictly under the
 *        multiplicative-group operator surface @c *, @c /, and
 *        exposes the multiplicative unit @c T{1}.
 *
 * @details
 * Sibling of @c HasGroupOperatorsAdd for the multiplicative side.
 * C++ has no unary @c 1/x operator (only @c *, @c &, @c +, @c -,
 * @c ++, @c --, @c ~, @c ! can be unary); the convention here is to
 * witness the multiplicative @b unit (the identity element @c 1) via
 * the structural constructor @c T{1}, parallel to @c
 * HasSemiringOperators's @c T{1} requirement.  The reciprocal of @c x is
 * then @c T{1} / x at the callsite, with no carrier-imposed @c
 * .inverse() method needed.
 *
 * The triple @c (*, /, T{1}) is the multiplicative-group surface a
 * callsite needs to write @c a * b, @c a / b, and recover the unit
 * @c T{1}, with results that stay in @c T.
 *
 * As with all shape concepts, this checks @b syntactic closure only;
 * it makes no semantic claim.  @c HasGroupOperatorsMul<unsigned int>
 * fires (@c *, @c /, @c unsigned int{1} all compile and yield @c
 * unsigned int) even though @c unsigned int is @b not a
 * multiplicative group --- the integer @c / is truncation, not the
 * field inverse.  The semantic claim "this carrier is a
 * multiplicative group" lives in @c IsMultiplicativeGroup, gated by
 * the species-trait registry.
 *
 * Canonical positive witness: @c Rational<I> (closes strictly under
 * @c (*, /, T{1})).  Negative witness (shape refuses): @c bool, @c
 * unsigned char, @c unsigned short --- integer promotion lifts @c *
 * and @c / to @c int.
 *
 * Introduced under #394.
 */
export template <typename T>
concept HasGroupOperatorsMul = requires(T a, T b) {
  { a * b } -> std::same_as<T>;
  { a / b } -> std::same_as<T>;
  T{1};
};

/**
 * @concept HasSuccessorOperators
 * @brief @b Pure @b syntactic @b shape: T supports the
 *        successor / predecessor operator surface (pre- and
 *        post-increment / -decrement, all four).
 *
 * @details
 * The Peano-flavoured shape concept naming the C++ operator surface
 * @c {++a, --a, a++, a--}.  The pre-form returns @c T&; the post-form
 * returns @c T (by value).  Mathematically, this is the
 * successor / predecessor surface --- the canonical generator of an
 * @c IsCyclicGroup or the position-advancement surface of an iterator.
 *
 * Sibling of @c HasGroupOperatorsAdd: this concept is the @b unary
 * @b in-place arithmetic (translate by 1), where @c HasGroupOperatorsAdd
 * is the binary out-of-place arithmetic.  The semantic claim "this
 * carrier carries a Peano successor structure" lives in @c
 * morphologies:cyclic (where @c Modular<N>'s coherence witnesses
 * landed under #391); the shape concept here only checks the
 * operators compile and yield the standard reference / value return
 * types.
 *
 * Deliberately stricter than @c std::weakly_incrementable / @c
 * std::incrementable: the standard concepts loosen postfix @c ++ to
 * allow any return type (to accommodate iterator categories).  The
 * Peano-aligned form requires @c a++ to return @c T by value, which
 * is what arithmetic carriers do but iterator categories do not.
 *
 * Carriers that fire: @c int, @c unsigned int, @c unsigned short, all
 * @c std::integral types, and also @c float / @c double (the shape
 * compiles --- @c ++a and @c --a are well-formed on floating-point
 * types).  The @b semantic claim "this carrier is a Peano successor
 * structure" lives in @c IsCyclicGroup and friends; IEEE
 * floating-point fails that semantic claim (rounding makes
 * @c ++a not coherent with the additive @c +1) but passes the
 * pure-shape check.  Carriers that refuse the shape: @c bool ---
 * pre/post @c ++ and @c -- on @c bool were removed in C++17, so
 * the operators are syntactically ill-formed.
 *
 * Introduced under #394.
 */
export template <typename T>
concept HasSuccessorOperators = requires(T a) {
  { ++a } -> std::same_as<T&>;
  { --a } -> std::same_as<T&>;
  { a++ } -> std::same_as<T>;
  { a-- } -> std::same_as<T>;
};

/**
 * @concept HasCompoundGroupOperatorsAdd
 * @brief @b Pure @b syntactic @b shape: T supports the in-place
 *        additive-group operator surface @c +=, @c -=.
 *
 * @details
 * The compound-assignment sibling of @c HasGroupOperatorsAdd: where
 * the latter checks the binary out-of-place operators @c +, @c -
 * (and unary @c -), this concept checks the in-place compound forms
 * @c +=, @c -=.  Both yield @c T& (the standard form).
 *
 * Mathematically, @c += / @c -= realise the additive group's @b action
 * on its own underlying set (translate-in-place by another element of
 * the group); @c HasSuccessorOperators is the special case where the
 * translation is by the unit @c 1.
 *
 * @b Subtle @b interaction @b with @b integer @b promotion: per
 * @c [expr.ass]/3, @c a += b is equivalent to @c a = T(a + b) ---
 * the implicit narrowing conversion brings the promoted result back
 * to @c T.  So @c HasCompoundGroupOperatorsAdd<unsigned short> @b
 * fires even though @c HasGroupOperatorsAdd<unsigned short> @b
 * refuses (because @c a + b returns @c int via promotion, not @c
 * unsigned short).  This is a real distinction the type system
 * surfaces: narrow unsigned types close under the in-place compound
 * forms but not under the out-of-place binary forms.
 *
 * Introduced under #394.
 */
export template <typename T>
concept HasCompoundGroupOperatorsAdd = requires(T a, T b) {
  { a += b } -> std::same_as<T&>;
  { a -= b } -> std::same_as<T&>;
};

/**
 * @concept HasCompoundGroupOperatorsMul
 * @brief @b Pure @b syntactic @b shape: T supports the in-place
 *        multiplicative-group operator surface @c *=, @c /=.
 *
 * @details
 * The multiplicative companion of @c HasCompoundGroupOperatorsAdd.
 * Where @c HasGroupOperatorsMul checks the out-of-place binary
 * operators @c *, @c / (plus the @c T{1} unit), this concept checks
 * the in-place compound forms @c *=, @c /=.  Both yield @c T& (the
 * standard form).
 *
 * The same @c [expr.ass]/3 narrowing-after-promotion rule that makes
 * @c HasCompoundGroupOperatorsAdd<unsigned short> fire also applies
 * to @c *= / @c /=, so narrow unsigned types close under the in-place
 * forms but not under the out-of-place ones.  Unlike its additive
 * sibling, this concept does @b not require a multiplicative unit
 * (the unit is a value-level claim, captured by @c HasGroupOperatorsMul);
 * the in-place surface is purely about state-mutation closure.
 *
 * Introduced under #394.
 */
export template <typename T>
concept HasCompoundGroupOperatorsMul = requires(T a, T b) {
  { a *= b } -> std::same_as<T&>;
  { a /= b } -> std::same_as<T&>;
};

/**
 * @concept IsAdditiveGroup
 * @brief Proposition: The species (T, +) forms an Abelian Group (ℤ).
 * @details Operator is configurable; default witness is `std::plus<T>`
 * (the canonical `+`).
 * @tparam T The carrier type.
 * @tparam Add The additive operation witness (defaults to `std::plus<T>`).
 */
export template <typename T, typename Add = std::plus<T>>
concept IsAdditiveGroup = IsGroup<T, Add> && IsAlgebra<T, Add>;

/**
 * @concept IsMultiplicativeGroup
 * @brief Proposition: The non-zero species (T*, *) forms a Group (ℚ*).
 * @details Operator is configurable; default witness is
 * `std::multiplies<T>` (the canonical `*`).
 * @tparam T The carrier type.
 * @tparam Mult The multiplicative operation witness (defaults to
 * `std::multiplies<T>`).
 */
export template <typename T, typename Mult = std::multiplies<T>>
concept IsMultiplicativeGroup =
    dedekind::category::IsGroup<T, Mult> && IsAlgebra<T, Mult>;

/**
 * @concept IsArithmeticAdditiveGroup
 * @brief @b The @b ℤ-deal: a carrier that is an additive group AND
 *        whose group structure agrees with the standard C++ @c +,
 *        @c -, unary @c - operators.
 *
 * @details
 * Where the concepts meet for the additive-group surface, parallel
 * to @c IsArithmeticRing (@c :ring) for the ring surface.
 * @c IsArithmeticAdditiveGroup<T> requires both:
 *
 *   - @c IsAdditiveGroup<T, std::plus<T>> --- the strict categorical
 *     additive-group proof under the canonical @c + functor;
 *   - @c HasGroupOperatorsAdd<T> --- the literal C++ operators
 *     @c +, @c -, unary @c - close strictly on @c T.
 *
 * Canonical inhabitant: the project's exact ℤ carrier
 * @c numbers::Z1 = @c sets::SignedExtensionalCardinal<>.  Modular
 * carriers also fire (e.g.\ @c unsigned int): the modular wrap
 * @c Z/2^N Z is a finite additive group and the literal operators
 * close strictly.
 *
 * Introduced under #394 as the user-requested ℤ-deal.
 */
export template <typename T>
concept IsArithmeticAdditiveGroup =
    IsAdditiveGroup<T, std::plus<T>> && HasGroupOperatorsAdd<T>;

/** @section group__Formal_Verification */

// During experimental reintegration, full group witnesses for int
// are deferred pending structure-proof registration in category module.

// static_assert(IsAdditiveGroup<int>, "Axiom Failure: (ℤ, +) must be a
// Group.");

// Multiplicative group verification is deferred during experimental
// reintegration while inverse witnesses are being retargeted.

// Pure-syntactic-shape witnesses: HasGroupOperatorsAdd on primitive
// carriers that close strictly under literal +, binary -, unary -.
static_assert(HasGroupOperatorsAdd<int>,
              "int has the additive-group operator surface.");
static_assert(HasGroupOperatorsAdd<unsigned int>,
              "unsigned int has the additive-group operator surface.");
// Narrow unsigneds fail strict closure (integer promotion lifts to
// int) -- consistent with HasRingOperators<unsigned short> refusing.
static_assert(!HasGroupOperatorsAdd<unsigned short>,
              "unsigned short is not closed under the literal additive "
              "operators (integer promotion to int).");

// IsArithmeticAdditiveGroup seal: unsigned int as the canonical
// modular ℤ-deal -- strict (Z/2^N Z is a finite additive group) AND
// literal (+, -, unary - all close on unsigned int).  The exact ℤ
// carrier (SignedExtensionalCardinal<>) lives in dedekind.sets and is
// witnessed there to keep the algebra layer free of upward
// dependencies.
static_assert(IsArithmeticAdditiveGroup<unsigned int>,
              "unsigned int is the canonical modular ℤ-deal: strict "
              "additive group (modular wrap) AND literal +,-,unary - "
              "close on the carrier.");

// HasSuccessorOperators: int / unsigned int / unsigned short all
// support pre/post ++/-- with the standard reference / value return
// types.  bool deliberately refuses: pre/post ++/-- on bool were
// removed in C++17 (the project's "math wins" stance happens to
// agree with the standard's deprecation here).
static_assert(HasSuccessorOperators<int>,
              "int has the successor operator surface "
              "(++a, --a, a++, a-- all well-formed).");
static_assert(HasSuccessorOperators<unsigned int>);
static_assert(HasSuccessorOperators<unsigned short>,
              "unsigned short has the successor operator surface: "
              "++ and -- on a reference modify in place and return "
              "T& / T, regardless of integer-promotion paths in "
              "binary arithmetic.");
static_assert(!HasSuccessorOperators<bool>,
              "bool does not have ++/-- (removed in C++17).");

// HasCompoundGroupOperatorsAdd: int / unsigned int / unsigned short
// all close under +=, -= because [expr.ass]/3 narrows the promoted
// result back to T.  The narrow-unsigned case is the interesting
// one: HasGroupOperatorsAdd<unsigned short> refuses (binary `+` on
// unsigned short returns int) but HasCompoundGroupOperatorsAdd does
// fire, because `a += b` is `a = T(a + b)` with the implicit narrow.
static_assert(HasCompoundGroupOperatorsAdd<int>);
static_assert(HasCompoundGroupOperatorsAdd<unsigned int>);
static_assert(HasCompoundGroupOperatorsAdd<unsigned short>,
              "unsigned short closes under in-place +=, -= via the "
              "[expr.ass]/3 narrowing-after-promotion rule, even "
              "though HasGroupOperatorsAdd<unsigned short> refuses.");

// HasCompoundGroupOperatorsMul: same [expr.ass]/3 mechanic for the
// multiplicative side.  Narrow unsigned carriers close under in-place
// *=, /= even though they do not close under out-of-place *, /.
static_assert(HasCompoundGroupOperatorsMul<int>);
static_assert(HasCompoundGroupOperatorsMul<unsigned int>);
static_assert(HasCompoundGroupOperatorsMul<unsigned short>,
              "unsigned short closes under in-place *=, /= via the "
              "[expr.ass]/3 narrowing-after-promotion rule.");

}  // namespace dedekind::algebra
