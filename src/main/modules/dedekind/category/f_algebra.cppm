/**
 * @file dedekind/category/f_algebra.cppm
 * @partition :f_algebra
 * @brief Level 2: Initial F-algebras and terminal F-coalgebras.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section f_algebra__FAlg_Universal_Property
 *
 * Pierce §5 introduces F-algebras and F-coalgebras immediately after
 * functors and before natural transformations because they are the
 * load-bearing concept that grounds recursive types and the universal
 * property of recursion.  This project's @c :functor partition already
 * carries the @b structural-shape concepts:
 *
 *   @c IsFAlgebra<Carrier, Structure, Functor>  — structure map
 *   @c F<A> @c → @c A.
 *
 *   @c IsFCoalgebra<Carrier, Structure, Functor> — structure map
 *   @c A @c → @c F<A>.
 *
 * What was missing — and what this partition adds — is the
 * @b universal-property layer:
 *
 *   @b Initial @b F-algebra: an F-algebra @c (A, α) is initial if for
 *   every other F-algebra @c (B, β) there exists a @b unique morphism
 *   @c h @c : @c A @c → @c B with @c h @c ∘ @c α @c = @c β @c ∘ @c F(h).
 *   That unique morphism is the @b catamorphism (fold).
 *
 *   @b Terminal @b F-coalgebra: dually; the unique morphism is the
 *   @b anamorphism (unfold).
 *
 * @section f_algebra__Bridge_to_NNO
 *
 * The @c :nno partition reifies Lawvere's NNO universal property as
 * the @c (z, s) shape — pedagogically the more recognisable form for
 * a working mathematician.  The @b textbook reading of NNO is that it
 * is the @b initial @b F-algebra for @c F(X) @c = @c 1 @c + @c X (the
 * "Maybe" endofunctor; Pierce §5.4, Mac Lane III–VI).  This partition
 * exposes that reading as a separate concept; the two universal
 * properties are operationally equivalent — the @c (z, s) pair
 * @b induces the structure map @c [z, @c s] @c : @c 1 @c + @c N @c →
 * @c N via coproduct copairing on the standard injections @c inl @c
 * : @c 1 @c → @c 1 @c + @c N and @c inr @c : @c N @c → @c 1 @c + @c N.
 *
 * The @c 1 in @c 1 @c + @c N is the @b terminal object of the
 * ambient category — concretely, the @c category::One @c = @c
 * std::monostate reified in the @c :limit partition (which carries
 * @c IsTerminalObject<One> as a static_assert).  The categorical
 * pattern @c IsInitialFAlgebra is itself one level up from the
 * @c IsInitialObject reification in @c :limit (which pins
 * @c category::Zero @c = @c std::nullptr_t as the initial in Set):
 * the F-Alg category has its own initial object @c (μF, in), and
 * @c IsInitialFAlgebra is the engineer's honesty obligation that
 * a particular @c (A, α) is that initial object.  See
 * @c f_algebra_test.cpp for the static_assert witnesses linking the
 * two layers.
 *
 * @section f_algebra__Honesty_Obligation
 *
 * As with @c :nno, C++ concepts cannot quantify over the universal
 * property's @b uniqueness clause.  The structural shape ( @c
 * IsFAlgebra / @c IsFCoalgebra in @c :functor) is what the type
 * system can pin; the universal-property concepts here are @b opt-in
 * @b traits that carriers must explicitly register via
 * @c is_initial_f_algebra_v / @c is_terminal_f_coalgebra_v.  The
 * trait is the engineer's honesty obligation: declaring it asserts
 * that the carrier truly satisfies the universal property, and the
 * paper's @b ethical @b register applies (Wadler 2015, NSPE Code of
 * Ethics).  An identical opt-in pattern is used by @c
 * algebra:initial_ring (PR #451) for @c ℤ as the initial ring.
 *
 * @section f_algebra__Categorical_Duality
 *
 * The dual pair carries the recursion ↔ corecursion split
 * structurally — no catamorphism without an anamorphism.  Both halves
 * land in this partition (concepts only); the operational discharge
 * ( @c cata / @c ana / @c hylo combinators) is deferred to a sibling
 * partition under the same issue ladder.
 *
 * @code
 * | side               | UP                  | unique morphism |
 * |--------------------|---------------------|-----------------|
 * | initial (μF, in)   | initial in F-Alg    | cata (fold)     |
 * | terminal (νF, out) | terminal in F-Coalg | ana  (unfold)   |
 * @endcode
 *
 * @note "An algebra over an endofunctor F is just a structure map
 *        @c F(A) @c → @c A; what makes some particular algebra
 *        @b interesting is that other algebras factor through it
 *        uniquely."
 *       — paraphrase of Pierce 1991, §5.
 *
 * @see Pierce, B.C. (1991) "Basic Category Theory for Computer
 *       Scientists" §5.
 * @see Mac Lane, S. (1971) "Categories for the Working Mathematician"
 *       III–VI.
 * @see Meijer, E., Fokkinga, M., Paterson, R. (1991) "Functional
 *       Programming with Bananas, Lenses, Envelopes and Barbed Wire"
 *       (FPCA).
 */
module;

#include <concepts>
#include <type_traits>

export module dedekind.category:f_algebra;

export import :functor;
import :limit;

namespace dedekind::category {

// The structural-shape concepts @c IsFAlgebra / @c IsFCoalgebra and
// the @c f_algebra / @c f_coalgebra witness types are authored in
// @c :functor (they were there first).  This partition imports them
// transitively via @c export @c import @c :functor above so the
// universal-property layer + structural-shape layer share a single
// namespace surface for downstream callers.

/**
 * @brief Opt-in trait: does carrier @c A witness the @b initial
 *        F-algebra universal property for endofunctor @c F with
 *        structure map @c α?
 *
 * @details Defaults to @c false.  Carriers register by partial-
 * specialising this template to @c true at the point where they
 * have the operational evidence (typically near the structural
 * @c IsFAlgebra witness).  The trait is the engineer's honesty
 * obligation: declaring it asserts that the carrier truly satisfies
 * the universal property's existence + uniqueness clause.
 */
export template <typename F, typename A, typename α>
inline constexpr bool is_initial_f_algebra_v = false;

/**
 * @brief Opt-in trait: does carrier @c A witness the @b terminal
 *        F-coalgebra universal property for endofunctor @c F with
 *        structure map @c α?
 */
export template <typename F, typename A, typename α>
inline constexpr bool is_terminal_f_coalgebra_v = false;

/**
 * @concept IsInitialFAlgebra
 * @brief Carrier @c A is the initial F-algebra for endofunctor @c F
 *        with structure map @c α.
 *
 * @details Combines the structural-shape clause @c IsFAlgebra (from
 * @c :functor) with the universal-property opt-in trait above.  The
 * structural part is mechanically checked; the universal-property
 * part is the engineer's honesty obligation.
 *
 * The catamorphism @c cata(β) @c : @c A @c → @c B for any F-algebra
 * @c (B, β) is the unique morphism guaranteed by initiality; its
 * operational discharge is filed as a sibling concern.
 */
export template <typename F, typename A, typename α>
concept IsInitialFAlgebra =
    IsFAlgebra<A, α, F> && is_initial_f_algebra_v<F, A, α>;

/**
 * @concept IsTerminalFCoalgebra
 * @brief Carrier @c A is the terminal F-coalgebra for endofunctor
 *        @c F with structure map @c α.
 *
 * @details Dual to @c IsInitialFAlgebra.  The anamorphism
 * @c ana(β) @c : @c B @c → @c A for any F-coalgebra @c (B, β) is the
 * unique morphism guaranteed by terminality.
 */
export template <typename F, typename A, typename α>
concept IsTerminalFCoalgebra =
    IsFCoalgebra<A, α, F> && is_terminal_f_coalgebra_v<F, A, α>;

// Cross-partition invariants connecting @c :f_algebra to @c :limit.
// These compose primitives from @c :limit ( @c One, @c Zero,
// @c TerminalCategory, @c InitialCategory, @c unit<>, @c zero<>)
// with the structural-shape concept @c IsFAlgebra from @c :functor.
// The combinations are non-trivial — they fail to type-check if
// either partition's invariants drift independently — and the
// @c static_asserts here make the compiler the witness rather than
// deferring the check to the test suite.

// (1) The unique terminal-typed morphism @c unit<One>() :
//     @c One @c → @c One is structurally the structure map of an
//     F-algebra over @c identity_functor<TerminalCategory> with
//     carrier @c One.  Composes @c :limit's terminal-object data
//     ( @c One, @c TerminalCategory, @c unit<>) with @c :functor's
//     @c identity_functor and @c IsFAlgebra.  Categorical
//     content: the identity endomorphism on the terminal object is
//     the canonical F-algebra structure map at carrier @c One.
static_assert(
    IsFAlgebra<One, decltype(unit<One>()), identity_functor<TerminalCategory>>,
    "Bridge :f_algebra ↔ :limit: unit<One>() : One → One is structurally "
    "the F-algebra structure map for identity_functor on TerminalCategory "
    "with carrier One.  Fails fast if :limit's One / TerminalCategory or "
    ":functor's IsFAlgebra / identity_functor invariants drift apart.");

// (2) Dually, @c zero<Zero>() : @c Zero @c → @c Zero is structurally
//     the structure map of an F-coalgebra over @c
//     identity_functor<InitialCategory> with carrier @c Zero.  Same
//     cross-partition invariant on the initial side.
static_assert(IsFCoalgebra<Zero, decltype(zero<Zero>()),
                           identity_functor<InitialCategory>>,
              "Bridge :f_algebra ↔ :limit: zero<Zero>() : Zero → Zero is "
              "structurally the F-coalgebra structure map for "
              "identity_functor on InitialCategory with carrier Zero.");

}  // namespace dedekind::category
