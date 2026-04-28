/**
 * @file dedekind/category/f_algebra.cppm
 * @partition :f_algebra
 * @module dedekind.category:f_algebra
 * @brief Level 2 (Bridge): Universal-property reification of @b F-algebras
 *        and @b F-coalgebras — the @b initial F-algebra (the carrier of
 *        catamorphisms) and the @b terminal F-coalgebra (the carrier of
 *        anamorphisms).  Closes the universal-property layer for #449.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section FAlg_Universal_Property
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
 * @section Bridge_to_NNO
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
 * @section Honesty_Obligation
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
 * @section Categorical_Duality
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

}  // namespace dedekind::category
