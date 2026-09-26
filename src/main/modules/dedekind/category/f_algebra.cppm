/**
 * @file dedekind/category/f_algebra.cppm
 * @partition :f_algebra
 * @brief Initial F-algebras and terminal F-coalgebras.
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
 * @see @c dedekind.algebra:universal (@c IsAlgebra<T, Ops...>) for the
 *       universal-algebra @c (A, F) reading of the same notion.  Per
 *       the textbook isomorphism @c Alg(Σ) @c ≅ @c F_Σ-Alg (Pierce
 *       §2.2), every @c IsAlgebra<T, Ops...> there corresponds to an
 *       F-algebra here for the polynomial endofunctor @c F_Σ generated
 *       by the signature @c Σ @c = @c (Ops...).  The two partitions
 *       name the same structural fragment from the universal-algebra
 *       and category-theoretic traditions respectively.
 */
module;

#include <concepts>
#include <type_traits>
#include <utility>

export module dedekind.category:f_algebra;

export import :functor;
import :limit;
import :morphism;  // IsArrow, Compose, π_1 / π_2: the first concrete term
                   // functor

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

/**
 * @concept IsBinaryTerm
 * @brief The @c (X @c × @c X) summand of the term functor @c F(X) @c =
 *        @c Atom @c + @c X @c × @c X: a binary term node is a @b
 *        categorical product read through its projections.
 *
 * @details Rather than restate the @c π_1 / @c π_2 projection clause,
 * this @b delegates to @c :limit's @c IsProduct (whose defining clause
 * IS those two projections), deducing the leg types from the nodes's own
 * @c π_1 / @c π_2.  @c Compose is the first such node (#892:
 * @c IsProduct<Compose<F,G>, @c F, @c G>).  The leading @c requires
 * guards the @c decltype so a node without projections is simply @b not
 * a binary term (SFINAE), not a hard error.  @c cata reads the children
 * @b structurally through the projections --- no injected profile or
 * tag.
 */
export template <typename P>
concept IsBinaryTerm =
    requires(const P& p) {
      π_1(p);
      π_2(p);
    } &&
    IsProduct<P, std::remove_cvref_t<decltype(π_1(std::declval<const P&>()))>,
              std::remove_cvref_t<decltype(π_2(std::declval<const P&>()))>>;

// Evidence for the term functor @c F(X) @c = @c Atom @c + @c X @c × @c X:
// its two @c cata injections (leaf / binary) are exhaustive AND exclusive
// over ARROWS --- the term universe is @c IsArrow, and the split is on
// product-ness.  We cannot PROVE the coproduct sealed (C++ type universes
// are open), so these witnesses STATE the intention (#961 review):
//   - an atom arrow is a leaf (arrow, not a product node);
static_assert(IsArrow<Identity<int>> && !IsBinaryTerm<Identity<int>>,
              "cata leaf: an atom arrow carries no π_1 / π_2 sub-terms");
//   - a Compose is a binary term (arrow AND product node);
static_assert(IsArrow<Compose<Identity<int>, Identity<int>>> &&
                  IsBinaryTerm<Compose<Identity<int>, Identity<int>>>,
              "cata binary: a Compose is an arrow that is also a product node");
//   - a bare product (std::pair) IS IsBinaryTerm but is NOT an arrow, so
//     the IsArrow gate on BOTH cata overloads keeps it out of the term
//     universe: product-ness alone does not make a term.
static_assert(IsBinaryTerm<std::pair<int, bool>> &&
                  !IsArrow<std::pair<int, bool>>,
              "a bare product is not an arrow, hence not a cata term");

/** @name reduce_β for a Compose node --- the canonical β exhibit
 *
 *  @brief @c reduce_β is the @b β (F-algebra structure map, i.e. the
 *  one-step β-reduction) that @c cata applies after folding a node's legs.
 *  This overload set is the @b canonical exhibit: the reducer's first law,
 *  the @b monoid law of composition, co-located with the @c cata engine.
 *  The identity is the @b unit (@c id∘f @c = @c f @c = @c f∘id), so an
 *  @c Identity in a @b reduced leg is dropped.
 *
 *  Dispatch is @b structural on @c Identity<T> in the reduced leg (no
 *  is-identity tag): the overloads pattern-match the leg types, and partial
 *  ordering picks the @c Identity ones over the generic @c IsArrow one.  The
 *  first parameter is the @c Compose node as a @b type-tag for β dispatch
 *  (which constructor is this?); its own legs are unused --- @c cata passes
 *  the @b already-reduced legs @c rf / @c rg, which is what β combines.
 *  Found by ADL, so a partition MAY instead scatter its node's @c reduce_β
 *  next to that node's vocabulary; the composition law lives here as the
 *  reference instance.
 *  @{ */
export template <IsArrow F, IsArrow G, IsArrow RF, IsArrow RG>
constexpr auto reduce_β(const Compose<F, G>&, const RF& rf, const RG& rg) {
  return Compose<RF, RG>{rf, rg};  // neither leg is the unit → inert composite
}
export template <IsArrow F, IsArrow G, typename T, IsArrow RG>
constexpr RG reduce_β(const Compose<F, G>&, const Identity<T>&, const RG& rg) {
  return rg;  // id ∘ g = g
}
export template <IsArrow F, IsArrow G, IsArrow RF, typename T>
constexpr RF reduce_β(const Compose<F, G>&, const RF& rf, const Identity<T>&) {
  return rf;  // f ∘ id = f
}
export template <IsArrow F, IsArrow G, typename T, typename U>
constexpr Identity<T> reduce_β(const Compose<F, G>&, const Identity<T>& i,
                               const Identity<U>&) {
  return i;  // id ∘ id = id (disambiguates the two single-sided overloads)
}
/** @} */

/**
 * @brief @c cata ⦇β⦈ --- the @b catamorphism (fold): the unique
 *        F-algebra homomorphism from the initial algebra (the term
 *        itself, @c μF) into an algebra @c (B, β).  @c simplify @b is
 *        @c cata.
 *
 * @details Pierce (§2.2, and Meijer et al.'s "bananas"): for the term
 * functor @c F, the term type @b is the initial algebra @c μF, with
 * @c in @c : @c F(μF) @c → @c μF the constructor (@c Compose here) and,
 * by Lambek, an iso, so @c μF @c ≅ @c F(μF).  For any algebra
 * @c (B, β) with @c β @c : @c F(B) @c → @c B there is a @b unique
 * homomorphism @c cata(β) @c : @c μF @c → @c B satisfying
 * @c cata(β) @c ∘ @c in @c = @c β @c ∘ @c F(cata(β)).  Reading that
 * fixpoint left to right @b is the implementation: @c out (@c π_1 /
 * @c π_2) exposes the children, @c F(cata) folds them (recurse), then
 * @c β combines the results --- a @b post-order fold applying @c β
 * once per node.
 *
 * The @b engine is law-free (a Scheme-@c apply-style dispatcher): it
 * only knows to recurse and apply @c β.  @c β itself is @c reduce_β,
 * found by ADL.  The reference law --- the composition unit law for
 * @c Compose --- is the canonical exhibit @b above, co-located with the
 * engine; further node laws (an inverse-cancellation law, a lattice law)
 * add their own @c reduce_β overloads, here or scattered next to their
 * node's vocabulary.  No node carries an is-this-kind tag; dispatch is on
 * the node type.
 *
 * @note A grow-then-shrink @c β re-invokes @c cata on its result to
 * reach a fixpoint; the unit law here is shrink-only, so one pass
 * suffices.
 *
 * @tparam A a leaf arrow (an @c IsArrow that is not a binary term).
 * @param atom the leaf term.
 * @return @c atom unchanged --- @c cata is the identity on @c F's leaf
 *         summand (an atom is already its own normal form).
 */
export template <IsArrow A>
  requires(!IsBinaryTerm<A>)
constexpr A cata(const A& atom) {
  return atom;
}

/**
 * @brief @c cata on a binary term node: @c F(cata) folds the two legs,
 *        then @c β @c = @c reduce_β combines the reduced legs under this
 *        node's law.
 *
 * @details The @c IsArrow constraint (alongside @c IsBinaryTerm) closes
 * the term universe to arrows: a bare product that is not an arrow is not
 * a term.  @c reduce_β is a dependent call resolved by ADL at
 * instantiation against the node type, so the law lives next to the node
 * (the @c Compose unit law is in @c :morphism).
 *
 * @tparam P a binary term node (an @c IsArrow that is also an
 *         @c IsBinaryTerm, e.g. @c Compose).
 * @param t the node.
 * @return the node's β-image over its reduced legs.
 */
export template <typename P>
  requires IsArrow<P> && IsBinaryTerm<P>
constexpr auto cata(const P& t) {
  return reduce_β(t, cata(π_1(t)), cata(π_2(t)));
}

}  // namespace dedekind::category
