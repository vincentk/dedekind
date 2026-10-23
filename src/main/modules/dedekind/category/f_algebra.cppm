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
 * land in this partition as concepts; the operational discharge of the
 * fold is @c cata below ( @c ana / @c hylo are still to come).
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
#include <cstddef>
#include <functional>  // std::less_equal: LawAlgebra's default order
#include <tuple>
#include <type_traits>
#include <utility>

export module dedekind.category:f_algebra;

export import :functor;
import :limit;
import :posetal;    // ArrowLaws, arrow_node, arrow_laws_v: the law engine
                    // LawAlgebra folds
import :cartesian;  // the formers ⟨,⟩ [,] and the defining forms of × +
import :morphism;   // IsArrow, Compose, π_1 / π_2: the first concrete term
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

// (1) The unique terminal-typed morphism @c bang<One>() :
//     @c One @c → @c One is structurally the structure map of an
//     F-algebra over @c identity_functor<TerminalCategory> with
//     carrier @c One.  Composes @c :limit's terminal-object data
//     ( @c One, @c TerminalCategory, @c unit<>) with @c :functor's
//     @c identity_functor and @c IsFAlgebra.  Categorical
//     content: the identity endomorphism on the terminal object is
//     the canonical F-algebra structure map at carrier @c One.
static_assert(
    IsFAlgebra<One, decltype(bang<One>()), identity_functor<TerminalCategory>>,
    "Bridge :f_algebra ↔ :limit: bang<One>() : One → One is structurally "
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
/** @brief A pairing (copairing) rebuilt from its reduced legs: inert. */
export template <typename F, typename G, typename RF, typename RG>
constexpr auto reduce_β(const Pairing<F, G>&, const RF& rf, const RG& rg) {
  return Pairing<RF, RG>{rf, rg};
}
export template <typename F, typename G, typename RF, typename RG>
constexpr auto reduce_β(const Copairing<F, G>&, const RF& rf, const RG& rg) {
  return Copairing<RF, RG>{rf, rg};
}
/** @} */

/** @section f_algebra__The_Term_Functor
 *
 *  @c legs is the term functor's @c out @c : @c μF @c → @c F(μF), stated
 *  @b once per node: the tuple of the sub-terms a node is built from, found
 *  by ADL beside the node's definition.  A type without @c legs is a leaf.
 *  @c Compose is the first node, its legs the two arrows it composes (its
 *  product projections, #892); a bare product that is not an arrow is not a
 *  term and has no legs.  Every reading of a term (a rewrite to normal form,
 *  a certificate, its value) is then an @b algebra over this one functor, and
 *  @c cata is the one engine that folds it.
 */

/** @brief The legs of a composite: the arrows it composes.  @param c the
 *  node. */
export template <IsArrow F, IsArrow G>
constexpr auto legs(const Compose<F, G>& c) {
  return std::tuple{π_1(c), π_2(c)};
}

/** @brief A node of the term functor: a type that exposes its legs. */
export template <typename T>
concept IsTermNode = requires(const T& t) { legs(t); };

/** @brief An algebra that steers the fold at @c T (Mendler style): it recurses
 *  into the legs it chooses by @c cata itself, instead of the strict fold. */
export template <typename β, typename T>
concept Steers = requires(const β& a, const T& t) { a.steer(t); };

export template <typename β, typename T>
  requires Steers<β, T> || IsTermNode<T> || std::invocable<const β&, const T&>
constexpr auto cata(const β& alg, const T& t);

namespace detail_f_algebra {
/** @brief The component orders of a product order, where @c Op is one. */
template <typename Op>
struct component_orders {};
template <typename LA, typename LB>
struct component_orders<ProductLeq<LA, LB>> {
  using first = LA;
  using second = LB;
};
template <typename β, typename T, std::size_t... I>
  requires IsTermNode<T> &&
           std::invocable<
               const β&, const T&,
               decltype(cata(std::declval<const β&>(),
                             std::get<I>(legs(std::declval<const T&>()))))...>
constexpr auto fold_legs(const β& alg, const T& t, std::index_sequence<I...>) {
  const auto ls = legs(t);
  return alg(t, cata(alg, std::get<I>(ls))...);
}
}  // namespace detail_f_algebra

/**
 * @brief @c cata ⦇β⦈ --- the @b catamorphism (fold): the unique
 *        F-algebra homomorphism from the initial algebra (the term
 *        itself, @c μF) into an algebra @c (B, β).
 *
 * @details Pierce (§2.2, and Meijer et al.'s "bananas"): for the term
 * functor @c F, the term type @b is the initial algebra @c μF, with
 * @c in @c : @c F(μF) @c → @c μF the constructor and, by Lambek, an iso,
 * so @c μF @c ≅ @c F(μF).  For any algebra @c (B, β) with
 * @c β @c : @c F(B) @c → @c B there is a @b unique homomorphism
 * @c cata(β) @c : @c μF @c → @c B satisfying
 * @c cata(β) @c ∘ @c in @c = @c β @c ∘ @c F(cata(β)).  Reading that
 * fixpoint left to right @b is the implementation: @c legs exposes the
 * sub-terms, @c F(cata) folds them (recurse), then @c β combines the
 * results --- a @b post-order fold applying @c β once per node.
 *
 * The @b engine is law-free (a Scheme-@c apply-style dispatcher): it
 * only knows to recurse and apply @c β.  The algebra is a callable:
 * @c β(leaf) on a leaf, @c β(node, @c folded_legs...) on a node, so an
 * algebra states only the nodes it reads specially and one rule for the
 * rest.  An algebra whose results are @c std::integral_constant folds
 * entirely inside @c decltype: a type-level reading (a certificate) and a
 * value-level one (an evaluation) are the same fold.
 *
 * @b Steering (Mendler style): a strict fold evaluates every leg.  Where
 * an algebra must not (McCarthy's conditional evaluates only the branch
 * it selects), it defines @c β.steer(node) and recurses with
 * @c cata(β, @c leg) into the legs it chooses.  The engine tries @c steer
 * first, then the strict fold, then the leaf.
 *
 * @note A grow-then-shrink @c β re-invokes @c cata on its result to
 * reach a fixpoint; the laws here are shrink-only, so one pass suffices.
 *
 * @tparam β the algebra.  @tparam T the term.
 * @param alg the algebra.  @param t the term.
 * @return the term's image under the algebra.
 */
export template <typename β, typename T>
  requires Steers<β, T> || IsTermNode<T> || std::invocable<const β&, const T&>
constexpr auto cata(const β& alg, const T& t) {
  if constexpr (Steers<β, T>) {
    return alg.steer(t);
  } else if constexpr (IsTermNode<T>) {
    return detail_f_algebra::fold_legs(
        alg, t,
        std::make_index_sequence<
            std::tuple_size_v<std::remove_cvref_t<decltype(legs(t))>>>{});
  } else {
    return alg(t);
  }
}

/** @brief The β-reduction algebra: a leaf is its own normal form; a node is
 *  @c reduce_β over its reduced legs.  @c reduce_β is a dependent call
 *  resolved by ADL against the node type, so a law lives next to its node:
 *  the composition unit law above is the canonical exhibit, and further node
 *  laws add their own overloads beside their vocabulary.  No node carries an
 *  is-this-kind tag; dispatch is on the node type. */
export struct BetaReduction {
  template <typename A>
  constexpr A operator()(const A& atom) const {
    return atom;
  }
  template <typename N, typename... R>
    requires requires(const N& n, const R&... r) { reduce_β(n, r...); }
  constexpr auto operator()(const N& node, const R&... reduced) const {
    return reduce_β(node, reduced...);
  }
};

/** @brief The law algebra: a leaf is its declared profile, a node the law
 *  engine on its legs' profiles (@c :posetal, @c arrow_node); any other
 *  node certifies nothing (sound).  The second
 *  algebra for the same @c cata: @c BetaReduction lands in a normal form (a
 *  free algebra, so it decides equality), @c LawAlgebra in a finite domain of
 *  profiles (an abstract interpretation, so it certifies, sound, not
 *  complete).  @tparam Op the order the variances are read under. */
export template <typename Op = std::less_equal<>>
struct LawAlgebra {
  template <typename A>
  constexpr ArrowLaws operator()(const A&) const {
    return arrow_laws_v<A, Op>;
  }
  template <typename N, typename R0, typename... R>
  constexpr ArrowLaws operator()(const N&, const R0&, const R&...) const {
    return ArrowLaws{};
  }
  template <typename F, typename G>
  constexpr ArrowLaws operator()(const Compose<F, G>&, const ArrowLaws& f,
                                 const ArrowLaws& g) const {
    return arrow_node(ArrowNode::Compose, f, g);
  }
  /** @brief The defining forms of × and +, recognised before the fold
   *  descends (@c cata's @c steer): the product (sum) fact on the factors,
   *  which the pairing (copairing) rule alone would lose. */
  template <typename A, typename F, typename B, typename G>
    requires(IsPairProjection<A, 1> && IsPairProjection<B, 2>)
  constexpr ArrowLaws steer(
      const Pairing<Compose<A, F>, Compose<B, G>>& t) const {
    // A product's variance is about the componentwise order: each factor
    // under its own component order; under any other order, no variance.
    if constexpr (requires {
                    typename detail_f_algebra::component_orders<Op>::first;
                  }) {
      using O = detail_f_algebra::component_orders<Op>;
      return arrow_node(ArrowNode::Product,
                        cata(LawAlgebra<typename O::first>{}, t.f.g),
                        cata(LawAlgebra<typename O::second>{}, t.g.g));
    } else {
      return without_variance(arrow_node(ArrowNode::Product, cata(*this, t.f.g),
                                         cata(*this, t.g.g)));
    }
  }
  template <typename F, typename G, typename X, typename Y>
  constexpr ArrowLaws steer(
      const Copairing<Compose<F, Injection<1, X, Y>>,
                      Compose<G, Injection<2, X, Y>>>& t) const {
    // A sum's variance is about std::variant's own order (by summand).
    const ArrowLaws laws =
        arrow_node(ArrowNode::Sum, cata(*this, t.f.f), cata(*this, t.g.f));
    if constexpr (std::same_as<Op, std::less_equal<>>)
      return laws;
    else
      return without_variance(laws);
  }
  template <typename F, typename G>
  constexpr ArrowLaws operator()(const Pairing<F, G>&, const ArrowLaws& f,
                                 const ArrowLaws& g) const {
    return arrow_node(ArrowNode::Pairing, f, g);
  }
  template <typename F, typename G>
  constexpr ArrowLaws operator()(const Copairing<F, G>&, const ArrowLaws& f,
                                 const ArrowLaws& g) const {
    return arrow_node(ArrowNode::Copairing, f, g);
  }
};

/** @brief @c cata(t): the term rewritten to its normal form under the
 *  β-reduction algebra.  @c simplify @b is @c cata.  The @c IsArrow gate keeps
 *  a bare product out of the term universe.  @tparam T an arrow term.
 *  @param t the term. */
export template <IsArrow T>
constexpr auto cata(const T& t) {
  return cata(BetaReduction{}, t);
}

// The term functor @c F(X) @c = @c Atom @c + @c X @c × @c X over arrows: its
// two injections (leaf / node) are exhaustive and exclusive.  We cannot PROVE
// the coproduct sealed (C++ type universes are open), so these witnesses STATE
// the intention (#961 review).
static_assert(IsArrow<Identity<int>> && !IsTermNode<Identity<int>>,
              "cata leaf: an atom arrow has no legs");
static_assert(IsTermNode<Compose<Identity<int>, Identity<int>>> &&
                  !IsTermNode<std::pair<int, bool>>,
              "cata node: a Compose exposes its two legs; a bare product is "
              "not an arrow, hence not a term");

}  // namespace dedekind::category
