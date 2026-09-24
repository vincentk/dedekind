/**
 * @file dedekind/category/cartesian_bicategory.cppm
 * @partition :cartesian_bicategory
 * @brief The meet is the right adjoint of the diagonal (@c Δ⊣∧), reified.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section cartesian_bicategory__Overview Meet as the adjoint of the diagonal
 *
 * Sketch (epic #946, slice S0).  Reifies the theory @c Δ @c ⊣ @c ∧ as a
 * @c concept first (@c IsMeetAsRightAdjoint), so the composite meet
 * @c = @c Δ† @c ∘ @c (⊗) @c ∘ @c Δ must type-check against it.  In a cartesian
 * bicategory (Carboni & Walters) every object carries a commutative comonoid:
 * copy @c Δ:A→A×A, delete @c ε:A→1, and the dagger merge @c Δ†:A×A→A; the meet
 * @b is that merge, and @c "meet @c = @c right @c adjoint @c of @c Δ" is its
 * 1-categorical shadow (Mac Lane, adjunctions in a poset degenerate to Galois
 * connections).
 *
 * @section cartesian_bicategory__Load_bearing What is compiled here
 *
 * A compiled, type-checked partition.  @c :lattice_term imports it and
 * @c static_asserts @c IsMeetAsRightAdjoint over its canonical carrier, so the
 * reification is load-bearing early rather than an orphan.  The arrow-level
 * @c (⊗) (@c Tensor) and the composite meet @c Δ† @c ∘ @c (⊗) @c ∘ @c Δ
 * (@c Intersect) are provided; the value-level product-order leg (a proof that
 * the composite computes the glb over a non-trivial pair, and so distinguishes
 * the meet from the join) remains @c FIXME(#946).
 *
 * Wikipedia: Cartesian bicategory, Frobenius algebra, Adjoint functors
 *
 * @note "Only connectivity matters."
 *       Bob Coecke & Aleks Kissinger, *Picturing Quantum Processes*
 *       (Cambridge, 2017), on spider fusion: the copy / merge Frobenius
 *       maps compose by their wiring alone, which is exactly why the meet
 *       falls out of the comonoid structure rather than the carrier.
 */
module;

#include <concepts>    // std::same_as
#include <functional>  // std::less_equal (default order), std::logical_and
#include <utility>     // std::pair: the binary product (cf. :limit)

export module dedekind.category:cartesian_bicategory;

import :morphism;    // IsArrow, Dom, Cod, Identity
import :adjunction;  // IsGaloisConnection (F left-adjoint-to G in a poset)
import :posetal;     // IsPosetal (thin cat); is_monotone_v / IsMonotone
import :limit;       // One (terminal object), π_1 / π_2, IsProduct
import :species;     // Inf / Sup: the injected value-level glb / lub (∧ / ∨)
import :logic;       // Ternary (Kleene K₃, the 3-chain carrier witness)
import :lattice;  // Meet AST node + MakeMeet pairing factory (the bridge below)

namespace dedekind::category {

/**
 * @section cartesian_bicategory__Design Meet as the adjoint of the diagonal
 *
 * @b Thesis. Lattice reduction (@c {x>5}∩{x>3} @c → @c {x>5}) is
 * @b order-theoretic, not set-theoretic. The meet @b is the greatest lower
 * bound, fixed by the order alone; the powerset @c Sub(A) is merely @b one
 * model that realises it. So the reduction needs only a poset (a thin
 * category) plus a category-level handle on the glb, and set theory stays
 * downstream as an injected model. The apparent circular dependency
 * (@c "the category layer would need @c Sub(A) to reduce") is a category
 * error between the two lattices.
 *
 * @b The @b comonoid. In a cartesian bicategory every object carries a
 * commutative comonoid: @b copy @c Δ:A→A⊗A (@c a↦(a,a)) and @b delete
 * @c ε:A→1, plus the dagger @b merge @c Δ†:A⊗A→A. Then
 *
 *   R ∩ S @c = @c Δ† @c ∘ @c (R⊗S) @c ∘ @c Δ   (copy the input, run both,
 *                                                merge where they agree).
 *
 * @b Two @b shadows @b of @b one @b structure.
 *  - @b Order-adjoint: the copy @c Δ is the diagonal @c Δ:P→P×P; its right
 *    adjoint (the merge) is the meet. @c "meet @c = @c right @c adjoint @c of
 *    @c Δ" is the 1-categorical shadow. In a poset an adjunction degenerates
 *    to a Galois connection (see @c IsGaloisConnection), so
 *    @c Δ(c)≤(a,b) @c ⟺ @c c≤a∧b, which (since @c Δc=(c,c) under the product
 *    order) unfolds to the glb universal property
 *        @c c≤a @c and @c c≤b @c ⟺ @c c≤a∧b.
 *  - @b Set: @c Rel restricted to @c Sub(A) is one cartesian bicategory; its
 *    meet is this general @c Δ†∘(⊗)∘Δ instantiated.
 *
 * @b Why @b no @b cycle. The copy @c Δ here is a @b function @c A→A×A, NOT
 * the relational identity @c Δ={(a,a)} in @c :relational (which is @c a↦a,
 * the @c ; unit, and which imports @c dedekind.category and so cannot be
 * hoisted). Defining copy/merge in @c :category, upstream of @c :sets and
 * @c :relational, keeps the DAG acyclic: the reducer @b injects the order
 * algebra (the existing @c reduce<Term,Ord,Combine> already does this), so no
 * downstream set machinery is pulled upstream.
 *
 * @b Honest @b asymmetry. The meet @c ∩ falls out of the comonoid. The join
 * @c ∨ needs more than bare Frobenius (a bicategory of relations); it is NOT
 * in this sketch. FIXME(#946): pin the @c ∨ story.
 */

/** @brief Copy (comonoid comultiplication) @c Δ:A→A×A, @c a↦(a,a). The left
 *         adjoint of the meet; the fan-out of a cartesian bicategory.
 *  @tparam A the carrier object. */
export template <typename A>
struct Copy {
  using Domain = A;
  using Codomain = std::pair<A, A>;
  /** @brief Fan the input out onto both legs @c Δ(a)=(a,a).
   *  @param a the value to copy.
   *  @return the diagonal pair @c (a,a).
   *  @note The @c requires clause gates @c A on @c std::copy_constructible: the
   *        diagonal duplicates its input, so a move-only @c A removes
   *        @c operator() from the overload set and @c IsArrow (hence @c IsCopy)
   *        rejects @c Copy<A> at the concept surface rather than only when the
   *        @c {a,a} body is later instantiated (#950 review).  Same guard as
   *        @c Merge::operator() on its injected op. */
  constexpr Codomain operator()(const A& a) const
    requires std::copy_constructible<A>
  {
    return {a, a};
  }
};

/** @brief Delete (counit) @c ε:A→1. Completes the commutative comonoid.
 *  @tparam A the carrier object.
 *  @note The category terminal object @c One (@c :limit) IS @c std::monostate,
 *        so this is an @c IsTerminalMorphism; grounding resolved the earlier
 *        placeholder. */
export template <typename A>
struct Delete {
  using Domain = A;
  using Codomain = One;
  /** @brief Discard the input into the terminal object @c ε(a)=•.
   *  @param a the value to delete (unused; every value maps to @c One).
   *  @return the unique inhabitant of @c One. */
  constexpr Codomain operator()(const A&) const { return {}; }
};

/** @brief Merge (the dagger of copy) @c Δ†:A×A→A, @c (a,b)↦a⊓b. The right
 *         adjoint of the diagonal; the value that @c IsMeetAsRightAdjoint
 *         certifies as the glb.
 *  @tparam A the carrier object.
 *  @tparam Meet the glb, @b injected: the order-algebra's meet, exactly as
 *          @c reduce<Term,Ord,Combine> injects it. No default and no
 *          @c :species dependency, so this partition does not reach into the
 *          @c :species value-op catalogue; the merge is whatever glb the
 *          caller supplies. */
export template <typename A, typename Meet>
struct Merge {
  using Domain = std::pair<A, A>;
  using Codomain = A;
  /** @brief Fold the copied pair through the injected glb @c Δ†(a,b)=a⊓b.
   *  @param p the pair @c (a,b) to merge.
   *  @return @c Meet{}(a,b), the injected order-algebra meet.
   *  @note The @c requires clause probes @c Meet on the call so an
   *        ill-formed injected op (a @c Meet with no @c operator()(A,A))
   *        removes @c operator() from the overload set.  @c IsArrow then fails
   *        for that @c Merge, so @c IsMerge rejects it at the concept surface
   *        rather than only at a later call site (#946 review). */
  constexpr A operator()(const Domain& p) const
    requires requires(const A& a) {
      { Meet{}(a, a) } -> std::convertible_to<A>;
    }
  {
    return Meet{}(p.first, p.second);
  }
};

/** @brief @concept IsCopy: the DIAGONAL notion @c Δ:A→A×A --- an arrow whose
 *  Codomain is the square of its Domain (the comonoid comultiplication /
 *  fan-out). @c Copy<A> is the canonical MODEL; this concept is the matchable
 *  "diagonal" handle. Purely structural: it names the crossed A→A×A signature,
 *  not that the map is @c a↦(a,a) (that is @c Copy's own obligation). Cartesian
 *  only: the non-cartesian ⊗ setting needs the carrier generalized past
 *  @c std::pair (#951); a type carrying more than one arrow reading needs the
 *  dom/cod contract (#952). */
export template <typename F>
concept IsCopy = IsArrow<F> && std::same_as<Cod<F>, std::pair<Dom<F>, Dom<F>>>;

/** @brief @concept IsMerge: the FOLD notion @c Δ†:A×A→A --- an arrow whose
 *  Domain is the square of its Codomain (the comonoid dagger / binary
 * combiner).
 *  @c Merge<A,Meet> is the canonical MODEL; this concept is the matchable
 * handle. Purely structural: it names the A×A→A signature, so a PROJECTION @c
 * π:A×A→A also satisfies it (the shape cannot tell a fold from a projection,
 * the same honesty gap as glb-vs-lub). Same seams as @c IsCopy: carrier past @c
 * std::pair
 *  (#951); the multi-reading dom/cod contract (#952). */
export template <typename F>
concept IsMerge = IsArrow<F> && std::same_as<Dom<F>, std::pair<Cod<F>, Cod<F>>>;

// Variance registrations for the comonoid legs (:posetal @c is_monotone_v),
// against the COMPONENTWISE product order @c ProductLeq (@c :posetal), NOT the
// lexicographic @c std::less_equal on the pair.  Lex is unsound here: e.g.
// @c (0,100) <= (1,-100) lexicographically, yet @c Inf maps them to @c 100 and
// @c -100, so min would report a false monotonicity under lex; min is monotone
// only under @c ≤× (#950 review).  The shared @c P×P object of the connection
// (Copy's codomain, Merge's domain) carries @c ≤×; @c P carries the carrier's
// @c Leq.
//
// REACHABILITY (#950 review): these @c is_monotone_v partial specializations
// are deliberately NOT @c export ed --- template specializations are @b
// reachable, not @b visible, so an importer's instantiation of @c
// IsMeetAsRightAdjoint sees them without a name lookup.  This is the
// SpeciesTraits precedent: @c :posetal itself registers @c
// is_monotone_v<Identity<T>,Op> non-exported (posetal.cppm), and @c :numbers /
// @c :morphologies / @c :sets / @c :algebra all specialize it unexported and
// rely on it cross-partition.  @c :lattice_term's load-bearing
// @c static_assert(IsMeetAsRightAdjoint<Copy<bool>,Merge<...>>) therefore does
// NOT see only the @c false primary; CI (which instantiates it across the
// import boundary) is the audit trail.  Exporting a variable-template
// specialization is ill-formed anyway --- only the primary is exported.
//
// Neither leg is a hand-asserted per-carrier bool: both ride the @c
// is_monotone_v disjunct of @c IsMonotone as a DERIVED THEOREM.  Copy @c
// a↦(a,a) is UNIVERSALLY monotone into @c ≤× (from @c c≤c' infer @c
// (c,c)≤(c',c') componentwise), so it is registered for ANY per-component
// order.  Merge @c Δ†=∧ is monotone in @c ≤× as a THEOREM about a
// meet-semilattice, gated on @c IsOrderMeetSemilattice<A,Meet>: min / max /
// Boolean AND each preserve @c ≤ in every argument, so a @c ≤×-ordered pair
// maps to a @c ≤-ordered output.  The gate fires for EVERY certified carrier /
// op (Inf, Sup, logical_and, ...) by INFERENCE from the algebra, retiring the
// four per-carrier tags.  This is the step #908 (reify predicate variance)
// would derive fully structurally (and add the antitone dual); cf. #791
// (certify monotone / join operations).
template <typename A, typename LeqA>
inline constexpr bool is_monotone_v<Copy<A>, ProductLeq<LeqA, LeqA>> = true;
template <typename A, typename Meet>
  requires IsOrderMeetSemilattice<A, Meet>
inline constexpr bool is_monotone_v<
    Merge<A, Meet>, ProductLeq<std::less_equal<A>, std::less_equal<A>>> = true;

/**
 * @concept IsMeetAsRightAdjoint
 * @brief The STRUCTURAL shape of "the meet @c ∧ is the right adjoint of the
 *        diagonal @c Δ" (@c Δ⊣∧): the reified theory this slice postulates as a
 *        type-check.
 * @warning This is a structural SHAPE gate, NOT a dispatch-safe glb certifier.
 *          It names the crossed signatures + a posetal carrier + a definite
 *          variance; it CANNOT see whether the merge computes the glb (meet) or
 *          the lub (join), so the join @c Sup passes it too (see the labelled
 *          limitation witness below).  glb-correctness is the injected op's
 *          obligation, tightened later via #908 (reify predicate variance) plus
 *          the value-level product-order leg, FIXME(#946).  Do NOT branch
 *          dispatch on a positive result as if it guaranteed a meet.
 * @details @c Δ:P→P×P (copy) is the left adjoint, @c ∧:P×P→P the right, so
 *          the pair @b is an @c IsGaloisConnection whose left leg is the
 *          diagonal (its codomain is the square of its domain). The Galois
 *          test @c Δ(c)≤(a,b) @c ⟺ @c c≤∧(a,b), with @c Δc=(c,c) under the
 *          product order, unfolds to the glb universal property
 *              @c c≤a @c and @c c≤b @c ⟺ @c c≤a∧b.
 *          C++ concepts cannot quantify over @c c,a,b, so that equivalence is
 *          the engineer's honesty obligation (as with @c IsGaloisConnection);
 *          the structural shape names the signatures.
 * @note Monotonicity is INHERITED, not re-gated here: @c IsGaloisConnection
 *       was tightened (#946) to require MATCHED variance (both legs monotone,
 *       or both antitone), so both @c Cp and @c Mg must carry a declared, and
 *       agreeing, variance (see the @c is_monotone_v registrations above).
 *       That NARROWS the gap (a variance-less or mismatched-polarity merge is
 *       now rejected) but does NOT close the meet-vs-join residual: the join
 *       @c Sup is monotone too, so it still passes.  Which order-op the merge
 *       computes stays the value-level product-order leg, FIXME(#946).  #908
 *       (reify predicate variance) would derive the variance structurally;
 *       #791 certifies the monotone / join operations themselves.
 * @note @b Threaded @b orders (#950).  The Galois legs live on @c P×P (Copy's
 *       codomain, Merge's domain), so their variance is tested against the
 *       COMPONENTWISE product order @c ProductLeq<Leq,Leq> (@c ≤×), while the
 *       base carrier @c P is checked posetal under @c Leq itself.  @c Leq is no
 *       longer inert: it flows into BOTH the product order threaded through
 *       @c IsGaloisConnection and the @c IsPosetal check (#950 review), so a
 *       custom poset relation is honoured rather than silently overridden by a
 *       hard-coded @c std::less_equal<>.
 * @tparam Cp the copy/diagonal @c Δ.
 * @tparam Mg the meet/merge @c ∧.
 * @tparam Leq the order on @c P; defaults to @c std::less_equal<P>.  The
 *         product order on @c P×P is derived as @c ProductLeq<Leq,Leq>. */
export template <typename Cp, typename Mg,
                 typename Leq = std::less_equal<Dom<Cp>>>
concept IsMeetAsRightAdjoint =
    IsGaloisConnection<Cp, Mg, ProductLeq<Leq, Leq>> &&
    IsPosetal<Dom<Cp>, Leq> && IsCopy<Cp>;

/** @brief The parallel product @c ⊗ as the @b arrow-half of the product
 *         bifunctor: @c R⊗S : @c A×B→C×D is @c IsProduct acting on the morphism
 *         pair @c (R,S). The object-half is @c IsProduct itself (the carrier @c
 *         A×B @b is the product object); the arrow-half is this @c Tensor. The
 *         action is the mediating morphism @c ⟨R∘π₁, @c S∘π₂⟩ built from the
 *         product's own reified projections @c Π_1 / @c Π_2 (@c :limit) and its
 *         pairing (the @c std::pair mediator @c mediate_product wraps as an
 *         arrow); nothing here reimplements the universal property.
 *  @tparam R the left leg @c A→C.
 *  @tparam S the right leg @c B→D.
 *  @note @b Generalization @b seam. HERE, in the cartesian setting, @c ⊗
 *        coincides with the categorical product @c × --- the carrier is the
 *        product object and @c IsProduct expresses it. The name @c Tensor
 *        deliberately anticipates the later NON-cartesian monoidal regime (the
 *        LA / bra-ket layer, where @c ⊗ is the genuine tensor product with no
 *        projections and the copy/merge Frobenius algebra becomes an
 *        orthonormal basis / ZX spider), which @c IsProduct cannot express.
 *        @c Tensor-on-@c IsProduct now, carrier generalized later.
 *        FIXME(#946): generalize the carrier past @c IsProduct at that seam. */
export template <IsArrow R, IsArrow S>
  requires IsProduct<std::pair<Dom<R>, Dom<S>>, Dom<R>, Dom<S>> &&
           IsProduct<std::pair<Cod<R>, Cod<S>>, Cod<R>, Cod<S>>
struct Tensor {
  using Domain = std::pair<Dom<R>, Dom<S>>;
  using Codomain = std::pair<Cod<R>, Cod<S>>;
  R r{};
  S s{};
  /** @brief Run both legs in parallel @c (R⊗S)(a,b)=(R(a),S(b)), routed
   *         through the product's own projections @c Π_1 / @c Π_2.
   *  @param p the input pair @c (a,b) in @c A×B.
   *  @return the pair @c (R(a),S(b)) in @c C×D. */
  constexpr Codomain operator()(const Domain& p) const {
    return {r(Π_1<Domain>{}(p)), s(Π_2<Domain>{}(p))};
  }
};

/** @brief @concept IsTensor: the structural shape of the parallel product ⊗ ---
 *  an arrow whose Domain AND Codomain are product objects (the arrow-half of
 * the product bifunctor). @c Tensor<R,S> is the canonical MODEL; this concept
 * is the matchable handle. Cartesian only: the non-cartesian monoidal ⊗ (LA
 * outer product) needs the carrier generalized past IsProduct (#951); a type
 * with >1 arrow reading needs the dom/cod contract (#952). */
export template <typename T>
concept IsTensor = IsArrow<T> && requires {
  typename Dom<T>::first_type;
  typename Dom<T>::second_type;
  typename Cod<T>::first_type;
  typename Cod<T>::second_type;
  requires IsProduct<Dom<T>, typename Dom<T>::first_type,
                     typename Dom<T>::second_type>;
  requires IsProduct<Cod<T>, typename Cod<T>::first_type,
                     typename Cod<T>::second_type>;
};

/** @brief The composite meet @c R∩S @c = @c Δ† @c ∘ @c (R⊗S) @c ∘ @c Δ: copy
 * the input, run both legs in parallel, merge where they agree. The
 *         1-categorical realisation of the relational intersection whose
 *         @c (Copy,Merge) legs are certified by @c IsMeetAsRightAdjoint.
 *  @warning @b Not @b dispatch-safe as a glb (#950 review).  This is the meet
 *           composite @b by @b intent, but its @c requires clause gates only
 * the STRUCTURAL @c Δ⊣∧ shape via @c IsMeetAsRightAdjoint, which this file
 *           deliberately proves true for @c Merge<A,Sup> (the JOIN) too.  So
 *           @c Intersect<R,S,Sup> is publicly constructible and computes a
 *           @b join under an intersection name.  glb-correctness is the
 * injected
 *           @c Meet's obligation, NOT something this composite discharges;
 *           closing it is deferred to #908 (reify predicate variance) plus the
 *           value-level product-order leg, FIXME(#946).  Do NOT branch dispatch
 *           on the mere existence of this type as if it guaranteed a meet.
 *  @tparam R the left endo-leg @c A→A.
 *  @tparam S the right endo-leg @c A→A.
 *  @tparam Meet the injected glb, forwarded to @c Merge (see there).
 *  @note @c R and @c S are endomorphisms of the common carrier @c A so the
 *        composite is again @c A→A; @c Intersect<Identity,Identity> collapses
 * to the identity, the arrow-level shadow of idempotence @c a∧a=a. */
export template <IsArrow R, IsArrow S, typename Meet>
  requires std::same_as<Dom<R>, Dom<S>> && std::same_as<Cod<R>, Dom<R>> &&
           std::same_as<Cod<S>, Dom<R>> &&
           IsMeetAsRightAdjoint<Copy<Dom<R>>, Merge<Dom<R>, Meet>>
struct Intersect {
  using Domain = Dom<R>;
  using Codomain = Dom<R>;
  R r{};
  S s{};
  /** @brief The composite meet @c (R∩S)(a)=Δ†((R⊗S)(Δ(a))): copy, run both
   *         legs, merge where they agree.
   *  @param a the input value.
   *  @return @c R(a)⊓S(a), the injected glb of the two legs' outputs.
   *  @note The @c requires clause gates only the STRUCTURAL @c Δ⊣∧ shape
   *        (crossed signatures + posetal carrier + definite variance).  It does
   *        NOT certify that @c Meet is the glb rather than the lub; that is the
   *        injected op's obligation (see @c IsMeetAsRightAdjoint), tightened
   *        later via #908 + the value-level product-order leg, FIXME(#946). */
  constexpr Codomain operator()(const Domain& a) const {
    return Merge<Domain, Meet>{}(Tensor<R, S>{r, s}(Copy<Domain>{}(a)));
  }
};

// The comonoid legs are the arrows the theory names, each the canonical MODEL
// of its concept: Copy ⊨ IsCopy (Δ:A→A×A), Merge ⊨ IsMerge (Δ†:A×A→A), Delete ⊨
// IsTerminalMorphism (ε:A→One, the counit; concept reused from :limit).
static_assert(IsArrow<Copy<bool>>, "Δ: A → A×A must be an arrow.");
static_assert(IsCopy<Copy<bool>>, "Copy is the canonical model of IsCopy.");
static_assert(IsMerge<Merge<bool, std::logical_and<bool>>>,
              "Merge is the canonical model of IsMerge.");
static_assert(IsTerminalMorphism<Delete<bool>>,
              "ε: A → One must be the terminal morphism (counit).");

// Concept discrimination.  IsCopy (product Codomain) and IsMerge (product
// Domain) are dual crossed shapes, so each rejects the other leg.
static_assert(!IsCopy<Merge<bool, std::logical_and<bool>>>,
              "Δ† (A×A→A) is not the diagonal Δ (A→A×A).");
static_assert(!IsMerge<Copy<bool>>, "Δ (A→A×A) is not the fold Δ† (A×A→A).");
// IsMerge is STRUCTURAL: the left projection π₁:A×A→A has the merge signature,
// so it too satisfies IsMerge --- the shape cannot tell a fold from a
// projection (the same honesty gap that lets the JOIN pass
// IsMeetAsRightAdjoint).  Pinned here so the limitation stays visible; #952
// (dom/cod contract) would separate the readings.
static_assert(
    IsMerge<Π_1<std::pair<bool, bool>>>,
    "structural witness: a projection π₁:A×A→A also satisfies IsMerge "
    "(fold-vs-projection honesty gap).");
// An injected op with no @c operator()(A,A) is rejected at the concept surface:
// the @c requires clause on @c Merge::operator() removes it from the overload
// set, so @c IsArrow (hence @c IsMerge) fails --- not deferred to a call site.
struct MergeBadOp {};  // not callable as a binary meet
static_assert(!IsMerge<Merge<int, MergeBadOp>>,
              "an injected op that is not callable as (a,b)↦a⊓b must NOT model "
              "IsMerge (the ill-formed call is caught structurally).");

// Dually, a MOVE-ONLY carrier is rejected at the concept surface: the diagonal
// duplicates its input, so the @c std::copy_constructible<A> guard on
// @c Copy::operator() removes it from the overload set and @c IsArrow (hence
// @c IsCopy) fails --- not deferred to the @c {a,a} body (#950 review).
struct CopyMoveOnly {  // move-only: the diagonal cannot duplicate it
  CopyMoveOnly() = default;
  CopyMoveOnly(CopyMoveOnly&&) = default;
  CopyMoveOnly(const CopyMoveOnly&) = delete;
};
static_assert(!IsArrow<Copy<CopyMoveOnly>>,
              "a move-only carrier must NOT model IsArrow via Copy: the "
              "diagonal a↦(a,a) copies, so copy-constructibility is required.");

// The parallel product and the composite meet are genuine arrows.
static_assert(IsArrow<Tensor<Identity<bool>, Identity<bool>>>,
              "R ⊗ S must be an arrow (A×B → C×D).");

// Compiler witness that @c Tensor IS the arrow-action of @c IsProduct: both the
// object it acts on and the object it produces are certified products (the
// requires-clause gate, restated here as a proof co-located with the arrow).
static_assert(
    IsProduct<Dom<Tensor<Identity<bool>, Identity<bool>>>, bool, bool>,
    "Tensor's source object A×B must model IsProduct.");
static_assert(
    IsProduct<Cod<Tensor<Identity<bool>, Identity<bool>>>, bool, bool>,
    "Tensor's target object C×D must model IsProduct.");
static_assert(
    IsArrow<Intersect<Identity<bool>, Identity<bool>, std::logical_and<bool>>>,
    "the composite meet Δ† ∘ (R ⊗ S) ∘ Δ must be an arrow.");

// @c Tensor is the canonical MODEL of @c IsTensor: product Domain AND product
// Codomain.  The two NEGATIVE witnesses discriminate the near-miss comonoid
// legs
// --- Copy has a bare (non-product) Domain @c bool, Merge a bare Codomain @c
// bool
// --- so neither is the arrow-half of the product bifunctor.
static_assert(IsTensor<Tensor<Identity<bool>, Identity<bool>>>,
              "R ⊗ S is the arrow-half of the product bifunctor (A×B → C×D).");
static_assert(!IsTensor<Copy<bool>>,
              "Δ: bool → bool×bool has a NON-product Domain, so not a Tensor.");
static_assert(
    !IsTensor<Merge<bool, std::logical_and<bool>>>,
    "Δ†: bool×bool → bool has a NON-product Codomain, so not a Tensor.");

// Over the identity endo-leg the composite collapses to the identity: it is the
// arrow-level shadow of the idempotent law a ∧ a = a (Δ copies, id⊗id is inert,
// Δ† merges the two equal copies).  A runtime witness, not just a shape check.
static_assert(
    Intersect<Identity<bool>, Identity<bool>, std::logical_and<bool>>{}(true) ==
        true,
    "Δ† ∘ (id ⊗ id) ∘ Δ must compute a ∧ a = a.");
static_assert(Intersect<Identity<bool>, Identity<bool>,
                        std::logical_and<bool>>{}(false) == false,
              "Δ† ∘ (id ⊗ id) ∘ Δ must compute a ∧ a = a.");

// ---------------------------------------------------------------------------
// Carrier coherence witnesses (co-located with the concept they exercise;
// relocated here from :lattice_term, #946).  The Δ ⊣ ∧ theory is carrier-
// generic, so the meet path type-checks against IsMeetAsRightAdjoint over the
// order-certified carriers the reducer reduces, not just the Boolean seed (that
// one edge stays in :lattice_term, next to the reducer it type-checks).
// ---------------------------------------------------------------------------

// int: the integral chain.  The injected glb is @c Inf (min), supplied exactly
// as @c reduce<> injects its order-algebra meet.
static_assert(
    IsMeetAsRightAdjoint<Copy<int>, Merge<int, Inf>>,
    "the meet on the integral chain must be the right adjoint of the diagonal "
    "(glb = min).");

// STRUCTURAL LIMITATION, pinned --- NOT a soundness guarantee.  This assertion
// is a KNOWN false positive, kept only so the gap stays compile-visible: if a
// later revision (#908 + the value-level product-order leg) tightens the
// concept to reject the join, this line fails and forces the narrative to be
// updated.  It does NOT endorse @c Sup as a meet.  @c IsMeetAsRightAdjoint is a
// structural SHAPE gate (crossed signatures + posetal carrier + definite
// variance); it cannot see which order-op the merge computes, so the JOIN
// (@c Sup = max) passes it too (the join is monotone, so the #946 variance
// tightening does not reject it).  Certifying the merge is the glb and not the
// lub is the injected op's obligation, exactly as with @c IsGaloisConnection.
static_assert(
    IsMeetAsRightAdjoint<Copy<int>, Merge<int, Sup>>,
    "STRUCTURAL LIMITATION (not a soundness guarantee): the JOIN (Sup) also "
    "passes the shape gate IsMeetAsRightAdjoint; glb-vs-lub is the injected "
    "op's obligation, NOT something this concept discharges.  Do not dispatch "
    "on it.");

// Ternary (Kleene K₃): the 3-chain False < Unknown < True.  The injected glb is
// @c Inf (min), which on this chain IS the Kleene AND.  Witnessable because
// @c :logic hand-registers K₃'s ≤ as transitive + antisymmetric for the typed
// @c std::less_equal<Ternary> (the :species blanket certifies only integral /
// bool, NOT arbitrary enums; #946 review), so IsPosetal<Ternary,
// less_equal<Ternary>> holds.
static_assert(
    IsMeetAsRightAdjoint<Copy<Ternary>, Merge<Ternary, Inf>>,
    "the Kleene meet (AND = min) on the K₃ chain must be the right adjoint of "
    "the diagonal.");

// The meet-trichotomy bridge (#946 Task C).  Three meet presentations that
// never referenced each other are ONE universal property, the glb:
//   (1) Δ ⊣ ∧           : Merge = Δ† (this partition);
//   (2) meet-as-product : category::Meet with the MakeMeet pairing factory;
//   (3) meet-as-pullback: sets::MeetSet ⊨ IsPullback (#881, downstream :sets).
// (1) and (2) share ONE substrate: the SAME IsProduct concept certifies both
// the comonoid's product OBJECT (the pair Δ copies into, which Tensor's
// arrow-action is gated on) and the lattice AST's product NODE.  Object half
// and arrow half of the one product bifunctor.
static_assert(IsProduct<std::pair<bool, bool>, bool, bool>,
              "comonoid/Tensor substrate: the pair A×A is the product OBJECT.");
static_assert(
    IsProduct<Meet<bool, bool>, bool, bool, MakeMeet>,
    "meet-as-product: the AST Meet node is that SAME product, MakeMeet the "
    "pairing factory.");
// (3) is a documented cross-reference, not a forced assert: sets::MeetSet ⊨
// IsPullback lives in :sets, downstream of both partitions, so a structural
// bridge to the IsPullback family is more than a low-risk local witness.
// FIXME(#946): unify the glb across the product and pullback presentations.

}  // namespace dedekind::category
