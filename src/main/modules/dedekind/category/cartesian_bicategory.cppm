/**
 * @file
 * @brief Sketch (epic #946, slice S0): the meet is the right adjoint of the
 *        diagonal. Reifies the theory @c Δ @c ⊣ @c ∧ as a @c concept first
 *        (@c IsMeetAsRightAdjoint), so the composite meet @c = @c Δ† @c ∘
 *        @c (⊗) @c ∘ @c Δ must type-check against it.
 *
 * @details A compiled, type-checked partition. @c :lattice_term imports it and
 *          @c static_asserts @c IsMeetAsRightAdjoint over its canonical
 * carrier, so the reification is load-bearing early rather than an orphan. The
 *          arrow-level @c (⊗) (@c Tensor) and the composite meet
 *          @c = @c Δ† @c ∘ @c (⊗) @c ∘ @c Δ (@c Intersect) are provided; the
 *          product-order leg (a value-level proof that the composite computes
 *          the glb over a non-trivial pair) remains @c FIXME(#946).
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
  constexpr Codomain operator()(const A& a) const { return {a, a}; }
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
  constexpr A operator()(const Domain& p) const {
    return Meet{}(p.first, p.second);
  }
};

// Variance registrations for the comonoid legs (:posetal @c is_monotone_v).
// @c IsMeetAsRightAdjoint gates on @c IsGaloisConnection, tightened (#946) to
// require @c IsVariant on both legs, so the copy and merge arrows the witnesses
// use must carry a declared variance or the (correctly) narrowed concept would
// reject them.  These per-arrow opt-ins are the MANUAL step #908 (reify
// predicate variance) would DERIVE structurally from each injected op's
// variance (and would add the antitone dual); cf. #791 (certify monotone / join
// operations).  Monotonicity is undecidable in general, so the declaration is
// trusted and the public review is the audit trail (as with @c
// is_monic_arrow_v).
//
// Copy @c a↦(a,a) is monotone under the product order for ANY relation (both
// components move together), so it is registered generically like @c Identity.
template <typename A, typename Op>
inline constexpr bool is_monotone_v<Copy<A>, Op> = true;
// The injected binary glb / lub are monotone in each argument: min (@c Inf) and
// max (@c Sup) preserve the order, as does Boolean AND (the reducer edge).
template <typename A, typename Op>
inline constexpr bool is_monotone_v<Merge<A, Inf>, Op> = true;
template <typename A, typename Op>
inline constexpr bool is_monotone_v<Merge<A, Sup>, Op> = true;
template <typename Op>
inline constexpr bool is_monotone_v<Merge<bool, std::logical_and<bool>>, Op> =
    true;

/**
 * @concept IsMeetAsRightAdjoint
 * @brief The meet @c ∧ is the right adjoint of the diagonal @c Δ (@c Δ⊣∧):
 *        the reified theory this slice postulates as a type-check.
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
 *       was tightened (#946) to require @c IsVariant on both legs, so both @c
 * Cp and @c Mg must carry a declared variance (see the @c is_monotone_v
 *       registrations above).  That NARROWS the gap (a non-variant merge is now
 *       rejected) but does NOT close the meet-vs-join residual: the join @c Sup
 *       is monotone too, so it still passes.  Which order-op the merge computes
 *       stays the value-level product-order leg, FIXME(#946).  #908 (reify
 *       predicate variance) would derive the variance structurally; #791
 *       certifies the monotone / join operations themselves.
 * @tparam Cp the copy/diagonal @c Δ.
 * @tparam Mg the meet/merge @c ∧.
 * @tparam Leq the order on @c P; defaults to @c std::less_equal<P>. */
export template <typename Cp, typename Mg,
                 typename Leq = std::less_equal<Dom<Cp>>>
concept IsMeetAsRightAdjoint =
    IsGaloisConnection<Cp, Mg> && IsPosetal<Dom<Cp>, Leq> &&
    std::same_as<Cod<Cp>, std::pair<Dom<Cp>, Dom<Cp>>>;

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
 *  @tparam R the left endo-leg @c A→A.
 *  @tparam S the right endo-leg @c A→A.
 *  @tparam Meet the injected glb, forwarded to @c Merge (see there).
 *  @note @c R and @c S are endomorphisms of the common carrier @c A so the
 *        composite is again @c A→A; @c Intersect<Identity,Identity> collapses
 * to the identity, the arrow-level shadow of idempotence @c a∧a=a. */
export template <IsArrow R, IsArrow S, typename Meet>
  requires std::same_as<Dom<R>, Dom<S>> && std::same_as<Cod<R>, Dom<R>> &&
           std::same_as<Cod<S>, Dom<R>>
struct Intersect {
  using Domain = Dom<R>;
  using Codomain = Dom<R>;
  R r{};
  S s{};
  constexpr Codomain operator()(const Domain& a) const {
    return Merge<Domain, Meet>{}(Tensor<R, S>{r, s}(Copy<Domain>{}(a)));
  }
};

// The comonoid legs are the arrows the theory names: Copy is an arrow, Delete
// is the unique terminal morphism ε: A → One.
static_assert(IsArrow<Copy<bool>>, "Δ: A → A×A must be an arrow.");
static_assert(IsTerminalMorphism<Delete<bool>>,
              "ε: A → One must be the terminal morphism (counit).");

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

// HONESTY OBLIGATION, made visible.  IsMeetAsRightAdjoint is a STRUCTURAL shape
// (crossed signatures + a posetal carrier + definite variance); it cannot see
// which order-op the merge computes, so the JOIN (@c Sup = max) passes the very
// same test --- the join is monotone too, so the #946 variance tightening does
// NOT reject it.  That is a genuine false positive: certifying the merge is the
// glb and not the lub is the engineer's obligation (as with
// IsGaloisConnection), not something the concept discharges.  This
// static_assert pins the gap so it stays honest.
static_assert(
    IsMeetAsRightAdjoint<Copy<int>, Merge<int, Sup>>,
    "structural witness: the JOIN (Sup) also passes IsMeetAsRightAdjoint, the "
    "glb-vs-lub honesty obligation the concept cannot discharge.");

// Ternary (Kleene K₃): the 3-chain False < Unknown < True.  The injected glb is
// @c Inf (min), which on this chain IS the Kleene AND.  Witnessable because the
// :species is_enum_v blanket certifies K₃'s ≤ as transitive + antisymmetric, so
// IsPosetal<Ternary, less_equal<Ternary>> holds.
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
