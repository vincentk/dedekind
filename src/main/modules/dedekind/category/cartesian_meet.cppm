/**
 * @file
 * @brief Sketch (epic #946, slice S0): the meet is the right adjoint of the
 *        diagonal. Reifies the theory @c Δ @c ⊣ @c ∧ as a @c concept first
 *        (@c IsMeetViaAdjunction), so the composite meet @c = @c Δ† @c ∘ @c (⊗)
 *        @c ∘ @c Δ must type-check against it.
 *
 * @details DESIGN SKETCH for architectural review. NOT yet wired into the
 *          @c dedekind.category aggregator and NOT expected to build clean:
 *          the product-order leg, the terminal object, and the @c (⊗) plumbing
 *          are marked @c FIXME(#946). The point of this file is the @b concept
 *          and the @b names, not a finished implementation.
 */
export module dedekind.category:cartesian_meet;

import :morphism;    // IsArrow, Dom, Cod
import :adjunction;  // IsGaloisConnection (F left-adjoint-to G in a poset)
import :posetal;     // IsPosetal (a poset IS a thin category)
import :species;     // Inf: the value-level meet (min) reused as the merge

import <utility>;     // std::pair: the binary product (cf. :limit)
import <variant>;     // std::monostate: stand-in terminal object
import <functional>;  // std::less_equal

namespace dedekind::category {

/**
 * @section cartesian_meet__Design Meet as the adjoint of the diagonal
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
 *  @note FIXME(#946): @c std::monostate stands in for the category terminal
 *        object; wire to the ETCS @c 1 when the comonoid laws land. */
export template <typename A>
struct Delete {
  using Domain = A;
  using Codomain = std::monostate;
  constexpr Codomain operator()(const A&) const { return {}; }
};

/** @brief Merge (the dagger of copy) @c Δ†:A×A→A, @c (a,b)↦a⊓b. The right
 *         adjoint of the diagonal; the value that @c IsMeetViaAdjunction
 *         certifies as the glb.
 *  @tparam A the carrier object.
 *  @tparam Meet the value-level glb; defaults to @c Inf (min).
 *  @note FIXME(#946): @c Inf is the totally-ordered merge; the general poset
 *        merge is the glb of the injected order algebra. */
export template <typename A, typename Meet = Inf>
struct Merge {
  using Domain = std::pair<A, A>;
  using Codomain = A;
  constexpr A operator()(const Domain& p) const {
    return Meet{}(p.first, p.second);
  }
};

/**
 * @concept IsMeetViaAdjunction
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
 * @tparam Cp the copy/diagonal @c Δ.
 * @tparam Mg the meet/merge @c ∧.
 * @tparam Leq the order on @c P; defaults to @c std::less_equal<P>. */
export template <typename Cp, typename Mg,
                 typename Leq = std::less_equal<Dom<Cp>>>
concept IsMeetViaAdjunction =
    IsGaloisConnection<Cp, Mg> && IsPosetal<Dom<Cp>, Leq> &&
    std::same_as<Cod<Cp>, std::pair<Dom<Cp>, Dom<Cp>>>;

// FIXME(#946): the implementation leg (a). The relational-intersection
// composite  R ∩ S = Δ† ∘ (R ⊗ S) ∘ Δ  (copy the input, run both, merge)
// is the ⊗-plumbing that must satisfy IsMeetViaAdjunction; sketched separately
// once the concept above is agreed. It is the A'DA / ZX-spider shape the LA
// layer (bra-ket / Mat(S) / transfer matrices) later inherits for free.

}  // namespace dedekind::category
