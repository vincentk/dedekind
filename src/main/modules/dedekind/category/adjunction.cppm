/**
 * @file dedekind/category/adjunction.cppm
 * @partition :adjunction
 * @module dedekind.category:adjunction
 * @brief Level 2.3: Adjoint functors (the Free / Forgetful axis).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Adjunctions
 *
 * An @b adjunction @c F @c ⊣ @c U is a pair of functors
 *   @c F : @c 𝒟 @c → @c 𝒞 (the @b free / left adjoint) and
 *   @c U : @c 𝒞 @c → @c 𝒟 (the @b forgetful / right adjoint),
 * with a natural isomorphism @c Hom_𝒞(F(d), c) @c ≅ @c Hom_𝒟(d, U(c))
 * between the two hom-sets.  Equivalently: a @b unit
 *   @c η @c : @c id_𝒟 @c ⇒ @c U @c ∘ @c F
 * and a @b counit
 *   @c ε @c : @c F @c ∘ @c U @c ⇒ @c id_𝒞
 * satisfying the @b triangle @b identities (Mac Lane Ch. IV; Pierce
 * gives adjunctions their own section because the structure recurs
 * everywhere a free construction does).
 *
 * @section In_this_codebase
 *
 * Concrete carrier-side instances:
 *   * @c F : @c Rig @c → @c Ring (the Grothendieck construction):
 *     @c SignedCardinality @c = @c F(Cardinality), @c Cardinality @c
 *     = @c U(SignedCardinality).
 *   * @c F : @c IntegralDomain @c → @c Field (the field of fractions):
 *     @c Rational<Z> @c = @c F(Z) for @c Z @c = @c
 *     SignedExtensionalCardinal<>.
 *   * @c F : @c OrderedField @c → @c CompleteOrderedField (the
 *     Cauchy completion): @c Real @c = @c F(Rational).
 *   * @c F : @c Field @c → @c AlgebraicallyClosedField (the algebraic
 *     closure): @c Complex @c = @c F(Real).
 *
 * Each step in the carrier lattice ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ is a free
 * construction in its respective category.  The closure-forcing
 * operators (@c -Cardinality @c → @c SignedCardinality, etc.) are the
 * unit's components made operational at the operator level — see
 * @c docs/design/carrier-lattice.md.
 *
 * @section Two_concept_tiers
 *
 * The partition exposes two conceptually distinct surfaces:
 *
 * @li @c HasAdjunctionShape<F, @c U> — @b structural-only @b
 *     2-parameter form.  Names just the categorical signatures of
 *     the adjoint pair; the unit / counit are unspecified.  Used in
 *     the directional aliases @c IsFreeFunctor / @c IsForgetfulFunctor
 *     where the directional reading is what's load-bearing.
 *
 * @li @c IsAdjunction<Left, @c Right, @c Unit, @c Counit> — @b
 *     stronger 4-parameter @b witness form.  Requires explicit unit
 *     and counit arrows and exercises the typed triangle-identity
 *     surface.  Used where the actual unit / counit components are
 *     concretely available (e.g.\ for monad-style derivations).
 *
 * The split mirrors a recurring project pattern: the 2-param shape
 * concept (cheap, deducible) anchors the textbook vocabulary and
 * gates downstream uses; the n-param witness concept binds the
 * operational evidence.
 *
 * @note "Adjoint functors arise everywhere."
 *       — Saunders Mac Lane, @c Categories @c for @c the @c Working
 *         @c Mathematician (1971), as quoted in Benjamin C. Pierce,
 *         @c Basic @c Category @c Theory @c for @c Computer @c
 *         Scientists (1991), §2.6.
 */
module;

#include <concepts>
#include <type_traits>
#include <utility>

export module dedekind.category:adjunction;

import :functor;
import :morphism;
import :natural;  // For IsNaturalTransformation, used in unit/counit witnesses
                  // (#434)
import :small;

namespace dedekind::category {

/**
 * @concept HasAdjunctionShape
 * @brief @b Structural @b shape: @c F and @c U have the categorical
 *        signature of an adjoint pair @c F @c ⊣ @c U — @c F : @c 𝒟
 *        @c → @c 𝒞 (left, free) and @c U : @c 𝒞 @c → @c 𝒟 (right,
 *        forgetful).
 *
 *  Concept-level test: both are functors, and their categorical
 *  signatures are compatible — @c F's source category equals
 *  @c U's target category (both are @c 𝒟), and @c F's target
 *  category equals @c U's source category (both are @c 𝒞).
 *
 *  @b Relationship @b to @b @c IsAdjunction (defined below): @c
 *  IsAdjunction is the @b stronger 4-parameter witness @c <Left,
 *  @c Right, @c Unit, @c Counit> that requires explicit unit /
 *  counit arrows and exercises the typed triangle-identity surface.
 *  @c HasAdjunctionShape is the @b structural-only @b 2-parameter
 *  form that names just the signatures of the adjoint pair, leaving
 *  the unit / counit unspecified.  Used in the directional aliases
 *  @c IsFreeFunctor / @c IsForgetfulFunctor below, where the
 *  directional reading is what's load-bearing rather than the unit
 *  / counit content.
 *
 *  The @b content of the adjunction (the natural isomorphism, the
 *  triangle identities) is universal-property territory that the
 *  type-checker cannot quantify over; engineers asserting "this F,
 *  U pair forms an adjunction" either provide the explicit unit /
 *  counit (use @c IsAdjunction) or carry the proof obligation as
 *  an opt-in trait (the @c HasAdjunctionShape route).
 */
export template <typename F, typename U>
concept HasAdjunctionShape =
    IsFunctor<F> && IsFunctor<U> &&
    std::same_as<typename F::Σ_cat,
                 typename U::Τ_cat> &&  // F starts where U ends
    std::same_as<typename F::Τ_cat,
                 typename U::Σ_cat>;  // F ends where U starts

/**
 * @concept IsFreeFunctor
 * @brief Directional alias: @c F is the @b left adjoint of an
 *        adjunction @c F @c ⊣ @c U.  Names the @b free direction —
 *        @c F sends an object @c d : @c 𝒟 to its @b free construction
 *        @c F(d) : @c 𝒞 (the universal recipient under @c U).
 *
 *  Mathematical instances: free monoid (Set → Mon), Grothendieck
 *  group (CommMon → Ab), tensor algebra (Vect → Ring), free
 *  module / vector space, group ring, etc.  In this codebase: the
 *  carrier-lattice steps ℕ → ℤ → ℚ → ℝ → ℂ are each a free
 *  construction in their respective category.
 */
export template <typename F, typename U>
concept IsFreeFunctor = HasAdjunctionShape<F, U>;

/**
 * @concept IsForgetfulFunctor
 * @brief Directional alias: @c U is the @b right adjoint of an
 *        adjunction @c F @c ⊣ @c U.  Names the @b forgetful
 *        direction — @c U drops the structure that distinguishes
 *        @c 𝒞 from @c 𝒟 (e.g., @c U_Ring→Rig forgets that you can
 *        do @c −).
 *
 *  Concrete sense: @c U @c : @c Ring @c → @c Rig sends a ring to
 *  its underlying rig; the adjunction with the Grothendieck-group
 *  free functor recovers @c F(R) as "the smallest ring containing
 *  R as a sub-rig".
 */
export template <typename U, typename F>
concept IsForgetfulFunctor = HasAdjunctionShape<F, U>;

/**
 * @concept IsUnitOfAdjunction
 * @brief @b Structural @b shape: @c η is the unit of an adjunction
 *        @c F ⊣ @c U — a natural transformation @c id_𝒟 ⇒ @c U ∘ @c F.
 *
 *  Composes the existing @c IsNaturalTransformation (in @c :natural)
 *  with @c HasAdjunctionShape.  The triangle identities — the actual
 *  content of the adjunction — remain universal-property territory
 *  (engineer's honesty obligation; not deducible by the type-checker).
 *
 *  In carrier-lattice terms, @c η_d : @c d → @c U(F(d)) is the
 *  canonical embedding of @c d into the underlying object of its
 *  free construction — for example, @c η_ℕ : @c ℕ ↪ @c ℤ is the
 *  variant-level analog of @c embed_uint_sint_.  Closes #434.
 */
export template <typename Eta, typename F, typename U>
concept IsUnitOfAdjunction =
    HasAdjunctionShape<F, U> &&
    IsNaturalTransformation<Eta, identity_functor<typename F::Σ_cat>,
                            composite_functor<F, U>>;

/**
 * @concept IsCounitOfAdjunction
 * @brief @b Structural @b shape: @c ε is the counit of an adjunction
 *        @c F ⊣ @c U — a natural transformation @c F ∘ @c U ⇒
 *        @c id_𝒞.  Dual of @c IsUnitOfAdjunction.
 */
export template <typename Epsilon, typename F, typename U>
concept IsCounitOfAdjunction =
    HasAdjunctionShape<F, U> &&
    IsNaturalTransformation<Epsilon, composite_functor<U, F>,
                            identity_functor<typename F::Τ_cat>>;

/**
 * @concept IsAdjunction
 * @brief A typed unit/counit witness relating a left and right functor.
 *
 *  Tightened (#434) to require the unit and counit to be @b natural
 *  @b transformations, not raw arrows.  Per Mac Lane (Ch. IV) /
 *  Milewski (*Category Theory for Programmers*, Ch. on Adjunctions):
 *  an adjunction is @b defined by natural transformations; the
 *  earlier arrow-based form (pre-#434) was structurally under-strict.
 *  The arrow-shape constraints on @c Unit / @c Counit are retained
 *  via @c IsNaturalTransformation's own requirements.
 *
 *  The triangle identities — @c (εF) ∘ (Fη) = @c 1_F and @c (Gε) ∘
 *  (ηG) = @c 1_G — remain universal-property territory; they're the
 *  engineer's honesty obligation at the witness site.
 */
export template <typename Left, typename Right, typename Unit, typename Counit>
concept IsAdjunction =
    HasAdjunctionShape<Left, Right> && IsUnitOfAdjunction<Unit, Left, Right> &&
    IsCounitOfAdjunction<Counit, Left, Right>;

/**
 * @brief Typed unit/counit data for an adjunction witness.
 */
export template <typename Left, typename Right, typename Unit, typename Counit>
  requires IsAdjunction<Left, Right, Unit, Counit>
struct adjunction_witness {
  using left_functor_type = Left;
  using right_functor_type = Right;
  using unit_type = Unit;
  using counit_type = Counit;

  Left left{};
  Right right{};
  Unit unit{};
  Counit counit{};
};

/**
 * @brief Factory for adjunction witnesses.
 */
export template <typename Left, typename Right, typename Unit, typename Counit>
  requires IsAdjunction<std::remove_cvref_t<Left>, std::remove_cvref_t<Right>,
                        std::remove_cvref_t<Unit>, std::remove_cvref_t<Counit>>
constexpr auto make_adjunction(Left&& left, Right&& right, Unit&& unit,
                               Counit&& counit) {
  return adjunction_witness<
      std::remove_cvref_t<Left>, std::remove_cvref_t<Right>,
      std::remove_cvref_t<Unit>, std::remove_cvref_t<Counit>>{
      std::forward<Left>(left), std::forward<Right>(right),
      std::forward<Unit>(unit), std::forward<Counit>(counit)};
}

// ===========================================================================
// Galois connections — adjunctions in posets (closes #435)
// ===========================================================================
//
// When the source and target categories of an adjunction are @b
// posets (categories where there's at most one morphism between any
// two objects, namely the @c ≤ relation), the adjunction degenerates
// to its simplest form: a Galois connection.  The Hom-set bijection
// @c Hom_𝒞(F(d), c) ≅ Hom_𝒟(d, U(c)) collapses to the order-theoretic
// equivalence
//
//   f(x) ≤ y  ⟺  x ≤ g(y)
//
// (Pierce, @c Basic @c Category @c Theory @c §2.6.)  In posets the
// triangle identities trivialise — there's at most one arrow between
// any two objects, so all naturality squares commute automatically;
// the universal-property content reduces to the inequality above.
//
// Concrete examples in arithmetic (per the user-shared Gemini
// conversation during PR #433):
//   * Inclusion @c ι : ℤ ↪ ℝ; floor @c ⌊·⌋ is its right adjoint:
//     @c ι(n) ≤ x ⟺ n ≤ ⌊x⌋.
//   * Ceiling @c ⌈·⌉ is the left adjoint of @c ι:
//     @c ⌈x⌉ ≤ n ⟺ x ≤ ι(n).
//   * Addition @c +k ⊣ subtraction @c -k:
//     @c x + k ≤ y ⟺ x ≤ y - k.
//   * In ℕ, the right adjoint of @c +k is @b truncated @b subtraction
//     @c y −̇ k = max(y - k, 0); see @c
//     docs/design/carrier-lattice.md, "A road not taken: truncated
//     subtraction" — the order-theoretic dual of the closure-forcing
//     @c ℕ - ℕ → ℤ this codebase chose.
//
// Composing both directions of a Galois connection produces a @b
// closure @b operator: @c g ∘ f is idempotent (@c g(f(g(f(x)))) =
// @c g(f(x))), monotone, and extensive (@c x ≤ g(f(x))).  The
// closure-operator concept below names this structural rotation.

/**
 * @concept IsGaloisConnection
 * @brief @b Structural @b shape: @c F and @c G form a Galois
 *        connection @c F ⊣ @c G in a posetal setting — the simplest
 *        adjunction.  Both are monotone arrows; @c F : @c P → @c Q
 *        and @c G : @c Q → @c P with the source/target carriers
 *        crossed (so @c F's @c Codomain matches @c G's @c Domain
 *        and vice versa).  The universal-property test
 *
 *          f(x) ≤ y  ⟺  x ≤ g(y)
 *
 *        holds.  C++ concepts cannot quantify universally over @c x
 *        and @c y, so the equivalence is the engineer's honesty
 *        obligation; the structural shape names the @b signatures.
 *
 *  @b Relationship @b to @b @c HasAdjunctionShape: a Galois
 *  connection @b is an adjunction in the simplest possible
 *  category-theoretic setting (posets).  @c IsGaloisConnection is
 *  the order-theoretic specialisation — it doesn't @b require the
 *  full functor / natural-transformation machinery because in a
 *  poset there's at most one arrow between any two objects, so the
 *  naturality content trivialises.  Useful as a lightweight
 *  vocabulary anchor for arithmetic-flavoured adjunctions
 *  (floor/ceiling, addition/subtraction, etc.).
 *
 *  @c P and @c Q are not free template parameters — they are
 *  recovered from @c F's @c Domain / @c Codomain.  @c G's
 *  carriers are required to be the cross-pair: @c G : @c Q → @c P.
 */
export template <typename F, typename G>
concept IsGaloisConnection =
    IsArrow<F> && IsArrow<G> && std::same_as<Dom<G>, Cod<F>> &&
    std::same_as<Cod<G>, Dom<F>>;

/**
 * @concept IsClosureOperator
 * @brief @b Structural @b shape: @c C is a closure operator on a
 *        poset @c P — an arrow @c C : @c P → @c P that is
 *        idempotent (@c C(C(x)) @c = @c C(x)), monotone (@c x @c ≤
 *        @c y @c ⇒ @c C(x) @c ≤ @c C(y)), and extensive (@c x @c ≤
 *        @c C(x)) for all @c x.
 *
 *  Every Galois connection induces a closure operator via @c g ∘
 *  @c f (the round-trip on the source side).  Example in arithmetic:
 *  for the ceiling/inclusion adjunction @c ⌈·⌉ ⊣ @c ι, the
 *  source-side round-trip @c ι ∘ @c ⌈·⌉ : @c ℝ → @c ℝ is the
 *  round-up-to-nearest-integer closure (extensive, monotone, and
 *  idempotent on @c (ℝ, ≤)).  Dually, for the floor/inclusion
 *  adjunction @c ι ⊣ @c ⌊·⌋, the source-side round-trip @c ⌊·⌋ ∘ @c ι :
 *  @c ℤ → @c ℤ is a closure operator as well — indeed, the identity
 *  on @c ℤ.  Note: @c embed_uint_sint_ ∘ @c abs is @b not a closure operator
 *  because it isn't monotone on @c (ℤ, ≤) (sign-folding inverts
 *  order on the negative fragment); the @c (embed_uint_sint_, @c abs) pair
 *  is a split mono / split epi rather than a Galois connection.
 *
 *  C++ concepts cannot quantify universally over @c P, so the
 *  three properties (idempotent / monotone / extensive) are the
 *  engineer's honesty obligation; the structural shape names the
 *  @b signature only (an @c IsArrow whose @c Domain equals its
 *  @c Codomain).
 */
export template <typename C>
concept IsClosureOperator = IsArrow<C> && std::same_as<Dom<C>, Cod<C>>;

}  // namespace dedekind::category
