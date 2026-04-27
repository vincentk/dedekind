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
import :natural;  // For IsNaturalTransformation, used in unit/counit witnesses (#434)
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
 *  variant-level analog of @c embed_ℕ_ℤ.  Closes #434.
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
    HasAdjunctionShape<Left, Right> &&
    IsUnitOfAdjunction<Unit, Left, Right> &&
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

}  // namespace dedekind::category
