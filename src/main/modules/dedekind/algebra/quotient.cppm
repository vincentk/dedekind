/**
 * @file dedekind/algebra/quotient.cppm
 * @partition :quotient
 * @brief Quotient algebras as a structural functor — declared base,
 *        propagated traits.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section quotient__The_Meta_Symmetry
 *
 * A "quotient over a bona fide algebra is itself a bona fide algebra".
 * Categorically: a structure-preserving functor @c F: @c C @c → @c C
 * (e.g.\ field-of-fractions @c Frac, complex extension @c Cplx, dual
 * extension @c Dual) preserves the algebraic surface — associativity,
 * commutativity, distributivity, and the totality / saturation
 * certificate — of its input.  This partition reifies that meta-
 * symmetry at the type level: the carrier-side declaration
 * @c quotient_algebra_base<Q>::type @c = @c Base records the
 * functorial relation, and the propagation specialisations below lift
 * the species-trait pins from @c Base to @c Q uniformly.
 *
 * The arrow-side cousin is @c IsQuotientMorphism in @c :universal —
 * a declared homomorphism + declared surjectivity, naming the
 * canonical projection @c π: @c Base @c → @c Base/~ @c = @c Q.  The
 * two together formalise the quotient construction at both ends:
 *
 *   - @c IsQuotientAlgebra<Q>          (this partition; carrier side)
 *   - @c IsQuotientMorphism<Arrow>     (@c :universal; morphism side)
 *
 * Three concrete instantiations ship today (#498/#499 NEW-A):
 *
 *   - @c Rational<I> @c = @c Frac(I)         (numbers:rational)
 *   - @c Complex<R>  @c = @c R[i]/(i² @c + @c 1)   (numbers:complex)
 *   - @c Dual<F>     @c = @c F[ε]/(ε²)             (analysis:dual)
 *
 * Each carrier site declares the quotient relation @b once via a
 * single @c quotient_algebra_base<Q> specialisation; the species
 * traits propagate uniformly without per-trait enumeration.  Carrier-
 * specific bits (additive identity values, additive inverse via @c -q)
 * remain at the carrier site as @c identity_trait / @c inverse_trait
 * specialisations because their construction depends on the carrier's
 * internal layout.
 *
 * @section quotient__Categorical_Reading
 *
 * In the @c :morphism vocabulary, the quotient construction is a
 * @b hub @b arrow (a functor between categories) and its image is a
 * @b spoke (an object in the target category).  The propagation
 * specialisations express functoriality at the trait level: structural
 * traits on @c Q lift from the corresponding traits on @c Base under
 * @c std::plus / @c std::multiplies, exactly as a structure-preserving
 * functor does on objects in @b CRing or @b Mod_R.
 *
 * Issues #498 (Algebraic Tower) / #499 (NEW-A trait registry).
 */
module;

#include <functional>  // std::plus / std::multiplies in the propagation

export module dedekind.algebra:quotient;

import dedekind.category; // species traits, IsFunctor (the meta-symmetry context)

namespace dedekind::category {

/** @brief @c quotient_algebra_base<Q>: carrier-side declaration that
 *         @c Q is a quotient of some base algebra.  Specialise the
 *         @c ::type member at the carrier-defining partition (e.g.\
 *         @c Rational<I> in @c numbers:rational records
 *         @c quotient_algebra_base<Rational<I>>::type @c = @c I). */
export template <typename Q>
struct quotient_algebra_base {};

/** @brief Convenience alias for @c quotient_algebra_base<Q>::type. */
export template <typename Q>
using quotient_algebra_base_t = typename quotient_algebra_base<Q>::type;

/** @concept IsQuotientAlgebra
 *  @brief @c Q is declared as a quotient of some base algebra.
 *  @details Triggered by a specialisation of
 *           @c quotient_algebra_base<Q> exposing a @c ::type member.
 *           Carrier-side cousin of @c algebra::IsQuotientMorphism (the
 *           projection @c π: @c Base @c → @c Q) in @c :universal.
 */
export template <typename Q>
concept IsQuotientAlgebra =
    requires { typename quotient_algebra_base<Q>::type; };

// --- Propagation: structural traits lift from Base to Q. -------------------
//
// Each propagation rule expresses functoriality of the quotient
// construction at the trait level: @c Q inherits the structural pin
// from @c Base under the corresponding operation.  Together with the
// carrier-site identity / inverse specialisations, this is sufficient
// to lift @c IsAdditiveGroup, @c IsRing, @c IsModule on @c Q from the
// strict gating on @c Base.

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_associative<Q, std::plus<Q>>
    : is_associative<quotient_algebra_base_t<Q>,
                     std::plus<quotient_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_associative<Q, std::multiplies<Q>>
    : is_associative<quotient_algebra_base_t<Q>,
                     std::multiplies<quotient_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_commutative<Q, std::plus<Q>>
    : is_commutative<quotient_algebra_base_t<Q>,
                     std::plus<quotient_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_commutative<Q, std::multiplies<Q>>
    : is_commutative<quotient_algebra_base_t<Q>,
                     std::multiplies<quotient_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsQuotientAlgebra<Q>
inline constexpr bool is_distributive_v<Q, std::multiplies<Q>, std::plus<Q>> =
    is_distributive_v<quotient_algebra_base_t<Q>,
                      std::multiplies<quotient_algebra_base_t<Q>>,
                      std::plus<quotient_algebra_base_t<Q>>>;

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_saturating<Q, std::plus<Q>>
    : is_saturating<quotient_algebra_base_t<Q>,
                    std::plus<quotient_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_saturating<Q, std::multiplies<Q>>
    : is_saturating<quotient_algebra_base_t<Q>,
                    std::multiplies<quotient_algebra_base_t<Q>>> {};

}  // namespace dedekind::category
