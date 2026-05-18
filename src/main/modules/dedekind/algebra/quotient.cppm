/**
 * @file dedekind/algebra/quotient.cppm
 * @partition :quotient
 * @brief HSP structure-preserving operations on algebras — H (quotient)
 *        + P (direct product) carrier-side concepts and structural-trait
 *        propagation.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "A class @c K of algebras is closed under the formation of
 *        subalgebras, homomorphic images, and direct products if and
 *        only if @c K is the class of all algebras satisfying some
 *        set of identities."
 *       — Garrett Birkhoff, @em On @em the @em structure @em of
 *         @em abstract @em algebras, Proc.\ Cambridge Phil.\ Soc.\ 31
 *         (1935), Theorem 1; the modern formulation is in
 *         Burris--Sankappanavar~\cite{burris1981universalalgebra}
 *         §II.11 (Theorem 11.9, the HSP theorem).
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

// is_associative + is_commutative propagate via the @b variable-template
// (not the struct): the species.cppm specs for primitives like
// @c unsigned @c int are set directly on the variable template
// (e.g.\ @c is_associative_v<T, @c std::plus<T>> @c = @c true), and a
// struct-level inheritance would read the struct's default @c false.
template <typename Q>
  requires IsQuotientAlgebra<Q>
inline constexpr bool is_associative_v<Q, std::plus<Q>> =
    is_associative_v<quotient_algebra_base_t<Q>,
                     std::plus<quotient_algebra_base_t<Q>>>;

template <typename Q>
  requires IsQuotientAlgebra<Q>
inline constexpr bool is_associative_v<Q, std::multiplies<Q>> =
    is_associative_v<quotient_algebra_base_t<Q>,
                     std::multiplies<quotient_algebra_base_t<Q>>>;

template <typename Q>
  requires IsQuotientAlgebra<Q>
inline constexpr bool is_commutative_v<Q, std::plus<Q>> =
    is_commutative_v<quotient_algebra_base_t<Q>,
                     std::plus<quotient_algebra_base_t<Q>>>;

template <typename Q>
  requires IsQuotientAlgebra<Q>
inline constexpr bool is_commutative_v<Q, std::multiplies<Q>> =
    is_commutative_v<quotient_algebra_base_t<Q>,
                     std::multiplies<quotient_algebra_base_t<Q>>>;

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

// is_periodic + is_idempotent propagate too: the IsTotal certificate
// in :species is the disjunction of these three paths, so any of
// them sufficing on Base must lift to Q for IsTotal to fire on Q.

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_periodic<Q, std::plus<Q>>
    : is_periodic<quotient_algebra_base_t<Q>,
                  std::plus<quotient_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_periodic<Q, std::multiplies<Q>>
    : is_periodic<quotient_algebra_base_t<Q>,
                  std::multiplies<quotient_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_idempotent<Q, std::plus<Q>>
    : is_idempotent<quotient_algebra_base_t<Q>,
                    std::plus<quotient_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsQuotientAlgebra<Q>
struct is_idempotent<Q, std::multiplies<Q>>
    : is_idempotent<quotient_algebra_base_t<Q>,
                    std::multiplies<quotient_algebra_base_t<Q>>> {};

// ---------------------------------------------------------------------------
// P (Direct Product) — Birkhoff's HSP, Burris-Sankappanavar §II.10.
//
// A direct product @c Q @c = @c Base × @c Base × ... × @c Base
// (n copies) is the dual construction to the quotient: it preserves
// the same structural properties as @c Base does, lifted
// componentwise.  In the codebase, @c Vec2V<T> @c = @c Free_2(T) =
// @c T × @c T is the worked rank-2 instance.
// ---------------------------------------------------------------------------

/** @brief @c product_algebra_base<Q>: carrier-side declaration that
 *         @c Q is a finite direct product of some base algebra
 *         (the @c P operation in Birkhoff HSP).  Specialise the
 *         @c ::type member at the carrier-defining partition (e.g.\
 *         @c Vec2V<T> in @c linear_algebra:vec2 records
 *         @c product_algebra_base<Vec2V<T>>::type @c = @c T). */
export template <typename Q>
struct product_algebra_base {};

/** @brief Convenience alias for @c product_algebra_base<Q>::type. */
export template <typename Q>
using product_algebra_base_t = typename product_algebra_base<Q>::type;

/** @concept IsProductAlgebra
 *  @brief @c Q is declared as a finite direct product of some base
 *         algebra.
 *  @details Triggered by a specialisation of
 *           @c product_algebra_base<Q> exposing a @c ::type member.
 *           Sibling of @c IsQuotientAlgebra; both are HSP operations
 *           that preserve the structural species traits of @c Base.
 */
export template <typename Q>
concept IsProductAlgebra = requires { typename product_algebra_base<Q>::type; };

// --- Propagation: structural traits lift componentwise from Base. ----------
//
// Direct products preserve the same axioms as quotients do:
// associativity / commutativity / distributivity / saturation all
// lift componentwise from Base to Base × Base × ... × Base.

template <typename Q>
  requires IsProductAlgebra<Q>
inline constexpr bool is_associative_v<Q, std::plus<Q>> =
    is_associative_v<product_algebra_base_t<Q>,
                     std::plus<product_algebra_base_t<Q>>>;

template <typename Q>
  requires IsProductAlgebra<Q>
inline constexpr bool is_associative_v<Q, std::multiplies<Q>> =
    is_associative_v<product_algebra_base_t<Q>,
                     std::multiplies<product_algebra_base_t<Q>>>;

template <typename Q>
  requires IsProductAlgebra<Q>
inline constexpr bool is_commutative_v<Q, std::plus<Q>> =
    is_commutative_v<product_algebra_base_t<Q>,
                     std::plus<product_algebra_base_t<Q>>>;

template <typename Q>
  requires IsProductAlgebra<Q>
inline constexpr bool is_commutative_v<Q, std::multiplies<Q>> =
    is_commutative_v<product_algebra_base_t<Q>,
                     std::multiplies<product_algebra_base_t<Q>>>;

template <typename Q>
  requires IsProductAlgebra<Q>
inline constexpr bool is_distributive_v<Q, std::multiplies<Q>, std::plus<Q>> =
    is_distributive_v<product_algebra_base_t<Q>,
                      std::multiplies<product_algebra_base_t<Q>>,
                      std::plus<product_algebra_base_t<Q>>>;

template <typename Q>
  requires IsProductAlgebra<Q>
struct is_saturating<Q, std::plus<Q>>
    : is_saturating<product_algebra_base_t<Q>,
                    std::plus<product_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsProductAlgebra<Q>
struct is_saturating<Q, std::multiplies<Q>>
    : is_saturating<product_algebra_base_t<Q>,
                    std::multiplies<product_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsProductAlgebra<Q>
struct is_periodic<Q, std::plus<Q>>
    : is_periodic<product_algebra_base_t<Q>,
                  std::plus<product_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsProductAlgebra<Q>
struct is_periodic<Q, std::multiplies<Q>>
    : is_periodic<product_algebra_base_t<Q>,
                  std::multiplies<product_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsProductAlgebra<Q>
struct is_idempotent<Q, std::plus<Q>>
    : is_idempotent<product_algebra_base_t<Q>,
                    std::plus<product_algebra_base_t<Q>>> {};

template <typename Q>
  requires IsProductAlgebra<Q>
struct is_idempotent<Q, std::multiplies<Q>>
    : is_idempotent<product_algebra_base_t<Q>,
                    std::multiplies<product_algebra_base_t<Q>>> {};

// ---------------------------------------------------------------------------
// S (Subalgebra) — Birkhoff's HSP, Burris-Sankappanavar §II.5 / §II.10.
//
// The third leg of HSP closure: a subalgebra S of A is a subobject
// (S ⊆ A in Set) that is closed under the algebraic operations of A.
// For a single operation Op : A × A → A, closure means: for all
// s, s' ∈ S, Op(s, s') ∈ S.  Universal-algebra anchor for the S leg
// completing the H (IsQuotientAlgebra) + P (IsProductAlgebra) + S
// triple — #718 Slice 3, blocking Slice 5's HSP-closed crown witness.
//
// Single-operation form lands first; multi-op variadic
// IsSubalgebra<S, A, Op...> for ring-flavoured carriers waits on a
// downstream demand (Sollbruchstelle, mirroring Slice 0's IsCongruence
// shape).
// ---------------------------------------------------------------------------

/**
 * @brief User-declared closure witness: subobject @c S of @c A is
 *        closed under operation @c Op.
 *
 * @details Closure: for all @c m1, m2 of @c S::Member, the value
 *          @c Op(ι(m1), ι(m2)) lies in the image of @c ι (i.e.\ is
 *          itself representable as a Member of @c S).  Cannot be
 *          checked at compile time in general — opt-in.
 *
 *          Universal-algebra reference: Burris-Sankappanavar §II.5.
 */
export template <typename S, typename A, typename Op>
inline constexpr bool is_closed_under_v = false;

/**
 * @concept IsSubalgebra
 * @brief Subobject @c S of @c A is a @b subalgebra under operation
 *        @c Op when it is closed under @c Op.
 *
 * @details The S leg of Birkhoff's HSP closure (#718 Slice 3),
 *          completing the triple with @c IsQuotientAlgebra (H, above
 *          in this partition) and @c IsProductAlgebra (P, also
 *          above).  Mirrors @c :cartesian's @c IsCongruence shape
 *          (Slice 0): structural shape (the @c IsSubobject witness +
 *          Op signature gate) plus opt-in closure trait.
 *
 *          Universal-algebra references: Burris-Sankappanavar §II.5
 *          (subalgebras) + §II.10 (the HSP closure operators).
 *
 * @tparam S The candidate subalgebra (a Subobject of @c A).
 * @tparam A The ambient algebra carrier.
 * @tparam Op The binary operation @c V @c × @c V @c → @c V that
 *            @c S must be closed under.
 */
export template <typename S, typename A, typename Op>
concept IsSubalgebra = IsSubobject<S, A> && is_closed_under_v<S, A, Op> &&
                       requires(const A& a, const Op& op) {
                         { op(a, a) } -> std::convertible_to<A>;
                       };

}  // namespace dedekind::category
