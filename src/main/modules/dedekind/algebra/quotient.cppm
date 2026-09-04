/**
 * @file dedekind/algebra/quotient.cppm
 * @partition :quotient
 * @brief HSP structure-preserving operations on algebras — H (quotient,
 *        with a relational congruence reading) + P (direct product) + S
 *        (subalgebra) carrier-side concepts and structural-trait
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

#include <concepts>  // std::same_as (the projection tie in IsCongruenceQuotient)
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

// --- H-leg, relationally: the quotient rides a CONGRUENCE (a relation) ------
//
// The propagation above rides @c quotient_algebra_base<Q>::type --- the
// base carrier, a bare type pointer.  This block adds the @b relational
// reading, symmetric with the S-leg's @c is_closed_under_v / @c IsSubalgebra
// (below): a quotient @c Q @c = @c V/R is witnessed by a @b congruence
// relation @c R on the carrier @c V --- an equivalence preserved by the
// operation, @c :cartesian's @c IsCongruence.  Declaring @c R makes the
// H-leg's Birkhoff justification @b a @b relation @b you @b can @b see,
// closing the asymmetry where H carried only a type pointer while S already
// carried a relation.
//
// @b Deliberately @b decoupled from @c IsQuotientAlgebra (the trait-
// propagation base).  The two are separate concerns: propagation lifts a
// base's traits to @c Q via @c quotient_algebra_base, whereas the congruence
// merely @b witnesses that @c Q @c = @c V/R.  A carrier may set its traits
// @b directly and still be a congruence quotient --- e.g.\ @c Modular<N> is
// total by wraparound while its integer carrier @c V is not, so it must
// @b not inherit @c V's (non-total) traits by propagation, yet @c Modular<N>
// @c = @c V/(≡ mod N) is a genuine congruence quotient.  Coupling the two
// would corrupt such a carrier's certification; keeping them apart is the
// honest factoring.

/** @brief @c quotient_congruence<Q>: carrier-side declaration of the
 *  congruence witnessing @c Q @c = @c V/R.  @c ::type is the relation @c R,
 *  @c ::carrier is the carrier @c V that @c R lives on (@c R is a homogeneous
 *  relation on @c V).  Mirrors the S-leg's @c subalgebra_base<S>. */
export template <typename Q>
struct quotient_congruence {};

/** @brief Convenience alias for @c quotient_congruence<Q>::type (the
 *  congruence relation @c R). */
export template <typename Q>
using quotient_congruence_t = typename quotient_congruence<Q>::type;

/** @brief Convenience alias for @c quotient_congruence<Q>::carrier (the
 *  carrier @c V that the congruence lives on). */
export template <typename Q>
using quotient_congruence_carrier_t = typename quotient_congruence<Q>::carrier;

/** @concept IsCongruenceQuotient
 *  @brief @c Q is a quotient of a carrier @c V by a @b declared @b congruence
 *         @c R for @c Op @b and reduces from @c V --- the relational reading of
 *         the H leg.
 *  @details Holds when
 *           (1) @c Q declares a @c quotient_congruence whose relation @c R is a
 *               genuine congruence on its carrier @c V (@c
 * IsCongruence<R,V,Op>: an equivalence preserved by @c Op), @b and (2) the
 * canonical projection @f$V \twoheadrightarrow Q@f$ exists ---
 *               @c Q is constructible from a carrier value (@c Q{v}) --- so
 *               @c Q is @b tied to @c V/R, not merely accompanied by a
 *               free-floating congruence.  A bare marker type with no reduction
 *               is rejected.
 *
 *           Clause (2) is the honest ceiling: a full quotient @b proof
 *           (@f$\forall a,b:\ \mathrm{proj}\,a=\mathrm{proj}\,b \iff R(a,b)@f$)
 *           is Rice-undecidable, so the concept certifies the @b structural tie
 *           (a congruence on @c V plus a reduction @c V\to Q), not the
 *           extensional identity.  The H-leg analogue of @c IsSubalgebra (S),
 *           and independent of @c IsQuotientAlgebra (see the note above): a
 *           carrier may certify its own traits directly yet still be witnessed
 *           relationally as @c V/R.
 *  @tparam Q  the quotient carrier (declares @c quotient_congruence, reduces
 *             from @c V).
 *  @tparam Op the carrier operation the congruence must respect (e.g.\
 *             @c std::plus).
 */
export template <typename Q, typename Op>
concept IsCongruenceQuotient =
    requires {
      typename quotient_congruence<Q>::type;
      typename quotient_congruence<Q>::carrier;
    } &&
    IsCongruence<quotient_congruence_t<Q>, quotient_congruence_carrier_t<Q>,
                 Op> &&
    requires(quotient_congruence_carrier_t<Q> v) {
      { Q{v} } -> std::same_as<Q>;  // the canonical projection V ->> Q
    };

// Self-contained witness that the relational H-leg plumbing composes.  @c
// IntByEquality is the identity quotient @c int/(=): @c std::equal_to is the
// diagonal (finest) congruence (@c :cartesian), and the projection @c int ->> Q
// is the identity (the ctor from @c int).  Being a real reducing type, not an
// empty marker, it satisfies clause (2).  A genuine non-trivial carrier
// (@c Modular<2^k> @c = @c unsigned/(≡ mod 2^k), @c N a power of two) declares
// its congruence downstream in @c morphologies:cyclic.
namespace detail_quotient {
struct IntByEquality {
  int value;  ///< int / (=) ≅ int; the projection V ->> Q is the identity.
  constexpr explicit IntByEquality(int v) : value(v) {}
};
}  // namespace detail_quotient

template <>
struct quotient_congruence<detail_quotient::IntByEquality> {
  using type = std::equal_to<int>;
  using carrier = int;
};

static_assert(
    IsCongruenceQuotient<detail_quotient::IntByEquality, std::plus<int>>,
    "H-leg relational reading: a quotient by a declared congruence that also "
    "reduces from its carrier (here int/(=), the diagonal congruence, reducing "
    "by the identity) is recognised as a congruence quotient.");

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

// ---------------------------------------------------------------------------
// subalgebra_base<S>::type — carrier-side registry mirroring
// quotient_algebra_base and product_algebra_base.  The propagation
// specs below lift the species traits (is_associative, is_commutative,
// is_distributive, …) from the ambient algebra A to its subalgebra S
// uniformly, completing the HSP closure of axioms on the trait registry.
// ---------------------------------------------------------------------------

/** @brief @c subalgebra_base<S>: carrier-side declaration that @c S
 *         is a subalgebra of some base algebra (the S operation in
 *         Birkhoff HSP).  Specialise the @c ::type member at the
 *         carrier-defining partition; the propagation specs below
 *         then lift the species-trait pins from base to @c S. */
export template <typename S>
struct subalgebra_base {};

/** @brief Convenience alias for @c subalgebra_base<S>::type. */
export template <typename S>
using subalgebra_base_t = typename subalgebra_base<S>::type;

/** @concept IsSubalgebraOf
 *  @brief @c S is declared as a subalgebra of some base algebra.
 *  @details Triggered by a specialisation of @c subalgebra_base<S>
 *           exposing a @c ::type member.  Sibling of
 *           @c IsQuotientAlgebra and @c IsProductAlgebra; all three
 *           are HSP operations that preserve the structural species
 *           traits of @c Base. */
export template <typename S>
concept IsSubalgebraOf = requires { typename subalgebra_base<S>::type; };

// --- S (subalgebra) propagation: structural traits lift from Base. ---------
//
// A subalgebra @c S of @c Base inherits the same axioms as @c Base
// for the corresponding operation, because @c S's operations are the
// restrictions of @c Base's operations.  Identical propagation
// pattern as the H (quotient_algebra_base) and P (product_algebra_base)
// sections above.

template <typename S>
  requires IsSubalgebraOf<S>
inline constexpr bool is_associative_v<S, std::plus<S>> =
    is_associative_v<subalgebra_base_t<S>, std::plus<subalgebra_base_t<S>>>;

template <typename S>
  requires IsSubalgebraOf<S>
inline constexpr bool is_associative_v<S, std::multiplies<S>> =
    is_associative_v<subalgebra_base_t<S>,
                     std::multiplies<subalgebra_base_t<S>>>;

template <typename S>
  requires IsSubalgebraOf<S>
inline constexpr bool is_commutative_v<S, std::plus<S>> =
    is_commutative_v<subalgebra_base_t<S>, std::plus<subalgebra_base_t<S>>>;

template <typename S>
  requires IsSubalgebraOf<S>
inline constexpr bool is_commutative_v<S, std::multiplies<S>> =
    is_commutative_v<subalgebra_base_t<S>,
                     std::multiplies<subalgebra_base_t<S>>>;

template <typename S>
  requires IsSubalgebraOf<S>
inline constexpr bool is_distributive_v<S, std::multiplies<S>, std::plus<S>> =
    is_distributive_v<subalgebra_base_t<S>,
                      std::multiplies<subalgebra_base_t<S>>,
                      std::plus<subalgebra_base_t<S>>>;

template <typename S>
  requires IsSubalgebraOf<S>
struct is_saturating<S, std::plus<S>>
    : is_saturating<subalgebra_base_t<S>, std::plus<subalgebra_base_t<S>>> {};

template <typename S>
  requires IsSubalgebraOf<S>
struct is_saturating<S, std::multiplies<S>>
    : is_saturating<subalgebra_base_t<S>,
                    std::multiplies<subalgebra_base_t<S>>> {};

template <typename S>
  requires IsSubalgebraOf<S>
struct is_periodic<S, std::plus<S>>
    : is_periodic<subalgebra_base_t<S>, std::plus<subalgebra_base_t<S>>> {};

template <typename S>
  requires IsSubalgebraOf<S>
struct is_periodic<S, std::multiplies<S>>
    : is_periodic<subalgebra_base_t<S>, std::multiplies<subalgebra_base_t<S>>> {
};

template <typename S>
  requires IsSubalgebraOf<S>
struct is_idempotent<S, std::plus<S>>
    : is_idempotent<subalgebra_base_t<S>, std::plus<subalgebra_base_t<S>>> {};

template <typename S>
  requires IsSubalgebraOf<S>
struct is_idempotent<S, std::multiplies<S>>
    : is_idempotent<subalgebra_base_t<S>,
                    std::multiplies<subalgebra_base_t<S>>> {};

}  // namespace dedekind::category

// The Birkhoff HSP-closed @b exhibit (typed crown) lives downstream in
// the test surface — it consumes both @c :algebra::quotient (the
// propagation specs above) and @c :morphologies::cyclic (for the
// concrete @c Modular<N> base).  See
// @c src/test/cpp/modules/dedekind/algebra/hsp_closed_test.cpp.
