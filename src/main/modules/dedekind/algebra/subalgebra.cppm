/**
 * @file dedekind/algebra/subalgebra.cppm
 * @partition :subalgebra
 * @brief @c IsSubalgebra<Sub, Super, Ops...> --- a @b signature-relative
 *        subalgebra: a subset of a carrier closed under a @b chosen reduct of
 *        operations, together with a declared inclusion homomorphism.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @details A subalgebra in the Birkhoff--Sankappanavar sense (BS 1981 §II.1)
 * is a subset of a carrier that is @b closed under the operations of a fixed
 * signature.  The signature is a first-class parameter here: a set can be a
 * subalgebra of @c Super under @b one reduct of operations and @b fail to be
 * one under a larger reduct.  This is exactly the honest statement one wants
 * when bridging a physical carrier to a platonic one --- e.g.\ @c safe_float
 * is a subalgebra of @f$\mathbb{R}@f$ under @f$\{\le,\ \mathrm{neg}\}@f$ (the
 * finite lattice reduct) but @b not under @f$\{+\}@f$ (rounding/overflow
 * leaves the finite subset); the exact bounded dyadics are a subalgebra under
 * @f$\{+, -, \times\}@f$ but @b not under @f$\div@f$.  The @b fracture ---
 * the first operation whose closure fails --- is the content, not a defect.
 *
 * @section subalgebra__Postulate_vs_proof
 * The inclusion @f$\iota: \mathrm{Sub} \hookrightarrow \mathrm{Super}@f$ must
 * preserve each operation of the signature (a homomorphism for that reduct).
 * Two tiers, mirroring @c IsHomomorphism (opt-in) vs @c IsRingHomomorphism
 * (structural, in @c :ring):
 *   - Between two @b reified carriers, @f$\iota@f$'s preservation laws are
 *     mechanically checkable (both sides are inhabited), so the claim can be
 *     a @b proof.
 *   - Into an @b uninhabited carrier such as @f$\mathbb{R}@f$ (which by the
 *     unconstructibility theorem carries no realisable operations to preserve),
 *     the claim is a @b postulate --- a documented modelling axiom.
 * @c IsSubalgebra is the opt-in gate carrying that honesty obligation via
 * @c is_subalgebra_v; it composes with the checkable per-signature
 * homomorphism concepts where those apply.
 *
 * @note "L'algèbre est généreuse: elle donne souvent plus qu'on ne lui
 *        demande."  Choosing a subalgebra is choosing a hypothesis; Birkhoff
 *        closure then hands back every law of that signature for free (the
 *        subalgebra dividend).  Cf.\ the d'Alembert line in @c :universal.
 *
 * @see Burris & Sankappanavar 1981 §II.1 (subalgebras); §II.9 (HSP).
 */
module;

#include <concepts>

export module dedekind.algebra:subalgebra;

import dedekind.category; // IsSet
import :universal;        // IsClosedAlgebra

namespace dedekind::algebra {
using namespace dedekind::category;

/**
 * @brief Opt-in honesty declaration that @c Sub embeds into @c Super as a
 *        subalgebra for the signature @c Ops... .
 *
 * @details The primary template is @c false: nothing is a subalgebra of
 * anything until declared.  A declaration
 * @code
 *   template <> inline constexpr bool
 *   is_subalgebra_v<QSet, RSet, std::plus<Q>, std::multiplies<Q>> = true;
 * @endcode
 * is the audit trail asserting that the inclusion arrow preserves each listed
 * operation.  For reified/reified pairs it should sit beside a mechanical
 * homomorphism witness; into an uninhabited @c Super it stands as the
 * documented modelling postulate.
 */
export template <typename Sub, typename Super, typename... Ops>
inline constexpr bool is_subalgebra_v = false;

/**
 * @concept IsSubalgebra
 * @brief @c Sub is a subalgebra of @c Super for the signature @c Ops... .
 *
 * @details Three conjuncts, phrased through the @c Ddk @c = @c Trsk @c ∩
 * @c Alg hierarchy (Listing 16 / @c :universal) rather than re-spelling it:
 *   - @c Sub @b is an algebra on a set for this signature ---
 *     @c IsAlgebraOnSet<Sub, Ops...>, i.e.\ @c IsSet<Sub> @b and @c Sub's
 *     carrier is closed under every @c Op (the "internally consistent" half,
 *     relative to the chosen reduct).  A subalgebra is first a genuine algebra.
 *   - @c Super is a set (@c IsSet).  It is @b not required to be an
 *     @c IsAlgebraOnSet for the signature: an uninhabited ambient such as
 *     @f$\mathbb{R}@f$ carries no @b realisable operations to close, so its
 *     algebra structure is postulated, not closed on the type.
 *   - the inclusion is @b declared to preserve the signature
 *     (@c is_subalgebra_v) --- the homomorphism obligation, checkable between
 *     reified carriers and postulated into an uninhabited one.
 *
 * @tparam Sub   The candidate subalgebra (an @c IsAlgebraOnSet for @c Ops).
 * @tparam Super The ambient set (an @c IsSet; its algebra may be postulated).
 * @tparam Ops   The signature: the reduct of operations closure is asked of.
 */
export template <typename Sub, typename Super, typename... Ops>
concept IsSubalgebra = IsAlgebraOnSet<Sub, Ops...> && IsSet<Super> &&
                       is_subalgebra_v<Sub, Super, Ops...>;

}  // namespace dedekind::algebra
