/**
 * @file dedekind/linear_algebra/basis.cppm
 * @partition :basis
 * @brief Free-module / basis / endomorphism-ring trait registry.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section basis__Scope
 *
 * The "rank + basis" structural metadata that distinguishes free
 * modules from arbitrary modules — and the endomorphism-ring identity
 * @c End_R(V) @c = @c M_n(R) on rank-@c n free modules — lives here.
 * These claims carry @b structural payload (a rank @c N, an identity
 * between two named carriers) that no concept on its own derives from
 * the operator surface, so they remain opt-in trait variables rather
 * than concept-based defaults.
 *
 * Sister anchor in @c algebra:modules: @c is_module_v<M, R>, which
 * @b is concept-based (it fires whenever @c IsModule<M, R> holds, no
 * carrier-side opt-in needed).  The split is the right one because
 *
 *  - "M is a module over R" is purely an algebraic-axioms claim;
 *  - "M is a free module of rank N over R" pins a specific @c N as
 *    metadata, and "A is the endomorphism ring of V" pins a specific
 *    @c (A, V) pair as a structural identity — neither is recoverable
 *    from the operator surface alone.
 *
 * Issues #498 (Algebraic Tower) / #499 (NEW-A trait registry).
 */
module;

#include <cstddef>

export module dedekind.linear_algebra:basis;

import dedekind.algebra; // IsModule precondition gate.

namespace dedekind::linear_algebra {

/**
 * @brief @c is_free_module_v<M, R, N>: declare that @c M is a free
 *        module of rank @c N over @c R.
 *
 * @details Default @c false; specialised by carrier sites at the rank
 * payload they own (e.g.\ @c Vec2V<T> over @c T at rank 2 in
 * @c :vec2).  Specialisations are gated on
 * @c dedekind::algebra::IsModule<M, R> so the algebraic precondition
 * (R is a ring, M is a module over R) holds before the rank claim is
 * recorded — a free module is a module with a chosen basis, and the
 * module structure must already be present.
 */
export template <typename M, typename R, std::size_t N>
inline constexpr bool is_free_module_v = false;

/**
 * @brief @c is_endomorphism_ring_v<A, V>: declare that @c A is the
 *        endomorphism ring of the module @c V, i.e.\
 *        @c A @c = @c End_R(V) for the appropriate scalar @c R.
 *
 * @details Default @c false; specialised by carrier sites that own
 * the ring–module pair (e.g.\ @c Matrix2x2V<T> over @c Vec2V<T> in
 * @c :mat2x2).  Pins the internal-hom of the module category
 * mechanically; downstream Figure~1 invariant tests can read this
 * trait rather than reconstructing the equality.
 */
export template <typename A, typename V>
inline constexpr bool is_endomorphism_ring_v = false;

}  // namespace dedekind::linear_algebra
