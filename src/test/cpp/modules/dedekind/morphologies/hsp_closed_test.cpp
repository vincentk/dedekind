/** @file dedekind/algebra/hsp_closed_test.cpp
 *
 * Birkhoff's HSP theorem — typed witness at the propagation level
 * (#718 Slice 5, paper-§3 crown).
 *
 * Theorem (Birkhoff 1935; Burris-Sankappanavar §II.11): a class K of
 * algebras is a @b variety (equationally definable) iff K is closed
 * under H (homomorphic images / quotients), S (subalgebras), and P
 * (direct products).
 *
 * The dedekind codebase realises HSP closure as @b trait @b propagation:
 * each equationally-defined axiom (@c is_associative_v,
 * @c is_commutative_v, @c is_distributive_v, @c is_periodic,
 * @c is_idempotent, @c is_saturating) propagates uniformly from a
 * base algebra @c B to any @c Q with @c quotient_algebra_base<Q>
 * @c = @c B (H), to any @c S with @c subalgebra_base<S> @c = @c B
 * (S, added in this slice), or to any @c P with
 * @c product_algebra_base<P> @c = @c B (P).
 *
 * This exhibit pins the closure at the canonical cyclic-ring base
 * @c Modular<6>.  The variety @c V is "carriers with associative @c +
 * AND commutative @c +".  Synthetic H/S/P markers wire to the three
 * registries; the eight static_asserts witness that the variety is
 * HSP-closed at this base by direct propagation.
 *
 * The same propagation works for @b any concrete H/S/P-construction
 * over @c Modular<6> (or over any other base in @c V) — the synthetic
 * markers stand in as type-level placeholders for any such
 * construction.
 *
 * Coverage targets:
 *  - All eight @c static_assert legs (compile-time, via STATIC_CHECK).
 *  - Regression-of-propagation safety: if a propagation spec breaks,
 *    the corresponding leg honestly fails.
 */

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;

namespace dedekind::category::hsp_closed_witnesses {

/** @brief Synthetic type-level marker for a generic H-image of
 *         @c Modular<6>.  No operator overloads needed — the
 *         propagation operates purely on the trait registry. */
struct H_image_of_Z6 {};

/** @brief Synthetic marker for a generic subalgebra of @c Modular<6>. */
struct S_subalg_of_Z6 {};

/** @brief Synthetic marker for a generic direct product of @c Modular<6>. */
struct P_product_of_Z6 {};

}  // namespace dedekind::category::hsp_closed_witnesses

namespace dedekind::category {

template <>
struct quotient_algebra_base<hsp_closed_witnesses::H_image_of_Z6> {
  using type = dedekind::morphologies::Modular<6>;
};

template <>
struct subalgebra_base<hsp_closed_witnesses::S_subalg_of_Z6> {
  using type = dedekind::morphologies::Modular<6>;
};

template <>
struct product_algebra_base<hsp_closed_witnesses::P_product_of_Z6> {
  using type = dedekind::morphologies::Modular<6>;
};

}  // namespace dedekind::category

using namespace dedekind::category;
using dedekind::morphologies::Modular;
using Z6 = Modular<6>;

TEST_CASE(
    "algebra:hsp_closed — base Modular<6> in V (associative + commutative +)",
    "[algebra][quotient][HSP][birkhoff][base]") {
  /** @brief The variety V we exercise: carriers with both
   *         associative + and commutative +.  @c Modular<6> is the
   *         canonical cyclic-ring inhabitant. */
  STATIC_CHECK(is_associative_v<Z6, std::plus<Z6>>);
  STATIC_CHECK(is_commutative_v<Z6, std::plus<Z6>>);
}

TEST_CASE(
    "algebra:hsp_closed — H closure: associativity and commutativity "
    "propagate to quotients",
    "[algebra][quotient][HSP][birkhoff][H]") {
  /** @brief H leg of Birkhoff: @c quotient_algebra_base propagation
   *         lifts both axioms from the base @c Modular<6> to any
   *         @c Q registered as a quotient. */
  using H = hsp_closed_witnesses::H_image_of_Z6;
  STATIC_CHECK(is_associative_v<H, std::plus<H>>);
  STATIC_CHECK(is_commutative_v<H, std::plus<H>>);
}

TEST_CASE(
    "algebra:hsp_closed — S closure: associativity and commutativity "
    "propagate to subalgebras",
    "[algebra][quotient][HSP][birkhoff][S]") {
  /** @brief S leg of Birkhoff (new in #718 Slice 5):
   *         @c subalgebra_base propagation lifts both axioms from the
   *         base to any registered subalgebra. */
  using S = hsp_closed_witnesses::S_subalg_of_Z6;
  STATIC_CHECK(is_associative_v<S, std::plus<S>>);
  STATIC_CHECK(is_commutative_v<S, std::plus<S>>);
}

TEST_CASE(
    "algebra:hsp_closed — P closure: associativity and commutativity "
    "propagate componentwise to direct products",
    "[algebra][quotient][HSP][birkhoff][P]") {
  /** @brief P leg of Birkhoff: @c product_algebra_base propagation
   *         lifts both axioms componentwise from the base to any
   *         registered direct product. */
  using P = hsp_closed_witnesses::P_product_of_Z6;
  STATIC_CHECK(is_associative_v<P, std::plus<P>>);
  STATIC_CHECK(is_commutative_v<P, std::plus<P>>);
}

TEST_CASE(
    "algebra:hsp_closed — Birkhoff HSP theorem realised: the variety "
    "of associative-commutative + carriers is closed under H, S, P",
    "[algebra][quotient][HSP][birkhoff][crown]") {
  /** @brief The Birkhoff crown: all three HSP operations applied to
   *         a base in V yield carriers still in V.  Pinned at compile
   *         time via the propagation specs in :algebra::quotient. */
  using H = hsp_closed_witnesses::H_image_of_Z6;
  using S = hsp_closed_witnesses::S_subalg_of_Z6;
  using P = hsp_closed_witnesses::P_product_of_Z6;

  // The four-fold static_assert pinning Birkhoff: base + three HSP
  // legs, each carrier showing both axioms of V.
  STATIC_CHECK(is_associative_v<Z6, std::plus<Z6>>);  // base
  STATIC_CHECK(is_associative_v<H, std::plus<H>>);    // H
  STATIC_CHECK(is_associative_v<S, std::plus<S>>);    // S
  STATIC_CHECK(is_associative_v<P, std::plus<P>>);    // P
  STATIC_CHECK(is_commutative_v<Z6, std::plus<Z6>>);  // base
  STATIC_CHECK(is_commutative_v<H, std::plus<H>>);    // H
  STATIC_CHECK(is_commutative_v<S, std::plus<S>>);    // S
  STATIC_CHECK(is_commutative_v<P, std::plus<P>>);    // P
}

TEST_CASE(
    "algebra:hsp_closed — S propagation: distributivity + is_periodic "
    "also lift to subalgebras",
    "[algebra][quotient][HSP][birkhoff][S][completeness]") {
  /** @brief The S-propagation specs added in this slice cover six
   *         traits total: is_associative_v, is_commutative_v,
   *         is_distributive_v, is_saturating, is_periodic,
   *         is_idempotent.  The crown above only exercised the first
   *         two; this test additionally exercises is_distributive_v
   *         and is_periodic so silent regressions in those
   *         propagation specs surface at compile time.
   *
   *         Modular<6> is distributive over (*, +) and periodic
   *         under + by construction, so both legs fire on Z6 and
   *         propagate to S via subalgebra_base. */
  using S = hsp_closed_witnesses::S_subalg_of_Z6;

  // Distributivity propagation:
  STATIC_CHECK(is_distributive_v<Z6, std::multiplies<Z6>, std::plus<Z6>>);
  STATIC_CHECK(is_distributive_v<S, std::multiplies<S>, std::plus<S>>);

  // is_periodic propagation (struct trait; tests the struct-inheritance
  // path of the propagation spec, complementing the variable-template
  // paths covered above):
  STATIC_CHECK(is_periodic_v<Z6, std::plus<Z6>>);
  STATIC_CHECK(is_periodic_v<S, std::plus<S>>);
}
