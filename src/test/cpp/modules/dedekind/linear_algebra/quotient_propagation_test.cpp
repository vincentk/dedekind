// Tests for the HSP H/P + free-algebra trait propagation
// (dedekind.algebra:quotient + :free).  The propagation rules lift
// is_associative / is_commutative / is_distributive_v / is_periodic /
// is_idempotent / is_saturating from a base algebra to its declared
// HSP image.  The static_assert witnesses in the main partition
// already pin the canonical instances; this file adds Catch2-visible
// coverage for the propagation behaviour and the absence of
// over-firing.
//
// Issues #498 (Algebraic Tower) / #499 (NEW-A trait registry).

#include <catch2/catch_test_macros.hpp>
#include <functional>

import dedekind.algebra;
import dedekind.analysis; // Dual<F>
import dedekind.category;
import dedekind.linear_algebra; // Vec2V<T>
import dedekind.numbers;        // Rational, Complex, default_integer

namespace {

using dedekind::algebra::RigPolynomial;
using dedekind::analysis::Dual;
using dedekind::category::is_associative_v;
using dedekind::category::is_commutative_v;
using dedekind::category::is_periodic_v;
using dedekind::category::is_total_v;
using dedekind::category::IsFreeAlgebra;
using dedekind::category::IsProductAlgebra;
using dedekind::category::IsQuotientAlgebra;
using dedekind::linear_algebra::Vec2V;
using dedekind::numbers::Complex;
using dedekind::numbers::default_integer;
using dedekind::numbers::Rational;

using Q = Rational<default_integer>;
using CQ = Complex<Q>;
using DQ = Dual<Q>;
using V2u = Vec2V<unsigned int>;
using PQ = RigPolynomial<Q>;

}  // namespace

TEST_CASE("HSP H: quotient_algebra_base detection",
          "[algebra][quotient][hsp]") {
  STATIC_CHECK(IsQuotientAlgebra<Q>);
  STATIC_CHECK(IsQuotientAlgebra<CQ>);
  STATIC_CHECK(IsQuotientAlgebra<DQ>);
  STATIC_CHECK_FALSE(IsQuotientAlgebra<unsigned int>);  // not declared
  STATIC_CHECK_FALSE(IsQuotientAlgebra<int>);
}

TEST_CASE("HSP P: product_algebra_base detection", "[algebra][quotient][hsp]") {
  STATIC_CHECK(IsProductAlgebra<V2u>);
  STATIC_CHECK_FALSE(IsProductAlgebra<unsigned int>);
  STATIC_CHECK_FALSE(IsProductAlgebra<Q>);  // Rational is H, not P
}

TEST_CASE("HSP F: free_algebra_base detection", "[algebra][free][hsp]") {
  STATIC_CHECK(IsFreeAlgebra<PQ>);
  STATIC_CHECK_FALSE(IsFreeAlgebra<Q>);  // Q is H, not F
  STATIC_CHECK_FALSE(IsFreeAlgebra<unsigned int>);
}

TEST_CASE("HSP propagation: associativity lifts to Q from base",
          "[algebra][quotient][propagation]") {
  // Base default_integer (Z1) is associative under +/* via the species
  // specs in cardinality.cppm; the H propagation lifts to Rational<I>.
  STATIC_CHECK(is_associative_v<Q, std::plus<Q>>);
  STATIC_CHECK(is_associative_v<Q, std::multiplies<Q>>);
  // Composes through the second H (Cplx, Dual).
  STATIC_CHECK(is_associative_v<CQ, std::plus<CQ>>);
  STATIC_CHECK(is_associative_v<DQ, std::plus<DQ>>);
}

TEST_CASE("HSP propagation: commutativity lifts componentwise (P)",
          "[algebra][quotient][propagation]") {
  // unsigned int is commutative under +/* (modular arithmetic);
  // P propagation lifts to Vec2V<unsigned int>.
  STATIC_CHECK(is_commutative_v<V2u, std::plus<V2u>>);
  // Q under + is commutative (lifted via H from default_integer).
  STATIC_CHECK(is_commutative_v<Q, std::plus<Q>>);
}

TEST_CASE("HSP propagation: IsTotal certificate lifts via the right path",
          "[algebra][quotient][propagation]") {
  // Post-ℚ-retarget: default_integer is now SignedCardinality
  // (saturating), so is_periodic no longer applies on Rational<I>; the
  // IsTotal disjunction in :species closes via is_saturating instead.
  STATIC_CHECK(is_total_v<Q, std::plus<Q>>);
  STATIC_CHECK(is_total_v<Q, std::multiplies<Q>>);
  // unsigned int certifies IsTotal via is_periodic; P propagation
  // lifts it to Vec2V<unsigned int>.
  STATIC_CHECK(is_total_v<V2u, std::plus<V2u>>);
}

TEST_CASE("HSP propagation: no over-firing on undeclared carriers",
          "[algebra][quotient][propagation][negative]") {
  // signed int does NOT satisfy is_associative under + (signed-overflow
  // UB), and the propagation must not invent it.
  STATIC_CHECK_FALSE(is_associative_v<int, std::plus<int>>);
  // Also does not falsely fire on plain primitives that aren't declared
  // as quotient / product / free algebras.
  STATIC_CHECK_FALSE(IsQuotientAlgebra<int>);
  STATIC_CHECK_FALSE(IsProductAlgebra<int>);
  STATIC_CHECK_FALSE(IsFreeAlgebra<int>);
}

TEST_CASE("HSP carrier is_module_v witnesses fire",
          "[algebra][quotient][module]") {
  STATIC_CHECK(dedekind::algebra::is_module_v<Q, default_integer>);
  STATIC_CHECK(dedekind::algebra::is_module_v<CQ, Q>);
  STATIC_CHECK(dedekind::algebra::is_module_v<DQ, Q>);
  STATIC_CHECK(dedekind::algebra::is_module_v<V2u, unsigned int>);
}
