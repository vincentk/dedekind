/** @file test/cpp/modules/dedekind/numbers/rational_as_product_test.cpp
 *
 *  Pilot witness tests for the #573 product-as-parthood seams on the
 *  canonical Rational<I> carrier (slice 4 / #646).
 *
 *  Two halves:
 *
 *    - @c SetAsProduct: the @e set-level witness is in main code (a
 *      static_assert at the ℚ definition site in rational.cppm, since
 *      ℚ = UniversalSet<Rational<default_integer>> already walks
 *      IsSetObject without adapters).
 *    - @c AlgebraAsProduct: the @e algebra-level witness lives here in
 *      the test fork, via a thin @c rational_view<I> wrapper that
 *      exposes Rational<I> through the HasCarrier mereology arms
 *      (predicate-style part-of + arrow_drill_down).  Same pattern as
 *      slice 3's @c BoolAlgebraView (#651).
 */
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <type_traits>

import dedekind.algebra;
import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::algebra;
using namespace dedekind::category;
using namespace dedekind::numbers;

// File-scope thin Rational view wrapper.  Real-shaped (not mock-shaped):
// wraps a single Rational<I> value and exposes both mereology arms
// HasCarrier asks for --- predicate-style part-of (whole(part) iff
// equal) and operator->() drill-down to recover the wrapped Rational
// through arrow_drill_down.  Same shape as
// _has_carrier_witness::algebra_view<T> in :algebra:universal, scoped
// here so the test does not poke at the main module's internal
// namespace.
namespace rational_as_product_test {
template <typename I>
struct RationalView {
  Rational<I> value;
  constexpr bool operator()(const Rational<I>& part) const noexcept {
    return part == value;
  }
  constexpr const Rational<I>* operator->() const noexcept { return &value; }
  friend constexpr bool operator==(const RationalView&,
                                   const RationalView&) = default;
};
}  // namespace rational_as_product_test

TEST_CASE(
    "Numbers: AlgebraAsProduct pilot witness on Rational<I> (#573 slice 4)",
    "[numbers][rational][algebra][product][parthood]") {
  using I = default_integer;
  using View = rational_as_product_test::RationalView<I>;

  // ℚ is the universal set object over Rational<I>; its Ambient matches
  // the carrier type the algebra-side HasCarrier reads.
  using QuniverseT = std::remove_cvref_t<decltype(ℚ)>;

  SECTION("AlgebraAsProduct: Rational view walks the (Set, Operations) pair") {
    // The two field operations are the standard textbook ones, picked at
    // the carrier-type level so HasCarrier's IsAlgebra arm closes.
    STATIC_CHECK(AlgebraAsProduct<View, QuniverseT, std::plus<Rational<I>>,
                                  std::multiplies<Rational<I>>>);
  }

  SECTION("closure tier (no Ops): set-level shape without operation closure") {
    // With no Ops, AlgebraAsProduct reduces to its closure-tier shape:
    // IsSetObject<ℚ-type, Rational<I>> still holds AND HasCarrier's
    // no-Ops form (std::regular + IsPartOfRelation + arrow_drill_down)
    // still holds on the View.  Only the per-operation closure
    // requirements simplify away.
    STATIC_CHECK(AlgebraAsProduct<View, QuniverseT>);
  }
}
