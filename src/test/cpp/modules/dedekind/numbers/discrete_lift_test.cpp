/** @file test/cpp/modules/dedekind/numbers/discrete_lift_test.cpp
 *
 * Long-range test for @c discrete_lift_t<S> from @c category:etcs (#572).
 * Exercises the lift on @c IsSet-witnessing carriers built from the
 * project's concrete numeric tower (@c int, @c unsigned, @c Rational,
 * @c Real) — illustrates that the textbook @c Set @c ↪ @c Cat
 * embedding fires at every rung of the carrier ladder, not only on
 * primitive int.  The short-range test that pins the lift on
 * primitive ambients lives in @c category/discrete_lift_test.cpp.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::numbers;

TEST_CASE("discrete_lift_t fires across the numeric tower",
          "[numbers][category][etcs][discrete][lift][tower]") {
  // Build IsSet-witnessing carriers at concrete tower positions.
  // Each predicate selects a non-trivial subobject so the lift's
  // subobject-preservation behaviour is observable.
  const auto naturals_under_10 =
      ambient_set<unsigned>([](const unsigned& n) { return n < 10u; });
  const auto integers_in_range =
      ambient_set<int>([](const int& n) { return n >= -5 && n <= 5; });
  const auto positive_rationals = ambient_set<Rational<default_integer>>(
      [](const Rational<default_integer>& q) { return q.num() > 0; });

  using NatSubset = decltype(naturals_under_10);
  using IntSubset = decltype(integers_in_range);
  using RationalSubset = decltype(positive_rationals);

  // Each carrier is a bona fide IsSet over its tower-rung Ambient.
  STATIC_CHECK(IsSet<NatSubset>);
  STATIC_CHECK(IsSet<IntSubset>);
  STATIC_CHECK(IsSet<RationalSubset>);

  // The lift fires at every tower rung; the textbook Set ↪ Cat
  // embedding is mechanically observable per concrete carrier.
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<NatSubset>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<IntSubset>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<RationalSubset>>);

  // IsCategory falls out of IsDiscreteCategory; pinned for clarity.
  STATIC_CHECK(IsCategory<discrete_lift_t<NatSubset>>);
  STATIC_CHECK(IsCategory<discrete_lift_t<IntSubset>>);
  STATIC_CHECK(IsCategory<discrete_lift_t<RationalSubset>>);
}

TEST_CASE("discrete_lift_t distinguishes refined subobjects of ℚ",
          "[numbers][category][etcs][discrete][lift][rational]") {
  // Two distinct subobjects of ℚ — the strictly-positive rationals
  // and the rationals strictly between 0 and 1.  Both share the
  // same Ambient (Rational<default_integer>), but the predicates
  // pick out different subobjects; the lift should produce
  // distinct discrete categories (the textbook
  // Disc(ℚ_{>0}) /= Disc(ℚ_{(0,1)}) reading).
  const auto positive = ambient_set<Rational<default_integer>>(
      [](const Rational<default_integer>& q) { return q.num() > 0; });
  const auto open_unit_interval = ambient_set<Rational<default_integer>>(
      [](const Rational<default_integer>& q) {
        return q.num() > 0 && q.num() < q.den();
      });

  using S1 = decltype(positive);
  using S2 = decltype(open_unit_interval);

  // Same ambient, distinct types.
  STATIC_CHECK(std::same_as<typename S1::Ambient, Rational<default_integer>>);
  STATIC_CHECK(std::same_as<typename S2::Ambient, Rational<default_integer>>);
  STATIC_CHECK_FALSE(std::same_as<S1, S2>);

  // The lift produces distinct discrete categories — subobject
  // information is preserved through Set ↪ Cat.
  STATIC_CHECK_FALSE(std::same_as<discrete_lift_t<S1>, discrete_lift_t<S2>>);

  // Both still satisfy IsDiscreteCategory; the distinction is in
  // identity, not concept-membership.
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<S1>>);
  STATIC_CHECK(IsDiscreteCategory<discrete_lift_t<S2>>);
}

TEST_CASE("discrete_lift_t adjunction witnesses fire across the numeric tower",
          "[numbers][category][etcs][discrete][lift][adjunction][tower]") {
  // The bona fide adjunction surface (HasAdjunctionShape /
  // IsAdjunction from category:adjunction) is type-level mechanical
  // for every IsSet-witnessing carrier from the numeric tower —
  // matching the short-range exhibit in category/discrete_lift_test.
  const auto naturals_under_10 =
      ambient_set<unsigned>([](const unsigned& n) { return n < 10u; });
  const auto integers_in_range =
      ambient_set<int>([](const int& n) { return n >= -5 && n <= 5; });
  const auto positive_rationals = ambient_set<Rational<default_integer>>(
      [](const Rational<default_integer>& q) { return q.num() > 0; });

  // disc_self_endofunctor_t / disc_self_unit_t (in :natural) take a
  // discrete category C, not the source IsSet directly.  Each tower
  // rung's lifted Disc(S) is the discrete-category arg.
  using DiscNat = discrete_lift_t<decltype(naturals_under_10)>;
  using DiscInt = discrete_lift_t<decltype(integers_in_range)>;
  using DiscRat = discrete_lift_t<decltype(positive_rationals)>;

  // Structural-shape witness fires at every tower rung.
  STATIC_CHECK(HasAdjunctionShape<disc_self_endofunctor_t<DiscNat>,
                                  disc_self_endofunctor_t<DiscNat>>);
  STATIC_CHECK(HasAdjunctionShape<disc_self_endofunctor_t<DiscInt>,
                                  disc_self_endofunctor_t<DiscInt>>);
  STATIC_CHECK(HasAdjunctionShape<disc_self_endofunctor_t<DiscRat>,
                                  disc_self_endofunctor_t<DiscRat>>);

  // Full IsAdjunction with identity-transformation unit/counit fires
  // on the full tower as well.
  STATIC_CHECK(IsAdjunction<disc_self_endofunctor_t<DiscNat>,
                            disc_self_endofunctor_t<DiscNat>,
                            disc_self_unit_t<DiscNat>,
                            disc_self_unit_t<DiscNat>>);
  STATIC_CHECK(IsAdjunction<disc_self_endofunctor_t<DiscRat>,
                            disc_self_endofunctor_t<DiscRat>,
                            disc_self_unit_t<DiscRat>,
                            disc_self_unit_t<DiscRat>>);
}

TEST_CASE("discrete_lift_t illustrates 'simple set vs. complex algebra' on 𝔹",
          "[numbers][category][etcs][discrete][lift][adjunction][boolean]") {
  // Pedagogical anchor for the textbook Free / Forgetful adjunction
  // intuition (Pierce §2.6 / Mac Lane Ch. IV; conversation framing
  // shared by the author): the @c bool species is the underlying
  // carrier for two structurally distinct upgrades --
  //
  //   * the @b simple view: 𝔹 as the 2-element set @c {false, true},
  //     which lifts to a discrete category with two objects via Disc;
  //   * the @b complex view: 𝔹 as the idempotent commutative semiring
  //     in @c numbers:boolean / @c algebra:boolean (addition = OR,
  //     multiplication = AND, identities = false / true).
  //
  // The Disc ⊣ U adjunction (this PR's exhibit) lives over the simple
  // view and is mechanically witnessed below.  The Free-Boolean-
  // Semiring ⊣ U-to-Set adjunction over the complex view is a sister
  // carrier-side adjunction listed in adjunction.cppm; the project
  // does not yet wire it as a type-level HasAdjunctionShape exhibit.
  // The two views are independent "upgrades" of the same bool
  // ambient: Disc only sees the cardinality / element identity; the
  // semiring upgrade sees the algebraic operations.
  //
  // FIXME(#586): the canonical 𝔹 carrier @c FiniteBooleanSet<L>
  // does not directly satisfy @c IsSet (no @c Ambient member), so we
  // detour through @c ambient_set<bool>(...) here.  Issue #586 tracks
  // dog-fooding the canonical carriers on the @c IsSet aggregator so
  // tests can write @c discrete_lift_t<FiniteBooleanSet<L>> directly.
  const auto bool_set =
      ambient_set<bool>([](const bool&) { return true; });  // 𝔹 = {0, 1}
  using BoolSet = decltype(bool_set);

  // Simple-view structural facts.
  STATIC_CHECK(IsSet<BoolSet>);
  STATIC_CHECK(std::same_as<typename BoolSet::Ambient, bool>);

  // Disc(𝔹) is the discrete category on the 2-element ambient.
  using DiscB = discrete_lift_t<BoolSet>;
  STATIC_CHECK(IsDiscreteCategory<DiscB>);
  STATIC_CHECK(std::same_as<typename DiscB::Species, BoolSet>);

  // Bona fide adjunction surface fires on Disc(𝔹) -- same machinery
  // as for the int / Rational examples above.  This is the
  // discrete-restriction encoding of the meta-categorical Disc ⊣ U.
  using DiscFB = disc_self_endofunctor_t<DiscB>;
  using UnitTB = disc_self_unit_t<DiscB>;
  STATIC_CHECK(IsFunctor<DiscFB>);
  STATIC_CHECK(IsNaturalTransformation<UnitTB, DiscFB, DiscFB>);
  STATIC_CHECK(HasAdjunctionShape<DiscFB, DiscFB>);
  STATIC_CHECK(IsAdjunction<DiscFB, DiscFB, UnitTB, UnitTB>);

  // Operational unit / counit witness: per the textbook, the unit
  // η_S sends each "object" of S to its embedding in U(F(S)); for
  // the discrete-restriction encoding F = U = Id and η is the
  // identity natural transformation, so η(c) = id_c.
  //
  // A note on object granularity: in the project's encoding,
  // discrete_lift_t<S> = DiscreteCategory<S>, so the @b objects of
  // Disc(BoolSet) are values of type @c BoolSet (the subobject), not
  // values of type @c bool (the elements).  This is the price of
  // preserving subobject identity through the lift (the original #583
  // Copilot finding): @c Disc({false,true}) and @c Disc(𝔹) are
  // distinct types, but the trade-off is that "objects" lift to the
  // subobject-as-singleton level rather than to the element level.
  // The textbook "objects = elements" reading would require lifting
  // to @c DiscreteCategory<S::Member> instead, which collapses
  // distinct subobjects of the same ambient.  Pinning at the
  // subobject-value level below.
  const auto adjunction =
      make_adjunction(DiscFB{}, DiscFB{}, UnitTB{}, UnitTB{});
  STATIC_CHECK(IsArrow<decltype(adjunction.unit(bool_set))>);
  STATIC_CHECK(IsArrow<decltype(adjunction.counit(bool_set))>);
}
