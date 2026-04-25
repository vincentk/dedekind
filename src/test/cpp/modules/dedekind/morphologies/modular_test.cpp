/** @file dedekind/morphologies/modular_test.cpp
 *
 * Tests for @c Modular<N> after its relocation from
 * @c dedekind.category:species (and the transient move
 * to @c dedekind.morphologies:cyclic).
 *
 * Exercises the dual nature of the carrier: it satisfies the
 * operational @c morphologies::IsCyclic shape concept (via
 * @c Domain / @c generator() / @c successor() member API) AND the
 * categorical @c category::IsCyclicGroup<T, std::plus<T>> axiom-
 * bearing concept.  These two concepts are kept complementary in
 * the project's strict / operational vocabulary pattern, and
 * @c Modular<N> is the canonical carrier that bridges them.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <functional>

import dedekind.algebra;
import dedekind.category;
import dedekind.morphologies;

using dedekind::algebra::is_galois_field_v;
using dedekind::category::cyclic_order_v;
using dedekind::category::IsCyclicGroup;
using dedekind::morphologies::IsCyclic;
using dedekind::morphologies::Modular;

TEST_CASE("Modular<N> — operational IsCyclic shape (morphologies)",
          "[morphologies][modular][cyclic]") {
  using M256 = Modular<256>;
  using M17 = Modular<17>;

  // Duck-typed: Domain alias, generator(), successor() all present.
  STATIC_CHECK(IsCyclic<M256>);
  STATIC_CHECK(IsCyclic<M17>);

  // Generator is 1.
  STATIC_CHECK(M256::generator() == M256{1});
  STATIC_CHECK(M17::generator() == M17{1});

  // Successor walks the chain modulo N.
  STATIC_CHECK(M17::successor(M17{16}) == M17{0});  // 16 + 1 ≡ 0 (mod 17)
  STATIC_CHECK(M17::successor(M17{0}) == M17{1});
}

TEST_CASE("Modular<N> — axiomatic IsCyclicGroup (category)",
          "[morphologies][modular][cyclic-group]") {
  using M256 = Modular<256>;
  using M17 = Modular<17>;

  STATIC_CHECK(IsCyclicGroup<M256, std::plus<M256>>);
  STATIC_CHECK(cyclic_order_v<M256, std::plus<M256>> == 256);

  STATIC_CHECK(IsCyclicGroup<M17, std::plus<M17>>);
  STATIC_CHECK(cyclic_order_v<M17, std::plus<M17>> == 17);
}

TEST_CASE("Modular<N> — arithmetic round-trip", "[morphologies][modular]") {
  using M = Modular<7>;
  constexpr M a{3};
  constexpr M b{5};
  STATIC_CHECK((a + b) == M{1});  // 3 + 5 ≡ 1 (mod 7)
  STATIC_CHECK((a * b) == M{1});  // 3 * 5 ≡ 1 (mod 7)
}

TEST_CASE(
    "Modular<N> — hand-written SpeciesTraits not broadened by Registration",
    "[morphologies][modular][registration]") {
  // Regression for #382/#385.  `SpeciesTraits<Modular<N>>` in
  // `morphologies:cyclic` has Op-agnostic member templates
  // (`template <typename Op> static constexpr bool is_associative_v
  // = true;`) with pre-Registration semantics.  Without marker
  // gating in `algebra:registration`, the discovery would pick these
  // up and claim `is_associative<Modular<N>, Op>` = true for any Op
  // (e.g.\ `std::modulus` --- which isn't associative).  The marker
  // gate prevents that: `SpeciesTraits<Modular<N>>` does not carry
  // the `dedekind_registration_tag`, so the discovery specs don't
  // apply.  Modular<N>'s free-standing specialisations (for the
  // specific `std::plus<Modular<N>>` / `std::multiplies<Modular<N>>`
  // Ops it genuinely supports) continue to govern.
  using M = Modular<256>;

  // Positive: the hand-written specialisations for the canonical
  // (+, *) ops still hold.
  STATIC_CHECK(dedekind::category::is_associative_v<M, std::plus<M>>);
  STATIC_CHECK(dedekind::category::is_associative_v<M, std::multiplies<M>>);

  // Negative: the discovery did NOT accidentally claim Modular<N>
  // is a Galois field under (+, *).  Modular<256> is a commutative
  // ring but not a field (256 = 2^8 isn't a prime, and even prime-
  // power orders need polynomial-quotient construction, not
  // integer modular arithmetic).
  STATIC_CHECK(!is_galois_field_v<M, std::plus<M>, std::multiplies<M>>);
}
