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

TEST_CASE("Modular<N> — Peano successor / generator coherence (#388)",
          "[morphologies][modular][peano][successor][generator]") {
  // The carrier-level T::successor(x) member API and the algebraic
  // Peano successor S(x) = x + 1 must agree pointwise.  This is
  // pinned at the type level in cyclic.cppm; the runtime walk here
  // exercises the same coherence at every index, including the
  // wrap-around at N-1 → 0 that the algebraic identity needs to
  // respect.
  using M7 = Modular<7>;

  // Successor walks the chain back to its starting point in exactly
  // N steps (this IS the cyclicity claim).
  M7 cursor = M7::generator();  // = M7{1}
  CHECK(cursor == M7{1});
  for (int i = 0; i < 7; ++i) {
    cursor = M7::successor(cursor);
  }
  // After 7 successor applications starting from 1, we should be
  // back at 1: 1 → 2 → 3 → 4 → 5 → 6 → 0 → 1.
  CHECK(cursor == M7{1});

  // Pointwise: T::successor(x) == x + 1 across every residue.
  for (int k = 0; k < 7; ++k) {
    const M7 x{k};
    CHECK(M7::successor(x) == (x + M7{1}));
  }

  // Generator + successor enumerate every residue (the orbit
  // covers Z/7Z, since 1 is a generator of Z/7Z under +).
  bool seen[7] = {false};
  M7 walker = M7{0};
  for (int i = 0; i < 7; ++i) {
    seen[walker.value] = true;
    walker = M7::successor(walker);
  }
  for (int i = 0; i < 7; ++i) CHECK(seen[i]);
}
