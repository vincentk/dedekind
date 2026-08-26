/** @file test/cpp/modules/dedekind/analysis/pruning_lattice_laws_test.cpp
 *
 * The two absorbing laws of the complement lattice, witnessed as compile-time
 * collapses on an EXTENSIONAL carrier (𝔹, whose two values enumerate) and an
 * INTENSIONAL one (ℕ, infinite and predicate-only).  Source for the §3 Listing
 * (lst:pruning).  The collapse is STRUCTURAL on both carriers — a complement
 * pair P, ~P joins to the universe (a ∪ ¬a = ⊤) and meets to the empty set
 * (a ∩ ¬a = ⊥) — and the intermediate TYPES are telling: a static Singleton on
 * bool, a Halfspace on ℕ.  No element is ever enumerated.
 *
 * NB: the empty set is spelled by carrier (Ø<bool> / Ø<Cardinality>); the
 * ambient spelling Ø<𝔹> / Ø<ℕ> is pending the empty-set-on-ambient migration.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::sets;
using namespace dedekind::numbers;
using namespace dedekind::order;

TEST_CASE("complement-lattice absorbing laws collapse (𝔹 and ℕ)",
          "[sets][lattice][pruning]") {
  // ── 𝔹 : extensional carrier — the value is in the type (static Singleton) ──
  {
    constexpr Singleton<true> T{};      // {true} ⊂ 𝔹
    constexpr Singleton<false> F = ~T;  // complement: the other singleton
    static_assert((F | T) == Ω<bool>);  // | : round-trip to the universe (⊤)
    constexpr Ø<bool> empty = F & T;    // & : collapse to the empty set (⊥)
    static_assert(Ø<bool>{} == empty);
  }
  // ── ℕ : intensional carrier — bare, first-class Halfspaces (telling types)
  // ──
  {
    constexpr Above<5> gt_5 = in<ℕ> > bound<5>;  // {x > 5} ⊂ ℕ
    constexpr AtMost<5> le_5 = ~gt_5;            // {x <= 5}, the complement
    static_assert((le_5 | gt_5) == ℕ);  // | : round-trip to the universe (⊤)
    constexpr Ø<Cardinality> empty = le_5 & gt_5;  // & : the empty set (⊥)
    static_assert(Ø<Cardinality>{} == empty);
  }
  CHECK(true);  // runtime anchor for coverage
}
