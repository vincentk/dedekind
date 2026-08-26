/** @file test/cpp/modules/dedekind/analysis/pruning_lattice_laws_test.cpp
 *
 * The two absorbing laws of the complement lattice, witnessed as compile-time
 * collapses on an EXTENSIONAL carrier (𝔹, whose two values enumerate) and an
 * INTENSIONAL one (ℕ, infinite and predicate-only).  Source for the §3 Listing
 * (lst:pruning): from the universe, @c operator- reaches ¬B; ∪ B round-trips
 * back to the universe (a ∪ ¬a = ⊤), while ∩ B collapses to Ø (a ∩ ¬a = ⊥).
 *
 * Lives in the analysis layer (downstream of order) because the ℕ subset is a
 * halfspace.  NB: the empty set is spelled by its carrier (Ø<Cardinality> /
 * Ø<bool>) here; the ambient spelling Ø<ℕ> / Ø<𝔹> of the paper Listing is
 * pending the empty-set-on-ambient reform.  The algebra below is what the
 * reform must preserve.
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
  // ── ℕ : intensional carrier (infinite, predicate-only) ──
  {
    constexpr auto B = Set{in<ℕ> | in<ℕ> > bound<5>};  // {x > 5} ⊂ ℕ
    constexpr auto notB = ℕ - B;                       // step out: {x <= 5}
    static_assert((notB | B) == ℕ);  // | B: round-trip to the universe
    constexpr Ø<Cardinality> empty =
        notB & B;  // & B: to the empty set, by TYPE
    static_assert(Ø<Cardinality>{} == empty);
  }
  // ── 𝔹 : extensional carrier (its two values enumerate) ──
  {
    constexpr auto 𝔹 = Ω<bool>;
    constexpr auto B = Set{in<𝔹> | in<𝔹>};  // {true} ⊂ 𝔹
    constexpr auto notB = 𝔹 - B;            // step out: {false}
    static_assert((notB | B) == 𝔹);         // | B: round-trip to the universe
    constexpr Ø<bool> empty = notB & B;     // & B: to the empty set, by TYPE
    static_assert(Ø<bool>{} == empty);
  }
  CHECK(true);  // runtime anchor for coverage
}
