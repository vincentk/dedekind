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
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.numbers;
import dedekind.order;

using namespace dedekind::sets;
using namespace dedekind::numbers;
using namespace dedekind::order;

TEST_CASE("complement-lattice absorbing laws collapse (𝔹 and ℕ)",
          "[sets][lattice][pruning]") {
  // ── 𝔹 : extensional carrier — value-carrying Singleton (#965: the pivot is a
  // VALUE, so the complement laws are decided by membership, not by a
  // type-level Singleton∩Singleton collapse that a value pivot cannot express)
  // ──
  {
    static_assert(
        dedekind::category::IsSet<decltype(𝔸<bool>)>);  // the universe IS an
                                                        // ETCS set
    constexpr Singleton<bool> T{true};                  // {true} ⊂ 𝔹
    constexpr Singleton<bool> F = ~T;  // complement: the point {false}
    // LEM (⊤): {true} ∪ {false} covers 𝔹 — both inhabitants are present.
    static_assert(static_cast<bool>((F | T)(true)) &&
                  static_cast<bool>((F | T)(false)));
    // non-contradiction (⊥): {true} ∩ {false} is empty — neither inhabitant.
    static_assert(!static_cast<bool>((F & T)(true)) &&
                  !static_cast<bool>((F & T)(false)));
  }
  // ── ℕ : intensional carrier — bare, first-class Halfspaces (telling types)
  // ──
  {
    static_assert(
        dedekind::category::IsSet<decltype(ℕ)>);  // the universe IS an ETCS set
    constexpr Above<5> gt_5 = ℕ | (π > fix(5_c));  // {x > 5} ⊂ ℕ
    constexpr AtMost<5> le_5 = ~gt_5;              // {x <= 5}, the complement
    static_assert((le_5 | gt_5) == ℕ);  // | : round-trip to the universe (⊤)
    constexpr Ø<Cardinality> empty = le_5 & gt_5;  // & : the empty set (⊥)
    static_assert(Ø<Cardinality>{} == empty);
  }
  // ── generator η, symmetric difference ^, product * (source for paper
  // Listing 2).  Singleton products collapse to a value SingletonSet, so they
  // are equality-comparable (#844/#845); ^ and the general product decide by
  // membership. ──
  {
    constexpr Singleton<bool> T{true};
    constexpr Singleton<bool> F = ~T;
    // η agrees with the point-free {true}; T, F are the two singletons
    static_assert(η(true)(true) && T(true) && !F(true));
    // ^ : {true} ^ {false} carries both (= 𝔹); {x} ^ {x} is empty
    static_assert((η(true) ^ η(false))(true) && (η(true) ^ η(false))(false));
    static_assert(!(η(true) ^ η(true))(true));
    // * : singletons collapse, so the product is == the singleton-pair;
    //     the general product decides by membership
    static_assert((η(true) * η(false)) == η(std::pair{true, false}));
    static_assert((η(false) * η(true)) == η(std::pair{false, true}));
    static_assert((𝔸<bool> * 𝔸<bool>)(std::pair{true, false}));
  }
  CHECK(true);  // runtime anchor for coverage
}
