/** @file dedekind/order/set_expression_exhibit_test.cpp
 *
 * THE EXHIBIT (epic #888, paper §3 + README): a non-trivial set expression
 * that the C++ type system COLLAPSES at compile time — the compile-time
 * set-algebra showcase.  Two overlapping halfspaces on ℤ,
 *
 *     A = { x | x < 5 }        B = { x | x > -5 }
 *
 * collapse in BOTH lattice directions, and the collapse is proven at
 * compile time (the result TYPE changes), not merely computed at runtime:
 *
 *     A & B  ⟶  the bounded open interval (-5, 5)   (halfspace ∧ ⟹ interval)
 *     A | B  ⟶  the universe 𝔸                       (opposing cover ⟹
 * universe)
 *
 * This is the STRUCTURAL (lattice-law) axis of the exhibit.  A second TEST_CASE
 * (added with #892) shows the reducer's residual behaviour: an irreducible
 * meet / join MATERIALIZES as a MeetSet / JoinSet carrying both operands,
 * absorption reads through that materialization, and the complement is a
 * certified involution (!!A ≡ A).  The decidability axis
 * (undecidable-in-general ⟶ decidable-on-a-bounded-subset, #860/#858/#861) and
 * the Python mirror (#886) are the sibling sub-items of #888.
 */

#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <type_traits>

import dedekind.category;
import dedekind.sets;
import dedekind.order;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

namespace set_expression_exhibit {

// A = { x < 5 } (downward, strict);  B = { x > -5 } (upward, strict).
using A = Halfspace<int, 5, Direction::Downward, Strictness::Strict, Boole>;
using B = Halfspace<int, -5, Direction::Upward, Strictness::Strict, Boole>;

// ── Compile-time collapse proof (the "wow"): the result TYPE is the reduced
//    form, not an unevaluated AndPredicate / OrPredicate. ──────────────────

// A ∧ B ⟹ the bounded interval (-5, 5).  structured_and is what Set::operator&
// routes through for a halfspace meet.
using MeetForm = std::decay_t<decltype(structured_and(A{}, B{}))>;
static_assert(MeetForm::lower_pivot == -5,
              "A & B collapsed to the interval with lower bound -5.");
static_assert(MeetForm::upper_pivot == 5,
              "A & B collapsed to the interval with upper bound 5.");
static_assert(MeetForm::lower_strictness == Strictness::Strict &&
                  MeetForm::upper_strictness == Strictness::Strict,
              "the open interval (-5, 5).");
// Nine integers -4..4 — decided at compile time.
static_assert(structured_and(A{}, B{}).size() == 9u,
              "|(-5, 5) ∩ ℤ| = 9, decided at compile time.");

// A ∨ B ⟹ the universe.  The two opposing halfspaces cover ℤ (they overlap on
// (-5, 5)), so the join collapses to UniversalSet — NOT an OrPredicate.
// structured_or's opposing overload wants (Upward, Downward) order.
using JoinForm = std::decay_t<decltype(structured_or(B{}, A{}))>;
static_assert(std::same_as<JoinForm, UniversalSet<int, Boole>>,
              "A | B collapsed to the universe 𝔸 (opposing halfspaces cover).");

}  // namespace set_expression_exhibit

// ── Membership: the collapsed sets classify exactly as the original
//    expression would, so the compile-time optimization is meaning-preserving.
//    All bona fide constexpr (Jlt) — STATIC_CHECK, decided at compile time.
TEST_CASE(
    "Exhibit: two overlapping halfspaces collapse (structural axis, #888)",
    "[order][sets][exhibit][collapse]") {
  using namespace set_expression_exhibit;
  constexpr Set<int, Boole, A> SA{A{}};
  constexpr Set<int, Boole, B> SB{B{}};

  SECTION("A & B is the open interval (-5, 5)") {
    constexpr auto meet = SA & SB;
    // The collapse IS the point: pin the reduced TYPE, not just membership —
    // else the exhibit would still pass if operator& stopped collapsing.
    STATIC_CHECK(std::same_as<std::decay_t<decltype(meet)>, MeetForm>);
    STATIC_CHECK(meet(0));
    STATIC_CHECK(meet(4));
    STATIC_CHECK(meet(-4));
    STATIC_CHECK_FALSE(meet(5));   // boundary excluded (strict)
    STATIC_CHECK_FALSE(meet(-5));  // boundary excluded (strict)
    STATIC_CHECK_FALSE(meet(100));
  }

  SECTION("A | B is the universe 𝔸 (covers ℤ)") {
    constexpr auto join = SA | SB;
    STATIC_CHECK(
        std::same_as<std::decay_t<decltype(join)>, UniversalSet<int, Boole>>);
    STATIC_CHECK(join(0));
    STATIC_CHECK(join(100));
    STATIC_CHECK(join(-100));
    STATIC_CHECK(join(5));
    STATIC_CHECK(join(-5));
  }

  // Complement laws (the lattice ⊥/⊤): the collapse also fires the boundary
  // reductions A & ~A → Ø and A | ~A → 𝔸 (~ = set complement; ~{x<5} = {x≥5}).
  SECTION(
      "A & ~A collapses to Ø (contradiction), A | ~A to 𝔸 (excluded middle)") {
    constexpr auto contradiction = SA & ~SA;
    STATIC_CHECK(
        std::same_as<std::decay_t<decltype(contradiction)>, Ø<int, Boole>>);
    STATIC_CHECK_FALSE(contradiction(0));
    STATIC_CHECK_FALSE(contradiction(4));
    STATIC_CHECK_FALSE(contradiction(5));
    constexpr auto excluded_middle = SA | ~SA;
    STATIC_CHECK(std::same_as<std::decay_t<decltype(excluded_middle)>,
                              UniversalSet<int, Boole>>);
    STATIC_CHECK(excluded_middle(0));
    STATIC_CHECK(excluded_middle(4));
    STATIC_CHECK(excluded_middle(5));
  }
}

// ── The #892 axis: when a meet / join does NOT collapse to a boundary or an
//    interval, it MATERIALIZES as a MeetSet / JoinSet that carries both operand
//    sets (the AST is the set).  The reducer still reads through that
//    materialization boundary for absorption, and the complement is a certified
//    involution.  These are the capabilities #892 added on top of the glb / lub
//    and boundary collapses above.
TEST_CASE(
    "Exhibit: irreducible meet/join materialize, absorption, involution (#892)",
    "[order][sets][exhibit][collapse][892]") {
  using namespace set_expression_exhibit;
  // Lo = {x ≥ 5}, Hi = {x ≤ 2}: a disjoint pair with a gap at 3, 4, so their
  // union has no boundary or interval normal form to collapse into.
  using Lo = Halfspace<int, 5, Direction::Upward, Strictness::NonStrict, Boole>;
  using Hi =
      Halfspace<int, 2, Direction::Downward, Strictness::NonStrict, Boole>;
  constexpr Set<int, Boole, Lo> SLo{Lo{}};
  constexpr Set<int, Boole, Hi> SHi{Hi{}};

  SECTION("an irreducible union materializes as a JoinSet carrying both sets") {
    constexpr auto uni = SLo | SHi;  // {x ≥ 5} ∪ {x ≤ 2}, a genuine gap at 3, 4
    STATIC_CHECK(
        std::same_as<std::decay_t<decltype(uni)>,
                     JoinSet<Set<int, Boole, Lo>, Set<int, Boole, Hi>>>);
    STATIC_CHECK(uni(7));        // in Lo
    STATIC_CHECK(uni(1));        // in Hi
    STATIC_CHECK_FALSE(uni(3));  // the gap
  }

  SECTION("an irreducible meet materializes as a MeetSet carrying both sets") {
    // Cap = {x < 10} is not one of the union's operands, so the meet does not
    // absorb; it materializes as a MeetSet wrapping Cap and the JoinSet.
    using Cap =
        Halfspace<int, 10, Direction::Downward, Strictness::Strict, Boole>;
    constexpr Set<int, Boole, Cap> SCap{Cap{}};
    constexpr auto met = SCap & (SLo | SHi);  // {x<10} ∩ ({x≥5} ∪ {x≤2})
    STATIC_CHECK(std::same_as<
                 std::decay_t<decltype(met)>,
                 MeetSet<Set<int, Boole, Cap>,
                         JoinSet<Set<int, Boole, Lo>, Set<int, Boole, Hi>>>>);
    STATIC_CHECK(met(7));         // < 10 and ≥ 5
    STATIC_CHECK(met(1));         // < 10 and ≤ 2
    STATIC_CHECK_FALSE(met(3));   // < 10 but in the gap
    STATIC_CHECK_FALSE(met(12));  // ≥ 5 but not < 10
  }

  SECTION("absorption reads through the materialized node: A & (A | B) → A") {
    // The union above is a JoinSet, yet the reducer still recognises Lo as one
    // of its operands, so the meet absorbs to Lo rather than nesting a MeetSet.
    constexpr auto absorbed = SLo & (SLo | SHi);
    STATIC_CHECK(
        std::same_as<std::decay_t<decltype(absorbed)>, Set<int, Boole, Lo>>);
    STATIC_CHECK(absorbed(7));
    STATIC_CHECK_FALSE(absorbed(1));  // 1 is in Hi = B, not in Lo = A
  }

  SECTION("complement is a certified involution: !!A ≡ A by type") {
    // A second complement peels the first rather than nesting it, so the double
    // negation eliminates and the original set TYPE is recovered (the
    // :involution witness that ¬ is involutive on the classical logic).
    STATIC_CHECK(std::same_as<std::decay_t<decltype(!!SLo)>,
                              std::decay_t<decltype(SLo)>>);
    STATIC_CHECK((!!SLo)(7) == SLo(7));
    STATIC_CHECK((!!SLo)(1) == SLo(1));
  }
}

// ══ #865 slice: distributivity of the subobject lattice ═════════════════════
// #865's motivating showcases and the term-level distributivity witness.  Two
// findings this slice pins:
//
//  1. The motivating showcase ((n>5)∪(n>7)) ∩ (n<3) ALREADY collapses to Ø ---
//     but the operative mechanism is the JOIN pre-collapsing (structured_or
//     unions the two same-direction halfspaces to {n>5}), then structured_and
//     yields the contradiction {n>5}∩{n<3}=Ø.  Distributivity never fires,
//     because there is no surviving join to distribute over.
//
//  2. Distributivity IS now a first-class law of Sub(T) (the marker
//     is_distributive_lattice_for_v<T, subobject_order<L>> is asserted in
//     :boundaries): the reducer distributes a GENUINELY non-collapsing Sub(T)
//     meet-over-join into disjunctive normal form.  This is witnessed at the
//     term level, since the value-level DSL materialises an irreducible union
//     as an opaque JoinSet (per #892), which the reducer does not distribute
//     --- driving the DNF through the value path is a deferred normal-form
//     decision.
namespace dist865 {

// The #865 motivating showcase, over ℤ (int).
using Above5 = Halfspace<int, 5, Direction::Upward, Strictness::Strict, Boole>;
using Above7 = Halfspace<int, 7, Direction::Upward, Strictness::Strict, Boole>;
using Below3 =
    Halfspace<int, 3, Direction::Downward, Strictness::Strict, Boole>;

constexpr Set<int, Boole, Above5> gt5{Above5{}};
constexpr Set<int, Boole, Above7> gt7{Above7{}};
constexpr Set<int, Boole, Below3> lt3{Below3{}};

// FINDING 1: the union pre-collapses (structured_or), so the whole expression
// reduces to Ø WITHOUT distributivity ever firing.
static_assert(
    std::same_as<std::decay_t<decltype(gt5 | gt7)>, Set<int, Boole, Above5>>,
    "((n>5)∪(n>7)) pre-collapses to {n>5} via structured_or, so no "
    "join survives to distribute over.");
static_assert(
    std::same_as<std::decay_t<decltype((gt5 | gt7) & lt3)>, Ø<int, Boole>>,
    "((n>5)∪(n>7)) ∩ (n<3) → Ø (contradiction after the union pre-collapse).");

// The distributivity marker is now ON for the subobject lattice Sub(ℤ).
static_assert(is_distributive_lattice_for_v<int, subobject_order<Boole>>,
              "Sub(ℤ) is a distributive (Heyting/Boolean) lattice under "
              "subobject_order.");
static_assert(is_distributive_lattice_for_v<int, subobject_order<Kleene>>,
              "the Kleene subobject lattice is Heyting, hence distributive.");

// FINDING 2: a genuinely non-collapsing meet-over-join distributes to DNF.
// Cap = {x<10}; the union {x≥5} ∪ {x≤2} has a gap at 3,4, so it does not
// collapse.  Distribute:  {x<10} ∩ ({x≥5}∪{x≤2})
//                       → ({x<10}∩{x≥5}) ∪ ({x<10}∩{x≤2})
//                       → [5,10) ∪ {x≤2}.
using Cap = Halfspace<int, 10, Direction::Downward, Strictness::Strict, Boole>;
using Ge5 = Halfspace<int, 5, Direction::Upward, Strictness::NonStrict, Boole>;
using Le2 =
    Halfspace<int, 2, Direction::Downward, Strictness::NonStrict, Boole>;
using SCap = Set<int, Boole, Cap>;
using SGe5 = Set<int, Boole, Ge5>;
using SLe2 = Set<int, Boole, Le2>;

// The distributed disjunctive normal form the reducer produces.
using Interval5to10 =
    OrderInterval<int, 5, 10, Strictness::NonStrict, Strictness::Strict, Boole>;
using DnfForm = Join<Interval5to10, SLe2>;

static_assert(
    std::same_as<
        subobject_reduce_t<Meet<SCap, Join<SGe5, SLe2>>, Boole, SetCombine>,
        DnfForm>,
    "distributivity of Sub(ℤ): {x<10} ∩ ({x≥5}∪{x≤2}) → [5,10) ∪ {x≤2}.");

// NON-REGRESSION: the marker fires only on a bare category::Join node, so the
// VALUE path (which materialises an irreducible union as a JoinSet) is
// unchanged --- SCap & (SGe5 ∪ SLe2) still materialises as a MeetSet (#892).
using JoinSetGe5Le2 = JoinSet<SGe5, SLe2>;

}  // namespace dist865

TEST_CASE("Exhibit: #865 distributivity of the subobject lattice",
          "[order][sets][exhibit][collapse][865]") {
  using namespace dist865;

  SECTION("motivating showcase ((n>5)∪(n>7)) ∩ (n<3) collapses to Ø") {
    // The union pre-collapses; distributivity is not the mechanism.
    constexpr auto reduced = (gt5 | gt7) & lt3;
    STATIC_CHECK(std::same_as<std::decay_t<decltype(reduced)>, Ø<int, Boole>>);
    // Meaning preservation: the reduced empty set rejects everything.
    STATIC_CHECK_FALSE(reduced(6));  // in (n>5)∪(n>7) but not (n<3)
    STATIC_CHECK_FALSE(reduced(2));  // in (n<3) but not the union
    STATIC_CHECK_FALSE(reduced(4));  // in neither (not >5, not <3)
  }

  SECTION("Sub(ℤ) distributes a genuinely non-collapsing meet-over-join") {
    // Pin the DNF the reducer produces (the collapse IS the point: the result
    // TYPE is the disjunctive normal form).
    STATIC_CHECK(
        std::same_as<
            subobject_reduce_t<Meet<SCap, Join<SGe5, SLe2>>, Boole, SetCombine>,
            DnfForm>);

    // Runtime meaning preservation: the DNF classifies exactly as the original
    // meet-over-join  (x<10) ∧ ((x≥5) ∨ (x≤2))  would, at representative
    // points.
    constexpr DnfForm dnf{Interval5to10{}, SLe2{Le2{}}};
    auto original = [](int x) { return (x < 10) && ((x >= 5) || (x <= 2)); };
    for (int x : {-5, 1, 2, 3, 4, 5, 7, 9, 10, 12}) {
      CHECK(static_cast<bool>(dnf(x)) == original(x));
    }
  }

  SECTION("value path is unchanged: the marker does not force DNF (#892)") {
    // {x≥5} ∪ {x≤2} has a gap at 3,4 (no boundary/interval normal form), so it
    // materialises as a JoinSet; the meet with {x<10} materialises as a MeetSet
    // rather than distributing --- the value-level normal form is untouched.
    constexpr SGe5 SGe5v{Ge5{}};
    constexpr SLe2 SLe2v{Le2{}};
    constexpr SCap SCapv{Cap{}};
    constexpr auto uni = SGe5v | SLe2v;
    STATIC_CHECK(std::same_as<std::decay_t<decltype(uni)>, JoinSetGe5Le2>);
    constexpr auto met = SCapv & uni;
    STATIC_CHECK(std::same_as<std::decay_t<decltype(met)>,
                              MeetSet<SCap, JoinSetGe5Le2>>);
    // Meaning preservation of the (un-distributed) materialised meet, which
    // equals the distributed DNF pointwise (the reducer's law is sound).
    CHECK(met(1));         // <10 and ≤2
    CHECK_FALSE(met(3));   // <10 but in the gap
    CHECK(met(7));         // <10 and ≥5
    CHECK_FALSE(met(12));  // not <10
  }
}
