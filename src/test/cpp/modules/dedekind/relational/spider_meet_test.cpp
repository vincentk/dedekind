/** @file test/cpp/modules/dedekind/relational/spider_meet_test.cpp
 *  The point-free composite meet @c Intersect @c = @c Δ† @c ∘ @c (R⊗S) @c ∘
 *  @c Δ (@c :relational:dyadic): the arrow-level (spider) realisation of the
 *  relational intersection @c R∩S, built from the @c :category cartesian-
 *  bicategory legs @c Copy / @c Merge / @c Tensor.  Exercises the endo,
 *  predicate (X→Ω), and Set-meet cases, and its pointwise agreement with the
 *  extensional @c operator&. */
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.relational;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::relational;

namespace {

// The composite meet is a genuine arrow, and over the identity endo-leg it
// collapses to the identity: the arrow-level shadow of the idempotent law
// a ∧ a = a (Δ copies, id⊗id is inert, Δ† merges the two equal copies).
using EndoMeet =
    Intersect<Identity<bool>, Identity<bool>, std::logical_and<bool>>;
static_assert(IsArrow<EndoMeet>,
              "the composite meet Δ† ∘ (R ⊗ S) ∘ Δ must be an arrow.");
static_assert(EndoMeet{}(true) == true,
              "Δ† ∘ (id ⊗ id) ∘ Δ must compute a ∧ a = a.");
static_assert(EndoMeet{}(false) == false,
              "Δ† ∘ (id ⊗ id) ∘ Δ must compute a ∧ a = a.");

// #954: the loosened Intersect types the PREDICATE / relational meet X→Ω
// directly (legs X→Ω, Copy on the domain X, Merge = ∧ on the codomain Ω), not
// only the endo A→A case.  A set IS χ:X→Ω, so this is the categorical meet apex
// the sets Sub(A) meet is a model of (#946).  Two distinct int→bool predicates
// meet where they agree.
struct EvenChi {
  using Domain = int;
  using Codomain = bool;
  constexpr bool operator()(const int& x) const { return x % 2 == 0; }
};
struct PositiveChi {
  using Domain = int;
  using Codomain = bool;
  constexpr bool operator()(const int& x) const { return x > 0; }
};
using PredMeet = Intersect<EvenChi, PositiveChi, std::logical_and<bool>>;
static_assert(
    IsArrow<PredMeet>,
    "the loosened Intersect types the predicate meet X→Ω (legs int→bool), not "
    "just endo A→A: a set IS χ:X→Ω (#954/#946).");
static_assert(PredMeet{}(4) == true, "(even ∩ positive)(4) = true ∧ true.");
static_assert(PredMeet{}(3) == false, "(even ∩ positive)(3) = false ∧ true.");

// #954 / #946 slice 2: the sets meet IS the spider Intersect apex.  A Set is an
// arrow χ:T→Ω (Domain=T, Codomain=Ω), so the loosened Intersect (X→Ω legs)
// types the meet of two sets DIRECTLY: Copy fans the shared domain int, the
// merge Δ†=∧ (Boole::MeetOp) glues the two membership answers on Ω.  The spider
// computes the UNREDUCED pointwise meet; operator& then REDUCES it to a normal
// form.  Both realise the SAME order-theoretic meet on Sub(int).
struct IsEven {
  constexpr bool operator()(int x) const { return x % 2 == 0; }
};
struct IsPositive {
  constexpr bool operator()(int x) const { return x > 0; }
};
using A_set = Set<int, Boole, IsEven>;
using B_set = Set<int, Boole, IsPositive>;
constexpr A_set a_set{IsEven{}};
constexpr B_set b_set{IsPositive{}};
constexpr auto meet_set = a_set & b_set;

using SpiderMeet = Intersect<A_set, B_set, Boole::MeetOp>;
static_assert(IsArrow<SpiderMeet>,
              "a Set is an arrow χ:T→Ω, so Intersect<Set,Set,∧> types: the "
              "spider meet apex over Sub(int) (#954/#946).");
static_assert(SpiderMeet{a_set, b_set}(4) == (a_set(4) && b_set(4)),
              "the spider meet computes the pointwise conjunction of the two "
              "set memberships (Δ†∘(A⊗B)∘Δ).");
static_assert(
    SpiderMeet{a_set, b_set}(4) == meet_set(4) &&
        SpiderMeet{a_set, b_set}(3) == meet_set(3) &&
        SpiderMeet{a_set, b_set}(-2) == meet_set(-2),
    "the spider meet agrees POINTWISE with operator& (a_set & b_set): "
    "operator& is the reduced representative of this apex.");

}  // namespace

TEST_CASE("relational:spider-meet — Intersect runs at run time",
          "[relational][spider][meet][cartesian-bicategory]") {
  // The static_asserts above are invisible to Codecov; drive the bodies at run
  // time here.
  SECTION("endo idempotent meet a ∧ a = a") {
    const EndoMeet meet{};
    CHECK(meet(true));
    CHECK_FALSE(meet(false));
  }

  SECTION("predicate meet X→Ω agrees where the two legs agree") {
    const PredMeet meet{};
    CHECK(meet(4) == true);    // even ∧ positive
    CHECK(meet(3) == false);   // odd ∧ positive
    CHECK(meet(-2) == false);  // even ∧ non-positive
  }

  SECTION("Set spider meet agrees pointwise with operator&") {
    const SpiderMeet meet{a_set, b_set};
    for (int x : {-2, 3, 4, 7, 8}) {
      CHECK(meet(x) == meet_set(x));
    }
  }
}
