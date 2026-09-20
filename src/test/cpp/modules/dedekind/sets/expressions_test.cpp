#include <catch2/catch_test_macros.hpp>
#include <optional>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

// Test-fork helpers for the retract-decidable image specialisation
// (#602 Layer 2 / Case A / #659).  A non-iso monic arrow whose image
// is decidable through a partial inverse (retract).
//
// DoubleArrow: int → int, x ↦ 2*x.  Monic (the user declares
// is_monic_arrow_v) but not iso (the codomain is "even ints only", so
// no total inverse on the natural int).  Retract: y ↦ y/2 if y is
// even, else nullopt.
namespace retract_image_test {
struct DoubleArrow {
  using Domain = int;
  using Codomain = int;
  constexpr int operator()(int x) const { return 2 * x; }
};

constexpr auto retract(DoubleArrow) {
  return [](const int& y) -> std::optional<int> {
    if (y % 2 == 0) return y / 2;
    return std::nullopt;
  };
}
}  // namespace retract_image_test

// The NON-injective case (the sign-fold abs(x) = |x|, whose two branches are
// the reflections x↦x and x↦−x) is decided ANALYTICALLY and point-free in
// dedekind.order --- image over the union of the mono reflection branches, no
// retract/cofibre fibre-walk (§3.3, Listing 13).

// Register the monic trait so IsMonicArrow (and IsRetractableArrow) fire on
// DoubleArrow (the single-lookup retract path, no enumeration).
template <>
inline constexpr bool
    dedekind::category::is_monic_arrow_v<retract_image_test::DoubleArrow> =
        true;

// #881: the non-collapsed meet's classifier AndPredicate<P,Q> IS the pairing
// ⟨χ_A, χ_B⟩ of its two operand classifiers, so it satisfies IsProduct
// (π_1 → χ_A, π_2 → χ_B) via the free overloads in :sets, found by ADL.
namespace and_predicate_product_test {
struct IsEven {
  constexpr bool operator()(int x) const { return x % 2 == 0; }
};
struct IsPositive {
  constexpr bool operator()(int x) const { return x > 0; }
};
using Meet = dedekind::sets::AndPredicate<IsEven, IsPositive>;
static_assert(dedekind::category::IsProduct<Meet, IsEven, IsPositive>,
              "AndPredicate ⟨χ_A, χ_B⟩ is the categorical product of its two "
              "operand classifiers (π_1 → χ_A, π_2 → χ_B): #881.");
static_assert(π_1(Meet{IsEven{}, IsPositive{}})(4),
              "π_1 recovers χ_A = IsEven: π_1(meet)(4) = true.");
static_assert(π_2(Meet{IsEven{}, IsPositive{}})(4),
              "π_2 recovers χ_B = IsPositive: π_2(meet)(4) = true.");

// #881 step 2: OrPredicate carries the same pairing ⟨χ_A, χ_B⟩, so it too is an
// IsProduct (the join's classifier).  Same substrate as AndPredicate; the
// pushout vs pullback distinction is the ∨ vs ∧ reduction, not the pairing.
using Join = dedekind::sets::OrPredicate<IsEven, IsPositive>;
static_assert(
    dedekind::category::IsProduct<Join, IsEven, IsPositive>,
    "OrPredicate ⟨χ_A, χ_B⟩ is also the categorical product of its two "
    "operand classifiers (π_1 → χ_A, π_2 → χ_B): #881 step 2.");

// #881: the APPLIED meet A & B (the reified intersection of two concrete sets)
// IS the pullback of those two sets --- the cospan of their inclusions
// ι_A, ι_B, with the meet's co-restriction legs π1/π2.  In the poset Sub(U),
// product = pullback = meet.  (The predicate alone, AndPredicate, is only the
// classifier-pairing above; the pullback is the applied predicate.)
using A_set =
    dedekind::sets::Set<int, dedekind::category::ClassicalLogic, IsEven>;
using B_set =
    dedekind::sets::Set<int, dedekind::category::ClassicalLogic, IsPositive>;
constexpr A_set a_set{IsEven{}};
constexpr B_set b_set{IsPositive{}};
constexpr auto meet_set = a_set & b_set;
constexpr auto iota_A = dedekind::sets::inclusion_arrow(a_set);
constexpr auto iota_B = dedekind::sets::inclusion_arrow(b_set);
static_assert(
    dedekind::category::IsPullback<decltype(meet_set), decltype(iota_A),
                                   decltype(iota_B)>,
    "the applied meet A & B is the pullback of its two concrete sets (the "
    "cospan ι_A, ι_B); in Sub(U) product = pullback = meet. #881.");
}  // namespace and_predicate_product_test

TEST_CASE("Dedekind MVP: Basic Membership and Symbols", "[sets]") {
  SECTION("Integer Universe Membership") {
    auto x = element<𝔸<int>>;  // A variable representing an element of
                               // the integer universe

    // Should be Set<int, ClassicalLogic>
    auto finite = Set{x % singleton(1) | (x == 1)};
    REQUIRE(finite(1) == true);
    REQUIRE(finite(2) == false);
  }

  SECTION("Natural-numbers membership") {
    // Post-#559, ℕ is the universe value 𝔸<Cardinality>; the underlying
    // carrier is Cardinality (the variant ℕ-proxy from #402, which
    // accepts unsigned literals via implicit construction).  Callsites
    // here use unsigned values that lift into Cardinality.
    auto n = element<ℕ>;
    auto infinite = Set{n | (n > 0u)};
    REQUIRE(infinite(5u));
    REQUIRE_FALSE(infinite(0u));
  }
}

TEST_CASE("Dedekind Sets: symmetric difference (^) — #469",
          "[sets][operators]") {
  auto x = element<ℕ>;

  SECTION(
      "Singleton ^ Singleton — equal pivots empty, distinct pivots union "
      "(#469)") {
    auto same_a = singleton(7);
    auto same_b = singleton(7);
    auto distinct = singleton(11);
    auto sym_eq = same_a ^ same_b;
    auto sym_neq = same_a ^ distinct;
    // Post-#622 (carrier-axis cut): ℕ is countable (ℵ_0) and routes to
    // ClassicalLogic, so the Set CTAD lands @c bool, not @c Ternary.
    // {7} ^ {7} is empty pointwise.
    REQUIRE_FALSE(sym_eq(7));
    REQUIRE_FALSE(sym_eq(0));
    // {7} ^ {11} contains exactly 7 and 11.
    REQUIRE(sym_neq(7));
    REQUIRE(sym_neq(11));
    REQUIRE_FALSE(sym_neq(0));
  }

  SECTION("Singleton ^ Set — pivot toggles membership (#469)") {
    auto x_int = element<𝔸<int>>;
    auto positives = Set{x_int % UniversalSet<int>{} | (x_int > 0)};
    auto sing_in_set = singleton(5);
    auto sing_out_set = singleton(-3);
    auto in_xor = sing_in_set ^ positives;    // 5 ∈ positives → result drops 5
    auto out_xor = sing_out_set ^ positives;  // -3 ∉ positives → result adds -3
    // Same TernaryLogic ascent as above.
    // 5 was in positives, now isn't (singleton toggled it off).
    REQUIRE_FALSE(in_xor(5));
    // 7 stays in (was in positives, not toggled).
    REQUIRE(in_xor(7));
    // -3 wasn't in positives, now is (singleton toggled it on).
    REQUIRE(out_xor(-3));
    REQUIRE(out_xor(7));         // 7 stays in
    REQUIRE_FALSE(out_xor(-1));  // -1 stays out
  }

  SECTION("Boundary collapses: A ^ ∅ = A, ∅ ^ A = A (#469)") {
    auto S = Set{x | x > 10u};
    // Use the deduced Domain / logic species from S rather than
    // hard-coding `unsigned int` / TernaryLogic — the carrier choice
    // is set by N's CTAD, and the test should not pre-empt it.
    using SDomain = decltype(S)::Domain;
    using SLogic = decltype(S)::logic_species;
    Ø<SDomain, SLogic> empty{};
    // Both directions collapse structurally to S (the type is preserved,
    // not erased to a lambda predicate).
    auto right_collapse = S ^ empty;
    auto left_collapse = empty ^ S;
    static_assert(std::is_same_v<decltype(right_collapse), decltype(S)>,
                  "S ^ Ø collapses structurally to the same Set type as S.");
    static_assert(std::is_same_v<decltype(left_collapse), decltype(S)>,
                  "Ø ^ S collapses structurally to the same Set type as S.");
    REQUIRE(right_collapse(50u));
    REQUIRE_FALSE(right_collapse(5u));
  }

  SECTION("Boundary collapses: A ^ 𝔸 = ¬A, 𝔸 ^ A = ¬A (#469)") {
    auto S = Set{x | x > 10u};
    using SDomain = decltype(S)::Domain;
    using SLogic = decltype(S)::logic_species;
    UniversalSet<SDomain, SLogic> universe{};
    auto right_collapse = S ^ universe;  // type: !S
    auto left_collapse = universe ^ S;   // type: !S
    REQUIRE_FALSE(right_collapse(50u));  // 50 ∈ S → ∉ !S
    REQUIRE(right_collapse(5u));         // 5 ∉ S  → ∈ !S
    REQUIRE_FALSE(left_collapse(50u));
    REQUIRE(left_collapse(5u));
  }

  SECTION("Self-XOR is empty at every input: A ^ A is ∅ pointwise") {
    // The structural type-level collapse `same_as<P, P> → Ø` is
    // unsound for stateful predicates (two Set<T, L, P> with the same
    // predicate TYPE may carry different predicate VALUES classifying
    // different sets — see the stateful-predicate-disjoint-instances
    // section below).  The honest claim is therefore the runtime one:
    // for any Set S, S ^ S evaluates to false at every input.
    auto S = Set{x | x > 10u};
    auto S_xor_S = S ^ S;
    REQUIRE_FALSE(S_xor_S(5u));
    REQUIRE_FALSE(S_xor_S(50u));
    REQUIRE_FALSE(S_xor_S(200u));
  }

  SECTION(
      "Stateful-predicate disjoint instances: type equality does NOT "
      "imply set equality (#469 regression test)") {
    // Two Set<bool, L, BooleanEqPredicate> instances with the same
    // Predicate TYPE but disjoint VALUES.  The XOR of {true} and
    // {false} should be the full {true, false} universe, NOT empty.
    // This regression test guards against a same-Predicate-type
    // collapse that would wrongly fire on every BooleanEqPredicate
    // pair regardless of the .expected field.
    using BoolAmbient = UniversalSet<bool, ClassicalLogic, Finite>;
    constexpr BoolAmbient B_bool{};
    constexpr auto b = element<𝔸<bool>>;
    auto only_true = Set{b % B_bool | (b == true)};
    auto only_false = Set{b % B_bool | (b == false)};
    auto sym_diff = only_true ^ only_false;
    // The symmetric difference of two disjoint singletons is their
    // union — every element of {true, false} appears in exactly one,
    // so both inputs must be in the result.
    CHECK(sym_diff(true) == true);
    CHECK(sym_diff(false) == true);
  }

  SECTION(
      "Compile-time-disjoint optimisation: A ^ B = A | B when A & B is "
      "structurally empty (#469)") {
    // Halfspace-style disjoint pair: (x > 10) and (x < 5) over ℕ — the
    // structured_and overload in :order:halfspace detects emptiness of
    // the intersection at the type level, so A & B reduces to
    // Ø<unsigned int, TernaryLogic>.  In that case A ^ B should
    // collapse to A | B (no XOR formula needed in the result lambda).
    auto A = Set{x | x > 10u};
    auto B = Set{x | x < 5u};
    auto sym_diff_disjoint = A ^ B;
    auto union_disjoint = A | B;
    // Membership matches the union (since the intersection is empty,
    // every element in either is in exactly one).
    REQUIRE(sym_diff_disjoint(3u) == union_disjoint(3u));
    REQUIRE(sym_diff_disjoint(7u) == union_disjoint(7u));
    REQUIRE(sym_diff_disjoint(20u) == union_disjoint(20u));
    // 3 < 5 → in B → in symmetric difference / union.
    REQUIRE(sym_diff_disjoint(3u));
    // 7 is in neither (7 < 5 false, 7 > 10 false).
    REQUIRE_FALSE(sym_diff_disjoint(7u));
    // 20 > 10 → in A → in symmetric difference / union.
    REQUIRE(sym_diff_disjoint(20u));
  }

  SECTION("De Morgan negation peel: A ^ ¬B = ¬(A ^ B) (#469)") {
    // When the rhs predicate is wrapped in NegatedPredicate (e.g.\
    // !some_set), the operator^ peels the negation outward.
    // Resulting semantic: x ∈ A ^ ¬B iff x is in exactly one, which
    // is equivalent to x ∈ A ↔ x ∈ B (the biconditional).
    auto A = Set{x | x > 10u};
    auto B = Set{x | x < 100u};
    auto sym_diff_neg = A ^ !B;
    auto biconditional = !(A ^ B);
    // Both should agree pointwise: A ^ ¬B = ¬(A ^ B).
    REQUIRE(sym_diff_neg(5u) == biconditional(5u));
    REQUIRE(sym_diff_neg(50u) == biconditional(50u));
    REQUIRE(sym_diff_neg(200u) == biconditional(200u));
    // Concrete values:
    // 5: ∈ B (5 < 100), ∉ A (5 ≯ 10) → A ^ B classifies True at 5
    //   → ¬(A ^ B) classifies False at 5 → A ^ ¬B = False.
    REQUIRE_FALSE(sym_diff_neg(5u));
    // 50: ∈ both A and B → A ^ B classifies False → ¬(A ^ B) = True.
    REQUIRE(sym_diff_neg(50u));
  }

  SECTION(
      "Complementary-pair XOR — A ^ ¬A is universe at every input "
      "(De Morgan peel covers the runtime case)") {
    // The IsComplementPair_v structural collapse only fires for
    // stateless predicate types (guarded by std::is_empty_v); the
    // halfspace-style predicate (x > 10u) produces a capturing
    // lambda whose closure type is non-empty, so the type-level
    // collapse to UniversalSet<T, L> does NOT fire here.  What DOES fire is
    // the De Morgan negation-peel branch (A ^ !B → !(A ^ B)),
    // which leaves the result a Set<T, L, lambda> that pointwise
    // evaluates to true at every input.  We test the runtime
    // semantics rather than the structural type.
    auto S = Set{x | x > 10u};
    auto S_xor_notS = S ^ !S;
    REQUIRE(S_xor_notS(5u));
    REQUIRE(S_xor_notS(50u));
    REQUIRE(S_xor_notS(200u));
  }

  SECTION("Membership: x ∈ A ^ B iff x is in exactly one") {
    auto A = Set{x | x > 10u};
    auto B = Set{x | x < 100u};
    auto sym_diff = A ^ B;
    // 5: in B only (5 < 100, 5 ≯ 10) → in symmetric difference
    REQUIRE(sym_diff(5u));
    // 50: in both (50 > 10 AND 50 < 100) → NOT in symmetric difference
    REQUIRE_FALSE(sym_diff(50u));
    // 200: in A only (200 > 10, 200 ≮ 100) → in symmetric difference
    REQUIRE(sym_diff(200u));
  }

  SECTION("Textbook identity: A ^ B == (A | B) & !(A & B)") {
    auto A = Set{x | x > 10u};
    auto B = Set{x | x < 100u};
    auto sym_diff = A ^ B;
    auto union_minus_inter = (A | B) & !(A & B);
    REQUIRE(sym_diff(5u) == union_minus_inter(5u));
    REQUIRE(sym_diff(50u) == union_minus_inter(50u));
    REQUIRE(sym_diff(200u) == union_minus_inter(200u));
  }
}

TEST_CASE("Dedekind Identities: Extremal Collapse", "[sets][identities]") {
  auto x = element<ℕ>;

  SECTION("Identity: Set{N} is N") {
    // Naturals remain stable when materialized through Set{...}.
    auto U = Set{N};

    // Post-#622: ℕ → ClassicalLogic on the carrier axis.
    static_assert(std::is_same_v<decltype(U)::logic_species, ClassicalLogic>);
    // ℕ-as-carrier (= unsigned int) accepts every unsigned value; the
    // classifier reading on int is reachable via direct N(-1) calls.
    REQUIRE(U(42u));
    // Direct N(int) call returns ClassicalLogic::Ω (= bool), not Ternary,
    // because the int overload short-circuits to the classical answer
    // without lifting through the ambient logic.
    REQUIRE(N(-1) == false);
  }

  SECTION("Contradiction: {x ∈ ℕ | x > 10 ∧ x < 5} is ∅") {
    // Here we combine the symbolic predicates
    auto S = Set{x | (x > 10u && x < 5u)};

    // For a non-trivial polish, we verify it is 'Total Absence'
    REQUIRE_FALSE(S(0u));
    REQUIRE_FALSE(S(7u));
    REQUIRE_FALSE(S(12u));
  }

  SECTION("Tautology: {x ∈ ℕ | x > 10 ∨ x <= 10} is 𝔸") {
    auto S = Set{x | (x > 10u || x <= 10u)};
    REQUIRE(S(7u));
  }
}

TEST_CASE("Dedekind Identities: Boolean literals collapse over 𝔹",
          "[sets][identities][boolean]") {
  using BoolAmbient = UniversalSet<bool, ClassicalLogic, Finite>;
  constexpr BoolAmbient B_bool{};

  constexpr auto b = element<𝔸<bool>>;

  constexpr auto b_false = Set{b % B_bool | !b};
  constexpr auto b_true = Set{b % B_bool | (b == true)};

  STATIC_CHECK(Ø<bool, ClassicalLogic>{} == (b_false & b_true));
  STATIC_CHECK(B_bool == (b_false | b_true));

  CHECK((b_false & b_true)(false) == false);
  CHECK((b_false & b_true)(true) == false);
  CHECK((b_false | b_true)(false) == true);
  CHECK((b_false | b_true)(true) == true);
}

TEST_CASE(
    "Dedekind Identities: bare-Variable<bool> truthy form collapses (#408)",
    "[sets][identities][boolean][variable-truthy]") {
  // The textbook DSL form `Set{b | b}` reads "elements of B for
  // which b holds" — the bare-b form is the truthy predicate, and
  // should be recognised as semantically equivalent to b == true by
  // the structured-and / FiniteBooleanSet collapse machinery.
  using BoolAmbient = UniversalSet<bool, ClassicalLogic, Finite>;
  constexpr BoolAmbient B_bool{};

  constexpr auto b = element<𝔸<bool>>;

  // Bare-b form (the issue's target ergonomics).
  constexpr auto b_true_bare = Set{b % B_bool | b};
  // Equivalent comparison form.
  constexpr auto b_true_eq = Set{b % B_bool | (b == true)};
  // Negated bare-b form.
  constexpr auto b_false = Set{b % B_bool | !b};

  // The collapse machinery treats both bare-b and (b == true) as the
  // same predicate (BooleanEqPredicate{true}) so the static_asserts
  // pinning the Boolean partition laws fire on the bare-b form.
  STATIC_CHECK(Ø<bool, ClassicalLogic>{} == (b_false & b_true_bare));
  STATIC_CHECK(B_bool == (b_false | b_true_bare));

  // Operational witness: bare-b agrees with (b == true) at every input.
  CHECK(b_true_bare(false) == b_true_eq(false));
  CHECK(b_true_bare(true) == b_true_eq(true));
}

TEST_CASE("Dedekind Sets: Cartesian product witnesses", "[sets][cartesian]") {
  // The RELATION witnesses (Relation / IsRelation / relates / SetFunction /
  // is_single_valued_at) moved with their concepts to
  // relational/relation_core_test (the concept left :sets, so its test did
  // too --- the test DAG imports upstream only).
  auto x = element<𝔸<int>>;

  const auto positive = Set{x % UniversalSet<int>{} | (x > 0)};
  const auto small = Set{x % UniversalSet<int>{} | (x <= 3)};

  const auto product = cartesian_product(positive, small);
  using ProductDomain = typename decltype(product)::Domain;
  const auto product_set = ambient_set<ProductDomain>(product);

  STATIC_CHECK(IsProduct<ProductDomain, int, int>);
  STATIC_CHECK(IsSet<decltype(product_set)>);

  CHECK(product(ProductDomain{1, 2}));
  CHECK_FALSE(product(ProductDomain{-1, 2}));
  CHECK_FALSE(product(ProductDomain{1, 7}));
}

TEST_CASE("Dedekind Sets: Ambient cartesian product ergonomics",
          "[sets][relations][cartesian][ambient]") {
  constexpr auto ambient = UniversalSet<int>{};
  constexpr auto p_via_function = cartesian_product(ambient, ambient);
  constexpr auto p_via_operator = ambient * ambient;

  using PDomain = typename decltype(p_via_operator)::Domain;

  STATIC_CHECK(IsProduct<PDomain, int, int>);
  STATIC_CHECK(p_via_function(PDomain{1, 2}));
  STATIC_CHECK(p_via_operator(PDomain{3, 4}));
}

// (The ordered / convex power set 𝔓(setexpr) moved to
// dedekind.order:powerset
// (#830) — a Set over the subobject domain Sub(C), decided by the subset order
// downstream of :sets; those tests live in order/powerset_test.cpp.  The one
// closed form that needs no Sub — 𝔓(∅) = {∅} — stays in :sets and is tested
// here.)
TEST_CASE("sets:powerset — 𝔓(∅) = {∅} is a :sets closed form (#830)",
          "[sets][powerset]") {
  constexpr auto P0 = 𝔓(Ø<int>{});

  SECTION("𝔓(∅) is a bona-fide IsSet over the empty-set carrier Ø<int>") {
    STATIC_CHECK(IsSet<std::remove_cvref_t<decltype(P0)>>);
    STATIC_CHECK(
        std::same_as<typename std::remove_cvref_t<decltype(P0)>::Domain,
                     Ø<int>>);
    STATIC_CHECK(
        std::same_as<decltype(power_set(Ø<int>{})), decltype(𝔓(Ø<int>{}))>);
  }

  SECTION("its sole member is ∅ (so |𝔓(∅)| = 1 = 2^0, not the empty set)") {
    // ∅ ∈ 𝔓(∅): membership is True, which witnesses that 𝔓(∅) is the NON-empty
    // singleton {∅} --- the honest power set of ∅ --- not ∅ itself.  (Ø == P0
    // is deliberately unavailable: the Rice wall, so we witness via
    // membership.)
    CHECK(bool(P0(Ø<int>{})));
  }
}

// ("Relation witnesses preserve ternary logic" moved to
// relational/relation_core_test with the Relation type and the relates /
// is_single_valued_at surface.)

TEST_CASE("Dedekind Sets: Heterogeneous subset semantics",
          "[sets][subset][logic]") {
  // FIXME(#693): "Ternary logic yields Unknown for heterogeneous predicates"
  // — pre-#622 the test exhibited heterogeneous subset returning Unknown
  // because ℕ-carrier Sets routed Ternary by default; post-#622 ℕ →
  // Classical on the carrier axis, so the heterogeneous subset between
  // two opaque-λ ℕ-Sets is no longer a Ternary case.  Predicate-level
  // axis (#693) is the principled home for this witness — an explicit
  // Ternary-typed predicate carrier would expose Unknown without going
  // through the carrier-axis resolver.

  SECTION("Classical logic has no heterogeneous subset operator") {
    const auto positive_pred = [](const int& v) { return v > 0; };
    const auto small_pred = [](const int& v) { return v <= 3; };

    const Set<int, ClassicalLogic, decltype(positive_pred)> positive{
        positive_pred};
    const Set<int, ClassicalLogic, decltype(small_pred)> small{small_pred};

    CHECK(positive.is_subset_of_at(small, 5) == false);
    CHECK(positive.is_subset_of_at(small, -1) == true);
  }
}

TEST_CASE(
    "Dedekind Sets: image(iso f, Set<T, L, P>) — #602 Layer 2 decidable "
    "specialisation",
    "[sets][image][iso][layer2][602]") {
  // Identity<int> is an isomorphism (inverse = self).  The image of an
  // intensional Set<int> under the identity iso must be (i) on the same
  // ambient int, (ii) on the same logic species (no demotion to Ternary
  // as the generic IsArrow fallback would), and (iii) decidable point-
  // wise --- membership query lifts through the inverse (here = the
  // identity), so the image preserves the source's truth table exactly.

  SECTION("Identity iso preserves classical decidability") {
    const auto positive_pred = [](const int& v) { return v > 0; };
    const Set<int, ClassicalLogic, decltype(positive_pred)> positive{
        positive_pred};

    auto img = image(Identity<int>{}, positive);

    // The image's logic species is the source's (ClassicalLogic),
    // NOT TernaryLogic (which would be the IsArrow-fallback result).
    STATIC_CHECK(
        std::same_as<typename decltype(img)::logic_species, ClassicalLogic>);
    // Ambient is preserved.
    STATIC_CHECK(std::same_as<typename decltype(img)::Domain, int>);
    // Decidable: same truth values as the source through the identity.
    CHECK(img(5) == true);
    CHECK(img(-1) == false);
    CHECK(img(0) == false);
  }

  // FIXME(#693): "Identity iso on a ternary-logic source preserves
  // Ternary" — pre-#622 the ℕ fixture routed to TernaryLogic.  Post-#622
  // the carrier-axis cut puts ℕ on ClassicalLogic; recovering the
  // Ternary-preserves-Ternary witness requires an explicit Ternary-typed
  // predicate carrier — the principled home is the predicate-level axis
  // (#693).
}

TEST_CASE(
    "Dedekind Sets: image(monic-with-retract, Set) — #602 Layer 2 / Case A",
    "[sets][image][retract][monic][layer2][602]") {
  // DoubleArrow is monic (declared above) but not iso --- the codomain
  // is "even ints", a strict subset of int.  The retract sends even y
  // back to y/2 and odd y to nullopt.  Test that:
  //   (i)  image(DoubleArrow, S) preserves the source's logic species
  //        (no demotion to TernaryLogic);
  //   (ii) y in image iff y is even AND y/2 in S (decidable);
  //   (iii) odd y is always out of image (the retract returns nullopt).

  SECTION("Classical: image preserves Classical decidability") {
    const auto positive_pred = [](const int& v) { return v > 0; };
    const Set<int, ClassicalLogic, decltype(positive_pred)> positive{
        positive_pred};

    auto img = image(retract_image_test::DoubleArrow{}, positive);

    // Logic species is preserved (NOT TernaryLogic, which would be the
    // IsArrow-fallback result).
    STATIC_CHECK(
        std::same_as<typename decltype(img)::logic_species, ClassicalLogic>);
    STATIC_CHECK(std::same_as<typename decltype(img)::Domain, int>);

    // y = 10 is even, 10/2 = 5 > 0, so in image.
    CHECK(img(10) == true);
    // y = 4 is even, 4/2 = 2 > 0, so in image.
    CHECK(img(4) == true);
    // y = -6 is even, -6/2 = -3, not > 0, so NOT in image.
    CHECK(img(-6) == false);
    // y = 7 is odd, retract returns nullopt → NOT in image.
    CHECK(img(7) == false);
    // y = -3 is odd, same story.
    CHECK(img(-3) == false);
  }
}
