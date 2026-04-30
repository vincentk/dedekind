#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Dedekind MVP: Basic Membership and Symbols", "[sets]") {
  SECTION("Integer Universe Membership") {
    auto x = var<Ω<int>>;  // A variable representing an element of the integer
                           // universe

    // Should be Set<int, ClassicalLogic>
    auto finite = Set{x % singleton(1) | (x == 1)};
    REQUIRE(finite(1) == true);
    REQUIRE(finite(2) == false);
  }

  SECTION("Natural-numbers membership") {
    // Post-#401, ℕ is the unsigned-int carrier; the predicate-set N has
    // Domain = unsigned int, so callsites use unsigned values.
    auto n = var<ℕ>;
    auto infinite = Set{n % N | (n > 0u)};
    REQUIRE(infinite(5u) == Ternary::True);
    REQUIRE(infinite(0u) == Ternary::False);
  }
}

TEST_CASE("Dedekind Sets: symmetric difference (^) — #469",
          "[sets][operators]") {
  auto x = var<ℕ>;

  SECTION(
      "Singleton ^ Singleton — equal pivots empty, distinct pivots union "
      "(#469)") {
    auto same_a = singleton(7);
    auto same_b = singleton(7);
    auto distinct = singleton(11);
    auto sym_eq = same_a ^ same_b;
    auto sym_neq = same_a ^ distinct;
    // {7} ^ {7} is empty pointwise.
    REQUIRE(sym_eq(7) == false);
    REQUIRE(sym_eq(0) == false);
    // {7} ^ {11} contains exactly 7 and 11.
    REQUIRE(sym_neq(7) == true);
    REQUIRE(sym_neq(11) == true);
    REQUIRE(sym_neq(0) == false);
  }

  SECTION("Singleton ^ Set — pivot toggles membership (#469)") {
    auto x_int = var<Ω<int>>;
    auto positives = Set{x_int % Ω<int>{} | (x_int > 0)};
    auto sing_in_set = singleton(5);
    auto sing_out_set = singleton(-3);
    auto in_xor = sing_in_set ^ positives;    // 5 ∈ positives → result drops 5
    auto out_xor = sing_out_set ^ positives;  // -3 ∉ positives → result adds -3
    // 5 was in positives, now isn't (singleton toggled it off).
    REQUIRE(in_xor(5) == false);
    REQUIRE(in_xor(7) == true);  // 7 stays in (was in positives, not toggled)
    // -3 wasn't in positives, now is (singleton toggled it on).
    REQUIRE(out_xor(-3) == true);
    REQUIRE(out_xor(7) == true);    // 7 stays in
    REQUIRE(out_xor(-1) == false);  // -1 stays out
  }

  SECTION("Boundary collapses: A ^ ∅ = A, ∅ ^ A = A (#469)") {
    auto S = Set{x % N | x > 10u};
    Ø<unsigned int, TernaryLogic> empty{};
    // Both directions collapse structurally to S (the type is preserved,
    // not erased to a lambda predicate).
    auto right_collapse = S ^ empty;
    auto left_collapse = empty ^ S;
    static_assert(std::is_same_v<decltype(right_collapse), decltype(S)>,
                  "S ^ Ø collapses structurally to the same Set type as S.");
    static_assert(std::is_same_v<decltype(left_collapse), decltype(S)>,
                  "Ø ^ S collapses structurally to the same Set type as S.");
    REQUIRE(right_collapse(50u) == Ternary::True);
    REQUIRE(right_collapse(5u) == Ternary::False);
  }

  SECTION("Boundary collapses: A ^ Ω = ¬A, Ω ^ A = ¬A (#469)") {
    auto S = Set{x % N | x > 10u};
    Ω<unsigned int, TernaryLogic> universe{};
    auto right_collapse = S ^ universe;              // type: !S
    auto left_collapse = universe ^ S;               // type: !S
    REQUIRE(right_collapse(50u) == Ternary::False);  // 50 ∈ S → ∉ !S
    REQUIRE(right_collapse(5u) == Ternary::True);    // 5 ∉ S  → ∈ !S
    REQUIRE(left_collapse(50u) == Ternary::False);
    REQUIRE(left_collapse(5u) == Ternary::True);
  }

  SECTION("Self-XOR is empty at every input: A ^ A is ∅ pointwise") {
    // The structural type-level collapse `same_as<P, P> → Ø` is
    // unsound for stateful predicates (two Set<T, L, P> with the same
    // predicate TYPE may carry different predicate VALUES classifying
    // different sets — see the stateful-predicate-disjoint-instances
    // section below).  The honest claim is therefore the runtime one:
    // for any Set S, S ^ S evaluates to false at every input.
    auto S = Set{x % N | x > 10u};
    auto S_xor_S = S ^ S;
    REQUIRE(S_xor_S(5u) == Ternary::False);
    REQUIRE(S_xor_S(50u) == Ternary::False);
    REQUIRE(S_xor_S(200u) == Ternary::False);
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
    using BoolAmbient = Ω<bool, ClassicalLogic, Finite>;
    constexpr BoolAmbient B_bool{};
    constexpr auto b = var<BoolAmbient>;
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
    auto A = Set{x % N | x > 10u};
    auto B = Set{x % N | x < 5u};
    auto sym_diff_disjoint = A ^ B;
    auto union_disjoint = A | B;
    // Membership matches the union (since the intersection is empty,
    // every element in either is in exactly one).
    REQUIRE(sym_diff_disjoint(3u) == union_disjoint(3u));
    REQUIRE(sym_diff_disjoint(7u) == union_disjoint(7u));
    REQUIRE(sym_diff_disjoint(20u) == union_disjoint(20u));
    // 3 < 5 → in B → in symmetric difference / union.
    REQUIRE(sym_diff_disjoint(3u) == Ternary::True);
    // 7 is in neither (7 < 5 false, 7 > 10 false).
    REQUIRE(sym_diff_disjoint(7u) == Ternary::False);
    // 20 > 10 → in A → in symmetric difference / union.
    REQUIRE(sym_diff_disjoint(20u) == Ternary::True);
  }

  SECTION("De Morgan negation peel: A ^ ¬B = ¬(A ^ B) (#469)") {
    // When the rhs predicate is wrapped in NegatedPredicate (e.g.\
    // !some_set), the operator^ peels the negation outward.
    // Resulting semantic: x ∈ A ^ ¬B iff x is in exactly one, which
    // is equivalent to x ∈ A ↔ x ∈ B (the biconditional).
    auto A = Set{x % N | x > 10u};
    auto B = Set{x % N | x < 100u};
    auto sym_diff_neg = A ^ !B;
    auto biconditional = !(A ^ B);
    // Both should agree pointwise: A ^ ¬B = ¬(A ^ B).
    REQUIRE(sym_diff_neg(5u) == biconditional(5u));
    REQUIRE(sym_diff_neg(50u) == biconditional(50u));
    REQUIRE(sym_diff_neg(200u) == biconditional(200u));
    // Concrete values:
    // 5: ∈ B (5 < 100), ∉ A (5 ≯ 10) → A ^ B classifies True at 5
    //   → ¬(A ^ B) classifies False at 5 → A ^ ¬B = False.
    REQUIRE(sym_diff_neg(5u) == Ternary::False);
    // 50: ∈ both A and B → A ^ B classifies False → ¬(A ^ B) = True.
    REQUIRE(sym_diff_neg(50u) == Ternary::True);
  }

  SECTION("Complementary-pair XOR collapses to universe: A ^ ¬A = Ω") {
    auto S = Set{x % N | x > 10u};
    auto S_xor_notS = S ^ !S;
    static_assert(
        std::is_same_v<decltype(S_xor_notS), Ω<unsigned int, TernaryLogic>>,
        "A ^ ¬A reduces structurally to Ω.");
  }

  SECTION("Membership: x ∈ A ^ B iff x is in exactly one") {
    auto A = Set{x % N | x > 10u};
    auto B = Set{x % N | x < 100u};
    auto sym_diff = A ^ B;
    // 5: in B only (5 < 100, 5 ≯ 10) → in symmetric difference
    REQUIRE(sym_diff(5u) == Ternary::True);
    // 50: in both (50 > 10 AND 50 < 100) → NOT in symmetric difference
    REQUIRE(sym_diff(50u) == Ternary::False);
    // 200: in A only (200 > 10, 200 ≮ 100) → in symmetric difference
    REQUIRE(sym_diff(200u) == Ternary::True);
  }

  SECTION("Textbook identity: A ^ B == (A | B) & !(A & B)") {
    auto A = Set{x % N | x > 10u};
    auto B = Set{x % N | x < 100u};
    auto sym_diff = A ^ B;
    auto union_minus_inter = (A | B) & !(A & B);
    REQUIRE(sym_diff(5u) == union_minus_inter(5u));
    REQUIRE(sym_diff(50u) == union_minus_inter(50u));
    REQUIRE(sym_diff(200u) == union_minus_inter(200u));
  }
}

TEST_CASE("Dedekind Identities: Extremal Collapse", "[sets][identities]") {
  auto x = var<ℕ>;

  SECTION("Identity: Set{N} is N") {
    // Naturals remain stable when materialized through Set{...}.
    auto U = Set{N};

    static_assert(std::is_same_v<decltype(U)::logic_species, TernaryLogic>);
    // ℕ-as-carrier (= unsigned int) accepts every unsigned value; the
    // classifier reading on int is reachable via direct N(-1) calls.
    REQUIRE(U(42u) == Ternary::True);
    // Direct N(int) call returns ClassicalLogic::Ω (= bool), not Ternary,
    // because the int overload short-circuits to the classical answer
    // without lifting through the ambient logic.
    REQUIRE(N(-1) == false);
  }

  SECTION("Contradiction: {x ∈ ℕ | x > 10 ∧ x < 5} is ∅") {
    // Here we combine the symbolic predicates
    auto S = Set{x % N | (x > 10u && x < 5u)};

    // For a non-trivial polish, we verify it is 'Total Absence'
    REQUIRE(S(0u) == Ternary::False);
    REQUIRE(S(7u) == Ternary::False);
    REQUIRE(S(12u) == Ternary::False);
  }

  SECTION("Tautology: {x ∈ ℕ | x > 10 ∨ x <= 10} is Ω") {
    auto S = Set{x % N | (x > 10u || x <= 10u)};
    REQUIRE(S(7u) == Ternary::True);
  }
}

TEST_CASE("Dedekind Identities: Boolean literals collapse over 𝔹",
          "[sets][identities][boolean]") {
  using BoolAmbient = Ω<bool, ClassicalLogic, Finite>;
  constexpr BoolAmbient B_bool{};

  constexpr auto b = var<BoolAmbient>;

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
  // The textbook DSL form `Set{b % B | b}` reads "elements of B for
  // which b holds" — the bare-b form is the truthy predicate, and
  // should be recognised as semantically equivalent to b == true by
  // the structured-and / FiniteBooleanSet collapse machinery.
  using BoolAmbient = Ω<bool, ClassicalLogic, Finite>;
  constexpr BoolAmbient B_bool{};

  constexpr auto b = var<BoolAmbient>;

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

TEST_CASE("Dedekind Sets: Cartesian product and relation witnesses",
          "[sets][relations][cartesian]") {
  auto x = var<Ω<int>>;

  const auto positive = Set{x % Ω<int>{} | (x > 0)};
  const auto small = Set{x % Ω<int>{} | (x <= 3)};

  const auto product = cartesian_product(positive, small);
  using ProductDomain = typename decltype(product)::Domain;
  const auto product_set = ambient_set<ProductDomain>(product);

  STATIC_CHECK(IsProduct<ProductDomain, int, int>);
  STATIC_CHECK(IsSet<decltype(product_set)>);

  CHECK(product(ProductDomain{1, 2}) == Ternary::True);
  CHECK(product(ProductDomain{-1, 2}) == Ternary::False);
  CHECK(product(ProductDomain{1, 7}) == Ternary::False);

  const auto graph_pred = [](const std::pair<int, int>& p) {
    return p.second == 2 * p.first;
  };
  const Relation<int, int, ClassicalLogic, decltype(graph_pred)> R{graph_pred};

  STATIC_CHECK(IsRelation<decltype(R), int, int>);
  CHECK(relates(R, 3, 6) == true);
  CHECK(relates(R, 3, 7) == false);

  const SetFunction<int, int, ClassicalLogic, decltype(graph_pred)> F{
      graph_pred};
  CHECK(is_single_valued_at(F, 3, 6, 6) == true);
  CHECK(is_single_valued_at(F, 3, 6, 7) == true);
}

TEST_CASE("Dedekind Sets: Ambient cartesian product ergonomics",
          "[sets][relations][cartesian][ambient]") {
  constexpr auto ambient = Ω<int>{};
  constexpr auto p_via_function = cartesian_product(ambient, ambient);
  constexpr auto p_via_operator = ambient * ambient;

  using PDomain = typename decltype(p_via_operator)::Domain;

  STATIC_CHECK(IsProduct<PDomain, int, int>);
  STATIC_CHECK(p_via_function(PDomain{1, 2}) == Ternary::True);
  STATIC_CHECK(p_via_operator(PDomain{3, 4}) == Ternary::True);
}

TEST_CASE("Dedekind Sets: Power-set witness over homogeneous predicates",
          "[sets][powerset]") {
  auto x = var<Ω<int>>;

  const auto positive = Set{x % Ω<int>{} | (x > 0)};

  const auto p_positive = power_set(positive);
  // Textbook fraktur-P alias: 𝔓(A) ≡ power_set(A).
  const auto 𝔓_positive = 𝔓(positive);

  STATIC_CHECK(std::same_as<typename decltype(p_positive)::Domain,
                            std::remove_cvref_t<decltype(positive)>>);
  STATIC_CHECK(std::same_as<decltype(p_positive), decltype(𝔓_positive)>);
  CHECK(p_positive(positive) == Ternary::True);
  CHECK(𝔓_positive(positive) == Ternary::True);
}

TEST_CASE("Dedekind Sets: Power-set preserves ambient logic",
          "[sets][powerset][logic]") {
  auto x = var<ℕ>;

  const auto gt_zero = Set{x % N | (x > 0u)};
  const auto p_gt_zero = power_set(gt_zero);

  STATIC_CHECK(
      std::same_as<typename decltype(p_gt_zero)::logic_species, TernaryLogic>);
  CHECK(p_gt_zero(gt_zero) == Ternary::True);
}

TEST_CASE("Dedekind Sets: Relation witnesses preserve ternary logic",
          "[sets][relations][logic]") {
  const auto tri_rel_pred = [](const std::pair<int, int>& p) {
    if (p.first == 3 && p.second == 6) return Ternary::Unknown;
    if (p.first == 3 && p.second == 7) return Ternary::True;
    return Ternary::False;
  };

  const Relation<int, int, TernaryLogic, decltype(tri_rel_pred)> R{
      tri_rel_pred};

  CHECK(relates(R, 3, 6) == Ternary::Unknown);
  CHECK(relates(R, 3, 7) == Ternary::True);

  const SetFunction<int, int, TernaryLogic, decltype(tri_rel_pred)> F{
      tri_rel_pred};
  CHECK(is_single_valued_at(F, 3, 6, 7) == Ternary::Unknown);
}

TEST_CASE("Dedekind Sets: Heterogeneous subset semantics",
          "[sets][subset][logic]") {
  SECTION("Ternary logic yields Unknown for heterogeneous predicates") {
    auto x = var<ℕ>;
    const auto gt_zero = Set{x % N | (x > 0u)};
    const auto ge_zero = Set{x % N | (x >= 0u)};

    REQUIRE((gt_zero <= ge_zero) == Ternary::Unknown);
  }

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
