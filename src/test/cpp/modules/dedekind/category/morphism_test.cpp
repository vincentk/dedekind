#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Category: Morphisms and Arrow Factories", "[category][morphisms]") {
  SECTION("Identity Morphism (id)") {
    auto identity = id<int>();

    // Exercise the Identity struct's operator()
    CHECK(identity(42) == 42);
    CHECK(identity(-1) == -1);

    // Verify type traits at runtime
    STATIC_CHECK(std::same_as<typename decltype(identity)::Domain, int>);
  }

  SECTION("Arrow Factory (Morphism Tagging)") {
    // Tagging a lambda as a Morphism from int to bool
    auto is_even = arrow<int, bool>([](int x) { return x % 2 == 0; });

    CHECK(is_even(2) == true);
    CHECK(is_even(3) == false);

    // Verify it satisfies the IsArrow concept
    STATIC_CHECK(IsArrow<decltype(is_even)>);
  }

  SECTION("Endomorphism Factory (endo)") {
    // Endomorphisms preserve the species (Domain == Codomain)
    auto square = endo<int>([](int x) { return x * x; });

    CHECK(square(5) == 25);
    CHECK(square(-3) == 9);
  }

  SECTION("Zero Morphism (Absorption)") {
    // Zero morphism maps everything to the identity of the target monoid
    // For (int, +), identity is 0.
    auto absorb_to_zero = zero<double, int, std::plus<int>>();

    CHECK(absorb_to_zero(3.14) == 0);
    CHECK(absorb_to_zero(-99.9) == 0);

    // For (bool, &&), identity is true.
    auto absorb_to_true = zero<int, bool, std::logical_and<bool>>();

    CHECK(absorb_to_true(42) == true);
  }

  SECTION("Composition (The Highway Flow)") {
    auto f = endo<int>([](int x) { return x + 1; });
    auto g = endo<int>([](int x) { return x * 2; });

    // Compose: h = g . f (x -> (x+1)*2)
    auto h = f >> g;

    CHECK(h(5) == 12);
    CHECK(h(0) == 2);
  }
}

TEST_CASE("Category: Algebraic Proofs (Runtime Witnesses)",
          "[category][morphisms][algebra]") {
  SECTION("Small Category Identity Proof") {
    // Verify that the identity_trait is correctly resolved for primitive
    // species.
    CHECK(identity_v<int, std::plus<int>> == 0);
    CHECK(identity_v<bool, std::logical_and<bool>> == true);
    CHECK(identity_v<bool, std::logical_or<bool>> == false);
  }

  SECTION("Monic and Epic Arrow concepts on Identity") {
    // Identity is both monic and epic (it is an isomorphism).
    STATIC_CHECK(IsMonicArrow<Identity<int>>);
    STATIC_CHECK(IsEpicArrow<Identity<int>>);
  }

  SECTION(
      "Function-as-relation primitive (#460): IsBinaryRelation, "
      "IsBinaryFunction, arrow_as_relation bridge") {
    // (1) Positive: an arrow_as_relation<F> for an IsArrow F is a
    //     binary function — left-total + right-unique by construction.
    using ArrowRel = arrow_as_relation<Identity<int>>;
    STATIC_CHECK(IsBinaryRelation<ArrowRel, int, int>);
    STATIC_CHECK(is_left_total_v<ArrowRel>);
    STATIC_CHECK(is_right_unique_v<ArrowRel>);
    STATIC_CHECK(IsBinaryFunction<ArrowRel, int, int>);

    // Operational witness: identity-as-relation relates (a, b) iff a == b.
    constexpr ArrowRel id_rel{Identity<int>{}};
    CHECK(id_rel(7, 7));
    CHECK_FALSE(id_rel(7, 8));
  }

  SECTION(
      "Function-as-relation negative case: a multi-valued relation "
      "fails right-uniqueness (so IsBinaryFunction does not fire)") {
    // A relation that violates right-uniqueness: for x = 1, both (1,1)
    // and (1,2) are related — so it's a relation but NOT a function.
    struct MultiValued {
      constexpr bool operator()(int x, int y) const {
        return (x == 1 && (y == 1 || y == 2)) || (x == 2 && y == 2);
      }
    };
    STATIC_CHECK(IsBinaryRelation<MultiValued, int, int>);
    // Default: not a function (opt-in traits default to false).  The
    // engineer would NOT register either trait on a multi-valued
    // relation, so IsBinaryFunction does not fire.
    STATIC_CHECK(!is_left_total_v<MultiValued>);
    STATIC_CHECK(!is_right_unique_v<MultiValued>);
    STATIC_CHECK(!IsBinaryFunction<MultiValued, int, int>);
    // Operational witness: the multi-valued behaviour is observable.
    constexpr MultiValued mv{};
    CHECK(mv(1, 1));
    CHECK(mv(1, 2));  // ← right-uniqueness fails here
    CHECK(mv(2, 2));
    CHECK_FALSE(mv(1, 3));
    CHECK_FALSE(mv(2, 1));
  }

  SECTION("Pedagogical synonyms (#459) agree with categorical originals") {
    // Set-theoretic spellings are = aliases of the categorical
    // primitives; either name fires the same opt-in trait.
    STATIC_CHECK(IsInjective<Identity<int>>);
    STATIC_CHECK(IsSurjective<Identity<int>>);
    STATIC_CHECK(IsBijective<Identity<int>>);
    // Cross-check: a clearly non-injective arrow (constant function
    // returning 0) is structurally NOT injective and NOT bijective —
    // the default opt-in traits return false, and the synonyms
    // mechanically agree.  Picked a constant rather than @c
    // arrow([](int x){return x;}) per Copilot review: the latter is
    // structurally identity (injective) and would be misleading as a
    // negative-case witness if property inference is added later.
    auto const constant = arrow<int, int>([](int) { return 0; });
    STATIC_CHECK(IsArrow<std::decay_t<decltype(constant)>>);
    STATIC_CHECK(!IsInjective<std::decay_t<decltype(constant)>>);
    STATIC_CHECK(!IsSurjective<std::decay_t<decltype(constant)>>);
    STATIC_CHECK(!IsBijective<std::decay_t<decltype(constant)>>);
  }

  SECTION("Fish-operator concept tier (#450) — HasArrowComposeOperators") {
    // Categorical arrow composition: f : A → B and g : B → C compose
    // via f >> g : A → C.  The concept pins syntactic availability;
    // identity-on-int is the canonical witness.
    STATIC_CHECK(HasArrowComposeOperators<Identity<int>, Identity<int>>);
    // Negative: a non-arrow type does NOT support categorical composition.
    STATIC_CHECK(!HasArrowComposeOperators<int, int>);
  }
}

TEST_CASE("operator>> auto-wraps an RHS lambda as an arrow",
          "[category][morphism][operator_compose][lambda_adapter]") {
  // Convenience overload: arrow<A, B>(f) >> [](const B& b) { ... }
  // wraps the RHS lambda via arrow<B>(λ) implicitly, tightening call
  // sites that would otherwise spell out the per-site arrow<>{ }
  // wrapping.

  SECTION("Arrow >> lambda type-checks and composes correctly") {
    const auto plus_one = arrow<int, int>([](int x) { return x + 1; });
    const auto chain = plus_one >> [](const int& x) { return x * 2; };

    STATIC_CHECK(IsArrow<decltype(chain)>);
    REQUIRE(chain(3) == 8);    // (3 + 1) * 2 = 8
    REQUIRE(chain(10) == 22);  // (10 + 1) * 2 = 22
  }

  SECTION("Chained Arrow >> lambda >> lambda lifts both lambdas") {
    // Both RHS positions are bare lambdas; the adapter fires twice.
    const auto base = arrow<int, int>([](int x) { return x + 1; });
    const auto chain = base >> [](const int& x) { return x * 2; } >>
                       [](const int& x) { return x - 3; };

    STATIC_CHECK(IsArrow<decltype(chain)>);
    REQUIRE(chain(5) == 9);  // ((5 + 1) * 2) - 3 = 9
  }

  SECTION("RHS lambda's codomain can differ from its domain") {
    // Arrow<int, int> chained with a bool-returning lambda.
    const auto plus_one = arrow<int, int>([](int x) { return x + 1; });
    const auto is_even =
        plus_one >> [](const int& x) -> bool { return x % 2 == 0; };

    STATIC_CHECK(IsArrow<decltype(is_even)>);
    REQUIRE(is_even(1) == true);   // (1 + 1) % 2 == 0
    REQUIRE(is_even(2) == false);  // (2 + 1) % 2 != 0
  }
}
