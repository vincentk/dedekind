/** @file src/test/cpp/modules/dedekind/category/cartesian_test.cpp */
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>
#include <utility>
#include <variant>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE("Discrete: Product and Coproduct (Cartesian Bridge)",
          "[category][discrete][universal]") {
  // Section 2.3.5: Mapping categorical products to C++ primitives

  SECTION("Product (A x B) via std::pair") {
    using P = std::pair<int, bool>;
    STATIC_CHECK(IsProduct<P, int, bool>);

    P p{42, true};
    // Updated to use native members per your preference
    CHECK(p.first == 42);
    CHECK(p.second == true);
  }

  SECTION("Coproduct (A + B) via std::variant") {
    STATIC_CHECK(IsCoproduct<std::variant<int, bool>, int, bool>);

    auto choice_1 = ι_1<int, bool>(10);
    STATIC_CHECK(std::same_as<decltype(choice_1), std::variant<int, bool>>);
    CHECK(std::get<int>(choice_1) == 10);

    auto choice_2 = ι_1<int, int>(10);
    STATIC_CHECK(std::same_as<decltype(choice_2), std::variant<int, int>>);
    CHECK(std::get<0>(choice_2) == 10);

    auto choice_3 = ι_2<int, bool>(true);
    STATIC_CHECK(std::same_as<decltype(choice_3), std::variant<int, bool>>);
    CHECK(std::get<bool>(choice_3) == true);

    auto choice_4 = ι_2<bool, bool>(true);
    STATIC_CHECK(std::same_as<decltype(choice_4), std::variant<bool, bool>>);
    CHECK(std::get<1>(choice_4) == true);
  }
}
TEST_CASE("Cartesian: Labeled Coproduct and Uncurry",
          "[category][cartesian][labeled]") {
  SECTION("Coproduct Mediation (Case Analysis)") {
    // Let arrow() deduce Morphism<int, double> and Morphism<bool, double>
    auto f = arrow([](int i) { return static_cast<double>(i); });
    auto g = arrow([](bool b) { return b ? 1.0 : 0.0; });

    auto cases = mediate_coproduct(f, g);

    // IsArrow now uses your inferred labels
    STATIC_CHECK(IsArrow<decltype(cases)>);
    using Var = std::variant<int, bool>;

    CHECK(cases(Var{42}) == 42.0);
    CHECK(cases(Var{true}) == 1.0);
  }

  SECTION("Uncurry Adjunction Identity") {
    // Instead of move_only_function, we return a nested arrow
    // This allows uncurry to deduce its labels from the nested Morphism
    auto curried_val =
        arrow([](int x) { return arrow([x](int y) { return x * y; }); });

    auto uncurried = uncurry(curried_val);

    STATIC_CHECK(IsArrow<decltype(uncurried)>);
    // Domain should be inferred as std::pair<int, int>
    CHECK(uncurried({6, 7}) == 42);
  }
}

TEST_CASE("CCC: CurryPlus", "[category][cartesian]") {
  // Use the simplest arrow inference
  auto plus_morphism =
      arrow([](std::pair<int, int> p) { return p.first + p.second; });

  auto curried_plus = curry(plus_morphism);

  // curried_plus(5) returns a labeled arrow, so it is callable
  auto add_five = curried_plus(5);

  CHECK(add_five(10) == 15);
  CHECK(curried_plus(10)(20) == 30);
}

TEST_CASE("Cartesian: Product Mediation", "[category][cartesian][labeled]") {
  SECTION("Product Mediation (Pairing)") {
    // 1. Define two morphisms f: X -> A and g: X -> B
    auto f = arrow([](int i) { return i * 2.0; });  // int -> double
    auto g = arrow([](int i) { return i > 0; });    // int -> bool

    // 2. Mediate them into a product arrow <f, g>: int -> (double x bool)
    auto pairing = mediate_product(f, g);

    // 3. Verify it satisfies the skeletal IsArrow concept
    STATIC_CHECK(IsArrow<decltype(pairing)>);

    // 4. Verify Domain and Codomain labels are correct
    using ProductType = std::pair<double, bool>;
    STATIC_CHECK(std::same_as<Dom<decltype(pairing)>, int>);
    STATIC_CHECK(std::same_as<Cod<decltype(pairing)>, ProductType>);

    // 5. Verify the action: <f, g>(x) == (f(x), g(x))
    auto result = pairing(5);
    CHECK(result.first == 10.0);
    CHECK(result.second == true);

    auto result_zero = pairing(-1);
    CHECK(result_zero.first == -2.0);
    CHECK(result_zero.second == false);
  }
}

TEST_CASE("Cartesian: Product Projection Semantics",
          "[category][cartesian][projection]") {
  SECTION("Canonical product projections are type-checked") {
    using P = std::pair<int, bool>;
    auto pi1 = [](const P& p) { return p.first; };
    auto pi2 = [](const P& p) { return p.second; };
    STATIC_CHECK(IsProductProjection<decltype(pi1), P, int>);
    STATIC_CHECK(IsProductProjection<decltype(pi2), P, bool>);
    STATIC_CHECK(IsProjectedProduct<P, int, bool>);
  }

  SECTION("Opt-in operator-> projection exposes functional parts") {
    struct ProductEnvelope {
      std::pair<int, bool> value{7, true};
      constexpr const std::pair<int, bool>* operator->() const {
        return &value;
      }
    };

    STATIC_CHECK(IsProjectedProduct<
                 ProductEnvelope, int, bool,
                 decltype([](const ProductEnvelope& envelope) constexpr
                              -> const std::pair<int, bool>& {
                   return arrow_drill_down(envelope);
                 })>);
  }

  SECTION("π_1 / π_2 / mediate_product exercised at runtime") {
    // Runtime witnesses for the product-side factories re-homed from
    // :cartesian to :limit under #637.  Compile-time witnesses live
    // alongside the definitions; this exercises the bodies at runtime
    // so coverage instrumentation sees the move.

    std::pair<int, bool> p{42, true};
    CHECK(π_1<int, bool>(p) == 42);
    CHECK(π_2<int, bool>(p) == true);

    // mediate_product: given f: X → A and g: X → B, build the unique
    // u: X → A × B such that f = π_1 ∘ u and g = π_2 ∘ u.
    auto f = arrow<int, int>([](int x) { return x * 2; });
    auto g = arrow<int, bool>([](int x) { return x > 0; });
    auto u = mediate_product(f, g);

    auto y = u(7);
    CHECK(π_1<int, bool>(y) == 14);    // (π_1 ∘ u)(7) = f(7) = 14
    CHECK(π_2<int, bool>(y) == true);  // (π_2 ∘ u)(7) = g(7) = true

    auto y_neg = u(-3);
    CHECK(π_1<int, bool>(y_neg) == -6);     // f(-3) = -6
    CHECK(π_2<int, bool>(y_neg) == false);  // g(-3) = false
  }
}

TEST_CASE(
    "Cartesian: Relation classification — equivalence / directed / "
    "undirected graph axes (#464)",
    "[category][cartesian][relation][equivalence][graph]") {
  SECTION(
      "std::equal_to<int> is the canonical equivalence relation: "
      "reflexive, symmetric, transitive (registered upstream in :cartesian)") {
    STATIC_CHECK(IsBinaryRelation<std::equal_to<int>, int, int>);
    STATIC_CHECK(is_reflexive_relation_v<std::equal_to<int>>);
    STATIC_CHECK(is_symmetric_relation_v<std::equal_to<int>>);
    STATIC_CHECK(is_transitive_relation_v<std::equal_to<int>>);
    STATIC_CHECK(IsEquivalenceRelation<std::equal_to<int>, int>);
    // Equivalence on V × V is structurally an undirected graph (the
    // identity loops + a-loops only when a == b).
    STATIC_CHECK(IsUndirectedGraph<std::equal_to<int>, int>);

    // Operational witness: equality reflects, symmetrises, transits.
    constexpr std::equal_to<int> eq{};
    CHECK(eq(7, 7));
    CHECK(eq(3, 3));
    CHECK(eq(3, 4) == eq(4, 3));  // symmetry
    CHECK_FALSE(eq(3, 4));
  }

  SECTION(
      "Negative case: std::less<int> is a binary relation but NOT an "
      "equivalence — it is reflexive-failure (a < a is false), symmetry-"
      "failure (a < b ≠ b < a), and antisymmetric instead.  Default "
      "trait registration leaves it unclassified; this test pins the "
      "negative facts as type-checked documentation") {
    STATIC_CHECK(IsBinaryRelation<std::less<int>, int, int>);
    STATIC_CHECK(IsDirectedGraph<std::less<int>, int>);
    // No traits opt-in by default — std::less is not an equivalence.
    STATIC_CHECK(!is_reflexive_relation_v<std::less<int>>);
    STATIC_CHECK(!is_symmetric_relation_v<std::less<int>>);
    STATIC_CHECK(!IsEquivalenceRelation<std::less<int>, int>);
    STATIC_CHECK(!IsUndirectedGraph<std::less<int>, int>);
  }

  SECTION(
      "Negative case: a multi-valued relation { (1,1), (1,2), (2,2) } "
      "is a binary relation and a (degenerate) directed graph but NOT "
      "an equivalence (no symmetry without (2,1)) and NOT a function "
      "(fails right-uniqueness)") {
    struct R {
      constexpr bool operator()(int x, int y) const {
        return (x == 1 && (y == 1 || y == 2)) || (x == 2 && y == 2);
      }
    };
    STATIC_CHECK(IsBinaryRelation<R, int, int>);
    STATIC_CHECK(IsDirectedGraph<R, int>);
    STATIC_CHECK(!IsEquivalenceRelation<R, int>);
    STATIC_CHECK(!IsBinaryFunction<R, int, int>);
  }
}

TEST_CASE(
    "Cartesian: IsArrowExponential — IsExponential + IsArrow refinement (#706)",
    "[category][cartesian][exponential][isarrow]") {
  /** @brief @c IsArrowExponential refines @c IsExponential to additionally
   *         require the project's @c IsArrow morphism discipline (explicit
   *         @c Domain / @c Codomain typedefs).  Function-space inhabitants
   *         satisfy @c IsExponential but @b not @c IsArrowExponential —
   *         the discipline is honest about the distinction. */

  // Negative witness: std::function<B(A)> is IsExponential but not IsArrow.
  STATIC_CHECK(IsExponential<std::function<bool(int)>, int, bool>);
  STATIC_CHECK_FALSE(IsArrowExponential<std::function<bool(int)>, int, bool>);

  // Positive witness: an arrow-shaped exponential from the arrow<A, B> factory
  // satisfies both — the project's canonical arrow shape.
  using ProjectArrow = Exponential<int, bool>;
  STATIC_CHECK(IsExponential<ProjectArrow, int, bool>);
  STATIC_CHECK(IsArrow<ProjectArrow>);
  STATIC_CHECK(IsArrowExponential<ProjectArrow, int, bool>);
}
