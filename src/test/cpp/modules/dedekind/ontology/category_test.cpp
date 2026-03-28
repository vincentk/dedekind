#include <catch2/catch_test_macros.hpp>
#include <functional>  // for std::plus
#include <type_traits>

import dedekind.ontology;

using namespace dedekind::ontology;
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <functional>

// Assuming the dedekind.ontology:species and :category are imported
// import dedekind.ontology;

TEST_CASE("Ontology: Arrow Factory Verification",
          "[ontology][species][category]") {
  SECTION("6. Functorial Composition (fmap)") {
    auto increment = [](int x) { return x + 1; };
    auto double_it = [](int x) { return x * 2; };

    // Pass the lambda into fmap first
    auto result = 5 >> into<Box> >> fmap<Box>(endo<int>(increment)) >>
                  fmap<Box>(endo<int>(double_it));

    CHECK(result == Box<int>{12});

    // // Proof of the Functor Law
    // auto composed_pure = [&](int x) { return double_it(increment(x)); };

    // auto left_side = (5 >> into<Box>) << fmap(composed_pure);
    // auto right_side = ((5 >> into<Box>) << fmap(increment)) <<
    // fmap(double_it);

    // CHECK(left_side == right_side);
  }

  SECTION("6. Composite Morphism (The Refined Way)") {
    // Convert plain lambdas into tagged endomorphisms:
    auto f = endo<int>([](int x) { return x + 1; });
    auto g = endo<int>([](int x) { return x * 2; });

    // Morphism composition: (h = g . f):
    auto h = f >> g;

    // Lifting into the Box and applying the composed morphism:
    auto result = 5 >> into<Box> >> fmap<Box>(h);

    CHECK(result == Box<int>{12});
  }

  SECTION("5. Monadic Chain Composition (The Kleisli Flow)") {
    // Define two monadic arrows (Species -> Boxed Species)
    auto f = [](int x) { return pure(x + 1); };
    auto g = [](int x) { return pure(x * 2); };

    // Chain composition via Bind (>>=)
    // Starting with a raw value, lifting it, then chaining the monadic arrows.
    auto x = (5 >> into<Box> >>= f) >>= g;

    // (5 + 1) * 2 = 12, wrapped in a Box
    CHECK(x == Box<int>{12});

    // Verification of the Monadic Associativity Law:
    // (m >>= f) >>= g  is equivalent to  m >>= (x => f(x) >>= g)
    auto lh = (5 >> into<Box> >>= f) >>= g;

    auto rh = 5 >> into<Box> >>= [&](int x) { return f(x) >>= g; };

    CHECK(lh == rh);
    STATIC_CHECK(std::same_as<decltype(x), Box<int>>);
  }

  SECTION("Standard Function Object Tagging (Endomorphisms)") {
    using Negate = std::negate<int>;
    // Testing the 'endo' factory which implies Domain == Codomain
    using TaggedNegate = decltype(dedekind::ontology::endo<int>(Negate{}));

    STATIC_CHECK(std::same_as<typename TaggedNegate::Domain, int>);
    STATIC_CHECK(std::same_as<typename TaggedNegate::Codomain, int>);

    // Operational check
    TaggedNegate f{Negate{}};
    CHECK(f(42) == -42);
  }

  SECTION("Cross-Species Lambda Tagging (Z -> B)") {
    auto is_positive_lambda = [](int x) { return x > 0; };

    // Testing the explicit 'arrow' factory for Domain -> Codomain mapping
    using TaggedIsPositive =
        decltype(dedekind::ontology::arrow<int, bool>(is_positive_lambda));

    STATIC_CHECK(std::same_as<typename TaggedIsPositive::Domain, int>);
    STATIC_CHECK(std::same_as<typename TaggedIsPositive::Codomain, bool>);

    // Operational check
    TaggedIsPositive g{is_positive_lambda};
    CHECK(g(10) == true);
    CHECK(g(-5) == false);
  }

  SECTION("Categorical Concept Fulfillment") {
    auto logic_gate = [](int x) { return x != 0; };
    using TaggedGate =
        decltype(dedekind::ontology::arrow<int, bool>(logic_gate));

    // Final proof: Does the factory output satisfy the foundational IsArrow
    // concept?
    STATIC_CHECK(dedekind::ontology::IsArrow<TaggedGate, int, bool>);
  }
}

TEST_CASE("Level 0 Final Proof: The Box Monad & Comonad",
          "[ontology][category][highway]") {
  SECTION("1. Structural Identity (Monad/Comonad)") {
    // Verify the dual concepts at compile-time
    static_assert(IsMonad<Box, int, std::plus<int>>,
                  "Ontology: Box must be recognized as a formal Monad.");

    static_assert(IsComonad<Box, int, std::plus<int>>,
                  "Ontology: Box must be recognized as a formal Comonad.");

    SUCCEED("Box satisfies the dual algebraic requirements.");
  }

  SECTION("2. Monadic Highway: Push (η) and Collapse (μ)") {
    // The "Push" (η)
    auto pushed = 42 >> into<Box>;
    REQUIRE(pushed == Box<int>{42});

    // The "Collapse" (μ) via named variable (Lvalue)
    auto nested = pushed >> into<Box>;  // Box<Box<int>>
    auto collapsed = nested >> join<Box>;

    REQUIRE(collapsed == pushed);

    // Static Proof (The Highway Silk)
    static_assert(
        (42 >> into<Box> >> into<Box> >> join<Box>) == (42 >> into<Box>),
        "Ontology: Monadic Join (μ) failed the Action Proof.");
  }

  SECTION("3. The Logic Highway: Chaining (>>=)") {
    // The "Drive" (Bind)
    // We take a box, apply a function, and return a new box context.
    auto result = 21 >> into<Box> >>= [](int x) { return pure(x * 2); };

    REQUIRE(result == Box<int>{42});

    static_assert(
        (21 >> into<Box> >>= [](int x) { return pure(x * 2); }) == Box<int>{42},
        "Ontology: Monadic Bind (>>=) failed the Action Proof.");
  }

  SECTION("4. Comonadic Highway: Pull (ε) and Duplicate (δ)") {
    auto start = 42 >> into<Box>;

    // The "Duplicate" (δ) - Pulling a context out of a context
    auto duplicated = start << duplicate<Box>;
    REQUIRE(duplicated.value.value == 42);

    // The "Extract" (ε) - Pulling the species out of the context
    auto pulled = duplicated << extract<Box>;  // Pulling inner box
    int value = pulled << extract<Box>;        // Pulling raw int

    REQUIRE(value == 42);

    // The Combined Round-Trip Proof
    // Push -> Duplicate -> Extract -> Extract -> Result
    static_assert(
        (42 >> into<Box> << duplicate<Box> << extract<Box> << extract<Box>) ==
            42,
        "Ontology: Comonadic round-trip failed the Action Proof.");
  }
}
