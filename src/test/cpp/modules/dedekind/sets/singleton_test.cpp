#include <catch2/catch_test_macros.hpp>
#include <concepts>     // std::same_as
#include <type_traits>  // std::decay_t
import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Sets: Singleton Final Proof: The Highway",
          "[sets][singleton][highway]") {
  SECTION("1. The Singleton Lifting") {
    auto atom = singleton(42);
    REQUIRE(atom(42) == true);
  }

  SECTION("2. The Pull from the Identity (ε)") {
    auto atom = singleton(42);
    int value = origin(atom);

    REQUIRE(value == 42);
    static_assert(origin(singleton(7)) == 7, "The Round-trip Axiom.");
  }
}

TEST_CASE("Sets: Singleton Acceptance", "[sets][singleton][acceptance]") {
  auto _s = ι<size_t>(42);
  STATIC_REQUIRE(IsSet<decltype(_s)>);
  SECTION("Construction") {
    INFO("Successful membership test.");
    REQUIRE(_s(std::size_t{42}));
    INFO("Failed membership test.");
    REQUIRE(!_s(std::size_t{4}));
    INFO("A foreign type compares in the common type, never by narrowing.");
    STATIC_REQUIRE(!Singleton<int>{1}(1.5));
    STATIC_REQUIRE(Singleton<int>{1}(1.0));
  }
  SECTION("Cardinality") {
    REQUIRE(_s.size() == 1);
    REQUIRE(_s.cardinality() == Finite{});
    STATIC_REQUIRE(
        std::same_as<typename decltype(_s)::cardinality_type, Finite>);
  }
  SECTION("Complement is the reducer's Not node; ~~s is s") {
    STATIC_REQUIRE(IsSetObject<decltype(~_s)>);
    INFO("Inverted membership test vis-a-vis base set.");
    REQUIRE((~_s)(std::size_t{4}));
    REQUIRE_FALSE((~_s)(std::size_t{42}));
    INFO("Complement is an involution: the Not node peels structurally.");
    STATIC_REQUIRE(std::same_as<decltype(~~_s), decltype(_s)>);
    REQUIRE((~~_s)(std::size_t{42}));
    REQUIRE(~~_s == _s);
    INFO(
        "On the two-element carrier the complement of a point is the other "
        "point (found from sets alone, no order namespace in scope).");
    STATIC_REQUIRE(
        std::same_as<decltype(~Singleton<bool>{true}), Singleton<bool>>);
    STATIC_REQUIRE((~Singleton<bool>{true})(false));
    STATIC_REQUIRE(!(~Singleton<bool>{true})(true));
  }
  SECTION("Intersections") {
    // FIXME(#685): structural identity ({a}∩{a} == {a}, {a}∩¬{a} == Ø) is
    // tracked there; today the self-meet is the reducer's Meet node of two
    // value-carrying points, witnessed by membership.
    INFO("The intersection of a set with itself is a fixed point.");
    REQUIRE((_s & _s)(std::size_t{42}));
    REQUIRE_FALSE((_s & _s)(std::size_t{4}));
  }
  SECTION("Union") {
    // The union is the recoverable Join node, so it is tested by MEMBERSHIP
    // and by recovering both atoms from the type.
    INFO(
        "The union of a set with itself is a fixed point: {42} ∪ {42} = {42}.");
    REQUIRE((_s | _s)(42));
    REQUIRE(!(_s | _s)(std::size_t{4}));
    // Two DISTINCT atoms: {42} ∪ {7} = {42, 7} — contains both, nothing else.
    // (Regression guard: the old lvalue comprehension-over-*this wrongly gave
    // {42} here; the recoverable OrPredicate is correct for distinct atoms.)
    const auto _t = ι<size_t>(7);
    REQUIRE((_s | _t)(std::size_t{42}));
    REQUIRE((_s | _t)(std::size_t{7}));
    REQUIRE(!(_s | _t)(std::size_t{4}));
    // The STRUCTURAL contract (#691/#842), which membership alone does not pin:
    // the union is a RECOVERABLE Join node whose two atoms survive in the type
    // (an opaque predicate with the same membership would pass the checks
    // above).  Pin the result type and recover both pivots.
    using UnionT = std::decay_t<decltype(_s | _t)>;
    STATIC_REQUIRE(
        std::same_as<
            UnionT, Comprehension<𝔸<size_t, Boole>,
                                  Join<Singleton<size_t>, Singleton<size_t>>>>);
    REQUIRE(origin((_s | _t).predicate.lhs) == 42);
    REQUIRE(origin((_s | _t).predicate.rhs) == 7);
  }
}

/**
 * @brief Functor Highway — exercises the @c Singleton monad-bind
 *        + co-monad-extract pipeline (#687).
 *
 * @details Rewritten from the original `into<Singleton>` /
 *          `extract<Singleton>` factory syntax to the current
 *          surface: `singleton(value)` for η, `origin(s)` for ε,
 *          `s >>= f` for Kleisli bind (all exported by `:sets:singleton`).
 *          The Set-monad structure lives directly on `Singleton`; this
 *          TEST_CASE exercises the composition behaviour at the value level.
 *          (The former `singleton_functor` witness struct was retired: it was
 *          unused, and its codomain `category::Set<Singleton<T>>` was the
 *          project's lone category-of-sets-as-objects --- a set is an object of
 *          Set, not itself a category.)
 */
TEST_CASE("Sets: Composition of Operations: The Functor Highway",
          "[sets][composition][monad]") {
  SECTION("1. Forward Monadic Composition (Bind)") {
    auto plus_one = [](int x) { return singleton(x + 1); };
    auto times_two = [](int x) { return singleton(x * 2); };

    // η(5) >>= plus_one >>= times_two   ≡   singleton((5+1)*2) = {12}.
    // Explicit parens because `>>=` is right-associative in C++; the
    // monad-bind reading wants left-fold.
    auto result_set = (singleton(5) >>= plus_one) >>= times_two;

    REQUIRE(result_set(12) == true);
    REQUIRE(result_set(6) == false);
  }

  SECTION("2. Compile-Time Semantic Mapping of Composition") {
    // Zero-overhead claim: the composition resolves to a constant 12.
    // Explicit left-fold parens on `>>=` (right-associative in C++).
    static_assert(origin((singleton(5) >>= [](int x) {
      return singleton(x + 1); }) >>=
                   [](int x) {
      return singleton(x * 2);
                   })) == 12,
                  "The Composition Axiom must be resolved at compile-time.");
  }
}

TEST_CASE("Sets: Comprehension runtime membership (χ coverage)",
          "[sets][comprehension][runtime]") {
  // Runtime companion to the STATIC_REQUIRE witnesses: call the comprehension's
  // characteristic map (operator()) at RUNTIME so its L::AND-and-lift body is
  // exercised, not only compile-time-asserted.
  auto _s = ι<size_t>(42);
  const auto self_meet = _s & _s;     // the reducer's Meet node of two points
  CHECK(self_meet(size_t{42}));       // 42 ∈ {42} ∧ 42 ∈ {42}
  CHECK_FALSE(self_meet(size_t{7}));  // 7 ∉ {42}
}
