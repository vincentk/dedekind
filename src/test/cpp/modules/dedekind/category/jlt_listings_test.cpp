/** @file test/cpp/modules/dedekind/category/jlt_listings_test.cpp
 *
 * The listings of the Jlt paper (docs/paper/jlt/jlt.tex, Sections 2 to 4),
 * verbatim up to formatting,
 * so that CI holds the paper to the code.  The listings declare their own
 * unit and laws, so they live in their own namespace; the TEST_CASE then
 * checks that the tree's unit<T>(Op{}) and laws agree with every fact the
 * listings certify.
 */
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <utility>

import dedekind.category;

namespace jlt_listing {
using std::size_t;

// Listing 1, top: the shape, unpopulated.
template <typename T, typename Op>
constexpr T unit(Op) = delete;

template <typename T, typename Op>
concept IsMonoid = std::semiregular<T> && requires(const T a, const T b) {
  { Op{}(a, b) } -> std::same_as<T>;
  { unit<T>(Op{}) } -> std::same_as<T>;
};

template <typename T, typename Op = std::plus<T>>
  requires IsMonoid<T, Op>
constexpr T twice(T x) {
  return Op{}(x, x);
}

// Listing 1, middle: bool.
template <>
constexpr bool unit<bool>(std::logical_and<bool>) {
  return true;
}
template <>
constexpr bool unit<bool>(std::logical_or<bool>) {
  return false;
}

static_assert(IsMonoid<bool, std::logical_and<bool>>);
static_assert(IsMonoid<bool, std::logical_or<bool>>);
static_assert(!IsMonoid<bool, std::plus<bool>>);

// Listing 1, bottom: size_t.
template <>
constexpr size_t unit<size_t>(std::plus<size_t>) {
  return 0;
}
template <>
constexpr size_t unit<size_t>(std::multiplies<size_t>) {
  return 1;
}
template <>
constexpr size_t unit<size_t>(std::bit_or<size_t>) {
  return 0;
}
template <>
constexpr size_t unit<size_t>(std::bit_and<size_t>) {
  return SIZE_MAX;
}

static_assert(IsMonoid<size_t, std::plus<size_t>>);
static_assert(IsMonoid<size_t, std::multiplies<size_t>>);
static_assert(IsMonoid<size_t, std::bit_or<size_t>>);
static_assert(IsMonoid<size_t, std::bit_and<size_t>>);

static_assert(twice<size_t>(21) == 42);
static_assert(twice<size_t, std::multiplies<size_t>>(21) == 441);

// Listing 2: laws as functions of the operation; the semiring shape.
template <typename T, typename Op>
constexpr bool commutative(Op) = delete;
template <typename T, typename Mul, typename Add>
constexpr bool distributive(Mul, Add) = delete;

template <typename T, typename Op>
concept IsCommutativeMonoid = IsMonoid<T, Op> && commutative<T>(Op{});

template <typename T, typename Add, typename Mul>
concept IsSemiring = IsCommutativeMonoid<T, Add> && IsMonoid<T, Mul> &&
                     distributive<T>(Mul{}, Add{});

template <>
constexpr bool commutative<size_t>(std::plus<size_t>) {
  return true;
}
template <>
constexpr bool commutative<size_t>(std::bit_or<size_t>) {
  return true;
}
template <>
constexpr bool commutative<bool>(std::logical_or<bool>) {
  return true;
}
template <>
constexpr bool distributive<size_t>(std::multiplies<size_t>,
                                    std::plus<size_t>) {
  return true;
}
template <>
constexpr bool distributive<size_t>(std::bit_and<size_t>, std::bit_or<size_t>) {
  return true;
}
template <>
constexpr bool distributive<bool>(std::logical_and<bool>,
                                  std::logical_or<bool>) {
  return true;
}

static_assert(IsSemiring<size_t, std::plus<size_t>, std::multiplies<size_t>>);
static_assert(IsSemiring<size_t, std::bit_or<size_t>, std::bit_and<size_t>>);
static_assert(IsSemiring<bool, std::logical_or<bool>, std::logical_and<bool>>);
static_assert(!IsCommutativeMonoid<size_t, std::multiplies<size_t>>);

// Section 2, the posture: one panel per rule.
static_assert(std::semiregular<size_t>);
static_assert(std::semiregular<std::pair<size_t, bool>>);

constexpr size_t square(size_t x) { return x * x; }

static_assert(square(12) == 144);

template <typename T>
constexpr T twice_loose(T x) {
  return x + x;
}

static_assert(twice_loose(std::string{"ab"}) == "abab");

template <typename T, typename Op = std::plus<T>>
  requires IsMonoid<T, Op>
constexpr T times(T x, size_t n) {
  T acc = unit<T>(Op{});
  for (size_t i = 0; i < n; ++i) acc = Op{}(acc, x);
  return acc;
}

static_assert(times<size_t>(7, 6) == 42);

// Section 4, the laws of functions: Kleene's three values and the profile
// with its engine; two arrows and a former as programs; the laws declared at
// the leaves, inferred at the former, read by a concept and by a fold.
namespace laws {
enum class Ternary { False, Unknown, True };
constexpr Ternary both(Ternary a, Ternary b) { return a < b ? a : b; }
constexpr Ternary either(Ternary a, Ternary b) { return a < b ? b : a; }

struct Variance {
  Ternary monotone, antitone;
  constexpr bool operator==(const Variance&) const = default;
};

constexpr Variance compose(Variance a, Variance b) {
  return {either(both(a.monotone, b.monotone), both(a.antitone, b.antitone)),
          either(both(a.monotone, b.antitone), both(a.antitone, b.monotone))};
}

struct Same {
  using Domain = bool;
  using Codomain = bool;
  constexpr bool operator()(bool x) const { return x; }
};
struct Negation {
  using Domain = bool;
  using Codomain = bool;
  constexpr bool operator()(bool x) const { return !x; }
};
template <typename F, typename G>
struct Compose {
  using Domain = typename F::Domain;
  using Codomain = typename G::Codomain;
  F f;
  G g;
  constexpr Codomain operator()(Domain x) const { return g(f(x)); }
};

using Order = std::less_equal<bool>;

template <typename F, typename Op>
constexpr Variance variance(F, Op) {
  return {Ternary::Unknown, Ternary::Unknown};
}
template <>
constexpr Variance variance(Same, Order) {
  return {Ternary::True, Ternary::Unknown};
}
template <>
constexpr Variance variance(Negation, Order) {
  return {Ternary::Unknown, Ternary::True};
}
template <typename F, typename G, typename Op>
constexpr Variance variance(Compose<F, G> c, Op op) {
  return compose(variance(c.f, op), variance(c.g, op));
}

template <typename F, typename Op>
concept IsMonotone = variance(F{}, Op{}).monotone == Ternary::True;

static_assert(IsMonotone<Compose<Negation, Negation>, Order>);
static_assert(!IsMonotone<Compose<Same, Negation>, Order>);
static_assert(variance(Compose<Same, Negation>{}, Order{}) ==
              Variance{Ternary::Unknown, Ternary::True});

enum class Step { Same, Negation };
template <size_t N>
constexpr Variance variance(std::array<Step, N> steps, Order op) {
  Variance acc = variance(Same{}, op);
  for (size_t i = 0; i < N; ++i)
    acc = compose(acc, steps[i] == Step::Negation ? variance(Negation{}, op)
                                                  : variance(Same{}, op));
  return acc;
}

static_assert(variance(std::array{Step::Negation, Step::Negation}, Order{}) ==
              variance(Compose<Negation, Negation>{}, Order{}));
}  // namespace laws
}  // namespace jlt_listing

TEST_CASE("Jlt paper listings: the unit and the laws, read at runtime",
          "[category][jlt][paper]") {
  using std::size_t;
  namespace L = jlt_listing;
  namespace C = dedekind::category;

  SECTION("Listing 1: the same unit, called on a value") {
    std::plus<size_t> plus;
    std::bit_and<size_t> bit_and;
    CHECK(L::unit<size_t>(plus) == 0);
    CHECK(L::unit<size_t>(bit_and) == SIZE_MAX);
    CHECK(L::twice<size_t>(21) == 42);
  }

  SECTION("Section 2: the rules' functions, called at runtime") {
    size_t twelve = 12;
    CHECK(L::square(twelve) == 144);
    CHECK(L::times<size_t>(7, twelve) == 84);
  }

  SECTION("The tree agrees with every unit the listings declare") {
    CHECK(C::unit<bool>(std::logical_and<bool>{}) ==
          L::unit<bool>(std::logical_and<bool>{}));
    CHECK(C::unit<bool>(std::logical_or<bool>{}) ==
          L::unit<bool>(std::logical_or<bool>{}));
    CHECK(C::unit<size_t>(std::plus<size_t>{}) ==
          L::unit<size_t>(std::plus<size_t>{}));
    CHECK(C::unit<size_t>(std::multiplies<size_t>{}) ==
          L::unit<size_t>(std::multiplies<size_t>{}));
    CHECK(C::unit<size_t>(std::bit_or<size_t>{}) ==
          L::unit<size_t>(std::bit_or<size_t>{}));
    CHECK(C::unit<size_t>(std::bit_and<size_t>{}) ==
          L::unit<size_t>(std::bit_and<size_t>{}));
  }

  SECTION("The tree knows every law the listings certify") {
    CHECK(C::commutative<size_t>(std::plus<size_t>{}));
    CHECK(C::commutative<size_t>(std::bit_or<size_t>{}));
    CHECK(C::commutative<bool>(std::logical_or<bool>{}));
    CHECK(C::distributive<size_t>(std::multiplies<size_t>{},
                                  std::plus<size_t>{}));
    CHECK(
        C::distributive<size_t>(std::bit_and<size_t>{}, std::bit_or<size_t>{}));
    CHECK(C::distributive<bool>(std::logical_and<bool>{},
                                std::logical_or<bool>{}));
  }

  SECTION("Section 4: the laws of a pipeline built at runtime") {
    namespace W = jlt_listing::laws;
    std::array<W::Step, 3> steps{W::Step::Negation, W::Step::Same,
                                 W::Step::Negation};
    CHECK(W::variance(steps, W::Order{}).monotone == W::Ternary::True);
    CHECK(W::variance(steps, W::Order{}).antitone == W::Ternary::Unknown);
  }

  SECTION("Section 4: the tree's laws agree with the listing's") {
    using Neg = C::NegationArrow<C::Boole>;
    using Id = C::Identity<bool>;
    constexpr C::Compose<Neg, Neg> nn{Neg{}, Neg{}};
    constexpr C::Compose<Id, Neg> sn{Id{}, Neg{}};
    STATIC_CHECK(C::monotone(nn) == C::Ternary::True);
    STATIC_CHECK(C::antitone(nn) == C::Ternary::Unknown);
    STATIC_CHECK(C::monotone(sn) == C::Ternary::Unknown);
    STATIC_CHECK(C::antitone(sn) == C::Ternary::True);
    STATIC_CHECK(C::IsMonotone<C::Compose<Neg, Neg>>);
    STATIC_CHECK(!C::IsMonotone<C::Compose<Id, Neg>>);
    STATIC_CHECK(C::injective(C::Compose<Id, Id>{Id{}, Id{}}) ==
                 C::Ternary::True);
    STATIC_CHECK(C::injective(nn) == C::Ternary::Unknown);
    STATIC_CHECK(C::arrow_laws(nn)[C::ArrowLaw::Monotone] == C::monotone(nn));
    CHECK(C::monotone(nn) == C::Ternary::True);
  }

  SECTION("The tree's shapes accept the listings' semirings") {
    STATIC_CHECK(
        C::IsSemiring<size_t, std::plus<size_t>, std::multiplies<size_t>>);
    STATIC_CHECK(
        C::IsSemiring<bool, std::logical_or<bool>, std::logical_and<bool>>);
  }
}
