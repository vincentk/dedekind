/** @file test/cpp/modules/dedekind/category/jlt_listings_test.cpp
 *
 * The listings of the Jlt paper (docs/paper/jlt/jlt.tex, Sections 1 to 4),
 * verbatim up to formatting,
 * so that CI holds the paper to the code.  The listings declare their own
 * unit and laws, so they live in their own namespace; the TEST_CASE then
 * checks that the tree's unit<T>(Op{}) and laws agree with every fact the
 * listings certify.
 */
#include <algorithm>
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <ranges>
#include <string>
#include <utility>

import dedekind.category;

namespace jlt_listing {
using std::size_t;

// Section 1: declared, entailed, inferred.
namespace intro {
template <typename F, typename G>
struct Compose {
  F f;
  G g;
  template <typename X>
    requires std::invocable<F, X> &&
             std::invocable<G, std::invoke_result_t<F, X>>
  constexpr auto operator()(X x) const {
    return g(f(x));
  }
};

template <typename F>
constexpr bool involutive(F) {
  return false;
}
template <>
constexpr bool involutive(std::logical_not<bool>) {
  return true;
}
template <>
constexpr bool involutive(std::bit_not<size_t>) {
  return true;
}
template <>
constexpr bool involutive(std::negate<size_t>) {
  return true;
}

template <typename F>
constexpr bool bijective(F f) {
  return involutive(f);
}
template <typename F, typename G>
constexpr bool bijective(Compose<F, G> c) {
  return bijective(c.f) && bijective(c.g);
}

static_assert(bijective(Compose{std::bit_not<size_t>{},
                                std::negate<size_t>{}}));
static_assert(!involutive(Compose{std::logical_not<bool>{},
                                  std::logical_not<bool>{}}));
static_assert(!involutive(std::negate<int>{}));
}  // namespace intro

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
  return std::ranges::fold_left(std::views::repeat(x, n), unit<T>(Op{}), Op{});
}

static_assert(times<size_t>(7, 6) == 42);

// Section 4, two binding times: fuel, and running out of it.
template <typename P>
concept IsPredicate = std::semiregular<P> && requires(const P p, size_t n) {
  { p(n) } -> std::same_as<bool>;
};

template <IsPredicate P>
constexpr std::pair<size_t, bool> advance(std::pair<size_t, bool> s, size_t i) {
  return s.second ? s : std::pair{i, P{}(i)};
}

template <IsPredicate P>
constexpr std::optional<size_t> first(size_t fuel) {
  std::pair<size_t, bool> s =
      std::ranges::fold_left(std::views::iota(size_t{0}, fuel),
                             std::pair{size_t{0}, false}, advance<P>);
  return s.second ? std::optional{s.first} : std::nullopt;
}

struct Over1000 {
  constexpr bool operator()(size_t n) const { return n * n > 1000; }
};

static_assert(first<Over1000>(40) == 32);
static_assert(!first<Over1000>(30));

}  // namespace jlt_listing

TEST_CASE("Jlt paper listings: the unit and the laws, read at runtime",
          "[category][jlt][paper]") {
  using std::size_t;
  namespace L = jlt_listing;
  namespace C = dedekind::category;

  SECTION("Section 1: the same laws, read at runtime") {
    namespace I = jlt_listing::intro;
    I::Compose<std::bit_not<size_t>, std::negate<size_t>> c{};
    CHECK(I::bijective(c));
    CHECK(!I::involutive(c));
    CHECK(c(size_t{5}) == 6);
    CHECK(!I::involutive(std::negate<int>{}));
  }

  SECTION("Section 1: the tree certifies the same involutions") {
    STATIC_CHECK(C::IsInvolution<std::logical_not<bool>, bool>);
    STATIC_CHECK(C::IsInvolution<std::bit_not<size_t>, size_t>);
    STATIC_CHECK(C::IsInvolution<std::negate<size_t>, size_t>);
    STATIC_CHECK(!C::IsInvolution<std::negate<int>, int>);
    CHECK(C::Reverse{}(C::Reverse{}(std::string{"posture"})) == "posture");
  }

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

  SECTION(
      "Section 4: the same fold, with a fuel it did not have at compile "
      "time") {
    for (size_t fuel : {size_t{30}, size_t{32}, size_t{33}, size_t{40}}) {
      const std::optional<size_t> found = L::first<L::Over1000>(fuel);
      CHECK(found.has_value() == (fuel >= 33));
      if (found) CHECK(*found == 32);
    }
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

  SECTION("The tree's shapes accept the listings' semirings") {
    STATIC_CHECK(
        C::IsSemiring<size_t, std::plus<size_t>, std::multiplies<size_t>>);
    STATIC_CHECK(
        C::IsSemiring<bool, std::logical_or<bool>, std::logical_and<bool>>);
  }
}
