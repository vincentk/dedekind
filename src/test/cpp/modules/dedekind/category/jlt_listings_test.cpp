/** @file test/cpp/modules/dedekind/category/jlt_listings_test.cpp
 *
 * The listings of the Jlt paper (docs/paper/jlt/jlt.tex),
 * verbatim up to formatting and qualification (a listing may say
 * `using namespace std`),
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
#include <limits>
#include <optional>
#include <ranges>
#include <string>
#include <utility>

import dedekind.category;

namespace jlt_listing {
using std::size_t;
using namespace std::string_literals;

// Listing 1, top: the shape, unpopulated.
template <typename T, typename Op>
constexpr T unit(Op) = delete;

template <typename T, typename Op>
constexpr bool associative(Op) = delete;

// Listing 3, left: an instance for a family, before the shape (Section 6).
template <std::unsigned_integral T>
constexpr T unit(std::plus<T>) {
  return 0;
}
template <std::unsigned_integral T>
constexpr bool associative(std::plus<T>) {
  return true;
}

template <typename T, typename Op>
concept IsMonoid = std::regular<T> && requires(const T a, const T b) {
  { Op{}(a, b) } -> std::same_as<T>;
  { unit<T>(Op{}) } -> std::same_as<T>;
} && associative<T>(Op{});

// Listing 3, right: std::string, and concat at size_t and at std::string.
template <>
constexpr std::string unit<std::string>(std::plus<std::string>) {
  return {};
}
template <>
constexpr bool associative<std::string>(std::plus<std::string>) {
  return true;
}

template <>
constexpr size_t unit<size_t>(std::multiplies<size_t>) {
  return 1;
}
template <>
constexpr bool associative<size_t>(std::multiplies<size_t>) {
  return true;
}

template <typename T, typename Op = std::plus<T>, std::ranges::input_range R>
  requires IsMonoid<T, Op> && std::same_as<std::ranges::range_value_t<R>, T>
constexpr T concat(R words) {
  return std::ranges::fold_left(std::move(words), unit<T>(Op{}), Op{});
}

static_assert(concat<size_t>(std::array{1uz, 2uz, 3uz}) == 6);
static_assert(concat<size_t, std::multiplies<size_t>>(std::array{2uz, 3uz,
                                                                 7uz}) == 42);
static_assert(concat<std::string>(std::array{"ab"s, "c"s, "d"s}) == "abcd");
static_assert(concat<size_t>(std::array<size_t, 0>{}) == 0);

// Listing 1, middle: bool.
template <>
constexpr bool unit<bool>(std::logical_and<bool>) {
  return true;
}
template <>
constexpr bool unit<bool>(std::logical_or<bool>) {
  return false;
}

template <>
constexpr bool associative<bool>(std::logical_and<bool>) {
  return true;
}
template <>
constexpr bool associative<bool>(std::logical_or<bool>) {
  return true;
}

static_assert(IsMonoid<bool, std::logical_and<bool>>);
static_assert(IsMonoid<bool, std::logical_or<bool>>);
static_assert(!IsMonoid<bool, std::plus<>>);

// Listing 1, bottom: size_t.
template <>
constexpr size_t unit<size_t>(std::bit_or<size_t>) {
  return 0;
}
template <>
constexpr size_t unit<size_t>(std::bit_and<size_t>) {
  return SIZE_MAX;
}
template <>
constexpr bool associative<size_t>(std::bit_or<size_t>) {
  return true;
}
template <>
constexpr bool associative<size_t>(std::bit_and<size_t>) {
  return true;
}

static_assert(IsMonoid<size_t, std::plus<size_t>>);
static_assert(IsMonoid<size_t, std::multiplies<size_t>>);
static_assert(IsMonoid<size_t, std::bit_or<size_t>>);
static_assert(IsMonoid<size_t, std::bit_and<size_t>>);

// Listing 4, top: the semiring the semimodule's scalars form, and the
// commutative monoid both build on; laws as functions of the operation.
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
// bool's own arithmetic is the Boolean semiring: plus<bool> is true + true = 2,
// converted back, so or, and multiplies<bool> is and.  These are the
// operations a bool count brings to the semimodule.
template <>
constexpr bool unit<bool>(std::plus<bool>) {
  return false;
}
template <>
constexpr bool unit<bool>(std::multiplies<bool>) {
  return true;
}
template <>
constexpr bool associative<bool>(std::plus<bool>) {
  return true;
}
template <>
constexpr bool associative<bool>(std::multiplies<bool>) {
  return true;
}
template <>
constexpr bool commutative<bool>(std::plus<bool>) {
  return true;
}
template <>
constexpr bool distributive<bool>(std::multiplies<bool>, std::plus<bool>) {
  return true;
}
static_assert(IsSemiring<bool, std::plus<bool>, std::multiplies<bool>>);
static_assert(!IsCommutativeMonoid<size_t, std::multiplies<size_t>>);

// Section 2, the posture: one panel per rule.
static_assert(std::semiregular<size_t>);
static_assert(std::semiregular<std::pair<size_t, bool>>);

template <std::semiregular T>
constexpr T id(const T& x) {
  return x;
}

static_assert(id(size_t{42}) == 42);

template <typename T>
constexpr T twice_loose(T x) {
  return x + x;
}

static_assert(twice_loose(std::string{"ab"}) == "abab");

// Listing 3: a finite prefix of the naturals, a chain with its ordinal, the
// order embedding onto an initial segment of size_t.
template <typename N>
constexpr size_t ordinal(N) = delete;
template <>
constexpr size_t ordinal<size_t>(size_t n) {
  return n;
}
template <>
constexpr size_t ordinal<bool>(bool b) {
  return b ? 1 : 0;
}

template <typename N>
concept IsFinitePrefix = std::totally_ordered<N> && requires(const N n) {
  { ordinal(n) } -> std::same_as<size_t>;
};

// times: concat of the constant list, n copies of x, for a count n of any
// finite prefix.
template <typename T, typename Op = std::plus<T>, IsFinitePrefix N>
  requires IsMonoid<T, Op>
constexpr T times(T x, N n) {
  return ordinal(n) == 0 || x == unit<T>(Op{})
             ? unit<T>(Op{})
             : concat<T, Op>(std::views::repeat(x, ordinal(n)));
}

static_assert(times<size_t>(7, 6uz) == 42);

// char counts too: its ordinal is its distance from the least char, an order
// embedding whether char is signed or not.  No semiring is registered for it,
// so times folds.
template <>
constexpr size_t ordinal<char>(char c) {
  return static_cast<size_t>(c - std::numeric_limits<char>::min());
}
constexpr char char_min = std::numeric_limits<char>::min();
static_assert(times<std::string>("ha"s, static_cast<char>(char_min + 3)) ==
              "hahaha");
static_assert(times<size_t>(7, char_min) == 0);

// Listing 3, right: the same fold with a budget known only at run time; its
// LLVM IR at -O2 is one multiplication (the paper's panel).
constexpr size_t times_n(size_t x, size_t n) { return times<size_t>(x, n); }
static_assert(times_n(7, 100) == 700);

// Section 3: the semimodule, the library's shape with its laws declared and
// its action n * x.  A certificate vouches for the laws and for the premise
// that each count n is the sum of ordinal(n) ones of its semiring, which the
// language's arithmetic makes true of size_t and bool.  Scalars of type
// size_t are the integers modulo 2^64, so the certificate holds for the
// unsigned carriers, which wrap at a power of two dividing it.
template <typename M, typename S, typename AddM, typename AddS, typename MultS>
constexpr bool linear(AddM, AddS, MultS) = delete;
template <std::unsigned_integral M, std::same_as<size_t> S>
constexpr bool linear(std::plus<M>, std::plus<S>, std::multiplies<S>) {
  return true;
}

// bool under || over the Boolean semiring: n copies of x are n * x, which is
// n and x.
template <>
constexpr bool linear<bool, bool>(std::logical_or<bool>, std::plus<bool>,
                                  std::multiplies<bool>) {
  return true;
}

// The library's interface (dedekind.category:action), its laws declared and
// its action the library's default, *; the action's result is up to
// conversion, since true * true is an int.
template <typename M, typename S, typename AddM = std::plus<M>,
          typename AddS = std::plus<S>, typename MultS = std::multiplies<S>>
concept IsSemimodule =
    IsCommutativeMonoid<M, AddM> && IsSemiring<S, AddS, MultS> &&
    requires(const S s, const M m) {
      { s * m } -> std::convertible_to<M>;
    } && linear<M, S>(AddM{}, AddS{}, MultS{});

static_assert(IsSemimodule<size_t, size_t>);
static_assert(!IsSemimodule<std::string, size_t>);
// Under * the carrier is no semimodule over size_t: the certificate is for +,
// so the overload drops out and a power stays a fold.
static_assert(!IsSemimodule<size_t, size_t, std::multiplies<size_t>>);
// The count type decides the algebra: bool under || is a semimodule over the
// Boolean semiring, but not over size_t's counts, where 2^63 + 2^63 copies
// wrap to none while the copies are true.
static_assert(IsSemimodule<bool, bool, std::logical_or<bool>>);
static_assert(!IsSemimodule<bool, size_t, std::logical_or<bool>>);

// Section 3, the sugar: Listing 3's times with a branch on a concept: in a
// semimodule over size_t under +, n copies of x are one action of n on x.
namespace sugar {
template <typename T, typename Op = std::plus<T>, IsFinitePrefix N>
  requires IsMonoid<T, Op>
constexpr T times(T x, N n) {
  if constexpr (IsSemimodule<T, N, Op>)
    return n * x;
  else
    return ordinal(n) == 0 || x == unit<T>(Op{})
               ? unit<T>(Op{})
               : concat<T, Op>(std::views::repeat(x, ordinal(n)));
}
}  // namespace sugar

// Section 3, Listing 4: the family in the kernel: Listing 3's times, and the
// semimodule overload (Listing 4 top), whose constraint contains Listing 3's;
// Listing 4's bottom is its System F image.
namespace kernel {
template <typename T, typename Op = std::plus<T>, IsFinitePrefix N>
  requires IsMonoid<T, Op>
constexpr T times(T x, N n) {
  return ordinal(n) == 0 || x == unit<T>(Op{})
             ? unit<T>(Op{})
             : concat<T, Op>(std::views::repeat(x, ordinal(n)));
}
template <typename T, typename Op = std::plus<T>, IsFinitePrefix N>
  requires IsSemimodule<T, N, Op>
constexpr T times(T x, N n) {
  return n * x;
}
}  // namespace kernel

static_assert(sugar::times<size_t>(7, 6uz) == 42);
static_assert(kernel::times<size_t>(7, 6uz) == 42);
static_assert(sugar::times<size_t, std::multiplies<size_t>>(2, 5uz) == 32);
static_assert(kernel::times<size_t, std::multiplies<size_t>>(2, 5uz) == 32);
static_assert(sugar::times(std::string{"ab"}, 3uz) == "ababab");
static_assert(kernel::times(std::string{"ab"}, 3uz) == "ababab");
static_assert(sugar::times<size_t>(7, static_cast<char>(char_min + 3)) == 21);
static_assert(kernel::times<size_t>(7, static_cast<char>(char_min + 3)) == 21);
// Section 6: at compile time only the law shortens the fold.  With the
// semimodule overload a budget of 10^8 is one step; Listing 3's times alone
// hits the constant evaluator's step limit (quoted in the paper, not held
// here, since a refused static_assert does not compile).
static_assert(kernel::times<size_t>(3, 100'000'000uz) == 300'000'000);
static_assert(sugar::times<size_t>(3, 100'000'000uz) == 300'000'000);
// Both agree with Listing 3's times, the fold, on and off the shortcut.
static_assert(sugar::times<size_t>(0, 9uz) == times<size_t>(0, 9uz));
static_assert(kernel::times<size_t>(0, 9uz) == times<size_t>(0, 9uz));
static_assert(sugar::times<size_t>(7, 0uz) == times<size_t>(7, 0uz));
static_assert(kernel::times<size_t>(7, 0uz) == times<size_t>(7, 0uz));
static_assert(sugar::times<size_t, std::multiplies<size_t>>(1, 1000uz) == 1);
static_assert(kernel::times<size_t, std::multiplies<size_t>>(1, 1000uz) == 1);
static_assert(sugar::times(std::string{}, 5uz).empty());
static_assert(kernel::times(std::string{}, 5uz).empty());
static_assert(sugar::times<size_t>(7, 6uz) == times<size_t>(7, 6uz));
static_assert(kernel::times<size_t>(7, 6uz) == times<size_t>(7, 6uz));
static_assert(sugar::times<size_t>(SIZE_MAX, 3uz) ==
              times<size_t>(SIZE_MAX, 3uz));
static_assert(kernel::times<size_t>(SIZE_MAX, 3uz) ==
              times<size_t>(SIZE_MAX, 3uz));
// Counted in bool, the Boolean semiring acts: one step.  Counted in size_t,
// the same monoid folds.
static_assert(kernel::times<bool, std::logical_or<bool>>(true, true));
static_assert(!kernel::times<bool, std::logical_or<bool>>(true, false));
static_assert(!kernel::times<bool, std::logical_or<bool>>(false, true));
static_assert(kernel::times<bool, std::logical_or<bool>>(true, 5uz));
static_assert(!kernel::times<bool, std::logical_or<bool>>(false,
                                                          100'000'000uz));
static_assert(sugar::times<bool, std::logical_or<bool>>(true, true));

// Section 4, two binding times: budget, and running out of it.
template <typename P>
concept IsPredicate = std::semiregular<P> && requires(const P p, size_t n) {
  { p(n) } -> std::same_as<bool>;
};

template <IsPredicate P>
constexpr std::pair<size_t, bool> advance(std::pair<size_t, bool> s, size_t i) {
  return s.second ? s : std::pair{i, P{}(i)};
}

constexpr std::optional<size_t> found(std::pair<size_t, bool> s) {
  return s.second ? std::optional{s.first} : std::nullopt;
}

template <IsPredicate P>
constexpr std::optional<size_t> first(size_t budget) {
  return found(std::ranges::fold_left(std::views::iota(size_t{0}, budget),
                                      std::pair{size_t{0}, false}, advance<P>));
}

struct Over1000 {
  constexpr bool operator()(size_t n) const { return n * n > 1000; }
};

static_assert(first<Over1000>(40) == 32);
static_assert(!first<Over1000>(30));

// The library's view of a contiguous interval of a finite chain, the range the
// paper's grammar admits beside repeat and iota.
using dedekind::category::chain_view;
static_assert(std::ranges::fold_left(chain_view{size_t{3}, size_t{5}},
                                     size_t{0}, std::plus<size_t>{}) == 12);

// Rule 3's other kind: a fold over a term, the library's one engine folding
// the term not-after-not with the algebra that reads a function's laws.
using dedekind::category::ArrowLaw;
using dedekind::category::Boole;
using dedekind::category::cata;
using dedekind::category::Compose;
using dedekind::category::IsMonotone;
using dedekind::category::LawAlgebra;
using dedekind::category::monotone;
using dedekind::category::NegationArrow;
using dedekind::category::Ternary;
using Negation = NegationArrow<Boole>;
static_assert(monotone(cata(LawAlgebra<>{}, Compose<Negation, Negation>{})) ==
              Ternary::True);
static_assert(IsMonotone<Compose<Negation, Negation>>);

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
    size_t twenty = 20;
    CHECK(L::concat<size_t>(std::array{twenty, size_t{21}, size_t{1}}) == 42);
    CHECK(L::concat<size_t, std::multiplies<size_t>>(
              std::array{size_t{6}, size_t{7}}) == 42);
    CHECK(L::concat<std::string>(
              std::array{std::string{"ab"}, std::string{"cd"}}) == "abcd");
    CHECK(L::unit<std::string>(std::plus<std::string>{}).empty());
  }

  SECTION("Section 2: the rules' functions, called at runtime") {
    size_t twelve = 12;
    CHECK(L::id(twelve) == 12);
    CHECK(L::times<size_t>(7, twelve) == 84);
    CHECK(L::times_n(twelve, 100) == 1200);
    CHECK(L::sugar::times<size_t>(7, twelve) ==
          L::kernel::times<size_t>(7, twelve));
    CHECK(L::sugar::times(std::string{"ab"}, twelve) ==
          L::kernel::times(std::string{"ab"}, twelve));
  }

  SECTION("Section 4: a fold over a term, at runtime") {
    L::Compose<L::Negation, L::Negation> twice_not{};
    CHECK(L::monotone(L::cata(L::LawAlgebra<>{}, twice_not)) ==
          L::Ternary::True);
    CHECK(L::cata(L::LawAlgebra<>{}, twice_not)[L::ArrowLaw::Antitone] ==
          L::Ternary::Unknown);
  }

  SECTION("Section 4: a fold over an interval of a chain, at runtime") {
    size_t lo = 3, hi = 5;
    CHECK(std::ranges::fold_left(L::chain_view{lo, hi}, size_t{0},
                                 std::plus<size_t>{}) == 12);
    CHECK(std::ranges::fold_left(L::chain_view{false, true}, 0,
                                 [](int n, bool) { return n + 1; }) == 2);
  }

  SECTION(
      "Section 4: the same fold, with a budget it did not have at compile "
      "time") {
    for (size_t budget : {size_t{30}, size_t{32}, size_t{33}, size_t{40}}) {
      const std::optional<size_t> found = L::first<L::Over1000>(budget);
      CHECK(found.has_value() == (budget >= 33));
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

  SECTION("The tree's semimodule accepts the listings' at size_t") {
    STATIC_CHECK(C::IsSemimodule<size_t, size_t>);
    STATIC_CHECK(
        C::IsSemimodule<size_t, size_t, std::plus<size_t>, std::plus<size_t>,
                        std::multiplies<size_t>, std::multiplies<>>);
  }
}
