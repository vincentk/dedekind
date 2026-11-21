/**
 * @file src/test/cpp/modules/dedekind/python/showcase_16_listing3_two_folds.cpp
 * @brief Showcase 16: the Jlt paper's two recursions, compiled.  The listings
 *        of Section 2, self-contained, and what the compiler leaves of each:
 *        the fold over a value at both binding times, and the folds over a
 *        term's type, which are finished while compiling.
 *
 * Every name is declared here or is the standard's: the arrows and the law of
 * Listing 1, the monoid of Listing 2, and @c concat, @c then and @c length of
 * Listing 3.  The functions below are @c extern @c "C" and never inlined, so
 * each is its own symbol in the IR the fixture script reads.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <algorithm>
#include <array>
#include <bit>
#include <concepts>
#include <cstddef>
#include <functional>
#include <ranges>
#include <utility>

namespace {
using std::size_t;

// Listing 1: arrows on size_t, and a law as a table.
struct Succ {
  constexpr size_t operator()(size_t x) const { return x + 1; }
};
struct Neg {
  constexpr size_t operator()(size_t x) const { return -x; }
};
template <typename F>
concept Arrow = requires(const F f, size_t x) {
  { f(x) } -> std::same_as<size_t>;
};
template <Arrow F, Arrow G>
struct Then {
  F f;
  G g;
  constexpr size_t operator()(size_t x) const { return g(f(x)); }
};
template <Arrow F, Arrow G>
constexpr Then<F, G> operator>>(F f, G g) {
  return Then<F, G>{f, g};
}
template <typename F>
constexpr bool injective = false;
template <>
constexpr bool injective<Succ> = true;
template <>
constexpr bool injective<Neg> = true;
template <Arrow F, Arrow G>
constexpr bool injective<Then<F, G>> = injective<F> && injective<G>;
template <typename F>
concept IsInjective = Arrow<F> && injective<F>;
template <Arrow F>
constexpr bool is_injective(F) {
  return IsInjective<F>;
}
// A constant arrow: it returns the value it holds.
struct Const {
  size_t v;
  constexpr size_t operator()(size_t) const { return v; }
};
// A second law on arrows, with the opposite lemma: a composite is constant
// where either part is.
template <typename F>
constexpr bool constant = false;
template <>
constexpr bool constant<Const> = true;
template <Arrow F, Arrow G>
constexpr bool constant<Then<F, G>> = constant<F> || constant<G>;
template <typename F>
concept IsConstant = Arrow<F> && constant<F>;

// Listing 2: a monoid as a carrier, an operation and its unit.
template <typename T, typename Op>
constexpr bool associative = false;
template <>
constexpr bool associative<size_t, std::plus<size_t>> = true;
template <>
constexpr bool associative<size_t, std::multiplies<size_t>> = true;
template <typename T, typename Op, T e>
concept IsMonoid = std::regular<T> && associative<T, Op> && requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
} && Op{}(e, e) == e;

// Listing 3: the two recursions.
template <typename T, typename Op, T e, std::ranges::sized_range R>
  requires IsMonoid<T, Op, e> && std::same_as<std::ranges::range_value_t<R>, T>
constexpr T concat(R r) {
  return std::ranges::fold_left(r, e, Op{});
}

// Listing 4: n copies by repeated squaring, and the 2x2 matrices over a
// semiring, their associativity inferred from the semiring of the entries.
template <typename T>
struct Power {
  T product, base;
  size_t exponent;
};
// One step of the power by squaring: product * base^exponent stays what it
// was while the exponent is halved.  The step names the calls it makes: its
// constraint is the monoid.
template <typename T, typename Op, T e>
struct Halving {
  constexpr Power<T> operator()(Power<T> p, size_t) const
    requires IsMonoid<T, Op, e>
  {
    return {p.exponent % 2 == 1 ? Op{}(p.product, p.base) : p.product,
            Op{}(p.base, p.base), p.exponent / 2};
  }
};
template <typename T, typename Op, T e>
  requires IsMonoid<T, Op, e>
constexpr T power(T x, size_t n) {
  return std::ranges::fold_left(
             std::views::iota(0uz, size_t(std::bit_width(n))),
             Power<T>{e, x, n}, Halving<T, Op, e>{})
      .product;
}
// The fast fold, for the image of an arrow that is known to be constant: the
// law selects it, and associativity makes it correct.
template <typename T, typename Op, T e, std::ranges::sized_range V,
          IsConstant F>
  requires IsMonoid<T, Op, e> &&
           std::same_as<
               std::ranges::range_value_t<std::ranges::transform_view<V, F>>, T>
constexpr T concat(std::ranges::transform_view<V, F> r) {
  return std::ranges::size(r) == 0
             ? e
             : power<T, Op, e>(*std::ranges::begin(r), std::ranges::size(r));
}
// The monoid pattern one level up.  Two more laws, each a trait with its
// entries, and a shape that names them beside the two monoids: a semiring.
template <typename T, typename Op>
constexpr bool commutative = false;
template <typename T, typename Times, typename Add>
constexpr bool distributes = false;
template <>
constexpr bool commutative<size_t, std::plus<size_t>> = true;
template <>
constexpr bool distributes<size_t, std::multiplies<size_t>, std::plus<size_t>> =
    true;
template <typename T, T zero, T one>
concept IsSemiring =
    IsMonoid<T, std::plus<T>, zero> && IsMonoid<T, std::multiplies<T>, one> &&
    commutative<T, std::plus<T>> &&
    distributes<T, std::multiplies<T>, std::plus<T>>;
// The 2x2 matrices over a semiring under the type's own + and *, as a type
// with a product of its own, so that std::multiplies names that product.  The
// product's body writes the operators, and the shape names std::plus<T> and
// std::multiplies<T>, which the standard defines as those operators.
template <typename T, T zero, T one>
struct Mat {
  T a, b, c, d;
  constexpr bool operator==(const Mat&) const = default;
  constexpr Mat operator*(Mat y) const
    requires IsSemiring<T, zero, one>
  {
    return {a * y.a + b * y.c, a * y.b + b * y.d, c * y.a + d * y.c,
            c * y.b + d * y.d};
  }
};
// The inferred law: the matrices' product is associative for a semiring T.
template <typename T, T zero, T one>
  requires IsSemiring<T, zero, one>
constexpr bool
    associative<Mat<T, zero, one>, std::multiplies<Mat<T, zero, one>>> = true;
using M = Mat<size_t, 0uz, 1uz>;
using Mul = std::multiplies<M>;
constexpr M identity{1, 0, 0, 1};
constexpr M fib{1, 1, 1, 0};
// The same value at every position, for the plain fold over n copies.
struct Fib {
  constexpr M operator()(size_t) const { return fib; }
};
template <Arrow F, Arrow G>
constexpr Then<F, G> then(F f, G g) {
  return Then<F, G>{f, g};
}
template <Arrow F, Arrow G, Arrow H>
constexpr auto then(Then<F, G> t, H h) {
  return then(t.f, then(t.g, h));
}
template <typename F>
constexpr size_t length = 1;
template <Arrow F, Arrow G>
constexpr size_t length<Then<F, G>> = length<F> + length<G>;

constexpr auto ns = std::views::reverse(std::views::iota(0uz, 10uz));
constexpr auto a = then(Succ{} >> Neg{}, Succ{});
static_assert(concat<size_t, std::plus<size_t>, 0uz>(ns) == 45 &&
              is_injective(a) && length<Then<Succ, Then<Neg, Succ>>> == 3);
// Listing 4's last assertion: a composite with a constant part is constant by
// the lemma, so its image takes the power.
static_assert(IsConstant<decltype(Succ{} >> Const{7uz})> &&
              concat<size_t, std::plus<size_t>, 0uz>(std::views::transform(
                  std::views::iota(0uz, 10uz), Succ{} >> Const{7uz})) == 70);
// The power of the unit is the unit: power, with that said before it folds.
template <typename T, typename Op, T e>
  requires IsMonoid<T, Op, e>
constexpr T power_unit_first(T x, size_t n) {
  return x == e ? x : power<T, Op, e>(x, n);
}
}  // namespace

/** @return the fold over a value, everything known while compiling. */
extern "C" __attribute__((noinline)) std::size_t listing3_sum_known() {
  return concat<size_t, std::plus<size_t>, 0uz>(ns);
}
/** @return the same fold, the length known only at run time. */
extern "C" __attribute__((noinline)) std::size_t listing3_sum(std::size_t n) {
  return concat<size_t, std::plus<size_t>, 0uz>(
      std::views::reverse(std::views::iota(0uz, n)));
}
/** @return the same fold on the other monoid, the product of 1 to n - 1. */
extern "C" __attribute__((noinline)) std::size_t listing3_product(
    std::size_t n) {
  return concat<size_t, std::multiplies<size_t>, 1uz>(
      std::views::reverse(std::views::iota(1uz, n)));
}
/** @return the law of a term, a fold over its type. */
extern "C" __attribute__((noinline)) bool listing1_law() {
  return is_injective(a);
}
/** @return the length of a term, a fold over its type. */
extern "C" __attribute__((noinline)) std::size_t listing3_length() {
  return length<Then<Succ, Then<Neg, Succ>>>;
}
/** @return the term applied to a value known only at run time. */
extern "C" __attribute__((noinline)) std::size_t listing3_term(std::size_t x) {
  return a(x);
}
/** @return the n-th Fibonacci number: the n-th power of one matrix, which the
 *  matrices' inferred monoid licenses. */
extern "C" __attribute__((noinline)) std::size_t listing4_fib(std::size_t n) {
  return power<M, Mul, identity>(fib, n).b;
}
/** @return the same number by the plain fold, one step for each copy. */
extern "C" __attribute__((noinline)) std::size_t listing4_fib_plain(
    std::size_t n) {
  return concat<M, Mul, identity>(
             std::views::transform(std::views::iota(0uz, n), Fib{}))
      .b;
}
/** @return the power of the unit under +, by the fold alone, the count known
 *  only when the program runs. */
extern "C" __attribute__((noinline)) std::size_t power_of_zero(std::size_t n) {
  return power<size_t, std::plus<size_t>, 0uz>(0uz, n);
}
/** @return the same, with the unit recognised before the fold. */
extern "C" __attribute__((noinline)) std::size_t power_of_zero_unit_first(
    std::size_t n) {
  return power_unit_first<size_t, std::plus<size_t>, 0uz>(0uz, n);
}
/** @return an entry of the identity matrix to the n-th power, by the fold
 *  alone. */
extern "C" __attribute__((noinline)) std::size_t power_of_identity(
    std::size_t n) {
  return power<M, Mul, identity>(identity, n).a;
}
/** @return the same, with the unit recognised before the fold. */
extern "C" __attribute__((noinline)) std::size_t power_of_identity_unit_first(
    std::size_t n) {
  return power_unit_first<M, Mul, identity>(identity, n).a;
}

// The four listings of the paper's Section 2 as they now stand: a monoid named
// by its unit, every law keyed on that name.  The same code as the namespace
// paper_listings of the listings' test, without strings, which the IR need not
// show.  Every declaration is used below, so that no warning fires.
namespace now_listings {
using std::size_t;

template <typename E>
constexpr bool associative = false;
template <typename E>
constexpr bool unital = false;
template <typename E>
struct op_of {};
template <typename E>
using carrier_t = decltype(E{}());
template <typename E>
using op_t = typename op_of<E>::type;
template <typename E>
concept IsMagma = requires {
  E{}();
  typename op_t<E>;
} && std::copyable<carrier_t<E>> && requires(carrier_t<E> a, carrier_t<E> b) {
  { op_t<E>{}(a, b) } -> std::same_as<carrier_t<E>>;
};
template <typename E>
concept IsUnitalMagma = IsMagma<E> && unital<E>;
template <typename E>
concept IsMonoid = IsUnitalMagma<E> && associative<E>;

struct Sum {
  constexpr size_t operator()() const { return 0; }
};
template <>
struct op_of<Sum> {
  using type = std::plus<size_t>;
};
template <>
constexpr bool associative<Sum> = true;
template <>
constexpr bool unital<Sum> = true;
struct Product {
  constexpr size_t operator()() const { return 1; }
};
template <>
struct op_of<Product> {
  using type = std::multiplies<size_t>;
};
template <>
constexpr bool associative<Product> = true;
template <>
constexpr bool unital<Product> = true;
struct All {
  constexpr bool operator()() const { return true; }
};
template <>
struct op_of<All> {
  using type = std::logical_and<bool>;
};
template <>
constexpr bool associative<All> = true;
template <>
constexpr bool unital<All> = true;
struct Any {
  constexpr bool operator()() const { return false; }
};
template <>
struct op_of<Any> {
  using type = std::logical_or<bool>;
};
template <>
constexpr bool associative<Any> = true;
template <>
constexpr bool unital<Any> = true;

template <typename E, std::ranges::sized_range R>
  requires IsMonoid<E> &&
           std::same_as<std::ranges::range_value_t<R>, carrier_t<E>>
constexpr carrier_t<E> concat2(R r) {
  return std::ranges::fold_left(r, E{}(), op_t<E>{});
}

template <typename E>
struct Squaring {
  size_t n;
  using T = carrier_t<E>;
  constexpr std::pair<T, T> operator()(std::pair<T, T> s, size_t i) const
    requires IsMonoid<E>
  {
    return {((n >> i) & 1) == 1 ? op_t<E>{}(s.first, s.second) : s.first,
            op_t<E>{}(s.second, s.second)};
  }
};
template <typename E, typename T>
  requires IsMonoid<E> && std::same_as<T, carrier_t<E>>
constexpr T power(T x, size_t n) {
  return std::ranges::fold_left(
             std::views::iota(0uz, size_t(std::bit_width(n))),
             std::pair<T, T>{E{}(), x}, Squaring<E>{n})
      .first;
}
template <typename E>
constexpr bool idempotent = false;
template <typename E>
concept IsIdempotentMonoid = IsMonoid<E> && idempotent<E>;
template <typename E, typename T>
  requires IsIdempotentMonoid<E> && std::same_as<T, carrier_t<E>>
constexpr T power(T x, size_t n) {
  return n == 0 ? E{}() : x;
}
template <>
constexpr bool idempotent<All> = true;

struct Xor {
  constexpr size_t operator()() const { return 0; }
};
template <>
struct op_of<Xor> {
  using type = std::bit_xor<size_t>;
};
template <>
constexpr bool associative<Xor> = true;
template <>
constexpr bool unital<Xor> = true;
struct Larger {
  constexpr size_t operator()(size_t a, size_t b) const {
    return a < b ? b : a;
  }
};
struct Max {
  constexpr size_t operator()() const { return 0; }
};
template <>
struct op_of<Max> {
  using type = Larger;
};
template <>
constexpr bool associative<Max> = true;
template <>
constexpr bool unital<Max> = true;
template <>
constexpr bool idempotent<Max> = true;
static_assert(power<Xor>(5uz, 3uz) == 5 && power<Max>(7uz, 10uz) == 7);

template <typename E>
concept HasPower = IsMonoid<E> && requires(carrier_t<E> x, size_t n) {
  { power<E>(x, n) } -> std::same_as<carrier_t<E>>;
};
template <typename E>
  requires HasPower<E>
constexpr carrier_t<E> named_power(carrier_t<E> x, size_t n) {
  return power<E>(x, n);
}

template <typename H, typename A, typename B>
constexpr bool homomorphism = false;
template <typename F, typename A, typename B>
concept Maps = requires(F f, carrier_t<A> x) {
  { f(x) } -> std::same_as<carrier_t<B>>;
};
struct IsZero {
  constexpr bool operator()(size_t n) const { return n == 0; }
};
struct Not {
  constexpr bool operator()(bool b) const { return !b; }
};
template <>
constexpr bool homomorphism<Not, All, Any> = true;
template <typename A, typename F, typename B, typename G, typename C>
struct Via {
  F f;
  G g;
  constexpr carrier_t<C> operator()(carrier_t<A> x) const
    requires Maps<F, A, B> && Maps<G, B, C>
  {
    return g(f(x));
  }
};
template <typename A, typename F, typename B, typename G, typename C>
constexpr bool homomorphism<Via<A, F, B, G, C>, A, C> =
    homomorphism<F, A, B> && homomorphism<G, B, C>;
template <>
constexpr bool homomorphism<IsZero, All, All> = false;

static_assert(IsMonoid<Sum> && IsMonoid<Product> && IsMonoid<All> &&
              IsMonoid<Any> && IsIdempotentMonoid<All> && HasPower<Sum> &&
              homomorphism<Not, All, Any> && !homomorphism<IsZero, All, All> &&
              concat2<Sum>(std::array{1uz, 2uz, 3uz}) == 6 &&
              power<Product>(2uz, 10uz) == 1024 &&
              power<All>(false, 5uz) == false);
}  // namespace now_listings

/** @return Listing 1's concat2 at the sum, everything known while compiling. */
extern "C" __attribute__((noinline)) std::size_t now_concat2_known() {
  return now_listings::concat2<now_listings::Sum>(std::array{1uz, 2uz, 3uz});
}
/** @return the sum of the counts below n, the count known only at run time. */
extern "C" __attribute__((noinline)) std::size_t now_concat2(std::size_t n) {
  return now_listings::concat2<now_listings::Sum>(std::views::iota(0uz, n));
}
/** @return Listing 2's power at the product, by repeated squaring. */
extern "C" __attribute__((noinline)) std::size_t now_power(std::size_t x,
                                                           std::size_t n) {
  return now_listings::power<now_listings::Product>(x, n);
}
/** @return the same value by its definition, n copies folded one by one. */
extern "C" __attribute__((noinline)) std::size_t now_power_plain(
    std::size_t x, std::size_t n) {
  return now_listings::concat2<now_listings::Product>(std::views::repeat(x, n));
}
/** @return the power of the unit of the sum, the count left for later. */
extern "C" __attribute__((noinline)) std::size_t now_power_of_unit(
    std::size_t n) {
  return now_listings::power<now_listings::Sum>(0uz, n);
}
/** @return the power under conjunction, the case that idempotence selects. */
extern "C" __attribute__((noinline)) bool now_power_idempotent(bool x,
                                                               std::size_t n) {
  return now_listings::power<now_listings::All>(x, n);
}
/** @return Listing 3's named_power at the sum. */
extern "C" __attribute__((noinline)) std::size_t now_named_power(
    std::size_t x, std::size_t n) {
  return now_listings::named_power<now_listings::Sum>(x, n);
}
/** @return Listing 4's lemma at a composite, a law the compiler derives. */
extern "C" __attribute__((noinline)) bool now_derived_law() {
  using namespace now_listings;
  return homomorphism<Via<All, Not, Any, Not, All>, All, All> ==
         (homomorphism<Not, All, Any> && homomorphism<Not, Any, All>);
}
/** @return the power under exclusive or: the parity of the count. */
extern "C" __attribute__((noinline)) std::size_t now_power_xor(std::size_t x,
                                                               std::size_t n) {
  return now_listings::power<now_listings::Xor>(x, n);
}
/** @return the power under max, the case that idempotence selects. */
extern "C" __attribute__((noinline)) std::size_t now_power_max(std::size_t x,
                                                               std::size_t n) {
  return now_listings::power<now_listings::Max>(x, n);
}
