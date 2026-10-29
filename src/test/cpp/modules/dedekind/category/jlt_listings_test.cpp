/** @file test/cpp/modules/dedekind/category/jlt_listings_test.cpp
 *
 * The listings of the Jlt paper (docs/paper/jlt/jlt.tex),
 * verbatim up to formatting, qualification (a listing may say
 * `using namespace std`), repetitions a comment elides and definitions a
 * listing omits, which live here,
 * so that CI holds the paper to the code.  The listings declare their own
 * unit and laws, so they live in their own namespace; the TEST_CASE then
 * checks that the tree's unit<T>(Op{}) and laws agree with every fact the
 * listings certify.
 */
#include <algorithm>
#include <array>
#include <bit>
#include <catch2/catch_test_macros.hpp>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>

import dedekind.category;

namespace jlt_listing {
using std::size_t;
using namespace std::string_literals;

// Listing 2, left: the shape, unpopulated.
template <typename T, typename Op>
constexpr T unit(Op) = delete;

template <typename T, typename Op>
constexpr bool associative(Op) = delete;

// Listing 2, left: an instance for a family, before the shape (Section 7).
template <std::unsigned_integral T>
constexpr T unit(std::plus<T>) {
  return 0;
}
template <std::unsigned_integral T>
constexpr bool associative(std::plus<T>) {
  return true;
}

// The one comparison the shape keeps: with the unit, which has one
// representation; by default the carrier's ==.
template <typename T, typename Op>
concept HasUnit = requires(Op op) {
  { unit<T>(op) } -> std::same_as<T>;
};
template <typename T, typename Op>
  requires HasUnit<T, Op> && std::equality_comparable<T>
constexpr bool is_unit(const T& a, Op op) {
  return a == unit<T>(op);
}

template <typename T, typename Op>
concept IsMonoid = std::semiregular<T> && requires(const T a, const T b) {
  { Op{}(a, b) } -> std::same_as<T>;
  { unit<T>(Op{}) } -> std::same_as<T>;
  { is_unit(a, Op{}) } -> std::same_as<bool>;
} && associative<T>(Op{});

// Listing 2, right: std::string, and concat at size_t and at std::string.
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

// concat at the lengths that need no recursion: none, one and two, the unit,
// the identity and one application of Op.  Each body uses only what the
// constraint names; the fold proper is Listing 3's.
template <typename T, typename Op = std::plus<T>>
  requires IsMonoid<T, Op>
constexpr T concat() {
  return unit<T>(Op{});
}
template <typename T, typename Op = std::plus<T>>
  requires IsMonoid<T, Op>
constexpr T concat(T w) {
  return w;
}
template <typename T, typename Op = std::plus<T>>
  requires IsMonoid<T, Op>
constexpr T concat(T v, T w) {
  return Op{}(v, w);
}

static_assert(concat<size_t>() == 0 && concat<size_t>(42uz) == 42 &&
              concat<size_t>(40uz, 2uz) == 42 &&
              concat<size_t, std::multiplies<size_t>>(6uz, 7uz) == 42 &&
              concat<std::string>() == "" &&
              concat<std::string>("ab"s, "c"s) == "abc");

// Certificates the listings use and do not print: bool.
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
static_assert(concat<bool, std::logical_or<bool>>(false, true) &&
              concat<bool, std::logical_and<bool>>(true, true));

// Certificates the listings use and do not print: size_t.
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

// char under +: the arithmetic is int's, converted back modulo 2^8, which
// C++20 defines for a signed char too.
template <>
constexpr char unit<char>(std::plus<char>) {
  return 0;
}
template <>
constexpr bool associative<char>(std::plus<char>) {
  return true;
}
static_assert(IsMonoid<char, std::plus<char>>);

// The algebra the paper keeps out of its listings: the semiring the
// semimodule's scalars form, and the
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
constexpr bool commutative<std::uint8_t>(std::plus<std::uint8_t>) {
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
constexpr bool unit<bool>(std::multiplies<bool>) {
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

// Listing 1: values and arrows.  Equals cannot be told apart, so a function
// is its graph, and functions are the arrows of a category, with composition
// their only structure.  The definitions are the tree's and the facts are
// checked here; a law is asked on the arrow's value (is_monic(f) reads what
// IsMonicArrow<F> reads), a composite's law derived by the engine in :posetal.
using dedekind::category::Cod;
using dedekind::category::Constant;
using dedekind::category::constant;
using dedekind::category::Copairing;
using dedekind::category::Dom;
using dedekind::category::endo;
using dedekind::category::id;
using dedekind::category::Identity;
using dedekind::category::is_antitone;
using dedekind::category::is_constant;
using dedekind::category::is_epic;
using dedekind::category::is_identity;
using dedekind::category::is_involution;
using dedekind::category::is_iso;
using dedekind::category::is_monic;
using dedekind::category::is_monotone;
using dedekind::category::IsArrow;
using dedekind::category::Pairing;
using dedekind::category::Π;
using dedekind::category::operator!;
using dedekind::category::operator-;
using dedekind::category::operator>>;
using dedekind::category::operator~;
using dedekind::category::ι;

// Identity arrows:
constexpr auto id_b = id<bool>();
constexpr auto id_u = id<size_t>();
static_assert(id_b(true) == true && id_u(42uz) == 42 && is_monotone(id_u) &&
              is_monotone(id_b) && is_involution(id_u) && is_involution(id_b) &&
              is_identity(id_u >> id_u) && (id_u >> id_u)(42uz) == 42 &&
              is_monotone(id_u >> id_u) && is_involution(id_u >> id_u));
// Constant arrows:
constexpr auto nil = constant<bool>(0uz);
constexpr auto one = constant<bool>(1uz);
constexpr auto ans = constant<bool>(42uz);
constexpr auto aye = constant<size_t>(true);
constexpr auto nay = constant<size_t>(false);
static_assert((ans >> id_u)(true) == ans(false) && is_constant(ans) &&
              is_monotone(ans) && is_constant(aye) && is_antitone(aye) &&
              !is_monic(ans) && !is_epic(ans) && !is_monic(aye >> id_b) &&
              !is_epic(nay >> id_b) && is_constant(nil >> nay >> ans));
// Involutions, pointwise:
static_assert((!aye)(0uz) == false && is_involution(!id_b) &&
              is_constant(!aye) && (~nil)(true) + 1 == 0 &&
              is_involution(~id_u) && is_constant(~nil) &&
              (-one)(true) == (~nil)(true) && is_involution(-id_u) &&
              is_constant(-nil) && is_iso(-id_u >> ~id_u) &&
              !is_involution(-id_u >> ~id_u));
// Products and coproducts:
constexpr auto fst = Π<1, std::pair<bool, size_t>>{};
constexpr auto sec = Pairing{id_b, nil};  // ⟨1, 0⟩
constexpr auto inl = ι<1, bool, size_t>{};
constexpr auto ret = Copairing{id_b, nay};  // [1, ⊥]
static_assert(is_epic(fst) && !is_monic(fst) && is_monic(sec) &&
              !is_epic(sec) && (sec >> fst)(true) && !(sec >> fst)(false) &&
              is_monic(inl) && !is_epic(inl) && is_epic(ret) &&
              !is_monic(ret) && (inl >> ret)(true) && !(inl >> ret)(false));

// Listing 2's fifth monoid, stated here where Listing 1's arrows exist: the
// arrows on size_t under composition, the tree's Arrows, a coproduct of the
// identity, the sections and the morphisms over a function pointer and of
// their composites, with the identity as unit, which has one representation;
// compose is the tree's operation, the function object whose call is >>.
using dedekind::category::Arrows;
using dedekind::category::compose;
template <>
constexpr Arrows<size_t> unit<Arrows<size_t>>(compose) {
  return {};
}
template <>
constexpr bool associative<Arrows<size_t>>(compose) {
  return true;
}
static_assert(IsMonoid<Arrows<size_t>, compose> &&
              is_unit(Arrows<size_t>{}, compose{}) &&
              !is_unit(Arrows<size_t>{~id_u}, compose{}) &&
              concat<Arrows<size_t>, compose>()(42uz) == 42 &&
              concat<Arrows<size_t>, compose>(~id_u)(3uz) == ~3uz &&
              concat<Arrows<size_t>, compose>(~id_u, -id_u)(3uz) == 4);

template <typename T>
constexpr T twice_loose(T x) {
  return x + x;
}

static_assert(twice_loose(std::string{"ab"}) == "abab");

// Listing 3: Figure 2 in code, four panels.  Top left: the counts, a finite
// chain, so the type supplies its prior and the chain is a default; top
// right: Collatz's step, guarded by until(equals(1), ...), a chain finite by
// fact and not by Rule 3; bottom left: the fold on a monoid, homogeneous,
// Listing 2's concat at a range, one chain under two operations; bottom
// right: the fold on an action M × S → M, heterogeneous, the standard's
// fold_left as it stands.  Both chains are input ranges, which is what ties
// them to fold_left.  chain, until, equals, arrow, IsFinite, IsTotalOrder and
// HasNNOStep are the library's.
namespace listing3 {
using dedekind::category::arrow;
using dedekind::category::chain;
using dedekind::category::equals;
using dedekind::category::HasNNOStep;
using dedekind::category::IsFinite;
using dedekind::category::IsTotalOrder;
using dedekind::category::until;

// top left: the counts, a finite chain
static_assert(IsFinite<size_t> && IsTotalOrder<size_t, std::less_equal<>> &&
              HasNNOStep<size_t>);
constexpr auto ns = 10uz >> chain<size_t>();  // 9, …, 0
static_assert(std::ranges::input_range<decltype(ns)> &&
              std::ranges::distance(ns) == 10 &&
              std::ranges::equal(ns, std::views::iota(0uz, 10uz) |
                                         std::views::reverse) &&
              std::ranges::empty(0uz >> chain<size_t>()) &&
              std::ranges::equal(ns >> id<size_t>(), ns));

// top right: Collatz's step, until 1
constexpr auto prior =
    until(equals(1uz),
          arrow<size_t>([](size_t n) { return n % 2 ? 3 * n + 1 : n / 2; }));
constexpr auto cs = 1'000uz >> chain(prior);  // 500, 250, …, 1
static_assert(std::ranges::input_range<decltype(cs)> &&
              std::ranges::distance(cs) == 111 && std::ranges::max(cs) == 9232);

// bottom left: the fold on a monoid, one chain under two operations
template <std::ranges::input_range R,
          typename T = std::ranges::range_value_t<R>,
          typename Op = std::plus<T>>
  requires IsMonoid<T, Op>
constexpr T concat(R r, Op op = {}) {
  return std::ranges::fold_left(r, unit<T>(op), op);
}
static_assert(concat(ns) == 45 &&
              concat(ns, std::multiplies<size_t>{}) == 0 &&  // 0 absorbs
              concat(ns >> constant<size_t>(7uz)) == 70 &&
              concat(cs) == 102512);

// bottom right: the fold on an action, M × S → M: 2×2 matrices acting on
// vectors, the chain of 10 under the constant Fibonacci matrix
using V = std::array<size_t, 2>;
using M2 = std::array<V, 2>;  // row by row
constexpr auto act = [](V v, M2 a) {
  return V{a[0][0] * v[0] + a[0][1] * v[1], a[1][0] * v[0] + a[1][1] * v[1]};
};
constexpr M2 fib{{{1, 1}, {1, 0}}};
constexpr auto ms = ns >> constant<size_t>(fib);
// F(11), F(10)
constexpr auto fs_10 = std::ranges::fold_left(ms, V{1, 0}, act);
static_assert(fs_10 == V{89, 55});
// The test's other actions: the booleans on the counts and on the words.
constexpr auto is_even = arrow<size_t>([](size_t k) { return k % 2 == 0; });
constexpr auto pop_count = [](size_t m, bool b) -> size_t { return m + b; };
constexpr auto append_bit = [](std::string s, bool b) {
  return s + (b ? '1' : '0');
};
static_assert(std::ranges::fold_left(ns >> is_even, 0uz, pop_count) == 5 &&
              std::ranges::fold_left(ns >> is_even, std::string{},
                                     append_bit) == "0101010101" &&
              std::ranges::fold_left(cs >> is_even, 0uz, pop_count) == 71);

// Listing 4's law: 2×2 matrices under multiplication are a monoid, declared,
// so n copies of one matrix fold by squaring.
struct MatMul {
  constexpr M2 operator()(M2 a, M2 b) const {
    return M2{V{a[0][0] * b[0][0] + a[0][1] * b[1][0],
                a[0][0] * b[0][1] + a[0][1] * b[1][1]},
              V{a[1][0] * b[0][0] + a[1][1] * b[1][0],
                a[1][0] * b[0][1] + a[1][1] * b[1][1]}};
  }
};
}  // namespace listing3
template <>
constexpr listing3::M2 unit<listing3::M2>(listing3::MatMul) {
  return listing3::M2{listing3::V{1, 0}, listing3::V{0, 1}};
}
template <>
constexpr bool associative<listing3::M2>(listing3::MatMul) {
  return true;
}
namespace listing3 {
constexpr MatMul mat_mul{};
// The type of n >> chain<size_t>() >> constant(x): n copies of one value.
template <typename T>
using ConstantChain = dedekind::category::unfold_view<
    size_t,
    dedekind::category::Mapped<size_t,
                               dedekind::category::StepCoalgebra<
                                   size_t, dedekind::category::Prior<size_t>>,
                               Constant<size_t, T>>>;
// The fold of n copies under an associative operation is the n-th power,
// by the binary method, one squaring per bit of n; resolution picks it over
// the plain fold, since its parameter is the more specific.
template <typename T, typename Op>
  requires IsMonoid<T, Op>
constexpr T concat(ConstantChain<T> r, Op op) {
  const size_t n = r.seed();
  const T x = r.coalgebra().f.value;
  return std::ranges::fold_left(
             std::views::iota(0uz, static_cast<size_t>(std::bit_width(n))),
             std::pair<T, T>{unit<T>(op), x},
             [n, op](std::pair<T, T> p, size_t k) {
               return std::pair<T, T>{
                   ((n >> k) & 1) ? op(p.first, p.second) : p.first,
                   op(p.second, p.second)};
             })
      .first;
}

// Listing 4: Listing 3's matrix chain, its length a parameter; its LLVM IR
// at -O2 -fno-unroll-loops with n at run time is the paper's loop, and with 10
// given, the value.
constexpr size_t fibonacci(size_t n) {
  const auto ms = n >> chain<size_t>() >> constant<size_t>(fib);
  return std::ranges::fold_left(ms, V{1, 0}, act)[1];
}
static_assert(fibonacci(0) == 0 && fibonacci(1) == 1 && fibonacci(10) == 55 &&
              fibonacci(90) == 2'880'067'194'370'816'120uz);
constexpr size_t fibonacci_10() { return fibonacci(10); }
static_assert(fibonacci_10() == 55);
// The action law, declared: act is an action of the monoid (M2, mat_mul),
// act(act(v, A), B) = act(v, B·A), act(v, I) = v.
template <typename H>
constexpr std::nullptr_t monoid_of = nullptr;  // undeclared
template <>  // keyed by the closure's type, as fold deduces it, without const
constexpr MatMul monoid_of<std::remove_cvref_t<decltype(act)>>{};
template <typename H>
using monoid_of_t = std::remove_cvref_t<decltype(monoid_of<H>)>;
// A fold along the walk, and its specialisation on n copies of one value
// under a monoid action: v acted on by their concat, which the binary method
// computes; resolution picks each, one law apiece.
template <std::ranges::input_range R, typename S, typename H>
constexpr S fold(R r, S v, H h) {
  return std::ranges::fold_left(r, v, h);
}
template <typename T, typename S, typename H>
  requires IsMonoid<T, monoid_of_t<H>>
constexpr S fold(ConstantChain<T> r, S v, H h) {
  return h(v, concat(r, monoid_of_t<H>{}));
}
// With the laws: 10^8 copies of the matrix, 27 squarings, where the plain
// fold of 10^8 steps is refused at compile time.
constexpr size_t fibonacci_fast(size_t n) {
  return fold(n >> chain<size_t>() >> constant<size_t>(fib), V{1, 0}, act)[1];
}
static_assert(fibonacci_fast(0) == 0 && fibonacci_fast(10) == 55 &&
              fibonacci_fast(90) == fibonacci(90) &&
              fibonacci_fast(100'000'000) == 14'139'011'350'745'967'675uz);

// The chain summed with the count known only at run time.
constexpr size_t sum_n(size_t n) { return concat(n >> chain<size_t>()); }
static_assert(sum_n(100) == 4950);
}  // namespace listing3

// Listing 4's machinery: the fold in its previous spelling, a family of
// terminators out<T, Op> and concat(chain, f) with the arrow beside the
// chain, kept until Listing 4 is ported to the arrows above.
using dedekind::category::chain;
using dedekind::category::chain_t;
// The step, a family: a count's predecessor, a word's init.
template <typename T>
constexpr T pred(T) = delete;
template <std::integral T>
constexpr T pred(T x) {
  return static_cast<T>(x - 1);
}
template <>
constexpr std::string pred<std::string>(std::string s) {
  return s.substr(0, s.size() - 1);
}

// The terminator: the monoid's unit, which the chain descends to and does not
// include; a count below it has an empty chain.
template <typename T, typename Op = std::plus<T>>
  requires IsMonoid<T, Op> && std::totally_ordered<T> && requires(const T x) {
    { pred(x) } -> std::same_as<T>;
  }
struct out {
  constexpr std::optional<T> operator()(T x) const {
    return x <= unit<T>(Op{}) ? std::nullopt : std::optional<T>{pred(x)};
  }
};

// A count: an element whose chain the fold can descend.
template <typename T, typename Op = std::plus<T>>
concept IsCount = requires(const T x) {
  { out<T, Op>{}(x) } -> std::same_as<std::optional<T>>;
  { chain(x, out<T, Op>{}) } -> std::same_as<chain_t<T, out<T, Op>>>;
};
// The algebra of the fold: a, then f(k), for an arrow from the counts into
// the carrier.
template <typename T, typename Op, typename N, IsArrow F>
  requires IsMonoid<T, Op> && std::same_as<Dom<F>, N> && std::same_as<Cod<F>, T>
struct Step {
  F f;
  constexpr T operator()(T a, N k) const { return Op{}(a, f(k)); }
};
// The counts N and the carrier T coincide when a count is an element of the
// carrier, Figure 2's bootstrap; a physical array parts them.
template <typename T, typename Op = std::plus<T>, typename N, typename OpN,
          IsArrow F>
  requires IsCount<N, OpN> && IsMonoid<T, Op> && std::same_as<Dom<F>, N> &&
           std::same_as<Cod<F>, T>
constexpr T concat(chain_t<N, out<N, OpN>> ns, F f) {
  return std::ranges::fold_left(ns, unit<T>(Op{}), Step<T, Op, N, F>{f});
}

// Listing 3's fold at a constant arrow, spelled out: what Listing 4 and the
// algebra below are held to.
template <
    typename T, typename Op = std::plus<T>, typename N = T,
    typename OpN = std::conditional_t<std::same_as<N, T>, Op, std::plus<N>>>
  requires IsCount<N, OpN> && IsMonoid<T, Op>
constexpr T copies(T x, N n) {
  return std::ranges::fold_left(
      chain(n, out<N, OpN>{}), unit<T>(Op{}),
      Step<T, Op, N, Constant<N, T>>{Constant<N, T>{x}});
}
static_assert(copies<size_t>(7, 10uz) == 70);
static_assert(copies<std::string>("ab"s, 3uz) == "ababab");

// Listing 4: the constant arrow specialises the fold.  Every step folds in
// the same x, so the fold is as many copies of x as the chain is long, and
// copies compose by squaring: the binary method over the bits of the count,
// log n steps for any monoid (Knuth, TAOCP 2, 4.6.3; Haskell's stimes).
template <std::integral N>
struct Bits {  // the bits of n, low to high: a coalgebra, the chain of halves
  constexpr std::optional<std::pair<bool, N>> operator()(N n) const {
    return n <= 0 ? std::nullopt
                  : std::optional<std::pair<bool, N>>{
                        {n % 2 == 1, static_cast<N>(n / 2)}};
  }
};
template <typename T, typename Op>
  requires IsMonoid<T, Op>
struct Square {  // the algebra: a set bit folds the base in; the base squares
  constexpr std::pair<T, T> operator()(std::pair<T, T> s, bool bit) const {
    return {bit ? Op{}(s.first, s.second) : s.first, Op{}(s.second, s.second)};
  }
};
template <typename T, typename Op = std::plus<T>, typename N, typename OpN>
  requires IsCount<N, OpN> && IsMonoid<T, Op> && std::integral<N>
constexpr T concat(chain_t<N, out<N, OpN>> ns, Constant<N, T> f) {
  const N u = unit<N>(OpN{});  // the chain's length: 0 at or below the unit
  const N n = static_cast<N>(std::max(ns.seed(), u) - u);
  return std::ranges::fold_left(dedekind::category::unfold(n, Bits<N>{}),
                                std::pair<T, T>{unit<T>(Op{}), f(n)},
                                Square<T, Op>{})
      .first;
}
// The counts and the carrier part: three copies of a word, by squaring.
static_assert(concat<std::string>(chain(3uz, out<size_t>{}),
                                  constant<size_t>("ab"s)) == "ababab");

static_assert(concat<size_t>(chain(10uz, out<size_t>{}),
                             constant<size_t>(7uz)) == 70);
static_assert(concat<size_t, std::multiplies<size_t>>(
                  chain(10uz, out<size_t, std::multiplies<size_t>>{}),
                  constant<size_t>(2uz)) == 512);
static_assert(concat<bool, std::logical_or<bool>>(
    chain(true, out<bool, std::logical_or<bool>>{}), constant<bool>(true)));
// A seed at or below the count's unit is the empty chain, whose fold is the
// unit: under multiplication the unit is 1, and 0 must not wrap to a length.
static_assert(concat<size_t, std::multiplies<size_t>>(
                  chain(0uz, out<size_t, std::multiplies<size_t>>{}),
                  constant<size_t>(2uz)) == 1 &&
              concat<size_t, std::multiplies<size_t>>(
                  chain(1uz, out<size_t, std::multiplies<size_t>>{}),
                  constant<size_t>(2uz)) == 1);
// Section 6: at compile time only a law shortens a fold: 10^8 copies are
// refused with Listing 3 alone (the step limit) and are 27 steps here.
static_assert(concat<size_t>(chain(100'000'000uz, out<size_t>{}),
                             constant<size_t>(3uz)) == 300'000'000);
// Listing 4, right: the copies with the count known only at run time.
constexpr size_t copies_n(size_t x, size_t n) {
  return concat<size_t>(chain(n, out<size_t>{}), constant<size_t>(x));
}
static_assert(copies_n(7, 100) == 700);

// Listing 4, second: the identity along the chain is monotone, so into a join
// semilattice, an idempotent commutative monoid such as size_t under max, the
// fold is the chain's top: one step.  Max is an operation type of ours, so
// its certificates may follow the shapes (Section 7).
template <std::totally_ordered T>
struct Max {
  constexpr T operator()(T a, T b) const { return a < b ? b : a; }
};
template <typename T, typename Op>
constexpr bool idempotent(Op) = delete;
template <std::unsigned_integral T>
constexpr T unit(Max<T>) {
  return 0;
}
template <std::unsigned_integral T>
constexpr bool associative(Max<T>) {
  return true;
}
template <std::unsigned_integral T>
constexpr bool commutative(Max<T>) {
  return true;
}
template <std::unsigned_integral T>
constexpr bool idempotent(Max<T>) {
  return true;
}
template <typename T, typename Op>
concept IsSemilattice = IsCommutativeMonoid<T, Op> && idempotent<T>(Op{});
static_assert(IsSemilattice<size_t, Max<size_t>> &&
              !IsSemilattice<size_t, std::plus<size_t>>);
// The join is the order's own, a <= b iff Op(a, b) == b, so that a chain
// descending in T's order is descending in the semilattice's: bitwise or is a
// semilattice on size_t whose order is not the chain's (2 | 1 = 3).  The
// tree's name for the certified shape is IsCertifiedOrderJoinSemilattice.
template <typename T, typename Op>
constexpr bool join_of_order(Op) = delete;
template <std::unsigned_integral T>
constexpr bool join_of_order(Max<T>) {
  return true;
}
template <typename T, typename Op>
concept IsOrderJoinSemilattice =
    IsSemilattice<T, Op> && std::totally_ordered<T> && join_of_order<T>(Op{});
static_assert(IsOrderJoinSemilattice<size_t, Max<size_t>> &&
              !IsOrderJoinSemilattice<size_t, std::bit_or<size_t>>);

// The fold's Op and the chain's OpN are named apart and required equal, so
// that a Max chain folded under the default, +, stays the fold.
template <typename T, typename Op = std::plus<T>, typename OpN>
  requires IsCount<T, OpN> && std::same_as<Op, OpN> &&
           IsOrderJoinSemilattice<T, Op>
constexpr T concat(chain_t<T, out<T, OpN>> ns, Identity<T>) {
  return out<T, OpN>{}(ns.seed()).value_or(unit<T>(Op{}));  // the top
}
static_assert(concat<size_t>(chain(10uz, out<size_t, Max<size_t>>{}),
                             Identity<size_t>{}) == 45);
static_assert(concat<size_t, Max<size_t>>(chain(10uz,
                                                out<size_t, Max<size_t>>{}),
                                          Identity<size_t>{}) == 9);
static_assert(concat<size_t, Max<size_t>>(chain(0uz,
                                                out<size_t, Max<size_t>>{}),
                                          Identity<size_t>{}) == 0);
// The fold spelled out agrees, and the count below the unit folds to it.
static_assert(std::ranges::fold_left(chain(10uz, out<size_t, Max<size_t>>{}),
                                     0uz, Max<size_t>{}) == 9);
// Listing 4, right, second: the top with the count known only at run time.
constexpr size_t top_n(size_t n) {
  return concat<size_t, Max<size_t>>(chain(n, out<size_t, Max<size_t>>{}),
                                     Identity<size_t>{});
}
static_assert(top_n(100) == 99);

// Listing 4, third: the arrow is a lookup into a physical array, Figure 2's
// array itself, along the chain of its length, so the symbolic array is
// represented by a block of memory and the fold is Listing 2's concat at
// length K: the chain goes, the memory is walked.
template <typename T, size_t K>
struct At {
  using Domain = size_t;
  using Codomain = T;
  std::array<T, K> a;
  constexpr T operator()(size_t k) const { return a[K - 1 - k]; }  // in order
};
template <typename T, typename Op = std::plus<T>, size_t K>
  requires IsCount<size_t> && IsMonoid<T, Op>
constexpr T concat(chain_t<size_t, out<size_t>> ns, At<T, K> f) {
  // The chain of n visits the last n elements, in order; all of them when n
  // exceeds K, where the lookup itself is out of bounds.
  return std::ranges::fold_left(f.a | std::views::reverse |
                                    std::views::take(ns.seed()) |
                                    std::views::reverse,
                                unit<T>(Op{}), Op{});
}
static_assert(concat<size_t>(chain(4uz, out<size_t>{}),
                             At<size_t, 4>{{1, 2, 3, 4}}) == 10);
static_assert(concat<std::string>(chain(3uz, out<size_t>{}),
                                  At<std::string, 3>{{"a"s, "b"s, "c"s}}) ==
              "abc");
static_assert(concat<std::string>(chain(2uz, out<size_t>{}),
                                  At<std::string, 3>{{"a"s, "b"s, "c"s}}) ==
              "bc");
// The fold spelled out agrees: the lookup runs the array in order.
static_assert(
    std::ranges::fold_left(
        chain(3uz, out<size_t>{}), ""s,
        Step<std::string, std::plus<std::string>, size_t, At<std::string, 3>>{
            At<std::string, 3>{{"a"s, "b"s, "c"s}}}) == "abc");
// Listing 4, right, third: the sum over an array read at run time.
constexpr size_t sum_a(std::array<size_t, 8> a) {
  return concat<size_t>(chain(8uz, out<size_t>{}), At<size_t, 8>{a});
}
static_assert(sum_a({1, 2, 3, 4, 5, 6, 7, 8}) == 36);

// The semimodule, the library's shape with its laws declared and
// its action n * x.  A certificate vouches for the laws and for the premise
// that each count n is the sum of as many ones of its semiring as its chain
// has elements, which the language's arithmetic makes true of size_t and
// bool.  Scalars of type
// size_t are the integers modulo 2^64, so the certificate holds for the
// unsigned carriers, which wrap at a power of two dividing it.
template <typename M, typename S, typename AddM, typename AddS, typename MultS>
constexpr bool linear(AddM, AddS, MultS) = delete;
template <std::unsigned_integral M, std::same_as<size_t> S>
  requires(!std::same_as<M, bool>)
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

// The library's shape (dedekind.category:action), its laws declared and its
// action the library's default, *, relaxed here: the action's result is up to
// conversion, since true * true is an int, where the library's IsAction still
// asks for M exactly (#1099).
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
// bool is an unsigned integral, but its + saturates: no semimodule over
// size_t's counts under + either.
static_assert(!IsSemimodule<bool, size_t>);

// Over a semimodule the copies are one action, n * x; the paper keeps this
// in the test, where the fold, the binary method and the action are held to
// agree.
namespace algebra {
template <typename T, typename Op = std::plus<T>, typename N, typename OpN>
  requires IsCount<N, OpN> && IsSemimodule<T, N, Op> && std::integral<N>
constexpr T concat(chain_t<N, out<N, OpN>> ns, Constant<N, T> f) {
  const N u = unit<N>(OpN{});
  const N n = static_cast<N>(std::max(ns.seed(), u) - u);
  return static_cast<T>(n * f(n));
}
}  // namespace algebra

static_assert(copies<size_t>(7, 10uz) ==
                  concat<size_t>(chain(10uz, out<size_t>{}),
                                 Constant<size_t>{7}) &&
              copies<size_t>(7, 10uz) ==
                  algebra::concat<size_t>(chain(10uz, out<size_t>{}),
                                          Constant<size_t>{7}));
static_assert(copies<size_t>(0, 9uz) == 0 && copies<size_t>(7, 0uz) == 0 &&
              copies<size_t>(SIZE_MAX, 3uz) ==
                  concat<size_t>(chain(3uz, out<size_t>{}),
                                 Constant<size_t>{SIZE_MAX}));
static_assert(copies<bool, std::logical_or<bool>>(true, true) &&
              copies<bool, std::logical_or<bool>>(true, true) ==
                  algebra::concat<bool, std::logical_or<bool>>(
                      chain(true, out<bool, std::logical_or<bool>>{}),
                      Constant<bool>{true}));
static_assert(!copies<bool, std::logical_or<bool>>(false, true) &&
              !concat<bool, std::logical_or<bool>>(
                  chain(true, out<bool, std::logical_or<bool>>{}),
                  Constant<bool>{false}));
static_assert(copies<std::string>("ab"s, "xyz"s) == "ababab");

// Appendix, the search listing: a budget, and running out of it.
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
    CHECK(L::concat<size_t>() == 0);
    CHECK(L::concat<size_t>(twenty) == 20);
    CHECK(L::concat<size_t>(twenty, size_t{22}) == 42);
    CHECK(L::concat<size_t, std::multiplies<size_t>>(size_t{6}, size_t{7}) ==
          42);
    CHECK(L::concat<std::string>(std::string{"ab"}, std::string{"cd"}) ==
          "abcd");
    CHECK(L::unit<std::string>(std::plus<std::string>{}).empty());
  }

  SECTION("Section 2: the rules' functions, called at runtime") {
    size_t twelve = 12;
    CHECK(L::id<size_t>()(twelve) == 12);
    CHECK(L::listing3::sum_n(twelve) == 66);
    CHECK(L::listing3::fibonacci(twelve) == 144);
    CHECK(L::copies_n(7, twelve) == 84);
    CHECK(L::top_n(twelve) == 11);
    CHECK(L::sum_a({1, 2, 3, 4, 5, 6, 7, twelve}) == 40);
    CHECK(L::copies<size_t>(7, twelve) == 84);
    bool yes = true;
    CHECK(!L::concat<bool, std::logical_or<bool>>(
        L::chain(yes, L::out<bool, std::logical_or<bool>>{}),
        L::Identity<bool>{}));
    CHECK(L::concat<bool, std::logical_or<bool>>(
        L::chain(yes, L::out<bool, std::logical_or<bool>>{}),
        L::endo<bool>(std::logical_not<bool>{})));
    std::string abc = "abc";
    CHECK(L::concat<std::string>(L::chain(abc, L::out<std::string>{}),
                                 L::Identity<std::string>{}) == "aba");
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
