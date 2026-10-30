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
using dedekind::category::Constant;
using dedekind::category::constant;
using dedekind::category::Copairing;
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

// Listing 2's fifth monoid: the sections of + on size_t under composition,
// the tree's Sections, closed, two sections composing to one by
// associativity, with the identity as unit.  (The tree's Arrows, which adds
// morphisms over a function pointer and their composites, is a partial
// monoid, its composition defined to one level, and is certified as none.)
using dedekind::category::compose;
using dedekind::category::curry;
using dedekind::category::Sections;
constexpr auto adding = curry<size_t>(std::plus<size_t>{});
template <>
constexpr Sections<size_t> unit<Sections<size_t>>(compose) {
  return {};
}
template <>
constexpr bool associative<Sections<size_t>>(compose) {
  return true;
}
static_assert(IsMonoid<Sections<size_t>, compose> &&
              is_unit(Sections<size_t>{}, compose{}) &&
              !is_unit(Sections<size_t>{adding(1uz)}, compose{}) &&
              concat<Sections<size_t>, compose>()(42uz) == 42 &&
              concat<Sections<size_t>, compose>(adding(40uz),
                                                adding(2uz))(0uz) == 42);

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

// bottom left: the fold on a monoid, one chain under two operations: the
// tree's concat (:total), Listing 2's at a range
using dedekind::category::concat;
static_assert(concat(ns) == 45 &&
              concat(ns, std::multiplies<size_t>{}) == 0 &&  // 0 absorbs
              concat(ns >> constant<size_t>(7uz)) == 70 &&
              concat(cs) == 102512);

// bottom right: the fold on an action, M × S → M: 2×2 matrices acting on
// vectors, the chain of 10 under the constant Fibonacci matrix
// M2 is a struct of the test's, not an array of arrays, so that its operators
// are found: the standard's function objects, std::multiplies<> among them,
// call a * b inside namespace std, and argument-dependent lookup searches only
// the arguments' namespaces, which for std::array is std (the paper's lookup
// wall, Section 7).  Its operators are hidden friends, found by that lookup.
using V = std::array<size_t, 2>;
struct M2 {  // row by row
  V top, bottom;
  friend constexpr V operator*(V v, M2 a) {  // the row vector v·A
    return {v[0] * a.top[0] + v[1] * a.bottom[0],
            v[0] * a.top[1] + v[1] * a.bottom[1]};
  }
  friend constexpr M2 operator*(M2 a, M2 b) {  // row by row, each times b
    return {a.top * b, a.bottom * b};
  }
  friend constexpr M2 operator+(M2 a, M2 b) {
    return {{a.top[0] + b.top[0], a.top[1] + b.top[1]},
            {a.bottom[0] + b.bottom[0], a.bottom[1] + b.bottom[1]}};
  }
  friend constexpr M2 operator*(size_t k, M2 a) {  // entry by entry
    return {{k * a.top[0], k * a.top[1]}, {k * a.bottom[0], k * a.bottom[1]}};
  }
  constexpr bool operator==(const M2&) const = default;
};
constexpr auto act = [](V v, M2 a) { return v * a; };
constexpr M2 fib{{1, 1}, {1, 0}};
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

// Listing 4's example: 2×2 matrices under multiplication, registered with the
// tree's traits as a monoid, and act as its action; the overloads that use the
// laws are the tree's, concat in :total and fold in :action.
using MatMul = std::multiplies<M2>;
using MatAdd = std::plus<M2>;
}  // namespace listing3
}  // namespace jlt_listing
namespace dedekind::category {
template <>
struct is_associative<jlt_listing::listing3::M2, jlt_listing::listing3::MatMul>
    : std::true_type {};
template <>  // size_t arithmetic wraps, so the product is total
struct is_periodic<jlt_listing::listing3::M2, jlt_listing::listing3::MatMul>
    : std::true_type {};
template <>
struct unit_trait<jlt_listing::listing3::M2, jlt_listing::listing3::MatMul> {
  using value_type = jlt_listing::listing3::M2;
  static constexpr value_type value{jlt_listing::listing3::V{1, 0},
                                    jlt_listing::listing3::V{0, 1}};
};
template <>  // act(act(v, A), B) = act(v, A·B), act(v, I) = v
struct acting_monoid<
    std::remove_cvref_t<decltype(jlt_listing::listing3::act)>> {
  using type = jlt_listing::listing3::MatMul;
};
template <>
struct is_associative<jlt_listing::listing3::M2, jlt_listing::listing3::MatAdd>
    : std::true_type {};
template <>
struct is_periodic<jlt_listing::listing3::M2, jlt_listing::listing3::MatAdd>
    : std::true_type {};
template <>
struct unit_trait<jlt_listing::listing3::M2, jlt_listing::listing3::MatAdd> {
  using value_type = jlt_listing::listing3::M2;
  static constexpr value_type value{};
};
template <>
struct is_commutative<jlt_listing::listing3::M2, jlt_listing::listing3::MatAdd>
    : std::true_type {};
template <>  // a semimodule over the counts: k·A entry by entry
struct is_linear_action<jlt_listing::listing3::M2, size_t,
                        jlt_listing::listing3::MatAdd, std::plus<size_t>,
                        std::multiplies<size_t>, std::multiplies<>>
    : std::true_type {};
}  // namespace dedekind::category
namespace jlt_listing {
namespace listing3 {
using dedekind::category::fold;
// The third rung: matrix addition, a semimodule over the counts, so n copies
// of A are n·A in one action, 10^8 of them included.
constexpr M2 shear{{1, 2}, {0, 1}};
static_assert(dedekind::category::IsSemimodule<M2, size_t, MatAdd> &&
              concat(10uz >> chain<size_t>() >> constant<size_t>(shear),
                     MatAdd{}) == 10uz * shear &&
              concat(std::views::repeat(shear, 100'000'000uz), MatAdd{}) ==
                  100'000'000uz * shear);
// The certificate, checked on two matrices that do not commute: act is a
// right action, the product in the walk's order.
static_assert(dedekind::category::IsMonoid<M2, MatMul> &&
              dedekind::category::IsMonoidAction<decltype(act), V, M2> &&
              fib * shear != shear * fib &&
              act(act(V{1, 2}, fib), shear) == act(V{1, 2}, fib* shear));

// Listing 5: Listing 3's matrix chain, its length a parameter; its LLVM IR
// at -O2 -fno-unroll-loops with n at run time is the paper's loop, and with 10
// given, the value.
constexpr size_t fibonacci(size_t n) {
  const auto copies = n >> chain<size_t>() >> constant<size_t>(fib);
  return std::ranges::fold_left(copies, V{1, 0}, act)[1];
}
static_assert(fibonacci(0) == 0 && fibonacci(1) == 1 && fibonacci(10) == 55 &&
              fibonacci(90) == 2'880'067'194'370'816'120uz);
constexpr size_t fibonacci_10() { return fibonacci(10); }
static_assert(fibonacci_10() == 55);
// Listing 4: the same chain under the tree's fold; the action law and
// associativity compose, so 10^8 copies of the matrix are 27 steps, where
// the plain fold of 10^8 steps is refused at compile time.
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
    CHECK(L::listing3::fibonacci_fast(twelve) == 144);
    CHECK(
        C::fold(twelve >> C::chain<size_t>() >> C::constant<size_t>(size_t{7}),
                size_t{0}, std::plus<size_t>{}) == 84);
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
