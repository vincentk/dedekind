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
#include <cstdio>
#include <functional>
#include <optional>
#include <ranges>
#include <string>
#include <type_traits>
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

// Listing 1: values and arrows.  A function made a value, an arrow: copied,
// composed with >>, and replaced by its value at compile time.  The
// definitions are the tree's and the facts are checked here.
using dedekind::category::constant;
using dedekind::category::Copairing;
using dedekind::category::id;
using dedekind::category::is_antitone;
using dedekind::category::is_constant;
using dedekind::category::is_epic;
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
static_assert(id_b(true) == true && id_u(42uz) == 42 &&
              (id_u >> id_u)(42uz) == 42 && id_b == id_b >> id_b);
// Listing 1, the category of Jlt's types and functions after Pierce's FPL:
// three involutions, the identity law, each involution undoing itself.
using dedekind::category::equals;
using dedekind::category::One;
constexpr auto no = !id_b;   // not
constexpr auto neg = -id_u;  // negate
constexpr auto cpl = ~id_u;  // complement
static_assert(cpl == id_u >> cpl && id_b == no >> no && id_u == neg >> neg &&
              id_u == cpl >> cpl);
// A copy is as good as its original; the successor, a composite of two
// involutions, is an isomorphism and no involution; an arrow after a point
// is the point of its value (Pierce: iszero . zero = true).
constexpr auto twin = cpl;
constexpr auto succ = cpl >> neg;  // x + 1
constexpr auto zero = constant<One>(0uz);
constexpr auto yes = constant<One>(true);
static_assert(id_u == twin >> cpl && succ(41uz) == 42 && is_iso(succ) &&
              !is_involution(succ) && yes == zero >> equals(0uz));
// Constant arrows:
constexpr auto nil = constant<bool>(0uz);
constexpr auto one = constant<bool>(1uz);
constexpr auto ans = constant<bool>(42uz);
constexpr auto aye = constant<size_t>(true);
constexpr auto nay = constant<size_t>(false);
constexpr auto cpy = ans;  // a copy
static_assert(cpy(true) == ans(false) && (ans >> id_u)(true) == 42 &&
              (nil >> nay >> ans)(false) == 42);
// A constant absorbs the arrows before it: an equation between arrows.
static_assert(ans == nil >> nay >> ans);

// Listing 2, top: certificates on arrows alone, before any carrier.  A law
// is asked on the arrow's value (is_monic(f) reads what IsMonicArrow<F>
// reads), a composite's law derived by the engine in :posetal.
// Points and involutions:
static_assert(is_constant(ans) && is_monotone(ans) && is_antitone(aye) &&
              !is_monic(ans) && !is_epic(ans) && (!aye)(0uz) == false &&
              is_involution(!id_b) && is_constant(!aye) &&
              (~nil)(true) + 1 == 0 && is_involution(~id_u) &&
              is_constant(~nil) && (-one)(true) == (~nil)(true) &&
              is_involution(-id_u) && is_constant(-nil) &&
              is_iso(-id_u >> ~id_u) && !is_involution(-id_u >> ~id_u));
// Whether two arrows compare by the laws at all (the library's == on arrow
// terms, :f_algebra).
template <typename A, typename B>
concept Comparable = requires(const A& a, const B& b) { a == b; };

// Listing 3, bottom: a fold over a type.  The term's shape is its type, a
// composite of composites, depth 3; it is folded once to a value and once
// to a law, the law inferred from its legs'.
constexpr auto dec = neg >> cpl;   // x - 1
constexpr auto dec2 = dec >> dec;  // x - 2
static_assert(dec2(42uz) == 40 && is_iso(dec2));
static_assert(id_u == ~id_u >> ~id_u && id_u == cpl >> cpl);
static_assert((id_u == dec).verdict ==
              dedekind::category::Ternary::Unknown);  // undecided, not unequal
// Copilot's two counterexamples: different endpoints do not compare, and
// constants with different values are not equated by their type.
static_assert(Comparable<decltype(id_u), decltype(~id_u >> ~id_u)> &&
              !Comparable<decltype(id_b), decltype(~id_u >> ~id_u)>);
static_assert(((nil >> nay >> ans) == (nil >> (nay >> one))).verdict ==
              dedekind::category::Ternary::Unknown);
// Two composites of one type compare through their normal forms too.
static_assert((~id_u >> ~id_u) == (~id_u >> ~id_u) &&
              (dec >> ~id_u) == (-id_u >> (~id_u >> ~id_u)) &&
              (dec == (~id_u >> -id_u)).verdict ==
                  dedekind::category::Ternary::Unknown);
// A category's identity on size_t is the empty path too (the unit law for
// every identity arrow).
struct OnCounts {
  using Species = size_t;
  using Arrow = dedekind::category::Identity<size_t>;
  static constexpr Arrow unit_arrow() { return {}; }
};
static_assert(dedekind::category::IdentityIn<OnCounts>{} == ~id_u >> ~id_u);
// Products and coproducts:
using P = std::pair<bool, size_t>;
constexpr auto fst = Π<1, P>{};
constexpr auto snd = Π<2, P>{};
constexpr auto fan = Pairing{id_b, nil};  // ⟨1, 0⟩, Haskell's id &&& const 0
// The product's two equations, as arrow laws: a projection after the fanout
// is its leg.
static_assert(id_b == fan >> fst && nil == fan >> snd && is_epic(snd));
// Listing 2: the monoid a second time, by its two arrows: Listing 1's point
// zero, and addition as an arrow out of the product, id * id the identity on
// it.  The point's type does not record its value, so another point of the
// same type is no unit.
constexpr auto add = id_u * id_u >> std::plus<size_t>{};
static_assert(add(std::pair{40uz, 2uz}) == 42 && is_monoid(zero, add) &&
              !is_monoid(constant<One>(1uz), add));
constexpr auto inl = ι<1, bool, size_t>{};
constexpr auto ret = Copairing{id_b, nay};  // [1, ⊥]
static_assert(is_epic(fst) && !is_monic(fst) && is_monic(fan) &&
              !is_epic(fan) && (fan >> fst)(true) && !(fan >> fst)(false) &&
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

// Listing 3: Figure 2 in code.  Bottom, over a type: dec and dec2 above.
// Top left, over a value, the simplest case S = M: the counts' chain below
// the seed, folded on a monoid by the library's concat, a bounded range.
// Top right: the general fold, S acting on M, matrices on vectors, Fibonacci's,
// the standard's fold_left as it stands.  The checks the listing does not
// print (the chain is an input range of the right length; Collatz's chain,
// bounded by a budget declared with it) stay here.  chain, until, equals,
// arrow, bound, IsBoundedRange, IsFinite, IsTotalOrder and HasNNOStep are
// the library's.
namespace listing3 {
using dedekind::category::arrow;
using dedekind::category::bound;
using dedekind::category::chain;
using dedekind::category::equals;
using dedekind::category::HasNNOStep;
using dedekind::category::IsBoundedRange;
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
constexpr IsBoundedRange auto cs =  // 500, 250, …, 1
    1'000uz >> chain(prior, 200uz);
static_assert(bound(cs) == 200 && std::ranges::distance(cs) == 111 &&
              std::ranges::max(cs) == 9232);

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
struct Act {
  constexpr V operator()(V v, M2 a) const { return v * a; }
};
constexpr Act act{};
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
struct acting_monoid<jlt_listing::listing3::Act> {
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
              dedekind::category::IsRightAction<decltype(act), V, M2> &&
              fib * shear != shear * fib &&
              act(act(V{1, 2}, fib), shear) == act(V{1, 2}, fib* shear));
// The same certificate makes act a functor out of the matrices' delooping,
// the monoid as a category with one object: the action law is its
// composition law, and acting by the product is acting twice (#1113).
using Acts =
    dedekind::category::ActionFunctor<std::remove_cvref_t<decltype(act)>, V,
                                      M2>;
using Mat = dedekind::category::Element<M2, MatMul>;
static_assert(dedekind::category::IsFunctor<Acts> &&
              Acts{}(Mat{fib} >> Mat{shear})(V{1, 2}) ==
                  (Acts{}(Mat{fib}) >> Acts{}(Mat{shear}))(V{1, 2}) &&
              Acts{}(Mat{fib} >> Mat{shear})(V{1, 2}) ==
                  act(act(V{1, 2}, fib), shear));
// The semimodule's scaling is a left action of the counts, so a functor out
// of the opposite of their delooping: 3 then 4 scales by 4 · 3.
using Scaling =
    dedekind::category::LeftActionFunctor<M2, size_t, MatAdd, std::plus<size_t>,
                                          std::multiplies<size_t>,
                                          std::multiplies<>>;
using Count = dedekind::category::Element<
    size_t, dedekind::category::Opposite<std::multiplies<size_t>>>;
static_assert(dedekind::category::IsFunctor<Scaling> &&
              Scaling{}(Count{3} >> Count{4})(shear) ==
                  (Scaling{}(Count{3}) >> Scaling{}(Count{4}))(shear) &&
              Scaling{}(Count{3} >> Count{4})(shear) == 12uz * shear);
// On a sequence that is not copies the product has no fast form, so the
// library's fold is the plain one; the action law, acting once by the product
// in the walk's order, agrees with it on matrices that do not commute.
constexpr std::array<M2, 3> mixed{fib, shear, fib};
static_assert(fold(mixed, V{1, 2}, act) ==
                  std::ranges::fold_left(mixed, V{1, 2}, act) &&
              fold(mixed, V{1, 2}, act) ==
                  act(V{1, 2}, concat(mixed, MatMul{})));

// Listing 4: Listing 3's fold, by the library's fold in place of the
// standard's fold_left; the action law and associativity compose, so 10^8
// copies of the matrix are 27 steps, where the plain fold of 10^8 steps is
// refused at compile time.
constexpr size_t fibonacci(size_t n) {
  return fold(n >> chain<size_t>() >> constant<size_t>(fib), V{1, 0}, act)[1];
}
static_assert(fibonacci(0) == 0 && fibonacci(10) == fs_10[1] &&
              fibonacci(100'000'000) == 14'139'011'350'745'967'675uz);
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

// Appendix A's listing, read in the terms of the grammar's figure.
namespace reading {
template <typename T>
constexpr T op(T, T) = delete;

template <>
constexpr std::string op<std::string>(std::string a, std::string b) {
  return a + b;
}

template <typename T>
concept IsMagma = requires(const T a, const T b) {
  { op(a, b) } -> std::same_as<T>;
};

template <typename T>
  requires IsMagma<T>
constexpr T twice(T x) {
  return op(x, x);
}

/** @return the listing's closed term. */
constexpr bool closed_term() { return twice("ab"s) + "c"s == "ababc"s; }
static_assert(closed_term());
static_assert(IsMagma<std::string> && !IsMagma<int>);
}  // namespace reading

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

// Listing 1, self-contained: every name is declared here
// or is the standard's, so the page and a compiler suffice.  Left, Rule 1 in
// code: arrows on size_t as function objects, a composite's parts its fields,
// every call const and constexpr.  Succ and Pred are mutual inverses because
// unsigned arithmetic wraps; Neg is an involution; Half is not one-to-one.
// Right: a law as a table, a variable template and its specialisations,
// false where nothing is declared, declared at two atoms, inferred at a
// composite from its parts, read by a function on the value.  Pred is
// one-to-one and has no entry, Half is not one-to-one: both answer false,
// which means not known.
namespace listing1_self_contained {
using std::size_t;
struct Succ {
  constexpr size_t operator()(size_t x) const { return x + 1; }
};
struct Pred {
  constexpr size_t operator()(size_t x) const { return x - 1; }
};
struct Neg {
  constexpr size_t operator()(size_t x) const { return -x; }
};
struct Half {
  constexpr size_t operator()(size_t x) const { return x / 2; }
};
// A constant arrow: it returns the value it holds.
struct Const {
  size_t v;
  constexpr size_t operator()(size_t) const { return v; }
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
constexpr Then<F, G> operator>>(const F& f, const G& g) {
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
constexpr auto up = Neg{} >> Succ{};
static_assert((Succ{} >> Pred{})(0uz) == 0 && up(1uz) == 0 &&
              is_injective(up) && !is_injective(up >> Pred{}) &&
              !is_injective(up >> Half{}));
static_assert(constant<decltype(up >> Const{7uz})> &&
              constant<decltype(Const{7uz} >> up)> &&
              !constant<decltype(Neg{} >> Succ{})> &&
              !constant<std::remove_const_t<decltype(up)>> &&
              (up >> Const{7uz})(41uz) == 7 && Arrow<Const> &&
              IsConstant<decltype(up >> Const{7uz})> && !IsConstant<Succ>);
// Beyond the listing: the order of >> matters, the wrap that makes Succ and
// Pred inverses, the involution, the fold at depth, what Half identifies,
// and an arrow whose call is not const is no Arrow.
struct Mutating {
  constexpr size_t operator()(size_t x) { return x; }
};
constexpr size_t top = static_cast<size_t>(-1);
static_assert((Succ{} >> Neg{})(1uz) != 0 && (Pred{} >> Succ{})(0uz) == 0 &&
              (Succ{} >> Pred{})(top) == top && (Neg{} >> Neg{})(42uz) == 42 &&
              Half{}(4uz) == Half{}(5uz) &&
              is_injective(up >> up >> Succ{} >> Neg{}) &&
              !is_injective(Succ{} >> Half{} >> Neg{}) &&
              Arrow<Then<Succ, Neg>> && !Arrow<Mutating> && !injective<int>);
}  // namespace listing1_self_contained

// A probe, not a listing: which declarations of a trait a shape finds, by
// where they stand (Section 7, "Lookup stops at std").  The carriers are
// fundamental or standard types, so argument-dependent lookup adds nothing.
namespace lookup_probe {
// 1. A generic function before the shape.
template <typename T>
constexpr int early(T) = delete;
template <std::unsigned_integral T>
constexpr int early(T) {
  return 1;
}
template <typename T>
concept HasEarly = requires(T x) {
  { early(x) } -> std::same_as<int>;
};
constexpr bool generic_before = HasEarly<unsigned>;

// 2. A generic function after the shape; 3. an explicit specialisation after.
template <typename T>
constexpr int late(T) = delete;
template <typename T>
concept HasLate = requires(T x) {
  { late(x) } -> std::same_as<int>;
};
template <std::unsigned_integral T>
constexpr int late(T) {
  return 1;
}
template <>
constexpr int late<long>(long) {
  return 2;
}
constexpr bool generic_after = HasLate<unsigned>;
constexpr bool specialisation_after = HasLate<long>;

// 4. A generic function before the shape whose body calls the trait at the
// parts of a pair; the atom is an explicit specialisation after the shape.
template <typename T>
constexpr bool law(T) = delete;
template <typename F, typename G>
constexpr bool law(std::pair<F, G> p) {
  return law(p.first) && law(p.second);
}
template <typename T>
concept HasLaw = requires(T x) {
  { law(x) } -> std::same_as<bool>;
};
template <>
constexpr bool law<int>(int) {
  return true;
}
constexpr bool body_recursion_before =
    HasLaw<std::pair<int, std::pair<int, int>>> &&
    law(std::pair{1, std::pair{2, 3}});

// 5. An instance that requires its own shape at the parts, so after the shape.
template <typename T>
constexpr bool own(T) = delete;
template <typename T>
concept HasOwn = requires(T x) {
  { own(x) } -> std::same_as<bool>;
};
template <>
constexpr bool own<int>(int) {
  return true;
}
template <typename F, typename G>
  requires HasOwn<F> && HasOwn<G>
constexpr bool own(std::pair<F, G>) {
  return true;
}
constexpr bool own_shape_function = HasOwn<std::pair<int, int>>;

// 6. The same through a class template's partial specialisation.
template <typename T>
struct own_trait {};
template <typename T>
concept HasOwnTrait = requires {
  { own_trait<T>::value } -> std::convertible_to<bool>;
};
template <>
struct own_trait<int> {
  static constexpr bool value = true;
};
template <typename F, typename G>
  requires HasOwnTrait<F> && HasOwnTrait<G>
struct own_trait<std::pair<F, G>> {
  static constexpr bool value = true;
};
constexpr bool own_shape_class = HasOwnTrait<std::pair<int, int>>;
}  // namespace lookup_probe

// A probe, not a listing: the law of Listing 1's draft, constrained as Rule 2
// asks, in the two spellings of a trait: a function (deleted primary, explicit
// specialisations, one constrained generic case) and a table (a class
// template and its specialisations).  Atoms on size_t: Succ and Pred, mutual
// inverses because unsigned arithmetic wraps; Neg, an involution; Half, not
// injective, declared so; Twice, with no declaration.  -1 not declared, 0
// declared false, 1 true.
namespace spelling_probe {
using std::size_t;
struct Succ {
  constexpr size_t operator()(size_t x) const { return x + 1; }
};
struct Pred {
  constexpr size_t operator()(size_t x) const { return x - 1; }
};
struct Neg {
  constexpr size_t operator()(size_t x) const { return -x; }
};
struct Half {
  constexpr size_t operator()(size_t x) const { return x / 2; }
};
struct Twice {
  constexpr size_t operator()(size_t x) const { return 2 * x; }
};
template <typename F>
concept Arrow = requires(F f, size_t x) {
  { f(x) } -> std::same_as<size_t>;
};
template <Arrow F, Arrow G>
struct Then {
  F f;
  G g;
  constexpr size_t operator()(size_t x) const { return g(f(x)); }
};
constexpr size_t top = static_cast<size_t>(-1);
static_assert(Then<Succ, Pred>{}(0uz) == 0 && Then<Pred, Succ>{}(0uz) == 0 &&
              Then<Succ, Pred>{}(top) == top && Pred{}(0uz) == top &&
              Succ{}(top) == 0 && Then<Neg, Neg>{}(42uz) == 42 &&
              Neg{}(0uz) == 0 && Half{}(4uz) == Half{}(5uz) &&
              Twice{}(0uz) == Twice{}(size_t{1} << 63));

namespace by_function {
template <typename T>
constexpr bool monic(T) = delete;
template <typename T>
concept HasMonic = requires(T t) {
  { monic(t) } -> std::same_as<bool>;
};
template <typename F, typename G>
  requires HasMonic<F> && HasMonic<G>
constexpr bool monic(Then<F, G> t) {
  return monic(t.f) && monic(t.g);
}
template <>
constexpr bool monic<Succ>(Succ) {
  return true;
}
template <>
constexpr bool monic<Pred>(Pred) {
  return true;
}
template <>
constexpr bool monic<Neg>(Neg) {
  return true;
}
template <>
constexpr bool monic<Half>(Half) {
  return false;
}
template <typename T>
constexpr int ask() {
  if constexpr (HasMonic<T>) {
    return monic(T{}) ? 1 : 0;
  } else {
    return -1;
  }
}
}  // namespace by_function

namespace by_table {
template <typename T>
struct monic;
template <typename T>
concept HasMonic = requires {
  { monic<T>::value } -> std::convertible_to<bool>;
};
template <>
struct monic<Succ> {
  static constexpr bool value = true;
};
template <>
struct monic<Pred> {
  static constexpr bool value = true;
};
template <>
struct monic<Neg> {
  static constexpr bool value = true;
};
template <>
struct monic<Half> {
  static constexpr bool value = false;
};
template <typename F, typename G>
  requires HasMonic<F> && HasMonic<G>
struct monic<Then<F, G>> {
  static constexpr bool value = monic<F>::value && monic<G>::value;
};
template <typename T>
constexpr int ask() {
  if constexpr (HasMonic<T>) {
    return monic<T>::value ? 1 : 0;
  } else {
    return -1;
  }
}
}  // namespace by_table
}  // namespace spelling_probe

// A probe, not a listing: equality of arrow terms as identity of their
// types.  Composing re-brackets to the right, a recursion over the term's
// type, so a term's type is its word in the atoms, and two terms equal up to
// bracketing have one type.  The comparison calls neither arrow.
namespace equality_probe {
using spelling_probe::Neg;
using spelling_probe::Pred;
using spelling_probe::Succ;
using spelling_probe::Then;
template <typename F, typename G>
constexpr Then<F, G> operator>>(F f, G g) {
  return Then<F, G>{f, g};
}
template <typename F, typename G, typename H>
constexpr auto operator>>(Then<F, G> t, H h) {
  return t.f >> (t.g >> h);
}
template <typename F, typename G>
constexpr bool same(F, G) {
  return std::same_as<F, G>;
}
constexpr bool rebrackets =
    same((Succ{} >> Neg{}) >> Pred{}, Succ{} >> (Neg{} >> Pred{}));
constexpr bool rebrackets_deep =
    same(((Succ{} >> Neg{}) >> (Pred{} >> Succ{})) >> Neg{},
         Succ{} >> (Neg{} >> (Pred{} >> (Succ{} >> Neg{}))));
constexpr bool tells_words_apart = !same(Succ{} >> Neg{}, Neg{} >> Succ{});
constexpr bool computes =
    ((Succ{} >> Neg{}) >> Pred{})(41uz) == (Succ{} >> (Neg{} >> Pred{}))(41uz);
}  // namespace equality_probe

// Listing 2, self-contained: the monoid as the triple (S, +, 0).  The shape
// and concat take the carrier as a parameter, and the unit is passed as a
// value of the carrier; the law is one table, with its entries at concrete
// carriers (an entry for a family cannot fix a unit whose type is the
// family's parameter: see carrier_probe below).  std::plus and
// std::multiplies are used as the standard gives them; a wrong unit, and an
// operation with no entry, are refused.
namespace listings_2_to_4 {
using listing1_self_contained::Const;
using listing1_self_contained::IsConstant;
using std::size_t;
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
// Associativity is declared.  The unit is the programmer's claim, made by
// naming it, and the shape checks it at one point.  The check refutes and
// does not prove: 1 is refused for plus, 0 is not refused for multiplies.
static_assert(!IsMonoid<size_t, std::plus<size_t>, 1uz> &&
              IsMonoid<size_t, std::multiplies<size_t>, 0uz>);
// A type declared associative that has no call is refused by the conjunct
// before the check, so the check is never formed: false, and no error.
struct NoCall {};
template <>
constexpr bool associative<size_t, NoCall> = true;
static_assert(!IsMonoid<size_t, NoCall, 0uz>);
template <typename T, typename Op, T e>
  requires IsMonoid<T, Op, e>
constexpr T concat() {
  return e;
}
template <typename T, typename Op, T e>
  requires IsMonoid<T, Op, e>
constexpr T concat(T v, T w) {
  return Op{}(v, w);
}
static_assert(concat<size_t, std::plus<size_t>, 0uz>() == 0 &&
              concat<size_t, std::plus<size_t>, 0uz>(40uz, 2uz) == 42 &&
              concat<size_t, std::multiplies<size_t>, 1uz>(6uz, 7uz) == 42 &&
              !IsMonoid<size_t, std::plus<size_t>, 1uz> &&
              !IsMonoid<size_t, std::minus<size_t>, 0uz> &&
              IsMonoid<size_t, std::plus<size_t>, 0uz>);
// Listing 2, lower panels: a call that Rule 2 refuses, in the listing's own
// names.  The value of an empty fold is 0 unless the operation is known to
// have the unit 1; guessed asks for neither, and its result varies with a
// law it does not declare.  The linter reports the same body in its fixture
// (tools/jlt-lint/fixtures/rule2-decided-resolution.cpp).  Three callers
// declare the dependence: by requiring the shape, by negating it, and by
// naming the call as a requirement.
template <typename Op>
constexpr size_t empty(Op) {
  return 0;
}
template <typename Op>
  requires IsMonoid<size_t, Op, 1uz>
constexpr size_t empty(Op) {
  return 1;
}
template <typename Op>
constexpr size_t guessed(Op op) {
  return empty(op);
}
static_assert(guessed(std::plus<size_t>{}) == 0 &&
              guessed(std::multiplies<size_t>{}) == 1);
template <typename Op>
  requires IsMonoid<size_t, Op, 1uz>
constexpr size_t of_a_product(Op op) {
  return empty(op);
}
template <typename Op>
  requires(!IsMonoid<size_t, Op, 1uz>)
constexpr size_t of_the_rest(Op op) {
  return empty(op);
}
template <typename Op>
concept HasEmpty = requires(Op op) {
  { empty(op) } -> std::same_as<size_t>;
};
template <HasEmpty Op>
constexpr size_t named(Op op) {
  return empty(op);
}
static_assert(of_a_product(std::multiplies<size_t>{}) == 1 &&
              of_the_rest(std::plus<size_t>{}) == 0 &&
              named(std::plus<size_t>{}) == 0 &&
              concat<size_t, std::multiplies<size_t>, 1uz>() == 1);

// Listing 3, left: concat at any length, a fold over a range that declares
// its bound, here a count-down to the origin.
constexpr auto ns = std::views::reverse(std::views::iota(0uz, 10uz));
template <typename T, typename Op, T e, std::ranges::sized_range R>
  requires IsMonoid<T, Op, e> && std::same_as<std::ranges::range_value_t<R>, T>
constexpr T concat(R r) {
  return std::ranges::fold_left(r, e, Op{});
}
static_assert(concat<size_t, std::plus<size_t>, 0uz>(ns) == 45 &&
              concat<size_t, std::multiplies<size_t>, 1uz>(
                  std::views::iota(1uz, 5uz)) == 24);
// Beyond the listing: the count-down starts at 9 and ends at the origin, and
// the empty range gives the unit.
static_assert(
    *ns.begin() == 9 && *std::ranges::prev(ns.end()) == 0 &&
    std::ranges::size(ns) == 10 &&
    concat<size_t, std::plus<size_t>, 0uz>(std::views::iota(0uz, 0uz)) == 0 &&
    concat<size_t, std::multiplies<size_t>, 1uz>(std::views::iota(0uz, 0uz)) ==
        1);

// Listing 4: a declared law buys something.  concat over n copies of one
// value takes at most 64 steps whatever n is, by repeated squaring, which
// regroups the copies and is licensed by the monoid's associativity.  It is
// one more overload of concat, chosen for a range of copies, and itself a
// bounded fold, over the bits of n.  At a second carrier, a 2x2 matrix under
// multiplication with the identity matrix as its unit, the same concat gives
// a Fibonacci number.
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
template <typename T, typename Add, typename Times, T zero, T one>
concept IsSemiringOver = IsMonoid<T, Add, zero> && IsMonoid<T, Times, one> &&
                         commutative<T, Add> && distributes<T, Times, Add>;
// The listing's shape: a semiring under the type's own + and *.
template <typename T, T zero, T one>
concept IsSemiring =
    IsMonoid<T, std::plus<T>, zero> && IsMonoid<T, std::multiplies<T>, one> &&
    commutative<T, std::plus<T>> &&
    distributes<T, std::multiplies<T>, std::plus<T>>;
// 2x2 matrices over a semiring, row by row.  The product names the calls it
// makes through the shape.
template <typename T, typename Add, typename Times, T zero, T one>
struct MatMulOver {
  constexpr std::array<T, 4> operator()(std::array<T, 4> a,
                                        std::array<T, 4> b) const
    requires IsSemiringOver<T, Add, Times, zero, one>
  {
    return {Add{}(Times{}(a[0], b[0]), Times{}(a[1], b[2])),
            Add{}(Times{}(a[0], b[1]), Times{}(a[1], b[3])),
            Add{}(Times{}(a[2], b[0]), Times{}(a[3], b[2])),
            Add{}(Times{}(a[2], b[1]), Times{}(a[3], b[3]))};
  }
};
// The inferred law: the matrices' product is associative, provided their
// entries form a semiring.  An entry with a context, and no law of its own.
template <typename T, typename Add, typename Times, T zero, T one>
  requires IsSemiringOver<T, Add, Times, zero, one>
constexpr bool
    associative<std::array<T, 4>, MatMulOver<T, Add, Times, zero, one>> = true;
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
// The inference, asked directly: a semiring below gives a monoid of matrices,
// and where the entries are no semiring nothing is known.
static_assert(IsSemiringOver<size_t, std::plus<size_t>, std::multiplies<size_t>,
                             0uz, 1uz> &&
              IsSemiring<size_t, 0uz, 1uz> && associative<M, Mul> &&
              !commutative<M, Mul> && IsMonoid<M, Mul, identity> &&
              !IsSemiringOver<size_t, std::minus<size_t>,
                              std::multiplies<size_t>, 0uz, 1uz>);
// The general form, on arrays and with the two operations as parameters.
using A = std::array<size_t, 4>;
static_assert(associative<A, MatMulOver<size_t, std::plus<size_t>,
                                        std::multiplies<size_t>, 0uz, 1uz>> &&
              !associative<A, MatMulOver<size_t, std::minus<size_t>,
                                         std::multiplies<size_t>, 0uz, 1uz>>);
// A second semiring costs four entries and no code: the bits of size_t under
// | and &, with units 0 and all ones.  The same concat then composes
// relations: the swap of two nodes, taken twice, is the identity relation,
// and taken three times is itself.
template <>
constexpr bool associative<size_t, std::bit_or<size_t>> = true;
template <>
constexpr bool associative<size_t, std::bit_and<size_t>> = true;
template <>
constexpr bool commutative<size_t, std::bit_or<size_t>> = true;
template <>
constexpr bool distributes<size_t, std::bit_and<size_t>, std::bit_or<size_t>> =
    true;
constexpr size_t top = ~0uz;
using Rel =
    MatMulOver<size_t, std::bit_or<size_t>, std::bit_and<size_t>, 0uz, top>;
constexpr A swap_nodes{0, top, top, 0};
static_assert(power<A, Rel, A{top, 0, 0, top}>(swap_nodes, 2uz) ==
                  A{top, 0, 0, top} &&
              power<A, Rel, A{top, 0, 0, top}>(swap_nodes, 3uz) == swap_nodes);
// The listing's assertions.  One function at two monoids of size_t: under * it
// is 2 to the power 10, under + it is 2 times 10.
static_assert(power<size_t, std::multiplies<size_t>, 1uz>(2uz, 10uz) == 1024 &&
              power<size_t, std::plus<size_t>, 0uz>(2uz, 10uz) == 20 &&
              IsMonoid<M, Mul, identity> && !commutative<M, Mul> &&
              power<M, Mul, identity>(fib, 10uz).b == 55 &&
              power<M, Mul, identity>(fib, 100'000'000uz).b ==
                  14'139'011'350'745'967'675uz &&
              concat<size_t, std::plus<size_t>, 0uz>(std::views::transform(
                  std::views::iota(0uz, 10uz),
                  listing1_self_contained::up >> Const{7uz})) == 70);
// The lemma selects: a composite with a constant part is constant, so its
// image takes the power.  10^12 copies can only finish that way.
static_assert(concat<size_t, std::plus<size_t>, 0uz>(std::views::transform(
                  std::views::iota(0uz, 1'000'000'000'000uz),
                  listing1_self_contained::up >> Const{7uz})) ==
              7'000'000'000'000uz);
// Beyond the listing: the scale at +, no copies give the unit, a few copies
// agree with the plain fold, and a matrix that is not the unit is refused.
static_assert(concat<size_t, std::plus<size_t>, 0uz>(std::views::transform(
                  std::views::iota(0uz, 1'000'000'000'000uz), Const{7uz})) ==
                  7'000'000'000'000uz &&
              concat<size_t, std::plus<size_t>, 0uz>(std::views::transform(
                  std::views::iota(0uz, 0uz), Const{7uz})) == 0 &&
              concat<size_t, std::plus<size_t>, 0uz>(std::views::transform(
                  std::views::iota(0uz, 10uz), Const{7uz})) == 70 &&
              concat<size_t, std::multiplies<size_t>, 1uz>(
                  std::views::transform(std::views::iota(0uz, 13uz),
                                        Const{3uz})) == 1'594'323 &&
              !IsMonoid<M, Mul, fib>);

// The standard's structural types: a value of one can be lifted into a type,
// and the compiler decides the identity of two lifted values member by
// member, with no operator== of ours.  Listing 1's arrows are structural, and
// so is an arrow that carries a value.
template <auto v>
struct Lift {};
struct Add {
  size_t k;
  constexpr size_t operator()(size_t x) const { return x + k; }
};
using listing1_self_contained::Neg;
using listing1_self_contained::Succ;
using listing1_self_contained::Then;
static_assert(std::same_as<Lift<Add{3}>, Lift<Add{3}>> &&
              !std::same_as<Lift<Add{3}>, Lift<Add{4}>> &&
              std::same_as<Lift<Then<Succ, Neg>{}>, Lift<Then<Succ, Neg>{}>> &&
              std::same_as<Lift<Then<Add, Add>{Add{1}, Add{2}}>,
                           Lift<Then<Add, Add>{Add{1}, Add{2}}>> &&
              !std::same_as<Lift<Then<Add, Add>{Add{1}, Add{2}}>,
                            Lift<Then<Add, Add>{Add{2}, Add{1}}>>);
}  // namespace listings_2_to_4

// Listing 3, right: the recursion over a type, on Listing 1's names.
// Composition that re-brackets to the right, so two terms that differ only in
// bracketing have one type; and a second fold over the term's type, its
// length, with + where Listing 1's law has &&.
namespace listing4_self_contained {
using listing1_self_contained::Arrow;
using listing1_self_contained::is_injective;
using listing1_self_contained::Neg;
using listing1_self_contained::Pred;
using listing1_self_contained::Succ;
using listing1_self_contained::Then;
using std::size_t;

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
constexpr auto a = then(Succ{} >> Neg{}, Succ{});
constexpr auto b = then(Succ{}, Neg{} >> Succ{});
static_assert(std::same_as<decltype(a), decltype(b)> &&
              length<Then<Succ, Then<Neg, Succ>>> == 3 && is_injective(a));
// Beyond the listing: the one type is the right-nested one, the two terms
// compute the same, a deeper left nesting flattens too, and >> itself does
// not re-bracket.
static_assert(std::same_as<decltype(a), const Then<Succ, Then<Neg, Succ>>> &&
              a(41uz) == b(41uz) &&
              std::same_as<decltype(then((Succ{} >> Neg{}) >> Pred{}, Succ{})),
                           Then<Succ, Then<Neg, Then<Pred, Succ>>>> &&
              !std::same_as<decltype((Succ{} >> Neg{}) >> Succ{}),
                            decltype(Succ{} >> (Neg{} >> Succ{}))> &&
              length<Succ> == 1);
}  // namespace listing4_self_contained

// A probe, not a listing: the monoid's carrier a parameter, any unsigned
// integer, with the unit still a value.  An entry for the family cannot fix a
// value whose type depends on the entry's own parameter, so two spellings are
// tried: (a) the value stays a parameter and the entry constrains it; (b) the
// carrier is deduced from the unit, which does not compile (see below).
// Appendix A's worked program, each declaration one form of the kernel: a
// trait's default, an entry, a shape with a law and a required call, a generic
// function, and the closed term the program evaluates.
namespace appendix_a_reading {
using namespace std::string_literals;
template <typename T, typename Op>
constexpr bool associative = false;
template <>
constexpr bool associative<std::string, std::plus<std::string>> = true;
template <typename T, typename Op>
concept IsSemigroup = associative<T, Op> && requires(const T a, const T b) {
  { Op{}(a, b) } -> std::same_as<T>;
};
template <typename T, typename Op>
  requires IsSemigroup<T, Op>
constexpr T twice(T x) {
  return Op{}(x, x);
}
static_assert(twice<std::string, std::plus<std::string>>("ab"s) + "c"s ==
              "ababc"s);
static_assert(!IsSemigroup<std::string, std::minus<std::string>>);
}  // namespace appendix_a_reading

// A probe, not yet a listing: a trait read at a type variable only through a
// shape that names it.  The shape IsInjective reads the trait, the entry at a
// composite has the shape as its context at the two parts, and the reader's
// body asks the shape, never the trait.  The second half does the same for a
// trait whose value is a number.
namespace badge_probe {
using std::size_t;
namespace l1 = listing1_self_contained;
template <typename F>
constexpr bool injective = false;
template <>
constexpr bool injective<l1::Succ> = true;
template <>
constexpr bool injective<l1::Neg> = true;
template <typename F>
concept IsInjective = l1::Arrow<F> && injective<F>;
template <IsInjective F, IsInjective G>
constexpr bool injective<l1::Then<F, G>> = true;
// The reader has its own name here: the arrows are Listing 1's, so a second
// is_injective would be found beside Listing 1's by argument-dependent lookup.
template <l1::Arrow F>
constexpr bool wears_badge(F) {
  return IsInjective<F>;
}
constexpr auto up = l1::Neg{} >> l1::Succ{};
static_assert(wears_badge(up) && wears_badge(up >> up) &&
              !wears_badge(up >> l1::Pred{}) &&
              !wears_badge(up >> l1::Half{}) && !wears_badge(l1::Pred{} >> up));

template <typename F>
constexpr size_t length = 1;
template <typename F>
concept HasLength = requires {
  { length<F> } -> std::convertible_to<size_t>;
};
template <HasLength F, HasLength G>
constexpr size_t length<l1::Then<F, G>> = length<F> + length<G>;
static_assert(length<l1::Then<l1::Succ, l1::Then<l1::Neg, l1::Succ>>> == 3 &&
              length<l1::Succ> == 1);
}  // namespace badge_probe

namespace carrier_probe {
template <typename T, T e>
concept IsZero = e == T{0};
template <typename T, T e>
concept IsOne = e == T{1};
template <std::unsigned_integral T, typename Op, T e>
constexpr bool monoid_a = false;
template <std::unsigned_integral T, T e>
  requires IsZero<T, e>
constexpr bool monoid_a<T, std::plus<T>, e> = true;
template <std::unsigned_integral T, T e>
  requires IsOne<T, e>
constexpr bool monoid_a<T, std::multiplies<T>, e> = true;
static_assert(
    monoid_a<std::size_t, std::plus<std::size_t>, 0uz> &&
    monoid_a<std::uint8_t, std::multiplies<std::uint8_t>, std::uint8_t{1}> &&
    !monoid_a<std::size_t, std::plus<std::size_t>, 1uz>);

// Spelling (b), the carrier deduced from the unit with entries such as
// monoid_b<std::plus<T>, T{0}>, is refused by the compiler: "type of
// specialized non-type template argument depends on a template parameter of
// the partial specialization".  So a family's entry cannot fix its unit, and
// either the entry constrains the value, as above, or the entries stay at
// concrete carriers, as in Listing 2.
}  // namespace carrier_probe

// A probe, not yet a listing: a monoid as a triple of types, as the algebraic
// statement has it: a carrier, a binary operation and a nullary one.  The
// unit is the type of a function object of no arguments, so no value stands
// among the template's parameters and the carrier need not be a structural
// type: strings under concatenation are a monoid here.  Then the free theorem
// of concat on two monoids and a homomorphism between them: the length of a
// concatenation is the sum of the lengths.
namespace triple_probe {
using std::size_t;
using namespace std::string_literals;

template <typename T, typename Op>
constexpr bool associative = false;
template <>
constexpr bool associative<size_t, std::plus<size_t>> = true;
template <>
constexpr bool associative<size_t, std::multiplies<size_t>> = true;
template <>
constexpr bool associative<std::string, std::plus<std::string>> = true;

struct Zero {
  constexpr size_t operator()() const { return 0; }
};
struct One {
  constexpr size_t operator()() const { return 1; }
};
struct Empty {
  constexpr std::string operator()() const { return {}; }
};

// That E is the unit of Op on T is an axiom like the others, an entry of a
// trait, so that a tool lists it.  The shape also checks it at one point,
// which can refute it and cannot prove it.
template <typename T, typename Op, typename E>
constexpr bool unital = false;
template <>
constexpr bool unital<size_t, std::plus<size_t>, Zero> = true;
template <>
constexpr bool unital<size_t, std::multiplies<size_t>, One> = true;
template <>
constexpr bool unital<std::string, std::plus<std::string>, Empty> = true;

// The shape, built up as algebra builds it: a magma is a carrier with an
// operation, a unital magma has a unit as well, and a monoid is a unital
// magma whose operation is associative.
template <typename T, typename Op>
concept IsMagma = std::regular<T> && requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
};
template <typename T, typename Op, typename E>
concept IsUnitalMagma = IsMagma<T, Op> && unital<T, Op, E> && requires {
  { E{}() } -> std::same_as<T>;
} && Op{}(E{}(), E{}()) == E{}();
template <typename T, typename Op, typename E>
concept IsMonoid = IsUnitalMagma<T, Op, E> && associative<T, Op>;

template <typename T, typename Op, typename E>
  requires IsMonoid<T, Op, E>
constexpr T concat() {
  return E{}();
}
template <typename T, typename Op, typename E>
  requires IsMonoid<T, Op, E>
constexpr T concat(T v, T w) {
  return Op{}(v, w);
}
template <typename T, typename Op, typename E, std::ranges::sized_range R>
  requires IsMonoid<T, Op, E> && std::same_as<std::ranges::range_value_t<R>, T>
constexpr T concat(R r) {
  return std::ranges::fold_left(r, E{}(), Op{});
}

using Str = std::string;
using Cat = std::plus<std::string>;

// The triple, at size_t twice and at strings; a wrong unit, an operation
// that is not associative and a unit of another carrier are refused.
static_assert(IsMonoid<size_t, std::plus<size_t>, Zero> &&
              IsMonoid<size_t, std::multiplies<size_t>, One> &&
              IsMonoid<Str, Cat, Empty> &&
              !IsMonoid<size_t, std::plus<size_t>, One> &&
              !IsMonoid<size_t, std::minus<size_t>, Zero> &&
              !IsMonoid<Str, Cat, Zero>);
static_assert(concat<size_t, std::plus<size_t>, Zero>() == 0 &&
              concat<size_t, std::plus<size_t>, Zero>(40uz, 2uz) == 42 &&
              concat<size_t, std::multiplies<size_t>, One>(6uz, 7uz) == 42 &&
              concat<Str, Cat, Empty>() == ""s &&
              concat<Str, Cat, Empty>("ab"s, "c"s) == "abc"s &&
              concat<Str, Cat, Empty>(std::array{"ab"s, "c"s, "def"s}) ==
                  "abcdef"s);

// A homomorphism from strings to size_t: the length.  That it is one is a
// law somebody states; here it is checked at one point.
struct Size {
  constexpr size_t operator()(Str s) const { return s.size(); }
};
static_assert(Size{}(Empty{}()) == Zero{}() &&
              Size{}("ab"s + "c"s) == Size{}("ab"s) + Size{}("c"s));

// The free theorem of concat, on these two monoids and this homomorphism:
// the length of the concatenation is the concat of the lengths.
static_assert(Size{}(concat<Str, Cat, Empty>(std::array{"ab"s, "c"s,
                                                        "def"s})) ==
              concat<size_t, std::plus<size_t>, Zero>(std::views::transform(
                  std::array{"ab"s, "c"s, "def"s}, Size{})));

// The check at one point, on two false axioms.  It refutes the first, since
// 2 + 2 is not 2.  It does not refute the second: true is not the unit of ||,
// but true || true is true, as for any idempotent element.  So the check is a
// check and not a proof, and the axiom stays the programmer's.
struct True {
  constexpr bool operator()() const { return true; }
};
struct Two {
  constexpr size_t operator()() const { return 2; }
};
using And = std::logical_and<bool>;
using Or = std::logical_or<bool>;
template <>
constexpr bool associative<bool, And> = true;
template <>
constexpr bool associative<bool, Or> = true;
template <>
constexpr bool unital<bool, And, True> = true;
template <>
constexpr bool unital<size_t, std::plus<size_t>, Two> = true;  // false
template <>
constexpr bool unital<bool, Or, True> = true;  // false
static_assert(IsMonoid<bool, And, True> &&
              !IsMonoid<size_t, std::plus<size_t>, Two> &&
              IsMonoid<bool, Or, True>);

// A candidate first listing.  concat1 is what one writes without thinking:
// it hard-codes the operation, +, and the unit, a default-constructed value.
// Its signature asks for nothing of the kind.
template <std::ranges::range R>
constexpr std::ranges::range_value_t<R> concat1(R r) {
  std::ranges::range_value_t<R> s{};
  for (auto x : r) s = s + x;
  return s;
}
// concat2 names the monoid in its signature and uses nothing else.
template <typename T, typename Op, typename E, std::ranges::sized_range R>
  requires IsMonoid<T, Op, E> && std::same_as<std::ranges::range_value_t<R>, T>
constexpr T concat2(R r) {
  return std::ranges::fold_left(r, E{}(), Op{});
}
// At two lucky monoids the two agree.
static_assert(concat1(std::array{1uz, 2uz, 3uz}) == 6 &&
              concat2<size_t, std::plus<size_t>, Zero>(std::array{1uz, 2uz,
                                                                  3uz}) == 6 &&
              concat1(std::array{"ab"s, "c"s}) == "abc"s &&
              concat2<Str, Cat, Empty>(std::array{"ab"s, "c"s}) == "abc"s);
// At a third, concat1 compiles, runs and is wrong: bool + bool is "or" from
// false, and the monoid is "and" from true.
static_assert(concat1(std::array{true, false}) == true &&
              concat2<bool, And, True>(std::array{true, false}) == false);
// The minimal monoid: a value with an equality, an operation and a unit, and
// nothing else.  concat2 is defined there, so its body uses only what its
// signature names.  concat1 is not: it does not compile at Bare, which no
// assertion can say and a test that expects the refusal will hold.
struct Bare {
  constexpr bool operator==(const Bare&) const = default;
};
struct BareOp {
  constexpr Bare operator()(Bare, Bare) const { return {}; }
};
struct BareUnit {
  constexpr Bare operator()() const { return {}; }
};
template <>
constexpr bool associative<Bare, BareOp> = true;
template <>
constexpr bool unital<Bare, BareOp, BareUnit> = true;
static_assert(IsMonoid<Bare, BareOp, BareUnit> &&
              concat2<Bare, BareOp, BareUnit>(std::array{Bare{}, Bare{}}) ==
                  Bare{});
// What C++ itself says of concat1 at Bare: asked whether the call is valid,
// it answers yes, since it reads the signature and not the body.  The call
// does not compile.  For concat2 the same question has the right answers.
template <typename R>
concept Accepts1 = requires(R r) { concat1(r); };
template <typename T, typename Op, typename E, typename R>
concept Accepts2 = requires(R r) { concat2<T, Op, E>(r); };
static_assert(
    Accepts1<std::array<Bare, 2>> &&
    Accepts2<Bare, BareOp, BareUnit, std::array<Bare, 2>> &&
    !Accepts2<size_t, std::minus<size_t>, Zero, std::array<size_t, 3>>);

// A candidate second listing: the power of a monoid.  power_plain is its
// definition, concat2 over n copies of one value, in n steps.  power computes
// the same in as many steps as n has bits, which is right because the
// operation is associative.
template <typename T>
struct Always {
  T x;
  constexpr T operator()(size_t) const { return x; }
};
template <typename T, typename Op, typename E>
  requires IsMonoid<T, Op, E>
constexpr T power_plain(T x, size_t n) {
  return concat2<T, Op, E>(
      std::views::transform(std::views::iota(0uz, n), Always<T>{x}));
}
template <typename T>
struct Power {
  T product;
  T base;
  size_t exponent;
};
template <typename T, typename Op, typename E>
struct Halving {
  constexpr Power<T> operator()(Power<T> p, size_t) const
    requires IsMonoid<T, Op, E>
  {
    return {p.exponent % 2 == 1 ? Op{}(p.product, p.base) : p.product,
            Op{}(p.base, p.base), p.exponent / 2};
  }
};
template <typename T, typename Op, typename E>
  requires IsMonoid<T, Op, E>
constexpr T power(T x, size_t n) {
  return std::ranges::fold_left(
             std::views::iota(0uz, size_t(std::bit_width(n))),
             Power<T>{E{}(), x, n}, Halving<T, Op, E>{})
      .product;
}
// A law selects a faster case on the same signature: where the operation is
// idempotent, a power is the value itself, in one step.
template <typename T, typename Op>
constexpr bool idempotent = false;
template <>
constexpr bool idempotent<bool, And> = true;
template <>
constexpr bool idempotent<bool, Or> = true;
template <typename T, typename Op, typename E>
concept IsIdempotentMonoid = IsMonoid<T, Op, E> && idempotent<T, Op>;
template <typename T, typename Op, typename E>
  requires IsIdempotentMonoid<T, Op, E>
constexpr T power(T x, size_t n) {
  return n == 0 ? E{}() : x;
}
// One function, a meaning at each monoid: a multiple, a power, a repetition.
static_assert(power<size_t, std::plus<size_t>, Zero>(2uz, 10uz) == 20 &&
              power<size_t, std::multiplies<size_t>, One>(2uz, 10uz) == 1024 &&
              power<Str, Cat, Empty>("ab"s, 3uz) == "ababab"s &&
              power<bool, And, True>(false, 5uz) == false &&
              power<bool, And, True>(false, 0uz) == true &&
              power_plain<size_t, std::plus<size_t>, Zero>(2uz, 10uz) == 20 &&
              power_plain<size_t, std::multiplies<size_t>, One>(2uz, 10uz) ==
                  1024 &&
              power_plain<Str, Cat, Empty>("ab"s, 3uz) == "ababab"s &&
              power_plain<bool, And, True>(false, 5uz) == false &&
              power<size_t, std::multiplies<size_t>, One>(3uz,
                                                          1'000'000'000uz) ==
                  9'215'150'800'219'179'009uz);

// The semiring: two monoids on one carrier, with three more laws, and the map
// that says what a count stands for in it.  Under it a multiple is one
// product.  size_t and bool are semirings; strings have one monoid only.
struct False {
  constexpr bool operator()() const { return false; }
};
template <>
constexpr bool unital<bool, Or, False> = true;
template <typename T, typename Op>
constexpr bool commutative = false;
template <>
constexpr bool commutative<size_t, std::plus<size_t>> = true;
template <>
constexpr bool commutative<size_t, std::multiplies<size_t>> = true;
template <>
constexpr bool commutative<bool, And> = true;
template <>
constexpr bool commutative<bool, Or> = true;
template <typename T, typename Mul, typename Add>
constexpr bool distributes = false;
template <>
constexpr bool distributes<size_t, std::multiplies<size_t>, std::plus<size_t>> =
    true;
template <>
constexpr bool distributes<bool, And, Or> = true;
struct Count {
  constexpr size_t operator()(size_t n) const { return n; }
};
struct Positive {
  constexpr bool operator()(size_t n) const { return n > 0; }
};
template <typename T, typename Add, typename Mul, typename Num>
constexpr bool counts = false;
template <>
constexpr bool
    counts<size_t, std::plus<size_t>, std::multiplies<size_t>, Count> = true;
template <>
constexpr bool counts<bool, Or, And, Positive> = true;
template <typename T, typename Add, typename Nil, typename Mul, typename Unit,
          typename Num>
concept IsSemiring =
    IsMonoid<T, Add, Nil> && IsMonoid<T, Mul, Unit> && commutative<T, Add> &&
    distributes<T, Mul, Add> && counts<T, Add, Mul, Num> && requires(size_t n) {
      { Num{}(n) } -> std::same_as<T>;
    };
template <typename T, typename Add, typename Nil, typename Mul, typename Unit,
          typename Num>
  requires IsSemiring<T, Add, Nil, Mul, Unit, Num>
constexpr T multiple(T x, size_t n) {
  return Mul{}(Num{}(n), x);
}
static_assert(IsSemiring<size_t, std::plus<size_t>, Zero,
                         std::multiplies<size_t>, One, Count> &&
              IsSemiring<bool, Or, False, And, True, Positive> &&
              multiple<size_t, std::plus<size_t>, Zero, std::multiplies<size_t>,
                       One, Count>(2uz, 10uz) ==
                  power<size_t, std::plus<size_t>, Zero>(2uz, 10uz) &&
              multiple<bool, Or, False, And, True, Positive>(true, 3uz) ==
                  power<bool, Or, False>(true, 3uz) &&
              multiple<bool, Or, False, And, True, Positive>(true, 0uz) ==
                  power<bool, Or, False>(true, 0uz) &&
              multiple<bool, Or, False, And, True, Positive>(false, 3uz) ==
                  power<bool, Or, False>(false, 3uz));

// A law inferred from a law of another kind.  Where the operation commutes,
// raising to a power is a homomorphism of the monoid to itself: (x.y)^n is
// x^n.y^n.  The lemma is stated once, and the compiler applies it.
template <typename T, typename Op, typename E>
struct PowerBy {
  size_t n;
  constexpr T operator()(T x) const
    requires IsMonoid<T, Op, E>
  {
    return power<T, Op, E>(x, n);
  }
};
template <typename T, typename Op, typename E>
concept IsCommutativeMonoid = IsMonoid<T, Op, E> && commutative<T, Op>;
template <typename H, typename T, typename Op, typename E>
constexpr bool endomorphism = false;
template <typename T, typename Op, typename E>
  requires IsCommutativeMonoid<T, Op, E>
constexpr bool endomorphism<PowerBy<T, Op, E>, T, Op, E> = true;
static_assert(endomorphism<PowerBy<size_t, std::plus<size_t>, Zero>, size_t,
                           std::plus<size_t>, Zero> &&
              endomorphism<PowerBy<size_t, std::multiplies<size_t>, One>,
                           size_t, std::multiplies<size_t>, One> &&
              endomorphism<PowerBy<bool, And, True>, bool, And, True> &&
              !endomorphism<PowerBy<Str, Cat, Empty>, Str, Cat, Empty>);
// The free theorem of concat2 then applies where the homomorphism was
// inferred: three times a sum is the sum of three times each.  At strings it
// was not inferred, and it does not hold.
static_assert(
    PowerBy<size_t, std::plus<size_t>, Zero>{3uz}(
        concat2<size_t, std::plus<size_t>, Zero>(std::array{1uz, 2uz, 3uz})) ==
        concat2<size_t, std::plus<size_t>, Zero>(std::views::transform(
            std::array{1uz, 2uz, 3uz},
            PowerBy<size_t, std::plus<size_t>, Zero>{3uz})) &&
    PowerBy<Str, Cat, Empty>{
        2uz}(concat2<Str, Cat, Empty>(std::array{"ab"s, "c"s})) == "abcabc"s &&
    concat2<Str, Cat, Empty>(std::views::transform(
        std::array{"ab"s, "c"s}, PowerBy<Str, Cat, Empty>{2uz})) == "ababcc"s);

// The free theorem as a function in plain C++: its signature is its
// hypothesis.  It can be called where h is known to be a homomorphism of the
// monoid, and there it returns true.  Where that is not known it cannot be
// called at all.
template <typename H, typename T, typename Op, typename E>
concept IsEndomorphism = IsMonoid<T, Op, E> && endomorphism<H, T, Op, E> &&
                         requires(const H h, T x) {
                           { h(x) } -> std::same_as<T>;
                         };
template <typename T, typename Op, typename E, typename H,
          std::ranges::sized_range R>
  requires IsEndomorphism<H, T, Op, E> &&
           std::same_as<std::ranges::range_value_t<R>, T>
constexpr bool fold_commutes(H h, R r) {
  return h(concat2<T, Op, E>(r)) ==
         concat2<T, Op, E>(std::views::transform(r, h));
}
template <typename T, typename Op, typename E, typename H, typename R>
concept CanAsk = requires(H h, R r) { fold_commutes<T, Op, E>(h, r); };
static_assert(
    fold_commutes<size_t, std::plus<size_t>, Zero>(
        PowerBy<size_t, std::plus<size_t>, Zero>{3uz},
        std::array{1uz, 2uz, 3uz}) &&
    fold_commutes<size_t, std::multiplies<size_t>, One>(
        PowerBy<size_t, std::multiplies<size_t>, One>{2uz},
        std::array{1uz, 2uz, 3uz}) &&
    fold_commutes<bool, And, True>(PowerBy<bool, And, True>{4uz},
                                   std::array{true, false}) &&
    CanAsk<size_t, std::plus<size_t>, Zero,
           PowerBy<size_t, std::plus<size_t>, Zero>, std::array<size_t, 3>> &&
    !CanAsk<Str, Cat, Empty, PowerBy<Str, Cat, Empty>, std::array<Str, 2>>);

// A generic caller that declares a monoid and calls power.  Which case of
// power it gets is decided where it is used, by laws its signature never
// named.  With a false axiom in sight the value changes: Plus2 adds, someone
// declared it idempotent, and ten twos come out as two.
struct Plus2 {
  constexpr size_t operator()(size_t a, size_t b) const { return a + b; }
};
template <>
constexpr bool associative<size_t, Plus2> = true;
template <>
constexpr bool unital<size_t, Plus2, Zero> = true;
template <>
constexpr bool idempotent<size_t, Plus2> = true;  // false
template <typename T, typename Op, typename E>
  requires IsMonoid<T, Op, E>
constexpr T guessed_power(T x, size_t n) {
  return power<T, Op, E>(x, n);
}
static_assert(guessed_power<size_t, std::plus<size_t>, Zero>(2uz, 10uz) == 20 &&
              guessed_power<size_t, Plus2, Zero>(2uz, 10uz) == 2);
}  // namespace triple_probe

TEST_CASE("Spelling probe: a law as a function and as a table",
          "[category][jlt][spelling][probe]") {
  using namespace spelling_probe;
  const auto row = [](const char* what, int f, int t) {
    std::printf("SPELLING | function %2d | table %2d | %s\n", f, t, what);
  };
  using SP = Then<Succ, Pred>;
  using SPS = Then<SP, Succ>;
  using NN = Then<Neg, Neg>;
  using SH = Then<Succ, Half>;
  using SHP = Then<SH, Pred>;
  using ST = Then<Succ, Twice>;
  row("Succ", by_function::ask<Succ>(), by_table::ask<Succ>());
  row("Neg", by_function::ask<Neg>(), by_table::ask<Neg>());
  row("Half", by_function::ask<Half>(), by_table::ask<Half>());
  row("Twice", by_function::ask<Twice>(), by_table::ask<Twice>());
  row("Then<Succ, Pred>", by_function::ask<SP>(), by_table::ask<SP>());
  row("Then<Then<Succ, Pred>, Succ>", by_function::ask<SPS>(),
      by_table::ask<SPS>());
  row("Then<Neg, Neg>", by_function::ask<NN>(), by_table::ask<NN>());
  row("Then<Succ, Half>", by_function::ask<SH>(), by_table::ask<SH>());
  row("Then<Then<Succ, Half>, Pred>", by_function::ask<SHP>(),
      by_table::ask<SHP>());
  row("Then<Succ, Twice>", by_function::ask<ST>(), by_table::ask<ST>());
  CHECK(Then<Succ, Pred>{}(41uz) == 41);
  const auto fact = [](const char* what, bool holds) {
    std::printf("EQUALITY | %s | %s\n", holds ? "yes" : "no ", what);
  };
  fact("(Succ >> Neg) >> Pred and Succ >> (Neg >> Pred) have one type",
       equality_probe::rebrackets);
  fact("the same at depth five", equality_probe::rebrackets_deep);
  fact("Succ >> Neg and Neg >> Succ have different types",
       equality_probe::tells_words_apart);
  fact("the two bracketings compute the same value at 41",
       equality_probe::computes);
}

TEST_CASE("Lookup probe: which declarations of a trait a shape finds",
          "[category][jlt][lookup][probe]") {
  using namespace lookup_probe;
  const auto row = [](const char* what, bool found) {
    std::printf("LOOKUP | %s | %s\n", found ? "found    " : "not found", what);
  };
  row("1 generic function before the shape", generic_before);
  row("2 generic function after the shape", generic_after);
  row("3 explicit specialisation after the shape", specialisation_after);
  row("4 generic before the shape, body calls the trait at the parts",
      body_recursion_before);
  row("5 instance requiring its own shape at the parts (function)",
      own_shape_function);
  row("6 instance requiring its own shape at the parts (class template)",
      own_shape_class);
  CHECK(generic_before);
}

TEST_CASE("Jlt paper listings: Appendix A's small program, read at runtime",
          "[category][jlt][listings]") {
  CHECK(jlt_listing::reading::closed_term());
}
