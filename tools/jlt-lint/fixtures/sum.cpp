// The binary sum of the kernel (#1222), std::variant<A, B> of two types, read
// as Pierce's coproduct, Haskell's Either a b: built by the named injections
// inl and inr and read by the copairing [f, g], as a pair is built by
// pair{a, b} and read by .first and .second.  The copairing is typed by dom
// and cod, an arrow's two objects (Pierce, Definition 1.1.1), read off its
// call operator.  dom and cod are given to the kernel, as cod is in the
// listings, and declared in jlt_given, outside the namespace the linter
// judges; the injections and the copairing are judged as any definition.  The positional spellings, the conditional on index() and
// std::expected are sugar, each beside the kernel program it is read
// as; the other uses of a variant are refused or undecided.  The levels are
// pinned definition by definition in sum.levels (levels.py --expect).
#include <cstddef>
#include <expected>
#include <limits>
#include <type_traits>
#include <utility>
#include <variant>

namespace jlt_given {
// An arrow's domain and codomain, read off its one call operator.
template <typename F>
struct call_of {};
template <typename R, typename F, typename A>
struct call_of<R (F::*)(A) const> {
  using dom = std::remove_cvref_t<A>;
  using cod = R;
};
template <typename R, typename F, typename A>
struct call_of<R (F::*)(A) const noexcept> : call_of<R (F::*)(A) const> {};
template <typename R, typename A>
struct call_of<R (*)(A)> {  // a static call operator
  using dom = std::remove_cvref_t<A>;
  using cod = R;
};
// Empty where F has no one call operator of these forms, so that a
// constraint naming dom<F>::type fails as a constraint, not as an error.
template <typename F>
struct dom {};
template <typename F>
  requires requires { typename call_of<decltype(&F::operator())>::dom; }
struct dom<F> {
  using type = typename call_of<decltype(&F::operator())>::dom;
};
template <typename F>
struct cod {};
template <typename F>
  requires requires { typename call_of<decltype(&F::operator())>::cod; }
struct cod<F> {
  using type = typename call_of<decltype(&F::operator())>::cod;
};
}  // namespace jlt_given

namespace jlt_listing {
// The injections, named as a pair's projections are.
template <typename A, typename B>
struct Inl {
  constexpr std::variant<A, B> operator()(A a) const { return std::variant<A, B>(std::in_place_index<0>, a); }
};
template <typename A, typename B>
struct Inr {
  constexpr std::variant<A, B> operator()(B b) const { return std::variant<A, B>(std::in_place_index<1>, b); }
};
// The copairing [f, g] : dom f + dom g -> cod f, where cod f = cod g.
template <typename F, typename G>
  requires std::same_as<typename jlt_given::cod<F>::type, typename jlt_given::cod<G>::type>
struct Copairing {
  F f;
  G g;
  constexpr typename jlt_given::cod<F>::type operator()(
      const std::variant<typename jlt_given::dom<F>::type, typename jlt_given::dom<G>::type>& x) const {
    return x.index() == 0 ? f(std::get<0>(x)) : g(std::get<1>(x));
  }
};

// ---- Subtraction on the naturals: the difference, or how far short. ----
// size_t's subtraction wraps and is defined everywhere; the sum gives the
// naturals' own, which has no value where m exceeds n.
struct Short {
  std::size_t by;
};
constexpr std::variant<std::size_t, Short> minus(std::size_t n, std::size_t m) {
  return m <= n ? Inl<std::size_t, Short>{}(n - m) : Inr<std::size_t, Short>{}(Short{m - n});
}
// Read by a copairing: the difference, or zero (truncated subtraction).
struct Same {
  constexpr std::size_t operator()(std::size_t n) const { return n; }
};
struct Zero {
  constexpr std::size_t operator()(Short) const { return 0; }
};
constexpr std::size_t monus(std::size_t n, std::size_t m) { return Copairing<Same, Zero>{Same{}, Zero{}}(minus(n, m)); }
static_assert(monus(7, 2) == 5 && monus(2, 7) == 0);

// A shape, a square by its side or a rectangle by its sides, and its area by
// a copairing: the twin of sugar-conditions.cpp's area by std::visit.
struct Square {
  constexpr std::size_t operator()(std::size_t s) const { return s * s; }
};
struct Rectangle {
  constexpr std::size_t operator()(std::pair<std::size_t, std::size_t> s) const { return s.first * s.second; }
};
constexpr std::size_t area(std::variant<std::size_t, std::pair<std::size_t, std::size_t>> shape) {
  return Copairing<Square, Rectangle>{Square{}, Rectangle{}}(shape);
}
static_assert(area(Inl<std::size_t, std::pair<std::size_t, std::size_t>>{}(3)) == 9);

// ---- Sugar: the injections and the copairing spelled otherwise. ----
using Difference = std::variant<std::size_t, Short>;
// The standard's positional spelling of inl and inr.
constexpr Difference minus_by_index(std::size_t n, std::size_t m) {
  return m <= n ? Difference(std::in_place_index<0>, n - m) : Difference(std::in_place_index<1>, Short{m - n});
}
// The copairing as a choice on the index, on a const variable: get<0> only
// where alternative 0 is known, get<1> only where alternative 1 is.
constexpr std::size_t monus_by_index(std::size_t n, std::size_t m) {
  const Difference d = minus(n, m);
  return d.index() == 0 ? std::get<0>(d) : 0uz;
}
// The same choice by if, negated, as an early exit, and joined by &&.
constexpr std::size_t short_by(const Difference d) {
  if (d.index() == 1) {
    return std::get<1>(d).by;
  } else {
    return 0uz;
  }
}
constexpr std::size_t or_zero_negated(const Difference d) { return d.index() != 0 ? 0uz : std::get<0>(d); }
constexpr std::size_t or_zero_guarded(const Difference d) {
  if (d.index() == 1) return 0uz;
  return std::get<0>(d);
}
constexpr bool more_than(const Difference d, std::size_t k) { return d.index() == 0 && std::get<0>(d) > k; }
// A disjunction negated: where !(d.index() == 0 || p) holds, each operand
// fails, so alternative 1 is known.
constexpr std::size_t negated_disjunction(const Difference d, bool p) {
  return !(d.index() == 0 || p) ? std::get<1>(d).by : 0uz;
}
static_assert(monus_by_index(7, 2) == monus(7, 2) && monus_by_index(2, 7) == monus(2, 7));
static_assert(Copairing<Same, Zero>{Same{}, Zero{}}(minus_by_index(5, 1)) == monus(5, 1) &&
              Copairing<Same, Zero>{Same{}, Zero{}}(minus_by_index(1, 5)) == monus(1, 5));
static_assert(short_by(minus(1, 5)) == 4 && or_zero_negated(minus(5, 1)) == 4 && or_zero_guarded(minus(1, 5)) == 0);
static_assert(more_than(minus(9, 1), 3) && !more_than(minus(1, 9), 3));

// std::expected<A, B>, the other spelling of the binary sum, read as
// variant<A, B>: its tagged constructions are inl and inr, and
// x.has_value() ? ... *x ... : ... x.error() ... is the copairing.
using Checked = std::expected<std::size_t, Short>;
constexpr Checked minus_checked(std::size_t n, std::size_t m) {
  return m <= n ? Checked{std::in_place, n - m} : Checked{std::unexpect, Short{m - n}};
}
constexpr std::size_t monus_checked(std::size_t n, std::size_t m) {
  const Checked c = minus_checked(n, m);
  return c.has_value() ? *c : 0uz;
}
constexpr std::size_t short_checked(const Checked c) { return c.has_value() ? 0uz : c.error().by; }
static_assert(monus_checked(7, 2) == monus(7, 2) && monus_checked(2, 7) == monus(2, 7) &&
              short_checked(minus_checked(1, 5)) == 4);

// The test in other spellings: an integer constant, a conjunction in the
// condition, and an expected as a truth value.  Jlt.
constexpr std::size_t by_constant(const Difference d) { return d.index() == std::size_t{0} ? std::get<0>(d) : 0uz; }
constexpr std::size_t by_conjunction(const Difference d, bool p) { return p && d.index() == 0 ? std::get<0>(d) : 0uz; }
constexpr std::size_t checked_as_bool(const Checked c) { return c ? *c : 0uz; }
static_assert(by_constant(minus(5, 1)) == 4 && by_conjunction(minus(5, 1), true) == 4 && checked_as_bool(minus_checked(5, 1)) == 4);

// ---- Refused: each may read an alternative that is not there, or convert. ----
constexpr std::size_t read_by_get(const Difference d) { return std::get<0>(d); }
constexpr std::size_t read_in_wrong_branch(const Difference d) { return d.index() == 0 ? 0uz : std::get<0>(d); }
// By type, which cannot tell the alternatives of variant<A, A> apart.
constexpr std::size_t read_by_type(const Difference d) { return d.index() == 0 ? std::get<std::size_t>(d) : 0uz; }
constexpr std::size_t read_other(const Difference p, const Difference q) { return p.index() == 0 ? std::get<0>(q) : 0uz; }
// Branches of different types, 0 an int converted.
constexpr std::size_t read_converted(const Difference d) { return d.index() == 0 ? std::get<0>(d) : 0; }
// A tested variable that can be written: a write could stand between the
// test and the reading.
constexpr std::size_t read_mutable(Difference d) { return d.index() == 0 ? std::get<0>(d) : 0uz; }
// A payload not of its alternative's type, converted inside the standard's
// constructor: narrowing, or not.
constexpr Difference negative_payload() { return Difference(std::in_place_index<0>, -1); }
constexpr Checked negative_checked() { return Checked{std::in_place, -1}; }
constexpr Difference widened_payload(unsigned k) { return Difference(std::in_place_index<0>, k); }
// expected: the value with no test, value(), the other alternative where
// the value is known.
constexpr std::size_t checked_untested(const Checked c) { return *c; }
constexpr std::size_t checked_by_value(const Checked c) { return c.value(); }
constexpr std::size_t checked_wrong_branch(const Checked c) { return c.has_value() ? c.error().by : 0uz; }

// The test on a reference to const, the reading in another statement: the
// object may be written through another path in between, and get throw.
constexpr std::size_t read_through_alias(const Difference& d) {
  if (d.index() == 1) return 0uz;
  return std::get<0>(d);
}
// emplace, which may leave a variant without a value where a throw
// interrupts it.
constexpr std::size_t emplaced(std::size_t n) {
  Difference d = minus(n, 1);
  d.emplace<0>(n);
  return n;
}

// ---- Undecided: total, sugar not yet written. ----
struct Either {
  constexpr std::size_t operator()(std::size_t n) const { return n; }
  constexpr std::size_t operator()(Short) const { return 0; }
};
constexpr std::size_t read_by_visit(const Difference d) { return std::visit(Either{}, d); }
constexpr bool short_of(const Difference d) { return std::holds_alternative<Short>(d); }
// A variant of three types, read as binary ones nested.
constexpr std::size_t three(const std::variant<std::size_t, bool, Short> v) { return v.index() == 0 ? std::get<0>(v) : 0uz; }
constexpr std::size_t checked_or(const Checked c) { return c.value_or(0uz); }
constexpr bool ordered(const std::variant<std::size_t, bool> a, const std::variant<std::size_t, bool> b) { return a < b; }
constexpr Checked checked_untagged(std::size_t n) { return n; }
// A payload whose type only an instance tells: lift(-1) with T = int is
// SIZE_MAX, the constructor converting.  The same in an expected.
template <typename T>
constexpr Difference lift(T t) {
  return Difference(std::in_place_index<0>, t);
}
template <typename T>
constexpr Checked lift_checked(T t) {
  return Checked{std::in_place, t};
}
static_assert(std::get<0>(lift(-1)) == std::numeric_limits<std::size_t>::max() && *lift_checked(2) == 2);
// std::expected<void, B>: its reading, variant<void, B>, is ill-formed.
using Done = std::expected<void, Short>;
constexpr std::size_t short_or_done(const Done c) { return c.has_value() ? 0uz : c.error().by; }
static_assert(short_or_done(Done{std::unexpect, Short{3}}) == 3);
// A switch on the index: total, not yet read as sugar; its get is open, and
// the definition is refused only for the returns from inside the switch
// (Stmt:EarlyReturn).  get_if is open too; held is refused for the address
// and the null pointer, not for the sum.
constexpr std::size_t by_switch(const Difference d) {
  switch (d.index()) {
    case 0:
      return std::get<0>(d);
    default:
      return 0uz;
  }
}
constexpr bool held(const Difference d) { return std::get_if<0>(&d) != nullptr; }
constexpr bool valueless(const Difference d) { return d.valueless_by_exception(); }
static_assert(by_switch(minus(5, 1)) == 4 && held(minus(5, 1)) && !valueless(minus(5, 1)));
static_assert(read_by_visit(minus(5, 1)) == 4 && short_of(minus(1, 5)));

// ---- NaN as a sum (#1215). ----
// A double is a number or not, read by a copairing.  On the numbers equality
// is reflexive again; their arithmetic is still neither closed nor
// associative, so no law follows.
struct NaN {};
struct Number {
  double v;
};
constexpr std::variant<Number, NaN> classify(double x) {
  return x != x ? Inr<Number, NaN>{}(NaN{}) : Inl<Number, NaN>{}(Number{x});
}
struct Known {
  constexpr bool operator()(Number) const { return true; }
};
struct Unknown {
  constexpr bool operator()(NaN) const { return false; }
};
constexpr bool is_number(std::variant<Number, NaN> f) { return Copairing<Known, Unknown>{Known{}, Unknown{}}(f); }
static_assert(!is_number(classify(std::numeric_limits<double>::quiet_NaN())) && is_number(classify(1.5)));
// Clang's constant evaluation does not produce a NaN: infinity minus
// infinity is not a constant there, so no sum built at compile time holds one
// by arithmetic.  The standard leaves this to the implementation.
template <double A, double B>
concept ConstantSum = requires { typename std::integral_constant<double, A + B>; };
static_assert(ConstantSum<1.0, 2.0>);
static_assert(!ConstantSum<std::numeric_limits<double>::infinity(), -std::numeric_limits<double>::infinity()>);

// ---- Copilot on #1226: what only an instance tells. ----
// A choice whose branches differ in type as written, in a template: at
// T = size_t the 0 converts, which the choice's row does not read.
template <std::integral T>
constexpr T first_or_zero(const std::variant<T, bool> d) {  // undecided
  return d.index() == 0 ? std::get<0>(d) : 0;
}
template <std::integral T>
constexpr T first_or_same(const std::variant<T, bool> d, T z) {  // sound: T for T
  return d.index() == 0 ? std::get<0>(d) : z;
}
static_assert(first_or_zero<std::size_t>(std::variant<std::size_t, bool>(std::in_place_index<0>, 3uz)) == 3 &&
              first_or_same<std::size_t>(std::variant<std::size_t, bool>(std::in_place_index<1>, true), 4uz) == 4);
// Two arguments written, a pack among them: as many alternatives as the pack
// has, and more than two at Box<int, char>.
template <typename... Ts>
struct Box {  // undecided
  std::variant<Ts..., bool> value;
};

}  // namespace jlt_listing
