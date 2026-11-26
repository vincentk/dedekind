// The binary sum of the kernel (#1222), std::variant<A, B>, read as Pierce's
// coproduct, Haskell's Either a b: by its two injections and its copairing
// only.  They are the library's Injection (ι) and Copairing
// (category/cartesian.cppm), copied here with the same shape so that the
// fixture stands alone: arrows with a Domain, a Codomain and a const call
// operator.  Their bodies are the one place a sum is built or taken apart by
// hand.  Every other use of a variant is a derived form not yet written
// (open) or refused, each beside a twin through the arrows.  The levels are
// pinned definition by definition in sum.levels (levels.py --expect).
#include <cstddef>
#include <expected>
#include <limits>
#include <type_traits>
#include <utility>
#include <variant>

namespace jlt_listing {

// ---- The structure arrows, as in the library. ----
template <std::size_t K, typename A, typename B>
  requires(K == 1 || K == 2)
struct Injection {
  using Domain = std::conditional_t<K == 1, A, B>;
  using Codomain = std::variant<A, B>;
  constexpr Codomain operator()(const Domain& x) const { return Codomain(std::in_place_index<K - 1>, x); }
};
template <std::size_t K, typename A, typename B>
using ι = Injection<K, A, B>;

template <typename F, typename G>
  requires std::same_as<typename F::Codomain, typename G::Codomain>
struct Copairing {
  using Domain = std::variant<typename F::Domain, typename G::Domain>;
  using Codomain = typename F::Codomain;
  F f;
  G g;
  constexpr Codomain operator()(const Domain& x) const {
    return x.index() == 0 ? f(std::get<0>(x)) : g(std::get<1>(x));
  }
};

// ---- Subtraction on the naturals: the difference, or how far short. ----
// size_t subtraction wraps, and is defined everywhere; the sum gives the
// naturals' own subtraction, which has no value where m exceeds n.
struct Short {
  std::size_t by;
};
using Difference = std::variant<std::size_t, Short>;
constexpr Difference minus(std::size_t n, std::size_t m) {
  return m <= n ? ι<1, std::size_t, Short>{}(n - m) : ι<2, std::size_t, Short>{}(Short{m - n});
}
// Read by a copairing: the difference, or zero (truncated subtraction).
struct Same {
  using Domain = std::size_t;
  using Codomain = std::size_t;
  constexpr std::size_t operator()(std::size_t n) const { return n; }
};
struct Nothing {
  using Domain = Short;
  using Codomain = std::size_t;
  constexpr std::size_t operator()(Short) const { return 0; }
};
constexpr std::size_t monus(std::size_t n, std::size_t m) {
  return Copairing<Same, Nothing>{Same{}, Nothing{}}(minus(n, m));
}
static_assert(monus(7, 2) == 5 && monus(2, 7) == 0);

// A shape, a square by its side or a rectangle by its sides, and its area by
// a copairing: the twin of derived-conditions.cpp's area by std::visit.
using Sides = std::pair<std::size_t, std::size_t>;
struct Square {
  using Domain = std::size_t;
  using Codomain = std::size_t;
  constexpr std::size_t operator()(std::size_t s) const { return s * s; }
};
struct Rectangle {
  using Domain = Sides;
  using Codomain = std::size_t;
  constexpr std::size_t operator()(Sides s) const { return s.first * s.second; }
};
constexpr std::size_t area(std::variant<std::size_t, Sides> shape) {
  return Copairing<Square, Rectangle>{Square{}, Rectangle{}}(shape);
}
static_assert(area(ι<1, std::size_t, Sides>{}(3)) == 9 && area(ι<2, std::size_t, Sides>{}(Sides{2, 5})) == 10);

// ---- The copairing written as a choice on the index: sugar. ----
// x.index() == 0 ? e1 : e2, with std::get<0>(x) read only where side 0 is
// known and std::get<1>(x) only where side 1 is: the copairing of two
// function objects whose bodies are the branches.  Jlt, each beside the
// copairing it is read as.
constexpr std::size_t monus_by_index(std::size_t n, std::size_t m) {
  const Difference d = minus(n, m);
  return d.index() == 0 ? std::get<0>(d) : 0uz;
}
constexpr std::size_t area_by_index(std::variant<std::size_t, Sides> shape) {
  return shape.index() == 0 ? Square{}(std::get<0>(shape)) : Rectangle{}(std::get<1>(shape));
}
// The same choice by if, negated, as a guard, and joined by &&.
constexpr std::size_t short_by(Difference d) {
  if (d.index() == 1) {
    return std::get<1>(d).by;
  } else {
    return 0uz;
  }
}
constexpr std::size_t or_zero_negated(Difference d) { return d.index() != 0 ? 0uz : std::get<0>(d); }
constexpr std::size_t or_zero_guarded(Difference d) {
  if (d.index() == 1) return 0uz;
  return std::get<0>(d);
}
constexpr bool more_than(Difference d, std::size_t k) { return d.index() == 0 && std::get<0>(d) > k; }
static_assert(monus_by_index(7, 2) == monus(7, 2) && monus_by_index(2, 7) == monus(2, 7));
static_assert(area_by_index(ι<2, std::size_t, Sides>{}(Sides{2, 5})) == area(ι<2, std::size_t, Sides>{}(Sides{2, 5})));
static_assert(short_by(minus(1, 5)) == 4 && or_zero_negated(minus(5, 1)) == 4 && or_zero_guarded(minus(1, 5)) == 0);
static_assert(more_than(minus(9, 1), 3) && !more_than(minus(1, 9), 3));

// ---- Other uses of a variant. ----
// Refused, each may fail or changes the sum: a side read by get with no test,
constexpr std::size_t read_by_get(Difference d) { return std::get<0>(d); }
// or in the branch that knows the other side,
constexpr std::size_t read_in_wrong_branch(Difference d) { return d.index() == 0 ? 0uz : std::get<0>(d); }
// or by type, which cannot tell the sides of variant<A, A> apart,
constexpr std::size_t read_by_type(Difference d) { return d.index() == 0 ? std::get<std::size_t>(d) : 0uz; }
// or after a test of another sum;
constexpr std::size_t read_other(Difference p, Difference q) { return p.index() == 0 ? std::get<0>(q) : 0uz; }
// a choice whose branches differ in type, 0 an int converted;
constexpr std::size_t read_converted(Difference d) { return d.index() == 0 ? std::get<0>(d) : 0; }
// a sum changed in place.
constexpr std::size_t changed(Difference d) {
  d.emplace<0>(1uz);
  return monus(1, 0);
}
// Open, each total, a derived form not yet written: the tagged
// construction, which is an injection;
constexpr Difference built_by_tag(std::size_t n) { return Difference(std::in_place_index<0>, n); }
// visit on two distinct alternatives, which is a copairing;
struct Either {
  constexpr std::size_t operator()(std::size_t n) const { return n; }
  constexpr std::size_t operator()(Short) const { return 0; }
};
constexpr std::size_t read_by_visit(Difference d) { return std::visit(Either{}, d); }
// holds_alternative, a copairing into the truth values;
constexpr bool short_of(Difference d) { return std::holds_alternative<Short>(d); }
// a variant of three types, read as nested binary ones;
constexpr std::size_t three(std::variant<std::size_t, bool, Short> v) { return v.index(); }
static_assert(read_by_visit(minus(5, 1)) == 4 && short_of(minus(1, 5)));

// ---- std::expected<A, B>: the other spelling of a binary sum. ----
// Read as variant<A, B>: its tagged constructions are the injections, and
// x.has_value() ? ... *x ... : ... x.error() ... the copairing.  Jlt.
using Checked = std::expected<std::size_t, Short>;
constexpr Checked minus_checked(std::size_t n, std::size_t m) {
  return m <= n ? Checked{std::in_place, n - m} : Checked{std::unexpect, Short{m - n}};
}
constexpr std::size_t monus_checked(std::size_t n, std::size_t m) {
  const Checked c = minus_checked(n, m);
  return c.has_value() ? *c : 0uz;
}
constexpr std::size_t short_checked(Checked c) { return c.has_value() ? 0uz : c.error().by; }
static_assert(monus_checked(7, 2) == monus(7, 2) && monus_checked(2, 7) == monus(2, 7) &&
              short_checked(minus_checked(1, 5)) == 4);
// Refused: the value read with no test, value(), which throws on the other
// side, and the other side read where the value is known.
constexpr std::size_t checked_untested(Checked c) { return *c; }
constexpr std::size_t checked_by_value(Checked c) { return c.value(); }
constexpr std::size_t checked_wrong_branch(Checked c) { return c.has_value() ? c.error().by : 0uz; }
// Open: the rest of its interface, a derived form not yet written.
constexpr std::size_t checked_or(Checked c) { return c.value_or(0uz); }
constexpr Checked checked_untagged(std::size_t n) { return n; }

// ---- NaN as a sum (#1215). ----
// A double is a number or it is not, and the kernel reads it by a copairing.
// On the side of numbers equality is reflexive again; the arithmetic is
// still neither closed nor associative, so no law follows.
struct NaN {};
struct Number {
  double v;
};
using Float = std::variant<Number, NaN>;
constexpr Float classify(double x) {
  return x != x ? ι<2, Number, NaN>{}(NaN{}) : ι<1, Number, NaN>{}(Number{x});
}
struct Known {
  using Domain = Number;
  using Codomain = bool;
  constexpr bool operator()(Number) const { return true; }
};
struct Unknown {
  using Domain = NaN;
  using Codomain = bool;
  constexpr bool operator()(NaN) const { return false; }
};
constexpr bool is_number(Float f) { return Copairing<Known, Unknown>{Known{}, Unknown{}}(f); }
static_assert(!is_number(classify(std::numeric_limits<double>::quiet_NaN())) && is_number(classify(1.5)));
// Clang's constant evaluation does not produce a NaN: infinity minus
// infinity is not a constant there, so no sum built at compile time holds one
// by arithmetic.  The standard leaves this to the implementation.
template <double A, double B>
concept ConstantSum = requires { typename std::integral_constant<double, A + B>; };
static_assert(ConstantSum<1.0, 2.0>);
static_assert(!ConstantSum<std::numeric_limits<double>::infinity(), -std::numeric_limits<double>::infinity()>);

}  // namespace jlt_listing
