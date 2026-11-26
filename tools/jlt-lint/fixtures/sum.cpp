// The binary sum of the kernel, std::expected<A, B> (#1222): its two
// introductions, which name the side, and its one elimination, the case
// x.has_value() ? ... *x ... : ... x.error() ... on a variable x.  Beside each
// use the kernel does not read, a twin it does.  The levels the linter gives
// are pinned definition by definition in sum.levels (levels.py --expect).
// Standalone, no imports, so it is checked without the build.
#include <concepts>
#include <cstddef>
#include <expected>
#include <limits>
#include <type_traits>
#include <utility>

namespace jlt_listing {

// Division on the naturals, total: the quotient, or the reason there is none.
// The division stands where a test has shown the divisor not zero.
struct DivisionByZero {};
using Quotient = std::expected<std::size_t, DivisionByZero>;

constexpr Quotient divide(std::size_t n, std::size_t d) {
  return d == 0 ? Quotient{std::unexpect, DivisionByZero{}} : Quotient{std::in_place, n / d};
}
// The same after a guard, an early exit.
constexpr Quotient divide_guarded(std::size_t n, std::size_t d) {
  if (d == 0) return Quotient{std::unexpect, DivisionByZero{}};
  return Quotient{std::in_place, n / d};
}
// The same with the introductions as sugar: a value, and std::unexpected,
// each converted to the sum.
constexpr Quotient divide_as_written(std::size_t n, std::size_t d) {
  if (d == 0) return std::unexpected(DivisionByZero{});
  return n / d;
}
// No test: division by a value stays open.
constexpr std::size_t divide_unguarded(std::size_t n, std::size_t d) { return n / d; }
// A test of another variable shows nothing about the divisor.
constexpr std::size_t divide_tested_wrongly(std::size_t n, std::size_t d) { return n == 0 ? 0uz : n / d; }

// The case: the one way out of the sum.
constexpr std::size_t or_zero(Quotient q) { return q.has_value() ? *q : 0uz; }
constexpr std::size_t or_zero_by_if(Quotient q) {
  if (q.has_value()) {
    return *q;
  } else {
    return 0uz;
  }
}
// Both payloads read, each in the branch that knows its side.
struct Reason {
  std::size_t code;
};
using Coded = std::expected<std::size_t, Reason>;
constexpr std::size_t value_or_code(Coded c) { return c.has_value() ? *c : c.error().code; }
// The case with a constant for the other side, written as the library's
// value_or: sugar.
constexpr std::size_t or_default(Quotient q) { return q.value_or(0uz); }

// Near misses, each refused.  The payload read with no test.
constexpr std::size_t read_without_test(Quotient q) { return *q; }
// value() throws on the other side.
constexpr std::size_t read_by_value(Quotient q) { return q.value(); }
// The payload read in the branch that knows the other side.
constexpr std::size_t read_in_wrong_branch(Quotient q) { return q.has_value() ? 0uz : *q; }
// The two branches of different types: 0 is an int, converted.
constexpr std::size_t read_converted(Quotient q) { return q.has_value() ? *q : 0; }
// The test spelled as a conversion to bool.
constexpr std::size_t read_by_bool(Quotient q) { return q ? *q : 0uz; }
// A sum constructed empty holds its first side's default value: sugar.
constexpr bool default_sum() {
  const Quotient q;
  return q.has_value() ? true : false;
}
// A test of one sum, the payload of another.
constexpr std::size_t read_other(Quotient p, Quotient q) { return p.has_value() ? *q : 0uz; }

// The practitioner's cases on #1226.  A conversion inside the standard's
// constructors narrows as one written out does: refused.
constexpr Quotient negative_payload() { return Quotient{std::in_place, -1}; }
constexpr Quotient negative_returned() { return -1; }
constexpr std::size_t negative_default(Quotient q) { return q.value_or(-1); }
// A sum of other types converted: refused.
using Real = std::expected<double, DivisionByZero>;
using Count = std::expected<std::size_t, DivisionByZero>;
constexpr Count from_real(Real r) { return r; }
// The divisor written after its test: the division is open again.
constexpr std::size_t divide_after_write(std::size_t n, std::size_t d) {
  if (d == 0) return 0;
  --d;
  return n / d;
}
// Signed division: a divisor not zero is not enough (INT_MIN / -1): open.
constexpr int divide_signed(int n, int d) { return d != 0 ? n / d : 0; }
// The test in other spellings, each the case's sugar: Jlt.
constexpr bool tested_alone(Quotient q) { return q.has_value(); }
constexpr std::size_t guarded_negated(Quotient q) {
  if (!q.has_value()) return 0uz;
  return *q;
}
constexpr std::size_t guarded_both(Coded c) {
  if (c.has_value()) return *c;
  return c.error().code;
}
constexpr bool tested_and(Quotient q) { return q.has_value() && *q > 3; }
// In a template: the case, and a member read with no test (refused).
template <typename T>
  requires std::copyable<T>
constexpr T or_given(std::expected<T, DivisionByZero> x, T d) {
  return x.has_value() ? *x : d;
}
template <typename T>
  requires std::copyable<T>
constexpr double member_untested(std::expected<T, DivisionByZero> x) {
  return x->v;
}

// A map inside the sum, total but not a way out: open.
struct Half {
  constexpr std::size_t operator()(std::size_t n) const { return n / 2; }
};
constexpr Quotient halved(const Quotient q) { return q.transform(Half{}); }

static_assert(or_zero(divide(7, 2)) == 3 && or_zero(divide(7, 0)) == 0);
static_assert(or_zero(divide_guarded(9, 3)) == 3 && or_zero_by_if(divide_as_written(9, 0)) == 0);
static_assert(value_or_code(Coded{std::unexpect, Reason{4}}) == 4 && or_default(divide(8, 4)) == 2);
static_assert(or_zero(halved(divide(8, 2))) == 2);
static_assert(default_sum() && guarded_negated(divide(9, 3)) == 3 && guarded_both(Coded{std::unexpect, Reason{5}}) == 5);
static_assert(!tested_alone(divide(1, 0)) && tested_and(divide(8, 2)) && or_given(divide(9, 0), 7uz) == 7);

// NaN as a sum (#1215): a double is a number or it is not, and an operation
// decides by cases before it operates.  On the side of numbers equality is
// reflexive again; the arithmetic is still neither closed nor associative.
struct NaN {};
struct Number {
  double v;
};
using Float = std::expected<Number, NaN>;
constexpr Float classify(double x) {
  return x != x ? Float{std::unexpect, NaN{}} : Float{std::in_place, Number{x}};
}
constexpr Float plus(Float a, Float b) {
  return a.has_value() ? (b.has_value() ? classify(a->v + b->v) : b) : a;
}
constexpr bool same(Number a, Number b) { return a.v == b.v; }
static_assert(!classify(std::numeric_limits<double>::quiet_NaN()).has_value());
static_assert(classify(1.5).has_value() && same(*plus(classify(1.0), classify(2.0)), Number{3.0}));
// Clang's constant evaluation does not produce a NaN: infinity minus
// infinity is not a constant there, so no sum built at compile time holds one
// by arithmetic.  The standard leaves this to the implementation.
template <double A, double B>
concept ConstantSum = requires { typename std::integral_constant<double, A + B>; };
static_assert(ConstantSum<1.0, 2.0>);
static_assert(!ConstantSum<std::numeric_limits<double>::infinity(), -std::numeric_limits<double>::infinity()>);

}  // namespace jlt_listing
