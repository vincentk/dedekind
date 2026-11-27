// The conditions of Table 7's rows, tested: beside each definition that
// breaks a row's condition, a twin that keeps it.  The levels the linter
// gives are pinned definition by definition in sugar-conditions.levels
// (levels.py --expect), so that a condition once checked stays checked.
// The audit of 2026-11-25 found each of the broken ones classed Jlt or FJlt.
// if constexpr is undecided wherever it stands: its two rows have no check
// yet, so neither its sound uses nor these two are classed.
// Standalone, no imports, so it is checked without the build.
#include <array>
#include <initializer_list>
#include <concepts>
#include <cstddef>
#include <cstdlib>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

namespace jlt_listing {

// An implicit conversion of an arithmetic value: it does not narrow.
constexpr bool narrowed_to_bool(std::size_t n) { return n; }
constexpr bool compared(std::size_t n) { return n != 0; }  // sound
constexpr std::size_t narrowed_to_char(std::size_t n) {
  const unsigned char c = n;
  return c;
}
constexpr int written_out(std::size_t i) { return int(i); }  // sound: the conversion is written
// The same conversion spelled static_cast: read as int(i), a derived form.
constexpr int written_static(std::size_t i) { return static_cast<int>(i); }
constexpr double widened_static(std::size_t i) { return static_cast<double>(i); }
// Not read as T(e): a cast to a reference, and one through a conversion
// operator.
constexpr int cast_to_reference(int x) { return static_cast<const int&>(x); }
struct Boxed {
  int v;
  constexpr operator int() const { return v; }
};
constexpr int cast_through_operator(Boxed b) { return static_cast<int>(b); }
constexpr unsigned char low_byte(std::size_t n) {  // refused: += converts back
  unsigned char c = 0;
  c += n;
  return c;
}
// The value stored back is computed in int: 255 + 1 is 256, which an unsigned
// char holds as 0, though the right side, 1, fits (Copilot on #1219).
constexpr unsigned char wrapped_byte() {  // refused: c += 1 converts int back
  unsigned char c = 255;
  c += 1;
  return c;
}
static_assert(wrapped_byte() == 0);
// value() of an optional throws where it is empty: open until the sum's
// eliminator (#1222), engaged or not.
constexpr std::size_t empty_value() {
  const std::optional<std::size_t> o;
  return o ? 1uz : 0uz;
}
constexpr std::size_t engaged_value(std::optional<std::size_t> o) { return o.value(); }
static_assert(empty_value() == 0 && engaged_value(3uz) == 3);
// A builtin that ends the program, or has no behaviour when reached: no value
// at any argument (Copilot on #1219).
constexpr std::size_t trapped(std::size_t n) {
  if (n == 0) __builtin_trap();
  return n;
}
constexpr std::size_t unreached(std::size_t n) {
  if (n == 0) __builtin_unreachable();
  return n;
}
constexpr std::size_t truncated(double x) { return x; }  // refused: undefined out of range
constexpr std::size_t widened(unsigned char c) { return c; }  // sound
constexpr std::size_t constant_held() { return 1; }           // sound: 1 is a size_t

// A return: at the end, or at the end of a branch the rest follows.
constexpr std::size_t guarded(bool c) {  // sound: c ? 7 : (the rest)
  std::size_t x = 0;
  if (c) return 7;
  x = 1;
  return x;
}
constexpr std::size_t guarded_in_else(bool c) {  // sound
  std::size_t x = 0;
  if (c) {
    x = 1;
  } else {
    return 9;
  }
  x = x + 1;
  return x;
}
constexpr std::size_t both_branches(bool c) {  // sound: c ? 1 : 2
  if (c) {
    return 1;
  } else {
    return 2;
  }
}
constexpr std::size_t sometimes_returns(bool a, bool b) {  // refused
  if (a) {
    if (b) return 1;
  }
  return 2;
}
static_assert(guarded(true) == 7 && guarded(false) == 1 && guarded_in_else(true) == 2 &&
              guarded_in_else(false) == 9 && sometimes_returns(true, false) == 2);

// A function is evaluable at compile time only if what it calls is: since
// C++23 the keyword alone does not say so.
constexpr std::size_t seeded() { return std::size_t(std::rand()); }  // refused
// A declaration with no body is not a definition, and is not counted.
constexpr std::size_t declared_elsewhere(std::size_t);
struct Forward;
// Division and a shift by a value are not defined at every argument.
constexpr std::size_t quotient(std::size_t a, std::size_t b) { return a / b; }   // undecided
constexpr std::size_t halved(std::size_t a) { return a / 2; }                    // sound
constexpr std::size_t shifted(std::size_t a, std::size_t i) { return a << i; }   // undecided
constexpr std::size_t doubled(std::size_t a) { return a << 1; }                  // sound
// A signed division or remainder by -1 overflows at the least value
// (Copilot on #1226): by a constant, but not one defined at every argument.
constexpr long negated_quotient(long a) { return a / -1; }   // undecided
constexpr long negated_remainder(long a) { return a % -1; }  // undecided
constexpr long negated_in_place(long a) {  // undecided
  long q = a;
  q /= -1;
  return q;
}
constexpr long remainder_in_place(long a) {  // undecided
  long r = a;
  r %= -1;
  return r;
}
constexpr long signed_halved(long a) { return a / 2; }       // sound
// A scoped enumeration is a record of one field of its underlying type
// (#1233): its enumerators, comparisons and conversions are sugar.
enum class Side : bool { Below, Above };                                  // sound
enum class Rel { Lt, Eq, Gt };                                            // sound
constexpr bool above(Side s) { return s == Side::Above; }                 // sound
constexpr bool before(Rel a, Rel b) { return a < b; }                     // sound
constexpr int code(Rel r) { return static_cast<int>(r); }                 // sound
constexpr Rel from_code(int n) { return static_cast<Rel>(n); }            // sound: every int is a Rel
static_assert(above(Side::Above) && before(Rel::Lt, Rel::Gt) && code(from_code(7)) == 7);
// An unscoped one has no fixed type: a conversion outside its enumerators'
// width is undefined, so it stays open.
enum Untyped { untyped_a, untyped_b };                                    // undecided
// Compared, it is promoted implicitly to int, which the narrowing check
// refuses: the implicit conversion a scoped one does not have.
constexpr bool is_a(Untyped p) { return p == untyped_a; }                 // refused
// A switch over a scoped enumeration has its own conditions, not yet a row.
constexpr int by_switch_on(Rel r) {                                       // undecided
  switch (r) {
    case Rel::Lt: return -1;
    case Rel::Eq: return 0;
    default: return 1;
  }
}
// A value among a template's parameters: as a number, or as a size, which
// asks for a type computed from a number (both open, counted apart).
template <std::size_t N>
constexpr std::size_t modulo(std::size_t x) {  // undecided (Value:number)
  return x % N;
}
template <std::size_t N>
constexpr std::size_t first_of(std::array<std::size_t, N> a) {  // undecided (Value:size)
  return a[0];
}
static_assert(modulo<3>(7) == 1 && first_of<2>({5, 6}) == 5);
// A structured binding has no row yet.
constexpr std::size_t bound(std::pair<std::size_t, std::size_t> p) {  // undecided
  auto [a, b] = p;
  return a + b;
}

// if consteval: no tool lists the trust its row asks for.
constexpr std::size_t by_evaluation() {
  if consteval {
    return 1;
  } else {
    return 2;
  }
}
// The same choice by the library's observer (Copilot on #1219).
constexpr std::size_t by_observer() { return std::is_constant_evaluated() ? 1uz : 2uz; }

// A lambda: not generic.
constexpr std::size_t generic_lambda() {
  auto id = [](auto x) { return x; };
  return id(1uz);
}
constexpr std::size_t plain_lambda() {  // sound
  auto id = [](std::size_t x) { return x; };
  return id(1uz);
}

// if constexpr with branches of two types, and on a shape the signature does
// not name: undecided, as every if constexpr is until its rows have checks.
template <typename T>
constexpr bool is_small = false;
template <typename T>
constexpr auto two_types(T x) {
  if constexpr (is_small<T>) {
    return true;
  } else {
    return x;
  }
}
template <typename T>
constexpr std::size_t unnamed_shape(T) {
  if constexpr (std::integral<T>) {
    return 1;
  } else {
    return 2;
  }
}

// A floating value converted to an integer, written out: undefined where the
// value does not fit (Copilot on #1219), so open as division by a value is.
constexpr std::size_t truncated_float(double x) { return std::size_t(x); }
constexpr int truncated_static(double x) { return static_cast<int>(x); }
static_assert(truncated_float(2.5) == 2 && truncated_static(2.5) == 2);

// A sum read through std::visit: sugar not yet written, so
// undecided.  Its twin, the copairing of the two arrows, is in sum.cpp.
struct Area {
  constexpr std::size_t operator()(std::size_t side) const { return side * side; }
  constexpr std::size_t operator()(std::pair<std::size_t, std::size_t> sides) const {
    return sides.first * sides.second;
  }
};
constexpr std::size_t area(std::variant<std::size_t, std::pair<std::size_t, std::size_t>> shape) {
  return std::visit(Area{}, shape);
}
static_assert(area(3uz) == 9 && area(std::pair{2uz, 5uz}) == 10);

// A destructor that does nothing is scaffolding; one that is virtual is not
// (and its class is refused for subtyping).
struct Plain {
  std::size_t v;
  constexpr ~Plain() = default;
};
struct Quiet {
  std::size_t v;
  constexpr ~Quiet() {}
};
constexpr std::size_t plain_value(Plain p) { return p.v; }
// sizeof of a type that is not dependent: a constant.
constexpr std::size_t width() { return sizeof(std::size_t) * 8; }
// T(x) to a class type with parentheses: the construction T{x}.
struct Pt {
  std::size_t x;
  std::size_t y;
};
constexpr Pt diagonal(std::size_t n) { return Pt(n, n); }
// The same construction narrowing an argument: refused.
constexpr Pt narrowed_point(int n) { return Pt(n, n); }
static_assert(plain_value(Plain{3}) == 3 && width() == 64 && diagonal(2).y == 2 && narrowed_point(1).x == 1);
// A class whose braces would choose another constructor (Copilot on #1229):
// Listed{n} is the list {n}, Listed(n) the count n, so no rewrite; open.
struct Listed {
  std::size_t n;
  constexpr explicit Listed(std::size_t count) : n(count) {}
  constexpr Listed(std::initializer_list<std::size_t> l) : n(l.size()) {}
};
constexpr std::size_t listed(std::size_t count) { return Listed(count).n; }
static_assert(listed(3) == 3 && Listed{3}.n == 1);
// A floating argument narrowed, or an integer made floating, as braces
// would refuse them: refused.  A constant within range is not narrowing.
struct Single {
  float x;
};
struct Double {
  double x;
};
constexpr Single to_single(double d) { return Single(d); }
constexpr Double to_double(std::size_t n) { return Double(n); }
constexpr Single one_single() { return Single(1.0); }
static_assert(to_single(0.5).x == 0.5f && to_double(2).x == 2.0 && one_single().x == 1.0f);
// An empty destructor whose member's destructor does something: its class's
// destruction is not nothing, so not scaffolding (Copilot on #1229).
struct Counted {
  std::size_t v;
  constexpr ~Counted() { v = 0; }
};
struct Owner {
  Counted c;
  constexpr ~Owner() = default;
};
constexpr std::size_t owned(std::size_t n) { return Owner{Counted{n}}.c.v; }
static_assert(owned(4) == 4);


// A const accessor (#1223): a const member function returning const T& to a
// field of its own object, or an element of one, is read as returning it by
// value; any other reference returned stays refused.
struct Held {  // sound
  std::size_t v;
  constexpr const std::size_t& get() const { return v; }
};
struct Indexed {  // sound: an element of a std::array field
  std::array<std::size_t, 2> a;
  constexpr const std::size_t& at(std::size_t i) const { return a[i]; }
};
struct Nested {  // sound: a field of a field
  Held h;
  constexpr const std::size_t& inner() const { return h.v; }
};
struct Passing {  // refused: a reference to a parameter
  constexpr const std::size_t& pick(const std::size_t& x) const { return x; }
};
struct Shared {  // refused: a reference to a static
  static constexpr std::size_t s = 3;
  constexpr const std::size_t& get() const { return s; }
};
struct Writable {  // refused: a non-const member function, a mutable reference
  std::size_t v;
  constexpr std::size_t& get() { return v; }
};
static_assert(Held{2}.get() == 2 && Indexed{{4, 5}}.at(1) == 5 && Nested{Held{6}}.inner() == 6 &&
              Shared{}.get() == 3);

}  // namespace jlt_listing
