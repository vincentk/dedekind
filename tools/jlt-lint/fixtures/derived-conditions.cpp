// The conditions of Table 7's rows, tested: beside each definition that
// breaks a row's condition, a twin that keeps it.  The levels the linter
// gives are pinned definition by definition in derived-conditions.levels
// (levels.py --expect), so that a condition once checked stays checked.
// The audit of 2026-11-25 found each of the broken ones classed Jlt or FJlt.
// if constexpr is undecided wherever it stands: its two rows have no check
// yet, so neither its sound uses nor these two are classed.
// Standalone, no imports, so it is checked without the build.
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

// A sum as the standard's variant: undecided until its row exists.  Beside it
// the record it is to be read as, a tag and a field for each alternative.
struct Area {
  constexpr std::size_t operator()(std::size_t side) const { return side * side; }
  constexpr std::size_t operator()(std::pair<std::size_t, std::size_t> sides) const {
    return sides.first * sides.second;
  }
};
constexpr std::size_t area(std::variant<std::size_t, std::pair<std::size_t, std::size_t>> shape) {
  return std::visit(Area{}, shape);
}
struct Tagged {
  bool second;
  std::size_t side;
  std::pair<std::size_t, std::size_t> sides;
};
constexpr std::size_t area_as_read(Tagged shape) {  // sound
  return shape.second ? Area{}(shape.sides) : Area{}(shape.side);
}
static_assert(area(3uz) == area_as_read({false, 3, {}}) &&
              area(std::pair{2uz, 5uz}) == area_as_read({true, 0, {2, 5}}));

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

}  // namespace jlt_listing
