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

// if consteval: no tool lists the trust its row asks for.
constexpr std::size_t by_evaluation() {
  if consteval {
    return 1;
  } else {
    return 2;
  }
}

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

}  // namespace jlt_listing
