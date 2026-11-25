// The conditions of Table 7's rows, tested: beside each definition that
// breaks a row's condition, a twin that keeps it.  The levels the linter
// gives are pinned definition by definition in derived-conditions.levels
// (levels.py --expect), so that a condition once checked stays checked.
// The audit of 2026-11-25 found each of the broken ones classed Jlt or FJlt.
// Two rows are still open, and their cases are pinned at the level they get
// today, to be moved when their checks exist: if constexpr on a trait with
// branches of two types, and on a shape the signature does not name.
// Standalone, no imports, so it is checked without the build.
#include <concepts>
#include <cstddef>

namespace jlt_listing {

// An implicit conversion of an arithmetic value: it does not narrow.
constexpr bool narrowed_to_bool(std::size_t n) { return n; }
constexpr bool compared(std::size_t n) { return n != 0; }  // sound
constexpr std::size_t narrowed_to_char(std::size_t n) {
  const unsigned char c = n;
  return c;
}
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

// Still open: if constexpr with branches of two types, and on a shape the
// signature does not name.  Classed Jlt today.
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

}  // namespace jlt_listing
