// Negative control for rules/rule3-no-loops.query: the gate must flag the
// loops whose end is not stated before they start, and no counted loop (Table
// 7: a counted for, a range-for over a sized range, the bound known before the
// first step, the counter and the bound not assigned in the body, no break,
// return or goto inside; Meyer and Ritchie's LOOP, whose count is fixed on
// entry).  Every finding is marked; the loops marked "sound" must not be
// flagged, and nothing outside the jlt_listing namespaces.  Standalone, no
// imports, so it is checked without the build.
#include <array>
#include <cstddef>

namespace jlt_listing {

// ---- sound: a counted for
constexpr std::size_t sum_for(std::size_t n) {
  std::size_t s = 0;
  for (std::size_t i = 0; i < n; ++i) s += i;  // sound
  return s;
}
constexpr std::size_t sum_literal() {
  std::size_t s = 0;
  for (std::size_t i = 0; i < 4; i++) s += i;  // sound
  return s;
}
constexpr std::size_t sum_const() {
  constexpr std::size_t n = 4;
  std::size_t s = 0;
  for (std::size_t i = 0; i < n; ++i) s += i;  // sound
  return s;
}
constexpr double trace(std::array<double, 9> a) {
  double t = 0;
  for (std::size_t i = 0; i < 3; ++i)  // sound
    for (std::size_t j = 0; j < 3; ++j)  // sound
      if (i == j) t += a[3 * i + j];
  return t;
}
constexpr std::size_t sum_array_size(std::array<std::size_t, 3> a) {
  std::size_t s = 0;
  for (std::size_t i = 0; i < a.size(); ++i) s += a[i];  // sound
  return s;
}

// ---- sound: a range-for over a sized range (an array)
constexpr std::size_t sum_range(const int (&xs)[3]) {
  std::size_t s = 0;
  for (int x : xs) s += static_cast<std::size_t>(x);  // sound
  return s;
}
constexpr std::size_t sum_std_array(std::array<std::size_t, 3> a) {
  std::size_t s = 0;
  for (std::size_t x : a) s += x;  // sound
  return s;
}

// ---- unsound: no count before the first step
namespace intro {
constexpr std::size_t halve(std::size_t n) {
  while (n > 1) n /= 2;  // finding 1
  return n;
}
constexpr std::size_t once(std::size_t n) {
  do --n;  // finding 2
  while (n > 0);
  return n;
}
}  // namespace intro

constexpr std::size_t counter_assigned(std::size_t n) {
  std::size_t s = 0;
  for (std::size_t i = 0; i < n; ++i) {  // finding 3
    s += i;
    i = i + 1;
  }
  return s;
}
constexpr std::size_t bound_assigned(std::size_t n) {
  std::size_t s = 0;
  for (std::size_t i = 0; i < n; ++i) {  // finding 4
    s += i;
    --n;
  }
  return s;
}
constexpr std::size_t breaks(std::size_t n) {
  std::size_t s = 0;
  for (std::size_t i = 0; i < n; ++i) {  // finding 5
    if (s > 10) break;
    s += i;
  }
  return s;
}
constexpr std::size_t returns(std::size_t n) {
  for (std::size_t i = 0; i < n; ++i)  // finding 6
    if (i * i > n) return i;
  return n;
}
constexpr std::size_t other_condition(std::size_t n) {
  std::size_t s = 0;
  for (std::size_t i = 0; s < n; ++i) s += i;  // finding 7
  return s;
}
constexpr std::size_t no_condition(std::size_t n) {
  std::size_t s = 0;
  for (std::size_t i = 0;; ++i) {  // finding 8
    if (i > n) return s;
    s += i;
  }
}

// A range with no size: its end is not known before the first step.
struct Unsized {
  struct It {
    std::size_t k;
    constexpr std::size_t operator*() const { return k; }
    constexpr It& operator++() {
      ++k;
      return *this;
    }
    constexpr bool operator!=(const It& o) const { return k != o.k; }
  };
  constexpr It begin() const { return {0}; }
  constexpr It end() const { return {3}; }
};
constexpr std::size_t sum_unsized(Unsized u) {
  std::size_t s = 0;
  for (std::size_t x : u) s += x;  // finding 9
  return s;
}

}  // namespace jlt_listing

// Outside the listings' namespaces: the harness, not judged.
inline std::size_t harness(std::size_t n) {
  std::size_t s = 0;
  while (s < n) ++s;
  return s;
}
