// Negative control for rules/rule3-no-loops.query: the gate must flag the four
// loops inside the jlt_listing namespaces and none outside them.  Standalone,
// no imports, so it is checked without the build.
#include <cstddef>

namespace jlt_listing {

constexpr std::size_t sum_for(std::size_t n) {
  std::size_t s = 0;
  for (std::size_t i = 0; i < n; ++i) s += i;  // finding 1
  return s;
}

constexpr std::size_t sum_range(const int (&xs)[3]) {
  std::size_t s = 0;
  for (int x : xs) s += static_cast<std::size_t>(x);  // finding 2
  return s;
}

namespace intro {
constexpr std::size_t halve(std::size_t n) {
  while (n > 1) n /= 2;  // finding 3
  return n;
}
constexpr std::size_t once(std::size_t n) {
  do --n;  // finding 4
  while (n > 0);
  return n;
}
}  // namespace intro

}  // namespace jlt_listing

// Outside the listings' namespaces: the harness, not judged.
inline std::size_t harness(std::size_t n) {
  std::size_t s = 0;
  for (std::size_t i = 0; i < n; ++i) s += i;
  return s;
}
