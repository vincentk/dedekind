// Local mutable state: code written in place, with loops, that has a System F
// reading all the same (the state-passing reading of Table 7: an assignment
// to a local is the next state of a fold).  A sound computation, Fibonacci
// in place, and beside each near-miss its sound twin.  The near-misses break the aliasing conditions A1-A6 of
// research_notes' local-state study: a write through one name must not change
// what another name reads.  The static_asserts are the algorithms' own
// correctness, tested whenever the file is parsed; the rules judge only
// whether the code can be read as a pure function.
//
// rule1-local-state refuses the eight near-misses, one finding each, and
// nothing else here: Fibonacci and the sound twins pass.  (An LU solve is left
// out until division and fields are under control: #1221.)  Standalone, no
// imports, so it is checked without the build.
#include <array>
#include <bit>
#include <cstddef>
#include <utility>
#include <vector>

namespace jlt_listing {

// ---- sound: Fibonacci by repeated squaring of [[1,1],[1,0]], in place,
// mod 2^64.  The state of the fold over the 64 bits is the pair (r, m).
constexpr std::size_t fibonacci(std::size_t n) {
  std::array<std::size_t, 4> r{1, 0, 0, 1};  // the result, the identity
  std::array<std::size_t, 4> m{1, 1, 1, 0};  // the base
  for (std::size_t i = 0; i < 64; ++i) {
    if ((std::rotr(n, int(i)) & 1) == 1) {  // bit i of n
      const std::size_t a = r[0] * m[0] + r[1] * m[2];
      const std::size_t b = r[0] * m[1] + r[1] * m[3];
      const std::size_t c = r[2] * m[0] + r[3] * m[2];
      const std::size_t d = r[2] * m[1] + r[3] * m[3];
      r[0] = a;
      r[1] = b;
      r[2] = c;
      r[3] = d;
    }
    const std::size_t a = m[0] * m[0] + m[1] * m[2];
    const std::size_t b = m[0] * m[1] + m[1] * m[3];
    const std::size_t c = m[2] * m[0] + m[3] * m[2];
    const std::size_t d = m[2] * m[1] + m[3] * m[3];
    m[0] = a;
    m[1] = b;
    m[2] = c;
    m[3] = d;
  }
  return r[1];
}
// The reference: the definition, F(k + 1) = F(k) + F(k - 1), as a fold.
constexpr std::size_t fibonacci_by_definition(std::size_t n) {
  std::size_t previous = 1, current = 0;  // F(-1), F(0)
  for (std::size_t i = 0; i < n; ++i) {
    const std::size_t next = previous + current;
    previous = current;
    current = next;
  }
  return current;
}
static_assert(fibonacci(0) == 0 && fibonacci(1) == 1 && fibonacci(10) == 55);
static_assert(fibonacci(90) == 2880067194370816120uz);
static_assert(fibonacci(77) == fibonacci_by_definition(77) &&
              fibonacci(93) == fibonacci_by_definition(93));

// ---- near-misses, each beside its sound twin.

// A1-A6, a local of reference type: a const name for a mutable object, so a
// later write to r[0] changes it.  Refused.
constexpr std::size_t aliased_element() {
  std::array<std::size_t, 2> r{1, 2};
  const auto& x = r[0];
  r[0] = 7;
  return x;  // 7, not 1
}
constexpr std::size_t copied_element() {  // sound
  std::array<std::size_t, 2> r{1, 2};
  const auto x = r[0];
  r[0] = 7;
  return x;  // 1
}
static_assert(aliased_element() == 7 && copied_element() == 1);

// A5, an index not provably in range.  Refused.
constexpr std::size_t unbounded_index(std::size_t k) {
  std::array<std::size_t, 3> r{0, 0, 0};
  r[k] = 1;
  return r[0];
}
constexpr std::size_t bounded_index() {  // sound
  std::array<std::size_t, 3> r{0, 0, 0};
  r[2] = 1;
  return r[0];
}

// A4, two writes in one expression.  Refused.
constexpr std::size_t chained_writes() {
  std::array<std::size_t, 2> r{0, 0};
  r[0] = r[1] = 3;
  return r[0] + r[1];
}
constexpr std::size_t separate_writes() {  // sound
  std::array<std::size_t, 2> r{0, 0};
  r[1] = 3;
  r[0] = 3;
  return r[0] + r[1];
}

// A6, a lambda capturing the array by reference and writing it.  Refused.
constexpr std::size_t captured_by_reference() {
  std::array<std::size_t, 1> r{0};
  auto set = [&] { r[0] = 9; };
  set();
  return r[0];
}
constexpr std::size_t captured_by_value() {  // sound
  std::array<std::size_t, 1> r{9};
  auto get = [r] { return r[0]; };
  return get();
}

// swap of a value with itself: undefined for an array by the standard's
// precondition on swap_ranges.  Refused.
constexpr double swapped_with_itself() {
  std::array<double, 2> v{1, 2};
  std::swap(v, v);
  return v[0];
}
constexpr double swapped_with_another() {  // sound
  std::array<double, 2> v{1, 2}, w{3, 4};
  std::swap(v, w);
  return v[0];
}

// Written before it is read: a local with no initialiser.  Refused.
constexpr std::size_t uninitialised(bool b) {
  std::size_t x;
  if (b) x = 1;
  return b ? x : 0;
}
constexpr std::size_t initialised(bool b) {  // sound
  std::size_t x = 0;
  if (b) x = 1;
  return x;
}
// An array default-constructed, its elements indeterminate.  Refused.
constexpr std::size_t indeterminate() {
  std::array<std::size_t, 3> r;
  r[0] = 1;
  return r[0];
}
constexpr std::size_t value_initialised() {  // sound
  std::array<std::size_t, 3> r{};
  r[0] = 1;
  return r[0];
}

// Not refused: an element of a member array, its index in the member's range;
// a range-for's loop variable in a template, written by the loop (the loop
// rule refuses that loop, a range of template type; rule1-local-state does not).
struct Rows {
  std::array<std::size_t, 2> top, bottom;
};
constexpr std::size_t member_element(Rows a) { return a.top[0] + a.bottom[1]; }
template <typename R>
constexpr std::size_t counted_over(R r) {
  std::size_t s = 0;
  for (auto x : r) s += x;
  return s;
}
static_assert(member_element({{1, 2}, {3, 4}}) == 5 &&
              counted_over(std::array<std::size_t, 2>{1, 2}) == 3);

// A reference through an iterator names what the iterator came from: of a
// local array, a second name for its storage (refused); of a parameter, not.
constexpr std::size_t aliased_through_iterator() {
  std::array<std::size_t, 2> r{1, 2};
  auto it = r.begin();
  const auto& e = *it;
  r[0] = 7;
  return e;  // 7, not 1
}
constexpr std::size_t read_through_iterator(const std::array<std::size_t, 2>& a) {
  auto it = a.begin();
  const auto& e = *it;  // sound: a is not local state
  return e;
}
static_assert(aliased_through_iterator() == 7 &&
              read_through_iterator({1, 2}) == 1);
// Not refused: a default-constructed class template in a template (its
// constructor writes it), and two writes the comma operator sequences.
template <typename T>
constexpr std::size_t built() {
  std::vector<T> v;
  v.push_back(T{});
  return v.size();
}
constexpr std::size_t comma_sequenced() {
  std::size_t a = 0, b = 0;
  ++a, ++b;
  return a + b;
}
static_assert(built<int>() == 1 && comma_sequenced() == 2);

}  // namespace jlt_listing
