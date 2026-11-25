// Local mutable state: code written in place, with loops, that has a System F
// reading all the same (the state-passing reading of Table 7: an assignment
// to a local is the next state of a fold).  Two sound computations, a running
// sum and Fibonacci, each in place, and beside each near-miss its sound twin.
// The near-misses break the aliasing conditions A1-A6 of
// research_notes' local-state study: a write through one name must not change
// what another name reads.  The static_asserts are the algorithms' own
// correctness, tested whenever the file is parsed; the rules judge only
// whether the code can be read as a pure function.
//
// rule1-local-state checks every use of a mutable local against a list of
// the positions Table 7's rows read (an allowlist): it refuses each near-miss
// (some with a finding at more than one use, 38 in all) and nothing else
// here: Fibonacci and the sound twins pass.  (An LU solve is left
// out until division and fields are under control: #1221.)  Standalone, no
// imports, so it is checked without the build.
#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <functional>
#include <optional>
#include <ranges>
#include <span>
#include <tuple>
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

// ---- sound: a running sum in place, mod 2^64.  Step i reads a[i], which
// step i - 1 wrote: the state of the fold over the indices is the whole array.
constexpr std::array<std::size_t, 4> running_sum(std::array<std::size_t, 4> a) {
  for (std::size_t i = 0; i < 3; ++i) a[i + 1] = a[i + 1] + a[i];
  return a;
}
// The kernel program it is read as (Table 7's rows, applied): the state a
// record with a field for each element, the step a function object of the
// state and the index, the element's update a choice on the index, the loop
// the kernel's iteration.
struct Sums {
  std::size_t a0, a1, a2, a3;
};
struct RunningStep {
  constexpr Sums operator()(Sums s, std::size_t i) const {
    return i == 0   ? Sums{s.a0, s.a1 + s.a0, s.a2, s.a3}
           : i == 1 ? Sums{s.a0, s.a1, s.a2 + s.a1, s.a3}
                    : Sums{s.a0, s.a1, s.a2, s.a3 + s.a2};
  }
};
constexpr Sums running_sum_as_read(Sums s) {
  return std::ranges::fold_left(std::views::iota(0uz, 3uz), s, RunningStep{});
}
// The reference: the definition, element k the sum of the first k + 1.
constexpr std::array<std::size_t, 4> running_sum_by_definition(std::array<std::size_t, 4> a) {
  return {a[0], a[0] + a[1], a[0] + a[1] + a[2], a[0] + a[1] + a[2] + a[3]};
}
static_assert(running_sum({1, 2, 3, 4}) == std::array<std::size_t, 4>{1, 3, 6, 10} &&
              running_sum({5, 6, 7, 8}) == running_sum_by_definition({5, 6, 7, 8}));
// The in-place code and its reading agree, here at one argument.
static_assert(running_sum({5, 6, 7, 8})[0] == running_sum_as_read({5, 6, 7, 8}).a0 &&
              running_sum({5, 6, 7, 8})[1] == running_sum_as_read({5, 6, 7, 8}).a1 &&
              running_sum({5, 6, 7, 8})[2] == running_sum_as_read({5, 6, 7, 8}).a2 &&
              running_sum({5, 6, 7, 8})[3] == running_sum_as_read({5, 6, 7, 8}).a3);

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
// A default-constructed class template in a template (its constructor
// writes it) is not refused, nor is v.size(); push_back, a mutating member
// call, is (no row of Table 7 reads it: the theoretician's C8).  Two writes
// the comma operator sequences are not refused.
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

// Not refused: an index that is a constant, an enumerator.
enum Slot : std::size_t { first, second };
constexpr std::size_t by_enumerator(std::array<std::size_t, 2> p) { return p[second]; }
static_assert(by_enumerator({1, 2}) == 2);

// Not refused: the standard's forwarding signatures that only read or copy,
// a closed list (the kernel's iteration is written with fold_left and iota);
// an algorithm that writes its range, with the same shape of signature, is.
constexpr std::optional<std::size_t> counted_to(std::size_t budget, std::pair<std::size_t, bool> s) {
  const std::size_t n = std::ranges::fold_left(std::views::iota(0uz, budget), 0uz, std::plus{});
  return s.second ? std::optional{s.first + n} : std::nullopt;
}
constexpr std::size_t sorted_in_place() {  // refused
  std::array<std::size_t, 2> r{2, 1};
  std::ranges::sort(r);
  return r[0];
}
static_assert(counted_to(4, {1, true}) == 7 && sorted_in_place() == 1);

// The practitioner's cases on #1219, each refused.
// A reference whose source is not traced but mentions local state.
constexpr std::size_t aliased_through_max() {
  std::size_t a = 1, b = 0;
  const auto& m = std::max(a, b);
  a = 9;
  return m;  // 9
}
constexpr std::size_t aliased_through_choice(bool c) {
  std::array<std::size_t, 2> r{1, 2};
  const auto& s = c ? r[0] : r[1];
  r[0] = 9;
  return s;
}
constexpr std::size_t aliased_through_get() {
  std::array<std::size_t, 2> r{1, 2};
  const auto& g = std::get<0>(r);
  r[0] = 9;
  return g;
}
// An index written through swap, so not bounded by its initialiser.
constexpr std::size_t swapped_index() {
  std::array<std::size_t, 3> r{1, 2, 3};
  std::size_t k = 0, m = 5;
  std::swap(k, m);
  return r[k];  // k is 5
}
// An index whose value does not fit its type: 200 wraps to -56.
constexpr int wrapped_index() {
  std::array<int, 256> r{};
  signed char k = static_cast<signed char>(200);
  return r[k];
}
// An index of a sequence that is not local state: a parameter's, unbounded.
constexpr std::size_t parameter_index(const std::array<std::size_t, 3>& a, std::size_t i) {
  return a[i];
}
constexpr std::size_t parameter_literal(const std::array<std::size_t, 3>& a) {  // sound
  return a[2];
}
// An index of a sequence whose size its type does not give.
constexpr std::size_t vector_index(const std::vector<std::size_t>& v) { return v[0]; }

// The theoretician's cases on #1219 (C1, C2 and C6 are the practitioner's
// above), each refused: a value type with reference semantics, a call that
// writes inside an expression, a swap of an object with itself, a mutating
// member call, a class template that leaves its field indeterminate.
constexpr std::size_t aliased_through_tie() {  // C3
  std::size_t x = 1;
  auto t = std::tie(x);
  std::get<0>(t) = 7;
  return x;  // 7
}
constexpr std::size_t aliased_through_ref() {  // C4
  std::size_t x = 1;
  auto w = std::ref(x);
  w.get() = 7;
  return x;  // 7
}
constexpr std::size_t aliased_through_span() {  // C4
  std::array<std::size_t, 2> r{1, 2};
  std::span<std::size_t> s(r);
  s.front() = 7;
  return r[0];  // 7
}
constexpr std::size_t aliased_through_view() {  // C4
  std::array<std::size_t, 2> r{1, 2};
  auto v = std::views::all(r);
  r[0] = 5;
  return v.front();  // 5
}
constexpr std::size_t exchanged_and_read() {  // C5: 2 or 3, unspecified
  std::size_t x = 1;
  return std::exchange(x, 2) + x;
}
constexpr std::size_t swapped_by_member() {  // C7
  std::array<std::size_t, 2> r{1, 2};
  r.swap(r);
  return r[0];
}
constexpr std::size_t swapped_element_with_itself() {  // C7
  std::array<std::array<std::size_t, 2>, 2> m{};
  std::swap(m[0], m[0]);
  return m[0][0];
}
constexpr std::size_t swapped_by_ranges() {  // C7
  std::array<std::size_t, 2> r{1, 2};
  std::ranges::swap(r, r);
  return r[0];
}
constexpr std::size_t swapped_elements() {  // sound: two elements, disjoint
  std::array<std::array<std::size_t, 2>, 2> m{{{1, 2}, {3, 4}}};
  std::swap(m[0], m[1]);
  return m[0][0];
}
constexpr std::size_t emplaced_and_read() {  // C8: unspecified, as C5
  std::optional<std::size_t> o;
  return o.emplace(1) + o.value_or(0);
}
constexpr std::size_t optional_read() {  // sound
  const std::optional<std::size_t> o{1};
  return o.value_or(0);
}
template <typename T>
struct box {
  T v;
};
template <typename T>
constexpr T unboxed() {  // C9: at T = size_t, v is indeterminate
  box<T> b;
  return b.v;
}
static_assert(aliased_through_tie() == 7 && aliased_through_ref() == 7 &&
              aliased_through_span() == 7 && aliased_through_view() == 5 &&
              swapped_elements() == 3 && optional_read() == 1);

}  // namespace jlt_listing
