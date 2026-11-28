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
// (some with a finding at more than one use, 55 in all) and nothing else
// here: Fibonacci and the sound twins pass.  (An LU solve is left
// out until division and fields are under control: #1221.)  Standalone, no
// imports, so it is checked without the build.
#include <algorithm>
#include <array>
#include <bit>
#include <bitset>
#include <cstddef>
#include <expected>
#include <functional>
#include <optional>
#include <ranges>
#include <span>
#include <string_view>
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
  const auto& e = *it;  // no finding of this rule: a is not local state (the kinds refuse *it)
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
constexpr std::size_t repeated(std::size_t n, std::size_t times) {  // sound: views::repeat copies n
  return std::ranges::fold_left(std::views::repeat(n, times), 0uz, std::plus{});
}
constexpr std::size_t sorted_in_place() {  // refused
  std::array<std::size_t, 2> r{2, 1};
  std::ranges::sort(r);
  return r[0];
}
static_assert(counted_to(4, {1, true}) == 7 && repeated(5, 3) == 15 && sorted_in_place() == 1);

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

// The referees' cases of the second round on #1219, each refused, and two
// that were false refusals.
// (A writer of the standard that shares a name with a reader, std::ranges::iota
// against views::iota, is told apart by its qualified name; it has no case
// here because one of the two standard libraries the fixture is parsed with
// does not have it yet.)
// In a template, a call into the standard is judged as outside one: its
// behaviour is known, whatever the argument's type.
template <std::regular T>
constexpr T aliased_through_tie_generic(T x) {
  auto t = std::tie(x);
  std::get<0>(t) = T{};
  return x;
}
template <std::totally_ordered T>
constexpr T smallest(std::array<T, 2> r) {
  std::ranges::sort(r);
  return r[0];
}
// A temporary with a reference field: a write of k that is not an assignment
// to k.
struct Ref {
  std::size_t& x;
};
constexpr std::size_t written_through_temporary() {
  std::array<std::size_t, 8> r{1, 2, 3};
  std::size_t k = 0;
  Ref{k}.x = 7;
  return r[k];
}
// A write through a choice of two variables, and through a comma.
constexpr std::size_t written_through_choice(bool c) {
  std::array<std::size_t, 3> r{1, 2, 3};
  std::size_t i = 0, j = 0;
  (c ? i : j) = 9;
  return r[i];
}
constexpr void bump(std::size_t& x) { ++x; }
constexpr std::size_t written_through_comma() {
  std::size_t x = 0;
  bump((0, x));
  return x;
}
// A parameter that is a view: an element written through it is its caller's.
struct Overwrite {
  constexpr std::size_t operator()(std::span<std::size_t, 4> s) const {
    s[0] = 7;
    return s[1];
  }
};
// A lambda that writes its argument, which a dependent call would pass.
template <typename F>
constexpr std::size_t after(F f, std::size_t y) {
  f(y);
  return y;
}
constexpr std::size_t written_by_lambda() {
  return after([](std::size_t& v) { v = 9; return 0uz; }, 1uz);
}
// An index and an end of a sequence whose type does not give its size.
constexpr char character_at(std::string_view s, std::size_t i) { return s[i]; }
constexpr std::size_t first_of(const std::vector<std::size_t>& v) { return v.front(); }
// Not refused by local state: a local of a value type of the standard that
// is not a container of old, std::expected.  Undecided, since its value is
// built by conversion, a spelling of the sum not yet written (#1222).
constexpr std::expected<std::size_t, bool> half(std::size_t n) {
  std::expected<std::size_t, bool> r = n / 2;
  return r;
}
// Copilot's cases on #1219.  An element written through a const span: the
// span is const, its elements are its caller's.
constexpr std::size_t overwrite_const(const std::span<std::size_t, 4> s) {
  s[0] = 7;
  return s[1];
}
// A view of a local returned: views::transform keeps a ref_view of an lvalue,
// so the view outlives the array it reads.
constexpr auto dangling_view() {
  std::array<std::size_t, 3> r{1, 2, 3};
  r[0] = 4;
  return std::views::transform(r, [](std::size_t x) { return x; });
}
// Its twin: the view consumed by a fold inside the call.  rule1-local-state
// is silent here; the definition is refused by Rule 3 alone, a fold whose
// range no IsBoundedRange parameter bounds.
constexpr std::size_t consumed_view() {
  std::array<std::size_t, 3> r{1, 2, 3};
  r[0] = 4;
  return std::ranges::fold_left(std::views::transform(r, [](std::size_t x) { return x; }), 0uz,
                                std::plus<>{});
}
static_assert(consumed_view() == 9);
// The view handed to a function of the program, in a template, as Listing 4's
// free_theorem does: the function takes it by value and returns a value, so
// passed_view has no finding of rule1-local-state (it is refused by Rule 2
// alone).  first_element has one of its own: begin() on its parameter is a
// member call that is not const.
template <typename R>
constexpr std::size_t first_element(R r) {
  return *r.begin();
}
template <typename R>
constexpr bool passed_view(R r) {  // sound
  return first_element(std::views::transform(r, [](std::size_t x) { return x; })) == first_element(r);
}
static_assert(passed_view(std::array{1uz, 2uz}));
// The view kept in a pair that is returned: the pair holds the ref_view, which
// dangles as the view alone does (Copilot on #1219).
constexpr auto dangling_in_pair() {
  std::array<std::size_t, 3> r{1, 2, 3};
  r[0] = 4;
  return std::make_pair(std::views::transform(r, [](std::size_t x) { return x; }), 0uz);
}
// at() out of range throws: refused as [] is; in range it is not.
constexpr std::size_t at_out_of_range() {
  std::array<std::size_t, 2> r{1, 2};
  r[0] = 3;
  return r.at(2);
}
constexpr std::size_t at_in_range() {  // sound
  std::array<std::size_t, 2> r{1, 2};
  r[0] = 3;
  return r.at(1);
}
static_assert(at_in_range() == 2);
// A function of the program that returns the view it is given: the view
// still dangles (Copilot on #1219).
template <typename R>
constexpr R keep(R r) {
  return r;
}
constexpr auto dangling_through_keep() {
  std::array<std::size_t, 3> r{1, 2, 3};
  r[0] = 4;
  return keep(std::views::transform(r, [](std::size_t x) { return x; }));
}
// An index whose bounds multiply beyond a long long: not known, so not shown
// in range (and the linter's own arithmetic stays defined).
constexpr std::size_t huge_product() {
  std::array<std::size_t, 2> a{1, 2};
  std::size_t m = 1uz << 40;
  std::size_t n = 1uz << 40;
  a[0] = 3;
  return a[m * n];
}
// A reference local bound to the reference a call returns: its lifetime is
// the call's object's.  Here the object is a temporary, gone at the end of
// the declaration, so r dangles; the reading of the accessor would copy.
struct Boxed {
  std::size_t v;
  constexpr const std::size_t& get() const { return v; }
};
constexpr Boxed make_boxed() { return Boxed{4}; }
constexpr std::size_t through_returned_reference() {  // refused
  const std::size_t& r = make_boxed().get();
  return r + 0;
}
// A temporary bound directly lives as long as the reference: no finding.
constexpr std::size_t bound_to_temporary() {  // sound
  const std::size_t& r = make_boxed().v;
  return r;
}
// A reference from a call on an object that outlives it: no temporary, no
// dangling, and a constant is no local state to alias.
constexpr std::size_t through_accessor_of_constant() {  // sound
  const Boxed b{5};
  const std::size_t& r = b.get();
  return r;
}
// Through fields and calls on both sides: a field of what a call on a
// temporary returns, and an accessor called on a field of a temporary,
// dangle; a field of a field of a temporary is extended with it.
struct Outer {
  Boxed h;
  constexpr const Boxed& get() const { return h; }
};
constexpr Outer make_outer() { return Outer{Boxed{6}}; }
constexpr std::size_t dangling_through_field() {  // refused
  const std::size_t& r = make_outer().get().v;
  return r + 0;
}
constexpr std::size_t dangling_through_member() {  // refused
  const std::size_t& r = make_outer().h.get();
  return r + 0;
}
constexpr std::size_t extended_through_fields() {  // sound
  const std::size_t& r = make_outer().h.v;
  return r;
}
// Through ?: and the comma: what the result names is an arm's, the right's.
constexpr std::size_t dangling_through_conditional() {  // refused
  const std::size_t& r = true ? make_boxed().get() : make_boxed().get();
  return r + 0;
}
constexpr std::size_t dangling_through_comma_operator() {  // refused
  const std::size_t& r = (0, make_boxed().get());
  return r + 0;
}
// A static member is its own storage, not the temporary's.
struct Holder {
  static constexpr Boxed h{6};
};
constexpr std::size_t through_static_member() {  // sound
  const std::size_t& r = Holder{}.h.get();
  return r;
}
// A temporary passed by value is a copy the result cannot name.
struct Tag {};
struct Tagged {
  std::size_t v;
  constexpr const std::size_t& get(Tag) const { return v; }
};
constexpr std::size_t accessor_with_tag() {  // sound
  const Tagged b{5};
  const std::size_t& r = b.get(Tag{});
  return r;
}
static_assert(written_through_temporary() == 0 &&
              written_through_comma() == 1 && written_by_lambda() == 9 && half(4) == 2uz &&
              bound_to_temporary() == 4 && through_accessor_of_constant() == 5 &&
              extended_through_fields() == 6 && accessor_with_tag() == 5 &&
              through_static_member() == 6);

// std::move of a local at its last use is read as the local, a copy
// (#1223): nothing after it can see what the move leaves behind.
constexpr std::size_t moved_into_local(std::array<std::size_t, 3> a) {  // sound
  std::array<std::size_t, 3> b = std::move(a);
  return b[0];
}
constexpr std::array<std::size_t, 2> moved_in_return(std::array<std::size_t, 2> a) {  // sound
  return std::move(a);
}
constexpr std::size_t sum_of(std::array<std::size_t, 2> a) { return a[0] + a[1]; }
constexpr std::size_t moved_last(std::array<std::size_t, 2> a) {  // sound
  a[0] = 5;
  return sum_of(std::move(a));
}
// Near misses: a use after the move, a use beside it in one expression (the
// operands of + are unsequenced), a move in a loop, a reference moved.
constexpr std::size_t used_after_move(std::array<std::size_t, 2> a) {  // refused
  std::array<std::size_t, 2> b = std::move(a);
  return b[0] + a[1];
}
constexpr std::array<std::size_t, 2> moved_beside_its_type(std::array<std::size_t, 2> a) {  // sound
  return decltype(a){std::move(a)};
}
constexpr bool moved_after_its_test(std::array<std::size_t, 2> a) {  // sound: && sequences its left first
  return a[0] == 1 && sum_of(std::move(a)) == 3;
}
constexpr std::size_t moved_beside_use(std::array<std::size_t, 2> a) {  // refused
  return a[0] + sum_of(std::move(a));
}
// A move in a macro (its order not seen), and a write through what a move
// yields (a span's elements are the caller's).
#define JLT_MOVE_THEN_READ(a)                \
  std::array<std::size_t, 2> b = std::move(a); \
  return b[0] + a[1]
constexpr std::size_t moved_in_macro(std::array<std::size_t, 2> a) {  // refused
  JLT_MOVE_THEN_READ(a);
}
constexpr std::size_t written_through_move(std::span<std::size_t, 1> s) {  // refused
  std::move(s)[0] = 7;
  return 0;
}
constexpr std::size_t moved_in_loop(std::array<std::size_t, 2> a) {  // refused
  std::size_t s = 0;
  for (std::size_t i = 0; i < 2; ++i) s = s + sum_of(std::move(a));
  return s;
}
constexpr std::size_t moved_reference(std::array<std::size_t, 2> a) {  // refused
  std::array<std::size_t, 2>& r = a;
  return sum_of(std::move(r));
}
static_assert(moved_into_local({1, 2, 3}) == 1 && moved_in_return({1, 2})[1] == 2 &&
              moved_last({1, 2}) == 7 && used_after_move({1, 2}) == 3 &&
              moved_beside_use({1, 2}) == 4 && moved_in_macro({1, 2}) == 3 && moved_after_its_test({1, 2}) && moved_beside_its_type({1, 2})[1] == 2 && moved_in_loop({1, 2}) == 6 &&
              moved_reference({1, 2}) == 3);

// A standard container's update on a local, in a statement of its own: the
// assignment of the container updated.  A position, an empty container's
// pop, a bitset's position that may lie out of range, an argument that
// shares storage, a container not of the standard: refused.
constexpr std::vector<std::size_t> appended(std::vector<std::size_t> v, std::size_t x) {  // sound
  v.push_back(x);
  return v;
}
constexpr std::bitset<256> marked_byte(std::bitset<256> b, unsigned char c) {  // sound
  b.set(c);
  return b;
}
constexpr std::size_t emptied(std::vector<std::size_t> v) {  // sound
  v.clear();
  return v.size();
}
struct Grown {
  std::vector<std::size_t> xs;
};
constexpr Grown grown(Grown g) {  // sound: through a field of a deep value
  g.xs.push_back(1);
  return g;
}
constexpr std::size_t popped(std::vector<std::size_t> v) {
  v.pop_back();
  return v.size();
}
constexpr std::bitset<8> marked_anywhere(std::bitset<8> b, std::size_t i) {
  b.set(i);
  return b;
}
constexpr std::size_t inserted_at(std::vector<std::size_t> v) {
  v.insert(v.begin(), 1);
  return v.size();
}
constexpr std::size_t pushed_address(std::size_t x) {
  std::vector<const std::size_t*> v;
  v.push_back(&x);
  return v.size();
}
struct Pile {
  std::size_t n = 0;
  constexpr void push_back(std::size_t) { ++n; }
};
constexpr std::size_t pushed_on_own_type(Pile p) {
  p.push_back(1);
  return p.n;
}
constexpr bool pushed_in_condition(std::vector<std::size_t> v) {
  return (v.push_back(1), v.size()) == 1;
}
static_assert(appended({1}, 2).size() == 2 && marked_byte({}, 3).test(3) && emptied({4}) == 0 &&
              grown({}).xs.size() == 1 && popped({1, 2}) == 1 && marked_anywhere({}, 2).test(2) &&
              inserted_at({}) == 1 && pushed_address(1) == 1 && pushed_on_own_type({}) == 1 &&
              pushed_in_condition({}));

}  // namespace jlt_listing
