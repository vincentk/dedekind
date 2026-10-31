// Negative control for rules/rule3-no-self-recursion.query: recursion on a
// value is a finding unless its descent is visible (p / c, p >> c, p - 1 on
// an unsigned parameter, under a test of it against 0); recursion over a
// type's structure is a fold and passes.  Standalone.
#include <cstddef>

namespace jlt_listing {

constexpr int count_down(int n) {  // self-recursion 1: on a value
  return n == 0 ? 0 : count_down(n - 1);
}

template <typename T>
constexpr T halve(T x) {  // self-recursion 2: on a value, in a template
  return x == 0 ? x : halve(x / 2);
}
static_assert(halve(8) == 0);  // instantiates halve<int>

template <int N>
struct Tag {};
template <int N>
constexpr int depth(Tag<N>) {  // fine: depth<N> calls depth<N - 1>
  if constexpr (N == 0)
    return 0;
  else
    return 1 + depth(Tag<N - 1>{});
}
static_assert(depth(Tag<3>{}) == 3);

template <typename T, typename... Ts>
constexpr int count() {  // fine: count<T, Ts...> calls count<Ts...>
  if constexpr (sizeof...(Ts) == 0)
    return 1;
  else
    return 1 + count<Ts...>();
}
static_assert(count<int, char, bool>() == 3);

struct Counter {
  constexpr int down(int n) const;
};
constexpr int Counter::down(int n) const {  // self-recursion 3: out of line
  return n == 0 ? 0 : down(n - 1);
}

struct Unfold {
  constexpr Unfold(int n) {  // self-recursion 4: a constructor
    if (n > 0) {
      Unfold next(n - 1);
    }
  }
};

struct Delegate {
  constexpr Delegate() : Delegate(0) {}  // fine: a different constructor
  constexpr Delegate(int) {}
};

constexpr std::size_t width(int n) {  // fine: sizeof does not evaluate
  return sizeof(width(n));
}
constexpr int typed(int n) {  // fine: decltype does not evaluate
  decltype(typed(n)) r = n;
  return r;
}
constexpr bool safe(int n) {  // fine: noexcept does not evaluate
  return noexcept(safe(n));
}

// Recursion on a value that visibly descends to a base case: structural.
constexpr std::size_t down(std::size_t n) {  // fine: n - 1, guarded by n == 0
  return n == 0 ? 0 : down(n - 1);
}
constexpr std::size_t bits(std::size_t n) {  // fine: n / 2, guarded by n == 0
  if (n == 0) {
    return 0;
  } else {
    return 1 + bits(n / 2);
  }
}
constexpr std::size_t shifts(std::size_t n) {  // fine: n >> 1, guarded by n
  return n ? 1 + shifts(n >> 1) : 0;
}
static_assert(down(3) == 0 && bits(8) == 4 && shifts(8) == 4);

constexpr std::size_t stuck(std::size_t n) {  // self-recursion 5: no guard
  return stuck(n / 2);
}
constexpr std::size_t up(std::size_t n) {  // self-recursion 6: ascends
  return n == 0 ? 0 : up(n + 1);
}
constexpr std::size_t still(std::size_t n) {  // self-recursion 7: n / 1
  return n == 0 ? 0 : still(n / 1);
}
constexpr std::size_t leap(std::size_t n) {  // self-recursion 8: n - 2 wraps
  return n == 0 ? 0 : leap(n - 2);
}

// self-recursion 9: n / 2 descends, but in m's slot; n never changes
constexpr std::size_t aside(std::size_t n, std::size_t m) {
  return n == 0 ? m : aside(n, n / 2);
}

}  // namespace jlt_listing

inline int harness(int n) {
  return n == 0 ? 0 : harness(n - 1);
}  // not counted
