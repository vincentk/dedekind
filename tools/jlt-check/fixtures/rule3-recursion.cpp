// Negative control for rules/rule3-no-self-recursion.query: recursion on a
// value is a finding; recursion over a type's structure is a fold and passes.
// Standalone.
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

}  // namespace jlt_listing

inline int harness(int n) { return n == 0 ? 0 : harness(n - 1); }  // not counted
