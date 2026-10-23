// Negative control for rules/rule3-no-self-recursion.query: recursion on a
// value is a finding; recursion over a type's structure is a fold and passes.
// Standalone.
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

}  // namespace jlt_listing

inline int harness(int n) { return n == 0 ? 0 : harness(n - 1); }  // not counted
