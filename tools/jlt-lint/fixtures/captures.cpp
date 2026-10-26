// Negative control for rules/rule1-no-escaping-reference-captures.query: a
// closure that carries a reference out of its function.  Each planted case
// inside jlt_listing is labelled; local uses and the harness outside must not
// count.  Standalone.
#include <algorithm>
#include <cstddef>
#include <utility>
#include <variant>

namespace jlt_listing {

constexpr auto adder_ref(const int& x) {  // escaping-capture 1: [&x]
  return [&x](int y) { return x + y; };
}

constexpr auto adder_default(const int& x) {  // escaping-capture 2: [&]
  return [&](int y) { return x + y; };
}

constexpr auto adder_value(int x) {  // fine: captured by value
  return [x](int y) { return x + y; };
}

constexpr int visited(const std::variant<int, long>& v, int k) {
  return std::visit([&](auto a) { return static_cast<int>(a) + k; },
                    v);  // fine: a visitor, called on the spot by std
}

constexpr int called_now(int k) {
  return [&] { return k; }();  // fine: invoked at once
}

template <typename F, std::size_t N>
constexpr int sum_each(const F& f) {
  return [&f]<std::size_t... I>(std::index_sequence<I...>) {
    return (f(I) + ... + 0);
  }(std::make_index_sequence<N>{});  // fine: invoked at once, in a template
}

inline auto paired(const int& x) {  // escaping-capture 3: std::make_pair
  return std::make_pair([&x] { return x; }, 0);  // stores the closure
}

constexpr bool all_small(const int (&xs)[3], int k) {
  return std::ranges::all_of(xs, [&](int v) { return v < k; });  // fine: an algorithm
}

}  // namespace jlt_listing

inline auto harness(const int& x) { return [&x] { return x; }; }  // not counted
