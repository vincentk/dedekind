// Negative control for rules/rule2-constrained-templates.query.  Each planted
// case inside jlt_listing is labelled; the constrained forms, the exemptions
// and the harness outside must not count.  Standalone.
#include <concepts>

namespace jlt_listing {

template <typename T>
constexpr T loose(T x) {  // unconstrained 1
  return x + x;
}

constexpr auto abbreviated(auto x) { return x; }  // unconstrained 2: auto

template <std::integral T>
constexpr T typed(T x) {  // fine: a type constraint
  return x + x;
}

template <typename T>
  requires std::integral<T>
constexpr T claused(T x) {  // fine: a requires clause
  return x;
}

template <typename T>
constexpr T trailing(T x)
  requires std::integral<T>
{  // fine: a trailing requires clause
  return x;
}

constexpr auto abbreviated_c(std::integral auto x) { return x; }  // fine

template <typename T, typename Op>
constexpr T unit(Op) = delete;  // fine: a deleted dispatch primary
template <>
constexpr int unit<int>(char) {  // fine: an explicit specialisation
  return 0;
}

template <typename T>
struct is_trait {  // fine: a class template (a trait)
  static constexpr bool value = false;
};

inline constexpr auto lambda = [](auto x) { return x; };  // fine: a lambda

}  // namespace jlt_listing

template <typename T>
constexpr T harness(T x) {  // outside the namespace: not counted
  return x;
}
