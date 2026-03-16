module;
#include <cmath>
#include <concepts>
#include <functional>
#include <type_traits>

export module dedekind.sets:predicates;
// C++23 feature:
// import std;

namespace dedekind::sets::predicates {

// Generic Logical NOT for any predicate function
export template <typename P>
constexpr auto operator!(P&& pred) {
  return [p = std::forward<P>(pred)](const auto& x) { return !p(x); };
}

// Logical AND for predicates
export template <typename P1, typename P2>
auto operator&&(P1&& p1, P2&& p2) {
  return [f1 = std::forward<P1>(p1), f2 = std::forward<P2>(p2)](const auto& x) {
    return f1(x) && f2(x);
  };
}

// Logical OR for predicates
export template <typename P1, typename P2>
auto operator||(P1&& p1, P2&& p2) {
  return [f1 = std::forward<P1>(p1), f2 = std::forward<P2>(p2)](const auto& x) {
    return f1(x) || f2(x);
  };
}

// Works for any type that supports > 0
export inline constexpr auto is_positive = [](const auto& x) { return x > 0; };

// Works for any type that supports % 2 (integers, custom bitsets)
export inline constexpr auto is_even = [](const auto& x) { return x % 2 == 0; };
export inline constexpr auto is_odd = !is_even;  // Defined via negation

// Works for any type that supports absolute subtraction (floats, complex)
// We use a trailing template or auto for the epsilon to keep it generic
export inline constexpr auto is_near = [](const auto& target,
                                          const auto& epsilon) {
  return [target, epsilon](const auto& x) {
    return std::abs(x - target) < epsilon;
  };
};

// Convenient alias for the unit circle/value
export inline constexpr auto is_unit = [](const auto& x) {
  return std::abs(x - 1.0) < 1e-9;
};
}  // namespace dedekind::sets::predicates
