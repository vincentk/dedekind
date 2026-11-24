// Negative control for rules/rule2-no-type-observation.query: five
// observations of a dependent type outside any shape (findings 1 to 5) and
// three that are not findings.  Standalone.
#include <concepts>
#include <type_traits>

namespace jlt_listing {

template <typename T>
constexpr int size_of(T) {
  return sizeof(T);  // finding 1: sizeof of a type parameter
}
template <typename T>
constexpr int aligned(T) {
  return alignof(T);  // finding 2: alignof
}
template <typename T>
constexpr int pick_integral(T x) {
  return std::is_integral_v<T> ? 1 : 0;  // finding 3: a trait, as a value
}
template <typename T, typename U>
constexpr bool same(T, U) {
  return std::is_same<T, U>::value;  // finding 4: a trait's ::value
}
template <typename T>
constexpr bool integral(T) {
  return std::integral<T>;  // finding 5: a standard concept as a value
}

// Not findings.
template <typename T>
constexpr int branch(T x) {
  if constexpr (std::integral<T>)  // the condition of an if constexpr
    return 1;
  else
    return 0;
}
template <typename T>
constexpr int fixed(T) {
  return sizeof(int);  // not dependent
}
constexpr int plain(int x) { return sizeof(x); }  // not a template
template <typename T>
struct Holder {
  T value;
  constexpr T get() const noexcept(std::is_nothrow_copy_constructible_v<T>)
    requires std::copyable<T>  // a constraint and a noexcept, not the body
  {
    return value;
  }
};

}  // namespace jlt_listing
