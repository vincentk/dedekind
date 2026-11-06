// Negative control for rules/rule2-context-order.query (the tool's
// context-order check): an instance with a context whose head has no proper
// part may mention only shapes declared before the trait's primary, or
// resolving the trait asks for itself.  Standalone.
#include <concepts>
#include <cstddef>
#include <utility>

namespace jlt_listing {

template <typename T>
constexpr std::size_t g(T) = delete;  // the trait's primary
template <typename T>
concept C = requires(const T a) {
  { g(a) } -> std::same_as<std::size_t>;
};  // declared after the primary it asks about
template <typename T>
  requires(!C<T>)
constexpr std::size_t g(T) {  // context-order 1: C follows g's primary
  return 0;
}

template <typename T>
constexpr std::size_t k(T) = delete;
template <typename T>
concept K = requires(const T a) {
  { k(a) } -> std::same_as<std::size_t>;
};
template <K T>  // context-order 2: a constrained parameter, K follows k's
                // primary
                constexpr std::size_t k(T) {
  return 0;
}

template <typename T>
concept D = std::integral<T>;  // before the primary it serves
template <typename T>
constexpr std::size_t h(T) = delete;
template <typename T>
  requires D<T>
constexpr std::size_t h(T x) {  // fine: D precedes h
  return static_cast<std::size_t>(x);
}

template <typename T>
constexpr std::size_t u(T) = delete;
template <typename T>
concept E = std::integral<T>;  // after u's primary
template <typename A, typename B>
  requires E<A> && E<B>
constexpr std::size_t u(std::pair<A, B>) {  // fine: a structured head descends
  return 0;
}

}  // namespace jlt_listing
