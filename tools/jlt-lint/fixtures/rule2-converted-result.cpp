// Negative control for rules/rule2-converted-result.query (the tool's
// converted-result check): a converted requirement's result used at its own
// type is a finding, and at the declared type it is not.  Standalone.
#include <concepts>

namespace jlt_listing {

template <typename T>
T f(T);
template <typename T>
T g(T);
template <typename T>
int h(T);

template <typename T>
concept Loose = requires(const T a, const T b) {
  { a + b } -> std::convertible_to<T>;
  { a == b } -> std::convertible_to<bool>;
  { f(a) } -> std::convertible_to<T>;
  { g(a) } -> std::same_as<T>;
};

template <Loose T>
constexpr bool compared(const T& a, const T& b) {
  return (a + b) == (b + a);  // converted-result 1, 2: == at the own type
}
template <Loose T>
constexpr T chained(const T& a, const T& b) {
  return a + b + a;  // converted-result 3: the inner sum at its own type
}
template <Loose T>
constexpr T held(const T& a, const T& b) {
  const auto x = a + b;  // converted-result 4: auto keeps the own type
  return x;
}
template <Loose T>
constexpr auto member(const T& a, const T& b) {
  return (a + b).size();  // converted-result 5: a member of the own type
}
template <Loose T>
constexpr T called(const T& a) {
  return f(a) + a;  // converted-result 6: f's result at its own type
}

template <typename T>
concept Membered = requires(const T a) {
  { a.value } -> std::convertible_to<T>;
  { a.next() } -> std::convertible_to<T>;
};
template <Membered T>
constexpr T field(const T& a) {
  const auto x = a.value;  // converted-result 7: a data member's result
  return x;
}
template <Membered T>
constexpr T stepped(const T& a) {
  const auto x = a.next();  // converted-result 8: a member call's result
  return x;
}
template <typename T>
concept Shaped = requires(const T a, const T b) {
  { a + b } -> std::convertible_to<T>;
  { a + b } -> std::semiregular;
};
template <Shaped T>
constexpr T shaped(const T& a, const T& b) {
  return a + b + a;  // converted-result 9: a shape is not the exact type
}
template <typename T>
concept Exactly = requires(const T a, const T b) {
  { a + b } -> std::same_as<T>;
};
template <typename T>
  requires Shaped<T> && (Exactly<T> || std::semiregular<T>)
constexpr T either(const T& a, const T& b) {
  return a + b + a;  // converted-result 10: a disjunct guarantees nothing
}
template <Loose T>
struct Box {
  template <typename U>
  constexpr T inside(const T& a, const T& b, const U&) const {
    return a + b + a;  // converted-result 11: the class's constraint holds
  }
};
template <typename T>
concept Counted = requires(const T a) {
  { h(a) } -> std::convertible_to<int>;
};
template <Counted T>
constexpr int tested(const T& a) {
  return h(a) ? 1 : 0;  // converted-result 12: a condition is at bool, not int
}
template <Counted T>
constexpr int tested_if(const T& a) {
  if (h(a)) {  // converted-result 13: the same with an if
    return 1;
  } else {
    return 0;
  }
}
template <Loose T>
constexpr auto invoked(const T& a) {
  return f(a)();  // converted-result 14: the result called at its own type
}
template <Loose T>
constexpr T braced(const T& a, const T& b) {
  const auto x{a + b};  // converted-result 15: braces keep the own type too
  return x;
}

template <typename T, typename R>
concept EqAs = requires(const T a, const T b) {
  { a == b } -> std::convertible_to<R>;
};
template <typename T>
  requires EqAs<T, bool>
constexpr bool as_bool(const T& a, const T& b) {  // fine: R is bool here
  return !(a == b);
}

template <Loose T>
constexpr T at_type(const T& a, const T& b) {  // fine: returned at T
  return a + b;
}
template <Loose T>
constexpr T declared(const T& a, const T& b) {  // fine: held at T
  const T x = a + b;
  return x;
}
template <Loose T>
constexpr T chosen(const T& a, const T& b) {  // fine: a condition is at bool
  return (a == b) ? a : b;
}
template <Loose T>
constexpr bool both(const T& a, const T& b) {  // fine: !, && and || are at bool
  return !(a == b) && (b == a);
}
template <Loose T>
constexpr T exact(const T& a) {  // fine: g's result is exactly T
  return g(a) + a;
}

}  // namespace jlt_listing
