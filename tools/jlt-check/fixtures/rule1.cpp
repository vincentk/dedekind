// Negative control for the rule1-*.query matchers: each planted violation
// inside jlt_listing is labelled with its rule; everything outside the
// namespace is harness and must not count.  Standalone, no imports.
#include <cstddef>

namespace jlt_listing {

struct Counter {
  mutable std::size_t hits = 0;  // a mutable member: plugin work, no matcher
  std::size_t operator()(std::size_t x) {  // const-call-operator 1, constexpr 1
    return x;
  }
};

inline std::size_t bump(std::size_t& x) {  // constexpr 2, lvalue-reference 1
  return ++x;
}

constexpr std::size_t checked(std::size_t x) {
  if (x == 0) throw 0;  // no-throw 1
  return x;
}

constexpr std::size_t sink(std::size_t&& x) {  // rvalue-reference 1
  return x;
}

template <typename T>
constexpr std::size_t forward_one(T&& x) {  // forwarding-reference 1
  return sizeof(x);
}

constexpr std::size_t forward_auto(auto&& x) {  // forwarding-reference 2
  return sizeof(x);
}

template <typename T>
struct Sink {
  constexpr void take(T&& x) const {}  // rvalue-reference 2: T is the class's
};

using Ref = std::size_t&;
constexpr std::size_t via(Ref x) {  // lvalue-reference 2
  return x;
}

inline std::size_t counter() {  // constexpr 3
  static std::size_t n = 0;  // no-static-locals 1
  return ++n;
}

// Exempt, each for its stated reason.
struct Fine {
  constexpr std::size_t operator()(std::size_t x) const { return x; }
  static constexpr std::size_t twice(std::size_t x) { return 2 * x; }
};
struct StaticCall {
  static constexpr std::size_t operator()(std::size_t x) { return x; }
};
constexpr std::size_t read(const std::size_t& x) { return x; }
constexpr std::size_t read_temp(const std::size_t&& x) { return x; }
constexpr std::size_t table(std::size_t i) {
  constexpr static std::size_t t[2] = {1, 2};
  return t[i];
}
template <typename T>
constexpr T unit(T) = delete;
template <typename T>
concept Incrementable = requires(T& t) { ++t; };  // a requires local, not a parameter

constexpr auto lambda = [](std::size_t x) mutable { return ++x; };

}  // namespace jlt_listing

// Harness: every violation again, outside the namespace, not counted.
inline int harness(std::size_t& x) {
  static int n = 0;
  if (x == 0) throw n;
  struct H {
    mutable int m = 0;
    int operator()() { return m; }
  };
  return H{}();
}
