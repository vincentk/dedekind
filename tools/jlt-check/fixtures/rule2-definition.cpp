// Negative control for rules/rule2-definition.query (jlt-tool's definition
// check, Rule 2 at the level of names): a constrained body may use only
// operations some requirement names.  Each planted finding inside jlt_listing
// is labelled; the rest must stay silent.  Standalone.
#include <concepts>
#include <cstddef>
#include <utility>

namespace jlt_listing {

template <typename T, typename Op>
constexpr T unit(Op) = delete;

template <typename T, typename Op>
concept IsMonoid = std::semiregular<T> && requires(const T a, const T b) {
  { Op{}(a, b) } -> std::same_as<T>;
  { unit<T>(Op{}) } -> std::same_as<T>;
};

template <typename T, typename Add, typename Mul>
concept IsSemiring = IsMonoid<T, Add> && IsMonoid<T, Mul>;  // a concept chain

template <typename T, typename Op>
  requires IsMonoid<T, Op>
constexpr T twice(T x) {  // fine: Op{}(a, b) is named
  return Op{}(x, x);
}

template <typename T, typename Op>
  requires IsMonoid<T, Op>
constexpr T leak(T x) {  // definition 1: '+' is not named
  return x + x;
}

template <typename T>
constexpr std::size_t loose(T x) {  // definition 2: unconstrained, uses x
  return x.size();
}

template <typename T>
constexpr int tag(T) {  // fine: unconstrained, never uses its parameter
  return 0;
}

template <std::integral T>
constexpr T arith(T x) {  // fine: integral permits every built-in operator
  return (x * x + 1) % 7;
}

template <typename T, typename Add, typename Mul>
  requires IsSemiring<T, Add, Mul>
constexpr T both(T x) {  // fine: both operations named, through the chain
  return Add{}(Mul{}(x, x), unit<T>(Add{}));
}

template <std::equality_comparable T>
constexpr bool below(T a, T b) {  // definition 3: '<' is not named, '==' is
  return a == b || a < b;
}

template <typename A, typename B>
  requires std::equality_comparable<A>
constexpr A first_of(std::pair<A, B> p) {  // fine: .first is the pair's
  return p.first;
}

template <typename T, typename Op>
  requires IsMonoid<T, Op>
constexpr T four(T x) {  // fine: twice is a constrained template (its own check)
  return twice<T, Op>(twice<T, Op>(x));
}

template <typename T, typename Op>
  requires IsMonoid<T, Op>
constexpr T moved(T x) {  // fine: std::move is a utility, not vocabulary
  return Op{}(std::move(x), x);
}

template <typename T>
  requires requires(T t) { t.foo(); }
constexpr int adhoc(T t) {  // definition 4: bar is not named, foo is
  t.foo();
  return t.bar();
}

}  // namespace jlt_listing

template <typename T>
constexpr T harness(T x) {  // outside the scope: not judged
  return x + x;
}
