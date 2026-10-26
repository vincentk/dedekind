// Negative control for rules/rule2-definition.query (jlt-lint's definition
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

template <typename F>
constexpr int applied(F f, int x) {  // definition 5: f is called, '()' unnamed
  return f(x);
}

template <typename F>
constexpr bool involutive(F) {  // a law predicate: a trait in function spelling
  return false;
}
template <typename F>
constexpr bool bijective(F f) {  // fine: a call to a bool-returning constexpr
  return involutive(f);        // template is a trait read, which Rule 2 names
}

constexpr int reading(std::pair<int, int> p) { return p.first; }
template <typename T>
constexpr int concrete(T x) {  // fine: a non-template function on a concrete
  return reading(std::pair{0, 0}) + 0 * sizeof(x);  // argument, not on x
}

template <typename A, typename B>
struct Box {
  A a;
  B b;
};
template <typename A, typename B>
constexpr A unbox(Box<A, B> box) {  // fine: a member of a library aggregate
  return box.a;
}

template <typename T, typename Op>
  requires IsMonoid<T, Op>
constexpr T through(T x) {  // fine: a call on a call's result, not on x
  return twice<T, Op>(x);
}

template <typename A, typename B>
constexpr int apply_box(Box<A, B> box, A a) {  // fine: a call on the box's
  return box.b(a);                              // member, the box's vocabulary
}

template <std::integral N>
struct Counter {
  template <typename F>
    requires std::invocable<const F&, N>
  constexpr N apply(const F& f, N n) const {  // fine: the class's integral
    return f(n) + n * 2;                      // grants * and +, the member's
  }                                           // invocable grants ()
};

struct Inactive {};
template <typename R>
concept Fires = !std::same_as<R, Inactive>;
template <typename A>
constexpr Inactive some_law(const A&) { return {}; }

template <typename T>
constexpr auto probed(T t) {  // definition 6-9: an inline requires-expression
  if constexpr (requires { t.steer(); }) {  // or concept test is NOT a
    return t.steer();                        // constraint; the body probes
  } else if constexpr (Fires<decltype(some_law(t))>) {  // steer and some_law
    return some_law(t);                      // instead of requiring them
  } else {
    return t;
  }
}

template <typename T>
  requires requires(T t) { t.steer(); } || Fires<decltype(some_law(std::declval<T>()))>
constexpr auto declared(T t) {  // fine: the same body under declared constraints
  if constexpr (requires { t.steer(); }) {
    return t.steer();
  } else if constexpr (Fires<decltype(some_law(t))>) {
    return some_law(t);
  } else {
    return t;
  }
}

template <typename T>
constexpr int unprobed(T t) {  // definition 10: no probe, no constraint
  return t.steer();
}

template <std::regular T>
constexpr bool same(T a, T b) {  // fine: regular grants == (and =)
  return a == b;
}

template <typename... Ts>
  requires(std::equality_comparable<Ts> && ...)
constexpr bool all_self_equal(Ts... xs) {  // fine: a fold over the pack grants ==
  return ((xs == xs) && ...);
}

template <typename T>
constexpr auto doubler(T) {  // fine: the lambda is its own function, with its
  return []<std::integral U>(U x) { return x + x; };  // own constraint
}

}  // namespace jlt_listing

template <typename T>
constexpr T harness(T x) {  // outside the scope: not judged
  return x + x;
}
