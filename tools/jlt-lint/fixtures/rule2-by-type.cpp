// Negative control for rules/rule2-by-type.query and rules/admitted.query:
// a monoid named by its unit, and three templates against it: one whose body
// uses only the promised operation (admitted), one that uses + (not
// admitted: finding 1), and one under a disjunction no rule reads
// (undecided: finding 2).  Standalone.
#include <bit>
#include <concepts>
#include <ranges>

namespace jlt_listing {

template <typename E>
constexpr bool assoc = false;
template <typename E>
struct op_of {};
template <typename E>
using carrier_t = decltype(E{}());
template <typename E>
using op_t = typename op_of<E>::type;
template <typename E>
concept Mon = requires {
  E{}();
  typename op_t<E>;
} && requires(carrier_t<E> a, carrier_t<E> b) {
  { op_t<E>{}(a, b) } -> std::same_as<carrier_t<E>>;
} && assoc<E>;

template <typename E>
  requires Mon<E>
constexpr carrier_t<E> twice(carrier_t<E> x) {
  return op_t<E>{}(x, x);  // admitted: the operation is promised
}
template <typename E>
  requires Mon<E>
constexpr carrier_t<E> doubled(carrier_t<E> x) {
  return x + x;  // finding 1: + is not promised
}
template <typename E>
  requires Mon<E> || (sizeof(E) == 1)
constexpr int either(E) {  // finding 2: undecided, a disjunction
  return 0;
}

// Known archetype escapes, each of which must come out refused or undecided
// (the practitioner's cold read of 2026-11-22).

// 1. A required call with no result type: the result is not invented.
template <typename F, typename E>
concept Applies = Mon<E> && requires(F f, carrier_t<E> x) { f(x); };
template <typename F, typename E>
  requires Applies<F, E>
constexpr carrier_t<E> applied(F f, carrier_t<E> x) {
  return f(x);  // finding 3: undecided, a required call without a result type
}

// 2. An input range has no size, though the archetype of a sized one would.
template <std::ranges::input_range R>
constexpr auto counted(R r) {
  return std::ranges::size(r);  // finding 4: not admitted, no size promised
}

// 3. An if constexpr: the archetypes take one branch only.
template <typename E>
  requires Mon<E>
constexpr carrier_t<E> branching(carrier_t<E> x) {
  if constexpr (std::integral<carrier_t<E>>)
    return x + 1;  // never checked at an archetype
  else
    return x;
}  // finding 5: undecided, an if constexpr

// 4. The use names an overload set: at the archetypes of the looser one, the
// tighter sibling is chosen, and the looser body would go unchecked.  The
// parameter is E itself, so that partial ordering compares the two and the
// tighter constraint wins (over carrier_t<E>, an alias of a decltype, the two
// would be ambiguous instead).
template <typename E>
  requires Mon<E>
constexpr E pick(E x) {
  return x + x;  // finding 6: undecided, the use resolves to the sibling
}
template <typename E>
  requires Mon<E> && std::copyable<E>
constexpr E pick(E x) {
  return x;  // admitted: chosen, and its body uses nothing unpromised
}

// 5. Constraints that come with a type in the signature.  A class whose
// constraints ask a law the templates below do not state.
template <typename E>
constexpr bool comm = false;
template <typename E>
  requires Mon<E> && comm<E>
struct Commuted {
  carrier_t<E> v;
};
// Admitted: whoever calls it holds a Commuted<E>, so comm<E> holds, and it
// is read as if stated (implied bounds, after Rust's RFC 2089).
template <typename E>
  requires Mon<E>
constexpr carrier_t<E> swapped(const Commuted<E>& c) {
  return op_t<E>{}(c.v, c.v);
}
// Finding 8: not admitted, the result type asks comm<E>, which nothing the
// caller holds guarantees: the constraints do not suffice to call it.
template <typename E>
  requires Mon<E>
constexpr Commuted<E> commuted(carrier_t<E> x) {
  return {x};
}

// 6. A result a shape asks only to be convertible to the carrier, read as if
// it were same_as but forced through a conversion: the archetype's result
// converts on the spot and is good for nothing else.
template <typename E>
concept LooseMon = requires {
  E{}();
  typename op_t<E>;
} && requires(carrier_t<E> a, carrier_t<E> b) {
  { op_t<E>{}(a, b) } -> std::convertible_to<carrier_t<E>>;
};
// Admitted: each result goes into a declared carrier or the return, the
// derived form's copy-initialisation of a T (Table 7).
template <typename E>
  requires LooseMon<E>
constexpr carrier_t<E> thrice(carrier_t<E> x) {
  const carrier_t<E> y = op_t<E>{}(x, x);
  return op_t<E>{}(y, x);
}
// Finding 10: not admitted, the inner result goes straight into a call,
// converted to whatever parameter type resolution picks there, which a real
// operation's overloads may pick differently.
template <typename E>
  requires LooseMon<E>
constexpr carrier_t<E> nested(carrier_t<E> x) {
  return op_t<E>{}(op_t<E>{}(x, x), x);
}
// Finding 9: not admitted, the result is kept under a name of its own type
// and converted later, which the shape does not promise.
template <typename E>
  requires LooseMon<E>
constexpr carrier_t<E> kept(carrier_t<E> x) {
  auto r = op_t<E>{}(x, x);
  return op_t<E>{}(r, x);
}

// 7. An integer parameter, checked at the smallest and the largest integer
// type its constraint admits (here bool and unsigned long long).
// Finding 11: not admitted, at bool the sum is an int, and braces do not
// narrow it back; at the largest type it compiles.
template <std::unsigned_integral N>
constexpr N summed(N a, N b) {
  return N{a + b};
}
// Admitted: the conversion written as a call, the kernel's (Cast:integer).
template <std::unsigned_integral N>
constexpr N cast_sum(N a, N b) {
  return static_cast<N>(a + b);
}

// 8. More of each rule, from the cold read of #1212.
// Finding 12: not admitted, the proxy converted in a constructor's argument,
// not into a declared carrier: late.
template <typename E>
struct Boxed {
  carrier_t<E> v;
};
template <typename E>
  requires LooseMon<E>
constexpr Boxed<E> boxed(carrier_t<E> x) {
  return Boxed<E>{op_t<E>{}(x, x)};
}
// Finding 13: not admitted, std::countr_zero refuses the character types,
// which lie between the smallest and the largest integer type admitted.
template <std::unsigned_integral N>
  requires(!std::same_as<N, bool>)
constexpr int zeros(N n) {
  return std::countr_zero(n);
}
// An overloaded pair whose result has a destructor, so that the call form's
// call sits under a temporary.  The looser one is undecided, resolved to
// its sibling; the tighter one is admitted.
struct Owned {
  int v;
  constexpr ~Owned() {}
};
template <typename E>
  requires Mon<E>
constexpr Owned listed(E x) {
  return Owned{x + x};
}
template <typename E>
  requires Mon<E> && std::copyable<E>
constexpr Owned listed(E) {
  return Owned{0};
}

}  // namespace jlt_listing
