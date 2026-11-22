// Negative control for rules/rule2-by-type.query and rules/admitted.query:
// a monoid named by its unit, and three templates against it: one whose body
// uses only the promised operation (admitted), one that uses + (not
// admitted: finding 1), and one under a disjunction no rule reads
// (undecided: finding 2).  Standalone.
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
// tighter sibling is chosen.  The compiler instantiates the looser body on
// the way, a constexpr body, and its + fails there, inside the checked body
// and at archetypes of its own constraints: a witness all the same.
template <typename E>
  requires Mon<E>
constexpr carrier_t<E> pick(carrier_t<E> x) {
  return x + x;  // finding 6: not admitted, though the sibling is chosen
}
template <typename E>
  requires Mon<E> && std::copyable<carrier_t<E>>
constexpr carrier_t<E> pick(carrier_t<E> x) {
  // finding 7: undecided, the compiler instantiates the looser body on the
  // way and its error is not this body's
  return op_t<E>{}(x, x);
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

}  // namespace jlt_listing
