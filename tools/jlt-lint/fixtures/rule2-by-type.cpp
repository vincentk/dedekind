// Negative control for rules/rule2-by-type.query and rules/admitted.query:
// a monoid named by its unit, and three templates against it: one whose body
// uses only the promised operation (admitted), one that uses + (not
// admitted: finding 1), and one under a disjunction no rule reads
// (undecided: finding 2).  Standalone.
#include <concepts>

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

}  // namespace jlt_listing
