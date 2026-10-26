// Negative control for rules/rule2-decided-resolution.query (the tool's
// undecided-resolution check): a call at a type variable to an overload set
// separated by a concept the caller's constraints do not decide is a finding;
// a caller that requires the concept, one that negates it, one whose
// constraints name the call, and a call at a concrete type are not.
// Standalone.
namespace jlt_listing {

template <typename T>
concept Q = requires(const T a) { a; };
template <typename T>
concept C = sizeof(T) == 1;

template <typename T>
  requires Q<T>
constexpr int g(T) {
  return 0;
}
template <typename T>
  requires Q<T> && C<T>
constexpr int g(T) {
  return 1;
}

template <typename T>
  requires Q<T>
constexpr int undecided(T x) {
  return g(x);  // finding 1: g resolves by C, which Q does not decide
}

template <typename T>
  requires Q<T> && C<T>
constexpr int requires_it(T x) {
  return g(x);  // fine: C required
}

template <typename T>
  requires Q<T> && (!C<T>)
constexpr int negates_it(T x) {
  return g(x);  // fine: C negated
}

template <typename T>
concept HasG = requires(const T a) { g(a); };

template <typename T>
  requires HasG<T>
constexpr int named(T x) {
  return g(x);  // fine: the constraint names the call
}

template <typename T>
  requires Q<T>
constexpr int concrete(T) {
  return g(char{});  // fine: no value of parameter type
}

template <typename T>
constexpr int unconstrained(T x) {
  return g(x);  // finding 2: no constraint decides C
}

}  // namespace jlt_listing

template <typename T>
  requires jlt_listing::Q<T>
constexpr int outside(T x) {
  return jlt_listing::g(x);  // outside the scope: not counted
}
