// Negative control for rules/rule2-decided-resolution.query (the tool's
// undecided-resolution check): a call at a type variable to an overload set
// separated by a concept the caller's constraints do not decide is a finding;
// so are the complementary chain, a constraint on another parameter and a
// disjunction; a caller that requires the concept (directly or through a
// concept), one that negates it, one whose constraints name the call, a call
// at a concrete type and an overload of another arity are not.
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

// The complementary chain: the two branches differ only in polarity.
template <typename T>
  requires Q<T> && C<T>
constexpr int k(T) {
  return 1;
}
template <typename T>
  requires Q<T> && (!C<T>)
constexpr int k(T) {
  return 0;
}
template <typename T>
  requires Q<T>
constexpr int chain(T x) {
  return k(x);  // finding 3: k resolves by C, which Q does not decide
}

// Different arity: the two-argument overload cannot take one argument.
template <typename T>
  requires Q<T>
constexpr int h(T) {
  return 0;
}
template <typename T>
  requires Q<T> && C<T>
constexpr int h(T, T) {
  return 1;
}
template <typename T>
  requires Q<T>
constexpr int arity(T x) {
  return h(x);  // fine: one viable candidate
}

// C on another parameter decides nothing about T.
template <typename T, typename U>
  requires Q<T> && C<U>
constexpr int unrelated(T x, U) {
  return g(x);  // finding 4: C<U> is not C<T>
}

// A disjunction guarantees neither side.
template <typename T>
concept D = sizeof(T) == 2;
template <typename T>
  requires Q<T> && (C<T> || D<T>)
constexpr int disjunction(T x) {
  return g(x);  // finding 5: C<T> || D<T> does not decide C<T>
}

// A concept that requires C, read through.
template <typename T>
concept QC = Q<T> && C<T>;
template <typename T>
  requires QC<T>
constexpr int nested(T x) {
  return g(x);  // fine: QC<T> guarantees C<T>
}

// A binary concept: the caller's fact must be about the same arguments.
template <typename T, typename U>
concept EqualSize = sizeof(T) == sizeof(U);
template <typename T, typename U>
  requires Q<T>
constexpr int e(T, U) {
  return 0;
}
template <typename T, typename U>
  requires Q<T> && EqualSize<T, U>
constexpr int e(T, U) {
  return 1;
}
template <typename T, typename U>
  requires Q<T> && EqualSize<T, U>
constexpr int same_pair(T x, U y) {
  return e(x, y);  // fine: EqualSize<T, U> required
}
template <typename T, typename U>
  requires Q<T> && EqualSize<T, T>
constexpr int other_pair(T x, U y) {
  return e(x, y);  // finding 6: EqualSize<T, T> is not EqualSize<T, U>
}

// A class template's constrained partial specialisation decides for its
// members.
template <typename T>
struct W {};
template <typename T>
  requires Q<T> && C<T>
struct W<T> {
  template <typename V>
  static constexpr int m(T x, V) {
    return g(x);  // fine: the specialisation guarantees C<T>
  }
};

// A candidate the caller's facts contradict is no candidate.
template <typename T>
  requires Q<T>
constexpr int c3(T) {
  return 0;
}
template <typename T>
  requires Q<T> && C<T> && D<T>
constexpr int c3(T) {
  return 1;
}
template <typename T>
  requires Q<T> && (!C<T>)
constexpr int contradicted(T x) {
  return c3(x);  // fine: !C<T> rules out the second c3, D is moot
}

}  // namespace jlt_listing

template <typename T>
  requires jlt_listing::Q<T>
constexpr int outside(T x) {
  return jlt_listing::g(x);  // outside the scope: not counted
}
