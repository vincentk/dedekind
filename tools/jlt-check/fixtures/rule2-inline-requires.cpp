// Negative control for rules/rule2-no-inline-requires.query (the tool's
// inline-requires check): an `if constexpr` whose condition contains a
// requires-expression is a finding, also as an `else if` and under an
// operator; a branch on a named concept, a requires clause and a plain `if`
// are not.  Standalone.
namespace jlt_listing {

template <typename T>
concept HasDomain = requires { typename T::Domain; };

template <typename T>
constexpr int a(const T& t) {
  if constexpr (requires { t.size(); }) return 1;  // finding 1
  return 0;
}

template <typename T>
constexpr int b(const T& t) {
  if constexpr (HasDomain<T>) return 1;                 // fine: a named shape
  else if constexpr (requires { typename T::Codomain; }) return 2;  // finding 2
  return 0;
}

template <typename T>
constexpr int c(const T& t) {
  if constexpr (HasDomain<T> && requires { t.first; }) return 1;  // finding 3
  return 0;
}

template <typename T>
  requires requires(const T& t) { t.size(); }  // fine: declared
constexpr int d(const T& t) {
  if (t.size() > 0) return 1;  // fine: a plain if on a value
  return 0;
}

}  // namespace jlt_listing

template <typename T>
constexpr int outside(const T& t) {
  if constexpr (requires { t.size(); }) return 1;  // outside the scope: not counted
  return 0;
}
