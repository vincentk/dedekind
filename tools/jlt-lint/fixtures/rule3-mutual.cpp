// Negative control for rules/rule3-no-mutual-recursion.query (the tool's
// mutual-recursion check): functions on a call cycle of length >= 2 are
// findings, one per function; a self-call (another rule's), a fold and plain
// calls are not.  Standalone.
namespace jlt_listing {

constexpr bool is_odd(int n);
constexpr bool is_even(int n) {  // mutual 1: even -> odd -> even
  return n == 0 ? true : is_odd(n - 1);
}
constexpr bool is_odd(int n) {  // mutual 2
  return n == 0 ? false : is_even(n - 1);
}

constexpr int c(int n);
constexpr int a(int n) { return n == 0 ? 0 : c(n - 1); }  // mutual 3: a -> b -> c -> a
constexpr int b(int n) { return a(n); }                    // mutual 4
constexpr int c(int n) { return b(n); }                    // mutual 5

constexpr int self(int n) { return n == 0 ? 0 : self(n - 1); }  // not mutual

constexpr int helper(int n) { return n + 1; }
constexpr int caller(int n) { return helper(helper(n)); }  // fine: no cycle

}  // namespace jlt_listing

inline int h_odd(int n);
inline int h_even(int n) { return n == 0 ? 1 : h_odd(n - 1); }  // outside: not counted
inline int h_odd(int n) { return n == 0 ? 0 : h_even(n - 1); }
