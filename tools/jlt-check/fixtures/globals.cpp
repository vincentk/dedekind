// Negative control for rules/rule1-no-mutable-globals.query: state that
// outlives a call.  Each planted case inside jlt_listing is labelled; local
// mutation and the harness outside must not count.  Standalone, no imports.
namespace jlt_listing {

int counter = 0;         // no-mutable-globals 1
inline int shared = 0;   // no-mutable-globals 2

struct Stats {
  static int hits;               // no-mutable-globals 3: a static member
  static constexpr int base = 0;  // fine: constexpr
};

constexpr int fine = 1;          // fine: constexpr
const int also_fine = 2;         // fine: const
inline constexpr int too = 3;    // fine: constexpr

constexpr int sum_to(int n) {    // fine: local mutation cannot leak
  int total = 0;
  total += n;
  return total;
}

}  // namespace jlt_listing

int harness_state = 0;  // outside the namespace: not counted
