// Negative control for rules/rule1-no-volatile.query: volatile is never a
// Jlt value.  Each planted case inside jlt_listing is labelled; the harness
// outside repeats them and must not count.  Standalone, no imports.
namespace jlt_listing {

struct Register {
  volatile int level = 0;  // no-volatile 1
};

template <typename T>
constexpr int keep(volatile T&& x) {  // no-volatile 2
  return 0;
}

constexpr int peek(const volatile int* p) {  // no-volatile 3
  return p == nullptr;
}

constexpr int fine(const int& x) { return x; }

}  // namespace jlt_listing

inline volatile int harness_register = 0;
