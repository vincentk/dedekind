// Negative control for rule1-no-reference-returns and
// rule1-const-member-functions.  Each planted violation inside jlt_listing is
// labelled; the iterator protocol and the harness outside must not count.
// Standalone, no imports.
namespace jlt_listing {

struct Counter {
  int n = 0;
  constexpr Counter() = default;
  constexpr int count() const { return n; }  // fine
  constexpr void bump() { ++n; }             // const-member 1
  constexpr Counter& reset() {               // const-member 2, reference 1
    n = 0;
    return *this;
  }
  static constexpr int zero() { return 0; }  // fine: static
};

struct Pair {
  int a;
  int b;
};

constexpr const int& first(const Pair& p) {  // reference 2
  return p.a;
}

template <typename P>
constexpr const auto& second(const P& p) {  // reference 3: in a template
  return p.b;
}

constexpr int value_of(const Pair& p) { return p.a; }  // fine: a value

struct Steps {
  struct iterator {  // exempt: the standard iterator protocol
    int i = 0;
    constexpr iterator& operator++() {
      ++i;
      return *this;
    }
    constexpr int operator*() const { return i; }
  };
};

}  // namespace jlt_listing

// Harness: the violations again, outside the namespace, not counted.
struct Harness {
  int n = 0;
  int& get() { return n; }
};
