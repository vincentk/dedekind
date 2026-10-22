// Negative control for the rule1-boxed-*.query matchers: a shared_ptr is a
// Jlt value when its pointee is const and nothing observes which box it is.
// Each planted violation inside jlt_listing is labelled; the harness outside
// repeats them and must not count.  Standalone, no imports.
#include <memory>

namespace jlt_listing {

struct Leaf {
  int v = 0;
};

struct Node {
  std::shared_ptr<const Leaf> leg;      // a boxed value: fine
  std::shared_ptr<Leaf> mutable_leg;    // boxed-mutable-pointee 1
};

template <typename T>
struct Tree {
  std::shared_ptr<const Tree> fine;  // a boxed value in a template: fine
  std::shared_ptr<T> leg;            // boxed-mutable-pointee 3: dependent
  std::shared_ptr<const T> cleg;     // fine: dependent, const
};

inline std::shared_ptr<const int> boxed() {
  return std::make_shared<const int>(1);  // fine
}

inline std::shared_ptr<int> unboxed() {
  return std::make_shared<int>(1);  // boxed-mutable-pointee 2
}

inline bool same_box(const std::shared_ptr<const int>& p,
                     const std::shared_ptr<const int>& q) {
  return p == q;  // boxed-identity 1
}

inline const int* address(const std::shared_ptr<const int>& p) {
  return p.get();  // boxed-identity 2
}

inline long sharers(const std::shared_ptr<const int>& p) {
  return p.use_count();  // boxed-identity 3
}

template <typename T>
long sharers_of(const std::shared_ptr<const T>& p) {
  return p.use_count();  // boxed-identity 4: dependent member
}

template <typename T>
bool same_box_of(const std::shared_ptr<const T>& p,
                 const std::shared_ptr<const T>& q) {
  return p == q;  // boxed-identity 5: dependent equality
}

template <typename T>
bool empty_of(const std::shared_ptr<const T>& p) {
  return p == nullptr;  // fine: dependent, but asks only for emptiness
}

inline bool empty(const std::shared_ptr<const int>& p) {
  return p == nullptr;  // fine: asks whether the box is empty
}

inline int read(const std::shared_ptr<const int>& p) {
  return *p;  // fine: reads the value
}

}  // namespace jlt_listing

// Harness: the violations again, outside the namespace, not counted.
inline long harness() {
  auto p = std::make_shared<int>(1);
  return p.use_count() + (p.get() != nullptr);
}
