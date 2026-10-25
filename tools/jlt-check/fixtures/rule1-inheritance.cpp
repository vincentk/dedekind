// Negative control for rules/rule1-no-inheritance.query (the tool's
// inheritance check): a class with a base that is not an empty class, or with
// a virtual function, is a finding; an empty base, a tag base and a
// registration deriving from another are not.  Standalone.
#include <type_traits>

namespace jlt_listing {

struct State {
  int x;
};
struct Derived : State {};  // finding 1: the base holds state

struct Shape {
  virtual int area() const { return 0; }  // finding 2: a virtual function
};
struct Square : Shape {  // finding 3: a polymorphic base
  int area() const override { return 1; }
};

struct Tag {};
struct Tagged : Tag {};  // fine: an empty base

template <typename T>
struct is_x : std::false_type {};  // fine: a registration
template <>
struct is_x<int> : std::true_type {};  // fine
template <typename T>
struct Wrap {};
template <typename T>
struct is_x<Wrap<T>> : is_x<T> {};  // fine: a dependent empty base

}  // namespace jlt_listing

struct OutsideBase {
  int y;
};
struct Outside : OutsideBase {};  // outside the scope: not counted
