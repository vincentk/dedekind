module;  // Start of Global Module Fragment

#include <functional>
#include <memory>
#include <variant>

export module dedekind.sets:expressions;

import :traits;
// import std;

namespace dedekind::sets {

// 1. The Handle: The "Smart Value" that the user holds.
// It erases the specific node type but preserves the element type T.
export template <typename T>
class SetHandle {
  std::shared_ptr<const SetTraitBase<T>> ptr;
  AnyCardinality card_val;

 public:
  // Constructor for a specific Node (e.g. IntegerUniverse)
  template <IsCardinality Card>
  SetHandle(std::shared_ptr<const SetTrait<T, Card>> p)
      : ptr(p), card_val(Card{}) {}

  // Constructor for general use (e.g. from operators)
  SetHandle(std::shared_ptr<const SetTraitBase<T>> p, AnyCardinality c)
      : ptr(std::move(p)), card_val(std::move(c)) {}

  // Accessors for symbolic operators
  auto get_ptr() const { return ptr; }

  // Mathy interface
  bool contains(const T& v) const { return ptr->contains(v); }
};

// 2. The Predicate Node: Wraps a "Black Box" lambda
export template <typename T, IsCardinality C>
class PredicateNode : public SetTrait<T, C> {
  std::function<bool(const T&)> logic;

 public:
  PredicateNode(std::function<bool(const T&)> f) : logic(std::move(f)) {}
  bool contains(const T& v) const override { return logic(v); }
};

// 3. The Set Builder Node: Carves a "Sub-space" from a Parent Universe
export template <typename T, IsCardinality C, typename Predicate>
class SetBuilderNode : public SetTrait<T, C> {
  std::shared_ptr<const SetTrait<T, C>> parent;
  Predicate filter;

 public:
  SetBuilderNode(std::shared_ptr<const SetTrait<T, C>> p, Predicate f)
      : parent(std::move(p)), filter(std::move(f)) {}

  bool contains(const T& v) const override {
    // Logic: x is in S iff x is in Parent AND x satisfies Filter
    return parent->contains(v) && filter(v);
  }
};

// 5. Symbolic Intersection Node
export template <typename T, IsCardinality C>
class IntersectionNode : public SetTrait<T, C> {
  std::shared_ptr<const SetTrait<T, C>> left;
  std::shared_ptr<const SetTrait<T, C>> right;

 public:
  IntersectionNode(std::shared_ptr<const SetTrait<T, C>> l,
                   std::shared_ptr<const SetTrait<T, C>> r)
      : left(std::move(l)), right(std::move(r)) {}

  bool contains(const T& v) const override {
    return left->contains(v) && right->contains(v);
  }

  // Only available if the Resulting Cardinality Card is Extensional
  std::size_t size() const
    requires std::is_base_of_v<Extensional, C>
  {
    // Optimization: Resulting size is at most the smaller of the parents
    return std::min(left->size(), right->size());
  }
};

// 6. Symbolic Complement Node
export template <typename T, IsCardinality C>
class ComplementNode : public SetTrait<T, C> {
  std::shared_ptr<const SetTrait<T, C>> inner;

 public:
  ComplementNode(std::shared_ptr<const SetTrait<T, C>> i)
      : inner(std::move(i)) {}

  bool contains(const T& v) const override { return !inner->contains(v); }
};
}  // namespace dedekind::sets
