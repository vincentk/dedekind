module;

#include <concepts>
#include <ranges>
#include <type_traits>
#include <variant>

export module dedekind.sets:algebra;

// C++23 feature, disabled for now:
// import std;

namespace dedekind::sets {

// 1. The absolute base (no cardinality template)
export template <typename T>
struct SetTraitBase {
  virtual ~SetTraitBase() = default;
  virtual bool contains(const T& value) const = 0;
  virtual AnyCardinality get_cardinality() const = 0;
};

// 2. The specialized trait (inherited by nodes)
export template <typename T, IsCardinality Card>
struct SetTrait : public SetTraitBase<T> {
  using cardinality_type = Card;

  // This is the ONLY place that knows the static 'Card' tag.
  // We implement it here so the leaf nodes (Universes) get it for free.
  AnyCardinality get_cardinality() const override { return Card{}; }
};

// 1. Base Set: The "Uncountable" baseline (Membership Only)
export template <typename S, typename T>
concept Set = requires(S s, T v) {
  { s.contains(v) } -> std::convertible_to<bool>;
};

// 2. Countable Set: Can be enumerated (Generator available)
export template <typename S>
concept CountableSet =
    Set<S, typename S::element_type> &&
    std::is_base_of_v<Countable, typename S::cardinality_type> &&
    requires(S s) {
      { s.elements() } -> std::ranges::input_range;
    };

// 3. Finite Set: Enumerable AND guaranteed to terminate
export template <typename S>
concept FiniteSet =
    CountableSet<S> && std::is_base_of_v<Finite, typename S::cardinality_type>;

// 4. Extensional Set: Finite AND size fits in memory (O(1) size check)
export template <typename S>
concept ExtensionalSet =
    FiniteSet<S> &&
    std::is_base_of_v<Extensional, typename S::cardinality_type> &&
    requires(S s) {
      { s.size() } -> std::same_as<std::size_t>;
    };

export template <typename T, IsCardinality C, typename Derived>
class SetExpression : public SetTrait<T, C>,
                      public std::enable_shared_from_this<Derived> {
 public:
  auto self_ptr() const {
    return static_cast<const Derived*>(this)->shared_from_this();
  }
};

// A simple alias for our predicate type
export template <typename T>
using Predicate = std::function<bool(const T&)>;

// A "Theorem" for subset: We only implement what we can prove.
template <typename L, typename R>
constexpr bool operator<=(const L& lhs, const R& rhs) {
  // 1. Structural Proof: A set is always a subset of its Universe
  if constexpr (std::is_same_v<typename L::universe_type, R>) {
    return true;
  }

  // 2. Identity Proof: Empty set is a subset of everything
  if constexpr (std::is_same_v<typename L::cardinality_type, Empty>) {
    return true;
  }

  // 3. Fallback: If we don't have a specific theorem for these types,
  // we don't provide a definition, leading to a Compile Error.
  // This is Wadler's "Theorems for Free" in action.
}

// --- The Power Set Node ---

template <Set S>
struct PowerSetNode {
  using element_type = S;
  // Calculate the new cardinality type using the operator<<
  using cardinality_type =
      decltype(2u << std::declval<typename S::cardinality_type>());

  S base_set;
  cardinality_type card;

  constexpr PowerSetNode(S s)
      : base_set(std::move(s)), card(2u << base_set.card) {}

  // Membership: candidate is in P(S) if candidate <= base_set
  template <Set T>
  bool contains(const T& candidate) const {
    // This will only compile if an operator<= exists for these specific sets!
    return candidate <= base_set;
  }
};

// Global "Power Set" factory
template <Set S>
auto power_set(S s) {
  return PowerSetNode<S>(std::move(s));
}

}  // namespace dedekind::sets
