module;

#include <concepts>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <utility>

export module dedekind.sets:algebra;

import :cardinalities;

import dedekind.ontology;

namespace dedekind::sets {

// --- 1. THE CARRIER (IsSet) ---
// Defines the "Ground" requirements for membership and metadata.
export template <typename S, typename T>
concept IsSet = requires(S s, T v) {
  // Membership interface
  { s.contains(v) } -> std::convertible_to<bool>;
  { s[v] } -> std::convertible_to<bool>;

  // Required Metadata
  typename S::element_type;
  typename S::cardinality_type;
  typename S::base_set_type;

  // Every set must be able to return its parent and its measure
  { s.base_set() } -> std::convertible_to<typename S::base_set_type>;
  { s.cardinality() } -> std::same_as<typename S::cardinality_type>;
};



// Constraints on cardinalities:
export template <typename S, typename T>
concept CountableSet =
    IsSet<S, T> && std::is_base_of_v<Countable, typename S::cardinality_type>;

export template <typename S, typename T>
concept FiniteSet = CountableSet<S, T> &&
                    std::is_base_of_v<Finite, typename S::cardinality_type>;

export template <typename S, typename T>
concept ExtensionalSet =
    FiniteSet<S, T> &&
    std::is_base_of_v<Extensional, typename S::cardinality_type>;

// Theorem: A \ B = A ∩ ¬B
export template <typename L, typename R>
  requires IsSet<L, typename L::element_type> &&
           IsSet<R, typename R::element_type>
auto operator-(L l, R r) {
  return std::move(l) & !std::move(r);
}

export template <typename S, typename P>
auto operator^(S s, P p);

export template <typename L, typename R>
auto operator*(L l, R r);

export template <typename T, IsCardinality Card, typename Derived>
struct SetExpression {
  using element_type = T;
  using cardinality_type = Card;

  const Card card;
  constexpr SetExpression(Card c) : card(std::move(c)) {}

  constexpr Card cardinality() const { return card; }

  // operator[] implemented via CRTP
  bool operator[](const T& v) const {
    return static_cast<const Derived*>(this)->contains(v);
  }
};

// --- Tier 4: The Morphic (Product) ---
export template <typename T, typename U, typename L, typename R,
                 IsCardinality Card>
struct ProductNode : public SetExpression<std::pair<T, U>, Card,
                                          ProductNode<T, U, L, R, Card>> {
  using Parent =
      SetExpression<std::pair<T, U>, Card, ProductNode<T, U, L, R, Card>>;
  using base_set_type =
      ProductNode<T, U, L, R, Card>;  // Products are usually their own base

  L left;
  R right;

  constexpr ProductNode(L l, R r, Card c)
      : Parent(std::move(c)), left(std::move(l)), right(std::move(r)) {}

  // Membership: (a, b) ∈ L × R iff a ∈ L AND b ∈ R
  constexpr bool contains(const std::pair<T, U>& p) const {
    return left.contains(p.first) && right.contains(p.second);
  }

  constexpr const ProductNode& base_set() const { return *this; }
};

export template <typename L, typename R>
auto operator*(L l, R r) {
  using T = typename L::element_type;
  using U = typename R::element_type;

  // Theorem: |L × R| = |L| * |R|
  auto new_card = l.cardinality() * r.cardinality();

  return ProductNode<T, U, L, R, decltype(new_card)>(std::move(l), std::move(r),
                                                     new_card);
}

// --- Tier 2A: The Ground (Identity) ---
export template <typename T, IsCardinality Card, typename Derived>
struct IdentityExpression : public SetExpression<T, Card, Derived> {
  using Parent = SetExpression<T, Card, Derived>;
  using base_set_type = Derived;

  using Parent::Parent;  // Inherit constructor

  // Axiom: An identity set is its own base
  constexpr const Derived& base_set() const {
    return static_cast<const Derived&>(*this);
  }
};

// --- Tier 2B: The Relative (Inductive) ---
export template <typename T, typename Base, IsCardinality Card,
                 typename Derived>
struct RelativeExpression : public SetExpression<T, Card, Derived> {
  using Parent = SetExpression<T, Card, Derived>;
  using base_set_type = Base;

  const Base base;  // Stored by VALUE

  constexpr RelativeExpression(Base b, Card c)
      : Parent(std::move(c)), base(std::move(b)) {}

  constexpr const Base& base_set() const { return base; }
};

// --- THE COMBINATION (Lattice Theory) ---
// Wikipedia: "Lattice (order)" - Supports Meet (&) and Join (|)
export template <typename S, typename T>
concept IsLatticeSet = IsSet<S, T> && requires(S s) {
  { s & s } -> IsSet<T>;
  { s | s } -> IsSet<T>;
};

// --- 3. THE LOGIC (Boolean Algebra) ---
// Wikipedia: "Complemented lattice" - Supports Negation (!) and Difference (-)
export template <typename S, typename T>
concept IsComplementedSet = IsLatticeSet<S, T> && requires(S s) {
  { !s } -> IsSet<T>;
  { s - s } -> IsSet<T>;
  // Theorem: Every complemented set can be reduced to its Normal Form
  { normalize(s) } -> IsSet<T>;
};

// --- 4. THE SPACE (Category Theory) ---
// Wikipedia: "Morphism" / "Cartesian Product" - Supports Product (*)
export template <typename S, typename T>
concept IsMorphicSet = IsSet<S, T> && requires(S s) {
  // Structural Identity: Can it map to itself?
  //{ s.id() } -> IsSet<T>;
  // Cartesian Product with itself?
  { s * s } -> IsSet<typename decltype(s * s)::element_type>;
};

// --- 5. THE INFINITY (Axiomatic Set Theory) ---
// Wikipedia: "Power set" / "Higher-order logic" - Supports Successor (<<)
export template <typename S, typename T>
concept IsHigherOrderSet = IsSet<S, T> && requires(S s) {
  { s.power() } -> IsSet<S>;
};

// Empty and universal sets:

// ø (The Empty Set / Zero of the Lattice)
export template <typename T, IsSet<T> Base>
struct ø : public RelativeExpression<T, Base, Zero, ø<T, Base>> {
  using Parent = RelativeExpression<T, Base, Zero, ø<T, Base>>;

  constexpr ø(Base b) : Parent(std::move(b), Zero()) {}

  // Axiom of Emptiness: No x is in ø
  constexpr bool contains(const T&) const { return false; }
};

// UniversalSet (The Whole / One of the Lattice)
export template <typename T, IsCardinality Card>
struct UniversalSet
    : public IdentityExpression<T, Card, UniversalSet<T, Card>> {
  // The Alias MUST match the Inheritance
  using Parent = IdentityExpression<T, Card, UniversalSet<T, Card>>;

  constexpr UniversalSet(Card c) : Parent(std::move(c)) {}

  // Axiom of Universality: For all x, x is in U
  constexpr bool contains(const T&) const { return true; }
};

// --- 2. THE COMPLEMENT THEOREMS (Overloads) ---

// --- Tier 2C: The Complement (Negation) ---
export template <typename T, typename Base, IsCardinality Card>
struct ComplementNode
    : public RelativeExpression<T, Base, Card, ComplementNode<T, Base, Card>> {
  using Parent =
      RelativeExpression<T, Base, Card, ComplementNode<T, Base, Card>>;

  constexpr ComplementNode(Base b, Card c)
      : Parent(std::move(b), std::move(c)) {}

  // Axiom of Complement: x ∈ ¬A iff x ∉ A
  constexpr bool contains(const T& v) const {
    return !this->base_set().contains(v);
  }
};

// Theorem: !!S -> S (Law of Double Negation)
export template <typename T, typename B, typename C>
auto operator!(const ComplementNode<T, B, C>& c) {
  return c.base;  // Simply "unwrap" the complement
}

// Theorem: !UniversalSet -> ø
export template <typename T, IsCardinality Card>
auto operator!(const UniversalSet<T, Card>& u) {
  return ø<T, UniversalSet<T, Card>>(u);
}

// Theorem: !ø -> UniversalSet
export template <typename T, IsSet<T> Base>
auto operator!(const ø<T, Base>& e) {
  return e.base_set();
}

export template <typename S>
  requires IsSet<S, typename S::element_type>
auto operator!(S s) {
  using T = typename S::element_type;

  // Theorem: |¬A| = |Universe| - |A|
  // (We'll assume your :cardinalities partition can handle this
  // subtraction/estimate)
  auto new_card = s.base_set().cardinality() - s.cardinality();

  return ComplementNode<T, S, decltype(new_card)>(std::move(s), new_card);
}

// --- 1. PRODUCT ELEMENT CONCEPT ---
// Defines what it means to be a "Point" in a Cartesian space.
export template <typename P, typename T, typename U>
concept IsProductElement = requires(P p) {
  { p.first } -> std::convertible_to<T>;
  { p.second } -> std::convertible_to<U>;
};

// --- 6. THE DEDEKIND COMPLETENESS (The Rich Set) ---
// A set that is all of the above: A complete Algebraic System.
export template <typename S, typename T>
concept IsRichSet = IsComplementedSet<S, T> &&
                    IsMorphicSet<S, T>;  // && IsHigherOrderSet<S, T>;

// --- Tier 3: The Binary (Intersection) ---
export template <typename T, typename L, typename R, IsCardinality Card>
struct IntersectionNode
    : public RelativeExpression<T, L, Card, IntersectionNode<T, L, R, Card>> {
  using Parent =
      RelativeExpression<T, L, Card, IntersectionNode<T, L, R, Card>>;

  R right;  // Left is stored in Parent::base

  constexpr IntersectionNode(L l, R r, Card c)
      : Parent(std::move(l), std::move(c)), right(std::move(r)) {}

  // Lattice Axiom: x ∈ (L ∩ R) iff (x ∈ L) AND (x ∈ R)
  constexpr bool contains(const T& v) const {
    return this->base_set().contains(v) && right.contains(v);
  }
};

export template <typename L, typename R>
  requires std::is_same_v<typename L::element_type, typename R::element_type> &&
           ontology::IsSet<L, typename L::element_type> &&
           ontology::IsSet<R, typename R::element_type>
auto operator&(L l, R r) {
  using T = typename L::element_type;

  // Theorem 1: L ∩ ø = ø
  if constexpr (std::is_base_of_v<Zero, typename R::cardinality_type>) {
    return r;  // Returns the Empty Set instance
  }
  // Theorem 2: ø ∩ R = ø
  else if constexpr (std::is_base_of_v<Zero, typename L::cardinality_type>) {
    return l;
  }
  // Theorem 3: Idempotency (A ∩ A = A)
  else if constexpr (std::is_same_v<L, R>) {
    return l;
  }
  // Theorem 4: General Case
  else {
    auto new_card = l.cardinality() & r.cardinality();
    return IntersectionNode<T, L, R, decltype(new_card)>(
        std::move(l), std::move(r), new_card);
  }
}

// --- Tier 3: The Binary (Union) ---
export template <typename T, typename L, typename R, IsCardinality Card>
struct UnionNode
    : public RelativeExpression<T, L, Card, UnionNode<T, L, R, Card>> {
  using Parent = RelativeExpression<T, L, Card, UnionNode<T, L, R, Card>>;

  R right;

  constexpr UnionNode(L l, R r, Card c)
      : Parent(std::move(l), std::move(c)), right(std::move(r)) {}

  // Lattice Axiom: x ∈ (L ∪ R) iff (x ∈ L) OR (x ∈ R)
  constexpr bool contains(const T& v) const {
    return this->base_set().contains(v) || right.contains(v);
  }
};

export template <typename L, typename R>
  requires ontology::IsSet<L, typename L::element_type> &&
           ontology::IsSet<R, typename R::element_type>
auto operator|(L l, R r) {
  // 1. Identity / Equality Proof (A ∪ A = A)
  if constexpr (requires { l == r; }) {
    if (l == r) return l;
  }

  // 2. Subsumption Proof (A ⊆ B => A ∪ B = B)
  // This uses your symbolic operator<= theorems!
  if constexpr (requires { l <= r; }) {
    if (l <= r) return r;
  }
  if constexpr (requires { r <= l; }) {
    if (r <= l) return l;
  }

  // 3. Lattice Identities (Empty/Universe)
  // ... (Already handled by your <= if ø and U are in the hierarchy)

  // 4. General Case
  auto new_card = l.cardinality() | r.cardinality();
  return UnionNode<typename L::element_type, L, R, decltype(new_card)>(
      std::move(l), std::move(r), new_card);
}

// --- Tier 3: The Filter (Predicate) ---
export template <typename T, typename S, typename P, IsCardinality Card>
struct PredicateNode
    : public RelativeExpression<T, S, Card, PredicateNode<T, S, P, Card>> {
  using Parent = RelativeExpression<T, S, Card, PredicateNode<T, S, P, Card>>;

  const P predicate;  // The "Rule" (Lambda, Functor, or Function Pointer)

  constexpr PredicateNode(S s, P p, Card c)
      : Parent(std::move(s), std::move(c)), predicate(std::move(p)) {}

  // Axiom of Specification: x ∈ {y ∈ S | P(y)} iff x ∈ S AND P(x)
  constexpr bool contains(const T& v) const {
    return this->base_set().contains(v) && predicate(v);
  }
};

export template <typename T, typename S,
                 typename P = std::function<bool(const T&)>>
using PredicateSet = PredicateNode<T, S, P, typename S::cardinality_type>;

export template <typename S, typename P>
  requires IsSet<S, typename S::element_type> &&
           std::predicate<P, typename S::element_type>
auto operator^(S s, P p) {
  using T = typename S::element_type;

  // If we are already a PredicateNode, merge the logic
  if constexpr (requires { s.predicate; }) {
    auto merged_p = [p1 = s.predicate, p](const T& v) { return p1(v) && p(v); };
    return PredicateNode<T, typename S::base_set_type, decltype(merged_p),
                         typename S::cardinality_type>(
        s.base_set(), std::move(merged_p), s.cardinality());
  } else {
    return PredicateNode<T, S, P, typename S::cardinality_type>(
        std::move(s), std::move(p), s.cardinality());
  }
}

export template <typename S>
  requires IsSet<S, typename S::element_type>
constexpr auto normalize(const S s) {
  return s;
}

export template <typename T, typename L, typename R, typename C>
constexpr auto normalize(const IntersectionNode<T, L, R, C>& node) {
  return normalize(node.base_set()) & normalize(node.right);
}

export template <typename T, typename L, typename R, typename C>
constexpr auto normalize(const UnionNode<T, L, R, C>& node) {
  return normalize(node.base_set()) | normalize(node.right);
}

// Theorem: !(A & B) -> !A | !B
export template <typename T, typename L, typename R, typename C, typename NC>
constexpr auto normalize(
    const ComplementNode<T, IntersectionNode<T, L, R, C>, NC>& node) {
  auto& inner = node.base_set();
  return normalize(!inner.base_set()) | normalize(!inner.right);
}

// Theorem: !(A | B) -> !A & !B
export template <typename T, typename L, typename R, typename C, typename NC>
constexpr auto normalize(
    const ComplementNode<T, UnionNode<T, L, R, C>, NC>& node) {
  auto& inner = node.base_set();
  // We "push" the negation down: Not (L or R) is (Not L) and (Not R)
  return normalize(!inner.base_set()) & normalize(!inner.right);
}

// Theorem: !!A -> A
export template <typename T, typename S, typename C, typename NC>
constexpr auto normalize(
    const ComplementNode<T, ComplementNode<T, S, C>, NC>& node) {
  return normalize(node.base_set().base_set());
}

/*
export template <typename T>
auto universe() {
    auto c = ℕ<T>::value();
    return UniversalSet<T, decltype(c)>(std::move(c));
}*/

// A Factory for the Boolean Universe
export auto bool_universe() { return UniversalSet<bool, ℕ1>(ℕ1()); }
export auto long_universe() { return UniversalSet<uint64_t, ℕ64>(ℕ64()); }

};  // namespace dedekind::sets
