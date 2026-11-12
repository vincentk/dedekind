/**
 * @file dedekind/sets/expr.cppm
 * @partition :expr
 * @brief μF at the value level: the term algebras ℕ, ℤ, ℚ as one type each,
 *        atoms of machine words with an AST layered on top.
 *
 * @details A natural number is not a bit sequence with arithmetic layered on
 * top; it is atomic machine words with a term layered on top, and the term
 * has a canonical form that gives equality and order.  The typed terms of
 * @c :terms (@c Const, @c BinOp) carry this where a shape is static: a
 * template argument, a @c static_assert.  Where values must share one type
 * (a span, a struct member, reassigned state), the term is a value of
 * @c Expr<S, N>: the initial algebra μF of the sort's term functor, stored as
 * a fixed array of nodes, each a @c std::variant over the node kinds and its
 * children referred to by index, every child before its parent.  A fold over
 * a term is then a fold over the array in index order, bounded by its
 * capacity @c N (Rule 3 by construction); a term is read by @c std::visit,
 * the case form.
 *
 * @c canonical rewrites a term to its canonical form, unique per value, and
 * @c == and @c <=> are equality and order of canonical forms.  The normal
 * form a canonical form is read back from (@c Numeral's limbs, here) is the
 * internal machinery of @c canonical, never a value a program holds.  The
 * normalisation point is always explicit: a computation that iterates calls
 * @c canonical where it stores, and nothing converts a term to its canonical
 * form implicitly.
 *
 * @c node(t) is the bridge T-Node (the Jlt paper's Appendix C,
 * @f$\mathit{node}_O : O\langle\bar\tau\rangle \to \mu F@f$): the injection
 * of a typed term into μF, the fold of its type.
 *
 * The capacity is the declared bound of a term, a template parameter, so a
 * site that needs more says so.  Exceeding it is a checked precondition, never
 * undefined: refused by constant evaluation, a deterministic trap at run time.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <algorithm>  // std::ranges::transform: one copy per join
#include <array>
#include <bit>  // std::bit_cast: a double's bits, exactly
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>  // std::plus / std::multiplies, the laws' operations
#include <limits>
#include <optional>
#include <ranges>  // std::views::iota: a term's nodes walked as a fold
#include <type_traits>
#include <utility>
#include <variant>  // the node kinds: a term is read by std::visit
#include <vector>   // a numeral's limbs, written back as a term

export module dedekind.sets:expr;

import dedekind.category;
import :boundaries;  // 𝔸: ℕ, the universe over its terms
import :cardinality;
import :terms;

namespace dedekind::sets {

/** @section expr__Sorts_And_Nodes  The sorts and their node kinds
 *  The sort fixes the node kinds by type: ℕ has atoms, sums and products, so
 *  a natural number's term has no subtraction by construction; ℤ adds the
 *  negation, ℚ the quotient. */
/** @brief The sort of the natural numbers. */
export struct NatSort {};
/** @brief The sort of the integers. */
export struct IntSort {};
/** @brief The sort of the rationals. */
export struct RatSort {};

/** @brief The node kinds: an atom is a machine word, and an operation refers
 *  to its operands by their index in the term's node array. */
export namespace nodes {
/** @brief A machine word, the one atom kind. */
struct Atom {
  std::uint64_t value;
  constexpr bool operator==(const Atom&) const = default;
};
/** @brief The sum of the nodes at @c a and @c b. */
struct Add {
  std::uint32_t a;
  std::uint32_t b;
  constexpr bool operator==(const Add&) const = default;
};
/** @brief The product of the nodes at @c a and @c b. */
struct Mul {
  std::uint32_t a;
  std::uint32_t b;
  constexpr bool operator==(const Mul&) const = default;
};
/** @brief The negation of the node at @c a (ℤ, ℚ). */
struct Neg {
  std::uint32_t a;
  constexpr bool operator==(const Neg&) const = default;
};
/** @brief The quotient of the node at @c a by the node at @c b (ℚ). */
struct Div {
  std::uint32_t a;
  std::uint32_t b;
  constexpr bool operator==(const Div&) const = default;
};
}  // namespace nodes

/** @brief The node kinds of a sort.  @tparam S the sort. */
export template <typename S>
struct node_kinds;
template <>
struct node_kinds<NatSort> {
  using type = std::variant<nodes::Atom, nodes::Add, nodes::Mul>;
};
template <>
struct node_kinds<IntSort> {
  using type = std::variant<nodes::Atom, nodes::Add, nodes::Mul, nodes::Neg>;
};
template <>
struct node_kinds<RatSort> {
  using type =
      std::variant<nodes::Atom, nodes::Add, nodes::Mul, nodes::Neg, nodes::Div>;
};
/** @brief A sort: one whose node kinds are stated.  @tparam S the sort. */
export template <typename S>
concept IsSort = requires { typename node_kinds<S>::type; };

/** @brief The default capacity of a term: its declared bound. */
export inline constexpr std::size_t default_capacity = 64;

/** @section expr__Expr  A term of a sort, as a value */
export template <IsSort S, std::size_t N>
  requires(N >= 1 && N <= std::numeric_limits<std::uint32_t>::max())
class Expr;
/** @brief Declared here for the comparisons against a floating atom; defined
 *  in @ref expr__Floating_Atoms. */
export template <std::size_t N = default_capacity>
constexpr std::optional<Expr<RatSort, N>> from(double x);

namespace detail_expr {
/** @brief The value of a certified check, or a deterministic trap where the
 *  check failed: a precondition, refused by constant evaluation.  For a check
 *  that cannot fail (a canonical form's denominator), it is consumed, not
 *  performed.  @param o the check's result. */
template <typename T>
constexpr T certified(const std::optional<T>& o) {
  if (!o) __builtin_trap();  // the check refused
  return *o;
}
}  // namespace detail_expr

/**
 * @class Expr
 * @brief A term of the sort @c S with at most @c N nodes: μF as a value.
 * @details The nodes are stored in order, every child before its parent, the
 * root last.  A term is built from atoms and the sort's operations; two terms
 * are equal when their canonical forms are.
 * @tparam S the sort.  @tparam N the capacity, the declared bound.
 */
export template <IsSort S, std::size_t N = default_capacity>
  requires(N >= 1 && N <= std::numeric_limits<std::uint32_t>::max())
class Expr {
 public:
  using sort = S;
  using node_type = typename node_kinds<S>::type;
  static constexpr std::size_t capacity = N;
  /** @brief ℕ, ℤ and ℚ are countable; @c :order's halfspaces read this. */
  using cardinality_type = ℵ_0;

  /** @brief The atom @c 0. */
  constexpr Expr() = default;
  /** @brief A machine integer, by the total embedding: a word is an atom; a
   *  negative integer, in ℤ and ℚ, the negation of its magnitude's atom.  The
   *  embedding is a declared arrow, so the conversion is its sugar.
   *  @tparam K the integer's type.  @param k the integer. */
  template <std::integral K>
    requires(!std::same_as<K, bool> &&
             (std::unsigned_integral<K> || !std::same_as<S, NatSort>))
  constexpr Expr(K k)  // NOLINT(google-explicit-constructor)
      : Expr(embedded(k)) {}
  /** @brief A literal fraction @f$n/d@f$, in ℚ: the check into ℚ* refuses a
   *  zero @c d, at compile time, the only time a @c consteval runs.
   *  @param n the numerator.  @param d the denominator. */
  template <std::integral A, std::integral B>
    requires(std::same_as<S, RatSort> && !std::same_as<A, bool> &&
             !std::same_as<B, bool>)
  consteval Expr(A n, B d)
      : Expr(Expr(n) /
             detail_expr::certified(dedekind::category::nonzero(Expr(d)))) {}
  /** @brief The atom @c w, a machine word.  @param w the word. */
  static constexpr Expr atom(std::uint64_t w) {
    Expr e;
    e.nodes_[0] = nodes::Atom{w};
    return e;
  }

  /** @return the number of nodes. */
  constexpr std::size_t size() const { return size_; }
  /** @return the node at @c i, by value.  @param i the index. */
  constexpr node_type at(std::size_t i) const { return nodes_[i]; }
  /** @return the index of the root. */
  constexpr std::uint32_t root() const {
    return static_cast<std::uint32_t>(size_ - 1);
  }

  /** @brief The term with the node @c n appended as its new root.  The
   *  capacity is a checked precondition: refused by constant evaluation, a
   *  deterministic trap at run time.  @param n the node, its operands already
   *  in the term. */
  constexpr Expr appended(node_type n) const {
    if (size_ >= N) __builtin_trap();  // the declared bound, exceeded
    Expr e = *this;
    e.nodes_[size_] = n;
    e.size_ = size_ + 1;
    return e;
  }

  /** @brief Equality of terms: equality of their canonical forms. */
  friend constexpr bool operator==(const Expr& x, const Expr& y) {
    return order_of(x, y) == 0;
  }
  /** @brief The order of terms: the order of their canonical forms. */
  friend constexpr std::strong_ordering operator<=>(const Expr& x,
                                                    const Expr& y) {
    return order_of(x, y);
  }
  /** @return @f$n \cdot q = \iota(n)\, q@f$: ℤ acts on ℚ through its
   *  embedding, which makes ℚ a ℤ-module (Lang, @e Algebra, §III.1).
   *  @param n the integer.  @param q the rational. */
  template <IsSort T>
    requires(std::same_as<S, RatSort> && std::same_as<T, IntSort>)
  friend constexpr Expr operator*(const Expr<T, N>& n, const Expr& q) {
    return widened(n) * q;
  }
  /** @brief Order and equality against a floating atom go through ℚ: the
   *  double is read exactly, and C++'s own conversion of the integer side to
   *  @c double ([expr.arith.conv]), which rounds above @f$2^{53}@f$, is never
   *  taken.  NaN is unordered and equal to nothing.  Hidden friends, found by
   *  argument-dependent lookup only, so no other overload set sees them. */
  template <std::floating_point F>
    requires(std::same_as<S, RatSort> &&
             (std::same_as<F, double> || std::same_as<F, float>))
  friend constexpr bool operator==(const Expr& q, F x) {
    const auto r = from<N>(static_cast<double>(x));  // a float is a double
    return r && *r == q;
  }
  template <std::floating_point F>
    requires(std::same_as<S, RatSort> &&
             (std::same_as<F, double> || std::same_as<F, float>))
  friend constexpr std::partial_ordering operator<=>(const Expr& q, F x) {
    if (const auto r = from<N>(static_cast<double>(x))) return q <=> *r;
    return std::partial_ordering::unordered;
  }

  /** @brief The sum, a term. */
  friend constexpr Expr operator+(const Expr& x, const Expr& y) {
    return joined(x, y, MakeAdd{});
  }
  /** @brief The product, a term. */
  friend constexpr Expr operator*(const Expr& x, const Expr& y) {
    return joined(x, y, MakeMul{});
  }
  /** @brief The negation, a term (ℤ, ℚ). */
  friend constexpr Expr operator-(const Expr& x)
    requires(!std::same_as<S, NatSort>)
  {
    return x.appended(nodes::Neg{x.root()});
  }
  /** @brief The difference, @c x + (−y), a term (ℤ, ℚ). */
  friend constexpr Expr operator-(const Expr& x, const Expr& y)
    requires(!std::same_as<S, NatSort>)
  {
    return x + (-y);
  }
  /** @brief The quotient by a certified divisor, a term (ℚ): division is total
   *  on ℚ × ℚ*, and there is no division by a term of ℚ. */
  friend constexpr Expr operator/(const Expr& x,
                                  const dedekind::category::NonZero<Expr>& d)
    requires std::same_as<S, RatSort>
  {
    return joined(x, d.value(), MakeDiv{});
  }

  /** @brief The term with every node of @c e, of a smaller sort, kept: the
   *  embeddings ℕ ↪ ℤ ↪ ℚ, a node of the smaller sort being one of this.
   *  @tparam T the smaller sort.  @param e the term. */
  template <IsSort T>
  static constexpr Expr widened(const Expr<T, N>& e) {
    return dedekind::category::fold(std::views::iota(std::size_t{1}, e.size()),
                                    Expr::atom(std::visit(AtomOf{}, e.at(0))),
                                    Widen<T>{e});
  }

 private:
  template <std::integral K>
  static constexpr Expr embedded(K k) {
    if constexpr (std::signed_integral<K>) {
      if (k < K{}) {
        const auto magnitude =
            std::uint64_t{0} -
            static_cast<std::uint64_t>(static_cast<std::int64_t>(k));
        return atom(magnitude).appended(nodes::Neg{0});
      }
    }
    return atom(static_cast<std::uint64_t>(k));
  }

  /** @brief The term @c a with the nodes of @c b after it, their operands
   *  shifted, and @c combine(root of a, root of b) on top. */
  template <typename Combine>
    requires std::regular_invocable<const Combine&, std::uint32_t,
                                    std::uint32_t>
  static constexpr Expr joined(const Expr& a, const Expr& b,
                               const Combine& combine) {
    const auto offset = static_cast<std::uint32_t>(a.size_);
    if (a.size_ + b.size_ >= N) __builtin_trap();  // the declared bound
    // One copy of the term, not one per node: b's nodes are written in place.
    Expr e = a;
    std::ranges::transform(
        b.nodes_.begin(),
        b.nodes_.begin() + static_cast<std::ptrdiff_t>(b.size_),
        e.nodes_.begin() + static_cast<std::ptrdiff_t>(offset),
        ShiftNode{offset});
    e.nodes_[a.size_ + b.size_] = combine(a.root(), offset + b.root());
    e.size_ = a.size_ + b.size_ + 1;
    return e;
  }

  struct MakeAdd {
    constexpr node_type operator()(std::uint32_t a, std::uint32_t b) const {
      return nodes::Add{a, b};
    }
  };
  struct MakeMul {
    constexpr node_type operator()(std::uint32_t a, std::uint32_t b) const {
      return nodes::Mul{a, b};
    }
  };
  struct MakeDiv {
    constexpr node_type operator()(std::uint32_t a, std::uint32_t b) const {
      return nodes::Div{a, b};
    }
  };
  /** @brief A node with its operands' indices shifted by @c by. */
  struct Shift {
    std::uint32_t by;
    constexpr node_type operator()(nodes::Atom n) const { return n; }
    constexpr node_type operator()(nodes::Add n) const {
      return nodes::Add{n.a + by, n.b + by};
    }
    constexpr node_type operator()(nodes::Mul n) const {
      return nodes::Mul{n.a + by, n.b + by};
    }
    constexpr node_type operator()(nodes::Neg n) const {
      return nodes::Neg{n.a + by};
    }
    constexpr node_type operator()(nodes::Div n) const {
      return nodes::Div{n.a + by, n.b + by};
    }
  };
  /** @brief A node of @c joined's right operand, its operands shifted. */
  struct ShiftNode {
    std::uint32_t by;
    constexpr node_type operator()(const node_type& n) const {
      return std::visit(Shift{by}, n);
    }
  };
  /** @brief The word of the first node, always an atom: children come before
   *  their parents. */
  struct AtomOf {
    constexpr std::uint64_t operator()(nodes::Atom n) const { return n.value; }
    constexpr std::uint64_t operator()(nodes::Add) const { __builtin_trap(); }
    constexpr std::uint64_t operator()(nodes::Mul) const { __builtin_trap(); }
    constexpr std::uint64_t operator()(nodes::Neg) const { __builtin_trap(); }
    constexpr std::uint64_t operator()(nodes::Div) const { __builtin_trap(); }
  };
  /** @brief One step of @c widened: the node at @c i, as one of this sort. */
  template <IsSort T>
  struct Widen {
    Expr<T, N> from;
    constexpr Expr operator()(const Expr& acc, std::size_t i) const {
      return acc.appended(std::visit(Same{}, from.at(i)));
    }
  };
  /** @brief A node, unchanged, in this sort's variant. */
  struct Same {
    constexpr node_type operator()(nodes::Atom n) const { return n; }
    constexpr node_type operator()(nodes::Add n) const { return n; }
    constexpr node_type operator()(nodes::Mul n) const { return n; }
    constexpr node_type operator()(nodes::Neg n) const { return n; }
    constexpr node_type operator()(nodes::Div n) const { return n; }
  };

  std::array<node_type, N> nodes_{};
  std::size_t size_{1};
};

/** @brief A natural number as a value: a term of ℕ.  @tparam N the capacity.
 */
export template <std::size_t N = default_capacity>
using NatExpr = Expr<NatSort, N>;
/** @brief An integer as a value: a term of ℤ.  @tparam N the capacity. */
export template <std::size_t N = default_capacity>
using IntExpr = Expr<IntSort, N>;
/** @brief A rational as a value: a term of ℚ.  @tparam N the capacity. */
export template <std::size_t N = default_capacity>
using RatExpr = Expr<RatSort, N>;

/** @section expr__Reading  The values canonical forms are read back from
 *  @c Numeral is the machinery @c canonical computes with: ℕ's value of a term,
 *  limb by limb.  ℤ's is the Grothendieck pair @f$(p, n)@f$, meaning
 *  @f$p - n@f$, which needs only @c + and @c × of naturals; ℚ's a fraction of
 *  two such pairs.  None of them is a value a program holds. */
namespace detail_expr {
/** @brief @f$p - n@f$: a node of ℤ read in ℕ × ℕ. */
struct Pair {
  Numeral p;
  Numeral n;
};
constexpr Pair add(const Pair& x, const Pair& y) {
  return {x.p + y.p, x.n + y.n};
}
constexpr Pair mul(const Pair& x, const Pair& y) {
  return {x.p * y.p + x.n * y.n, x.p * y.n + x.n * y.p};
}
constexpr Pair neg(const Pair& x) { return {x.n, x.p}; }
/** @brief The order of ℤ on pairs: @f$p - n \lessgtr p' - n'@f$ iff
 *  @f$p + n' \lessgtr p' + n@f$. */
constexpr std::strong_ordering order(const Pair& x, const Pair& y) {
  return x.p + y.n <=> y.p + x.n;
}
/** @brief @f$a / b@f$: a node of ℚ read as a fraction of pairs, @c b never
 *  zero (every divisor is certified). */
struct Frac {
  Pair num;
  Pair den;
};

struct EvalNat {
  const std::vector<Numeral>& v;
  constexpr Numeral operator()(nodes::Atom n) const { return Numeral{n.value}; }
  constexpr Numeral operator()(nodes::Add n) const { return v[n.a] + v[n.b]; }
  constexpr Numeral operator()(nodes::Mul n) const { return v[n.a] * v[n.b]; }
};
struct EvalInt {
  const std::vector<Pair>& v;
  constexpr Pair operator()(nodes::Atom n) const {
    return {Numeral{n.value}, Numeral{}};
  }
  constexpr Pair operator()(nodes::Add n) const { return add(v[n.a], v[n.b]); }
  constexpr Pair operator()(nodes::Mul n) const { return mul(v[n.a], v[n.b]); }
  constexpr Pair operator()(nodes::Neg n) const { return neg(v[n.a]); }
};
struct EvalRat {
  const std::vector<Frac>& v;
  constexpr Frac operator()(nodes::Atom n) const {
    return {{Numeral{n.value}, Numeral{}}, {Numeral{1u}, Numeral{}}};
  }
  constexpr Frac operator()(nodes::Add n) const {
    const Frac& a = v[n.a];
    const Frac& b = v[n.b];
    return {add(mul(a.num, b.den), mul(b.num, a.den)), mul(a.den, b.den)};
  }
  constexpr Frac operator()(nodes::Mul n) const {
    return {mul(v[n.a].num, v[n.b].num), mul(v[n.a].den, v[n.b].den)};
  }
  constexpr Frac operator()(nodes::Neg n) const {
    return {neg(v[n.a].num), v[n.a].den};
  }
  constexpr Frac operator()(nodes::Div n) const {
    return {mul(v[n.a].num, v[n.b].den), mul(v[n.a].den, v[n.b].num)};
  }
};
/** @brief The values of a term's nodes, one per node it has (not per node it
 *  could hold), so a fold step moves them in O(1).  @tparam V the value
 *  type. */
template <typename V>
struct Values {
  std::vector<V> v;
};
template <std::size_t N>
struct NatStep {
  const NatExpr<N>& e;
  constexpr Values<Numeral> operator()(Values<Numeral> s, std::size_t i) const {
    s.v[i] = std::visit(EvalNat{s.v}, e.at(i));
    return s;
  }
};
template <std::size_t N>
struct IntStep {
  const IntExpr<N>& e;
  constexpr Values<Pair> operator()(Values<Pair> s, std::size_t i) const {
    s.v[i] = std::visit(EvalInt{s.v}, e.at(i));
    return s;
  }
};
template <std::size_t N>
struct RatStep {
  const RatExpr<N>& e;
  constexpr Values<Frac> operator()(Values<Frac> s, std::size_t i) const {
    s.v[i] = std::visit(EvalRat{s.v}, e.at(i));
    return s;
  }
};
}  // namespace detail_expr

/** @return a term's value, the normal form its canonical form is read back
 *  from: internal to @c canonical, @c == and @c <=>.  @param e the term. */
template <std::size_t N>
constexpr Numeral value_of(const NatExpr<N>& e) {
  return dedekind::category::fold(
             std::views::iota(std::size_t{0}, e.size()),
             detail_expr::Values<Numeral>{std::vector<Numeral>(e.size())},
             detail_expr::NatStep<N>{e})
      .v[e.root()];
}
template <std::size_t N>
constexpr detail_expr::Pair value_of(const IntExpr<N>& e) {
  return dedekind::category::fold(std::views::iota(std::size_t{0}, e.size()),
                                  detail_expr::Values<detail_expr::Pair>{
                                      std::vector<detail_expr::Pair>(e.size())},
                                  detail_expr::IntStep<N>{e})
      .v[e.root()];
}
template <std::size_t N>
constexpr detail_expr::Frac value_of(const RatExpr<N>& e) {
  return dedekind::category::fold(std::views::iota(std::size_t{0}, e.size()),
                                  detail_expr::Values<detail_expr::Frac>{
                                      std::vector<detail_expr::Frac>(e.size())},
                                  detail_expr::RatStep<N>{e})
      .v[e.root()];
}

/** @return the order of two terms' canonical forms, read off their values:
 *  ℕ's directly; ℤ's on pairs; ℚ's by cross-multiplication, its sign that of
 *  @f$(ad - cb) \cdot bd@f$.  @param x a term.  @param y a term. */
template <std::size_t N>
constexpr std::strong_ordering order_of(const NatExpr<N>& x,
                                        const NatExpr<N>& y) {
  return value_of(x) <=> value_of(y);
}
template <std::size_t N>
constexpr std::strong_ordering order_of(const IntExpr<N>& x,
                                        const IntExpr<N>& y) {
  return detail_expr::order(value_of(x), value_of(y));
}
template <std::size_t N>
constexpr std::strong_ordering order_of(const RatExpr<N>& x,
                                        const RatExpr<N>& y) {
  using namespace detail_expr;
  const Frac a = value_of(x);
  const Frac b = value_of(y);
  const Pair zero{};
  const std::strong_ordering d = order(mul(a.num, b.den), mul(b.num, a.den));
  return order(mul(a.den, b.den), zero) < 0 ? 0 <=> d : d;
}

/** @section expr__Canonical  The canonical forms, written back as terms */
namespace detail_expr {
/** @brief One step of writing a natural back: Horner, @c acc·B + l_k, the
 *  limbs from the top, @c B = 2^64 the node at index 1.  @tparam S @tparam N
 *  the sort and the capacity of the term written into. */
template <IsSort S, std::size_t N>
struct HornerStep {
  using E = Expr<S, N>;
  std::vector<Numeral::limb> limbs;  // little-endian
  constexpr E operator()(const E& acc, std::size_t i) const {
    const E shifted = acc.appended(nodes::Mul{acc.root(), 1});
    const E with = shifted.appended(nodes::Atom{limbs[limbs.size() - 2 - i]});
    return with.appended(nodes::Add{shifted.root(), with.root()});
  }
};
/** @return the canonical term of the natural @c n, in the sort of @c E: one
 *  atom where it fits a word, else Horner over @f$B = 2^{32} \cdot 2^{32}@f$
 *  with @c B shared.  @param n the natural. */
template <IsSort S, std::size_t N>
constexpr Expr<S, N> natural(const Numeral& n) {
  using E = Expr<S, N>;
  if (n.limb_count() == 1) return E::atom(n.low());
  const std::vector<Numeral::limb> limbs = n.limbs();
  const E base = E::atom(std::uint64_t{1} << 32)
                     .appended(nodes::Mul{0, 0})  // B = 2^64
                     .appended(nodes::Atom{limbs.back()});
  return dedekind::category::fold(
      std::views::iota(std::size_t{0}, limbs.size() - 1), base,
      HornerStep<S, N>{limbs});
}
/** @return the magnitude and the sign of the pair's integer.  @param x the
 *  pair. */
struct Signed {
  Numeral magnitude;
  bool negative;
};
constexpr Signed signed_of(const Pair& x) {
  if (x.n <= x.p)
    return {Numeral(detail_numeral::subtract(x.p.limbs(), x.n.limbs())), false};
  return {Numeral(detail_numeral::subtract(x.n.limbs(), x.p.limbs())), true};
}
/** @return the canonical term of a signed integer: the natural, or its
 *  negation (never of zero).  @param z the integer. */
template <IsSort S, std::size_t N>
constexpr Expr<S, N> integer(const Signed& z) {
  const Expr<S, N> m = natural<S, N>(z.magnitude);
  if (!z.negative || z.magnitude == Numeral{}) return m;
  return m.appended(nodes::Neg{m.root()});
}
}  // namespace detail_expr

/** @brief ℕ's canonical form: one atom where the value fits a word; otherwise
 *  its limbs in Horner form over @f$B = 2^{32} \cdot 2^{32}@f$,
 *  @f$l_n B + \dots + l_0@f$, @c B shared.  Unique per value, so terms are
 *  equal iff their canonical forms are.  @param e the term. */
export template <std::size_t N>
constexpr NatExpr<N> canonical(const NatExpr<N>& e) {
  return detail_expr::natural<NatSort, N>(value_of(e));
}
/** @brief ℤ's canonical form: ℕ's, or the negation of ℕ's, the
 *  @f$(n, 0)@f$ or @f$(0, n)@f$ representative of the Grothendieck class,
 *  zero never negated.  @param e the term. */
export template <std::size_t N>
constexpr IntExpr<N> canonical(const IntExpr<N>& e) {
  return detail_expr::integer<IntSort, N>(detail_expr::signed_of(value_of(e)));
}
/** @brief ℚ's canonical form: ℤ's where the denominator is 1; otherwise the
 *  reduced fraction, @f$\gcd(|p|, q) = 1@f$ and @f$q > 1@f$, the sign in the
 *  numerator.  The gcd is the definition of this form, not ℚ's arithmetic:
 *  @c gcd and the division it needs are @c Numeral's, internal to
 *  @c canonical.  @param e the term. */
export template <std::size_t N>
constexpr std::optional<RatExpr<N>> checked_canonical(const RatExpr<N>& e);
export template <std::size_t N>
constexpr RatExpr<N> canonical(const RatExpr<N>& e) {
  return detail_expr::certified(checked_canonical(e));
}
namespace detail_expr {
/** @return the nodes of a natural's canonical form: one atom, or three per
 *  limb in Horner form.  @param n the natural. */
constexpr std::size_t natural_nodes(const Numeral& n) {
  return n.limb_count() == 1 ? 1 : 3 * n.limb_count();
}
}  // namespace detail_expr
/** @return ℚ's canonical form where it fits the declared bound, or nothing:
 *  its size is counted before a node is written, so a caller that cannot
 *  trap (the Python binding) refuses instead.  @param e the term. */
export template <std::size_t N>
constexpr std::optional<RatExpr<N>> checked_canonical(const RatExpr<N>& e) {
  using namespace detail_expr;
  const Frac f = value_of(e);
  const Signed num = signed_of(f.num);
  const Signed den = signed_of(f.den);
  const Numeral g = gcd(num.magnitude, den.magnitude);
  const Numeral p = detail_numeral::divide(num.magnitude, g).q;
  const Numeral q = detail_numeral::divide(den.magnitude, g).q;
  const bool negative = num.negative != den.negative && p != Numeral{};
  const std::size_t nodes = natural_nodes(p) + (negative ? 1 : 0) +
                            (q == Numeral{1u} ? 0 : natural_nodes(q) + 1);
  if (nodes > N) return std::nullopt;
  const RatExpr<N> top = integer<RatSort, N>(Signed{p, negative});
  if (q == Numeral{1u}) return top;
  return top / certified(dedekind::category::nonzero(natural<RatSort, N>(q)));
}

/** @return @f$2^k@f$ as a canonical term of the sort @c S, written directly
 *  in Horner form: the dyadic scale of a double.  @param k the exponent. */
export template <IsSort S, std::size_t N = default_capacity>
constexpr Expr<S, N> power_of_two(std::size_t k) {
  std::vector<Numeral::limb> limbs(k / 64 + 1, Numeral::limb{});
  limbs.back() = Numeral::limb{1} << (k % 64);
  return detail_expr::natural<S, N>(Numeral{std::move(limbs)});
}

/** @section expr__Floating_Atoms  A double enters ℚ exactly
 *  Every finite @c double is the dyadic rational @f$m \cdot 2^e@f$ its bits
 *  spell (@c std::bit_cast; Knuth, @em TAOCP, Vol. 2, §4.2), so it is a term
 *  of ℚ: @c m over a power of two, or @c m times one.  NaN and ±∞ are not
 *  values of ℚ and the check refuses them; @f$-0@f$ and @f$+0@f$ are one
 *  rational.  A @c float is a @c double exactly; a @c long double is not
 *  read here, its format being the platform's. */
/** @return the term of ℚ the double is, or nothing for NaN and ±∞.
 *  @param x the double. */
export template <std::size_t N>
constexpr std::optional<RatExpr<N>> from(double x) {
  const auto bits = std::bit_cast<std::uint64_t>(x);
  const bool sign = (bits >> 63) & 1U;
  const auto biased = static_cast<unsigned>((bits >> 52) & 0x7FFU);
  const std::uint64_t fraction = bits & 0x000F'FFFF'FFFF'FFFFULL;
  if (biased == 0x7FFU) return std::nullopt;  // NaN or ±∞
  // x = ±mantissa · 2^exp exactly; a subnormal has no implicit leading one.
  const std::uint64_t mantissa =
      biased == 0 ? fraction : (std::uint64_t{1} << 52) | fraction;
  const int exp = biased == 0 ? -1074 : static_cast<int>(biased) - 1075;
  if (mantissa == 0) return RatExpr<N>{};  // ±0
  const RatExpr<N> m =
      sign ? -RatExpr<N>::atom(mantissa) : RatExpr<N>::atom(mantissa);
  if (exp >= 0)
    return m * power_of_two<RatSort, N>(static_cast<std::size_t>(exp));
  return m / detail_expr::certified(dedekind::category::nonzero(
                 power_of_two<RatSort, N>(static_cast<std::size_t>(-exp))));
}
/** @overload */
export template <std::size_t N = default_capacity>
constexpr std::optional<RatExpr<N>> from(float x) {
  return from<N>(static_cast<double>(x));
}

namespace detail_expr {
template <typename T>
inline constexpr bool is_expr_v = false;
template <IsSort S, std::size_t N>
inline constexpr bool is_expr_v<Expr<S, N>> = true;
}  // namespace detail_expr
/** @brief A value of a carrier that is not a term is already canonical in
 *  its own algebra (a word in ℤ/2^w, a double in IEEE's arithmetic, which
 *  carries no law, a word cardinal): generic code calls @c canonical
 *  uniformly.  A composite with parts declares its own, part by part, which
 *  argument-dependent lookup prefers.  @param w the value. */
export template <typename W>
  requires(!detail_expr::is_expr_v<W> && !is_term_v<W>)
constexpr W canonical(W w) {
  return w;
}
/** @concept HasCanonical
 *  @brief @c T has a canonical form, the explicit normalisation point of
 *  generic code: a term's, a composite's part by part, or the identity.
 *  @tparam T the carrier. */
export template <typename T>
concept HasCanonical = requires(const T& t) {
  { canonical(t) } -> std::same_as<T>;
};
/** @brief Whether @c T is ℕ's terms, of any capacity: the bound is storage,
 *  not the set.  @tparam T the carrier. */
export template <typename T>
inline constexpr bool is_natural_term_v = false;
template <std::size_t N>
inline constexpr bool is_natural_term_v<Expr<NatSort, N>> = true;

/** @brief The successor and predecessor on ℕ, the NNO's step, as canonical
 *  terms: @c n + 1, and the monus @c n ∸ 1, @c 0 its fixed point.
 *  @param n the natural. */
export template <std::size_t N>
constexpr NatExpr<N> successor(const NatExpr<N>& n) {
  return canonical(n + NatExpr<N>::atom(1));
}
export template <std::size_t N>
constexpr NatExpr<N> predecessor(const NatExpr<N>& n) {
  if (n == NatExpr<N>{}) return n;
  return detail_expr::natural<NatSort, N>(Numeral(
      detail_numeral::subtract(value_of(n).limbs(), Numeral{1u}.limbs())));
}
/** @brief Lambek on ℕ: @f$[Z, S] : 1 + \mathbb{N} \to \mathbb{N}@f$ is an
 *  isomorphism, its inverse @c Prior, where no word's is (the Lambek section
 *  of @c :nno).  @return the inverse arrow. */
export template <std::size_t N>
constexpr dedekind::category::Prior<NatExpr<N>> inverse(
    dedekind::category::In<NatExpr<N>>) {
  return {};
}
/** @brief The other direction: @c Prior's inverse is @c In.
 *  @return the inverse arrow. */
export template <std::size_t N>
constexpr dedekind::category::In<NatExpr<N>> inverse(
    dedekind::category::Prior<NatExpr<N>>) {
  return {};
}
/** @brief An unsigned constant meeting ℕ, the hook @c :terms' @c carrier_cast
 *  reaches by ADL.  A signed constant has no hook here and is refused there:
 *  ℕ's atoms are the unsigned words.  @tparam K the word's type. */
export template <std::size_t N, std::unsigned_integral K>
  requires(!std::same_as<K, bool>)
constexpr NatExpr<N> carrier_cast(std::type_identity<NatExpr<N>>, K k) {
  return NatExpr<N>{k};
}

/** @brief The canonical natural-numbers universe @c ℕ = @c 𝔸<NatExpr<>>.
 *
 *  @details Per #559 the named species symbols (@c 𝔹 / @c ℕ / @c ℤ / @c ℚ /
 *  @c ℝ / @c ℂ / @c 𝔻) denote the @b universe values (constexpr instances of
 *  @c 𝔸 over the carrier), not carrier @b types; carrier types are spelled
 *  directly in type positions.  So @c ℕ @c | @c (π @c ⋈ @c c) is the
 *  spelling of a subset of the natural numbers, close to textbook notation.
 *  The carrier is ℕ's terms, exact above the word.  The subobject "ℕ ⊂ ℤ"
 *  over a signed carrier is not a second ℕ: it is the halfspace
 *  @c 𝔸<int>{} & (π >= 0) in @c :order. */
export inline constexpr auto ℕ = 𝔸<NatExpr<>>{};

/** @brief The successor and predecessor on ℤ, the NNO's step, as canonical
 *  terms: @c z ± 1, total, ℤ has no floor.  @param z the integer. */
export template <std::size_t N>
constexpr IntExpr<N> successor(const IntExpr<N>& z) {
  return canonical(z + IntExpr<N>::atom(1));
}
export template <std::size_t N>
constexpr IntExpr<N> predecessor(const IntExpr<N>& z) {
  return canonical(z - IntExpr<N>::atom(1));
}
/** @brief A machine integer meeting ℤ, the hook @c :terms' @c carrier_cast
 *  reaches by ADL: the total embedding.  @tparam K the integer's type. */
export template <std::size_t N, std::integral K>
  requires(!std::same_as<K, bool>)
constexpr IntExpr<N> carrier_cast(std::type_identity<IntExpr<N>>, K k) {
  return IntExpr<N>{k};
}

/** @section expr__T_Node  The bridge: a typed term into μF */
namespace detail_expr {
/** @brief The algebra of @c node: a constant is the machine integer's term,
 *  an operation the sort's.  @tparam S the sort.  @tparam N the capacity. */
template <IsSort S, std::size_t N>
struct NodeAlg {
  template <std::integral K>
    requires(!std::same_as<K, bool>)
  constexpr Expr<S, N> operator()(const Const<K>& c) const {
    return Expr<S, N>{c.value};
  }
  template <typename A, typename B>
  constexpr Expr<S, N> operator()(const BinOp<std::plus<>, A, B>&,
                                  const Expr<S, N>& a,
                                  const Expr<S, N>& b) const {
    return a + b;
  }
  template <typename A, typename B>
  constexpr Expr<S, N> operator()(const BinOp<std::multiplies<>, A, B>&,
                                  const Expr<S, N>& a,
                                  const Expr<S, N>& b) const {
    return a * b;
  }
  template <typename A, typename B>
    requires(!std::same_as<S, NatSort>)
  constexpr Expr<S, N> operator()(const BinOp<std::minus<>, A, B>&,
                                  const Expr<S, N>& a,
                                  const Expr<S, N>& b) const {
    return a - b;
  }
  /** @brief A typed division's divisor is certified where the term is built
   *  (a constant 0 is refused there), so the check is consumed here. */
  template <typename A, typename B>
    requires std::same_as<S, RatSort>
  constexpr Expr<S, N> operator()(const BinOp<std::divides<>, A, B>&,
                                  const Expr<S, N>& a,
                                  const Expr<S, N>& b) const {
    return a / certified(dedekind::category::nonzero(b));
  }
};
}  // namespace detail_expr

/** @brief T-Node: the injection of a typed term into μF, the fold of its type
 *  (@c cata).  @tparam S the sort.  @tparam N the capacity.  @param t the
 *  typed term. */
export template <IsSort S = NatSort, std::size_t N = default_capacity,
                 typename T>
  requires IsTerm<T>
constexpr Expr<S, N> node(const T& t) {
  return dedekind::category::cata(detail_expr::NodeAlg<S, N>{}, t);
}

/** @brief The canonical embedding of Kleene's three values into ℤ:
 *  @c False ↦ −1, @c Unknown ↦ 0, @c True ↦ 1, the enum's own values. */
export inline constexpr auto embed_𝕂3_ℤ_ =
    arrow<Ternary, IntExpr<>>([](const Ternary& t) {
      return IntExpr<>{static_cast<int>(std::to_underlying(t))};
    });

}  // namespace dedekind::sets

// ── The laws of ℕ, ℤ and ℚ, on their terms ─────────────────────────────────
// Certified on the term algebras, equality through the canonical forms: + and
// · associative and commutative, · distributing, 0 and 1 the units, the order
// of canonical forms a chain; ℤ adds the additive inverse, ℚ the
// multiplicative one on ℚ*.  Within the capacity, the declared bound, the
// operations are exact: a term past it is refused, never wrapped.
namespace dedekind::category {
template <typename S, std::size_t N>
struct unit_trait<dedekind::sets::Expr<S, N>,
                  std::plus<dedekind::sets::Expr<S, N>>> {
  using value_type = dedekind::sets::Expr<S, N>;
  static constexpr value_type value = value_type::atom(0);
};
template <typename S, std::size_t N>
struct unit_trait<dedekind::sets::Expr<S, N>,
                  std::multiplies<dedekind::sets::Expr<S, N>>> {
  using value_type = dedekind::sets::Expr<S, N>;
  static constexpr value_type value = value_type::atom(1);
};
template <typename S, std::size_t N>
struct is_exact_total<dedekind::sets::Expr<S, N>,
                      std::plus<dedekind::sets::Expr<S, N>>> : std::true_type {
};
template <typename S, std::size_t N>
struct is_exact_total<dedekind::sets::Expr<S, N>,
                      std::multiplies<dedekind::sets::Expr<S, N>>>
    : std::true_type {};
template <typename S, std::size_t N>
inline constexpr bool is_associative_v<dedekind::sets::Expr<S, N>,
                                       std::plus<dedekind::sets::Expr<S, N>>> =
    true;
template <typename S, std::size_t N>
inline constexpr bool is_associative_v<
    dedekind::sets::Expr<S, N>, std::multiplies<dedekind::sets::Expr<S, N>>> =
    true;
template <typename S, std::size_t N>
inline constexpr bool is_commutative_v<dedekind::sets::Expr<S, N>,
                                       std::plus<dedekind::sets::Expr<S, N>>> =
    true;
template <typename S, std::size_t N>
inline constexpr bool is_commutative_v<
    dedekind::sets::Expr<S, N>, std::multiplies<dedekind::sets::Expr<S, N>>> =
    true;
template <typename S, std::size_t N>
inline constexpr bool is_distributive_v<
    dedekind::sets::Expr<S, N>, std::multiplies<dedekind::sets::Expr<S, N>>,
    std::plus<dedekind::sets::Expr<S, N>>> = true;
template <typename S, std::size_t N>
inline constexpr bool
    is_reflexive_v<dedekind::sets::Expr<S, N>, std::less_equal<>> = true;
template <typename S, std::size_t N>
inline constexpr bool
    is_transitive_v<dedekind::sets::Expr<S, N>, std::less_equal<>> = true;
template <typename S, std::size_t N>
inline constexpr bool
    is_antisymmetric_v<dedekind::sets::Expr<S, N>, std::less_equal<>> = true;
template <typename S, std::size_t N>
inline constexpr bool
    is_connex_v<dedekind::sets::Expr<S, N>, std::less_equal<>> = true;
// ℤ and ℚ: the additive inverse is the negation node.
template <typename S, std::size_t N>
  requires(!std::same_as<S, dedekind::sets::NatSort>)
inline constexpr bool is_invertible_v<dedekind::sets::Expr<S, N>,
                                      std::plus<dedekind::sets::Expr<S, N>>> =
    true;
template <typename S, std::size_t N>
  requires(!std::same_as<S, dedekind::sets::NatSort>)
struct inverse_trait<dedekind::sets::Expr<S, N>,
                     std::plus<dedekind::sets::Expr<S, N>>> {
  static constexpr bool exists = true;
  using value_type = dedekind::sets::Expr<S, N>;
  static constexpr value_type compute(const value_type& x) { return -x; }
};
// ℚ: every non-zero term has an inverse, on ℚ* (the convention of IsField);
// its divisors are its certified non-zeros.
template <std::size_t N>
inline constexpr bool is_invertible_v<
    dedekind::sets::RatExpr<N>, std::multiplies<dedekind::sets::RatExpr<N>>> =
    true;
template <std::size_t N>
struct divisor_of<dedekind::sets::RatExpr<N>> {
  using type = NonZero<dedekind::sets::RatExpr<N>>;
  static constexpr std::optional<type> check(
      const dedekind::sets::RatExpr<N>& q) {
    return nonzero(q);
  }
};
template <typename S, std::size_t N>
struct SpeciesTraits<dedekind::sets::Expr<S, N>> {
  using Domain = dedekind::sets::Expr<S, N>;
  using machine_type = dedekind::sets::Expr<S, N>;
};
/** @brief 𝕂3 ↪ ℤ is injective and monotone. */
template <>
inline constexpr bool
    is_monic_arrow_v<std::decay_t<decltype(dedekind::sets::embed_𝕂3_ℤ_)>> =
        true;
template <>
inline constexpr bool is_monotone_v<
    std::decay_t<decltype(dedekind::sets::embed_𝕂3_ℤ_)>, std::less_equal<>> =
    true;
}  // namespace dedekind::category

namespace dedekind::sets::detail_expr_witness {
using N64 = NatExpr<>;
using Z64 = IntExpr<>;
using Q64 = RatExpr<>;
constexpr std::uint64_t top = std::numeric_limits<std::uint64_t>::max();
/** @return @f$2^{64}@f$ as a term: a word plus one. */
constexpr N64 two_64() { return N64::atom(top) + N64::atom(1); }
// Small values only: the big-value cases are runtime tests, so constant
// evaluation stays cheap.
static_assert(std::regular<N64> && std::totally_ordered<N64> &&
                  std::regular<Z64> && std::regular<Q64>,
              "the terms are value types");
static_assert(N64::atom(6) * N64::atom(7) == N64::atom(42) &&
                  N64::atom(2) + N64::atom(3) != N64::atom(6) &&
                  N64::atom(3) < N64::atom(4) && two_64() > N64::atom(top),
              "== and <=> are those of the canonical forms");
static_assert(canonical(N64::atom(6) * N64::atom(7)).size() == 1 &&
                  canonical(two_64()).size() == 6,
              "an atom where the value fits a word, else Horner over 2^64");
static_assert(Z64{-3} + Z64{5} == Z64{2} && Z64{2} - Z64{5} == Z64{-3} &&
                  -Z64{-3} == Z64{3} && Z64{-3} < Z64{2} &&
                  canonical(Z64{2} - Z64{5}).size() == 2 &&
                  canonical(Z64{5} - Z64{5}).size() == 1,
              "ℤ: the canonical form is ℕ's or its negation, zero never "
              "negated");
static_assert(Q64{1, 2} + Q64{1, 3} == Q64{5, 6} && Q64{2, 4} == Q64{1, 2} &&
                  Q64{2, -4} == Q64{-1, 2} && Q64{-1, 2} < Q64{} &&
                  Q64{1, 3} < Q64{1, 2} && canonical(Q64{6, 3}).size() == 1 &&
                  canonical(Q64{2, -4}).size() == 4,
              "ℚ: reduced, the sign in the numerator; 6/3 is the atom 2, "
              "2/−4 is Div(Neg 1, 2)");
static_assert(canonical(std::size_t{42}) == std::size_t{42},
              "a word is its own canonical form, in ℤ/2^64");
static_assert(node(Const<std::size_t>{6u} * Const<std::size_t>{7u}) ==
                      N64::atom(42) &&
                  node<IntSort>(Const<int>{2} - Const<int>{5}) == Z64{-3},
              "T-Node: a typed term injects into μF, its value kept");
static_assert(Z64::widened(N64::atom(7)) == Z64{7} &&
                  Q64::widened(Z64{-3}) == Q64{-3},
              "ℕ ↪ ℤ ↪ ℚ keep every node");
static_assert(
    dedekind::category::IsRig<N64, std::plus<N64>, std::multiplies<N64>> &&
        !dedekind::category::IsInvertible<N64, std::plus<N64>> &&
        dedekind::category::IsCommutativeRing<Z64, std::plus<Z64>,
                                              std::multiplies<Z64>> &&
        dedekind::category::IsField<Q64, std::plus<Q64>, std::multiplies<Q64>>,
    "ℕ is a commutative rig, ℤ a commutative ring, ℚ a field, certified on "
    "their terms");
static_assert(successor(N64::atom(top)) == two_64() &&
                  predecessor(two_64()) == N64::atom(top) &&
                  predecessor(N64{}) == N64{},
              "ℕ's step crosses the word's edge both ways; 0 is the monus' "
              "fixed point");
static_assert(
    dedekind::category::IsIsomorphism<dedekind::category::In<N64>> &&
        inverse(dedekind::category::In<N64>{})(two_64()) ==
            std::optional<N64>{N64::atom(top)},
    "Lambek holds on ℕ: [Z, S] is an isomorphism, across the word's edge");
static_assert(std::same_as<std::remove_cvref_t<decltype(ℕ)>, 𝔸<N64>> &&
                  dedekind::category::IsSet<𝔸<N64>> &&
                  !dedekind::category::IsFiniteLSet<Ø<N64>>,
              "ℕ is the universe over its terms, an ETCS set, not a finite "
              "chain");
static_assert(
    checked_canonical(Q64{1, 2}).has_value() &&
        !checked_canonical(RatExpr<3>::atom(top) * RatExpr<3>::atom(top))
             .has_value(),
    "a canonical form is counted before it is written: (2^64 - 1)^2 needs "
    "two limbs, six nodes, past a bound of three");
static_assert(HasCanonical<Q64> && HasCanonical<int> &&
                  HasCanonical<ExtensionalCardinal<>> &&
                  is_natural_term_v<NatExpr<128>> && !is_natural_term_v<Z64>,
              "every carrier has a canonical form, the identity where it is "
              "not a term; ℕ's terms are ℕ at every capacity");
}  // namespace dedekind::sets::detail_expr_witness
