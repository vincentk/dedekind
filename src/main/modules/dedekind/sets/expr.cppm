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

#include <array>
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

  /** @brief The atom @c 0. */
  constexpr Expr() = default;
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

  /** @brief The term @c a with the nodes of @c b appended after it, their
   *  operands shifted, and the node @c combine(root of a, root of b) on top:
   *  the binary operations' one construction.  @param a the left term.
   *  @param b the right term.  @param combine the operation's node. */
  template <typename Combine>
    requires std::regular_invocable<const Combine&, std::uint32_t,
                                    std::uint32_t>
  static constexpr Expr joined(const Expr& a, const Expr& b,
                               const Combine& combine) {
    const auto offset = static_cast<std::uint32_t>(a.size_);
    const Expr both = dedekind::category::fold(
        std::views::iota(std::size_t{0}, b.size_), a, AppendShifted{b, offset});
    return both.appended(combine(a.root(), offset + b.root()));
  }

  /** @brief Equality of terms: equality of their canonical forms. */
  friend constexpr bool operator==(const Expr& x, const Expr& y)
    requires std::same_as<S, NatSort>
  {
    return value_of(x) == value_of(y);
  }
  /** @brief The order of terms: the order of their canonical forms. */
  friend constexpr std::strong_ordering operator<=>(const Expr& x,
                                                    const Expr& y)
    requires std::same_as<S, NatSort>
  {
    return value_of(x) <=> value_of(y);
  }

  /** @brief The sum, a term. */
  friend constexpr Expr operator+(const Expr& x, const Expr& y) {
    return joined(x, y, MakeAdd{});
  }
  /** @brief The product, a term. */
  friend constexpr Expr operator*(const Expr& x, const Expr& y) {
    return joined(x, y, MakeMul{});
  }

 private:
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
  /** @brief One step of @c joined: the node of @c from at @c i, shifted,
   *  appended. */
  struct AppendShifted {
    Expr from;
    std::uint32_t offset;
    constexpr Expr operator()(const Expr& acc, std::size_t i) const {
      return acc.appended(std::visit(Shift{offset}, from.nodes_[i]));
    }
  };

  std::array<node_type, N> nodes_{};
  std::size_t size_{1};
};

/** @brief A natural number as a value: a term of ℕ.  @tparam N the
 *  capacity. */
export template <std::size_t N = default_capacity>
using NatExpr = Expr<NatSort, N>;

/** @section expr__Reading_ℕ  The canonical form of a natural number
 *  @c Numeral is the machinery @c canonical computes with: the value of a
 *  term, limb by limb, from which the canonical form is written back.  It is
 *  not a value a program holds. */
namespace detail_expr {
/** @brief A node's value, from the values of the nodes before it. */
template <std::size_t N>
struct EvalNat {
  const std::array<Numeral, N>& v;
  constexpr Numeral operator()(nodes::Atom n) const { return Numeral{n.value}; }
  constexpr Numeral operator()(nodes::Add n) const { return v[n.a] + v[n.b]; }
  constexpr Numeral operator()(nodes::Mul n) const { return v[n.a] * v[n.b]; }
};
/** @brief The values of the nodes so far. */
template <std::size_t N>
struct Values {
  std::array<Numeral, N> v{};
};
/** @brief One step of the fold: the value of the node at @c i. */
template <std::size_t N>
struct EvalStep {
  NatExpr<N> e;
  constexpr Values<N> operator()(Values<N> s, std::size_t i) const {
    s.v[i] = std::visit(EvalNat<N>{s.v}, e.at(i));
    return s;
  }
};
/** @brief One step of writing back: Horner, @c acc·B + l_k, the limbs from
 *  the top, with @c B = 2^64 the node at index 1. */
template <std::size_t N>
struct HornerStep {
  std::vector<Numeral::limb> limbs;  // little-endian
  constexpr NatExpr<N> operator()(const NatExpr<N>& acc, std::size_t i) const {
    const std::uint32_t times = acc.root();
    const NatExpr<N> shifted = acc.appended(nodes::Mul{times, 1});
    const NatExpr<N> with =
        shifted.appended(nodes::Atom{limbs[limbs.size() - 2 - i]});
    return with.appended(nodes::Add{shifted.root(), with.root()});
  }
};
}  // namespace detail_expr

/** @return the value of the term @c e, the normal form @c canonical reads
 *  back from: internal to @c canonical, @c == and @c <=>.  @param e the term.
 */
template <std::size_t N>
constexpr Numeral value_of(const NatExpr<N>& e) {
  return dedekind::category::fold(std::views::iota(std::size_t{0}, e.size()),
                                  detail_expr::Values<N>{},
                                  detail_expr::EvalStep<N>{e})
      .v[e.root()];
}

/** @brief The canonical form of a natural number's term: one atom where the
 *  value fits a word; otherwise its limbs in Horner form over
 *  @f$B = 2^{32} \cdot 2^{32}@f$, @f$l_n B + \dots + l_0@f$, the node @c B
 *  shared.  Unique per value, so terms are equal iff their canonical forms
 *  are.  @param e the term. */
export template <std::size_t N>
constexpr NatExpr<N> canonical(const NatExpr<N>& e) {
  const Numeral n = value_of(e);
  if (n.limb_count() == 1) return NatExpr<N>::atom(n.low());
  const std::vector<Numeral::limb> limbs = n.limbs();
  const NatExpr<N> base = NatExpr<N>::atom(std::uint64_t{1} << 32)
                              .appended(nodes::Mul{0, 0})  // B = 2^64
                              .appended(nodes::Atom{limbs.back()});
  return dedekind::category::fold(
      std::views::iota(std::size_t{0}, limbs.size() - 1), base,
      detail_expr::HornerStep<N>{limbs});
}

/** @brief A word is already canonical in its own algebra, ℤ/2^w: generic code
 *  calls @c canonical uniformly.  @param w the word. */
export template <std::unsigned_integral W>
constexpr W canonical(W w) {
  return w;
}

/** @section expr__T_Node  The bridge: a typed term into μF */
namespace detail_expr {
/** @brief The algebra of @c node: a constant is an atom, an operation the
 *  sort's.  @tparam N the capacity. */
template <std::size_t N>
struct NodeNat {
  template <std::unsigned_integral K>
  constexpr NatExpr<N> operator()(const Const<K>& c) const {
    return NatExpr<N>::atom(static_cast<std::uint64_t>(c.value));
  }
  template <typename A, typename B>
  constexpr NatExpr<N> operator()(const BinOp<std::plus<>, A, B>&,
                                  const NatExpr<N>& a,
                                  const NatExpr<N>& b) const {
    return a + b;
  }
  template <typename A, typename B>
  constexpr NatExpr<N> operator()(const BinOp<std::multiplies<>, A, B>&,
                                  const NatExpr<N>& a,
                                  const NatExpr<N>& b) const {
    return a * b;
  }
};
}  // namespace detail_expr

/** @brief T-Node: the injection of a typed term of ℕ into μF, the fold of its
 *  type (@c cata).  @tparam N the capacity.  @param t the typed term. */
export template <std::size_t N = default_capacity, typename T>
  requires IsTerm<T>
constexpr NatExpr<N> node(const T& t) {
  return dedekind::category::cata(detail_expr::NodeNat<N>{}, t);
}

}  // namespace dedekind::sets

// ── ℕ's laws, on its terms ─────────────────────────────────────────────────
// Certified on the term algebra, equality through the canonical form: + and ·
// of terms are associative, commutative, · distributes, 0 and 1 are units, and
// the order of canonical forms is a chain.  Within the capacity, the declared
// bound, the operations are exact: a term past it is refused, never wrapped.
namespace dedekind::category {
template <std::size_t N>
struct unit_trait<dedekind::sets::NatExpr<N>,
                  std::plus<dedekind::sets::NatExpr<N>>> {
  using value_type = dedekind::sets::NatExpr<N>;
  static constexpr value_type value = value_type::atom(0);
};
template <std::size_t N>
struct unit_trait<dedekind::sets::NatExpr<N>,
                  std::multiplies<dedekind::sets::NatExpr<N>>> {
  using value_type = dedekind::sets::NatExpr<N>;
  static constexpr value_type value = value_type::atom(1);
};
template <std::size_t N>
struct is_exact_total<dedekind::sets::NatExpr<N>,
                      std::plus<dedekind::sets::NatExpr<N>>> : std::true_type {
};
template <std::size_t N>
struct is_exact_total<dedekind::sets::NatExpr<N>,
                      std::multiplies<dedekind::sets::NatExpr<N>>>
    : std::true_type {};
template <std::size_t N>
inline constexpr bool is_associative_v<dedekind::sets::NatExpr<N>,
                                       std::plus<dedekind::sets::NatExpr<N>>> =
    true;
template <std::size_t N>
inline constexpr bool is_associative_v<
    dedekind::sets::NatExpr<N>, std::multiplies<dedekind::sets::NatExpr<N>>> =
    true;
template <std::size_t N>
inline constexpr bool is_commutative_v<dedekind::sets::NatExpr<N>,
                                       std::plus<dedekind::sets::NatExpr<N>>> =
    true;
template <std::size_t N>
inline constexpr bool is_commutative_v<
    dedekind::sets::NatExpr<N>, std::multiplies<dedekind::sets::NatExpr<N>>> =
    true;
template <std::size_t N>
inline constexpr bool is_distributive_v<
    dedekind::sets::NatExpr<N>, std::multiplies<dedekind::sets::NatExpr<N>>,
    std::plus<dedekind::sets::NatExpr<N>>> = true;
template <std::size_t N>
inline constexpr bool
    is_reflexive_v<dedekind::sets::NatExpr<N>, std::less_equal<>> = true;
template <std::size_t N>
inline constexpr bool
    is_transitive_v<dedekind::sets::NatExpr<N>, std::less_equal<>> = true;
template <std::size_t N>
inline constexpr bool
    is_antisymmetric_v<dedekind::sets::NatExpr<N>, std::less_equal<>> = true;
template <std::size_t N>
inline constexpr bool
    is_connex_v<dedekind::sets::NatExpr<N>, std::less_equal<>> = true;
}  // namespace dedekind::category

namespace dedekind::sets::detail_expr_witness {
using N64 = NatExpr<>;
constexpr std::uint64_t top = std::numeric_limits<std::uint64_t>::max();
/** @return @f$2^{64}@f$ as a term: a word plus one. */
constexpr N64 two_64() { return N64::atom(top) + N64::atom(1); }
static_assert(std::regular<N64> && std::totally_ordered<N64>,
              "a natural number's term is a value type");
static_assert(N64::atom(6) * N64::atom(7) == N64::atom(42) &&
                  N64::atom(2) + N64::atom(3) != N64::atom(6) &&
                  N64::atom(3) < N64::atom(4) && two_64() > N64::atom(top),
              "== and <=> are those of the canonical forms");
static_assert(
    canonical(N64::atom(6) * N64::atom(7)).size() == 1 &&
        canonical(two_64()).size() == 6 && canonical(two_64()) == two_64() &&
        canonical(two_64() * two_64()).size() == 9,
    "a canonical form is an atom where the value fits a word, "
    "else Horner over 2^64: 2^64 is 1·B + 0, 2^128 is (1·B + 0)·B + 0");
static_assert(canonical(N64::atom(top) * N64::atom(top) + N64::atom(1)) ==
                      N64::atom(top) * N64::atom(top) + N64::atom(1) &&
                  N64::atom(top) * N64::atom(top) != N64::atom(1),
              "past the word the term is exact: (2^64 − 1)^2 is not the word "
              "product, which wraps to 1");
static_assert(canonical(std::size_t{42}) == std::size_t{42},
              "a word is its own canonical form, in ℤ/2^64");
static_assert(node(Const<std::size_t>{6u} * Const<std::size_t>{7u}) ==
                      N64::atom(42) &&
                  node(Const<std::size_t>{top} * Const<std::size_t>{top}) ==
                      N64::atom(top) * N64::atom(top),
              "T-Node: a typed term injects into μF, its value kept");
static_assert(
    dedekind::category::IsRig<N64, std::plus<N64>, std::multiplies<N64>> &&
        dedekind::category::IsCommutative<N64, std::multiplies<N64>>,
    "ℕ is a commutative rig, certified on its terms");
static_assert(!dedekind::category::IsInvertible<N64, std::plus<N64>>,
              "ℕ has no additive inverses: its terms have no subtraction");
}  // namespace dedekind::sets::detail_expr_witness
