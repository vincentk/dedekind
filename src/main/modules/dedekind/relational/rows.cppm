/**
 * @file dedekind/relational/rows.cppm
 * @partition :rows
 * @brief Relations built at run time as terms: arrows @f$A \rightsquigarrow
 *        B@f$ between products of carriers, their objects data (#1037).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section rows__The_Term
 * A relation built at run time is an @b arrow @f$R : A \rightsquigarrow B@f$
 * between two @b objects, each a product of carriers held as a flat list of
 * sorts (a binary tree of products modulo associativity: the strict monoidal
 * normal form, Mac Lane's coherence).  Its points are rows of the columns of
 * @f$A@f$ then @f$B@f$.  It is a @b term whose nodes are the set grammar's and
 * the relational calculus's:
 *
 * | node | typed twin | built by |
 * |---|---|---|
 * | @c Universe | @c 𝔸 over @f$A \times B@f$ | @c universe |
 * | @c Comprehension | @c Comprehension, the one set former | @c R @c & @c p
 * | | @c Meet, @c Join, @c Not | @c category::Meet / @c Join / @c Not | @c & @c
 * \| @c ~ | | @c Converse | @c relational:dyadic's @c converse | @c converse |
 * | @c Tensor | the bicategory's ⊗ | @c * |
 * | @c Compose | @c relational:dyadic's @c >> | @c >> |
 *
 * Nothing n-ary is primitive: a @b table is a relation out of the unit,
 * @f$() \rightsquigarrow A_1 \times \dots \times A_n@f$, and a projection is
 * composition with the structural relation @c proj.  Nothing is flattened at
 * construction: @c R @c & @c S is a @c Meet node over two relations.  The
 * @b classifier χ of a relation on its columns is a @b reading (a fold,
 * @c predicate_of), as are its verdict and its written form; @c canonical
 * (algebra) normalises the reading.  Rewrites on the tree are the phase after
 * this.
 *
 * @section rows__The_Kleisli_Structure
 * A relation is the Kleisli arrow @f$A \to \mathcal{P}B@f$ of the powerset
 * monad, and the relational operations are the monad's:
 *
 * | monad | relation | here |
 * |---|---|---|
 * | @f$\eta@f$, @f$a \mapsto \{a\}@f$ | the identity @f$\Delta_A@f$ | @c diag |
 * | Kleisli composition | the relative product @f$R;S@f$ | @c >> |
 * | @f$\mu@f$, the union | the @f$\exists@f$ over the middle object | @c
 * Pred::exists, one per middle column, in @c predicate_of(R @c >> @c S) |
 *
 * The monad laws are decided by @c canonical where the ∃ eliminates:
 * @f$\Delta;R = R = R;\Delta@f$ and @f$(R;S);T = R;(S;T)@f$.
 *
 * @section rows__The_Frobenius_Structure
 * Relations form a cartesian bicategory (@c category:cartesian_bicategory,
 * #946): every object carries @c copy @f$A \rightsquigarrow A \times A@f$ and
 * @c merge, its converse, both relations here like any other, and
 * @f$R \wedge S = \mathrm{merge} \circ (R \otimes S) \circ \mathrm{copy}@f$.
 * Composing along a product object is the general ∃ of @c >>, so the law is a
 * test, decided by @c canonical once the middle is eliminated (a conjunction's
 * operands are sorted, #884).  The converse is the dagger:
 * @f$(R;S)^\circ = S^\circ;R^\circ@f$, @f$R^{\circ\circ} = R@f$.
 */
module;

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <limits>  // the slot bits of a column mask
#include <memory>
#include <type_traits>
#include <utility>  // std::pair: a point of a relation, dom × cod
#include <variant>
#include <vector>

export module dedekind.relational:rows;

import dedekind.category; // Ternary
import dedekind.sets;     // Row, Term, Pred, the sorts, graph_term
import :dyadic;           // Law, Laws: the rule table, read over the term

namespace dedekind::relational {

/** @brief The sorts of a row, one per column. */
export using Columns = std::vector<std::size_t>;
/** @brief The row @c r has the columns @c cols: one cell per column, each of
 *  the column's sort.  @tparam R the row.  @param r a row.  @param cols the
 *  sorts. */
template <typename R>
bool of_columns(const R& r, const Columns& cols) {
  if (r.cells.size() != cols.size()) return false;
  for (std::size_t i = 0; i < cols.size(); ++i)
    if (r.cells[i].index() != cols[i]) return false;
  return true;
}

export struct Object;
inline bool same_object(const Object& a, const Object& b);
/** @brief A factor of an object: a carrier (a leaf, its sort the column's), or
 *  the sum @f$X + Y@f$ of two objects (#1039).  A sum lays out a tag column
 *  (𝔹: which summand), then the left summand's columns, then the right's; a
 *  row reads only the active side, so every column keeps one carrier. */
export struct Factor {
  enum class Kind { Leaf, Sum };
  Kind kind{Kind::Leaf};
  std::size_t sort{};  // a leaf's carrier; a sum's tag column (𝔹)
  std::shared_ptr<const Object> left, right;  // a sum's summands
  /** @brief The leaf of a carrier.  @param c its sort. */
  static Factor leaf(std::size_t c) {
    Factor f;
    f.sort = c;
    return f;
  }
  friend bool operator==(const Factor& a, const Factor& b) {
    if (a.kind != b.kind || a.sort != b.sort) return false;
    return a.kind == Kind::Leaf ||
           (same_object(*a.left, *b.left) && same_object(*a.right, *b.right));
  }
};

/** @brief An object: a product of factors in @b normal @b form, a list.  The
 *  product is associative with unit @c 1 (the empty list), so
 *  @f$(A \times B) \times C@f$ and @f$A \times (B \times C)@f$ are the
 *  same object by construction, and @c >> composes regardless of grouping.
 *  Read as a binary tree, @c (first, rest), where a tree is asked for
 *  (@c kind / @c operands in Python).  The columns are its factors'. */
export struct Object {
  std::vector<Factor> factors;
  Object() = default;
  /** @brief The product of these carriers, one leaf each. */
  Object(std::initializer_list<std::size_t> sorts) {
    for (const std::size_t c : sorts) factors.push_back(Factor::leaf(c));
  }
  explicit Object(const Columns& sorts) {
    for (const std::size_t c : sorts) factors.push_back(Factor::leaf(c));
  }
  friend bool operator==(const Object&, const Object&) = default;
};

inline bool same_object(const Object& a, const Object& b) { return a == b; }
/** @brief The columns of an object: per factor, a leaf's sort; a sum's tag,
 *  then its left summand's columns, then its right's. */
inline void append_columns(const Object& o, Columns& out) {
  for (const Factor& f : o.factors) {
    out.push_back(f.sort);
    if (f.kind == Factor::Kind::Sum) {
      append_columns(*f.left, out);
      append_columns(*f.right, out);
    }
  }
}
export inline Columns columns(const Object& o) {
  Columns out;
  append_columns(o, out);
  return out;
}
/** @brief The object @f$X + Y@f$: one sum factor, its tag column of sort
 *  @c tag (the 𝔹 carrier's).  @param x, y the summands. */
export inline Object sum(const Object& x, const Object& y, std::size_t tag) {
  Factor f;
  f.kind = Factor::Kind::Sum;
  f.sort = tag;
  f.left = std::make_shared<const Object>(x);
  f.right = std::make_shared<const Object>(y);
  Object out;
  out.factors.push_back(std::move(f));
  return out;
}
/** @brief The product @f$A \times B@f$: the factors side by side (the normal
 *  form of an associative product). */
export inline Object operator*(const Object& a, const Object& b) {
  Object out = a;
  out.factors.insert(out.factors.end(), b.factors.begin(), b.factors.end());
  return out;
}

/** @brief A relation @f$A \rightsquigarrow B@f$ built at run time: a term
 *  over relations, its objects @c dom and @c cod data, its classifier a
 *  reading.  @tparam K the constants' type.  @tparam Cs the carriers a
 *  column may have. */
export template <typename K, typename... Cs>
struct RelExpr {
  using Point = dedekind::sets::Row<Cs...>;
  using P = dedekind::sets::Pred<K>;
  using T = dedekind::sets::Term<K>;
  enum class Kind {
    Universe,
    Comprehension,
    Meet,
    Join,
    Not,
    Converse,
    Tensor,
    Compose,
    Sum
  };

  Kind kind{Kind::Universe};
  Object dom, cod;
  P predicate = P::top();  // a comprehension's: on the columns of dom then cod
  std::shared_ptr<const RelExpr> a, b;            // the legs
  std::uint64_t budget = std::uint64_t{1} << 16;  // verdict's search

  unsigned arity() const { return static_cast<unsigned>(columns().size()); }
  /** @brief The columns: @c dom's then @c cod's. */
  Columns columns() const { return relational::columns(dom * cod); }
  /** @brief A row of the columns: the points' representative. */
  Point rep() const { return dedekind::sets::row_of_sorts<Cs...>(columns()); }

  // ── A RelExpr IS a relation (relational::IsRelation) ──
  /** @brief A point of the relation: a row of the domain and a row of the
   *  codomain, the product @c dom @c × @c cod, read through @c π_1 / @c π_2. */
  using Domain = std::pair<Point, Point>;
  /** @brief Membership answers in K₃: decided, or not yet (the verdict's
   *  search). */
  using logic_species = dedekind::category::Kleene;
  using Codomain = dedekind::category::Ternary;
  struct Member {
    Domain value;
  };
  Domain ι(const Member& m) const { return m.value; }
  /** @brief χ: the verdict at the joined row of the point; a point not of
   *  the shape @c dom @c × @c cod is none of the relation's. */
  Codomain operator()(const Domain& p) const {
    if (!of_columns(p.first, relational::columns(dom)) ||
        !of_columns(p.second, relational::columns(cod)))
      return dedekind::category::Ternary::False;
    Point row = p.first;
    row.cells.insert(row.cells.end(), p.second.cells.begin(),
                     p.second.cells.end());
    return verdict(*this, row);
  }

  /** @brief The universe @f$A \times B@f$: the full relation. */
  static RelExpr universe(Object dom, Object cod) {
    RelExpr r;
    r.dom = std::move(dom);
    r.cod = std::move(cod);
    return r;
  }
  static RelExpr unary(Kind k, RelExpr x, Object dom, Object cod,
                       P p = P::top()) {
    RelExpr r;
    r.kind = k;
    r.dom = std::move(dom);
    r.cod = std::move(cod);
    r.budget = x.budget;
    r.predicate = std::move(p);
    r.a = std::make_shared<const RelExpr>(std::move(x));
    return r;
  }
  static RelExpr binary(Kind k, RelExpr x, RelExpr y, Object dom, Object cod) {
    RelExpr r;
    r.kind = k;
    r.dom = std::move(dom);
    r.cod = std::move(cod);
    r.budget = x.budget;
    r.a = std::make_shared<const RelExpr>(std::move(x));
    r.b = std::make_shared<const RelExpr>(std::move(y));
    return r;
  }
};

// ── The set grammar on relations ────────────────────────────────────────────

/** @brief @c R @c & @c p: the former, the meet with the subobject @c p
 *  classifies (#826), every slot, constant and operator of @c p checked in its
 *  column's sort now.  @throws not_admitted for a slot past the columns, or an
 *  operator not admitted. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> operator&(const RelExpr<K, Cs...>& r,
                            const dedekind::sets::Pred<K>& p) {
  using R = RelExpr<K, Cs...>;
  if (!dedekind::sets::only_arguments(dedekind::sets::slots(p), r.arity() + 1))
    throw dedekind::sets::not_admitted(
        "R & pred: a slot past the relation's columns");
  dedekind::sets::bind_check<typename R::Point>(p, r.rep());
  return R::unary(R::Kind::Comprehension, r, r.dom, r.cod, p);
}
/** @brief @c R @c | @c p: refused; the filter is @c R @c & @c p (#826). */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> operator|(const RelExpr<K, Cs...>&,
                            const dedekind::sets::Pred<K>&) = delete;
/** @brief @c R @c & @c S and @c R @c | @c S between the same objects: the
 *  lattice's nodes.  @throws not_admitted otherwise. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> operator&(const RelExpr<K, Cs...>& a,
                            const RelExpr<K, Cs...>& b) {
  using R = RelExpr<K, Cs...>;
  if (a.dom != b.dom || a.cod != b.cod)
    throw dedekind::sets::not_admitted(
        "R & S: relations between different objects");
  return R::binary(R::Kind::Meet, a, b, a.dom, a.cod);
}
export template <typename K, typename... Cs>
RelExpr<K, Cs...> operator|(const RelExpr<K, Cs...>& a,
                            const RelExpr<K, Cs...>& b) {
  using R = RelExpr<K, Cs...>;
  if (a.dom != b.dom || a.cod != b.cod)
    throw dedekind::sets::not_admitted(
        "R | S: relations between different objects");
  return R::binary(R::Kind::Join, a, b, a.dom, a.cod);
}
/** @brief @c ~R: the complement. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> operator~(const RelExpr<K, Cs...>& a) {
  using R = RelExpr<K, Cs...>;
  return R::unary(R::Kind::Not, a, a.dom, a.cod);
}

// ── The relational calculus: °, ⊗, Kleisli composition ──────────────────────

/** @brief @c R°: the converse, @f$B \rightsquigarrow A@f$, the dagger. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> converse(const RelExpr<K, Cs...>& a) {
  using R = RelExpr<K, Cs...>;
  return R::unary(R::Kind::Converse, a, a.cod, a.dom);
}
/** @brief @c R @c * @c S: the tensor @f$R \otimes S : A \times C
 *  \rightsquigarrow B \times D@f$.  Two universes make the product
 *  universe (the universe over a product is the product of the universes). */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> operator*(const RelExpr<K, Cs...>& a,
                            const RelExpr<K, Cs...>& b) {
  using R = RelExpr<K, Cs...>;
  Object dom = a.dom * b.dom;
  Object cod = a.cod * b.cod;
  if (a.kind == R::Kind::Universe && b.kind == R::Kind::Universe)
    return R::universe(std::move(dom), std::move(cod));
  return R::binary(R::Kind::Tensor, a, b, std::move(dom), std::move(cod));
}
/** @brief @c R @c >> @c S: Kleisli composition, @f$R;S : A \rightsquigarrow
 *  C@f$ for @f$R : A \rightsquigarrow B@f$ and @f$S : B \rightsquigarrow
 *  C@f$, the ∃ over the whole middle object @f$B@f$.  @throws not_admitted
 *  where @c R's codomain is not @c S's domain. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> operator>>(const RelExpr<K, Cs...>& a,
                             const RelExpr<K, Cs...>& b) {
  using R = RelExpr<K, Cs...>;
  if (a.cod != b.dom)
    throw dedekind::sets::not_admitted("R >> S: R's codomain is S's domain");
  return R::binary(R::Kind::Compose, a, b, a.dom, b.cod);
}
/** @brief @f$A \rightsquigarrow B@f$ read as the table @f$1
 *  \rightsquigarrow A \times B@f$: every factor moved to the codomain.  Rel
 *  is compact closed, @f$A \times B \rightsquigarrow C \cong A
 *  \rightsquigarrow B \times C@f$, so the rows are the same; only @c >>,
 *  @c * and @c converse see where the factors sit.  Whole factors move: a
 *  sum's columns stay together. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> table(const RelExpr<K, Cs...>& r) {
  RelExpr<K, Cs...> out = r;
  out.cod = r.dom * r.cod;
  out.dom = Object{};
  return out;
}
/** @brief A relation read as the arrow of all its factors but the last (the
 *  @c Function[A, …, B] convention): @f$1 \rightsquigarrow A_1 \times \dots
 *  \times A_n@f$ as @f$A_1 \times \dots \times A_{n-1} \rightsquigarrow
 *  A_n@f$.  The same compact-closed iso as @c table, the other way. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> arrow(const RelExpr<K, Cs...>& r) {
  const Object all = r.dom * r.cod;
  if (all.factors.empty())
    throw dedekind::sets::not_admitted("arrow(R): R has no factor");
  RelExpr<K, Cs...> out = r;
  out.dom.factors.assign(all.factors.begin(), all.factors.end() - 1);
  out.cod.factors.assign(all.factors.end() - 1, all.factors.end());
  return out;
}

// ── The sum of two tables (#1039) ───────────────────────────────────────────

namespace detail_rows {
/** @brief A carrier's value @c c as a constant of the grammar: the constant
 *  type's own hook beside it where it has one (@c carrier_constant, found by
 *  argument-dependent lookup), else @c K(c).  A sum's tag and the defaults of
 *  its inactive side are written with it. */
template <typename K, typename C>
K as_constant(const C& c) {
  if constexpr (requires { carrier_constant(std::type_identity<K>{}, c); })
    return carrier_constant(std::type_identity<K>{}, c);
  else
    return K(c);
}
/** @brief @c as_constant as a visitor over a row's cells. */
template <typename K>
struct AsConstant {
  template <typename C>
  K operator()(const C& c) const {
    return as_constant<K>(c);
  }
};
/** @brief The default of the sort @c sort as a constant: the representative a
 *  sum's inactive side holds. */
template <typename K, typename... Cs>
K default_constant(std::size_t sort) {
  return std::visit(AsConstant<K>{},
                    dedekind::sets::row_of_sorts<Cs...>({sort}).cells.front());
}
/** @brief The sort of the 𝔹 carrier among the columns' carriers: a sum's tag.
 */
template <typename... Cs>
constexpr std::size_t tag_sort() {
  constexpr bool is_bool[] = {std::same_as<Cs, bool>...};
  for (std::size_t i = 0; i < sizeof...(Cs); ++i)
    if (is_bool[i]) return i;
  return sizeof...(Cs);
}
}  // namespace detail_rows

/** @brief @f$A + B@f$: the sum of two tables (sets), a table over the object
 *  @f$X + Y@f$, its χ the copairing @f$[\chi_A, \chi_B]@f$ on the tag.
 *  @throws not_admitted for a relation with a domain (a sum of arrows is
 *  not taken yet). */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> sum(const RelExpr<K, Cs...>& a, const RelExpr<K, Cs...>& b) {
  using R = RelExpr<K, Cs...>;
  static_assert(detail_rows::tag_sort<Cs...>() < sizeof...(Cs),
                "a sum's tag is a 𝔹 column: bool among the carriers");
  if (!a.dom.factors.empty() || !b.dom.factors.empty())
    throw dedekind::sets::not_admitted("A + B: the sum of two tables (sets)");
  return R::binary(R::Kind::Sum, a, b, Object{},
                   sum(a.cod, b.cod, detail_rows::tag_sort<Cs...>()));
}

// ── The named relations: η and the Frobenius spiders ────────────────────────

namespace detail_rows {
/** @brief @c ∧_i π_{from + i} == π_{to + i} over @c n columns: the equations
 *  identifying two copies of an object. */
template <typename K>
dedekind::sets::Pred<K> identified(
    unsigned n, unsigned from, unsigned to,
    dedekind::sets::Pred<K> onto = dedekind::sets::Pred<K>::top()) {
  using P = dedekind::sets::Pred<K>;
  using T = dedekind::sets::Term<K>;
  P out = std::move(onto);
  for (unsigned i = 1; i <= n; ++i) {
    const P eq =
        P::cmp(dedekind::sets::Rel::Eq, T::proj(from + i), T::proj(to + i));
    out = out.kind == P::Kind::Top ? eq : P::conj(out, eq);
  }
  return out;
}
}  // namespace detail_rows

/** @brief @c η: the identity @f$\Delta_A = \{(x, x)\}@f$, the unit of
 *  @c >>.  @param obj the object. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> diag(const Object& obj) {
  using R = RelExpr<K, Cs...>;
  const unsigned n = static_cast<unsigned>(columns(obj).size());
  return (R::universe(obj, obj) & detail_rows::identified<K>(n, 0, n));
}
/** @brief @c Copy: @f$A \rightsquigarrow A \times A@f$, @f$x \mapsto (x,
 *  x)@f$, the comonoid of the Frobenius structure.  @param obj the object. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> copy(const Object& obj) {
  using R = RelExpr<K, Cs...>;
  const unsigned n = static_cast<unsigned>(columns(obj).size());
  return (R::universe(obj, obj * obj) &
          detail_rows::identified<K>(n, 0, 2 * n,
                                     detail_rows::identified<K>(n, 0, n)));
}
/** @brief @c Merge: @f$A \times A \rightsquigarrow A@f$, the converse of
 *  @c copy.  @param obj the object. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> merge(const Object& obj) {
  return converse(copy<K, Cs...>(obj));
}
/** @brief The structural projection @f$\pi : A \rightsquigarrow A[keep]@f$,
 *  the columns @c keep (1-based) of the object in that order.  @throws
 *  not_admitted on a repeated or absent column. */
export template <typename K, typename... Cs>
RelExpr<K, Cs...> proj(const Object& obj, const std::vector<unsigned>& keep) {
  using R = RelExpr<K, Cs...>;
  using P = typename R::P;
  using T = typename R::T;
  const Columns cols = columns(obj);
  const unsigned n = static_cast<unsigned>(cols.size());
  std::vector<bool> seen(n + 1, false);
  Columns cod;
  P eqs = P::top();
  for (std::size_t j = 0; j < keep.size(); ++j) {
    const unsigned c = keep[j];
    if (c == 0 || c > n || seen[c])
      throw dedekind::sets::not_admitted(
          "proj(A, c, …): distinct columns 1 … n");
    seen[c] = true;
    cod.push_back(cols[c - 1]);
    const P eq = P::cmp(dedekind::sets::Rel::Eq,
                        T::proj(n + static_cast<unsigned>(j) + 1), T::proj(c));
    eqs = eqs.kind == P::Kind::Top ? eq : P::conj(eqs, eq);
  }
  return (R::universe(obj, Object(cod)) & eqs);
}

// ── The predicate: a reading of the term ────────────────────────────────────

namespace detail_rows {
/** @brief A comprehension over a universe reads as its predicate (⊤ is the unit
 * of ∧: the one identity the reading applies); over anything else, the
 *  conjunction. */
template <typename K>
dedekind::sets::Pred<K> conjoined(const dedekind::sets::Pred<K>& base,
                                  const dedekind::sets::Pred<K>& p) {
  using Pr = dedekind::sets::Pred<K>;
  if (base.kind == Pr::Kind::Top) return p;
  if (p.kind == Pr::Kind::Top) return base;
  return Pr::conj(base, p);
}
/** @brief A slot map by three ranges: the domain's columns, the codomain's,
 *  and the slots past the arity (a factor's own bound slots). */
template <typename K>
struct Ranges {
  unsigned dom, arity;
  int on_dom, on_cod, on_bound;  // the shift of each range
  dedekind::sets::Term<K> operator()(unsigned s) const {
    const int shift = s == 0       ? on_dom + 1
                      : s <= dom   ? on_dom
                      : s <= arity ? on_cod
                                   : on_bound;
    return dedekind::sets::Term<K>::proj(
        static_cast<unsigned>(static_cast<int>(s == 0 ? 0 : s) + shift));
  }
};
/** @brief @c π_slot == @c b: a sum's tag picks its side, ⊥ the left. */
template <typename K>
dedekind::sets::Pred<K> tag_is(unsigned slot, bool b) {
  return dedekind::sets::Pred<K>::cmp(
      dedekind::sets::Rel::Eq, dedekind::sets::Term<K>::proj(slot),
      dedekind::sets::Term<K>::constant(as_constant<K>(b)));
}
/** @brief The columns of @c o from the slot @c first + 1 on at their defaults:
 *  the inactive side of a sum. */
template <typename K, typename... Cs>
dedekind::sets::Pred<K> at_defaults(const Object& o, unsigned first) {
  using Pr = dedekind::sets::Pred<K>;
  using T = dedekind::sets::Term<K>;
  const Columns cols = columns(o);
  Pr out = Pr::top();
  for (std::size_t i = 0; i < cols.size(); ++i)
    out = conjoined(out,
                    Pr::cmp(dedekind::sets::Rel::Eq,
                            T::proj(first + static_cast<unsigned>(i) + 1),
                            T::constant(default_constant<K, Cs...>(cols[i]))));
  return out;
}
/** @brief The columns of @c o from the slot @c first + 1 on encode a point of
 *  it: at each sum, the tag picks a side and the other holds its defaults,
 *  recursively.  ⊤ for an object without sums.  Every row of a product of
 *  leaves is a point; a row of a sum is one only so (a quantifier or a
 *  complement over the columns must not range over the others). */
template <typename K, typename... Cs>
dedekind::sets::Pred<K> encodes(const Object& o, unsigned first) {
  using Pr = dedekind::sets::Pred<K>;
  Pr out = Pr::top();
  unsigned at = first;
  for (const Factor& f : o.factors) {
    if (f.kind == Factor::Kind::Leaf) {
      ++at;
      continue;
    }
    const unsigned nl = static_cast<unsigned>(columns(*f.left).size());
    const unsigned nr = static_cast<unsigned>(columns(*f.right).size());
    const Pr left = conjoined(
        conjoined(tag_is<K>(at + 1, false), encodes<K, Cs...>(*f.left, at + 1)),
        at_defaults<K, Cs...>(*f.right, at + 1 + nl));
    const Pr right =
        conjoined(conjoined(tag_is<K>(at + 1, true),
                            encodes<K, Cs...>(*f.right, at + 1 + nl)),
                  at_defaults<K, Cs...>(*f.left, at + 1));
    out = conjoined(out, Pr::disj(left, right));
    at += 1 + nl + nr;
  }
  return out;
}
}  // namespace detail_rows

/** @brief The predicate of a relation on the columns of its domain then its
 *  codomain, @c π_1 … @c π_n: a reading of the term.  A universe ⊤ (over a
 *  sum, the rows that encode a point: @c encodes); a former
 *  its predicate on its base's; the lattice nodes their connectives (a
 * complement the negation normal form, ∃ and ∀ swapped); a converse its base's
 * with the two objects' columns exchanged; a tensor the conjunction, the
 * columns arranged domain, domain, codomain, codomain; a composite the ∃ over
 * every column of the middle object (the monad's μ), outermost first.  A
 *  quantifier binds the next slot past the columns, so a leg's own bound
 *  slots are renumbered with its columns and never captured. */
export template <typename K, typename... Cs>
dedekind::sets::Pred<K> predicate_of(const RelExpr<K, Cs...>& r) {
  using R = RelExpr<K, Cs...>;
  using P = dedekind::sets::Pred<K>;
  using Map = detail_rows::Ranges<K>;
  const auto d = [](const R& x) {
    return static_cast<unsigned>(columns(x.dom).size());
  };
  const auto c = [](const R& x) {
    return static_cast<unsigned>(columns(x.cod).size());
  };
  switch (r.kind) {
    case R::Kind::Universe:
      // ⊤ over a product of leaves; over a sum, the rows that encode points.
      return detail_rows::encodes<K, Cs...>(r.dom * r.cod, 0);
    case R::Kind::Comprehension:
      return detail_rows::conjoined(predicate_of(*r.a), r.predicate);
    case R::Kind::Meet:
      return P::conj(predicate_of(*r.a), predicate_of(*r.b));
    case R::Kind::Join:
      return P::disj(predicate_of(*r.a), predicate_of(*r.b));
    case R::Kind::Not:
      // The complement within the points: a row that encodes none stays out.
      return detail_rows::conjoined(
          detail_rows::encodes<K, Cs...>(r.dom * r.cod, 0),
          predicate_of(*r.a).negated());
    case R::Kind::Converse: {
      const R& x = *r.a;
      return predicate_of(x).map_slots(Map{
          d(x), x.arity(), static_cast<int>(c(x)), -static_cast<int>(d(x)), 0});
    }
    case R::Kind::Tensor: {
      // Columns: dom a, dom b, cod a, cod b.
      const R& x = *r.a;
      const R& y = *r.b;
      const P pa = predicate_of(x).map_slots(Map{d(x), x.arity(), 0,
                                                 static_cast<int>(d(y)),
                                                 static_cast<int>(y.arity())});
      const P pb = predicate_of(y).map_slots(
          Map{d(y), y.arity(), static_cast<int>(d(x)),
              static_cast<int>(d(x) + c(x)), static_cast<int>(x.arity())});
      return detail_rows::conjoined(pa, pb);
    }
    case R::Kind::Sum: {
      // Columns: the tag, then x's, then y's; each leg's bound slots past
      // the sum's width.  χ is the copairing on the tag, the inactive side at
      // its defaults: a row of the sum is a point.
      const R& x = *r.a;
      const R& y = *r.b;
      const unsigned nx = x.arity(), ny = y.arity();
      const P px =
          predicate_of(x).map_slots(Map{0, nx, 0, 1, static_cast<int>(1 + ny)});
      const P py = predicate_of(y).map_slots(
          Map{0, ny, 0, static_cast<int>(1 + nx), static_cast<int>(1 + nx)});
      return P::disj(
          detail_rows::conjoined(
              detail_rows::conjoined(detail_rows::tag_is<K>(1, false), px),
              detail_rows::at_defaults<K, Cs...>(y.cod, 1 + nx)),
          detail_rows::conjoined(
              detail_rows::conjoined(detail_rows::tag_is<K>(1, true), py),
              detail_rows::at_defaults<K, Cs...>(x.cod, 1)));
    }
    case R::Kind::Compose: {
      // Columns: dom a, cod b; the middle (cod a = dom b, m columns) bound at
      // the slots n + 1 … n + m, outermost first.
      const R& x = *r.a;
      const R& y = *r.b;
      const unsigned n = r.arity(), m = c(x);
      const P pa = predicate_of(x).map_slots(
          Map{d(x), x.arity(), 0, static_cast<int>(n) - static_cast<int>(d(x)),
              static_cast<int>(c(y))});
      const P pb = predicate_of(y).map_slots(
          Map{d(y), y.arity(), static_cast<int>(n),
              static_cast<int>(d(x)) - static_cast<int>(m),
              static_cast<int>(d(x))});
      // The ∃ ranges over the middle's points: over a sum, the rows that
      // encode one (a row that encodes none would invent a witness).
      P body = detail_rows::conjoined(detail_rows::encodes<K, Cs...>(x.cod, n),
                                      detail_rows::conjoined(pa, pb));
      for (unsigned j = m; j >= 1; --j)
        body = P::exists(columns(x.cod)[j - 1], n + j, body);
      return body;
    }
  }
  return P::top();
}
/** @brief χ at a row of the columns, in K₃: the predicate reading with every
 *  quantifier an atom defines eliminated first (the fusion law), the rest
 *  searched within the relation's budget.  The one χ: Python binds this. */
export template <typename K, typename... Cs>
dedekind::category::Ternary verdict(
    const RelExpr<K, Cs...>& r, const typename RelExpr<K, Cs...>::Point& row) {
  using Point = typename RelExpr<K, Cs...>::Point;
  return dedekind::sets::verdict(
      dedekind::sets::eliminate_quantifiers<Point>(
          predicate_of(r), dedekind::sets::NoConstants{}, r.rep()),
      row, r.budget);
}
/** @brief The term of a relation certified a function of its first @c n − 1
 *  columns: a quantifier-free graph atom @c π_n @c == @c t(π_1 … π_{n−1})
 *  whose term lands in the last column's sort; @c nullptr otherwise.
 *  @param r the relation.  @param p its classifier, pinned or with its
 *  quantifiers eliminated. */
export template <typename K, typename... Cs>
const dedekind::sets::Term<K>* function_term(const RelExpr<K, Cs...>& r,
                                             const dedekind::sets::Pred<K>& p) {
  using Point = typename RelExpr<K, Cs...>::Point;
  if (r.arity() < 2 || dedekind::sets::has_quantifier(p)) return nullptr;
  const auto* t = dedekind::sets::graph_term(p, r.arity());
  if (t == nullptr) return nullptr;
  const std::size_t s = dedekind::sets::sort_of<Point>(*t, r.rep());
  const Columns all = r.columns();
  if (s != dedekind::sets::no_sort &&
      !dedekind::sets::sort_embeds<Point>(s, all.back()))
    return nullptr;
  return t;
}

// ── The laws of an arrow, inferred over the term (#1045) ─────────────────────
namespace detail_rows {
/** @brief The bit of a slot in a mask, as @c slots spells it. */
constexpr unsigned slot_bit(unsigned s) {
  return s < std::numeric_limits<unsigned>::digits ? 1u << s : ~0u;
}
constexpr unsigned column_mask(unsigned lo, unsigned hi) {
  unsigned m = 0;
  for (unsigned s = lo; s < hi; ++s) m |= slot_bit(s);
  return m;
}
/** @brief Whether each column in @c [lo, hi) is defined by a conjunct of @c p
 *  over the columns of @c others alone, by a term that lands in the column's
 *  sort (as @c function_term asks: a K₃ term defines no 𝔹 column), and
 *  whether nothing else is asked (the residual ⊤).
 *  @return (every column defined, residual ⊤). */
template <typename P, typename K>
std::pair<bool, bool> defined_columns(const dedekind::sets::Pred<K>& p,
                                      unsigned lo, unsigned hi, unsigned others,
                                      const Columns& cols, const P& rep) {
  using Q = dedekind::sets::Pred<K>;
  Q rest = p;
  for (unsigned s = lo; s < hi; ++s) {
    const auto d = dedekind::sets::defining_term(rest, s);
    if (!d || (dedekind::sets::slots(d->term) & ~others) != 0u)
      return {false, false};
    const std::size_t sort = dedekind::sets::sort_of<P>(d->term, rep);
    if (sort != dedekind::sets::no_sort &&
        !dedekind::sets::sort_embeds<P>(sort, cols[s - 1]))
      return {false, false};
    rest = d->rest;
  }
  return {true, rest.kind == Q::Kind::Top};
}
/** @brief The laws a comprehension certifies by the shape of its predicate:
 *  every codomain column defined by a term over the domain columns is
 *  univalence (at most one row above a point); with nothing else asked,
 *  totality too (an admitted operator is total on its carrier, so the term
 *  is a value at every point).  Dually, the domain columns defined over the
 *  codomain's: injective, and surjective.  @c Δ is all four; @c copy a map
 *  and injective; its converse @c merge univalent, injective and surjective.
 *  @param p the predicate on the columns.  @param nd the domain's columns.
 *  @param cols the sorts of all the columns.  @param rep a row of them. */
template <typename P, typename K>
Laws predicate_laws(const dedekind::sets::Pred<K>& p, unsigned nd,
                    const Columns& cols, const P& rep) {
  const unsigned n = static_cast<unsigned>(cols.size());
  Laws l;
  const auto [cod_defined, cod_only] =
      defined_columns(p, nd + 1, n + 1, column_mask(1, nd + 1), cols, rep);
  const auto [dom_defined, dom_only] =
      defined_columns(p, 1, nd + 1, column_mask(nd + 1, n + 1), cols, rep);
  if (cod_defined) {
    l.set(Law::Univalent);
    if (cod_only) l.set(Law::Total);
  }
  if (dom_defined) {
    l.set(Law::Injective);
    if (dom_only) l.set(Law::Surjective);
  }
  return l;
}
/** @brief The laws of @c r read at the split @c nd (its first @c nd columns
 *  the domain): a leaf by its shape, a node by the engine (@c Laws::node,
 *  @c :dyadic) on its legs' laws.  The split belongs to the node being read,
 *  so a lattice node's legs share its columns, a converse's leg has them
 *  transposed, and only the legs of @c ⊗ @c ; @c + carry their own (@c arrow
 *  re-splits a root, not its legs). */
template <typename K, typename... Cs>
Laws laws_at(const RelExpr<K, Cs...>& r, unsigned nd) {
  using R = RelExpr<K, Cs...>;
  const auto own = [](const R& x) {
    return static_cast<unsigned>(columns(x.dom).size());
  };
  const auto legs = [&](Node n) {
    return Laws::node(n, laws_at(*r.a, own(*r.a)), laws_at(*r.b, own(*r.b)));
  };
  switch (r.kind) {
    case R::Kind::Universe:
      // The full relation on inhabited carriers: total and surjective.
      return Laws::from(true, false, false, true);
    case R::Kind::Comprehension: {
      const Laws here = predicate_laws(r.predicate, nd, r.columns(), r.rep());
      // {x ∈ U | p}: the universe adds nothing; R & p is a meet.
      return r.a->kind == R::Kind::Universe
                 ? here
                 : Laws::node(Node::Meet, laws_at(*r.a, nd), here);
    }
    case R::Kind::Meet:
      return Laws::node(Node::Meet, laws_at(*r.a, nd), laws_at(*r.b, nd));
    case R::Kind::Join:
      return Laws::node(Node::Join, laws_at(*r.a, nd), laws_at(*r.b, nd));
    case R::Kind::Not:
      return Laws::node(Node::Not, laws_at(*r.a, nd));
    case R::Kind::Converse:
      return Laws::node(Node::Converse, laws_at(*r.a, r.arity() - nd));
    case R::Kind::Tensor:
      return legs(Node::Tensor);
    case R::Kind::Compose:
      return legs(Node::Compose);
    case R::Kind::Sum:
      return legs(Node::Sum);
  }
  return Laws{};
}
}  // namespace detail_rows

/** @brief The laws of a relation, inferred: a fold over the term reading the
 *  engine (@c infer, @c :dyadic) at each node, the leaves certified by
 *  their shape (@c detail_rows::predicate_laws), at the relation's own split
 *  of its columns into domain and codomain.  @c True where a rule infers the
 *  law, @c Unknown where none does; the normal form may still decide the
 *  inclusion (@c algebra, @c canonical).  @param r the relation. */
export template <typename K, typename... Cs>
Laws laws(const RelExpr<K, Cs...>& r) {
  return detail_rows::laws_at(r, static_cast<unsigned>(columns(r.dom).size()));
}

/** @brief The relations certified the law @c W: a datum on the universe of
 *  relations, so @c 𝔸<Re>{} @c & @c Total<Re>{} is the set of total
 *  relations, membership its χ in K₃ (inferred, or not), and a map or an
 *  isomorphism a meet of such sets, nothing stored.  Its χ obeys the facts
 *  the set is declared: closed under @c >>, so @c S(f) @c ∧ @c S(g) @c ≤
 *  @c S(f @c >> @c g); a down-set or an up-set in ⊆; carried by @c ° to its
 *  dagger law.  Its complement is never inhabited by inference (@c ¬True,
 *  else @c Unknown): Σ, not Π.
 *  @tparam Re the relation type.  @tparam W the law. */
export template <typename Re, Law W>
struct LawSet {
  using Domain = Re;
  using Codomain = dedekind::category::Ternary;
  Codomain operator()(const Re& r) const { return laws(r)[W]; }
};
export template <typename Re>
using Total = LawSet<Re, Law::Total>;
export template <typename Re>
using Univalent = LawSet<Re, Law::Univalent>;
export template <typename Re>
using Injective = LawSet<Re, Law::Injective>;
export template <typename Re>
using Surjective = LawSet<Re, Law::Surjective>;

}  // namespace dedekind::relational
