/**
 * @file dedekind/relational/rows.cppm
 * @partition :rows
 * @brief Relations of any arity as terms built at run time, their schema as
 *        data: a table (#1037).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section rows__The_Term
 * A relation built at run time is a @b term over relations, the run-time twin
 * of the typed set expressions.  Its nodes are the set grammar's, two of the
 * relational calculus's, and the quantifier:
 *
 * | node | typed twin | built by |
 * |---|---|---|
 * | @c Universe | @c 𝔸 over a product of carriers | @c U @c * @c V of universes
 * | | @c Where | @c Comprehension, the one set former | @c R @c \| @c p | | @c
 * Meet, @c Join, @c Not | @c category::Meet / @c Join / @c Not | @c & @c \| @c
 * ~ | | @c Tensor | @c cartesian_product (the bicategory's ⊗, columns
 * regrouped) | @c * | | @c Compose | @c relational:dyadic's @c >> | @c >> | |
 * @c Exists | the ∃ of a projection | @c project |
 *
 * Nothing is flattened at construction: @c R @c & @c S is a @c Meet node over
 * two relations, not one predicate.  The @b predicate of a relation is a
 * @b reading (@c predicate, a fold), as are its verdict and its written form;
 * @c canonical (algebra) normalises that reading.  Rewrites on the tree
 * (filter fusion, the associativity of @c >>, distribution over @c |) are the
 * optimisation phase, after this.
 *
 * @section rows__The_Kleisli_Structure
 * A relation @f$A \rightsquigarrow B@f$ is the Kleisli arrow @f$A \to
 * \mathcal{P}B@f$ of the powerset monad, and the relational operations are the
 * monad's:
 *
 * | monad | relation | here |
 * |---|---|---|
 * | @f$\eta : A \to \mathcal{P}A@f$, @f$a \mapsto \{a\}@f$ | the identity
 * relation @f$\Delta@f$ | @c diag | | Kleisli composition | the relative
 * product @f$R;S@f$ | @c >> | | @f$\mu@f$, the union | the @f$\exists@f$ over
 * the middle | @c Pred::exists in @c predicate(R @c >> @c S) |
 *
 * The monad laws are the relational ones, decided by @c canonical where the
 * ∃ eliminates: @f$\Delta;R = R = R;\Delta@f$ (unit) and @f$(R;S);T =
 * R;(S;T)@f$ (associativity).  They are the laws the rewrite phase may use.
 *
 * @section rows__The_Frobenius_Structure
 * Relations form a cartesian bicategory: every carrier has @c Copy @f$A \to
 * A \times A@f$ and @c Merge, its converse, satisfying the Frobenius law, and
 * @f$R \cap S = \mathrm{Merge} \circ (R \otimes S) \circ \mathrm{Copy}@f$
 * (@c category:cartesian_bicategory, #946).  Here @c Tensor is the ⊗; @c Copy
 * and @c Merge compose along @b two columns, which @c >> (one middle column)
 * does not express: the join along several columns is the next node (#798),
 * and the Frobenius witness lands with it.
 */
module;

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

export module dedekind.relational:rows;

import dedekind.category; // Ternary
import dedekind.sets;     // Row, Term, Pred, the sorts, graph_term

namespace dedekind::relational {

/** @brief A relation of any arity built at run time: a term over relations,
 *  its schema (a sort per column) fixed at construction, its predicate a
 *  reading.  @tparam K the constants' type.  @tparam Cs the carriers a
 *  column may have. */
export template <typename K, typename... Cs>
struct Rel {
  using Point = dedekind::sets::Row<Cs...>;
  using P = dedekind::sets::Pred<K>;
  using T = dedekind::sets::Term<K>;
  enum class Kind { Universe, Where, Meet, Join, Not, Tensor, Compose, Exists };

  Kind kind{Kind::Universe};
  std::vector<std::size_t> schema;  // a sort per column: an index into Cs
  P predicate = P::top();           // Where: the filter; Universe: ⊤
  std::shared_ptr<const Rel> a, b;  // the legs
  std::vector<unsigned> keep;       // Exists: the columns kept, in order
  std::uint64_t budget = std::uint64_t{1} << 16;  // verdict's search

  unsigned arity() const { return static_cast<unsigned>(schema.size()); }
  /** @brief A row of the schema: the points' representative. */
  Point rep() const { return dedekind::sets::row_of_sorts<Cs...>(schema); }

  /** @brief The universe over the columns @c schema. */
  static Rel universe(std::vector<std::size_t> schema) {
    Rel r;
    r.schema = std::move(schema);
    return r;
  }
  /** @brief A relation given by its predicate on its columns: the former
   *  @c Universe @c | @c p. */
  static Rel leaf(std::vector<std::size_t> schema, P p,
                  std::uint64_t budget = std::uint64_t{1} << 16) {
    Rel r = universe(std::move(schema));
    r.budget = budget;
    return node(Kind::Where, std::move(r), std::move(p));
  }
  static Rel node(Kind k, Rel x, P p = P::top()) {
    Rel r;
    r.kind = k;
    r.schema = x.schema;
    r.budget = x.budget;
    r.predicate = std::move(p);
    r.a = std::make_shared<const Rel>(std::move(x));
    return r;
  }
  static Rel node(Kind k, Rel x, Rel y, std::vector<std::size_t> schema) {
    Rel r;
    r.kind = k;
    r.schema = std::move(schema);
    r.budget = x.budget;
    r.a = std::make_shared<const Rel>(std::move(x));
    r.b = std::make_shared<const Rel>(std::move(y));
    return r;
  }
};

// ── The set grammar on relations ────────────────────────────────────────────

/** @brief @c R @c | @c p: the former, every slot, constant and operator of
 *  @c p checked in its column's sort now.  @throws not_admitted for a slot
 *  past the columns, or an operator not admitted. */
export template <typename K, typename... Cs>
Rel<K, Cs...> where(const Rel<K, Cs...>& r, const dedekind::sets::Pred<K>& p) {
  if (!dedekind::sets::only_arguments(dedekind::sets::slots(p), r.arity() + 1))
    throw dedekind::sets::not_admitted(
        "R | pred: a slot past the relation's columns");
  dedekind::sets::bind_check<typename Rel<K, Cs...>::Point>(p, r.rep());
  return Rel<K, Cs...>::node(Rel<K, Cs...>::Kind::Where, r, p);
}
/** @brief @c R @c & @c S and @c R @c | @c S over one product: the lattice's
 *  nodes.  @throws not_admitted over two products. */
export template <typename K, typename... Cs>
Rel<K, Cs...> operator&(const Rel<K, Cs...>& a, const Rel<K, Cs...>& b) {
  if (a.schema != b.schema)
    throw dedekind::sets::not_admitted("relations over different products");
  return Rel<K, Cs...>::node(Rel<K, Cs...>::Kind::Meet, a, b, a.schema);
}
export template <typename K, typename... Cs>
Rel<K, Cs...> operator|(const Rel<K, Cs...>& a, const Rel<K, Cs...>& b) {
  if (a.schema != b.schema)
    throw dedekind::sets::not_admitted("relations over different products");
  return Rel<K, Cs...>::node(Rel<K, Cs...>::Kind::Join, a, b, a.schema);
}
/** @brief @c ~R: the complement. */
export template <typename K, typename... Cs>
Rel<K, Cs...> operator~(const Rel<K, Cs...>& a) {
  return Rel<K, Cs...>::node(Rel<K, Cs...>::Kind::Not, a);
}

// ── The relational calculus: ⊗, Kleisli composition, ∃ ──────────────────────

/** @brief @c R @c * @c S: the tensor, the schemas concatenated.  Two
 *  universes make the product universe. */
export template <typename K, typename... Cs>
Rel<K, Cs...> operator*(const Rel<K, Cs...>& a, const Rel<K, Cs...>& b) {
  using R = Rel<K, Cs...>;
  std::vector<std::size_t> schema = a.schema;
  schema.insert(schema.end(), b.schema.begin(), b.schema.end());
  if (a.kind == R::Kind::Universe && b.kind == R::Kind::Universe)
    return R::universe(std::move(schema));
  return R::node(R::Kind::Tensor, a, b, std::move(schema));
}
/** @brief @c R @c >> @c S: Kleisli composition, the relative product
 *  @f$\{(a, c) \mid \exists m.\, R(a, m) \wedge S(m, c)\}@f$, @c R's last
 *  column and @c S's first the middle.  @throws not_admitted where those two
 *  columns differ in carrier. */
export template <typename K, typename... Cs>
Rel<K, Cs...> operator>>(const Rel<K, Cs...>& a, const Rel<K, Cs...>& b) {
  using R = Rel<K, Cs...>;
  if (a.arity() == 0 || b.arity() == 0 || a.schema.back() != b.schema.front())
    throw dedekind::sets::not_admitted(
        "R >> S: R's last column and S's first are of one carrier");
  std::vector<std::size_t> schema(a.schema.begin(), a.schema.end() - 1);
  schema.insert(schema.end(), b.schema.begin() + 1, b.schema.end());
  return R::node(R::Kind::Compose, a, b, std::move(schema));
}
/** @brief @c η: the identity relation @f$\Delta = \{(x, x)\}@f$ on a
 *  carrier, the unit of @c >>.  @param sort the carrier's sort. */
export template <typename K, typename... Cs>
Rel<K, Cs...> diag(std::size_t sort) {
  using R = Rel<K, Cs...>;
  using P = typename R::P;
  using T = typename R::T;
  return where(R::universe({sort, sort}),
               P::cmp(dedekind::sets::Rel::Eq, T::proj(1), T::proj(2)));
}
/** @brief The columns @c keep (1-based) in that order, the others quantified
 *  away by ∃.  @throws not_admitted on a repeated or absent column. */
export template <typename K, typename... Cs>
Rel<K, Cs...> project(const Rel<K, Cs...>& r,
                      const std::vector<unsigned>& keep) {
  using R = Rel<K, Cs...>;
  std::vector<bool> seen(r.arity() + 1, false);
  std::vector<std::size_t> schema;
  for (const unsigned c : keep) {
    if (c == 0 || c > r.arity() || seen[c])
      throw dedekind::sets::not_admitted(
          "project(R, c, …): distinct columns 1 … n");
    seen[c] = true;
    schema.push_back(r.schema[c - 1]);
  }
  R out = R::node(R::Kind::Exists, r);
  out.schema = std::move(schema);
  out.keep = keep;
  return out;
}

// ── The predicate: a reading of the term ────────────────────────────────────

namespace detail_rows {
template <typename K>
dedekind::sets::Pred<K> meet(const dedekind::sets::Pred<K>& p,
                             const dedekind::sets::Pred<K>& q) {
  using Pr = dedekind::sets::Pred<K>;
  if (p.kind == Pr::Kind::Top) return q;
  if (q.kind == Pr::Kind::Top) return p;
  return Pr::conj(p, q);
}
/** @brief A tensor's left factor: a bound slot (past its arity) moves past
 *  the right factor's columns as well. */
template <typename K>
struct LeftOfTensor {
  unsigned arity, right;
  dedekind::sets::Term<K> operator()(unsigned s) const {
    return dedekind::sets::Term<K>::proj(s <= arity ? s : s + right);
  }
};
/** @brief A tensor's right factor: every slot after the left's columns; the
 *  unary @c π of a set is its first column. */
template <typename K>
struct RightOfTensor {
  unsigned left;
  dedekind::sets::Term<K> operator()(unsigned s) const {
    return dedekind::sets::Term<K>::proj((s == 0 ? 1 : s) + left);
  }
};
/** @brief A composite's left factor @c R (arity @c r) in @c R @c >> @c S
 *  (arity @c s): its last column is the middle, the first slot past the
 *  result's @c n @c = @c r @c + @c s @c − @c 2 columns; its bound slots
 *  follow. */
template <typename K>
struct LeftOfCompose {
  unsigned r, s;
  dedekind::sets::Term<K> operator()(unsigned i) const {
    const unsigned n = r + s - 2;
    return dedekind::sets::Term<K>::proj(i < r    ? i
                                         : i == r ? n + 1
                                                  : i + s - 1);
  }
};
/** @brief A composite's right factor: its first column is the middle, the
 *  rest follow the left's columns, its bound slots follow all. */
template <typename K>
struct RightOfCompose {
  unsigned r, s;
  dedekind::sets::Term<K> operator()(unsigned i) const {
    const unsigned n = r + s - 2;
    return dedekind::sets::Term<K>::proj(i == 1   ? n + 1
                                         : i <= s ? i + r - 2
                                                  : i + r - 1);
  }
};
/** @brief A renumbering by a table: π_i ↦ π_{to[i]}, the identity past it. */
template <typename K>
struct Renumber {
  const std::vector<unsigned>& to;
  dedekind::sets::Term<K> operator()(unsigned s) const {
    return dedekind::sets::Term<K>::proj(s < to.size() ? to[s] : s);
  }
};
}  // namespace detail_rows

/** @brief The predicate of a relation on its columns @c π_1 … @c π_n, a
 *  reading of the term: a universe ⊤, a former its filter met with its
 *  base's, the lattice nodes their connectives (a complement the negation
 *  normal form, ∃ and ∀ swapped), a tensor the conjunction with the right
 *  factor's slots shifted, a composite the ∃ over the middle (the monad's
 *  @f$\mu@f$), a projection the ∃ over the dropped columns.  A quantifier
 *  binds the next slot past the columns, so a factor's own bound slots are
 *  renumbered with its columns and never captured.  @param r the relation. */
export template <typename K, typename... Cs>
dedekind::sets::Pred<K> predicate(const Rel<K, Cs...>& r) {
  using R = Rel<K, Cs...>;
  using P = dedekind::sets::Pred<K>;
  switch (r.kind) {
    case R::Kind::Universe:
      return P::top();
    case R::Kind::Where:
      return detail_rows::meet(predicate(*r.a), r.predicate);
    case R::Kind::Meet:
      return detail_rows::meet(predicate(*r.a), predicate(*r.b));
    case R::Kind::Join:
      return P::disj(predicate(*r.a), predicate(*r.b));
    case R::Kind::Not:
      return predicate(*r.a).negated();
    case R::Kind::Tensor:
      return detail_rows::meet(
          predicate(*r.a).map_slots(
              detail_rows::LeftOfTensor<K>{r.a->arity(), r.b->arity()}),
          predicate(*r.b).map_slots(
              detail_rows::RightOfTensor<K>{r.a->arity()}));
    case R::Kind::Compose: {
      const unsigned n = r.arity();
      return P::exists(
          r.a->schema.back(), n + 1,
          detail_rows::meet(
              predicate(*r.a).map_slots(
                  detail_rows::LeftOfCompose<K>{r.a->arity(), r.b->arity()}),
              predicate(*r.b).map_slots(
                  detail_rows::RightOfCompose<K>{r.a->arity(), r.b->arity()})));
    }
    case R::Kind::Exists: {
      // The kept columns first, the dropped ones after them, bound outermost
      // first; the base's own bound slots (past its arity) keep their numbers.
      const unsigned base = r.a->arity(), n = r.arity();
      std::vector<unsigned> to(base + 1, 0);
      for (unsigned i = 0; i < n; ++i) to[r.keep[i]] = i + 1;
      std::vector<std::size_t> dropped;
      for (unsigned c = 1; c <= base; ++c)
        if (to[c] == 0) {
          dropped.push_back(r.a->schema[c - 1]);
          to[c] = n + static_cast<unsigned>(dropped.size());
        }
      P body = predicate(*r.a).map_slots(detail_rows::Renumber<K>{to});
      for (unsigned j = static_cast<unsigned>(dropped.size()); j >= 1; --j)
        body = P::exists(dropped[j - 1], n + j, body);
      return body;
    }
  }
  return P::top();
}
/** @brief χ at a row of the schema, in K₃: the verdict of the predicate
 *  reading within the relation's budget. */
export template <typename K, typename... Cs>
dedekind::category::Ternary verdict(const Rel<K, Cs...>& r,
                                    const typename Rel<K, Cs...>::Point& row) {
  return dedekind::sets::verdict(predicate(r), row, r.budget);
}
/** @brief The term of a relation certified a function of its first @c n − 1
 *  columns: a quantifier-free graph atom @c π_n @c == @c t(π_1 … π_{n−1})
 *  whose term lands in the last column's sort; @c nullptr otherwise.
 *  @param r the relation.  @param p its predicate reading, pinned or with its
 *  quantifiers eliminated. */
export template <typename K, typename... Cs>
const dedekind::sets::Term<K>* function_term(const Rel<K, Cs...>& r,
                                             const dedekind::sets::Pred<K>& p) {
  using Point = typename Rel<K, Cs...>::Point;
  if (r.arity() < 2 || dedekind::sets::has_quantifier(p)) return nullptr;
  const auto* t = dedekind::sets::graph_term(p, r.arity());
  if (t == nullptr) return nullptr;
  const std::size_t s = dedekind::sets::sort_of<Point>(*t, r.rep());
  if (s != dedekind::sets::no_sort &&
      !dedekind::sets::sort_embeds<Point>(s, r.schema.back()))
    return nullptr;
  return t;
}

}  // namespace dedekind::relational
