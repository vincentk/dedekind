/**
 * @file dedekind/relational/rows.cppm
 * @partition :rows
 * @brief Relations of any arity whose schema is data: a table (#1037).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section rows__Model
 * A relation over a product of carriers, built at run time, is a @b schema (a
 * sort per column) and a @b predicate on its columns @c π_1 … @c π_n
 * (@c sets:terms' run-time @c Pred): the one set former @c {x ∈ 𝔸 | P}, over
 * the run-time universe the schema names.  A table is a multi-sorted
 * relation; relations in Trsk are sets in Lwv, so the set grammar's @c | @c &
 * @c ~ carry over, and the relational operations are built @b symbolically on
 * the predicate:
 *
 * | operation | the predicate |
 * |---|---|
 * | @c R @c * @c S | @c P_R ∧ P_S, S's slots after R's |
 * | @c R @c >> @c S | @c ∃m. P_R(a, m) ∧ P_S(m, c): the relative product, the
 *   middle a quantified column (@c Pred::exists) |
 * | @c project(R, keep) | the dropped columns quantified away |
 * | @c ~R | the predicate negated: @c ∃ and @c ∀ swap (De Morgan) |
 *
 * The ∃ is @c sets:quantifier's @c exists in its run-time form: a relation is
 * the Kleisli arrow @f$A \to \mathcal{P}B@f$ of the powerset monad and @c >>
 * is Kleisli composition, the ∃ its multiplication.  Nothing is eliminated at
 * construction: @c canonical (algebra) eliminates a quantifier where an atom
 * defines its column or its carrier is a finite chain, and @c verdict reads
 * what remains as a bounded search (Σ for ∃, Π for ∀).  The witnesses of the
 * monad and of the Frobenius structure, and the rewrites on this term tree,
 * come after the predicate.
 */
module;

#include <cstddef>
#include <cstdint>
#include <vector>

export module dedekind.relational:rows;

import dedekind.category; // Ternary
import dedekind.sets;     // Row, Term, Pred, the sorts, graph_term

namespace dedekind::relational {

/** @brief A relation of any arity, its schema as data.  @tparam K the
 *  constants' type.  @tparam Cs the carriers a column may have. */
export template <typename K, typename... Cs>
struct Table {
  using Point = dedekind::sets::Row<Cs...>;
  using P = dedekind::sets::Pred<K>;
  using T = dedekind::sets::Term<K>;

  std::vector<std::size_t> schema;  // a sort per column: an index into Cs
  P predicate;                      // on π_1 … π_n, quantifiers past them
  std::uint64_t budget = std::uint64_t{1} << 16;  // verdict's search

  unsigned arity() const { return static_cast<unsigned>(schema.size()); }
  /** @brief A row of the schema: the points' representative. */
  Point rep() const { return dedekind::sets::row_of_sorts<Cs...>(schema); }
};

namespace detail_rows {
/** @brief The meet of two predicates, ⊤ a unit. */
template <typename K>
dedekind::sets::Pred<K> meet(const dedekind::sets::Pred<K>& p,
                             const dedekind::sets::Pred<K>& q) {
  using Pr = dedekind::sets::Pred<K>;
  if (p.kind == Pr::Kind::Top) return q;
  if (q.kind == Pr::Kind::Top) return p;
  return Pr::conj(p, q);
}
/** @brief The slots of a product's left factor: a bound slot (past its
 *  arity) moves past the right factor's columns as well. */
template <typename K>
struct LeftOf {
  unsigned arity, right;
  dedekind::sets::Term<K> operator()(unsigned s) const {
    return dedekind::sets::Term<K>::proj(s <= arity ? s : s + right);
  }
};
/** @brief The slots of a product's right factor: every one after the left's
 *  columns; the unary @c π of a set is its first column. */
template <typename K>
struct RightOf {
  unsigned left;
  dedekind::sets::Term<K> operator()(unsigned s) const {
    return dedekind::sets::Term<K>::proj((s == 0 ? 1 : s) + left);
  }
};
/** @brief The slots of a composite's left factor @c R (arity @c r) in
 *  @c R @c >> @c S (arity @c s): its last column is the middle, the first
 *  slot past the result's @c n @c = @c r @c + @c s @c − @c 2 columns; its
 *  bound slots follow. */
template <typename K>
struct LeftOfComposite {
  unsigned r, s;
  dedekind::sets::Term<K> operator()(unsigned i) const {
    const unsigned n = r + s - 2;
    return dedekind::sets::Term<K>::proj(i < r    ? i
                                         : i == r ? n + 1
                                                  : i + s - 1);
  }
};
/** @brief The slots of a composite's right factor: its first column is the
 *  middle, the rest follow the left's columns, its bound slots follow all. */
template <typename K>
struct RightOfComposite {
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

/** @brief @c R | p: the former, every slot, constant and operator of @c p
 *  checked in its column's sort now.  @throws not_admitted for a slot past
 *  the columns, or an operator not admitted. */
export template <typename K, typename... Cs>
Table<K, Cs...> where(const Table<K, Cs...>& r,
                      const dedekind::sets::Pred<K>& p) {
  if (!dedekind::sets::only_arguments(dedekind::sets::slots(p), r.arity() + 1))
    throw dedekind::sets::not_admitted(
        "R | pred: a slot past the relation's columns");
  dedekind::sets::bind_check<typename Table<K, Cs...>::Point>(p, r.rep());
  return {r.schema, detail_rows::meet(r.predicate, p), r.budget};
}
/** @brief @c R @c × @c S: the schemas concatenated, the predicates met. */
export template <typename K, typename... Cs>
Table<K, Cs...> operator*(const Table<K, Cs...>& a, const Table<K, Cs...>& b) {
  std::vector<std::size_t> schema = a.schema;
  schema.insert(schema.end(), b.schema.begin(), b.schema.end());
  return {
      std::move(schema),
      detail_rows::meet(
          a.predicate.map_slots(detail_rows::LeftOf<K>{a.arity(), b.arity()}),
          b.predicate.map_slots(detail_rows::RightOf<K>{a.arity()})),
      a.budget};
}
/** @brief @c R @c >> @c S: the relative product, @c {(a, c) | ∃m. R(a, m) ∧
 *  S(m, c)}, @c R's last column and @c S's first the quantified middle.
 *  @throws not_admitted where those two columns differ in carrier. */
export template <typename K, typename... Cs>
Table<K, Cs...> operator>>(const Table<K, Cs...>& a, const Table<K, Cs...>& b) {
  using P = dedekind::sets::Pred<K>;
  if (a.arity() == 0 || b.arity() == 0 || a.schema.back() != b.schema.front())
    throw dedekind::sets::not_admitted(
        "R >> S: R's last column and S's first are of one carrier");
  const unsigned r = a.arity(), s = b.arity(), n = r + s - 2;
  std::vector<std::size_t> schema(a.schema.begin(), a.schema.end() - 1);
  schema.insert(schema.end(), b.schema.begin() + 1, b.schema.end());
  const P body = detail_rows::meet(
      a.predicate.map_slots(detail_rows::LeftOfComposite<K>{r, s}),
      b.predicate.map_slots(detail_rows::RightOfComposite<K>{r, s}));
  return {std::move(schema), P::exists(a.schema.back(), n + 1, body), a.budget};
}
/** @brief @c R @c & @c S and @c R @c | @c S over one product.  @throws
 *  not_admitted over two. */
export template <typename K, typename... Cs>
Table<K, Cs...> operator&(const Table<K, Cs...>& a, const Table<K, Cs...>& b) {
  if (a.schema != b.schema)
    throw dedekind::sets::not_admitted("relations over different products");
  return {a.schema, detail_rows::meet(a.predicate, b.predicate), a.budget};
}
export template <typename K, typename... Cs>
Table<K, Cs...> operator|(const Table<K, Cs...>& a, const Table<K, Cs...>& b) {
  if (a.schema != b.schema)
    throw dedekind::sets::not_admitted("relations over different products");
  return {a.schema, dedekind::sets::Pred<K>::disj(a.predicate, b.predicate),
          a.budget};
}
/** @brief @c ~R: the complement, the predicate in negation normal form (∃
 *  and ∀ swap). */
export template <typename K, typename... Cs>
Table<K, Cs...> operator~(const Table<K, Cs...>& a) {
  return {a.schema, a.predicate.negated(), a.budget};
}
/** @brief The columns @c keep (1-based) in that order, the others quantified
 *  away by ∃, outermost first.  @throws not_admitted on a repeated or absent
 *  column. */
export template <typename K, typename... Cs>
Table<K, Cs...> project(const Table<K, Cs...>& r,
                        const std::vector<unsigned>& keep) {
  using P = dedekind::sets::Pred<K>;
  std::vector<unsigned> to(r.arity() + 1, 0);
  std::vector<std::size_t> schema, dropped;
  for (const unsigned c : keep) {
    if (c == 0 || c > r.arity() || to[c] != 0)
      throw dedekind::sets::not_admitted(
          "project(R, c, …): distinct columns 1 … n");
    to[c] = static_cast<unsigned>(schema.size()) + 1;
    schema.push_back(r.schema[c - 1]);
  }
  const unsigned n = static_cast<unsigned>(schema.size());
  for (unsigned c = 1; c <= r.arity(); ++c)
    if (to[c] == 0) {
      dropped.push_back(r.schema[c - 1]);
      to[c] = n + static_cast<unsigned>(dropped.size());
    }
  // The dropped columns take the slots n + 1 … arity, so the predicate's own
  // bound slots (past the arity) keep their numbers.
  P body = r.predicate.map_slots(detail_rows::Renumber<K>{to});
  for (unsigned j = static_cast<unsigned>(dropped.size()); j >= 1; --j)
    body = P::exists(dropped[j - 1], n + j, body);
  return {std::move(schema), body, r.budget};
}
/** @brief χ at a row of the schema, in K₃ (@c sets::verdict within the
 *  table's budget). */
export template <typename K, typename... Cs>
dedekind::category::Ternary verdict(
    const Table<K, Cs...>& r, const typename Table<K, Cs...>::Point& row) {
  return dedekind::sets::verdict(r.predicate, row, r.budget);
}
/** @brief The term of a relation certified a function of its first @c n − 1
 *  columns: a quantifier-free graph atom @c π_n @c == @c t(π_1 … π_{n−1})
 *  whose term lands in the last column's sort; @c nullptr otherwise.
 *  @param r the relation.  @param p its predicate, or that pinned or
 *  normalised. */
export template <typename K, typename... Cs>
const dedekind::sets::Term<K>* function_term(const Table<K, Cs...>& r,
                                             const dedekind::sets::Pred<K>& p) {
  using Point = typename Table<K, Cs...>::Point;
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
