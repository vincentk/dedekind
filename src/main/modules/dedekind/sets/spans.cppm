/**
 * @file spans.cppm
 * @partition :spans
 * @brief Relations of any arity as terms (#1037): a schema as data, held as a
 *        span until its image is taken.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section spans__Model
 * A relation built at run time over a product of carriers is a @b span in
 * prenex form: its visible columns @c π_1 … @c π_n (the schema, a sort per
 * column), hidden columns @c π_{n+1} … @c π_{n+k} bound by @c ∃ or @c ∀,
 * outermost first, and one quantifier-free predicate on all of them.  The
 * points are @c Row<Cs...>: a row is its own schema.
 *
 * | operation | the span |
 * |---|---|
 * | @c a.times(b) | schemas concatenated, the hidden columns after both |
 * | @c a.meet(b), @c a.join(b) | one product; hidden columns renamed apart |
 * | @c a.then(b) | @c a's last column and @c b's first identified and hidden
 *   (@c ∃): the composite, kept as the pullback over the middle |
 * | @c r.project(keep) | the dropped columns hidden (@c ∃), outermost |
 * | @c r.complement() | by duality: the predicate negated, each @c ∃ a @c ∀ |
 *
 * Building a span is free.  Its @b image (@c fused) eliminates the hidden
 * columns, innermost block first: by substitution where an atom defines one
 * (@c defining_term: the fusion law), else by Shannon's expansion on a finite
 * chain.  Where a column remains, membership (@c verdict) searches within a
 * budget: a witness of @c ∃ is ⊤, a counterexample to @c ∀ is ⊥, an
 * exhausted budget U (Σ, Π); alternating quantifiers answer U.
 *
 * The full Tarski relation algebra has an undecidable equational theory, so
 * no canonical form is promised beyond the image; equality is decided where
 * both images exist (by the caller's normal form) and refused otherwise.
 */
module;

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

export module dedekind.sets:spans;

import dedekind.category; // Ternary, IsPst, Successor
import :boundaries;       // chain_bottom / chain_top: a chain's ends
import :terms;            // Term, Pred, Row, the sorts, defining_term

namespace dedekind::sets {

using dedekind::category::Ternary;

/** @brief A hidden column: its sort (an index into the row's carriers) and
 *  its quantifier, @c ∃ or @c ∀. */
export struct Hidden {
  std::size_t sort;
  bool universal = false;
};

/** @brief The @c i-th value of a carrier for the bounded search: an unsigned
 *  word counts up, a finite chain cycles through its values from ⊥, and any
 *  other carrier states its enumeration as @c nth_value(type_identity<C>, i),
 *  found by ADL beside its definition.  @tparam C the carrier.  @param i the
 *  index. */
export template <typename C>
C search_value(std::uint64_t i) {
  if constexpr (dedekind::category::IsPst<C>) {
    std::uint64_t n = 1;
    for (C v = chain_bottom<C>(); v != chain_top<C>();
         v = dedekind::category::Successor<C>{}(v))
      ++n;
    C v = chain_bottom<C>();
    for (std::uint64_t s = i % n; s > 0; --s)
      v = dedekind::category::Successor<C>{}(v);
    return v;
  } else if constexpr (std::unsigned_integral<C>) {
    return static_cast<C>(i);
  } else if constexpr (requires { nth_value(std::type_identity<C>{}, i); }) {
    return nth_value(std::type_identity<C>{}, i);
  } else {
    throw not_admitted("a carrier the bounded search cannot enumerate");
  }
}

namespace detail_spans {
/** @brief The meet of two predicates, ⊤ a unit. */
template <typename K>
Pred<K> meet(const Pred<K>& p, const Pred<K>& q) {
  if (p.kind == Pred<K>::Kind::Top) return q;
  if (q.kind == Pred<K>::Kind::Top) return p;
  return Pred<K>::conj(p, q);
}
/** @brief A renumbering of the slots by a table: π_i ↦ π_{to[i]}. */
template <typename K>
struct Renumber {
  const std::vector<unsigned>& to;
  Term<K> operator()(unsigned s) const {
    return Term<K>::proj(s < to.size() ? to[s] : s);
  }
};
/** @brief @c 1 … @c n, after @c from. */
inline std::vector<unsigned> columns(unsigned n, unsigned from = 0) {
  std::vector<unsigned> out(n);
  for (unsigned i = 0; i < n; ++i) out[i] = from + i + 1;
  return out;
}
/** @brief The values of a finite chain as constants, through @c lift: the
 *  cases of Shannon's expansion; none for any other carrier. */
template <typename K, typename Lift>
struct ChainValues {
  const Lift& lift;
  template <typename C>
  std::vector<Term<K>> operator()(std::type_identity<C>) const {
    std::vector<Term<K>> out;
    if constexpr (dedekind::category::IsPst<C> && requires(const C& c) {
                    { lift(c) } -> std::convertible_to<K>;
                  }) {
      for (C v = chain_bottom<C>();;
           v = dedekind::category::Successor<C>{}(v)) {
        out.push_back(Term<K>::constant(lift(v)));
        if (v == chain_top<C>()) break;
      }
    }
    return out;
  }
};
/** @brief The @c i-th search value of a sort, as a cell of the row. */
template <typename... Cs>
struct SearchCell {
  std::uint64_t i;
  template <typename C>
  typename Row<Cs...>::Cell operator()(std::type_identity<C>) const {
    if constexpr (is_sort_of_v<C, std::tuple<Cs...>>)
      return
          typename Row<Cs...>::Cell{std::in_place_type<C>, search_value<C>(i)};
    else
      throw not_admitted("a sort the row's carriers have not");
  }
};
}  // namespace detail_spans

/** @brief A relation over a product of the carriers @c Cs, held as a span.
 *  @tparam K the constants' type.  @tparam Cs the carriers a column may have.
 */
export template <typename K, typename... Cs>
struct Span {
  using Point = Row<Cs...>;
  using P = Pred<K>;
  using T = Term<K>;

  std::vector<std::size_t> schema;
  P predicate;
  std::vector<Hidden> hidden{};
  std::uint64_t budget = std::uint64_t{1} << 16;  // the search's candidates

  unsigned arity() const { return static_cast<unsigned>(schema.size()); }
  unsigned width() const {
    return static_cast<unsigned>(schema.size() + hidden.size());
  }
  /** @brief A row of the visible and hidden columns: the representative. */
  Point rep() const {
    std::vector<std::size_t> all = schema;
    for (const Hidden& h : hidden) all.push_back(h.sort);
    return row_of_sorts<Cs...>(all);
  }

  /** @brief @c R | p: @c p on the visible columns, every slot, constant and
   *  operator checked in its column's sort now.  @throws not_admitted. */
  Span where(const P& p) const {
    if (!only_arguments(slots(p), arity() + 1))
      throw not_admitted("R | pred: a slot past the relation's columns");
    bind_check<Point>(p, rep());
    Span out = *this;
    out.predicate = detail_spans::meet(predicate, p);
    return out;
  }
  /** @brief @c A × B: B's columns after A's, the hidden ones after both. */
  Span times(const Span& b) const {
    const unsigned n = arity() + b.arity();
    std::vector<std::size_t> s = schema;
    s.insert(s.end(), b.schema.begin(), b.schema.end());
    return combine(detail_spans::columns(arity()), n, b,
                   detail_spans::columns(b.arity(), arity()),
                   n + static_cast<unsigned>(hidden.size()), std::move(s), {},
                   true);
  }
  /** @brief @c R & S and @c R | S on one product: @c ∃h.P ∘ ∃g.Q =
   *  ∃h,g. P ∘ Q, the hidden columns renamed apart (the carriers are not
   *  empty).  @throws not_admitted on two products. */
  Span meet(const Span& b) const { return lattice(b, true); }
  Span join(const Span& b) const { return lattice(b, false); }
  /** @brief @c R >> S: this relation's last column identified with @c b's
   *  first and hidden, @c {(a, c) | ∃m. R(a, m) ∧ S(m, c)}: the pullback over
   *  the middle, until the image is taken.  @throws not_admitted where the
   *  two columns are of different carriers. */
  Span then(const Span& b) const {
    if (arity() == 0 || b.arity() == 0 || schema.back() != b.schema.front())
      throw not_admitted(
          "R >> S: R's last column and S's first are of one carrier");
    const unsigned n = arity() + b.arity() - 2;
    std::vector<std::size_t> s(schema.begin(), schema.end() - 1);
    s.insert(s.end(), b.schema.begin() + 1, b.schema.end());
    std::vector<unsigned> mine = detail_spans::columns(arity() - 1);
    mine.push_back(n + 1);  // the middle: the first hidden column
    std::vector<unsigned> theirs{n + 1};
    for (const unsigned c : detail_spans::columns(b.arity() - 1, arity() - 1))
      theirs.push_back(c);
    return combine(mine, n + 1, b, theirs,
                   n + 1 + static_cast<unsigned>(hidden.size()), std::move(s),
                   {Hidden{schema.back()}}, true);
  }
  /** @brief The columns @c keep (1-based) in that order, the others hidden by
   *  @c ∃, outermost.  @throws not_admitted on a repeated or absent column. */
  Span project(const std::vector<unsigned>& keep) const {
    std::vector<unsigned> to(width() + 1, 0);
    std::vector<std::size_t> s;
    std::vector<Hidden> dropped;
    for (const unsigned c : keep) {
      if (c == 0 || c > arity() || to[c] != 0)
        throw not_admitted("R.project(c, …): distinct columns 1 … n");
      to[c] = static_cast<unsigned>(s.size()) + 1;
      s.push_back(schema[c - 1]);
    }
    const unsigned n = static_cast<unsigned>(s.size());
    for (unsigned c = 1; c <= arity(); ++c)
      if (to[c] == 0) {
        dropped.push_back(Hidden{schema[c - 1]});
        to[c] = n + static_cast<unsigned>(dropped.size());
      }
    for (unsigned j = 1; j <= hidden.size(); ++j)
      to[arity() + j] = n + static_cast<unsigned>(dropped.size()) + j;
    Span out{std::move(s), predicate.map_slots(detail_spans::Renumber<K>{to}),
             std::move(dropped), budget};
    out.hidden.insert(out.hidden.end(), hidden.begin(), hidden.end());
    return out;
  }
  /** @brief @c ~R by duality: @c ~(Q₁h₁ … Q_kh_k. P) = Q̄₁h₁ … Q̄_kh_k. ~P,
   *  the predicate in negation normal form. */
  Span complement() const {
    Span out = *this;
    out.predicate = predicate.negated();
    for (Hidden& h : out.hidden) h.universal = !h.universal;
    return out;
  }

  /** @brief The span with its innermost quantifiers eliminated as far as
   *  they go: the innermost block (the trailing hidden columns under one
   *  quantifier) first, by substitution where an atom defines a column (a
   *  conjunct @c π_h @c == @c t under @c ∃, a disjunct @c π_h @c != @c t
   *  under @c ∀), else by Shannon's expansion on a finite chain (@c ∨ under
   *  @c ∃, @c ∧ under @c ∀).  Its numerals are pinned first, so a
   *  substituted term keeps its sort.  @param lift a value into a constant
   *  that names its sort. */
  template <typename Lift>
  Span reduced(const Lift& lift) const {
    Span r = *this;
    r.predicate = pin_constants<Point>(r.predicate, lift, r.rep());
    while (!r.hidden.empty()) {
      const bool universal = r.hidden.back().universal;
      unsigned lo = r.width();  // the innermost block: lo … width
      while (lo > r.arity() + 1 &&
             r.hidden[lo - r.arity() - 2].universal == universal)
        --lo;
      bool done = false;
      for (unsigned h = r.width(); h >= lo && !done; --h)
        if (auto d = defining_term(r.predicate, h, universal)) {
          r = r.without(h, d->rest.subst(h, d->term));
          done = true;
        }
      for (unsigned h = r.width(); h >= lo && !done; --h) {
        const auto cases =
            with_sort<Point>(r.hidden[h - r.arity() - 1].sort,
                             detail_spans::ChainValues<K, Lift>{lift});
        if (cases.empty()) continue;
        P all = r.predicate.subst(h, cases.front());
        for (std::size_t i = 1; i < cases.size(); ++i) {
          const P next = r.predicate.subst(h, cases[i]);
          all = universal ? P::conj(all, next) : P::disj(all, next);
        }
        r = r.without(h, all);
        done = true;
      }
      if (!done) break;
    }
    return r;
  }
  /** @brief The image: every hidden column eliminated (@c reduced).
   *  @throws not_admitted where one remains: a column on a carrier too large
   *  to expand, which no atom defines. */
  template <typename Lift>
  Span fused(const Lift& lift) const {
    Span r = reduced(lift);
    if (!r.hidden.empty())
      throw not_admitted(
          "a hidden column no atom defines, on a carrier too large to expand: "
          "the span has no quantifier-free image here");
    return r;
  }
  /** @brief χ at a row of the visible columns, in K₃: decided (⊤ or ⊥) where
   *  the image is quantifier-free; else the search over the remaining hidden
   *  columns, each within the first @c b of its values with @c b^k ≤
   *  @c budget: a witness of @c ∃ is ⊤, a counterexample to @c ∀ is ⊥, an
   *  exhausted budget U, never the other verdict (Σ, Π).  Alternating
   *  quantifiers answer U.  @param visible the row.  @param lift as for
   *  @c reduced. */
  template <typename Lift>
  Ternary verdict(const Point& visible, const Lift& lift) const {
    const Span r = reduced(lift);
    if (r.hidden.empty())
      return r.predicate(visible) ? Ternary::True : Ternary::False;
    const bool universal = r.hidden.front().universal;
    for (const Hidden& h : r.hidden)
      if (h.universal != universal) return Ternary::Unknown;
    const std::size_t k = r.hidden.size();
    std::uint64_t b = 1;  // the largest b with b^k ≤ budget
    for (;;) {
      std::uint64_t power = 1;
      bool fits = true;
      for (std::size_t j = 0; j < k && fits; ++j) {
        fits = power <= r.budget / (b + 1);
        if (fits) power *= b + 1;
      }
      if (!fits) break;
      ++b;
    }
    std::vector<std::uint64_t> index(k, 0);
    for (;;) {
      Point row = visible;
      for (std::size_t j = 0; j < k; ++j)
        row.cells.push_back(with_sort<Point>(
            r.hidden[j].sort, detail_spans::SearchCell<Cs...>{index[j]}));
      const bool holds = r.predicate(row);
      if (!universal && holds) return Ternary::True;
      if (universal && !holds) return Ternary::False;
      std::size_t j = 0;
      while (j < k && ++index[j] == b) index[j++] = 0;
      if (j == k) return Ternary::Unknown;
    }
  }
  /** @brief The term of a quantifier-free relation certified a function of
   *  its first @c n − 1 columns: a graph atom @c π_n @c == @c t(π_1 …
   *  π_{n−1}) whose term lands in the last column's sort; @c nullptr
   *  otherwise.  @param p the predicate (this one, or it pinned). */
  const T* function_term(const P& p) const {
    if (arity() < 2 || !hidden.empty()) return nullptr;
    const T* t = graph_term(p, arity());
    if (t == nullptr) return nullptr;
    const std::size_t s = sort_of<Point>(*t, rep());
    if (s != no_sort && !sort_embeds<Point>(s, schema.back())) return nullptr;
    return t;
  }

 private:
  /** @brief Two spans in one layout: this one's visible columns at @c mine
   *  and its hidden ones from @c my_hidden, @c b's likewise; the predicates
   *  met or joined, the hidden columns @c lead first. */
  Span combine(const std::vector<unsigned>& mine, unsigned my_hidden,
               const Span& b, const std::vector<unsigned>& theirs,
               unsigned their_hidden, std::vector<std::size_t> s,
               std::vector<Hidden> lead, bool conjunction) const {
    const auto ta = layout(mine, my_hidden);
    const auto tb = b.layout(theirs, their_hidden);
    const P p = predicate.map_slots(detail_spans::Renumber<K>{ta});
    const P q = b.predicate.map_slots(detail_spans::Renumber<K>{tb});
    Span r{std::move(s), conjunction ? detail_spans::meet(p, q) : P::disj(p, q),
           std::move(lead), budget};
    r.hidden.insert(r.hidden.end(), hidden.begin(), hidden.end());
    r.hidden.insert(r.hidden.end(), b.hidden.begin(), b.hidden.end());
    return r;
  }
  Span lattice(const Span& b, bool conjunction) const {
    if (schema != b.schema)
      throw not_admitted("relations over different products");
    const unsigned n = arity();
    return combine(detail_spans::columns(n), n, b, detail_spans::columns(n),
                   n + static_cast<unsigned>(hidden.size()), schema, {},
                   conjunction);
  }
  /** @brief The table placing this span's slots in a wider layout: visible
   *  column @c i at @c vis[i − 1], hidden column @c j at @c from + j. */
  std::vector<unsigned> layout(const std::vector<unsigned>& vis,
                               unsigned from) const {
    std::vector<unsigned> to(width() + 1, 0);
    for (unsigned i = 1; i <= arity(); ++i) to[i] = vis[i - 1];
    for (unsigned j = 1; j <= hidden.size(); ++j) to[arity() + j] = from + j;
    return to;
  }
  /** @brief The span with the hidden column @c h eliminated: @c p, which no
   *  longer mentions it, renumbered so the columns above close the gap. */
  Span without(unsigned h, const P& p) const {
    std::vector<unsigned> to(width() + 1, 0);
    for (unsigned s = 0; s <= width(); ++s) to[s] = s < h ? s : s - 1;
    Span out{schema, p.map_slots(detail_spans::Renumber<K>{to}), hidden,
             budget};
    out.hidden.erase(out.hidden.begin() + (h - arity() - 1));
    return out;
  }
};

}  // namespace dedekind::sets
