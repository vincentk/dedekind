/**
 * @file dedekind/sequences/relprod.cppm
 * @partition :relprod
 * @brief The relative product @f$R;S@f$ over a FINITE ℕ-prefix middle: the
 *        generalization of @c :relational's Boolean-middle @c >> (which
 *        enumerates @c {false,true}) to a bounded ℕ carrier @f$[0,M)@f$.  The
 *        @f$\exists@f$-over-the-middle is a @b short-circuiting scan (@c
 *        sets::exists: it stops at the first witness), and
 *        the bound @c M is carried on the composed type so repeated squaring
 *        (@c R;R, @c (R;R);(R;R), …) stays bounded --- the semantic for-loop of
 *        bounded transitive closure (#795).
 *
 * @section relprod__Why_Here
 * Homed downstream of @c relational (where the Boolean @c >> lives): the bound
 * @c M is read from an @b order-level @c ProjBound (the half-space cut @c
 * collatz @c | @c (π1 @c < @c fix(M_c))), which @c relational --- @b upstream
 * of @c order --- cannot see, so the bounded @c >> cannot live beside its
 * Boolean sibling.  It sits in @c sequences (the first module below both @c
 * order and @c relational that the exhibit already imports); consumers reach
 * the bare @c >> via @c using @c namespace @c dedekind::sequences.  The
 * @f$\exists@f$ itself is @c sets::exists (@b upstream, short-circuiting).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <cstddef>
#include <ranges>
#include <utility>

export module dedekind.sequences:relprod;

import dedekind.sets;  // Set<...>, finite_cardinality, exists (short-circuit ∃)
import dedekind.order; // ProjBound, ProductRestrict, Rel, IsRelPredicate

namespace dedekind::sequences {
using dedekind::order::IsRelPredicate;
using dedekind::sets::exists;
using dedekind::sets::finite_cardinality;
using dedekind::sets::Set;

/** @brief The composed predicate for @f$R;S@f$ over the finite ℕ-prefix
 *  @f$[0,M)@f$ middle.  Carries @c M (so the bound survives repeated squaring)
 *  and the two operand @c IsRelPredicate operands.  The
 *  @f$\exists@f$-over-the-middle is the @b short-circuiting @c sets::exists
 * over a @b lazy @c iota generator: @c filter's @c begin() advances only to the
 *  @e first witness @f$b@f$, so a hit returns immediately (@f$\top \vee x =
 *  \top@f$) --- O(1) memory (no bool accumulator threaded), O(M) steps
 *  @e worst-case, @b no array. */
export template <std::size_t M, IsRelPredicate PR, IsRelPredicate PS>
struct ComposePrefixPred {
  using is_rel_predicate = void;
  PR r;
  PS s;
  /** @param ac the endpoint pair @f$(a,c)@f$ --- any @c .first / @c .second
   *  carrier the operands accept. */
  template <typename Pair>
    requires requires(const Pair& ac) {
      ac.first;
      ac.second;
    }
  constexpr bool operator()(const Pair& ac) const {
    // ∃ b ∈ [0,M). R(a,b) ∧ S(b,c) --- exists stops at the first witness b.
    return exists(std::views::iota(std::size_t{0}, M), [&](std::size_t i) {
      const auto b = finite_cardinality(i);
      return static_cast<bool>(r(std::pair{ac.first, b})) &&
             static_cast<bool>(s(std::pair{b, ac.second}));
    });
  }
};

/** @brief @c prefix_bound<P> --- recover the finite-prefix bound @c M carried
 *  by a bounded relation's predicate @c P: either from the half-space
 *  restriction (a @c ProductRestrict wrapping a @c ProjBound<1,Lt,M> domain
 *  cut), or from a prior compose (@c ComposePrefixPred).  Left @b undefined
 *  otherwise --- an unbounded relation has no finite middle to compose over, so
 *  its @c >> stays the honest Rice wall. */
template <typename P>
struct prefix_bound;
template <typename Pp, auto V>
struct prefix_bound<dedekind::order::ProductRestrict<
    Pp, dedekind::order::ProjBound<1, dedekind::order::Rel::Lt, V>>> {
  static constexpr std::size_t value = static_cast<std::size_t>(V);
};
template <std::size_t M, typename PR, typename PS>
struct prefix_bound<ComposePrefixPred<M, PR, PS>> {
  static constexpr std::size_t value = M;
};

/** @brief A relation is @b bounded (composable over a finite middle) iff its
 *  predicate carries a @c prefix_bound. */
template <typename P>
concept HasPrefixBound = requires { prefix_bound<P>::value; };

/** @brief @c R @c >> @c S over the finite ℕ-prefix middle @b inferred from
 *  @c R's bound (no middle argument): @f$(R;S)(a,c) = \exists b \in [0,M).\,
 *  R(a,b) \wedge S(b,c)@f$, streamed as an OR-fold.  The result is @b itself
 *  bounded (@c ComposePrefixPred carries @c M), so the squaring chain
 *  @c collatzM @c >> @c collatzM @c >> @c … composes without re-supplying the
 *  middle.  Generalizes @c :relational's Boolean-middle @c >> to a bounded ℕ
 *  carrier (#795).  Gated on @c HasPrefixBound so the unbounded @c >> is
 *  non-viable here (it stays the Boolean case in @c :relational). */
export template <typename A, typename B, typename C, typename L,
                 IsRelPredicate PR, IsRelPredicate PS>
  requires HasPrefixBound<PR>
constexpr auto operator>>(const Set<std::pair<A, B>, L, PR>& r,
                          const Set<std::pair<B, C>, L, PS>& s) {
  constexpr std::size_t M = prefix_bound<PR>::value;
  using CP = ComposePrefixPred<M, PR, PS>;
  return Set<std::pair<A, C>, L, CP>{CP{r.predicate(), s.predicate()}};
}

}  // namespace dedekind::sequences
