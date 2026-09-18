/**
 * @file dedekind/sequences/relprod.cppm
 * @partition :relprod
 * @brief The relative product @f$R;S@f$ over a FINITE ℕ-prefix middle: the
 *        generalization of @c :relational's Boolean-middle @c >> (which
 *        enumerates @c {false,true}) to a bounded ℕ carrier @f$[0,M)@f$.  The
 *        @f$\exists@f$-over-the-middle is a @b streaming Boolean OR-fold, and
 *        the bound @c M is carried on the composed type so repeated squaring
 *        (@c R;R, @c (R;R);(R;R), …) stays bounded --- the semantic for-loop of
 *        bounded transitive closure (#795).
 *
 * @section relprod__Why_Here
 * Homed in @c sequences, not @c relational (where the Boolean @c >> lives),
 * for two layering reasons: (1) the bound @c M is read from an @b order-level
 * @c ProjBound (the half-space cut @c collatz @c | @c (π1 @c < @c fix(M_c))),
 * which @c relational --- @b upstream of @c order --- cannot see; and (2) the
 * @f$\exists@f$-fold uses this layer's own @c fold (@c :fold).  Consumers reach
 * the bare @c >> via @c using @c namespace @c dedekind::sequences.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <cstddef>
#include <ranges>
#include <utility>

export module dedekind.sequences:relprod;

import dedekind.sets;  // Set<std::pair<...>, L, P>, finite_cardinality
import dedekind.order; // ProjBound, ProductRestrict, Rel (the half-space cut)
import :fold;          // fold --- the streaming catamorphism

namespace dedekind::sequences {
using dedekind::sets::finite_cardinality;
using dedekind::sets::Set;

/** @brief The composed predicate for @f$R;S@f$ over the finite ℕ-prefix
 *  @f$[0,M)@f$ middle.  Carries @c M (so the bound survives repeated squaring)
 *  and the two operand predicates.  The @f$\exists@f$-over-the-middle is a
 *  streaming Boolean OR-fold over a @b lazy @c iota generator: O(1) memory (one
 *  bool), O(M) steps, @b no array. */
export template <std::size_t M, typename PR, typename PS>
struct ComposePrefixPred {
  using is_rel_predicate = void;
  PR r;
  PS s;
  template <typename Pair>
  constexpr bool operator()(const Pair& ac) const {
    return fold(std::views::iota(std::size_t{0}, M), false,
                [&](bool& acc, std::size_t i) {
                  const auto b = finite_cardinality(i);
                  acc = acc || (static_cast<bool>(r(std::pair{ac.first, b})) &&
                                static_cast<bool>(s(std::pair{b, ac.second})));
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
export template <typename A, typename B, typename C, typename L, typename PR,
                 typename PS>
  requires HasPrefixBound<PR>
constexpr auto operator>>(const Set<std::pair<A, B>, L, PR>& r,
                          const Set<std::pair<B, C>, L, PS>& s) {
  constexpr std::size_t M = prefix_bound<PR>::value;
  using CP = ComposePrefixPred<M, PR, PS>;
  return Set<std::pair<A, C>, L, CP>{CP{r.predicate(), s.predicate()}};
}

}  // namespace dedekind::sequences
