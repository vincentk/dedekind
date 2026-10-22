/**
 * @file dedekind/order/runs.cppm
 * @partition :runs
 * @brief Run lists: the normal form of the sets of a chain built from
 *        intervals, valued in a species.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section runs__Normal_Form
 * A set of a chain valued in a species @c L, built from intervals with the
 * species' @c ∧, @c ∨ and @c ¬, is a @b step function: a value before the
 * first step, and finitely many steps, each a position on the chain where the
 * value changes.  In canonical form the positions are closed where the chain
 * has the step and no step repeats the value before it, so each set has one
 * run list, and @c &, @c |, @c ~ are total on the one type.  It is the
 * canonical form of the Boolean (De Morgan) algebra the intervals generate,
 * quotiented by the chain's order; @c Interval (@c :halfspace) is its
 * generator.  A step at the carrier's own end is kept (@c {x ≤ max} on a
 * machine word read as a window of ℤ stays bounded above); equality reads the
 * carrier's ends.
 *
 * Wikipedia: Step function, Run-length encoding, Dedekind cut.
 *
 * @note "To denote the subsequence of natural numbers 2, 3, ..., 12 without
 *       the pernicious three dots, four conventions are open to us."
 *       -- Edsger W. Dijkstra, *Why numbering should start at zero*, EWD831
 *          (1982).  A position names the cut before or after a value, which
 *          is how the four conventions become one.
 */
module;

#include <algorithm>  // a connective's operands sorted (#884)
#include <array>
#include <compare>  // the structural order of terms (#884)
#include <concepts>
#include <cstddef>
#include <functional>  // std::less_equal: the chain's own order
#include <iterator>
#include <map>
#include <optional>
#include <ranges>
#include <type_traits>
#include <utility>
#include <vector>  // a connective's operands

export module dedekind.order:runs;

import dedekind.category;
import dedekind.sets;
import :total;  // IsTotallyOrdered: positions live on a certified chain

namespace dedekind::order {
using namespace dedekind::sets;
using namespace dedekind::category;

/** @brief Orientation of a halfspace along the chain. */
export enum class Direction { Upward, Downward };

/** @brief Whether the boundary is strict (`>`, `<`) or inclusive (`>=`, `<=`).
 */
export enum class Strictness { Strict, NonStrict };

/** @brief Flip a @c Direction --- the direction half of a halfspace complement
 *  (@c ~{x>P} is @c {x≤P}).  Reused by the reflection-image pushforward
 *  (@c dedekind.algebra:halfspace_transport) to flip a halfspace's sense under
 *  @f$x \mapsto -x@f$. */
export constexpr Direction flip(Direction d) {
  return d == Direction::Upward ? Direction::Downward : Direction::Upward;
}
/** @brief Flip a @c Strictness --- the strictness half of the same complement
 *  (@c ~{x>P} is @c {x≤P}: @c > relaxes to @c ≤). */
export constexpr Strictness flip(Strictness s) {
  return s == Strictness::Strict ? Strictness::NonStrict : Strictness::Strict;
}

/** @brief Cardinality class of a halfspace's carrier @c T (#848/#927).
 *  @details A carrier that declares its own @c cardinality_type is the
 *  authority (e.g.\ @c numbers::Rational → @c ℵ_0, @c ExtensionalCardinal →
 *  @c Finite): this partition (@c :order) is @b upstream of @c :numbers, so a
 *  countable @b non-integral carrier such as ℚ cannot be recognised
 *  structurally here (@c IsRingIntegral is false on @c Rational), and must
 *  self-declare so the point-free @c A|pred halfspace classifies identically
 *  to the scout comprehension (which inherits the ambient @c C directly).
 *  Otherwise fall back to the @c IsRingIntegral discriminator: the structural
 *  integers @b and the @c Cardinality / @c SignedCardinality (ℕ/ℤ) proxies are
 *  @c ℵ_0, and the real proxies (@c QuadraticReal, @c double) are the continuum
 *  @c ℶ_1.  The bound only has to be tight enough for the countability
 *  verdict (@c IsCountable). */
// Module-private: the fallback is a halfspace-classification heuristic, not a
// general carrier-cardinality authority (that is @c :sets:cardinality).  It
// only feeds @c Halfspace::cardinality_type below; external carriers customise
// through their own @c T::cardinality_type, not this helper.  A finite @b enum
// carrier (e.g.\ @c Ternary) cannot self-declare a member typedef, so it is
// recognised structurally as countable and classified @c ℵ_0 --- the same
// conservative countable BOUND @c bool already gets via @c IsRingIntegral, NOT
// @c Finite.  @c Finite is the @c elevate_meet "return bare, do not @c Set
// -wrap" signal (@c :expressions); a halfspace must not trigger it, and @c ℵ_0
// keeps the pre-existing @c Set-wrapping path and stays countable, matching
// @c Ternary's @c 𝕂3 ambient.
// The @c IsRingIntegral integers / ℕ,ℤ proxies are @c ℵ_0 likewise; the real
// proxies are @c ℶ_1.
template <typename T>
struct carrier_cardinality {
  using type =
      std::conditional_t<std::is_enum_v<T> || IsRingIntegral<T>, ℵ_0, ℶ_1>;
};
template <typename T>
  requires IsCardinality<typename T::cardinality_type>
struct carrier_cardinality<T> {
  // Fires only when the self-declared alias is a genuine @c IsCardinality (a
  // carrier with an unrelated / incomplete @c cardinality_type falls to the
  // primary, not a hard error), and classifies it with the canonical
  // @c IsCountable concept.  Collapse to the
  // COUNTABILITY BOUND (never pass a bare @c Finite through): @c
  // ExtensionalCardinal declares @c Finite, and letting that reach @c
  // Halfspace::cardinality_type would trip @c elevate_meet's Finite "return
  // bare" path for an @c ExtensionalCardinal halfspace.  Countable (incl.\
  // @c Finite) ⟹ @c ℵ_0, uncountable ⟹ @c ℶ_1.
  using type =
      std::conditional_t<IsCountable<typename T::cardinality_type>, ℵ_0, ℶ_1>;
};
template <typename T>
using carrier_cardinality_t = typename carrier_cardinality<T>::type;

// A finite enum carrier classifies ℵ_0 (a conservative countable bound, like
// bool), so a Ternary halfspace stays decidable (Boole), not Kleene (#927), and
// does NOT trip the elevate_meet Finite "return bare" path.
static_assert(std::same_as<carrier_cardinality_t<Ternary>, ℵ_0>,
              "a finite enum carrier (e.g. Ternary) is countable ℵ_0, not the "
              "continuum ℶ_1 (and not Finite, the elevate_meet bare signal).");
// A self-declared @c Finite carrier is likewise collapsed to @c ℵ_0, so its
// halfspace stays Set-wrapped by @c elevate_meet (not returned bare).
static_assert(std::same_as<carrier_cardinality_t<ExtensionalCardinal<>>, ℵ_0>,
              "a self-declared Finite carrier collapses to the countable bound "
              "ℵ_0, never the bare-signal Finite.");
namespace detail_halfspace_witness {
// A carrier whose @c cardinality_type alias is NOT an @c IsCardinality: the
// self-declaring specialization must @b not fire (its IsCardinality constraint
// fails), so @c carrier_cardinality falls back to the primary rather than
// hard-erroring on the missing @c is_countable member.
struct UnrelatedCardinalityAlias {
  using cardinality_type = int;  // not a cardinality (no is_countable etc.)
};
}  // namespace detail_halfspace_witness
static_assert(
    std::same_as<carrier_cardinality_t<
                     detail_halfspace_witness::UnrelatedCardinalityAlias>,
                 ℶ_1>,
    "a carrier with an unrelated/incomplete cardinality_type falls back to the "
    "primary (ℶ_1 here), never making Halfspace ill-formed.");

/** @brief One side of an interval as a value: the bound and whether it is
 *  attained (non-strict) or not (strict).  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Bound {
  V at;
  Strictness strictness = Strictness::NonStrict;
  friend constexpr bool operator==(const Bound&, const Bound&) = default;
};
/** @brief The sides of an inhabited convex set; an absent side is unbounded.
 *  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Span {
  std::optional<Bound<V>> lower;
  std::optional<Bound<V>> upper;
  friend constexpr bool operator==(const Span&, const Span&) = default;
};

/** @section runs__Positions
 *  The places on a chain a side can sit: just below a value or just above it,
 *  a Dedekind cut named by the value beside it.  An interval's sides and a run
 *  list's steps are positions, so both compare, close and read the carrier's
 *  ends through this one layer. */

/** @brief Which side of its value a position sits on. */
export enum class Side : bool { Below, Above };

/** @brief A position on the chain: just below @c at, or just above it.
 *  @tparam V the carrier. */
export template <typename V>
  requires IsTotallyOrdered<V>
struct Position {
  V at;
  Side side;
  friend constexpr bool operator==(const Position&, const Position&) = default;
  /** @brief Along the chain: by value, and below a value before above it. */
  friend constexpr bool operator<(const Position& a, const Position& b) {
    return a.at < b.at ||
           (a.at == b.at && a.side == Side::Below && b.side == Side::Above);
  }
};

namespace detail_position {
/** @brief The position a lower side starts at: below an attained bound, above a
 *  strict one.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> start_of(const Bound<V>& b) {
  return {b.at,
          b.strictness == Strictness::NonStrict ? Side::Below : Side::Above};
}
/** @brief The position an upper side ends at: above an attained bound, below a
 *  strict one.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> end_of(const Bound<V>& b) {
  return {b.at,
          b.strictness == Strictness::NonStrict ? Side::Above : Side::Below};
}
/** @brief The lower side starting at @c p.  @tparam V the carrier. */
template <typename V>
constexpr Bound<V> lower_at(const Position<V>& p) {
  return {p.at,
          p.side == Side::Below ? Strictness::NonStrict : Strictness::Strict};
}
/** @brief The upper side ending at @c p.  @tparam V the carrier. */
template <typename V>
constexpr Bound<V> upper_at(const Position<V>& p) {
  return {p.at,
          p.side == Side::Above ? Strictness::NonStrict : Strictness::Strict};
}
/** @brief The carrier's least element, where it has one as a value: a
 *  registered lattice bottom (the machine words), ℕ's 0 and ℤ's −ℵ₀, a truth
 *  chain's ⊥.  @tparam V the carrier. */
template <typename V>
constexpr std::optional<V> least() {
  if constexpr (requires { LatticeBottom<V, std::less_equal<V>>::value; })
    return LatticeBottom<V, std::less_equal<V>>::value;
  else if constexpr (std::same_as<V, Cardinality>)
    return V{};
  else if constexpr (std::same_as<V, SignedCardinality>)
    return V{NegativeInfinity{}};
  else if constexpr (IsPst<V>)
    return unit<V>(Sup{});
  else
    return std::nullopt;
}
/** @brief The carrier's greatest element, where it has one as a value, dual to
 *  @ref least (ℵ₀ on ℕ).  @tparam V the carrier. */
template <typename V>
constexpr std::optional<V> greatest() {
  if constexpr (requires { LatticeTop<V, std::less_equal<V>>::value; })
    return LatticeTop<V, std::less_equal<V>>::value;
  else if constexpr (std::same_as<V, Cardinality>)
    return V{ℵ_0{}};
  else if constexpr (std::same_as<V, SignedCardinality>)
    return V{PositiveInfinity{}};
  else if constexpr (IsPst<V>)
    return unit<V>(Inf{});
  else
    return std::nullopt;
}
/** @brief A position in its canonical form: just above a value is just below
 *  the element covering it, where there is one, so on a discrete chain
 *  @c (0, 3) and @c [1, 3) have the same positions.  @tparam V the carrier. */
template <typename V>
constexpr Position<V> canonical(Position<V> p) {
  if constexpr (HasNNOStep<V>)
    if (p.side == Side::Above)
      if (const auto n = cover(p.at)) return {*n, Side::Below};
  return p;
}
/** @brief Whether @c p lies before every element of the carrier.
 *  @tparam V the carrier. */
template <typename V>
constexpr bool before_all(const Position<V>& p) {
  const auto lo = least<V>();
  return lo && !(Position<V>{*lo, Side::Below} < p);
}
/** @brief Whether @c p lies after every element of the carrier.
 *  @tparam V the carrier. */
template <typename V>
constexpr bool after_all(const Position<V>& p) {
  const auto hi = greatest<V>();
  return hi && !(p < Position<V>{*hi, Side::Above});
}
}  // namespace detail_position
/** @brief A position in its canonical form: closed where the chain has the
 *  step.  @tparam V the carrier. */
export template <typename V>
constexpr Position<V> canonical(const Position<V>& p) {
  return detail_position::canonical(p);
}

/** @section runs__Steps
 *  A set of the chain valued in a species is a step function: a value before
 *  every step, then the steps along the chain, each a canonical position and
 *  the value from there on.  A @b step source is that value and a
 *  @c std::ranges forward range of steps, composed lazily from the standard
 *  views: nothing is stored, and every query is one traversal.  The sources
 *  compose (a meet is a merge of two sources), so the normal form of a term is
 *  itself a source.  @c Runs is the one stored form, materialised on request.
 */

/** @brief Where the value changes, and to what.  A source holds its steps
 *  by value (an interval's two sides, an iterator's current step), so the
 *  carrier and the truth values are default-initialisable.  @tparam V the
 *  carrier.  @tparam L the species. */
export template <typename V, typename L>
  requires std::default_initializable<V> &&
           std::default_initializable<typename L::Ω>
struct Step {
  Position<V> at;
  typename L::Ω value;
  friend constexpr bool operator==(const Step&, const Step&) = default;
};

/** @brief A step source: the carrier and species, the value before every step,
 *  and the steps as a forward range, in order along the chain. */
export template <typename S>
concept IsStepSource =
    requires(const S& s) {
      typename S::Domain;
      typename S::logic_species;
      { s.first() } -> std::same_as<typename S::logic_species::Ω>;
      { s.steps() } -> std::ranges::forward_range;
    } &&
    std::same_as<
        std::ranges::range_value_t<decltype(std::declval<const S&>().steps())>,
        Step<typename S::Domain, typename S::logic_species>>;

namespace detail_runs {
/** @brief The species' operations as named functions on @c L::Ω. */
template <typename L>
struct And {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::AND(a, b);
  }
};
template <typename L>
struct Or {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::OR(a, b);
  }
};
template <typename L>
struct Rfl {
  constexpr auto operator()(typename L::Ω a) const { return L::RFL(a); }
};
/** @brief @c a ⇔ @c b in the species: both, or neither. */
template <typename L>
struct Iff {
  constexpr auto operator()(typename L::Ω a, typename L::Ω b) const {
    return L::OR(L::AND(a, b), L::AND(L::RFL(a), L::RFL(b)));
  }
};
/** @brief A value lifted along the dominance into the species @c J. */
template <typename J>
struct LiftInto {
  template <typename Ω>
  constexpr typename J::Ω operator()(Ω v) const {
    if constexpr (std::same_as<Ω, typename J::Ω>)
      return v;
    else
      return lift_logic<J>(v);
  }
};
/** @brief Whether a value reaches a level, and whether it is the level: the
 *  α-cut and the fibre, pointwise. */
template <typename Ω>
struct AtLeast {
  Ω level;
  constexpr bool operator()(Ω v) const { return !(v < level); }
};
template <typename Ω>
struct EqualTo {
  Ω level;
  constexpr bool operator()(Ω v) const { return v == level; }
};
/** @brief A step with its value mapped into the species @c L2. */
template <typename F, typename L2>
struct MapValue {
  F f;
  template <typename V, typename L>
  constexpr Step<V, L2> operator()(const Step<V, L>& s) const {
    return {s.at, f(s.value)};
  }
};
/** @brief Two steps with one value; the first step of a chunk; a step at a
 *  given value: the parts of dropping repeats. */
struct SameValue {
  template <typename S>
  constexpr bool operator()(const S& a, const S& b) const {
    return a.value == b.value;
  }
};
struct Front {
  template <typename R>
  constexpr auto operator()(R&& chunk) const {
    return *std::ranges::begin(chunk);
  }
};
template <typename Ω>
struct ValueIs {
  Ω value;
  template <typename S>
  constexpr bool operator()(const S& s) const {
    return s.value == value;
  }
};
}  // namespace detail_runs
/** @brief Steps with no step repeating the value before it, as a view: runs of
 *  one value kept by their first step, and a first step at the start value
 *  dropped.  @param r the steps.  @param first the value before them. */
export template <std::ranges::viewable_range R, typename Ω>
constexpr auto distinct_steps(R&& r, Ω first) {
  using namespace detail_runs;
  return std::views::drop_while(
      std::views::transform(
          std::views::chunk_by(std::forward<R>(r), SameValue{}), Front{}),
      ValueIs<Ω>{first});
}

/** @brief An interval's sides as a source, valued @c value on it and ⊥
 *  elsewhere: at most two steps.  No sides is the whole carrier, no span the
 *  empty set, so the constants are spans too.  @tparam V the carrier.
 *  @tparam L the species. */
export template <typename V, typename L>
struct SpanSteps {
  using Domain = V;
  using logic_species = L;
  std::optional<Span<V>> span;
  typename L::Ω value = L::True;
  constexpr typename L::Ω first() const {
    return span && !span->lower ? value : L::False;
  }
  constexpr auto steps() const {
    std::array<Step<V, L>, 2> s{};
    std::size_t n = 0;
    if (span && value != L::False) {
      if (span->lower)
        s[n++] = {canonical(detail_position::start_of(*span->lower)), value};
      if (span->upper)
        s[n++] = {canonical(detail_position::end_of(*span->upper)), L::False};
    }
    return std::views::take(std::move(s), static_cast<std::ptrdiff_t>(n));
  }
};

/** @brief A source read through a reference: a stored run list inside a term
 *  is traversed where it is, not copied.  @tparam S the source. */
export template <typename S>
struct SourceRef {
  using Domain = typename S::Domain;
  using logic_species = typename S::logic_species;
  const S* s;
  constexpr auto first() const { return s->first(); }
  constexpr auto steps() const { return s->steps(); }
};

/** @brief A source with every value relabelled into the species @c L2: a
 *  complement, a lift into a wider species, an α-cut.  @tparam A the source.
 *  @tparam F the operation @c A's Ω → @c L2::Ω.  @tparam L2 the species. */
export template <IsStepSource A, typename F,
                 typename L2 = typename A::logic_species>
struct RelabelSteps {
  using Domain = typename A::Domain;
  using logic_species = L2;
  A a;
  F f;
  constexpr typename L2::Ω first() const { return f(a.first()); }
  constexpr auto steps() const {
    return distinct_steps(
        std::views::transform(a.steps(), detail_runs::MapValue<F, L2>{f}),
        first());
  }
};

/** @brief The steps of two ranges merged along the chain, each read as @c f of
 *  the two values there: the one lazy merge C++23 does not ship.  A forward
 *  view; repeats are dropped by the caller.  @tparam RA,RB the step ranges.
 *  @tparam F the operation.  @tparam V,L the carrier and species. */
template <typename RA, typename RB, typename F, typename V, typename L>
class MergeView
    : public std::ranges::view_interface<MergeView<RA, RB, F, V, L>> {
 public:
  using Ω = typename L::Ω;
  constexpr MergeView() = default;
  constexpr MergeView(RA a, RB b, F f, Ω va, Ω vb)
      : a_(std::move(a)), b_(std::move(b)), f_(f), va_(va), vb_(vb) {}

  class iterator {
   public:
    using iterator_concept = std::forward_iterator_tag;
    using value_type = Step<V, L>;
    using difference_type = std::ptrdiff_t;
    constexpr iterator() = default;
    constexpr explicit iterator(MergeView* m)
        : m_(m),
          ia_(std::ranges::begin(m->a_)),
          ib_(std::ranges::begin(m->b_)),
          va_(m->va_),
          vb_(m->vb_) {
      advance();
    }
    constexpr const value_type& operator*() const { return cur_; }
    constexpr iterator& operator++() {
      advance();
      return *this;
    }
    constexpr iterator operator++(int) {
      iterator before = *this;
      advance();
      return before;
    }
    /** @brief Equal at one position of the merge, or both at its end. */
    friend constexpr bool operator==(const iterator& x, const iterator& y) {
      if (!x.live_ || !y.live_) return !x.live_ && !y.live_;
      return x.ia_ == y.ia_ && x.ib_ == y.ib_;
    }

   private:
    constexpr void advance() {
      const bool ha = ia_ != std::ranges::end(m_->a_);
      const bool hb = ib_ != std::ranges::end(m_->b_);
      live_ = ha || hb;
      if (!live_) return;
      const auto at =
          !hb || (ha && (*ia_).at < (*ib_).at) ? (*ia_).at : (*ib_).at;
      if (ha && (*ia_).at == at) va_ = (*ia_++).value;
      if (hb && (*ib_).at == at) vb_ = (*ib_++).value;
      cur_ = value_type{at, m_->f_(va_, vb_)};
    }
    MergeView* m_ = nullptr;
    std::ranges::iterator_t<RA> ia_{};
    std::ranges::iterator_t<RB> ib_{};
    Ω va_{}, vb_{};
    // The current step and whether there is one: a value and a flag, not an
    // optional, whose reset would end the value's lifetime under a copy in
    // constant evaluation.
    value_type cur_{};
    bool live_ = false;
  };
  constexpr iterator begin() { return iterator{this}; }
  constexpr iterator end() { return {}; }

 private:
  RA a_;
  RB b_;
  F f_{};
  Ω va_{}, vb_{};
};

/** @brief Two sources merged along the chain under a pointwise operation: the
 *  steps of both, each read as @c f of the two values there.
 *  @tparam A,B the sources.  @tparam F the operation. */
export template <IsStepSource A, IsStepSource B, typename F>
struct MergeSteps {
  using Domain = typename A::Domain;
  using logic_species = typename A::logic_species;
  A a;
  B b;
  F f;
  constexpr typename logic_species::Ω first() const {
    return f(a.first(), b.first());
  }
  constexpr auto steps() const {
    using M = MergeView<decltype(a.steps()), decltype(b.steps()), F, Domain,
                        logic_species>;
    return distinct_steps(M{a.steps(), b.steps(), f, a.first(), b.first()},
                          first());
  }
};

/** @brief The decidable sets of an L-valued source: the α-cut
 *  @f$\{\chi \ge \ell\}@f$ and the fibre @f$\chi^{-1}(\ell)@f$.
 *  @param s the source.  @param level the level. */
export template <IsStepSource S>
constexpr auto alpha_cut(const S& s, typename S::logic_species::Ω level) {
  using Ω = typename S::logic_species::Ω;
  return RelabelSteps<S, detail_runs::AtLeast<Ω>, Boole>{s, {level}};
}
export template <IsStepSource S>
constexpr auto fibre(const S& s, typename S::logic_species::Ω level) {
  using Ω = typename S::logic_species::Ω;
  return RelabelSteps<S, detail_runs::EqualTo<Ω>, Boole>{s, {level}};
}

/** @section runs__Chain_View
 *  @c chain_view, the bounded chain @c [lo, hi] as a range, lives in
 *  @c category:nno beside the covering step it walks with (hoisted for the
 *  Jlt paper, so that @c sets can fold over it too); re-exported here for its
 *  original readers. */
export using dedekind::category::chain_view;

namespace detail_chain_view_witness {
consteval bool walks(auto view, auto... expected) {
  std::array<std::common_type_t<decltype(expected)...>, sizeof...(expected)>
      want{expected...};
  std::size_t i = 0;
  for (const auto x : view) {
    if (i >= want.size() || x != want[i]) return false;
    ++i;
  }
  return i == want.size();
}
static_assert(walks(chain_view{Ternary::False, Ternary::True}, Ternary::False,
                    Ternary::Unknown, Ternary::True),
              "K₃ walked ⊥ → U → ⊤ by the cover.");
static_assert(walks(chain_view{false, true}, false, true) &&
                  walks(chain_view{true, true}, true) &&
                  walks(chain_view{3, 6}, 3, 4, 5, 6),
              "𝔹, a one-point window, and an integral window.");
static_assert(std::ranges::forward_range<chain_view<int>> &&
                  std::ranges::view<chain_view<Ternary>>,
              "chain_view is a std::ranges forward view.");
static_assert(chain_view{5, 2}.begin() == std::default_sentinel &&
                  chain_view{2, 2}.begin() != std::default_sentinel,
              "lo > hi is the empty window; lo == hi the one-point window.");
}  // namespace detail_chain_view_witness

namespace detail_runs {
/** @brief An element as the step to its own value: χ = id. */
template <typename C, typename L>
struct ToItself {
  constexpr Step<C, L> operator()(const C& x) const {
    return {Position<C>{x, Side::Below}, x};
  }
};
}  // namespace detail_runs
/** @brief χ(x) = x on a finite chain, valued in the chain's own species: a
 *  step at every element above the least (none on a one-element chain).
 *  @tparam C the chain. */
export template <typename C>
  requires HasNNOStep<C> && (detail_position::least<C>().has_value()) &&
           (detail_position::greatest<C>().has_value())
struct IdentitySteps {
  using Domain = C;
  using logic_species = classifier_logic_t<C>;
  constexpr C first() const { return *detail_position::least<C>(); }
  constexpr auto steps() const {
    return std::views::transform(
        std::views::drop(chain_view{first(), *detail_position::greatest<C>()},
                         1),
        detail_runs::ToItself<C, logic_species>{});
  }
};

/** @name Queries: one traversal of a source
 *  @{ */
/** @brief χ(x): the value of the last step at or before @c x. */
export template <IsStepSource S>
constexpr auto value_at(const S& s, const typename S::Domain& x) {
  auto v = s.first();
  for (const auto& st : s.steps()) {
    if (st.at.side == Side::Below ? x < st.at.at : !(st.at.at < x)) break;
    v = st.value;
  }
  return v;
}
namespace detail_runs {
/** @brief The species' fold of the values over the carrier's own pieces: a
 *  step before every element sets the first value, one after every element
 *  ends the fold.  @tparam Op the fold. */
template <typename Op, IsStepSource S>
constexpr auto fold_over_carrier(const S& s, Op op) {
  using L = typename S::logic_species;
  auto cur = s.first();
  std::optional<typename L::Ω> acc;
  for (const auto& st : s.steps()) {
    if (detail_position::before_all(st.at)) {
      cur = st.value;
      continue;
    }
    if (detail_position::after_all(st.at)) break;
    acc = acc ? op(*acc, cur) : cur;
    cur = st.value;
  }
  return acc ? op(*acc, cur) : cur;
}
}  // namespace detail_runs
/** @brief @f$\bigwedge_x \chi(x)@f$ and @f$\bigvee_x \chi(x)@f$ in the species
 *  over the carrier's own elements: on @c Ø both are ⊥, on @c 𝔸 both ⊤. */
export template <IsStepSource S>
constexpr auto forall(const S& s) {
  return detail_runs::fold_over_carrier(
      s, detail_runs::And<typename S::logic_species>{});
}
export template <IsStepSource S>
constexpr auto exists(const S& s) {
  return detail_runs::fold_over_carrier(
      s, detail_runs::Or<typename S::logic_species>{});
}
/** @brief Equality of two sources as sets of the carrier, in the species:
 *  @f$\bigwedge_x (\chi_a(x) \Leftrightarrow \chi_b(x))@f$, so over K₃ two
 *  sets agree to degree U where either is U. */
export template <IsStepSource A, IsStepSource B>
  requires std::same_as<typename A::Domain, typename B::Domain> &&
           std::same_as<typename A::logic_species, typename B::logic_species>
constexpr auto equal(const A& a, const B& b) {
  using L = typename A::logic_species;
  return forall(MergeSteps<A, B, detail_runs::Iff<L>>{a, b, {}});
}
/** @} */

/** @brief A piece on which a two-valued source holds: a start (@c nullopt
 *  below every element) and an end (@c nullopt above).  @tparam V the
 *  carrier. */
export template <typename V>
struct Piece {
  std::optional<Position<V>> start;
  std::optional<Position<V>> end;
  friend constexpr bool operator==(const Piece&, const Piece&) = default;
};
/** @brief The pieces of a two-valued source over the carrier, in order: its
 *  steps read in enter / leave pairs, a step outside the carrier read as the
 *  carrier's end.  A forward view.  @tparam R the steps.  @tparam V the
 *  carrier. */
template <typename R, typename V>
class PiecesView : public std::ranges::view_interface<PiecesView<R, V>> {
 public:
  constexpr PiecesView() = default;
  constexpr PiecesView(R r, bool first) : r_(std::move(r)), first_(first) {}

  class iterator {
   public:
    using iterator_concept = std::forward_iterator_tag;
    using value_type = Piece<V>;
    using difference_type = std::ptrdiff_t;
    constexpr iterator() = default;
    constexpr explicit iterator(PiecesView* p)
        : p_(p), it_(std::ranges::begin(p->r_)), inside_(p->first_) {
      advance();
    }
    constexpr const value_type& operator*() const { return cur_; }
    constexpr iterator& operator++() {
      advance();
      return *this;
    }
    constexpr iterator operator++(int) {
      iterator before = *this;
      advance();
      return before;
    }
    friend constexpr bool operator==(const iterator& x, const iterator& y) {
      if (!x.live_ || !y.live_) return !x.live_ && !y.live_;
      return x.it_ == y.it_;
    }

   private:
    constexpr void advance() {
      live_ = false;
      for (; !done_ && it_ != std::ranges::end(p_->r_); ++it_) {
        const auto& st = *it_;
        if (detail_position::before_all(st.at)) {
          inside_ = static_cast<bool>(st.value);
          continue;
        }
        if (detail_position::after_all(st.at)) break;
        if (st.value) {
          open_ = st.at;
          opened_ = inside_ = true;
        } else if (inside_) {
          cur_ = Piece<V>{start(), st.at};
          live_ = true;
          opened_ = inside_ = false;
          ++it_;
          return;
        }
      }
      if (!done_ && inside_) {
        cur_ = Piece<V>{start(), std::nullopt};
        live_ = true;
      }
      done_ = true;
    }
    /** @brief Where the open run starts: a step inside the carrier, or below
     *  every element. */
    constexpr std::optional<Position<V>> start() const {
      if (opened_) return open_;
      return std::nullopt;
    }
    PiecesView* p_ = nullptr;
    std::ranges::iterator_t<R> it_{};
    bool inside_ = false;
    bool done_ = false;
    // The open run's start and the current piece: values and flags, not
    // optionals, whose reset would end the value's lifetime under a copy in
    // constant evaluation.
    Position<V> open_{};
    bool opened_ = false;
    value_type cur_{};
    bool live_ = false;
  };
  constexpr iterator begin() { return iterator{this}; }
  constexpr iterator end() { return {}; }

 private:
  R r_;
  bool first_ = false;
};
/** @brief The pieces of a two-valued source, as a forward view. */
export template <IsStepSource S>
  requires std::same_as<typename S::logic_species, Boole>
constexpr auto pieces(const S& s) {
  return PiecesView<decltype(s.steps()), typename S::Domain>{
      s.steps(), static_cast<bool>(s.first())};
}

/** @brief A maximal run of membership as its closed ends, @c [lo, hi].
 *  @tparam C the chain. */
export template <typename C>
struct Run {
  C lo;
  C hi;
  friend constexpr bool operator==(const Run&, const Run&) = default;
};
namespace detail_runs {
/** @brief A piece of a discrete chain with ends, read closed: a canonical
 *  start is just below its first element, an end just below the element after
 *  its last; an absent side at the carrier's end. */
template <typename C>
struct Closed {
  constexpr Run<C> operator()(const Piece<C>& p) const {
    return {p.start ? p.start->at : *detail_position::least<C>(),
            p.end ? *lower_cover(p.end->at) : *detail_position::greatest<C>()};
  }
};
}  // namespace detail_runs
/** @brief The runs of a two-valued source of a finite chain, as a view of
 *  closed pairs: every run that ends does so just below an element with a
 *  lower cover (ℕ's proxy, whose ℵ₀ has none, is read through positions). */
export template <IsStepSource S>
  requires IsFiniteChain<typename S::Domain> &&
           HasNNOStep<typename S::Domain> &&
           (detail_position::least<typename S::Domain>().has_value())
constexpr auto runs(const S& s) {
  return std::views::transform(pieces(s),
                               detail_runs::Closed<typename S::Domain>{});
}

/** @brief A set of the chain @c V valued in @c L, materialised: its
 *  breakpoints, each the value from that position on, and the value before
 *  them.  The one stored form, for a value that must outlive the term it was
 *  read from (a Python handle); a source like any other, its steps a view of
 *  the map.  @tparam V the carrier.  @tparam L the species. */
export template <typename V, typename L = Boole>
  requires IsTotallyOrdered<V> && IsOckhamAlgebra<L>
struct Runs : dedekind::sets::SetExpr<Runs<V, L>, V, L> {
  using Domain = V;
  using Codomain = typename L::Ω;
  using logic_species = L;
  using cardinality_type = carrier_cardinality_t<V>;
  using Ω = typename L::Ω;
  using Map = std::map<Position<V>, Ω>;

  /** @brief The empty set. */
  Runs() = default;
  static Runs empty() { return {}; }
  static Runs universe() { return Runs(SpanSteps<V, L>{Span<V>{}}); }
  /** @brief Materialise a source: drain its steps into the map. */
  template <IsStepSource S>
    requires std::same_as<typename S::Domain, V> &&
             std::same_as<typename S::logic_species, L>
  explicit Runs(const S& s) : first_(s.first()) {
    for (const auto& st : s.steps())
      breakpoints_.emplace_hint(breakpoints_.end(), st.at, st.value);
  }

  /** @brief The breakpoints, in order. */
  const Map& breakpoints() const { return breakpoints_; }
  Ω first() const { return first_; }
  auto steps() const { return std::views::transform(breakpoints_, AsStep{}); }
  /** @brief χ(x): the value from the last breakpoint at or before @c x. */
  Ω operator()(const V& x) const {
    const auto it = breakpoints_.lower_bound(Position<V>{x, Side::Above});
    return it == breakpoints_.begin() ? first_ : std::prev(it)->second;
  }
  bool contains(const V& x) const { return (*this)(x) == L::True; }

 private:
  struct AsStep {
    Step<V, L> operator()(const typename Map::value_type& b) const {
      return {b.first, b.second};
    }
  };
  Ω first_ = L::False;
  Map breakpoints_;
};

static_assert(dedekind::sets::IsSetObject<Runs<int>> &&
                  IsStepSource<Runs<int>> &&
                  IsStepSource<Runs<Ternary, Kleene>>,
              "a run list is a set object over its universe, and a source.");

namespace detail_runs {
/** @brief A breakpoint of a map as a step. */
template <typename V, typename L>
struct ToStep {
  Step<V, L> operator()(
      const std::pair<const Position<V>, typename L::Ω>& b) const {
    return {b.first, b.second};
  }
};
/** @brief A sorted map of breakpoints as a source. */
template <typename V, typename L>
struct MapSteps {
  using Domain = V;
  using logic_species = L;
  typename L::Ω value_before;
  std::map<Position<V>, typename L::Ω> map;
  typename L::Ω first() const { return value_before; }
  auto steps() const { return std::views::transform(map, ToStep<V, L>{}); }
};
}  // namespace detail_runs

/** @brief The translate of a run list by @c k on a machine word, the chain
 *  read as the cyclic group ℤ/2^w it is: @f$x \mapsto x + k@f$ wraps at the
 *  word's end, so a run through the end splits in two.  It is the image of
 *  the set under the translation, and the preimage under the inverse one.
 *  @tparam V an unsigned word.  @tparam L the species.  @param s the set.
 *  @param k the shift. */
export template <std::unsigned_integral V, typename L>
  requires IsAbelianGroup<V, std::plus<V>>  // the word's +, not bool's ∨
Runs<V, L> translate(const Runs<V, L>& s, V k) {
  if (k == V{}) return s;
  // The image's value at 0 is s(−k); each breakpoint moves by k; the word's
  // end wraps to k, where the image reads s(0).  A step past the end names no
  // element and is dropped.
  detail_runs::MapSteps<V, L> out{s(static_cast<V>(V{} - k)), {}};
  for (const auto& [at, v] : s.breakpoints()) {
    if (at.side == Side::Above) continue;
    out.map.insert_or_assign(
        Position<V>{static_cast<V>(at.at + k), Side::Below}, v);
  }
  out.map.insert_or_assign(Position<V>{k, Side::Below}, s(V{}));
  auto prev = out.value_before;  // no breakpoint repeats the value before it
  for (auto it = out.map.begin(); it != out.map.end();) {
    if (it->second == prev) {
      it = out.map.erase(it);
    } else {
      prev = it->second;
      ++it;
    }
  }
  return Runs<V, L>(out);
}

/** @section runs__Formal_Verification */
namespace detail_runs_witness {
using B = SpanSteps<int, Boole>;
using And = detail_runs::And<Boole>;
using Or = detail_runs::Or<Boole>;
constexpr auto closed = Strictness::NonStrict;
constexpr B span(int lo, int hi) {
  return {Span<int>{Bound<int>{lo, closed}, Bound<int>{hi, closed}}};
}
constexpr RelabelSteps<B, detail_runs::Rfl<Boole>> without(int p) {
  return {span(p, p), {}};
}
static_assert(
    equal(MergeSteps<B, decltype(without(5)), And>{span(0, 10), without(5), {}},
          MergeSteps<B, B, Or>{span(0, 4), span(6, 10), {}}),
    "[0,10] ∩ ¬{5} = [0,4] ∪ [6,10], by one traversal");
static_assert(std::ranges::distance(runs(without(5))) == 2 &&
                  std::ranges::distance(runs(B{std::nullopt})) == 0,
              "¬{5} is two runs, Ø none");
}  // namespace detail_runs_witness

/** @section runs__Opaque_Tier
 *  The other half of the normal form (#970, #884): a predicate no run list
 *  reads (an equation of a ring, a quantifier) keeps its atoms, and its
 *  connectives forget the order, grouping and repetition of their operands.
 *  The operands are sorted by a total order on the term trees, by kind first
 *  (the decided comparisons before ∃ before ∀), then structurally; a
 *  constant through its carrier's own order or the hook beside the carrier.
 */
/** @brief The order of two constants of the grammar: their own weak (or
 *  strong) three-way comparison where they have one; a constant type with
 *  none, or only a partial one, names a total order beside it (found by
 *  argument-dependent lookup, as @c carrier_cast).  @param a, b the
 *  constants. */
export template <typename K>
  requires requires(const K& a, const K& b) { std::weak_order(a, b); }
std::weak_ordering constant_order(const K& a, const K& b) {
  return std::weak_order(a, b);
}
export template <typename K>
std::weak_ordering structural_order(const Pred<K>& a, const Pred<K>& b);
/** @brief The structural order of two terms (#884): by kind (a projection, a
 *  constant, an operation, a conditional), then a projection by its slot, a
 *  constant by @c constant_order, an operation by its operator and operands,
 *  a conditional by its guard and branches.  Total; two terms are equivalent
 *  in it exactly when they are the same tree.  @param a, b the terms. */
export template <typename K>
std::weak_ordering structural_order(const Term<K>& a, const Term<K>& b) {
  if (const auto c = a.kind <=> b.kind; c != 0) return c;
  switch (a.kind) {
    case Term<K>::Kind::Proj:
      return a.slot <=> b.slot;
    case Term<K>::Kind::Const:
      return constant_order(a.value, b.value);
    case Term<K>::Kind::Bin:
      if (const auto c = a.op <=> b.op; c != 0) return c;
      if (const auto c = structural_order(*a.a, *b.a); c != 0) return c;
      return structural_order(*a.b, *b.b);
    case Term<K>::Kind::Cond:
      if (const auto c = structural_order(*a.p, *b.p); c != 0) return c;
      if (const auto c = structural_order(*a.a, *b.a); c != 0) return c;
      return structural_order(*a.b, *b.b);
  }
  return std::weak_ordering::equivalent;
}
/** @brief The structural order of two predicates (#884): by kind first, so
 *  the comparisons (decided, Δ) precede the connectives and the quantifiers
 *  (∃ Σ, then ∀ Π); a comparison by its left term, its relation, its right
 *  term; a connective by its operands; a quantifier by its sort, slot and
 *  body.  The key @c canonical sorts a conjunction / disjunction by.
 *  @param a, b the predicates. */
export template <typename K>
std::weak_ordering structural_order(const Pred<K>& a, const Pred<K>& b) {
  if (const auto c = a.kind <=> b.kind; c != 0) return c;
  switch (a.kind) {
    case Pred<K>::Kind::Cmp:
      if (const auto c = structural_order(*a.a, *b.a); c != 0) return c;
      if (const auto c = a.rel <=> b.rel; c != 0) return c;
      return structural_order(*a.b, *b.b);
    case Pred<K>::Kind::And:
    case Pred<K>::Kind::Or:
      if (const auto c = structural_order(*a.p, *b.p); c != 0) return c;
      return structural_order(*a.q, *b.q);
    case Pred<K>::Kind::Exists:
    case Pred<K>::Kind::Forall:
      if (const auto c = a.sort <=> b.sort; c != 0) return c;
      if (const auto c = a.slot <=> b.slot; c != 0) return c;
      return structural_order(*a.p, *b.p);
    case Pred<K>::Kind::Top:
    case Pred<K>::Kind::Bottom:
      break;
  }
  return std::weak_ordering::equivalent;
}
namespace detail_connective {
/** @brief Whether a predicate is of the kind @c k (a bound to fold). */
template <typename K>
struct OfKind {
  typename Pred<K>::Kind k;
  bool operator()(const Pred<K>& q) const { return q.kind == k; }
};
/** @brief @c a before @c b in the structural order. */
struct StructurallyBefore {
  template <typename X>
  bool operator()(const X& a, const X& b) const {
    return structural_order(a, b) < 0;
  }
};
/** @brief @c a and @c b the same tree (equivalent in the structural order). */
struct StructurallySame {
  template <typename X>
  bool operator()(const X& a, const X& b) const {
    return structural_order(a, b) == 0;
  }
};
}  // namespace detail_connective

/** @brief The connective @c k (@c And or @c Or) of the operands, with the
 *  order, grouping and repetition forgotten (#884): the bounds folded (the
 *  unit dropped, an absorber absorbing), the operands sorted by
 *  @c structural_order, each kept once, folded left.  None left: the unit.
 *  @param k the connective.  @param ops its operands (not chains of @c k). */
export template <typename K>
Pred<K> connective(typename Pred<K>::Kind k, std::vector<Pred<K>> ops) {
  using Pr = Pred<K>;
  const bool meet = k == Pr::Kind::And;
  const auto absorber = meet ? Pr::Kind::Bottom : Pr::Kind::Top;
  const auto unit = meet ? Pr::Kind::Top : Pr::Kind::Bottom;
  std::erase_if(ops, detail_connective::OfKind<K>{unit});
  for (const Pr& q : ops)
    if (q.kind == absorber) return q;
  if (ops.empty()) return meet ? Pr::top() : Pr::bottom();
  std::ranges::sort(ops, detail_connective::StructurallyBefore{});
  ops.erase(
      std::ranges::unique(ops, detail_connective::StructurallySame{}).begin(),
      ops.end());
  Pr all = ops.front();
  for (std::size_t i = 1; i < ops.size(); ++i)
    all = meet ? Pr::conj(all, ops[i]) : Pr::disj(all, ops[i]);
  return all;
}

}  // namespace dedekind::order
