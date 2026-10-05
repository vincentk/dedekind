/**
 * @file dedekind/numbers/cut.cppm
 * @partition :cut
 * @brief The genuine real: a @b decidable Dedekind cut on @f$\mathbb{Q}@f$.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section cut__Reals_As_Cuts
 * A real number @b is a Dedekind lower cut of @f$\mathbb{Q}@f$ (Dedekind 1872).
 * @c Rational<I> names only the @b principal cuts --- one rational
 * bound, so it can name @f$\mathbb{Q}@f$ but no irrational.  @c Cut<Q> is the
 * leaf that adds the first @b non-principal cuts we can still @b decide:
 *   - @b principal @f$q@f$: the cut @f$\{x\in\mathbb{Q}\mid x<q\}@f$;
 *   - @b radical @f$\pm\sqrt{c}@f$ (@f$c\in\mathbb{Q},\,c\ge 0@f$): the cut
 *     @f$\{x\mid x<0 \lor x^2<c\}@f$ (and its reflection).
 * Membership of a @b rational point, and the order between two such cuts, are
 * both @b decidable by exact @f$\mathbb{Q}@f$ arithmetic (compare @f$q^2@f$ to
 * @f$c@f$) --- so @c Cut<Q> is a genuine @c IsTotallyOrdered carrier, not a
 * postulate.  Arithmetic (@f$+,\times@f$) is @b not yet closed at this leaf:
 * the sum of two cuts is a Minkowski sum whose membership is
 * @f$\exists@f$-over-@f$\mathbb{Q}@f$ (Ternary), and lands with the @c Expr
 * layer that grows @c Cut into the full field.  The order fabric decidable here
 * is exactly what a real @b interval (e.g.\ @f$(-\sqrt2,\sqrt2)@f$) needs.
 *
 * @section cut__The_Cut_Is_Its_Lower_Set
 * The real @b is its lower set: @c lower_set(r) is the comprehension of the cut
 * over @c 𝔸<Q>, an @c IsLSet whose χ is @c contains.  A rational @f$p@f$ embeds
 * as the principal cut, whose lower set is the order datum's ray @f$\{q<p\}@f$
 * (@c principal_ray); a radical cut's lower set @f$\{q<0\lor q^2<c\}@f$ is not
 * a ray over @f$\mathbb{Q}@f$, which is what makes it a new real.
 *
 * @note "Ich sehe es, aber ich glaube es nicht." — Richard Dedekind
 *       (letter to Cantor, 1877).  [Trans: "I see it, but I don't believe it."]
 */
module;

#include <compare>
#include <concepts>
#include <functional>  // std::less_equal (the poset-trait registration key)
#include <stdexcept>   // std::domain_error (release-active radicand guard)
#include <type_traits>

export module dedekind.numbers:cut;

import dedekind.category;
import dedekind.order;
import dedekind.sets;
import :rational;

namespace dedekind::numbers {
using namespace dedekind::category;
using dedekind::order::DownRay;
using dedekind::order::Strictness;
using dedekind::sets::Comprehension;
using dedekind::sets::π;
using dedekind::sets::𝔸;

/**
 * @class Cut
 * @brief A decidable Dedekind real: a principal rational cut or a rational
 *        radical @f$\pm\sqrt{c}@f$.
 *
 * @tparam Q the rational scalar carrier (defaults to @f$\mathbb{Q}@f$).  Only
 *         its ordered-ring surface is used (@c *, unary @c -, @c <=>, @c Q{}),
 *         so every decision is exact.
 */
export template <typename Q = Rational<default_integer>>
  requires IsRational<Q>
class Cut {
  // Q must be the rational field ℚ = Rational<Z>.  Over ℚ the cut order is
  // exact, decidable and strongly ordered; and (the condition QuadraticReal
  // downstream depends on) a nonsquare integer's √D is irrational.  Restricting
  // to ℚ is the single gate that keeps the real carriers genuine — it subsumes
  // strong-ordering / dense / non-integral, and rules out degenerate
  // coefficient fields that already contain √D (see QuadraticReal).
 public:
  using Domain = Cut;
  using ScalarCarrier = Q;

  /** @brief The additive identity @f$0@f$ (principal cut at @f$0@f$). */
  constexpr Cut() = default;

  /** @brief A rational @b is a real: its principal cut. Implicit, so
   *  @f$\mathbb{Q}\hookrightarrow@f$ @c Cut is spelled by ordinary assignment.
   */
  constexpr Cut(Q value)  // NOLINT(google-explicit-constructor)
      : radicand_(value), kind_(Kind::Principal) {}

  /** @brief Single-step implicit lift @c V @c → @c Q @c → @c Cut (e.g.\ an
   *  @c int literal names the principal cut at that integer). */
  template <typename V>
    requires(!std::same_as<V, Q> && std::convertible_to<V, Q>)
  constexpr Cut(V v)  // NOLINT(google-explicit-constructor)
      : Cut(Q{v}) {}

  /** @brief The radical real @f$\sqrt{c}@f$ (requires @f$c\ge 0@f$; a perfect
   *  square is admitted --- it simply compares @c == to its rational root). */
  static constexpr Cut sqrt(Q radicand) {
    if (radicand < Q{})  // √c is real only for c ≥ 0 (release-active, like
                         // Rational's div-by-zero — not an NDEBUG-only assert)
      throw std::domain_error("Cut::sqrt: radicand must be non-negative");
    return Cut{radicand, Kind::Radical, /*neg=*/false};
  }

  /** @brief Additive reflection @f$-x@f$: negate the rational, or flip the
   *  root's sign. */
  constexpr Cut operator-() const {
    if (kind_ == Kind::Principal) return Cut{-radicand_};
    return Cut{radicand_, Kind::Radical, !neg_};
  }

  /** @brief Is the rational @p q strictly below this real (@f$q\in@f$ the lower
   *  cut)?  The decidable membership query the cut is @b defined by: the χ of
   *  @c lower_set(*this). */
  constexpr bool contains(const Q& q) const {
    return compare(Cut{q}, *this) == std::strong_ordering::less;
  }

  /** @brief The order on reals @b is cut-inclusion, decided exactly on the leaf
   *  shapes.  Everything else (@c <, @c >, @c <=, @c >=, @c ==) derives. */
  friend constexpr std::strong_ordering operator<=>(const Cut& a,
                                                    const Cut& b) {
    return compare(a, b);
  }
  friend constexpr bool operator==(const Cut& a, const Cut& b) {
    return compare(a, b) == std::strong_ordering::equal;
  }

 private:
  enum class Kind { Principal, Radical };

  constexpr Cut(Q r, Kind k, bool neg = false)
      : radicand_(r), kind_(k), neg_(neg) {}

  /** @brief @f$p \lessgtr (\text{neg}?-\sqrt{c}:\sqrt{c})@f$, @f$c\ge 0@f$, by
   *  comparing @f$p^2@f$ to @f$c@f$ (the whole decidability of the radical). */
  static constexpr std::strong_ordering cmp_rational_radical(const Q& p,
                                                             const Q& c,
                                                             bool neg) {
    if (!neg) {
      if (p < Q{}) return std::strong_ordering::less;  // p<0 ≤ √c
      return (p * p) <=> c;  // p,√c ≥ 0 ⇒ p<=>√c is p²<=>c
    }
    if (p > Q{}) return std::strong_ordering::greater;  // p>0 ≥ −√c
    return c <=> (p * p);  // p≤0: p<=>−√c reverses (−p)²<=>c
  }

  /** @brief Signed value of a radical leaf: @f$-1,0,+1@f$ for
   *  @f$-\sqrt{c},\,0,\,+\sqrt{c}@f$ (@f$c=0@f$ collapses either sign to 0). */
  constexpr int radical_sign() const {
    if (radicand_ == Q{}) return 0;
    return neg_ ? -1 : 1;
  }

  static constexpr std::strong_ordering compare(const Cut& a, const Cut& b) {
    const bool a_principal = a.kind_ == Kind::Principal;
    const bool b_principal = b.kind_ == Kind::Principal;
    if (a_principal && b_principal) return a.radicand_ <=> b.radicand_;
    if (a_principal)  // a rational vs b = ±√c
      return cmp_rational_radical(a.radicand_, b.radicand_, b.neg_);
    if (b_principal)  // symmetric
      return reverse(cmp_rational_radical(b.radicand_, a.radicand_, a.neg_));
    // both radical: compare signs, then magnitudes (√c₁<=>√c₂ is c₁<=>c₂).
    const int sa = a.radical_sign();
    const int sb = b.radical_sign();
    if (sa != sb) return sa <=> sb;
    if (sa == 0) return std::strong_ordering::equal;
    const std::strong_ordering mag = a.radicand_ <=> b.radicand_;
    return sa > 0 ? mag : reverse(mag);
  }

  Q radicand_{};  ///< Principal: the value; Radical: the radicand.
  Kind kind_{Kind::Principal};
  bool neg_{false};  ///< Radical only: sign of the root.
};

/** @brief The datum of a real's lower set: the real itself, χ(q) =
 *  @c real.contains(q).  @tparam Q the rational scalar carrier. */
export template <IsRational Q>
struct LowerCut {
  using Domain = Q;
  Cut<Q> real{};
  constexpr bool operator()(const Q& q) const { return real.contains(q); }
};

/** @brief The lower set of a real, @f$\{q\in\mathbb{Q}\mid q<r\}@f$, as a set
 *  over @f$\mathbb{Q}@f$.  @tparam Q the rational scalar carrier. */
export template <IsRational Q = Rational<default_integer>>
using LowerSet = Comprehension<𝔸<Q, Boole>, LowerCut<Q>>;

/** @brief @f$r \mapsto \{q<r\}@f$: the real read as its lower set.
 *  @tparam Q the rational scalar carrier. */
export template <IsRational Q>
constexpr LowerSet<Q> lower_set(const Cut<Q>& r) {
  return 𝔸<Q>{} | LowerCut<Q>{r};
}

/** @brief The ray @f$\{q<p\}@f$ over @f$\mathbb{Q}@f$: the lower set of a
 *  principal cut, @f$\mathbb{Q}\hookrightarrow\mathbb{R}@f$ as the order datum.
 *  @tparam Q the rational scalar carrier. */
export template <IsRational Q = Rational<default_integer>>
using PrincipalRay = DownRay<Q, Strictness::Strict>;

/** @brief @f$p \mapsto \{q<p\}@f$: the rational embedded as the principal
 *  ray, extensionally @c lower_set(Cut<Q>{p}).
 *  @tparam Q the rational scalar carrier. */
export template <IsRational Q>
constexpr PrincipalRay<Q> principal_ray(const Q& p) {
  return 𝔸<Q>{} | (π < p);
}

}  // namespace dedekind::numbers

namespace dedekind::category {

/** @brief Atlas registration: @c Cut<Q> is a first-class species. */
template <typename Q>
struct SpeciesTraits<dedekind::numbers::Cut<Q>> {
  using Domain = dedekind::numbers::Cut<Q>;
  using machine_type = dedekind::numbers::Cut<Q>;
};

/** @section cut__Order_Fabric
 *  @c Cut<Q>'s @c <=> is a genuine total order (exact on the leaf shapes), so
 *  the poset traits are @b earned, not postulated.  Registered parametrically
 *  over @c Q --- the same shape @c Rational<I> uses --- so no witness is
 *  cloned per instantiation. */
template <typename Q>
inline constexpr bool
    is_reflexive_v<dedekind::numbers::Cut<Q>, std::less_equal<>> = true;
template <typename Q>
inline constexpr bool
    is_transitive_v<dedekind::numbers::Cut<Q>, std::less_equal<>> = true;
template <typename Q>
inline constexpr bool
    is_antisymmetric_v<dedekind::numbers::Cut<Q>, std::less_equal<>> = true;

}  // namespace dedekind::category

namespace dedekind::numbers {

/** @section cut__Formal_Verification
 *  The cut is a genuine ordered species, and its order is @b decided exactly:
 *  @f$1<\sqrt2<2@f$ and @f$-\sqrt2<0@f$ are compile-time facts, and the lower
 *  cut of @f$\sqrt2@f$ contains @f$1@f$ but not @f$2@f$. */

static_assert(std::regular<Cut<>>,
              "Cut<> is a value type (default + copy + semantic ==).");
static_assert(dedekind::category::IsSpecies<Cut<>>,
              "Cut<> is a first-class species (Atlas-registered).");
static_assert(dedekind::order::IsTotallyOrdered<Cut<>>,
              "Cut<> is totally ordered — the order is EARNED by exact ℚ "
              "arithmetic on the leaf shapes, not postulated.");

// √2 sits strictly between its rational neighbours, decided by q² <=> 2.
static_assert(Cut<>{1} < Cut<>::sqrt(Rational<>{2}), "1 < √2.");
static_assert(Cut<>::sqrt(Rational<>{2}) < Cut<>{2}, "√2 < 2.");
static_assert(-Cut<>::sqrt(Rational<>{2}) < Cut<>{}, "−√2 < 0.");
static_assert(-Cut<>::sqrt(Rational<>{2}) < Cut<>::sqrt(Rational<>{2}),
              "−√2 < √2 (the interval is non-empty).");

// The defining lower cut of √2: contains 1, not 2 (and not √2's own value).
static_assert(Cut<>::sqrt(Rational<>{2}).contains(Rational<>{1}), "1 < √2.");
static_assert(!Cut<>::sqrt(Rational<>{2}).contains(Rational<>{2}),
              "¬(2 < √2).");

// A perfect square radical is exactly its rational root (== via <=>).
static_assert(Cut<>::sqrt(Rational<>{4}) == Cut<>{2}, "√4 = 2.");

using dedekind::category::IsLSet;
using dedekind::order::pivot;

// The cut IS its lower set: a set over ℚ, with contains as its χ.
static_assert(IsLSet<LowerSet<>>,
              "the lower set of a real is a set over ℚ (IsLSet).");
static_assert(lower_set(Cut<>::sqrt(Rational<>{2}))(Rational<>{1}) &&
                  !lower_set(Cut<>::sqrt(Rational<>{2}))(Rational<>{2}),
              "↓√2 ∋ 1, ∌ 2: the radical's lower set is decided by q² <=> 2.");
// ℚ ↪ ℝ: a principal cut's lower set is the order datum's ray {q < p}.
static_assert(IsLSet<PrincipalRay<>>,
              "the principal ray is a set over ℚ (IsLSet).");
static_assert(principal_ray(Rational<>{3})(Rational<>{2}) ==
                      lower_set(Cut<>{Rational<>{3}})(Rational<>{2}) &&
                  principal_ray(Rational<>{3})(Rational<>{3}) ==
                      lower_set(Cut<>{Rational<>{3}})(Rational<>{3}) &&
                  principal_ray(Rational<>{3})(Rational<>{4}) ==
                      lower_set(Cut<>{Rational<>{3}})(Rational<>{4}),
              "the principal cut at p and the ray {q < p} agree on ℚ.");
static_assert(pivot(principal_ray(Rational<>{3})) == Rational<>{3},
              "the embedded rational is the ray's bound.");

}  // namespace dedekind::numbers
