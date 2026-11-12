/**
 * @file dedekind/sets/cardinality.cppm
 * @partition :cardinality
 * @brief Cardinality types and the one-word extensional cardinal carrier.
 *
 * @details
 * This partition owns all concrete cardinality types:
 *   - `Finite`, `ℵ<N>`, `ℵ_0`, `ℶ_1` — the cardinality ladder (moved from
 *     `:mereology`, where only the structural concepts now remain).
 *   - `ExtensionalCardinal<W>` — the machine-total finite cardinal carrier
 *     with wrapping arithmetic, compatible with `dedekind.category:total`.
 *   - `Numeral` — the limbs of a natural, internal: the value ℕ's canonical
 *     form is read from (`:expr`), never a carrier a program holds.
 *
 * Cardinality is a property of sets — how many elements a set contains —
 * so it belongs in the `sets` layer, below `numbers` and `algebra`.
 * The `numbers:cardinality` partition re-exports this and adds the
 * `IsNatural` and `IsTotallyOrdered` proofs that require the `numbers`
 * and `order` layers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Das Wesen der Mathematik liegt in ihrer Freiheit."
 *       ("The essence of mathematics lies entirely in its freedom.")
 *       -- Georg Cantor, Grundlagen einer allgemeinen
 *          Mannigfaltigkeitslehre (1883)
 */
module;

#include <algorithm>  // std::ranges::find_if_not
#include <bit>        // the limbs' width
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>  // a numeral's limbs, std::uint64_t
#include <functional>
#include <iterator>  // reverse iterators: the limbs read from the top
#include <limits>
#include <optional>     // a numeral that is a word, or not
#include <ranges>       // std::views::iota: the limbs walked as a fold
#include <type_traits>  // std::remove_cvref_t --- IsRingIntegral normalisation
#include <utility>
#include <vector>  // a numeral's limbs above the first

export module dedekind.sets:cardinality;

import dedekind.category;

namespace dedekind::sets {

using namespace dedekind::category;

// ---------------------------------------------------------------------------
// Cardinality ladder (moved from :mereology)
// ---------------------------------------------------------------------------

/** @concept IsCardinality
 *  @brief A type-level tag for the size of a species: whether it is finite
 *  and whether it is countable.  A tag about sets, not a number: cardinal
 *  arithmetic is the term algebra's (#1035).  @tparam C the tag. */
export template <typename C>
concept IsCardinality = requires {
  { C::is_finite } -> std::convertible_to<bool>;
  { C::is_countable } -> std::convertible_to<bool>;
};

/** @concept IsCountable: Magnitude is at most Aleph_0. */
export template <typename C>
concept IsCountable = IsCardinality<C> && (C::is_countable == true);

/** @concept IsFinite: Strictly terminating. */
export template <typename C>
concept IsFinite = IsCountable<C> && (C::is_finite == true);

/** @struct Finite: Hardware-bound magnitude. */
export struct Finite {
  static constexpr bool is_finite = true;
  static constexpr bool is_countable = true;

  auto operator<=>(const Finite&) const = default;
};

static_assert(IsCardinality<Finite>);
static_assert(IsFinite<Finite>);

/** @brief Primary trait: Is a species defined by its members? */
export template <typename T>
struct is_extensional : std::false_type {};

// Atomic Proof: Integrals are always extensional.
template <std::integral T>
struct is_extensional<T> : std::true_type {};

/** @concept IsExtensional (The Proof)
 *
 *  @note The @c size() branch is guarded by a @c requires expression so the
 *  member-access lookup happens in the immediate context: types without a
 *  @c size() member (e.g. @c int) cleanly fail this branch and fall through
 *  to the @c is_extensional trait specialization, rather than producing a
 *  hard error. */
export template <typename S>
concept IsExtensional = requires(const S& s) {
  { s.size() } -> std::same_as<std::size_t>;
} || is_extensional<S>::value;

/** @struct ℵ: The Transfinite Ladder. */
export template <std::size_t N>
struct ℵ {
  static constexpr bool is_finite = false;
  static constexpr bool is_countable = (N == 0);

  constexpr friend bool operator==(const ℵ&, const ℵ&) = default;
};

export using ℵ_0 = ℵ<0>;  ///< Countable Infinity
/** @struct ℶ_1
 *  @brief The continuum, @f$2^{\aleph_0}@f$, by definition: the size of the
 *  power set of a countable set.  Not @c ℵ<1>: @f$\aleph_1 = \beth_1@f$ is
 *  the continuum hypothesis, which ZFC neither proves nor refutes, so no tag
 *  assumes it.  @f$\aleph_0 < \beth_1@f$ and @f$\aleph_1 \le \beth_1@f$
 *  are theorems; @f$\aleph_n@f$ against @f$\beth_1@f$ for @f$n \ge 2@f$ is
 *  not decided. */
export struct ℶ_1 {
  static constexpr bool is_finite = false;
  static constexpr bool is_countable = false;
  constexpr friend bool operator==(const ℶ_1&, const ℶ_1&) = default;
};
static_assert(IsCardinality<ℶ_1> && !IsCountable<ℶ_1> &&
                  !std::same_as<ℶ_1, ℵ<1>>,
              "the continuum is uncountable, and not ℵ_1: that is CH");

// ---------------------------------------------------------------------------
// The finite cardinals as numerals: ℕ, not a word
// ---------------------------------------------------------------------------

struct Numeral;
namespace detail_numeral {
// The operations of the limbs, behind Numeral's hidden friends, so that no
// overload set outside the module meets them (sets:terms exports its own).
constexpr Numeral plus(const Numeral& x, const Numeral& y);
constexpr Numeral times(const Numeral& x, const Numeral& y);
constexpr std::strong_ordering order(const Numeral& x, const Numeral& y);
}  // namespace detail_numeral

/** @section cardinality__Numeral  A natural's limbs, internal to ℕ's form
 *  ℕ is its terms (@c :expr's @c NatExpr): @c size_t atoms and @c + and @c *
 *  as nodes, nothing evaluated, so no term overflows.  A @c Numeral is the
 *  value a term's canonical form is read from and written back from, not
 *  exported: equality of terms is equality of canonical forms, and decides
 *  both ways.  The working
 *  definition of "arbitrarily large, but bounded" (#1035): an atom is a
 *  @c size_t, and a value above @f$2^{64}@f$ exists only as a term or as its
 *  normal form, never as an atom.  The representation says the same: one limb
 *  inline, the atom's word, and the limbs above it in a vector that is empty
 *  below @f$2^{64}@f$, so @c 0 and @c 1 are constants that own nothing and the
 *  heap is reached only where a word cannot.  Little-endian limbs, none
 *  trailing zero: the form is unique and @c == is memberwise.  Arithmetic is
 *  schoolbook in base @f$2^{64}@f$, by folds over the limbs (Rule 3); it need
 *  not be fast, it is exact.  The word is this numeral's quotient by
 *  @f$2^{64}@f$ (@c ExtensionalCardinal): below it the two agree exactly,
 *  which is where the paper's listings live. */
struct Numeral {
  using limb = std::uint64_t;
  static_assert(std::numeric_limits<std::size_t>::digits <= 64,
                "an atom, a size_t, fits the inline limb");
  /** @brief ℕ is countable: @c :order's halfspaces read this, as they read
   *  ℚ's, since @c IsRingIntegral is false on a numeral. */
  using cardinality_type = ℵ_0;

  constexpr Numeral() = default;
  /** @param n an atom: ℕ's atoms are the unsigned words. */
  template <std::unsigned_integral N>
  constexpr explicit Numeral(N n) : low_(static_cast<limb>(n)) {}
  /** @param limbs little-endian; normalised here, trailing zeros dropped. */
  constexpr explicit Numeral(std::vector<limb> limbs)
      : low_(limbs.empty() ? limb{} : limbs.front()),
        high_(limbs.empty()
                  ? std::vector<limb>{}
                  : std::vector<limb>(limbs.begin() + 1, limbs.end())) {
    high_.erase(
        std::ranges::find_if_not(high_.rbegin(), high_.rend(), IsZeroLimb{})
            .base(),
        high_.end());
  }
  constexpr bool operator==(const Numeral&) const = default;
  /** @brief The sum, the product and the order, exact (below). */
  friend constexpr Numeral operator+(const Numeral& x, const Numeral& y) {
    return detail_numeral::plus(x, y);
  }
  friend constexpr Numeral operator*(const Numeral& x, const Numeral& y) {
    return detail_numeral::times(x, y);
  }
  friend constexpr std::strong_ordering operator<=>(const Numeral& x,
                                                    const Numeral& y) {
    return detail_numeral::order(x, y);
  }
  /** @return the first limb: the numeral modulo @f$2^{64}@f$. */
  constexpr limb low() const { return low_; }
  /** @return the number of limbs, the inline one counted: no allocation. */
  constexpr std::size_t limb_count() const { return 1 + high_.size(); }
  /** @return every limb, little-endian, the inline one first. */
  constexpr std::vector<limb> limbs() const {
    std::vector<limb> v{low_};
    v.insert(v.end(), high_.begin(), high_.end());
    return v;
  }
  /** @return the word, where the numeral is one: below @f$2^{64}@f$. */
  constexpr std::optional<std::size_t> word() const {
    if (high_.empty() && low_ <= std::numeric_limits<std::size_t>::max())
      return static_cast<std::size_t>(low_);
    return std::nullopt;
  }

 private:
  struct IsZeroLimb {
    constexpr bool operator()(limb l) const { return l == limb{}; }
  };
  // The normal form: no trailing zero limb, so == is memberwise.  The vector
  // constructor normalises; writable members would not.
  limb low_{};  // the first limb: an atom is this, and nothing above
  std::vector<limb> high_{};  // the limbs above, little-endian
};

namespace detail_numeral {
using limb = Numeral::limb;
using wide = __uint128_t;  // a limb product, with its carry
/** @return the limb at @c i, zero beyond the end. */
constexpr limb at(const std::vector<limb>& v, std::size_t i) {
  return i < v.size() ? v[i] : limb{};
}
/** @brief Whether both numerals are one limb: the gate of the single-limb
 *  fast paths.  Each path is a certificate about evaluation: on one limb the
 *  word's own arithmetic computes the same normal form as the fold over the
 *  limbs, where its result fits the word, so the fold is skipped and nothing
 *  is allocated.  Where the result does not fit, the fold runs. */
constexpr bool words(const Numeral& x, const Numeral& y) {
  return x.limb_count() == 1 && y.limb_count() == 1;
}
/** @brief The state of a carried walk over limbs: those done, and the carry. */
struct Carried {
  std::vector<limb> limbs{};
  limb carry{};
};
/** @brief One step of @c a @c + @c b: the limbs at @c i and the carry. */
struct AddLimbs {
  std::vector<limb> a, b;
  constexpr Carried operator()(Carried s, std::size_t i) const {
    const wide w = wide{at(a, i)} + at(b, i) + s.carry;
    s.limbs.push_back(static_cast<limb>(w));
    s.carry = static_cast<limb>(w >> 64);
    return s;
  }
};
/** @brief One step of @c a @c * @c k: the limb at @c i times @c k, and the
 *  carry. */
struct ScaleLimbs {
  std::vector<limb> a;
  limb k;
  constexpr Carried operator()(Carried s, std::size_t i) const {
    const wide w = wide{a[i]} * k + s.carry;
    s.limbs.push_back(static_cast<limb>(w));
    s.carry = static_cast<limb>(w >> 64);
    return s;
  }
};
/** @brief One step of @c a @c - @c b for @c a @c >= @c b: the limbs at @c i
 *  less the borrow, the carry holding the borrow. */
struct SubtractLimbs {
  std::vector<limb> a, b;
  constexpr Carried operator()(Carried s, std::size_t i) const {
    const wide taken = wide{at(b, i)} + s.carry;
    s.limbs.push_back(static_cast<limb>(wide{a[i]} - taken));
    s.carry = wide{a[i]} < taken ? limb{1} : limb{};
    return s;
  }
};
/** @return @c a @c + @c b, with a final carry limb; the constructor drops it
 *  when zero. */
constexpr std::vector<limb> add(std::vector<limb> a, std::vector<limb> b) {
  const std::size_t n = std::max(a.size(), b.size());
  Carried s =
      dedekind::category::fold(std::views::iota(std::size_t{0}, n), Carried{},
                               AddLimbs{std::move(a), std::move(b)});
  s.limbs.push_back(s.carry);
  return s.limbs;
}
/** @return @c a @c * @c k shifted up by @c shift limbs: one row of the
 *  schoolbook product. */
constexpr std::vector<limb> row(const std::vector<limb>& a, limb k,
                                std::size_t shift) {
  Carried s = dedekind::category::fold(
      std::views::iota(std::size_t{0}, a.size()),
      Carried{std::vector<limb>(shift, limb{}), limb{}}, ScaleLimbs{a, k});
  s.limbs.push_back(s.carry);
  return s.limbs;
}
/** @return @c a @c - @c b, for @c a @c >= @c b: the step down of ℕ and the
 *  magnitude difference of ℤ's normal form, internal to both; ℕ has no
 *  subtraction. */
constexpr std::vector<limb> subtract(std::vector<limb> a, std::vector<limb> b) {
  const std::size_t n = a.size();
  return dedekind::category::fold(std::views::iota(std::size_t{0}, n),
                                  Carried{},
                                  SubtractLimbs{std::move(a), std::move(b)})
      .limbs;
}
/** @brief One step of @c a @c * @c b: the row for @c b's limb @c j, added. */
struct AddRow {
  std::vector<limb> a, b;
  constexpr std::vector<limb> operator()(std::vector<limb> acc,
                                         std::size_t j) const {
    return add(std::move(acc), row(a, b[j], j));
  }
};
}  // namespace detail_numeral

/** @brief The sum, exact; on one limb each, the word's sum where it fits. */
constexpr Numeral detail_numeral::plus(const Numeral& x, const Numeral& y) {
  if (detail_numeral::words(x, y)) {
    const detail_numeral::wide w = detail_numeral::wide{x.low()} + y.low();
    if ((w >> 64) == 0) return Numeral{static_cast<Numeral::limb>(w)};
  }
  return Numeral(detail_numeral::add(x.limbs(), y.limbs()));
}
/** @brief The product, exact: schoolbook, a row per limb of @c y; on one
 *  limb each, the word's product where it fits. */
constexpr Numeral detail_numeral::times(const Numeral& x, const Numeral& y) {
  if (detail_numeral::words(x, y)) {
    const detail_numeral::wide w = detail_numeral::wide{x.low()} * y.low();
    if ((w >> 64) == 0) return Numeral{static_cast<Numeral::limb>(w)};
  }
  const std::vector<Numeral::limb> a = x.limbs();
  const std::vector<Numeral::limb> b = y.limbs();
  return Numeral(dedekind::category::fold(
      std::views::iota(std::size_t{0}, b.size()), std::vector<Numeral::limb>{},
      detail_numeral::AddRow{a, b}));
}
/** @brief The order of ℕ: more limbs is larger, else the top limb decides. */
constexpr std::strong_ordering detail_numeral::order(const Numeral& x,
                                                     const Numeral& y) {
  if (x.limb_count() != y.limb_count())
    return x.limb_count() <=> y.limb_count();
  if (detail_numeral::words(x, y)) return x.low() <=> y.low();
  const std::vector<Numeral::limb> a = x.limbs();
  const std::vector<Numeral::limb> b = y.limbs();
  return std::lexicographical_compare_three_way(a.rbegin(), a.rend(),
                                                b.rbegin(), b.rend());
}
namespace detail_numeral {
/** @brief The state of a division by shift and subtract: the quotient's bits
 *  so far, and the remainder. */
struct Divided {
  Numeral q;
  Numeral r;
};
/** @brief One step of @c a @c / @c d over the dividend's bits, from the top:
 *  the remainder doubles and takes the bit; where it reaches @c d, @c d is
 *  taken from it and the quotient's bit is set. */
struct DivideBit {
  std::vector<limb> a;  // the dividend's limbs
  Numeral d;
  constexpr Divided operator()(Divided s, std::size_t i) const {
    const std::size_t bit = a.size() * 64 - 1 - i;
    const Numeral taken{(a[bit / 64] >> (bit % 64)) & limb{1}};
    Numeral r = s.r + s.r + taken;
    Numeral q = s.q + s.q;
    if (d <= r) {
      r = Numeral(subtract(r.limbs(), d.limbs()));
      q = q + Numeral{1u};
    }
    return Divided{std::move(q), std::move(r)};
  }
};
/** @brief One step of @c a @c / @c k for a one-limb @c k, over the limbs from
 *  the top: the remainder carried into the next limb, the quotient's limb
 *  written (top first; reversed after). */
struct DivideLimb {
  std::vector<limb> a;  // the dividend's limbs
  limb k;
  constexpr Carried operator()(Carried s, std::size_t i) const {
    const wide cur = (wide{s.carry} << 64) | a[a.size() - 1 - i];
    s.limbs.push_back(static_cast<limb>(cur / k));
    s.carry = static_cast<limb>(cur % k);
    return s;
  }
};
/** @return the quotient and remainder of @c a by @c d: on one limb each the
 *  word's; by a one-limb @c d, limb by limb, a fold of one step per limb of
 *  @c a; else shift and subtract over the dividend's bits, a fold of 64
 *  steps per limb of @c a.  A zero divisor is undefined, as for the words
 *  ([expr.mul]/4), and refused by constant evaluation; a divisor certified
 *  non-zero by its type is ℚ's (#1035). */
constexpr Divided divide(const Numeral& a, const Numeral& d) {
  if (words(a, d))
    return {Numeral{a.low() / d.low()}, Numeral{a.low() % d.low()}};
  if (d == Numeral{}) return {Numeral{limb{1} / d.low()}, Numeral{}};
  if (a < d) return {Numeral{}, a};
  if (d.limb_count() == 1) {
    const Carried s = dedekind::category::fold(
        std::views::iota(std::size_t{0}, a.limb_count()), Carried{},
        DivideLimb{a.limbs(), d.low()});
    return {Numeral(std::vector<limb>(s.limbs.rbegin(), s.limbs.rend())),
            Numeral{s.carry}};
  }
  return dedekind::category::fold(
      std::views::iota(std::size_t{0}, a.limb_count() * 64), Divided{},
      DivideBit{a.limbs(), d});
}
/** @brief Euclid's step on a pair, @f$(a, b) \mapsto (b, a \bmod b)@f$, and
 *  the pair itself at @f$b = 0@f$: its fixed point, where @c iterate stops. */
struct Pair {
  Numeral a;
  Numeral b;
  constexpr bool operator==(const Pair&) const = default;
};
struct EuclidStep {
  constexpr Pair operator()(const Pair& p) const {
    if (p.b == Numeral{}) return p;
    return Pair{p.b, divide(p.a, p.b).r};  // `%` is declared below
  }
};
/** @return Lamé's bound on Euclid's steps @cite lame1844note (Knuth
 *  @cite knuth1997art2 §4.5.3): fewer than @f$1.45@f$ per bit of the smaller
 *  argument, here its limbs' bits; and two more, the step that sees the fixed
 *  point and a first swap. */
constexpr std::size_t lame_bound(const Numeral& smaller) {
  return (smaller.limb_count() * 64 * 145 + 99) / 100 + 2;
}
}  // namespace detail_numeral

/** @brief The greatest common divisor, Euclid's: the step iterated to its
 *  fixed point under Lamé's bound, so Rule 3 holds by the declared bound and
 *  the early exit costs the steps taken (@c iterate, @c :morphism).
 *  @f$\gcd(n, 0) = n@f$.  It is the definition of ℚ's canonical form, the
 *  reduced fraction (@c :expr), not ℚ's arithmetic: internal to
 *  @c canonical, with the division it needs. */
constexpr Numeral gcd(const Numeral& x, const Numeral& y) {
  return dedekind::category::iterate(detail_numeral::EuclidStep{},
                                     detail_numeral::Pair{x, y},
                                     detail_numeral::lame_bound(std::min(x, y)))
      .a;
}
// ---------------------------------------------------------------------------
// Extensional cardinal carrier
// ---------------------------------------------------------------------------

/**
 * @brief Finite extensional cardinal: one machine word @c W.
 *
 * @details The bare carrier is the wrapping ring ℤ/2^w (registered periodic
 * and cyclic below); ℕ itself is its terms, @c NatExpr, exact above the word.
 * It is not a bignum: its width is the width of @c W.  Overflow is detected
 * with the compiler's checked-arithmetic builtins, which compute exactly for
 * any word width, so no promotion to @c int can overflow on the way.
 *
 * @tparam W The word (default @c std::size_t).
 */
export template <IsCountWord W = std::size_t>
struct ExtensionalCardinal {
  using cardinality_type = Finite;
  using word_type = W;

  /** @brief A wrapped result and whether the exact result overflowed @c W. */
  struct CheckedResult {
    ExtensionalCardinal result;
    bool overflowed;
  };

  W value{};

  constexpr ExtensionalCardinal() = default;

  constexpr ExtensionalCardinal(W v) noexcept  // NOLINT
      : value(v) {}

  /** @brief Construction from any integral type via two's-complement wrapping.
   *  The explicit @c static_cast keeps the sign-conversion at this boundary
   *  rather than at each call site.
   *
   *  Intentionally non-explicit so integral values convert implicitly. */
  template <std::integral S>
    requires(!std::same_as<S, W>)
  constexpr ExtensionalCardinal(S v) noexcept  // NOLINT
      : value(static_cast<W>(v)) {}

  /** @brief Explicit projection to @c std::size_t.  Used by
   *  @c :sequences:path 's helpers when the user-chosen @c Index Form flows
   *  through a @c std::size_t counter.  Explicit, so a word wider than
   *  @c std::size_t is never narrowed silently. */
  explicit constexpr operator std::size_t() const noexcept {
    return static_cast<std::size_t>(value);
  }

  constexpr friend bool operator==(const ExtensionalCardinal&,
                                   const ExtensionalCardinal&) = default;

  constexpr friend std::strong_ordering operator<=>(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    return lhs.value <=> rhs.value;
  }

  constexpr friend ExtensionalCardinal operator+(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    return checked_add(lhs, rhs).result;
  }

  constexpr friend ExtensionalCardinal operator*(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    return checked_mul(lhs, rhs).result;
  }

  constexpr friend ExtensionalCardinal operator-(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    return lhs + (-rhs);
  }

  /** @brief Euclidean division.  Division by zero yields zero by convention
   *  (total). */
  constexpr friend ExtensionalCardinal operator/(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    if (rhs.value == W{0}) return ExtensionalCardinal{};
    return ExtensionalCardinal{static_cast<W>(lhs.value / rhs.value)};
  }

  /** @brief Euclidean modulo.  Division by zero yields @c lhs by convention
   *  (total). */
  constexpr friend ExtensionalCardinal operator%(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    if (rhs.value == W{0}) return lhs;
    return ExtensionalCardinal{static_cast<W>(lhs.value % rhs.value)};
  }

  /** @brief The additive inverse in ℤ/2^w. */
  constexpr ExtensionalCardinal operator-() const noexcept {
    return ExtensionalCardinal{static_cast<W>(W{0} - value)};
  }

  /** @brief The value as @c std::size_t, or @p sentinel when it does not
   *  fit (only possible for a word wider than @c std::size_t). */
  constexpr std::size_t realize_to_size_t(
      std::size_t sentinel =
          std::numeric_limits<std::size_t>::max()) const noexcept {
    if constexpr (std::numeric_limits<W>::digits >
                  std::numeric_limits<std::size_t>::digits) {
      if (value > static_cast<W>(std::numeric_limits<std::size_t>::max()))
        return sentinel;
    }
    return static_cast<std::size_t>(value);
  }

  static constexpr CheckedResult checked_add(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    W sum{};
    const bool overflowed = __builtin_add_overflow(lhs.value, rhs.value, &sum);
    return CheckedResult{ExtensionalCardinal{sum}, overflowed};
  }

  static constexpr CheckedResult checked_mul(
      const ExtensionalCardinal& lhs, const ExtensionalCardinal& rhs) noexcept {
    W product{};
    const bool overflowed =
        __builtin_mul_overflow(lhs.value, rhs.value, &product);
    return CheckedResult{ExtensionalCardinal{product}, overflowed};
  }
};

/** @brief Additive inverse of ExtensionalCardinal<> (two's-complement). */
export constexpr ExtensionalCardinal<> inverse(
    ExtensionalCardinal<> value, std::plus<ExtensionalCardinal<>>) {
  return -value;
}

/** @brief Generic additive inverse for a carrier supporting unary negation: the
 *  @c +-inverse @b is @c -x, inferred from the negation structure rather than
 *  registered per carrier.
 *
 *  @details This is the canonical @c inverse(x, @c op) bridge that
 *  @c category::inverse_v (and the reified group-inverse arrow
 *  @c algebra::Inverse<T, @c +>, #959) consume.  It covers ℤ's @c IntExpr
 *  and any sibling additive carrier, so the additive inverse need not be
 *  hand-registered per type.  The @c ExtensionalCardinal<>
 *  overload above is a more-specialised non-template and still wins for it.
 *
 *  @note The gate is @b unary @b negation (@c {-x}), the @b computational
 *  witness of an additive inverse --- @b not @c IsGroup.  Gating on @c IsGroup
 *  would be circular: @c IsGroup routes through @c IsInvertible /
 *  @c is_invertible_v, and forcing that evaluation for every candidate @c T
 *  during overload resolution trips a non-constant-expression substitution.
 *  Unary @c - is the honest, SFINAE-friendly signal that @c -x is the inverse.
 */
export template <typename T>
  requires requires(T x) {
    { -x } -> std::convertible_to<T>;
  }
constexpr T inverse(const T& x, std::plus<T>) {
  return -x;
}

/** @brief The NNO's step on the extensional cardinals, the wrapping ring
 *  @f$\mathbb{Z}/2^w@f$: @c n + 1 and @c n - 1 modulo the word, as on the
 *  word itself.  @param n a cardinal.  @return its successor. */
export template <IsCountWord W>
constexpr ExtensionalCardinal<W> successor(
    const ExtensionalCardinal<W>& n) noexcept {
  return n + ExtensionalCardinal<W>{W{1}};
}
/** @param n a cardinal.  @return its predecessor, modulo the word. */
export template <IsCountWord W>
constexpr ExtensionalCardinal<W> predecessor(
    const ExtensionalCardinal<W>& n) noexcept {
  return n - ExtensionalCardinal<W>{W{1}};
}
}  // namespace dedekind::sets

// The cardinals are integer chains: registered with @c :chain's
// @c IsIntegerChain, which refines the NNO's step.
namespace dedekind::category {
template <typename W>
struct is_integer_chain<dedekind::sets::ExtensionalCardinal<W>>
    : std::true_type {};
}  // namespace dedekind::category

namespace dedekind::sets {
/** @brief @c :chain's @c IsIntegerChain. */
export using dedekind::category::IsIntegerChain;
/** @brief The old name of @c IsIntegerChain, which never claimed a ring.
 *  @deprecated Use @c IsIntegerChain.  @tparam T the carrier. */
export template <typename T>
concept IsRingIntegral = IsIntegerChain<T>;

// Positive witnesses: built-in integrals and the word cardinal.
static_assert(IsRingIntegral<int>);
static_assert(IsRingIntegral<unsigned int>);
static_assert(IsRingIntegral<long>);
static_assert(IsRingIntegral<long long>);
static_assert(IsRingIntegral<std::size_t>);
static_assert(IsRingIntegral<bool>);
static_assert(IsRingIntegral<ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must satisfy IsRingIntegral — the bounded "
              "word cardinal.");

// Cv-/ref-qualified spellings normalise via std::remove_cvref_t, so the concept
// fires in deduced contexts.
static_assert(IsRingIntegral<const int>);
static_assert(IsRingIntegral<int&>);
static_assert(IsRingIntegral<long&&>);

// Negative witnesses: continuous / non-integer carriers correctly refused.
static_assert(!IsRingIntegral<double>);
static_assert(!IsRingIntegral<float>);
static_assert(!IsRingIntegral<long double>);

}  // namespace dedekind::sets

// ---------------------------------------------------------------------------
// Category trait registrations for ExtensionalCardinal<>
// ---------------------------------------------------------------------------

namespace dedekind::category {
using namespace dedekind::sets;

using C1 = dedekind::sets::ExtensionalCardinal<>;

template <>
struct unit_trait<C1, std::plus<C1>> {
  using value_type = C1;
  static constexpr value_type value = value_type{0};
};

template <>
struct unit_trait<C1, std::multiplies<C1>> {
  using value_type = C1;
  static constexpr value_type value = value_type{1};
};

template <>
inline constexpr bool is_associative_v<C1, std::plus<C1>> = true;

template <>
inline constexpr bool is_associative_v<C1, std::multiplies<C1>> = true;

template <>
inline constexpr bool is_commutative_v<C1, std::plus<C1>> = true;

template <>
inline constexpr bool is_commutative_v<C1, std::multiplies<C1>> = true;

template <>
inline constexpr bool
    is_distributive_v<C1, std::multiplies<C1>, std::plus<C1>> = true;

template <>
struct is_periodic<C1, std::plus<C1>> : std::true_type {};

template <>
struct is_periodic<C1, std::multiplies<C1>> : std::true_type {};

template <typename W>
inline constexpr bool
    is_reflexive_v<dedekind::sets::ExtensionalCardinal<W>, std::less_equal<>> =
        true;

template <typename W>
inline constexpr bool
    is_transitive_v<dedekind::sets::ExtensionalCardinal<W>, std::less_equal<>> =
        true;

template <typename W>
inline constexpr bool is_antisymmetric_v<dedekind::sets::ExtensionalCardinal<W>,
                                         std::less_equal<>> = true;
template <typename W>
inline constexpr bool
    is_connex_v<dedekind::sets::ExtensionalCardinal<W>, std::less_equal<>> =
        true;

static_assert(IsRing<C1, std::plus<C1>, std::multiplies<C1>>,
              "ExtensionalCardinal<> must certify as a total ring.");

// ---------------------------------------------------------------------------
// IsCyclicGroup witnesses for the cardinal carriers (#378).
// ---------------------------------------------------------------------------
//
// `ExtensionalCardinal<W>` is the word cardinal; under addition
// it wraps modulo 2^w, forming a finite cyclic group.
// Order is exposed via `cyclic_order_v`; here we report 0
// ("order not representable as std::size_t") rather than the exact
// 2^w bound --- the concept-level claim is cyclicity,
// not the cardinality value.

template <typename W>
struct is_cyclic_group<dedekind::sets::ExtensionalCardinal<W>,
                       std::plus<dedekind::sets::ExtensionalCardinal<W>>>
    : std::true_type {};

static_assert(IsCyclicGroup<C1, std::plus<C1>>,
              "ExtensionalCardinal<> under + must satisfy IsCyclicGroup "
              "(bounded ℕ, wraps modulo capacity).");

}  // namespace dedekind::category

namespace dedekind::sets::detail_numeral_witness {
constexpr std::uint64_t top = std::numeric_limits<std::uint64_t>::max();
static_assert(Numeral{top} + Numeral{1u} ==
                      Numeral(std::vector<std::uint64_t>{0, 1}) &&
                  (Numeral{top} + Numeral{1u}).word() == std::nullopt &&
                  Numeral{top}.word() == std::size_t{top},
              "2^64 is the first value with a limb above the word");
static_assert(Numeral{top} * Numeral{top} ==
                  Numeral(std::vector<std::uint64_t>{1, top - 1}),
              "(2^64 - 1)^2 = 2^128 - 2^65 + 1: the limbs 1 and 2^64 - 2");
static_assert(Numeral{3u} < Numeral{top} &&
                  Numeral{top} < Numeral{top} + Numeral{1u} &&
                  Numeral(std::vector<std::uint64_t>{5, 0}) == Numeral{5u},
              "the order is by limbs, the longer the larger; a trailing zero "
              "limb is no limb");
static_assert((Numeral{top} + Numeral{top}) * Numeral{3u} ==
                      Numeral{top} * Numeral{3u} + Numeral{top} * Numeral{3u} &&
                  Numeral{3u} * (Numeral{top} + Numeral{1u}) ==
                      (Numeral{top} + Numeral{1u}) * Numeral{3u} &&
                  (Numeral{top} * Numeral{top}) * Numeral{top} ==
                      Numeral{top} * (Numeral{top} * Numeral{top}),
              "the rig laws across the word's edge");
}  // namespace dedekind::sets::detail_numeral_witness
