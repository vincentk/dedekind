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
 *   - `Numeral` — ℕ itself, exact above the word.
 *   - `Integer`: ℤ, Grothendieck's completion of `(Numeral, +)`; `rank`
 *     and `IsRanked`, the measure both carry.
 *   - `SignedCardinality`: the saturating ℤ proxy, kept for `Rational`'s
 *     default carrier and the interval ends (#1035's next slice).
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

#include <algorithm>  // std::ranges::fold_left
#include <array>
#include <bit>  // the limbs' width
#include <climits>
#include <cmath>
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>  // a numeral's limbs, std::uint64_t
#include <functional>
#include <iterator>  // reverse iterators: the limbs read from the top
#include <limits>
#include <optional>     // a numeral that is a word, or not
#include <ranges>       // std::views::repeat: the doublings as a fold
#include <type_traits>  // std::remove_cvref_t --- IsRingIntegral normalisation
#include <utility>
#include <variant>
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

/** @section cardinality__Numeral  ℕ, arbitrarily large but bounded
 *  A term over ℕ has @c size_t atoms and @c + and @c * as nodes, the typed
 *  grammar of @c :terms: nothing is evaluated, so no term overflows.  Its
 *  @b normal @b form is a @c Numeral, the exact value, read off by the fold
 *  @c canonical<Numeral> (@c algebra:polynomial, at zero variables); equality
 *  of terms is equality of normal forms, and decides both ways.  The working
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
export struct Numeral {
  using limb = std::uint64_t;
  static_assert(std::numeric_limits<std::size_t>::digits <= 64,
                "an atom, a size_t, fits the inline limb");
  /** @brief ℕ is countable: @c :order's halfspaces read this, as they read
   *  ℚ's, since @c IsRingIntegral is false on a numeral. */
  using cardinality_type = ℵ_0;
  limb low{};  ///< the first limb: an atom is this, and nothing above
  std::vector<limb>
      high{};  ///< the limbs above, little-endian, none trailing zero

  constexpr Numeral() = default;
  /** @param n an atom: ℕ's atoms are the unsigned words. */
  template <std::unsigned_integral N>
  constexpr explicit Numeral(N n) : low(static_cast<limb>(n)) {}
  /** @param limbs little-endian; normalised here, trailing zeros dropped. */
  constexpr explicit Numeral(std::vector<limb> limbs)
      : low(limbs.empty() ? limb{} : limbs.front()),
        high(limbs.empty()
                 ? std::vector<limb>{}
                 : std::vector<limb>(limbs.begin() + 1, limbs.end())) {
    high.erase(
        std::ranges::find_if_not(high.rbegin(), high.rend(), IsZeroLimb{})
            .base(),
        high.end());
  }
  constexpr bool operator==(const Numeral&) const = default;
  /** @return every limb, little-endian, the inline one first. */
  constexpr std::vector<limb> limbs() const {
    std::vector<limb> v{low};
    v.insert(v.end(), high.begin(), high.end());
    return v;
  }
  /** @return the word, where the numeral is one: below @f$2^{64}@f$. */
  constexpr std::optional<std::size_t> word() const {
    if (high.empty() && low <= std::numeric_limits<std::size_t>::max())
      return static_cast<std::size_t>(low);
    return std::nullopt;
  }

 private:
  struct IsZeroLimb {
    constexpr bool operator()(limb l) const { return l == limb{}; }
  };
};

namespace detail_numeral {
using limb = Numeral::limb;
using wide = __uint128_t;  // a limb product, with its carry
/** @return the limb at @c i, zero beyond the end. */
constexpr limb at(const std::vector<limb>& v, std::size_t i) {
  return i < v.size() ? v[i] : limb{};
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

/** @brief The sum, exact. */
export constexpr Numeral operator+(const Numeral& x, const Numeral& y) {
  return Numeral(detail_numeral::add(x.limbs(), y.limbs()));
}
/** @brief The product, exact: schoolbook, a row per limb of @c y. */
export constexpr Numeral operator*(const Numeral& x, const Numeral& y) {
  const std::vector<Numeral::limb> a = x.limbs();
  const std::vector<Numeral::limb> b = y.limbs();
  return Numeral(dedekind::category::fold(
      std::views::iota(std::size_t{0}, b.size()), std::vector<Numeral::limb>{},
      detail_numeral::AddRow{a, b}));
}
/** @brief The order of ℕ: more limbs is larger, else the top limb decides. */
export constexpr std::strong_ordering operator<=>(const Numeral& x,
                                                  const Numeral& y) {
  if (x.high.size() != y.high.size()) return x.high.size() <=> y.high.size();
  const std::vector<Numeral::limb> a = x.limbs();
  const std::vector<Numeral::limb> b = y.limbs();
  return std::lexicographical_compare_three_way(a.rbegin(), a.rend(),
                                                b.rbegin(), b.rend());
}
/** @brief The NNO's step on ℕ: @c n @c + @c 1, past the word's edge. */
export constexpr Numeral successor(const Numeral& n) { return n + Numeral{1u}; }
/** @brief The step down: @c n @c - @c 1, and @c 0 at @c 0, the monus, so the
 *  step is total; @c Prior (@c :nno) stops at @c 0 before asking. */
export constexpr Numeral predecessor(const Numeral& n) {
  if (n == Numeral{}) return n;
  return Numeral(detail_numeral::subtract(n.limbs(), {Numeral::limb{1}}));
}
/** @brief An unsigned constant meeting ℕ, the hook @c :terms' @c carrier_cast
 *  reaches by ADL.  A signed constant has no hook here and is refused there:
 *  ℕ's atoms are the unsigned words. */
export template <std::unsigned_integral K>
constexpr Numeral carrier_cast(std::type_identity<Numeral>, K k) {
  return Numeral{k};
}
/** @brief Lambek on ℕ, honestly: @f$[Z, S] : 1 + \mathbb{N} \to \mathbb{N}@f$
 *  is an isomorphism, its inverse @c Prior, where no word's is (the Lambek
 *  section of @c :nno).  @return the inverse arrow. */
export constexpr dedekind::category::Prior<Numeral> inverse(
    dedekind::category::In<Numeral>) {
  return {};
}
/** @brief The other direction: @c Prior's inverse is @c In.
 *  @return the inverse arrow. */
export constexpr dedekind::category::In<Numeral> inverse(
    dedekind::category::Prior<Numeral>) {
  return {};
}

// ---------------------------------------------------------------------------
// ℤ: Grothendieck's completion of (ℕ, +)
// ---------------------------------------------------------------------------

/** @section cardinality__Integer  ℤ, the formal differences of ℕ
 *  Grothendieck's completion of the commutative monoid @f$(\mathbb{N}, +)@f$:
 *  the pairs @f$a - b@f$ modulo @f$a - b = c - d \iff a + d = c + b@f$.  Each
 *  class has one normal form, @f$n - 0@f$ or @f$0 - n@f$, so an @c Integer is
 *  stored as its normal form: a magnitude, a @c Numeral, and a sign, zero
 *  never negative.  Every constructor and operation lands on it, so @c == is
 *  memberwise.  The atoms are the words: a signed constant embeds exactly
 *  (@c INT_MIN included), an unsigned one through ℕ.  The arithmetic is
 *  ℕ's, the sign carried; opposite signs subtract the smaller magnitude from
 *  the larger, inside this normalisation and nowhere else: ℕ gains no
 *  subtraction.  No value is infinite: an @c Integer is arbitrarily large but
 *  bounded, as a @c Numeral is (#1035). */
export struct Integer {
  /** @brief ℤ is countable. */
  using cardinality_type = ℵ_0;
  Numeral magnitude{};  ///< @f$|z|@f$, the rank of the normal form
  bool negative{};      ///< the sign, false at zero

  constexpr Integer() = default;
  /** @param s an atom: a signed word, embedded exactly. */
  template <std::signed_integral S>
  constexpr explicit Integer(S s)
      : magnitude(Numeral{s < S{} ? static_cast<std::make_unsigned_t<S>>(
                                        std::make_unsigned_t<S>{} -
                                        static_cast<std::make_unsigned_t<S>>(s))
                                  : static_cast<std::make_unsigned_t<S>>(s)}),
        negative(s < S{}) {
    static_assert(std::numeric_limits<S>::digits < 64,
                  "a signed atom's magnitude fits the inline limb");
  }
  /** @param n a natural: ℕ ↪ ℤ, @f$n \mapsto n - 0@f$. */
  constexpr explicit Integer(Numeral n) : magnitude(std::move(n)) {}
  /** @param m a magnitude.  @param minus the sign, dropped at zero: the
   *  normal form. */
  constexpr Integer(Numeral m, bool minus)
      : magnitude(std::move(m)), negative(minus && magnitude != Numeral{}) {}
  constexpr bool operator==(const Integer&) const = default;
};

/** @brief The additive inverse: the sign flipped, zero kept. */
export constexpr Integer operator-(const Integer& z) {
  return Integer(z.magnitude, !z.negative);
}
/** @brief The sum: magnitudes add under one sign; under two, the smaller is
 *  taken from the larger, which gives its sign. */
export constexpr Integer operator+(const Integer& x, const Integer& y) {
  if (x.negative == y.negative)
    return Integer(x.magnitude + y.magnitude, x.negative);
  if (y.magnitude <= x.magnitude)
    return Integer(Numeral(detail_numeral::subtract(x.magnitude.limbs(),
                                                    y.magnitude.limbs())),
                   x.negative);
  return Integer(Numeral(detail_numeral::subtract(y.magnitude.limbs(),
                                                  x.magnitude.limbs())),
                 y.negative);
}
/** @brief The difference: the sum with the inverse. */
export constexpr Integer operator-(const Integer& x, const Integer& y) {
  return x + -y;
}
/** @brief The product: magnitudes multiply, signs multiply. */
export constexpr Integer operator*(const Integer& x, const Integer& y) {
  return Integer(x.magnitude * y.magnitude, x.negative != y.negative);
}
/** @brief The order of ℤ: the negatives below, ordered by magnitude reversed;
 *  the naturals above, ordered as ℕ. */
export constexpr std::strong_ordering operator<=>(const Integer& x,
                                                  const Integer& y) {
  if (x.negative != y.negative) return y.negative <=> x.negative;
  return x.negative ? y.magnitude <=> x.magnitude : x.magnitude <=> y.magnitude;
}
/** @brief A signed constant meeting ℤ, the hook @c :terms' @c carrier_cast
 *  reaches by ADL: exact.  @tparam K the constant's type.  @param k it. */
export template <std::signed_integral K>
constexpr Integer carrier_cast(std::type_identity<Integer>, K k) {
  return Integer{k};
}
/** @brief An unsigned constant meeting ℤ: through ℕ.  @tparam K the
 *  constant's type.  @param k it. */
export template <std::unsigned_integral K>
constexpr Integer carrier_cast(std::type_identity<Integer>, K k) {
  return Integer{Numeral{k}};
}

/** @section cardinality__Rank  The rank of a normal form
 *  A @c Numeral measuring a value: on ℕ the fold out of the initial algebra
 *  into itself, the identity (Lambek), the size of Dedekind's chain below
 *  @c n, the well-founded measure Peano's induction descends; on ℤ the rank
 *  of the normal form, @f$|z|@f$.  An @c ℵ_0 carrier's @c cardinality_type
 *  reads as the least upper bound of the ranks, never attained. */
/** @param n a natural.  @return @c n. */
export constexpr Numeral rank(const Numeral& n) { return n; }
/** @param z an integer.  @return @f$|z|@f$. */
export constexpr Numeral rank(const Integer& z) { return z.magnitude; }
/** @concept IsRanked
 *  @brief Each value has a rank, a @c Numeral.  @tparam T the carrier. */
export template <typename T>
concept IsRanked = requires(const T& x) {
  { rank(x) } -> std::same_as<Numeral>;
};

// ---------------------------------------------------------------------------
// Extensional cardinal carrier
// ---------------------------------------------------------------------------

/**
 * @brief Finite extensional cardinal: one machine word @c W.
 *
 * @details The bare carrier is the wrapping ring ℤ/2^w (registered periodic
 * and cyclic below); ℕ itself is @c Numeral, exact above the word.
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
 *  @c algebra::Inverse<T, @c +>, #959) consume.  It covers ℤ's @c Integer
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

/**
 * @brief Signed one-word integer: two's complement in the signed word @c S.
 *
 * @details One structural field, @ref value, of type @c S.  Arithmetic runs
 * through the overflow builtins on that type, which yield the wrapped result,
 * so the carrier is exactly the residue ring ℤ/2^w read in the signed range
 * [-2^(w-1), 2^(w-1)-1]: a genuine commutative ring and cyclic group, which the
 * registrations below claim.  @c -min wraps to @c min, and @c min / @c -1 is @c
 * min (the ring answer), so no operation is undefined.
 *
 * It satisfies IsInteger (see registrations after the numbers:integer
 * module pulls this file), making it the signed backing carrier for
 * fixed-width `Rational<Z>`.  The `SignedCardinality` variant downstream
 * escalates to ±ℵ_0 where this carrier wraps.
 *
 * Structural on purpose: the public field makes the type usable as a
 * non-type template parameter wherever `Rational<Z>` is.
 *
 * @tparam S The signed word (default @c std::ptrdiff_t, the signed twin of
 *           @c std::size_t).  Gated on being a signed built-in integer whose
 *           unsigned twin is an @c IsCountWord: the raw signed type is not a
 *           ring (overflow is UB), but this carrier's builtin-based arithmetic
 *           is exactly the ring of its unsigned twin.
 */
export template <typename S = std::ptrdiff_t>
  requires std::signed_integral<S> && IsCountWord<std::make_unsigned_t<S>>
struct SignedExtensionalCardinal {
  using signed_type = S;
  using word_type = std::make_unsigned_t<S>;
  using magnitude_type = ExtensionalCardinal<word_type>;

  /** @brief A wrapped result and whether the exact result overflowed. */
  struct CheckedResult {
    SignedExtensionalCardinal result;
    bool overflowed;
  };

  signed_type value{};

  constexpr SignedExtensionalCardinal() noexcept = default;

  /** @brief Construction from any integral source, modulo 2^w. */
  template <std::integral I>
  constexpr SignedExtensionalCardinal(I v) noexcept  // NOLINT
      : value(static_cast<signed_type>(static_cast<word_type>(v))) {}

  /** @brief Is the value negative? */
  constexpr bool negative() const noexcept { return value < 0; }

  /** @brief |value| as an unsigned word (exact, also for @c min). */
  constexpr magnitude_type magnitude() const noexcept {
    const word_type bits = static_cast<word_type>(value);
    return magnitude_type{
        negative() ? static_cast<word_type>(word_type{0} - bits) : bits};
  }

  /** @brief Explicit projection to @c std::size_t — the magnitude (sign is
   *         dropped).  Sibling to @c ExtensionalCardinal::operator
   *         @c std::size_t() ; used at the same call sites in
   *         @c :sequences:path 's helpers. */
  explicit constexpr operator std::size_t() const noexcept {
    return static_cast<std::size_t>(magnitude());
  }

  constexpr friend bool operator==(const SignedExtensionalCardinal&,
                                   const SignedExtensionalCardinal&) = default;

  constexpr friend std::strong_ordering operator<=>(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    return lhs.value <=> rhs.value;
  }

  /** @brief Heterogeneous comparison with built-in integrals, exact across
   *  signedness (compared in 128 bits).  Without it, @c SEC @c == @c int is
   *  ambiguous between the @c SEC and @c SignedCardinality overloads, each
   *  one user-defined conversion away. */
  template <std::integral T>
  constexpr friend bool operator==(const SignedExtensionalCardinal& lhs,
                                   T rhs) noexcept {
    return static_cast<__int128>(lhs.value) == static_cast<__int128>(rhs);
  }
  template <std::integral T>
  constexpr friend bool operator==(
      T lhs, const SignedExtensionalCardinal& rhs) noexcept {
    return static_cast<__int128>(lhs) == static_cast<__int128>(rhs.value);
  }
  template <std::integral T>
  constexpr friend std::strong_ordering operator<=>(
      const SignedExtensionalCardinal& lhs, T rhs) noexcept {
    return static_cast<__int128>(lhs.value) <=> static_cast<__int128>(rhs);
  }
  template <std::integral T>
  constexpr friend std::strong_ordering operator<=>(
      T lhs, const SignedExtensionalCardinal& rhs) noexcept {
    return static_cast<__int128>(lhs) <=> static_cast<__int128>(rhs.value);
  }

  static constexpr CheckedResult checked_add(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    SignedExtensionalCardinal sum;
    const bool overflowed =
        __builtin_add_overflow(lhs.value, rhs.value, &sum.value);
    return CheckedResult{sum, overflowed};
  }

  static constexpr CheckedResult checked_sub(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    SignedExtensionalCardinal difference;
    const bool overflowed =
        __builtin_sub_overflow(lhs.value, rhs.value, &difference.value);
    return CheckedResult{difference, overflowed};
  }

  static constexpr CheckedResult checked_mul(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    SignedExtensionalCardinal product;
    const bool overflowed =
        __builtin_mul_overflow(lhs.value, rhs.value, &product.value);
    return CheckedResult{product, overflowed};
  }

  /** @brief The additive inverse in ℤ/2^w (@c -min wraps to @c min). */
  constexpr SignedExtensionalCardinal operator-() const noexcept {
    SignedExtensionalCardinal result;
    (void)__builtin_sub_overflow(signed_type{0}, value, &result.value);
    return result;
  }

  constexpr friend SignedExtensionalCardinal operator+(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    return checked_add(lhs, rhs).result;
  }

  constexpr friend SignedExtensionalCardinal operator-(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    return lhs + (-rhs);
  }

  constexpr friend SignedExtensionalCardinal operator*(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    return checked_mul(lhs, rhs).result;
  }

  /** @brief Division truncating toward zero (C++ semantics).  Division by
   *  zero yields zero by convention; @c min / @c -1 wraps to @c min. */
  constexpr friend SignedExtensionalCardinal operator/(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    if (rhs.value == 0) return SignedExtensionalCardinal{};
    if (rhs.value == -1) return -lhs;
    SignedExtensionalCardinal result;
    result.value = static_cast<signed_type>(lhs.value / rhs.value);
    return result;
  }

  /** @brief Remainder with the sign of the dividend (C++ semantics:
   *  `a == (a / b) * b + a % b`).  Modulo zero yields @c lhs. */
  constexpr friend SignedExtensionalCardinal operator%(
      const SignedExtensionalCardinal& lhs,
      const SignedExtensionalCardinal& rhs) noexcept {
    if (rhs.value == 0) return lhs;
    if (rhs.value == -1) return SignedExtensionalCardinal{};
    SignedExtensionalCardinal result;
    result.value = static_cast<signed_type>(lhs.value % rhs.value);
    return result;
  }

  /** @brief Explicit conversion to a signed integral type (modular, like the
   *  carrier).  Used by the IR-fixture showcases to extract a
   *  compile-time-known rational's numerator as a concrete machine integer
   *  for the emitted `ret i64 ...` witness. */
  template <std::signed_integral Target>
  constexpr explicit operator Target() const noexcept {
    return static_cast<Target>(value);
  }
};

// ---------------------------------------------------------------------------
// SignedCardinality: extended-integer carrier with ±ℵ_0 escalation (#377)
// ---------------------------------------------------------------------------
//
// `SignedExtensionalCardinal<S>` is a *finite* signed carrier --- ℤ/2^w ℤ
// in two's complement.  Useful for
// showcase arithmetic, but not ℤ.  `SignedCardinality` is an extended-
// integer carrier whose addition / subtraction / multiplication
// escalate to ±ℵ_0 (or `NaZ` for indeterminate forms) rather than wrap.
//
// Design choices:
//   - Mixed-infinity arithmetic (e.g.\ +ℵ_0 + (−ℵ_0)) returns the
//     `NaZ` sentinel rather than throwing, mirroring IEEE NaN
//     propagation: NaZ + anything = NaZ, NaZ * anything = NaZ.  Keeps
//     the type usable in constexpr / NTTP-like contexts that cannot
//     observe exceptions.
//   - The variant alternative ordering (finite first) makes
//     `SignedCardinality{}` the canonical zero, satisfying the
//     `IsInteger` `T{}` requirement.
//   - The total `+` / `*` are saturating, not periodic --- the
//     `is_saturating_v` species trait (introduced under this issue)
//     is the load-bearing certificate that lifts `IsTotal` and the
//     downstream `IsAbelianGroup<SignedCardinality, std::plus<...>>`.

/** @struct PositiveInfinity: the @f$+\aleph_0@f$ sentinel for
 * SignedCardinality. */
export struct PositiveInfinity {
  constexpr friend bool operator==(const PositiveInfinity&,
                                   const PositiveInfinity&) = default;
};

/** @struct NegativeInfinity: the @f$-\aleph_0@f$ sentinel. */
export struct NegativeInfinity {
  constexpr friend bool operator==(const NegativeInfinity&,
                                   const NegativeInfinity&) = default;
};

/** @struct NaZ: Not-a-ℤ-element.  Result of indeterminate forms
 *  (+ℵ_0 + (−ℵ_0), 0 * ±ℵ_0, ±ℵ_0 / ±ℵ_0, etc.).  Propagates through
 *  arithmetic in the IEEE-NaN style.
 */
export struct NaZ {
  constexpr friend bool operator==(const NaZ&, const NaZ&) = default;
};

/** @brief The ℤ proxy with ±ℵ_0 escalation;
 *         the library's bona-fide proxy for @f$\mathbb{Z}@f$ modulo
 *         physical limits.
 *
 *  @details Variant of @c (SignedExtensionalCardinal<>, +ℵ_0, −ℵ_0, NaZ).
 *  The finite alternative is the default-constructed first slot, so
 *  @c SignedCardinality{} is canonical zero.
 *
 *  The picture:
 *    - @b Not @b cyclic --- arithmetic does not wrap modulo capacity.
 *    - On signed overflow, @b escalates (saturates) into @c ±ℵ_0
 *      according to the sign of the operands.
 *    - Indeterminate forms (@c +ℵ_0 + (−ℵ_0), @c 0 * ±ℵ_0,
 *      @c ±ℵ_0 / ±ℵ_0, @c x / 0) propagate the @c NaZ sentinel
 *      IEEE-NaN-style rather than throwing, keeping the carrier
 *      usable in @c constexpr / NTTP-shaped contexts that cannot
 *      observe exceptions.
 *    - The finite fragment @c SignedExtensionalCardinal<> is one
 *      two's-complement word (so the variant is @b not a bignum).  The explicit
 * overflow story is escalation to @f$\pm \aleph_0@f$, not bigint growth:
 * signed-overflow UB and silent wrap are replaced by the saturation behaviour.
 *
 *  Strictly mathematically, @f$\mathbb{Z} \cup \{\pm \aleph_0,
 *  \mathit{NaZ}\}@f$ is not a group in the textbook sense
 *  (@c +ℵ_0 + (−ℵ_0) = NaZ breaks the inverse law for the
 *  saturating elements).  The library's pragmatic certificate
 *  accepts it as a proxy for @f$\mathbb{Z}@f$ because the finite
 *  fragment is @f$\mathbb{Z}@f$ exactly and the saturation /
 *  NaZ behaviour is the @b explicit overflow contract: "if you
 *  compute beyond memory limits, you saturate at @c ±ℵ_0; if you
 *  compute an indeterminate form, you get @c NaZ".  Downstream code
 *  that requires strict-ℤ behaviour should test the finite-fragment
 *  alternative (@c std::holds_alternative<SignedExtensionalCardinal<>>).
 *
 *  Same shape as how @c unsigned int's @c IsRing claim is "modulo
 *  wrap" rather than literal @f$\mathbb{Z}@f$ --- the project's
 *  stance is that the truthful name for the carrier includes the
 *  overflow / saturation behaviour, and the trait registry pins
 *  what the carrier actually does at the limits.
 */
export using SignedCardinality =
    std::variant<SignedExtensionalCardinal<>, PositiveInfinity,
                 NegativeInfinity, NaZ>;

// noexcept justification for SignedCardinality's operators (pinned per
// Copilot review on PR #425 to make the std::get-can't-throw guarantee
// compile-time explicit).
// All four variant alternatives are nothrow-move-constructible
// (SignedExtensionalCardinal<> wraps an integral magnitude + bool;
// PositiveInfinity / NegativeInfinity / NaZ are empty sentinel
// structs), so SignedCardinality is provably never valueless and the
// noexcept on its operators (in use since PR #396) is sound.
static_assert(std::is_nothrow_move_constructible_v<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> must be nothrow-move-"
              "constructible so SignedCardinality is provably never "
              "valueless_by_exception.");
static_assert(std::is_nothrow_move_constructible_v<PositiveInfinity> &&
                  std::is_nothrow_move_constructible_v<NegativeInfinity> &&
                  std::is_nothrow_move_constructible_v<NaZ>,
              "All SignedCardinality sentinel alternatives (±ℵ_0, NaZ) "
              "must be nothrow-move-constructible (empty structs --- "
              "trivially true; pinned for the noexcept guarantee).");

namespace detail {
constexpr bool sc_is_finite(const SignedCardinality& v) noexcept {
  return std::holds_alternative<SignedExtensionalCardinal<>>(v);
}
constexpr bool sc_is_pos_inf(const SignedCardinality& v) noexcept {
  return std::holds_alternative<PositiveInfinity>(v);
}
constexpr bool sc_is_neg_inf(const SignedCardinality& v) noexcept {
  return std::holds_alternative<NegativeInfinity>(v);
}
constexpr bool sc_is_naz(const SignedCardinality& v) noexcept {
  return std::holds_alternative<NaZ>(v);
}
constexpr bool sc_is_zero(const SignedCardinality& v) noexcept {
  return sc_is_finite(v) && std::get<SignedExtensionalCardinal<>>(v) ==
                                SignedExtensionalCardinal<>{};
}
constexpr int sc_sign(const SignedCardinality& v) noexcept {
  // Returns +1 for positive (incl. +ℵ_0), -1 for negative (incl. -ℵ_0),
  // 0 for zero or NaZ.  Used by escalation logic.
  if (sc_is_naz(v)) return 0;
  if (sc_is_pos_inf(v)) return +1;
  if (sc_is_neg_inf(v)) return -1;
  const auto& z = std::get<SignedExtensionalCardinal<>>(v);
  if (z == SignedExtensionalCardinal<>{}) return 0;
  return z.negative() ? -1 : +1;
}
}  // namespace detail

/** @brief Is this signed cardinal a FINITE ℤ value (not @c ±ℵ_0 / @c NaZ)?  The
 *  public query behind Honest-Rejection guards at the carrier boundary --- e.g.
 *  @c Rational::simplify skips @c euclidean_gcd (which would not terminate on a
 *  @c NaZ, since @c NaZ @c % @c · @c == @c NaZ) when a numerator/denominator is
 *  a non-finite sentinel (#680). */
export constexpr bool is_finite(const SignedCardinality& v) noexcept {
  return detail::sc_is_finite(v);
}

/** @brief Convenience constructor for finite signed values. */
export template <std::integral S>
constexpr SignedCardinality finite_signed_cardinality(S value) noexcept {
  return SignedCardinality{SignedExtensionalCardinal<>{value}};
}

/** @brief An integer constant meeting ℤ, the hook the term grammar's
 *  @c carrier_cast reaches by ADL.  @tparam K the constant's type.
 *  @param k the constant. */
export template <std::integral K>
constexpr SignedCardinality carrier_cast(std::type_identity<SignedCardinality>,
                                         K k) {
  return finite_signed_cardinality(k);
}

/** @brief Unary negation: swaps signs and ±ℵ_0; preserves NaZ. */
export constexpr SignedCardinality operator-(
    const SignedCardinality& v) noexcept {
  if (detail::sc_is_naz(v)) return SignedCardinality{NaZ{}};
  if (detail::sc_is_pos_inf(v)) return SignedCardinality{NegativeInfinity{}};
  if (detail::sc_is_neg_inf(v)) return SignedCardinality{PositiveInfinity{}};
  const auto& z = std::get<SignedExtensionalCardinal<>>(v);
  // -min is not representable: it is past the word, so it escalates.
  if (z.value == std::numeric_limits<decltype(z.value)>::min())
    return SignedCardinality{PositiveInfinity{}};
  return SignedCardinality{-z};
}

/** @brief Addition with ±ℵ_0 escalation on overflow. */
export constexpr SignedCardinality operator+(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  // Mixed infinities: indeterminate.
  if (sc_is_pos_inf(lhs) && sc_is_neg_inf(rhs)) return SignedCardinality{NaZ{}};
  if (sc_is_neg_inf(lhs) && sc_is_pos_inf(rhs)) return SignedCardinality{NaZ{}};
  // Same-sign infinities: absorb.
  if (sc_is_pos_inf(lhs) || sc_is_pos_inf(rhs))
    return SignedCardinality{PositiveInfinity{}};
  if (sc_is_neg_inf(lhs) || sc_is_neg_inf(rhs))
    return SignedCardinality{NegativeInfinity{}};
  // Both finite: overflow is only possible for same-sign operands, and its
  // direction is their sign.
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  const auto checked = SignedExtensionalCardinal<>::checked_add(a, b);
  if (checked.overflowed) {
    return a.negative() ? SignedCardinality{NegativeInfinity{}}
                        : SignedCardinality{PositiveInfinity{}};
  }
  return SignedCardinality{checked.result};
}

/** @brief Subtraction, computed directly so that only an overflowing
 *  difference saturates (@c a + (−b) would saturate @c −min first, so
 *  @c min − min would not be 0).  Infinities as for @c a + (−b). */
export constexpr SignedCardinality operator-(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  if ((sc_is_pos_inf(lhs) && sc_is_pos_inf(rhs)) ||
      (sc_is_neg_inf(lhs) && sc_is_neg_inf(rhs)))
    return SignedCardinality{NaZ{}};
  if (sc_is_pos_inf(lhs) || sc_is_neg_inf(rhs))
    return SignedCardinality{PositiveInfinity{}};
  if (sc_is_neg_inf(lhs) || sc_is_pos_inf(rhs))
    return SignedCardinality{NegativeInfinity{}};
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  const auto checked = SignedExtensionalCardinal<>::checked_sub(a, b);
  if (checked.overflowed) {
    return a.negative() ? SignedCardinality{NegativeInfinity{}}
                        : SignedCardinality{PositiveInfinity{}};
  }
  return SignedCardinality{checked.result};
}
static_assert(
    finite_signed_cardinality(
        std::numeric_limits<SignedExtensionalCardinal<>::signed_type>::min()) -
            finite_signed_cardinality(
                std::numeric_limits<
                    SignedExtensionalCardinal<>::signed_type>::min()) ==
        finite_signed_cardinality(0),
    "min − min is 0: only an overflowing difference saturates.");

/** @brief The step up on the ℤ proxy, the free customization point the order
 *  layer's point collapse calls (@c category:nno, found here by ADL):
 *  @c +1 through the saturating @c +, so @c +ℵ_0 is a fixpoint
 *  and @c NaZ propagates.
 *  @param n the value to step from.
 *  @return @c n @c + @c 1, saturating. */
export constexpr SignedCardinality successor(
    const SignedCardinality& n) noexcept {
  return n + finite_signed_cardinality(1);
}
/** @brief The step down on the ℤ proxy: @c −1 through the saturating @c -, so
 *  @c −ℵ_0 is a fixpoint and @c NaZ propagates.  Unlike ℕ there is no monus:
 *  ℤ steps down past zero.
 *  @param n the value to step from.
 *  @return @c n @c − @c 1, saturating. */
export constexpr SignedCardinality predecessor(
    const SignedCardinality& n) noexcept {
  return n - finite_signed_cardinality(1);
}

/** @brief Multiplication with ±ℵ_0 escalation on overflow.  @c 0 * ±ℵ_0
 *         is indeterminate (returns @c NaZ).
 */
export constexpr SignedCardinality operator*(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  const bool lhs_inf = sc_is_pos_inf(lhs) || sc_is_neg_inf(lhs);
  const bool rhs_inf = sc_is_pos_inf(rhs) || sc_is_neg_inf(rhs);
  if (lhs_inf || rhs_inf) {
    if (sc_is_zero(lhs) || sc_is_zero(rhs))
      return SignedCardinality{NaZ{}};  // 0 * ±ℵ_0
    const int s = sc_sign(lhs) * sc_sign(rhs);
    return s > 0 ? SignedCardinality{PositiveInfinity{}}
                 : SignedCardinality{NegativeInfinity{}};
  }
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  const auto checked = SignedExtensionalCardinal<>::checked_mul(a, b);
  if (checked.overflowed) {
    return (a.negative() != b.negative())
               ? SignedCardinality{NegativeInfinity{}}
               : SignedCardinality{PositiveInfinity{}};
  }
  return SignedCardinality{checked.result};
}

/** @brief Truncating division.  Division by zero and @c ±ℵ_0/±ℵ_0
 *         return @c NaZ; @c finite/±ℵ_0 returns @c 0 (the limit);
 *         @c ±ℵ_0 divided by a non-zero finite value saturates to
 *         signed infinity according to the operand signs.
 */
export constexpr SignedCardinality operator/(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  if (sc_is_zero(rhs)) return SignedCardinality{NaZ{}};  // x / 0
  const bool lhs_inf = sc_is_pos_inf(lhs) || sc_is_neg_inf(lhs);
  const bool rhs_inf = sc_is_pos_inf(rhs) || sc_is_neg_inf(rhs);
  if (lhs_inf && rhs_inf) return SignedCardinality{NaZ{}};
  if (lhs_inf) {
    const int s = sc_sign(lhs) * sc_sign(rhs);
    return s > 0 ? SignedCardinality{PositiveInfinity{}}
                 : SignedCardinality{NegativeInfinity{}};
  }
  if (rhs_inf) return finite_signed_cardinality(0);  // finite / ±ℵ_0 → 0
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  // min / -1 = 2^(w-1) is past the word: it escalates rather than wraps.
  if (a.value == std::numeric_limits<decltype(a.value)>::min() && b.value == -1)
    return SignedCardinality{PositiveInfinity{}};
  return SignedCardinality{a / b};
}

/** @brief Truncating remainder.  Mirrors C++ semantics on the finite
 *         alternative; non-finite operands give @c NaZ.
 */
export constexpr SignedCardinality operator%(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return SignedCardinality{NaZ{}};
  if (!sc_is_finite(lhs) || !sc_is_finite(rhs)) return SignedCardinality{NaZ{}};
  if (sc_is_zero(rhs)) return SignedCardinality{NaZ{}};
  const auto& a = std::get<SignedExtensionalCardinal<>>(lhs);
  const auto& b = std::get<SignedExtensionalCardinal<>>(rhs);
  return SignedCardinality{a % b};
}

/** @brief Partial ordering on the extended integers: −ℵ_0 < finite <
 *         +ℵ_0; @c NaZ is @b unordered with respect to everything
 *         (including itself), mirroring IEEE-NaN's unordered
 *         semantics.  Returning @c std::partial_ordering rather than
 *         @c std::strong_ordering is the principled choice: @c NaZ
 *         compares @c unordered, which keeps ordering and equality
 *         (variant's defaulted @c ==, which finds @c NaZ == @c NaZ
 *         true) consistent without forcing a fake @c equal answer.
 */
export constexpr std::partial_ordering compare_signed(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs) || sc_is_naz(rhs)) return std::partial_ordering::unordered;
  // -ℵ_0 < everything except itself.
  if (sc_is_neg_inf(lhs) && sc_is_neg_inf(rhs))
    return std::partial_ordering::equivalent;
  if (sc_is_neg_inf(lhs)) return std::partial_ordering::less;
  if (sc_is_neg_inf(rhs)) return std::partial_ordering::greater;
  // +ℵ_0 > everything except itself.
  if (sc_is_pos_inf(lhs) && sc_is_pos_inf(rhs))
    return std::partial_ordering::equivalent;
  if (sc_is_pos_inf(lhs)) return std::partial_ordering::greater;
  if (sc_is_pos_inf(rhs)) return std::partial_ordering::less;
  return std::get<SignedExtensionalCardinal<>>(lhs) <=>
         std::get<SignedExtensionalCardinal<>>(rhs);
}

/** @brief @c < derived from @c compare_signed; required by @c IsInteger.
 *         For @c NaZ operands the comparison is @c unordered so this
 *         returns @c false, mirroring IEEE-NaN's @c < behaviour.
 */
export constexpr bool operator<(const SignedCardinality& lhs,
                                const SignedCardinality& rhs) noexcept {
  return compare_signed(lhs, rhs) == std::partial_ordering::less;
}

/** @brief Explicit homogeneous @c == on @c SignedCardinality (#669).
 *
 * @details
 * The @c std::variant's
 * defaulted @c == lives inside the module's purview and is @b not
 * reachable across the @c dedekind.sets module boundary (the
 * @c \<variant\> header is in the global module fragment).  Defining
 * the operator explicitly in @c dedekind::sets --- where ADL can find
 * it from importers like @c dedekind.numbers and
 * @c dedekind.algebra:ordered_algebra --- closes the gap so
 * @c std::equality_comparable<SignedCardinality>,
 * @c std::regular<SignedCardinality>, and
 * @c std::three_way_comparable<SignedCardinality,
 * @c std::partial_ordering> fire downstream.
 *
 * @c NaZ-vs-NaZ returns @c true: this is the variant-alternative
 * equality reading, required for @c == to be an equivalence relation
 * (reflexivity).  The companion @c <=> below @b mirrors this --- the
 * NaZ-vs-NaZ case is special-cased to return
 * @c std::partial_ordering::equivalent so that @c == and @c <=>
 * @b agree on this pair, per C++20's @c std::three_way_comparable
 * contract @c (a @c == @c b) ↔ @c std::is_eq(a @c <=> @c b).  @c NaZ
 * vs @b non-@c NaZ remains @c unordered in @c <=> (and @c false in
 * @c ==), preserving IEEE-NaN-style unordered semantics for mixed
 * comparisons.
 *
 * Closes #669, which itself closes the persistent local-only build
 * failure on @c sint.cppm:228
 * (@c embed_sint_ℤ(η(42)).pivot @c == @c
 * finite_signed_cardinality(42)).
 */
export constexpr bool operator==(const SignedCardinality& lhs,
                                 const SignedCardinality& rhs) noexcept {
  // Both NaZ: equal as variant alternatives (reflexive ==).
  if (detail::sc_is_naz(lhs) && detail::sc_is_naz(rhs)) return true;
  if (detail::sc_is_naz(lhs) || detail::sc_is_naz(rhs)) return false;
  // Either ±ℵ_0: alternative-tag equality.
  if (detail::sc_is_pos_inf(lhs)) return detail::sc_is_pos_inf(rhs);
  if (detail::sc_is_neg_inf(lhs)) return detail::sc_is_neg_inf(rhs);
  if (detail::sc_is_pos_inf(rhs) || detail::sc_is_neg_inf(rhs)) return false;
  // Finite-vs-finite: delegate to SignedExtensionalCardinal<>'s
  // canonical-zero-aware ==.
  return std::get<SignedExtensionalCardinal<>>(lhs) ==
         std::get<SignedExtensionalCardinal<>>(rhs);
}

/** @brief Explicit homogeneous @c <=> on @c SignedCardinality (#669).
 *
 * @details
 * Returns @c std::partial_ordering because @c NaZ is unordered with
 * @b non-@c NaZ operands (IEEE-NaN-style).  Special-case: @c NaZ
 * @c <=> @c NaZ returns @c std::partial_ordering::equivalent so that
 * the @c == above (which returns @c true on NaZ-vs-NaZ for
 * @c std::equality_comparable reflexivity) and this @c <=> agree on
 * the equivalence case, per @c std::three_way_comparable's contract
 * @c (a @c == @c b) ↔ @c std::is_eq(a @c <=> @c b).  Mixed @c NaZ
 * vs non-@c NaZ comparisons remain @c unordered.
 *
 * Required for @c std::three_way_comparable<SignedCardinality,
 * @c std::partial_ordering> to fire downstream.  Pairs with the @c ==
 * above to close the full standard-concept surface for the saturating
 * ℤ proxy.
 */
export constexpr std::partial_ordering operator<=>(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  // Special-case NaZ-vs-NaZ to agree with the homogeneous == above
  // (which returns true on this pair for std::equality_comparable
  // reflexivity).  compare_signed returns unordered for any
  // NaZ-involving pair, including both-NaZ; we override that case here
  // so the C++20 three_way_comparable contract holds:
  //   (a == b) ↔ std::is_eq(a <=> b).
  if (detail::sc_is_naz(lhs) && detail::sc_is_naz(rhs)) {
    return std::partial_ordering::equivalent;
  }
  return compare_signed(lhs, rhs);
}

/** @brief The structural order of signed cardinalities as constants of a
 *  term (#884, @c canonical's sort key): total, unlike @c <=>.  By
 *  alternative first (finite, +∞, −∞, then @c NaZ), a finite value by its
 *  order.  @param lhs, rhs the constants. */
export constexpr std::weak_ordering constant_order(
    const SignedCardinality& lhs, const SignedCardinality& rhs) noexcept {
  if (lhs.index() != rhs.index()) return lhs.index() <=> rhs.index();
  const std::partial_ordering c = lhs <=> rhs;
  return c < 0   ? std::weak_ordering::less
         : c > 0 ? std::weak_ordering::greater
                 : std::weak_ordering::equivalent;
}

// ---------------------------------------------------------------------------
// Heterogeneous comparison: variant ↔ std::integral (closes #415)
// ---------------------------------------------------------------------------
//
// Cross-type relational operators between the variant ℤ-proxy carrier
// @c SignedCardinality and built-in @c std::integral values.  The load-bearing
// path is the halfspace machinery's
// @c (x @c > @c Pivot) substitution where @c x is a variant ℤ-proxy and
// @c Pivot is the @c int NTTP from @c -21: previously this
// required an explicit lift of the literal to the variant alternative.
//
// Defining @c operator<=> together with @c operator== lets the C++20
// rewrite rules synthesise the four partial-order operators (@c <,
// @c <=, @c >, @c >=) and the symmetric (rhs-first) direction
// uniformly --- the witness section in @c numbers/cardinality.cppm
// pins both directions via @c HasPartialOrderOperatorsWith.

/** @brief @c SignedCardinality @c <=> @c std::integral.  Lifts @c rhs
 *         to a finite @c SignedCardinality and routes through
 *         @c compare_signed; @c NaZ on @c lhs propagates as
 *         @c std::partial_ordering::unordered, mirroring IEEE-NaN's
 *         unordered semantics. */
export template <std::integral T>
constexpr std::partial_ordering operator<=>(const SignedCardinality& lhs,
                                            T rhs) noexcept {
  return compare_signed(lhs, finite_signed_cardinality(rhs));
}

/** @brief @c SignedCardinality @c == @c std::integral.  @c NaZ and
 *         @c ±ℵ_0 never equal a finite integer; the finite
 *         alternative delegates to @c SignedExtensionalCardinal<>'s
 *         canonical-zero-aware equality. */
export template <std::integral T>
constexpr bool operator==(const SignedCardinality& lhs, T rhs) noexcept {
  if (!detail::sc_is_finite(lhs)) return false;
  return std::get<SignedExtensionalCardinal<>>(lhs) ==
         SignedExtensionalCardinal<>{rhs};
}

// ---------------------------------------------------------------------------
// Heterogeneous comparison: variant ↔ std::floating_point (closes #428)
// ---------------------------------------------------------------------------
//
// Cross-type relational operators between the variant ℤ-proxy
// carrier and built-in @c std::floating_point values.  Mirrors the
// @c std::integral overloads above; needed for showcase callsites like
// @c (var<ℤ> @c > @c -21.0) where the literal is a @c double and
// the carrier is the variant.  Returns @c
// std::partial_ordering throughout — a NaN @c rhs (or @c NaZ on @c
// lhs) propagates as @c unordered, IEEE-NaN-style.
//
// Float extraction: @c SignedExtensionalCardinal<> doesn't carry an
//     @c operator @c F(); we lift via @c (negative @c ? @c -1 @c : @c 1) @c *
//     @c static_cast<F>(magnitude) on the one-word carrier.

namespace detail {
/** @brief @f$2^{d}@f$ with @c d the digits of @c LT, as an exact @c F: the
 *  doublings are exact in @c F since @f$2^k@f$ is representable until @c k
 *  overflows @c F's exponent range (well above any plausible @c LT::digits;
 *  for @c LT @c = @c uint64_t and @c F @c = @c double this is @f$2^{64}@f$).
 *  A fold of the doublings (Rule 3); @c std::ldexp would be cleaner but is
 *  not @c constexpr until C++26.  @tparam F the floating type.  @tparam LT
 *  the unsigned word. */
template <std::floating_point F, std::unsigned_integral LT>
inline constexpr F two_to_the_digits_v = std::ranges::fold_left(
    std::views::repeat(F{2}, std::numeric_limits<LT>::digits), F{1},
    std::multiplies<F>{});

/** @brief Whether @c x lies strictly below zero (@c -0.0 does not); and
 *  whether it is zero of either sign.  The operators on @c F live here, ahead
 *  of the overloads below, which read @c F through these and the standard
 *  classification functions only.  @tparam F the floating type. */
template <std::floating_point F>
constexpr bool is_negative(F x) noexcept {
  return x < F{0};
}
template <std::floating_point F>
constexpr bool is_zero(F x) noexcept {
  return x == F{0};
}

/** @brief Compare a non-negative integer @c lhs with a finite
 *         floating-point value @c rhs in @b integer @b domain — the
 *         comparison is precision-correct even when @c lhs exceeds @c
 *         2^digits<F> (where the cast @c LT @c → @c F is no longer
 *         exact and consecutive integers collapse to the same float).
 *
 *  Pre: @c rhs is finite, @b not NaN.  Sign of @c rhs is handled.
 *
 *  Strategy:
 *    * @c rhs negative @c → @c lhs (≥ 0) is strictly greater.
 *    * @c rhs out of @c [0, 2^digits<LT>) range @c → @c lhs ≤ LT_max
 *      < rhs, so @c lhs is strictly less.
 *    * @c rhs in range @c → split @c rhs into integer part
 *      @c trunc(rhs) and a fractional indicator; compare @c lhs to the
 *      integer part in @c LT domain (exact); if rhs has no fractional
 *      part, @c equivalent at integer-equality, else @c lhs sits below
 *      @c rhs at integer-equality (since @c rhs > floor in that case).
 */
template <std::floating_point F, std::unsigned_integral LT>
constexpr std::partial_ordering compare_unsigned_to_floating(LT lhs,
                                                             F rhs) noexcept {
  if (rhs < F{0}) return std::partial_ordering::greater;
  if (rhs >= two_to_the_digits_v<F, LT>) return std::partial_ordering::less;
  const F rhs_trunc = std::trunc(rhs);
  const bool rhs_is_integer = (rhs == rhs_trunc);
  const LT rhs_floor = static_cast<LT>(rhs_trunc);
  if (lhs < rhs_floor) return std::partial_ordering::less;
  if (lhs > rhs_floor) return std::partial_ordering::greater;
  return rhs_is_integer ? std::partial_ordering::equivalent
                        : std::partial_ordering::less;
}

/** @brief Equality counterpart of @c compare_unsigned_to_floating:
 *         requires @c rhs to be finite, non-negative, integer-valued,
 *         in @c [0, 2^digits<LT>) range, and equal to @c lhs in @c LT
 *         domain.  Anything else is @c false. */
template <std::floating_point F, std::unsigned_integral LT>
constexpr bool eq_unsigned_to_floating(LT lhs, F rhs) noexcept {
  if (!std::isfinite(rhs)) return false;
  if (rhs < F{0}) return false;
  const F rhs_trunc = std::trunc(rhs);
  if (rhs != rhs_trunc) return false;
  if (rhs >= two_to_the_digits_v<F, LT>) return false;
  return lhs == static_cast<LT>(rhs_trunc);
}
}  // namespace detail

/** @brief @c SignedCardinality @c <=> @c std::floating_point.
 *         @c NaZ propagates as @c unordered (mirrors compare_signed's
 *         NaZ semantics); @c ±ℵ_0 maps to @c ±inf-equivalent;
 *         @c NaN @c rhs returns @c unordered.  Finite-vs-finite is
 *         settled by sign-aware delegation to the unsigned integer-
 *         domain helper, so the comparison stays precision-correct
 *         even when the underlying magnitude exceeds @c 2^digits<F>. */
export template <std::floating_point F>
constexpr std::partial_ordering operator<=>(const SignedCardinality& lhs,
                                            F rhs) noexcept {
  using namespace detail;
  // rhs is read by the standard classification functions and the detail
  // helpers, not by the operators: an operator on F here would be resolved
  // against this file's own overload set, which F's constraint alone does not
  // decide (Rule 2).
  if (sc_is_naz(lhs)) return std::partial_ordering::unordered;
  if (std::isnan(rhs)) return std::partial_ordering::unordered;
  const bool rhs_is_pos_inf = std::isinf(rhs) && !std::signbit(rhs);
  const bool rhs_is_neg_inf = std::isinf(rhs) && std::signbit(rhs);
  if (sc_is_pos_inf(lhs)) {
    return rhs_is_pos_inf ? std::partial_ordering::equivalent
                          : std::partial_ordering::greater;
  }
  if (sc_is_neg_inf(lhs)) {
    return rhs_is_neg_inf ? std::partial_ordering::equivalent
                          : std::partial_ordering::less;
  }
  if (rhs_is_pos_inf) return std::partial_ordering::less;
  if (rhs_is_neg_inf) return std::partial_ordering::greater;
  // Both finite.  Sign first, then magnitude comparison via the unsigned
  // helper (with order reversed for negative-vs-negative).
  const auto& z = std::get<SignedExtensionalCardinal<>>(lhs);
  using LT = ExtensionalCardinal<>::word_type;
  const LT magnitude_value = z.magnitude().value;
  const bool z_is_zero = (magnitude_value == 0);
  const bool z_is_neg = z.negative() && !z_is_zero;
  const bool rhs_is_neg = detail::is_negative(rhs);
  if (z_is_neg && !rhs_is_neg) return std::partial_ordering::less;
  if (!z_is_neg && rhs_is_neg) return std::partial_ordering::greater;
  if (!z_is_neg) {
    return compare_unsigned_to_floating<F, LT>(magnitude_value, rhs);
  }
  // Both negative; compare |lhs| with |rhs|, then reverse the order.
  const auto magnitude_cmp =
      compare_unsigned_to_floating<F, LT>(magnitude_value, -rhs);
  if (magnitude_cmp == std::partial_ordering::less)
    return std::partial_ordering::greater;
  if (magnitude_cmp == std::partial_ordering::greater)
    return std::partial_ordering::less;
  return magnitude_cmp;  // equivalent stays equivalent
}

/** @brief @c SignedCardinality @c == @c std::floating_point.  @c NaZ /
 *         @c NaN never equal anything; @c ±ℵ_0 equals only @c ±inf;
 *         finite delegates to the integer-domain @c
 *         eq_unsigned_to_floating on the magnitude with sign
 *         reconciled, so float rounding cannot fake equality on
 *         large-magnitude integers. */
export template <std::floating_point F>
constexpr bool operator==(const SignedCardinality& lhs, F rhs) noexcept {
  using namespace detail;
  if (sc_is_naz(lhs)) return false;
  if (std::isnan(rhs)) return false;
  if (sc_is_pos_inf(lhs)) return std::isinf(rhs) && !std::signbit(rhs);
  if (sc_is_neg_inf(lhs)) return std::isinf(rhs) && std::signbit(rhs);
  if (!std::isfinite(rhs)) return false;
  const auto& z = std::get<SignedExtensionalCardinal<>>(lhs);
  using LT = ExtensionalCardinal<>::word_type;
  const LT magnitude_value = z.magnitude().value;
  const bool z_is_zero = (magnitude_value == 0);
  if (z_is_zero) return detail::is_zero(rhs);  // ±0 collapse
  const bool z_is_neg = z.negative();
  const bool rhs_is_neg = detail::is_negative(rhs);
  if (z_is_neg != rhs_is_neg) return false;
  return eq_unsigned_to_floating<F, LT>(magnitude_value,
                                        rhs_is_neg ? -rhs : rhs);
}

/** @brief Canonical embedding K3 ↪ ℤ: @c False ↦ -1, @c Unknown ↦ 0,
 *  @c True ↦ 1, the enum's own values, exactly (#430). */
export inline constexpr auto embed_𝕂3_ℤ_ = arrow<Ternary, Integer>(
    [](const Ternary& t) { return Integer{std::to_underlying(t)}; });

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
/** @param n a signed cardinal.  @return its successor, modulo the word. */
export template <std::integral S>
constexpr SignedExtensionalCardinal<S> successor(
    const SignedExtensionalCardinal<S>& n) noexcept {
  return n + SignedExtensionalCardinal<S>{1};
}
/** @param n a signed cardinal.  @return its predecessor, modulo the word. */
export template <std::integral S>
constexpr SignedExtensionalCardinal<S> predecessor(
    const SignedExtensionalCardinal<S>& n) noexcept {
  return n - SignedExtensionalCardinal<S>{1};
}
}  // namespace dedekind::sets

// The cardinals are integer chains: registered with @c :chain's
// @c IsIntegerChain, which refines the NNO's step.
namespace dedekind::category {
template <>
struct is_integer_chain<dedekind::sets::SignedCardinality> : std::true_type {};
template <typename W>
struct is_integer_chain<dedekind::sets::ExtensionalCardinal<W>>
    : std::true_type {};
template <typename S>
struct is_integer_chain<dedekind::sets::SignedExtensionalCardinal<S>>
    : std::true_type {};
}  // namespace dedekind::category

namespace dedekind::sets {
/** @brief @c :chain's @c IsIntegerChain. */
export using dedekind::category::IsIntegerChain;
/** @brief The old name of @c IsIntegerChain, which never claimed a ring.
 *  @deprecated Use @c IsIntegerChain.  @tparam T the carrier. */
export template <typename T>
concept IsRingIntegral = IsIntegerChain<T>;

// Positive witnesses: built-in integrals + the variant ℤ-proxy carrier.
static_assert(IsRingIntegral<int>);
static_assert(IsRingIntegral<unsigned int>);
static_assert(IsRingIntegral<long>);
static_assert(IsRingIntegral<long long>);
static_assert(IsRingIntegral<std::size_t>);
static_assert(IsRingIntegral<bool>);
static_assert(IsRingIntegral<SignedCardinality>,
              "SignedCardinality must satisfy IsRingIntegral — the variant "
              "ℤ-proxy is the canonical exact-ℤ integer-range carrier.");
static_assert(IsRingIntegral<SignedExtensionalCardinal<>>,
              "SignedExtensionalCardinal<> must satisfy IsRingIntegral — the "
              "bounded exact-ℤ carrier underlying the ℤ alias.");
static_assert(IsRingIntegral<ExtensionalCardinal<>>,
              "ExtensionalCardinal<> must satisfy IsRingIntegral — the bounded "
              "exact-ℕ carrier (sibling of SEC<> on the unsigned side).");

// Cv-/ref-qualified spellings normalise via std::remove_cvref_t, so the concept
// fires in deduced contexts.
static_assert(IsRingIntegral<const int>);
static_assert(IsRingIntegral<int&>);
static_assert(IsRingIntegral<SignedCardinality&&>);

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
// Category trait registrations for SignedExtensionalCardinal<>
// ---------------------------------------------------------------------------
//
// Mirrors the ExtensionalCardinal<> registrations above, with the crucial
// addition of `is_invertible_v` under + (every integer has an additive
// inverse — this is what lifts ℤ from a commutative monoid to an
// abelian group at the trait-registry level).

using Z1 = dedekind::sets::SignedExtensionalCardinal<>;

template <>
struct unit_trait<Z1, std::plus<Z1>> {
  using value_type = Z1;
  static constexpr value_type value = value_type{0};
};

template <>
struct unit_trait<Z1, std::multiplies<Z1>> {
  using value_type = Z1;
  static constexpr value_type value = value_type{1};
};

template <>
inline constexpr bool is_associative_v<Z1, std::plus<Z1>> = true;

template <>
inline constexpr bool is_associative_v<Z1, std::multiplies<Z1>> = true;

template <>
inline constexpr bool is_commutative_v<Z1, std::plus<Z1>> = true;

template <>
inline constexpr bool is_commutative_v<Z1, std::multiplies<Z1>> = true;

template <>
inline constexpr bool
    is_distributive_v<Z1, std::multiplies<Z1>, std::plus<Z1>> = true;

// Periodicity: the operation is total on the finite carrier, wrapping at
// the word's capacity boundary. Required for IsTotal / IsMagma dispatch.
template <>
struct is_periodic<Z1, std::plus<Z1>> : std::true_type {};

template <>
struct is_periodic<Z1, std::multiplies<Z1>> : std::true_type {};

// The defining trait of ℤ over ℕ: every element has an additive inverse
// (unary minus is total on SignedExtensionalCardinal, canonical zero is
// preserved under negation).
template <>
inline constexpr bool is_invertible_v<Z1, std::plus<Z1>> = true;

template <>
struct inverse_trait<Z1, std::plus<Z1>> {
  static constexpr bool exists = true;
  using value_type = Z1;
  static constexpr value_type compute(const Z1& value) noexcept {
    return -value;
  }
};

template <typename S>
inline constexpr bool is_reflexive_v<
    dedekind::sets::SignedExtensionalCardinal<S>, std::less_equal<>> = true;

template <typename S>
inline constexpr bool is_transitive_v<
    dedekind::sets::SignedExtensionalCardinal<S>, std::less_equal<>> = true;

template <typename S>
inline constexpr bool is_antisymmetric_v<
    dedekind::sets::SignedExtensionalCardinal<S>, std::less_equal<>> = true;
template <typename S>
inline constexpr bool is_connex_v<dedekind::sets::SignedExtensionalCardinal<S>,
                                  std::less_equal<>> = true;

static_assert(IsRing<Z1, std::plus<Z1>, std::multiplies<Z1>>,
              "SignedExtensionalCardinal<> must certify as a total ring.");

static_assert(IsAbelianGroup<Z1, std::plus<Z1>>,
              "SignedExtensionalCardinal<> must certify as an abelian group "
              "under addition (the defining ℤ-versus-ℕ distinction).");

// ---------------------------------------------------------------------------
// IsCyclicGroup witnesses for the cardinal carriers (#378).
// ---------------------------------------------------------------------------
//
// `ExtensionalCardinal<W>` is the bounded ℕ carrier; under addition
// it wraps modulo 2^w, forming a finite cyclic group.
// `SignedExtensionalCardinal<S>` is its signed counterpart, also
// cyclic under addition (same capacity, signed representation).
// Order is exposed via `cyclic_order_v`; here we report 0
// ("order not representable as std::size_t") rather than the exact
// 2^w bound --- the concept-level claim is cyclicity,
// not the cardinality value.

template <typename W>
struct is_cyclic_group<dedekind::sets::ExtensionalCardinal<W>,
                       std::plus<dedekind::sets::ExtensionalCardinal<W>>>
    : std::true_type {};

template <typename S>
struct is_cyclic_group<dedekind::sets::SignedExtensionalCardinal<S>,
                       std::plus<dedekind::sets::SignedExtensionalCardinal<S>>>
    : std::true_type {};

static_assert(IsCyclicGroup<C1, std::plus<C1>>,
              "ExtensionalCardinal<> under + must satisfy IsCyclicGroup "
              "(bounded ℕ, wraps modulo capacity).");

static_assert(IsCyclicGroup<Z1, std::plus<Z1>>,
              "SignedExtensionalCardinal<> under + must satisfy IsCyclicGroup "
              "(bounded ℤ, wraps modulo capacity).");

// ---------------------------------------------------------------------------
// Category trait registrations for SignedCardinality (#377)
// ---------------------------------------------------------------------------
//
// The extended-integer carrier is a saturating abelian group under +
// (escalating to ±ℵ_0 rather than wrapping or being undefined).  The
// `is_saturating_v` trait is the load-bearing certificate that lifts
// IsTotal -> IsMagma -> IsMonoid -> IsGroup -> IsAbelianGroup on this
// carrier.  It is *not* periodic (unbounded), nor idempotent
// (a+a != a in general), nor cyclic (no finite generator).

using SC = dedekind::sets::SignedCardinality;

template <>
struct unit_trait<SC, std::plus<SC>> {
  using value_type = SC;
  static constexpr value_type value =
      SC{dedekind::sets::SignedExtensionalCardinal<>{0}};
};

template <>
struct unit_trait<SC, std::multiplies<SC>> {
  using value_type = SC;
  static constexpr value_type value =
      SC{dedekind::sets::SignedExtensionalCardinal<>{1}};
};

template <>
inline constexpr bool is_associative_v<SC, std::plus<SC>> = true;

template <>
inline constexpr bool is_associative_v<SC, std::multiplies<SC>> = true;

template <>
inline constexpr bool is_commutative_v<SC, std::plus<SC>> = true;

template <>
inline constexpr bool is_commutative_v<SC, std::multiplies<SC>> = true;

template <>
inline constexpr bool
    is_distributive_v<SC, std::multiplies<SC>, std::plus<SC>> = true;

// Saturation rather than periodicity: SignedCardinality is unbounded
// and escalates to ±ℵ_0 on overflow.  This is the load-bearing
// trait that satisfies the IsTotal pragmatic certificate.  See
// `is_saturating` in `category:total`.
template <>
struct is_saturating<SC, std::plus<SC>> : std::true_type {};

template <>
struct is_saturating<SC, std::multiplies<SC>> : std::true_type {};

// Subtraction is x + (−y): it saturates exactly as addition does.
template <>
struct is_saturating<SC, std::minus<SC>> : std::true_type {};

// The defining ℤ trait exposes unary `-` as the operation used by
// the additive-inverse machinery.  -ℵ_0 maps to +ℵ_0 and vice versa;
// for finite values this is ordinary sign-flip; for NaZ, unary `-`
// propagates NaZ (mirroring how IEEE -NaN behaves), without
// asserting that NaZ satisfies an additive-inverse law.  The
// pragmatic certificate is "the finite fragment is ℤ exactly; the
// saturating elements satisfy the law mutually; NaZ propagates",
// which is enough to lift IsAbelianGroup on the carrier.
template <>
inline constexpr bool is_invertible_v<SC, std::plus<SC>> = true;

template <>
struct inverse_trait<SC, std::plus<SC>> {
  static constexpr bool exists = true;
  using value_type = SC;
  static constexpr value_type compute(const SC& value) noexcept {
    return -value;
  }
};

// Order-axiom traits under std::less_equal<>: the homogeneous <=> above
// ranks −ℵ_0 < finite < +ℵ_0 and is reflexive on every alternative (NaZ
// <=> NaZ is equivalent), so <= is reflexive, transitive and antisymmetric
// on the whole carrier.  Comparability is the one law NaZ breaks (it is
// unordered against every other value), and the carrier is certified a
// CHAIN regardless, under the posture ℚ takes for the same sentinels: the
// non-finite alternatives are the out-of-memory boundary of the finite
// backing, not inhabitants of ℤ --- NaZ is the indeterminate-form
// tripwire, never a point of the integers.  ℤ = 𝔸<SignedCardinality> is
// thereby a certified chain like ℕ = 𝔸<Numeral>: halfspaces, Interval
// and max / min admit it on the same footing.
template <>
inline constexpr bool is_reflexive_v<SC, std::less_equal<>> = true;

template <>
inline constexpr bool is_transitive_v<SC, std::less_equal<>> = true;

template <>
inline constexpr bool is_antisymmetric_v<SC, std::less_equal<>> = true;
template <>
inline constexpr bool is_connex_v<SC, std::less_equal<>> = true;

static_assert(IsTotalOrder<SC, std::less_equal<>, bool>,
              "SignedCardinality is a certified total order under <=: the "
              "ℤ proxy is a chain on the same footing as ℕ.");

static_assert(IsAbelianGroup<SC, std::plus<SC>>,
              "SignedCardinality must certify as an abelian group under "
              "addition --- the canonical exact-ℤ extended-integer carrier "
              "with ±ℵ_0 escalation (closes #377's abelian-group on ℤ "
              "acceptance).");

static_assert(IsRing<SC, std::plus<SC>, std::multiplies<SC>>,
              "SignedCardinality must certify as a total ring (the "
              "saturation certificate satisfies IsTotal on the "
              "multiplicative side too).");

static_assert(IsCommutativeRing<SC, std::plus<SC>, std::multiplies<SC>>,
              "SignedCardinality must certify as a commutative ring.");

// ---------------------------------------------------------------------------
// SpeciesTraits specialisations: lift ℕ's carrier @c Numeral and the
// variant ℤ-proxy to first-class @c IsSpecies status (per #413), so
// @c ambient_set<Numeral>(...) and @c ambient_set<SignedCardinality>(...)
// anchor IsSet witnesses (the primary @c SpeciesTraits<T> registration
// covers @c std::integral / @c std::floating_point / @c bool only).
//
// Pattern follows @c SpeciesTraits<Rational<Z>> in @c numbers/rational.cppm:
// @c Domain @c = the carrier itself, @c machine_type @c = the carrier (no
// separate machine sibling).
// ---------------------------------------------------------------------------

template <>
struct SpeciesTraits<dedekind::sets::Numeral> {
  using Domain = dedekind::sets::Numeral;
  using machine_type = dedekind::sets::Numeral;
};

template <>
struct SpeciesTraits<dedekind::sets::Integer> {
  using Domain = dedekind::sets::Integer;
  using machine_type = dedekind::sets::Integer;
};

template <>
struct SpeciesTraits<dedekind::sets::SignedCardinality> {
  using Domain = dedekind::sets::SignedCardinality;
  using machine_type = dedekind::sets::SignedCardinality;
};

// Lift the bounded exact ℤ carrier @c SignedExtensionalCardinal<S>
// to IsSpecies status as well, parallel to @c SignedCardinality
// above.  Required for @c ℤ @c = @c SignedExtensionalCardinal<>
// to flow into @c ambient_set<...> as the canonical carrier per
// #399 slice 3 (the value-level @c Z constant in
// @c numbers/integer.cppm anchors @c IsSet on this carrier).  The word
// @c W is kept parametric, mirroring the carrier.
template <typename S>
struct SpeciesTraits<dedekind::sets::SignedExtensionalCardinal<S>> {
  using Domain = dedekind::sets::SignedExtensionalCardinal<S>;
  using machine_type = dedekind::sets::SignedExtensionalCardinal<S>;
};

// IsSpecies witnesses: the carriers are first-class species and can flow
// into @c ambient_set<...> / ETCS object construction in downstream
// partitions (cf.\ @c sets:boundaries, where @c ℕ @c = @c 𝔸<Numeral>).
static_assert(IsSpecies<dedekind::sets::Numeral>,
              "Numeral, ℕ's carrier, must be a recognised Species.");
static_assert(IsSpecies<dedekind::sets::Integer>,
              "Integer, ℤ's carrier, must be a recognised Species.");
static_assert(IsSpecies<dedekind::sets::SignedCardinality>,
              "SignedCardinality must be a recognised Species (post-#413).");
static_assert(
    IsSpecies<dedekind::sets::SignedExtensionalCardinal<>>,
    "SignedExtensionalCardinal<> must be a recognised Species — anchors "
    "the @c ℤ alias's IsSet witness in @c numbers:integer (#399 slice 3).");

// Standard-concept witnesses (#669): the explicit homogeneous operators
// added above (operator== and operator<=>) close the module-boundary
// gap between the variant carriers and the std-library's concept
// templates.  Pinning these means downstream code can freely write
// `requires std::regular<T>` or `requires std::three_way_comparable<T,
// std::partial_ordering>` against the variant ℤ-proxy carrier
// without surprise rejection.

// --- SignedCardinality (ℤ ∪ {±ℵ_0, NaZ}) ---
// <=> returns std::partial_ordering: NaZ is unordered against every other
// value (IEEE-NaN-style).  std::totally_ordered is a SYNTAX check (the four
// relational operators exist, rewritten from <=>) and holds; the semantic
// chain certificate is the IsTotalOrder registration above.
static_assert(std::equality_comparable<dedekind::sets::SignedCardinality>,
              "SignedCardinality must be std::equality_comparable; the "
              "explicit homogeneous == above (alternative-tag equality + "
              "NaZ-as-equal) closes #669 for the ℤ proxy.");
static_assert(std::regular<dedekind::sets::SignedCardinality>,
              "SignedCardinality must be std::regular --- closes the "
              "persistent :sint.cppm local-only build failure "
              "(origin(embed_sint_ℤ(η(42))) == "
              "finite_signed_cardinality(42) static_assert at "
              "sint.cppm:228).");
static_assert(std::three_way_comparable<dedekind::sets::SignedCardinality,
                                        std::partial_ordering>,
              "SignedCardinality must be std::three_way_comparable with "
              "std::partial_ordering --- NaZ is unordered with everything, "
              "mirroring IEEE-NaN's partial-order semantics.");
static_assert(std::totally_ordered<dedekind::sets::SignedCardinality>,
              "SignedCardinality carries the four relational operators "
              "(rewritten from <=>); comparability of NaZ is a semantic "
              "matter settled by the order registration, not by syntax.");

template <>
inline constexpr bool is_monic_arrow_v<std::decay_t<decltype(embed_𝕂3_ℤ_)>> =
    true;
static_assert(IsInjective<std::decay_t<decltype(embed_𝕂3_ℤ_)>>,
              "embed_𝕂3_ℤ_ (𝕂3 → ℤ) is registered injective.");
static_assert(embed_𝕂3_ℤ_(Ternary::False) == dedekind::sets::Integer{-1} &&
                  embed_𝕂3_ℤ_(Ternary::Unknown) == dedekind::sets::Integer{} &&
                  embed_𝕂3_ℤ_(Ternary::True) == dedekind::sets::Integer{1},
              "𝕂3 ↪ ℤ on its three values");

// IsMonotone witness (#664 morphism vocabulary): @c False @c ↦ @c -1,
// @c Unknown @c ↦ @c 0, @c True @c ↦ @c 1, which respects the canonical
// Kleene three-value order @c False @c < @c Unknown @c < @c True.
template <>
inline constexpr bool
    is_monotone_v<std::decay_t<decltype(embed_𝕂3_ℤ_)>, std::less_equal<>> =
        true;
static_assert(IsMonotone<std::decay_t<decltype(embed_𝕂3_ℤ_)>>,
              "embed_𝕂3_ℤ_ (𝕂3 → ℤ) is monotone — preserves the Kleene "
              "three-value order through the canonical embedding.");
}  // namespace dedekind::category

// ── ℕ's algebra, declared, and its witnesses ────────────────────────────────
namespace dedekind::category {
using Nat = dedekind::sets::Numeral;
template <>
struct unit_trait<Nat, std::plus<Nat>> {
  using value_type = Nat;
  static constexpr value_type value{};
};
template <>
struct unit_trait<Nat, std::multiplies<Nat>> {
  using value_type = Nat;
  static constexpr value_type value{1u};
};
// Path D to totality: + and * are exact, closed and lossless, the boundary
// the machine's memory, as for Rational (is_exact_total names ℕ among them).
template <>
struct is_exact_total<Nat, std::plus<Nat>> : std::true_type {};
template <>
struct is_exact_total<Nat, std::multiplies<Nat>> : std::true_type {};
template <>
inline constexpr bool is_associative_v<Nat, std::plus<Nat>> = true;
template <>
inline constexpr bool is_associative_v<Nat, std::multiplies<Nat>> = true;
template <>
inline constexpr bool is_commutative_v<Nat, std::plus<Nat>> = true;
template <>
inline constexpr bool is_commutative_v<Nat, std::multiplies<Nat>> = true;
template <>
inline constexpr bool
    is_distributive_v<Nat, std::multiplies<Nat>, std::plus<Nat>> = true;
// The order: by limbs, the longer the larger, then lexicographic from the
// top (operator<=> above), a chain.
template <>
inline constexpr bool is_reflexive_v<Nat, std::less_equal<>> = true;
template <>
inline constexpr bool is_transitive_v<Nat, std::less_equal<>> = true;
template <>
inline constexpr bool is_antisymmetric_v<Nat, std::less_equal<>> = true;
template <>
inline constexpr bool is_connex_v<Nat, std::less_equal<>> = true;
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
static_assert(successor(Numeral{top}) == Numeral{top} + Numeral{1u} &&
                  predecessor(successor(Numeral{top})) == Numeral{top} &&
                  predecessor(Numeral{}) == Numeral{},
              "the step crosses the word's edge both ways; 0 is the monus' "
              "fixed point");
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
static_assert(
    dedekind::category::IsRig<Numeral, std::plus<Numeral>,
                              std::multiplies<Numeral>> &&
        dedekind::category::IsCommutative<Numeral, std::multiplies<Numeral>> &&
        dedekind::category::IsIsomorphism<dedekind::category::In<Numeral>>,
    "ℕ is a commutative rig, and Lambek holds on it: [Z, S] is an isomorphism");
constexpr auto in_ = dedekind::category::In<Numeral>{};
constexpr auto prior_ = inverse(in_);
static_assert(prior_(in_(std::nullopt)) == std::nullopt &&
                  prior_(in_(Numeral{top})) == Numeral{top} &&
                  in_(prior_(Numeral{})) == Numeral{} &&
                  in_(prior_(Numeral{top} + Numeral{1u})) ==
                      Numeral{top} + Numeral{1u} &&
                  inverse(prior_)(prior_(Numeral{top} + Numeral{1u})) ==
                      Numeral{top} + Numeral{1u},
              "the inverse laws on values: Prior after In and In after Prior "
              "are identities, at zero and across the word's edge");
}  // namespace dedekind::sets::detail_numeral_witness

// ── ℤ's algebra, declared, and its witnesses ────────────────────────────────
namespace dedekind::category {
using Int = dedekind::sets::Integer;
template <>
struct unit_trait<Int, std::plus<Int>> {
  using value_type = Int;
  static constexpr value_type value{};
};
template <>
struct unit_trait<Int, std::multiplies<Int>> {
  using value_type = Int;
  static constexpr value_type value{dedekind::sets::Numeral{1u}};
};
// Path D to totality, as for ℕ: exact, closed and lossless.  The additive
// inverse is unary -, read by sets' inverse(x, +): a group without a
// registration.
template <>
struct is_exact_total<Int, std::plus<Int>> : std::true_type {};
template <>
struct is_exact_total<Int, std::multiplies<Int>> : std::true_type {};
template <>
inline constexpr bool is_associative_v<Int, std::plus<Int>> = true;
template <>
inline constexpr bool is_associative_v<Int, std::multiplies<Int>> = true;
template <>
inline constexpr bool is_commutative_v<Int, std::plus<Int>> = true;
template <>
inline constexpr bool is_commutative_v<Int, std::multiplies<Int>> = true;
template <>
inline constexpr bool
    is_distributive_v<Int, std::multiplies<Int>, std::plus<Int>> = true;
// The order: the negatives below, by magnitude reversed, then ℕ (operator<=>
// above), a chain.
template <>
inline constexpr bool is_reflexive_v<Int, std::less_equal<>> = true;
template <>
inline constexpr bool is_transitive_v<Int, std::less_equal<>> = true;
template <>
inline constexpr bool is_antisymmetric_v<Int, std::less_equal<>> = true;
template <>
inline constexpr bool is_connex_v<Int, std::less_equal<>> = true;
}  // namespace dedekind::category

namespace dedekind::sets::detail_integer_witness {
constexpr std::uint64_t top = std::numeric_limits<std::uint64_t>::max();
constexpr std::int64_t least = std::numeric_limits<std::int64_t>::min();
/** @return @f$2^{64}@f$, the first integer above the word. */
constexpr Integer two_64() { return Integer{Numeral{top} + Numeral{1u}}; }
static_assert(Integer{-0} == Integer{} && -Integer{} == Integer{} &&
                  !(-Integer{}).negative &&
                  !(Integer{5} - Integer{5}).negative &&
                  Integer{Numeral{}, true} == Integer{},
              "zero is never negative: the normal form");
static_assert(Integer{least}.magnitude == Numeral{std::uint64_t{1} << 63} &&
                  Integer{least} - Integer{1} ==
                      -Integer{Numeral{(std::uint64_t{1} << 63) + 1u}} &&
                  -two_64() - Integer{1} < -two_64() &&
                  -two_64() < Integer{least},
              "the signed word's least value embeds exactly, and ℤ goes on "
              "below it");
static_assert(two_64() - Integer{1} == Integer{Numeral{top}} &&
                  Integer{3} - two_64() == -Integer{Numeral{top - 2u}} &&
                  two_64() + -two_64() == Integer{} &&
                  Integer{-7} * -two_64() == Integer{7} * two_64() &&
                  Integer{-2} < Integer{-1} && Integer{-1} < Integer{} &&
                  Integer{} < Integer{1} && Integer{1} < two_64(),
              "opposite signs subtract magnitudes across the word's edge; the "
              "order is a chain");
static_assert((two_64() + Integer{-3}) + -two_64() ==
                      two_64() + (Integer{-3} + -two_64()) &&
                  -two_64() * (two_64() + Integer{-1}) ==
                      -two_64() * two_64() + -two_64() * Integer{-1} &&
                  (Integer{least} * Integer{least}) * -two_64() ==
                      Integer{least} * (Integer{least} * -two_64()) &&
                  Integer{least} + -two_64() == -two_64() + Integer{least},
              "the ring laws across the word's edge, above 2^64 and below "
              "-2^63");
static_assert(
    dedekind::category::IsCommutativeRing<Integer, std::plus<Integer>,
                                          std::multiplies<Integer>> &&
        dedekind::category::IsAbelianGroup<Integer, std::plus<Integer>> &&
        !dedekind::category::IsField<Integer, std::plus<Integer>,
                                     std::multiplies<Integer>>,
    "ℤ is a commutative ring, its + a group; not a field");
static_assert(IsRanked<Numeral> && IsRanked<Integer> &&
                  rank(Numeral{top}) == Numeral{top} &&
                  rank(-two_64()) == rank(two_64()) &&
                  rank(Integer{least}) == Numeral{std::uint64_t{1} << 63} &&
                  rank(Integer{}) == Numeral{},
              "ℕ and ℤ are ranked: the identity on ℕ, |z| on ℤ");
}  // namespace dedekind::sets::detail_integer_witness
