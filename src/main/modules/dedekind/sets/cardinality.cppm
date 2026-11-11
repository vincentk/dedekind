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
  if (x.limb_count() != y.limb_count())
    return x.limb_count() <=> y.limb_count();
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

  constexpr Integer() = default;
  /** @param s an atom: a signed word, embedded exactly. */
  template <std::signed_integral S>
  constexpr explicit Integer(S s)
      : magnitude_(Numeral{s < S{}
                               ? static_cast<std::make_unsigned_t<S>>(
                                     std::make_unsigned_t<S>{} -
                                     static_cast<std::make_unsigned_t<S>>(s))
                               : static_cast<std::make_unsigned_t<S>>(s)}),
        negative_(s < S{}) {
    static_assert(std::numeric_limits<S>::digits < 64,
                  "a signed atom's magnitude fits the inline limb");
  }
  /** @param n a natural: ℕ ↪ ℤ, @f$n \mapsto n - 0@f$. */
  constexpr explicit Integer(Numeral n) : magnitude_(std::move(n)) {}
  /** @param m a magnitude.  @param minus the sign, dropped at zero: the
   *  normal form. */
  constexpr Integer(Numeral m, bool minus)
      : magnitude_(std::move(m)), negative_(minus && magnitude_ != Numeral{}) {}
  /** @brief Equality of normal forms, read through the accessors. */
  constexpr bool operator==(const Integer& other) const {
    return magnitude_ == other.magnitude_ && negative() == other.negative();
  }
  /** @return @f$|z|@f$, the rank of the normal form. */
  constexpr Numeral magnitude() const { return magnitude_; }
  /** @return the sign: false at zero. */
  constexpr bool negative() const {
    return negative_ && magnitude_ != Numeral{};
  }

 private:
  // The normal form is what the accessors read: a sign is reported only with
  // a magnitude, so no value, a moved-from one included, is a negative zero.
  Numeral magnitude_{};
  bool negative_{};
};

/** @brief The additive inverse: the sign flipped, zero kept. */
export constexpr Integer operator-(const Integer& z) {
  return Integer(z.magnitude(), !z.negative());
}
/** @brief The sum: magnitudes add under one sign; under two, the smaller is
 *  taken from the larger, which gives its sign. */
export constexpr Integer operator+(const Integer& x, const Integer& y) {
  if (x.negative() == y.negative())
    return Integer(x.magnitude() + y.magnitude(), x.negative());
  if (y.magnitude() <= x.magnitude())
    return Integer(Numeral(detail_numeral::subtract(x.magnitude().limbs(),
                                                    y.magnitude().limbs())),
                   x.negative());
  return Integer(Numeral(detail_numeral::subtract(y.magnitude().limbs(),
                                                  x.magnitude().limbs())),
                 y.negative());
}
/** @brief The difference: the sum with the inverse. */
export constexpr Integer operator-(const Integer& x, const Integer& y) {
  return x + -y;
}
/** @brief The product: magnitudes multiply, signs multiply. */
export constexpr Integer operator*(const Integer& x, const Integer& y) {
  return Integer(x.magnitude() * y.magnitude(), x.negative() != y.negative());
}
/** @brief The order of ℤ: the negatives below, ordered by magnitude reversed;
 *  the naturals above, ordered as ℕ. */
export constexpr std::strong_ordering operator<=>(const Integer& x,
                                                  const Integer& y) {
  if (x.negative() != y.negative()) return y.negative() <=> x.negative();
  return x.negative() ? y.magnitude() <=> x.magnitude()
                      : x.magnitude() <=> y.magnitude();
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
export constexpr Numeral rank(const Integer& z) { return z.magnitude(); }
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

// ---------------------------------------------------------------------------
// SpeciesTraits specialisations: lift ℕ's carrier @c Numeral and ℤ's
// @c Integer to first-class @c IsSpecies status (per #413), so
// @c ambient_set<Numeral>(...) and @c ambient_set<Integer>(...)
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

// IsSpecies witnesses: the carriers are first-class species and can flow
// into @c ambient_set<...> / ETCS object construction in downstream
// partitions (cf.\ @c sets:boundaries, where @c ℕ @c = @c 𝔸<Numeral>).
static_assert(IsSpecies<dedekind::sets::Numeral>,
              "Numeral, ℕ's carrier, must be a recognised Species.");
static_assert(IsSpecies<dedekind::sets::Integer>,
              "Integer, ℤ's carrier, must be a recognised Species.");
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
                  !(-Integer{}).negative() &&
                  !(Integer{5} - Integer{5}).negative() &&
                  Integer{Numeral{}, true} == Integer{},
              "zero is never negative: the normal form");
static_assert(Integer{least}.magnitude() == Numeral{std::uint64_t{1} << 63} &&
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
