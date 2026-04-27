#include <catch2/catch_test_macros.hpp>
#include <compare>
#include <cstddef>
#include <functional>
#include <limits>
#include <variant>

import dedekind.category;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE("Numbers: Cardinality draft policy", "[numbers][cardinality]") {
  SECTION("Finite and ℵ_0 comparison policy") {
    const Cardinality finite_three = finite_cardinality(3);
    const Cardinality inf = ℵ_0{};

    CHECK(compare(finite_three, inf) == std::strong_ordering::less);
    CHECK(compare(inf, finite_three) == std::strong_ordering::greater);
    CHECK(compare(inf, inf) == std::strong_ordering::equal);
  }

  SECTION("Addition and multiplication absorb to ℵ_0") {
    const Cardinality finite_five = finite_cardinality(5);
    const Cardinality finite_six = finite_cardinality(6);
    const Cardinality inf = ℵ_0{};

    CHECK(add(finite_five, finite_six) == finite_cardinality(11));
    CHECK(add(finite_five, inf) == Cardinality{ℵ_0{}});

    CHECK(mul(finite_five, finite_six) == finite_cardinality(30));
    CHECK(mul(finite_five, inf) == Cardinality{ℵ_0{}});
  }

  SECTION("Realization to size_t remains explicit") {
    const std::size_t sentinel = std::numeric_limits<std::size_t>::max() - 17;

    CHECK(realize_to_size_t(finite_cardinality(42), sentinel) == 42);
    CHECK(realize_to_size_t(Cardinality{ℵ_0{}}, sentinel) == sentinel);
  }
}

TEST_CASE("Numbers: ExtensionalCardinal<N> multi-limb semantics",
          "[numbers][cardinality][cardinal]") {
  using C1 = ExtensionalCardinal<1>;
  using C2 = ExtensionalCardinal<2>;

  SECTION("size_t is a Liskov subtype: implicit embedding") {
    // size_t constructs ExtensionalCardinal<N> without explicit cast
    const C1 a = std::size_t{42};
    CHECK(a.limbs[0] == 42);

    const C2 b = std::size_t{7};
    CHECK(b.limbs[0] == 7);
    CHECK(b.limbs[1] == 0);
  }

  SECTION("Operator results are ExtensionalCardinal<N>, not size_t") {
    const C1 x = std::size_t{3};
    const C1 y = std::size_t{4};
    const C1 sum = x + y;
    STATIC_CHECK(std::is_same_v<decltype(sum), const C1>);
    CHECK(sum.limbs[0] == 7);
  }

  SECTION("Carry propagates across limbs in ExtensionalCardinal<2>") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    // limb[0] = SIZE_MAX, limb[1] = 0
    C2 hi{};
    hi.limbs[0] = max;
    // Adding 1 should carry into limb[1]
    const C2 one = std::size_t{1};
    const C2 result = hi + one;
    CHECK(result.limbs[0] == 0);
    CHECK(result.limbs[1] == 1);
  }

  SECTION("checked_add detects 1-limb overflow") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    const C1 a = max;
    const C1 b = std::size_t{1};
    const auto res = C1::checked_add(a, b);
    CHECK(res.overflowed == true);
    CHECK(res.value.limbs[0] == 0);  // wraps to zero
  }

  SECTION("add_or_ℵ_0 converts 1-limb overflow to ℵ_0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    const C1 a = max;
    const C1 b = std::size_t{1};
    const auto result = add_or_ℵ_0(a, b);
    REQUIRE(std::holds_alternative<ℵ_0>(result));
  }

  SECTION("mul_or_ℵ_0 converts large-product overflow to ℵ_0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    const C1 a = max;
    const C1 b = std::size_t{2};
    const auto result = mul_or_ℵ_0(a, b);
    REQUIRE(std::holds_alternative<ℵ_0>(result));
  }

  SECTION("ExtensionalCardinal<2> addition overflow into variant-level ℵ_0") {
    const std::size_t max = std::numeric_limits<std::size_t>::max();
    C2 big{};
    big.limbs[0] = max;
    big.limbs[1] = max;
    const C2 one = std::size_t{1};
    const auto result = add_or_ℵ_0(big, one);
    REQUIRE(std::holds_alternative<ℵ_0>(result));
  }

  SECTION("realize_to_size_t on ExtensionalCardinal<2> with high limb") {
    const std::size_t sentinel = 0xDEAD;
    C2 big{};
    big.limbs[0] = 99;
    big.limbs[1] = 1;  // value > SIZE_MAX
    CHECK(big.realize_to_size_t(sentinel) == sentinel);

    C2 small{};
    small.limbs[0] = 42;
    small.limbs[1] = 0;
    CHECK(small.realize_to_size_t(sentinel) == 42);
  }

  SECTION("Three-way comparison is lexicographic on limbs") {
    C2 a{};
    a.limbs[0] = 0;
    a.limbs[1] = 1;  // value = 2^64

    C2 b{};
    b.limbs[0] = std::numeric_limits<std::size_t>::max();
    b.limbs[1] = 0;  // value = SIZE_MAX < 2^64

    CHECK((a <=> b) == std::strong_ordering::greater);
    CHECK((b <=> a) == std::strong_ordering::less);
  }
}

TEST_CASE("Numbers: Cardinality witnesses and composition",
          "[numbers][cardinality][effects]") {
  SECTION("Writer-like bind respects left identity") {
    const auto f = [](const Cardinality& c) {
      return CardinalityEffect{add(c, finite_cardinality(2)),
                               finite_cardinality(2)};
    };

    const Cardinality seed = finite_cardinality(7);
    const CardinalityEffect lhs = bind_effect(unit_effect(seed), f);
    const CardinalityEffect rhs = f(seed);

    CHECK(lhs.value == rhs.value);
    CHECK(lhs.trace == rhs.trace);
  }
}

TEST_CASE("Numbers: ExtensionalCardinal additive inverse",
          "[numbers][cardinality][inverse]") {
  using C1 = ExtensionalCardinal<>;

  SECTION("inverse() free function returns additive inverse") {
    const C1 three{3};
    const C1 neg_three = inverse(three, std::plus<C1>{});
    // In two's-complement wrapping arithmetic: 3 + (-3) == 0
    CHECK((three + neg_three) == C1{0});
  }

  SECTION("Additive inverse of zero is zero") {
    const C1 zero{0};
    CHECK(inverse(zero, std::plus<C1>{}) == C1{0});
  }
}

TEST_CASE(
    "Numbers: Cardinality heterogeneous comparison vs std::integral (#415)",
    "[numbers][cardinality][order][heterogeneous]") {
  const Cardinality five = finite_cardinality(5);
  const Cardinality inf = ℵ_0{};

  SECTION("Finite vs unsigned and signed (non-negative) — three-way") {
    CHECK((five <=> 5u) == std::strong_ordering::equal);
    CHECK((five <=> 4u) == std::strong_ordering::greater);
    CHECK((five <=> 6u) == std::strong_ordering::less);
    CHECK((five <=> 5) == std::strong_ordering::equal);  // signed int, rhs >= 0
    CHECK(five == 5u);
    CHECK(five == 5);
    CHECK(five > 4u);
    CHECK(five < 6);
  }
  SECTION("Negative signed values land strictly below the ℕ proxy") {
    // No element of Cardinality is negative; finite > any negative int.
    CHECK((five <=> -1) == std::strong_ordering::greater);
    CHECK(five > -1);
    CHECK_FALSE(five == -1);
    // Even the empty cardinal (zero) is greater than a negative int.
    const Cardinality zero = finite_cardinality(0);
    CHECK(zero > -1);
    CHECK_FALSE(zero == -7);
  }
  SECTION("ℵ_0 dominates every finite int") {
    CHECK((inf <=> 0u) == std::strong_ordering::greater);
    CHECK(inf > std::numeric_limits<unsigned>::max());
    CHECK(inf > -1);
    CHECK_FALSE(inf == 0u);
  }
  SECTION("bool is std::integral (regression for make_unsigned_t<bool>)") {
    // std::make_unsigned_t<bool> is undefined; the comparison path must
    // delegate through ExtensionalCardinal<>'s integral ctor instead.
    const Cardinality one = finite_cardinality(1);
    const Cardinality zero = finite_cardinality(0);
    CHECK(one == true);
    CHECK(zero == false);
    CHECK(one > false);
    CHECK(zero < true);
  }
  SECTION("Rhs-first direction synthesised by C++20 rewrite rules") {
    CHECK(5u == five);
    CHECK(4 < five);
    CHECK(0u < inf);
  }
}

// Compile-time contract pinning for #415: variant carrier ↔ std::integral
// relational ops MUST be constexpr-evaluable.  These static_asserts lock
// the contract that downstream halfspace machinery relies on — for
// example, @c var<ℤ> @c > @c bound<-21> after the @c #402 retarget.
namespace {
constexpr auto sa_c5 = finite_cardinality(5);
constexpr auto sa_c0 = finite_cardinality(0);
constexpr auto sa_inf = Cardinality{ℵ_0{}};
constexpr auto sa_z5 = finite_signed_cardinality(5);
constexpr auto sa_z_neg3 = finite_signed_cardinality(-3);
constexpr auto sa_z_pos_inf = SignedCardinality{PositiveInfinity{}};
constexpr auto sa_z_neg_inf = SignedCardinality{NegativeInfinity{}};

// --- Cardinality × std::integral (forward and reversed) ---
static_assert(sa_c5 > 3u);
static_assert(sa_c5 > 3);
static_assert(sa_c5 < 10u);
static_assert(sa_c5 >= 5);
static_assert(sa_c5 <= 5u);
static_assert(sa_c5 == 5u);
static_assert(sa_c5 > -1);  // any Cardinality > any negative int
static_assert(sa_c0 > -7);
static_assert(sa_inf > 0u);
static_assert(sa_inf > -1);
// Reversed direction (T on LHS) — C++20 spaceship rewrite synthesis.
static_assert(3u < sa_c5);
static_assert(5u == sa_c5);
static_assert(-1 < sa_inf);

// --- SignedCardinality × std::integral (forward and reversed) ---
//   * signed-int axis
static_assert(sa_z5 > -3);
static_assert(sa_z5 < 10);
static_assert(sa_z5 >= 5);
static_assert(sa_z5 <= 5);
static_assert(sa_z5 == 5);
static_assert(sa_z_neg3 < 0);
static_assert(sa_z_neg3 > -10);
static_assert(sa_z_neg3 == -3);
//   * unsigned-int axis (positive finite + negative-finite-vs-unsigned)
static_assert(sa_z5 > 4u);
static_assert(sa_z5 == 5u);
static_assert(sa_z_neg3 < 0u);  // negative SignedCardinality < any unsigned
static_assert(sa_z_neg3 < 5u);
//   * bool axis
static_assert(sa_z5 > false);
static_assert(sa_z5 > true);
static_assert(finite_signed_cardinality(1) == true);
static_assert(finite_signed_cardinality(0) == false);
//   * ±ℵ_0 dominates any finite int unconditionally
static_assert(sa_z_pos_inf > 1'000'000);
static_assert(sa_z_pos_inf > std::numeric_limits<unsigned>::max());
static_assert(sa_z_neg_inf < -1'000'000);
//   * reversed direction (T on LHS)
static_assert(-3 < sa_z5);
static_assert(0 > sa_z_neg3);
static_assert(5 == sa_z5);
static_assert(4u < sa_z5);
static_assert(5u == sa_z5);
static_assert(false < sa_z5);
}  // namespace

TEST_CASE(
    "Numbers: SignedCardinality heterogeneous comparison vs std::integral "
    "(#415)",
    "[numbers][cardinality][order][heterogeneous][signed]") {
  const SignedCardinality pos_five = finite_signed_cardinality(5);
  const SignedCardinality neg_three = finite_signed_cardinality(-3);
  const SignedCardinality pos_inf{PositiveInfinity{}};
  const SignedCardinality neg_inf{NegativeInfinity{}};
  const SignedCardinality naz{NaZ{}};

  SECTION("Positive finite vs signed and unsigned int") {
    CHECK(pos_five > -3);
    CHECK(pos_five > 0);
    CHECK(pos_five > 4);
    CHECK(pos_five < 6);
    CHECK(pos_five >= 5);
    CHECK(pos_five <= 5);
    CHECK(pos_five == 5);
    CHECK(pos_five > 4u);
    CHECK(pos_five == 5u);
  }
  SECTION("Negative finite vs signed and unsigned int") {
    CHECK(neg_three < 0);
    CHECK(neg_three < -2);
    CHECK(neg_three > -4);
    CHECK(neg_three == -3);
    CHECK_FALSE(neg_three == 3);
    // Negative SignedCardinality is strictly less than any unsigned.
    CHECK(neg_three < 0u);
    CHECK(neg_three < 5u);
  }
  SECTION("±ℵ_0 dominates / is dominated unconditionally") {
    CHECK(pos_inf > std::numeric_limits<int>::max());
    CHECK(pos_inf > 0);
    CHECK(pos_inf > -1);
    CHECK(pos_inf > 0u);
    CHECK_FALSE(pos_inf == 0);
    CHECK(neg_inf < std::numeric_limits<int>::min());
    CHECK(neg_inf < 0);
    CHECK(neg_inf < -1);
    CHECK_FALSE(neg_inf == 0);
  }
  SECTION("NaZ propagates as unordered: all relational ops false, == false") {
    CHECK_FALSE(naz < 0);
    CHECK_FALSE(naz <= 0);
    CHECK_FALSE(naz > 0);
    CHECK_FALSE(naz >= 0);
    CHECK_FALSE(naz == 0);
    CHECK(naz != 0);  // unordered semantics: NaZ is not equal to anything
  }
  SECTION("bool is std::integral on the SignedCardinality side too") {
    const SignedCardinality one = finite_signed_cardinality(1);
    const SignedCardinality zero = finite_signed_cardinality(0);
    CHECK(one == true);
    CHECK(zero == false);
    CHECK(one > false);
    CHECK(zero < true);
  }
  SECTION("Rhs-first direction synthesised by C++20 rewrite rules") {
    CHECK(-3 < pos_five);
    CHECK(0u < pos_five);
    CHECK(5 == pos_five);
    CHECK(0 > neg_three);
    CHECK(-3 == neg_three);
  }
}

TEST_CASE("Numbers: Cardinality homogeneous operator surface (#424)",
          "[numbers][cardinality][operators]") {
  const Cardinality three = finite_cardinality(3);
  const Cardinality four = finite_cardinality(4);
  const Cardinality inf = ℵ_0{};

  SECTION("operator+ wraps add() with ℵ_0 absorption") {
    CHECK((three + four) == finite_cardinality(7));
    CHECK((three + inf) == inf);
    CHECK((inf + inf) == inf);
  }
  SECTION("operator* wraps mul() with ℵ_0 absorption") {
    CHECK((three * four) == finite_cardinality(12));
    CHECK((three * inf) == inf);
    CHECK((inf * inf) == inf);
  }
  SECTION("operator<=> wraps compare(); finite < ℵ_0") {
    CHECK((three <=> four) == std::strong_ordering::less);
    CHECK((four <=> three) == std::strong_ordering::greater);
    CHECK((three <=> three) == std::strong_ordering::equal);
    CHECK((three <=> inf) == std::strong_ordering::less);
    CHECK((inf <=> three) == std::strong_ordering::greater);
    CHECK((inf <=> inf) == std::strong_ordering::equal);
  }
  SECTION("Four partial-order operators synthesised from <=>") {
    CHECK(three < four);
    CHECK(four > three);
    CHECK(three <= three);
    CHECK(three >= three);
    CHECK_FALSE(three > four);
    CHECK(three < inf);
  }
  SECTION("Explicit operator== covers cross-module visibility") {
    CHECK(three == finite_cardinality(3));
    CHECK_FALSE(three == four);
    CHECK(inf == Cardinality{ℵ_0{}});
    CHECK_FALSE(three == inf);
  }
  SECTION("Euclidean division and modulo (the ℕ-closed pair)") {
    const Cardinality seven = finite_cardinality(7);
    const Cardinality two = finite_cardinality(2);
    const Cardinality zero = finite_cardinality(0);
    // Textbook: 7 = 2*3 + 1, both quotient and remainder in ℕ.
    CHECK((seven / finite_cardinality(3)) == two);
    CHECK((seven % finite_cardinality(3)) == finite_cardinality(1));
    // Division-by-zero policy (totalised; ExtensionalCardinal convention).
    CHECK((seven / zero) == zero);
    CHECK((seven % zero) == seven);
    // ℵ_0 interactions: finite / ℵ_0 = 0; ℵ_0 / finite-non-zero = ℵ_0.
    CHECK((seven / inf) == zero);
    CHECK((inf / two) == inf);
  }
}

TEST_CASE("Numbers: variant carriers ↔ std::floating_point comparison (#428)",
          "[numbers][cardinality][order][floating]") {
  using std::numeric_limits;
  const auto pos_inf_d = numeric_limits<double>::infinity();
  const auto neg_inf_d = -numeric_limits<double>::infinity();
  const auto nan_d = numeric_limits<double>::quiet_NaN();

  SECTION("Cardinality vs double — finite, ℵ_0, ±inf, NaN, negatives") {
    const Cardinality five = finite_cardinality(5);
    const Cardinality inf = ℵ_0{};
    // Finite vs finite double.
    CHECK(five == 5.0);
    CHECK(five > 4.5);
    CHECK(five < 5.5);
    // Negative double — every Cardinality > -anything.
    CHECK(five > -1.0);
    CHECK(five > neg_inf_d);
    CHECK_FALSE(five == -5.0);
    // ℵ_0 dominates every finite double; equivalent to +inf.
    CHECK(inf > 1e308);
    CHECK(inf == pos_inf_d);
    CHECK(inf > neg_inf_d);
    // NaN propagates as unordered (all comparisons false; != is true).
    CHECK_FALSE(five < nan_d);
    CHECK_FALSE(five > nan_d);
    CHECK_FALSE(five == nan_d);
    CHECK(five != nan_d);
    // Rhs-first direction (synthesised by C++20 rewrite rules).
    CHECK(5.0 == five);
    CHECK(4.5 < five);
    CHECK(5.5 > five);
    CHECK(pos_inf_d == inf);
    CHECK(0.0 < inf);
    // Precision regime: above 2^53 a double can't distinguish consecutive
    // integers, so a naive @c static_cast<double>(lhs) @c == @c rhs path
    // would fake equality.  The integer-domain comparator must say no.
    const Cardinality two_to_53 = finite_cardinality(1ULL << 53);
    const Cardinality two_to_53_plus_1 = finite_cardinality((1ULL << 53) + 1);
    const double two_to_53_d = static_cast<double>(1ULL << 53);
    CHECK(two_to_53 == two_to_53_d);  // exact at 2^53
    CHECK_FALSE(two_to_53_plus_1 ==
                two_to_53_d);  // 2^53+1 ≠ 2^53 (no rounding-fake)
    CHECK(two_to_53_plus_1 > two_to_53_d);
    // Non-integer rhs at integer lhs.
    CHECK(five != 5.5);
    CHECK(five < 5.5);
    CHECK(five > 4.5);
  }
  SECTION("SignedCardinality vs double — full sign + sentinel matrix") {
    const auto pos_three = finite_signed_cardinality(3);
    const auto neg_three = finite_signed_cardinality(-3);
    const auto pos_inf_sc = SignedCardinality{PositiveInfinity{}};
    const auto neg_inf_sc = SignedCardinality{NegativeInfinity{}};
    const auto naz = SignedCardinality{NaZ{}};
    // Finite vs finite double.
    CHECK(pos_three == 3.0);
    CHECK(neg_three == -3.0);
    CHECK(pos_three > 2.5);
    CHECK(neg_three < -2.5);
    // ±ℵ_0 maps to ±inf for the comparison.
    CHECK(pos_inf_sc == pos_inf_d);
    CHECK(neg_inf_sc == neg_inf_d);
    CHECK(pos_inf_sc > 1e308);
    CHECK(neg_inf_sc < -1e308);
    // NaZ propagates as unordered; NaN rhs same.
    CHECK_FALSE(naz < 0.0);
    CHECK_FALSE(naz > 0.0);
    CHECK_FALSE(naz == 0.0);
    CHECK_FALSE(pos_three < nan_d);
    CHECK_FALSE(pos_three == nan_d);
    // Rhs-first direction (synthesised by C++20 rewrite rules).
    CHECK(3.0 == pos_three);
    CHECK(-3.0 == neg_three);
    CHECK(2.5 < pos_three);
    CHECK(pos_inf_d == pos_inf_sc);
    CHECK(neg_inf_d == neg_inf_sc);
    // Precision regime: same 2^53 rounding-fake guard as on the ℕ side,
    // applied to both signs.
    const auto two_to_53_z = finite_signed_cardinality(1LL << 53);
    const auto neg_two_to_53_z = finite_signed_cardinality(-(1LL << 53));
    const double two_to_53_d = static_cast<double>(1LL << 53);
    CHECK(two_to_53_z == two_to_53_d);
    CHECK(neg_two_to_53_z == -two_to_53_d);
    // Non-integer rhs at integer lhs.
    CHECK(pos_three != 3.25);
    CHECK(pos_three < 3.25);
    CHECK(neg_three > -3.25);
    // Mixed sign: any positive ≠ any negative.
    CHECK_FALSE(pos_three == -3.0);
    CHECK(pos_three > -3.0);
  }
  SECTION("Cross-variant ℕ ↔ ℤ comparison (the canonical ℕ ⊂ ℤ embedding)") {
    const Cardinality five_n = finite_cardinality(5);
    const Cardinality inf_n = ℵ_0{};
    const auto five_z = finite_signed_cardinality(5);
    const auto neg_three_z = finite_signed_cardinality(-3);
    const auto pos_inf_z = SignedCardinality{PositiveInfinity{}};
    const auto neg_inf_z = SignedCardinality{NegativeInfinity{}};
    const auto naz = SignedCardinality{NaZ{}};
    // Finite ℕ vs ℤ — the lift settles equality and ordering.
    CHECK(five_n == five_z);
    CHECK(five_n > neg_three_z);  // ℕ ≥ 0 > -3
    CHECK(five_n < finite_signed_cardinality(7));
    // ℵ_0 ↦ +ℵ_0 under the lift; equivalent to PositiveInfinity.
    CHECK(inf_n == pos_inf_z);
    CHECK_FALSE(inf_n > pos_inf_z);  // equivalent, not strictly greater
    CHECK(inf_n > five_z);
    // ℕ never below -ℵ_0.
    CHECK(five_n > neg_inf_z);
    // NaZ on the ℤ side propagates as unordered.
    CHECK_FALSE(five_n < naz);
    CHECK_FALSE(five_n > naz);
    CHECK_FALSE(five_n == naz);
    // Rhs-first direction (synthesised by C++20 rewrite rules).
    CHECK(five_z == five_n);
    CHECK(neg_three_z < five_n);
    CHECK(pos_inf_z == inf_n);
    CHECK(neg_inf_z < five_n);
  }
}

TEST_CASE(
    "Carrier strength-reduction: existential proof on (Cardinality, "
    "SignedCardinality) (slice of #362)",
    "[numbers][cardinality][carrier-lattice][order]") {
  // The existential proof of the carrier-promotion rule on the variant
  // pair: the load-bearing case for Paper 3's type-directed-collapse
  // story.  See docs/design/carrier-lattice.md for the design.
  // Mathematically:
  //   A ⊂ ℕ,  B ⊂ ℤ,  ℕ ↪ ℤ
  //   ⇒  A ∩ B ⊂ ℕ   (intersection tightens to the smaller carrier)
  //   ⇒  A ∪ B ⊂ ℤ   (union widens to the larger carrier)
  //
  // The ℤ side is built directly on the variant carrier @c
  // SignedCardinality (rather than @c var<ℤ> via the predicate-set
  // alias) because the @c ℤ alias-flip to the variant lives behind
  // #402 — see the @c FIXME breadcrumb.  Once #402 lands this test can
  // shift to the more idiomatic @c var<ℤ> form.
  using L = TernaryLogic;
  SECTION("Set<ℕ> & Set<ℤ> tightens to Set<ℕ>") {
    constexpr auto n = var<ℕ>;
    constexpr auto positive_n = Set{n % N | (n > 5u)};  // {6, 7, 8, …} ⊂ ℕ
    auto bounded_pred = [](const SignedCardinality& v) {
      // {…, -1, 0, …, 10} ⊂ ℤ
      return (v <= 10) ? L::True : L::False;
    };
    const Set<SignedCardinality, L, decltype(bounded_pred)> bounded_z{
        bounded_pred};
    const auto meet = positive_n & bounded_z;  // {6, 7, …, 10} ⊂ ℕ
    // Type-level proof: result carrier is ℕ (Cardinality), not ℤ.
    STATIC_CHECK(std::same_as<typename decltype(meet)::Domain, ℕ>);
    // Members in the natural-side window are in the meet.
    CHECK(meet(finite_cardinality(6)) == Ternary::True);
    CHECK(meet(finite_cardinality(10)) == Ternary::True);
    // Members of A but outside the ℤ window are excluded.
    CHECK(meet(finite_cardinality(11)) == Ternary::False);
    // Members below the ℕ window are excluded.
    CHECK(meet(finite_cardinality(5)) == Ternary::False);
  }
  SECTION("Set<ℕ> | Set<ℤ> widens to Set<ℤ> ({1} ∪ {-1} ⊂ ℤ)") {
    constexpr auto n = var<ℕ>;
    constexpr auto one_n = Set{n % N | (n == 1u)};  // {1} ⊂ ℕ
    auto neg_one_pred = [](const SignedCardinality& v) {
      return (v == -1) ? L::True : L::False;
    };
    const Set<SignedCardinality, L, decltype(neg_one_pred)> neg_one_z{
        neg_one_pred};                         // {-1} ⊂ ℤ
    const auto union_set = one_n | neg_one_z;  // {1, -1} ⊂ ℤ
    // Type-level proof: result carrier widens to ℤ (SignedCardinality).
    STATIC_CHECK(
        std::same_as<typename decltype(union_set)::Domain, SignedCardinality>);
    // Both 1 (from ℕ) and -1 (from ℤ) are in the union.
    CHECK(union_set(finite_signed_cardinality(1)) == Ternary::True);
    CHECK(union_set(finite_signed_cardinality(-1)) == Ternary::True);
    // Other values are excluded.
    CHECK(union_set(finite_signed_cardinality(0)) == Ternary::False);
    CHECK(union_set(finite_signed_cardinality(2)) == Ternary::False);
    CHECK(union_set(finite_signed_cardinality(-2)) == Ternary::False);
  }
  SECTION("Symmetric direction: Set<ℤ> & Set<ℕ> still tightens to Set<ℕ>") {
    constexpr auto n = var<ℕ>;
    auto bounded_pred = [](const SignedCardinality& v) {
      return (v >= -3) ? L::True : L::False;  // {-3, -2, …} ⊂ ℤ
    };
    const Set<SignedCardinality, L, decltype(bounded_pred)> bounded_z{
        bounded_pred};
    constexpr auto small_n = Set{n % N | (n < 5u)};  // {0, …, 4} ⊂ ℕ
    const auto meet = bounded_z & small_n;           // {0, …, 4} ⊂ ℕ
    STATIC_CHECK(std::same_as<typename decltype(meet)::Domain, ℕ>);
    CHECK(meet(finite_cardinality(0)) == Ternary::True);
    CHECK(meet(finite_cardinality(4)) == Ternary::True);
    CHECK(meet(finite_cardinality(5)) == Ternary::False);
  }
}

TEST_CASE(
    "Elementwise carrier-promotion: closure-forcing unary − (slice of #432)",
    "[numbers][cardinality][carrier-lattice][elementwise]") {
  // Unary minus IS well-defined on ℕ as a function ℕ → ℤ; ℕ simply
  // isn't closed under it.  The operator's existence with a wider
  // return type is the Grothendieck construction in code.  See
  // docs/design/carrier-lattice.md, "The elementwise dual" section.
  SECTION("Type-level: -Cardinality returns SignedCardinality") {
    constexpr auto five_n = finite_cardinality(5);
    STATIC_CHECK(std::same_as<decltype(-five_n), SignedCardinality>);
  }
  SECTION("Negation of a positive finite natural lands in the negative ℤ") {
    const auto neg_five = -finite_cardinality(5);
    REQUIRE(std::holds_alternative<SignedExtensionalCardinal<>>(neg_five));
    const auto& z = std::get<SignedExtensionalCardinal<>>(neg_five);
    CHECK(z.negative);
    CHECK(z.magnitude == ExtensionalCardinal<>{5});
    // Witness: -n equals the corresponding negative finite_signed_cardinality.
    CHECK(neg_five == finite_signed_cardinality(-5));
  }
  SECTION("Negation of 0 is canonical +0 (no spurious -0)") {
    const auto neg_zero = -finite_cardinality(0);
    REQUIRE(std::holds_alternative<SignedExtensionalCardinal<>>(neg_zero));
    const auto& z = std::get<SignedExtensionalCardinal<>>(neg_zero);
    CHECK_FALSE(z.negative);  // canonical zero
    CHECK(z.magnitude == ExtensionalCardinal<>{});
    CHECK(neg_zero == finite_signed_cardinality(0));
  }
  SECTION("Negation of ℵ_0 maps to -ℵ_0 (NegativeInfinity sentinel)") {
    const auto neg_inf = -Cardinality{ℵ_0{}};
    CHECK(std::holds_alternative<NegativeInfinity>(neg_inf));
    CHECK(neg_inf == SignedCardinality{NegativeInfinity{}});
  }
}

TEST_CASE(
    "Elementwise carrier-promotion: closure-forcing binary − on ℕ × ℕ → ℤ "
    "(slice of #432)",
    "[numbers][cardinality][carrier-lattice][elementwise]") {
  // Subtraction IS well-defined on ℕ × ℕ → ℤ; ℕ isn't closed under it.
  // The cross-carrier overload makes that explicit at the type level.
  SECTION("Type-level: Cardinality - Cardinality returns SignedCardinality") {
    constexpr auto five_n = finite_cardinality(5);
    constexpr auto three_n = finite_cardinality(3);
    STATIC_CHECK(std::same_as<decltype(five_n - three_n), SignedCardinality>);
  }
  SECTION("5 - 3 = 2 (positive in ℕ; result still typed as ℤ)") {
    const auto diff = finite_cardinality(5) - finite_cardinality(3);
    CHECK(diff == finite_signed_cardinality(2));
  }
  SECTION("3 - 5 = -2 (closure forces ℤ; ℕ would reject)") {
    const auto diff = finite_cardinality(3) - finite_cardinality(5);
    CHECK(diff == finite_signed_cardinality(-2));
  }
  SECTION("n - n = 0 for any n") {
    const auto diff = finite_cardinality(7) - finite_cardinality(7);
    CHECK(diff == finite_signed_cardinality(0));
  }
}

TEST_CASE(
    "Elementwise carrier-promotion: closed cross-carrier + and * on (ℕ, ℤ) "
    "(slice of #432)",
    "[numbers][cardinality][carrier-lattice][elementwise]") {
  // Closed cross-carrier ops: both operands have the operator and the
  // result lives in the larger carrier.  Carrier-promotion in the
  // math-correct direction (signed wins).
  SECTION("Type-level: Cardinality + SignedCardinality returns ℤ") {
    constexpr auto n = finite_cardinality(5);
    constexpr auto z = finite_signed_cardinality(-3);
    STATIC_CHECK(std::same_as<decltype(n + z), SignedCardinality>);
    STATIC_CHECK(std::same_as<decltype(z + n), SignedCardinality>);
    STATIC_CHECK(std::same_as<decltype(n * z), SignedCardinality>);
    STATIC_CHECK(std::same_as<decltype(z * n), SignedCardinality>);
    STATIC_CHECK(std::same_as<decltype(n - z), SignedCardinality>);
    STATIC_CHECK(std::same_as<decltype(z - n), SignedCardinality>);
  }
  SECTION("5 (ℕ) + (-3) (ℤ) = 2 (ℤ)") {
    const auto sum = finite_cardinality(5) + finite_signed_cardinality(-3);
    CHECK(sum == finite_signed_cardinality(2));
  }
  SECTION("(-3) (ℤ) + 5 (ℕ) = 2 (ℤ); commutativity") {
    const auto sum = finite_signed_cardinality(-3) + finite_cardinality(5);
    CHECK(sum == finite_signed_cardinality(2));
  }
  SECTION("5 (ℕ) * (-3) (ℤ) = -15 (ℤ)") {
    const auto product = finite_cardinality(5) * finite_signed_cardinality(-3);
    CHECK(product == finite_signed_cardinality(-15));
  }
  SECTION("(-3) (ℤ) * 5 (ℕ) = -15 (ℤ); symmetric direction (commutativity)") {
    const auto product = finite_signed_cardinality(-3) * finite_cardinality(5);
    CHECK(product == finite_signed_cardinality(-15));
  }
  SECTION("Cross-carrier subtraction: 5 (ℕ) - (-3) (ℤ) = 8 (ℤ)") {
    const auto diff = finite_cardinality(5) - finite_signed_cardinality(-3);
    CHECK(diff == finite_signed_cardinality(8));
  }
  SECTION("Cross-carrier subtraction: (-3) (ℤ) - 5 (ℕ) = -8 (ℤ)") {
    const auto diff = finite_signed_cardinality(-3) - finite_cardinality(5);
    CHECK(diff == finite_signed_cardinality(-8));
  }
  // Coverage discipline: exercise the @c ℵ_0 branch of @c
  // lift_cardinality_to_signed, which the cross-carrier ops route
  // through.  Without these the lift's @c std::holds_alternative<ℵ_0>
  // path goes uncovered (caught in PR #433's first review pass).
  SECTION("ℵ_0 (ℕ) + 5 (ℤ) = +ℵ_0 (ℤ); lift's ℵ_0 branch exercised") {
    const auto sum = Cardinality{ℵ_0{}} + finite_signed_cardinality(5);
    CHECK(sum == SignedCardinality{PositiveInfinity{}});
  }
  SECTION("ℵ_0 (ℕ) - 5 (ℤ) = +ℵ_0 (ℤ)") {
    const auto diff = Cardinality{ℵ_0{}} - finite_signed_cardinality(5);
    CHECK(diff == SignedCardinality{PositiveInfinity{}});
  }
  SECTION("ℵ_0 (ℕ) * 5 (ℤ) = +ℵ_0 (ℤ)") {
    const auto product = Cardinality{ℵ_0{}} * finite_signed_cardinality(5);
    CHECK(product == SignedCardinality{PositiveInfinity{}});
  }
  SECTION("Binary closure-forcing on ℵ_0 inputs: ℵ_0 - 5 = +ℵ_0") {
    const auto diff = Cardinality{ℵ_0{}} - finite_cardinality(5);
    CHECK(diff == SignedCardinality{PositiveInfinity{}});
  }
  SECTION("Binary closure-forcing on dual ℵ_0 inputs: ℵ_0 - ℵ_0 = NaZ") {
    // (+ℵ_0) - (+ℵ_0) is the indeterminate form; SignedCardinality's
    // saturation semantics propagate it as NaZ (see PR #396).
    const auto diff = Cardinality{ℵ_0{}} - Cardinality{ℵ_0{}};
    CHECK(std::holds_alternative<NaZ>(diff));
  }
  // 𝔹 ↪ ℕ ↪ ℤ chain composition.  Subtle but important: a @c bool
  // value does @b not implicitly convert to @c Cardinality in a single
  // step — the conversion would have to go via @c
  // ExtensionalCardinal<>{bool} (the integral ctor template, PR #412)
  // and then through @c std::variant's alternative-selection ctor, and
  // C++ allows at most one user-defined conversion in an implicit
  // conversion sequence.  So call sites that want to lift a @c bool
  // into the variant ℕ-proxy spell @c Cardinality{b} explicitly; the
  // @b chain composition the cross-carrier overloads provide is from
  // @c Cardinality forward, not from @c bool.  These SECTIONs witness
  // that explicit construction + the cross-carrier dispatch.
  SECTION("Cardinality{bool} + ℕ → ℕ: explicit lift + same-carrier sum") {
    const auto sum = Cardinality{true} + finite_cardinality(5);
    CHECK(sum == finite_cardinality(6));
  }
  SECTION("Cardinality{bool} + ℤ → ℤ: explicit lift + cross-carrier sum") {
    const auto sum = Cardinality{true} + finite_signed_cardinality(-3);
    STATIC_CHECK(std::same_as<decltype(sum), const SignedCardinality>);
    CHECK(sum == finite_signed_cardinality(-2));
  }
  SECTION("Cardinality{bool} - ℕ → ℤ: explicit lift + closure-forcing −") {
    const auto diff = Cardinality{false} - finite_cardinality(3);
    STATIC_CHECK(std::same_as<decltype(diff), const SignedCardinality>);
    CHECK(diff == finite_signed_cardinality(-3));
  }
}

TEST_CASE(
    "Elementwise carrier-promotion: abs as the retraction ℤ → ℕ "
    "(closes #436)",
    "[numbers][cardinality][carrier-lattice][elementwise]") {
  // abs is the downward complement of the closure-forcing direction:
  // where unary − widens ℕ → ℤ along the Grothendieck embedding,
  // abs narrows ℤ → ℕ along the same embedding's retraction.  The
  // result type announces the non-negativity guarantee at the type
  // level.  See docs/design/carrier-lattice.md.
  SECTION("Type-level: abs(SignedCardinality) returns Cardinality") {
    const auto z = finite_signed_cardinality(-5);
    STATIC_CHECK(std::same_as<decltype(abs(z)), Cardinality>);
  }
  SECTION("Positive finite: abs preserves magnitude") {
    CHECK(abs(finite_signed_cardinality(5)) == finite_cardinality(5));
  }
  SECTION("Negative finite: abs folds the sign") {
    CHECK(abs(finite_signed_cardinality(-5)) == finite_cardinality(5));
  }
  SECTION("Canonical zero: abs(0) == 0") {
    CHECK(abs(finite_signed_cardinality(0)) == finite_cardinality(0));
  }
  SECTION("Retraction identity: abs(positive_signed_with_magnitude_n) == n") {
    // The retraction-on-the-non-negative-fragment claim:
    //   abs ∘ embed_ℕ_ℤ = id_ℕ
    // witnessed without reaching into the detail::lift helper — we
    // build a non-negative SignedCardinality directly with magnitude
    // 7 (the embedding's image of finite_cardinality(7)) and verify
    // abs returns to the original.
    const auto z = finite_signed_cardinality(7);  // ≡ embed_ℕ_ℤ(7)
    CHECK(abs(z) == finite_cardinality(7));
  }
  SECTION("PositiveInfinity → ℵ_0") {
    CHECK(abs(SignedCardinality{PositiveInfinity{}}) == Cardinality{ℵ_0{}});
  }
  SECTION("NegativeInfinity → ℵ_0 (sign folded; magnitude escalates)") {
    CHECK(abs(SignedCardinality{NegativeInfinity{}}) == Cardinality{ℵ_0{}});
  }
  SECTION("NaZ → ℵ_0 (saturating fallback for indeterminate forms)") {
    CHECK(abs(SignedCardinality{NaZ{}}) == Cardinality{ℵ_0{}});
  }
}
