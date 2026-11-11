#include <catch2/catch_test_macros.hpp>
#include <climits>  // INT_MAX: the exhibit's edge
#include <cstdint>
#include <cstdlib>  // For std::abs in the partial-commuting-square test.
#include <limits>  // For std::numeric_limits in the abs honest-extension tests.
#include <utility>  // std::move

import dedekind.category;
import dedekind.algebra; // AdditiveInverse + IsOrderedAdditiveGroup (#959)
import dedekind.morphologies;
import dedekind.numbers;

using namespace dedekind::category;
using namespace dedekind::algebra;
using namespace dedekind::morphologies;
using namespace dedekind::numbers;
using namespace dedekind::sets;

TEST_CASE(
    "Integer: abs_int_unsigned is the type-honest machine-layer absolute "
    "value — int → unsigned, total at INT_MIN where std::abs is UB",
    "[numbers][integer][carrier-lattice][abs][honest-extension]") {
  // abs_int_unsigned : int → unsigned is the type-honest sibling of
  // std::abs : int → int.  The unsigned codomain announces
  // non-negativity at the type level; the implementation is total
  // (works for INT_MIN where std::abs is UB).
  constexpr int int_min = std::numeric_limits<int>::min();
  // |INT_MIN| computed via unsigned modular arithmetic — well-defined
  // for any int width / two's-complement ABI.
  constexpr unsigned int_min_magnitude = 0u - static_cast<unsigned>(int_min);
  // abs_int_unsigned(INT_MIN) = |INT_MIN| — total, no UB.
  CHECK(abs_int_unsigned(int_min) == int_min_magnitude);
  // Sign-folding: distinct +n / -n map to the same unsigned magnitude
  // (so the function is NOT injective).
  CHECK(abs_int_unsigned(3) == 3u);
  CHECK(abs_int_unsigned(-3) == 3u);
  CHECK(abs_int_unsigned(0) == 0u);

  // Partial commuting square: on the non-INT_MIN fragment of int, the
  // type-honest abs_int_unsigned agrees with std::abs coerced to unsigned.
  // (At INT_MIN the std::abs side would be UB, so the comparison is
  // omitted there — the previous CHECK already pinned that case.)
  for (const int v :
       {1, -1, 42, -42, 1000, -1000, std::numeric_limits<int>::max()}) {
    CHECK(abs_int_unsigned(v) == static_cast<unsigned>(std::abs(v)));
  }
}

// ===========================================================================
// (3.5) embed_𝔹_ℕ_ — runtime witness for the variant-layer canonical
//       mono 𝔹 ↪ ℕ landing on the @c Numeral carrier.  The
//       compile-time witnesses live in @c :natural; this exercises
//       the same equalities at runtime so the arrow's lambda body
//       shows up as covered for codecov.
// ===========================================================================

TEST_CASE(
    "carrier-lattice: embed_𝔹_ℕ_ maps true to 1 and false to 0 (𝔹 ↪ ℕ via "
    "the Numeral carrier)",
    "[carrier-lattice][boolean][cardinality][embed]") {
  CHECK(embed_𝔹_ℕ_(true) == Numeral{1u});
  CHECK(embed_𝔹_ℕ_(false) == Numeral{0u});
}

TEST_CASE(
    "carrier-lattice: embed_𝔹_ℕ (set-level lift) on Singleton<true> lands at "
    "Singleton<1> in ℕ",
    "[carrier-lattice][boolean][cardinality][embed][image]") {
  // Set-level lift exercises the runtime dispatch through
  // dedekind::sets::image(arrow, Singleton) — covers the
  // forwarding-reference body for codecov.
  constexpr Singleton<bool, Boole> s_true{true};
  const auto image_set = embed_𝔹_ℕ(s_true);
  CHECK(origin(image_set) == Numeral{1u});

  constexpr Singleton<bool, Boole> s_false{false};
  const auto image_set_false = embed_𝔹_ℕ(s_false);
  CHECK(origin(image_set_false) == Numeral{0u});
}

// ===========================================================================
// (4) Embedding chain: 𝕂3 → sint → ℤ (signed counterpart to 𝔹 → uint → ℕ)
// ===========================================================================

TEST_CASE("sint: 𝕂3 → sint → ℤ chain via embed_𝕂3_ℤ_ + lift",
          "[numbers][sint][lift][embedding-chain]") {
  // The two factorisations of 𝕂3 → ℤ, directly and through the machine
  // integers -1 / 0 / 1, agree on each Kleene truth value.
  CHECK(embed_𝕂3_ℤ_(Ternary::False) == embed_sint_ℤ(-1));
  CHECK(embed_𝕂3_ℤ_(Ternary::Unknown) == embed_sint_ℤ(0));
  CHECK(embed_𝕂3_ℤ_(Ternary::True) == embed_sint_ℤ(1));
}

TEST_CASE("Integer: additive inverse -x is antitone on ℤ (#908/#959)",
          "[numbers][integer][algebra][antitone]") {
  // ℤ is an ordered additive group, so the unary additive inverse -x is
  // ORDER-REVERSING: the algebraic dog-food of the antitone reification, the
  // same shape as the logic NegationArrow's negation on Omega.
  constexpr AdditiveInverse<Integer> neg{};
  const Integer two{2};
  const Integer three{3};

  CHECK(neg(two) == Integer{-2});
  CHECK(neg(three) == Integer{-3});

  // Order-reversal: 2 <= 3, so -3 <= -2.
  CHECK(two <= three);
  CHECK(neg(three) <= neg(two));

  // Self-adjoint antitone Galois connection: a <= -b iff b <= -a.  The
  // concept witnesses are pinned at the definition site,
  // algebra/ordered_algebra.cppm.
  const Integer neg_five{-5};
  CHECK((neg_five <= neg(two)) == (two <= neg(neg_five)));
  CHECK((two <= neg(three)) == (three <= neg(two)));
}

// ===========================================================================
// (5) The exhibit: int against ℤ
// ===========================================================================
//
// int carries the ring's operators and not the ring: signed overflow is
// undefined, so constant evaluation refuses it.  The line
//
//   static_assert(INT_MAX + 1 > INT_MAX);  // ill-formed: not a constant
//
// does not compile, and no static_assert can say so, since a refusal is not
// a value.  The same term over ℤ is exact, and so is its law.
namespace int_vs_integer {
constexpr Integer int_max{INT_MAX};
static_assert(int_max + Integer{1} > int_max,
              "INT_MAX + 1 in ℤ: the successor, above the word");
static_assert(int_max + Integer{1} ==
                  Integer{Numeral{static_cast<unsigned>(INT_MAX) + 1u}},
              "where an unsigned word holds the value, ℤ agrees with it");
static_assert((int_max + Integer{1}) - Integer{1} == int_max &&
                  Integer{INT_MIN} - Integer{1} < Integer{INT_MIN},
              "the group laws hold at both ends of int, where int has none");
static_assert(int_max * int_max * int_max * int_max * int_max >
                  Integer{std::numeric_limits<long long>::max()},
              "and past every word: (2^31 - 1)^5 exceeds LLONG_MAX");
}  // namespace int_vs_integer

TEST_CASE("Integer: int overflows, ℤ does not", "[numbers][integer][exhibit]") {
  const Integer int_max{INT_MAX};
  CHECK(int_max + Integer{1} > int_max);
  CHECK(rank(Integer{INT_MIN}) == Numeral{0u - static_cast<unsigned>(INT_MIN)});
}

TEST_CASE("numbers:integer: a moved-from integer is a value in normal form",
          "[numbers][integer][normal-form]") {
  // -2^64: the magnitude's low limb is zero, so moving the limbs out could
  // leave a zero magnitude beside the sign.  The accessors read the normal
  // form, and the additive identity holds on the source.
  Integer a(Numeral{std::numeric_limits<std::uint64_t>::max()} + Numeral{1u},
            true);
  const Integer b = std::move(a);
  CHECK(b.negative());
  CHECK(a + Integer{} == a);
  CHECK((a == Integer{} || a == b));
}

TEST_CASE("numbers:integer: a borrow runs through maximum-value limbs",
          "[numbers][integer][limbs]") {
  // 2^128 - 1: subtracting one from the limbs [0, 0, 1] borrows through
  // both zero limbs, each becoming the maximum limb with a borrow in.
  const Numeral w{std::numeric_limits<std::uint64_t>::max()};
  const Integer two_128{(w + Numeral{1u}) * (w + Numeral{1u})};
  CHECK(two_128 + Integer{-1} == Integer{w * (w + Numeral{1u}) + w});
  CHECK((two_128 + Integer{-1}).magnitude().limb_count() == 2);
  // 2^128 - (2^128 - 1): each subtrahend limb is the maximum and receives a
  // borrow, so the amount taken is 2^64, wider than a limb.
  CHECK(two_128 + -Integer{w * (w + Numeral{1u}) + w} == Integer{1});
}

TEST_CASE(
    "numbers:integer: division rounds toward zero, the remainder has "
    "the dividend's sign, past the word",
    "[numbers][integer][division]") {
  // The witnesses are static_asserts at the definition site; these runtime
  // checks make the same facts visible to coverage, and read (x / y) * y +
  // x % y == x on values int cannot hold.
  const Numeral w{std::numeric_limits<std::uint64_t>::max()};
  const Integer two_64{w + Numeral{1u}};
  const Integer x = two_64 * Integer{-3} + Integer{-1};  // -(3·2^64 + 1)
  const Integer y{3};
  CHECK(x / y == -two_64);
  CHECK(x % y == Integer{-1});
  CHECK((x / y) * y + x % y == x);
  CHECK((-x) / (-y) == -two_64);  // positive by negative: toward zero
  CHECK((-x) % (-y) == Integer{1});
  CHECK(Integer{-7} / Integer{2} == Integer{-3});
  CHECK(Integer{-7} % Integer{2} == Integer{-1});
  CHECK(gcd(Integer{-12}, Integer{18}) == Numeral{6u});
  CHECK(gcd(x, two_64 * Integer{-6}) == Numeral{1u});
}
