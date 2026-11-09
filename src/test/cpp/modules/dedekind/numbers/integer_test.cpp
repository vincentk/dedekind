#include <catch2/catch_test_macros.hpp>
#include <climits>  // INT_MAX: the exhibit's edge
#include <cstdlib>  // For std::abs in the partial-commuting-square test.
#include <limits>  // For std::numeric_limits in the abs honest-extension tests.

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

  STATIC_CHECK(IsAntiMonotone<AdditiveInverse<Integer>>);
  STATIC_CHECK(IsVariant<AdditiveInverse<Integer>>);

  // Self-adjoint antitone Galois connection: a <= -b iff b <= -a.
  STATIC_CHECK(
      IsGaloisConnection<AdditiveInverse<Integer>, AdditiveInverse<Integer>>);
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
namespace int_vs_ℤ {
constexpr Integer int_max{INT_MAX};
static_assert(int_max + Integer{1} > int_max,
              "INT_MAX + 1 in ℤ: the successor, above the word");
static_assert(int_max + Integer{1} ==
                  Integer{static_cast<long long>(INT_MAX) + 1LL},
              "where a wider word holds the value, ℤ agrees with it");
static_assert((int_max + Integer{1}) - Integer{1} == int_max &&
                  Integer{INT_MIN} - Integer{1} < Integer{INT_MIN},
              "the group laws hold at both ends of int, where int has none");
static_assert(int_max * int_max * int_max * int_max * int_max >
                  Integer{std::numeric_limits<long long>::max()},
              "and past every word: (2^31 - 1)^5 exceeds 2^64");
}  // namespace int_vs_ℤ

TEST_CASE("Integer: int overflows, ℤ does not", "[numbers][integer][exhibit]") {
  const Integer int_max{INT_MAX};
  CHECK(int_max + Integer{1} > int_max);
  CHECK(rank(Integer{INT_MIN}) == Numeral{0u - static_cast<unsigned>(INT_MIN)});
}
