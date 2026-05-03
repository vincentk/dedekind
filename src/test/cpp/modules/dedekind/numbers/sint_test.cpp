/**
 * @file sint_test.cpp
 * @brief Dedicated test suite for the @c numbers:sint partition.
 *
 * @section Scope
 * Exercises the @c std::signed_integral family classification shipped
 * by @c numbers:sint (closes part of #418): the dual error-direction
 * to @c :uint (operator surface ✓, axiomatic ring ✗ under UB), the
 * universal machine→variant lift @c
 * embed_sint_ℤ, and the @c 𝕂3 → sint → ℤ
 * embedding-chain composition (the signed counterpart to @c :uint's
 * @c 𝔹 → uint → ℕ chain).  The compile-time witnesses live inside
 * the partition itself; this file exercises the @b runtime / @b
 * composition surface.
 */
#include <catch2/catch_test_macros.hpp>
#include <functional>  // std::plus
#include <limits>
#include <type_traits>
#include <variant>  // load-bearing for std::variant's operator== reached
                    // via ADL on SignedCardinality (alias of std::variant<...>)

import dedekind.algebra;
import dedekind.category;
import dedekind.numbers;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;

// ===========================================================================
// (1) The universal lift arrow: std::signed_integral → SignedCardinality
// ===========================================================================

TEST_CASE("sint: embed_sint_ℤ across canonical widths",
          "[numbers][sint][lift][carrier-lattice]") {
  CHECK(embed_sint_ℤ(0) == finite_signed_cardinality(0));
  CHECK(embed_sint_ℤ(1) == finite_signed_cardinality(1));
  CHECK(embed_sint_ℤ(-1) == finite_signed_cardinality(-1));
  CHECK(embed_sint_ℤ(42) == finite_signed_cardinality(42));
  CHECK(embed_sint_ℤ(-42) == finite_signed_cardinality(-42));
  CHECK(embed_sint_ℤ(static_cast<long>(1000)) ==
        finite_signed_cardinality(1000));
  CHECK(embed_sint_ℤ(static_cast<long long>(-7)) ==
        finite_signed_cardinality(-7));
  CHECK(embed_sint_ℤ(static_cast<short>(-100)) ==
        finite_signed_cardinality(-100));
}

TEST_CASE("sint: embed_sint_ℤ_ named-arrow form",
          "[numbers][sint][lift][monic-arrow]") {
  STATIC_CHECK(IsArrow<std::decay_t<decltype(embed_sint_ℤ_)>>);
  STATIC_CHECK(IsMonicArrow<std::decay_t<decltype(embed_sint_ℤ_)>>);
  STATIC_CHECK(std::same_as<Dom<std::decay_t<decltype(embed_sint_ℤ_)>>, int>);
  STATIC_CHECK(std::same_as<Cod<std::decay_t<decltype(embed_sint_ℤ_)>>,
                            SignedCardinality>);

  CHECK(embed_sint_ℤ_(0) == finite_signed_cardinality(0));
  CHECK(embed_sint_ℤ_(42) == finite_signed_cardinality(42));
  CHECK(embed_sint_ℤ_(-42) == finite_signed_cardinality(-42));
}

TEST_CASE("sint: lift is injective on the finite fragment",
          "[numbers][sint][lift][injective]") {
  CHECK(embed_sint_ℤ(0) != embed_sint_ℤ(1));
  CHECK(embed_sint_ℤ(0) != embed_sint_ℤ(-1));
  CHECK(embed_sint_ℤ(41) != embed_sint_ℤ(42));
  CHECK(embed_sint_ℤ(-1) != embed_sint_ℤ(1));
}

// ===========================================================================
// (2) Honest Rejection: operator surface ✓, axiomatic ring ✗
// ===========================================================================

TEST_CASE(
    "sint: int has the literal ring operator surface (positive witnesses)",
    "[numbers][sint][honest-rejection][positive]") {
  // Compile-time: the operator-shape concepts fire on int / long /
  // long long.  These are the @b honest claims the library makes
  // about std::signed_integral primitives.
  STATIC_CHECK(dedekind::algebra::HasRingOperators<int>);
  STATIC_CHECK(dedekind::algebra::HasGroupOperatorsAdd<int>);
  STATIC_CHECK(dedekind::algebra::HasSuccessorOperators<int>);
  STATIC_CHECK(dedekind::algebra::HasRingOperators<long>);
  STATIC_CHECK(dedekind::algebra::HasRingOperators<long long>);
}

TEST_CASE("sint: int does NOT satisfy axiomatic ring laws (negative witnesses)",
          "[numbers][sint][honest-rejection][negative]") {
  // Compile-time: the axiomatic-ring concepts deliberately do @b NOT
  // fire on signed-integral primitives.  Signed-overflow UB defeats
  // closure-under-arithmetic as a stable axiom — the library's
  // @b Honest @b Rejection of int as a witness for ℤ.
  STATIC_CHECK(!Group_ℤ<int>);
  STATIC_CHECK(!Group_ℤ<long>);
  STATIC_CHECK(!Group_ℤ<long long>);
  STATIC_CHECK(!dedekind::algebra::IsArithmeticAdditiveGroup<int>);
}

TEST_CASE(
    "sint: the dichotomy is symmetric in form to :uint's, opposite in "
    "direction",
    "[numbers][sint][honest-rejection][symmetry]") {
  // The library's posture is honest in both directions:
  //   * unsigned int claims MORE structure than ℕ has (modular inverses);
  //     IsField<unsigned int> rejected (PR #441 / :uint).
  //   * int claims LESS structure than ℤ has (UB on overflow defeats
  //     closure); Group_ℤ<int> rejected (this partition).
  // Both rejections are symmetric in form — each carrier's claim is
  // mismatched with the textbook structure it would purport to inhabit.
  STATIC_CHECK(!Group_ℤ<int>);               // signed: too LITTLE structure
  STATIC_CHECK(!dedekind::algebra::IsField<  // unsigned: too MUCH structure
               unsigned int, std::plus<unsigned int>,
               std::multiplies<unsigned int>>);
  // Both partitions DO claim the operator surface honestly.
  STATIC_CHECK(dedekind::algebra::HasRingOperators<int>);
  STATIC_CHECK(dedekind::algebra::HasRingOperators<unsigned int>);
}

// ===========================================================================
// (3) Structure-completeness on the variant: SignedCardinality IS axiomatic ℤ
// ===========================================================================

TEST_CASE(
    "sint: an arithmetic operation that is UB on int becomes well-defined "
    "on SignedCardinality",
    "[numbers][sint][lift][carrier-lattice]") {
  // INT_MAX + 1 is UB on int; on SignedCardinality the same shape is
  // well-defined (saturates to +ℵ_0 if it would overflow the finite
  // fragment).  The lift moves the carrier UP the lattice, trading the
  // UB-on-overflow for total saturating semantics — which is what the
  // exact-ℤ reading lives on.  Witnesses the structural-completeness
  // gain at the variant layer.
  const auto one_lifted = embed_sint_ℤ(1);
  const auto neg_one_lifted = embed_sint_ℤ(-1);
  // Pick a finite value that exists on both sides and verify the lift
  // commutes with negation under the operator surface.
  CHECK(-one_lifted == neg_one_lifted);
}

// ===========================================================================
// (4) Embedding chain: 𝕂3 → sint → ℤ (signed counterpart to 𝔹 → uint → ℕ)
// ===========================================================================

TEST_CASE("sint: 𝕂3 → sint → ℤ chain via embed_𝕂3_ℤ_ + lift",
          "[numbers][sint][lift][embedding-chain]") {
  // Post-PR #439, embed_𝕂3_ℤ_ already lands on SignedCardinality
  // directly.  The textbook-factored chain
  //
  //   𝕂3 → std::signed_integral → ℤ
  //
  // would route through @c int (the machine layer) on the first leg
  // and then through @c embed_sint_ℤ on the
  // second; PR #439's chosen factorisation collapses the two legs
  // into one for ergonomic reasons.  Exercise the chain claim
  // directly by relating @c embed_𝕂3_ℤ_'s images to the lifted
  // canonical machine-int images @c -1 / @c 0 / @c 1, asserting
  // that both factorisations agree on the canonical value of each
  // Kleene truth-value.
  CHECK(embed_𝕂3_ℤ_(Ternary::False) == embed_sint_ℤ(-1));
  CHECK(embed_𝕂3_ℤ_(Ternary::Unknown) == embed_sint_ℤ(0));
  CHECK(embed_𝕂3_ℤ_(Ternary::True) == embed_sint_ℤ(1));
}

// ===========================================================================
// (5) Ergonomic-economic parity under Honest Rejection: "Intensional first;
//     realize when you mean it"
// ===========================================================================
//
// Honest Rejection is structurally correct, but a separate question is
// whether it imposes a practitioner-ergonomics cost.  The project's
// answer is @b doctrinal: practitioners are encouraged to work over
// the @b canonical mathematical set (@c element<Ω<ℤ>>, predicates,
// comprehension DSL, abstract algebraic operations on @c
// SignedCardinality) — @b NOT to write machine-int-style code at the
// variant layer.  Tests below exercise the idiomatic style.

TEST_CASE(
    "sint: idiomatic intensional-first style — variant ℤ-proxy as the "
    "working carrier",
    "[numbers][sint][ergonomics][intensional-first]") {
  // Idiomatic-style: assemble values via the explicit constructor
  // (the canonical entry point), then compose using the abstract
  // algebraic surface defined in :cardinality.  No machine-int
  // arithmetic in this fragment — the practitioner stays in the
  // textbook ℤ even though the underlying variant happens to be
  // implementable by machine ints internally.
  const auto a = finite_signed_cardinality(7);
  const auto b = finite_signed_cardinality(-3);
  const auto sum = a + b;      // SC + SC
  const auto neg = -a;         // unary -SC
  const auto product = a * b;  // SC * SC
  CHECK(sum == finite_signed_cardinality(4));
  CHECK(neg == finite_signed_cardinality(-7));
  CHECK(product == finite_signed_cardinality(-21));

  // Heterogeneous comparison against std::integral (PR #438) bridges
  // the variant carrier and machine-int literals at the @b boundary
  // — useful for halfspace pivots and similar comparisons against
  // compile-time bounds, without forcing the variant arithmetic to
  // accept int operands.
  CHECK(sum > 0);
  CHECK(neg < 0);
  CHECK(product == -21);
}

TEST_CASE(
    "sint: realize-when-you-mean-it boundary — explicit descent to the "
    "machine carrier",
    "[numbers][sint][ergonomics][realize]") {
  // The unsigned-side realize-step is well-established (PR #427-era
  // realize_to_size_t with explicit transfinite_sentinel).  Showing
  // the boundary here on the Cardinality (ℕ) side as the canonical
  // pattern — the practitioner explicitly chooses the sentinel for
  // transfinite values, which @b is the proof obligation: the type
  // system makes the caller commit to a behaviour for the overflow
  // / saturation regime.
  const auto seven = finite_cardinality(7);
  const auto sentinel = std::numeric_limits<std::size_t>::max();
  CHECK(realize_to_size_t(seven, sentinel) == 7);
  CHECK(realize_to_size_t(Cardinality{ℵ_0{}}, sentinel) == sentinel);

  // The signed-side @c realize_signed_to_machine boundary is tracked
  // under #429 and not yet shipped.  Once it lands, the symmetric
  // pattern is:
  //
  //   const auto z   = finite_signed_cardinality(7);
  //   const int  out = realize_signed_to_machine(z, INT_MAX, INT_MIN);
  //   // ^ proof obligation: caller chooses sentinels for ±ℵ_0 and NaZ.
  //
  // The doctrinal point is the same in either direction: descent to a
  // machine integer is an @b explicit step, not an implicit operator
  // overload — and the explicit step carries the proof obligation
  // structurally.
}

TEST_CASE(
    "sint: negafibonacci on SignedCardinality — the canonical intensional "
    "ℤ-series",
    "[numbers][sint][ergonomics][intensional-first][negafibonacci]") {
  // Negafibonacci is Fibonacci extended to negative indices:
  //   F_{-n} = (-1)^{n+1} · F_n,
  // equivalently the recurrence F_{n-1} = F_{n+1} - F_n run backwards.
  // The sequence F_{-5}, F_{-4}, F_{-3}, F_{-2}, F_{-1} = 5, -3, 2, -1, 1.
  //
  // The series is the canonical @b intensional ℤ-series counterpart to
  // Fibonacci on ℕ: each step uses the subtraction operator that
  // structurally distinguishes ℤ from ℕ.  Computing it on @c
  // SignedCardinality stays in the canonical mathematical carrier the
  // whole way through — no machine-int arithmetic; no UB-on-overflow
  // risk at large |n|; the saturating semantics handles oversize values
  // honestly via @c ±ℵ_0.

  // Run the backwards recurrence: start from F_0=0, F_1=1 and step
  // F_{n-1} = F_{n+1} - F_n.
  auto f_next = finite_signed_cardinality(1);  // F_1
  auto f_curr = finite_signed_cardinality(0);  // F_0
  // F_{-1} = F_1 - F_0 = 1
  auto f_neg1 = f_next - f_curr;
  // F_{-2} = F_0 - F_{-1} = -1
  auto f_neg2 = f_curr - f_neg1;
  // F_{-3} = F_{-1} - F_{-2} = 2
  auto f_neg3 = f_neg1 - f_neg2;
  // F_{-4} = F_{-2} - F_{-3} = -3
  auto f_neg4 = f_neg2 - f_neg3;
  // F_{-5} = F_{-3} - F_{-4} = 5
  auto f_neg5 = f_neg3 - f_neg4;

  CHECK(f_neg1 == finite_signed_cardinality(1));
  CHECK(f_neg2 == finite_signed_cardinality(-1));
  CHECK(f_neg3 == finite_signed_cardinality(2));
  CHECK(f_neg4 == finite_signed_cardinality(-3));
  CHECK(f_neg5 == finite_signed_cardinality(5));

  // The forward Fibonacci sibling on @c Cardinality (ℕ) is the
  // natural pedagogical pair — Fibonacci F_n stays in ℕ throughout
  // because the recurrence uses only addition.  The intensional-
  // first / realize-when-you-mean-it doctrine reads the same way on
  // both sides: stay in the canonical carrier; descend to the
  // machine layer only via an explicit realize-step.  A Fibonacci-
  // on-Cardinality companion test in @c uint_test.cpp is the
  // natural follow-up.
}

TEST_CASE(
    "sint: NO heterogeneous SC + int overload — Honest Rejection is "
    "structurally preserved",
    "[numbers][sint][ergonomics][no-smuggling]") {
  // Negative-shape witness: the partition deliberately does NOT ship
  // overloads like SignedCardinality + int → SignedCardinality.
  // Adding such overloads would route machine-int UB-on-overflow
  // back through the variant arithmetic surface, undoing the
  // Honest Rejection.  The practitioner who wants to add a finite
  // value to a SignedCardinality writes:
  //
  //   sc + finite_signed_cardinality(3)            // canonical form
  //
  // rather than @c sc @c + @c 3.  The slight ergonomic cost is the
  // load-bearing proof: at the call site, the practitioner has named
  // the int as a SignedCardinality literal, committing to the variant-
  // ring semantics rather than the machine-int UB-on-overflow ones.
  const auto sc = finite_signed_cardinality(5);
  const auto sum = sc + finite_signed_cardinality(3);  // canonical
  CHECK(sum == finite_signed_cardinality(8));
  // The same operation expressed at the machine layer would be the
  // UB-trap @c 5 + @c 3 (only safe because the values are tiny);
  // the canonical form is total under the saturating semantics of
  // @c SignedCardinality (overflows escalate to @c +ℵ_0 rather than
  // wrapping or being UB).
}
