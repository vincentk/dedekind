/**
 * @file tower_test.cpp
 * @brief The numerical tower: embedding-mediated membership through ℕ ↪ ℤ ↪ ℚ ↪
 * ℝ ↪ ℂ.
 *
 * @section Design_Note
 * We deliberately model inclusions as *embedding arrows* rather than subset
 * declarations.  This avoids closure / totality commitments: each embedding
 * e: X -> Y is a user-declared monic arrow; membership in Y is tested via
 * χ_Y(e(x)), not by asserting X ⊆ Y in a categorical sense.
 *
 * See: etcs.cppm — `in_via(x, e, S)` for the underlying primitive.
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;
import dedekind.numbers;
import dedekind.sets;
import dedekind.ieee;

using namespace dedekind::category;
using namespace dedekind::numbers;
using namespace dedekind::sets;
using namespace dedekind::ieee;

// ---------------------------------------------------------------------------
// Compile-time monicity declarations are honoured
// ---------------------------------------------------------------------------

static_assert(IsMonicArrow<Identity<machine_integer>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_𝔹_uint_)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_uint_sint_)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_𝕂3_ℤ_)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_ℤ_ℚ<>)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_ℚ_ℝ<>)>>);
static_assert(IsMonicArrow<std::decay_t<decltype(embed_ℝ_ℂ<>)>>);

// ---------------------------------------------------------------------------
// Arrow types are correct (compile-time)
// ---------------------------------------------------------------------------

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_𝔹_uint_)>>, bool>);
static_assert(
    std::same_as<Cod<std::decay_t<decltype(embed_𝔹_uint_)>>, unsigned>);

static_assert(
    std::same_as<Dom<std::decay_t<decltype(embed_uint_sint_)>>, unsigned>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_uint_sint_)>>,
                           machine_integer>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_𝕂3_ℤ_)>>, Ternary>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_𝕂3_ℤ_)>>,
                           dedekind::sets::SignedCardinality>);

// embed_ℤ_ℚ<> takes a machine_integer (the source-side type is hardwired
// in the arrow definition, independent of the I template parameter) and
// returns a Rational<I> with I = default_integer post-#499 retarget
// (default_integer = SignedExtensionalCardinal<>).
static_assert(
    std::same_as<Dom<std::decay_t<decltype(embed_ℤ_ℚ<>)>>, machine_integer>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_ℤ_ℚ<>)>>,
                           Rational<default_integer>>);

// embed_ℚ_ℝ<> default-instantiates with I = default_integer (so the source
// is Rational<default_integer> = Rational<SignedExtensionalCardinal<>>)
// and S = machine_real_scalar.
static_assert(std::same_as<Dom<std::decay_t<decltype(embed_ℚ_ℝ<>)>>,
                           Rational<default_integer>>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_ℚ_ℝ<>)>>,
                           Real<machine_real_scalar>>);

static_assert(std::same_as<Dom<std::decay_t<decltype(embed_ℝ_ℂ<>)>>,
                           Real<machine_real_scalar>>);
static_assert(std::same_as<Cod<std::decay_t<decltype(embed_ℝ_ℂ<>)>>,
                           Complex<machine_real_scalar>>);

static_assert(IsSpecies<Rational<machine_integer>>);
static_assert(IsSpecies<Real<machine_real_scalar>>);
static_assert(IsSpecies<Complex<machine_real_scalar>>);

// ---------------------------------------------------------------------------
// Generic unsigned / signed / floating-point embeddings
// ---------------------------------------------------------------------------

TEST_CASE("Tower: embed_to_ℕ<U> covers any unsigned_integral",
          "[numbers][tower][embedding]") {
  // unsigned long is distinct from unsigned — exercises the template overload
  CHECK(embed_to_ℕ(42UL).limbs[0] == 42UL);
  CHECK(embed_to_ℕ(0UL).limbs[0] == 0UL);
  CHECK(embed_to_ℕ(static_cast<unsigned char>(7)).limbs[0] == 7UL);
}

TEST_CASE("Tower: embed_unsigned_ℕ (concrete monic arrow)",
          "[numbers][tower][embedding]") {
  CHECK(embed_unsigned_ℕ(0u).limbs[0] == 0UL);
  CHECK(embed_unsigned_ℕ(42u).limbs[0] == 42UL);
  CHECK(embed_unsigned_ℕ(1000u).limbs[0] == 1000UL);
}

TEST_CASE("Tower: embed_uint_ℕ (universal machine→variant lift)",
          "[numbers][tower][embedding][carrier-lattice]") {
  // The structure-forgetting lift from std::unsigned_integral
  // (modular ring ℤ/2^wℤ) into the variant ℕ-proxy Cardinality
  // (saturating commutative monoid).  Closes part of #417.
  STATIC_CHECK(IsMonicArrow<std::decay_t<decltype(embed_uint_ℕ_)>>);
  STATIC_CHECK(
      std::same_as<Dom<std::decay_t<decltype(embed_uint_ℕ_)>>, unsigned>);
  STATIC_CHECK(std::same_as<Cod<std::decay_t<decltype(embed_uint_ℕ_)>>,
                            dedekind::sets::Cardinality>);

  // Function-template form covers any std::unsigned_integral width.
  CHECK(embed_uint_ℕ(0u) == dedekind::sets::finite_cardinality(0));
  CHECK(embed_uint_ℕ(42u) == dedekind::sets::finite_cardinality(42));
  CHECK(embed_uint_ℕ(static_cast<unsigned long>(1000)) ==
        dedekind::sets::finite_cardinality(1000));
  CHECK(embed_uint_ℕ(static_cast<std::size_t>(7)) ==
        dedekind::sets::finite_cardinality(7));

  // Concrete monic arrow form (the named-arrow variant for
  // composition with downstream IsMonicArrow / IsRingHomomorphism
  // callsites).
  CHECK(embed_uint_ℕ_(0u) == dedekind::sets::finite_cardinality(0));
  CHECK(embed_uint_ℕ_(42u) == dedekind::sets::finite_cardinality(42));
}

TEST_CASE("Tower: embed_signed_to_ℤ<S> covers any signed_integral",
          "[numbers][tower][embedding]") {
  CHECK(embed_signed_to_ℤ(static_cast<short>(5)) == 5);
  CHECK(embed_signed_to_ℤ(-3) == -3);
  CHECK(embed_signed_to_ℤ(static_cast<long long>(100LL)) == 100);
}

TEST_CASE("Tower: embed_floating_ℝ<F> covers any floating_point",
          "[numbers][tower][embedding]") {
  // float → Real<double>: widening conversion
  CHECK(embed_floating_ℝ(1.0f).resolve() == static_cast<double>(1.0f));
  // double → Real<double>: identity wrap
  CHECK(embed_floating_ℝ(2.5).resolve() == 2.5);
}

// ---------------------------------------------------------------------------
// 𝔹 ↪ ℕ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: 𝔹 ↪ ℕ via embed_𝔹_uint_", "[numbers][tower][embedding]") {
  const auto naturals_u =
      ambient_set<unsigned>([](const unsigned&) { return true; });

  CHECK(embed_𝔹_uint_(false) == 0u);
  CHECK(embed_𝔹_uint_(true) == 1u);

  CHECK(in_via(false, embed_𝔹_uint_, naturals_u) == true);
  CHECK(in_via(true, embed_𝔹_uint_, naturals_u) == true);
}

// ---------------------------------------------------------------------------
// ℕ ↪ ℤ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℕ ↪ ℤ via embed_uint_sint_", "[numbers][tower][embedding]") {
  // Unsigned naturals always embed into integers successfully.
  const auto integers = ambient_set<int>([](const int&) { return true; });

  CHECK(in_via(0u, embed_uint_sint_, integers) == true);
  CHECK(in_via(42u, embed_uint_sint_, integers) == true);
  CHECK(in_via(1000u, embed_uint_sint_, integers) == true);

  // Membership in the naturals set respects sign.
  CHECK(N(0) == true);
  CHECK(N(7) == true);
  CHECK(N(-1) == false);

  // Unsigned values are always natural.
  CHECK(N(0u) == true);
  CHECK(N(42u) == true);
}

// ---------------------------------------------------------------------------
// K3 ↪ ℤ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: K3 ↪ ℤ via embed_𝕂3_ℤ_", "[numbers][tower][embedding]") {
  // Post-#430: embed_𝕂3_ℤ_ lands on the variant ℤ-proxy SignedCardinality
  // rather than the int machine carrier.  The runtime equality checks
  // below use the heterogeneous SignedCardinality × std::integral
  // comparison contract pinned by #415 / PR #438; the ambient set is
  // built directly on SignedCardinality.
  const auto integers = ambient_set<dedekind::sets::SignedCardinality>(
      [](const dedekind::sets::SignedCardinality&) { return true; });

  CHECK(embed_𝕂3_ℤ_(Ternary::False) == -1);
  CHECK(embed_𝕂3_ℤ_(Ternary::Unknown) == 0);
  CHECK(embed_𝕂3_ℤ_(Ternary::True) == 1);

  CHECK(in_via(Ternary::False, embed_𝕂3_ℤ_, integers) == true);
  CHECK(in_via(Ternary::Unknown, embed_𝕂3_ℤ_, integers) == true);
  CHECK(in_via(Ternary::True, embed_𝕂3_ℤ_, integers) == true);
}

// ---------------------------------------------------------------------------
// ℤ ↪ ℚ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℤ ↪ ℚ via embed_ℤ_ℚ", "[numbers][tower][embedding]") {
  // Embedding preserves value: n -> n/1.
  CHECK(embed_ℤ_ℚ<>(3).num() == 3);
  CHECK(embed_ℤ_ℚ<>(3).den() == 1);
  CHECK(embed_ℤ_ℚ<>(-5).num() == -5);
}

// ---------------------------------------------------------------------------
// ℚ ↪ ℝ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℚ ↪ ℝ via embed_ℚ_ℝ", "[numbers][tower][embedding]") {
  // Embedding preserves value: 3/1 -> 3.0.  embed_ℚ_ℝ<> defaults its
  // integer carrier to default_integer (SignedExtensionalCardinal<>
  // post-#499 retarget); construct via the default_integer factory.
  using D = default_integer;
  CHECK(embed_ℚ_ℝ<>(Rational<D>{D{3}, D{1}}).resolve() == 3.0);
  CHECK(embed_ℚ_ℝ<>(Rational<D>{D{1}, D{2}}).resolve() == 0.5);
  CHECK(embed_ℚ_ℝ<>(Rational<D>{D{-7}, D{4}}).resolve() == -1.75);
}

// ---------------------------------------------------------------------------
// ℝ ↪ ℂ
// ---------------------------------------------------------------------------

TEST_CASE("Tower: ℝ ↪ ℂ via embed_ℝ_ℂ", "[numbers][tower][embedding]") {
  // Embedding: imaginary part is always 0.
  CHECK(embed_ℝ_ℂ<>(Real<machine_real_scalar>{3.0}).real() == 3.0);
  CHECK(embed_ℝ_ℂ<>(Real<machine_real_scalar>{3.0}).imag() == 0.0);
  CHECK(embed_ℝ_ℂ<>(Real<machine_real_scalar>{-2.5}).real() == -2.5);
  CHECK(embed_ℝ_ℂ<>(Real<machine_real_scalar>{-2.5}).imag() == 0.0);
}

// ---------------------------------------------------------------------------
// Composed tower: unsigned ↪ ℤ ↪ ℚ ↪ ℝ ↪ ℂ  (via >> composition)
// ---------------------------------------------------------------------------

TEST_CASE("Tower: composed chain unsigned -> ℤ -> ℚ -> ℝ -> ℂ",
          "[numbers][tower][embedding][composition]") {
  const auto embed_ℕ_ℚ = embed_uint_sint_ >> embed_ℤ_ℚ<>;
  const auto embed_ℕ_ℝ = embed_ℕ_ℚ >> embed_ℚ_ℝ<>;
  const auto embed_ℕ_ℂ = embed_ℕ_ℝ >> embed_ℝ_ℂ<>;

  // 3 travels the full chain and arrives in ℂ as (3 + 0i).
  const auto result = embed_ℕ_ℂ(3u);
  CHECK(result.real() == 3.0);
  CHECK(result.imag() == 0.0);
}

// ---------------------------------------------------------------------------
// Stress test: Im(K3 -> ℤ) ∩ { z in ℂ | Re(z) > 0 }
// ---------------------------------------------------------------------------

TEST_CASE("Stress: Im(K3->ℤ) intersect positive-real ℂ",
          "[numbers][tower][embedding][intersection]") {
  // Image of K3 in ℤ under canonical embedding: {-1, 0, 1}.  Post-#430
  // @c embed_𝕂3_ℤ_ lands on @c SignedCardinality; the heterogeneous
  // @c == contract pinned by #415 / PR #438 lets us verify the image
  // values directly.
  CHECK(embed_𝕂3_ℤ_(Ternary::False) == -1);
  CHECK(embed_𝕂3_ℤ_(Ternary::Unknown) == 0);
  CHECK(embed_𝕂3_ℤ_(Ternary::True) == 1);

  // FIXME(#429): bridging from @c SignedCardinality to the @c int-typed
  // @c embed_ℤ_ℚ<> needs the @c realize_signed_to_machine extraction
  // arrow (filed under #429 as a #402 prerequisite).  Until that lands,
  // we feed the downstream tower the canonical int values directly —
  // the variant-side embedding is verified by the @c CHECKs above.
  const int z_false = -1;
  const int z_unknown = 0;
  const int z_true = 1;

  const auto embed_ℤ_ℂ = embed_ℤ_ℚ<> >> embed_ℚ_ℝ<> >> embed_ℝ_ℂ<>;

  const auto c_false = embed_ℤ_ℂ(z_false);
  const auto c_unknown = embed_ℤ_ℂ(z_unknown);
  const auto c_true = embed_ℤ_ℂ(z_true);

  const auto has_positive_real = [](const Complex<machine_real_scalar>& z) {
    return z.real() > 0.0;
  };

  CHECK(has_positive_real(c_false) == false);
  CHECK(has_positive_real(c_unknown) == false);
  CHECK(has_positive_real(c_true) == true);

  // Therefore the intersection is exactly {1 + 0i}.
  CHECK(c_true.real() == 1.0);
  CHECK(c_true.imag() == 0.0);
}

// ---------------------------------------------------------------------------
// Partial Arithmetic and Ternary Logic Integration
// ---------------------------------------------------------------------------

TEST_CASE("Partial Arithmetic: Rational<I>",
          "[numbers][tower][partial][rational]") {
  const auto add_op = PartialAddRational<machine_integer>{};
  const auto mul_op = PartialMulRational<machine_integer>{};
  const auto div_op = HonestDivRational<machine_integer>{};

  // Partial addition on rationals is exact (Ternary::True)
  const auto q1 = Rational<machine_integer>(1, 2);
  const auto q2 = Rational<machine_integer>(1, 3);

  const auto add_result = add_op(std::make_pair(q1, q2));
  CHECK(add_result.status == Ternary::True);  // Exact arithmetic
  CHECK(add_result.value == Rational<machine_integer>(5, 6));

  // Partial multiplication is also exact
  const auto mul_result = mul_op(std::make_pair(q1, q2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value == Rational<machine_integer>(1, 6));

  const auto div_result = div_op(std::make_pair(q1, q2));
  CHECK(div_result.status == Ternary::True);
  CHECK(div_result.value == Rational<machine_integer>(3, 2));

  const auto div_zero =
      div_op(std::make_pair(q1, Rational<machine_integer>(0, 1)));
  CHECK(div_zero.status == Ternary::False);
}

TEST_CASE("Partial Arithmetic: Real<S>", "[numbers][tower][partial][real]") {
  const auto add_op = PartialAddReal<machine_real_scalar>{};
  const auto mul_op = PartialMulReal<machine_real_scalar>{};
  const auto div_op = PartialDivReal<machine_real_scalar>{};

  const auto r1 = Real<machine_real_scalar>{1.5};
  const auto r2 = Real<machine_real_scalar>{2.0};

  // Partial addition on reals always succeeds
  const auto add_result = add_op(std::make_pair(r1, r2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value.resolve() == 3.5);

  // Partial multiplication succeeds
  const auto mul_result = mul_op(std::make_pair(r1, r2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value.resolve() == 3.0);

  // Division by non-zero succeeds
  const auto div_result = div_op(std::make_pair(r1, r2));
  CHECK(div_result.status == Ternary::True);

  // Division by zero fails (returns False status)
  const auto div_zero =
      div_op(std::make_pair(r1, Real<machine_real_scalar>{0}));
  CHECK(div_zero.status == Ternary::False);
}

TEST_CASE("Partial Arithmetic: Complex<R>",
          "[numbers][tower][partial][complex]") {
  const auto add_op = PartialAddComplex<machine_real_scalar>{};
  const auto mul_op = PartialMulComplex<machine_real_scalar>{};

  const auto c1 = Complex<machine_real_scalar>{1.0, 2.0};
  const auto c2 = Complex<machine_real_scalar>{3.0, 4.0};

  // Partial addition: (1+2i) + (3+4i) = (4+6i)
  const auto add_result = add_op(std::make_pair(c1, c2));
  CHECK(add_result.status == Ternary::True);
  CHECK(add_result.value.real() == 4.0);
  CHECK(add_result.value.imag() == 6.0);

  // Partial multiplication: (1+2i)(3+4i) = 3+4i+6i+8i^2 = -5+10i
  const auto mul_result = mul_op(std::make_pair(c1, c2));
  CHECK(mul_result.status == Ternary::True);
  CHECK(mul_result.value.real() == -5.0);
  CHECK(mul_result.value.imag() == 10.0);
}

TEST_CASE("Partial Embeddings with Ternary Status",
          "[numbers][tower][partial][embeddings]") {
  // ℤ ↪ ℚ: exact embedding (Ternary::True)
  const auto embed_z_to_q = PartialEmbedIntegerToRational<machine_integer>{};
  const auto z_result = embed_z_to_q(42);
  CHECK(z_result.status == Ternary::True);
  CHECK(z_result.value.num() == 42);
  CHECK(z_result.value.den() == 1);

  // ℚ ↪ ℝ: lossy embedding (Ternary::Unknown)
  const auto embed_q_to_r =
      PartialEmbedRationalToReal<machine_integer, machine_real_scalar>{};
  const auto q = Rational<machine_integer>(1, 3);
  const auto q_result = embed_q_to_r(q);
  CHECK(q_result.status == Ternary::Unknown);  // Flagged as potentially lossy
  CHECK(q_result.value.resolve() ==
        static_cast<machine_real_scalar>(1.0 / 3.0));

  // ℝ ↪ ℂ: exact embedding (Ternary::True)
  const auto embed_r_to_c = PartialEmbedRealToComplex<machine_real_scalar>{};
  const auto r = Real<machine_real_scalar>{2.5};
  const auto r_result = embed_r_to_c(r);
  CHECK(r_result.status == Ternary::True);
  CHECK(r_result.value.real() == 2.5);
  CHECK(r_result.value.imag() == 0.0);
}

// Static asserts for Kleene traits
static_assert(is_kleene_associative_v<Rational<machine_integer>,
                                      PartialAddRational<machine_integer>>);
static_assert(is_kleene_commutative_v<Rational<machine_integer>,
                                      PartialAddRational<machine_integer>>);
static_assert(partial_identity_v<Rational<machine_integer>,
                                 PartialAddRational<machine_integer>>.num() ==
              0);

static_assert(is_kleene_associative_v<Rational<machine_integer>,
                                      PartialMulRational<machine_integer>>);
static_assert(is_kleene_commutative_v<Rational<machine_integer>,
                                      PartialMulRational<machine_integer>>);
static_assert(partial_identity_v<Rational<machine_integer>,
                                 PartialMulRational<machine_integer>>.num() ==
              1);

// Floating-point is commutative but NOT associative: (a+b)+c != a+(b+c) due to
// rounding. Associativity-by-fiat is reserved for the explicit
// dedekind::ieee::IEEE<F> opt-in.
static_assert(!is_kleene_associative_v<Real<machine_real_scalar>,
                                       PartialAddReal<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Real<machine_real_scalar>,
                                      PartialAddReal<machine_real_scalar>>);

static_assert(!is_kleene_associative_v<Real<machine_real_scalar>,
                                       PartialMulReal<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Real<machine_real_scalar>,
                                      PartialMulReal<machine_real_scalar>>);

static_assert(!is_kleene_associative_v<Complex<machine_real_scalar>,
                                       PartialAddComplex<machine_real_scalar>>);
static_assert(is_kleene_commutative_v<Complex<machine_real_scalar>,
                                      PartialAddComplex<machine_real_scalar>>);

static_assert(!is_kleene_associative_v<Complex<machine_real_scalar>,
                                       PartialMulComplex<machine_real_scalar>>);

// ---------------------------------------------------------------------------
// IEEE fast-lane: Set membership over IEEE<double> domain
// ---------------------------------------------------------------------------

TEST_CASE("Tower+IEEE: sets over IEEE<double> domain",
          "[numbers][tower][ieee][sets]") {
  using F = double;
  const auto positive =
      ambient_set<IEEE<F>>([](const IEEE<F>& v) { return v.resolve() > 0.0; });
  const auto bounded = ambient_set<IEEE<F>>(
      [](const IEEE<F>& v) { return v.resolve() <= 10.0; });
  const auto support = set_intersection(positive, bounded);

  // Positive and bounded
  CHECK(support.χ(IEEE<F>{5.0}) == true);
  // Positive but not bounded
  CHECK(support.χ(IEEE<F>{15.0}) == false);
  // Negative
  CHECK(support.χ(IEEE<F>{-1.0}) == false);
}

TEST_CASE("Tower+IEEE: IEEE<double> union and complement",
          "[numbers][tower][ieee][sets]") {
  using F = double;
  const auto neg =
      ambient_set<IEEE<F>>([](const IEEE<F>& v) { return v.resolve() < 0.0; });
  const auto pos =
      ambient_set<IEEE<F>>([](const IEEE<F>& v) { return v.resolve() > 0.0; });
  const auto nonzero = set_union(neg, pos);
  const auto zero_set = set_complement(nonzero);

  CHECK(nonzero.χ(IEEE<F>{3.0}) == true);
  CHECK(nonzero.χ(IEEE<F>{-2.0}) == true);
  CHECK(nonzero.χ(IEEE<F>{0.0}) == false);

  // Complement of nonzero should contain only zero
  CHECK(zero_set.χ(IEEE<F>{0.0}) == true);
  CHECK(zero_set.χ(IEEE<F>{1.0}) == false);
}

// ---------------------------------------------------------------------------
// ℂ ↪ Dual: Complex values embedded into Dual numbers (real part, zero ε)
//
// Moved to src/test/cpp/modules/dedekind/analysis/dual_test.cpp at PR #513
// — Dual<F> relocated from :numbers to :analysis (the carrier's
// structural meaning is differential / forward-mode AD).
// ---------------------------------------------------------------------------
static_assert(is_kleene_commutative_v<Complex<machine_real_scalar>,
                                      PartialMulComplex<machine_real_scalar>>);

// ---------------------------------------------------------------------------
// Inter-species set membership via embedding arrows: 𝔹 ↪ ℕ ↪ ℤ ↪ ℚ
// Demonstrates that `in_via` composes cleanly across the numeric tower.
// ---------------------------------------------------------------------------

TEST_CASE("Tower: inter-species set membership via embeddings",
          "[numbers][tower][embedding][sets]") {
  const auto naturals =
      ambient_set<unsigned>([](const unsigned&) { return true; });

  // Every boolean embeds into ℕ and stays in the universal set.
  CHECK(in_via(true, embed_𝔹_uint_, naturals) == true);
  CHECK(in_via(false, embed_𝔹_uint_, naturals) == true);

  // Compose 𝔹 ↪ ℕ ↪ ℤ ↪ ℚ and test against a ℚ+ predicate.
  // Rational<> is not an ETCS species, so use Set<> directly.
  // embed_ℤ_ℚ<> default-instantiates over default_integer (SEC<>
  // post-#499 retarget); the predicate-set type parameter must match.
  const auto positive_pred = [](const Rational<default_integer>& q) {
    return q.num() > default_integer{0};
  };
  const Set<Rational<default_integer>, ClassicalLogic, decltype(positive_pred)>
      positive_rationals{positive_pred};

  const auto embed_𝔹_ℚ = [](bool b) {
    return embed_ℤ_ℚ<>(embed_uint_sint_(embed_𝔹_uint_(b)));
  };

  CHECK(positive_rationals(embed_𝔹_ℚ(true)) == true);    // 1/1 ∈ ℚ+
  CHECK(positive_rationals(embed_𝔹_ℚ(false)) == false);  // 0/1 ∉ ℚ+
}
