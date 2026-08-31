/** @file dedekind/algebra/halfspace_transport_test.cpp
 *
 * Coverage for the ordered-group transport of halfspaces
 * (dedekind.algebra:halfspace_transport): the DSL's @c image / @c inverse /
 * @c argmax / @c is_function / @c is_entire spellings (in @c namespace
 * dedekind::order, backed by the @c algebra layer) and the entireness
 * inference.
 *
 * The compile-time witnesses (static_assert) below were relocated here from
 * @c order/halfspace.cppm when the transport operations moved down to @c
 * algebra to reach the canonical @c IsOrderedAdditiveGroup gate; the three
 * TEST_CASEs give the paired runtime coverage.
 */

#include <catch2/catch_test_macros.hpp>
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.order;
import dedekind.algebra;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::order;

namespace {
inline constexpr auto ℤ = Ω<SignedCardinality>;
inline constexpr auto ℕ = Ω<Cardinality>;

// ── image: bare onto-ness, and the affine pushforward of a halfspace ─────────
static_assert(image(ℤ* ℤ | π1 + fix(3_c) == π2) == ℤ,
              "image(graph of x+3) = ℤ: a translation is onto.");
static_assert(image((ℤ * ℤ | π1 + fix(3_c) == π2) | π1 <= fix(5_c)) ==
                  (ℤ | (π <= fix(8_c))),
              "image over {x ≤ 5} pushes forward to {y ≤ 8}.");

// ── image of the two sign-fold reflection BRANCHES.  Each branch is a mono
// (an injective reflection ℤ×ℤ), so its image is a plain halfspace pushed
// forward.  This is a BRANCH-IMAGE calculation: it exhibits the branch images
// and their union, NOT a constructed non-injective image(abs) on a single
// relation, and NOT a heterogeneous epi ℤ↠ℕ (both branches are typed ℤ×ℤ, so
// everything stays inside ℤ).
static_assert(image(ℤ* ℤ | π1 * fix(1_c) == π2 | π1 >= fix(0_c)) ==
                  (ℤ | π >= fix(0_c)),
              "image of the identity branch on {x≥0} = {y≥0}.");
static_assert(image(ℤ* ℤ | π1 * fix(-1_c) == π2 | π1 < fix(0_c)) ==
                  (ℤ | π > fix(0_c)),
              "image of the negate branch on {x<0} = {y>0} (the sense flips).");
static_assert(image(ℤ* ℤ | π1 * fix(-1_c) == π2 | π1 < fix(0_c))(3),
              "3 ∈ image(negate branch) via −3 (the canonical +3 ∉ {x<0}).");
static_assert(!image(ℤ * ℤ | π1 * fix(-1_c) == π2 | π1 < fix(0_c))(-2),
              "the branch image is non-negative: −2 ∉ it.");
// The JOIN of the two branch images collapses via structured_or: {y≥0} ∪ {y>0}
// = {y≥0} (the weaker, non-strict bound wins).  The result is the NON-NEGATIVE
// SUBOBJECT {y≥0} ⊆ ℤ (order-isomorphic to ℕ, but a subset of ℤ here) --- the
// SET the two branches' outputs cover, symmetric with the meet.  It is NOT an
// epi ℤ↠ℕ: no ℤ→ℕ arrow is constructed, only the branch images are unioned.
static_assert((image(ℤ * ℤ | π1 * fix(1_c) == π2 | π1 >= fix(0_c)) |
               image(ℤ * ℤ | π1 * fix(-1_c) == π2 | π1 < fix(0_c))) ==
                  (ℤ | π >= fix(0_c)),
              "branch images union to the non-negative subobject {y≥0} ⊆ ℤ "
              "(≅ ℕ), via structured_or.");

// The successor graph over ℕ (the SATURATING carrier admitted for K≥0): the
// image the OPAQUE arrow leaves Unknown is DECIDED by the pushforward.
static_assert(
    image((ℕ * ℕ | π1 + fix(1_c) == π2) | π1 > fix(5_c)) ==
        (ℕ | (π > fix(6_c))),
    "image(succ, {n>5}) = {n>6}: the graph decides where opacity walls.");

// Constrained image of a COMPOSITE folds to ∅: hc = T₂∘T₃ pushes {x≤1} to
// {y≤6}, which meets the incompatible codomain {y>6}; the complementary
// halfspaces collapse to Ø --- the same meet-to-empty, EMERGENT from
// composition.
static_assert(
    (image(((ℤ * ℤ | π1 + fix(2_c) == π2) >> (ℤ * ℤ | π1 + fix(3_c) == π2)) |
           π1 <= fix(1_c)) &
     (ℤ | (π > fix(6_c)))) == Ø{},
    "constrained image of the composite T₂∘T₃ collapses: {y≤6} ∩ {y>6} = ∅.");

// ── is_function / is_entire, and argmax over a partial function ──────────────
static_assert(is_function(ℤ* ℤ | π1 + fix(3_c) == π2),
              "the graph of x+3 is a total function (functional ∧ entire).");
static_assert(is_entire(ℤ* ℤ | π1 + fix(3_c) == π2),
              "the bare translation graph is total (entire).");
static_assert(!is_entire(ℤ * ℤ | π1 + fix(3_c) == π2 |
                         π2 <= fix(8_c) & π2 % fix(3_c) == fix(0_c)),
              "constraining the codomain makes the graph a partial function.");
static_assert(argmax(ℤ* ℤ | π1 + fix(3_c) == π2 |
                     π2 <= fix(8_c) & π2 % fix(3_c) == fix(0_c))(3),
              "argmax = max{x ≤ 5 ∧ x ≡ 0 mod 3} = 3: a compile-time "
              "constrained optimum.");
static_assert(!argmax(ℤ * ℤ | π1 + fix(3_c) == π2 |
                      π2 <= fix(8_c) & π2 % fix(3_c) == fix(0_c))(4),
              "4 is feasible-adjacent but not the optimiser (4 ≢ 0 mod 3).");

// ── The translation group, at compile time: closure (T₂∘T₃ = T₅), the converse
// as inverse (T₃⁻¹ = T₋₃), and the abelian cancellation f∘g∘h∘g⁻¹ = f∘h (g and
// its inverse annihilate through h because + commutes).
static_assert(((ℤ * ℤ | π1 + fix(2_c) == π2) >>
               (ℤ * ℤ | π1 + fix(3_c) == π2)) == (ℤ * ℤ | π1 + fix(5_c) == π2),
              "closure: T₂ ∘ T₃ = T₅ (shifts add, symbolically).");
static_assert(inverse(ℤ* ℤ | π1 + fix(3_c) == π2) ==
                  (ℤ * ℤ | π1 + fix(-3_c) == π2),
              "inverse = converse graph: T₃⁻¹ = T₋₃.");
// Contravariant inversion: (f∘g)⁻¹ = g⁻¹∘f⁻¹.  For an invertible map the
// inverse IS the retract, so this is retract composition in the total case
// (Listing 12).
static_assert(inverse((ℤ * ℤ | π1 + fix(3_c) == π2) >>
                      (ℤ * ℤ | π1 + fix(2_c) == π2)) ==
                  (inverse(ℤ * ℤ | π1 + fix(2_c) == π2) >>
                   inverse(ℤ * ℤ | π1 + fix(3_c) == π2)),
              "contravariant inversion: (f∘g)⁻¹ = g⁻¹∘f⁻¹.");
static_assert(((ℤ * ℤ | π1 + fix(2_c) == π2) >> (ℤ * ℤ | π1 + fix(3_c) == π2) >>
               (ℤ * ℤ | π1 + fix(5_c) == π2) >>
               inverse(ℤ * ℤ | π1 + fix(3_c) == π2)) ==
                  ((ℤ * ℤ | π1 + fix(2_c) == π2) >>
                   (ℤ * ℤ | π1 + fix(5_c) == π2)),
              "abelian: f∘g∘h∘g⁻¹ = f∘h (g cancels through h; + commutes).");
// Identity element and conjugation: g∘g⁻¹ is the identity translation T₀, and
// conjugating f by g is trivial (g∘f∘g⁻¹ = f) because + commutes.
static_assert(((ℤ * ℤ | π1 + fix(2_c) == π2) >>
               inverse(ℤ * ℤ | π1 + fix(2_c) == π2)) ==
                  (ℤ * ℤ | π1 + fix(0_c) == π2),
              "inverse law: g ∘ g⁻¹ = id (T₀).");
static_assert(((ℤ * ℤ | π1 + fix(2_c) == π2) >> (ℤ * ℤ | π1 + fix(3_c) == π2) >>
               inverse(ℤ * ℤ | π1 + fix(2_c) == π2)) ==
                  (ℤ * ℤ | π1 + fix(3_c) == π2),
              "abelian conjugation: g ∘ f ∘ g⁻¹ = f (+ commutes).");

// ── Existence proof: the DSL's graph relations are FUNCTIONS (functional AND
// entire), the property INFERRED through composition.  Entireness is the
// algebraic half, read off @c is_left_total_v here in the algebra layer.
static_assert(IsFunctional<decltype(ℤ * ℤ | π1 + fix(3_c) == π2)> &&
                  IsEntire<decltype(ℤ * ℤ | π1 + fix(3_c) == π2)>,
              "a translation graph is functional AND entire -- a function, "
              "inferred from its predicate shape, with no opt-in flag.");
static_assert(
    IsFunctional<decltype((ℤ * ℤ | π1 + fix(3_c) == π2) >>
                          (ℤ * ℤ | π1 + fix(2_c) == π2))> &&
        IsEntire<decltype((ℤ * ℤ | π1 + fix(3_c) == π2) >>
                          (ℤ * ℤ | π1 + fix(2_c) == π2))>,
    "the composite (x+3) ∘ (x+2) is a function too -- INFERRED through the "
    "relative product, no fresh certificate.");
static_assert(
    IsFunctional<decltype((𝔹 * 𝔹 | π1 == π2) >> (𝔹 * 𝔹 | π1 == π2))> &&
        IsEntire<decltype((𝔹 * 𝔹 | π1 == π2) >> (𝔹 * 𝔹 | π1 == π2))>,
    "id ∘ id (a ComposePred, not collapsed) is a function -- inferred from its "
    "two functional factors, exercising the compositional NODE rule.");

// ── A relation IS an arrow (dom/cod), and a BIJECTIVE relation is an
// ISOMORPHISM in Rel -- total ∧ functional ∧ injective ∧ surjective -- with the
// CONVERSE as its two-sided inverse.  A translation x↦x+K is a bijection, so
// inverse (= the converse, K↦−K) is BOTH a left inverse (retract) and a right
// inverse (section); the identity is the diagonal T₀ = {π2 = π1}.  (Tarski /
// Table 3: f⁻¹∘f = id ⟺ f functional ∧ surjective; f∘f⁻¹ = id ⟺ f total ∧
// injective.  Together: a two-sided inverse ⟺ a bijection ⟺ an iso.)  In a
// general allegory the converse is only the DAGGER, an iso exactly here.
static_assert(
    (inverse(ℤ * ℤ | π1 + fix(3_c) == π2) >> (ℤ * ℤ | π1 + fix(3_c) == π2)) ==
            (ℤ * ℤ | π1 + fix(0_c) == π2) &&
        ((ℤ * ℤ | π1 + fix(3_c) == π2) >>
         inverse(ℤ * ℤ | π1 + fix(3_c) == π2)) == (ℤ * ℤ | π1 + fix(0_c) == π2),
    "f⁻¹∘f = f∘f⁻¹ = id (T₀): the converse is a TWO-SIDED inverse, so "
    "the translation graph is an ISOMORPHISM in Rel.");
// A PARTIAL (restricted) graph is functional but NOT entire, hence NOT a
// bijection and NOT an iso: the converse loses the pruned domain on the
// round-trip, so it is at most a one-sided dagger, never a two-sided inverse.
static_assert(
    IsFunctional<decltype((ℤ * ℤ | π1 + fix(3_c) == π2) | π1 <= fix(5_c))> &&
        !IsEntire<decltype((ℤ * ℤ | π1 + fix(3_c) == π2) | π1 <= fix(5_c))>,
    "a restricted graph is a partial function, NOT a bijection, so "
    "NOT an iso -- its converse is a one-sided section at most.");
}  // namespace

// Runtime coverage for the relation-reading of a function: the graph of x+3,
// its inverse read backwards, and its image pushed forward.
TEST_CASE(
    "algebra:halfspace_transport — a function is its graph: inverse/image",
    "[algebra][relation][function][inverse][image]") {
  constexpr auto Z = Ω<SignedCardinality>;
  const auto f = Z * Z | π1 + fix(3_c) == π2;  // graph of x ↦ x+3

  STATIC_CHECK(is_function(f));

  volatile int ten = 10;
  CHECK(inverse(f)(std::pair{int(ten), 7}));        // (10,7) ∈ f⁻¹ (converse)
  CHECK_FALSE(inverse(f)(std::pair{int(ten), 8}));  // (10,8) ∉ f⁻¹

  const auto s = image(f | π1 <= fix(5_c));  // range over {x≤5} = {y≤8}
  volatile int eight = 8, nine = 9;
  CHECK(s(int(eight)));       // 8 ≤ 8
  CHECK_FALSE(s(int(nine)));  // 9 ≰ 8
  CHECK(image(f) == Z);       // the whole range: a translation is onto
}

// Runtime coverage for argmax over a PARTIAL function: constrain the codomain
// and the graph becomes partial; its feasible domain is the pullback
// {x≤5 ∧ x≡0 mod3}, and argmax reads the constrained optimum (3) structurally.
TEST_CASE("algebra:halfspace_transport — argmax over a partial function",
          "[algebra][argmax][partial][optimization]") {
  constexpr auto Z = Ω<SignedCardinality>;
  const auto g =
      Z * Z | π1 + fix(3_c) == π2 | π2 <= fix(8_c) & π2 % fix(3_c) == fix(0_c);
  STATIC_CHECK(!is_entire(g));
  STATIC_CHECK(is_entire(Z * Z | π1 + fix(3_c) == π2));

  const auto opt = argmax(g);
  volatile int three = 3, four = 4, zero = 0;
  CHECK(opt(int(three)));       // 3 = max{x≤5, x≡0 mod3}
  CHECK_FALSE(opt(int(four)));  // 4 ≢ 0 mod 3
  CHECK_FALSE(opt(int(zero)));  // 0 feasible but not maximal
}

// Runtime coverage for the point-free non-injective image: the sign-fold epi
// abs = union of two reflection branches, each a mono whose image is a
// halfspace pushed forward.  Sound where a lone retract is not; no walk.
TEST_CASE("algebra:halfspace_transport — image of the sign-fold reflection",
          "[algebra][image][reflection]") {
  constexpr auto Z = Ω<SignedCardinality>;
  const auto absNeg =
      Z * Z | π1 * fix(-1_c) == π2 | π1 < fix(0_c);  // x↦-x, x<0
  const auto img = image(absNeg);                    // {y>0}
  volatile int three = 3, minus2 = -2, zero = 0;
  CHECK(img(int(three)));  // 3 ∈ abs({x<0}) via -3 (canonical +3 ∉ {x<0})
  CHECK_FALSE(img(int(minus2)));  // abs is never negative
  CHECK_FALSE(img(int(zero)));    // 0 not > 0 (the x<0 branch is strict)
  const auto absPos = Z * Z | π1 * fix(1_c) == π2 | π1 >= fix(0_c);  // x↦x, x≥0
  const auto imgP = image(absPos);                                   // {y≥0}
  CHECK(imgP(int(zero)));  // 0 ≥ 0: the identity branch includes it
}
