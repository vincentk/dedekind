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
#include <functional>  // std::plus (equalizer parallel-pair arrow)
#include <limits>
#include <type_traits>  // std::is_same_v (𝔽64 converse type witness)
#include <utility>

import dedekind.category;
import dedekind.sets;
import dedekind.relational;
import dedekind.order;
import dedekind.algebra;

using namespace dedekind::category;
using namespace dedekind::sets;
using namespace dedekind::relational;
using namespace dedekind::order;

namespace {
inline constexpr auto ℤ = 𝔸<SignedCardinality>;
inline constexpr auto ℕ = 𝔸<Cardinality>;
using dedekind::algebra::𝔽64;  // GF(2⁶): operator+ is XOR (characteristic 2)

// ── #876 finding 2: the translation graph IS a categorical equalizer ─────────
// A translation graph is NOT an IsIsomorphism arrow: its Set-Domain is the
// product pair<T,T>.  It is the EQUALIZER subobject of pair<T,T> where the
// parallel pair (π1+K, π2): pair<T,T> → T coincide, representing the function
// whose (domain, codomain) are (T, T).  So the group⟹decidable-image mileage
// pins on the predicate slot of the (domain, codomain, predicate) triple, not
// on a Set→Ω arrow view.  Pinned via category::IsEqualizer for future/CP
// reference.
using ZT = SignedCardinality;
struct Pi1Plus3Arrow {
  using Domain = std::pair<ZT, ZT>;
  using Codomain = ZT;
  constexpr ZT operator()(const std::pair<ZT, ZT>& p) const {
    return std::plus<ZT>{}(p.first, static_cast<ZT>(3));
  }
};
struct Pi2Arrow {
  using Domain = std::pair<ZT, ZT>;
  using Codomain = ZT;
  constexpr ZT operator()(const std::pair<ZT, ZT>& p) const { return p.second; }
};
static_assert(
    dedekind::category::IsEqualizer<decltype(ℤ * ℤ | π1 + fix(3_c) == π2),
                                    Pi1Plus3Arrow, Pi2Arrow>,
    "the translation graph x+3==y is the equalizer subobject of pair<T,T> of "
    "the parallel pair (π1+3, π2): the categorical view CP's IsIsomorphism "
    "expectation missed (#876 finding 2).");

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
                         (π2 <= fix(8_c) && π2 % fix(3_c) == fix(0_c))),
              "constraining the codomain makes the graph a partial function.");
static_assert(argmax(ℤ* ℤ | π1 + fix(3_c) == π2 |
                     (π2 <= fix(8_c) && π2 % fix(3_c) == fix(0_c)))(3),
              "argmax = max{x ≤ 5 ∧ x ≡ 0 mod 3} = 3: a compile-time "
              "constrained optimum.");
static_assert(!argmax(ℤ * ℤ | π1 + fix(3_c) == π2 |
                      (π2 <= fix(8_c) && π2 % fix(3_c) == fix(0_c)))(4),
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

// ── #875 over a Galois field: is the free theorem operator-generic, or gated
// on std::plus?  It is gated on std::plus (the graph is π1+fix(K)), so it picks
// up any carrier whose GROUP operation is spelled operator+: ℤ, unsigned
// (ℤ/2ʷ), and 𝔽64 = GF(2⁶), whose operator+ IS XOR (characteristic 2).  A sharp
// coherence test: XOR also means set symmetric difference (^) in Trsk, but 𝔽64
// routes its group op through operator+, not ^, so the translation graph does
// not collide with the set-level ^.  In char 2 the shift is self-inverse (−K =
// K), computed through the group-inverse registry (category::inverse_v), so the
// inverse graph is the same graph.  (Generalizing the GRAPH itself to a
// non-additive op, e.g. π1·fix(K) over a multiplicative group, is #882.)
static_assert(dedekind::category::IsAbelianGroup<𝔽64, std::plus<𝔽64>>,
              "𝔽64 = GF(2⁶) is an additive abelian group under + (= XOR): the "
              "#875 IsAbelianGroup gate picks it up.");
// The inverse gate FIRES for a 𝔽64 translation graph AND the converse shift is
// the GROUP inverse, taken from the group-inverse registry (category::inverse_v
// → the 𝔽64 inverse(a, std::plus) hook in :galois), NOT carrier operator- on
// the NTTP.  In characteristic two −K = K, so the converse graph is the SAME
// graph: its type equals the forward graph's type.  This is the sharp coherence
// test the group carrier was chosen for, and it is the point of finding #876:1
// (derive the shift from the group-inverse API).  A plain −K would have leaned
// on 𝔽64's carrier operator-, which IsGroup never promises.
static_assert(
    std::is_same_v<decltype(inverse(𝔸<𝔽64> * 𝔸<𝔽64> |
                                    π1 + Bound<𝔽64{5}>{} == π2)),
                   decltype(𝔸<𝔽64> * 𝔸<𝔽64> | π1 + Bound<𝔽64{5}>{} == π2)>,
    "#876/#875: over 𝔽64 = GF(2⁶) the group inverse of the +5 shift is +5 "
    "itself (char 2), so the converse graph EQUALS the forward graph; the "
    "shift flows through the group-inverse registry, not carrier operator-.");
// FINDING (the sharp test paid off): retractability generalized to any
// IsAbelianGroup, and the converse now flows through the group-inverse registry
// rather than carrier negation.  Set==Set equality is still gated on
// IsSaturating and entireness on IsOrderedAdditiveGroup/ℕ, which 𝔽64 (a
// non-ordered field) does NOT satisfy, so a 𝔽64 graph gets its inverse but
// cannot yet be compared by value / asserted entire in the DSL (hence the
// type-level decltype witness above).  Widening those gates is the #882 thread.

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

// ── preimage: the CONTRAVARIANT inverse of image ─────────────────────────────
// image pushes a DOMAIN halfspace forward; preimage pulls a CODOMAIN halfspace
// back, closed-form and the same shape.  It DERIVES the reduced native
// predicate that numbers/strength_reduction_test writes by hand.
// Translation x↦x+3: {y≤8} pulls back to {x≤5} (pivot P−K = 8−3).
static_assert(preimage(ℤ* ℤ | π1 + fix(3_c) == π2, ℤ | (π <= fix(8_c))) ==
                  (ℤ | (π <= fix(5_c))),
              "preimage(x+3, {y≤8}) = {x≤5}: the exact inverse of image.");
// The DEFINING property preimage(f,P)(a) ⟺ P(f(a)), pointwise on the boundary.
static_assert(preimage(ℤ* ℤ | π1 + fix(3_c) == π2, ℤ | (π <= fix(8_c)))(5),
              "5 ∈ preimage: f(5)=8 ≤ 8.");
static_assert(!preimage(ℤ * ℤ | π1 + fix(3_c) == π2, ℤ | (π <= fix(8_c)))(6),
              "6 ∉ preimage: f(6)=9 ≰ 8.");
// Reflection x↦−x: {y≤5} pulls back to {x≥−5}, the sense FLIPS.
static_assert(preimage(ℤ* ℤ | π1 * fix(-1_c) == π2, ℤ | (π <= fix(5_c))) ==
                  (ℤ | (π >= fix(-5_c))),
              "preimage(−x, {y≤5}) = {x≥−5}: reflection flips the sense.");
// CONTRAVARIANCE (f;g)* = g*∘f*: pulling {y≤8} through T₂;T₃ = T₅ in one step
// equals pulling through T₃ then T₂ (the closed forms compose).
static_assert(
    preimage(ℤ* ℤ | π1 + fix(5_c) == π2, ℤ | (π <= fix(8_c))) ==
        preimage(ℤ * ℤ | π1 + fix(2_c) == π2,
                 preimage(ℤ* ℤ | π1 + fix(3_c) == π2, ℤ | (π <= fix(8_c)))),
    "contravariance: preimage(T₂;T₃, P) = preimage(T₂, preimage(T₃, P)).");
static_assert(preimage(ℤ* ℤ | π1 + fix(5_c) == π2, ℤ | (π <= fix(8_c))) ==
                  (ℤ | (π <= fix(3_c))),
              "and that common value is {x≤3} (8−5).");
}  // namespace

// Runtime coverage for the relation-reading of a function: the graph of x+3,
// its inverse read backwards, and its image pushed forward.
TEST_CASE(
    "algebra:halfspace_transport — a function is its graph: inverse/image",
    "[algebra][relation][function][inverse][image]") {
  constexpr auto Z = 𝔸<SignedCardinality>;
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
  constexpr auto Z = 𝔸<SignedCardinality>;
  const auto g = Z * Z | π1 + fix(3_c) == π2 |
                 (π2 <= fix(8_c) && π2 % fix(3_c) == fix(0_c));
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
  constexpr auto Z = 𝔸<SignedCardinality>;
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

// Runtime coverage for preimage: the contravariant inverse of image.  The
// derived domain predicate satisfies the defining property a ∈ preimage(f,P)
// ⟺ f(a) ∈ P, checked at the boundary; reflection flips the sense.
TEST_CASE("algebra:halfspace_transport — preimage, the contravariant inverse",
          "[algebra][preimage][pullback][inverse]") {
  constexpr auto Z = 𝔸<SignedCardinality>;
  const auto f = Z * Z | π1 + fix(3_c) == π2;         // x ↦ x+3
  const auto dom = preimage(f, Z | (π <= fix(8_c)));  // derived {x ≤ 5}
  volatile int five = 5, six = 6;
  CHECK(dom(int(five)));       // 5 ≤ 5, and f(5)=8 ≤ 8
  CHECK_FALSE(dom(int(six)));  // 6 ≰ 5, and f(6)=9 ≰ 8
  CHECK(dom == (Z | (π <= fix(5_c))));

  const auto neg = Z * Z | π1 * fix(-1_c) == π2;         // x ↦ −x
  const auto back = preimage(neg, Z | (π <= fix(5_c)));  // {x ≥ −5}
  volatile int m5 = -5, m6 = -6;
  CHECK(back(int(m5)));        // −5 ≥ −5, and −(−5)=5 ≤ 5
  CHECK_FALSE(back(int(m6)));  // −6 ≱ −5, and −(−6)=6 ≰ 5
}

// #875: retractability generalizes ℤ → arbitrary IsGroup.  The graph `inverse`
// gate relaxed from IsOrderedAdditiveGroup to IsAbelianGroup, so a
// translation's converse graph (the group inverse of the shift) is now
// available on a cyclic group such as `unsigned` (ℤ/2ʷ), which the old gate
// withheld.  The converse is the (modular) inverse: (x, x−1) ∈ inverse(succ),
// even under wrap.
TEST_CASE(
    "algebra:halfspace_transport: inverse over a cyclic group (unsigned), #875",
    "[algebra][inverse][group]") {
  constexpr auto U = 𝔸<unsigned>;
  const auto succ = U * U | π1 + fix(1_c) == π2;  // graph of x ↦ x+1 over ℤ/2ʷ
  const auto pred = inverse(succ);                // converse = modular x ↦ x−1
  volatile unsigned ten = 10u;
  CHECK(pred(std::pair{unsigned(ten), 9u}));         // (10, 9) ∈ inverse(succ)
  CHECK_FALSE(pred(std::pair{unsigned(ten), 11u}));  // (10, 11) ∉ inverse(succ)
  volatile unsigned zero = 0u;
  // The modular group inverse wraps: pred(0) = UINT_MAX (0 − 1 in ℤ/2ʷ).
  CHECK(pred(std::pair{unsigned(zero), std::numeric_limits<unsigned>::max()}));
}

// ---------------------------------------------------------------------------
// #895: concept smoke test for the ordered-algebra concepts/markers that
// survived the GroupScout scout-algebra retirement.  These previously had
// their only dedicated coverage in scout_algebra_test.cpp (deleted with the
// GroupScout layer), so pin the kept concept layer here.  The concepts are
// consumed for real by the transport operations above; this block is the
// direct concept-satisfaction witness.
// ---------------------------------------------------------------------------

// SignedCardinality (the project's saturating ℤ proxy) is the positive
// witness: it opts into the translation-invariance marker, so the ordered
// additive-group concept fires.
static_assert(
    dedekind::algebra::is_translation_invariant_ordered_v<SignedCardinality>,
    "SignedCardinality opts into the translation-invariance marker.");
static_assert(dedekind::algebra::IsOrderedAdditiveGroup<SignedCardinality>,
              "ℤ's carrier is an ordered additive group.");

// unsigned int is an additive group (ℤ/2^N ℤ) but MODULAR: its order is not
// translation-invariant (wrap reverses order at the boundary), so the marker
// defaults to false and the ordered-group concept rejects it --- even though
// the underlying IsAbelianGroup gate is satisfied.
static_assert(
    !dedekind::algebra::is_translation_invariant_ordered_v<unsigned int>,
    "modular unsigned does not opt into translation-invariance.");
static_assert(!dedekind::algebra::IsOrderedAdditiveGroup<unsigned int>,
              "modular unsigned is an additive group but not an ORDERED one.");

// Machine int is not modelled as an additive group at all (signed overflow is
// UB), so it fails the concept on its algebraic leg.
static_assert(!dedekind::algebra::IsOrderedAdditiveGroup<int>,
              "machine int is not an additive group (overflow is UB).");

TEST_CASE(
    "algebra:ordered-algebra concepts survive GroupScout retirement (#895)",
    "[algebra][scout_algebra][concept][smoke]") {
  // The static_asserts above carry the structural claim; this runtime body
  // gives Codecov a visible line (static_asserts are invisible to coverage).
  CHECK(
      dedekind::algebra::is_translation_invariant_ordered_v<SignedCardinality>);
  CHECK_FALSE(
      dedekind::algebra::is_translation_invariant_ordered_v<unsigned int>);
}
