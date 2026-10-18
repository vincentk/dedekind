/**
 * @file dedekind/sets/heterogeneous.cppm
 * @partition :heterogeneous
 * @brief Set expressions ACROSS carriers: the product @f$A \times B@f$, the
 *        sum @f$A + B@f$, and the one widening the library ships, ℕ ↪ ℤ.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section heterogeneous__Scope
 * @c :expressions is the homogeneous grammar: the former, @f$\wedge \vee
 * \neg@f$ and their laws on the subobjects of ONE carrier.  A binary operation
 * on set objects over DIFFERENT carriers lands in a universal construction on
 * the carriers (#1039): @c a @c * @c b in the product @f$X \times Y@f$
 * (@c std::pair, @c IsProduct, with the factors as its legs), @c a @c | @c b
 * in the sum @f$X + Y@f$ (@c std::variant, @c IsCoproduct, the summands its
 * restrictions), @c a @c & @c b across carriers the empty set of the sum.  The
 * sum's two arrows @c Inject / @c Restrict (@f$\exists_\iota \dashv
 * \iota^{-1}@f$) are the first instance of the hyperdoctrine
 * (@c relational:graph).  The ℕ / ℤ pair keeps its identification along the
 * embedding (@c NatZedPair), the sole exception to "different carriers build a
 * sum", pending the ℕ / ℤ re-implementation (#1035).
 *
 * Wikipedia: Product (category theory), Coproduct, Extensive category.
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>  // std::less_equal: the inclusion order the sum's arrows preserve
#include <type_traits>
#include <utility>
#include <variant>

export module dedekind.sets:heterogeneous;

import dedekind.category;
import :setobject;      // IsSetObject, universe_t
import :cardinality;    // Cardinality / SignedCardinality: the ℕ / ℤ widening
import :boundaries;     // 𝔸, Ø, Subobjects, finalize_combine
import :computability;  // HasDecidableMembership (witnesses)
import :terms;          // π: the atoms of the witnesses
import :expressions;    // Comprehension, the homogeneous & | ~, lift_to

namespace dedekind::sets {
using namespace dedekind::category;

// ---------------------------------------------------------------------------
// Cross-carrier meet on the variant pair (existential proof, slice of #362)
// ---------------------------------------------------------------------------
//
// When two sets are intersected and their carriers are related by the
// canonical embedding ℕ ↪ ℤ, the result's carrier should be the @b
// tighter of the two — the library picks the pullback along the
// embedding.  Mathematically:
//
//   A ⊂ ℕ,  B ⊂ ℤ,  ℕ ↪ ℤ
//   ⇒  A ∩ B ⊂ ℕ  (not ⊂ ℤ).
//
// This is the @b carrier @b strength-reduction rule: the carrier
// "lattice" (ℕ < ℤ < ℚ < ℝ < ℂ) drives the resulting carrier of binary
// set operations.  #362 tracks the @b general framework (any pair in
// the lattice); the overloads below land the @b existential @b proof
// for the variant pair (Cardinality, SignedCardinality), which is the
// load-bearing case for Paper 3's type-directed-collapse story.
//
// The result predicate evaluates @c lhs(v) @c && @c rhs(lift(v)) where
// @c v has the smaller carrier's type and @c lift is the exported
// @c lift_cardinality_to_signed from @c :cardinality (the single
// source of truth for the variant-level ℕ ↪ ℤ embedding; reachable
// here because @c :expressions imports @c :cardinality).
//
// Future iterations under #362 will replace this hand-coded pair with
// a @c carrier_lattice_meet_t<T1, T2> trait and a generic overload
// that dispatches across the full lattice.  See
// @c docs/design/carrier-lattice.md for the design discussion.

/** @brief The two carriers of the one cross-carrier pair the library ships,
 *  ℕ ↪ ℤ on the variant proxies: @c A is a set object over @c Cardinality,
 *  @c B one over @c SignedCardinality, in the same species.  Any set object
 *  qualifies (a halfspace, a comprehension, a node): only χ is read. */
template <typename A, typename B>
concept NatZedPair =
    IsSetObject<A> && IsSetObject<B> &&
    std::same_as<typename A::Domain, Cardinality> &&
    std::same_as<typename B::Domain, SignedCardinality> &&
    std::same_as<typename A::logic_species, typename B::logic_species>;

/** @brief Cross-carrier meet: a set over ℕ @c & a set over ℤ @c → a set over
 *  ℕ (carrier strength-reduction; closes a slice of #362).  The intersection
 *  is contained in ℕ, so the result carrier is @c Cardinality. */
export template <typename A, typename B>
  requires NatZedPair<A, B>
constexpr auto operator&(const A& lhs, const B& rhs) {
  using L = typename A::logic_species;
  auto predicate = [lhs, rhs](const Cardinality& v) {
    return L::AND(lhs(v), rhs(lift_cardinality_to_signed(v)));
  };
  return Comprehension<𝔸<Cardinality, L>, decltype(predicate)>{predicate};
}

/** @brief Symmetric: a set over ℤ @c & a set over ℕ delegates to the
 *  canonical direction; the result still tightens to ℕ. */
export template <typename A, typename B>
  requires NatZedPair<B, A>
constexpr auto operator&(const A& lhs, const B& rhs) {
  return rhs & lhs;
}

namespace detail {
/** @brief Helpers for the partial inverse of @c
 *         lift_cardinality_to_signed (the canonical ℕ ↪ ℤ embedding).
 *
 *  @c sc_is_in_natural_image(v) returns whether @c v lies in the
 *  image of ℕ inside @c SignedCardinality — equivalently, whether
 *  @c v is neither negative finite, @c -ℵ_0, nor @c NaZ.  When that
 *  predicate holds, @c project_signed_to_natural(v) projects @c v
 *  back to @c Cardinality.  Calling @c project_signed_to_natural on
 *  values outside the image violates its precondition.
 *
 *  This is the operational shadow of "ℕ ⊂ ℤ as a partial reverse":
 *  the injection ℕ ↪ ℤ has a partial inverse on the non-negative
 *  fragment of ℤ.  @c std::optional is intentionally not used here —
 *  the predicate-then-project split keeps the cross-carrier @c
 *  operator| below in pure @c constexpr-friendly territory and lets
 *  the predicate be reused on the @c Set's branching choice. */
constexpr bool sc_is_negative_finite(const SignedCardinality& v) noexcept {
  if (!std::holds_alternative<SignedExtensionalCardinal<>>(v)) return false;
  return std::get<SignedExtensionalCardinal<>>(v).negative();
}

constexpr bool sc_is_in_natural_image(const SignedCardinality& v) noexcept {
  if (std::holds_alternative<NaZ>(v)) return false;
  if (std::holds_alternative<NegativeInfinity>(v)) return false;
  if (sc_is_negative_finite(v)) return false;
  return true;
}

constexpr Cardinality project_signed_to_natural(
    const SignedCardinality& v) noexcept {
  // Pre: sc_is_in_natural_image(v) holds.
  if (std::holds_alternative<PositiveInfinity>(v)) {
    return Cardinality{ℵ_0{}};
  }
  return Cardinality{std::get<SignedExtensionalCardinal<>>(v).magnitude()};
}
}  // namespace detail

/** @brief Cross-carrier join: a set over ℕ @c | a set over ℤ @c → a set over
 *  ℤ (carrier widening; closes a slice of #362).  The union may contain
 *  negative integers from the @c rhs side, so the result carrier widens to ℤ.
 *
 *  Membership for @c v @c : @c ℤ:
 *    * If @c v lives in the image of ℕ ↪ ℤ (non-negative, not @c NaZ,
 *      not @c -ℵ_0), evaluate @c lhs on the projected ℕ value and OR
 *      with @c rhs(v).
 *    * Otherwise @c v is not in ℕ, so @c lhs(v) is structurally @c
 *      false; the union reduces to @c rhs(v). */
export template <typename A, typename B>
  requires NatZedPair<A, B>
constexpr auto operator|(const A& lhs, const B& rhs) {
  using L = typename A::logic_species;
  auto predicate = [lhs, rhs](const SignedCardinality& v) {
    if (detail::sc_is_in_natural_image(v)) {
      return L::OR(lhs(detail::project_signed_to_natural(v)), rhs(v));
    }
    return rhs(v);
  };
  return Comprehension<𝔸<SignedCardinality, L>, decltype(predicate)>{predicate};
}

/** @brief Symmetric: a set over ℤ @c | a set over ℕ delegates to the
 *  canonical direction; the result still widens to ℤ. */
export template <typename A, typename B>
  requires NatZedPair<B, A>
constexpr auto operator|(const A& lhs, const B& rhs) {
  return rhs | lhs;
}

/**
 * @brief Membership predicate of a Cartesian product: @f$(x,y)\in A\times B
 *        \iff x\in A \wedge y\in B@f$.
 *
 * @details A @b named functor (not a capturing lambda), so a product @c Set
 * carries a structural, comparable predicate type rather than an opaque closure
 * (#844, [[feedback_no_lambdas_opacity]]).  It stores the two component @c Set
 * operands and evaluates each on its projection, going through @c
 * Set::operator() so the logic lift is preserved.  Mirrors the @c
 * dedekind::relational::RelAnd shape, which already ships as a @c Set<pair>
 * predicate across translation units.
 */
export template <typename A, typename B>
struct ProductMembership {
  A a;
  B b;
  /** @brief The product's species: the join of the factors'.  Each factor's
   *  answer is lifted into it and the conjunction is that species' @c AND, so
   *  a Kleene factor keeps @c Unknown and a @c Chain<int> factor keeps its
   *  grade (the carrier's own @c && would collapse both to @c bool). */
  using logic_species =
      join_logic_t<typename A::logic_species, typename B::logic_species>;
  template <typename P>
  constexpr typename logic_species::Ω operator()(const P& p) const {
    using L = logic_species;
    return L::AND(dedekind::category::lift_logic<L>(a(p.first)),
                  dedekind::category::lift_logic<L>(b(p.second)));
  }
};

// The cartesian-product cardinality is the JOIN of the factors on the lattice
// Finite < ℵ<0> < ℵ<1> < …: |A×B| = |A|·|B| = max(|A|,|B|) for infinite
// factors, Finite only when both are finite.  The primary is left INCOMPLETE
// (no @c type): a cardinality pair outside the known lattice (Finite / ℵ<N>) is
// HONESTLY REJECTED at compile time rather than silently downgraded to ℵ_0 ---
// a blanket fallback would misclassify e.g. a custom uncountable tag × Finite
// as countable.
/** @brief The cardinality class of a set object's universe. */
template <typename U>
struct universe_cardinality;
template <typename T, typename L, typename C>
struct universe_cardinality<𝔸<T, L, C>> {
  using type = C;
};
template <typename CA, typename CB>
struct product_cardinality;
template <>
struct product_cardinality<Finite, Finite> {
  using type = Finite;
};
template <std::size_t N>
struct product_cardinality<Finite, ℵ<N>> {
  using type = ℵ<N>;
};
template <std::size_t N>
struct product_cardinality<ℵ<N>, Finite> {
  using type = ℵ<N>;
};
template <std::size_t M, std::size_t N>
struct product_cardinality<ℵ<M>, ℵ<N>> {
  using type = ℵ<(M > N ? M : N)>;
};

/**
 * @brief Cartesian product of two sets: {(a,b) | a ∈ A, b ∈ B}.
 *
 * Constructs a comprehension over 𝔸<pair<T1,T2>> whose membership
 * predicate checks element-wise membership in both component sets.
 */
export template <typename T1, typename L1, typename P1, typename T2,
                 typename L2, typename P2, typename C1, typename C2>
  requires dedekind::category::HaveLogicJoin<
      typename Comprehension<𝔸<T1, L1, C1>, P1>::logic_species,
      typename Comprehension<𝔸<T2, L2, C2>, P2>::logic_species>
constexpr auto cartesian_product(const Comprehension<𝔸<T1, L1, C1>, P1>& a,
                                 const Comprehension<𝔸<T2, L2, C2>, P2>& b) {
  // Dispatch and reconcile on the factors' EFFECTIVE species (a comprehension
  // may answer above its base's tag); the product lives in their join.
  using Pair = std::pair<T1, T2>;
  using Pred = ProductMembership<Comprehension<𝔸<T1, L1, C1>, P1>,
                                 Comprehension<𝔸<T2, L2, C2>, P2>>;
  using CC = typename product_cardinality<C1, C2>::type;
  return Comprehension<𝔸<Pair, typename Pred::logic_species, CC>, Pred>{
      Pred{a, b}};
}

/** @brief @f$\mathbb{A}_A \times \mathbb{A}_B = \mathbb{A}_{A\times B}@f$: the
 *  product of two universes is the universe over the pair carrier, in the
 *  join of the factors' species. */
export template <typename A, typename LA, typename CA, typename B, typename LB,
                 typename CB>
  requires dedekind::category::HaveLogicJoin<LA, LB>
constexpr auto cartesian_product(const 𝔸<A, LA, CA>& a, const 𝔸<B, LB, CB>& b) {
  using CC = typename product_cardinality<CA, CB>::type;
  using U = std::remove_cvref_t<decltype(finalize_combine(
      𝔸<std::pair<A, B>, join_logic_t<LA, LB>, CC>{}))>;
  // The universe over the pair only where its projections give the factors
  // back exactly; a factor whose cardinality bound or species the pair
  // universe cannot carry keeps its leg (#970: π_k return the factors).
  if constexpr (std::same_as<
                    std::remove_cvref_t<decltype(dedekind::category::π_1(
                        std::declval<const U&>()))>,
                    𝔸<A, LA, CA>> &&
                std::same_as<
                    std::remove_cvref_t<decltype(dedekind::category::π_2(
                        std::declval<const U&>()))>,
                    𝔸<B, LB, CB>>) {
    return U{};
  } else {
    using Pred = ProductMembership<𝔸<A, LA, CA>, 𝔸<B, LB, CB>>;
    return Comprehension<𝔸<std::pair<A, B>, typename Pred::logic_species, CC>,
                         Pred>{Pred{a, b}};
  }
}

/**
 * @brief Cartesian product over arbitrary set objects.
 *
 * Normalises each operand to a plain set over its universe and delegates to
 * the plain × plain product, which reconciles the species at their join.
 */
export template <IsSetObject A, IsSetObject B>
constexpr auto cartesian_product(const A& a, const B& b) {
  // The operands are kept as given, so π_1 / π_2 return them (#970): the
  // membership evaluates each factor's χ and lifts it into the join of their
  // species; the carrier is the pair of their carriers and the cardinality
  // the join of their universes'.
  using Pred = ProductMembership<A, B>;
  using Pair = std::pair<typename A::Domain, typename B::Domain>;
  using CC = typename product_cardinality<
      typename universe_cardinality<
          std::remove_cvref_t<decltype(universe(a))>>::type,
      typename universe_cardinality<
          std::remove_cvref_t<decltype(universe(b))>>::type>::type;
  return Comprehension<𝔸<Pair, typename Pred::logic_species, CC>, Pred>{
      Pred{a, b}};
}

/**
 * @brief The @b universal set of products: @f$\mathbb{A}_A \times \mathbb{A}_B
 * =
 *        \mathbb{A}_{A\times B}@f$, spelled as the universe over the product
 *        carrier (@c IsProduct).
 *
 * @details Two @b total factors carry no restriction to lift, so the product
 * @b is the pure product universe @c 𝔸<pair<A,B>> --- not a refinement of it.
 * This is the base case of the cylinder decomposition @f$A\times B =
 * \pi_1^{-1}(A)\cap\pi_2^{-1}(B)@f$: with @c A, @c B universal both cylinders
 * are the whole universe, so their intersection is too.  A @b restricted
 * factor instead lifts its predicate onto its axis (@c dedekind.order:
 * @c π_I over a halfspace), refining this universe into a proper subobject.
 * The old melting form (@c pa(first) @c && @c pb(second) captured in an
 * anonymous closure) discarded the factor structure; keeping the universe
 * explicit lets @c dom / @c cod read the factors back.
 */
export template <typename A, typename LA, typename CA, typename B, typename LB,
                 typename CB>
  requires std::same_as<LA, LB>
constexpr auto operator*(const 𝔸<A, LA, CA>& a, const 𝔸<B, LB, CB>& b) {
  return cartesian_product(a, b);
}

/** @brief Infix sugar for cartesian product over sets. */
export template <typename T1, typename L1, typename P1, typename T2,
                 typename L2, typename P2, typename C1, typename C2>
  requires std::same_as<L1, L2>
constexpr auto operator*(const Comprehension<𝔸<T1, L1, C1>, P1>& a,
                         const Comprehension<𝔸<T2, L2, C2>, P2>& b) {
  return cartesian_product(a, b);
}

/** @brief Infix sugar for cartesian product over ambient species values. */
export template <IsSetObject A, IsSetObject B>
constexpr auto operator*(const A& a, const B& b) {
  return cartesian_product(a, b);
}

using CanonicalIntSet = 𝔸<int>;
using CanonicalIntProductSet =
    decltype(cartesian_product(std::declval<const CanonicalIntSet&>(),
                               std::declval<const CanonicalIntSet&>()));
using CanonicalIntProductDomain = typename CanonicalIntProductSet::Domain;

static_assert(
    dedekind::category::IsProduct<CanonicalIntProductDomain, int, int>,
    "sets::cartesian_product must expose a std::pair product domain.");
// The product of two set objects is a set object (today's witness) ...
static_assert(IsSetObject<CanonicalIntProductSet>,
              "A × B is a set object over the pair carrier.");
// Mixed-species nodes: the complement of a Boole universe and of a Kleene one
// (the continuum universes carry Kleene); the product reconciles at the join
// (Kleene) instead of re-entering the generic overload.
static_assert(
    std::same_as<typename std::remove_cvref_t<decltype(cartesian_product(
                     Not<𝔸<int>>{𝔸<int>{}},
                     Not<𝔸<double, Kleene, ℶ_1>>{
                         𝔸<double, Kleene, ℶ_1>{}}))>::logic_species,
                 Kleene>,
    "a Boole × Kleene product lands in the join species.");
static_assert(cartesian_product(Not<𝔸<int>>{𝔸<int>{}},
                                Not<𝔸<double, Kleene, ℶ_1>>{
                                    𝔸<double, Kleene, ℶ_1>{}})(std::pair{
                  0, 0.0}) == Ternary::False,
              "(0, 0.0) ∉ ¬𝔸 × ¬𝔸: both complements are empty.");

/** @brief The product of two set objects is their categorical product, not
 *  only their carriers' (#970, #1039): @c π_1 / @c π_2 on @c A @c × @c B
 *  return the factor set objects, which the membership already carries.
 *  With the carrier-level @c IsProduct above and the universes' (@c
 *  boundaries), @c IsProduct<decltype(A * B), A, B> holds for set objects.
 *  @param s the product.  @return the first / second factor. */
export template <typename T1, typename T2, typename L, typename C, typename A,
                 typename B>
constexpr auto component(
    const Comprehension<𝔸<std::pair<T1, T2>, L, C>, ProductMembership<A, B>>& s,
    dedekind::category::At<1>) {
  return s.predicate.a;
}
export template <typename T1, typename T2, typename L, typename C, typename A,
                 typename B>
constexpr auto component(
    const Comprehension<𝔸<std::pair<T1, T2>, L, C>, ProductMembership<A, B>>& s,
    dedekind::category::At<2>) {
  return s.predicate.b;
}

namespace detail_product_witness {
inline constexpr auto t = 𝔸<bool>{} & (π == true);
inline constexpr auto f = 𝔸<bool>{} & (π == false);
using TF = std::remove_cvref_t<decltype(t * f)>;
static_assert(
    dedekind::category::IsProduct<TF, std::remove_cvref_t<decltype(t)>,
                                  std::remove_cvref_t<decltype(f)>>,
    "A × B is the product of the set objects A and B (#970)");
static_assert(dedekind::category::π_1(t* f)(true) &&
                  !dedekind::category::π_1(t * f)(false) &&
                  dedekind::category::π_2(t * f)(false),
              "π_1 / π_2 give the factors back, as sets");
}  // namespace detail_product_witness

/** @section expressions__Sums
 *  A binary operation over set objects on different carriers lands in their
 *  @b sum (#1039): the ambient is the coproduct @c std::variant<X, Y> (tagged:
 *  a value is in one summand), and @f$\mathrm{Sub}(X + Y) \cong \mathrm{Sub}(X)
 *  \times \mathrm{Sub}(Y)@f$, natural in @c X, @c Y, for every species (the
 *  coproduct's universal property on @f$\chi : X + Y \to \Omega@f$).  So a
 *  set of the sum is a @b pair of sets: @c a @c | @c b is @c (a, b), its χ the
 *  copairing @f$[\chi_a, \chi_b]@f$, and its restrictions to the summands
 *  give @c a / @c b back.  The isomorphism is one of lattices: a pair of sets
 *  is no set, and the set @c a @c | @c b is, as an object, the coproduct
 *  @c a @c + @c b (@c IsCoproduct on the carrier), not a product.  @c a @c & @c
 * b meets in the same ambient: the summands are disjoint, so it is @c Ø.  The
 *  ℕ / ℤ pair keeps its widening union (@c NatZedPair): ℕ embeds in ℤ, an
 *  identification the tagged sum does not make. */

/** @brief Membership of a sum: the copairing @f$[\chi_a, \chi_b]@f$, each
 *  answer lifted into the join of the summands' species.  A named functor, as
 *  @c ProductMembership. */
export template <typename A, typename B>
struct SumMembership {
  A a;
  B b;
  using logic_species =
      join_logic_t<typename A::logic_species, typename B::logic_species>;
  template <typename V>
  constexpr typename logic_species::Ω operator()(const V& v) const {
    using L = logic_species;
    if (v.index() == 0)
      return dedekind::category::lift_logic<L>(a(std::get<0>(v)));
    return dedekind::category::lift_logic<L>(b(std::get<1>(v)));
  }
};

/** @brief Two set objects on different carriers (the ℕ / ℤ pair aside): the
 *  operands of a sum. */
template <typename A, typename B>
concept HeterogeneousPair =
    IsSetObject<A> && IsSetObject<B> &&
    (!std::same_as<typename A::Domain, typename B::Domain>) &&
    (!NatZedPair<A, B>) && (!NatZedPair<B, A>) &&
    dedekind::category::HaveLogicJoin<typename A::logic_species,
                                      typename B::logic_species>;

/** @brief Whether @c S is an empty set by its type (@c Ø). */
template <typename S>
inline constexpr bool is_empty_set_v = false;
template <typename T, typename L>
inline constexpr bool is_empty_set_v<Ø<T, L>> = true;

/** @brief The sum of two set objects: the subobject @c (a, b) of the
 *  coproduct of their carriers, @f$|a + b| = |a| + |b|@f$ (the join on the
 *  cardinal lattice, as for the product).  Reduced where the types decide it:
 *  - an empty summand (@f$a + \varnothing_Y \cong a@f$): the set lies in the
 *    image of one injection, a monic arrow, and pulls back along it without
 *    loss, so the ambient narrows to that summand's carrier (strength
 *    reduction, as @f$a \vee \bot = a@f$ carried through @f$\iota@f$);
 *  A sum of two universes stays a sum (χ ≡ ⊤ on it) and keeps its summands,
 *  so each keeps its own cardinality.
 *  @param a, b the summands. */
/** @brief The universe of the sum of two universes: the coproduct carrier,
 *  the join of the species, the join of the cardinality classes (as for the
 *  product). */
template <typename UA, typename UB>
struct sum_universe;
template <typename X, typename LA, typename CA, typename Y, typename LB,
          typename CB>
struct sum_universe<𝔸<X, LA, CA>, 𝔸<Y, LB, CB>> {
  using type = 𝔸<std::variant<X, Y>, join_logic_t<LA, LB>,
                 typename product_cardinality<CA, CB>::type>;
};
template <typename UA, typename UB>
using sum_universe_t = typename sum_universe<UA, UB>::type;
/** @brief The subobject @c (a, b) of the sum over the universe @c USum, as
 *  built: no reduction.  @c sum reduces first; the pushforward @c Inject never
 *  does. */
template <Is𝔸 USum, IsSetObject A, IsSetObject B>
constexpr auto sum_over(const A& a, const B& b) {
  using M = SumMembership<A, B>;
  return Comprehension<USum, M>{M{a, b}};
}
export template <IsSetObject A, IsSetObject B>
constexpr auto sum(const A& a, const B& b) {
  if constexpr (is_empty_set_v<B>)
    return a;
  else if constexpr (is_empty_set_v<A>)
    return b;
  else
    return sum_over<sum_universe_t<universe_t<A>, universe_t<B>>>(a, b);
}
/** @brief @c a @c | @c b on different carriers: their sum. */
export template <typename A, typename B>
  requires HeterogeneousPair<A, B>
constexpr auto operator|(const A& a, const B& b) {
  return sum(a, b);
}
/** @brief @c a @c & @c b on different carriers: the summands of a sum are
 *  disjoint, so the meet is the empty set of the sum. */
export template <typename A, typename B>
  requires HeterogeneousPair<A, B>
constexpr auto operator&(const A&, const B&) {
  return finalize_combine(
      Ø<std::variant<typename A::Domain, typename B::Domain>,
        join_logic_t<typename A::logic_species, typename B::logic_species>>{});
}
/** @brief The restriction of a set of a sum to its @c k-th summand,
 *  @f$\iota_k^{-1}(s)@f$: the forward map of the lattice isomorphism
 *  @f$\mathrm{Sub}(X + Y) \cong \mathrm{Sub}(X) \times \mathrm{Sub}(Y)@f$.
 *  A @b pair of sets is no set: the sum itself is the coproduct of its
 *  summands in Set, not their product, so it carries no @c π_k.
 *  @tparam K the summand, 1 or 2.  @param s the set of the sum. */
export template <std::size_t K, typename X, typename Y, typename L, typename C,
                 typename A, typename B>
  requires(K == 1 || K == 2)
constexpr auto restriction(
    const Comprehension<𝔸<std::variant<X, Y>, L, C>, SumMembership<A, B>>& s) {
  if constexpr (K == 1)
    return s.predicate.a;
  else
    return s.predicate.b;
}

/** @brief A relation out of a sum restricted to one summand: @f$(A + B)
 *  \times C \cong A \times C + B \times C@f$ (distributivity), so
 *  @f$(\iota_k \times \mathrm{id})^{-1}(s \times c) = \iota_k^{-1}(s) \times
 *  c@f$.  A relation @c Either<a, b> @c ⇸ @c c that only accepts @c a is the
 *  relation @c a @c ⇸ @c c.  @tparam K the summand.  @param r the relation. */
export template <std::size_t K, typename X, typename Y, typename T2, typename L,
                 typename C, typename A, typename B>
  requires(K == 1 || K == 2) && requires(const A& a) { restriction<K>(a); }
constexpr auto restriction(
    const Comprehension<𝔸<std::pair<std::variant<X, Y>, T2>, L, C>,
                        ProductMembership<A, B>>& r) {
  return restriction<K>(r.predicate.a) * r.predicate.b;
}

/** @brief @f$\iota_K^{-1}@f$ is a lattice homomorphism (a pullback preserves
 *  @f$\wedge, \vee, \neg, \bot, \top@f$): the restriction reaches through the
 *  lattice's nodes to the sums beneath, and the boundaries restrict to the
 *  boundaries --- the universe of the sum to the universe of the summand (the
 *  sum's cardinality class kept: a sound bound, the summand's own is not
 *  recorded on a bare universe), @c Ø to @c Ø.  @tparam K the summand. */
export template <std::size_t K, typename X, typename Y, typename L, typename C>
  requires(K == 1 || K == 2)
constexpr auto restriction(const 𝔸<std::variant<X, Y>, L, C>&) {
  return 𝔸<std::conditional_t<K == 1, X, Y>, L, C>{};
}
export template <std::size_t K, typename X, typename Y, typename L>
  requires(K == 1 || K == 2)
constexpr auto restriction(const Ø<std::variant<X, Y>, L>&) {
  return Ø<std::conditional_t<K == 1, X, Y>, L>{};
}
export template <std::size_t K, typename A>
  requires(K == 1 || K == 2) && requires(const A& a) { restriction<K>(a); }
constexpr auto restriction(const dedekind::category::Not<A>& n) {
  return ~restriction<K>(n.base);
}
export template <std::size_t K, typename A, typename B>
  requires(K == 1 || K == 2) && requires(const A& a, const B& b) {
    restriction<K>(a);
    restriction<K>(b);
  }
constexpr auto restriction(const dedekind::category::Meet<A, B>& m) {
  return restriction<K>(m.lhs) & restriction<K>(m.rhs);
}
export template <std::size_t K, typename A, typename B>
  requires(K == 1 || K == 2) && requires(const A& a, const B& b) {
    restriction<K>(a);
    restriction<K>(b);
  }
constexpr auto restriction(const dedekind::category::Join<A, B>& j) {
  return restriction<K>(j.lhs) | restriction<K>(j.rhs);
}

/** @section expressions__Sum_Adjunction
 *  The two arrows of a sum, on the objects @f$\mathrm{Sub}(X, L)@f$ and
 *  @f$\mathrm{Sub}(X + Y, L)@f$ (@c Subobjects, @c :boundaries; @c L the sum's
 *  logic): the pushforward along the injection, @f$\exists_{\iota_K} : a
 *  \mapsto \iota_K[a]@f$ (@c Inject), and the pullback @f$\iota_K^{-1}@f$
 *  (@c Restrict).  Both monotone, and @f$\exists_{\iota_K} \dashv
 *  \iota_K^{-1}@f$: a Galois connection, witnessed through
 *  @c category::IsGaloisConnection on these objects --- the arrows carry their
 *  certificates (monotone, adjoint), read by member discovery.  The unit
 *  @f$\iota_K^{-1}\,\iota_K[a] = a@f$ holds @b by @b type (@f$\iota_K@f$ is
 *  monic, @c category::Injection); the counit @f$\iota_K[\iota_K^{-1} s]
 *  \subseteq s@f$ is witnessed at points. */
export template <std::size_t K, Is𝔸 UA, Is𝔸 UB>
  requires(K == 1 || K == 2)
struct Restrict;

/** @brief @f$\exists_{\iota_K}@f$: a set of the summand as the set of the sum
 *  that is it on its summand and empty on the other, @b kept as a sum (the
 *  strength reduction @c a @c | @c Ø @c → @c a is the inverse direction).  The
 *  left adjoint of @c Restrict.  @tparam K the summand.  @tparam UA, UB the
 *  summands' universes. */
export template <std::size_t K, Is𝔸 UA, Is𝔸 UB>
  requires(K == 1 || K == 2)
struct Inject {
  using USum = sum_universe_t<UA, UB>;
  using L = typename USum::logic_species;
  using Domain =
      Subobjects<typename std::conditional_t<K == 1, UA, UB>::Domain, L>;
  using Codomain = Subobjects<typename USum::Domain, L>;
  template <typename S>
    requires dedekind::category::Presents<S, Domain>
  constexpr auto operator()(const S& s) const {
    // Over the input's own universe: its cardinality bound is kept.
    if constexpr (K == 1)
      return sum_over<sum_universe_t<universe_t<S>, UB>>(
          s, Ø<typename UB::Domain, L>{});
    else
      return sum_over<sum_universe_t<UA, universe_t<S>>>(
          Ø<typename UA::Domain, L>{}, s);
  }
};
/** @brief @f$\iota_K^{-1}@f$ as an arrow: the restriction, lifted into the
 *  sum's logic (a summand given in a smaller logic sits in the sum's by
 *  @f$\Sigma \hookrightarrow \Omega@f$).  The right adjoint of @c Inject.
 *  Defined on every presentation @c restriction reaches: the sums, their
 *  lattice nodes, the boundaries.  @tparam K the summand.  @tparam UA, UB the
 *  summands' universes. */
export template <std::size_t K, Is𝔸 UA, Is𝔸 UB>
  requires(K == 1 || K == 2)
struct Restrict {
  using USum = sum_universe_t<UA, UB>;
  using L = typename USum::logic_species;
  using Domain = Subobjects<typename USum::Domain, L>;
  using Codomain =
      Subobjects<typename std::conditional_t<K == 1, UA, UB>::Domain, L>;
  template <typename S>
    requires dedekind::category::Presents<S, Domain> &&
             requires(const S& s) { restriction<K>(s); }
  constexpr auto operator()(const S& s) const {
    return lift_to<L>(restriction<K>(s));
  }
};

}  // namespace dedekind::sets
namespace dedekind::category {
/** @brief The pushforward and the pullback along an injection preserve
 *  inclusion, and @f$\exists_{\iota_K} \dashv \iota_K^{-1}@f$: @f$\iota[a]
 *  \subseteq s \iff a \subseteq \iota^{-1}(s)@f$, the image--preimage
 *  adjunction, for every sum; @f$\iota@f$ monic makes the unit the identity. */
template <std::size_t K, typename UA, typename UB>
struct is_monotone<dedekind::sets::Inject<K, UA, UB>, std::less_equal<>>
    : std::true_type {};
template <std::size_t K, typename UA, typename UB>
struct is_monotone<dedekind::sets::Restrict<K, UA, UB>, std::less_equal<>>
    : std::true_type {};
template <std::size_t K, typename UA, typename UB>
struct is_adjoint<dedekind::sets::Inject<K, UA, UB>,
                  dedekind::sets::Restrict<K, UA, UB>, std::less_equal<>>
    : std::true_type {};
}  // namespace dedekind::category
namespace dedekind::sets {
namespace detail_sum_witness {
using dedekind::category::Injections;
inline constexpr auto t = 𝔸<bool>{} & (π == true);
inline constexpr auto three = 𝔸<int>{} & (π == 3);
inline constexpr auto s = t | three;
using S = std::remove_cvref_t<decltype(s)>;
using In = Injections<bool, int>;
using USum = sum_universe_t<𝔸<bool>, 𝔸<int>>;
static_assert(std::same_as<typename S::Domain, std::variant<bool, int>>,
              "a | b over 𝔹 and int lives on 𝔹 + int");
static_assert(s(In::inl(true)) && !s(In::inl(false)) && s(In::inr(3)) &&
                  !s(In::inr(4)),
              "χ of a sum is the copairing [χ_a, χ_b]");
static_assert(
    dedekind::category::IsCoproduct<typename S::Domain, bool, int, In>,
    "the carrier is the coproduct, by its named injections");
static_assert(
    restriction<1>(s)(true) && !restriction<1>(s)(false) &&
        restriction<2>(s)(3) && !restriction<2>(s)(4) &&
        !dedekind::category::IsProduct<S, std::remove_cvref_t<decltype(t)>,
                                       std::remove_cvref_t<decltype(three)>>,
    "the restrictions give the summands back (Sub(X + Y) ≅ "
    "Sub(X) × Sub(Y)); the sum itself is no product of them");
static_assert(!(t & three)(In::inl(true)) && !(t & three)(In::inr(3)),
              "the summands are disjoint: a & b across carriers is Ø");
// Strength reduction: an empty summand narrows the ambient.
static_assert(std::same_as<std::remove_cvref_t<decltype(t | Ø<int>{})>,
                           std::remove_cvref_t<decltype(t)>>,
              "a + Ø ≅ a: the ambient narrows to a's carrier");
// The operands are kept as given: a lifted (Kleene) factor or summand comes
// back as itself, with its species; a sum of universes keeps its summands,
// each with its own cardinality.
inline constexpr auto tk = lift_to<Kleene>(t);
using TK = std::remove_cvref_t<decltype(tk)>;
static_assert(
    std::same_as<std::remove_cvref_t<decltype(dedekind::category::π_1(tk * t))>,
                 TK> &&
        std::same_as<std::remove_cvref_t<decltype(restriction<1>(tk | three))>,
                     TK> &&
        std::same_as<std::remove_cvref_t<decltype(restriction<1>(
                         𝔸<double, Kleene, ℶ_1>{} | 𝔸<bool>{}))>,
                     𝔸<double, Kleene, ℶ_1>>,
    "factors and summands come back as given");
static_assert(HasDecidableMembership<std::remove_cvref_t<decltype(tk & three)>>,
              "the disjoint meet is finalized: Ø of the sum is decidable");
// A relation out of a sum restricted to a summand (distributivity).
inline constexpr auto seven = 𝔸<int>{} & (π == 7);
inline constexpr auto r = (t | three) * seven;
static_assert(
    std::same_as<
        typename std::remove_cvref_t<decltype(restriction<1>(r))>::Domain,
        std::pair<bool, int>> &&
        restriction<1>(r)(std::pair{true, 7}) &&
        !restriction<1>(r)(std::pair{false, 7}) &&
        !restriction<1>(r)(std::pair{true, 8}) &&
        restriction<2>(r)(std::pair{3, 7}),
    "Either<a, b> ⇸ c restricts to a ⇸ c and b ⇸ c");
static_assert(
    std::same_as<
        typename std::remove_cvref_t<decltype((t | Ø<int>{}) * seven)>::Domain,
        std::pair<bool, int>>,
    "a relation out of a + Ø is one out of a");
static_assert(std::same_as<typename std::remove_cvref_t<decltype(restriction<1>(
                               (𝔸<bool>{} | 𝔸<int>{}) * 𝔸<char>{}))>::Domain,
                           std::pair<bool, char>>,
              "a relation out of a sum of universes restricts too");
// The sum's arrows and their adjunction, through the library's own concepts.
using In1 = Inject<1, 𝔸<bool>, 𝔸<int>>;
using Re1 = Restrict<1, 𝔸<bool>, 𝔸<int>>;
static_assert(dedekind::category::IsArrow<In1> &&
                  dedekind::category::IsArrow<Re1> &&
                  dedekind::category::IsGaloisConnection<In1, Re1>,
              "∃_ι ⊣ ι⁻¹ on Sub(𝔹) and Sub(𝔹 + int): a Galois connection "
              "between objects presented by many types");
static_assert(std::same_as<std::remove_cvref_t<decltype(Re1{}(In1{}(t)))>,
                           std::remove_cvref_t<decltype(t)>>,
              "the unit ι⁻¹ ι[a] = a, by type: ι is monic");
static_assert(In1{}(Re1{}(s))(In::inl(true)) &&
                  !In1{}(Re1{}(s))(In::inl(false)) &&
                  !In1{}(Re1{}(s))(In::inr(3)) && s(In::inr(3)),
              "the counit ι[ι⁻¹ s] ⊆ s, proper where s has a second summand");
static_assert(In1{}(t)(In::inl(true)) && !In1{}(t)(In::inr(3)),
              "ι[a] is a on its summand and empty on the other");
// ι⁻¹ through the lattice's nodes and at the boundaries.
static_assert(!Re1{}(~s)(true) && Re1{}(~s)(false) && Re1{}(s & s)(true) &&
                  Re1{}(s | ~s)(true) &&
                  dedekind::category::Presents<decltype(Re1{}(USum{})),
                                               Subobjects<bool>> &&
                  !Re1{}(Ø<std::variant<bool, int>>{})(true),
              "ι⁻¹ is a lattice homomorphism, Ø to Ø, the universe to the "
              "universe");
// The universe product keeps a factor its pair universe cannot carry (#970).
using FiniteInt = 𝔸<int, Boole, Finite>;
static_assert(
    dedekind::category::IsProduct<
        std::remove_cvref_t<decltype(FiniteInt{} * 𝔸<bool>{})>, FiniteInt,
        𝔸<bool>> &&
        dedekind::category::IsProduct<
            std::remove_cvref_t<decltype(𝔸<int, Kleene>{} * 𝔸<bool, Kleene>{})>,
            𝔸<int, Kleene>, 𝔸<bool, Kleene>> &&
        Is𝔸<std::remove_cvref_t<decltype(𝔸<int>{} * 𝔸<bool>{})>>,
    "π_k of a universe product give the factors back: collapsed to the pair "
    "universe only where that is exact");
// The pushforward keeps its input's cardinality bound.
static_assert(
    std::same_as<universe_t<std::remove_cvref_t<
                     decltype(Inject<1, 𝔸<double, Kleene>, 𝔸<bool, Kleene>>{}(
                         𝔸<double, Kleene, ℶ_1>{}))>>,
                 𝔸<std::variant<double, bool>, Kleene, ℶ_1>>,
    "ι[a] over the continuum stays over the continuum");
}  // namespace detail_sum_witness

}  // namespace dedekind::sets
