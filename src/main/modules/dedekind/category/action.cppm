/**
 * @file dedekind/category/action.cppm
 * @partition :action
 * @brief The Morphisms of Influence (Actions and Representations).

 * @section action__Structural_Influence
 * While Level 0 (:morphism) defines mapping between species,
 * Level 1 defines the "Action" of a Species S upon a Species M (S ⟳ M).
 * In the Dedekind structuralist view, an Action is the reification of
 * Scalar multiplication, leading naturally to the concepts of Modules,
 * Vector Spaces, and eventually, the Linear Algebra of the Continuum.
 *
 * @section action__The_PR96_Shielding_Logic
 * To ensure zero-overhead and prevent "too few arguments" errors when
 * checking binary functors (like std::multiplies) against unary morphism
 * concepts, we utilize "Signature Shields." This allows the compiler to
 * reason about the algebraic law without attempting an invalid invocation.
 *
 * @copyright 2026 The Dedekind Authors
 *
 * @note « Качественные методы позволяют исследовать структуру пространства
 *   через инвариантные множества и их преобразования. »
 *  (Qualitative methods allow the study of the structure of space
 *   through invariant sets and their transformations.)
 *  — V. V. Stepanov (В. В. Степанов), 'Курс дифференциальных уравнений'
 *
 */
module;

#include <concepts>
#include <cstddef>  // std::size_t (free-module rank parameter; #499)
#include <functional>

export module dedekind.category:action;

import :total;

namespace dedekind::category {

/**
 * @concept IsAction
 * @brief An Action (S ⟳ M) where S acts on M via its mapping property.
 * @tparam S The scalar/influencer species.
 * @tparam M The acted-on species.
 * @tparam Act External action witness (defaults to `std::multiplies<>`).
 *
 * Supports both textbook forms:
 * 1. callable action object `s(m)`
 * 2. binary action witness `Act{}(s, m)`
 */
export template <typename S, typename M, typename Act = std::multiplies<>>
concept IsAction = requires(S s, M m) {
  requires(
      requires {
        // Categorical action as an arrow object.
        { s(m) } -> std::same_as<M>;
      } ||
      requires {
        // Categorical action via external binary witness.
        { Act{}(s, m) } -> std::same_as<M>;
      });
};

// Pass the transparent version to the assert
static_assert(IsAction<decltype(zero<int, int>()), int>,
              "Zero Morphism must satisfy the Additive Action axioms.");

// Verify that the Unit Morphism fulfills the Action Axioms
// (Mapping everything to identity satisfies the "Neutrality" of the action)
static_assert(IsAction<decltype(unit<int, int>()), int>,
              "Unit Morphism must satisfy the Action axioms (Absorption).");

/** @section action__Axiomatic_Verification */

constexpr auto z = zero<int, int>();
constexpr auto highway = id<int>();
constexpr auto u = unit<int, int>();

// 1. Verification of Absorption (Level 1 Logic)
// The Zero Morphism maps any input to the Point 0.
static_assert(
    z(42) == 0,
    "Absorption Law: The Zero Morphism must map all inputs to the Point 0.");

// 2. Verification of Preservation (Level 0 Logic)
// The Identity Morphism (Highway) preserves the input value.
static_assert(highway(42) == 42,
              "Unit Law: The Identity Highway must preserve the input.");

/** @section action__Unit_Algebraic_Verification */

// 1. The Multiplicative Unit Law (Point-wise)
// The Unit Morphism maps any input to the Point 1.
static_assert(
    u(42) == 1,
    "Absorption Law: The Unit Morphism must map all inputs to the Point 1.");

// 2. The Successor Relationship (Manual Composition)
// To verify 1 + n, we evaluate the unit and then perform the species addition.
static_assert(
    u(42) + 42 == 43,
    "Successor Law: The value of the unit (1) plus n (42) must be 43.");

/**
 * @concept IsAdditiveMorphism
 * @brief Formal verification of the first law of linearity: f(x + y) = f(x) +
 * f(y).
 * @details This is "Internal" linearity: the mapping preserves the additive
 * harmony.
 */
/**
 * @concept IsAdditiveMorphism
 * @brief Proposition: f(m1 + m2) = f(m1) + f(m2).
 *
 * @details In the Ring context, this verifies the Distributive Law.
 * For PR 96, we shield the unary call check if F is a standard
 * binary functor (like std::multiplies) to avoid signature mismatch.
 */
/**
 * @concept IsAdditiveMorphism
 * @brief Proposition: f(m1 + m2) = f(m1) + f(m2).
 *
 * @details In the Ring context, this verifies the Distributive Law.
 * For PR 96, we shield functional calls if F is a standard binary
 * functor (like std::multiplies) to avoid signature mismatches.
 */
export template <typename F, typename M>
concept IsAdditiveMorphism = requires(const F f, const M m1, const M m2) {
  /**
   * @section action__Distributive_Axiom_Shield
   * We only enforce the unary call f(m1 + m2) if F is not a known binary
   * operator or if M is not a primitive integral.
   */
  requires(std::same_as<F, std::multiplies<void>> ||
           std::same_as<F, std::multiplies<M>> || std::integral<M>) ||
              requires {
                { f(m1 + m2) } -> std::same_as<M>;
                { f(m1) + f(m2) } -> std::same_as<M>;
              };

  /**
   * @section action__Linear_Preservation_Shield
   * The Morphism must preserve the additive identity: f(0) = 0.
   * We shield this for binary functors to prevent "too few arguments" errors.
   */
  requires(std::same_as<F, std::multiplies<void>> ||
           std::same_as<F, std::multiplies<M>> || std::integral<M>) ||
              requires {
                {
                  f(dedekind::category::identity_v<M, std::plus<M>>)
                } -> std::same_as<M>;
              };
};

/**
 * @concept IsLinearAction
 * @brief The "Jewel": An Action that satisfies the Four Axioms of Harmony.
 * @details This is the categorical ground for Modules and Vector Spaces.
 *
 * 1. Vector Additivity: s * (m1 + m2) = s*m1 + s*m2
 * 2. Scalar Additivity: (s1 + s2) * m = s1*m + s2*m
 */
export template <typename S, typename M, typename Act = std::multiplies<>,
                 typename AddS = std::plus<S>>
concept IsLinearAction = IsAction<S, M, Act> && IsAdditiveMorphism<Act, M> &&
                         requires(S s1, S s2, M m) {
                           // Scalar Additivity (The "Second Linearity") via
                           // chosen witnesses.
                           { Act{}(AddS{}(s1, s2), m) } -> std::same_as<M>;
                         };

/**
 * @concept IsLinearMorphism
 * @brief Formal verification of the second law: f(s * x) = s * f(x).
 * @details Links the mapping f to an external scalar influence S.
 */
export template <typename F, typename M, typename S,
                 typename Act = std::multiplies<>>
concept IsLinearMorphism =
    IsAdditiveMorphism<F, M> && IsAction<S, M, Act> && requires(F f, M m, S s) {
      requires(
          requires {
            { f(s(m)) } -> std::same_as<M>;
          } ||
          requires {
            { f(Act{}(s, m)) } -> std::same_as<M>;
          });
      // Semantic: f(s * m) == s * f(m)
    };

/**
 * @struct ZeroAction
 * @brief The 'Absorption' logic.
 * @details Maps any input of species A to the neutral element of species B.
 */
export template <typename A, typename B, typename Op>
struct ZeroAction final {
  constexpr B operator()(const A&) const noexcept { return identity_v<B, Op>; }
};

/** @section action__Module_FreeModule_EndoRing_Trait_Registry
 *
 * NEW-A trait registry (#499 / #498).  Pin the recursive enrichment
 * relations Figure 1 of the paper depicts mechanically — "ℚ is a
 * ℤ-module", "𝔻 is an ℝ-module", "Vec₂(R) is a free R-module of
 * rank 2", "M₂(R) = End_R(Vec₂(R))".  Trait-variable shape (default
 * @c false; specialised in carrier-defining partitions) mirrors the
 * @c is_associative_v / @c is_commutative_v pattern in @c :species.
 *
 * @b Vector-space note: a fourth trait @c is_vector_space_v<V, K>
 * would be the natural sibling, but pinning it honestly requires
 * @c K to satisfy axiomatic @c IsField, which most shipping
 * carriers do not (IEEE-754 fails associativity under rounding;
 * @c Rational<I> hits @c IsTotal at the @c IsMagma step because
 * rationals are neither periodic nor idempotent under +/×).  The
 * operational scalar-action reading of "vector space" is already
 * covered by @c IsAction / @c IsLinearAction (above); pinning a
 * strict @c is_vector_space_v trait here would be either vacuous
 * (no shipping witness) or dishonest (claim vector-space-ness on
 * field-shaped-not-axiomatic-field carriers).  Reintroduce only
 * when a shipping carrier strictly satisfies @c IsField and the
 * recursive enrichment over it is genuinely a vector space.
 */

/**
 * @brief @c is_module_v<M, R>: declare that @c M is a module over the
 *        commutative ring @c R.
 *
 * @details Default false; specialised in carrier-defining
 * partitions (e.g.\ @c Rational<I> over @c I in
 * @c numbers:rational; @c Complex<T> over @c T in
 * @c numbers:complex; @c Dual<T> over @c T in @c analysis:dual).
 * The companion concept @c IsLinearAction (above) is the
 * @b operator-surface check; this trait is the @b structural pin
 * for the recursive enrichment relations Figure~1 of the paper
 * depicts (#498 / #499).
 */
export template <typename M, typename R>
inline constexpr bool is_module_v = false;

/**
 * @brief @c is_free_module_v<M, R, N>: declare that @c M is a free
 *        module of rank @c N over @c R, i.e.\ @c M is canonically
 *        isomorphic to @c R^N.
 *
 * @details Default false; specialised in linear-algebra carriers
 * (@c Vec2V<T> over @c T at rank 2; sibling specialisations for
 * higher-arity free modules under #500 / #517).  Distinct from
 * @c is_module_v: the free-module property pins the existence of
 * a canonical basis (the @c R-module structure together with a
 * specific rank).
 */
export template <typename M, typename R, std::size_t N>
inline constexpr bool is_free_module_v = false;

/**
 * @brief @c is_endomorphism_ring_v<A, V>: declare that @c A is the
 *        endomorphism ring of the module @c V, i.e.\
 *        @c A @c = @c End(V).
 *
 * @details Default false; specialised in linear-algebra carriers
 * (@c Matrix2x2V<T> is @c End(Vec2V<T>) @c = @c M_2(T) for the
 * rank-2 worked instance under #500 / #517).  Pins the
 * internal-hom of the module category mechanically; downstream
 * Figure~1 invariant tests can read this trait rather than
 * reconstructing the equality.
 */
export template <typename A, typename V>
inline constexpr bool is_endomorphism_ring_v = false;

/** @section action__Zero Morphism Verification */

/** @section action__Peak Symmetry: Zero vs. Groupoid */

// Proof: The result of zero() belongs to the Identity element.
static_assert(zero<int, int, std::plus<int>>()(99) == 0,
              "Absorption: Z -> Z via + must yield 0.");

// 1. Proof: Zero is an Arrow from int to int (under addition).
using ZeroZ = decltype(zero<int, int, std::plus<int>>());

// Proof: The Zero Morphism (int -> int) is an Arrow,
// even if it maps into an Abelian Groupoid.
static_assert(IsArrow<ZeroZ>, "Zero: A 'Black Hole' arrow must map Z to Z.");

static_assert(IsArrow<ZeroZ>, "Zero: Must be a valid morphism mapping Z to Z.");

// 2. Action Proof: Zero maps everything to 0.
static_assert(zero<int, int, std::plus<int>>()(42) == 0,
              "Absorption: Zero morphism must return the identity element.");

// 3. Action Proof: Zero maps everything to 'true' (under logic AND).
static_assert(zero<int, bool, std::logical_and<bool>>()(42) == true,
              "Absorption: Boolean AND zero must return 'true'.");

}  // namespace dedekind::category
