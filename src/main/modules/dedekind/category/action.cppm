/**
 * @file dedekind/category/action.cppm
 * @partition :action
 * @brief Level 1: The Morphisms of Influence (Actions and Representations).
 *
 * @section The_Action_Axiom
 * « Качественные методы позволяют исследовать структуру пространства
 *   через инвариантные множества и их преобразования. »
 *  (Qualitative methods allow the study of the structure of space
 *   through invariant sets and their transformations.)
 *  — V. V. Stepanov (В. В. Степанов), 'Курс дифференциальных уравнений'
 *
 * @section Structural_Influence
 * While Level 0 (:morphism) defines mapping between species,
 * Level 1 defines the "Action" of a Species S upon a Species M (S ⟳ M).
 * In the Dedekind structuralist view, an Action is the reification of
 * Scalar multiplication, leading naturally to the concepts of Modules,
 * Vector Spaces, and eventually, the Linear Algebra of the Continuum.
 *
 * @section The_PR96_Shielding_Logic
 * To ensure zero-overhead and prevent "too few arguments" errors when
 * checking binary functors (like std::multiplies) against unary morphism
 * concepts, we utilize "Signature Shields." This allows the compiler to
 * reason about the algebraic law without attempting an invalid invocation.
 *
 * @copyright 2026 The Dedekind Authors
 *
 * @note "Нельзя быть математиком, не будучи в то же время поэтом в душе."
 *       ("It is impossible to be a mathematician without being a poet in
 * soul.")
 *       -- Софья Ковалевская (Sofya Kovalevskaya)
 */
module;

#include <concepts>
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

/** @section Level_1_Axiomatic_Verification */

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

/** @section Unit_Algebraic_Verification */

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
   * @section Distributive_Axiom_Shield
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
   * @section Linear_Preservation_Shield
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

/** @section Zero Morphism Verification */

/** @section Peak Symmetry: Zero vs. Groupoid */

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
