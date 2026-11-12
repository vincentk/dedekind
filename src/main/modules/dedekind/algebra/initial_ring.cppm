/**
 * @file dedekind/algebra/initial_ring.cppm
 * @partition :initial_ring
 * @brief @c ℤ as a Form — Initial Ring and Grothendieck group of @c ℕ.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section initial_ring__Two_Universal_Properties_For_ℤ
 * The categorical identity of @c ℤ has two textbook universal-
 * property readings, both load-bearing for the project:
 *
 *   1. @b Initial @b object @b in @b Ring.  For every (commutative,
 *      unital) ring @c R there exists a unique ring homomorphism
 *      @c χ_R @c : @c ℤ @c → @c R, sending @c 1_ℤ @c ↦ @c 1_R and
 *      determined uniquely by ring-axiom preservation.  Reified as
 *      @c IsInitialRing<R>.
 *
 *   2. @b Grothendieck @b group @b of @b ℕ.  @c ℤ is the free abelian
 *      group on the commutative monoid @c (ℕ, @c +, @c 0) — the left
 *      adjoint of the forgetful functor @c Ab @c → @c CMon applied
 *      at @c ℕ.  Universal property: for every abelian group @c G and
 *      monoid homomorphism @c φ @c : @c ℕ @c → @c G, there exists a
 *      unique group homomorphism @c φ̂ @c : @c ℤ @c → @c G extending
 *      @c φ.  Reified as @c IsGrothendieckGroup<G, M>.
 *
 * @section initial_ring__Why_Reify_Both
 * The two readings are mathematically equivalent up to canonical
 * isomorphism, but they sit in @b different categories ( @c Ring vs
 * @c Ab) and bind to different operational consumers in the project:
 *
 *   * The initial-object reading is what justifies @c χ_{Modular<n>}
 *     as a unique ring homomorphism (the mod-@c n reduction).
 *   * The Grothendieck reading is what a closure-forcing operator
 *     @c ℕ @c - @c ℕ @c → @c ℤ would realise operationally.
 *
 * @c sets::IntExpr inhabits both: the formal differences of ℕ in their
 * normal form, exact above the word as ℕ's @c NatExpr is.  A machine word
 * wraps, so it is @c ℤ/2^w, not @c ℤ.
 *
 * @section initial_ring__Honesty_Obligation
 * Existence and uniqueness of the canonical homomorphisms remain the
 * engineer's honesty obligation.  C++ concepts cannot quantify
 * universally over the target ring (in @c IsInitialRing) or the
 * target abelian group (in @c IsGrothendieckGroup).  What the type
 * system pins is the @b structural shape — the carrier is a ring /
 * abelian group of the appropriate kind, plus the opt-in trait that
 * names it as the universal one.
 *
 * @section initial_ring__Architecture
 * Three-layer chain mirroring the NNO story (PR #447):
 *
 *   @c IsInitialRig  →  @c NatExpr  →  @c ℕ
 *   @c IsInitialRing, @c IsGrothendieckGroup<·, ℕ>  →  @c IntExpr  →  @c ℤ
 *
 * @section initial_ring__Bridge_to_Limit
 * @c IsInitialRing instantiates the @b same universal-property
 * pattern that @c :limit's @c IsInitialObject<Zero> instantiates,
 * just at a different categorical level — initial in the @c Ring
 * category rather than initial in @c Set.  The two readings live at
 * different levels (forgetful functor @c U @c : @c Ring @c → @c Set
 * does @b not preserve initial: @c ℤ is initial in @c Ring but its
 * underlying set is not @c Zero @c = @c std::nullptr_t).  A
 * non-vacuous structural witness of this bridge requires reifying
 * @c IsTerminalRing for the trivial / zero ring (whose carrier is
 * @c One @c = @c std::monostate from @c :limit) — filed as a sibling
 * concern; the static_assert here would be premature without the
 * dual concept.  See @c :f_algebra for the analogous bridge at the
 * F-Alg category level.
 *
 * @note "Adjoint functors arise everywhere."
 *       — Saunders Mac Lane.
 */
module;

#include <array>  // φ̂: the images of a term's nodes
#include <cstddef>
#include <cstdint>  // the word's quotient, the extension property's target
#include <functional>
#include <limits>
#include <ranges>  // φ̂: a term's nodes walked as a fold
#include <type_traits>
#include <variant>  // φ̂: a term is read by std::visit

export module dedekind.algebra:initial_ring;

import dedekind.category;
import dedekind.sets;
import :group;
import :polynomial;  // Poly<NatExpr>: ℕ[π] with honest coefficients, the
                     // witness
import :ring;

namespace dedekind::category {
// (ℕ, [Z, S]) is the initial Maybe-algebra: the opt-in the structure earns,
// Lambek holding on ℕ where it fails on every word (sets:cardinality).
template <>
inline constexpr bool is_initial_f_algebra_v<
    maybe_functor<dedekind::sets::NatExpr<>>, dedekind::sets::NatExpr<>,
    In<dedekind::sets::NatExpr<>>> = true;
}  // namespace dedekind::category

namespace dedekind::algebra {
using namespace dedekind::sets;

// ===========================================================================
// (0) Initial Rig — ℕ as the initial object in CRig (#1035).
// ===========================================================================
/** @brief Opt-in: @c R is the initial commutative rig, ℕ.  The one carrier
 *  that is: @c sets::NatExpr, exact above the word.  A word is a rig and not
 *  initial, since it wraps. */
export template <typename R>
inline constexpr bool is_initial_rig_v = false;
/** @concept IsInitialRig
 *  @brief @c R is ℕ: for every commutative rig @c S there is exactly one rig
 *  homomorphism @c ℕ @c → @c S, evaluation.  The structural part is checked;
 *  the universal property is the opt-in, as for @c IsInitialRing. */
export template <typename R>
concept IsInitialRig =
    dedekind::category::IsRig<R, std::plus<R>, std::multiplies<R>> &&
    dedekind::category::IsCommutative<R, std::multiplies<R>> &&
    is_initial_rig_v<R>;
template <>
inline constexpr bool is_initial_rig_v<NatExpr<>> = true;

// ===========================================================================
// (1) Initial Ring — ℤ as the initial object in Ring.
// ===========================================================================

/**
 * @brief Opt-in trait: @c R is the initial object in the category
 *        of (commutative, unital) rings.
 *
 * @details Default: @c false.  Specialised to @c true on ℤ's carrier,
 * @c sets::IntExpr, below.
 *
 * Universal-property content (existence + uniqueness of the
 * canonical ring homomorphism @c χ_R @c : @c R @c → @c S for every
 * ring @c S) is the engineer's honesty obligation.  This trait is
 * the type-system anchor: a carrier opts in, and downstream concept
 * checks fire.
 */
export template <typename R>
inline constexpr bool is_initial_ring_v = false;

/**
 * @concept IsInitialRing
 * @brief @c R is the initial object in @c Ring — for every ring
 *        @c S there exists a unique ring homomorphism @c R @c → @c S.
 *
 * @details Composes:
 *   * The structural ring claim ( @c IsRing<R, std::plus<R>,
 *     std::multiplies<R>>): @c R must actually @b be a ring.
 *   * The opt-in trait @c is_initial_ring_v<R>: the engineer's
 *     claim that this specific ring is the initial one.
 *
 * Universal-property content is the engineer's honesty obligation.
 *
 * @tparam R The carrier type claimed to be the initial ring.
 */
export template <typename R>
concept IsInitialRing =
    dedekind::category::IsCommutativeRing<R, std::plus<R>,
                                          std::multiplies<R>> &&
    is_initial_ring_v<R>;

// ===========================================================================
// (2) Grothendieck Group — ℤ as the free abelian group on ℕ.
// ===========================================================================

/**
 * @brief Opt-in trait: @c G is the Grothendieck group of the
 *        commutative monoid @c M.
 *
 * @details Default: @c false.  One pair is registered: @c (IntExpr,
 * NatExpr), ℤ over ℕ.
 *
 * Universal-property content (existence + uniqueness of the
 * extension @c φ̂ @c : @c G @c → @c A for every abelian group @c A
 * and monoid homomorphism @c φ @c : @c M @c → @c A) is the
 * engineer's honesty obligation.
 */
export template <typename G, typename M>
inline constexpr bool is_grothendieck_group_v = false;

/**
 * @concept IsGrothendieckGroup
 * @brief @c G is the Grothendieck group of the commutative monoid
 *        @c M — the free abelian group on @c M, equivalently the
 *        left adjoint of the forgetful functor @c Ab @c → @c CMon
 *        applied at @c M.
 *
 * @details Composes:
 *   * The structural abelian-group claim ( @c IsAbelianGroup<G,
 *     std::plus<G>>): @c G must actually @b be an abelian group.
 *   * The structural commutative-monoid claim ( @c
 *     IsCommutativeMonoid<M, std::plus<M>>): @c M must actually @b be
 *     a commutative monoid.
 *   * The opt-in trait @c is_grothendieck_group_v<G, M>: the
 *     engineer's claim that this specific pair forms a Grothendieck-
 *     completion adjunction at the monoid @c M.
 *
 * Universal-property content (existence + uniqueness of the
 * extension @c φ̂) is the engineer's honesty obligation.  Operational
 * realisation: the closure-forcing operator @c M @c - @c M @c →
 * @c G implements the Grothendieck construction at the operator
 * level.
 *
 * @tparam G The carrier type claimed to be the Grothendieck group.
 * @tparam M The commutative monoid being completed.
 */
export template <typename G, typename M>
concept IsGrothendieckGroup =
    dedekind::category::IsAbelianGroup<G, std::plus<G>> &&
    dedekind::category::IsCommutativeMonoid<M, std::plus<M>> &&
    is_grothendieck_group_v<G, M>;

template <>
inline constexpr bool is_initial_ring_v<IntExpr<>> = true;
template <>
inline constexpr bool is_grothendieck_group_v<IntExpr<>, NatExpr<>> = true;

// ── ℕ's witnesses: the initial rig, the initial Maybe-algebra, ℕ[π] ────────
namespace detail_initial_rig_witness {
using dedekind::category::In;
using dedekind::category::maybe_functor;
constexpr std::size_t top = std::numeric_limits<std::size_t>::max();
static_assert(IsInitialRig<NatExpr<>> && !IsInitialRig<ExtensionalCardinal<>>,
              "ℕ is the initial rig; a word is a rig that wraps, not initial");
static_assert(dedekind::category::IsInitialFAlgebra<maybe_functor<NatExpr<>>,
                                                    NatExpr<>, In<NatExpr<>>>,
              "(ℕ, [Z, S]) is the initial Maybe-algebra: the first instance");
static_assert(
    canonical<NatExpr<>>(Const<std::size_t>{6u} * Const<std::size_t>{7u}) ==
            canonical<NatExpr<>>(Const<std::size_t>{42u}) &&
        canonical<NatExpr<>>(Const<std::size_t>{top} *
                             Const<std::size_t>{top}) ==
            canonical<NatExpr<>>(Const<std::size_t>{top} *
                                     Const<std::size_t>{top} +
                                 Const<std::size_t>{0u}) &&
        canonical<NatExpr<>>(Const<std::size_t>{top} *
                             Const<std::size_t>{top}) !=
            canonical<NatExpr<>>(Const<std::size_t>{1u}) &&
        canonical<std::size_t>(Const<std::size_t>{top} *
                               Const<std::size_t>{top}) ==
            canonical<std::size_t>(Const<std::size_t>{1u}),
    "a ground term's normal form in ℕ is its exact value, 6 * 7 = 42 and "
    "(2^64-1)^2 above the word; the same term read in ℤ/2^64 wraps to 1: a "
    "normal form per algebra");
}  // namespace detail_initial_rig_witness

// ── ℤ's witnesses: the initial ring, the Grothendieck group of ℕ ───────────
namespace detail_initial_ring_witness {
using Z = IntExpr<>;
constexpr std::uint64_t top = std::numeric_limits<std::uint64_t>::max();
/** @brief A node of ℤ's term read in the word, @f$\mathbb{Z}/2^{64}@f$, from
 *  the images of the nodes before it. */
struct InWord {
  const std::array<std::uint64_t, Z::capacity>& v;
  constexpr std::uint64_t operator()(nodes::Atom n) const { return n.value; }
  constexpr std::uint64_t operator()(nodes::Add n) const {
    return v[n.a] + v[n.b];
  }
  constexpr std::uint64_t operator()(nodes::Mul n) const {
    return v[n.a] * v[n.b];
  }
  constexpr std::uint64_t operator()(nodes::Neg n) const {
    return std::uint64_t{} - v[n.a];
  }
};
struct Images {
  std::array<std::uint64_t, Z::capacity> v{};
};
struct InWordStep {
  Z z;
  constexpr Images operator()(Images s, std::size_t i) const {
    s.v[i] = std::visit(InWord{s.v}, z.at(i));
    return s;
  }
};
/** @brief The extension @f$\hat\varphi : \mathbb{Z} \to \mathbb{Z}/2^{64}@f$
 *  of the word's quotient @f$\varphi@f$ of ℕ: the term's fold into the word,
 *  atoms to themselves and each node to the word's operation.  It is the one
 *  ring homomorphism extending @f$\varphi@f$, so it agrees on terms with equal
 *  canonical forms.  @param z an integer.  @return its image. */
constexpr std::uint64_t φ_hat(const Z& z) {
  return dedekind::category::fold(std::views::iota(std::size_t{0}, z.size()),
                                  Images{}, InWordStep{z})
      .v[z.root()];
}
/** @return @f$2^{64}@f$, an integer above the word. */
constexpr Z two_64() { return Z::atom(top) + Z::atom(1); }
static_assert(IsGrothendieckGroup<Z, NatExpr<>> && IsInitialRing<Z> &&
                  !IsInitialRing<ExtensionalCardinal<>>,
              "ℤ is the Grothendieck group of ℕ and the initial ring, on its "
              "terms; a word is ℤ/2^64, neither");
static_assert(φ_hat(two_64()) == 0u && φ_hat(Z::atom(top)) == top &&
                  φ_hat(Z{}) == 0u &&
                  φ_hat(Z{-3} + two_64()) == φ_hat(Z{-3}) + φ_hat(two_64()) &&
                  φ_hat(Z{7} - Z{9}) == std::uint64_t{} - 2u,
              "φ̂ extends φ along ℕ ↪ ℤ and is a homomorphism on values of "
              "both signs, past the word's edge");
}  // namespace detail_initial_ring_witness
}  // namespace dedekind::algebra
