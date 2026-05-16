/**
 * @file dedekind/category/lattice.cppm
 * @partition :lattice
 * @brief Lattice categories — the Form-chain row 4 (#698).
 *
 * @section lattice__Categorical_Definition
 * A @b lattice (in the categorical reading: a thin bicartesian category)
 * is a thin antisymmetric category in which every pair of objects has
 * both a binary product (meet, ∧) and a binary coproduct (join, ∨).
 *
 * In the order-theoretic encoding the codebase uses across
 * @c :mereology, @c :posetal, @c :thin, @c :filtered: a lattice over
 * carrier @c T with relation @c Rel and topos @c L assembles
 *
 *   - @b posetal: thin (preorder) + antisymmetric;
 *   - @b filtered: every pair has SOME upper bound (directedness);
 *   - @b cofiltered: every pair has SOME lower bound (codirectedness);
 *   - @b universality: the upper / lower bounds are unique (join / meet).
 *     In the posetal case (antisymmetric) this is @em automatic — if
 *     two least upper bounds @c a, @c b exist for a set, then
 *     @c a @c ≤ @c b and @c b @c ≤ @c a, hence @c a @c = @c b.
 *
 * Operationally, the bottom-up algebraic content — join / meet as
 * binary operations satisfying commutativity, associativity, idempotence,
 * and absorption — is named upstream by
 * @c :posetal::IsOrderLatticeOperations.  @c IsLatticeCategory @b binds
 * the top-down universal-property reading to the bottom-up algebraic
 * one: every carrier satisfying both presentations is a lattice in the
 * categorical sense.
 *
 * @section lattice__Form_Chain
 * Row 4 of the lattice Form-chain (#698):
 *
 * @code
 *   IsThinCategory          (row 1, :thin — preorder)
 *       ↓ + antisymmetry
 *   IsPosetal               (row 2, :posetal — poset)
 *       ↓ + directedness  ↓ + codirectedness
 *   IsFilteredCategory      (row 3, :filtered — directed poset)
 *       ↓ + cofiltered + universality
 *   IsLatticeCategory       (row 4, THIS PARTITION)
 *       ↓ + initial + terminal
 *   IsBoundedLatticeCategory  (row 5 — future slice)
 *       ↓ + exponentials
 *   IsHeytingLatticeCategory  (row 6 — future slice)
 *       ↓ + complement involution
 *   IsBooleanLatticeCategory  (row 7 — future slice)
 * @endcode
 *
 * Each `↓ +` is a @b faithful inclusion encoded definitionally in the
 * signature, per the project's @em "faithful specialization in the type
 * signature from day one" posture (#698).
 *
 * @section lattice__ETCS_Connection_Sollbruchstelle
 * @b Planned downstream connection — not yet structurally landed:
 *
 * In an ETCS topos (every @c :etcs::IsSet carrier), the subobject family
 * @c Sub(S) is automatically a @b Boolean lattice:
 *
 *   - Axiom 3 (terminal)        → initial / terminal in Sub(S) (∅, S).
 *   - Axiom 5 (products)        → binary meets (intersection = pullback).
 *   - Axiom 5 + Axiom 7 (Ω)     → binary joins (union via classifier).
 *   - Axiom 6 (exponentials)    → relative complement (Heyting structure).
 *   - Axiom 7 (classical Ω)     → complement involution (Boolean).
 *   - Axiom 10                  → direct: Sub(S) is a power-object lattice.
 *
 * In other words, @c IsSet<S> @b implies @c IsBooleanLatticeCategory<Sub<S>,
 * ClassicalLogic> once the Form-chain reaches row 7 AND a @c Sub<S>
 * categorical wrapper exists to host the witness.  This connection is
 * a @b Sollbruchstelle: the partition header names it so future slices
 * (rows 5–7 of the chain + the @c Sub<S> wrapper + the harmonization of
 * @c HasAxiom10PowerObjectLattice with the Form-chain) have an
 * unambiguous target.
 *
 * Until those slices land, the ETCS connection lives at the
 * @em documentation level only — @c IsLatticeCategory does @b not yet
 * require or witness anything about @c :etcs::IsSet.
 *
 * @section lattice__Boolean_Witness
 * @c bool with @c std::less_equal participates: the 2-element poset is
 * also a (Boolean) lattice, and totally-ordered carriers under
 * @c std::less_equal trivially satisfy directedness and codirectedness
 * (max / min are the join / meet).  Pinning @c bool here anchors the
 * Form-chain at its smallest non-trivial example.
 *
 * @see https://en.wikipedia.org/wiki/Lattice_(order)
 * @see https://ncatlab.org/nlab/show/lattice
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Two presentations meet here: the top-down universal-property
 *        reading (thin + bicartesian) and the bottom-up algebraic
 *        reading (commutative semilattices with absorption).  Each
 *        side is a witness of the other."
 */
module;

#include <algorithm>
#include <concepts>
#include <functional>
#include <limits>       // std::numeric_limits — LatticeBottom/Top
                        // specialisations for arithmetic carriers.
#include <type_traits>  // std::is_arithmetic_v — same specialisations.

export module dedekind.category:lattice;

import :logic;
import :posetal;   // IsPosetal — row 2 (thin + antisymmetric);
                   // IsOrderLatticeOperations — bottom-up algebraic surface
import :filtered;  // IsFilteredCategory — row 3 (directed thin cat)
import :species;   // is_codirected_v — cofiltered companion to is_directed_v
import :limit;     // IsInitialObject / IsTerminalObject — row 5
                   // universal-property witnesses (relaxed via tag
                   // discovery to admit LatticeBottom/LatticeTop).

namespace dedekind::category {

/**
 * @concept IsLatticeCategory
 * @brief A posetal category that is both filtered and cofiltered, with
 *        joins and meets given by the algebraic lattice operations.
 *
 * @details
 * The Form-witness for row 4 of the lattice Form-chain (#698).  Binds:
 *
 *   - the @b top-down categorical content (thin + antisymmetric +
 *     filtered + cofiltered);
 *   - the @b bottom-up algebraic content (@c IsOrderLatticeOperations:
 *     join / meet operations satisfying commutativity, associativity,
 *     idempotence, and absorption).
 *
 * Both presentations describe the same Form; every carrier satisfying
 * @c IsLatticeCategory participates in both.
 *
 * Faithful inclusions encoded definitionally:
 *
 *   IsLatticeCategory ⊊ IsFilteredCategory ⊊ IsThinCategory
 *   IsLatticeCategory ⊊ IsPosetal           ⊊ IsThinCategory
 *
 * (Filtered and posetal are parallel branches above thin; lattice
 * conjoins them and adds cofiltered + the algebraic join/meet
 * existence.)
 *
 * @tparam T    The Domain (Objects).
 * @tparam Rel  The Relation (the unique-morphism witness).
 * @tparam Join The join operation (default @c std::ranges::max).
 * @tparam Meet The meet operation (default @c std::ranges::min).
 * @tparam L    The Logic Species (the Subobject Classifier Ω).
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min),
                 typename L = ClassicalLogic>
concept IsLatticeCategory =
    IsPosetal<T, Rel, L> &&           // Faithful: lattice ⊊ posetal.
    IsFilteredCategory<T, Rel, L> &&  // Faithful: lattice ⊊ filtered.
    requires {
      /** @brief Cofiltered: every pair has a lower bound. */
      requires is_codirected_v<T, Rel>;
    } &&
    /** @brief Bottom-up algebraic content: join / meet as operations
     *         with the lattice laws (commutativity, associativity,
     *         idempotence, absorption).  Universality of the bounds
     *         (LUB / GLB) is automatic for posetal carriers by
     *         antisymmetry, so the Form-witness is complete with the
     *         algebraic surface bundled in. */
    IsOrderLatticeOperations<T, Join, Meet>;

/** @section lattice__Canonical_Witnesses */

static_assert(IsLatticeCategory<bool>,
              "bool with std::less_equal, std::ranges::max/min is the "
              "canonical 2-element lattice category (also the smallest "
              "non-trivial Boolean algebra; pending the row-7 Form-chain "
              "extension that will witness this categorically).");

static_assert(IsLatticeCategory<int>,
              "int is a lattice category — totally ordered carriers are "
              "trivially directed and codirected (max / min are the "
              "join / meet).");

/** @section lattice__Bounded_Witnesses
 *
 *  @brief Structural witness types for the bottom (initial) and top
 *         (terminal) of the lattice over @c (T, @c Rel), playing the
 *         role of initial / terminal objects of the lattice viewed as
 *         a thin category.
 *
 *  @details Each wrapper type:
 *
 *    - Declares the @c is_initial_object_tag / @c is_terminal_object_tag
 *      typedef so it satisfies the relaxed @c :limit::IsInitialObject /
 *      @c IsTerminalObject (tag-discovery branch).
 *    - Exposes the corresponding carrier element as a @c constexpr
 *      @c value member (extractable when consumers need the actual
 *      lattice element).
 *
 *  Specialisations are provided for arithmetic carriers under
 *  @c std::less_equal: @c std::numeric_limits<T>::min() / @c max() are
 *  the lattice bottom / top.  This covers @c bool (false / true),
 *  @c int (INT_MIN / INT_MAX), @c unsigned (0 / UINT_MAX), @c size_t,
 *  and the rest of the arithmetic family.
 *
 *  Downstream sub-categories (set algebras, posets with named extremes,
 *  …) opt in by specialising the wrappers for their @c (T, @c Rel)
 *  pair; no change to @c :species or duplicate concept surface.
 */

/** @brief Primary template: undefined — no value, no tag.  Specialise
 *         to register the bottom of the lattice over @c (T, @c Rel). */
export template <typename T, typename Rel>
struct LatticeBottom;

/** @brief Primary template: undefined.  Specialise to register the
 *         top of the lattice over @c (T, @c Rel). */
export template <typename T, typename Rel>
struct LatticeTop;

/** @brief Canonical specialisation: @b integral carriers under
 *         @c std::less_equal have bottom @c = @c numeric_limits<T>::min().
 *
 *  @note Floating-point carriers are intentionally @b excluded — they
 *  fail upstream @c IsThinCategory because @c is_transitive_v in
 *  @c :species is specialised only for integral + @c bool (IEEE 754 NaN
 *  breaks transitivity / reflexivity, see @c :thin tests).  Limiting
 *  this specialisation to @c std::is_integral_v also avoids the
 *  @c numeric_limits<T>::min() vs @c lowest() trap on floats:
 *  @c min() returns the smallest positive normal for floats, not the
 *  most-negative value. */
template <typename T>
  requires std::is_integral_v<T>
struct LatticeBottom<T, std::less_equal<T>> {
  using is_initial_object_tag = void;
  static constexpr T value = std::numeric_limits<T>::min();
};

/** @brief Canonical specialisation: @b integral carriers under
 *         @c std::less_equal have top @c = @c numeric_limits<T>::max(). */
template <typename T>
  requires std::is_integral_v<T>
struct LatticeTop<T, std::less_equal<T>> {
  using is_terminal_object_tag = void;
  static constexpr T value = std::numeric_limits<T>::max();
};

/**
 * @concept IsBoundedLatticeCategory
 * @brief A lattice category that has a designated bottom (initial) and
 *        top (terminal) element — row 5 of the lattice Form-chain (#698
 *        Slice 4).
 *
 * @details
 * Faithful inclusion @c IsBoundedLatticeCategory @c ⊊ @c IsLatticeCategory
 * encoded definitionally per the project's @em "faithful specialization
 * in the type signature from day one" posture (#698).
 *
 * The bounded refinement adds two @b structural witness types:
 *
 *   - @c LatticeBottom<T, Rel> — declared as an initial-object witness
 *     via @c :limit::IsInitialObject (relaxed with tag-discovery).
 *   - @c LatticeTop<T, Rel> — declared as a terminal-object witness
 *     via @c :limit::IsTerminalObject (relaxed similarly).
 *
 * These are the lattice's @b own initial / terminal objects —
 * universal-property witnesses local to the lattice over @c (T, Rel)
 * viewed as a thin category.  Distinct from the strict-global
 * @c std::same_as<T, Zero> / @c std::same_as<T, One> branches of the
 * @c :limit concepts, which name the unique initial / terminal of the
 * ambient category of sets.  The tag-discovery relaxation of @c :limit
 * (in turn) admits both readings without parallel concept surface.
 *
 * Faithful per #698 Q3: the Form-chain commits to categorical
 * universal-property witnesses — bottom / top are designated structural
 * witness types satisfying @c IsInitialObject / @c IsTerminalObject,
 * not operational @c lower_bound() / @c upper_bound() member functions
 * on a Sub<S>.
 *
 * @tparam T    The Domain (Objects).
 * @tparam Rel  The Relation.
 * @tparam Join The join operation (default @c std::ranges::max).
 * @tparam Meet The meet operation (default @c std::ranges::min).
 * @tparam L    The Logic Species.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min),
                 typename L = ClassicalLogic>
concept IsBoundedLatticeCategory =
    IsLatticeCategory<T, Rel, Join, Meet, L> &&  // Faithful: bounded ⊊ lattice.
    IsInitialObject<LatticeBottom<T, Rel>> &&    // Universal-property initial.
    IsTerminalObject<LatticeTop<T, Rel>> &&      // Universal-property terminal.
    requires {
      /** @brief The wrappers must expose the corresponding carrier
       *         element as a @c constexpr @c value member.  Without
       *         this, a tag-only specialisation (initial-object tag
       *         declared but no @c value) would silently satisfy the
       *         universal-property check while providing no usable
       *         bottom / top — defeating the structural contract. */
      { LatticeBottom<T, Rel>::value } -> std::convertible_to<T>;
      { LatticeTop<T, Rel>::value } -> std::convertible_to<T>;
    };

/** @section lattice__Bounded_Canonical_Witnesses */

static_assert(IsBoundedLatticeCategory<bool>,
              "bool is a bounded lattice category: bottom = false, top = "
              "true (the canonical 2-element bounded lattice; also the "
              "subobject classifier Ω in Set).");

static_assert(IsBoundedLatticeCategory<int>,
              "int is bounded: LatticeBottom = INT_MIN, "
              "LatticeTop = INT_MAX.");

static_assert(LatticeBottom<bool, std::less_equal<bool>>::value == false,
              "Bool's lattice bottom is false.");
static_assert(LatticeTop<bool, std::less_equal<bool>>::value == true,
              "Bool's lattice top is true.");

/** @section lattice__Involutive_Endofunctor
 *
 *  @brief Involutive endofunctor concept — an endomap @c F @c : @c T @c → @c T
 *         with @c F² @c ≅ @c Id.
 *
 *  @details
 *  An @b involutive @b endofunctor on a category @c C is a functor
 *  @c F @c : @c C @c → @c C whose square is naturally isomorphic to
 *  the identity functor.  In thin / lattice categories (where the
 *  category structure is reduced to a carrier @c T plus a relation),
 *  this collapses to an endomap on @c T satisfying @c F(F(x)) @c = @c x
 *  for all @c x @c ∈ @c T.
 *
 *  Used downstream by the row-7 @c IsBooleanLatticeCategory (#698
 *  Slice 7), where the @b complement is the canonical involution.
 *  Examples:
 *
 *    - @c std::logical_not<bool> on @c bool: @c !!x @c = @c x.
 *    - @c std::bit_not<T> on integral @c T: @c ~~x @c = @c x.
 *    - @c :sets::Complement on a Boolean subobject lattice @c Sub<S>:
 *      @c !!S @c ≡ @c S (the bona-fide involution from #683).
 *
 *  @section lattice__Involutive_Sollbruchstelle
 *  @b Pragmatic placement: this concept lives inline in @c :lattice
 *  per #698 Q2.  When a second consumer arrives (e.g.\ dual categories,
 *  opposite functors, monad-shaped opportunities), the natural
 *  extraction target is a new partition @c :involution or — if the
 *  consumer is monad-shaped — @c :monad.  Until then the lightweight
 *  inline location avoids partition-graph churn for a single consumer.
 *
 *  Strictly: involution is @b not a monad.  A monad @c (T, η, μ) has
 *  @c μ @c : @c T² @c → @c T; an involution has @c F² @c ≅ @c Id (the
 *  iteration goes back to identity, not to the functor itself).  The
 *  shared shape is "endofunctor + property of its second iteration".
 */

/** @brief Trait: a callable @c F is involutive on @c T iff @c F(F(x))
 *         @c = @c x for all @c x @c ∈ @c T.  Primary template is
 *         @c std::false_type; opt-in via specialisation or member
 *         discovery.  @b Exported so downstream code can specialise
 *         this trait for its own carrier types (mirrors the
 *         @c :species::is_reflexive / @c is_transitive export pattern). */
export template <typename F, typename T>
struct is_involutive : std::false_type {};

/** @brief Discovery: types may opt in via a nested @c is_involutive_v
 *         template member, mirroring the @c is_reflexive / @c
 *         is_transitive / @c is_directed pattern in @c :species. */
template <typename F, typename T>
  requires requires { F::template is_involutive_v<T>; }
struct is_involutive<F, T>
    : std::bool_constant<F::template is_involutive_v<T>> {};

export template <typename F, typename T>
inline constexpr bool is_involutive_v = is_involutive<F, T>::value;

/** @brief Canonical specialisation: @c std::logical_not<bool> is the
 *         involution on @c bool. */
template <>
struct is_involutive<std::logical_not<bool>, bool> : std::true_type {};

/** @brief Canonical specialisation: @c std::bit_not<T> is the
 *         involution on @b non-bool integral @c T (~~x = x).
 *
 *  @note @c bool is @b excluded: @c std::bit_not<bool>(x) computes
 *  @c ~x via integral promotion (yielding @c -1 or @c -2 in @c int),
 *  then converts back to @c bool — which is always @c true for any
 *  non-zero result.  So @c bit_not(bit_not(false)) @c == @c true,
 *  not @c false: @c std::bit_not<bool> is @b not an involution.
 *  Bool's involution is @c std::logical_not<bool>, specialised
 *  separately above. */
template <typename T>
  requires std::is_integral_v<T> && (!std::is_same_v<T, bool>)
struct is_involutive<std::bit_not<T>, T> : std::true_type {};

/**
 * @concept IsInvolutiveEndofunctor
 * @brief A callable @c F is an involutive endofunctor on @c T —
 *        invocable @c T @c → @c T with @c F² @c ≅ @c Id.
 *
 * @details
 * Three structural requirements:
 *
 *   - @c F is invocable on @c T (the endomap shape).
 *   - The invocation returns a value convertible to @c T (so @c F
 *     genuinely maps @c T to @c T, not to some larger codomain).
 *   - @c F² @c = @c Id, witnessed by @c is_involutive_v<F, T>.
 *
 * Used by @c IsBooleanLatticeCategory (#698 Slice 7) with @c F the
 * lattice complement.
 *
 * @tparam F The endofunctor (a callable).
 * @tparam T The carrier type on which @c F acts.
 */
export template <typename F, typename T>
concept IsInvolutiveEndofunctor =
    std::invocable<F, T> &&
    std::convertible_to<std::invoke_result_t<F, T>, T> && is_involutive_v<F, T>;

/** @section lattice__Involution_Canonical_Witnesses */

static_assert(IsInvolutiveEndofunctor<std::logical_not<bool>, bool>,
              "std::logical_not<bool> is the canonical Boolean involution: "
              "!!x == x for all x in {false, true}.");

static_assert(IsInvolutiveEndofunctor<std::bit_not<int>, int>,
              "std::bit_not<int> is involutive on int: ~~x == x.");

static_assert(IsInvolutiveEndofunctor<std::bit_not<unsigned>, unsigned>,
              "std::bit_not<unsigned> is involutive on unsigned.");

}  // namespace dedekind::category
