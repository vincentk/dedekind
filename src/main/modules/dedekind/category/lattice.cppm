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
 *   IsBoundedLatticeCategory  (row 5, #698 Slice 4 — landed)
 *       ↓ + exponentials
 *   IsHeytingLatticeCategory  (row 6, #698 Slice 6 — landed)
 *       ↓ + complement involution
 *   IsBooleanLatticeCategory  (row 7, #698 Slice 7 — THIS SLICE)
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
 * @section lattice__Omega_First_Pointwise_Lift
 * @b Textbook content (#698 Slice 8): the relation between a topos's
 * subobject classifier @c Ω and the subobject lattices @c Sub(A) of every
 * ambient @c A is @b directional, not symmetric:
 *
 *   @c Sub(A) @c ≅ @c Hom(A, @c Ω) @c ≅ @c Ω^A
 *
 * gives @c Sub(A) an automatic lattice structure for every @c A,
 * inherited @b pointwise from the lattice structure on @c Ω.  The lattice
 * operations on @c Sub(A) (meet, join, complement, ≤) correspond pointwise
 * to the lattice operations on @c Ω (AND, OR, NOT, ≤_Ω) applied to
 * characteristic morphisms @c χ.  Direction is Ω → @c Sub(A); the reverse
 * doesn't hold without representability.
 *
 * @section lattice__Constructive_Collapse
 * The project commits to @em intensional-first (lazy predicates over
 * potentially infinite carriers, per Q1 of #698); this admits
 * undecidability as a first-class value via @c TernaryLogic (Kleene K3).
 * Same Form-chain code, two regimes:
 *
 *   - @c L @c = @c ClassicalLogic → @c Ω @c = @c bool → Form-chain rows
 *     1–7 → standard set theory falls out as the @b decidable collapse.
 *   - @c L @c = @c TernaryLogic → @c Ω @c = @c Ternary → Form-chain rows
 *     1–6 (Heyting only — K3 is the smallest non-Boolean Heyting algebra;
 *     complement laws fail honestly at @c Unknown) → the "tricky to
 *     decide" escape door.
 *
 * The Form-chain row a carrier participates in is determined by the
 * carrier's @c logic_species; the same code witnesses both regimes via
 * parametric polymorphism on @c L.  This is Slice 8's architectural
 * commit: undecidability is @b architectural (L-parametric), not a
 * corner case (per-branch special-casing).
 *
 * @section lattice__Mereology_As_Special_Case
 * @c :mereology parthood is a special case of subobject classification:
 * "x is part of y" reads as @c subset_eq(x, @c y) on @c Sub(whole),
 * which is the pointwise lift of @c ≤_Ω applied to characteristic
 * morphisms.  Finite enumerable wholes → Classical → Boolean parthood;
 * infinite / intensional wholes → Kleene → Heyting parthood with
 * honest @c Unknown at undecidable points.  Same Form-chain machinery,
 * one foundation.
 *
 * @section lattice__CT_vs_Sets_Vocabulary
 * Pierce-style stratification: this partition's concept @b bodies use
 * CT-vocabulary primitives (the classifier @c Ω, the ambient @c A, the
 * characteristic morphism @c χ, free functions @c meet / @c join /
 * @c complement / @c subset_eq).  Default template arguments (@c Rel @c =
 * @c std::less_equal, @c Join @c = @c std::ranges::max, ...) are
 * @b set-theoretic hints — examples, not body content.  Operator
 * sugar (@c <=, @c &, @c |, @c !) lives downstream in @c :sets as
 * forwarders to the CT primitives.
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
#include <cstddef>      // std::size_t — Slice 10 meeting-point pin.
#include <functional>
#include <limits>       // std::numeric_limits — LatticeBottom/Top
                        // specialisations for arithmetic carriers.
#include <type_traits>  // std::is_arithmetic_v — same specialisations.

export module dedekind.category:lattice;

import :logic;
import :posetal;    // IsPosetal — row 2 (thin + antisymmetric);
                    // IsOrderLatticeOperations — bottom-up algebraic surface
import :filtered;   // IsFilteredCategory — row 3 (directed thin cat)
import :species;    // is_codirected_v — cofiltered companion to is_directed_v
import :limit;      // IsInitialObject / IsTerminalObject — row 5
                    // universal-property witnesses (relaxed via tag
                    // discovery to admit LatticeBottom/LatticeTop).
import :cartesian;  // IsExponential — row 6 universal-property witness
                    // (structural recogniser post-#698 Slice 6; admits
                    // both Set/Cpp function-space exponentials AND
                    // lattice-internal value exponentials uniformly).

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

/** @section lattice__Heyting_Exponential
 *
 *  @brief Structural witness type for an @b exponential object in the
 *         lattice over @c (T, @c Rel) viewed as a thin cartesian closed
 *         category — the relative complement @c a @c → b held as a
 *         carrier value, exposed as a callable that computes the eval
 *         morphism @c eval(e, x) @c = @c e @c ∧ x.
 *
 *  @details
 *  In a Heyting algebra, the exponential object @c b^a is the value
 *  @c (a @c → b) @c ∈ @c T satisfying the universal adjunction
 *
 *    @c a @c ∧ @c x @c ≤ @c b @c iff @c x @c ≤ @c (a @c → b).
 *
 *  @c HeytingExponential<T, Rel> wraps the exponential value and
 *  exposes the eval morphism @em structurally — its @c operator()(x)
 *  computes the meet @c value @c ∧ x.  The universal property says
 *  @c value @c ∧ x @c ≤ b when @c value @c = @c (a @c → b).
 *
 *  @section lattice__Heyting_Aligns_With_IsExponential
 *  The wrapper satisfies @c :cartesian::IsExponential<HeytingExponential<T,
 * Rel>, T, T> @b structurally — @c eval(e, x) is just the call expression
 *  @c e(x), and the wrapper's @c operator()(T) returns T.  This is the
 *  unification @b across CCCs: function-space exponentials (Set / Cpp)
 *  and value-shaped exponentials (Heyting) satisfy the same structural
 *  concept body, with no tag declarations or CPO machinery (#698 Slice 6).
 *
 *  The wrapper also exposes @c Domain / @c Codomain typedefs, so it
 *  satisfies @c IsArrow when the future @c IsArrowExponential
 *  refinement (#706) lands.
 *
 *  Concrete formulae for canonical carriers:
 *
 *    - Integral carriers under @c std::less_equal (with @c ∧ = min):
 *      @c eval(e, x) @c = @c min(value, x).  The universal exponential
 *      value @c (a @c → b) for input pair @c (a, b) is @c top if
 *      @c a @c ≤ b, else @c b — derivable from the meet's adjunction.
 *
 *  Specialisations beyond the integral / @c std::less_equal case are
 *  tracked under #708.
 */
export template <typename T, typename Rel, typename Meet>
struct HeytingExponential;

/** @brief Canonical specialisation: integral carriers under
 *         @c std::less_equal with @c std::ranges::min as the meet.
 *         Holds an exponential value @c value @c ∈ T; @c operator()(x)
 *         returns @c min(value, x) — the lattice meet, which @b is the
 *         eval morphism in a thin Heyting algebra.
 *
 *  @note The @c Meet template parameter is load-bearing: it pins the
 *  meet operation @c HeytingExponential uses for eval to the meet
 *  @c IsHeytingLatticeCategory commits to via @b its @c Meet
 *  parameter.  Without this coupling, a consumer could instantiate
 *  @c IsHeytingLatticeCategory with a non-canonical @c Meet while
 *  @c HeytingExponential::operator() silently used a different
 *  operation — categorical contract broken. */
export template <typename T>
  requires std::is_integral_v<T>
struct HeytingExponential<T, std::less_equal<T>, decltype(std::ranges::min)> {
  using Domain = T;  // IsArrow-shaped (for the future :morphism refinement)
  using Codomain = T;

  T value;  // the exponential element e = a → b

  /** @brief Eval morphism: @c value @c ∧ x (the meet, which @b is eval
   *         in a thin Heyting algebra).  Universal property:
   *         @c value @c ∧ x @c ≤ @c b when @c value @c = @c (a @c → b). */
  constexpr T operator()(T x) const noexcept {
    return std::ranges::min(value, x);
  }
};

/**
 * @concept IsHeytingLatticeCategory
 * @brief A bounded lattice category whose lattice exponentials are
 *        witnessed by @c :cartesian::IsExponential — row 6 of the
 *        lattice Form-chain (#698 Slice 6).
 *
 * @details
 * Faithful inclusion @c IsHeytingLatticeCategory @c ⊊
 * @c IsBoundedLatticeCategory encoded definitionally per the project's
 * @em "faithful specialization in the type signature from day one"
 * posture (#698).
 *
 * @section lattice__Heyting_Form_Chain_Row_6
 * The Heyting refinement requires the carrier @c T has an exponential
 * object structure: @c HeytingExponential<T, Rel> wraps the relative
 * complement value @c a @c → b and satisfies @c :cartesian::IsExponential.
 * The structural alignment with the CCC concept means lattice
 * exponentials and function-space exponentials inhabit the same
 * categorical surface — different concrete representations, one shared
 * concept (#698 Slice 6's generalisation of @c IsExponential).
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
concept IsHeytingLatticeCategory =
    IsBoundedLatticeCategory<T, Rel, Join, Meet,
                             L> &&  // Faithful: heyting ⊊ bounded.
    IsExponential<HeytingExponential<T, Rel, Meet>, T,
                  T>;  // Lattice exponential — Meet is passed through so
                       // the eval operation in the wrapper is pinned to
                       // the concept's Meet, not silently hardcoded.
                       // Without this, instantiating the concept with a
                       // non-canonical Meet would silently leave the
                       // wrapper using the wrong operation (PR #709
                       // review fix).

/** @section lattice__Heyting_Canonical_Witnesses */

static_assert(IsHeytingLatticeCategory<bool>,
              "bool with std::less_equal is the canonical 2-element "
              "Heyting (and Boolean) lattice; HeytingExponential's "
              "operator()(x) computes the meet on bool, satisfying "
              ":cartesian::IsExponential structurally.");

static_assert(IsHeytingLatticeCategory<int>,
              "int is a Heyting lattice under the totally-ordered "
              "implication; HeytingExponential<int, std::less_equal>::"
              "operator()(x) = min(value, x).");

static_assert(
    IsExponential<HeytingExponential<bool, std::less_equal<bool>,
                                     decltype(std::ranges::min)>,
                  bool, bool>,
    "HeytingExponential<bool, …, min> aligns with :cartesian::IsExponential "
    "structurally — pure call-shape recognition (#698 Slice 6).");

/** @section lattice__Boolean_Complement_Trait
 *
 *  @brief Opt-in trait: @c Not is the @b complement morphism for the
 *         Boolean lattice over @c (T, Rel, Join, Meet).  The trait
 *         is the @em semantic gate for @c IsBooleanLatticeCategory —
 *         the structural side (Heyting + involutiveness of @c Not) is
 *         already enforced by the upstream concepts, but the @b
 *         complement laws
 *
 *      @c x @c ∧ @c ¬x @c = @c ⊥ , @c x @c ∨ @c ¬x @c = @c ⊤
 *
 *  are value-level statements that can't be discharged at type level.
 *  Following the @c is_involutive_v / @c is_reflexive_v opt-in pattern
 *  (#698 Slice 5), the carrier-side @b registers itself by specialising
 *  this trait.  Non-canonical pairings (e.g. @c std::bit_not<int> with
 *  @c std::less_equal<int>) genuinely fail the laws and are @b
 *  excluded by default.
 *
 *  @note This is a 5-parameter trait, not 6: the logic species @c L
 *  doesn't participate in the complement laws (those are pinned by
 *  @c Rel via @c lattice_top / @c lattice_bottom and by @c Join / @c
 *  Meet directly).  Adding @c L would create spurious specialisation
 *  burden for no algebraic content.
 *
 *  The bitwise Boolean algebra on integers (\@c int / @c unsigned with
 *  bit-subset @c Rel, @c & / @c | as meet / join, @c ~ as complement)
 *  is a separate canonical witness tracked under #710.
 */
export template <typename Not, typename T, typename Rel, typename Join,
                 typename Meet>
struct is_complement : std::false_type {};

export template <typename Not, typename T, typename Rel, typename Join,
                 typename Meet>
inline constexpr bool is_complement_v =
    is_complement<Not, T, Rel, Join, Meet>::value;

/** @brief Canonical specialisation: @c std::logical_not<bool> is the
 *         complement for the 2-element Boolean lattice over
 *         @c (bool, std::less_equal<bool>, std::ranges::max,
 *         std::ranges::min).  Value-level laws hold trivially:
 *         @c true @c ∧ @c !true @c = @c min(true, false) @c = @c false
 *         @c = @c ⊥, and @c true @c ∨ @c !true @c = @c max(true, false)
 *         @c = @c true @c = @c ⊤. */
template <>
struct is_complement<std::logical_not<bool>, bool, std::less_equal<bool>,
                     decltype(std::ranges::max), decltype(std::ranges::min)>
    : std::true_type {};

/**
 * @concept IsBooleanLatticeCategory
 * @brief A Heyting lattice category whose complement is an involutive
 *        endofunctor satisfying the Boolean complement laws — row 7 of
 *        the lattice Form-chain (#698 Slice 7).
 *
 * @details
 * Faithful inclusion @c IsBooleanLatticeCategory @c ⊊
 * @c IsHeytingLatticeCategory encoded definitionally per the project's
 * @em "faithful specialization in the type signature from day one"
 * posture (#698).
 *
 * @section lattice__Boolean_Form_Chain_Row_7
 * Three layered requirements, each a single @c && term:
 *
 *   - @c IsHeytingLatticeCategory @em (structural prerequisite — bounded
 *     lattice with exponentials).
 *   - @c IsInvolutiveEndofunctor<Not, T> @em (structural: @c Not @c :
 *     T @c → T with @c Not² @c ≅ @c Id — Slice 5 machinery).
 *   - @c is_complement_v<Not, T, Rel, Join, Meet> @em (semantic
 *     opt-in: the complement laws hold for this (\@c Not, lattice) pair).
 *
 * @section lattice__Boolean_Equivalent_Characterisations
 * Equivalent textbook readings of "Boolean lattice", any of which the
 * caller can use to recognise the participation:
 *
 *   - A complemented distributive lattice.
 *   - A Heyting algebra in which @c ¬¬x @c = @c x (law of excluded
 *     middle).
 *   - A bounded lattice with a unary @c ¬ satisfying @c x @c ∧ @c ¬x
 *     @c = @c ⊥ and @c x @c ∨ @c ¬x @c = @c ⊤.
 *
 * This concept threads the third reading definitionally.  The first
 * two follow as derived theorems on any carrier participating in the
 * concept (distributivity in a lattice with relative complements is a
 * standard result; double-negation elimination is the registered
 * involutiveness of @c Not).
 *
 * @section lattice__Boolean_Witnesses
 *   - @c bool under @c std::less_equal with @c std::logical_not<bool>
 *     — the canonical 2-element Boolean algebra.
 *   - Integral carriers under @c std::less_equal with @c std::bit_not
 *     are @b not Boolean lattices (order doesn't match complement);
 *     the bitwise Boolean algebra requires a bit-subset @c Rel (#710).
 *
 * @tparam T    The Domain (Objects).
 * @tparam Rel  The Relation.
 * @tparam Join The join operation (default @c std::ranges::max).
 * @tparam Meet The meet operation (default @c std::ranges::min).
 * @tparam Not  The complement endofunctor (default
 *              @c std::logical_not<T>; fails closed for non-bool unless
 *              the carrier registers an alternative pairing).
 * @tparam L    The Logic Species.
 */
export template <typename T, typename Rel = std::less_equal<T>,
                 typename Join = decltype(std::ranges::max),
                 typename Meet = decltype(std::ranges::min),
                 typename Not = std::logical_not<T>,
                 typename L = ClassicalLogic>
concept IsBooleanLatticeCategory =
    IsHeytingLatticeCategory<T, Rel, Join, Meet,
                             L> &&      // Faithful: boolean ⊊ heyting.
    IsInvolutiveEndofunctor<Not, T> &&  // Structural: Not² ≅ Id.
    is_complement_v<Not, T, Rel, Join,
                    Meet>;  // Semantic opt-in: complement
                            // laws hold for this pairing.

/** @section lattice__Boolean_Canonical_Witnesses */

static_assert(IsBooleanLatticeCategory<bool>,
              "bool with std::less_equal and std::logical_not is the "
              "canonical 2-element Boolean lattice — every Form-chain row "
              "1 through 7 fires definitionally.");

static_assert(
    !IsBooleanLatticeCategory<int, std::less_equal<int>,
                              decltype(std::ranges::max),
                              decltype(std::ranges::min), std::bit_not<int>>,
    "int under std::less_equal with std::bit_not is NOT a Boolean lattice: "
    "the bitwise complement doesn't match the order-theoretic meet/join "
    "(e.g. min(5, ~5) = -6 ≠ INT_MIN).  Honest Rejection via the opt-in "
    "is_complement_v trait — see #710 for the bitwise Boolean algebra "
    "under a bit-subset relation.");

/** @section lattice__Slice_10_Meeting_Point
 *
 *  @brief Compile-time meeting-point pins (#698 Slice 10) — three
 *         structurally distinct carriers at the same Form-chain rows.
 *
 *  @details The closing slice for #698 establishes that the Form-chain
 *  machinery is genuinely carrier-uniform: same code, three readings.
 *  The full meeting-point matrix (rows × carriers) lives in
 *  @c test/cpp/.../meeting_point_test.cpp; the pins below are the
 *  compile-time anchors that catch regressions @em at @em compile-time
 *  rather than runtime.
 *
 *  Per @c feedback_static_assert_in_main: cross-partition / cross-
 *  carrier invariants live as @c static_assert in the main @c .cppm
 *  source when there's no mock-type dependency.
 *
 *  Carriers covered here:
 *  - @c size_t: large finite totally-ordered chain (Slices 6/7 already
 *    pinned @c bool and @c int).
 *  - @c std::ranges niebloid identity: the Form-chain @c Meet / @c Join
 *    slots @b are the @c std::ranges niebloid types — pinned via an
 *    explicit instantiation that names them. */

static_assert(IsHeytingLatticeCategory<std::size_t>,
              "size_t is a Heyting lattice under the totally-ordered "
              "implication; rows 1–6 of the Form-chain fire.");

static_assert(
    !IsBooleanLatticeCategory<std::size_t, std::less_equal<std::size_t>,
                              decltype(std::ranges::max),
                              decltype(std::ranges::min),
                              std::bit_not<std::size_t>>,
    "size_t under std::less_equal with std::bit_not is NOT a Boolean "
    "lattice — same Honest Rejection as int (the bitwise complement "
    "doesn't match the order-theoretic meet/join).  Bitwise route is "
    "#710's territory.");

static_assert(IsLatticeCategory<int, std::less_equal<int>,
                                decltype(std::ranges::max),
                                decltype(std::ranges::min)>,
              "Niebloid identity: the Form-chain Meet / Join slot "
              "defaults ARE the std::ranges niebloids "
              "(std::ranges::min, std::ranges::max).  Pinning the "
              "explicit instantiation here makes the structural fit "
              "type-checked at compile time.");

/**
 * @concept IsSubobjectFamilyMember
 * @brief A type @c R is a member of the subobject family over ambient
 *        @c A in classifier @c L.
 *
 * @details
 * The lattice operations on @c Sub(A) (meet, join, complement) close
 * over the @em family of subobjects of @c A in @c L, not over a single
 * predicate-type closed carrier.  Concretely: @c Set<T, L, P> @c &
 * @c Set<T, L, Q> returns @c Set<T, L, AndPredicate<P,Q>> — a different
 * predicate type, but the same @c Ambient and @c logic_species.
 *
 * @section lattice__Family_Anchor
 * The @b ambient @c A is the anchor (per #712 review): a subobject
 * family is determined by the pair @c (A, @c L), and family membership
 * is recognised by metadata equality.  This parallels the mereology
 * notion in @c :sets::mereology::IsSystem<S, Species, L>: a system is
 * "a space of parts" anchored on its ambient Species, and the parts
 * (subobjects) form a family by virtue of sharing that ambient.
 *
 * @section lattice__Family_Generalisation_Door
 * Per #712 review: if the @c (A, @c L) anchoring proves too
 * restrictive in some future regime (e.g.\ "families over a category
 * rather than a single ambient" — fibrations, sheaves over a site,
 * dependent power objects), the family concept can be generalised by
 * adding template parameters or splitting the anchor.  The current
 * shape is the minimum that supports the Slice 8 architectural
 * commit; broader generalisations land if and when a concrete consumer
 * surfaces a need.
 *
 * @tparam R The candidate family member (typically the result type of
 *           @c meet / @c join / @c complement).
 * @tparam A The ambient object.
 * @tparam L The classifier (logic species).
 */
export template <typename R, typename A, typename L>
concept IsSubobjectFamilyMember = requires {
  /** @brief Strip cv/ref so reference-returning carriers (e.g.\
   *  @c meet(a, b) @c -> @c const @c S& under expression-template
   *  optimisation) are admitted — the trailing-return-type
   *  substitution in @c requires-expressions can yield reference
   *  types, and the family-membership check is properties-of-the-
   *  type, not properties-of-the-expression-category (#712 review,
   *  Copilot). */
  typename std::remove_cvref_t<R>::Ambient;
  typename std::remove_cvref_t<R>::logic_species;
  requires std::same_as<typename std::remove_cvref_t<R>::Ambient, A>;
  requires std::same_as<typename std::remove_cvref_t<R>::logic_species, L>;
};

/**
 * @concept IsSubobjectLattice
 * @brief A type @c S is the carrier of a subobject lattice over
 *        @c S::Ambient, with lattice structure induced pointwise from
 *        the classifier @c S::logic_species::Ω.
 *
 * @details
 * The textbook content (#698 Slice 8): in a topos with subobject
 * classifier @c Ω, the bijection
 *
 *    @c Sub(A) @c ≅ @c Hom(A, @c Ω) @c ≅ @c Ω^A
 *
 * gives @c Sub(A) an automatic lattice structure for every @c A,
 * inherited @b pointwise from the lattice structure on @c Ω.  Direction
 * is Ω → @c Sub(A); not the reverse without representability.
 *
 * @section lattice__IsSubobjectLattice_Structural_Shape
 * @c IsSubobjectLattice<S> checks the carrier's CT-vocabulary metadata
 * plus the family-typed shape of @c meet / @c join / @c complement.
 * The Form-chain row 1 (thin) @c ≤ relation is @b derivable from
 * @c meet (see the derivability section below); the concept therefore
 * doesn't need a separate @c subset_eq / @c operator<= clause or a
 * carrier-side @c SubsetEqRel typedef.
 *
 * The strength @c S inherits at higher rows is determined by
 * @c L @c = @c S::logic_species:
 *
 *   - @c L @c = @c ClassicalLogic → @c S participates in
 *     @c IsBooleanSubobjectLattice (below) — the Boolean refinement
 *     mirroring Diaconescu's classical-Ω direction.
 *   - @c L @c = @c TernaryLogic → @c S stays Heyting-only (the
 *     "tricky to decide" escape door, Slice 8 constructive collapse).
 *
 * Complement is required @b unconditionally; its semantic strength
 * (Boolean vs Kleene-involution-only) is established at the
 * @c L-witness level via the parallel @c IsBooleanSubobjectLattice
 * concept, not gated inside this concept's body.
 *
 * @section lattice__IsSubobjectLattice_CT_Vocabulary
 * The concept body uses CT-vocabulary primitives: the carrier exposes
 * @c Ambient and @c logic_species typedefs, and free functions
 * @c meet, @c join, @c complement exist with the right shape.
 * Operator sugar (@c <=, @c &, @c |, @c !) lives in @c :sets as
 * forwarders.  Pierce-style stratification: abstract content in the
 * body, set-theoretic hints in the defaults.
 *
 * @tparam S The subobject carrier.  Must expose @c Ambient and
 *           @c logic_species typedefs (the latter satisfying
 *           @c IsLogicalSpecies).
 */
export template <typename S>
concept IsSubobjectLattice = requires(S a, S b) {
  /** @brief CT-vocabulary metadata: @c S exposes an ambient and a
   *         classifier logic species. */
  typename S::Ambient;
  typename S::logic_species;
  requires IsLogicalSpecies<typename S::logic_species>;

  /** @brief CT-vocabulary free functions for the binary lattice
   *         operations (binary product / coproduct in the subobject
   *         category).  Results inhabit the same subobject family —
   *         anchored on @c (S::Ambient, S::logic_species) per the
   *         family concept. */
  {
    meet(a, b)
  } -> IsSubobjectFamilyMember<typename S::Ambient, typename S::logic_species>;
  {
    join(a, b)
  } -> IsSubobjectFamilyMember<typename S::Ambient, typename S::logic_species>;
} && requires(S a) {
  /** @brief Complement is required unconditionally: classical carriers
   *         get a bona-fide Boolean complement, Kleene carriers get
   *         the involutive rotation that fails Boolean complement
   *         laws at @c Unknown.  The semantic strength is established
   *         at the @c L-witness level, not the concept boundary. */
  {
    complement(a)
  } -> IsSubobjectFamilyMember<typename S::Ambient, typename S::logic_species>;
};

/** @section lattice__IsSubobjectLattice_Order_Derivability
 *
 *  @brief The Form-chain @c ≤ relation on a subobject lattice is
 *         @b derivable from @c meet:
 *
 *      @c a @c ≤ @c b @c ⟺ @c meet(a, @c b) @c = @c a
 *                    @c ⟺ @c join(a, @c b) @c = @c b
 *
 *  This is a standard textbook equivalence (Birkhoff, "Lattice Theory",
 *  §1.4).  Hence @c IsSubobjectLattice's body does @b not require a
 *  separate @c subset_eq / @c operator<= clause — the row-1 (thin)
 *  inclusion content is recoverable from the row-3 (filtered) lattice
 *  ops.  Carriers exposing a direct @c operator<= or free
 *  @c subset_eq do so as @b set-side sugar (the Pierce-style
 *  stratification named in Slice 8's Sollbruchstelle text), not as a
 *  CT-vocabulary primitive of this concept. */

/**
 * @concept IsBooleanSubobjectLattice
 * @brief A subobject lattice over a @b classical classifier — the
 *        Boolean refinement that closes the L-parametric gap (#698
 *        Slice 9 review pass).
 *
 * @details
 * Mechanises the user's downstream intuition: parametrising a set
 * carrier with @c ClassicalLogic should automatically participate in
 * the Boolean lattice surface; parametrising with @c TernaryLogic
 * should not.  The previous parallel-track architecture left this
 * documentation-only ("easy to forget" per the review thread); this
 * concept makes it @b type-checked.
 *
 * @section lattice__IsBooleanSubobjectLattice_Justification
 * In topos-theoretic terms: a topos with classical subobject
 * classifier (@c Ω @c = @c bool) has Boolean @c Sub(A) for every
 * ambient @c A.  This is automatic — no semantic opt-in required at
 * the family level beyond the @c logic_species commitment.
 * @b Diaconescu's theorem (1975) further says the full Axiom of
 * Choice would propagate Boolean-ness in the reverse direction, but
 * the converse direction we use here (classical @c Ω @c ⟹ Boolean
 * @c Sub(A)) is uncontroversial.
 *
 * The concept is therefore just @c IsSubobjectLattice @c +
 * @c L @c = @c ClassicalLogic — no extra structural laws beyond
 * what @c IsSubobjectLattice already checks.  Kleene / Heyting
 * carriers fail closed because their @c logic_species @c ≠ @c
 * ClassicalLogic.
 *
 * @tparam S The subobject carrier.
 */
export template <typename S>
concept IsBooleanSubobjectLattice =
    IsSubobjectLattice<S> &&
    std::same_as<typename S::logic_species, ClassicalLogic>;

}  // namespace dedekind::category
