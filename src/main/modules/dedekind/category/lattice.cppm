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
 * Boole> once the Form-chain reaches row 7 AND a @c Sub<S>
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
 * undecidability as a first-class value via @c Kleene (Kleene K3).
 * Same Form-chain code, two regimes:
 *
 *   - @c L @c = @c Boole → @c Ω @c = @c bool → Form-chain rows
 *     1–7 → standard set theory falls out as the @b decidable collapse.
 *   - @c L @c = @c Kleene → @c Ω @c = @c Ternary → Form-chain rows
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
#include <cstddef>  // std::size_t — Slice 10 meeting-point pin.
#include <functional>
#include <limits>       // std::numeric_limits — LatticeBottom/Top
                        // specialisations for arithmetic carriers.
#include <ranges>       // std::ranges::iota_view, range_value_t — Slice 10
                        // niebloid-identity / tripwire pins.
#include <type_traits>  // std::is_arithmetic_v — same specialisations.

export module dedekind.category:lattice;

import :logic;
import :morphism;    // IsArrow: the Domain/Codomain surface a predicate leaf
                     // (χ:Domain→Ω) presents; carrier_of reuses it (below)
import :involution;  // is_involutive_v + IsInvolution (extracted from here)
import :posetal;     // IsPosetal — row 2 (thin + antisymmetric);
                     // IsOrderLatticeOperations — bottom-up algebraic surface
import :filtered;    // IsFilteredCategory — row 3 (directed thin cat)
import :species;     // is_codirected_v — cofiltered companion to is_directed_v
import :limit;       // IsInitialObject / IsTerminalObject — row 5
                     // universal-property witnesses (relaxed via tag
                     // discovery to admit LatticeBottom/LatticeTop).
import :cartesian;   // IsExponential — row 6 universal-property witness
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
                 typename Meet = decltype(std::ranges::min), typename L = Boole>
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
                 typename Meet = decltype(std::ranges::min), typename L = Boole>
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

/** @section lattice__Induced_Reduction_Laws
 *
 *  @brief The equational laws a lattice concept @b induces, as compile-time
 *         term-reduction rules over the term AST below.  Each law is a
 *         decomposable, independently-testable part; the assembled reducer
 *         (@c :lattice_term) applies exactly the laws the supplied carrier
 *         proves.  The richer the carrier's structure (a bounded chain is the
 *         sweet spot; @c bool the optimal witness), the more laws fire and the
 *         further a term collapses.
 *
 *  @details A law consumes two @b already-reduced operands and returns a
 *  @c std::type_identity of either the rewritten form or @c law_inactive (the
 *  law did not fire — the assembler then tries the next).  Order-dependent laws
 *  (units, glb/lub collapse) are keyed to the @b injected order @c Ord so that
 *  the
 *  boundary / comparison they use is the one this lattice's `∧`/`∨` induce, not
 *  another order the same carrier happens to bear (e.g. the numeric chain vs
 *  @c order::bit_subset_eq on an integer). */

/** @brief The type-level term AST.  Leaves are lattice-carrier types; the nodes
 *  combine them.
 *
 *  @section lattice__AST_dual_nature
 *  Primarily a @b compile-time tree: the reducer names @c Meet<A,B> only in
 *  @c type_identity / @c same_as contexts, which never instantiate the class,
 *  so for the reducer these stay empty tags with no completeness or
 *  default-construction obligation on the operands.
 *
 *  Secondarily, a node @b is the combined set VALUE when it is actually
 *  constructed from operand values (aggregate init @c Meet<A,B>{a,b}): it
 *  carries the operands and evaluates pointwise through the operands' shared
 *  @c logic_species (@c ∧ = @c L::AND, @c ∨ = @c L::OR, @c ¬ = @c L::RFL).  The
 *  @c operator() is @b guarded, so it exists only when the operands are
 *  callable predicates over one logic.  The type-level tag use is unaffected.
 *  This is the seam toward "the AST is the set" (#892): the set operators now
 *  return these nodes, and the predicate-nested collapse representation they
 *  replaced (the retired @c AndPredicate / @c OrPredicate) has been removed. */
export template <typename A, typename B>
struct Meet {  // A ∧ B
  A lhs;
  B rhs;
  /** @brief Evaluate the meet at @c x, through the @c π_1 / @c π_2 accessors
   *  (operand storage touched in one place).  With a @c logic_species the
   *  combination is that logic's @c AND (honouring Kleene / Heyting Ω); for
   *  plain bool-returning callables (comprehension lambdas) it is the bare
   *  @c &&.
   *  @note LAYERING: the node stays LATTICE ALGEBRA only; the Set-specific
   *  subobject lift (@c Member / @c ι / @c IsSet) lives DOWNSTREAM in @c sets.
   *  @note The @c logic_species branch feeds operand results straight to
   *  @c L::AND, so it assumes operands return the RAW @c L::Ω.  The @c :sets
   *  surface always does (membership is @c lift_logic<L>, which yields @c L::Ω,
   *  never a @c Truth<L> wrapper), so this is sound there.  A callable whose
   *  codomain is @c Truth<L> would need unwrapping first; that path is not
   *  reached through @c sets. */
  template <typename X>
    requires requires(const A& l, const B& r, const X& x) {
      l(x);
      r(x);
    }
  constexpr auto operator()(const X& x) const {
    if constexpr (requires { typename A::logic_species; }) {
      return A::logic_species::AND(π_1(*this)(x), π_2(*this)(x));
    } else {
      return π_1(*this)(x) && π_2(*this)(x);
    }
  }
};
export template <typename A, typename B>
struct Join {  // A ∨ B
  A lhs;
  B rhs;
  /** @brief Evaluate the join at @c x, dual to @c Meet::operator().  With a
   *  @c logic_species the combination is that logic's @c OR; for plain
   *  bool-returning callables it is the bare @c ||.  Operand storage is touched
   *  only through the @c π_1 / @c π_2 accessors. */
  template <typename X>
    requires requires(const A& l, const B& r, const X& x) {
      l(x);
      r(x);
    }
  constexpr auto operator()(const X& x) const {
    if constexpr (requires { typename A::logic_species; }) {
      return A::logic_species::OR(π_1(*this)(x), π_2(*this)(x));
    } else {
      return π_1(*this)(x) || π_2(*this)(x);
    }
  }
};
export template <typename A>
struct Not {  // ¬A (complement)
  A base;
  /** @brief Evaluate the complement at @c x.  With a @c logic_species the value
   *  is that logic's @c RFL (reflection / negation, honouring Kleene / Heyting
   *  Ω); for a plain bool-returning callable it is the bare @c !. */
  template <typename X>
    requires requires(const A& b, const X& x) { b(x); }
  constexpr auto operator()(const X& x) const {
    if constexpr (requires { typename A::logic_species; }) {
      return A::logic_species::RFL(base(x));
    } else {
      return !base(x);
    }
  }
};

/** @section lattice__AST_as_product
 *  A binary node @b is the categorical product / coproduct of its operands, so
 *  it inhabits @c category::IsProduct (the role @c sets::AndPredicate /
 *  @c OrPredicate held before #892, now hoisted to where the AST lives).  The
 * operands are recovered by the free @c π_1 / @c π_2 accessors (found by ADL,
 * overriding the default @c .first / @c .second projection), and @c MakeMeet /
 * @c MakeJoin are the pairing FACTORIES @f$\langle -,- \rangle: A \times B \to
 * P@f$, the
 *  @c Op that @c IsProduct names, whose @b result type (@c Meet vs @c Join)
 *  discriminates the meet-pairing (pullback) from the join-pairing (pushout).
 *  The meet / join distinction is the reduction (@c ∧ vs @c ∨), not the
 * pairing; both nodes store and project both operands. */
/** @brief π_1: the left-operand accessor of a binary node, returned @b by
 *  const-reference.  The operand may itself be a whole set expression (a nested
 *  node), and @c π_1 / @c π_2 are pure projections.  They must not copy the
 *  sub-structure.  When the operand is a set, @c π_1(node) is then a bona fide
 *  reference to that @c IsSet (the downstream @c sets lift relies on this).
 *  Still @c ->convertible_to<A>, so @c IsProduct is satisfied. */
export template <typename A, typename B>
constexpr const A& π_1(const Meet<A, B>& m) {
  return m.lhs;
}
/** @brief π_2: the right-operand accessor of a meet, by const-reference (see
 *  @c π_1). */
export template <typename A, typename B>
constexpr const B& π_2(const Meet<A, B>& m) {
  return m.rhs;
}
/** @brief π_1: the left-operand accessor of a join, by const-reference (see the
 *  meet overload). */
export template <typename A, typename B>
constexpr const A& π_1(const Join<A, B>& j) {
  return j.lhs;
}
/** @brief π_2: the right-operand accessor of a join, by const-reference. */
export template <typename A, typename B>
constexpr const B& π_2(const Join<A, B>& j) {
  return j.rhs;
}

/** @brief The pairing factory for the meet (∧, pullback): @c ⟨-,-⟩ : A×B→Meet.
 */
export struct MakeMeet {
  template <typename A, typename B>
  constexpr Meet<A, B> operator()(const A& a, const B& b) const {
    return {a, b};
  }
};
/** @brief The pairing factory for the join (∨, pushout): @c ⟨-,-⟩ : A×B→Join.
 */
export struct MakeJoin {
  template <typename A, typename B>
  constexpr Join<A, B> operator()(const A& a, const B& b) const {
    return {a, b};
  }
};

// The projection MORPHISMS Π_1 / Π_2 and the stronger concept
// IsArrowProduct (an IsProduct whose projections are arrows) live in the
// :limit partition, next to IsProduct and the free π_1 / π_2 accessors.  The
// reducer nodes Meet / Join (and their downstream sets lifts) target
// IsArrowProduct, so their projections are pinned to arrow-shaped signatures.

/** @brief Sentinel: a law that does not fire on the given node. */
export struct law_inactive {};

/** @brief The default @c Ord: the carrier's own canonical @c std::less_equal
 *  order (the order bundled with its axioms).  A caller whose `∧`/`∨` mean a
 *  different lattice on the same carrier injects that relation as @c Ord. */
export struct canonical_order {};

/** @brief The comparator @c Ord resolves to for a carrier @c T:
 *  @c std::less_equal<T> for @c canonical_order, otherwise @c Ord itself. */
export template <typename T, typename Ord>
using resolved_order_t = std::conditional_t<std::same_as<Ord, canonical_order>,
                                            std::less_equal<T>, Ord>;

// ── Boundedness, keyed to the injected order ──────────────────────────────
// A ⊥/⊤ marker counts for order @c Ord only when its OWN registered relation
// is the one @c Ord resolves to over the same carrier.  A bound of a different
// order on that carrier is therefore not mistaken for this lattice's bound.

/** @brief Is @c X the registered bottom of the lattice whose order is @c Ord?
 */
export template <typename X, typename Ord>
struct is_lattice_bottom_for : std::false_type {};
export template <typename T, typename Rel, typename Ord>
struct is_lattice_bottom_for<LatticeBottom<T, Rel>, Ord>
    : std::bool_constant<std::same_as<Rel, resolved_order_t<T, Ord>>> {};
export template <typename X, typename Ord>
inline constexpr bool is_lattice_bottom_for_v =
    is_lattice_bottom_for<X, Ord>::value;

/** @brief Is @c X the registered top of the lattice whose order is @c Ord? */
export template <typename X, typename Ord>
struct is_lattice_top_for : std::false_type {};
export template <typename T, typename Rel, typename Ord>
struct is_lattice_top_for<LatticeTop<T, Rel>, Ord>
    : std::bool_constant<std::same_as<Rel, resolved_order_t<T, Ord>>> {};
export template <typename X, typename Ord>
inline constexpr bool is_lattice_top_for_v = is_lattice_top_for<X, Ord>::value;

/** @brief The carrier type of a reduced term: a value-bearing leaf's @c ::value
 *  type, @b propagated through @c Meet / @c Join composites (a subterm that did
 *  not collapse, e.g. @c Join of two `≤`-incomparable elements of a bit-subset
 *  lattice), or @c void when the carrier is unknown (an order-opaque leaf) @b
 * or
 *  @b inhomogeneous.  A composite propagates a carrier only when @b both
 *  children agree on a known one, so a nested mixed-carrier term (at any depth)
 *  is @c void and fails the @c SameCarrier guard below (fail-closed
 * recursively). */
export template <typename X>
struct carrier_of {
  using type = void;
};
export template <typename X>
  requires requires { X::value; }
struct carrier_of<X> {
  using type = std::remove_cvref_t<decltype(X::value)>;
};
/** @brief Carrier of a predicate / subobject leaf.  Such a leaf is an arrow
 *  χ:Domain→Ω (@c IsArrow), not a wrapped value, so its carrier is the arrow's
 *  @b Domain.  Reading it through @c IsArrow (the category arrow surface)
 * rather than a bespoke typedef probe ties the reducer into @c category.  The
 *  carrier-based gates (@c SameCarrier, distributivity, complement, De Morgan
 *  negation) then apply to set expressions too.  An element leaf instead
 * carries
 *  @c ::value, a point 1→T whose carrier is the value's type; the @c ::value
 *  specialisation above handles it.  The two spellings extract the carrier from
 *  opposite ends of the arrow, so both coexist. */
export template <typename X>
  requires(IsArrow<X> && !requires { X::value; })
struct carrier_of<X> {
  using type = typename std::remove_cvref_t<X>::Domain;
};
namespace detail_carrier {
// The carrier shared by two children, or void if they differ or are unknown.
template <typename CA, typename CB>
using common =
    std::conditional_t<!std::is_void_v<CA> && std::is_same_v<CA, CB>, CA, void>;
}  // namespace detail_carrier
export template <typename A, typename B>
struct carrier_of<Meet<A, B>> {
  using type = detail_carrier::common<typename carrier_of<A>::type,
                                      typename carrier_of<B>::type>;
};
export template <typename A, typename B>
struct carrier_of<Join<A, B>> {
  using type = detail_carrier::common<typename carrier_of<A>::type,
                                      typename carrier_of<B>::type>;
};
export template <typename A>
struct carrier_of<Not<A>> {
  using type = typename carrier_of<A>::type;
};
export template <typename X>
using carrier_of_t = typename carrier_of<X>::type;

/** @brief Does the term @c X mix carriers anywhere — either two @b different
 *  known carriers, @b or a known carrier with an @b opaque (unknown) one?  Only
 *  a @b uniform term fails this: all leaves opaque (the all-unknown case), or
 *  all leaves the @b same known carrier.  A known/opaque mix counts as mixed
 *  because an opaque leaf carries no evidence it belongs to the known carrier's
 *  lattice.  The structural laws use this to fail closed on any mixed term
 *  while still firing on the all-opaque case.  (`carrier_of` reports @c void
 * for
 *  @b both all-opaque and mixed, so a per-node @c is_void mismatch is what
 *  distinguishes a known/opaque boundary here.) */
export template <typename X>
inline constexpr bool has_mixed_carrier_v = false;  // a leaf mixes nothing
export template <typename A>
inline constexpr bool has_mixed_carrier_v<Not<A>> = has_mixed_carrier_v<A>;
export template <typename A, typename B>
inline constexpr bool has_mixed_carrier_v<Meet<A, B>> =
    has_mixed_carrier_v<A> || has_mixed_carrier_v<B> ||
    (std::is_void_v<carrier_of_t<A>> != std::is_void_v<carrier_of_t<B>>) ||
    (!std::is_void_v<carrier_of_t<A>> && !std::is_void_v<carrier_of_t<B>> &&
     !std::is_same_v<carrier_of_t<A>, carrier_of_t<B>>);
export template <typename A, typename B>
inline constexpr bool has_mixed_carrier_v<Join<A, B>> =
    has_mixed_carrier_v<A> || has_mixed_carrier_v<B> ||
    (std::is_void_v<carrier_of_t<A>> != std::is_void_v<carrier_of_t<B>>) ||
    (!std::is_void_v<carrier_of_t<A>> && !std::is_void_v<carrier_of_t<B>> &&
     !std::is_same_v<carrier_of_t<A>, carrier_of_t<B>>);

/** @concept SameCarrier
 *  @brief Do both operands live in the @b same, known carrier?  The boundary
 *  laws require this so a bound of one carrier (e.g. an @c int @c ⊤) is never
 *  applied as the unit / annihilator of a term whose other operand lives in a
 *  different carrier: a mixed-carrier term (@c ⊤ᵢₙₜ @c ∧ @c bool-leaf) fails
 *  closed.  Because the carrier is read via @c carrier_of, the laws still apply
 *  to an arbitrary same-carrier @b composite subterm (@c ⊤∧X=X even when @c X
 *  is an un-collapsed @c Meet / @c Join), not only to leaves.  (The glb/lub
 *  law forces a shared carrier through @c OrderComparable; this is its
 *  boundary-law counterpart.) */
export template <typename A, typename B>
concept SameCarrier = !std::same_as<carrier_of_t<A>, void> &&
                      std::same_as<carrier_of_t<A>, carrier_of_t<B>>;

/** @brief Law induced by a @b bounded lattice (IsBoundedLatticeCategory): the
 *  unit and annihilator.  Meet: @c ⊥∧X=⊥ (annihilator), @c ⊤∧X=X (unit).
 *  Fires only when both operands share a carrier (mixed-carrier ⟹ inactive). */
export template <typename RA, typename RB, typename Ord>
consteval auto meet_bounded_law() {
  if constexpr (!SameCarrier<RA, RB>) {
    return std::type_identity<law_inactive>{};  // mixed carrier ⟹ fail closed
  } else if constexpr (is_lattice_bottom_for_v<RA, Ord>) {
    return std::type_identity<RA>{};  // ⊥ ∧ X = ⊥
  } else if constexpr (is_lattice_bottom_for_v<RB, Ord>) {
    return std::type_identity<RB>{};
  } else if constexpr (is_lattice_top_for_v<RA, Ord>) {
    return std::type_identity<RB>{};  // ⊤ ∧ X = X
  } else if constexpr (is_lattice_top_for_v<RB, Ord>) {
    return std::type_identity<RA>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

/** @brief The join dual: @c ⊤∨X=⊤ (annihilator), @c ⊥∨X=X (unit).
 *  Likewise fires only when both operands share a carrier. */
export template <typename RA, typename RB, typename Ord>
consteval auto join_bounded_law() {
  if constexpr (!SameCarrier<RA, RB>) {
    return std::type_identity<law_inactive>{};  // mixed carrier ⟹ fail closed
  } else if constexpr (is_lattice_top_for_v<RA, Ord>) {
    return std::type_identity<RA>{};  // ⊤ ∨ X = ⊤
  } else if constexpr (is_lattice_top_for_v<RB, Ord>) {
    return std::type_identity<RB>{};
  } else if constexpr (is_lattice_bottom_for_v<RA, Ord>) {
    return std::type_identity<RB>{};  // ⊥ ∨ X = X
  } else if constexpr (is_lattice_bottom_for_v<RB, Ord>) {
    return std::type_identity<RA>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

/** @brief Is @c X a leaf whose VALUE is determined by its TYPE?  Idempotence
 *  @c X∧X=X is sound only for such a leaf.  Two same-type instances are then
 *  necessarily the same set, so collapsing them is correct.  The default is
 *  @c std::is_empty_v<X>, true for a stateless tag or an NTTP-encoded carrier.
 *  A carrier with RUNTIME-STATEFUL leaves specialises this to @c false for
 * them.
 *  @c sets does so for @c Set<T,L,P> whose predicate @c P carries a runtime
 *  field (e.g.\ a @c BooleanEqPredicate's @c expected).  The type-based
 *  idempotence then does not collapse two distinct-but-same-type values. */
export template <typename X>
inline constexpr bool idempotent_leaf_v =
    std::is_empty_v<std::remove_cvref_t<X>>;

/** @brief A compound node is value-determined iff its operands are.  So a
 *  @c Meet / @c Join / @c Not of value-determined leaves stays collapsible,
 *  while one carrying a runtime-stateful leaf does not.  This keeps the
 *  value-safety gate @b recursive rather than treating every operand-storing
 *  node as stateful (@c Meet / @c Join / @c Not now store their operands, so
 * the bare @c std::is_empty_v default would report @c false for all of them).
 */
template <typename A, typename B>
inline constexpr bool idempotent_leaf_v<Meet<A, B>> =
    idempotent_leaf_v<A> && idempotent_leaf_v<B>;
template <typename A, typename B>
inline constexpr bool idempotent_leaf_v<Join<A, B>> =
    idempotent_leaf_v<A> && idempotent_leaf_v<B>;
template <typename A>
inline constexpr bool idempotent_leaf_v<Not<A>> = idempotent_leaf_v<A>;

/** @concept IsIdempotentLeaf
 *  @brief @c T's values are determined by its type, so a @b type-only lattice
 *  law (@c X∧X=X idempotence, @c a∧¬a=⊥ complement) collapses @b soundly on it:
 *  two same-type instances are necessarily the same value.  A runtime-stateful
 *  leaf (say a @c SingletonSet holding a value, or a predicate with an
 *  @c expected field) is @b not idempotent, so those laws must not fire on it,
 *  else @c Meet{S{7},¬S{3}} would wrongly collapse to @c ⊥.  This is the
 *  value-safety gate the type-level reducer shares with the value-first
 *  @c :lattice_term reducer (#922).  Recursive over @c Meet / @c Join / @c Not
 *  through @c idempotent_leaf_v. */
export template <typename T>
concept IsIdempotentLeaf = idempotent_leaf_v<T>;

/** @brief Law induced by a @b (meet/join-)semilattice: idempotence @c X∧X=X /
 *  @c X∨X=X.  Structural: it holds for the lattice operation itself, so it does
 *  not depend on the carrier's order.  It fires even for order-incomparable
 *  opaque leaves.  It is gated on @c idempotent_leaf_v, so a runtime-stateful
 *  leaf (two same-type-but-distinct instances) is not collapsed. */
export template <typename RA, typename RB>
consteval auto idempotent_law() {
  if constexpr (std::same_as<RA, RB> && IsIdempotentLeaf<RA>) {
    return std::type_identity<RA>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

/** @brief Is @c Elem one of the operands of the @c Join node @c Node? */
export template <typename Elem, typename Node>
inline constexpr bool is_join_containing_v = false;
export template <typename Elem, typename A, typename B>
inline constexpr bool is_join_containing_v<Elem, Join<A, B>> =
    std::same_as<Elem, A> || std::same_as<Elem, B>;

/** @brief Is @c Elem one of the operands of the @c Meet node @c Node? */
export template <typename Elem, typename Node>
inline constexpr bool is_meet_containing_v = false;
export template <typename Elem, typename A, typename B>
inline constexpr bool is_meet_containing_v<Elem, Meet<A, B>> =
    std::same_as<Elem, A> || std::same_as<Elem, B>;

/** @brief Law induced by a @b lattice (IsLatticeCategory): @b structural
 *  absorption @c a∧(a∨b)=a.  @b Structural like idempotence (a lattice axiom
 *  needing no order or carrier), so it fires even for order-incomparable opaque
 *  leaves; @b unlike the glb collapse (which needs comparable operands).  One
 *  level only: nested / associatively-buried occurrences (@c a∧((a∨b)∨c)) await
 *  an associativity-flattening law (tracked on the reducer epic #890).  Fails
 *  closed on a @b mixed-carrier term (@c has_mixed_carrier_v) so it upholds the
 *  same recursive fail-closed invariant as @c carrier_of / the boundary laws;
 *  the all-opaque (unknown-carrier) case still fires. */
export template <typename RA, typename RB>
consteval auto meet_structural_absorption_law() {
  if constexpr (has_mixed_carrier_v<Meet<RA, RB>>) {
    return std::type_identity<law_inactive>{};  // mixed carrier ⟹ fail closed
  } else if constexpr (is_join_containing_v<RA, RB> && IsIdempotentLeaf<RA>) {
    return std::type_identity<RA>{};  // a ∧ (a ∨ b) = a
  } else if constexpr (is_join_containing_v<RB, RA> && IsIdempotentLeaf<RB>) {
    return std::type_identity<RB>{};  // (a ∨ b) ∧ a = a
  } else {
    return std::type_identity<law_inactive>{};
  }
}

/** @brief The join dual: @c a∨(a∧b)=a.  Likewise fails closed on mixed
 * carriers. */
export template <typename RA, typename RB>
consteval auto join_structural_absorption_law() {
  if constexpr (has_mixed_carrier_v<Join<RA, RB>>) {
    return std::type_identity<law_inactive>{};  // mixed carrier ⟹ fail closed
  } else if constexpr (is_meet_containing_v<RA, RB> && IsIdempotentLeaf<RA>) {
    return std::type_identity<RA>{};  // a ∨ (a ∧ b) = a
  } else if constexpr (is_meet_containing_v<RB, RA> && IsIdempotentLeaf<RB>) {
    return std::type_identity<RB>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

// ── Absorption, decided by the injected order ─────────────────────────────

/** @concept OrderComparable
 *  @brief Can @c A and @c B be compared in the injected order @c Ord, @b at
 *  compile time?  Both must expose @c ::value of one carrier type @c T that is
 *  @c IsPosetal under @c Ord's resolved relation, and that relation must be
 *  @b default-constructible and @b constexpr-callable on those values.  A
 *  registered-but-runtime-only or stateful order therefore fails this guard and
 *  the glb/lub collapse stays @b inactive (fail-closed) rather than
 *  hard-erroring. */
export template <typename A, typename B, typename Ord>
concept OrderComparable =
    requires {
      A::value;
      B::value;
    } &&
    std::same_as<std::remove_cvref_t<decltype(A::value)>,
                 std::remove_cvref_t<decltype(B::value)>> &&
    IsPosetal<std::remove_cvref_t<decltype(A::value)>,
              resolved_order_t<std::remove_cvref_t<decltype(A::value)>, Ord>> &&
    std::default_initializable<
        resolved_order_t<std::remove_cvref_t<decltype(A::value)>, Ord>> &&
    requires {
      {
        resolved_order_t<std::remove_cvref_t<decltype(A::value)>, Ord>{}(
            A::value, B::value)
      } -> std::convertible_to<bool>;
      // Constant-evaluability gate: forces the comparison into a constant
      // expression, so a non-constexpr order fails the concept (fail-closed).
      typename std::bool_constant<(
          resolved_order_t<std::remove_cvref_t<decltype(A::value)>, Ord>{}(
              A::value, B::value),
          true)>;
    };

/** @brief Is @c A ≤ @c B in the injected order @c Ord? (`false` when the pair
 *  is not compile-time comparable there — the glb/lub law then does not
 *  fire.) */
export template <typename A, typename B, typename Ord>
consteval bool order_leq() {
  if constexpr (OrderComparable<A, B, Ord>) {
    using T = std::remove_cvref_t<decltype(A::value)>;
    return resolved_order_t<T, Ord>{}(A::value, B::value);
  } else {
    return false;
  }
}

/** @brief Law induced by a @b lattice's order-meet consistency (@c RA≤RB @c ⟺
 *  @c RA∧RB=RA): for @c ≤-comparable operands the meet is their @b glb (the
 *  smaller).  @b Note this is @b not the structural absorption identity
 *  @c a∧(a∨b)=a (which needs no comparison); that is the separate
 *  @c meet_structural_absorption_law.  Here only comparable operands collapse.
 */
export template <typename RA, typename RB, typename Ord>
consteval auto meet_glb_law() {
  if constexpr (order_leq<RA, RB, Ord>()) {
    return std::type_identity<RA>{};
  } else if constexpr (order_leq<RB, RA, Ord>()) {
    return std::type_identity<RB>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

/** @brief The join dual: for comparable operands the join is their @b lub (the
 *  larger), @c RA≤RB ⟹ RA∨RB=RB.  (Structural absorption @c a∨(a∧b)=a is the
 *  separate @c join_structural_absorption_law.) */
export template <typename RA, typename RB, typename Ord>
consteval auto join_lub_law() {
  if constexpr (order_leq<RA, RB, Ord>()) {
    return std::type_identity<RB>{};
  } else if constexpr (order_leq<RB, RA, Ord>()) {
    return std::type_identity<RA>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

// ── Distributivity (induced by IsDistributiveLattice) ─────────────────────

/** @brief Is @c X a @c Join node? */
export template <typename X>
inline constexpr bool is_join_node_v = false;
export template <typename A, typename B>
inline constexpr bool is_join_node_v<Join<A, B>> = true;

/** @brief Is the lattice (carrier @c T, order @c Ord) distributive?  Keyed to
 *  the order like the boundedness markers.  The canonical (@c std::less_equal
 *  chain) case derives from the @b concept gates @c IsLatticeCategory<T> @c &&
 *  @c IsOrderDistributiveLatticeOperations<T> — so a carrier that is not a
 *  lattice under @c std::less_equal is @b not licensed to distribute (NB the
 *  bare @c is_distributive_v<T,max,min> trait is unconditionally @c true, which
 *  would license any carrier — hence the concept gate).  A NON-chain
 *  distributive lattice (a custom @c Ord) opts in by specialising this to
 *  @c true (the Jlt assertion — the same posture as the injected total order).
 *  A canonical chain is gated distributive but never actually distributes: its
 *  joins glb/lub-collapse first. */
export template <typename T, typename Ord>
inline constexpr bool is_distributive_lattice_for_v =
    std::same_as<resolved_order_t<T, Ord>, std::less_equal<T>> &&
    IsLatticeCategory<T> && IsOrderDistributiveLatticeOperations<T>;

// Distribute a meet over a join node: X ∧ (P ∨ Q) = (X ∧ P) ∨ (X ∧ Q).
template <typename X, typename JoinNode>
struct distribute_meet_over;
template <typename X, typename P, typename Q>
struct distribute_meet_over<X, Join<P, Q>> {
  using type = Join<Meet<X, P>, Meet<X, Q>>;
};

/** @brief Law induced by a @b distributive lattice: distribute meet over join,
 *  @c X∧(P∨Q) → (X∧P)∨(X∧Q), driving toward a join-of-meets (DNF).  @b One
 *  direction only (meet over join, never join over meet), so re-reduction of
 *  the result — which the assembler performs — terminates.  Gated on both
 *  operands sharing a carrier (@c SameCarrier) whose lattice is distributive
 *  under @c Ord; a chain is gated but pre-empted by the glb collapse, so this
 *  fires only on a genuine non-chain distributive lattice. */
export template <typename RA, typename RB, typename Ord>
consteval auto meet_distributivity_law() {
  if constexpr (!(SameCarrier<RA, RB> &&
                  is_distributive_lattice_for_v<carrier_of_t<RA>, Ord>)) {
    return std::type_identity<law_inactive>{};
  } else if constexpr (is_join_node_v<RB>) {
    return std::type_identity<typename distribute_meet_over<RA, RB>::type>{};
  } else if constexpr (is_join_node_v<RA>) {
    return std::type_identity<typename distribute_meet_over<RB, RA>::type>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

// ── De Morgan negation (an involutive, order-reversing negation) ──────────
// This is @b De Morgan negation, NOT a genuine complement: the module reserves
// @c is_complement_v for a real complement (with @c a∧¬a=⊥ / @c a∨¬a=⊤), and
// the paper likewise distinguishes involutive De Morgan negation (e.g. the K3
// reflection) from complement.  These laws (involution + De Morgan) hold in any
// De Morgan algebra; the complement collapse @c a∧¬a→⊥ is a separate law that
// needs a genuinely complemented lattice (tracked on #890).

/** @brief Is @c X a @c Not node? */
export template <typename X>
inline constexpr bool is_not_node_v = false;
export template <typename A>
inline constexpr bool is_not_node_v<Not<A>> = true;

/** @brief Is @c X a @c Meet node? */
export template <typename X>
inline constexpr bool is_meet_node_v = false;
export template <typename A, typename B>
inline constexpr bool is_meet_node_v<Meet<A, B>> = true;

/** @brief Does the lattice (carrier @c T, order @c Ord) carry an @b involutive
 *  order-reversing @b negation (@c ¬¬a=a and the De Morgan laws)?  Keyed to the
 *  order and opt-in (default @c false — the Jlt assertion), the same posture as
 *  @c is_distributive_lattice_for_v. */
export template <typename T, typename Ord>
inline constexpr bool is_de_morgan_negation_for_v = false;
/** @brief Canonical @c bool is a De Morgan (indeed Boolean) algebra: @c ¬ is
 *  @c std::logical_not, involutive with the De Morgan laws — the production
 *  carrier that activates the negation laws (the all-laws-fire oracle). */
export template <>
inline constexpr bool is_de_morgan_negation_for_v<bool, canonical_order> = true;

// The De Morgan / involution rewrite of ¬(node): push the negation inward one
// level.  Defined only for the nodes it rewrites (Not / Meet / Join); a bare
// ¬leaf is already negation-normal and is left to the caller.
template <typename Node>
struct de_morgan_of;
template <typename B>
struct de_morgan_of<Not<B>> {
  using type = B;  // ¬¬B → B (involution)
};
template <typename P, typename Q>
struct de_morgan_of<Meet<P, Q>> {
  using type = Join<Not<P>, Not<Q>>;  // ¬(P∧Q) → ¬P ∨ ¬Q
};
template <typename P, typename Q>
struct de_morgan_of<Join<P, Q>> {
  using type = Meet<Not<P>, Not<Q>>;  // ¬(P∨Q) → ¬P ∧ ¬Q
};

/** @brief Law induced by an @b involutive De Morgan negation: rewrite
 *  @c ¬(reduced) by involution (@c ¬¬A→A) and De Morgan (@c ¬(A∧B)→¬A∨¬B,
 *  @c ¬(A∨B)→¬A∧¬B), pushing the negation toward the leaves (negation-normal
 *  form).  Gated on the carrier's lattice carrying such a negation under @c
 * Ord; a bare @c ¬leaf is already normal and stays (@c law_inactive).  The
 * driver re-reduces the pushed-down result (termination: @c ¬ strictly
 * descends). */
export template <typename RA, typename Ord>
consteval auto de_morgan_law() {
  if constexpr (!is_de_morgan_negation_for_v<carrier_of_t<RA>, Ord>) {
    return std::type_identity<law_inactive>{};
  } else if constexpr (is_not_node_v<RA> || is_meet_node_v<RA> ||
                       is_join_node_v<RA>) {
    return std::type_identity<typename de_morgan_of<RA>::type>{};
  } else {
    return std::type_identity<law_inactive>{};  // ¬leaf is negation-normal
  }
}

// ── Complement collapse (induced by a genuinely COMPLEMENTED lattice) ─────
// STRONGER than De Morgan negation: a complemented lattice additionally proves
// the complement laws a∧¬a=⊥ (contradiction) and a∨¬a=⊤ (excluded middle).  A
// De Morgan algebra alone (e.g. Kleene K3) does NOT (a∧¬a can be the middle).
// so this is a distinct, opt-in gate.

/** @brief Does the lattice (carrier @c T, order @c Ord) carry a genuine
 *  @b complement (@c a∧¬a=⊥, @c a∨¬a=⊤), not merely a De Morgan negation? Keyed
 *  to the order and opt-in (default @c false).  A complemented lattice is also
 * a De Morgan algebra, so a carrier asserting this should also assert
 *  @c is_de_morgan_negation_for_v. */
export template <typename T, typename Ord>
inline constexpr bool is_complemented_lattice_for_v = false;
/** @brief Canonical @c bool is a Boolean (hence complemented) lattice. */
export template <>
inline constexpr bool is_complemented_lattice_for_v<bool, canonical_order> =
    true;

/** @brief Are @c RA and @c RB a complement pair (@c RB=¬RA or @c RA=¬RB)? */
export template <typename RA, typename RB>
inline constexpr bool is_complement_pair_v =
    std::same_as<RB, Not<RA>> || std::same_as<RA, Not<RB>>;

/** @brief Law induced by a @b complemented lattice: the complement collapse
 *  @c a∧¬a→⊥ (meet) / @c a∨¬a→⊤ (join).  The bottom / top produced is the
 *  carrier's registered @c LatticeBottom / @c LatticeTop over its resolved
 *  order (for sets, that is @c Ø / @c 𝔸 once the sets layer registers them).
 *  Gated on the carrier being complemented under @c Ord; else inactive. */
export template <typename RA, typename RB, typename Ord>
consteval auto meet_complement_law() {
  // @c IsIdempotent gate: @c a∧¬a=⊥ is a type-only match, so it is sound only
  // when the leaves are value-determined; a runtime-stateful pair like
  // @c S{7}∧¬S{3} matches the type pair but is @b not empty (#922).
  if constexpr (is_complement_pair_v<RA, RB> && IsIdempotentLeaf<RA> &&
                IsIdempotentLeaf<RB> &&
                is_complemented_lattice_for_v<carrier_of_t<RA>, Ord>) {
    return std::type_identity<LatticeBottom<
        carrier_of_t<RA>, resolved_order_t<carrier_of_t<RA>, Ord>>>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

/** @brief The join dual: @c a∨¬a→⊤ (excluded middle). */
export template <typename RA, typename RB, typename Ord>
consteval auto join_complement_law() {
  // @c IsIdempotent gate (see @c meet_complement_law): @c a∨¬a=⊤ collapses
  // soundly only on value-determined leaves.
  if constexpr (is_complement_pair_v<RA, RB> && IsIdempotentLeaf<RA> &&
                IsIdempotentLeaf<RB> &&
                is_complemented_lattice_for_v<carrier_of_t<RA>, Ord>) {
    return std::type_identity<LatticeTop<
        carrier_of_t<RA>, resolved_order_t<carrier_of_t<RA>, Ord>>>{};
  } else {
    return std::type_identity<law_inactive>{};
  }
}

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

// The @c is_involutive trait machinery (@c is_involutive_v + the
// @c logical_not / @c bit_not opt-ins) has been extracted to the @c :involution
// atom --- its long-anticipated home (see @c :involution's @c @section
// Why_A_Seed).  It is re-exported through the @c :involution import above.

/**
 * @concept IsInvolutiveEndofunctor
 * @brief A callable @c F is an involutive endofunctor on @c T --- invocable
 *        @c T @c → @c T with @c F² @c ≅ @c Id.  This is exactly @c
 *        :involution's @c IsInvolution under the lattice-complement reading
 *        (used by @c IsBooleanLatticeCategory, #698 Slice 7); it reuses that
 *        concept rather than re-stating the shape + @c is_involutive_v
 *        certificate.
 */
export template <typename F, typename T>
concept IsInvolutiveEndofunctor = IsInvolution<F, T>;

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
                 typename Meet = decltype(std::ranges::min), typename L = Boole>
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
                 typename Not = std::logical_not<T>, typename L = Boole>
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
    !IsBooleanLatticeCategory<
        std::size_t, std::less_equal<std::size_t>, decltype(std::ranges::max),
        decltype(std::ranges::min), std::bit_not<std::size_t>>,
    "size_t under std::less_equal with std::bit_not is NOT a Boolean "
    "lattice — same Honest Rejection as int (the bitwise complement "
    "doesn't match the order-theoretic meet/join).  Bitwise route is "
    "#710's territory.");

static_assert(
    IsLatticeCategory<int, std::less_equal<int>, decltype(std::ranges::max),
                      decltype(std::ranges::min)>,
    "Niebloid identity: the Form-chain Meet / Join slot "
    "defaults ARE the std::ranges niebloids "
    "(std::ranges::min, std::ranges::max).  Pinning the "
    "explicit instantiation here makes the structural fit "
    "type-checked at compile time.");

static_assert(
    std::same_as<std::ranges::range_value_t<std::ranges::iota_view<int, int>>,
                 int>,
    "Tripwire: std::ranges::iota_view's value_type is the integral "
    "carrier the niebloid-identity pin above relies on.  If iota_view's "
    "value_type semantics ever changed (e.g.\\ producing a wrapper type "
    "instead of the underlying integer), the meeting-point structure "
    "for std::ranges would surface the divergence here.");

/**
 * @concept IsSubobjectFamilyMember
 * @brief A type @c R is a member of the subobject family over ambient
 *        @c A in classifier @c L.
 *
 * @details
 * The lattice operations on @c Sub(A) (meet, join, complement) close
 * over the @em family of subobjects of @c A in @c L, not over a single
 * predicate-type closed carrier.  Concretely, when no structural collapse
 * fires (a @c structured_and / @c structured_or reduction to a halfspace,
 * interval, @c Singleton, @c Ø or @c UniversalSet, or an @c IsComplementPair
 * short-circuit), @c A @c & @c B returns a @c MeetSet<A,B> carrying both
 * operand sets (dually, @c | returns a @c JoinSet<A,B>).  The node is a
 * different type, but over the same @c Ambient and @c logic_species.
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
  typename std::remove_cvref_t<R>::Domain;
  typename std::remove_cvref_t<R>::logic_species;
  requires std::same_as<typename std::remove_cvref_t<R>::Domain, A>;
  requires std::same_as<typename std::remove_cvref_t<R>::logic_species, L>;
};

/**
 * @concept IsSubobjectLattice
 * @brief A type @c S is the carrier of a subobject lattice over
 *        @c S::Domain, with lattice structure induced pointwise from
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
 *   - @c L @c = @c Boole → @c S participates in
 *     @c IsBooleanSubobjectLattice (below) — the Boolean refinement
 *     mirroring Diaconescu's classical-Ω direction.
 *   - @c L @c = @c Kleene → @c S stays Heyting-only (the
 *     "tricky to decide" escape door, Slice 8 constructive collapse).
 *
 * Complement is required @b unconditionally; its semantic strength
 * (Boolean vs Kleene-involution-only) is established at the
 * @c L-witness level via the parallel @c IsBooleanSubobjectLattice
 * concept, not gated inside this concept's body.
 *
 * @section lattice__IsSubobjectLattice_CT_Vocabulary
 * The concept body uses CT-vocabulary primitives: the carrier exposes
 * @c Domain and @c logic_species typedefs, and free functions
 * @c meet, @c join, @c complement exist with the right shape.
 * Operator sugar (@c <=, @c &, @c |, @c !) lives in @c :sets as
 * forwarders.  Pierce-style stratification: abstract content in the
 * body, set-theoretic hints in the defaults.
 *
 * @tparam S The subobject carrier.  Must expose @c Domain and
 *           @c logic_species typedefs (the latter satisfying
 *           @c IsOckhamAlgebra).
 */
export template <typename S>
concept IsSubobjectLattice = requires(S a, S b) {
  /** @brief CT-vocabulary metadata: @c S exposes a domain and a
   *         classifier logic species. */
  typename S::Domain;
  typename S::logic_species;
  requires IsOckhamAlgebra<typename S::logic_species>;

  /** @brief CT-vocabulary free functions for the binary lattice
   *         operations (binary product / coproduct in the subobject
   *         category).  Results inhabit the same subobject family —
   *         anchored on @c (S::Domain, S::logic_species) per the
   *         family concept. */
  {
    meet(a, b)
  } -> IsSubobjectFamilyMember<typename S::Domain, typename S::logic_species>;
  {
    join(a, b)
  } -> IsSubobjectFamilyMember<typename S::Domain, typename S::logic_species>;
} && requires(S a) {
  /** @brief Complement is required unconditionally: classical carriers
   *         get a bona-fide Boolean complement, Kleene carriers get
   *         the involutive rotation that fails Boolean complement
   *         laws at @c Unknown.  The semantic strength is established
   *         at the @c L-witness level, not the concept boundary. */
  {
    complement(a)
  } -> IsSubobjectFamilyMember<typename S::Domain, typename S::logic_species>;
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
 * carrier with @c Boole should automatically participate in
 * the Boolean lattice surface; parametrising with @c Kleene
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
 * @c L @c = @c Boole — no extra structural laws beyond
 * what @c IsSubobjectLattice already checks.  Kleene / Heyting
 * carriers fail closed because their @c logic_species @c ≠ @c
 * Boole.
 *
 * @tparam S The subobject carrier.
 */
export template <typename S>
concept IsBooleanSubobjectLattice =
    IsSubobjectLattice<S> && std::same_as<typename S::logic_species, Boole>;

}  // namespace dedekind::category
