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

export module dedekind.category:lattice;

import :logic;
import :posetal;   // IsPosetal — row 2 (thin + antisymmetric);
                   // IsOrderLatticeOperations — bottom-up algebraic surface
import :filtered;  // IsFilteredCategory — row 3 (directed thin cat)
import :species;   // is_codirected_v — cofiltered companion to is_directed_v

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
    IsPosetal<T, Rel, L> &&             // Faithful: lattice ⊊ posetal.
    IsFilteredCategory<T, Rel, L> &&    // Faithful: lattice ⊊ filtered.
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

}  // namespace dedekind::category
