/**
 * @file dedekind/algebra/universal.cppm
 * @partition :universal
 * @brief The universal-algebra (A, F) meta-pattern: carrier + finitary
 * operations.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * Wikipedia: Universal algebra, Algebraic structure, Free object.
 *
 * @section universal__Why_this_partition
 * Each specific concept in this module's algebraic hierarchy
 * (@c IsRing<T, Add, Mult>, @c IsField<T, Add, Mult>,
 * @c IsAbelianGroup<T, Op>, @c IsMonoid<T, Op>, ...) is an instance
 * of the universal-algebra (A, F) pattern of Burris--Sankappanavar
 * 1981 §I.1: a carrier set A with a family F of finitary operations
 * satisfying axioms.
 *
 * This partition names that pattern explicitly at the @b closure
 * tier --- @c IsAlgebra<T, Ops...> requires only that each Op closes
 * on T, with axioms living in the specific refinements
 * (IsMonoid, IsAbelianGroup, IsRing, IsField, ...).  The relationship
 * between this partition and the others mirrors the relationship
 * between @c HasRingOperators (closure tier) and @c IsRing (axiom
 * tier) within the ring partition.
 *
 * @section universal__Use_cases
 * - Documentation anchor: the (A, F) vocabulary is named here, with
 *   one citation, and downstream concepts inherit by reference.
 * - Future @c IsHomomorphism<Arrow, A1, A2> (filed as separate
 *   slice): an arrow between two @c IsAlgebra instances that
 *   respects the families of operations.
 * - Future @c IsQuotientMorphism (filed as separate slice):
 *   @c IsHomomorphism + @c IsSurjective, exhibiting a quotient as a
 *   surjective homomorphism between algebras.
 * - Generic dispatch for code that handles any algebra without
 *   pinning a specific algebraic theory.
 *
 * @note "Algebra in the Dedekind topos is the study of operations
 *        that preserve structural symmetry." (Per @ref algebra.)
 *        Identifying the universal pattern first lets specific
 *        symmetries (Ring, Field, AbelianGroup) refine it; the more
 *        symmetries we identify mechanically, the lower the
 *        per-functor implementation effort downstream.
 *
 * @section universal__Naming
 * The partition is named @c :universal (universal algebra), not the
 * shorter @c :algebra, because the umbrella module is itself named
 * @c dedekind.algebra and a same-named partition would collide.
 *
 * @see Burris & Sankappanavar 1981, @em A @em Course @em in
 *      @em Universal @em Algebra (Springer GTM 78), §I.1.
 *
 * @note "L'algèbre est généreuse: elle donne souvent plus qu'on ne lui
 *        demande."
 *       [Trans: "Algebra is generous; she often gives more than is
 *        asked of her."]
 *       — Jean le Rond d'Alembert, attributed (cf.\ Œuvres, ed.\
 *       Belin 1821, Vol. 1).
 */
module;

#include <concepts>
#include <functional>

export module dedekind.algebra:universal;

namespace dedekind::algebra {

// ---------------------------------------------------------------------------
// Operation-shape concepts: an operation closes on T (binary, unary, or one
// of those).  Nullary operations (constants like T{}, T{1}) are not reified
// as Op-callables here; the library handles them at the value level.
// ---------------------------------------------------------------------------

/** @brief A unary operation closes on T: @c op(a) returns @c T. */
export template <typename T, typename Op>
concept IsUnaryOpOn = std::regular<T> && requires(T a, Op op) {
  { op(a) } -> std::same_as<T>;
};

/** @brief A binary operation closes on T: @c op(a, b) returns @c T. */
export template <typename T, typename Op>
concept IsBinaryOpOn = std::regular<T> && requires(T a, T b, Op op) {
  { op(a, b) } -> std::same_as<T>;
};

/** @brief An operation closes on T (either binary or unary). */
export template <typename T, typename Op>
concept IsOpOn = IsBinaryOpOn<T, Op> || IsUnaryOpOn<T, Op>;

// ---------------------------------------------------------------------------
// IsAlgebra: the universal-algebra (A, F) meta-pattern.
// ---------------------------------------------------------------------------

/**
 * @concept IsAlgebra
 * @brief The universal-algebra (A, F) pattern at the closure tier.
 *
 * @details An algebra in the sense of Burris--Sankappanavar 1981 §I.1
 * is a pair (A, F) of a carrier set A together with a family F of
 * finitary operations on A satisfying axioms.  This concept captures
 * the @b closure tier:
 *
 *   - @c T is @c std::regular (the carrier has equality + copy
 *     semantics; "regularity" in the @c std::regular sense).
 *   - Each @c Op in @c Ops closes on @c T (binary or unary).
 *
 * @b Axioms live in specific refinements
 * (@c IsMonoid<T, Op>, @c IsAbelianGroup<T, Op>,
 * @c IsRing<T, Add, Mult>, @c IsField<T, Add, Mult>, ...) --- each
 * of which IS an instance of @c IsAlgebra with a specific F and a
 * specific axiom-trait registry.
 *
 * @section universal__Compositional_use
 * Quotient functors, free constructions, and similar functors operate
 * at the algebra level.  A quotient morphism @c R @c → @c R[x]/I is
 * an arrow between two @c IsAlgebra instances; an
 * @c IsHomomorphism<Arrow, A1, A2> concept (future work) refines this
 * with the requirement that the arrow respects each operation in the
 * algebra's family.
 *
 * @tparam T   The carrier type (@c std::regular).
 * @tparam Ops The variadic family of operations on @c T.
 *
 * @see Burris & Sankappanavar 1981 §I.1 (the formal definition).
 * @see Lang, @em Algebra (3rd ed.), §III.1 (quotient rings); §II.4
 *      (field of fractions) --- specific instances of the
 *      construction-on-an-algebra pattern.
 */
export template <typename T, typename... Ops>
concept IsAlgebra = std::regular<T> && (... && IsOpOn<T, Ops>);

}  // namespace dedekind::algebra

// ---------------------------------------------------------------------------
// Witness: canonical carriers exhibit the (A, F) closure pattern.
// ---------------------------------------------------------------------------
//
// Each static_assert below pins a specific (carrier, operations) pair as
// satisfying @c IsAlgebra at the closure tier.  Axioms
// (associativity, commutativity, etc.) are NOT claimed here; those live
// in the specific refinements (IsRing, IsField, ...).  The pinning
// converts internalisation effort (memorising "yes, int is an algebra
// under (+,*)" etc.) into compiler work: each static_assert below is a
// load-bearing claim the compiler discharges mechanically.

namespace dedekind::algebra {

static_assert(IsAlgebra<int, std::plus<int>, std::multiplies<int>>,
              "int with (+, *) closes the (A, F) pattern at the closure tier "
              "(no axioms claimed; signed-overflow UB lives one tier deeper "
              "in the IsRing rejection).");

static_assert(IsAlgebra<unsigned int, std::plus<unsigned int>,
                        std::multiplies<unsigned int>>,
              "unsigned int with (+, *) closes (A, F) under modular wrap.");

static_assert(IsAlgebra<bool, std::bit_xor<bool>, std::bit_and<bool>>,
              "bool with (XOR, AND) closes (A, F) at the F_2 reading "
              "(see dedekind.algebra:field for the axiomatic IsField<bool, "
              "std::bit_xor, std::bit_and> witness on this carrier).");

}  // namespace dedekind::algebra
