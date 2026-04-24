/**
 * @file dedekind/algebra/registration.cppm
 * @partition :registration
 * @brief Level 3.4: Registration helpers for field-shaped carriers.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Scope
 * Closes #382.  When a concrete carrier @c T is registered as a
 * field under some @c (Add, Mult), the per-carrier trait block has
 * historically been ~40 lines of boilerplate (identity, associativity,
 * commutativity, distributivity, periodicity, invertibility on each
 * operation --- all written out per (T, Op)).
 *
 * This partition replaces that block with a single base-class
 * inheritance into @c SpeciesTraits<T>:
 *
 *   @code
 *   template <>
 *   struct SpeciesTraits<𝔽64>
 *       : dedekind::algebra::GaloisFieldRegistration<𝔽64, 𝔽64{}, 𝔽64{1}, 64> {
 *     using Domain = 𝔽64;
 *     using machine_type = std::uint8_t;
 *   };
 *   @endcode
 *
 * The mechanism has two pieces:
 *
 *   (1) @c FieldRegistration<T, Zero, One, Add, Mult> (and the
 *       derived @c GaloisFieldRegistration<..., Order>) --- CRTP-ish
 *       base classes that expose member templates
 *       @c is_associative_v<Op>, @c is_commutative_v<Op>,
 *       @c is_invertible_v<Op>, @c is_periodic_v<Op>,
 *       @c identity_v<Op>, @c is_distributive_v<MultOp, AddOp>, with
 *       the obvious per-op conditions baked in.
 *
 *   (2) @c SpeciesTraits-based \emph{discovery} partial
 *       specialisations for each of the corresponding free-standing
 *       traits in @c dedekind.category (@c is_associative,
 *       @c is_commutative, \ldots, @c identity_trait,
 *       @c is_invertible_v, \ldots).  Each one has the shape ``if
 *       @c SpeciesTraits<T> exposes member template @c is_foo_v,
 *       then @c is_foo<T, Op>::value is that''.
 *
 * The discovery layer lives here (not in @c category:species), so
 * @c :species stays a pure trait-definition layer.  Carriers that
 * do @em not inherit from a Registration helper are unaffected: the
 * discovery only fires when the member template is actually
 * present.  The @c dedekind.algebra umbrella re-exports this
 * partition, so any translation unit that uses a Registration-
 * retargeted carrier already sees the discovery specs.
 *
 * @note "The machine does not give us the answer, nor does the
 *        answer give us the machine; what matters is that we see
 *        the form."
 *       — paraphrase of Christopher Strachey's remark on
 *         denotational method.
 */
module;

#include <concepts>     // for std::same_as
#include <cstddef>      // for std::size_t
#include <functional>   // for std::plus, std::multiplies
#include <type_traits>  // for std::bool_constant

export module dedekind.algebra:registration;

import dedekind.category;

namespace dedekind::algebra {

// =====================================================================
// The Registration chain.  Each level derives from the previous and
// adds the incremental axioms that define its algebraic species:
//
//   RigRegistration        — additive commutative monoid + multiplicative
//                            monoid + distributive (no additive inverse)
//       ↓
//   RingRegistration       — + additive invertibility
//       ↓
//   CommutativeRingRegistration
//                          — + multiplicative commutativity
//       ↓
//   FieldRegistration      — + multiplicative invertibility
//       ↓
//   GaloisFieldRegistration
//                          — + finite-cardinality opt-in (Galois field)
//
// A carrier picks the appropriate level for its species and the
// SpeciesTraits-based discovery below lifts the member templates
// into the free-standing trait specialisations.
// =====================================================================

/**
 * @struct RigRegistration
 * @brief Base class for @c SpeciesTraits<T> of carriers forming a
 *        \emph{rig} (semiring) under @c (Add, Mult) with identities
 *        @c Zero (additive) and @c One (multiplicative).
 *
 * @details A rig has an additive commutative monoid, a multiplicative
 * monoid, and distributivity of @c Mult over @c Add.  Unlike a ring,
 * it does \emph{not} require an additive inverse.  Examples: @f$\mathbb{N}@f$
 * under @c (+, *), tropical semirings.
 *
 * Exposes member templates:
 *   - @c is_associative_v<Op>  = Op is Add or Mult
 *   - @c is_commutative_v<Op>  = Op is Add  (Mult is not required to
 *                                commute in a rig; @c
 * CommutativeRingRegistration extends this)
 *   - @c is_periodic_v<Op>     = Op is Add or Mult (carries @c IsTotal
 *                                for periodic wrapping carriers; idempotent
 *                                carriers choose a different totality witness)
 *   - @c identity_v<Op>        = Zero when Op is Add, One when Op is Mult
 *   - @c is_distributive_v<MultOp, AddOp>
 *                              = MultOp is Mult and AddOp is Add
 *
 * @c RigRegistration does \emph{not} expose @c is_invertible_v ---
 * a rig has no additive inverse claim.
 */
export template <typename T, auto Zero, auto One, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
struct RigRegistration {
  using Domain = T;

  // Marker that identifies this carrier's SpeciesTraits as coming
  // from a Registration helper (and not, say, a hand-written
  // SpeciesTraits<Modular<N>> with Op-agnostic `is_associative_v =
  // true`).  The discovery specs below gate on this marker so that
  // they only fire for carriers that opted in via the Registration
  // chain --- they will not accidentally broaden trait claims for
  // hand-registered carriers that happen to expose similarly-named
  // member templates with different semantics.
  using dedekind_registration_tag = void;

  template <typename Op>
  static constexpr bool is_associative_v =
      std::same_as<Op, Add> || std::same_as<Op, Mult>;

  template <typename Op>
  static constexpr bool is_commutative_v = std::same_as<Op, Add>;

  template <typename Op>
  static constexpr bool is_periodic_v =
      std::same_as<Op, Add> || std::same_as<Op, Mult>;

  template <typename Op>
    requires(std::same_as<Op, Add> || std::same_as<Op, Mult>)
  static constexpr T identity_v = std::same_as<Op, Add> ? T(Zero) : T(One);

  template <typename MultOp, typename AddOp>
  static constexpr bool is_distributive_v =
      std::same_as<MultOp, Mult> && std::same_as<AddOp, Add>;
};

/**
 * @struct RingRegistration
 * @brief A rig whose additive monoid is an abelian group (i.e.\ has
 *        additive inverses).  Mult is not required to commute here;
 *        see @c CommutativeRingRegistration for that.
 */
export template <typename T, auto Zero, auto One, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
struct RingRegistration : RigRegistration<T, Zero, One, Add, Mult> {
  template <typename Op>
  static constexpr bool is_invertible_v = std::same_as<Op, Add>;
};

/**
 * @struct CommutativeRingRegistration
 * @brief A ring whose multiplication is also commutative.
 */
export template <typename T, auto Zero, auto One, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
struct CommutativeRingRegistration : RingRegistration<T, Zero, One, Add, Mult> {
  template <typename Op>
  static constexpr bool is_commutative_v =
      std::same_as<Op, Add> || std::same_as<Op, Mult>;
};

/**
 * @struct FieldRegistration
 * @brief A commutative ring in which every non-zero multiplicative
 *        element is also invertible.
 *
 * @details Since concepts cannot quantify over values, the
 * multiplicative-invertibility member template returns @c true for
 * the declared @c Mult operation; zero is excluded by convention
 * (same pattern as the rest of the library).
 */
export template <typename T, auto Zero, auto One, typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
struct FieldRegistration
    : CommutativeRingRegistration<T, Zero, One, Add, Mult> {
  template <typename Op>
  static constexpr bool is_invertible_v =
      std::same_as<Op, Add> || std::same_as<Op, Mult>;
};

/**
 * @struct GaloisFieldRegistration
 * @brief Extends @c FieldRegistration with the Galois-specific opt-ins:
 *        the carrier is declared a finite field of a given order.
 *
 * @tparam Order The field's cardinality @f$q = |T| = p^n@f$ (a prime
 *         power).  Exposed as @c galois_order_v for the
 *         @c dedekind.algebra:galois traits.
 */
export template <typename T, auto Zero, auto One, std::size_t Order,
                 typename Add = std::plus<T>,
                 typename Mult = std::multiplies<T>>
struct GaloisFieldRegistration : FieldRegistration<T, Zero, One, Add, Mult> {
  template <typename AddOp, typename MultOp>
  static constexpr bool is_galois_field_v =
      std::same_as<AddOp, Add> && std::same_as<MultOp, Mult>;

  template <typename AddOp, typename MultOp>
  static constexpr std::size_t galois_order_v =
      (std::same_as<AddOp, Add> && std::same_as<MultOp, Mult>) ? Order : 0;
};

}  // namespace dedekind::algebra

// =====================================================================
// SpeciesTraits-based discovery partial specialisations.
// Each is opt-in on `requires { SpeciesTraits<T>::template is_foo_v<Op>; }`
// so carriers that do not inherit from a Registration helper remain
// unaffected.
// =====================================================================

namespace dedekind::category {

// Marker-gated discovery.  `RegisteredViaHelper<T>` fires only when
// `SpeciesTraits<T>` inherits (transitively) from a Registration
// helper, which exposes the `dedekind_registration_tag` alias on the
// inheritance chain.  Hand-written @c SpeciesTraits<T> specialisations
// (e.g.\ @c SpeciesTraits<Modular<N>>, which has Op-agnostic member
// templates with different semantics) do not carry this tag, so the
// discovery below does not over-broaden their trait claims.
template <typename T>
concept RegisteredViaHelper =
    requires { typename SpeciesTraits<T>::dedekind_registration_tag; };

// Helper concepts isolate the "SpeciesTraits<T> exposes member X"
// check into named concepts.

template <typename T, typename Op>
concept SpeciesTraitsHasIsAssociative = RegisteredViaHelper<T> && requires {
  SpeciesTraits<T>::template is_associative_v<Op>;
};

template <typename T, typename Op>
concept SpeciesTraitsHasIsCommutative = RegisteredViaHelper<T> && requires {
  SpeciesTraits<T>::template is_commutative_v<Op>;
};

template <typename T, typename Op>
concept SpeciesTraitsHasIsPeriodic = RegisteredViaHelper<T> && requires {
  SpeciesTraits<T>::template is_periodic_v<Op>;
};

template <typename T, typename Op>
concept SpeciesTraitsHasIsInvertible = RegisteredViaHelper<T> && requires {
  SpeciesTraits<T>::template is_invertible_v<Op>;
};

template <typename T, typename Mult, typename Add>
concept SpeciesTraitsHasIsDistributive = RegisteredViaHelper<T> && requires {
  SpeciesTraits<T>::template is_distributive_v<Mult, Add>;
};

template <typename T, typename Op>
concept SpeciesTraitsHasIdentity = RegisteredViaHelper<T> && requires {
  SpeciesTraits<T>::template identity_v<Op>;
};

// Discovery partial specialisations of the free-standing traits.

template <typename T, typename Op>
  requires SpeciesTraitsHasIsAssociative<T, Op>
struct is_associative<T, Op>
    : std::bool_constant<SpeciesTraits<T>::template is_associative_v<Op>> {};

template <typename T, typename Op>
  requires SpeciesTraitsHasIsCommutative<T, Op>
struct is_commutative<T, Op>
    : std::bool_constant<SpeciesTraits<T>::template is_commutative_v<Op>> {};

template <typename T, typename Op>
  requires SpeciesTraitsHasIsPeriodic<T, Op>
struct is_periodic<T, Op>
    : std::bool_constant<SpeciesTraits<T>::template is_periodic_v<Op>> {};

// is_invertible_v routes through `inverse_trait<T, Op>::exists`.
// Variable-template partial specs don't reliably fire across module
// boundaries in the current Clang, but struct partial specs do ---
// so discover by partial-specialising `inverse_trait` instead.
template <typename T, typename Op>
  requires SpeciesTraitsHasIsInvertible<T, Op>
struct inverse_trait<T, Op> {
  static constexpr bool exists = SpeciesTraits<T>::template is_invertible_v<Op>;
};

// is_distributive_v has no struct backing in :species (it's a pure
// variable template), and the SpeciesTraits-based all-free partial
// spec doesn't fire cross-module.  We lift it via a structural
// partial spec instead: match the canonical (std::multiplies<T>,
// std::plus<T>) op pair, reducing the free parameters from three to
// one.  This is the same pattern Polynomial uses at
// `polynomial.cppm:315` --- a valid partial spec that clang handles
// correctly across module boundaries.
template <typename T>
  requires SpeciesTraitsHasIsDistributive<T, std::multiplies<T>,
                                          std::plus<T>> &&
               (SpeciesTraits<T>::template is_distributive_v<std::multiplies<T>,
                                                             std::plus<T>>)
inline constexpr bool is_distributive_v<T, std::multiplies<T>, std::plus<T>> =
    true;

template <typename T, typename Op>
  requires SpeciesTraitsHasIdentity<T, Op>
struct identity_trait<T, Op, void> {
  using value_type = T;
  static constexpr T value = SpeciesTraits<T>::template identity_v<Op>;
};

}  // namespace dedekind::category

// Discovery for the Galois-specific variable templates lives in
// @c algebra:galois itself, to avoid a `:galois` ↔ `:registration`
// import cycle: `:galois` already imports `:registration` to use
// the helpers, and it hosts its own SpeciesTraits-based discovery
// for @c is_galois_field_v / @c galois_order_v inline.
