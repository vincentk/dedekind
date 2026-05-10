/**
 * @file dedekind/category/total.cppm
 * @partition :total
 * @brief The Laws of Total Algebra (The Algebraic Hierarchy).
 *
 * @section total__The_Categorical_Foundation
 * « Język jest aparatem wyznaczającym obraz świata. Struktura kategorii
 *   jest fundamentem wszelkiego poznania naukowego. »
 *  (Language is an apparatus that determines the image of the world.
 *   The structure of categories is the foundation of all scientific knowledge.)
 *  — Kazimierz Ajdukiewicz, 'Język i Poznanie' (Language and Cognition)
 *
 * @details
 * This partition defines the "Laws of Harmony" for a single species T under
 * a binary operation Op. Following the Polish School of Logic, we treat
 * these structures as species that "mature" as they gain axioms—moving
 * from the primal Magma to the perfect symmetry of an Abelian Group.
 *
 * @section total__Std_Namespace_Mappings
 * This partition asserts bidirectional mappings between algebraic hierarchy
 * concepts and standard C++ types:
 *
 * | Std type        | Highest algebraic structure                    |
 * |-----------------|------------------------------------------------|
 * | `bool`          | Rig (OR/AND), Distributive Lattice (OR/AND)    |
 * | `unsigned int`  | Abelian Group (+), Ring (+,*), Ring (XOR,AND)  |
 * | `int`           | Distributive Lattice (max,min)                 |
 * | `double`        | Distributive Lattice (max,min)                 |
 *
 * @copyright 2026 The Dedekind Authors
 *
 * @note "I obtained scientific guidance and stimulation mainly through
 * personal mathematical contacts in Erlangen and in Gottingen. Above all I am
 * indebted to E. Fischer, from whom I received the decisive impulse to study
 * abstract algebra from an arithmetical viewpoint."
 *       -- Emmy Noether, Habilitation curriculum vitae (1919)
 */
module;

#include <algorithm>
#include <concepts>
#include <cstddef>  // for std::size_t (cyclic_order_v)
#include <functional>
#include <limits>       // for std::numeric_limits (cyclic_order_v)
#include <type_traits>  // for std::false_type, std::integral_constant

export module dedekind.category:total;

import :discrete;
import :morphism;
import :posetal;
import :species;

namespace dedekind::category {
/**
 * @concept IsTotalArrow
 * @brief A Morphism whose domain species admits no undefined (hazard) values.
 * @details ∀x ∈ Domain: f(x) is valid (not a hazard).
 *
 * A morphism is "Total" when it is defined for every element of its domain.
 * This corresponds to functions that cannot produce undefined behaviour (e.g.
 * overflow, division by zero). The criterion is structural: the domain species
 * must use `bool` as its logic type, which is only satisfied by species whose
 * arithmetic wraps predictably (e.g. `unsigned int`, `bool`).
 */
export template <typename F>
concept IsTotalArrow =
    IsArrow<F> && std::same_as<typename GetLogic<domain_t<F>>::type::Ω, bool>;

/**
 * @struct TotalMorphism
 * @brief A concrete wrapper asserting that a Morphism A → B is total.
 * @details Extends `Morphism<A, B, Func>` with a compile-time assertion that
 * the underlying function satisfies `IsTotalArrow`. This makes totality an
 * explicit, type-level guarantee rather than a convention.
 *
 * @tparam A    The Domain species (must be a total species, e.g. unsigned int).
 * @tparam B    The Codomain species.
 * @tparam Func The callable implementing the mapping.
 */
export template <IsSpecies A, IsSpecies B, typename Func>
struct TotalMorphism : Morphism<A, B, Func> {
  // Fix: IsTotalArrow now only takes the function type
  static_assert(
      IsTotalArrow<Morphism<A, B, Func>>,
      "Totality Error: The provided function is not total over the domain.");
};

/**
 * @concept IsPointed
 * @brief A type @c T has a chosen identity element under operation @c Op.
 * @details Replaces the missing `HasIdentity` for Level 0.1.  The identity
 *          element is looked up via the @c identity_v trait registered in
 *          @c :species; @c IsPointed pins the existence claim at the concept
 *          level.  Re-homed from @c :species to @c :total under #637 ---
 *          this is universal-algebra content (chosen-element witness for a
 *          (T, Op) pair), conceptually adjacent to @c IsMagma / @c
 *          IsUnitalMagma / @c IsMonoid below, not species-level.
 */
export template <typename T, typename Op>
concept IsPointed = requires {
  { identity_v<T, Op> } -> std::convertible_to<T>;
};

static_assert(IsPointed<bool, std::logical_or<bool>>,
              "Pointed: Booleans must have an additive identity (false).");
static_assert(IsPointed<bool, std::logical_and<bool>>,
              "Pointed: Booleans must have a multiplicative identity (true).");
static_assert(IsPointed<int, std::plus<int>>,
              "Pointed: Integers must have an additive identity (0).");
static_assert(IsPointed<int, std::multiplies<int>>,
              "Pointed: Integers must have a multiplicative identity (1).");
static_assert(IsPointed<double, std::plus<double>>,
              "Pointed: Doubles must have an additive identity (0).");
static_assert(IsPointed<double, std::multiplies<double>>,
              "Pointed: Doubles must have a multiplicative identity (1).");

/** @section total__Pointed_ConstantMorphism_Factories
 *
 * @details Re-homed from @c :discrete under #637 (the constant morphisms
 * here use @c IsPointed, which is universal-algebra content).  The @c
 * ConstantMorphism construct itself stays in @c :discrete; what lives
 * here is the algebra-flavoured layer that picks out the identity
 * element of @c (B, Op) and wraps it as a constant morphism @c A @c → @c B.
 */

/**
 * @brief @c identity_registry specialisation for @c ConstantMorphism<A, B>.
 *        Registers the constant-at-identity-element-of-@c B as the identity
 *        element of @c ConstantMorphism<A, B> under operation @c Op.
 */
template <typename A, typename B, typename Op>
  requires IsPointed<B, Op>
struct identity_registry<ConstantMorphism<A, B>, Op> {
  static constexpr ConstantMorphism<A, B> value{identity_v<B, Op>};
};

/**
 * @brief @c zero<A, B, Op>(): the constant morphism @c A @c → @c B that
 *        maps everything to the @c Op-identity element of @c B.
 *        Defaults to @c std::plus<B> (additive identity, i.e. zero).
 */
export template <typename A, typename B, typename Op = std::plus<B>>
  requires IsPointed<B, Op>
constexpr auto zero() {
  return ConstantMorphism<A, B>{identity_v<B, Op>};
}

/**
 * @brief @c unit<A, B, Op>(): the constant morphism @c A @c → @c B that
 *        maps everything to the @c Op-identity element of @c B.
 *        Defaults to @c std::multiplies<B> (multiplicative identity, i.e.
 *        one).  Distinct from @c :limit's nullary @c unit<T>() which
 *        produces the unique terminal-morphism @c T @c → @c One; the
 *        two are namespace-overloaded with disjoint template-argument
 *        shapes.
 */
export template <typename A, typename B, typename Op = std::multiplies<B>>
  requires IsPointed<B, Op>
constexpr auto unit() {
  return ConstantMorphism<A, B>{identity_v<B, Op>};
}

static_assert(IsPointed<decltype(zero<int, int>()), std::plus<int>>);
static_assert(IsPointed<decltype(unit<int, int>()), std::multiplies<int>>);

// Functional correctness: maps to the Op-identity element.
static_assert(
    zero<int, int, std::plus<int>>()(123) == 0,
    "Logic Error: Zero mapping failed to return the identity element.");

// 3. Verify it is a Total Arrow (defined for all x in Domain)
static_assert(IsTotalArrow<decltype(zero<int, int>())>,
              "Totality Error: zero() must be total constant over its domain.");

// 3. Verify it is a Total Arrow (defined for all x in Domain)
static_assert(IsTotalArrow<decltype(unit<int, int>())>,
              "Totality Error: unit() must be total constant over its domain.");

/**
 * @concept IsMagma
 * @brief T × T → T (The Base Total Species).
 * @details A Magma is the most primitive algebraic structure: a set T together
 * with a binary operation Op that is closed (the result stays in T) and total
 * (defined for every pair of inputs). No further axioms are required.
 *
 * In the Dedekind taxonomy, `unsigned int` with wrapping addition is a Magma,
 * while `int` with standard addition is not (signed overflow is undefined
 * behaviour and therefore a "hazard").
 */
export template <typename T, typename Op>
concept IsMagma = IsTotal<T, Op> && requires(T a, T b) {
  { Op{}(a, b) } -> std::convertible_to<T>;
};

// REJECTION: Signed addition is NOT a Magma (No Periodicity, No Idempotency).
// It is correctly identified as a Hazard/Partial function.
static_assert(!IsMagma<int, std::plus<int>>);

/**
 * @concept IsUnitalMagma
 * @brief A Magma with a global Identity (0 or 1).
 */
export template <typename T, typename Op>
concept IsUnitalMagma = IsMagma<T, Op> && IsPointed<T, Op>;

/**
 * @concept IsInvertible
 * @brief A pointed (T, Op) where every element has an Op-inverse.
 * @details Re-homed from @c :species under #637, alongside @c IsPointed
 *          which it composes with.  The underlying @c is_invertible_v
 *          trait stays in @c :species as the trait-registry surface.
 */
export template <typename T, typename Op>
concept IsInvertible = IsPointed<T, Op> && is_invertible_v<T, Op>;

/**
 * @concept IsLoop
 * @brief A UnitalMagma with Inverses (-x).
 * @note "A Group without the Associativity Axiom."
 */
export template <typename T, typename Op>
concept IsLoop = IsUnitalMagma<T, Op> && IsInvertible<T, Op>;

/**
 * @concept IsSemigroup
 * @brief An Associative Magma.
 * @details Adds associativity to the Magma: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c).
 * This is the foundational property that allows operations to be "chained"
 * without ambiguity, enabling sequential composition.
 */
export template <typename T, typename Op>
concept IsSemigroup = IsMagma<T, Op> && IsAssociative<T, Op>;

/**
 * @concept IsCommutativeSemigroup
 * @brief An Associative Magma where the operation is also commutative.
 * @details Extends `IsSemigroup` with the symmetry axiom: a ⊕ b = b ⊕ a.
 */
export template <typename T, typename Op>
concept IsCommutativeSemigroup = IsSemigroup<T, Op> && IsCommutative<T, Op>;

/**
 * @concept IsMonoid
 * @brief A Semigroup with a two-sided Identity element.
 * @details Adds a neutral element `e` such that e ⊕ a = a = a ⊕ e for all a.
 * `bool` with `logical_or` is a Monoid (identity = `false`);
 * `unsigned int` with `plus` is a Monoid (identity = 0).
 */
export template <typename T, typename Op>
concept IsMonoid = IsSemigroup<T, Op> && IsUnitalMagma<T, Op>;

/**
 * @concept IsCommutativeMonoid
 * @brief A Monoid where the operation is commutative.
 * @details Both `bool` (OR, AND) and `unsigned int` (multiplication) satisfy
 * this level. Importantly, `unsigned int` with multiplication is a
 * CommutativeMonoid but not a Group (no multiplicative inverse for 2).
 */
export template <typename T, typename Op>
concept IsCommutativeMonoid = IsMonoid<T, Op> && IsCommutative<T, Op>;

static_assert(IsCommutativeMonoid<bool, std::logical_or<bool>>);
static_assert(IsCommutativeMonoid<bool, std::logical_and<bool>>);

// Strictly False: Multiplication on integers is NOT a Total Commutative Monoid
// (integer overflow). It is also NOT a group (no integer inverse for 2).
static_assert(!IsCommutativeMonoid<int, std::multiplies<int>>);
static_assert(IsCommutativeMonoid<unsigned int, std::multiplies<unsigned int>>);

/**
 * @concept IsGroup
 * @brief A Monoid with two-sided Inverses (The Perfect Symmetry).
 * @details Every element a has a unique inverse a⁻¹ such that a ⊕ a⁻¹ = e.
 * `unsigned int` with addition is a Group (modular inverse exists for every
 * element in Z/2ⁿZ). Signed `int` with addition is not, as overflow is
 * undefined behaviour.
 */
export template <typename T, typename Op>
concept IsGroup = IsMonoid<T, Op> && IsLoop<T, Op>;

// REJECTION: subtraction on uint is NOT a Group (no negative results).
// While it is periodic (wraps), it lacks the inverse axiom in the positive
// domain.
static_assert(!IsGroup<unsigned int, std::minus<unsigned int>>);

/**
 * @concept IsAbelianGroup
 * @brief A Commutative Group.
 * @details `unsigned int` with addition (Z/2ⁿZ) is the canonical example:
 * addition wraps, every element has a modular inverse, and a + b = b + a.
 */
export template <typename T, typename Op>
concept IsAbelianGroup = IsGroup<T, Op> && IsCommutative<T, Op>;

// Strictly True: Unsigned addition is a Total Abelian Group (Z/2^nZ).
static_assert(IsAbelianGroup<unsigned int, std::plus<unsigned int>>);

/**
 * @brief Opt-in trait: is @c (T, Op) a \emph{cyclic} abelian group?
 *
 * @details Closes #378.  A cyclic group is an abelian group generated
 * by a single element.  Every cyclic group is isomorphic to
 * @f$\mathbb{Z}/n\mathbb{Z}@f$ for some @f$n \ge 1@f$ (finite cyclic)
 * or to @f$\mathbb{Z}@f$ itself (infinite cyclic).  In this library,
 * all finite cyclic carriers that matter --- @c unsigned int /
 * @c unsigned long / @c size_t under @c std::plus (wrapping at
 * @f$2^N@f$), @c ExtensionalCardinal<N> under @c std::plus,
 * @c SignedExtensionalCardinal<N> under @c std::plus,
 * @c morphologies::Modular<N> under @c std::plus --- are cyclic
 * specialisations of the abelian-group case.
 *
 * The trait is struct-backed (@c is_cyclic_group<T, Op>) so
 * SpeciesTraits-based discovery works across module boundaries.
 * Default @c false; carrier author declares finiteness / cyclicity
 * (C++ concepts cannot derive it from a type signature).
 */
export template <typename T, typename Op>
struct is_cyclic_group : std::false_type {};

/** @brief Shorthand access to the @c is_cyclic_group trait. */
export template <typename T, typename Op>
inline constexpr bool is_cyclic_group_v = is_cyclic_group<T, Op>::value;

/**
 * @brief The order @f$n@f$ of a cyclic group (its cardinality).
 *
 * @details A positive value @f$n@f$ signals a finite cyclic group
 * of that order.  The sentinel value @c 0 covers two distinct cases
 * the trait does not distinguish: the infinite cyclic case
 * (isomorphic to @f$\mathbb{Z}@f$) and the finite cyclic case whose
 * order overflows @c std::size_t (e.g.\ @c uint64_t under @c std::plus
 * on a 64-bit platform, where the order @f$2^{64}@f$ is not
 * representable as @c std::size_t).
 */
export template <typename T, typename Op>
struct cyclic_order : std::integral_constant<std::size_t, 0> {};

export template <typename T, typename Op>
inline constexpr std::size_t cyclic_order_v = cyclic_order<T, Op>::value;

/**
 * @concept IsCyclicGroup
 * @brief A cyclic abelian group: the honest concept name for
 * @f$\mathbb{Z}/n\mathbb{Z}@f$ carriers and their machine-backed cousins.
 *
 * @details Composes @c IsAbelianGroup with the finite-cyclicity
 * opt-in @c is_cyclic_group_v.  Does \emph{not} require
 * @c cyclic_order_v > 0 --- that would exclude the infinite cyclic
 * case @f$\mathbb{Z}@f$, which is also genuinely cyclic (generated
 * by 1).
 */
export template <typename T, typename Op>
concept IsCyclicGroup = IsAbelianGroup<T, Op> && is_cyclic_group_v<T, Op>;

/** @section total__IsCyclicGroup_witnesses_for_primitive_unsigned_integrals
 *
 * Every @c std::unsigned_integral @c T under @c std::plus<T> is a
 * cyclic group: addition wraps modulo @c 2^N (with @c N the bit
 * width), and @c T(1) generates the whole group.  Specialisations
 * live here rather than in @c :species because they extend the
 * @c is_cyclic_group / @c cyclic_order struct templates declared
 * in @c :total, so the specialisations must be provided from this
 * partition.
 */
// Exclude bool: `std::plus<bool>(true, true)` is `true` (promotion to
// int gives 2, then conversion back to bool), not `false`, so `true`
// has no additive inverse and (bool, +) isn't a group.  The library
// models bool's additive-group structure via `std::bit_xor<bool>`
// instead (see `:species`).
template <std::unsigned_integral T>
  requires(!std::same_as<T, bool>)
struct is_cyclic_group<T, std::plus<T>> : std::true_type {};

// Order of the (T, +) cyclic group is 2^N where N = bit width of T.
// `std::numeric_limits<T>::digits` gives N for unsigned integral
// types (no sign bit).  For types where 2^N overflows std::size_t
// (e.g. N >= 64 on a 64-bit platform), report 0 ("order not
// representable as std::size_t"); callers treating 0 as "finite but
// oversized" should switch to a wider trait.
template <std::unsigned_integral T>
  requires(!std::same_as<T, bool>)
struct cyclic_order<T, std::plus<T>>
    : std::integral_constant<
          std::size_t, (std::numeric_limits<T>::digits <
                        std::numeric_limits<std::size_t>::digits)
                           ? (std::size_t{1} << std::numeric_limits<T>::digits)
                           : std::size_t{0}> {};

// `unsigned int` under + is the canonical primitive cyclic group.
static_assert(IsCyclicGroup<unsigned int, std::plus<unsigned int>>,
              "unsigned int under + must be a cyclic group (wraps mod 2^N).");

/**
 * @concept IsRig
 * @brief A Semiring without Negatives (Addition is a Monoid).
 * @details A Rig ("Ring without negatives") provides two monoidal operations
 * that distribute over each other. `bool` with (OR, AND) and `unsigned int`
 * with (+, *) are both Rigs. Neither has an additive inverse, so they stop
 * short of a Ring.
 *
 * Textbook note: In standard algebra literature, `semiring` is often used for
 * this structure, while `rig` is the explicit mnemonic "ring without
 * negatives".
 */
export template <typename T, typename Add, typename Mult>
concept IsRig = IsCommutativeMonoid<T, Add> && IsMonoid<T, Mult> &&
                IsDistributive<T, Mult, Add>;

// Strictly True: bool with OR/AND is a Rig (but not a Ring, no subtraction).
static_assert(IsRig<bool, std::logical_or<bool>, std::logical_and<bool>>);
// Strictly True: unsigned int is a Rig (no negative integers).
static_assert(IsRig<unsigned int, std::plus<unsigned int>,
                    std::multiplies<unsigned int>>);

/**
 * @concept IsRng
 * @brief A Ring without Identity (Multiplication is a Semigroup).
 * @details Provides an Abelian Group for addition and a Semigroup for
 * multiplication, with distribution. Called "Rng" (Ring without the
 * multiplicative identity "i").
 *
 * Textbook note: This is also commonly described as a "ring without unity".
 */
export template <typename T, typename Add, typename Mult>
concept IsRng = IsAbelianGroup<T, Add> && IsSemigroup<T, Mult> &&
                IsDistributive<T, Mult, Add>;

/**
 * @concept IsSemiring
 * @brief Alias for `IsRig`: a general multi-operation species.
 * @details The term "semiring" is used in some literature as a synonym for
 * "rig." In this library it is an exact alias for `IsRig`.
 */
export template <typename T, typename Add, typename Mult>
concept IsSemiring = IsRig<T, Add, Mult>;

/**
 * @concept IsRing
 * @brief The Perfect Species (Rig ∩ Rng).
 * @details A Ring combines an Abelian Group for addition and a Monoid for
 * multiplication, with mutual distributivity. `unsigned int` with (+, *)
 * forms a Ring (arithmetic in Z/2ⁿZ). Signed `int` does not satisfy this
 * partition's totality requirements due to undefined overflow.
 */
export template <typename T, typename Add, typename Mult>
concept IsRing = IsRig<T, Add, Mult> && IsRng<T, Add, Mult>;

// Strictly True: int with modular addition/multiplication is a Ring.
// (Assuming we use a modular_plus to stay in the :total partition).
static_assert(IsRing<unsigned int, std::plus<unsigned int>,
                     std::multiplies<unsigned int>>);

// `Modular<N>` is a total ring (axiomatic periodicity) — asserted in
// `morphologies:cyclic`, where the carrier now lives (see #378).

/**
 * @concept IsCommutativeRing
 * @brief A Ring whose multiplication is commutative.
 * @details Every commutative ring satisfies all of @c IsRing plus the
 * requirement that @c a * b == b * a for all @c a, @c b. The rationals
 * @c ℚ, reals @c ℝ, Gaussian rationals @c ℚ(i), and @c Modular<N> for
 * every @c N are commutative rings; @c M_2(ℝ) (2×2 real matrices) is a
 * ring but @emph{not} commutative.
 */
export template <typename T, typename Add, typename Mult>
concept IsCommutativeRing = IsRing<T, Add, Mult> && IsCommutative<T, Mult>;

/**
 * @concept IsField
 * @brief The axiomatic field witness --- a commutative ring
 *        whose multiplicative structure is an abelian group (zero
 *        excluded, by convention on @c is_invertible_v).
 *
 * @details A field is a commutative ring in which every non-zero
 * element has a multiplicative inverse. Canonical examples: @c ℚ,
 * @c ℝ, @c ℂ, and the Gaussian rationals @c ℚ(i).
 *
 * The concept composes two existing pieces of the category tower:
 *
 *   1. @c IsCommutativeRing<T, Add, Mult> --- ring laws plus
 *      commutativity of multiplication.
 *   2. @c IsAbelianGroup<T, Mult> --- the multiplicative structure
 *      is itself an abelian group.
 *
 * The strict mathematical definition --- "every \emph{non-zero}
 * element is multiplicatively invertible" --- cannot be stated in a
 * C++ concept (concepts reason about types, not values). A carrier
 * opts into the multiplicative-group witness by specialising
 * @c is_invertible_v<T, Mult> / @c inverse_trait<T, Mult> to assert
 * the field-level claim; zero is understood excluded.  This is the
 * same mechanism the library already uses for additive inverses
 * (e.g. @c is_invertible_v<Modular<N>, std::plus> = true).
 *
 * @note The concept is deliberately \emph{axiomatic}: no operator
 *       requirements, consistent with the @c category:total layer's
 *       convention that structural concepts check species traits
 *       rather than operator syntax. The operator-bearing witness
 *       --- which additionally requires @c operator/, @c .inverse(),
 *       and @c std::divides --- is @c dedekind::algebra::IsField,
 *       which \emph{builds on} this concept by composing it with
 *       @c IsDivisionRing.
 *
 * Downstream call sites that currently reach for
 * @c dedekind::algebra::HasFieldOperators will retarget to
 * @c IsField<T, Add, Mult> once carriers such as @c Rational<Z>,
 * @c Complex<R>, and the relevant composites register the full
 * ring- plus multiplicative-group-trait specialisations. That work
 * is tracked under epic #374 and in particular #371 (axiom-hook
 * auto-lifter).
 */
export template <typename T, typename Add, typename Mult>
concept IsField = IsCommutativeRing<T, Add, Mult> && IsAbelianGroup<T, Mult>;

/**
 * @concept IsSemilattice
 * @brief compatibility alias for order semilattice refinement.
 * @details An operation is idempotent when a ⊕ a = a. Combined with
 * commutativity and associativity, this gives the structure of a semilattice.
 * `bool` with OR (or AND), and `int`/`double` with `max` (or `min`) are
 * canonical examples.
 *
 * This alias now delegates the algebraic law shape to `:posetal`
 * (`IsOrderMeetSemilattice`) while keeping the `:total` partition's
 * totality guard explicit.
 */
export template <typename T, typename Op>
concept IsSemilattice = IsTotal<T, Op> && IsOrderMeetSemilattice<T, Op>;

/**
 * @concept IsJoinSemilattice
 * @brief A Semilattice used as the "join" (least upper bound / ∨) half of a
 * Lattice. In `bool` this is `logical_or`; in `int`/`double` it is `max`.
 */
export template <typename T, typename Op>
concept IsJoinSemilattice = IsTotal<T, Op> && IsOrderJoinSemilattice<T, Op>;

/**
 * @concept IsMeetSemilattice
 * @brief A Semilattice used as the "meet" (greatest lower bound / ∧) half of a
 * Lattice. In `bool` this is `logical_and`; in `int`/`double` it is `min`.
 */
export template <typename T, typename Op>
concept IsMeetSemilattice = IsTotal<T, Op> && IsOrderMeetSemilattice<T, Op>;

/**
 * @concept IsLattice
 * @brief compatibility alias for order-lattice refinement.
 * @details Combines a JoinSemilattice and a MeetSemilattice with the
 * absorption identities: a ∨ (a ∧ b) = a and a ∧ (a ∨ b) = a.
 * `bool` with (OR, AND) and `int`/`double` with (max, min) are Lattices.
 *
 * This alias now delegates order laws to `:posetal`
 * (`IsOrderLatticeOperations`) and keeps `:total` totality guards on the
 * selected operators.
 */
export template <typename T, typename Join, typename Meet>
concept IsLattice = IsTotal<T, Join> && IsTotal<T, Meet> &&
                    IsOrderLatticeOperations<T, Join, Meet>;

/**
 * @concept IsDistributiveLattice
 * @brief compatibility alias for distributive order lattices.
 * @details
 * Formal Laws:
 * 1. a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)  [Join over Meet]
 * 2. a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)  [Meet over Join]
 *
 * Std namespace mappings:
 * - `bool`   with (`std::logical_or`, `std::logical_and`) is a Distributive
 *            Lattice. This corresponds to the classical two-element Boolean
 *            lattice {false < true}.
 * - `int`    with (`std::ranges::max`, `std::ranges::min`) is a Distributive
 *            Lattice.  This corresponds to the total order on integers.
 * - `double` with (`std::ranges::max`, `std::ranges::min`) is similarly a
 *            Distributive Lattice.
 *
 * Note: (unsigned int, XOR, AND) is a Ring, not a Lattice, because XOR is not
 * idempotent.
 */
export template <typename T, typename Join, typename Meet>
concept IsDistributiveLattice =
    IsLattice<T, Join, Meet> &&
    IsOrderDistributiveLatticeOperations<T, Join, Meet>;

// Upstream ownership locks: :total aliases must track :posetal refinements.
static_assert(IsSemilattice<int, decltype(std::ranges::min)> ==
              (IsTotal<int, decltype(std::ranges::min)> &&
               IsOrderMeetSemilattice<int, decltype(std::ranges::min)>));
static_assert(IsJoinSemilattice<int, decltype(std::ranges::max)> ==
              (IsTotal<int, decltype(std::ranges::max)> &&
               IsOrderJoinSemilattice<int, decltype(std::ranges::max)>));
static_assert(IsMeetSemilattice<int, decltype(std::ranges::min)> ==
              (IsTotal<int, decltype(std::ranges::min)> &&
               IsOrderMeetSemilattice<int, decltype(std::ranges::min)>));
static_assert(
    IsLattice<int, decltype(std::ranges::max), decltype(std::ranges::min)> ==
    (IsTotal<int, decltype(std::ranges::max)> &&
     IsTotal<int, decltype(std::ranges::min)> &&
     IsOrderLatticeOperations<int, decltype(std::ranges::max),
                              decltype(std::ranges::min)>));
static_assert(
    IsDistributiveLattice<int, decltype(std::ranges::max),
                          decltype(std::ranges::min)> ==
    (IsTotal<int, decltype(std::ranges::max)> &&
     IsTotal<int, decltype(std::ranges::min)> &&
     IsOrderLatticeOperations<int, decltype(std::ranges::max),
                              decltype(std::ranges::min)> &&
     IsOrderDistributiveLatticeOperations<int, decltype(std::ranges::max),
                                          decltype(std::ranges::min)>));

// bool is a Distributive Lattice (AND/OR are both idempotent)
static_assert(
    IsDistributiveLattice<bool, std::logical_or<bool>, std::logical_and<bool>>);

// Lattice laws for integers under max/min (Total Order).
static_assert(IsDistributiveLattice<int, decltype(std::ranges::max),
                                    decltype(std::ranges::min)>);

/** @section total__Boolean_Ring_Negative_Proof */

using XOR = std::bit_xor<unsigned int>;
using AND = std::bit_and<unsigned int>;

// This MUST be false because XOR is not idempotent.
static_assert(!IsIdempotent<unsigned int, XOR>);

// This MUST be false because a ^ (a & b) != a.
static_assert(
    !IsAbsorptive<unsigned int, XOR, AND>,
    "Taxonomy Error: Boolean Ring operations must NOT be absorptive.");

// The Final Verdict:
static_assert(!IsLattice<unsigned int, XOR, AND>,
              "Taxonomy Error: A Boolean Ring is a Ring, not a Lattice.");

// ===========================================================================
// Closure-forcing operators (concept-only vocabulary, slice of #432)
// ===========================================================================
//
// Textbook reference: when an operation is well-defined on a smaller
// carrier @c S as a function @c S @c × @c S @c → @c T (or @c S @c →
// @c T for the unary case) but @c S itself is @b not closed under it,
// the operation is @b closure-forcing.  The operator's existence with
// the wider codomain @c T @b is the structure-forcing axiom expressed
// in code, and @c T is canonically the @b smallest carrier in the
// lattice that closes the operation — i.e., @c T @c = @c F(S) under
// the relevant free-functor adjunction (Grothendieck construction for
// ℕ → ℤ via @c −; field of fractions for ℤ → ℚ via @c ÷; etc.).
//
// See @c docs/design/carrier-lattice.md for the broader discussion
// and the relationship to the @c :functor / @c :natural / @c :species
// vocabularies.
//
// Concept-only here: structural-shape clause is deducible; the
// universal-property clause (@c T is the @b smallest closing carrier,
// not just @b a closing carrier) lives one layer above the type-
// checker and is carried by the engineer's opt-in trait
// @c is_closure_forcing_v, in the same pattern as @c
// is_associative_v / @c is_saturating.  Concrete trait specialisations
// live next to the carriers they pin (e.g., @c sets:cardinality for
// the @c (Cardinality, SignedCardinality) pair).

/**
 * @concept HasClosureForcingShape
 * @brief @b Structural shape: @c Op{} sends @c (A, A) → B (binary)
 *        or @c A → B (unary) with @c A @c ≠ @c B.  Pure deducible
 *        clause of @c IsClosureForcing.  The universal-property side
 *        — that @c B is the @b smallest carrier closing @c Op over
 *        @c A — is carried by the @c is_closure_forcing_v opt-in
 *        trait below.
 *
 *  @c A @c ≠ @c B is the structural witness that something widens; if
 *  @c A @c = @c B the operation is closed and @c IsClosureForcing
 *  doesn't apply.
 */
export template <typename Op, typename A, typename B>
concept HasClosureForcingShape =
    !std::same_as<A, B> &&
    (requires(const A& a) {
      { Op{}(a) } -> std::same_as<B>;
    } || requires(const A& a1, const A& a2) {
      { Op{}(a1, a2) } -> std::same_as<B>;
    });

/**
 * @brief Opt-in trait carrying the universal-property clause of
 *        @c IsClosureForcing — the engineer's honesty obligation that
 *        @c B is the @b canonical (smallest) carrier closing @c Op
 *        over @c A inputs.  C++ concepts cannot quantify universally,
 *        so this part of the witness is asserted, not deduced.
 *        Pattern mirrors @c is_associative_v / @c is_saturating.
 *
 *  Specialise for each concrete @c (Op, A, B) triple where the
 *  universal-property holds — typically next to the carrier
 *  definitions, so the canonicality claim is visible alongside the
 *  carrier choice.
 */
export template <typename Op, typename A, typename B>
inline constexpr bool is_closure_forcing_v = false;

/**
 * @concept IsClosureForcing
 * @brief @b Composite: structural shape + opt-in canonicality.  The
 *        full witness that @c Op @b is the closure-forcing operator
 *        @c A @c → @c B (or @c A @c × @c A @c → @c B), with @c B the
 *        canonical recipient.
 *
 *  Operationally: @c IsClosureForcing<std::negate<>, @c Cardinality,
 *  @c SignedCardinality> witnesses that @c -Cardinality (unary) is
 *  the Grothendieck embedding's operator-level shadow.  Similarly
 *  @c IsClosureForcing<std::minus<>, @c Cardinality,
 *  @c SignedCardinality> for the binary case.
 */
export template <typename Op, typename A, typename B>
concept IsClosureForcing =
    HasClosureForcingShape<Op, A, B> && is_closure_forcing_v<Op, A, B>;

}  // namespace dedekind::category
