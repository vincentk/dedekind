/**
 * @file ontology:category.cppm
 * @brief Level 0: The Skeletal Foundation (Algebraic Bricks and Categorical
 * Cement).
 *
 * @section The_Structuralist_Unity
 * This partition unifies machine-level instructions with the abstract laws of
 * Category Theory. By treating the C++ type system as a formal proof assistant,
 * we ensure structural integrity via compile-time verification (Static
 * Mereology).
 *
 * @subsection The_Bricks: Machine Primitives
 * Fundamental types are augmented with algebraic traits, promoting raw
 * bit-fields into formal members of mathematical structures (e.g., the Integer
 * Group ℤ).
 * - identity_v      : The neutral element (0, 1, true) for a given operation.
 * - is_associative_v : Proof of grouping independence.
 * - is_commutative_v : Proof of order independence.
 *
 * @subsection The_Cement: Categorical Morphisms
 * Primitives are bound by morphisms that preserve structural invariants:
 * - IsFunctor     : A structure-preserving mapping between categories.
 * - IsMonad       : A mechanism for lifting and chaining species (The Push).
 * - IsComonad     : A mechanism for sampling and extending contexts (The Pull).
 * - IsEmbedding   : A proof of injective (1:1) type promotion.
 *
 * @section The_Bootstrapping_Strategy: Action-First Derivation
 * To resolve the circular dependency between Functors and Monads, Dedekind
 * implements an "Action-First" bootstrapping strategy.
 *
 * @note Technical Implementation Constraints:
 * In textbook Category Theory, a Monad is a Monoid in the category of
 * Endofunctors. However, C++ language constraints necessitate a divergence to
 * Kleisli Triples:
 * 1. Partial Specialization: C++ forbids partial specialization of function
 *    templates (e.g., fmap<Box>), preventing a centralized definition.
 * 2. ADL Resolution: Function templates called via name-lookup (fmap<F>) cannot
 *    trigger Argument-Dependent Lookup.
 *
 * By defining Monads/Comonads as Extension Systems (η/ε + >>= / <<=), we
 * utilize operator overloading on concrete objects. This triggers ADL, allowing
 * Level 0 to instantiate a derived 'fmap' without prior knowledge of Level 1
 * species.
 *
 * - Monadic Discovery   : fmap(f) is derived as: m >>= (η ∘ f).
 * - Comonadic Discovery : fmap(f) is derived as: w <<= (f ∘ ε).
 *
 * @section The_Highway_Notation: Directional Vectors
 * Operators represent the flow of data across the ontology:
 * - value >> into<>    : η (Unit) - Lifting a species into a context.
 * - box   << extract<> : ε (Counit) - Sampling a species from a context.
 * - box   >>= f        : Bind - Monadic composition (Left-to-Right).
 * - box   <<= f        : Extend - Comonadic composition (Right-to-Left /
 * Contextual).
 *
 * @section Structural_Inference
 * Compile-time exhaustive proofs (e.g., Bool-to-Int embedding) guarantee
 * the security of the skeletal layer before higher-level complex species
 * are initialized.
 */

module;

#include <concepts>
#include <functional>

export module dedekind.ontology:category;

import :species;

namespace dedekind::ontology {

/**
 * @concept IsSmallCategory
 * @brief The fundamental structure of a Monoid viewed as a Category.
 *
 * @details A Category is 'Small' if its collection of morphisms forms a Set.
 *          In C++, this means the structure can be fully represented by a type
 * T. It adds the 'Identity Morphism' (Unit) to a Semigroupoid: f ∘ id = f = id
 * ∘ f.
 *
 * @section Structuralist_Identity
 * The 'identity_v' trait provides the neutral element for the operation Op.
 */
export template <typename T, typename Op>
concept IsSmallCategory = IsSemigroupoid<T, Op> &&
                          requires {
                            typename identity_trait<T, Op>;
                          } &&  // Ensure specialization exists
                          requires {
                            {
                              identity_trait<T, Op>::value
                            } -> std::same_as<const T&>;
                          };

// Proof: (int, +) with '0' is a Small Category.
static_assert(IsSmallCategory<int, std::plus<int>>,
              "SmallCategory: Integer addition with 0 is a Monoid.");

// Proof: (bool, &&) with 'true' is a Small Category.
static_assert(IsSmallCategory<bool, std::logical_and<bool>>,
              "SmallCategory: Boolean AND with 'true' is a Monoid.");

// 3. Proof: (int, &) is NOT a SmallCategory (No Identity).
// This fails the identity_v check because we haven't anchored a
// "Universal Mask" for the Integer Species.
static_assert(!IsSmallCategory<int, std::bit_and<int>>,
              "Bitwise: AND lacks a universal neutral element in Z.");

/**
 * @concept IsAbelian
 * @brief A Category where the binary composition is Commutative (a ∘ b = b ∘
 * a).
 *
 * @details In the Dedekind topos, Abelian structures (like Logic or Addition)
 *          provide formal permission for the DAG to reorder morphisms.
 *          This enables radical symbolic optimizations, such as SIMD folding,
 *          parallel reductions, and identity-erasure, without altering the
 *          mathematical essence of the result.
 */
export template <typename T, typename Op>
concept IsAbelian = IsSmallCategory<T, Op> && IsCommutative<T, Op>;

/** @section Verification */
static_assert(IsAbelian<int, std::plus<int>>);
static_assert(IsAbelian<bool, std::logical_or<bool>>);

/** @section The Morphism Factory: arrow <A, B> (f) */
export template <typename A, typename B, typename F>
constexpr auto arrow(F&& f) {
  return Morphism<A, B, std::decay_t<F>>{std::forward<F>(f)};
}

/** @brief The Endomorphism Factory: endo<A>(f) */
export template <typename A, typename F>
constexpr auto endo(F&& f) {
  return arrow<A, A>(std::forward<F>(f));
}

/** @section Arrow Factory Verification: Tagging & Species Integrity */

// 4. Action Proof: The tagged arrow preserves the underlying action.
// We verify that the factory-produced morphism actually executes.
static_assert(endo<int>([](int x) { return x * 2; })(21) == 42,
              "Arrow Factory: Action check failed for anonymous lambda.");

/**
 * @struct Box
 * @brief The Identity Monad: The "Cement" of the structuralist ontology.
 */
export template <typename T>
struct Box final {
  T value;

  constexpr bool operator==(const Box&) const = default;
};

/** @brief The Unit/Pure Factory: Lifts a raw value into the Box context. */
export template <typename T>
constexpr auto pure(T&& value) {
  return Box<std::decay_t<T>>{std::forward<T>(value)};
}

/** @section The_Universal_Functor_Interface */


/**
 * @section The_Skeletal_Hole (The Interface)
 * @brief The Universal Entry Point for Functorial Morphism Mapping.
 * 
 * @details 
 * In the Dedekind ontology, fmap is the "Highway" that lifts an arrow 
 * f: A -> B into a categorical context F<A> -> F<B>. This primary template 
 * acts as a formal interface and a diagnostic fallback.
 * 
 * @note The Structuralist "Hole" logic:
 * Following the Stroustrupian principle of "Clear Errors," this function 
 * provides a catch-all that only instantiates if no specialized or 
 * bootstrapped "Paved Highway" (e.g., via Kleisli Discovery) is found. 
 * It effectively serves as a compile-time "Proof of Absence."
 * 
 * @tparam F     The species template (The "Box" or "Context").
 * @tparam Arrow The morphism being lifted (Satisfies IsArrow).
 * 
 * @return Does not return a valid Morphism; triggers a static_assert instead.
 * 
 * @throws static_assert If the species F has not been formally elevated 
 *         to a Functor in the downstream partitions (Level 1+).
 */
export template <template <typename...> typename F, typename Arrow>
  requires IsArrow<Arrow, typename Arrow::Domain, typename Arrow::Codomain>
auto fmap(Arrow) {
  struct Fmap_Not_Found {
    static_assert(
        sizeof(F<int>) == 0,
        "Ontology Error: No fmap implementation found for the requested Species. "
        "Check the compiler trace below to see which 'F' failed to route.");
  };
  return Fmap_Not_Found{};
}


/** @brief The Identity Morphism: The "Zero-Length Highway" for Species T. */
export template <typename T>
struct Identity final {
  using Domain = T;
  using Codomain = T;

  // The logic is baked into the type itself.
  constexpr T operator()(const T& x) const noexcept { return x; }
};

/** @brief The Identity Factory: Returns the neutral arrow for Species A. */
export template <typename A>
constexpr auto id() {
  return Identity<A>{};
}

/** @brief The Identity Functor: F(X) = X. (The "Invisible Box") */
export template <typename T>
using Id = T;

/** @brief fmap for the Identity Functor: F(f) = f. */
export template <template <typename> typename F, typename Arrow>
  requires std::same_as<F<typename Arrow::Domain>, typename Arrow::Domain>
constexpr auto fmap(Arrow f) {
  return f;  // The invisible box doesn't change the highway.
}

/** @section Verification: The Identity Law (F(id_X) = id_F<X>) */

// Proof: Lifting the identity morphism on 'int' gives us an arrow on
// 'Box<int>'.
static_assert(IsArrow<decltype(fmap(id<int>())), Box<int>, Box<int>>,
              "Identity Law: fmap(id) must preserve the Boxed species.");

// Proof: The lifted Negate morphism correctly transforms a Boxed value.
static_assert(fmap<Box>(endo<int>(std::negate<int>{}))(Box<int>{42}).value ==
                  -42,
              "Action: fmap(f) must preserve the underlying machine logic.");

/** @section Identity_Verification: The Unit Laws */

constexpr auto f_neg = endo<int>(std::negate<int>{});
constexpr auto identity_int = id<int>();

/** @section Identity Verification */
static_assert(IsArrow<decltype(id<bool>()), bool, bool>,
              "The id<A>() factory must produce a valid Arrow.");

// 1. Right Identity: f(id(x)) == f(x)
static_assert(f_neg(identity_int(42)) == f_neg(42),
              "Unit Law: f ∘ id_A must equal f.");

// 2. Left Identity: id(f(x)) == f(x)
static_assert(identity_int(f_neg(42)) == f_neg(42),
              "Unit Law: id_B ∘ f must equal f.");

/** @section Lifting Traits to the Identity Functor */

// 1. If T is associative under Op, Identity<T> is associative.
template <typename T, typename Op>
inline constexpr bool is_associative_v<Identity<T>, Op> =
    is_associative_v<T, Op>;

// 2. If T is commutative under Op, Identity<T> is commutative.
template <typename T, typename Op>
inline constexpr bool is_commutative_v<Identity<T>, Op> =
    is_commutative_v<T, Op>;

/**
 * @section Lifting Traits: Morphism Identity
 * The identity element of the Identity Morphism under composition
 * is the Identity Morphism itself (id ∘ id = id).
 */
template <typename T, typename Op>
inline constexpr Identity<T> identity_v<Identity<T>, Op> = Identity<T>{};

/**
 * @section Categorical Composition (Explicitly Typed)
 * @brief Synthesizes an arrow A -> C from A -> B and B -> C.
 * @details By requiring A, B, and C as template parameters, we ensure
 *          the composition is a statically verified bridge.
 */
export template <typename F, typename G>
  requires requires {
    typename F::Domain;
    typename G::Codomain;
  }
constexpr auto operator>>(F&& f, G&& g) {
  using A = typename F::Domain;
  using C = typename G::Codomain;

  // Note: The lambda is implicitly constexpr in C++23 if possible
  return arrow<A, C>([f = std::forward<F>(f), g = std::forward<G>(g)](
                         A x) constexpr { return g(f(std::move(x))); });
}

/** @section Categorical Verification: The Unit Laws (f ∘ id = f = id ∘ f) */

// 1. Proof: Morphism Identity (Existence & Tagging)
// We verify that id exists and chains with tagged morphisms.
static_assert(
    IsArrow<decltype(id<int>() >> arrow<int, int>(std::negate<int>{})), int,
            int>,
    "Unit Law: id_A must be a left-identity for morphisms out of A.");

static_assert(
    IsArrow<decltype(endo<int>(std::negate<int>{}) >> id<int>()), int, int>,
    "Unit Law: id_B must be a right-identity for morphisms into B.");

// 2. Proof: Cross-Species Identity
// id_Z combined with a Z -> B bridge must result in a Z -> B bridge
static_assert(
    IsArrow<decltype(id<int>() >> arrow<int, bool>(IsPositive{})), int, bool>,
    "Unit Law: Identity must preserve the bridge from Z to B.");

// 3. Proof: Extensional Equality (The Action)
// The composite morphism (f ∘ id) must yield the same value as f.
static_assert((endo<int>(std::negate<int>{}) >> id<int>())(42) == -42,
              "Action Proof: Composition with id must be value-invariant.");

// 4. Proof: Categorical Unity (id ∘ id = id)
static_assert(
    IsArrow<decltype(id<int>() >> id<int>()), int, int>,
    "Unity: id composed with itself must remain the Identity Morphism.");

// 5. Negative Proof: Species Safety (The Broken Bridge)
// This should fail to compile if you uncomment it, because bool != int.
// static_assert(!IsArrow<decltype(id<bool>() >> arrow<int,
// int>(std::negate<int>{})), int, int>);

/**
 * @concept IsIsomorphism
 * @brief An Arrow f: A -> B with a guaranteed Inverse g: B -> A.
 * @details Represents a reversible morphism. In Level 0, we verify
 *          the structural existence of the 'Undo' path.
 */
export template <typename F, typename A, typename B>
concept IsIsomorphism = IsArrow<F, A, B> && requires(F f) {
  // We probe the global/partition 'inverse' bridge for this specific Morphism.
  { inverse(f) } -> IsArrow<B, A>;
};

/** @brief The structural inverse of an Identity is itself. */
template <typename T>
[[nodiscard]] constexpr auto inverse(Identity<T> id) noexcept {
  return id;
}

// Proof: id_int is an isomorphism from int to int.
static_assert(IsIsomorphism<Identity<int>, int, int>,
              "Identity must be a self-inverse isomorphism.");

// Proof: id_bool is an isomorphism from bool to bool.
static_assert(IsIsomorphism<Identity<bool>, bool, bool>,
              "Identity must be a self-inverse isomorphism.");

/** @brief The structural inverse of Negation is itself (Involutive). */
template <typename A, typename B, typename Impl>
  requires std::same_as<Impl, std::negate<A>>
[[nodiscard]] constexpr auto inverse(Morphism<A, B, Impl> f) noexcept {
  return f;  // Negate is its own inverse
}

// Proof: Tagged Negation is a formal Isomorphism.
static_assert(IsIsomorphism<TaggedNegate, int, int>,
              "Negation must be recognized as a reversible Morphism.");

/**
 * @concept IsInitialObject
 * @brief The "Zero" of the Category (0).
 * @details For any object X, there exists a unique morphism ! : 0 -> X.
 *          In Mereology, this is the set that contains nothing.
 */
export template <typename T>
concept IsInitialObject = requires(const T s) {
  /** @brief Axiom: Magnitude must be the additive identity (Zero). */
  requires s.cardinality().is_finite == true;
  requires s.upper_bound() == 0;
};

/**
 * @concept IsTerminalObject
 * @brief The "One" of the Category (1).
 * @details For any object X, there exists a unique morphism ! : X -> 1.
 *          In Mereology, this is the set that contains everything (The Domain).
 */
export template <typename T>
concept IsTerminalObject =
    requires(const T s, const typename T::element_type x) {
      /** @brief Axiom: The Membership Morphism is the identity of the Logic. */
      // It must always evaluate to the "Top" (True) of its internal logic.
      { s.contains(x) } -> std::same_as<typename T::logic_species::type>;
    };

/**
 * @struct ZeroAction
 * @brief The 'Absorption' logic.
 * @details Maps any input of species A to the neutral element of species B.
 */
export template <typename A, typename B, typename Op>
struct ZeroAction {
  constexpr B operator()(const A&) const noexcept { return identity_v<B, Op>; }
};

/**
 * @brief The Zero Morphism Factory: zero<A, B, Op>()
 * @details Synthesizes an arrow that absorbs all information,
 *          collapsing the Domain into the Codomain's identity.
 */
export template <typename A, typename B, typename Op>
  requires IsSmallCategory<B, Op>
constexpr auto zero() {
  return arrow<A, B>(ZeroAction<A, B, Op>{});
}

/** @section Zero Morphism Verification */

// 1. Proof: Zero is an Arrow from int to int (under addition).
using ZeroZ = decltype(zero<int, int, std::plus<int>>());
static_assert(IsArrow<ZeroZ, int, int>,
              "Zero: Must be a valid morphism mapping Z to Z.");

// 2. Action Proof: Zero maps everything to 0.
static_assert(zero<int, int, std::plus<int>>()(42) == 0,
              "Absorption: Zero morphism must return the identity element.");

// 3. Action Proof: Zero maps everything to 'true' (under logic AND).
static_assert(zero<int, bool, std::logical_and<bool>>()(42) == true,
              "Absorption: Boolean AND zero must return 'true'.");

/**
 * @concept IsGroupoid
 * @brief A Category where every morphism is an Isomorphism (Invertible).
 *
 * @details This represents a structure where every "action" has a perfect
 *          "undo". In Algebra, this is a Group. It requires an 'inverse'
 *          morphism such that: f ∘ f⁻¹ = id.
 *
 * @note For the Integers (Z), this is addition with negation.
 */
export template <typename T, typename Op>
concept IsGroupoid = IsSmallCategory<T, Op> && requires(T x) {
  // Every element must have an inverse relative to the operation Op
  { inverse<T, Op>(x) } -> std::same_as<T>;
};

/** @section Groupoid Verification: The Symmetry Law */

// 1. Proof: (int, +) is a Groupoid (Additive Group).
// Since we defined inverse<int, plus>(x) as -x, this must pass.
static_assert(IsGroupoid<int, std::plus<int>>,
              "Arithmetic: Integer addition must be a Groupoid.");

// 2. Proof: (int, ^) is a Groupoid (Self-inverse).
// XOR is its own inverse, satisfying a ^ a = 0.
static_assert(IsGroupoid<int, std::bit_xor<int>>,
              "Bitwise: XOR must be a Groupoid.");

// 3. Proof: (bool, ^) is a Groupoid (The Boolean Group).
// Unlike AND/OR, XOR/NOT logic allows for perfect reversibility.
static_assert(IsGroupoid<bool, std::bit_xor<bool>>,
              "Logic: Boolean XOR must be a Groupoid.");

// Verification: (bool, ∧) is a Small Category (Monoid) but NOT a Groupoid.
static_assert(!IsGroupoid<bool, std::logical_and<bool>>,
              "Logic: Boolean AND must not be a Groupoid.");

/**
 * @concept IsAbelianGroupoid
 * @brief A Category where every Morphism is an Isomorphism and
 *        Composition is Commutative.
 *
 * @details This represents the peak of structural symmetry in the
 *          Skeletal layer. In Algebra, this corresponds to an
 *          Abelian Group (e.g., Integer Addition).
 *
 * @section HPC_Authority
 * Because an Abelian Groupoid is both invertible and commutative,
 * the Dedekind engine can:
 * 1. Reorder operations safely for SIMD/Vectorization (Commutativity).
 * 2. Perform "Inverse Pruning" (f ∘ f⁻¹ → id) to eliminate
 *    redundant machine instructions (Invertibility).
 *
 * @note While we will define formal 'Groups' in ontology:algebra,
 *       this concept anchors the requirement at the Category level.
 */
export template <typename T, typename Op>
concept IsAbelianGroupoid = IsGroupoid<T, Op> && IsAbelian<T, Op>;

/** @section Verification of the Standard Model */

// Proof: (bool, &&) is NOT an Abelian Groupoid (It's an Abelian Monoid).
static_assert(!IsAbelianGroupoid<bool, std::logical_and<bool>>,
              "Logic is symmetric but not reversible (Annihilation).");

static_assert(IsAbelian<bool, std::logical_and<bool>>,
              "Logic: AND must be commutative.");

// Proof: (Z, +) is an Abelian Groupoid.
static_assert(IsAbelianGroupoid<int, std::plus<int>>,
              "Integer addition must be a reversible, symmetric action.");

// 1. Proof: (int, ^) is an Abelian Groupoid.
static_assert(IsAbelianGroupoid<int, std::bit_xor<int>>,
              "Bitwise: XOR is a reversible, symmetric Category.");

/** @section Peak Symmetry: Zero vs. Groupoid */

// Proof: The Zero Morphism (int -> int) is an Arrow,
// even if it maps into an Abelian Groupoid.
static_assert(IsArrow<ZeroZ, int, int>,
              "Zero: A 'Black Hole' arrow must map Z to Z.");

// Proof: The result of zero() belongs to the Identity element.
static_assert(zero<int, int, std::plus<int>>()(99) == 0,
              "Absorption: Z -> Z via + must yield 0.");

/**
 * @concept IsFunctor
 * @brief A structure-preserving mapping between two Categories 𝒞 and 𝒟.
 *
 * @details
 * Formally, a Functor F : 𝒞 → 𝒟 consists of:
 *   - Object Mapping: For every object X ∈ 𝒞, an object F⟨X⟩ ∈ 𝒟.
 *   - Morphism Mapping: For every arrow f : X → Y, an arrow F(f) : F⟨X⟩ → F⟨Y⟩.
 *
 * It must satisfy the Functor Laws:
 *   1. Identity Preservation: F(id_X) = id_F⟨X⟩
 *   2. Composition Preservation: F(g ∘ f) = F(g) ∘ F(f)
 *
 * @tparam F   The Type-Morphism (The "Box" or "Transformer").
 * @tparam 𝒯   The Object in the Source Category 𝒞.
 * @tparam Op𝒯 The Composition Rule (Morphism) in 𝒞.
 * @tparam 𝒰   The Object in the Target Category 𝒟.
 * @tparam Op𝒰 The Composition Rule (Morphism) in 𝒟.
 */
export template <template <typename> typename F, typename T, typename OpT,
                 typename U, typename OpU>
concept IsFunctor = IsSmallCategory<T, OpT> && IsSmallCategory<U, OpU> &&
                    requires(Identity<T> id_t) {
                      // 1. Object Mapping: Does the Species T exist in the Box
                      // F?
                      typename F<T>;

                      // 2. Morphism Mapping: F(f: T -> T) must yield an arrow
                      // F<T> -> F<T>. This 'lift' stays within the Functor's
                      // context F.
                      { fmap<F>(id_t) } -> IsArrow<F<T>, F<T>>;
                    };

/**
 * @section The_Kleisli_Triple_Bridge
 * @brief If a Species can Lift (η) and Chain (>>=), it IS a Functor.
 */
export template <template <typename...> typename F, typename Arrow>
  requires requires(F<typename Arrow::Domain> box, Arrow f) {
    // 1. The Unit (η): T -> F<T>
    {
      η<F, typename Arrow::Codomain>{}(
          f(box << extract<typename Arrow::Domain>()))
    } -> std::same_as<F<typename Arrow::Codomain>>;
    // 2. The Bind (>>=): F<T> -> (T -> F<U>) -> F<U>
    {
      box >>=
          [](typename Arrow::Domain x) { return F<typename Arrow::Codomain>{}; }
    };
  }
auto fmap(Arrow f) {
  return [f](auto box) {
    using T = typename Arrow::Domain;
    using U = typename Arrow::Codomain;
    // The Canonical Derivation: fmap(f, m) = m >>= (η ∘ f)
    return box >>= [f](T x) { return η<F, U>{}(f(x)); };
  };
}

/// .... or broken down

/** @section The_Kleisli_Triple */
export template <template <typename...> typename F, typename T, typename U>
concept IsKleisli = requires(T x, F<T> box, std::function<F<U>(T)> f) {
  { η<F, U>{}(x) } -> std::same_as<F<U>>;  // Can we Lift?
  { box >>= f } -> std::same_as<F<U>>;     // Can we Chain?
};

/** @section The_Monadic_Highway_Bridge */
export template <template <typename...> typename F, typename Arrow,
                 typename T = typename Arrow::Domain,
                 typename U = typename Arrow::Codomain>
  requires IsKleisli<F, T, U> && IsArrow<Arrow, T, U>
constexpr IsArrow<F<T>, F<U>> auto fmap(Arrow f) {
  // fmap(f, m) = m >>= (η ∘ f)
  return arrow<F<T>, F<U>>([f](const F<T>& box) {
    return box >>= [f](const T& x) { return η<F, U>{}(f(x)); };
  });
}

/** @section The_Co_Kleisli_Triple */
export template <template <typename...> typename F, typename T, typename U>
concept IsCoKleisli = requires(F<T> box, std::function<U(F<T>)> f) {
  { ε<F, T>{}(box) } -> std::same_as<T>;  // The Pull: F<T> -> T
  {
    box <<= f
  } -> std::same_as<F<U>>;  // The Stretch: F<T> -> (F<T> -> U) -> F<U>
};

/** @section The_Comonadic_Highway_Bridge */
export template <template <typename...> typename F, typename Arrow,
                 typename T = typename Arrow::Domain,
                 typename U = typename Arrow::Codomain>
  requires IsCoKleisli<F, T, U> && IsArrow<Arrow, T, U>
constexpr IsArrow<F<T>, F<U>> auto fmap(Arrow f) {
  // fmap(f, w) = w <<= (f ∘ ε)
  // We extend the box by sampling the species, applying f,
  // and letting the Comonad wrap it back up.
  return arrow<F<T>, F<U>>([f](const F<T>& box) {
    return box <<= [f](const F<T>& w) { return f(ε<F, T>{}(w)); };
  });
}

/** @section Kleisli_Extension_System (The Push) */
export template <template <typename...> typename F, typename T, typename U>
concept IsKleisliExtension = requires(T x, F<T> box, std::function<F<U>(T)> f) {
  { η<F, U>{}(x) } -> std::same_as<F<U>>;  // The Unit
  { box >>= f } -> std::same_as<F<U>>;     // The Bind
};

/** @section The_Monadic_Bootstrap */
export template <template <typename...> typename F, typename Arrow,
                 typename T = typename Arrow::Domain,
                 typename U = typename Arrow::Codomain>
  requires IsKleisliExtension<F, T, U> && IsArrow<Arrow, T, U>
constexpr IsArrow<F<T>, F<U>> auto fmap(Arrow f) {
  // fmap(f, m) = m >>= (η ∘ f)
  return arrow<F<T>, F<U>>([f](const F<T>& box) {
    return box >>= [f](const T& x) { return η<F, U>{}(f(x)); };
  });
}

/** @section CoKleisli_Extension_System (The Pull) */
export template <template <typename...> typename F, typename T, typename U>
concept IsCoKleisliExtension = requires(F<T> box, std::function<U(F<T>)> f) {
  { ε<F, T>{}(box) } -> std::same_as<T>;  // The Extract
  { box <<= f } -> std::same_as<F<U>>;    // The Extend
};

/** @section The_Comonadic_Bootstrap */
export template <template <typename...> typename F, typename Arrow,
                 typename T = typename Arrow::Domain,
                 typename U = typename Arrow::Codomain>
  requires IsCoKleisliExtension<F, T, U> && IsArrow<Arrow, T, U>
//   requires IsCoKleisliExtension<F, T, U> && (!IsKleisliExtension<F, T, U>)
constexpr IsArrow<F<T>, F<U>> auto fmap(Arrow f) {
  // fmap(f, w) = w <<= (f ∘ ε)
  return arrow<F<T>, F<U>>([f](const F<T>& box) {
    return box <<= [f](const F<T>& w) { return f(ε<F, T>{}(w)); };
  });
}

// Proof: Box is a Functor between the Additive Category of Integers and itself.
static_assert(
    IsFunctor<Box, int, std::plus<int>, int, std::plus<int>>,
    "Level 0 Proof: Box must satisfy the Functor concept for (Z, +).");

/**
 * @concept IsEndofunctor
 * @brief A structure-preserving mapping from a Category back to itself (F : 𝒞 →
 * 𝒞).
 *
 * @tparam F   The Type-Morphism (The "Box" or "Transformer").
 * @tparam 𝒯   The Object in the Category 𝒞.
 * @tparam Op𝒯 The Composition Rule (Morphism) in 𝒞.
 */
export template <template <typename> typename F, typename 𝒯, typename Op𝒯>
concept IsEndofunctor = IsFunctor<F, 𝒯, Op𝒯, 𝒯, Op𝒯>;

/** @section Functor Verification: The Identity Endofunctor */

// 1. Proof: Identity is an Endofunctor on (int, +).
// F=Identity, T=int, Op=plus<int>
static_assert(IsEndofunctor<Box, int, std::plus<int>>,
              "Functor: Identity must be a valid Endofunctor on Z.");

// 2. Proof: Identity is an Endofunctor on the Boolean Lattice (bool, &&).
static_assert(IsEndofunctor<Box, bool, std::logical_and<bool>>,
              "Functor: Identity must be a valid Endofunctor on B.");

// 3. Proof: Identity is an Endofunctor on XOR logic (bool, ^).
static_assert(
    IsEndofunctor<Box, bool, std::bit_xor<bool>>,
    "Functor: Identity must be a valid Endofunctor on the XOR Group.");

/** @brief The Functorial Bridge Tag. */
template <template <typename> typename F>
struct fmap_tag {};

/** @brief Global witness for Box mapping. */
export template <typename T = void>  // Template to keep it header-friendly
constexpr auto Boxed = fmap_tag<Box>{};

/** @brief Postfix Operator: arrow >> Boxed */
export template <typename Arrow, template <typename> typename F>
  requires IsArrow<Arrow, typename Arrow::Domain, typename Arrow::Codomain>
constexpr auto operator>>(Arrow f, fmap_tag<F>) {
  return fmap<F>(f);
}

/** @brief The Unit/Pure Tag for value lifting. */
template <template <typename> typename F>
struct into_tag {};

/** @brief Global witness for Box entry. */
export template <typename T = void>
constexpr auto into = into_tag<Box>{};

/** @brief Postfix Operator: value >> into<F> */
export template <typename T, template <typename> typename F>
constexpr auto operator>>(T&& value, into_tag<F>) {
  // Always produce a fresh Box from the value
  return F<std::decay_t<T>>{std::forward<T>(value)};
}

/**
 * @section The Action Bridge (Value >> Arrow)
 * @brief Proposition: A Value x can be piped into a Morphism f: A -> B.
 * @details This is the terminal step of a Highway pipeline. It maps the
 *          Species-level data into the Codomain result.
 *
 * @tparam T     The Input Value type (The 'Car').
 * @tparam Arrow The Morphism type (The 'Highway').
 *
 * @note Syntactic Sugar: x >> f ≡ f(x).
 */
export template <typename T, typename Arrow>
  requires IsArrow<Arrow, typename Arrow::Domain, typename Arrow::Codomain> &&
           std::convertible_to<T, typename Arrow::Domain>
constexpr auto operator>>(T&& value, const Arrow& f) ->
    typename Arrow::Codomain {
  return f(std::forward<T>(value));
}

/** @section Pipeline_Verification: From Brick to Box */

// The Logic (Brick)
constexpr auto increment = endo<int>([](int x) { return x + 1; });

// The Flow: Value -> Box -> Morphism-on-Box
// Reading: "Take 41, put it INTO a Box, then apply increment BOXED."
static_assert((41 >> into<> >> (increment >> Boxed<>)).value == 42,
              "Pipeline: The linear flow from value to boxed result failed.");

// The Composition Flow:
// Reading: "Take 10, put it INTO a Box, then increment it, then negate it."
constexpr auto negate = endo<int>(std::negate<int>{});

static_assert((10 >> into<> >> (increment >> Boxed<>) >> (negate >> Boxed<>))
                      .value == -11,
              "Pipeline: Multi-stage functorial composition failed.");

/** @brief The Join/Flatten Tag (μ: F ∘ F ⟹ F) */
template <template <typename> typename F>
struct join_tag {};

/** @brief Global witness for Box collapsing. */
export template <typename T = void>
constexpr auto join = join_tag<Box>{};

/**
 * @brief The Join/Collapse Morphism specialized for the Box Monad.
 * @details Axiom: μ : Box ∘ Box ⟹ Box is the identity on the inner Box.
 */
export template <typename T>
constexpr auto operator>>(const Box<Box<T>>& nested_box, join_tag<Box>) {
  // We return the inner Box<T> directly.
  // No re-construction, no temporary copies—just pure structural extraction.
  return nested_box.value;
}

/** @brief Rvalue overload for "High-Speed" Move semantics */
export template <typename T>
constexpr auto operator>>(Box<Box<T>>&& nested_box, join_tag<Box>) {
  return std::move(nested_box.value);
}

// A "Double-Entry" Pipeline:
// 42 >> into >> into -> Box<Box<int>>
// ... then >> join -> Box<int>
static_assert((42 >> into<> >> into<> >> join<>).value == 42,
              "Monad Law: Join must collapse the double-context.");

/**
 * @concept IsNaturalTransformation
 * @theorem Naturality (F ⟹ G)
 * @brief Proposition: There exists a structure-preserving bridge between two
 * Functors.
 *
 * @details
 * For this theorem to hold for a candidate (Eta), the following must be true:
 * 1. Categorical Context: F and G are verified Functors over the Species (T,
 * OpT).
 * 2. Morphism Signature: Eta is a valid Arrow mapping the context F⟨T⟩ to G⟨T⟩.
 *
 * For every object X, we define a morphism η_X: F⟨X⟩ → G⟨X⟩.
 * To be "Natural," the following square must commute for any f: X → Y:
 *
 *        η_X
 *   F⟨X⟩ ────→ G⟨X⟩
 *    │          │
 * F(f)│          │G(f)
 *    ↓          ↓
 *   F⟨Y⟩ ────→ G⟨Y⟩
 *        η_Y
 *
 * Formally: G(f) ∘ η_X = η_Y ∘ F(f)
 *
 * @tparam Eta  The candidate proof-object (The transformation type).
 * @tparam F    The source Functor (The origin context).
 * @tparam G    The target Functor (The destination context).
 * @tparam T    The source Species (The object).
 * @tparam U    The target Species (The object).
 * @tparam OpT   The Operation of the Source Category 𝒞.
 * @tparam OpU   The Operation of the Target Category 𝒟.
 */
export template <typename Alpha, template <typename> typename F,
                 template <typename> typename G, typename T, typename U,
                 typename OpT, typename OpU>
concept IsNaturalTransformation =
    IsFunctor<F, T, OpT, T, OpT> && IsFunctor<G, U, OpU, U, OpU> &&
    IsArrow<Alpha, F<T>, G<U>>;

/**
 * @concept IsNaturalEndoTransformation
 * @brief The "Highway" Theorem: F ⟹ G where F, G : 𝒞 → 𝒞.
 * @details Reduces the 7-parameter boilerplate to 5 for the 99% case.
 */
export template <typename Alpha, template <typename> typename F,
                 template <typename> typename G, typename T, typename Op>
concept IsNaturalEndoTransformation =
    IsNaturalTransformation<Alpha, F, G, T, T, Op, Op>;

/**
 * @struct Naturality
 * @witness The Proof-Object for the IsNaturalTransformation Theorem.
 *
 * @details
 * This structure serves as the formal inhabitant (the witness) of the
 * Naturality proposition. It does not perform discovery or lifting;
 * it simply carries a verified Morphism across a functorial bridge.
 *
 * Its role is to:
 * 1. Hold a pre-verified Morphism (η_X) that maps the Species.
 * 2. Define the Functorial Domain (F⟨T⟩) and Codomain (G⟨U⟩).
 * 3. Provide the Identity Preservation Lemma as a static property of the
 * witness.
 *
 * @tparam F    The Source Functor context.
 * @tparam G    The Target Functor context.
 * @tparam T    The source Species (The object).
 * @tparam U    The target Species (The object).
 * @tparam OpF  The Operation of the source category.
 * @tparam OpG  The Operation of the target category.
 * @tparam η_X  The Verified Morphism (The 'Secret Sauce' already lifted to an
 * Arrow).
 */
export template <template <typename> typename F, template <typename> typename G,
                 typename T, typename U, typename OpF, typename OpG, auto η_X>
  requires IsEndofunctor<F, T, OpF> && IsEndofunctor<G, U, OpG> &&
           IsArrow<decltype(η_X), F<T>, G<U>>
struct Naturality final {
  using Domain = F<T>;
  using Codomain = G<U>;

  /** @brief Execution: Invokes the verified Morphism η_X. */
  constexpr Codomain operator()(Domain x) const noexcept { return η_X(x); }

  /** @brief Lemma: η(id_F) = id_G (Verified via the internal Morphism). */
  static constexpr bool preserves_identity() noexcept {
    return η_X(identity_v<T, OpF>) == identity_v<U, OpG>;
  }
};

/**
 * @struct NaturalEndo
 * @brief Ergonomic Witness for transformations within the same Category (𝒞 →
 * 𝒞).
 * @details Reduces the 7-parameter boilerplate to 5 for the common endofunctor
 * case.
 */
export template <template <typename> typename F, template <typename> typename G,
                 typename T, typename Op, auto η_X>
using NaturalEndo = Naturality<F, G, T, T, Op, Op, η_X>;

/** @section Internal_Morphism_Traits (Private to :category) */

// Primary template: Redirects to member function pointer if T is a class
template <typename T>
struct morphism_traits : morphism_traits<decltype(&T::operator())> {};

// Specialization for const member functions (Standard Lambdas)
template <typename C, typename R, typename A>
struct morphism_traits<R (C::*)(A) const> {
  using argument_type = A;
  using result_type = R;
};

// Specialization for non-const member functions (Mutable Lambdas)
template <typename C, typename R, typename A>
struct morphism_traits<R (C::*)(A)> {
  using argument_type = A;
  using result_type = R;
};

// Specialization for raw function pointers
template <typename R, typename A>
struct morphism_traits<R (*)(A)> {
  using argument_type = A;
  using result_type = R;
};

// Specialization for raw function types (Final fallback)
template <typename R, typename A>
struct morphism_traits<R(A)> {
  using argument_type = A;
  using result_type = R;
};

/** @section Verification: Morphism Discovery */

// 1. Proof: Discovery for raw function pointers.
constexpr bool raw_func(int) { return true; }
static_assert(
    std::same_as<morphism_traits<decltype(&raw_func)>::argument_type, int>,
    "Discovery: Failed to extract argument from raw function pointer.");

// 2. Proof: Discovery for anonymous lambdas.
using IsZero = decltype([](int x) { return x == 0; });
static_assert(std::same_as<morphism_traits<IsZero>::argument_type, int>,
              "Discovery: Failed to extract argument from lambda.");

// 3. Proof: Discovery for mutable lambdas (Stateful).
using Counter = decltype([i = 0](int x) mutable { return x + i++; });
static_assert(std::same_as<morphism_traits<Counter>::argument_type, int>,
              "Discovery: Failed to extract argument from mutable lambda.");

/**
 * @section The Unit Proof (η: 1_𝒞 ⟹ G)
 *
 * @details
 * Under the Curry-Howard correspondence, this alias is the 'Assembler'.
 * It constructs the 'Naturality' witness by:
 * 1. Automatic Discovery: Extracting T and U from the 'Secret Sauce' (η_X).
 * 2. Formal Lifting: Wrapping the raw action into a tagged 'arrow'.
 * 3. Inhabitation: Instantiating the 'Naturality' struct with verified parts.
 *
 * @tparam G   The Target Functor (The "Box").
 * @tparam OpF The Source Operation (in 𝒞).
 * @tparam OpG The Target Operation (in 𝒟).
 * @tparam η_X The Morphism Component (The raw C++ action).
 */
export template <template <typename> typename F, template <typename> typename G,
                 typename OpF, typename OpG, auto η_X>
using unit = Naturality<
    F, G,
    typename morphism_traits<decltype(η_X)>::argument_type,  // T
    typename morphism_traits<decltype(η_X)>::result_type,    // U
    OpF, OpG,
    // The Lifting: Converts raw lambda into a tagged Arrow (Proof-Object)
    arrow<typename morphism_traits<decltype(η_X)>::argument_type,
          typename morphism_traits<decltype(η_X)>::result_type>(η_X)>;

/**
 * @brief The Unit Assembler (Endo-specialized).
 * @details Infers T and U from η_X, assuming a single Category (T, Op).
 */
export template <template <typename> typename F, template <typename> typename G,
                 typename Op, auto η_X>
using unit_endo = unit<F, G, Op, Op, η_X>;

/** @section Canonical_Embeddings: The One-Liner On-Ramps */

// 1. Logic -> Character (B ↪ Z/256Z)
export using η_bool_char =
    unit<Id, Id, std::logical_and<bool>, std::multiplies<char>,
         [](bool b) constexpr -> char { return b ? char(1) : char(0); }>;

// 2. Character -> Unsigned (Z/256Z ↪ Z_u)
export using η_char_uint =
    unit<Id, Id, std::plus<char>, std::plus<unsigned int>, [](char c) {
      return static_cast<unsigned int>(static_cast<char>(c));
    }>;

/** @section Canonical_Embeddings: Highway Proofs */

// 1. Logic -> Character (B ↪ Z/256Z)
// Path: bool >> η >> char_op   must equal  bool >> bool_op >> η
constexpr auto f_bool =
    endo<bool>([](bool x) { return !x; });  // A bool endomorphism

constexpr auto g_char =
    endo<char>([](char x) { return x == 0 ? char(1) : char(0); });

static_assert((true >> η_bool_char{} >> g_char) ==
                  (true >> f_bool >> η_bool_char{}),
              "Naturality: η_bool_char failed to commute on the Highway.");

// 2. Character -> Unsigned (Z/256Z ↪ Z_u)
static_assert((char{42} >> η_char_uint{}) == 42u,
              "Action: η_char_uint must be a bit-faithful promotion.");

/**
 * @brief Proof: Transitive Composition of Embeddings.
 * We compose the two Natural Transformations into a single 'Long Bridge'.
 * η_bool_uint = η_char_uint ∘ η_bool_char
 */
constexpr auto bool_to_uint = (η_bool_char{}) >> (η_char_uint{});

// 1. Proof: The 'Long Bridge' is a valid Arrow (B -> Z_u).
static_assert(IsArrow<decltype(bool_to_uint), bool, unsigned int>,
              "Transitivity: The composed promotion must be a valid Arrow from "
              "bool to uint.");

// 2. Action Proof: true -> 1u
static_assert((true >> bool_to_uint) == 1u,
              "Transitivity: The value was lost in translation.");

// 3. Action Proof: The 'False' identity (0) preservation.
static_assert((false >> bool_to_uint) == 0u,
              "Action: The escalator must preserve the 'False' identity (0) "
              "across species.");

/** @brief The Canonical Component η_X for a Functor F and Species T. */
export template <template <typename> typename F, typename T>
constexpr F<T> η_component(T x) = delete;

/** @brief Specialization: Identity's η_X is the Identity mapping. */
template <typename T>
constexpr Identity<T> η_component(T x) {
  return x;
}

/**
 * @section The Monadic Unit (η: Id ⟹ F)
 * @brief The CANONICAL unit for a verified Monad.
 * @details This is the 'Standard On-Ramp' for Functor F and Species T.
 *          Signature: (F, T, OpT). It finds the η_component automatically.
 */
export template <template <typename> typename F, typename T, typename OpT>
using η = unit<Id, F, OpT, OpT, η_component<F, T>>;

/**
 * @brief The Monadic Multiplication (μ_X: F⟨F⟨X⟩⟩ → F⟨X⟩)
 * @details In the Category of Endofunctors, the Monad is a Monoid.
 *          This 'Multiplication' collapses the nested composition
 *          of the functor back into a single layer.
 */
/*
export template <template <typename> typename F, typename T>
  requires(!std::same_as<F<F<T>>, F<T>>)  // The "Structure Gap" Constraint
constexpr F<T> multiplication(F<F<T>> box);*/

/** @brief Specialization: The Identity Monad's Multiplication. */
template <typename T>
constexpr Identity<T> multiplication(Identity<T> box) {
  return box;  // Unwrap the nested Identity
}

/**
 * @section The Monadic Multiplication (μ: F ∘ F ⟹ F)
 * @brief The CANONICAL multiplication (Join) for a verified Monad.
 * @details Signature: (F, T, OpT). It finds the multiplication logic
 * automatically.
 */
export template <template <typename> typename F, typename T, typename OpT>
using μ = Naturality<F, F, T, T, OpT, OpT, [](auto box) constexpr {
  return multiplication<F, T>(box);
}>;

/**
 * @concept η (The Monadic Unit)
 * @brief Formalizes the "On-Ramp" signature: T ⟹ F⟨T⟩.
 */
export template <typename Transform, template <typename> typename F, typename T>
concept is_η = IsArrow<Transform, T, F<T>>;

/**
 * @concept μ (The Monadic Multiplication)
 * @brief Formalizes the "Flattening" signature: F⟨F⟨T⟩⟩ ⟹ F⟨T⟩.
 */
export template <typename Transform, template <typename> typename F, typename T>
concept is_μ = IsArrow<Transform, F<F<T>>, F<T>>;

/**
 * @section Monad_as_Monoid (Explicit Definition)
 * We bridge the gap:
 *   η (Unit)           <--> identity_v (Monoid Unit)
 *   μ (Multiplication) <--> Op         (Monoid Operation)
 */
export template <template <typename> typename F, typename T, typename OpT>
concept IsMonad = IsEndofunctor<F, T, OpT> && requires(F<F<T>> nested, T x) {
  IsEndofunctor<F, T, OpT> && requires {
    // 1. Structural Verification: Do the canonical aliases exist?
    typename η<F, T, OpT>;
    typename μ<F, T, OpT>;

    // 2. Role Verification: Do these types satisfy the Monadic Concepts?
    requires is_η<η<F, T, OpT>, F, T>;
    requires is_μ<μ<F, T, OpT>, F, T>;

    // 3. Axiomatic Action (Uncommented & Refined):
    // We verify the actual mapping of data through the witness.
    { η<F, T, OpT>{}(x) } -> std::same_as<F<T>>;
    { μ<F, T, OpT>{}(nested) } -> std::same_as<F<T>>;
  };
};

/** @section Monadic_Guardrails: Enforcing the Contract
 *  If a species satisfies the concept but lacks a specialization,
 *  we delete the operator to prevent fallback to bit-shifts.
 */

/** @brief Deleted Push: η : T → F<T> */
export template <typename T, template <typename> typename F, typename OpT>
  requires IsMonad<F, T, OpT>
auto operator>>(T&&, into_tag<F>) = delete;

/** @brief Deleted Bind: >>= : F<T> → (T → F<U>) → F<U> */
export template <typename T, template <typename> typename F, typename OpT,
                 typename Function>
  requires IsMonad<F, T, OpT>
auto operator>>=(const F<T>&, Function) = delete;

/**
 * @section Monadic_Bind (The Logical Chain)
 * @brief The >>= operator (Bind/AndThen) for Monadic Composition.
 * @details Synthesized as: Map (f) followed by Join (μ).
 */
export template <typename T, template <typename> typename F, typename Function>
  requires IsMonad<F, T, std::plus<T>>  // Optional: Tighten with concepts
constexpr auto operator>>=(const F<T>& box, Function f) {
  // Logic: Unbox -> Apply -> Return new Box
  // For the Identity Monad (Box), this is just f(x).
  return f(box.value);
}

/** @brief Rvalue overload for "High-Speed" Bind */
export template <typename T, template <typename> typename F, typename Function>
constexpr auto operator>>=(F<T>&& box, Function f) {
  return f(std::move(box.value));
}

/** @section Level_0_Final_Proof: The Box Monad */

// 1. Proof: Box satisfies the formal IsMonad concept for (Z, +)
static_assert(IsMonad<Box, int, std::plus<int>>,
              "Ontology: Box must be recognized as a formal Monad.");

// 2. Action Proof: Join (μ) must collapse the context via the pipe
static_assert((42 >> into<> >> into<> >> join<>) == 42 >> into<>,
              "Ontology: The Monadic Join (μ) failed the Action Proof.");

// 3. Action Proof: Unit (η) must lift the species
static_assert((42 >> into<>) == Box{42},
              "Ontology: The Monadic Unit (η) failed the Action Proof.");

/**
 * @section Comonadic_Morphisms: Extract (ε) and Duplicate (δ)
 */

/**
 * @concept IsComonad
 * @brief The Dual of a Monad: A Functor with Extract and Duplicate.
 */
export template <template <typename> typename W, typename T, typename OpT>
concept IsComonad = IsEndofunctor<W, T, OpT> && requires(W<T> box) {
  { extract_v(box) } -> std::same_as<T>;
  { duplicate_v(box) } -> std::same_as<W<W<T>>>;
};

/** @brief ε: W⟨T⟩ → T (The Core Extraction) */
export template <template <typename> typename W, typename T>
constexpr T extract_v(W<T> box) {
  // If the compiler reaches here, it means no specialization matched.
  // In C++23, we can just static_assert(false) or similar.
  return box.value;
}

// The reality of how to extract from a Box
template <typename T>
constexpr T extract_v(Box<T> box) {
  return box.value;
}

/** @section Identity_Comonad_Specialization */
template <typename T>
constexpr T extract_v(Id<T> box) {
  return box;
}

/** @brief The Extraction Tag (ε: W ⟹ Id) */
template <template <typename> typename W>
struct extract_tag {};

template <typename T>
constexpr Id<Id<T>> duplicate_v(Id<T> box) {
  return box;
}

// The reality of how to duplicate a Box
template <typename T>
constexpr Box<Box<T>> duplicate_v(Box<T> box) {
  return Box<Box<T>>{box};
}

/** @brief The Duplication Tag (δ: W ⟹ W ∘ W) */
template <template <typename> typename W>
struct duplicate_tag {};

export template <typename T = void>
constexpr auto extract = extract_tag<Box>{};

export template <typename T = void>
constexpr auto duplicate = duplicate_tag<Box>{};

/** @brief δ: W⟨T⟩ → W⟨W⟨T⟩⟩ (The Contextual Duplication) */
export template <typename T, template <typename> typename W>
constexpr W<W<T>> duplicate_v(W<T> box) {
  return W<W<T>>{box};
}

/** @section Comonadic_Pull: The Directional Extraction */

// 1. The Lvalue Pull (For named variables like 'start')
export template <typename T, template <typename> typename W>
  requires requires(const W<T>& b) {
    { extract_v(b) } -> std::same_as<T>;
  }
constexpr auto operator<<(const W<T>& box, extract_tag<W>) {
  return extract_v(box);
}

// 2. The Rvalue Pull (For temporaries)
export template <typename T, template <typename> typename W>
  requires requires(const W<T>& b) {
    { extract_v(b) } -> std::same_as<T>;
  }
constexpr auto operator<<(W<T>&& box, extract_tag<W>) {
  return extract_v(std::move(box));
}

// 3. The Duplicate Pull (δ) - Lvalue
export template <typename T, template <typename> typename W>
  requires requires(const W<T>& b) {
    { duplicate_v(b) } -> std::same_as<W<W<T>>>;
  }
constexpr auto operator<<(const W<T>& box, duplicate_tag<W>) {
  return duplicate_v(box);
}

// 4. The Duplicate Pull (δ) - Rvalue
export template <typename T, template <typename> typename W>
  requires requires(const W<T>& b) {
    { duplicate_v(b) } -> std::same_as<W<W<T>>>;
  }
constexpr auto operator<<(W<T>&& box, duplicate_tag<W>) {
  return duplicate_v(std::move(box));
}

/** @section Comonad_Verification: The Slick Highway Proofs */

// 1. The Extract Law (ε): Getting the car out of the Box.
static_assert((42 >> into<> << extract<>) == 42,
              "Comonad Law: Extract (ε) must recover the raw Species.");

// 2. The Duplicate Law (δ): Making a 'Shadow' Box.
// Instead of that decltype(arrow) mess, we just pipe it.
static_assert((42 >> into<> << duplicate<>).value.value == 42,
              "Comonad Law: Duplicate (δ) must yield a nested Context.");

// 3. The Co-Unit Law: ext(dup(x)) == x
// Reading: "Take a box, duplicate it, then extract the outer layer."
static_assert((42 >> into<> << duplicate<> << extract<>) == 42 >> into<>,
              "Comonad Law: Extract ∘ Duplicate must be an Identity on Boxes.");

/** @brief The Retraction Morphism (r: 𝒢 ⟹ 1_𝒞).
    Primary template is deleted to enforce explicit existence. */
export template <typename 𝒯, typename Op𝒯, typename Op𝒢, auto η_X>
𝒯 retraction(decltype(η_X(std::declval<𝒯>())) y) = delete;

/**
 * @concept IsEmbedding
 * @brief A Natural Transformation η: 1_𝒞 ⟹ 𝒢 that preserves injectivity.
 *
 * @details
 * In Category Theory, an embedding (specifically a Monomorphism) is a morphism
 * f: A → B such that for any two morphisms g₁, g₂: X → A,
 * if f ∘ g₁ = f ∘ g₂, then g₁ = g₂.
 *
 * In our structuralist C++ approach, we verify this "Monomorphism" property
 * by proving the Morphism (η_X) is strictly injective (1:1)
 * across the entire Domain 𝒯.
 *
 * @tparam G   The Target Functor (The "Box" we are promoting into).
 * @tparam 𝒯   The Coordinate Species (The Domain Object, e.g., bool).
 * @tparam Op𝒯 The Binary Operation of the Source Category (e.g., AND).
 * @tparam Op𝒰 The Binary Operation of the Target Category (e.g., MULT).
 * @tparam η_X The Morphism Component (The actual C++ promotion logic).
 */
export template <template <typename> typename 𝒢, typename 𝒯, typename Op𝒯,
                 typename Op𝒢, auto η_X>
concept IsEmbedding =
    // 1. Structural Check: Does the Unit bridge actually exist?
    requires { typename unit<Id, 𝒢, Op𝒯, Op𝒢, η_X>; } && requires(𝒯 x) {
      // The Axiom: Mapping then Retracting must recover the exact same value.
      { retraction<𝒯, Op𝒯, Op𝒢, η_X>(η_X(x)) } -> std::same_as<𝒯>;

      // Proof of Identity preservation (Value Level)
      requires retraction<𝒯, Op𝒯, Op𝒢, η_X>(η_X(x)) == x;
    };

/** @section Self_Contained_Verification: The Local Embedding Proof */

static_assert(
    [] {
      // 1. Local Logic (The Brick)
      constexpr auto logic = [](bool b) { return b ? 1 : 0; };

      // 2. Local Highways (The Mortar)
      constexpr auto eta = arrow<bool, int>(logic);
      constexpr auto r = arrow<int, bool>([](int y) { return y != 0; });

      // 3. The Theorem: r ∘ η = id_bool
      // We verify both states of the Boolean species locally.
      return (true >> eta >> r) == true && (false >> eta >> r) == false;
    }(),
    "Local Proof: Bool-to-Int embedding must be a bit-faithful round-trip.");

}  // namespace dedekind::ontology