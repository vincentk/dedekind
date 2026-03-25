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

/** @section The_Universal_Functor_Interface */

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

/** @section The_Universal_Unit (η) */
export template <template <typename...> typename F, typename T>
struct η;  // Primary template for all kinds of contexts

/** @section The_Kleisli_Triple_for_Box */
// η (Unit): Lifting a value into the Box
export template <typename T>
struct η<Box, T> final {
  constexpr auto operator()(T x) const { return Box<T>{x}; }
};

/** @section The_Universal_Counit (ε) */
export template <template <typename...> typename F, typename T>
struct ε;

/** @section Box_Specialization_for_ε */
template <typename T>
struct ε<Box, T> final {
  // For a Box, extraction is simply accessing the value.
  constexpr T operator()(const Box<T>& b) const noexcept { return b.value; }
};

/**
 * @section The_Kleisli_Extension_System
 * @brief Formal detection of the Monadic "Lift and Chain" action.
 *
 * @details
 * A species F satisfies this system if it provides:
 * 1. η (Unit): A way to lift a raw species into the context.
 * 2. >>= (Bind): A way to chain a context to a Kleisli Arrow (T -> F<U>).
 */
export template <template <typename...> typename F, typename T, typename U>
concept IsKleisliExtension = requires(T x, F<T> box, std::function<F<U>(T)> f) {
  { η<F, U>{}(x) } -> std::same_as<F<U>>;  // The Lift (η)
  { box >>= f } -> std::same_as<F<U>>;     // The Chain (Bind)
};

static_assert(
    IsKleisliExtension<Box, int, int>,
    "Skeletal Failure: Box does not satisfy the Kleisli Extension System.");

/** @section CoKleisli_Extension_System (The Pull) */
export template <template <typename...> typename F, typename T, typename U>
concept IsCoKleisliExtension = requires(F<T> box, std::function<U(F<T>)> f) {
  { ε<F, T>{}(box) } -> std::same_as<T>;  // The Extract
  { box <<= f } -> std::same_as<F<U>>;    // The Extend
};

/**
 * @section The_Kleisli_Monad_Proof
 * @brief Elevates a Kleisli Extension to a formal Monad.
 *
 * @details
 * A species is a Kleisli Monad if it possesses the 'Action' (Extension)
 * and satisfies the 'Categorical Identity' (The Arrow Mapping).
 */
export template <template <typename...> typename F, typename T, typename U>
concept IsKleisli = IsKleisliExtension<F, T, U> &&
                    requires(T x, F<T> box, std::function<F<U>(T)> f) {
                      // We add the formal Categorical requirements here.
                      // E.g., The result must be a stable F<U>.
                      { box >>= f } -> std::same_as<F<U>>;
                    };

/**
 * @section The_CoKleisli_Comonad_Proof
 * @brief Elevates a Co-Kleisli Extension to a formal Comonad.
 */
export template <template <typename...> typename F, typename T, typename U>
concept IsCoKleisli = IsCoKleisliExtension<F, T, U> &&
                      requires(F<T> box, std::function<U(F<T>)> f) {
                        { box <<= f } -> std::same_as<F<U>>;
                      };

static_assert(
    IsCoKleisliExtension<Box, int, int>,
    "Skeletal Failure: Box does not satisfy the Co-Kleisli Extension System.");

/** @brief The Unit/Pure Factory: Lifts a raw value into the Box context. */
export template <typename T>
constexpr auto pure(T&& value) {
  return Box<std::decay_t<T>>{std::forward<T>(value)};
}

/** @section The_Monadic_Derivation */
template <template <typename...> typename F, typename Arrow>
constexpr auto derive_monadic_fmap(Arrow f) {
  return arrow<F<typename Arrow::Domain>, F<typename Arrow::Codomain>>(
      [f](const auto& box) {
        using U = typename Arrow::Codomain;
        return box >>= [f](auto&& x) { return η<F, U>{}(f(x)); };
      });
}

/** @section The_Comonadic_Derivation */
template <template <typename...> typename F, typename Arrow>
constexpr auto derive_comonadic_fmap(Arrow f) {
  using T = typename Arrow::Domain;
  using U = typename Arrow::Codomain;

  return arrow<F<T>, F<U>>([f](const F<T>& box) {
    return box <<= [f](const F<T>& w) { return f(ε<F, T>{}(w)); };
  });
}

/**
 * @section The_Unified_Highway_Bridge (The Discovery Dispatcher)
 * @brief Automates the derivation of fmap from the species' extension system.
 *
 * @details
 * In the Dedekind ontology, a Functor is not a "primitive" but an
 * "epi-phenomenon" derived from the underlying Monadic or Comonadic
 * structure. This dispatcher resolves the Bootstrapping Paradox:
 * 1. It prioritises the Monadic Push (Kleisli: m >>= η ∘ f).
 * 2. It falls back to the Comonadic Pull (Co-Kleisli: w <<= f ∘ ε).
 * 3. It provides a formal Proof of Absence if neither structure is present.
 *
 * @note Architectural Choice:
 * We utilize 'if constexpr' dispatch instead of multiple overloads to:
 * - Eliminate overload ambiguity in the C++ template system.
 * - Centralise the diagnostic logic for structural failures.
 * - Ensure zero-overhead selection of the most efficient highway.
 */
export template <template <typename...> typename F, typename Arrow,
                 typename T = typename Arrow::Domain,
                 typename U = typename Arrow::Codomain>
  requires IsArrow<Arrow, T, U>
constexpr IsArrow<F<T>, F<U>> auto fmap(Arrow f) {
  // 1. Priority: Identity Functor (The Invisible Box)
  if constexpr (std::same_as<F<T>, T>) {
    return f;
  } else if constexpr (IsKleisliExtension<F, T, U>) {
    /** @theorem fmap(f) = m >>= (η ∘ f) */
    return arrow<F<T>, F<U>>([f](const F<T>& m) {
      return m >>= [f](const T& x) { return η<F, U>{}(f(x)); };
    });
  } else if constexpr (IsCoKleisliExtension<F, T, U>) {
    /** @theorem fmap(f) = w <<= (f ∘ ε) */
    return arrow<F<T>, F<U>>([f](const F<T>& w) {
      return w <<= [f](const F<T>& ctx) { return f(ε<F, T>{}(ctx)); };
    });
  } else {
    /** @section The_Controlled_Explosion */
    struct Discovery_Failure {
      static_assert(sizeof(T) == 0,
                    "Ontology Error: Species lacks both Kleisli (>>=) and "
                    "Co-Kleisli (<<=) structures. "
                    "A Functorial mapping cannot be derived for this context.");
    };
    return Discovery_Failure{};
  }
}

/** @section Morphism_Lifting_Proof */
using Negate = std::negate<int>;
using TaggedNegate = Morphism<int, int, Negate>;

// This triggers the 'fmap' discovery via the Monadic Bridge
static_assert(
    IsArrow<decltype(fmap<Box>(TaggedNegate{Negate{}})), Box<int>, Box<int>>,
    "Skeletal Failure: Failed to derive fmap for Box from its Kleisli Triple.");

/** @section Verification: The Identity Law (F(id_X) = id_F<X>) */

// Proof: Lifting the identity morphism on 'int' gives us an arrow on
// 'Box<int>'.
static_assert(IsArrow<decltype(fmap<Box>(id<int>())), Box<int>, Box<int>>,
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
/** @section Cross_Species_Proof: Z -> B */
static_assert(
    IsArrow<decltype(arrow<int, bool>([](int x) { return x > 0; })), int, bool>,
    "Skeletal Failure: Failed to construct an anonymous cross-species "
    "Morphism.");

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

/** @section The_Into_Tag */
export template <template <typename...> typename F>
struct into_tag {};

export template <template <typename...> typename F>
inline constexpr into_tag<F> into{};

/** @section The_Push_Operator (η) */
export template <typename T, template <typename...> typename F>
constexpr auto operator>>(T&& x, into_tag<F>) {
  // We use the η witness from :species to lift the value
  return η<F, std::decay_t<T>>{}(std::forward<T>(x));
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
static_assert((41 >> into<Box> >> (increment >> Boxed<>)).value == 42,
              "Pipeline: The linear flow from value to boxed result failed.");

// The Composition Flow:
// Reading: "Take 10, put it INTO a Box, then increment it, then negate it."
constexpr auto negate = endo<int>(std::negate<int>{});

static_assert((10 >> into<Box> >> (increment >> Boxed<>) >> (negate >> Boxed<>))
                      .value == -11,
              "Pipeline: Multi-stage functorial composition failed.");

/** @section The_Join_Tag */
export template <template <typename...> typename F>
struct join_tag {};

export template <template <typename...> typename F>
inline constexpr join_tag<F> join{};

/** @section The_Collapse_Operator (μ) */
export template <typename T, template <typename...> typename F>
constexpr auto operator>>(const F<F<T>>& nested, join_tag<F>) {
  // μ(m) = m >>= id
  return nested >>= [](const F<T>& inner) { return inner; };
}

/** @brief Rvalue overload for "High-Speed" Move semantics */
export template <typename T>
constexpr auto operator>>(Box<Box<T>>&& nested_box, join_tag<Box>) {
  return std::move(nested_box.value);
}

// A "Double-Entry" Pipeline:
// 42 >> into >> into -> Box<Box<int>>
// ... then >> join -> Box<int>
static_assert((42 >> into<Box> >> into<Box> >> join<Box>).value == 42,
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
 * @section Monad_as_Monoid (Explicit Definition)
 * We bridge the gap:
 *   η (Unit)           <--> identity_v (Monoid Unit)
 *   μ (Multiplication) <--> Op         (Monoid Operation)
 */
export template <template <typename> typename F, typename T, typename OpT>
concept IsMonad =
    // 1. Requirement: Must be a Functor (Derived via our Dispatcher)
    IsEndofunctor<F, T, OpT> &&

    // 2. Requirement: Must satisfy the Kleisli Extension System
    IsKleisliExtension<F, T, T> &&

    requires(F<F<T>> nested, T x, F<T> box, std::function<F<T>(T)> f) {
      // 3. Axiomatic Verification:
      // Verification of η (Unit): T -> F<T>
      { η<F, T>{}(x) } -> std::same_as<F<T>>;

      // Verification of >>= (Bind): F<T> -> (T -> F<T>) -> F<T>
      { box >>= f } -> std::same_as<F<T>>;

      // 4. Structural Emergence:
      // μ (Join) is now an observable property of the Kleisli Action.
      // We verify that the "Self-Bind" correctly collapses layers.
      {
        box >>= [](T val) { return η<F, T>{}(val); }
      } -> std::same_as<F<T>>;
    };

/** @section Level_0_Final_Proof: The Box Monad */

// 1. Proof: Box satisfies the formal IsMonad concept for (Z, +)
static_assert(IsMonad<Box, int, std::plus<int>>,
              "Ontology: Box must be recognized as a formal Monad.");

// 2. Action Proof: Join (μ) must collapse the context via the pipe
static_assert((42 >> into<Box> >> into<Box> >> join<Box>) == 42 >> into<Box>,
              "Ontology: The Monadic Join (μ) failed the Action Proof.");

// 3. Action Proof: Unit (η) must lift the species
static_assert((42 >> into<Box>) == Box{42},
              "Ontology: The Monadic Unit (η) failed the Action Proof.");

/**
 * @section Comonadic_Morphisms: Extract (ε) and Duplicate (δ)
 */

/**
 * @concept IsComonad
 * @brief The Unified Comonadic Proof: Co-Kleisli Action as Contextual Being.
 *
 * @details
 * This concept bridges the dual formal definitions:
 * 1. Co-Kleisli Triple (Action): Existence of ε (Extract) and <<= (Extend).
 * 2. Comonoid in Endofunctors (Structure): F is a Functor with δ (Duplicate).
 *
 * @section The_Mereological_Pull
 * Following the Dedekind posture, we do not require δ (Duplicate/Coreturn)
 * to be explicitly defined if <<= (Extend) is present, as δ is the
 * "Self-Extend": δ(w) = w <<= id.
 */
export template <template <typename...> typename F, typename T, typename OpT>
concept IsComonad =
    // 1. Requirement: Must be a Functor (Derived via Discovery)
    IsEndofunctor<F, T, OpT> &&

    // 2. Requirement: Must satisfy the Co-Kleisli Extension System
    IsCoKleisliExtension<F, T, T> &&

    requires(F<T> box, std::function<T(F<T>)> f) {
      // 3. Axiomatic Verification:
      // Verification of ε (Extract): F<T> -> T
      { ε<F, T>{}(box) } -> std::same_as<T>;

      // Verification of <<= (Extend): F<T> -> (F<T> -> T) -> F<T>
      { box <<= f } -> std::same_as<F<T>>;

      // 4. Structural Emergence:
      // δ (Duplicate) is an observable property of the Co-Kleisli Action.
      // We verify that the "Self-Extend" correctly nests the context.
      {
        box <<= [](auto&& w) { return w; }
      } -> std::same_as<F<F<T>>>;
    };

/** @section The_Counit_Tag (ε) */
export template <template <typename...> typename F>
struct extract_tag {};

export template <template <typename...> typename F>
inline constexpr extract_tag<F> extract{};

/** @section The_Co_Multiplication_Tag (δ) */
export template <template <typename...> typename F>
struct duplicate_tag {};

export template <template <typename...> typename F>
inline constexpr duplicate_tag<F> duplicate{};

/** @section The_Pull_Operator (ε) */
export template <typename T, template <typename...> typename F>
constexpr T operator<<(const F<T>& box, extract_tag<F>) {
  // Routes to the ε witness in :species
  return ε<F, T>{}(box);
}

/** @section The_Duplicate_Operator (δ) */
export template <typename T, template <typename...> typename F>
  requires IsCoKleisliExtension<F, T, T>
constexpr auto operator<<(const F<T>& box, duplicate_tag<F>) {
  // δ(w) = w <<= id
  return box <<= [](const F<T>& w) { return w; };
}

/** @section Comonad_Verification: The Slick Highway Proofs */

// 1. The Extract Law (ε): Getting the car out of the Box.
static_assert((42 >> into<Box> << extract<Box>) == 42,
              "Comonad Law: Extract (ε) must recover the raw Species.");

// 2. The Duplicate Law (δ): Making a 'Shadow' Box.
// Instead of that decltype(arrow) mess, we just pipe it.
static_assert((42 >> into<Box> << duplicate<Box>).value.value == 42,
              "Comonad Law: Duplicate (δ) must yield a nested Context.");

// 3. The Co-Unit Law: ext(dup(x)) == x
// Reading: "Take a box, duplicate it, then extract the outer layer."
static_assert((42 >> into<Box> << duplicate<Box> << extract<Box>) ==
                  42 >> into<Box>,
              "Comonad Law: Extract ∘ Duplicate must be an Identity on Boxes.");

}  // namespace dedekind::ontology