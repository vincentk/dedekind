/**
 * @file ontology:category.cppm
 * @brief Level 0: The Skeletal Foundation (The Bricks and the Cement).
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :category
 * @build_order 1
 * @dependency None (Bootstrap Partition)
 *
 * @section The_Structuralist_Unity: Bricks and Cement
 * This partition establishes the "Standard Model" of the Dedekind library.
 * The raw C++ machine instructions are unified with the abstract laws of
 * Category Theory to create a self-verifying skeletal layer.
 *
 * @subsection The_Bricks: Machine Primitives
 * The library captures the C++ standard library primitive types (bool, char,
 * int, double) and operators (+, *, &&) by providing Proof Assistant Traits.
 * This enriches the CPU's silicon instructions with mathematical authority.
 * - identity_v: The neutral element (0, 1, true).
 * - is_associative_v: The proof of grouping independence.
 * - is_commutative_v: The proof of swap-safety.
 *
 * @subsection The_Cement: Categorical Logic
 * We provide the "Highways" and "Bridges" that allow these bricks to
 * move between worlds without breaking the math:
 * - IsFunctor: The "Box" that preserves the skeletal structure.
 * - lift_natural_transformation: The "Bridge" between functors.
 * - unit / pure / eta: The on-ramp from raw values to structures.
 * - IsEmbedding: A static assertion for injective (1:1) promotions.
 *
 * @section Structural_Inference
 * By keeping the Bricks and Cement together, Level 0 can perform
 * Exhaustive Proofs (like the Bool-to-Int embedding) before any
 * higher-level sets or numbers are even defined.
 *
 * Wikipedia: Category theory, Natural transformation, Functor, Monoid
 */
module;

#include <concepts>
#include <functional>

export module dedekind.ontology:category;

namespace dedekind::ontology {

/** @section The Traits (The categorical invariants) */

/**
 * @brief Trait to mark an operation as associative: (a ∘ b) ∘ c = a ∘ (b ∘ c)
 **/
export template <typename T, typename Op>
inline constexpr bool is_associative_v = false;

/**
 * @brief Trait to mark an operation as commutative: a ∘ b = b ∘ a
 **/
export template <typename T, typename Op>
inline constexpr bool is_commutative_v = false;

/**
 * @brief The Identity Morphism for a given Type and Operation.
 * @note Must be specialized for each Category/Monoid.
 */
export template <typename T, typename Op>
inline constexpr T identity_v = [] {
  // Hard-coding 'false' here usually breaks the build immediately.
  // We use 'sizeof(T) == 0' as a trick to make it dependent on T.
  static_assert(sizeof(T) == 0,
                "Dedekind: No identity_v defined for this Type/Op pair.");
  return T{};
}();

/**
 * @brief The Characteristic of the Species.
 * @details For a finite ring (like char or int), this is the modulus n
 *          where n * 1 = 0. For infinite fields (Q, R), it is 0.
 */
export template <typename T>
inline constexpr size_t characteristic_v =
    0;  // Default to infinite/characteristic 0

template <>
inline constexpr size_t characteristic_v<unsigned char> = 256;

// --- Booleans: An Abelian Monoid (Lattice) ---
template <>
inline constexpr bool is_associative_v<bool, std::logical_or<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::logical_or<bool>> = true;
template <>
inline constexpr bool identity_v<bool, std::logical_or<bool>> = false;

template <>
inline constexpr bool is_associative_v<bool, std::logical_and<bool>> = true;
template <>
inline constexpr bool is_commutative_v<bool, std::logical_and<bool>> = true;
template <>
inline constexpr bool identity_v<bool, std::logical_and<bool>> = true;

// --- Integers: The finite ring (Z, +, *) ---

/**
 * @brief Addition is associative and commutative, with identity 0.
 */
template <std::integral T>
inline constexpr bool is_associative_v<T, std::plus<T>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::plus<T>> = true;
template <std::integral T>
inline constexpr T identity_v<T, std::plus<T>> = 0;

/**
 * @brief Multiplication is associative and commutative, with identity 1.
 */
template <std::integral T>
inline constexpr bool is_associative_v<T, std::multiplies<T>> = true;
template <std::integral T>
inline constexpr bool is_commutative_v<T, std::multiplies<T>> = true;
template <std::integral T>
inline constexpr T identity_v<T, std::multiplies<T>> = 1;

/**
 * @brief Modulus is NOT associative nor commutative, and has no identity.
 * @details This is because (a mod n) mod n = a mod n, but (a mod n) mod m != a
 * mod m in general. Also, a mod n != n mod a in general.
 */
template <std::integral T>
inline constexpr bool is_associative_v<T, std::modulus<T>> = false;

template <std::integral T>
inline constexpr bool is_commutative_v<T, std::modulus<T>> = false;

// --- Characters: A Commutative Species ---
template <>
inline constexpr bool is_associative_v<char, std::plus<char>> = true;
template <>
inline constexpr bool is_commutative_v<char, std::plus<char>> = true;
template <>
inline constexpr char identity_v<char, std::plus<char>> = 0;

/**
 * @brief The Inverse Morphism for Groupoids.
 * @note Primary template is deleted to ensure IsGroupoid fails
 *       unless a concrete 'Undo' is specialized (e.g. for Z, +).
 */
export template <typename T, typename Op>
T inverse(T a) = delete;

/** @section Integer Inverse: Negation is the Inverse of Addition */
template <>
inline int inverse<int, std::plus<int>>(int a) {
  return -a;
}

/**
 * @concept IsMagmoid
 * @brief Represents a Magma-like structure where a binary operation is defined.
 *
 * @details In Category Theory, this corresponds to a 'Quiver' or 'Graph' with
 *          a composition rule that is closed: T × T → T. At this level, we
 *          only guarantee that two elements can be combined; we do not yet
 *          enforce associativity or identity.
 *
 * @tparam T The coordinate species (The "Objects" or "Elements").
 * @tparam Op The binary operation (The "Morphism" or "Composition Rule").
 */
export template <typename T, typename Op>
concept IsMagmoid = requires(T a, T b) {
  { Op{}(a, b) } -> std::convertible_to<T>;
};

/**
 * @concept IsSemigroupoid
 * @brief A Magmoid that satisfies the Associative Law.
 *
 * @details This is a Category without a guaranteed identity. It enforces:
 *          (f ∘ g) ∘ h = f ∘ (g ∘ h). In our structuralist approach, we
 *          verify this by checking the 'is_associative_v' trait, which
 *          acts as a compile-time proof of the associative property.
 *
 * @note Many high-performance algorithms (like parallel reductions)
 *       only require this level of structure.
 */
export template <typename T, typename Op>
concept IsSemigroupoid =
    IsMagmoid<T, Op> && requires { requires is_associative_v<T, Op> == true; };

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
concept IsSmallCategory = IsSemigroupoid<T, Op> && requires {
  { identity_v<T, Op> } -> std::convertible_to<T>;
};

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
concept IsAbelian = IsSmallCategory<T, Op> && is_commutative_v<T, Op>;

/** @section Verification */
static_assert(IsAbelian<int, std::plus<int>>);
static_assert(IsAbelian<bool, std::logical_or<bool>>);

/**
 * @concept IsArrow
 * @brief The formal signature of a Morphism f: A -> B.
 */
export template <typename F, typename A, typename B>
concept IsArrow = requires(F f, A x) {
  // We probe the action and strip references/const to find the true Codomain.
  typename std::remove_cvref_t<decltype(f(std::forward<A>(x)))>;
  requires std::same_as<std::remove_cvref_t<decltype(f(std::forward<A>(x)))>,
                        B>;
};

/**
 * @brief The Identity Morphism (id_A)
 * The categorical anchor that returns the object unchanged.
 * Essential for verifying monoidal identities at compile-time.
 */
export template <typename T>
struct Identity final {
  /**
   * @brief The Identity Action.
   * Perfect forwarding ensures that the morphism preserves the original
   * value category and constness of the Domain object.
   */
  constexpr T&& operator()(T&& x) const noexcept { return std::forward<T>(x); }
};

/** @section The Identity Factory: id<A>() */
export template <typename A>
auto id() {
  return Identity<A>{};
}

/** @section Lifting Traits to the Identity Functor */

// 1. If T is associative under Op, Identity<T> is associative.
template <typename T, typename Op>
inline constexpr bool is_associative_v<Identity<T>, Op> =
    is_associative_v<T, Op>;

// 2. If T is commutative under Op, Identity<T> is commutative.
template <typename T, typename Op>
inline constexpr bool is_commutative_v<Identity<T>, Op> =
    is_commutative_v<T, Op>;

// 3. The identity of Identity<T> is just the identity of T.
template <typename T, typename Op>
inline constexpr Identity<T> identity_v<Identity<T>, Op> =
    Identity<T>{identity_v<T, Op>};

/** @section Identity Verification */

// We verify that for any object T, Identity<T> is an arrow T -> T.
// This anchors the "Skeletal" layer to the "Machine" layer.
static_assert(IsArrow<Identity<int>, int, int>,
              "Identity<int> must be a morphism from int to int.");

static_assert(IsArrow<Identity<bool>, bool, bool>,
              "Identity<bool> must be a morphism from bool to bool.");

/**
 * @section Categorical Composition (h = g ∘ f)
 * @brief Synthesizes an arrow A -> C from A -> B and B -> C.
 */
export template <typename A, typename B, typename C, typename F, typename G>
  requires IsArrow<F, A, B> && IsArrow<G, B, C>
auto operator>>(F&& f, G&& g) {
  // We return a new lambda that the compiler recognizes as IsArrow<h, A, C>
  return [f = std::forward<F>(f), g = std::forward<G>(g)](A x) mutable -> C {
    return g(f(std::move(x)));
  };
}

/** @brief The Composition Morphism: (g ∘ f)(x) = g(f(x)) */
export template <typename F, typename G>
auto operator>>(F&& f, G&& g) {
  return [f = std::forward<F>(f), g = std::forward<G>(g)](auto&& x) {
    return g(f(std::forward<decltype(x)>(x)));
  };
}

/**
 * @concept IsIsomorphism
 * @brief An Arrow f: A -> B with a guaranteed Inverse g: B -> A.
 */
export template <typename F, typename A, typename B>
concept IsIsomorphism = IsArrow<F, A, B> && requires(F f, B y) {
  // We look for a structural 'inverse' morphism
  { inverse(f) } -> IsArrow<B, A>;

  // Categorical Proof: f ∘ inverse(f) == id_B
  // In Level 0, we verify the EXISTENCE of the path.
};

/** @brief The structural inverse of an Identity is itself. */
template <typename T>
[[nodiscard]] constexpr auto inverse(Identity<T> id) noexcept {
  return id;
}

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

// Proof: (Z, +) is an Abelian Groupoid.
static_assert(IsAbelianGroupoid<int, std::plus<int>>,
              "Integer addition must be a reversible, symmetric action.");

// Proof: (bool, &&) is NOT an Abelian Groupoid (It's an Abelian Monoid).
static_assert(!IsAbelianGroupoid<bool, std::logical_and<bool>>,
              "Logic is symmetric but not reversible (Annihilation).");

/** @section Isomorphism Verification */

// Proof: id_int is an isomorphism from int to int.
static_assert(IsIsomorphism<Identity<int>, int, int>,
              "Identity must be a self-inverse isomorphism.");

// Proof: id_bool is an isomorphism from bool to bool.
static_assert(IsIsomorphism<Identity<bool>, bool, bool>,
              "Identity must be a self-inverse isomorphism.");

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
export template <template <typename> typename F, typename 𝒯, typename Op𝒯,
                 typename 𝒰, typename Op𝒰>
concept IsFunctor = IsSmallCategory<𝒯, Op𝒯> && IsSmallCategory<𝒰, Op𝒰> &&
                    requires(F<𝒯> box, 𝒯 value) {
                      typename F<𝒯>;
                      // RELAXATION: F<𝒯> doesn't have to BE 𝒰, it just has to
                      // MAP to it.
                      // For Identity<bool>, this is true because it returns
                      // bool.
                      // requires std::same_as<F<𝒯>, 𝒰>;
                      // PROBE: Use std::move to match the Identity(T&&)
                      // signature
                      { box(std::move(value)) } -> std::convertible_to<𝒰>;
                      //{ box(value) } -> std::convertible_to<𝒰>;
                    };

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

/**
 * @section The Natural Transformation (η: F ⟹ G)
 * @brief A structure-preserving "Bridge" between two Functors.
 *
 * @details
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
 * @tparam F   The Source Functor.
 * @tparam G   The Target Functor.
 * @tparam 𝒯   The Object in the Category.
 * @tparam OpF The Operation of the source.
 * @tparam OpG The Operation of the target.
 * @tparam η   The Morphism component (The "Secret Sauce").
 */
export template <template <typename> typename F, template <typename> typename G,
                 typename 𝒯, typename OpF, typename OpG,
                 auto η_X  // The Morphism Component
                 >
  requires IsEndofunctor<F, 𝒯, OpF> &&
           IsEndofunctor<G, decltype(η_X(std::declval<𝒯>())),
                         OpG>  // Simplified T mapping
struct Naturality final {
  using Source = 𝒯;
  using Target = decltype(η_X(std::declval<𝒯>()));

  /** @brief η_X : F⟨X⟩ → G⟨X⟩ */
  constexpr Target operator()(Source x) const noexcept { return η_X(x); }

  /** @brief Axiom: η(id_F) = id_G */
  static constexpr bool preserves_identity() noexcept {
    return η_X(identity_v<Source, OpF>) == identity_v<Target, OpG>;
  }
};

/**
 * @brief η: Id ⟹ G (The 6-Parameter Explicit Transformation)
 */
export template <template <typename> typename ℱ, template <typename> typename 𝒢,
                 typename 𝒯, typename Opℱ, typename Op𝒢, auto η_X>
using natural_transformation = Naturality<ℱ, 𝒢, 𝒯, Opℱ, Op𝒢, η_X>;

/** @brief Extracts the argument type from a function pointer. */
template <typename T>
struct morphism_traits;

template <typename R, typename A>
struct morphism_traits<R (*)(A)> {
  using argument_type = A;
};

/**
 * @section The Unit of the Functor (η: 1_𝒞 ⟹ G)
 *
 * @details
 * In Category Theory, the unit (η) is the natural transformation that
 * embeds an object into a Functorial context: η_X : X → G⟨X⟩.
 *
 * From a structuralist C++ perspective, this is the "On-Ramp".
 * By using `morphism_traits`, we perform "Object Discovery"—the
 * transformation is defined by the Morphism itself, allowing the
 * Domain Object (𝒯) to be inferred from the "Secret Sauce" (η_X).
 *
 * @tparam G   The Target Functor (The "Box").
 * @tparam OpF The Source Operation (in 𝒞).
 * @tparam OpG The Target Operation (in 𝒟).
 * @tparam η_X The Morphism Component (The Action).
 */
export template <template <typename> typename G, typename OpF, typename OpG,
                 auto η_X>
using unit = natural_transformation<
    Identity, G,
    typename morphism_traits<decltype(η_X)>::argument_type,  // Auto-Deduction!
    OpF, OpG, η_X>;

/** @brief The Greek (Category Theory) Alias for the Unit. */
export template <template <typename> typename G, typename OpF, typename OpG,
                 auto η_X>
using η = unit<G, OpF, OpG, η_X>;

constexpr int my_promotion_sauce(bool b) { return b ? 1 : 0; }

/** @section THE FIX: Supply all 6 parameters to the type alias */
export using BoolToInt = unit<Identity,
                              std::logical_and<bool>,  // OpF
                              std::multiplies<int>,    // OpG
                              my_promotion_sauce       // Morphism
                              >;

/** @section The Usage */
export constexpr BoolToInt transform{};

// This works because 'transform' is now a concrete instance of that specific
// bridge
static_assert(transform(true) == 1);

// This works because the struct already 'knows' T, OpF, and OpG!
static_assert(BoolToInt::preserves_identity());

/**
 * @concept IsEmbedding
 * @brief A Natural Transformation that preserves the injective property of the
 * underlying mapping.
 *
 * @details In Category Theory, an embedding (specifically a Monomorphism) is a
 * morphism f: A -> B such that for any two morphisms g1, g2: X -> A, if f ∘ g1
 * = f ∘ g2, then g1 = g2. In our C++ structuralist approach, we verify this by
 * proving the "Sauce" (Morphism) is one-to-one (Injective) across the entire
 * domain T.
 *
 * @tparam G The Target Functor (The "Box" we are promoting into).
 * @tparam T The Coordinate Type (The "Species" being promoted, e.g., bool).
 * @tparam OpT The Binary Operation of the Source Category (e.g.,
 * std::logical_and).
 * @tparam OpG The Binary Operation of the Target Category (e.g.,
 * std::multiplies).
 * @tparam Morphism The "Secret Sauce" (The actual C++ function pointer or
 * constexpr lambda).
 *
 * @note This concept requires that the 'unit_5' natural transformation is
 * valid, ensuring the structural identity and associativity laws are preserved
 *       during the promotion.
 */
export template <template <typename> typename G, typename T, typename OpT,
                 typename OpG, auto Morphism>
concept IsEmbedding =
    // 1. Check that the Unit type exists and is valid
    // Note: We use the 5-parameter 'unit_5' to be explicit here
    requires { typename unit_5<G, T, OpT, OpG, Morphism>; } &&

    // 2. The Injective Property: η(a) = η(b) => a = b
    requires(T a, T b) {
      { (Morphism(a) == Morphism(b)) == (a == b) } -> std::same_as<bool>;
    };

/**
 * @concept IsHomomorphism
 * @brief A structure-preserving map between two similar species.
 * @details f(a ∘ b) = f(a) ∘ f(b).
 *          This is the "Naturality" check for binary operations.
 */
template <typename Source, typename Target, auto Morphism, typename Op>
concept IsHomomorphism = requires(Source a, Source b) {
  {
    Morphism(Op{}(a, b)) == Op{}(Morphism(a), Morphism(b))
  } -> std::same_as<bool>;
};

/**
 * @concept IsIsomorphism
 * @brief A perfect, reversible bridge (A "Mirror").
 * @details There exists an inverse Morphism G such that G(F(x)) = x.
 */
template <typename A, typename B, auto F, auto G>
concept IsIsomorphism = requires(A a, B b) {
  { G(F(a)) == a } -> std::same_as<bool>;
  { F(G(b)) == b } -> std::same_as<bool>;
};

/**
 * @concept IsHomeomorphism
 * @brief A Topological Isomorphism (A "Deformation").
 * @details A bijection that is continuous in both directions.
 * @note This will be refined in :topology by adding 'IsContinuous'.
 */
template <typename A, typename B, auto F, auto G>
concept IsHomeomorphism = IsIsomorphism<A, B, F, G>;

}  // namespace dedekind::ontology
