/**
 * @file ontology:category.cppm
 * @brief The Categorical "Skeletal" Foundations.
 *
 * Wikipedia: Category theory, Morphism, Functor
 */
export module dedekind.ontology:category;

namespace dedekind::ontology {

/** @concept IsMagmoid - Binary Composition. */
export template <typename T, typename Op>
concept IsMagmoid = requires(T a, T b) {
  { Op{}(a, b) } -> std::same_as<T>;
};

/** @concept IsSemigroupoid - Associativity: (f ∘ g) ∘ h = f ∘ (g ∘ h) */
export template <typename T, typename Op>
concept IsSemigroupoid =
    IsMagmoid<T, Op> && requires { requires is_associative_v<T, Op>; };

/**
 * @concept IsSmallCategory
 * @brief Every object has an Identity Morphism (id).
 */
export template <typename T, typename Op>
concept IsSmallCategory = IsSemigroupoid<T, Op> && requires {
  { identity_v<T, Op> } -> std::same_as<T>;
};

/** @concept IsGroupoid - Every arrow is reversible (Isomorphism). */
export template <typename T, typename Op>
concept IsGroupoid = IsSmallCategory<T, Op> && requires(T a) {
  { inverse<T, Op>(a) } -> std::same_as<T>;
};

/**
 * @concept IsFunctor
 * @brief A mapping between categories that preserves structure.
 *
 * @details F: C ↣ D such that F(f ∘ g) = F(f) ∘ F(g).
 *          In C++, this is a Type-Morphism (template) that preserves
 *          the IsSmallCategory "Soul".
 */
export template <template <typename> typename F, typename T, typename Op>
concept IsFunctor =
    IsSmallCategory<T, Op> &&
    IsSmallCategory<F<T>, Op>;  // Simplification: assuming Op maps over F
}  // namespace dedekind::ontology

/** @brief A Natural Transformation between Functors F and G. */
template <typename T>
G<T> natural_transformation(F<T> x);
