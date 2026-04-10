/**
 * @concept IsPosetal
 * @brief Represents a Posetal Category (A Category derived from a Partially
 * Ordered Set).
 *
 * @section Categorical_Definition
 * A Posetal Category is a category where for any two objects A and B, there is
 * **at most one** morphism from A to B. In this framework:
 * - Objects are elements of the set.
 * - Morphisms represent the relation (a ≤ b).
 * - Identity morphisms correspond to Reflexivity (a ≤ a).
 * - Composition corresponds to Transitivity (a ≤ b and b ≤ c implies a ≤ c).
 * - Skeletality in the category corresponds to Antisymmetry (a ≤ b and b ≤ a
 * implies a = b).
 *
 * @section Order_Structure
 * This structure corresponds to a **Partially Ordered Set (Poset)**. Unlike a
 * Preorder, a Posetal Category is skeletal, meaning isomorphic objects are
 * identical.
 *
 * @quote
 * "In a sense, the most basic category is a partially ordered set;
 *  the arrows are just the instances of the order relation."
 *  — Saunders Mac Lane, *Categories for the Working Mathematician*
 *
 * @tparam T The type of objects in the poset.
 * @tparam Rel The relation defining the order (the Morphism Generator).
 */
module;

#include <concepts>
#include <functional>

export module dedekind.category:pullback;

import :morphism;   // For arrow<>
import :logic;      // For LogicalValue
import :cartesian;  // For Product
import :topoi;

namespace dedekind::category {

/**
 * @concept IsPullback
 * @brief The Fiber Product (X ×_Z Y) as a Subobject of the Product (X × Y).
 *
 * @details A pullback of f: X ⟶ Z and g: Y ⟶ Z is an object P that represents
 * the subset of the product species X × Y where the images in Z coincide.
 *
 * In this implementation, the pullback is realized as a Subobject:
 * - ι: P ↣ X × Y is the inclusion into the product space.
 * - π₁ = p1 ∘ ι and π₂ = p2 ∘ ι are the projections to the domains.
 * - Commutativity (f ∘ π₁ = g ∘ π₂) is guaranteed by the characteristic
 *   morphism χ used to classify the subobject.
 *
 * @tparam P The candidate pullback species (a Subobject).
 * @tparam F The morphism f: X ⟶ Z.
 * @tparam G The morphism g: Y ⟶ Z.
 */
export template <typename P, typename F, typename G>
concept IsPullback = IsSubobject<P, typename P::Ambient> &&
                     IsProduct<typename P::Ambient, Dom<F>, Dom<G>> &&
                     requires(P p, typename P::Member m) {
                       // π₁: P ⟶ X and π₂: P ⟶ Y (Projections via the inclusion
                       // ι)
                       { p.π1(m) } -> std::same_as<Dom<F>>;
                       { p.π2(m) } -> std::same_as<Dom<G>>;
                     };

/**
 * @brief The Characteristic Morphism χ: (X × Y) ⟶ Ω.
 *
 * In a Topos, every subobject is uniquely determined by a morphism into the
 * subobject classifier Ω. For a pullback, χ identifies pairs (x, y)
 * that map to the same image in the codomain Z.
 *
 * @tparam L The Logic Species defining the Subobject Classifier Ω.
 * @tparam Π The Product Species (e.g., std::pair<X, Y>).
 * @tparam F Morphism X ⟶ Z.
 * @tparam G Morphism Y ⟶ Z.
 */
template <typename L, typename Π, typename F, typename G>
  requires LogicalSpecies<L> && IsArrow<F> && IsArrow<G> &&
           IsProduct<Π, Dom<F>, Dom<G>>
auto make_χ(F f, G g) {
  using Ω = typename L::type;

  // We capture by value because Morphism is a lightweight skeletal struct.
  // If F or G contain large lambdas, Morphism handles the storage.
  return arrow<Π, Ω>([f, g](const Π& pair) -> Ω {
    return (f(pair.first) == g(pair.second)) ? L::True : L::False;
  });
}

/**
 * @brief The Pullback Factory (The Join).
 *
 * Constructs the Fiber Product P ≅ X ×_Z Y as the kernel of the
 * characteristic morphism χ.
 */
template <typename L = ClassicalLogic, typename Π, typename F_Raw,
          typename G_Raw>
auto pullback(F_Raw&& f_raw, G_Raw&& g_raw) {
  // 1. Lift raw callables into formal Morphisms using your skeletal factory.
  auto f = arrow(std::forward<F_Raw>(f_raw));
  auto g = arrow(std::forward<G_Raw>(g_raw));

  // 2. Construct the characteristic morphism χ using the formal Arrows.
  auto χ = make_χ<L, Π>(f, g);

  // 3. The Pullback is the subobject of Π classified by χ.
  return classify<Π>(χ);
}

/**
 * @concept IsEqualizer
 * @brief The kernel of a parallel pair (f, g: A ⟶ B).
 *
 * @details In the Dedekind Topos, an Equalizer represents the subobject E ↣ A
 * containing exactly those members where the morphisms f and g coincide.
 * It is the internal "Solution Set" for the equation f(x) = g(x).
 *
 * @section Universal_Property
 * For any object Q and morphism q: Q ⟶ A such that f ∘ q = g ∘ q,
 * there exists a unique morphism u: Q ⟶ E such that ι ∘ u = q.
 *
 * @tparam E The candidate Equalizer species (The Subobject).
 * @tparam F The first parallel morphism f: A ⟶ B.
 * @tparam G The second parallel morphism g: A ⟶ B.
 */
export template <typename E, typename F, typename G>
concept IsEqualizer =
    IsArrow<F> && IsArrow<G> &&
    std::same_as<typename F::Domain, typename G::Domain> &&
    std::same_as<typename F::Codomain, typename G::Codomain> &&
    requires(E e, typename E::Member m) {
      /**
       * @brief ι: E ↣ A
       * The canonical inclusion morphism from the Equalizer to the domain.
       */
      { e.ι(m) } -> std::same_as<typename F::Domain>;

      /**
       * @section Type_Witness
       * Validates that the composition f ∘ ι and g ∘ ι are well-formed at the
       * type level, ensuring the species E acts as a valid domain for the
       * parallel pair.
       */
      requires requires(typename F::Domain a, F f, G g) {
        { f(a) } -> std::same_as<typename F::Codomain>;
        { g(a) } -> std::same_as<typename G::Codomain>;
      };
    };

void verify_ontology() {
  using X = int;
  using Y = int;
  using Z = int;
  auto f = arrow<X, Z>([](X x) { return x; });
  auto g = arrow<Y, Z>([](Y y) { return y; });

  using Π = std::pair<X, Y>;
  auto P = pullback<ClassicalLogic, Π>(f, g);

  // Define the parallel pair over the Product Π
  auto h = arrow<Π, Z>([f](const Π& p) { return f(p.first); });
  auto k = arrow<Π, Z>([g](const Π& p) { return g(p.second); });

  // Now this assert should pass: P equalizes h and k
  static_assert(IsEqualizer<decltype(P), decltype(h), decltype(k)>);
  static_assert(IsPullback<decltype(P), decltype(f), decltype(g)>);
}

}  // namespace dedekind::category
