/**
 * @file dedekind.category:mereology
 * @partition :mereology
 * @brief Level 0c: Embryonic Mereology (The Language of Parts).
 *
 * Following the formal axiomatization of Stanisław Leśniewski, this partition
 * introduces the "Part-Whole" relation as a foundational structural primitive.
 * In this "embryonic" stage, we define the axioms of a partial order that
 * govern any category with mereological structure.
 *
 * @section Axioms
 * 1. Reflexivity: Everything is a part of itself.
 * 2. Transitivity: A part of a part is a part of the whole.
 * 3. Antisymmetry: Two distinct things cannot be parts of each other.
 */

export module dedekind.category:monad;

import :natural;

namespace dedekind::category {

// A "Double-Entry" Pipeline:
// 42 >> into >> into -> Box<Box<int>>
// ... then >> join -> Box<int>
static_assert((42 >> into<Box> >> into<Box> >> join<Box>).value == 42,
              "Monad Law: Join must collapse the double-context.");

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

// >>= (Bind): Chaining the Box to a Kleisli Arrow
export template <typename T, typename Func>
constexpr auto operator>>=(const Box<T>& b, Func&& f) {
  // We sample the part and hand it to the factory.
  return std::forward<Func>(f)(b.value);
}

/** @section The_Extend_Operator (<<=) */
export template <typename T, typename Func>
constexpr auto operator<<=(const Box<T>& b, Func&& f) {
  using U = std::invoke_result_t<Func, Box<T>>;
  // Co-Kleisli Extend: apply 'f' to the whole box,
  // and re-wrap the result in a new Box.
  return Box<U>{std::forward<Func>(f)(b)};
}

/** @brief The Unit/Pure Factory: Lifts a raw value into the Box context. */
export template <typename T>
constexpr auto pure(T&& value) {
  return Box<std::decay_t<T>>{std::forward<T>(value)};
}

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

}  // namespace dedekind::category
