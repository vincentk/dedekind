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

/** @section The_Box_Species (The Standard Model) */
export template <typename T>
struct Box final {
  using machine_type = T;
  T value;

  constexpr bool operator==(const Box& other) const = default;
};

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

}  // namespace dedekind::category
