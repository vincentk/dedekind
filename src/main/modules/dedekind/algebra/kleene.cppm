/**
 * @file kleene.cppm
 * @partition :kleene
 * @brief Kleene starter: the three-valued truth surface 𝕂3 as an algebra on a
 *        set (the downstream sibling of the Boolean 𝔹).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section algebra_kleene__Intent
 * Downstream witness (sibling of @c :boolean) that Kleene's three-valued truth
 * surface @c 𝕂3 @c = @c 𝔸<Ternary> is an @b algebra @b on @b a @b set: the
 * carrier @c Ternary is closed under the Kleene meet @c ∧, join @c ∨ and
 * reflection @c ¬.  It is a bounded @b distributive @b lattice (the De Morgan /
 * Kleene sibling of the Boolean @c 𝔹 @c = @c 𝔸<bool>), but @b not a Boolean
 * algebra: the interior value @c Unknown is self-negating (@c ¬Unknown @c =
 * @c Unknown), so @c 𝕂3 is uncomplemented.
 *
 * The species-level statements live upstream in @c :logic
 * (@c IsBoundedDeMorganChain / @c IsBooleanLogic); this partition pins their
 * downstream algebra-on-set counterpart, where the logic's lattice structure
 * meets @c IsAlgebraOnSet.  Closes #912.
 */
module;

export module dedekind.algebra:kleene;

import dedekind.category;
import dedekind.sets;
import :universal;

namespace dedekind::algebra {
using namespace dedekind::category;
using namespace dedekind::sets;

/** @brief The canonical Kleene universe @c 𝕂3 @c = @c 𝔸<Ternary> (the
 *  three-valued sibling of @c 𝔹 @c = @c 𝔸<bool>). */
export inline constexpr auto 𝕂3 = 𝔸<Ternary>;

/** @brief Kleene meet @c ∧ = @c Kleene::AND (numeric min on the truth order),
 *  as an element-level function object closing on @c Ternary. */
struct KleeneMeet {
  constexpr Ternary operator()(Ternary a, Ternary b) const noexcept {
    return Kleene::AND(a, b);
  }
};
/** @brief Kleene join @c ∨ = @c Kleene::OR (numeric max). */
struct KleeneJoin {
  constexpr Ternary operator()(Ternary a, Ternary b) const noexcept {
    return Kleene::OR(a, b);
  }
};
/** @brief Kleene reflection @c ¬ = @c Kleene::RFL (the De Morgan involution).
 */
struct KleeneNot {
  constexpr Ternary operator()(Ternary a) const noexcept {
    return Kleene::RFL(a);
  }
};

// 𝕂3 is a bona-fide set object --- the IsSet anchor for the algebra-on-set.
static_assert(IsSet<decltype(𝕂3)>,
              "𝕂3 = 𝔸<Ternary> is the canonical IsSet anchor for the Kleene "
              "three-valued surface.");

// THE downstream witness (#912): 𝕂3 is an algebra on a set --- the Kleene
// meet / join / reflection close on the carrier Ternary.  The De Morgan /
// distributive-lattice sibling of 𝔹 = 𝔸<bool>.
static_assert(
    IsAlgebraOnSet<decltype(𝕂3), KleeneMeet, KleeneJoin, KleeneNot>,
    "𝕂3 is an algebra on a set under the Kleene meet / join / reflection.");

// Correspondence to the species-level statements upstream (:logic): Kleene is a
// bounded De Morgan chain and --- unlike 𝔹 --- NOT Boolean (uncomplemented,
// since ¬Unknown = Unknown pins a self-dual interior grade).
static_assert(IsBoundedDeMorganChain<Kleene> && !IsBooleanLogic<Kleene>,
              "Kleene: a bounded De Morgan chain on a set, NOT a Boolean "
              "algebra (uncomplemented).");

}  // namespace dedekind::algebra
