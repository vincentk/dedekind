/**
 * @file dedekind/algebra/kleene.cppm
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
 * meets @c IsAlgebraOnSet.  This is the algebra-on-set portion of #912; the
 * remaining acceptance criterion (registering @c Ternary in the total/order
 * lattice machinery) is deferred, so #912 stays open.
 *
 * Wikipedia: Three-valued logic, Kleene algebra (with involution),
 * Łukasiewicz–Moisil algebra.
 *
 * @note "Metoda algebraiczna w logice polega na traktowaniu każdego systemu
 *       logicznego jako pewnego określonego rodzaju algebry abstrakcyjnej."
 *       --- Helena Rasiowa (echoing @c :logic upstream).  This partition is
 *       exactly that thesis made concrete for the Kleene system: @c 𝕂3 is the
 *       abstract algebra of three-valued logic, an @c IsAlgebraOnSet.
 *       [Trans: The algebraic method in logic consists in treating every
 *       logical system as a specific type of abstract algebra.]
 */
module;

#include <functional>  // std::logical_and / logical_or / logical_not (transparent)

export module dedekind.algebra:kleene;

import dedekind.category;
import dedekind.sets;
import :universal;

namespace dedekind::algebra {
using namespace dedekind::category;
using namespace dedekind::sets;

/** @brief The canonical Kleene universe @c 𝕂3 @c = @c 𝔸<Ternary, Boole,
 *  Finite> (the three-valued sibling of @c 𝔹 @c = @c 𝔸<bool>).
 *
 *  The @c Finite cardinality tag is explicit: @c Ternary is a three-element
 *  carrier, but the @c 𝔸 primary defaults @c C to @c ℵ_0, which would report
 *  this universe as countably infinite (and route @c NaturalLogic through
 *  @c Kleene rather than the decided @c Boole membership classifier this
 *  ambient wants).  Mirrors the @c 𝔸<bool> @c = @c UniversalSet<bool, Boole,
 *  Finite> override in @c :boundaries, but kept local here so the Kleene
 *  carrier's cardinality stays a downstream (:algebra) fact rather than being
 *  pushed up into @c :sets. */
export inline constexpr auto 𝕂3 = 𝔸<Ternary, Boole, Finite>;

// 𝕂3 is a bona-fide set object --- the IsSet anchor for the algebra-on-set.
static_assert(IsSet<decltype(𝕂3)>,
              "𝕂3 = 𝔸<Ternary> is the canonical IsSet anchor for the Kleene "
              "three-valued surface.");

// THE downstream witness (#912): 𝕂3 is an algebra on a set.  The element-level
// operations are Ternary's OWN logical operators --- @c && (meet ∧ = min),
// @c || (join ∨ = max), @c ! (reflection ¬) --- reused via the @b transparent
// @c std function objects (which forward to Ternary's operators and return
// @c Ternary, unlike the homogeneous @c std::logical_and<Ternary> that would
// decay to @c bool).  No bespoke wrappers: this matches the "the logical
// operators ARE the meet/join register" decision from the @c Truth<L> register
// (#910).  The De Morgan / distributive-lattice sibling of 𝔹 = 𝔸<bool>.
static_assert(
    IsAlgebraOnSet<decltype(𝕂3), std::logical_and<>, std::logical_or<>,
                   std::logical_not<>>,
    "𝕂3 is an algebra on a set under Ternary's own ∧ / ∨ / ¬ (&& / || / !).");

// Correspondence to the species-level statements upstream (:logic): Kleene is a
// bounded De Morgan chain and --- unlike 𝔹 --- NOT Boolean (uncomplemented,
// since ¬Unknown = Unknown pins a self-dual interior grade).
static_assert(IsBoundedDeMorganChain<Kleene> && !IsBooleanLogic<Kleene>,
              "Kleene: a bounded De Morgan chain on a set, NOT a Boolean "
              "algebra (uncomplemented).");

}  // namespace dedekind::algebra
