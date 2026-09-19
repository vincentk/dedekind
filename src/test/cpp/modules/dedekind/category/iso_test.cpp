/**
 * @file iso_test.cpp
 * @brief Witnesses for @c dedekind.category:iso --- the retract / iso-enabling
 *        surface.  Chiefly: @c IsIsomorphism auto-wires monic, epic, bijective
 *        and retractable, and it does so for an iso defined in an @b unrelated
 *        client namespace (the blanket @c retract is found by ordinary lookup,
 *        not only ADL into @c dedekind::category).
 */
#include <catch2/catch_test_macros.hpp>

import dedekind.category;

// A client-defined isomorphism in a namespace that has NOTHING to do with
// dedekind::category: the involution x |-> -x on int, its own inverse.  ADL on
// this type does not reach dedekind::category, so the blanket iso retract can
// only be found because it is declared before the IsRetractableArrow concept
// (ordinary lookup), which is exactly the property CP asked to pin.
namespace client_ns {
struct Flip {
  using Domain = int;
  using Codomain = int;
  constexpr int operator()(int x) const { return -x; }
};
// Self-inverse (negation is an involution): makes Flip an IsIsomorphism.
constexpr Flip inverse(Flip f) noexcept { return f; }
}  // namespace client_ns

using namespace dedekind::category;

// ── Type-level: iso ⟹ monic ∧ epic ∧ bijective ∧ retractable, all derived ────
static_assert(IsIsomorphism<client_ns::Flip>,
              "a self-inverse endo in a client namespace is an isomorphism.");
static_assert(IsMonicArrow<client_ns::Flip>,
              "iso ⟹ monic, derived (no is_monic_arrow_v opt-in).");
static_assert(IsEpicArrow<client_ns::Flip>,
              "iso ⟹ epic, derived (no is_epic_arrow_v opt-in).");
static_assert(IsBijectiveArrow<client_ns::Flip>,
              "iso ⟹ bijective, derived from monic ∧ epic.");
static_assert(
    IsRetractableArrow<client_ns::Flip>,
    "iso ⟹ retractable via the blanket IsoRetract, for an iso in an UNRELATED "
    "namespace: the blanket retract is found by ordinary lookup, so the "
    "advertised implication does not depend on dedekind::category being an "
    "associated namespace of the arrow (CP #876 finding).");
// The image of an iso is the whole codomain, hence decidable (ClassicalLogic).
static_assert(
    std::same_as<typename decltype(image_of(client_ns::Flip{}))::logic_species,
                 ClassicalLogic>,
    "iso ⟹ decidable image, for a client-namespace iso.");

TEST_CASE("category:iso — client-namespace iso is retractable, retract inverts",
          "[category][iso][retract]") {
  // The derived retract is the total inverse wrapped in Some: retract(Flip)(5)
  // = η(maybe_hub, inverse(Flip)(5)) = Some(Flip(5)) = Some(-5).
  auto r = retract(client_ns::Flip{});
  auto m = r(5);
  CHECK(m.has_value());
  CHECK(*m == -5);

  // image_of(iso) recognises every codomain element as a member.
  auto img = image_of(client_ns::Flip{});
  CHECK(img(5) == true);
  CHECK(img(-7) == true);
}
