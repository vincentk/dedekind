/** @file dedekind/category/factorisation_test.cpp
 *
 * Unit coverage for @c :category::factorisation (#718 Slice 1) and the
 * @c :image rewire + @c image_of factory (#718 Slice 2).
 *
 * Coverage targets:
 *  - The five Form-chain concepts (rows 4–8 of #718): @c IsRegularEpi,
 *    @c IsRegularMono, @c IsFactorisationSystem, @c IsRegularCategory,
 *    @c IsExactCategory.
 *  - The two upgrade chains: @c IsExactCategory ⇒ @c IsRegularCategory
 *    ⇒ @c IsFactorisationSystem (category-level) and
 *    @c is_regular_epi_v ⇒ @c is_epic_arrow_v plus the dual (arrow-level).
 *  - The @c image_of(F) factory runtime body and the @c ImageChi<F>
 *    Honest-Rejection default classifier (@c Ternary::Unknown).
 *
 * Existential / structural witnesses live as @c static_assert inside
 * the main partition (cf.\ @c feedback_static_assert_in_main.md); the
 * runtime exercises below ensure the factory bodies are covered by
 * codecov in addition to the type-checked compile-time witnesses.
 */

#include <catch2/catch_test_macros.hpp>

import dedekind.category;

using namespace dedekind::category;

TEST_CASE(
    "category:factorisation — Identity<int> is the trivial regular epi / mono",
    "[category][factorisation][regular-epi][regular-mono][identity]") {
  /** @brief Identity<T> is the canonical witness for both legs of the
   *         (regular epi, mono) factorisation system: the coequalizer
   *         and equalizer of the degenerate parallel pair (id, id) are
   *         both id itself. */
  STATIC_CHECK(IsRegularEpi<Identity<int>>);
  STATIC_CHECK(IsRegularMono<Identity<int>>);
  // Arrow auto-upgrade chain: every regular epi is an epi by the
  // is_regular_epi_v ⇒ is_epic_arrow_v partial spec in :factorisation.
  STATIC_CHECK(IsEpicArrow<Identity<int>>);
  STATIC_CHECK(IsMonicArrow<Identity<int>>);
}

TEST_CASE(
    "category:factorisation — CanonicalSetCCC is exact, regular, and admits "
    "the (regular epi, mono) factorisation system",
    "[category][factorisation][set][upgrade-chain]") {
  /** @brief @b Set (CanonicalSetCCC<A>) is the exemplar exact category
   *         (Borceux vol 2 §2; "in Set and any topos, every epimorphism
   *         is the coequalizer of its kernel pair").  The upgrade chain
   *         @c IsExactCategory ⇒ @c IsRegularCategory ⇒
   *         @c IsFactorisationSystem fires from the single
   *         @c is_exact_category_v opt-in registered in @c :image. */
  STATIC_CHECK(IsExactCategory<CanonicalSetCCC<int>>);
  STATIC_CHECK(IsRegularCategory<CanonicalSetCCC<int>>);
  STATIC_CHECK(IsFactorisationSystem<CanonicalSetCCC<int>>);
}

TEST_CASE(
    "category:factorisation — negative gate: int is not category-shaped, so "
    "the category-level concepts honestly reject",
    "[category][factorisation][negative][category-shape-gate]") {
  /** @brief A non-category-shaped type (@c int has no @c ::Arrow /
   *         @c ::Species / @c ::Id aliases) cannot satisfy any of the
   *         category-level concepts, even if the variable trait were
   *         force-registered.  The @c IsSmallCategoryShape gate added
   *         in Slice 1's Copilot pass enforces this. */
  STATIC_CHECK_FALSE(IsRegularCategory<int>);
  STATIC_CHECK_FALSE(IsExactCategory<int>);
  STATIC_CHECK_FALSE(IsFactorisationSystem<int>);
}

TEST_CASE(
    "image:image_of — factory produces a Subobject of Cod<F>",
    "[category][image][image-of][factory]") {
  /** @brief The @c image_of(f) factory constructs a @c Subobject of
   *         @c Cod<F> from any @c IsArrow @c F.  This runtime test
   *         exercises the factory body (construction of
   *         @c Subobject<Cod<F>, ImageChi<F>>) and the
   *         @c ImageChi::operator() Honest-Rejection default
   *         (@c Ternary::Unknown).  Concrete carrier-specific
   *         specialisations of @c ImageChi<F> land in #718 Slice 4
   *         (First-Iso-Theorem). */
  constexpr Identity<int> id_int{};
  constexpr auto image = image_of(id_int);

  // Type-level: the result is a Subobject of int — the trivial
  // image-of-identity exhibit (the whole carrier).
  STATIC_CHECK(IsSubobject<decltype(image), int>);

  // Runtime: the default classifier returns Ternary::Unknown (Honest
  // Rejection until a concrete specialisation overrides).
  CHECK(image(42) == Ternary::Unknown);
  CHECK(image(0) == Ternary::Unknown);
  CHECK(image(-1) == Ternary::Unknown);
}
