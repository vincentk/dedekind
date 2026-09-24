/**
 * @file dedekind/category/morphism.cppm
 * @partition :morphism
 * @brief The Skeletal Morphism (Structural Cement).
 *
 * @section morphism__The_Structuralist_Framework
 * This partition defines the categorical primitives that govern the interaction
 * between species. In the Dedekind ontology, we do not treat functions as
 * ephemeral blocks of logic, but as formal "Arrows" that carry their own
 * metadata (Domain and Codomain).
 *
 * @details Implements type-level labelling to associate categorical metadata
 * (like domain and codomain species) with C++ types. These labels exist
 * strictly at compile-time via traits or nested aliases, allowing primitive
 * and standard library types to be embedded directly into categorical
 * structures with zero runtime overhead or invasive wrapping.
 *
 * @quote
 * "A morphism is not just a function, but a function *together with* its
 *  domain and codomain... This 'bookkeeping' is exactly what allows the
 *  categorical machinery to work its magic."
 *  — Urs Schreiber, n-Category Café
 *
 * @subsection The_Skeletal_Arrow
 * The primary purpose of this partition is to provide the "Box and Label"
 * for any mapping:
 * - @ref Morphism (Arrow) : A labeled struct f: A -> B.
 * - @ref IsArrow         : The skeletal concept verifying Domain/Codomain
 * labels.
 * - @ref Identity (id)   : The "Zero-Length Highway" (Reflexivity).
 * - @ref operator>>      : Categorical composition (The Path Axiom).
 *
 * @section morphism__The_Action_Axiom
 * To bypass C++ template limitations regarding ADL, morphisms are treated as
 * "Actions." This allows us to lift total functions into the monadic Kleisli
 * spine without losing the formal species-integrity of the Domain.
 *
 * @note Universal constructions (Initial, Terminal, Zero Morphism) are
 *       deferred to Level 0.5 (:limit) and Level 1 (:total), as they
 *       rely on specific algebraic properties of the species.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

module;

#include <concepts>
#include <functional>
#include <optional>
#include <utility>  // std::pair (relation-reading witness, #952)

export module dedekind.category:morphism;

import :species;

namespace dedekind::category {

// ---------------------------------------------------------------------------
// Morphisms vs functors in a single-species universe (clarified under #525)
// ---------------------------------------------------------------------------
//
// This block reads in two registers — categorical (morphism / functor)
// and architectural (spoke / hub).  The categorical content drives the
// design; the architectural vocabulary is the C++23 realisation of it.
//
// @section Single_Species_Universe
//
// The dedekind category model is @b single-species by design.  A
// category @c 𝒞 in this codebase has @b objects drawn from a single
// C++ type — its @c Species — and @b morphisms drawn from a single
// C++ type — its @c Arrow.  In code this looks like
//
//   struct Set<T> {                   // The category of T-valued things.
//     using Species = T;              //   Objects: values of type T.
//     using Arrow = Morphism<T, T, std::function<T(T)>>;  //   Morphisms.
//     using Id = SetId<T>;
//     ...
//   };
//
// This restriction is pragmatic, not principled.  In the textbook
// presentation a category's collection of objects is a class whose
// elements may belong to different types; modeling that polymorphism
// in C++23 would require runtime type-erasure (existentials,
// type-erased object containers), which the project's System-F-flavoured
// admissibility rules (Table~\ref{tab:admissibility-rules} in
// @c paper.tex) deliberately exclude.  The single-species commitment
// is the price of the disciplined-template fragment.
//
// Concrete instances of this pattern in the codebase:
//   * @c Set<int> — the category whose objects are int values and
//     whose morphisms are functions @c int @c → @c int.
//   * @c Set<Maybe<int>> — a @b different category, same structural
//     shape, distinct species.
//   * @c Set<Path<int>> — a category whose objects are sequences
//     @c ℕ @c → @c int.  Path<int> is an OBJECT in this category,
//     even though Path is itself an arrow at the meta-level (every
//     sequence IS a function ℕ → T) — exactly the CCC reading where
//     function spaces are first-class objects in @b Set.
//
// @section Morphisms_within_a_Category
//
// A morphism in @c 𝒞 is an arrow @c f: @c a → @c b where @c a, @c b
// are objects of @c 𝒞 (i.e., @c 𝒞.Species values).  Composition of
// morphisms is internal: given @c f: @c a → @c b and @c g: @c b → @c c
// in @c 𝒞, the composite @c g @c ∘ @c f: @c a → @c c is also a
// morphism in @c 𝒞.  This is the closure axiom: a category is closed
// under composition of its own morphisms.
//
// In code, @c 𝒞.Arrow instances ARE the morphisms.  In the dispatch
// vocabulary below they are called @b spoke @b arrows, because in the
// hub/spoke architecture they are arrows that connect ordinary
// (object-level) endpoints — the spokes of the wheel, with the
// category itself as the hub.  The fish @c f @c >> @c g (sugar for
// @c g @c ∘ @c f) is the operator surface for spoke composition.
//
// @section Functors_between_Categories
//
// A functor @c F: @c 𝒞 → @c 𝒟 is, mathematically, a structure-
// preserving map between two categories.  It has two parts:
//
//   * @b F_obj: an action on objects, taking @c 𝒞.Species @c → @c 𝒟.Species.
//     In code this is @c F::template @c Shape<U> (the type-constructor
//     part: @c maybe_functor<T>::Shape<U> @c = @c Maybe<U>, etc.).
//   * @b F_mor: an action on morphisms, taking @c 𝒞.Arrow @c → @c 𝒟.Arrow,
//     with @c F(g @c ∘ @c f) @c = @c F(g) @c ∘ @c F(f) and
//     @c F(id_a) @c = @c id_{F(a)}.  In code this is @c F.φ(f) (the
//     morphic-lift method on the functor Hub).
//
// Because functors are themselves arrows — they are the arrows of
// @b Cat, the category of categories — they have their own
// composition: given @c F: @c 𝒞 → @c 𝒟 and @c G: @c 𝒟 → @c ℰ, the
// composite @c G @c ∘ @c F: @c 𝒞 → @c ℰ is a functor (a morphism in
// @b Cat).  This is a @b different operation than morphism
// composition within a single category — different category, different
// composition law, different concrete realisation.
//
// In the architecture vocabulary below, functors are called @b hub
// @b arrows: their endpoints are categories (the source / target
// labels @c Σ_cat / @c Τ_cat that hub types like @c maybe_functor
// expose), so they sit "above" the spoke arrows that connect objects
// within a single category.  The fish @c F @c >> @c G (sugar for
// @c G @c ∘ @c F) at the hub level is functor composition; that
// operator is owned by @c :functor, not by this partition.
//
// @section Three_Composition_Shapes
//
// Together these give @b three distinct operator-@c >> shapes the
// codebase uses:
//
//   1. @b Morphism @c >> @b Morphism (spoke @c >> spoke) —
//      composition of arrows within a category.  Owned by this
//      partition (@c :morphism, line ~565).  Requires
//      @c IsSpokeArrow on both sides + matching middle types.
//   2. @b Functor @c >> @b Functor (hub @c >> hub) — composition in
//      @b Cat.  Owned by @c :functor (the @c composite_functor
//      machinery).  Requires @c IsShapedFunctor on both sides +
//      @c F::Τ_cat @c == @c G::Σ_cat (so the source/target categories
//      of the chain match up).
//   3. @b Functor @c >> @b Morphism (hub @c >> spoke) — @b not
//      composition; instead a syntactic-sugar reuse of @c >> for the
//      @b functorial @b action on arrows (i.e.\ @c F.φ(f) /
//      @c fmap @c F @c f).  Owned by @c :functor.  Categorically,
//      this is the F_mor part of the functor applied to an arrow in
//      the source category, returning an arrow in the target
//      category.  It is not "composition" in the same sense as (1)
//      and (2); the operator-symbol overlap is for ergonomics in the
//      Haskell idiom (@c functor @c <$> @c arrow), not for
//      categorical uniformity.  Reader beware: @c F @c >> @c f does
//      @b not mean "compose F with f as morphisms in the same
//      category" — that would be ill-typed, since F's codomain is a
//      category and f's domain is an object.
//
// The fourth combination, @b Morphism @c >> @b Functor (spoke @c >>
// hub), has no categorical reading and no overload exists.  Such an
// expression fails to compile.
//
// @section Discriminator (#525)
//
// The split between (1) and (2)/(3) at dispatch time hinges on a
// single question: @b is @b the @b Domain @b a @b category?
//
//   * If yes — Domain has @c ::Arrow / @c ::Species / @c ::Id aliases
//     identifying it as a category — the arrow is a functor
//     (between categories): a hub.
//   * If no — Domain is an object — the arrow is a morphism (within a
//     category): a spoke.
//
// The codebase realises this question structurally as
// @c IsSmallCategoryShape<Dom<T>>, defined further down.  @c
// IsSmallCategoryShape is the structural prefix of the full @c IsSmallCategory
// in @c :small — here we only need the shape to make the routing decision; the
// behavioural axioms (composition closure, identity laws) are not
// needed at this layer.
//
// Pre-#525 the discriminator used @c !IsArrow<Dom<T>> as a structural
// proxy for "Domain isn't a category".  This worked coincidentally
// for functor-Hubs (whose Domain is a category and therefore has
// @c Cat::Arrow, satisfying @c IsArrow) but over-fired on object-level
// types that happen to expose @c Domain / @c Codomain aliases for
// unrelated reasons.  The canonical example: @c Path<T> declares
// @c Domain @c = @c std::size_t to model the textbook reading "a
// sequence is a function @c ℕ @c → @c T", which makes
// @c IsArrow<Path<T>> trivially true even though @c Path<T> is an
// @b object in @b Set, not a category.  The post-#525 form
// @c !IsSmallCategoryShape<Dom<T>> asks the right question and the
// over-fire stops.
//
// @section Manual_Override
//
// In addition to the structural test, types may opt in / out manually
// via @c using @c ArrowKind @c = @c spoke_arrow_tag; or
// @c hub_arrow_tag; .  Today the @c hub_arrow_tag opt-out is the
// load-bearing override (functor Hubs use it to forcibly exclude
// themselves from spoke routing); @c spoke_arrow_tag is a reserved
// spoke-marker for parallel future opt-in cases (a type that wants
// to participate in spoke composition despite an unusual Domain
// shape, etc.).
// ---------------------------------------------------------------------------

/**
 * @brief Marker tag reserved for ordinary object-level arrows (spoke arrows).
 *
 * @details Currently @b reserved: @c IsSpokeArrow does not check for
 * @c ArrowKind @c = @c spoke_arrow_tag as an opt-in.  The structural
 * discriminator @c !IsSmallCategoryShape<Dom<T>> already catches the typical
 * spoke case automatically, so no opt-in is needed in practice.  This
 * tag is kept as a parallel slot to @c hub_arrow_tag so a future opt-in
 * mechanism (e.g., for a type whose Domain is unusually category-shaped
 * but should still route as a spoke) can be added symmetrically.
 *
 * See the architecture block above for the single-species design context.
 */
export struct spoke_arrow_tag {};

/**
 * @brief Marker tag for hub-level arrows (e.g., functor hubs).
 *
 * @details Attach as @c using @c ArrowKind @c = @c hub_arrow_tag; to
 * opt out of spoke-level composition overloads.  Hub arrows are arrows
 * @b between categories (functors, natural transformations, etc.); they
 * compose by their own rules in bridge partitions (@c :functor,
 * @c :natural).
 *
 * The opt-out keeps @c :morphism decoupled from downstream naming
 * conventions while still letting higher-level partitions route
 * overload resolution explicitly.  See the architecture block above
 * for the dispatch rationale.
 */
export struct hub_arrow_tag {};

/**
 * @concept IsArrow
 * @brief A type that knows its own Domain (A) and Codomain (B).
 */
export template <typename F>
concept IsArrow =
    requires {
      typename std::remove_cvref_t<F>::Domain;
      typename std::remove_cvref_t<F>::Codomain;
    } && requires(const std::remove_cvref_t<F>& f,
                  const typename std::remove_cvref_t<F>::Domain& x) {
      // CONST-invocable (not merely invocable on a mutable f): a morphism is
      // pure (Juliet Posture §2 "Pure, Terminating Arrows ... effect-free"), so
      // applying it must not mutate it.  A mutable-only operator() is a
      // stateful callable, not a morphism, and would break extensionality (a =
      // b ⟹ f(a) = f(b)); it is excluded here at the type boundary.  A memoized
      // arrow (mutable cache + const operator()) stays const-invocable and
      // observationally pure, so it survives.
      {
        f(x)
      } -> std::convertible_to<typename std::remove_cvref_t<F>::Codomain>;
    };

// ---------------------------------------------------------------------------
// Multi-reading arrow traits (#952 S0): a type may carry MORE THAN ONE arrow
// reading, disambiguated by a @c Shape tag.  The prototypical case is a
// function @c f: @c A → @c B that is @b also a relation / characteristic
// predicate on @c A × @c B (its graph).  This mirrors the
// @c SpeciesTraits<T, Args...> idiom (@c :species), not a foreign
// @c tag_invoke.
//
// The DEFAULT reading (@c default_arrow_tag) forwards to the existing
// @c ::Domain / @c ::Codomain nested typedefs, so @c Dom<F> / @c Cod<F> and
// every @c IsArrow site keep their exact pre-#952 meaning.  This pivot is
// strictly additive: a second reading is expressible without disturbing the
// default one.
// ---------------------------------------------------------------------------

/** @brief Default arrow reading: the nested-typedef map (A→B) reading. */
export struct default_arrow_tag {};

/** @brief The map reading of an arrow: @c f as a function @c A → @c B. */
export struct map_tag {};

/**
 * @brief The relation reading of an arrow: @c f as its graph /
 *        characteristic predicate on @c A × @c B.
 */
export struct relation_tag {};

/**
 * @brief Per-@c (T, @c Shape) arrow reading: the categorical Domain / Codomain
 *        that a type exposes under a given @c Shape tag.
 * @details Primary is undefined; register a reading by specialising on a
 *          concrete @c T and @c Shape.  The @c default_arrow_tag reading is
 *          supplied below by forwarding to @c T::Domain / @c T::Codomain.
 */
export template <typename T, typename Shape = default_arrow_tag>
struct arrow_traits;

/**
 * @brief Default reading: forward to the arrow's own @c ::Domain / @c
 *        ::Codomain typedefs.  This IS the pre-#952 meaning of @c Dom / @c Cod.
 */
template <typename T>
  requires requires {
    typename T::Domain;
    typename T::Codomain;
  }
struct arrow_traits<T, default_arrow_tag> {
  using Domain = typename T::Domain;
  using Codomain = typename T::Codomain;
};

/** @brief The Domain of @c F under reading @c Shape (default: map reading). */
export template <typename F, typename Shape = default_arrow_tag>
using DomFor = typename arrow_traits<std::remove_cvref_t<F>, Shape>::Domain;

/** @brief The Codomain of @c F under reading @c Shape (default: map reading).
 */
export template <typename F, typename Shape = default_arrow_tag>
using CodFor = typename arrow_traits<std::remove_cvref_t<F>, Shape>::Codomain;

/**
 * @concept HasArrowReading
 * @brief @c T exposes an arrow reading (Domain + Codomain) under @c Shape.
 * @details The query for whether a type carries a given reading; false (via
 *          the undefined primary) when no specialisation registers one.
 */
export template <typename T, typename Shape>
concept HasArrowReading = requires {
  typename arrow_traits<std::remove_cvref_t<T>, Shape>::Domain;
  typename arrow_traits<std::remove_cvref_t<T>, Shape>::Codomain;
};

// ---------------------------------------------------------------------------
// Picking policy for Domain-resolving helpers (closes #411).
//
// The codebase has @b three helpers for retrieving the underlying-element
// type of a species / arrow.  They are not strict duplicates — each
// serves a distinct audience.  Pick the right one for your call site:
//
//   * @c Dom<F> / @c Cod<F> — home: @c :morphism (here).
//     Use when the site is @c IsArrow-strict: functors, naturality,
//     hub-arrows; i.e. when the callable-with-Domain/Codomain shape
//     is required by the surrounding code.
//
//   * @c element_of_t<S> — home: @c :sets:boundaries.
//     Use when the site needs to accept @b primitive carriers
//     (@c bool, @c unsigned, …) as well as predicate-set carriers
//     (post-carrier-migration code paths; the "species or carrier"
//     generality).  This is the only helper with a primitive
//     fallback.
//
//   * @c MorphicBridge<signature_extractor<F>::type>::Domain —
//     home: @c :morphism (below).
//     Use in typed-lambda contexts where @c F doesn't expose
//     @c ::Domain directly.
//
// @b Bare @c typename @c T::Domain is acceptable @b only when:
//   * The site has its own @c requires @c { @c typename @c T::Domain; @c }
//     constraint that excludes primitives, OR
//   * The type is locally constructed (e.g. inside a producer site
//     like @c using @c Domain @c = @c T;).
//
// In post-carrier-migration code paths (post-#400 / #401 / #402 etc.)
// where a "species" parameter may be a primitive carrier, prefer
// @c element_of_t<S>; bare @c S::Domain there fails to compile on
// primitives and forces a cascade fix at every consumer site.  See
// PR #409 / @c :order:halfspace for the precedent where the cascade
// was caught.
// ---------------------------------------------------------------------------

/**
 * @brief Shorthand to look up the Domain of an Arrow type F.
 *
 * @details Picking-policy slot 1 of 3 — see the policy comment above.
 *          Use this in @c IsArrow-strict contexts; for primitive-aware
 *          generality use @c element_of_t<S> from @c :sets:boundaries.
 */
export template <IsArrow F>
using Dom = DomFor<F, default_arrow_tag>;

/**
 * @brief Shorthand to look up the Codomain of an Arrow type F.
 *
 * @details Codomain analogue of @c Dom; same picking-policy slot.
 */
export template <IsArrow F>
using Cod = CodFor<F, default_arrow_tag>;

// Multi-reading witness (#952 S0): ONE type carries TWO non-ambiguous arrow
// readings.  @c DemoFn is a function A→B (default / map reading via its own
// typedefs); the @c relation_tag specialisation below registers its graph
// reading on A×B.  This is the maintainer's function/relation driving case in
// miniature.  The real @c Set<pair> relation / @c Graph<F> types live in
// @c :relational --- downstream of @c :morphism (so unreachable here without a
// cycle) and off the same-layer test-import DAG --- so the concrete duality is
// pinned here on an in-partition function type; registering the downstream
// readings is S1.
namespace arrow_traits_multishape_witness {
struct DemoFn {
  using Domain = int;
  using Codomain = bool;
  constexpr bool operator()(int x) const { return x > 0; }
};
}  // namespace arrow_traits_multishape_witness

// The relation reading of @c DemoFn: its graph as a predicate on int × bool.
template <>
struct arrow_traits<arrow_traits_multishape_witness::DemoFn, relation_tag> {
  using Domain = std::pair<int, bool>;
  using Codomain = bool;  // Ω surrogate (truth object)
};

namespace arrow_traits_multishape_witness {
// The default (untagged) reading is UNCHANGED: still the map reading A→B.
static_assert(std::same_as<Dom<DemoFn>, int>,
              "untagged Dom must remain the map reading (Domain = int).");
static_assert(std::same_as<Cod<DemoFn>, bool>,
              "untagged Cod must remain the map reading (Codomain = bool).");
static_assert(std::same_as<DomFor<DemoFn, default_arrow_tag>, int>,
              "explicit default reading agrees with untagged Dom.");
// The SECOND reading now coexists, unambiguously, under relation_tag.
static_assert(HasArrowReading<DemoFn, relation_tag>,
              "DemoFn must carry a relation reading in addition to its map "
              "reading.");
static_assert(std::same_as<DomFor<DemoFn, relation_tag>, std::pair<int, bool>>,
              "the relation reading's Domain is the product A×B.");
static_assert(std::same_as<CodFor<DemoFn, relation_tag>, bool>,
              "the relation reading's Codomain is the truth object.");
// Discrimination: the two readings are DISTINCT, so the arrow is no longer
// ambiguous --- each reading is addressed by its own tag.
static_assert(!std::same_as<Dom<DemoFn>, DomFor<DemoFn, relation_tag>>,
              "the default and relation readings are distinct (disambiguated "
              "by tag, not merged).");
static_assert(HasArrowReading<DemoFn, default_arrow_tag>,
              "DemoFn carries the default reading too.");
// Negative: a plain type with no arrow typedefs exposes NO default reading,
// and an untagged query never conjures a relation reading out of nothing.
static_assert(!HasArrowReading<int, default_arrow_tag>,
              "a non-arrow type exposes no default reading.");
static_assert(!HasArrowReading<int, relation_tag>,
              "an unregistered (type, relation_tag) pair has no reading.");
}  // namespace arrow_traits_multishape_witness

/**
 * @concept IsSmallCategoryShape
 * @brief Structural prefix of @c dedekind::category::IsSmallCategory: a type
 *        carrying the identifying category-shaped aliases @c ::Arrow,
 *        @c ::Species, @c ::Id.
 *
 * @details The full @c IsSmallCategory in @c :small refines this with
 * behavioural checks (@c id_c factory, @c f @c >> @c g internal
 * closure, etc.).  At this layer we only need the structural test to
 * answer the question "is this thing a @b category" for purposes of
 * hub/spoke discrimination.  Full @c IsSmallCategory cannot be referenced
 * here because @c :small imports @c :morphism (cycle), and because
 * @c IsSmallCategory recursively uses @c f @c >> @c g which is the very
 * operator we are gating.  The structural prefix is the load-bearing
 * piece for routing — it identifies the @b argument-shape of a
 * functor (a thing taking a category, not an object), which is the
 * single distinction the hub/spoke split needs to make (#525).
 */
export template <typename T>
concept IsSmallCategoryShape = requires {
  typename std::remove_cvref_t<T>::Arrow;
  typename std::remove_cvref_t<T>::Species;
  typename std::remove_cvref_t<T>::Id;
};

/**
 * @concept IsHubArrow
 * @brief Higher-order arrow whose Domain/Codomain are categories
 *        (functors and similar arrows-between-categories; #525).
 * @details In the hub/spoke vocabulary, a "hub" arrow acts on
 *          categories as its objects: it is an arrow @b between
 *          categories, not within one.  Functors are the canonical
 *          example.  The @c IsSmallCategoryShape prefix detects the
 *          identifying @c ::Arrow / @c ::Species / @c ::Id alias
 *          surface; the prior structural-proxy form
 *          (@c IsArrow<Dom<T>>) over-fired on object-level types
 *          that happen to expose @c Domain / @c Codomain aliases
 *          for unrelated reasons (e.g., @c Path<T> = sequence
 *          @c ℕ → @c T at the type level).
 */
export template <typename T>
concept IsHubArrow =
    IsArrow<T> && IsSmallCategoryShape<Dom<T>> && IsSmallCategoryShape<Cod<T>>;

/**
 * @concept IsSpokeArrow
 * @brief Object-level arrow whose Domain/Codomain are @b not
 *        categories (#525 sharpening).
 * @details A "spoke" arrow connects ordinary species-level objects
 *          @b within a category.  The discriminator is "Domain is
 *          not a category-shaped thing" (@c !IsSmallCategoryShape<Dom<T>>);
 *          this is the formal expression of "this isn't a functor".
 *          Generic categorical composition in this partition is
 *          intentionally limited to spoke arrows so higher-order
 *          composition (functor @c ∘ functor in @b Cat) can be
 *          owned by the appropriate bridge partitions.
 *
 * @section morphism__Spoke_Discriminator_Sharpening
 * Pre-#525 the discriminator read @c !IsArrow<Dom<T>> — a structural
 * proxy that worked coincidentally for functor-Hubs (whose Domain is
 * a @c IsSmallCategory<C> witness, which trivially satisfies @c IsArrow
 * via @c Cat::Arrow), but over-fired on object-level carriers that
 * happen to expose @c Domain / @c Codomain aliases for unrelated
 * reasons.  The canonical example is @c Path<T>, which models the
 * textbook reading "a sequence is a function @c ℕ @c → @c T" by
 * declaring @c Domain @c = @c std::size_t and a call operator —
 * making @c IsArrow<Path<T>> @b true at the type level even though
 * @c Path<T> is plainly an @b object (not a category-to-category
 * arrow).  The sharpened discriminator @c !IsSmallCategoryShape<Dom<T>>
 * stops the over-fire while preserving the original intent: hub
 * arrows are arrows between categories.
 *
 * If a type advertises @c using @c ArrowKind @c = @c hub_arrow_tag;
 * it is still excluded from @c IsSpokeArrow as a manual override.
 */
export template <typename T>
concept IsSpokeArrow =
    IsArrow<T> && !IsSmallCategoryShape<Dom<T>> &&
    !IsSmallCategoryShape<Cod<T>> && !requires {
      requires std::same_as<typename std::remove_cvref_t<T>::ArrowKind,
                            hub_arrow_tag>;
    };

/**
 * @section morphism__The_Skeletal_Morphism
 * @brief The formal structure of an Arrow f: A -> B.
 */
export template <typename A, typename B, typename Func>
struct Morphism {
  using Domain = A;  // The concept is looking for these exact names
  using Codomain = B;
  Func transform;

  constexpr explicit Morphism(Func f) : transform(std::move(f)) {}

  // We provide the call operator, but NOT the composition (f ∘ g).
  // Constrained on CONST-invocability of the stored callable: the body invokes
  // `transform` through `const` (const method → const Func&).  Without this the
  // operator's DECLARATION is unconditional, so IsArrow's `f(x)` probe (which
  // validates the signature, not the body) would accept a Morphism wrapping a
  // mutable-only callable — then fail only when the body is instantiated.  The
  // constraint makes the operator non-viable for a mutable-only Func, so such a
  // Morphism is honestly !IsArrow at the boundary (#822).
  constexpr Codomain operator()(const Domain& x) const
    requires std::invocable<const Func&, const Domain&>
  {
    return transform(x);
  }
};

// Regression (#822): a Morphism wrapping a MUTABLE-ONLY callable is NOT an
// arrow.  A morphism is pure (Juliet Posture §2); the const-invocability
// constraint on operator() above makes IsArrow reject it at the type boundary,
// rather than admitting it and hard-erroring only when the body is called.
namespace morphism_const_invocability_witness {
struct MutableOnlyRule {
  int state = 0;
  constexpr int operator()(int x) { return state += x; }  // non-const: stateful
};
struct PureRule {
  constexpr int operator()(int x) const { return x + 1; }
};
static_assert(!IsArrow<Morphism<int, int, MutableOnlyRule>>,
              "a Morphism over a mutable-only callable is not a (pure) arrow.");
static_assert(IsArrow<Morphism<int, int, PureRule>>,
              "a Morphism over a const-invocable callable is an arrow.");

// DIRECT witness for the IsArrow probe itself (#822), NOT routed through
// Morphism's operator() constraint: a raw arrow-shaped type (Domain/Codomain +
// a NON-CONST operator()) must be !IsArrow.  This is the assertion that would
// FAIL if the probe were reverted to a mutable `f` — the Morphism assertions
// above would not, since Morphism::operator() rejects the mutable rule on its
// own.  So this pins the concept's const-invocability directly.
struct MutableOnlyArrow {
  using Domain = int;
  using Codomain = int;
  int state = 0;
  constexpr int operator()(int x) { return state += x; }  // non-const: stateful
};
static_assert(!IsArrow<MutableOnlyArrow>,
              "an arrow-shaped type with only a non-const operator() is not an "
              "arrow (IsArrow requires const-invocability).");
struct PureArrow {
  using Domain = int;
  using Codomain = int;
  constexpr int operator()(int x) const { return x + 1; }
};
static_assert(IsArrow<PureArrow>,
              "an arrow-shaped type with a const operator() is an arrow.");
}  // namespace morphism_const_invocability_witness

/** @brief Universal inference for any Morphism signature f: Args... -> Codomain
 */
template <typename F, typename... Args>
struct infer_morphism {
  // For binary operators, Domain is the first argument type
  using Domain = std::tuple_element_t<0, std::tuple<Args...>>;
  using Codomain = std::invoke_result_t<F, Args...>;
};

// 1. Unary Discovery (e.g. std::identity)
template <typename F, typename A>
  requires requires(F f, A x) { f(x); }
struct SpeciesTraits<F, A> : infer_morphism<F, A> {};

// 2. Binary Discovery (e.g. std::plus)
template <typename F, typename A>
  requires requires(F f, A x) { f(x, x); }
struct SpeciesTraits<F, A> : infer_morphism<F, A, A> {};

/** @section morphism__Logical_Species (bool) */
template <>
struct SpeciesTraits<std::logical_and<bool>>
    : infer_morphism<std::logical_and<bool>, bool, bool> {};
template <>
struct SpeciesTraits<std::logical_or<bool>>
    : infer_morphism<std::logical_or<bool>, bool, bool> {};
template <>
struct SpeciesTraits<std::equal_to<bool>>
    : infer_morphism<std::equal_to<bool>, bool, bool> {};

/** @section morphism__Integral_Species (uint/int) */
template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::plus<T>> : infer_morphism<std::plus<T>, T, T> {};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::minus<T>> : infer_morphism<std::minus<T>, T, T> {};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::multiplies<T>>
    : infer_morphism<std::multiplies<T>, T, T> {};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::divides<T>> : infer_morphism<std::divides<T>, T, T> {
};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::bit_and<T>> : infer_morphism<std::bit_and<T>, T, T> {
};

template <typename T>
  requires std::integral<T>
struct SpeciesTraits<std::bit_or<T>> : infer_morphism<std::bit_or<T>, T, T> {};

/** @section morphism__Relational_Morphisms (Subobject Classifiers) */
template <typename T>
struct SpeciesTraits<std::less_equal<T>>
    : infer_morphism<std::less_equal<T>, T, T> {};

template <typename T>
struct SpeciesTraits<std::less<T>> : infer_morphism<std::less<T>, T, T> {};

/** @section morphism__Lambda_Introspection
 *
 * @c MorphicBridge synthesises @c Domain / @c Codomain for callable
 * types that don't expose those nested aliases directly (lambdas,
 * function pointers, @c std::function instances).  This is picking-
 * policy slot 3 of 3 — see the policy comment near @c Dom / @c Cod
 * above.  Use @c MorphicBridge<signature_extractor<F>::type>::Domain
 * for typed-lambda contexts; for @c IsArrow-strict contexts use
 * @c Dom<F>; for primitive-aware sites use @c element_of_t<S> from
 * @c :sets:boundaries.
 */
template <typename T>
struct MorphicBridge;

// If the type is already a verified Arrow, just extract its labels.
template <IsArrow T>
struct MorphicBridge<T> {
  using Domain = typename T::Domain;
  using Codomain = typename T::Codomain;
};

// Pattern match for: ReturnType Class::operator()(ArgType) const
template <typename C, typename R, typename A>
struct MorphicBridge<R (C::*)(A) const> {
  using Domain = std::decay_t<A>;
  using Codomain = std::decay_t<R>;
};

// Pattern match for: ReturnType Class::operator()(ArgType)
template <typename C, typename R, typename A>
struct MorphicBridge<R (C::*)(A)> {
  using Domain = std::decay_t<A>;
  using Codomain = std::decay_t<R>;
};

/** @section morphism__Library_Function_Support */
template <typename R, typename A>
struct MorphicBridge<std::function<R(A)>> {
  using Domain = std::decay_t<A>;
  using Codomain = std::decay_t<R>;
};

/** @section morphism__Morphic_Resolution */

template <typename F>
struct signature_extractor {
  // Priority 1: Formal Arrows (Morphism, Identity, ZeroMorphism)
  template <IsArrow U>
  static auto resolve(int) -> U;

  // Priority 2: Lambdas and Functors (Internal operator())
  template <typename U>
  static auto resolve(int) -> decltype(&std::remove_cvref_t<U>::operator());

  // Priority 3: Function Pointers / std::function
  template <typename U>
  static auto resolve(...) -> std::remove_cvref_t<U>;

  using type = decltype(resolve<F>(0));
};

// 2. Define aliases for the Domain and Codomain extraction
template <typename F>
using domain_t =
    typename MorphicBridge<typename signature_extractor<F>::type>::Domain;

template <typename F>
using codomain_t =
    typename MorphicBridge<typename signature_extractor<F>::type>::Codomain;

/** @section morphism__CTAD_Guide */
template <typename F>
Morphism(F) -> Morphism<domain_t<F>, codomain_t<F>, F>;

static_assert(
    IsArrow<Morphism<int, bool, std::function<bool(int)>>>,
    "Taxonomy Error: Morphism must satisfy the skeletal IsArrow concept.");

/** @section morphism__Morphic_Factory */

// 1. Overload for raw Lambdas / Functors
// Restricted: Only if it's NOT already an Arrow but IS callable.
export template <typename F>
  requires(!IsArrow<std::remove_cvref_t<F>>) &&
          requires(F f) { typename signature_extractor<F>::type; }
constexpr auto arrow(F&& f) {
  using D = domain_t<F>;
  using C = codomain_t<F>;
  return Morphism<D, C, std::decay_t<F>>(std::forward<F>(f));
}

// 1. Explicit Arrow: arrow<A, B>(f)
export template <typename A, typename B, typename F>
constexpr auto arrow(F&& f) {
  return Morphism<A, B, std::decay_t<F>>(std::forward<F>(f));
}

// 2. Passthrough for things that are already Arrows (Idempotent factory)
template <IsArrow F>
constexpr auto arrow(F&& f) {
  return std::forward<F>(f);
}

// 3. Semi-Automatic: Explicit Domain, Inferred Codomain
// This is what classify<A>(f) needs to function.
export template <typename A, typename F>
  requires(!IsArrow<F> && std::invocable<std::decay_t<F>, const A&>)
constexpr auto arrow(F&& f) {
  using B = std::invoke_result_t<std::decay_t<F>, const A&>;
  return Morphism<A, B, std::decay_t<F>>(std::forward<F>(f));
}

// 2. Explicit Endomorphism: endo<A>(f)
export template <typename A, typename F>
constexpr auto endo(F&& f) {
  return arrow<A, A>(std::forward<F>(f));
}

// 3. Verify "Lambdas" (Functional Auto-Discovery)
auto logic_gate = arrow([](int x) -> bool { return x > 0; });

static_assert(
    IsArrow<decltype(logic_gate)>,
    "Inference Error: Lambda was not automatically discovered as an Arrow.");

// 4. Verify "The Universal Bridge" (Primitives as Identity Arrows)
// Allows using a raw '5' as an identity mapping in algebraic expressions
static_assert(
    IsArrow<decltype(arrow([](int x) { return x; }))>,
    "Bridge Error: Primitive int should act as an identity Arrow for itself.");
static_assert(IsArrow<decltype(arrow([](double x) { return x; }))>,
              "Bridge Error: Primitive double should act as an identity Arrow "
              "for itself.");

// 5. Verify "Morphism Struct" (Reification)
using IntToBool = Morphism<int, bool, std::function<bool(int)>>;
static_assert(IsArrow<IntToBool>,
              "Taxonomy Error: Formal Morphism struct failed IsArrow check.");

/**
 * @concept IsEndomorphism
 * @brief Proposition: A Morphism where Domain ≡ Codomain (f: A -> A).
 */
export template <typename T>
concept IsEndomorphism = IsArrow<T> && std::same_as<domain_t<T>, codomain_t<T>>;

/**
 * @struct Rule
 * @brief A type-erased Morphism A -> B between Species.
 * @details This wrapper reifies a functional rule into a formal
 *          Categorical Arrow, preventing C-style pointer decay.
 *
 * @tparam A The Domain Species (The Source).
 * @tparam B The Codomain Species (The Target).
 */
export template <IsSpecies A, IsSpecies B>
struct Rule {
  using Domain = A;
  using Codomain = B;

  // We use std::function to erase the lambda's unique type
  // and provide a stable "Morphic Identity."
  std::function<B(const A&)> apply;

  /** @brief The Morphic Application (The Arrow's Action) */
  constexpr B operator()(const A& x) const { return apply(x); }
};

/** @section morphism__Rule_Overloads */
// Specifically target Rule to kill the 'bool' ambiguity and fix deduction.
export template <typename A, typename B>
auto operator&&(const Rule<A, B>& p1, const Rule<A, B>& p2) {
  return Rule<A, B>{[p1, p2](const A& x) {
    using S = typename SpeciesTraits<B>::species;
    return B{S::AND(p1(x).value, p2(x).value)};
  }};
}

export template <typename A, typename B>
auto operator||(const Rule<A, B>& p1, const Rule<A, B>& p2) {
  return Rule<A, B>{[p1, p2](const A& x) {
    using S = typename SpeciesTraits<B>::species;
    return B{S::OR(p1(x).value, p2(x).value)};
  }};
}

/** @section morphism__Arrow Factory Verification: Tagging and Species
 * Integrity. */

// 4. Action Proof: The tagged arrow preserves the underlying action.
// We verify that the factory-produced morphism actually executes.
static_assert(endo<int>([](int x) { return x * 2; })(21) == 42,
              "Arrow Factory: Action check failed for anonymous lambda.");

/** @section morphism__The_Universal_Functor_Interface */

/**
 * @brief The Identity Morphism and Functorial Witness.
 *
 * In the Category of Species (Set), Identity acts as the unit morphism \(id_T:
 * T \to T\). In the Category of Functors, it acts as the Identity Functor \(Id:
 * \mathcal{C} \to \mathcal{C}\).
 *
 * This reification is "Dual-Action": it preserves the structure of both
 * individual elements (Species) and their transformations (Morphisms). It is
 * the canonical "Zero-Length Highway" that connects a species to itself without
 * distortion.
 *
 * @tparam T The Species (Object) for which this identity is defined.
 */
export template <typename T>
struct Identity final {
  /** @brief The source category species. */
  using Domain = T;
  /** @brief The target category species (invariant for identity). */
  using Codomain = T;

  /**
   * @section morphism__Categorical_Actions
   * The following operators ensure that Identity acts as a structure-preserving
   * map across different levels of the categorical hierarchy.
   */

  /**
   * @brief The Morphic Action (Object -> Object).
   *
   * Implements the identity function \(f(x) = x\). This satisfies the
   * "Identity on Set" requirement for Level 0 categorical grounding.
   *
   * @param x The species value to be mapped.
   * @return The identical species value.
   */
  constexpr T operator()(const T& x) const noexcept { return x; }

  /**
   * @brief The Functorial Action (Morphism -> Morphism).
   *
   * Implements the Identity Functor lift \(F(f) = f\). This satisfies the
   * Functorial Identity Law: mapping an arrow through the identity context
   * must result in the original arrow.
   *
   * @tparam Arrow A type satisfying the IsArrow concept.
   * @param f The arrow to be lifted into the identity context.
   * @return The original arrow, preserved without transformation.
   */
  template <IsArrow Arrow>
    requires std::same_as<typename Arrow::Domain, T>
  constexpr auto operator()(Arrow f) const noexcept {
    return f;
  }

  /**
   * @note Any type passed to this identity that is neither the Species @p T
   * nor a valid @p IsArrow will result in a substitution failure (SFINAE),
   * preventing "loose" or non-categorical sliding.
   */
};

/** @brief The Identity Factory: Returns the neutral arrow for Species A. */
export template <typename A>
constexpr auto id() {
  return Identity<A>{};
}

/** @section morphism__Morphism_Lifting_Proof */
using Negate = std::negate<int>;
using TaggedNegate = Morphism<int, int, Negate>;

constexpr auto f_neg = endo<int>(std::negate<int>{});
constexpr auto identity_int = id<int>();

/** @section morphism__Identity Verification */
static_assert(IsArrow<decltype(id<bool>())>,
              "The id<A>() factory must produce a valid Arrow.");

// 1. Right Identity: f(id(x)) == f(x)
static_assert(f_neg(identity_int(42)) == f_neg(42),
              "Unit Law: f ∘ id_A must equal f.");

// 2. Left Identity: id(f(x)) == f(x)
static_assert(identity_int(f_neg(42)) == f_neg(42),
              "Unit Law: id_B ∘ f must equal f.");

/** @section morphism__Lifting Traits to the Identity Functor */

// 1. If T is associative under Op, Identity<T> is associative.
template <typename T, typename Op>
inline constexpr bool is_associative_v<Identity<T>, Op> =
    is_associative_v<T, Op>;

// 2. If T is commutative under Op, Identity<T> is commutative.
template <typename T, typename Op>
inline constexpr bool is_commutative_v<Identity<T>, Op> =
    is_commutative_v<T, Op>;

/**
 * @section morphism__Lifting_2
 * The identity element of the Identity Morphism under composition
 * is the Identity Morphism itself (id ∘ id = id).
 */
template <typename T, typename Op>
inline constexpr Identity<T> identity_v<Identity<T>, Op> = Identity<T>{};

/**
 * @section morphism__Categorical
 * @brief Synthesizes an arrow A -> C from A -> B and B -> C.
 * @details By requiring A, B, and C as template parameters, we ensure
 *          the composition is a statically verified bridge.
 *          This overload is the spoke-level path constructor: it composes
 *          object-level arrows only. Hub-level arrows such as functors are
 *          composed in their own partitions.
 */
export template <typename F, typename G>
  requires IsSpokeArrow<std::decay_t<F>> && IsSpokeArrow<std::decay_t<G>> &&
           std::same_as<typename std::decay_t<F>::Codomain,
                        typename std::decay_t<G>::Domain>
constexpr auto operator>>(F&& f, G&& g) {
  using F_pure = std::decay_t<F>;
  using G_pure = std::decay_t<G>;

  using A = typename F_pure::Domain;
  using C = typename G_pure::Codomain;

  // Note: The lambda is implicitly constexpr in C++23 if possible
  return arrow<A, C>([f = std::forward<F>(f), g = std::forward<G>(g)](
                         A x) constexpr { return g(f(std::move(x))); });
}

/**
 * @brief @c operator>> with a callable on the RHS — auto-wraps the lambda
 *        as @c arrow<F::Codomain> .
 *
 * @details Convenience overload so call sites can chain a typed Arrow with
 * an inline lambda without per-site @c arrow<B>(λ) ceremony.  The lambda's
 * Domain is deduced from the LHS Arrow's Codomain; its Codomain is deduced
 * from the lambda's invoke result.  Equivalent to
 * @c std::forward<F>(f) @c >> @c arrow<B>(std::forward<G>(g)) where
 * @c B @c = @c F::Codomain — but spelled inline.
 *
 * @note Symmetric (lambda @c >> @c Arrow) adapter is intentionally NOT
 * shipped: a bare lambda LHS has no fixed Domain, and inferring it from
 * the RHS's Domain is fragile when the lambda is polymorphic.  Wrap the
 * lambda explicitly via @c arrow<A>(λ) for that direction.
 */
export template <typename F, typename G>
  requires IsSpokeArrow<std::decay_t<F>> && (!IsArrow<std::decay_t<G>>) &&
           std::invocable<std::decay_t<G>&,
                          const typename std::decay_t<F>::Codomain&>
constexpr auto operator>>(F&& f, G&& g) {
  using B = typename std::decay_t<F>::Codomain;
  return std::forward<F>(f) >> arrow<B>(std::forward<G>(g));
}

/** @section morphism__Categorical_2 Verification: The Unit Laws (f ∘ id = f =
 * id ∘ f) */

// 1. Proof: Morphism Identity (Existence & Tagging)
// We verify that id exists and chains with tagged morphisms.
static_assert(
    IsArrow<decltype(id<int>() >> arrow<int, int>(std::negate<int>{}))>,
    "Unit Law: id_A must be a left-identity for morphisms out of A.");

static_assert(IsArrow<decltype(endo<int>(std::negate<int>{}) >> id<int>())>,
              "Unit Law: id_B must be a right-identity for morphisms into B.");

// 2. Proof: Cross-Species Identity
// id_Z combined with a Z -> B bridge must result in a Z -> B bridge
/** @section morphism__Cross_Species_Proof: Z -> B */
static_assert(
    IsArrow<decltype(arrow<int, bool>([](int x) { return x > 0; }))>,
    "Skeletal Failure: Failed to construct an anonymous cross-species "
    "Morphism.");

// 3. Proof: Extensional Equality (The Action)
// The composite morphism (f ∘ id) must yield the same value as f.
static_assert((endo<int>(std::negate<int>{}) >> id<int>())(42) == -42,
              "Action Proof: Composition with id must be value-invariant.");

// 4. Proof: Categorical Unity (id ∘ id = id)
static_assert(
    IsArrow<decltype(id<int>() >> id<int>())>,
    "Unity: id composed with itself must remain the Identity Morphism.");

/**
 * @concept IsIsomorphism
 * @brief An Arrow f: A -> B with a guaranteed Inverse g: B -> A.
 * @details Represents a reversible morphism. In Level 0, we verify
 *          the structural existence of the 'Undo' path.
 */
export template <typename F>
concept IsIsomorphism = IsArrow<F> && requires(F f) {
  // An isomorphism must provide its own inverse arrow
  { inverse(f) } -> IsArrow;
  // And the inverse must run BOTH ways: its domain is the original codomain
  // AND its codomain is the original domain (a two-sided inverse B→A, not
  // merely an arrow starting at B).  remove_cvref_t so a reference-deduced F
  // (e.g. an lvalue arrow forwarded into image(F&&, S)) still resolves its
  // member types.
  requires std::same_as<
      typename std::remove_cvref_t<F>::Codomain,
      typename std::remove_cvref_t<decltype(inverse(f))>::Domain>;
  requires std::same_as<
      typename std::remove_cvref_t<F>::Domain,
      typename std::remove_cvref_t<decltype(inverse(f))>::Codomain>;
};

/** @brief The structural inverse of an Identity is itself. */
export template <typename T>
[[nodiscard]] constexpr auto inverse(Identity<T> id) noexcept {
  return id;
}

// Proof: id_int is an isomorphism from int to int.
static_assert(IsIsomorphism<Identity<int>>,
              "Identity must be a self-inverse isomorphism.");

// Proof: id_bool is an isomorphism from bool to bool.
static_assert(IsIsomorphism<Identity<bool>>,
              "Identity must be a self-inverse isomorphism.");

/** @brief The structural inverse of Negation is itself (Involutive). */
export template <typename A, typename B, typename Impl>
  requires std::same_as<Impl, std::negate<A>>
[[nodiscard]] constexpr auto inverse(Morphism<A, B, Impl> f) noexcept {
  return f;  // Negate is its own inverse
}

// Proof: Tagged Negation is a formal Isomorphism.
static_assert(IsIsomorphism<TaggedNegate>,
              "Negation must be recognized as a reversible Morphism.");

// (A standalone Translation<T,K> iso arrow lived here briefly; it is superseded
// by the functions-are-graphs reading in dedekind.order --- a translation is
// its graph ℤ*ℤ | π1+fix(K)==π2, with inverse = the converse graph and image
// the affine pushforward.  The reified projection arithmetic (π+fix, ...) is
// the surviving vocabulary; the arrow wrapper is not needed.)
/**
 * @section morphism__Pipe_Operator
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
  requires IsArrow<Arrow> && std::convertible_to<T, typename Arrow::Domain>
constexpr auto operator>>(T&& value, const Arrow& f) ->
    typename Arrow::Codomain {
  return f(std::forward<T>(value));
}

/**
 * @brief User-declared monicity witness for an arrow type.
 * @details Injectivity cannot be verified at compile time in general.
 *          Users specialize this to `true` to declare that a given arrow
 *          is injective (i.e., a monomorphism in the category of sets).
 *          The compiler trusts the declaration; no runtime proof is required.
 *
 *          This mirrors `is_kleene_associative_v` in the partial algebra layer.
 */
export template <typename E>
inline constexpr bool is_monic_arrow_v = false;

/**
 * @concept IsMonicArrow
 * @brief An arrow declared to be a monomorphism (ι: A ↣ B).
 * @details A monic arrow is injective: if e(x) == e(y) then x == y.
 *          The user declares monicity via `is_monic_arrow_v<E> = true`.
 *
 *          An @c IsIsomorphism is monic for free: a two-sided inverse makes
 *          it injective, so every iso satisfies @c IsMonicArrow without a
 *          separate opt-in.  This is the concept-level derivation (an @c ||
 *          in the concept body, @b not an @c is_monic_arrow_v partial spec):
 *          it fires across module boundaries (where a concept-constrained
 *          variable-template partial spec does not, cf.\ the note at
 *          @c algebra:registration) and avoids overlapping the explicit
 *          @c is_monic_arrow_v<Identity<T>> and regular-mono specialisations.
 *          Pairs with the blanket iso @c retract below so that
 *          @c IsIsomorphism ⟹ @c IsRetractableArrow: a sound iso yields a
 *          sound retract by construction (its total @c inverse wrapped in
 *          always-Some), so the mono/retract decidability path is available
 *          to isos with no manual, unaudited @c retract hook.
 *
 *          A third branch reads a structural @c E::is_monic_arrow_tag typedef,
 *          the same tag-discovery idiom @c IsInitialObject / @c
 * IsTerminalObject use.  It lets a @b downstream template @b family opt into
 * monicity (e.g. @c sets::SubobjectInclusion<S>, monic by definition) where an
 *          @c is_monic_arrow_v partial spec would not cross the module boundary
 *          --- only full specialisations on concrete types do.  #881.
 */
export template <typename E>
concept IsMonicArrow =
    IsArrow<E> && (is_monic_arrow_v<E> || IsIsomorphism<E> ||
                   requires { typename E::is_monic_arrow_tag; });

// Identity arrows are always monic.
template <typename T>
inline constexpr bool is_monic_arrow_v<Identity<T>> = true;

static_assert(IsMonicArrow<Identity<int>>,
              "Identity must be recognised as a monic arrow.");

/**
 * @brief User-declared epicity witness for an arrow type.
 * @details Surjectivity cannot be verified at compile time in general.
 *          Users specialize this to `true` to declare that a given arrow
 *          is surjective (i.e., an epimorphism in the category of sets).
 *          The compiler trusts the declaration; no runtime proof is required.
 *
 *          This mirrors `is_monic_arrow_v` and follows the same opt-in
 *          declaration pattern.
 */
export template <typename E>
inline constexpr bool is_epic_arrow_v = false;

/**
 * @concept IsEpicArrow
 * @brief An arrow declared to be an epimorphism (π: A ↠ B).
 * @details An epic arrow is surjective: for every b in B there exists a in A
 *          with e(a) == b. The user declares epicity via
 *          `is_epic_arrow_v<E> = true`.
 *
 *          An @c IsIsomorphism is epic for free, symmetrically to @c
 *          IsMonicArrow: a two-sided inverse makes it surjective.  So an iso
 *          is both monic and epic without a separate opt-in, and @c
 *          IsBijectiveArrow (monic ∧ epic) follows from @c IsIsomorphism.
 *          Concept-level @c || (not an @c is_epic_arrow_v partial spec) for
 *          the same cross-module-firing reason as @c IsMonicArrow.
 */
export template <typename E>
concept IsEpicArrow = IsArrow<E> && (is_epic_arrow_v<E> || IsIsomorphism<E>);

// Identity arrows are always epic: for every element t in the codomain,
// id(t) == t, so every element is hit.
template <typename T>
inline constexpr bool is_epic_arrow_v<Identity<T>> = true;

static_assert(IsEpicArrow<Identity<int>>,
              "Identity must be recognised as an epic arrow.");

// The retract / iso-enabling surface (IsoRetract, the blanket iso retract, and
// the IsRetractableArrow concept) was extracted to the @c :iso partition, which
// imports @c :morphism.  @c :morphism keeps the basic arrow / factorisation
// primitives (IsArrow, IsMonicArrow, IsEpicArrow, IsIsomorphism); the retract
// surface (morally a Kleisli arrow into the maybe monad) lives downstream.

/**
 * @concept IsBijectiveArrow
 * @brief An arrow declared to be both a monomorphism (ι: A ↣ B) and an
 *        epimorphism (π: A ↠ B), i.e. an isomorphism of sets (A ≅ B).
 * @details A bijective arrow is both injective and surjective. The user
 *          declares this by specialising both `is_monic_arrow_v<E> = true`
 *          and `is_epic_arrow_v<E> = true`. Identity arrows are always
 *          bijective.
 */
export template <typename E>
concept IsBijectiveArrow = IsMonicArrow<E> && IsEpicArrow<E>;

static_assert(IsBijectiveArrow<Identity<int>>,
              "Identity must be recognised as a bijective arrow.");

// Coherence: an iso is bijective for free, with NO is_monic_arrow_v /
// is_epic_arrow_v opt-in.  TaggedNegate is an iso (proven above) but never
// opts into either marker, so this holds only because IsMonicArrow and
// IsEpicArrow both derive from IsIsomorphism.
static_assert(IsMonicArrow<TaggedNegate> && IsEpicArrow<TaggedNegate> &&
                  IsBijectiveArrow<TaggedNegate>,
              "IsIsomorphism ⟹ monic ∧ epic ∧ bijective, derived (no opt-in).");

// ---------------------------------------------------------------------------
// Pedagogical-accessibility synonyms (closes #459).
//
// The categorical names @c IsMonicArrow / @c IsEpicArrow /
// @c IsBijectiveArrow are the canonical primitives.  The set-theoretic
// names @c IsInjective / @c IsSurjective / @c IsBijective are exposed
// here as @c = aliases so readers coming from the standard high-school /
// undergraduate set-theory baseline can find the concepts under the
// vocabulary they already know.  No new structural content; the
// underlying opt-in traits ( @c is_monic_arrow_v / @c is_epic_arrow_v)
// retain their categorical names — register monicity / epicity once,
// either spelling fires.
//
// Hierarchy (strict ⇒ relaxed):
//   @c IsIsomorphism<F>  (above) — has @c inverse(f) ADL arrow available.
//          ⇓ structurally implies (in Set: iso ⇒ mono + epi)
//   @c IsBijective<E>  ≡  @c IsBijectiveArrow<E>
//          ⇓
//   @c IsInjective<E>  (≡ @c IsMonicArrow), @c IsSurjective<E> (≡ @c
//   IsEpicArrow)
//
// @c IsIsomorphism is the @b strictest: it additionally demands that the
// inverse morphism be present as an ADL-discoverable @c inverse() arrow.
// A @c IsBijective arrow is the set-theoretic statement of the same fact
// (injective + surjective) without requiring the inverse arrow to be
// reified.  The reverse implication ( @c IsBijective @c ⇒ @c
// IsIsomorphism) does @b not hold: a bijection can be declared via the
// opt-in traits without its inverse being available as a callable arrow.
// ---------------------------------------------------------------------------

/**
 * @concept IsInjective
 * @brief Set-theoretic synonym for @c IsMonicArrow — an arrow is
 *        @b injective iff distinct inputs yield distinct outputs
 *        (@c f(x) @c == @c f(y) @c → @c x @c == @c y).
 * @details Categorically the same concept: a monomorphism in the
 *          category of sets.  Opt-in via @c is_monic_arrow_v<E>.
 */
export template <typename E>
concept IsInjective = IsMonicArrow<E>;

/**
 * @concept IsSurjective
 * @brief Set-theoretic synonym for @c IsEpicArrow — an arrow is
 *        @b surjective iff every codomain element has a preimage
 *        (∀ @c b ∈ @c B ∃ @c a ∈ @c A. @c f(a) @c == @c b).
 * @details Categorically the same concept: an epimorphism in the
 *          category of sets.  Opt-in via @c is_epic_arrow_v<E>.
 */
export template <typename E>
concept IsSurjective = IsEpicArrow<E>;

/**
 * @concept IsBijective
 * @brief Set-theoretic synonym for @c IsBijectiveArrow — an arrow is
 *        @b bijective iff it is both injective and surjective, i.e. a
 *        set-theoretic isomorphism @c A @c ≅ @c B.
 */
export template <typename E>
concept IsBijective = IsBijectiveArrow<E>;

// Mechanical witnesses that the synonyms agree with the categorical
// originals on the canonical Identity arrow (and would diverge if any
// future drift broke the alias).
static_assert(IsInjective<Identity<int>>,
              "IsInjective synonym must agree with IsMonicArrow on Identity.");
static_assert(IsSurjective<Identity<int>>,
              "IsSurjective synonym must agree with IsEpicArrow on Identity.");
static_assert(IsBijective<Identity<int>>,
              "IsBijective synonym must agree with IsBijectiveArrow on "
              "Identity.");

// ---------------------------------------------------------------------------
// Inter-carrier arrow class taxonomy (#380).
//
// The textbook arrow classes that name themselves cleanly in this
// partition are the @b monic / @b epic pair (already declared above
// as @c is_monic_arrow_v / @c is_epic_arrow_v + @c IsMonicArrow /
// @c IsEpicArrow).  The other two classes #380 enumerates have homes
// downstream where their textbook reading is direct:
//
//   * @b Kleisli @b arrow (@c e: @c A @c → @c M<B>) — declared as
//     @c is_kleisli_arrow_v<E, M> + @c IsKleisliArrow<E, M> in
//     @c dedekind.category:kleisli, where the composition / fmap
//     machinery already lives.
//   * @b Sentinel @b realisation (@c e: @c A @c → @c B, total with
//     a documented sentinel for out-of-range inputs) — deferred to a
//     downstream partition (likely co-located with the
//     @c realize_to_<primitive> family in @c dedekind.numbers) when
//     step 2 of #380 lands.  No clean Pierce-style textbook name
//     applies, so the trait is intentionally not declared at the
//     @c :morphism layer.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Fish-operator concept tier (closes part of #450).
//
// The project's operator surface for arrow composition / monadic bind /
// comonadic extend is informally called the "fish family":
//   * @c >>   — arrow composition (here in @c :morphism); fish alias for
//               monadic bind (in @c :kleisli) on monadic carriers.
//   * @c <<   — fish alias for comonadic extend (in @c :kleisli).
//               @b No spoke-level overload exists at the @c :morphism
//               layer; this is comonadic-extend specific.
//   * @c >>=  — monadic bind (in @c :kleisli).
//   * @c <<=  — comonadic extend (in @c :kleisli).
//
// This concept tier reifies the @b operator-shape availability — does
// the type support the named operator? — at the type level.  Categorical
// reading: @c >> on arrows is composition in the base category C; @c
// >>= on a monadic carrier is composition in the Kleisli category 𝒞_M.
// The concepts here pin only the @b syntactic surface; the equational
// laws (associativity, unit) are the engineer's honesty obligation for
// each registration site.
// ---------------------------------------------------------------------------

/**
 * @concept HasArrowComposeOperators
 * @brief Two spoke-arrow types @c F, @c G compose via @c >> .
 * @details Captures the operator-shape availability of categorical
 *          arrow composition aligned with this partition's
 *          @c operator>> policy: spoke-only ( @c IsSpokeArrow ) and
 *          codomain/domain-matched ( @c F::Codomain @c = @c G::Domain ).
 *          Given @c f @c : @c A @c → @c B and @c g @c : @c B @c → @c C,
 *          the expression @c f @c >> @c g must compile and yield a
 *          result modelling @c IsArrow.
 *
 *          @b Note: this concept covers @c >> only.  There is no
 *          spoke-level @c << overload at the @c :morphism layer
 *          ( @c << in the project is comonadic extend in @c :kleisli);
 *          the reverse-composition direction, if reified later, is a
 *          separate concept.  Hub arrows (e.g. functors) compose in
 *          their own partitions.
 *
 *          The laws (associativity, identity) are the engineer's
 *          honesty obligation, mechanically discharged by the
 *          existing @c "id @c ∘ @c id @c = @c id" static_asserts.
 */
export template <typename F, typename G>
concept HasArrowComposeOperators =
    IsSpokeArrow<F> && IsSpokeArrow<G> &&
    std::same_as<typename std::remove_cvref_t<F>::Codomain,
                 typename std::remove_cvref_t<G>::Domain> &&
    requires(F const& f, G const& g) {
      { f >> g } -> IsArrow;
    };

// Witness: Identity arrows compose (id_int >> id_int = id_int) — the
// canonical syntactic check of @c >> availability on the simplest pair.
static_assert(HasArrowComposeOperators<Identity<int>, Identity<int>>,
              "Identity<int> must support @c >> arrow composition with "
              "itself; this is the canonical fish-operator witness.");

}  // namespace dedekind::category
