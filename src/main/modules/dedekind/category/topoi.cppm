/**
 * @file dedekind/category/topoi.cppm
 * @partition :topoi
 * @brief The Internal Logic of the Topos.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @quote
 * "Grothendieck has made the category (rather than the space) the central
 *  aspect that we should explicitly axiomatise."
 *  — Myles Tierney
 *
 * @section topoi__The_Subobject_Classifier
 * The Lawvere-Tierney object Ω (Omega) is the central object that classifies
 * subobjects. Every elementary topos carries an internal logic which
 * generalizes standard set theory into objective categorical form.
 *
 * Textbook defaults in this partition:
 * - `operator&&` models meet/intersection of characteristic predicates.
 * - `operator||` models join/union of characteristic predicates.
 * - `operator!` models complement.
 *
 * Wikipedia: Lawvere–Tierney topology, Subobject classifier, Topos
 * @see Lawvere (1964), An elementary theory of the category of sets.
 * @see Lambek & Scott (1988), Introduction to Higher-Order Categorical Logic.
 *
 * @note "BUDOWANE JEST MNIEJ DOSKONALE OD BUDUJACEGO."
 *       ("What is built is less perfect than what builds it.")
 *       -- Stanislaw Lem, GOLEM XIV
 */

module;

#include <concepts>
#include <type_traits>
#include <utility>

export module dedekind.category:topoi;

import :logic;
import :morphism;
import :cartesian;

namespace dedekind::category {

template <typename>
inline constexpr bool always_false_v = false;

template <typename C>
concept IsClassifierConstant =
    std::same_as<std::remove_cvref_t<C>, bool> ||
    std::same_as<std::remove_cvref_t<C>, Ternary> || requires {
      typename std::remove_cvref_t<C>::logic_species;
      typename std::remove_cvref_t<C>::machine_type;
    };

template <typename OmegaTarget, typename Constant>
constexpr auto lift_classifier_constant(Constant&& value) {
  using C = std::remove_cvref_t<Constant>;
  if constexpr (std::same_as<C, OmegaTarget>) {
    return value;
  } else if constexpr (std::same_as<OmegaTarget, Ternary> &&
                       std::same_as<C, bool>) {
    return value ? Ternary::True : Ternary::False;
  } else {
    static_assert(always_false_v<OmegaTarget>,
                  "Unsupported classifier constant lift between logic species");
  }
}

/** @section topoi__Point_Free_Composition_Engine */

/**
 * @brief The Predicate Morphism (A Mapping from Domain to Truth).
 *
 * In the Dedekind topos, a Set is not a container, but a Rule. `IsPredicate`
 * formalizes this rule as a mapping from a Domain object (T) to a Logical
 * Species (Ω).
 *
 * @tparam P The Predicate candidate (typically a Lambda or a Functor).
 *
 * @req { p(x) } The candidate must be callable with an instance of the
 * Domain.
 * @req SpeciesTraits<Res>::species The return type must be registered in the
 *      Ontology Bridge.
 * @req IsLogicalSpecies<S> The resolved logic species must satisfy the
 *      formal algebraic requirements (AND/OR/NOT).
 *
 * @note By enforcing this contract, `dedekind` ensures that logical
 * composition (p1 && p2) only occurs between rules that share a common
 * mathematical foundation.
 */
export template <typename P>
concept IsPredicate =
    IsArrow<P> && LogicalValue<Cod<P>> && LogicalMap<P, Dom<P>> &&
    !std::same_as<std::remove_cvref_t<P>, bool>;

/**
 * @concept IsCharacteristic
 * @brief Categorical alias for a Predicate mapping to Ω.
 */
export template <typename P>
concept IsCharacteristic = IsPredicate<P>;

/**
 * @concept IsSieve
 * @brief Signature-level witness for a sieve of arrows into an object.
 *
 * @details
 * A sieve over an object X is represented as a collection of incoming arrows
 * together with closure under precomposition (modeled here by
 * `pullback_along`). This concept is intentionally lightweight and currently
 * models a single-arrow-type approximation: `S::Arrow` is fixed and
 * `pullback_along` returns the same sieve type `S`.
 *
 * It verifies shape/signature only for this narrowed surface; semantic laws
 * are exercised in tests and can be strengthened by later issue work.
 */
export template <typename S>
concept IsSieve = requires(S s, typename S::Arrow f) {
  typename S::Object;
  typename S::Arrow;
  requires IsArrow<typename S::Arrow>;
  requires std::same_as<Cod<typename S::Arrow>, typename S::Object>;
  { s.target() } -> std::same_as<typename S::Object>;
  { s.contains(f) } -> std::convertible_to<bool>;
  { s.pullback_along(f) } -> std::same_as<S>;
};

/**
 * @concept IsGrothendieckTopology
 * @brief Signature-level witness for Grothendieck coverage rules over sieves.
 *
 * @details
 * This concept encodes the three textbook shape obligations used by this
 * project's ETCS narrative:
 * 1. Identity cover witness,
 * 2. Pullback-stability witness,
 * 3. Transitivity witness.
 *
 * It is a proof-surface contract: implementations may use stronger internal
 * laws, but these witnesses keep API-level claims reviewable and testable.
 */
export template <typename J, typename S>
concept IsGrothendieckTopology =
    IsSieve<S> && requires(S s, typename S::Arrow f) {
      { J::is_cover(s) } -> std::convertible_to<bool>;
      { J::identity_cover(s.target()) } -> std::convertible_to<bool>;
      { J::is_cover(s.pullback_along(f)) } -> std::convertible_to<bool>;
      { J::pullback_stable(s, f) } -> std::convertible_to<bool>;
      { J::transitive(s) } -> std::convertible_to<bool>;
    };

/**
 * @concept IsBundleProjection
 * @brief A projection morphism π : E -> B for a bundle candidate.
 *
 * @details
 * This is a structural contract for the projection map in a fiber bundle.
 * It keeps the proof surface explicit without forcing a concrete atlas model
 * yet.
 */
export template <typename Pi, typename E, typename B>
concept IsBundleProjection =
    IsArrow<Pi> && std::same_as<Dom<Pi>, E> && std::same_as<Cod<Pi>, B>;

/**
 * @concept IsFiberBundle
 * @brief Signature-level witness for a fiber bundle over a base species.
 *
 * @details
 * This concept checks for the standard structural ingredients:
 * - Total space E,
 * - Base space B,
 * - Fiber species F,
 * - Projection map π : E -> B,
 * - Local trivialization witness.
 *
 * It intentionally verifies API shape only; stronger geometric laws can be
 * layered later.
 */
export template <typename Bundle>
concept IsFiberBundle = requires(Bundle bundle) {
  typename Bundle::TotalSpace;
  typename Bundle::BaseSpace;
  typename Bundle::Fiber;
  typename Bundle::Projection;

  requires IsBundleProjection<typename Bundle::Projection,
                              typename Bundle::TotalSpace,
                              typename Bundle::BaseSpace>;

  { bundle.projection() } -> std::same_as<typename Bundle::Projection>;
  requires IsBundleProjection<decltype(bundle.projection()),
                              typename Bundle::TotalSpace,
                              typename Bundle::BaseSpace>;

  { bundle.trivializes_locally() } -> std::convertible_to<bool>;
};

namespace detail {

// Compiler-validated documentation witnesses for the infrastructure concepts.
struct topo_demo_arrow {
  using Domain = int;
  using Codomain = int;
  constexpr int operator()(const int& x) const { return x; }
};

struct topo_demo_bool_arrow {
  using Domain = int;
  using Codomain = bool;
  constexpr bool operator()(const int& x) const { return x % 2 == 0; }
};

struct topo_demo_sieve {
  using Object = int;
  using Arrow = topo_demo_arrow;

  constexpr Object target() const { return 0; }
  constexpr bool contains(Arrow) const { return true; }
  constexpr topo_demo_sieve pullback_along(Arrow) const { return {}; }
};

struct topo_bad_sieve_codomain_mismatch {
  using Object = int;
  using Arrow = topo_demo_bool_arrow;

  constexpr Object target() const { return 0; }
  constexpr bool contains(Arrow) const { return true; }
  constexpr topo_bad_sieve_codomain_mismatch pullback_along(Arrow) const {
    return {};
  }
};

struct topo_demo_topology {
  static constexpr bool is_cover(const topo_demo_sieve&) { return true; }
  static constexpr bool identity_cover(int) { return true; }
  static constexpr bool pullback_stable(const topo_demo_sieve&,
                                        topo_demo_arrow) {
    return true;
  }
  static constexpr bool transitive(const topo_demo_sieve&) { return true; }
};

struct topo_projection_arrow {
  using Domain = std::pair<int, int>;
  using Codomain = int;
  constexpr int operator()(const Domain& e) const { return e.first; }
};

struct topo_bundle_witness {
  using TotalSpace = std::pair<int, int>;
  using BaseSpace = int;
  using Fiber = int;
  using Projection = topo_projection_arrow;

  constexpr Projection projection() const { return {}; }
  constexpr bool trivializes_locally() const { return true; }
};

}  // namespace detail

static_assert(IsSieve<detail::topo_demo_sieve>,
              "Sieve witness must satisfy the infrastructure contract.");
static_assert(!IsSieve<detail::topo_bad_sieve_codomain_mismatch>,
              "Sieve codomain must match target object.");
static_assert(
    IsGrothendieckTopology<detail::topo_demo_topology, detail::topo_demo_sieve>,
    "Topology witness must satisfy coverage contract over a sieve.");
static_assert(
    IsBundleProjection<detail::topo_projection_arrow, std::pair<int, int>, int>,
    "Bundle projection witness must type-check as E -> B.");
static_assert(
    IsFiberBundle<detail::topo_bundle_witness>,
    "Fiber bundle witness must expose projection and local trivialization.");

/**
 * @concept IsSubobject
 * @brief The categorical witness of a monomorphism ι: S ↣ A.
 *
 * @tparam S The Subobject Species (The "Body").
 * @tparam A The Ambient Species (The "Space").
 */
export template <typename S, typename A>
concept IsSubobject = requires(S s, typename S::Member m) {
  /**
   * @brief ι: S ↣ A
   * The canonical inclusion morphism. Every member of S must
   * uniquely map to a member of the ambient species A.
   */
  { s.ι(m) } -> std::same_as<A>;

  /**
   * @brief χ: A ⟶ Ω
   * Every subobject in a Topos is uniquely classified by a
   * morphism into the subobject classifier Ω.
   */
  { s.χ } -> IsPredicate;

  // Metadata verification: The declared ambient must match A.
  typename S::Ambient;
  requires std::same_as<typename S::Ambient, A>;

  // Characteristic morphism verification: χ must classify elements of A.
  requires std::same_as<Dom<decltype(s.χ)>, A>;
};

/**
 * @brief The Subobject Species S ↣ A.
 * @details Represents a subset of the ambient species A, defined by the
 * characteristic morphism χ: A ⟶ Ω.
 *
 * @tparam A The Ambient (Super) Species.
 * @tparam Chi The type of the characteristic morphism χ.
 */
export template <typename A, typename Chi>
struct Subobject {
  using Ambient = A;
  Chi χ;  // The Rule: A ⟶ Ω

  /**
   * @brief The Internal Member of the Subobject.
   * Wraps a value from the ambient space that satisfies the classifier.
   */
  struct Member {
    A value;
  };

  /** @brief ι: S ↣ A (The Textbook Inclusion) */
  constexpr A ι(const Member& m) const { return m.value; }

  /** @brief Membership query @c s(v): delegates to the classifier @c χ. */
  constexpr auto operator()(const A& v) const { return χ(v); }

  /**
   * @section topoi__Pullback_Projections
   * These projections are only valid when the Ambient space A satisfies
   * the IsProduct concept (X × Y).
   */

  /** @brief π₁: S ⟶ X */
  constexpr auto π1(const Member& m) const
    requires requires { m.value.first; }
  {
    return m.value.first;
  }

  /** @brief π2: S ⟶ Y */
  constexpr auto π2(const Member& m) const
    requires requires { m.value.second; }
  {
    return m.value.second;
  }
};

/**
 * @brief Characteristic Morphism Factory (classify)
 * Returns the Subobject Species (The Body) carrying the rule (The Arrow).
 */
export template <typename A, typename F>
  requires(!IsArrow<std::remove_cvref_t<F>>) &&
          std::invocable<std::decay_t<F>, const A&> &&
          LogicalValue<std::invoke_result_t<std::decay_t<F>, const A&>>
constexpr auto classify(F&& f) {
  auto rule = arrow<A>(std::forward<F>(f));
  return Subobject<A, decltype(rule)>{std::move(rule)};
}

/**
 * @brief Domain-inferring classify: deduces A from the callable's signature.
 *
 * Allows classify(f) when f is a lambda with a strongly-typed operator(),
 * e.g. [](const Complex<R>& x) -> bool { ... }.  The domain A is extracted
 * via arrow's signature_extractor, so no explicit template argument is needed.
 */
export template <typename F>
  requires(!IsArrow<std::remove_cvref_t<F>>) &&
          requires { typename signature_extractor<std::decay_t<F>>::type; } &&
          LogicalValue<codomain_t<std::decay_t<F>>>
constexpr auto classify(F&& f) {
  using Fn = std::decay_t<F>;
  using A = domain_t<Fn>;
  auto rule = arrow<A>(std::forward<F>(f));
  return Subobject<A, decltype(rule)>{std::move(rule)};
}

/**
 * @brief Idempotent classify: Passthrough for existing Arrows.
 */
export template <typename A, IsArrow F>
  requires IsPredicate<F> && std::same_as<Dom<F>, A>
constexpr auto classify(F&& f) {
  return Subobject<A, std::remove_cvref_t<F>>{std::forward<F>(f)};
}

/**
 * @concept IsQuotient
 * @brief The categorical witness of a regular epimorphism @c q: A ↠ Q ---
 *        the dual of @c IsSubobject.
 *
 * @details A quotient @c Q of @c A is given by:
 *
 *   - @c q.q @c : @c A @c ⟶⟶ @c Q::Class --- the regular-epi
 *     projection onto equivalence classes (an @c IsArrow),
 *   - @c q.r @c : @c A @c × @c A @c ⟶ @c Ω --- the kernel relation
 *     (the @em co-classifier, dual to @c χ for @c IsSubobject), pinned
 *     as an @c IsPredicate whose domain is a product object @c IsProduct
 *     of @c A and @c A: two elements @c x, @c y @c ∈ @c A are equated
 *     by @c q iff @c r((x, y)) @c = @c True.
 *
 * In a topos, just as every subobject is uniquely classified by a
 * unary characteristic morphism @c χ @c : @c A @c → @c Ω (membership
 * predicate), every regular-epi quotient is uniquely co-classified by
 * its kernel relation @c r @c : @c A @c × @c A @c → @c Ω (binary
 * equivalence predicate over a product object).  This concept names
 * the dual surface so downstream concepts ( @c IsCoequalizer in
 * @c :image, future pushouts) can constrain the quotient leg of a
 * colimit at the type level.
 *
 * @b Symmetry @b with @c IsSubobject: where @c IsSubobject pins
 * @c s.χ as an @c IsPredicate with @c Dom<χ> @c = @c A,
 * @c IsQuotient pins @c q.q as an @c IsArrow with
 * @c Dom<q> @c = @c A and @c Cod<q> @c = @c Q::Class, and @c q.r as
 * an @c IsPredicate over a product domain @c IsProduct<Dom<r>, A, A>.
 * Both members are proper morphisms (carrying @c Domain / @c Codomain
 * typedefs) so they compose with the rest of the @c :category arrow
 * machinery uniformly --- raw callables won't satisfy this concept.
 *
 * The @b structural shape pinned here is the operational signatures.
 * The @em equivalence-relation laws on @c r (reflexivity, symmetry,
 * transitivity) and the @em regular-epi universality of @c q
 * (existence and uniqueness of the unique factoring map for any
 * arrow that equates the kernel relation) are the engineer's honesty
 * obligation, as for @c IsNNO and @c IsSubobject --- C++ concepts can
 * pin shape, not the $\forall$ in the universal property.
 *
 * @tparam Q The Quotient Species (The "Quotient Body").
 * @tparam A The Ambient Species (The "Source Space").
 */
export template <typename Q, typename A>
concept IsQuotient = requires(Q q) {
  /**
   * @brief q: A ⟶⟶ Q::Class
   * The regular-epi projection, pinned as an @c IsArrow so it composes
   * uniformly with the rest of @c :category.
   */
  { q.q } -> IsArrow;

  /**
   * @brief r: A × A ⟶ Ω
   * The kernel relation --- co-classifier of the quotient, pinned as
   * an @c IsPredicate over a product domain.
   */
  { q.r } -> IsPredicate;

  // Metadata verification: declared ambient must match A.
  typename Q::Ambient;
  typename Q::Class;
  requires std::same_as<typename Q::Ambient, A>;

  // Arrow signatures: q : A ⟶ Q::Class.
  requires std::same_as<Dom<decltype(q.q)>, A>;
  requires std::same_as<Cod<decltype(q.q)>, typename Q::Class>;

  // Predicate domain: r is over a product object IsProduct<Dom<r>, A, A>.
  requires IsProduct<Dom<decltype(q.r)>, A, A>;
};

/** @brief Logical Conjunction (Intersection): Synthesizes a rule for A ∩ B.
 *  @note Textbook term: meet (∧) in the internal Heyting/Boolean algebra of Ω.
 */
export template <IsPredicate P, IsPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>> && std::same_as<Cod<P>, Cod<Q>>
auto operator&&(P&& p, Q&& q) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  return arrow<A, Ω>([p = std::forward<P>(p), q = std::forward<Q>(q)](
                         const A& x) { return L::AND(p(x), q(x)); });
}

/** @brief Conjunction of a constant logical value with a predicate. */
export template <IsClassifierConstant C, IsPredicate P>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator&&(C&& constant, P&& p) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  const Ω lifted = lift_classifier_constant<Ω>(std::forward<C>(constant));
  return arrow<A, Ω>([lifted, p = std::forward<P>(p)](const A& x) {
    return L::AND(lifted, p(x));
  });
}

/** @brief Conjunction of a predicate with a constant logical value. */
export template <IsPredicate P, IsClassifierConstant C>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator&&(P&& p, C&& constant) {
  return std::forward<C>(constant) && std::forward<P>(p);
}

/** @brief Logical Disjunction (Union): Synthesizes a rule for A ∪ B.
 *  @note Textbook term: join (∨) in the internal Heyting/Boolean algebra of Ω.
 */
export template <IsPredicate P, IsPredicate Q>
  requires std::same_as<Dom<P>, Dom<Q>> && std::same_as<Cod<P>, Cod<Q>>
auto operator||(P&& p, Q&& q) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  return arrow<A, Ω>([p = std::forward<P>(p), q = std::forward<Q>(q)](
                         const A& x) { return L::OR(p(x), q(x)); });
}

/** @brief Disjunction of a constant logical value with a predicate. */
export template <IsClassifierConstant C, IsPredicate P>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator||(C&& constant, P&& p) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;
  using Ω = Cod<P>;

  const Ω lifted = lift_classifier_constant<Ω>(std::forward<C>(constant));
  return arrow<A, Ω>([lifted, p = std::forward<P>(p)](const A& x) {
    return L::OR(lifted, p(x));
  });
}

/** @brief Disjunction of a predicate with a constant logical value. */
export template <IsPredicate P, IsClassifierConstant C>
  requires(!IsPredicate<std::remove_cvref_t<C>>) &&
          requires(C c) { lift_classifier_constant<Cod<P>>(c); }
auto operator||(P&& p, C&& constant) {
  return std::forward<C>(constant) || std::forward<P>(p);
}

/** @brief Logical Negation (Complement): Synthesizes a rule for ¬A.
 *  @note Textbook term: pseudocomplement/complement depending on Ω.
 */
export template <IsPredicate P>
auto operator!(P&& p) {
  using L = typename GetLogic<Cod<P>>::type;
  using A = Dom<P>;

  // Return a formal Morphism A -> Ω
  return arrow<A>(
      [p = std::forward<P>(p)](const A& x) { return L::NOT(p(x)); });
}

/**
 * @brief Compose two plain callable predicates over a declared domain A.
 * @details
 * This is a practical bridge for std::predicate-style lambdas/functions that
 * have not been lifted to Arrow/Predicate objects yet.
 */
export template <typename A, typename P, typename Q>
  requires std::invocable<const std::decay_t<P>&, const A&> &&
           std::invocable<const std::decay_t<Q>&, const A&> &&
           LogicalValue<std::remove_cvref_t<
               std::invoke_result_t<const std::decay_t<P>&, const A&>>> &&
           std::same_as<
               std::remove_cvref_t<
                   std::invoke_result_t<const std::decay_t<P>&, const A&>>,
               std::remove_cvref_t<
                   std::invoke_result_t<const std::decay_t<Q>&, const A&>>>
constexpr auto predicate_and(P&& p, Q&& q) {
  return classify<A>(std::forward<P>(p)).χ && classify<A>(std::forward<Q>(q)).χ;
}

/** @brief Disjunction bridge for plain callable predicates over domain A. */
export template <typename A, typename P, typename Q>
  requires std::invocable<const std::decay_t<P>&, const A&> &&
           std::invocable<const std::decay_t<Q>&, const A&> &&
           LogicalValue<std::remove_cvref_t<
               std::invoke_result_t<const std::decay_t<P>&, const A&>>> &&
           std::same_as<
               std::remove_cvref_t<
                   std::invoke_result_t<const std::decay_t<P>&, const A&>>,
               std::remove_cvref_t<
                   std::invoke_result_t<const std::decay_t<Q>&, const A&>>>
constexpr auto predicate_or(P&& p, Q&& q) {
  return classify<A>(std::forward<P>(p)).χ || classify<A>(std::forward<Q>(q)).χ;
}

/** @brief Negation bridge for plain callable predicates over domain A. */
export template <typename A, typename P>
  requires std::invocable<const std::decay_t<P>&, const A&> &&
           LogicalValue<std::remove_cvref_t<
               std::invoke_result_t<const std::decay_t<P>&, const A&>>>
constexpr auto predicate_not(P&& p) {
  return !classify<A>(std::forward<P>(p)).χ;
}

/**
 * @brief Constant classifier factory over domain A: A -> Ω.
 */
export template <typename A, typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
constexpr auto constant_classifier(typename L::Ω value) {
  return arrow<A, typename L::Ω>([value](const A&) { return value; });
}

/** @brief Default true classifier over domain A. */
export template <typename A, typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
constexpr auto classifier_true() {
  return constant_classifier<A, L>(L::True);
}

/** @brief Default false classifier over domain A. */
export template <typename A, typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
constexpr auto classifier_false() {
  return constant_classifier<A, L>(L::False);
}

/** @brief Default unknown classifier over domain A (only for logics with
 * Unknown). */
export template <typename A, typename L = TernaryLogic>
  requires IsLogicalSpecies<L> && requires { L::Unknown; }
constexpr auto classifier_unknown() {
  return constant_classifier<A, L>(L::Unknown);
}

/**
 * @brief The 'true' morphism: 1 → Ω.
 */
export template <typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
auto logical_true() {
  return arrow<One, typename L::Ω>([](One) { return L::True; });
}

/**
 * @brief The 'false' morphism: 1 → Ω.
 */
export template <typename L = ClassicalLogic>
  requires IsLogicalSpecies<L>
auto logical_false() {
  return arrow<One, typename L::Ω>([](One) { return L::False; });
}

/**
 * @brief Power Object Alias
 * @details In a Topos, P(A) is the exponential object Ω^A.
 */
export template <typename A, typename L = ClassicalLogic>
using PowerObject = Exponential<A, typename L::Ω>;

}  // namespace dedekind::category
