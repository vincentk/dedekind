/**
 * @file ontology:logic.cppm
 * @brief Level -1: The Rules of Thought (Boolean Algebra and Predicate Synthesis).
 * 
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @partition :logic
 * @build_order 0
 * @dependency None (The Absolute Foundation)
 * 
 * @section Logic: The Internal Language of the Topos
 * Before we can define an "Action" (Category) or a "Body" (Set), we must 
 * establish the "Subobject Classifier" (Ω). This partition defines the 
 * internal logic that governs all structuralist predicates.
 * 
 * @details 
 * Unlike the "External Logic" of the C++ compiler (which is strictly binary), 
 * this partition allows for "Pluggable Truths":
 * - Classical Logic: The standard {True, False} boolean lightbulb.
 * - Kleene Logic: The {True, False, Unknown} propositional engine for 
 *   handling undecidability and partial information.
 * - Lattice Morphisms: Infix operators (&&, ||, !) are anchored here as 
 *   Point-Free composition rules, turning Predicates into a Bounded Lattice.
 * 
 * @section Structural_Inference
 * By defining logic at Level -1, we enable "Decoupled Truth". A Set's 
 * membership rule can switch from Boolean to Ternary without altering 
 * the Mereological code in the partitions above.
 * 
 * @anchors C++ Logical Primitives: bool, Ternary (Kleene), &&, ||, !.
 * 
 * Wikipedia: Subobject classifier, Kleene logic, Internal logic, Point-free
 */
module

    import <concepts>;
import <algorithm>;

export module dedekind.ontology:logic;

namespace dedekind::ontology {

/** @brief The Logic Registry (The Socket) */
template <typename T>
struct LogicTraits;

/** @section Species 1: Classical Logic (The Lightbulb) */
template <>
struct LogicTraits<bool> {
  static constexpr bool AND(bool a, bool b) { return a && b; }
  static constexpr bool OR(bool a, bool b) { return a || b; }
  static constexpr bool NOT(bool a) { return !a; }
};

/** @section Species 2: Kleene Ternary Logic (The Unknown) */
export enum class Ternary : int8_t { False = -1, Unknown = 0, True = 1 };

template <>
struct LogicTraits<Ternary> {
  static constexpr Ternary AND(Ternary a, Ternary b) {
    return static_cast<Ternary>(
        std::min(static_cast<int8_t>(a), static_cast<int8_t>(b)));
  }
  static constexpr Ternary OR(Ternary a, Ternary b) {
    return static_cast<Ternary>(
        std::max(static_cast<int8_t>(a), static_cast<int8_t>(b)));
  }
  static constexpr Ternary NOT(Ternary a) {
    return static_cast<Ternary>(-static_cast<int8_t>(a));
  }
};

/** @section The Point-Free Composition Engine */

template <typename P, typename T, typename Res = bool>
concept IsPredicate = requires(P p, T x) {
  { p(x) } -> std::convertible_to<Res>;
};

// THE INFIX OPERATORS: Deduce the Logic Species (bool vs Ternary) automatically

export template <typename P1, typename P2>
auto operator&&(P1 p1, P2 p2) {
  return [=](auto&& x) {
    using L = decltype(p1(x));
    return LogicTraits<L>::AND(p1(x), p2(x));
  };
}

export template <typename P1, typename P2>
auto operator||(P1 p1, P2 p2) {
  return [=](auto&& x) {
    using L = decltype(p1(x));
    return LogicTraits<L>::OR(p1(x), p2(x));
  };
}

export template <typename P>
auto operator!(P p) {
  return [=](auto&& x) {
    using L = decltype(p(x));
    return LogicTraits<L>::NOT(p(x));
  };
}

/** @subsection Test 1: Boolean (Classical) Invariants */
static_assert((true && true) == true);
static_assert((true && false) == false);
static_assert(!true == false);
static_assert((true || false) == true);

/** @subsection Test 2: Ternary (Kleene) Invariants */
// Identity & Annihilation
static_assert((True && Unknown) == Unknown);
static_assert((False && Unknown) == False);
static_assert((True || Unknown) == True);
static_assert((False || Unknown) == Unknown);

// The "Soul" of Propositional Logic: Kleene's Negation
static_assert(!Unknown == Unknown);
static_assert(!True == False);
static_assert(!False == True);

// De Morgan's Law Proof: !(A && B) == !A || !B
static_assert(!(True && Unknown) ==
              (!True || !Unknown));  // !(U) == F || U => U == U

}  // namespace dedekind::ontology