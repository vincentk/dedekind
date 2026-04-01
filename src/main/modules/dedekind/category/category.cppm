/**
 * @file category.cppm
 * @brief Categorical Foundations for C++23.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Implementation_Philosophy
 * This module implements a structuralist approach to set theory based on the
 * Elementary Theory of the Category of Sets (ETCS). We use C++23 modules and
 * concepts to treat mathematical objects as positions within a system of
 * relations (morphisms) rather than traditional object-oriented hierarchies.
 *
 * By lifting these structural invariants into the type system, we aim to bridge
 * the performance gap between high-level symbolic reasoning and low-level
 * machine execution, allowing the compiler to optimize code based on
 * mathematical laws (e.g., algebraic pruning of contradictory predicates).
 *
 * @section Module_Hierarchy
 * To manage compile-time overhead ("template tax"), the implementation is
 * stratified into a Directed Acyclic Graph (DAG) of module partitions:
 *
 * - **Level -1: Base Primitives** (\ref logic, \ref species)
 *   Foundational types including the subobject classifier (Ω) and
 *   machine-level representations of mathematical species.
 *
 * - **Level 0a: Functorial Mappings** (\ref morphisms, \ref functorial, \ref
 * kleisli) The "Highway System" for data flow. Implements the mechanics of
 *   morphism transformation via Functors and Kleisli triples.
 *
 * - **Level 0b: Higher Category Theory** (\ref cartesian, \ref posetal)
 *   Cartesian Closed Categories (CCC) providing the logic for products,
 *   exponentials (function spaces), and order-theoretic relations.
 *
 * - **Level 0c: Set-Theoretic Axioms** (\ref etcs)
 *   A formal registry mapping ETCS axioms to C++ language features,
 *   enabling compile-time verification of algebraic laws.
 *
 * @see Wikipedia: [ETCS](https://en.wikipedia.org),
 *      [Cartesian Closed Category](https://en.wikipedia.org),
 *      [Topos Theory](https://en.wikipedia.org)
 * @see Lawvere, F.W. (1964) "An Elementary Theory of the Category of Sets"
 * @see McLarty, C. (1993) "Numbers can be just what they have to"
 */

export module dedekind.category;

/** @section Level_-1: The Bricks and Internal Language */
export import :logic;    // The Subobject Classifier (Ω)
export import :species;  // Reified Machine Primitives

/** @section Level_0a: The Mapping Engine */
export import :morphisms;   // Morphisms
export import :algebra;     // Functors and Natural Transformations
export import :functorial;  // Natural Transformations
export import :kleisli;     // The Algebra of Contextual Composition
export import :actions;     // The Action-First Bootstrapping of Monads and
                            // Comonads

/** @section Level_0b: The Higher Structures (The Universe Laws) */
export import :cartesian;  // Finite Products, Terminal Objects, and
                           // Exponentials
export import :posetal;    // Order-theoretic Categories (A ≤ B as a Morphism)
export import :small;      // Enumerated and Finite Categories

/** @section Level_0c: The Axiomatic Registry */
export import :etcs;  // Elementary Theory of the Category of Sets
