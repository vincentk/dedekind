/**
 * @file category.cppm
 * @brief The Categorical Engine: From Subobject Classifiers to ETCS.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section The_Dedekind_Universe
 * This module implements the internal language of the Dedekind ontology.
 * By adopting the Elementary Theory of the Category of Sets (ETCS), we
 * move away from "element-oriented" programming. Instead, mathematical
 * objects are treated as positions within a structure, defined entirely
 * by their morphisms (arrows).
 *
 * @section Structural_Levels
 * The ontology is organized into layers of increasing abstraction:
 *
 * - **Level -1: The Internal Language** (\ref logic, \ref species)
 *   Foundational machine primitives reified as categorical objects,
 *   governed by the Subobject Classifier (Ω).
 *
 * - **Level 0a: The Mapping Engine** (\ref morphisms, \ref functorial, \ref
 * kleisli) The "Highway System" for data flow. This layer defines how species
 *   transform while preserving algebraic invariants via Functors and
 *   Kleisli triples.
 *
 * - **Level 0b: Higher Structures** (\ref cartesian, \ref posetal)
 *   Cartesian Closed Categories (CCC) providing the logic for Products,
 *   Exponentials (Function Spaces), and Order-theoretic reductions.
 *
 * - **Level 0c: Axiomatic Foundations** (\ref etcs)
 *   The formalization of set-theoretic behavior using categorical axioms
 *   rather than ZFC membership.
 *
 * @see Wikipedia: [ETCS](https://en.wikipedia.org),
 *      [Cartesian Closed Category](https://en.wikipedia.org),
 *      [Topos Theory](https://en.wikipedia.org)
 */

export module dedekind.category;

/** @section Level_-1: The Bricks and Internal Language */
export import :logic;    // The Subobject Classifier (Ω)
export import :species;  // Reified Machine Primitives

/** @section Level_0a: The Mapping Engine */
export import :morphisms;   // Morphisms
export import :functorial;  // Natural Transformations
export import :kleisli;     // The Algebra of Contextual Composition

/** @section Level_0b: The Higher Structures (The Universe Laws) */
export import :cartesian;  // Finite Products, Terminal Objects, and
                           // Exponentials
export import :posetal;    // Order-theoretic Categories (A ≤ B as a Morphism)
export import :small;      // Enumerated and Finite Categories

/** @section Level_0c: The Axiomatic Registry */
export import :etcs;  // Elementary Theory of the Category of Sets
