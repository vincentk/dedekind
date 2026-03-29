/**
 * @file category.cppm
 * @brief The Foundational Topos: Logic, Species, and Functorial Control.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Category: The Internal Language and Mapping Engine
 * This module constitutes the structural bedrock of the Dedekind Universe
 * (Levels -1 to 0b). It synthesizes the relationship between "Truth" (Logic),
 * "Representation" (Species), and "Transformation" (Category Theory). 
 *
 * By housing the Subobject Classifier (Ω) alongside Functors and Monads, 
 * we enable a framework where logical predicates and computational contexts 
 * share a unified categorical grammar.
 *
 * @subsection Levels_and_Mechanics
 * - Level -1 (Logic): Pluggable Truths (Classical, Kleene, Lattice Morphisms).
 * - Level 0a (Species): The reification of machine primitives into the ontology.
 * - Level 0b (Functorial): The morphisms, lifting rules, and Kleisli pathways.
 *
 * @see dedekind.category:logic (The Internal Language / Ω)
 * @see dedekind.category:species (The Mathematical Primitives)
 * @see dedekind.category:functorial (Morphisms and Functorial Lifting)
 * @see dedekind.category:monad (The Logic of Context and Composition)
 * @see dedekind.category:comonad (The Logic of Extraction and Co-context)
 */

export module dedekind.category;

/** @section Foundations: Logic and Material */
export import :logic;      // Level -1: Truth Objects and Predicate Synthesis
export import :species;    // Level 0a: Machine Reification

/** @section Morphisms: The Functorial Engine */
export import :functorial; // Level 0b: Lifting and Natural Transformations
export import :monad;      // The Algebra of Composition (Unit, Bind)
export import :comonad;    // The Dual Algebra (Extract, Extend)

