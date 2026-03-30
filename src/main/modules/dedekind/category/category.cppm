/**
 * @file category.cppm
 * @brief The Categorical Engine: From Truth Objects to ETCS.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Category: The Structuralist Foundation
 * This module defines the internal language of the Dedekind Universe.
 * By implementing the Elementary Theory of the Category of Sets (ETCS),
 * we treat mathematical objects as positions within a structure rather
 * than collections of elements.
 *
 * @subsection Structural_Hierarchies
 * - Level -1: Internal Logic (Ω) and Machine Species.
 * - Level 0a: Functorial Mapping and Kleisli Highways.
 * - Level 0b: Cartesian Closed Structures (Products and Exponentials).
 * - Level 0c: Posetal Reductions (Order as Morphism).
 *
 * @see dedekind.category:logic (The Subobject Classifier Ω)
 * @see dedekind.category:cartesian (Finite Products and Exponentials)
 * @see dedekind.category:posetal (Morphisms as Relations)
 *
 * Wikipedia: ETCS, Cartesian closed category, Topos
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
