/**
 * @file ontology.cppm
 * @brief The Unified Dedekind Ontology: A Registry of Structural Truths.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Ontology: The Formal Specification of the Dedekind Universe
 * This module aggregates the skeletal, mereological, and algebraic laws
 * required to synthesize the Continuum from the Discrete. By treating
 * mathematical objects as structures (Structuralism) rather than
 * collections (Mereology), we enable zero-overhead formal verification
 * within the C++23 type system.
 *
 * @see dedekind.ontology:logic (Level -1)
 * @see dedekind.ontology:species (Level 0a)
 * @see dedekind.ontology:category (Level 0b)
 * @see dedekind.ontology:mereology (Level 1)
 *
 * Wikipedia: Formal ontology, Structuralism (philosophy of mathematics)
 */

export module dedekind.ontology;

/** @section Level_-1_to_0: The Foundations (The Bricks and Cement) */
export import :logic;     // Level -1: Truth Objects (Ω) and Predicate Logic
export import :species;   // Level 0a: Reified Machine Primitives (The Bricks)
export import :category;  // Level 0b: Morphisms, Functors, and Kleisli Highways

/** @section Level_1: The Space (The Body and Relation) */
export import :mereology;  // Rules of Presence (Sets, Parts, and Wholes)
export import :order;      // Rules of Relation (Posets, Lattices, and Chains)
export import :topology;   // Rules of Closeness (Open Sets and Continuity)

/** @section Level_2: The Scale (Measurement and Path) */
export import :sequences;  // The Path: Mapping Magnitudes to Enumerations

/** @section Level_3: The Soul (Harmony and Action) */
export import :algebra;       // The Pure Laws (Groups, Rings, and Fields)
export import :morphologies;  // The Realized Forms (Cyclic, Ordered, and
                              // Archimedean)

/** @section Level_4: The Realization (The Registry) */
export import :geometry;  // Metric Spaces and Manifolds
export import :numbers;   // The Species Registry (ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ)
