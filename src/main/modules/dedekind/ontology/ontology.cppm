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
 * within the C++20 type system.
 *
 * @see dedekind.ontology:logic (Level -1)
 * @see dedekind.ontology:category (Level 0)
 * @see dedekind.ontology:mereology (Level 1)
 *
 * Wikipedia: Formal ontology, Structuralism (philosophy of mathematics)
 */

export module dedekind.ontology;

/** @section Level_-1_to_0: The Foundations (The Bricks and Cement) */
export import :logic;     // Truth Objects (Ω) and Predicate Logic
export import :category;  // Morphisms, Functors, and Natural Units (η)

/** @section Level_1: The Space (The Body and Relation) */
export import :mereology;  // Rules of Presence (Sets, Parts, and Wholes)
export import :order;      // Rules of Relation (Posets, Lattices, and Chains)
export import :topology;   // Rules of Closeness (Open Sets and Continuity)

/** @section Level_2_to_3: The Soul (Magnitude and Harmony) */
export import :cardinalities;  // The Scale of Magnitude (Aleph and Beth Towers)
export import :algebra;  // The Laws of Harmony (Groups, Rings, and Fields)

/** @section Level_4: The Realization (The Registry) */
export import :geometry;  // Metric Spaces and Manifolds
export import :numbers;   // The Species Registry (ℕ ⊂ ℤ ⊂ ℚ ⊂ ℝ)

/** @section Extensions: Applied Structuralism */
export import :order_algebra;  // Minkowski Sums and Ordered Groups
