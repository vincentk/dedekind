/**
 * @file ontology.cppm
 * @brief The Unified Dedekind Ontology: A Registry of Structural Truths.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Ontology: The formal specification of the Dedekind universe.
 * @details This module aggregates the mereological, topological, algebraic,
 *          and numerical rules that define our "Naked" road to the continuum.
 * Wikipedia: Formal ontology, Structuralism (philosophy of mathematics)
 */

export module dedekind.ontology;

// --- The Structural Partitions ---
export import :mereology;  // Sets, Lattices, and Pointed Species
export import :topology;   // Open/Closed Sets and Sequences
export import :algebra;    // Monoids, Groups, and Fields
export import :geometry;
export import :numbers;        // N ⊂ Z ⊂ Q ⊂ R
export import :order;          // Orders, Density, and Midpoints
export import :order_algebra;  // Rays, Points, and Minkowski Sums
