/**
 * @file sets.cppm
 * @brief The Dedekind Universe: Realized Collections and Set Morphisms.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Set_Theory: The Realization of Mereological Bodies
 * This module provides the physical implementations for the abstract laws
 * defined in dedekind.ontology. While the Ontology defines "What is a Set,"
 * this module provides the "How to Build a Set."
 *
 * @details
 * In the Dedekind architecture, Set Theory serves as the bridge between
 * Pure Logic (Level -1) and Concrete Magnitude (Level 2). By implementing
 * the Monadic and Comonadic extensions for specific bodies, we enable:
 * - Extensional Sets: Materialized collections (Singleton, Finite Sets).
 * - Intentional Sets: Rule-based definitions (Comprehensions).
 * - Set Morphisms: Automatic lifting of functions (fmap) and filters (bind).
 *
 * @section The_Comprehension_Syntax
 * This module enables the "Aggressive" set-builder notation:
 * { f | x % S | P } -> The mapping of elements in S satisfying P by f.
 *
 * @see dedekind.sets:singleton (The Atomic Point)
 * @see dedekind.sets:expressions (The Symbolic Calculus)
 *
 * Wikipedia: Set theory, Axiom of Specification, Naive Set Theory
 */
export module dedekind.sets;

export import :boundaries;
export import :singleton;
// export import :expressions;
