/**
 * @file sets.cppm
 * @brief The Dedekind Set Model: Concrete Collections and Set Morphisms.
 *
 * Copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Set_Theory: Implementation of Mereological Relations
 * This module provides concrete implementations for the abstract laws
 * defined in dedekind.ontology. While the ontology defines the requirements
 * for a set, this module provides the mechanisms to construct them.
 *
 * @details
 * In the Dedekind architecture, set theory bridges logic (Level -1)
 * and magnitude (Level 2). By implementing monadic and comonadic
 * extensions for specific structures, we enable:
 * - Extensional Sets: Materialized collections (Singleton, Finite Sets).
 * - Intensional Sets: Rule-based definitions (Comprehensions).
 * - Set Morphisms: Automatic lifting of functions (fmap) and filters (bind).
 *
 * @section The_Comprehension_Syntax
 * This module enables symbolic set-builder notation:
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
export import :expressions;
