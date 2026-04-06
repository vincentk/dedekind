/**
 * @file dedekind/category.cppm
 * @module dedekind.category
 * @brief The Universal Category Theory Substrate.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Implementation_Philosophy
 * "La structure d’une chose n’est nullement une chose qu’on puisse « saisir » 
 *  si l’on n’est pas d’abord prêt à s’y fondre."
 *  (The structure of a thing is by no means something that one can "grasp" 
 *  if one is not first prepared to melt into it.)
 *  — Alexander Grothendieck, Récoltes et Semailles.
 * 
 * This module implements a structuralist approach based on the Elementary 
 * Theory of the Category of Sets (ETCS). We use C++23 modules and concepts 
 * to treat mathematical objects as positions within a system of relations 
 * (morphisms) rather than traditional object-oriented hierarchies.
 *
 * @section Fractal_Build_Order
 * To manage the "template tax" and ensure ontological decidability, the 
 * implementation follows a fractal progression across a Directed Acyclic 
 * Graph (DAG) of module partitions:
 *
 * 1.  ATOMS (:species, :logic, :morphisms): Reification of machine primitives 
 *     and the Subobject Classifier (Ω).
 * 2.  IDEALS (:total, :algebra_total): Defining the pure laws of total algebra.
 * 3.  BRIDGE (:functorial, :kleisli): Reifying algebraic laws as functors to 
 *     enable the Monadic Kleisli lift.
 * 4.  REALITY (:partial, :algebra_partial, :numeric): Reconciling the theory 
 *     with hardware-level indeterminacy and IEEE 754.
 * 5.  SIGNATURES (:mereology, :etcs): Establishing the skeletal vocabulary 
 *     for the high-level ontologies of Parthood and Sets.
 *
 * @see Lawvere, F.W. (1964) "An Elementary Theory of the Category of Sets"
 * @see McLarty, C. (1993) "Numbers can be just what they have to"
 */

export module dedekind.category;

// Level 0: The Taxonomic Bricks (Atoms)
export import :species;    // Machine Atoms (int, double, bool)
export import :logic;      // The Subobject Classifier (Omega)
export import :posetal;    // Partial Orders (Pre-requisite for Lattices/Mereology)
export import :morphism;  // The Base Arrow Signature (f: A -> B)
export import :small;      // Small Categories (Objects + Arrows)
export import :cartesian;  // Products/Coproducts (Binary)
export import :limit;     // Initial/Terminal (Empty)
export import :pullback;   // Universal Constructions (Equalizers/Intersections)

// Level 1: The Ideal (Total)
export import :total;      // Total Morphisms and Endomorphisms
export import :algebra_total; // Monoids, Rigs, Semirings on Atoms
export import :actions;    // Monoid Actions / Modules (Semi-Modules)

// Level 2: The Bridge (Reification)
export import :functor;    // Mappings between categories
export import :natural;    // Natural Transformations
export import :monad;      // Monads (η, μ) as algebraic triples
export import :kleisli;    // The Monadic Composition Engine (Lifting)

// Level 3: The Reality (Partial/Numeric)
export import :partial;    // Partial Functions and Algebras
export import :algebra_partial; // Monoids/Rigs on Lifted Types
export import :numeric;    // Reconciling Algebra with IEEE 754/Overflows

// Level 4: The Skeletal Signatures
export import :mereology;  // Parthood as a Sub-structural Relation
export import :etcs;       // The Elementary Theory of the Category of Sets

namespace dedekind::category {
    // Top-level category-theoretic aliases and universal compositions.
}
