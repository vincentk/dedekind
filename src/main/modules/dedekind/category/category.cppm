/**
 * @file dedekind/category/category.cppm
 * @module dedekind.category
 * @brief Level 0--4: the categorical bedrock of the project --- atoms,
 *        universal constructions, total and partial algebra, the
 *        functor / natural-transformation / monad / Kleisli spine, and
 *        the ETCS facade for sets-as-subobjects.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.

 *
 * @quote
 * "La structure d’une chose n’est nullement une chose qu’on puisse « saisir »
 *  si l’on n’est pas d’abord prêt à s’y fondre."
 *  (The structure of a thing is by no means something that one can "grasp"
 *  if one is not first prepared to melt into it.)
 *  — Alexander Grothendieck, Récoltes et Semailles.
 *
 * The table of contents for the dedekind.category module is as follows:
 *
 * 0.   ATOMS (:species, :morphism, :mereology, :logic): Reification of machine
 *      primitives, the Skeletal Arrow, and the grounding of Truth via Parthood.
 * 0.5  INFRASTRUCTURE (:posetal, :small, :cartesian, :limit, :pullback):
 *      The universal category-theoretic constructions.
 * 1.   IDEALS (:total, :action): The pure laws of total algebra.
 * 2.   BRIDGE (:functor, :natural, :monad, :kleisli): The Functorial Spine
 *      enabling the Monadic Kleisli lift.
 * 3.   REALITY (:numeric, :partial): Reconciling theory
 *      with hardware-level indeterminacy and IEEE 754.
 * 4.   SKELETAL SIGNATURES (:etcs): The Set-Theoretic Interface (ETCS).
 *
 * @see Lawvere, F.W. (1964) "An Elementary Theory of the Category of Sets"
 * @see McLarty, C. (1993) "Numbers can be just what they have to"
 *
 * @note "It is less than four years since cohomological methods were
 * introduced into algebraic geometry, and it seems certain that they are to
 * overflow the part of mathematics in the coming years, from the foundations up
 * to the most advanced parts."
 *       -- Alexander Grothendieck, ICM Edinburgh lecture (1958)
 */

export module dedekind.category;

// Level 0: The Taxonomic Bricks (Atoms)
export import :species;    // The Raw Machine Data
export import :morphism;   // The Base Arrow (Prerequisite for everything)
export import :mereology;  // Parthood (Prerequisite for Logic/Sets)
export import :logic;      // The Subobject Classifier (Truth)

// Level 0.5: The Structural Infrastructure
export import :posetal;    // Partial Orders (Verified by Logic)
export import :small;      // Small Categories
export import :discrete;   // The Discrete Category (Points as Arrows)
export import :limit;      // Universal Limits
export import :cartesian;  // Products/Coproducts, CCC Foundations
export import :topoi;      // Internal Logic of the Topos
export import :pullback;   // The Universal Construction "Sink"

// Level 1: The Ideal (Total Algebra)
export import :total;
export import :action;

// Level 2: The Bridge (Functorial Spine)
export import :functor;
export import :natural;
export import :adjunction;  // Free / Forgetful pair, IsAdjunction (Pierce-style separate section)
export import :monad;
export import :kleisli;

// Level 3: The Reality (Hardware -> Abstraction)
export import :numeric;  // Concrete Hardware Logic (IEEE 754)
export import :partial;  // The Final Abstracted Partial Interface

// Level 4: The Skeletal Signatures (Synthesis)
export import :etcs;  // The World of Sets (Importing Partial + Mereology)

namespace dedekind::category {
// Top-level category-theoretic aliases and universal compositions.
}
