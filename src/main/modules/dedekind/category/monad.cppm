/**
 * @file ontology:category.cppm
 * @brief Level 0: The Skeletal Foundation (Algebraic Bricks and Categorical
 * Cement).
 *
 * @section The_Structuralist_Unity
 * This partition unifies machine-level instructions with the abstract laws of
 * Category Theory. By treating the C++ type system as a formal proof assistant,
 * we ensure structural integrity via compile-time verification (Static
 * Mereology).
 *
 * @subsection The_Bricks: Machine Primitives
 * Fundamental types are augmented with algebraic traits, promoting raw
 * bit-fields into formal members of mathematical structures (e.g., the Integer
 * Group ℤ).
 * - identity_v      : The neutral element (0, 1, true) for a given operation.
 * - is_associative_v : Proof of grouping independence.
 * - is_commutative_v : Proof of order independence.
 *
 * @subsection The_Cement: Categorical Morphisms
 * Primitives are bound by morphisms that preserve structural invariants:
 * - IsFunctor     : A structure-preserving mapping between categories.
 * - IsMonad       : A mechanism for lifting and chaining species (The Push).
 * - IsComonad     : A mechanism for sampling and extending contexts (The Pull).
 * - IsEmbedding   : A proof of injective (1:1) type promotion.
 *
 * @section The_Bootstrapping_Strategy: Action-First Derivation
 * To resolve the circular dependency between Functors and Monads, Dedekind
 * implements an "Action-First" bootstrapping strategy.
 *
 * @note Technical Implementation Constraints:
 * In textbook Category Theory, a Monad is a Monoid in the category of
 * Endofunctors. However, C++ language constraints necessitate a divergence to
 * Kleisli Triples:
 * 1. Partial Specialization: C++ forbids partial specialization of function
 *    templates (e.g., fmap<Box>), preventing a centralized definition.
 * 2. ADL Resolution: Function templates called via name-lookup (fmap<F>) cannot
 *    trigger Argument-Dependent Lookup.
 *
 * By defining Monads/Comonads as Extension Systems (η/ε + >>= / <<=), we
 * utilize operator overloading on concrete objects. This triggers ADL, allowing
 * Level 0 to instantiate a derived 'fmap' without prior knowledge of Level 1
 * species.
 *
 * - Monadic Discovery   : fmap(f) is derived as: m >>= (η ∘ f).
 * - Comonadic Discovery : fmap(f) is derived as: w <<= (f ∘ ε).
 *
 * @section The_Highway_Notation: Directional Vectors
 * Operators represent the flow of data across the ontology:
 * - value >> into<>    : η (Unit) - Lifting a species into a context.
 * - box   << extract<> : ε (Counit) - Sampling a species from a context.
 * - box   >>= f        : Bind - Monadic composition (Left-to-Right).
 * - box   <<= f        : Extend - Comonadic composition (Right-to-Left /
 * Contextual).
 *
 * @section Structural_Inference
 * Compile-time exhaustive proofs (e.g., Bool-to-Int embedding) guarantee
 * the security of the skeletal layer before higher-level complex species
 * are initialized.
 */

module;

#include <concepts>
#include <functional>

export module dedekind.category:monad;

import :species;

namespace dedekind::category {}
