/**
 * @file dedekind/morphologies/morphologies.cppm
 * @brief Level 4: Cyclic and Archimedean morphology carriers.
 *
 * @section morphologies__The_Structural_Seal
 * « La mathématique est l'étude des structures qui sont invariantes par
 *   certains changements de forme. La morphologie est la saisie de
 *   cette stabilité. »
 *  (Mathematics is the study of structures that are invariant under
 *   certain changes of form. Morphology is the grasping of this stability.)
 *  — René Thom, 'Stabilité structurelle et morphogénèse'
 *
 * @section morphologies__Partitions
 *   - @c :archimedean --- @c IsCyclic, @c IsCyclicRing, @c IsSimplyInfinite,
 *     @c IsArchimedeanField, @c IsDedekindCompleteField, @c IsCauchy,
 *     @c IsConvergent, plus the @c CauchyPath carrier.  Hosts the
 *     @c successor / @c generator vocabulary notes (carrier-level
 *     member API ↔ axiomatic @c order::IsSuccessor; modern
 *     copula-theoretic Archimedean-generator bridge).
 *   - @c :cyclic --- the concrete cyclic carriers @c Modular<N>
 *     (@f$\mathbb{Z}/N\mathbb{Z}@f$ as a finite cyclic ring) and
 *     @c CyclicRing<T,N>.  Both expose the operational @c IsCyclic
 *     shape; @c Modular<N> is additionally registered with the
 *     axiomatic @c category::IsCyclicGroup<T, std::plus<T>>.
 *
 * @section morphologies__Taxonomic_Intent
 * While @c :algebra defines the internal harmony of a species,
 * @c :morphologies defines the valid mappings (Arrows) between distinct
 * species and the carriers that exhibit canonical morphological
 * structure (cyclic, Archimedean, Cauchy).  It establishes the criteria
 * for "Sameness" (Isomorphism) and "Structure Preservation"
 * (Homomorphism) across the Dedekind universe.
 *
 * @see dedekind.category:total --- the axiomatic side
 *      (@c IsCyclicGroup, @c IsAbelianGroup, @c IsCommutativeRing).
 * @see dedekind.algebra:field --- the field-level concept counterpart.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "Mathematics knows no races or geographic boundaries; for
 * mathematics, the cultural world is one country."
 *       -- David Hilbert, quoted in Mathematical Circles Revisited (1971)
 */
module;

export module dedekind.morphologies;

export import :archimedean;
export import :cyclic;
