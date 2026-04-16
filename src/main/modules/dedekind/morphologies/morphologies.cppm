/**
 * @file dedekind/morphology/morphologies.cppm
 * @module dedekind.morphology
 * @brief Level 4: The Logic of Form (Isomorphisms and Homomorphisms).
 *
 * @section The_Structural_Seal
 * « La mathématique est l'étude des structures qui sont invariantes par
 *   certains changements de forme. La morphologie est la saisie de
 *   cette stabilité. »
 *  (Mathematics is the study of structures that are invariant under
 *   certain changes of form. Morphology is the grasping of this stability.)
 *  — René Thom, 'Stabilité structurelle et morphogénèse'
 *
 * @section Taxonomic_Intent
 * While :algebra defines the internal harmony of a species, :morphology
 * defines the valid mappings (Arrows) between distinct species. It
 * establishes the criteria for "Sameness" (Isomorphism) and "Structure
 * Preservation" (Homomorphism) across the Dedekind universe.
 *
 * @details
 * This module reifies the Categorical Functors that allow us to transport
 * properties from the Integers to the Rationals, and ultimately to
 * the Real Continuum.
 *
 * @see dedekind.category:algebra
 * @see dedekind.algebra:fields
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "In dedekind.morphologies, structure is clarified by explicit
 * composition and typed interfaces." (Module-specific documentation note for
 * maintainers.)
 *       -- dedekind maintainers
 */
module;

export module dedekind.morphologies;

export import :archimedean;
export import :cyclic;
