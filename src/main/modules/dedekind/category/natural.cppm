/**
 * @file dedekind.category:mereology
 * @partition :mereology
 * @brief Level 0c: Embryonic Mereology (The Language of Parts).
 *
 * Following the formal axiomatization of Stanisław Leśniewski, this partition
 * introduces the "Part-Whole" relation as a foundational structural primitive.
 * In this "embryonic" stage, we define the axioms of a partial order that
 * govern any category with mereological structure.
 *
 * @section Axioms
 * 1. Reflexivity: Everything is a part of itself.
 * 2. Transitivity: A part of a part is a part of the whole.
 * 3. Antisymmetry: Two distinct things cannot be parts of each other.
 */

export module dedekind.category:natural;

import :functor;

namespace dedekind::category {}  // namespace dedekind::category
