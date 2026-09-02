/**
 * @file dedekind/relational/relational.cppm
 * @brief @b dedekind.relational --- a first-class module for the relation
 *        algebra (RAlg): converse, the relative product, and the Kleene
 *        @f$(+, ;, {}^{*})@f$ surface, extracted from where they were
 * squatting.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section relational__Why_A_Module
 * Relations are first-class in Trsk (hence the name), so they deserve their own
 * module rather than living as a @c :sets partition (@c sets/relational.cppm)
 * plus a scattering of combinators inside @c order/halfspace.cppm.  This is the
 * @b SEED for that extraction (GH #792); nothing has moved yet.
 *
 * @section relational__Layering_Finding
 * The move is @b not a flat relocation.  The relation-algebra splits across two
 * DAG layers, and they cannot be collapsed into one module without a cycle:
 *   @li the @b order-free @b core (@c sets/relational.cppm's combinators) is
 *       consumed by @c sets/graph.cppm and @c sequences/path.cppm, i.e. from
 *       @b below @c order --- it must stay low;
 *   @li the @b order-dependent relation-algebra (the halfspace combinators
 *       @c converse / @c SwapPred, @c RelAnd / @c RelOr (@c operator& /
 *       @c operator+), @c ComposePred / relative-product @c operator>>,
 *       @c diagonal, @c reflexive, @c symmetric, @c is_relation, with their
 *       @c ProjProj witnesses) sits @b above @c order.
 * Putting the whole thing above @c order would make @c sets → @c relational →
 * @c order → @c sets circular.  So @c dedekind.relational (this module) is the
 * @b upper home: it depends on @c order and receives the order-dependent
 * relation-algebra; the order-free core is either left in @c sets or its
 * ordered examples relocated to tests.
 *
 * @section relational__Extraction_Plan
 * Incremental, each slice built + green (@c git @c mv where a whole file moves,
 * cut-and-relocate verbatim for embedded symbols so the diff reads as a move):
 *   1. Move the halfspace relation-algebra symbols here, repointing consumers
 *      (@c linear_algebra:transfer imports @c dedekind::order::converse /
 *      @c is_relation → @c dedekind::relational::…).
 *   2. Repoint the @c FIXME(#795) breadcrumbs (@c >> boolean-middle) that still
 *      cite the merged #786.
 *   3. Decide the namespace: @c dedekind::relational vs keeping @c
 * dedekind::order for ADL (the transport-op precedent kept the namespace on the
 * move).
 *   4. Paper: upgrade §3.3 to a Section "A Reification of RAlg" (parallels
 *      "A Reification of Set").
 */
module;

export module dedekind.relational;

import dedekind.order; // the halfspace relation-algebra to be extracted here
import dedekind.sets;  // Set<pair, L, P> --- the DSL relation carrier

namespace dedekind::relational {

// (Extraction to follow --- see @ref relational__Extraction_Plan and GH #792.)

}  // namespace dedekind::relational
