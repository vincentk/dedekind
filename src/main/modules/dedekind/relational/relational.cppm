/**
 * @file dedekind/relational/relational.cppm
 * @brief @b dedekind.relational --- a first-class module for relations, popped
 *        out of @c :sets: Codd's relational model, Tarski's calculus of
 *        relations, and graphs (of arrows, binary relations).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section relational__Why_A_Module
 * Relations are first-class in Trsk, so they deserve their own module rather
 * than living as a scattering of @c :sets partitions (@c relational, @c graph)
 * plus combinators buried in @c order/halfspace.cppm.  This module is the
 * @b Reification of RAlg (GH #792), the relational counterpart to the
 * @c :sets reification of Set.
 *
 * @section relational__Layering
 * @c category → @c sets → @b relational → @c order.  The module sits @b above
 * @c sets (it builds on the @c Set / @c Relation DSL) and @b below @c order:
 * @c order/halfspace's ordered relation-algebra (@c converse, the relative
 * product, @c diagonal) moves DOWN into @ref relational__Partitions so the
 * ordered predicates can @b consume it, not the other way around.  The
 * order-free relation combinators were consumed from below @c order (by
 * @c sequences::path, @c linear_algebra::transfer), which is exactly why the
 * whole surface belongs on this low rung and not above @c order.
 *
 * @section relational__Partitions
 *   @li @c :tables  --- @b Codd's relational model (1970): the query algebra
 *       σ select, ⋈ natural_join, ∪ set_union, ∖ set_difference, ∩; relations
 *       are n-ary tables of tuples.
 *   @li @c :dyadic  --- @b Tarski's calculus of relations (1941): converse
 *       @f$R^{\circ}@f$, relative product @f$R;S@f$ (over a Boolean middle),
 *       diagonal @f$\Delta@f$, union @f$+@f$ / meet @f$\&@f$; relations are
 *       dyadic (binary), one carrier under a Boolean involutive monoid.  The
 *       BASE the other two build on.  (The reflexive-transitive closure
 *       @f$R^{*}@f$ is not yet a provided operator; FIXME(#786).)
 *   @li @c :graph   --- graphs of arrows as binary relations: @f$\Gamma_f@f$,
 *       @c is_graph_of.  A graph @b is a relation, so it tags along here.
 *
 * @section relational__Namespace
 * All extracted symbols land in the @c dedekind::sets namespace (NOT
 * @c dedekind::relational): they resolve by ADL on their @c dedekind::sets::Set
 * / @c Relation arguments, which is what keeps the unqualified operators
 * (@c >> / @c + / @c &) and the bare @c converse / @c select call sites
 * working. For @c :tables and @c :graph this is a pure @b module move --- they
 * were already @c dedekind::sets.  For @c :dyadic the namespace ALSO moved
 * (@c dedekind::order → @c dedekind::sets, since it was buried in the
 * halfspace), so its callers repoint @c dedekind::order:: → @c dedekind::sets::
 * (transfer, halfspace_transport).  A rename to @c dedekind::relational would
 * be a separate mechanical churn against the ADL wall.
 */
export module dedekind.relational;

export import :dyadic;  // Tarski BASE: converse °, relative product ;, Δ, +, &
export import :tables;  // Codd: σ ⋈ ∪ ∖ ∩ (dedekind::sets namespace, by ADL)
export import :graph;   // graphs of arrows (binary relations): graph(f),
                        // is_graph_of
