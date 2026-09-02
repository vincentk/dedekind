/**
 * @file dedekind/relational/relational.cppm
 * @brief @b dedekind.relational --- a first-class module for relations, popped
 *        out of @c :sets: Codd's relational model, Tarski's calculus of
 *        relations, and graphs (endorelations).
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
 *       @f$R^{\circ}@f$, relative product @f$R;S@f$, diagonal @f$\Delta@f$,
 *       @f$R^{*}@f$; relations are dyadic (binary), one carrier under a
 *       Boolean involutive monoid.  The BASE the other two build on.
 *   @li @c :graph   --- endorelations: the graph @f$\Gamma_f@f$ of an arrow,
 *       @c is_graph_of.  A graph @b is a relation, so it tags along here.
 *
 * @section relational__Namespace
 * The extracted symbols keep the @c dedekind::sets namespace: they resolve by
 * ADL on their @c dedekind::sets::Set / @c Relation arguments, so only the
 * @b module boundary moved, not the call-site spelling (the transport-op
 * precedent of #785).  A namespace rename to @c dedekind::relational would be a
 * separate, purely mechanical churn against the ADL wall.
 */
export module dedekind.relational;

export import :dyadic;  // Tarski BASE: converse °, relative product ;, Δ, +, &
export import :tables;  // Codd: σ ⋈ ∪ ∖ ∩ (dedekind::sets namespace, by ADL)
export import :graph;   // endorelations: graph(f), is_graph_of
