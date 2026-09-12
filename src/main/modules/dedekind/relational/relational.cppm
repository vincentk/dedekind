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
 * All relational symbols --- including the @c Relation / @c SetFunction aliases
 * and the @c IsRelation concept --- live in the @c dedekind::relational
 * namespace, the module's OWN namespace (no @c dedekind::sets leftover).  Only
 * the underlying @b carriers stay in @c dedekind::sets: @c Set (the
 * @c Set<pair> a relation @b is) and @c Ω (the declared factor universals),
 * brought in by a @c using-declaration inside each partition.
 *
 * Migration impact of the namespace move (do NOT under-state it): ADL searches
 * the namespaces of a call's ARGUMENT types, so every named API that takes a
 * @c Set / @c Relation argument --- @c converse, @c select, @c set_difference,
 * @c natural_join, @c preimage, @c reflexive, @c symmetric --- @b was
 * ADL-reachable from @c dedekind::sets and now is @b not; those bare calls need
 * qualification or a @c using.  The infix operators (@c >> / @c + / @c & / the
 * set-difference @c -) are the same story.  The ONLY symbols that never
 * ADL-reached @c sets are @c graph (called on an @b arrow, whose type is not in
 * @c sets) and the relation/function @b concepts (used as type traits, not
 * calls).  In practice consumers add @c using @c namespace @c
 * dedekind::relational once --- the explicit price for a module whose namespace
 * is honest about where its symbols live.
 */
export module dedekind.relational;

export import :dyadic;  // Tarski BASE: converse °, relative product ;, Δ, +, &
export import :tables;  // Codd: σ ⋈ ∪ ∖ ∩ (dedekind::relational namespace)
export import :graph;   // graphs of arrows (binary relations): graph(f),
                        // is_graph_of
