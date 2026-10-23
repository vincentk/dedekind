// jlt-tool: the Jlt checker's checks that need code rather than a pattern.
//
//   jlt-tool --check=<name> --scope=<namespace> <file> [-p <build>] [--
//   <flags>]
//
// Prints one finding per line as "path:line:col"; the driver counts them.
// Checks:
//   mutual-recursion  functions of the scope namespace, defined in the file,
//                     that lie on a call cycle of length >= 2 (a function
//                     calling itself is the clang-query rule's job).  Rule 3:
//                     recursion only folds, and a cycle through other
//                     functions is recursion the compiler does not bound.
//   definition        Rule 2, name-level: a function template's body may use,
//                     on a value whose type depends on a template parameter,
//                     only an operation some requirement of its constraints
//                     names (the constraints followed through their concepts;
//                     a few standard concepts and traits mapped by hand).  A
//                     mechanical sanity check, not a proof: it reports only
//                     what it is sure no requirement names, and stays silent
//                     where it cannot tell.  What it does not see is listed
//                     at the check.
#include <algorithm>
#include <functional>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprConcepts.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Analysis/CallGraph.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

namespace {

llvm::cl::OptionCategory category("jlt-tool options");
llvm::cl::opt<std::string> check("check", llvm::cl::desc("the check to run"),
                                 llvm::cl::Required, llvm::cl::cat(category));
llvm::cl::opt<std::string> scope("scope",
                                 llvm::cl::desc("the namespace judged"),
                                 llvm::cl::Required, llvm::cl::cat(category));

/// Whether `d` sits inside a namespace named `scope`, at any depth.
bool inScope(const clang::Decl* d) {
  for (const clang::DeclContext* c = d->getDeclContext(); c; c = c->getParent())
    if (const auto* ns = llvm::dyn_cast<clang::NamespaceDecl>(c))
      if (ns->getName() == scope) return true;
  return false;
}

/// The defining redeclaration of `f`, if any: the call graph names a function
/// by its canonical declaration, which may be a prototype in a header, and the
/// check judges and reports the definition.
const clang::FunctionDecl* definitionOf(const clang::FunctionDecl* f) {
  return f ? f->getDefinition() : nullptr;
}

/// Whether a definition is judged: in the main file, inside the scope.
bool judged(const clang::FunctionDecl* def, const clang::SourceManager& sm) {
  return def && sm.isInMainFile(def->getLocation()) && inScope(def);
}

/// Strongly connected components of the call graph (Tarjan); each component
/// of two or more judged functions is a cycle of mutual recursion.
class MutualRecursion : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    clang::CallGraph graph;
    graph.addToCallGraph(ctx.getTranslationUnitDecl());
    const clang::SourceManager& sm = ctx.getSourceManager();

    std::map<const clang::CallGraphNode*, int> index, low;
    std::set<const clang::CallGraphNode*> onStack;
    std::vector<const clang::CallGraphNode*> stack;
    int next = 0;
    std::set<const clang::FunctionDecl*> findings;

    std::function<void(const clang::CallGraphNode*)> visit =
        [&](const clang::CallGraphNode* v) {
          index[v] = low[v] = next++;
          stack.push_back(v);
          onStack.insert(v);
          for (const clang::CallGraphNode::CallRecord& rec : *v) {
            const clang::CallGraphNode* w = rec.Callee;
            if (!index.count(w)) {
              visit(w);
              low[v] = std::min(low[v], low[w]);
            } else if (onStack.count(w)) {
              low[v] = std::min(low[v], index[w]);
            }
          }
          if (low[v] != index[v]) return;
          std::vector<const clang::CallGraphNode*> component;
          for (;;) {
            const clang::CallGraphNode* w = stack.back();
            stack.pop_back();
            onStack.erase(w);
            component.push_back(w);
            if (w == v) break;
          }
          if (component.size() < 2) return;  // a self-call is another rule's
          for (const clang::CallGraphNode* w : component)
            if (const clang::FunctionDecl* def = definitionOf(
                    llvm::dyn_cast_or_null<clang::FunctionDecl>(w->getDecl())))
              if (judged(def, sm)) findings.insert(def);
        };

    for (const auto& entry : graph)
      if (entry.second && !index.count(entry.second.get()))
        visit(entry.second.get());

    for (const clang::FunctionDecl* f : findings) {
      const clang::SourceLocation loc = sm.getExpansionLoc(f->getLocation());
      llvm::outs() << sm.getFilename(loc) << ':'
                   << sm.getExpansionLineNumber(loc) << ':'
                   << sm.getExpansionColumnNumber(loc) << '\n';
    }
  }
};

// ---------------------------------------------------------------------------
// definition: Rule 2 at the level of names
// ---------------------------------------------------------------------------

/// The names an expression uses as operations: operator spellings ("+", "==",
/// "()", "[]"), called function names, member names.  Collected from the
/// requirements of the constraints (every operation they mention) and from a
/// body (only operations whose type depends on a template parameter).
class NameCollector : public clang::RecursiveASTVisitor<NameCollector> {
 public:
  std::set<std::string> names;
  bool dependentOnly = false;

  bool VisitBinaryOperator(clang::BinaryOperator* e) {
    if (!dependentOnly || e->isTypeDependent())
      names.insert(clang::BinaryOperator::getOpcodeStr(e->getOpcode()).str());
    return true;
  }
  bool VisitUnaryOperator(clang::UnaryOperator* e) {
    if (!dependentOnly || e->isTypeDependent())
      names.insert(clang::UnaryOperator::getOpcodeStr(e->getOpcode()).str());
    return true;
  }
  bool VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* e) {
    if (!dependentOnly || e->isTypeDependent())
      names.insert(clang::getOperatorSpelling(e->getOperator()));
    return true;
  }
  bool VisitArraySubscriptExpr(clang::ArraySubscriptExpr* e) {
    if (!dependentOnly || e->isTypeDependent()) names.insert("[]");
    return true;
  }
  bool VisitCallExpr(clang::CallExpr* e) {
    if (dependentOnly && !e->isTypeDependent()) return true;
    const clang::Expr* callee = e->getCallee()->IgnoreParenImpCasts();
    if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee))
      names.insert(u->getName().getAsString());
    else if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(callee))
      names.insert(llvm::isa<clang::VarDecl>(d->getDecl())
                       ? std::string("()")  // a call on a value
                       : d->getNameInfo().getAsString());
    else if (const auto* m =
                 llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(callee))
      names.insert(m->getMember().getAsString());
    else if (const auto* m = llvm::dyn_cast<clang::MemberExpr>(callee))
      names.insert(m->getMemberNameInfo().getAsString());
    else
      names.insert("()");  // a call on a value: its call operator
    return true;
  }
  bool VisitCXXDependentScopeMemberExpr(clang::CXXDependentScopeMemberExpr* e) {
    names.insert(e->getMember().getAsString());
    return true;
  }
};

/// What a standard concept or trait permits beyond what its body says: the
/// trait-based ones (integral, ...) permit every built-in operator, which no
/// requires-expression spells out.  A hand-made list, the Pareto choice.
struct Permits {
  bool everything = false;  // every built-in operator
  std::set<std::string> names;
};
Permits permitsOfStd(llvm::StringRef name) {
  Permits p;
  static const std::set<std::string> all = {"integral",
                                            "signed_integral",
                                            "unsigned_integral",
                                            "floating_point",
                                            "is_integral_v",
                                            "is_arithmetic_v",
                                            "is_floating_point_v",
                                            "is_signed_v",
                                            "is_unsigned_v",
                                            "is_integral",
                                            "is_arithmetic"};
  if (all.count(name.str())) p.everything = true;
  if (name == "equality_comparable" || name == "equality_comparable_with")
    p.names = {"==", "!="};
  if (name == "totally_ordered" || name == "totally_ordered_with")
    p.names = {"==", "!=", "<", ">", "<=", ">="};
  if (name == "three_way_comparable" || name == "three_way_comparable_with")
    p.names = {"<=>", "==", "!=", "<", ">", "<=", ">="};
  if (name == "invocable" || name == "regular_invocable" ||
      name == "predicate" || name == "relation" ||
      name == "equivalence_relation" || name == "strict_weak_order")
    p.names = {"()"};
  if (name == "swappable" || name == "swappable_with") p.names = {"swap"};
  if (name == "semiregular" || name == "regular" || name == "copyable" ||
      name == "movable" || name == "assignable_from")
    p.names = {"="};  // a value may be assigned to (Rule 1: a local)
  if (name.ends_with("range") || name == "view")
    p.names = {"begin", "end", "size", "empty"};
  return p;
}

/// Whether a declaration lives in namespace std (any depth).
bool inStd(const clang::Decl* d) {
  for (const clang::DeclContext* c = d->getDeclContext(); c; c = c->getParent())
    if (const auto* ns = llvm::dyn_cast<clang::NamespaceDecl>(c))
      if (ns->isStdNamespace()) return true;
  return false;
}

/// The vocabulary a template's constraints grant: every name a requirement
/// mentions, the constraints followed through their concepts, plus the
/// standard permissions.  `opaque` when a constraint could not be read.
struct Vocabulary {
  std::set<std::string> names;
  bool everything = false;
  bool opaque = false;
  std::set<const clang::ConceptDecl*> seen;
};

void gather(const clang::Expr* e, Vocabulary& v);

void gatherConcept(const clang::ConceptDecl* c, Vocabulary& v) {
  if (!c || !v.seen.insert(c).second) return;
  if (inStd(c)) {
    // A standard concept is read from the hand-made list, not its body: the
    // bodies use library-internal shapes this check does not read.
    const Permits p = permitsOfStd(c->getName());
    v.everything |= p.everything;
    v.names.insert(p.names.begin(), p.names.end());
    return;
  }
  if (const clang::Expr* body = c->getConstraintExpr())
    gather(body, v);
  else
    v.opaque = true;
}

/// Walk a constraint expression: concept-ids are followed, requires-expressions
/// contribute every name their requirements mention, trait references their
/// standard permissions.
void gather(const clang::Expr* e, Vocabulary& v) {
  if (!e) return;
  e = e->IgnoreParenImpCasts();
  if (const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e)) {
    gatherConcept(cs->getNamedConcept(), v);
    // Its written arguments name operations too: Fires<decltype(law(a, b))>.
    if (const auto* written = cs->getTemplateArgsAsWritten())
      for (const clang::TemplateArgumentLoc& arg : written->arguments()) {
        const clang::TemplateArgument& a = arg.getArgument();
        NameCollector nc;
        if (a.getKind() == clang::TemplateArgument::Type) {
          if (const auto* dt = a.getAsType()->getAs<clang::DecltypeType>())
            nc.TraverseStmt(dt->getUnderlyingExpr());
        } else if (a.getKind() == clang::TemplateArgument::Expression) {
          nc.TraverseStmt(a.getAsExpr());
        }
        v.names.insert(nc.names.begin(), nc.names.end());
      }
    return;
  }
  if (const auto* r = llvm::dyn_cast<clang::RequiresExpr>(e)) {
    for (const clang::concepts::Requirement* req : r->getRequirements()) {
      if (const auto* er =
              llvm::dyn_cast<clang::concepts::ExprRequirement>(req)) {
        if (!er->isExprSubstitutionFailure()) {
          NameCollector nc;
          nc.TraverseStmt(const_cast<clang::Expr*>(er->getExpr()));
          v.names.insert(nc.names.begin(), nc.names.end());
        }
        if (er->getReturnTypeRequirement().isTypeConstraint())
          gather(er->getReturnTypeRequirement()
                     .getTypeConstraint()
                     ->getImmediatelyDeclaredConstraint(),
                 v);
      } else if (const auto* nr =
                     llvm::dyn_cast<clang::concepts::NestedRequirement>(req)) {
        if (!nr->hasInvalidConstraint()) gather(nr->getConstraintExpr(), v);
      }
    }
    return;
  }
  if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
    gather(b->getLHS(), v);
    gather(b->getRHS(), v);
    return;
  }
  if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e)) {
    gather(u->getSubExpr(), v);
    return;
  }
  // A trait: std::is_integral_v<T> and the like.
  llvm::StringRef traitName;
  if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e)) {
    if (inStd(d->getDecl())) traitName = d->getDecl()->getName();
  } else if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(e)) {
    traitName = u->getName().getAsIdentifierInfo()
                    ? u->getName().getAsIdentifierInfo()->getName()
                    : llvm::StringRef();
  } else if (llvm::isa<clang::SubstNonTypeTemplateParmExpr>(e) ||
             llvm::isa<clang::CXXBoolLiteralExpr>(e) ||
             llvm::isa<clang::IntegerLiteral>(e) ||
             llvm::isa<clang::SizeOfPackExpr>(e) ||
             llvm::isa<clang::UnaryExprOrTypeTraitExpr>(e)) {
    return;  // a literal or a substituted value: nothing to name
  } else if (llvm::isa<clang::CXXFoldExpr>(e) ||
             llvm::isa<clang::ParenListExpr>(e)) {
    return;  // folds over packs: beyond the name level, stay silent
  } else {
    v.opaque = true;  // a constraint of a shape this check does not read
    return;
  }
  if (!traitName.empty()) {
    const Permits p = permitsOfStd(traitName);
    v.everything |= p.everything;
    v.names.insert(p.names.begin(), p.names.end());
  }
}

/// The constraints of a function template: type constraints on its
/// parameters, the requires clause on the parameter list, the trailing one.
void gatherParameters(const clang::TemplateParameterList* params,
                      Vocabulary& v) {
  if (!params) return;
  for (const clang::NamedDecl* p : *params)
    if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
      if (const clang::TypeConstraint* tc = tp->getTypeConstraint())
        gather(tc->getImmediatelyDeclaredConstraint(), v);
  gather(params->getRequiresClause(), v);
}

Vocabulary vocabularyOf(const clang::FunctionTemplateDecl* ft) {
  Vocabulary v;
  // A member template's vocabulary includes its enclosing class templates'
  // constraints: their parameters are in scope in its body.
  for (const clang::DeclContext* c = ft->getDeclContext(); c;
       c = c->getParent()) {
    if (const auto* ps =
            llvm::dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(c))
      gatherParameters(ps->getTemplateParameters(), v);
    else if (const auto* rd = llvm::dyn_cast<clang::CXXRecordDecl>(c))
      if (const clang::ClassTemplateDecl* ct = rd->getDescribedClassTemplate())
        gatherParameters(ct->getTemplateParameters(), v);
  }
  for (const clang::NamedDecl* p : *ft->getTemplateParameters())
    if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
      if (const clang::TypeConstraint* tc = tp->getTypeConstraint())
        gather(tc->getImmediatelyDeclaredConstraint(), v);
  gather(ft->getTemplateParameters()->getRequiresClause(), v);
  gather(ft->getTemplatedDecl()->getTrailingRequiresClause().ConstraintExpr, v);
  return v;
}

/// Whether a value's type is a template parameter itself (through references
/// and cv): the value whose vocabulary Rule 2 restricts.  A value of some
/// other dependent type, a library aggregate or a call's result, carries that
/// type's vocabulary, not the parameter's.
bool isParameterTyped(const clang::Expr* e) {
  if (!e) return false;
  clang::QualType t = e->getType();
  if (t.isNull()) return false;
  t = t.getNonReferenceType().getUnqualifiedType();
  return t->isTemplateTypeParmType() ||
         llvm::isa<clang::SubstTemplateTypeParmType>(t.getTypePtr());
}

bool anyParameterTyped(llvm::ArrayRef<const clang::Expr*> es) {
  return std::any_of(es.begin(), es.end(), isParameterTyped);
}

/// Whether a template is constrained: a type constraint on a parameter, a
/// requires clause, or a trailing one.
bool constrained(const clang::FunctionTemplateDecl* ft) {
  if (ft->getTemplateParameters()->getRequiresClause() ||
      ft->getTemplatedDecl()->getTrailingRequiresClause().ConstraintExpr)
    return true;
  for (const clang::NamedDecl* p : *ft->getTemplateParameters())
    if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
      if (tp->hasTypeConstraint()) return true;
  return false;
}

/// Calls that are not vocabulary of the value: a standard function (move,
/// forward, make_optional, ...); a non-template function, whose parameter
/// types are concrete; the library's own constrained templates, whose
/// constraints are checked at their definition (the obligation transfers);
/// and a constexpr template returning bool, a trait in function spelling
/// (involutive(f), holds_on<2>(law, rel, lo, hi)), which Rule 2 names beside
/// the concepts.  That last one is a heuristic: a bool-returning function
/// that works on the value escapes it.
bool exemptCall(const clang::CallExpr* call) {
  const clang::Expr* callee = call->getCallee()->IgnoreParenImpCasts();
  llvm::SmallVector<const clang::NamedDecl*, 4> cands;
  if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee)) {
    for (const clang::NamedDecl* d : u->decls()) cands.push_back(d);
  } else if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(callee)) {
    cands.push_back(d->getDecl());
  } else {
    return false;
  }
  if (cands.empty()) return false;  // found by ADL alone: judged by name
  for (const clang::NamedDecl* cand : cands) {
    if (inStd(cand)) continue;
    if (const auto* ft = llvm::dyn_cast<clang::FunctionTemplateDecl>(cand)) {
      if (constrained(ft)) continue;
      const clang::FunctionDecl* f = ft->getTemplatedDecl();
      if (f->isConstexpr() && f->getReturnType()->isBooleanType()) continue;
      return false;  // an unconstrained template: judged by name
    }
    if (llvm::isa<clang::FunctionDecl>(cand)) continue;  // concrete parameters
    return false;
  }
  return true;
}

/// The operations of a body, with their locations.
class BodyOperations : public clang::RecursiveASTVisitor<BodyOperations> {
 public:
  struct Op {
    std::string name;
    clang::SourceLocation loc;
    bool builtin;  // an operator a trait-based concept permits wholesale
  };
  std::vector<Op> ops;
  std::set<const clang::Expr*> asCallee;

  // Control operators act on results, not on the value: &&, ||, !, the comma.
  // Only an operation with an operand of parameter type is the parameter's
  // vocabulary.  Control operators (&&, ||, !, the comma) act on results.
  bool VisitBinaryOperator(clang::BinaryOperator* e) {
    if (e->isLogicalOp() || e->isCommaOp()) return true;
    if (anyParameterTyped({e->getLHS(), e->getRHS()}))
      ops.push_back({clang::BinaryOperator::getOpcodeStr(e->getOpcode()).str(),
                     e->getOperatorLoc(), true});
    return true;
  }
  bool VisitUnaryOperator(clang::UnaryOperator* e) {
    if (e->getOpcode() != clang::UO_LNot && isParameterTyped(e->getSubExpr()))
      ops.push_back({clang::UnaryOperator::getOpcodeStr(e->getOpcode()).str(),
                     e->getOperatorLoc(), true});
    return true;
  }
  bool VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* e) {
    llvm::SmallVector<const clang::Expr*, 2> args(e->arguments().begin(),
                                                  e->arguments().end());
    if (anyParameterTyped(args))
      ops.push_back({clang::getOperatorSpelling(e->getOperator()),
                     e->getOperatorLoc(), true});
    return true;
  }
  bool VisitArraySubscriptExpr(clang::ArraySubscriptExpr* e) {
    if (isParameterTyped(e->getBase()))
      ops.push_back({"[]", e->getRBracketLoc(), true});
    return true;
  }
  bool VisitCallExpr(clang::CallExpr* e) {
    if (llvm::isa<clang::CXXOperatorCallExpr>(e)) return true;  // above
    const clang::Expr* callee = e->getCallee()->IgnoreParenImpCasts();
    if (const auto* m =
            llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(callee)) {
      asCallee.insert(m);
      if (isParameterTyped(m->getBase()))
        ops.push_back({m->getMember().getAsString(), m->getMemberLoc(), false});
      return true;
    }
    if (isParameterTyped(callee)) {  // a value of parameter type, called
      ops.push_back({"()", e->getExprLoc(), false});
      return true;
    }
    if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(callee))
      if (llvm::isa<clang::VarDecl>(d->getDecl()))
        return true;  // a call on a value of some other type: its vocabulary
    llvm::SmallVector<const clang::Expr*, 4> args(e->arguments().begin(),
                                                  e->arguments().end());
    if (!anyParameterTyped(args) || exemptCall(e)) return true;
    if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee))
      ops.push_back({u->getName().getAsString(), u->getNameLoc(), false});
    else if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(callee))
      ops.push_back({d->getNameInfo().getAsString(), d->getLocation(), false});
    return true;
  }
  bool VisitCXXDependentScopeMemberExpr(clang::CXXDependentScopeMemberExpr* e) {
    if (!asCallee.count(e) && isParameterTyped(e->getBase()))
      ops.push_back({e->getMember().getAsString(), e->getMemberLoc(), false});
    return true;
  }
};

/// The check: every function template of the scope, defined in the file.
/// Not seen (by choice, this being a sanity check): class templates' ordinary
/// members, which use the class's parameters under the class's constraints;
/// lambdas; operations reached through a non-dependent helper; whether a
/// named operation has the right arity or types.
class Definition : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    const clang::SourceManager& sm = ctx.getSourceManager();
    Finder finder{sm, *this};
    finder.TraverseDecl(ctx.getTranslationUnitDecl());
  }

  struct Finder : clang::RecursiveASTVisitor<Finder> {
    const clang::SourceManager& sm;
    Definition& owner;
    Finder(const clang::SourceManager& s, Definition& o) : sm(s), owner(o) {}
    bool shouldVisitTemplateInstantiations() const { return false; }
    bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl* ft) {
      const clang::FunctionDecl* f = ft->getTemplatedDecl();
      if (!f || !f->doesThisDeclarationHaveABody() || f->isDeleted() ||
          f->isDefaulted())
        return true;
      if (const auto* m = llvm::dyn_cast<clang::CXXMethodDecl>(f))
        if (m->getParent()->isLambda()) return true;
      if (!sm.isInMainFile(ft->getLocation()) || !inScope(ft)) return true;
      owner.judge(ft, sm);
      return true;
    }
  };

  void judge(const clang::FunctionTemplateDecl* ft,
             const clang::SourceManager& sm) {
    // The declared constraints only: a requires-expression or concept test
    // inside the body is not one (by decision), so a body that probes an
    // operation instead of requiring it is a finding.
    const Vocabulary v = vocabularyOf(ft);
    if (v.opaque) return;  // a constraint this check cannot read: silence
    BodyOperations body;
    body.TraverseStmt(ft->getTemplatedDecl()->getBody());
    for (const BodyOperations::Op& op : body.ops) {
      if (v.names.count(op.name)) continue;
      if (op.builtin && v.everything) continue;
      const clang::SourceLocation loc = sm.getExpansionLoc(op.loc);
      llvm::outs() << sm.getFilename(loc) << ':'
                   << sm.getExpansionLineNumber(loc) << ':'
                   << sm.getExpansionColumnNumber(loc) << ": '" << op.name
                   << "' is not named by the constraints of "
                   << ft->getNameAsString() << '\n';
    }
  }
};

class Action : public clang::ASTFrontendAction {
 public:
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance&, llvm::StringRef) override {
    if (check == "mutual-recursion") return std::make_unique<MutualRecursion>();
    if (check == "definition") return std::make_unique<Definition>();
    llvm::errs() << "jlt-tool: unknown check " << check << '\n';
    return nullptr;
  }
};

}  // namespace

int main(int argc, const char** argv) {
  auto parser =
      clang::tooling::CommonOptionsParser::create(argc, argv, category);
  if (!parser) {
    llvm::errs() << llvm::toString(parser.takeError());
    return 2;
  }
  clang::tooling::ClangTool tool(parser->getCompilations(),
                                 parser->getSourcePathList());
  return tool.run(clang::tooling::newFrontendActionFactory<Action>().get());
}
