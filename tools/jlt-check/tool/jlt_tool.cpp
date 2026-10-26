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
//   inline-requires   Rule 2: an `if` (constexpr or not) whose condition
//                     contains a requires-expression tests a constraint the
//                     body did not declare, which Rule 2 does not count
//                     (#1068); the fix is a named shape, which the definition
//                     check then reads.
//   undecided-resolution
//                     Rule 2, GHC's rule for overlapping instances: in a
//                     function template, a call at a value of parameter type
//                     to an overload set whose candidates are separated by a
//                     concept the enclosing constraints do not mention (nor
//                     negate) resolves differently at different
//                     instantiations, a dependence on a constraint the
//                     template did not declare.  A call its constraints name
//                     is read through the dictionary and is not judged.
//   inventory         not a rule: the syntax a scope uses, one line per AST
//                     node kind (declarations, statements and expressions,
//                     type forms as written) with its count, for diffing
//                     against the kernel's grammar (#1079).
//   inheritance       Jlt has no subtyping: a class with a base that is not
//                     an empty class, or with a virtual function, is a
//                     finding.  An empty base (`: std::true_type`,
//                     `: is_x<Y>`) is a registration spelled by inheritance,
//                     read as the fact it states.
#include <algorithm>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprConcepts.h"
#include "clang/AST/ParentMapContext.h"
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
  if (name == "regular") p.names = {"=", "==", "!="};  // and compared
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
  } else if (const auto* fold = llvm::dyn_cast<clang::CXXFoldExpr>(e)) {
    gather(fold->getPattern(), v);  // (C<Ts> && ...): the pattern's grant
    return;
  } else if (llvm::isa<clang::ParenListExpr>(e)) {
    v.opaque = true;  // a shape this check does not read: silence
    return;
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

  // A lambda in the body is its own function (exempt, by the documented
  // choice): its operations are not the enclosing template's.
  bool TraverseLambdaExpr(clang::LambdaExpr*) { return true; }

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

// ---------------------------------------------------------------------------
// inline-requires: a constraint is declared, not tested in a body
// ---------------------------------------------------------------------------

/// Whether `e` contains a requires-expression, through operators and parens.
bool containsRequires(const clang::Expr* e) {
  if (!e) return false;
  if (llvm::isa<clang::RequiresExpr>(e->IgnoreParenImpCasts())) return true;
  for (const clang::Stmt* c : e->children())
    if (const auto* ce = llvm::dyn_cast_or_null<clang::Expr>(c))
      if (containsRequires(ce)) return true;
  return false;
}

/// The function whose body holds `s`, through the parent chain.
const clang::FunctionDecl* enclosingFunction(const clang::Stmt* s,
                                             clang::ASTContext& ctx) {
  clang::DynTypedNodeList parents = ctx.getParents(*s);
  while (!parents.empty()) {
    const clang::DynTypedNode& n = parents[0];
    if (const auto* f = n.get<clang::FunctionDecl>()) return f;
    parents = ctx.getParents(n);
  }
  return nullptr;
}

/// `if (requires { ... })`, constexpr or not, in a judged function: Rule 2
/// counts only
/// declared constraints, and a requires-expression in a body is a test of one
/// the body did not declare.  Every such `if` is a finding, at its `if`.
class InlineRequires : public clang::ASTConsumer,
                       public clang::RecursiveASTVisitor<InlineRequires> {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    this->ctx = &ctx;
    TraverseDecl(ctx.getTranslationUnitDecl());
  }
  bool shouldVisitTemplateInstantiations() const { return false; }
  bool VisitIfStmt(clang::IfStmt* s) {
    if (!containsRequires(s->getCond())) return true;
    const clang::FunctionDecl* f = enclosingFunction(s, *ctx);
    if (!judged(f, ctx->getSourceManager())) return true;
    const clang::SourceManager& sm = ctx->getSourceManager();
    const clang::SourceLocation loc = sm.getExpansionLoc(s->getIfLoc());
    llvm::outs() << sm.getFilename(loc) << ':' << sm.getExpansionLineNumber(loc)
                 << ':' << sm.getExpansionColumnNumber(loc)
                 << ": a requires-expression tested in a body is not a "
                    "declared constraint\n";
    return true;
  }

 private:
  clang::ASTContext* ctx = nullptr;
};

// ---------------------------------------------------------------------------
// inheritance: no subtyping
// ---------------------------------------------------------------------------

/// Whether a base type is an empty class without virtual functions.  A
/// dependent base (`: is_x<T>` in a partial specialisation) is judged by the
/// definition of its template's primary, the one declaration there is to read;
/// an alias template (`: std::bool_constant<T::v>`) is read through to the
/// type it names first.
bool emptyBase(clang::QualType t) {
  const clang::CXXRecordDecl* r = t->getAsCXXRecordDecl();
  for (int depth = 0; !r && depth < 8; ++depth) {
    const auto* ts = t->getAs<clang::TemplateSpecializationType>();
    if (!ts) break;
    const clang::TemplateDecl* td = ts->getTemplateName().getAsTemplateDecl();
    if (const auto* at =
            llvm::dyn_cast_or_null<clang::TypeAliasTemplateDecl>(td)) {
      t = at->getTemplatedDecl()->getUnderlyingType();
      r = t->getAsCXXRecordDecl();
      continue;
    }
    const auto* ct = llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(td);
    r = ct ? ct->getTemplatedDecl() : nullptr;
    break;
  }
  if (!r) return false;
  const clang::CXXRecordDecl* def = r->getDefinition();
  return def && def->isEmpty() && !def->isPolymorphic();
}

/// A judged class with a base that is not an empty class, or with a virtual
/// function, is a finding, at the class.
class Inheritance : public clang::ASTConsumer,
                    public clang::RecursiveASTVisitor<Inheritance> {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    sm = &ctx.getSourceManager();
    TraverseDecl(ctx.getTranslationUnitDecl());
  }
  bool shouldVisitTemplateInstantiations() const { return false; }
  bool VisitCXXRecordDecl(clang::CXXRecordDecl* r) {
    if (!r->isThisDeclarationADefinition() || r->isLambda()) return true;
    if (!sm->isInMainFile(r->getLocation()) || !inScope(r)) return true;
    bool finding = r->isPolymorphic();
    for (const clang::CXXBaseSpecifier& b : r->bases())
      if (!emptyBase(b.getType())) finding = true;
    if (!finding) return true;
    const clang::SourceLocation loc = sm->getExpansionLoc(r->getLocation());
    llvm::outs() << sm->getFilename(loc) << ':'
                 << sm->getExpansionLineNumber(loc) << ':'
                 << sm->getExpansionColumnNumber(loc)
                 << ": a base that is not an empty class, or a virtual "
                    "function: subtyping\n";
    return true;
  }

 private:
  const clang::SourceManager* sm = nullptr;
};

// ---------------------------------------------------------------------------
// undecided-resolution: GHC's rule, a call at a type variable decided by the
// enclosing constraints
// ---------------------------------------------------------------------------

/// A template type parameter, by depth and index: the identity an atom's
/// argument is compared by.
using Param = std::pair<unsigned, unsigned>;
std::optional<Param> paramOf(clang::QualType t) {
  if (t.isNull()) return std::nullopt;
  t = t.getNonReferenceType().getUnqualifiedType();
  if (const auto* p = t->getAs<clang::TemplateTypeParmType>())
    return Param{p->getDepth(), p->getIndex()};
  return std::nullopt;
}

/// A rendered argument: a template type parameter as "p<depth>:<index>"
/// (after substitution), any other type by its spelling.
using Subst = std::map<Param, std::string>;
std::string renderParam(Param p) {
  return "p" + std::to_string(p.first) + ":" + std::to_string(p.second);
}
std::string renderArg(clang::QualType t, const Subst& subst) {
  if (std::optional<Param> p = paramOf(t)) {
    if (auto it = subst.find(*p); it != subst.end()) return it->second;
    return renderParam(*p);
  }
  return t.getNonReferenceType().getUnqualifiedType().getAsString();
}

/// An atom of a constraint: a concept applied to its arguments, rendered,
/// with its polarity.
struct Atom {
  std::string concept_;
  std::vector<std::string> args;
  bool positive;
  bool sameFact(const Atom& o) const {
    return concept_ == o.concept_ && args == o.args;
  }
};

/// The atoms a constraint guarantees: the conjuncts of its top-level &&, a
/// negation of a concept-id as a negative atom; a disjunction guarantees
/// nothing.  When `follow`, the library's concepts are read through, their
/// parameters substituted by the arguments they are applied to.
void guaranteed(const clang::Expr* e, bool positive, std::vector<Atom>& out,
                bool follow, const Subst& subst, int depth = 0) {
  if (!e || depth > 16) return;
  e = e->IgnoreParenImpCasts();
  if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
    if (positive && b->getOpcode() == clang::BO_LAnd) {
      guaranteed(b->getLHS(), true, out, follow, subst, depth + 1);
      guaranteed(b->getRHS(), true, out, follow, subst, depth + 1);
    }
    return;  // ||, or && under a negation: no single atom is guaranteed
  }
  if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e)) {
    if (u->getOpcode() == clang::UO_LNot) {
      const clang::Expr* sub = u->getSubExpr()->IgnoreParenImpCasts();
      if (llvm::isa<clang::ConceptSpecializationExpr>(sub))
        guaranteed(sub, !positive, out, false, subst, depth + 1);
    }
    return;
  }
  const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e);
  if (!cs) return;
  Atom atom{cs->getNamedConcept()->getName().str(), {}, positive};
  for (const clang::TemplateArgument& a : cs->getTemplateArguments()) {
    if (a.getKind() != clang::TemplateArgument::Type) return;  // not read
    atom.args.push_back(renderArg(a.getAsType(), subst));
  }
  out.push_back(atom);
  const clang::ConceptDecl* c = cs->getNamedConcept();
  if (!follow || !positive || inStd(c)) return;
  // Read the concept's body, its parameters replaced by the arguments.
  Subst inner;
  const clang::TemplateParameterList* ps = c->getTemplateParameters();
  for (unsigned k = 0; k < ps->size() && k < atom.args.size(); ++k)
    if (const auto* tp =
            llvm::dyn_cast<clang::TemplateTypeParmDecl>(ps->getParam(k)))
      inner[{tp->getDepth(), tp->getIndex()}] = atom.args[k];
  guaranteed(c->getConstraintExpr(), true, out, follow, inner, depth + 1);
}

/// The atoms a template's own constraints guarantee.
std::vector<Atom> atomsOf(const clang::TemplateParameterList* ps,
                          const clang::Expr* trailing, bool follow,
                          const Subst& subst = {}) {
  std::vector<Atom> out;
  if (!ps) return out;
  for (const clang::NamedDecl* p : *ps)
    if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
      if (const clang::TypeConstraint* tc = tp->getTypeConstraint())
        guaranteed(tc->getImmediatelyDeclaredConstraint(), true, out, follow,
                   subst);
  guaranteed(ps->getRequiresClause(), true, out, follow, subst);
  guaranteed(trailing, true, out, follow, subst);
  return out;
}

/// Whether a candidate can take `n` arguments: its parameters without
/// defaults at most n, and at least n unless it ends in a pack.
bool viableArity(const clang::FunctionDecl* f, unsigned n) {
  unsigned required = f->getMinRequiredArguments();
  bool pack = f->getNumParams() > 0 &&
              f->getParamDecl(f->getNumParams() - 1)->isParameterPack();
  return required <= n && (pack || n <= f->getNumParams());
}

/// The check: in every judged function template, a call at a value of
/// parameter type to an overload set of two or more of the library's function
/// templates that can take the call's arguments, not named by the enclosing
/// constraints, whose candidates differ in an atom (a concept on the argument
/// the call passes, with its polarity) that the enclosing constraints do not
/// guarantee or refute, read through the library's concepts with their
/// arguments substituted and through enclosing class templates.  Not seen:
/// trait reads through class and variable templates, whose specialisations
/// differ by pattern rather than by concept; candidates that differ by head
/// alone; calls reached through a helper.
class UndecidedResolution : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    sm = &ctx.getSourceManager();
    Finder finder{*this};
    finder.TraverseDecl(ctx.getTranslationUnitDecl());
  }

  struct Calls : clang::RecursiveASTVisitor<Calls> {
    std::vector<const clang::CallExpr*> calls;
    bool TraverseLambdaExpr(clang::LambdaExpr*) { return true; }
    bool VisitCallExpr(clang::CallExpr* e) {
      calls.push_back(e);
      return true;
    }
  };

  struct Finder : clang::RecursiveASTVisitor<Finder> {
    UndecidedResolution& owner;
    explicit Finder(UndecidedResolution& o) : owner(o) {}
    bool shouldVisitTemplateInstantiations() const { return false; }
    bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl* ft) {
      const clang::FunctionDecl* f = ft->getTemplatedDecl();
      if (!f || !f->doesThisDeclarationHaveABody() || f->isDeleted())
        return true;
      if (const auto* m = llvm::dyn_cast<clang::CXXMethodDecl>(f))
        if (m->getParent()->isLambda()) return true;
      if (!owner.sm->isInMainFile(ft->getLocation()) || !inScope(ft))
        return true;
      owner.judge(ft);
      return true;
    }
  };

  void judge(const clang::FunctionTemplateDecl* ft) {
    const Vocabulary v = vocabularyOf(ft);
    if (v.opaque) return;  // a constraint this check cannot read: silence
    std::vector<Atom> decided = atomsOf(
        ft->getTemplateParameters(),
        ft->getTemplatedDecl()->getTrailingRequiresClause().ConstraintExpr,
        true);
    for (const clang::DeclContext* c = ft->getDeclContext(); c;
         c = c->getParent()) {
      const clang::TemplateParameterList* ps = nullptr;
      if (const auto* spec =
              llvm::dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(c))
        ps = spec->getTemplateParameters();
      else if (const auto* rd = llvm::dyn_cast<clang::CXXRecordDecl>(c))
        if (const clang::ClassTemplateDecl* ct =
                rd->getDescribedClassTemplate())
          ps = ct->getTemplateParameters();
      if (!ps) continue;
      std::vector<Atom> more = atomsOf(ps, nullptr, true);
      decided.insert(decided.end(), more.begin(), more.end());
    }
    // A fact the enclosing constraints fix: +1 guaranteed, -1 refuted.
    auto known = [&](const Atom& a) -> int {
      for (const Atom& d : decided)
        if (d.sameFact(a)) return d.positive ? 1 : -1;
      return 0;
    };
    Calls body;
    body.TraverseStmt(ft->getTemplatedDecl()->getBody());
    for (const clang::CallExpr* call : body.calls) {
      const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(
          call->getCallee()->IgnoreParenImpCasts());
      if (!u) continue;
      llvm::SmallVector<const clang::Expr*, 4> args(call->arguments().begin(),
                                                    call->arguments().end());
      if (!anyParameterTyped(args)) continue;
      const std::string name = u->getName().getAsString();
      if (v.names.count(name)) continue;  // named: read through the dictionary
      // Each viable candidate's atoms, re-expressed on the enclosing
      // template's parameters through the arguments the call passes; a
      // candidate the enclosing facts contradict can never be chosen.
      std::vector<std::vector<Atom>> cands;
      for (const clang::NamedDecl* d : u->decls()) {
        const auto* cand = llvm::dyn_cast<clang::FunctionTemplateDecl>(d);
        if (!cand || inStd(cand)) continue;
        const clang::FunctionDecl* cf = cand->getTemplatedDecl();
        if (cf->isDeleted() || !viableArity(cf, args.size())) continue;
        Subst toCaller;
        std::set<std::string> callerArgs;
        for (unsigned k = 0; k < cf->getNumParams() && k < args.size(); ++k)
          if (std::optional<Param> from =
                  paramOf(cf->getParamDecl(k)->getType())) {
            toCaller[*from] = renderArg(args[k]->getType(), {});
            callerArgs.insert(toCaller[*from]);
          }
        std::vector<Atom> atoms;
        bool bound = true, contradicted = false;
        for (const Atom& a :
             atomsOf(cand->getTemplateParameters(),
                     cf->getTrailingRequiresClause().ConstraintExpr, false,
                     toCaller)) {
          for (const std::string& arg : a.args)
            if (arg.rfind("p", 0) == 0 && arg.find(':') != std::string::npos &&
                !callerArgs.count(arg))
              bound = false;  // a candidate parameter the call does not bind
          const int k = known(a);
          if ((k == 1 && !a.positive) || (k == -1 && a.positive))
            contradicted = true;
          atoms.push_back(a);
        }
        if (contradicted || !bound) continue;
        cands.push_back(atoms);
      }
      if (cands.size() < 2) continue;
      std::vector<Atom> keys;
      for (const auto& as : cands)
        for (const Atom& a : as) {
          bool seen = false;
          for (const Atom& k : keys) seen |= k.sameFact(a);
          if (!seen) keys.push_back(a);
        }
      std::string undecided;
      for (const Atom& key : keys) {
        // The atom's state per candidate: required, refuted, or absent.
        std::set<int> states;
        for (const auto& as : cands) {
          int st = 0;
          for (const Atom& a : as)
            if (a.sameFact(key)) st = a.positive ? 1 : -1;
          states.insert(st);
        }
        if (states.size() > 1 && known(key) == 0) {
          undecided = key.concept_;
          break;
        }
      }
      if (undecided.empty()) continue;
      const clang::SourceLocation loc = sm->getExpansionLoc(u->getNameLoc());
      llvm::outs() << sm->getFilename(loc) << ':'
                   << sm->getExpansionLineNumber(loc) << ':'
                   << sm->getExpansionColumnNumber(loc) << ": the call to '"
                   << name << "' resolves by '" << undecided
                   << "', which the constraints of " << ft->getNameAsString()
                   << " do not decide\n";
    }
  }

 private:
  const clang::SourceManager* sm = nullptr;
};

// ---------------------------------------------------------------------------
// inventory: the syntax a scope uses
// ---------------------------------------------------------------------------

/// Every declaration, statement or expression, and type form as written,
/// inside a declaration of the scope in the main file, counted by kind.
/// Template instantiations and implicit code are not the program as written
/// and are not counted.
class Inventory : public clang::ASTConsumer,
                  public clang::RecursiveASTVisitor<Inventory> {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    sm = &ctx.getSourceManager();
    TraverseDecl(ctx.getTranslationUnitDecl());
    for (const auto& [kind, n] : counts)
      llvm::outs() << kind << ' ' << n << '\n';
  }
  bool shouldVisitTemplateInstantiations() const { return false; }
  bool shouldVisitImplicitCode() const { return false; }
  bool TraverseDecl(clang::Decl* d) {
    if (!d) return true;
    const bool was = inside;
    if (!inside && !llvm::isa<clang::TranslationUnitDecl>(d) &&
        sm->isInMainFile(sm->getExpansionLoc(d->getLocation())) &&
        (inScope(d) || isScopeNamespace(d)))
      inside = true;
    if (inside && !isScopeNamespace(d) && !d->isImplicit())
      ++counts[std::string("Decl:") + d->getDeclKindName()];
    const bool ok = clang::RecursiveASTVisitor<Inventory>::TraverseDecl(d);
    inside = was;
    return ok;
  }
  bool VisitStmt(clang::Stmt* st) {
    if (inside) ++counts[std::string("Stmt:") + st->getStmtClassName()];
    return true;
  }
  bool VisitTypeLoc(clang::TypeLoc tl) {
    if (inside && !tl.isNull())
      ++counts[std::string("Type:") + tl.getType()->getTypeClassName()];
    return true;
  }

 private:
  static bool isScopeNamespace(const clang::Decl* d) {
    const auto* ns = llvm::dyn_cast<clang::NamespaceDecl>(d);
    return ns && ns->getName() == scope;
  }
  const clang::SourceManager* sm = nullptr;
  bool inside = false;
  std::map<std::string, unsigned> counts;
};

class Action : public clang::ASTFrontendAction {
 public:
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance&, llvm::StringRef) override {
    if (check == "mutual-recursion") return std::make_unique<MutualRecursion>();
    if (check == "definition") return std::make_unique<Definition>();
    if (check == "inline-requires") return std::make_unique<InlineRequires>();
    if (check == "inheritance") return std::make_unique<Inheritance>();
    if (check == "inventory") return std::make_unique<Inventory>();
    if (check == "undecided-resolution")
      return std::make_unique<UndecidedResolution>();
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
