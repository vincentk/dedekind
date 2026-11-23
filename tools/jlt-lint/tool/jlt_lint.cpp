// jlt-lint: the Jlt linter, every rule of a file over one parse.
//
//   jlt-lint [--check=<name>[,<name>...]] [--query=<rule.query>[,...]]
//            --scope=<namespace> <file> [-p <build>] [-- <flags>]
//
// Prints one finding per line as "path:line:col", each rule's findings after
// a line naming it, which the driver counts: "== scope" first, the scope
// namespace's own declarations in the main file (none means the scope is
// absent, and every count would be a vacuous zero); then "== query <rule>"
// for each rule file, "== <check>" for each check, in the order named.
// A rule file holds clang-query matchers (`match <matcher>`, and at most one
// `set traversal <kind>`, IgnoreUnlessSpelledInSource otherwise), parsed by
// clang's dynamic matcher parser as clang-query parses them, with `scope`
// bound to the scope namespace, and run on the same AST as the checks; a
// match is a finding, located where clang-query's `set output diag` puts its
// "root" binding, or "-" where that binding has no location.
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
//                     at the check.  A conversion into a parameter type,
//                     T(n) or static_cast<T>(n) from a value of another
//                     type, is an operation of T like any other: the
//                     constraints must name it.
//   inline-requires   Rule 2: an `if` (constexpr or not) whose condition
//                     contains a requires-expression tests a constraint the
//                     body did not declare, which Rule 2 does not count
//                     (#1068); the fix is a named shape, which the definition
//                     check then reads.
//   type-observation  Rule 2: in a function template's body, sizeof,
//                     alignof, typeid, a standard type trait or a standard
//                     concept used as a value at a dependent type, outside an
//                     if constexpr's condition: an observation of a type no
//                     shape names, which an archetype answers for itself.
//   undecided-resolution
//                     Rule 2, GHC's rule for overlapping instances: in a
//                     function template, a call at a value of parameter type
//                     to an overload set whose candidates are separated by a
//                     concept the enclosing constraints do not mention (nor
//                     negate) resolves differently at different
//                     instantiations, a dependence on a constraint the
//                     template did not declare.  A call its constraints name
//                     is read through the dictionary and is not judged.
//   context-order     Rule 2, the well-formedness of resolution: an instance
//                     with a context whose head has no proper part (a bare
//                     type parameter or a concrete type) may mention in its
//                     context only shapes declared before the trait's
//                     primary; otherwise resolving the trait entails a shape
//                     whose requirement calls the trait, a satisfaction that
//                     depends on itself.  A structured head descends by its
//                     parts and is not judged.
//   converted-result  Rule 2, the conversion discipline (#1143): the result of
//                     a requirement `{ e } -> std::convertible_to<T>` is used
//                     only at the declared T.  A body that takes it as the
//                     operand of a further operation, reads a member of it or
//                     holds it in an `auto` uses it at its own type, an
//                     operation the constraint did not name, which C++ accepts
//                     and FJlt's typing refuses.  Name-level like `definition`;
//                     a call's argument is not judged, its parameter being
//                     unknown in a template, and !, && and || on a result
//                     converted to bool are read at bool, though a proxy may
//                     overload them: the check reports what it is sure of.
//   inventory         not a rule: the syntax a scope uses, one line per AST
//                     node kind (declarations, statements and expressions,
//                     type forms as written) with its count, for diffing
//                     against the kernel's grammar (#1079).
//   bounded-fold      Rule 3, a fold over a bounded range (#1172): a standard
//                     fold (std::ranges::fold_left, fold_right, ...) folds
//                     only a parameter whose type is a template parameter
//                     the signature bounds by category's IsBoundedRange, as
//                     a type constraint or a conjunct of a requires clause;
//                     the compiler then checks the bound at every call.  Any
//                     other call is a finding: an iterator pair, a range
//                     that is not the bounded parameter, a bound under a
//                     negation or a disjunction.
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
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/Dynamic/Parser.h"
#include "clang/Analysis/CallGraph.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/MultiplexConsumer.h"
#include "clang/Frontend/TextDiagnostic.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/raw_ostream.h"

namespace {

llvm::cl::OptionCategory category("jlt-lint options");
llvm::cl::opt<std::string> check("check",
                                 llvm::cl::desc("the checks to run, by name"),
                                 llvm::cl::cat(category));
llvm::cl::list<std::string> queryFiles(
    "query", llvm::cl::desc("the rule files of clang-query matchers to run"),
    llvm::cl::CommaSeparated, llvm::cl::cat(category));
llvm::cl::opt<std::string> scope("scope",
                                 llvm::cl::desc("the namespace judged"),
                                 llvm::cl::Required, llvm::cl::cat(category));

/// Where a check prints: stdout, or its section's buffer when several checks
/// share one parse.
llvm::raw_ostream* current = &llvm::outs();
llvm::raw_ostream& out() { return *current; }

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
      out() << sm.getFilename(loc) << ':' << sm.getExpansionLineNumber(loc)
            << ':' << sm.getExpansionColumnNumber(loc) << '\n';
    }
  }
};

// ---------------------------------------------------------------------------
// definition: Rule 2 at the level of names
// ---------------------------------------------------------------------------

/// The arguments of a construction, braces opened: T{} has none, T{x} has x.
llvm::SmallVector<const clang::Expr*, 2> constructionArgs(
    const clang::CXXUnresolvedConstructExpr* e) {
  llvm::SmallVector<const clang::Expr*, 2> out;
  for (const clang::Expr* a : e->arguments()) {
    if (const auto* list = llvm::dyn_cast<clang::InitListExpr>(a))
      out.append(list->inits().begin(), list->inits().end());
    else
      out.push_back(a);
  }
  return out;
}

/// Whether `target` is a template parameter type and some argument is a value
/// of another type: a conversion into the parameter type.  Default
/// construction (no arguments) and a copy (an argument of the same type) are
/// what semiregular grants, and are not conversions.
bool convertsIntoParameter(clang::QualType target,
                           llvm::ArrayRef<const clang::Expr*> args) {
  if (target.isNull()) return false;
  target = target.getNonReferenceType().getUnqualifiedType();
  if (!target->isTemplateTypeParmType()) return false;
  const clang::QualType want = target.getCanonicalType();
  for (const clang::Expr* a : args) {
    if (!a) continue;
    clang::QualType t = a->getType();
    if (t.isNull()) return false;  // cannot tell: silence
    t = t.getNonReferenceType().getUnqualifiedType().getCanonicalType();
    if (t == want) continue;
    // A dependent type may well be T: std::move(x) of a T x, or a parameter
    // U that a constraint makes T (std::same_as<T> U), which this check does
    // not read.  Cannot tell, so silence.
    if (t->isDependentType()) return false;
    return true;
  }
  return false;
}

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
  bool VisitCXXUnresolvedConstructExpr(clang::CXXUnresolvedConstructExpr* e) {
    if (convertsIntoParameter(e->getTypeAsWritten(), constructionArgs(e)))
      names.insert("conversion");  // T(x) in a requirement names it
    return true;
  }
  bool VisitCXXStaticCastExpr(clang::CXXStaticCastExpr* e) {
    if (convertsIntoParameter(e->getTypeAsWritten(), {e->getSubExpr()}))
      names.insert("conversion");
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
  if (name == "constructible_from" || name == "convertible_to" ||
      name == "is_constructible_v" || name == "is_convertible_v")
    p.names = {"conversion"};
  if (name == "semiregular" || name == "regular" || name == "copyable" ||
      name == "movable" || name == "assignable_from")
    p.names = {"="};  // a value may be assigned to (Rule 1: a local)
  if (name == "regular") p.names = {"=", "==", "!="};  // and compared
  if (name.ends_with("range") || name == "view")
    p.names = {"begin", "end", "size", "empty"};
  return p;
}

/// The operator a standard function object is defined by, in <functional>:
/// std::plus<T>::operator()(a, b) returns a + b, and so on.  A constraint that
/// names the function object has declared its operator.  Empty for any other
/// type.
llvm::StringRef operatorOfStdFunctionObject(clang::QualType t) {
  static const std::map<std::string, std::string> ops = {
      {"plus", "+"},         {"minus", "-"},         {"multiplies", "*"},
      {"divides", "/"},      {"modulus", "%"},       {"negate", "-"},
      {"equal_to", "=="},    {"not_equal_to", "!="}, {"less", "<"},
      {"greater", ">"},      {"less_equal", "<="},   {"greater_equal", ">="},
      {"logical_and", "&&"}, {"logical_or", "||"},   {"logical_not", "!"},
      {"bit_and", "&"},      {"bit_or", "|"},        {"bit_xor", "^"},
      {"bit_not", "~"}};
  const clang::CXXRecordDecl* r =
      t.isNull() ? nullptr : t->getAsCXXRecordDecl();
  if (!r) {
    if (const auto* ts = t->getAs<clang::TemplateSpecializationType>())
      if (const auto* td = ts->getTemplateName().getAsTemplateDecl())
        if (const auto* rd = llvm::dyn_cast_or_null<clang::CXXRecordDecl>(
                td->getTemplatedDecl()))
          r = rd;
  }
  if (!r) return {};
  bool std_ = false;
  for (const clang::DeclContext* c = r->getDeclContext(); c; c = c->getParent())
    if (const auto* ns = llvm::dyn_cast<clang::NamespaceDecl>(c))
      if (ns->isStdNamespace()) std_ = true;
  if (!std_) return {};
  auto it = ops.find(r->getName().str());
  return it == ops.end() ? llvm::StringRef() : llvm::StringRef(it->second);
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

/// Whether a call in a constraint is a law in function spelling: every
/// candidate a constexpr function template returning bool, as
/// associative<T>(Op{}) and distributive<T>(Mul{}, Add{}) are.
bool isLawCall(const clang::CallExpr* call) {
  const clang::Expr* callee = call->getCallee()->IgnoreParenImpCasts();
  llvm::SmallVector<const clang::NamedDecl*, 4> cands;
  if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee))
    cands.append(u->decls().begin(), u->decls().end());
  else if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(callee))
    cands.push_back(d->getDecl());
  if (cands.empty()) return false;
  for (const clang::NamedDecl* c : cands) {
    const clang::FunctionDecl* f = nullptr;
    if (const auto* ft = llvm::dyn_cast<clang::FunctionTemplateDecl>(c))
      f = ft->getTemplatedDecl();
    else
      f = llvm::dyn_cast<clang::FunctionDecl>(c);
    if (!f || !f->isConstexpr() || !f->getReturnType()->isBooleanType())
      return false;
  }
  return true;
}

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
    // An argument that is a standard function object grants its operator:
    // IsMonoid<T, std::plus<T>> declares +.
    for (const clang::TemplateArgument& a : cs->getTemplateArguments())
      if (a.getKind() == clang::TemplateArgument::Type) {
        llvm::StringRef op = operatorOfStdFunctionObject(a.getAsType());
        if (!op.empty()) v.names.insert(op.str());
      }
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
  } else if (const auto* call = llvm::dyn_cast<clang::CallExpr>(e)) {
    // A law in function spelling, associative<T>(Op{}): a declared fact about
    // the operation.  It names no operation of the value, and reading it as
    // opaque would silence the check on every function under the shape.
    if (!isLawCall(call)) {
      v.opaque = true;
      return;
    }
    // The law names no operation, but its arguments may: accepts<T>(T{0}).
    NameCollector nc;
    for (const clang::Expr* a : call->arguments())
      nc.TraverseStmt(const_cast<clang::Expr*>(a));
    v.names.insert(nc.names.begin(), nc.names.end());
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

/// Whether a value's type is known only through the template's parameters
/// (through references and cv): a parameter itself or an associated type of
/// one, the values whose vocabulary Rule 2 restricts.  A value of a library
/// aggregate's type carries that type's vocabulary, not the parameter's.
bool parameterKnown(clang::QualType t) {
  if (t.isNull()) return false;
  t = t.getNonReferenceType().getUnqualifiedType();
  if (t->isTemplateTypeParmType() ||
      llvm::isa<clang::SubstTemplateTypeParmType>(t.getTypePtr()))
    return true;
  // An associated type of a parameter: a dependent name (typename R::value
  // type) or the decltype of a dependent
  // expression, read through aliases such as range_value_t<R> or
  // carrier_t<E> to the canonical type.  It is known only through the
  // constraints, as the parameter is.  A class template's specialisation
  // (std::pair<T, T>) has a vocabulary of its own and is not one.
  if (!t->isDependentType()) return false;
  const clang::Type* c = t.getCanonicalType().getTypePtr();
  return llvm::isa<clang::TemplateTypeParmType>(c) ||
         llvm::isa<clang::DependentNameType>(c) ||
         llvm::isa<clang::DecltypeType>(c);
}
bool isParameterTyped(const clang::Expr* e) {
  return e && parameterKnown(e->getType());
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
  // T(n), T{n}: a construction of a parameter type from a value of another
  // type is a conversion, an operation of T no requirement may leave unnamed.
  bool VisitCXXUnresolvedConstructExpr(clang::CXXUnresolvedConstructExpr* e) {
    if (convertsIntoParameter(e->getTypeAsWritten(), constructionArgs(e)))
      ops.push_back({"conversion", e->getBeginLoc(), true});
    return true;
  }
  bool VisitCXXStaticCastExpr(clang::CXXStaticCastExpr* e) {
    if (convertsIntoParameter(e->getTypeAsWritten(), {e->getSubExpr()}))
      ops.push_back({"conversion", e->getBeginLoc(), true});
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
    // The then-branch of an if constexpr on a concept is typed under that
    // concept (the instance chain of Appendix A.7): its grants hold there.
    struct Branch {
      clang::SourceRange range;
      Vocabulary v;
    };
    std::vector<Branch> branches;
    struct Ifs : clang::RecursiveASTVisitor<Ifs> {
      std::vector<Branch>& out;
      explicit Ifs(std::vector<Branch>& o) : out(o) {}
      bool TraverseLambdaExpr(clang::LambdaExpr*) { return true; }
      bool VisitIfStmt(clang::IfStmt* st) {
        if (!st->isConstexpr() || !st->getThen()) return true;
        Branch b{st->getThen()->getSourceRange(), {}};
        if (!typeConstraint(st->getCond())) return true;
        gather(st->getCond(), b.v);
        if (!b.v.opaque) out.push_back(std::move(b));
        return true;
      }
      // A condition that constrains types: concept-ids joined by &&, each
      // applied to types, none the decltype of an operation.  A
      // requires-expression or a concept on decltype(op(x)) probes an
      // operation instead, and is not a constraint (#1068).
      static bool typeConstraint(const clang::Expr* e) {
        e = e->IgnoreParenImpCasts();
        if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e))
          return b->getOpcode() == clang::BO_LAnd &&
                 typeConstraint(b->getLHS()) && typeConstraint(b->getRHS());
        const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e);
        if (!cs) return false;
        for (const clang::TemplateArgument& a : cs->getTemplateArguments())
          if (!typeArgument(a)) return false;
        if (const auto* w = cs->getTemplateArgsAsWritten())
          for (const clang::TemplateArgumentLoc& al : w->arguments())
            if (!typeArgument(al.getArgument())) return false;
        return true;
      }
      // A type, a pack of types (std::invocable<T> has an empty one), or the
      // decltype of a plain variable, decltype(x) for a parameter x, which is
      // its type.  The decltype of any other expression probes an operation.
      static bool typeArgument(const clang::TemplateArgument& a) {
        if (a.getKind() == clang::TemplateArgument::Pack) {
          for (const clang::TemplateArgument& e : a.pack_elements())
            if (!typeArgument(e)) return false;
          return true;
        }
        if (a.getKind() != clang::TemplateArgument::Type) return false;
        if (const auto* dt = a.getAsType()->getAs<clang::DecltypeType>())
          return llvm::isa<clang::DeclRefExpr>(
              dt->getUnderlyingExpr()->IgnoreParenImpCasts());
        return true;
      }
    } ifs{branches};
    ifs.TraverseStmt(ft->getTemplatedDecl()->getBody());
    auto grantedInBranch = [&](const BodyOperations::Op& op) {
      for (const Branch& b : branches)
        if (sm.isPointWithin(op.loc, b.range.getBegin(), b.range.getEnd()) &&
            (b.v.names.count(op.name) || (op.builtin && b.v.everything)))
          return true;
      return false;
    };
    for (const BodyOperations::Op& op : body.ops) {
      if (v.names.count(op.name)) continue;
      if (op.builtin && v.everything) continue;
      if (grantedInBranch(op)) continue;
      const clang::SourceLocation loc = sm.getExpansionLoc(op.loc);
      out() << sm.getFilename(loc) << ':' << sm.getExpansionLineNumber(loc)
            << ':' << sm.getExpansionColumnNumber(loc) << ": '" << op.name
            << "' is not named by the constraints of " << ft->getNameAsString()
            << '\n';
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
    out() << sm.getFilename(loc) << ':' << sm.getExpansionLineNumber(loc) << ':'
          << sm.getExpansionColumnNumber(loc)
          << ": a requires-expression tested in a body is not a "
             "declared constraint\n";
    return true;
  }

 private:
  clang::ASTContext* ctx = nullptr;
};

// ---------------------------------------------------------------------------
// type-observation: a body varies with a type only through its shapes
// ---------------------------------------------------------------------------

/// In a function template's body, an expression that observes a dependent
/// type other than through a shape: sizeof, alignof, typeid, the standard's
/// type traits (std::is_integral_v<T>, std::is_same<T, U>::value, the
/// compiler's __is_same), or a standard concept used as a value.  An archetype
/// answers such an observation for itself, so the second stage would admit a
/// body that is not parametric (the cold reads of #1212).  The condition of an
/// if constexpr is not judged here: Table 7 reads it as a derived form, and
/// the second stage leaves such a body undecided.
class TypeObservation : public clang::ASTConsumer,
                        public clang::RecursiveASTVisitor<TypeObservation> {
 public:
  void HandleTranslationUnit(clang::ASTContext& c) override {
    ctx = &c;
    TraverseDecl(c.getTranslationUnitDecl());
  }
  bool shouldVisitTemplateInstantiations() const { return false; }
  bool TraverseFunctionDecl(clang::FunctionDecl* f) {
    return within(f, [&] {
      return clang::RecursiveASTVisitor<TypeObservation>::TraverseFunctionDecl(
          f);
    });
  }
  bool TraverseCXXMethodDecl(clang::CXXMethodDecl* f) {
    return within(f, [&] {
      return clang::RecursiveASTVisitor<TypeObservation>::TraverseCXXMethodDecl(
          f);
    });
  }
  bool TraverseIfStmt(clang::IfStmt* s) {
    if (!s->isConstexpr())
      return clang::RecursiveASTVisitor<TypeObservation>::TraverseIfStmt(s);
    if (s->getInit()) TraverseStmt(s->getInit());
    ++inCondition;
    TraverseStmt(s->getCond());
    --inCondition;
    TraverseStmt(s->getThen());
    if (s->getElse()) TraverseStmt(s->getElse());
    return true;
  }
  bool VisitUnaryExprOrTypeTraitExpr(clang::UnaryExprOrTypeTraitExpr* e) {
    const clang::QualType t = e->isArgumentType()
                                  ? e->getArgumentType()
                                  : e->getArgumentExpr()->getType();
    if (!t.isNull() && t->isDependentType())
      report(e->getBeginLoc(), "sizeof or alignof of a dependent type");
    return true;
  }
  bool VisitCXXTypeidExpr(clang::CXXTypeidExpr* e) {
    if (e->isTypeDependent() || e->isValueDependent())
      report(e->getBeginLoc(), "typeid of a dependent type");
    return true;
  }
  bool VisitTypeTraitExpr(clang::TypeTraitExpr* e) {
    if (e->isValueDependent())
      report(e->getBeginLoc(),
             "a type trait of the compiler at a dependent type");
    return true;
  }
  bool VisitConceptSpecializationExpr(clang::ConceptSpecializationExpr* e) {
    if (inStd(e->getNamedConcept()) && e->isValueDependent())
      report(e->getBeginLoc(), "a standard concept used as a value: " +
                                   e->getNamedConcept()->getName().str());
    return true;
  }
  bool VisitDeclRefExpr(clang::DeclRefExpr* e) {
    if (const auto* v =
            llvm::dyn_cast<clang::VarTemplateSpecializationDecl>(e->getDecl()))
      trait(v->getSpecializedTemplate(), e->isValueDependent(),
            e->getBeginLoc());
    return true;
  }
  bool VisitUnresolvedLookupExpr(clang::UnresolvedLookupExpr* e) {
    for (const clang::NamedDecl* d : e->decls())
      if (const auto* vt = llvm::dyn_cast<clang::VarTemplateDecl>(d)) {
        trait(vt, true, e->getBeginLoc());
        break;
      }
    return true;
  }
  bool VisitDependentScopeDeclRefExpr(clang::DependentScopeDeclRefExpr* e) {
    // std::is_same<T, U>::value
    if (e->getDeclName().getAsString() != "value") return true;
    const clang::Type* q = e->getQualifier().getAsType();
    if (const auto* ts =
            q ? q->getAs<clang::TemplateSpecializationType>() : nullptr)
      if (const clang::TemplateDecl* td =
              ts->getTemplateName().getAsTemplateDecl())
        if (inStd(td) && isTraitName(td->getName()))
          report(e->getBeginLoc(),
                 "a standard type trait at a dependent type: " +
                     td->getName().str());
    return true;
  }

 private:
  clang::ASTContext* ctx = nullptr;
  bool judging = false;
  int inCondition = 0;
  template <typename F>
  bool within(clang::FunctionDecl* f, F go) {
    const bool was = judging;
    judging = f->isTemplated() && f->doesThisDeclarationHaveABody() &&
              judged(f, ctx->getSourceManager());
    const bool r = go();
    judging = was;
    return r;
  }
  static bool isTraitName(llvm::StringRef n) {
    return n.starts_with("is_") || n.starts_with("has_") || n == "rank" ||
           n == "extent" || n == "alignment_of" || n == "tuple_size" ||
           n == "variant_size";
  }
  void trait(const clang::VarTemplateDecl* vt, bool dependent,
             clang::SourceLocation at) {
    if (!vt || !dependent || !inStd(vt)) return;
    llvm::StringRef n = vt->getName();
    if (n.ends_with("_v")) n = n.drop_back(2);
    if (isTraitName(n))
      report(at, "a standard type trait at a dependent type: " +
                     vt->getName().str());
  }
  void report(clang::SourceLocation at, const std::string& what) {
    if (!judging || inCondition) return;
    const clang::SourceManager& sm = ctx->getSourceManager();
    const clang::SourceLocation loc = sm.getExpansionLoc(at);
    out() << sm.getFilename(loc) << ':' << sm.getExpansionLineNumber(loc) << ':'
          << sm.getExpansionColumnNumber(loc) << ": " << what
          << ", not through a shape\n";
  }
};

// ---------------------------------------------------------------------------
// bounded-fold: a standard fold only where the signature states the bound
// ---------------------------------------------------------------------------

/// The declaration whose body or initialiser holds `s`, through the parent
/// chain: a function, or a variable at namespace scope.
const clang::Decl* enclosingDecl(const clang::Stmt* s, clang::ASTContext& ctx) {
  clang::DynTypedNodeList parents = ctx.getParents(*s);
  while (!parents.empty()) {
    const clang::DynTypedNode& n = parents[0];
    if (const auto* d = n.get<clang::Decl>()) return d;
    parents = ctx.getParents(n);
  }
  return nullptr;
}

/// Whether `c` states a range's count before a fold starts: category's
/// IsBoundedRange, read through transparent contexts such as a module's
/// export, or the standard's sized_range, whose size is known before the
/// first step.
bool isBoundedConcept(const clang::NamedDecl* c) {
  if (!c) return false;
  if (c->getName() == "sized_range") return inStd(c);
  if (c->getName() != "IsBoundedRange") return false;
  const auto* ns = llvm::dyn_cast<clang::NamespaceDecl>(
      c->getDeclContext()->getRedeclContext());
  return ns && ns->getName() == "category";
}

/// Whether a folded range is a count given before the first step: the
/// standard's views::iota with a bound, or views::repeat with a count, called
/// with two arguments, as fold_left(views::iota(0uz, k), ...) is.
bool isCountedView(const clang::Expr* r) {
  const auto* call = llvm::dyn_cast_or_null<clang::CallExpr>(r);
  if (!call) return false;
  const clang::Expr* callee = call->getCallee()->IgnoreParenImpCasts();
  unsigned first = 0;
  if (const auto* op = llvm::dyn_cast<clang::CXXOperatorCallExpr>(call)) {
    callee = op->getArg(0)->IgnoreParenImpCasts();
    first = 1;
  }
  const clang::NamedDecl* d = nullptr;
  if (const auto* ref = llvm::dyn_cast<clang::DeclRefExpr>(callee))
    d = ref->getDecl();
  else if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee))
    d = u->decls().empty() ? nullptr : *u->decls().begin();
  if (!d || !inStd(d)) return false;
  const std::string name = d->getNameAsString();
  if ((name != "iota" && name != "repeat") || call->getNumArgs() != first + 2)
    return false;
  // A bound of std::unreachable_sentinel_t is no bound: iota(0u,
  // unreachable_sentinel) and repeat(x, unreachable_sentinel) never end.
  const clang::QualType bound = call->getArg(first + 1)
                                    ->IgnoreParenImpCasts()
                                    ->getType()
                                    .getNonReferenceType()
                                    .getUnqualifiedType()
                                    .getCanonicalType();
  return bound.getAsString().find("unreachable_sentinel_t") ==
         std::string::npos;
}

/// The template parameters a constraint bounds: IsBoundedRange<P> as a
/// conjunct, through && and parentheses only; a negation or a disjunction
/// bounds nothing.  A parameter is keyed by its depth and index.
void boundedConjuncts(const clang::Expr* e,
                      std::set<std::pair<unsigned, unsigned>>& out) {
  if (!e) return;
  e = e->IgnoreParens();
  if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
    if (b->getOpcode() != clang::BO_LAnd) return;
    boundedConjuncts(b->getLHS(), out);
    boundedConjuncts(b->getRHS(), out);
    return;
  }
  const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e);
  if (!cs || !isBoundedConcept(cs->getNamedConcept())) return;
  llvm::ArrayRef<clang::TemplateArgument> args = cs->getTemplateArguments();
  if (args.empty() || args[0].getKind() != clang::TemplateArgument::Type)
    return;
  if (const auto* tp = args[0]
                           .getAsType()
                           .getCanonicalType()
                           ->getAs<clang::TemplateTypeParmType>())
    out.insert({tp->getDepth(), tp->getIndex()});
}

/// The parameters of `f`'s template whose bound the signature states: by a
/// type constraint, a requires clause or a trailing one.
std::set<std::pair<unsigned, unsigned>> boundedParameters(
    const clang::FunctionDecl* f) {
  std::set<std::pair<unsigned, unsigned>> out;
  if (!f) return out;
  const clang::FunctionTemplateDecl* t = f->getDescribedFunctionTemplate();
  if (!t) t = f->getPrimaryTemplate();
  if (!t) return out;
  for (const clang::NamedDecl* p : *t->getTemplateParameters())
    if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
      if (const clang::TypeConstraint* tc = tp->getTypeConstraint())
        boundedConjuncts(tc->getImmediatelyDeclaredConstraint(), out);
  boundedConjuncts(t->getTemplateParameters()->getRequiresClause(), out);
  boundedConjuncts(
      t->getTemplatedDecl()->getTrailingRequiresClause().ConstraintExpr, out);
  return out;
}

/// The range a fold call folds: its first argument after the callee object,
/// through parentheses, implicit casts and std::move; null for any other
/// call shape.  An iterator pair's first argument is no parameter range.
const clang::Expr* foldedRange(const clang::DeclRefExpr* fold,
                               clang::ASTContext& ctx) {
  clang::DynTypedNodeList parents = ctx.getParents(*fold);
  const clang::CallExpr* call = nullptr;
  while (!parents.empty()) {
    if (const auto* c = parents[0].get<clang::CallExpr>()) {
      call = c;
      break;
    }
    if (!parents[0].get<clang::ImplicitCastExpr>() &&
        !parents[0].get<clang::ParenExpr>())
      return nullptr;
    parents = ctx.getParents(parents[0]);
  }
  if (!call) return nullptr;
  const unsigned first = llvm::isa<clang::CXXOperatorCallExpr>(call) ? 1 : 0;
  if (call->getNumArgs() <= first) return nullptr;
  const clang::Expr* r = call->getArg(first)->IgnoreParenImpCasts();
  if (const auto* m = llvm::dyn_cast<clang::CallExpr>(r)) {
    std::string name;
    if (const clang::FunctionDecl* d = m->getDirectCallee())
      name = d->getNameAsString();
    else if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(
                 m->getCallee()->IgnoreParenImpCasts()))
      name = u->getName().getAsString();
    if (name == "move" && m->getNumArgs() == 1)
      r = m->getArg(0)->IgnoreParenImpCasts();
  }
  return r;
}

/// Whether the fold at `fold` folds a parameter of `f` whose type is a
/// template parameter the signature bounds.
bool foldsBoundedParameter(const clang::DeclRefExpr* fold,
                           const clang::FunctionDecl* f,
                           clang::ASTContext& ctx) {
  if (isCountedView(foldedRange(fold, ctx))) return true;
  const auto* r =
      llvm::dyn_cast_or_null<clang::DeclRefExpr>(foldedRange(fold, ctx));
  if (!r || !llvm::isa<clang::ParmVarDecl>(r->getDecl())) return false;
  const clang::QualType t = r->getDecl()
                                ->getType()
                                .getNonReferenceType()
                                .getUnqualifiedType()
                                .getCanonicalType();
  const auto* tp = t->getAs<clang::TemplateTypeParmType>();
  if (!tp) return false;
  return boundedParameters(f).count({tp->getDepth(), tp->getIndex()}) > 0;
}

/// A standard fold called directly, outside a function that states the
/// bound in its signature and not over a counted view: one finding per
/// reference to the fold.
class BoundedFold : public clang::ASTConsumer,
                    public clang::RecursiveASTVisitor<BoundedFold> {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    this->ctx = &ctx;
    TraverseDecl(ctx.getTranslationUnitDecl());
  }
  bool shouldVisitTemplateInstantiations() const { return false; }
  bool VisitDeclRefExpr(clang::DeclRefExpr* e) {
    static const std::set<std::string> folds{
        "fold_left",       "fold_left_first",     "fold_right",
        "fold_right_last", "fold_left_with_iter", "fold_left_first_with_iter"};
    const auto* v = llvm::dyn_cast<clang::VarDecl>(e->getDecl());
    if (!v || !folds.count(v->getNameAsString())) return true;
    const std::string q = v->getQualifiedNameAsString();
    if (q.rfind("std::", 0) != 0 || q.find("ranges::") == std::string::npos)
      return true;
    const clang::SourceManager& sm = ctx->getSourceManager();
    const clang::SourceLocation loc = sm.getExpansionLoc(e->getBeginLoc());
    if (!sm.isInMainFile(loc)) return true;
    const clang::Decl* d = enclosingDecl(e, *ctx);
    if (!d || !inScope(d)) return true;
    if (foldsBoundedParameter(e, enclosingFunction(e, *ctx), *ctx)) return true;
    out() << sm.getFilename(loc) << ':' << sm.getExpansionLineNumber(loc) << ':'
          << sm.getExpansionColumnNumber(loc)
          << ": std::ranges::" << v->getName()
          << " outside a function whose range is IsBoundedRange\n";
    return true;
  }

 private:
  clang::ASTContext* ctx = nullptr;
};

// ---------------------------------------------------------------------------
// converted-result: a converted requirement's result is used at its type
// ---------------------------------------------------------------------------

/// The name of an expression's outermost operation, as NameCollector spells
/// it, or empty where it has none.
std::string outerOperation(const clang::Expr* e) {
  e = e->IgnoreParenImpCasts();
  if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e))
    return clang::BinaryOperator::getOpcodeStr(b->getOpcode()).str();
  if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e))
    return clang::UnaryOperator::getOpcodeStr(u->getOpcode()).str();
  if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(e))
    return clang::getOperatorSpelling(o->getOperator());
  if (const auto* c = llvm::dyn_cast<clang::CallExpr>(e)) {
    const clang::Expr* callee = c->getCallee()->IgnoreParenImpCasts();
    if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee))
      return u->getName().getAsString();
    if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(callee))
      return llvm::isa<clang::VarDecl>(d->getDecl())
                 ? std::string("()")
                 : d->getNameInfo().getAsString();
    if (const auto* m =
            llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(callee))
      return m->getMember().getAsString();
    if (const auto* m = llvm::dyn_cast<clang::MemberExpr>(callee))
      return m->getMemberNameInfo().getAsString();
    return "()";
  }
  if (const auto* m = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(e))
    return m->getMember().getAsString();
  if (const auto* m = llvm::dyn_cast<clang::MemberExpr>(e))
    return m->getMemberNameInfo().getAsString();
  return "";
}

/// What a template's constraints say of each operation's result: converted to
/// a declared type (and whether that type is bool), or exact.
struct ResultTypes {
  std::map<std::string, bool> converted;  // name -> converted to bool
  std::set<std::string> exact;
};

/// A type as the enclosing concept's applied arguments make it: a concept's
/// parameter is read as what the concept was applied to, defaults included.
clang::QualType applied(clang::QualType t,
                        llvm::ArrayRef<clang::TemplateArgument> args) {
  const auto* p = t.getCanonicalType()->getAs<clang::TemplateTypeParmType>();
  if (p && p->getIndex() < args.size() &&
      args[p->getIndex()].getKind() == clang::TemplateArgument::Type)
    return args[p->getIndex()].getAsType();
  return t;
}

/// `args` are the applied arguments of the concept whose body `e` is; empty
/// at a function template's own constraints.  Concepts cannot recurse, so no
/// visited set is needed, and each application is read with its own arguments.
void gatherResults(const clang::Expr* e, ResultTypes& r,
                   llvm::ArrayRef<clang::TemplateArgument> args = {}) {
  if (!e) return;
  e = e->IgnoreParenImpCasts();
  if (const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e)) {
    const clang::ConceptDecl* c = cs->getNamedConcept();
    if (!c || inStd(c)) return;
    llvm::SmallVector<clang::TemplateArgument, 4> inner;
    for (const clang::TemplateArgument& a : cs->getTemplateArguments())
      inner.push_back(
          a.getKind() == clang::TemplateArgument::Type
              ? clang::TemplateArgument(applied(a.getAsType(), args))
              : a);
    gatherResults(c->getConstraintExpr(), r, inner);
    return;
  }
  if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
    // Only a conjunction guarantees both sides; a disjunct may not hold.
    if (b->getOpcode() != clang::BO_LAnd) return;
    gatherResults(b->getLHS(), r, args);
    gatherResults(b->getRHS(), r, args);
    return;
  }
  const auto* q = llvm::dyn_cast<clang::RequiresExpr>(e);
  if (!q) return;
  for (const clang::concepts::Requirement* req : q->getRequirements()) {
    if (const auto* n =
            llvm::dyn_cast<clang::concepts::NestedRequirement>(req)) {
      if (!n->hasInvalidConstraint())
        gatherResults(n->getConstraintExpr(), r, args);
      continue;
    }
    const auto* er = llvm::dyn_cast<clang::concepts::ExprRequirement>(req);
    if (!er || er->isExprSubstitutionFailure()) continue;
    const std::string name = outerOperation(er->getExpr());
    if (name.empty()) continue;
    const auto& ret = er->getReturnTypeRequirement();
    const clang::TypeConstraint* tc =
        ret.isTypeConstraint() ? ret.getTypeConstraint() : nullptr;
    if (!tc) continue;
    const llvm::StringRef concept_name = tc->getNamedConcept()->getName();
    if (concept_name == "same_as") r.exact.insert(name);
    if (concept_name != "convertible_to") continue;  // a shape of the result
    bool toBool = false;
    if (const auto* w = tc->getTemplateArgsAsWritten())
      for (const clang::TemplateArgumentLoc& a : w->arguments())
        if (a.getArgument().getKind() == clang::TemplateArgument::Type)
          toBool = applied(a.getArgument().getAsType(), args)->isBooleanType();
    r.converted.emplace(name, toBool);
  }
}

/// The check: in each function template of the scope, a use of a converted
/// requirement's result other than at the declared type.  A use in a
/// condition, or under !, && or ||, of a result converted to bool, is at bool.
class ConvertedResult : public clang::ASTConsumer,
                        public clang::RecursiveASTVisitor<ConvertedResult> {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    sm = &ctx.getSourceManager();
    TraverseDecl(ctx.getTranslationUnitDecl());
  }
  bool shouldVisitTemplateInstantiations() const { return false; }
  bool TraverseLambdaExpr(clang::LambdaExpr*) { return true; }
  bool TraverseFunctionTemplateDecl(clang::FunctionTemplateDecl* ft) {
    const clang::FunctionDecl* f = ft->getTemplatedDecl();
    if (!f->doesThisDeclarationHaveABody() ||
        !sm->isInMainFile(ft->getLocation()) || !inScope(ft))
      return true;
    results = ResultTypes{};
    const auto gatherHead = [this](const clang::TemplateParameterList* ps) {
      for (const clang::NamedDecl* p : *ps)
        if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
          if (const clang::TypeConstraint* tc = tp->getTypeConstraint())
            gatherResults(tc->getImmediatelyDeclaredConstraint(), results);
      gatherResults(ps->getRequiresClause(), results);
    };
    // A member template's enclosing class templates constrain its body too.
    for (const clang::DeclContext* c = f->getDeclContext(); c;
         c = c->getParent())
      if (const auto* ps =
              llvm::dyn_cast<clang::ClassTemplatePartialSpecializationDecl>(c))
        gatherHead(ps->getTemplateParameters());
      else if (const auto* r = llvm::dyn_cast<clang::CXXRecordDecl>(c))
        if (const clang::ClassTemplateDecl* ct = r->getDescribedClassTemplate())
          gatherHead(ct->getTemplateParameters());
    gatherHead(ft->getTemplateParameters());
    gatherResults(f->getTrailingRequiresClause().ConstraintExpr, results);
    if (!results.converted.empty()) TraverseStmt(f->getBody());
    results = ResultTypes{};  // only this body is judged by these constraints
    return true;
  }
  bool VisitBinaryOperator(clang::BinaryOperator* e) {
    if (e->isCommaOp()) return true;
    judge(e->getLHS(), e->isLogicalOp());
    if (!e->isAssignmentOp()) judge(e->getRHS(), e->isLogicalOp());
    return true;
  }
  bool VisitUnaryOperator(clang::UnaryOperator* e) {
    judge(e->getSubExpr(), e->getOpcode() == clang::UO_LNot);
    return true;
  }
  bool VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* e) {
    const clang::OverloadedOperatorKind k = e->getOperator();
    const bool logical = k == clang::OO_AmpAmp || k == clang::OO_PipePipe ||
                         k == clang::OO_Exclaim;
    const bool firstOnly =
        k == clang::OO_Call || k == clang::OO_Subscript || k == clang::OO_Equal;
    for (unsigned i = 0; i < e->getNumArgs() && (i == 0 || !firstOnly); ++i)
      judge(e->getArg(i), logical);
    return true;
  }
  bool VisitCXXDependentScopeMemberExpr(clang::CXXDependentScopeMemberExpr* e) {
    if (!e->isImplicitAccess()) judge(e->getBase(), false);
    return true;
  }
  bool VisitCallExpr(clang::CallExpr* e) {
    // The result called through its own type; the arguments are not judged.
    // A member callee, a.next in a.next(), names the operation, not a result.
    const clang::Expr* callee = e->getCallee()->IgnoreParenImpCasts();
    if (!llvm::isa<clang::CXXOperatorCallExpr>(e) &&
        !llvm::isa<clang::CXXDependentScopeMemberExpr>(callee) &&
        !llvm::isa<clang::MemberExpr>(callee))
      judge(callee, false);
    return true;
  }
  bool VisitIfStmt(clang::IfStmt* s) {
    judge(s->getCond(), true);
    return true;
  }
  bool VisitWhileStmt(clang::WhileStmt* s) {
    judge(s->getCond(), true);
    return true;
  }
  bool VisitDoStmt(clang::DoStmt* s) {
    judge(s->getCond(), true);
    return true;
  }
  bool VisitForStmt(clang::ForStmt* s) {
    judge(s->getCond(), true);
    return true;
  }
  bool VisitConditionalOperator(clang::ConditionalOperator* e) {
    judge(e->getCond(), true);
    return true;
  }
  bool VisitVarDecl(clang::VarDecl* d) {
    if (!d->hasInit() || !d->getType()->getContainedAutoType()) return true;
    const clang::Expr* init = d->getInit()->IgnoreParenImpCasts();
    if (const auto* list = llvm::dyn_cast<clang::InitListExpr>(init))
      for (const clang::Expr* x : list->inits()) judge(x, false);
    else if (const auto* paren = llvm::dyn_cast<clang::ParenListExpr>(init))
      for (const clang::Expr* x : paren->exprs()) judge(x, false);
    else
      judge(init, false);
    return true;
  }

 private:
  void judge(const clang::Expr* operand, bool contextualBool) {
    if (!operand) return;
    operand = operand->IgnoreParenImpCasts();
    if (!operand->isTypeDependent()) return;
    const std::string name = outerOperation(operand);
    const auto c = results.converted.find(name);
    if (c == results.converted.end() || results.exact.count(name)) return;
    if (contextualBool && c->second) return;
    bool onParameter = false;
    for (const clang::Stmt* child : operand->children())
      if (const auto* x = llvm::dyn_cast_or_null<clang::Expr>(child))
        onParameter |= isParameterTyped(x->IgnoreParenImpCasts());
    if (const auto* c = llvm::dyn_cast<clang::CallExpr>(operand))
      if (const auto* m = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(
              c->getCallee()->IgnoreParenImpCasts()))
        if (!m->isImplicitAccess())
          onParameter |= isParameterTyped(m->getBase()->IgnoreParenImpCasts());
    if (!onParameter) return;
    const clang::SourceLocation loc =
        sm->getExpansionLoc(operand->getBeginLoc());
    out() << sm->getFilename(loc) << ':' << sm->getExpansionLineNumber(loc)
          << ':' << sm->getExpansionColumnNumber(loc) << ": the result of '"
          << name << "' is converted by its requirement, and used here at its "
          << "own type\n";
  }
  const clang::SourceManager* sm = nullptr;
  ResultTypes results;
};

// ---------------------------------------------------------------------------
// context-order: a context on an unstructured head precedes the trait
// ---------------------------------------------------------------------------

/// Whether a function's parameters are unstructured at the type level: each a
/// bare template type parameter or a type that depends on none, so that a
/// context on them has no proper part to descend by.
bool unstructuredHead(const clang::FunctionDecl* f) {
  for (const clang::ParmVarDecl* p : f->parameters()) {
    const clang::QualType t =
        p->getType().getNonReferenceType().getUnqualifiedType();
    if (!t->isDependentType()) continue;
    if (t->getAs<clang::TemplateTypeParmType>()) continue;
    return false;
  }
  return true;
}

/// The concepts a constraint expression names, through conjunction,
/// disjunction and negation; a nested concept's own body is not followed,
/// since the order asked is of the shapes the clause names.
void namedConcepts(const clang::Expr* e,
                   std::vector<const clang::ConceptSpecializationExpr*>& out) {
  if (!e) return;
  e = e->IgnoreParenImpCasts();
  if (const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e)) {
    out.push_back(cs);
    return;
  }
  if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
    namedConcepts(b->getLHS(), out);
    namedConcepts(b->getRHS(), out);
    return;
  }
  if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e))
    namedConcepts(u->getSubExpr(), out);
}

/// The requires clause written after a template's head, `template <...>
/// requires C<T>`, which is not the trailing clause after its declarator.
const clang::Expr* contextOf(const clang::FunctionTemplateDecl* ft) {
  return ft->getTemplateParameters()->getRequiresClause();
}

/// The check: for every trait of the scope, a deleted primary function
/// template, each other function template of the same name with a requires
/// clause and an unstructured head names in that clause only concepts
/// declared before the primary.  A concept from another file precedes by the
/// module order.  A finding is at the instance.
class ContextOrder : public clang::ASTConsumer,
                     public clang::RecursiveASTVisitor<ContextOrder> {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    sm = &ctx.getSourceManager();
    TraverseDecl(ctx.getTranslationUnitDecl());
    for (const clang::FunctionTemplateDecl* ft : instances) {
      const clang::FunctionDecl* f = ft->getTemplatedDecl();
      const auto primary = primaries.find(f->getNameAsString());
      if (primary == primaries.end()) continue;
      std::vector<const clang::ConceptSpecializationExpr*> named;
      namedConcepts(contextOf(ft), named);
      for (const clang::NamedDecl* p : *ft->getTemplateParameters())
        if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
          if (const clang::TypeConstraint* tc = tp->getTypeConstraint())
            namedConcepts(tc->getImmediatelyDeclaredConstraint(), named);
      namedConcepts(f->getTrailingRequiresClause().ConstraintExpr, named);
      for (const auto* cs : named) {
        const clang::ConceptDecl* c = cs->getNamedConcept();
        const clang::SourceLocation at = sm->getExpansionLoc(c->getLocation());
        if (!sm->isInMainFile(at)) continue;
        if (!sm->isBeforeInTranslationUnit(primary->second, at)) continue;
        const clang::SourceLocation loc =
            sm->getExpansionLoc(ft->getLocation());
        out() << sm->getFilename(loc) << ':' << sm->getExpansionLineNumber(loc)
              << ':' << sm->getExpansionColumnNumber(loc) << ": the context of "
              << f->getNameAsString() << " names " << c->getNameAsString()
              << ", declared after the trait's primary\n";
      }
    }
  }
  bool shouldVisitTemplateInstantiations() const { return false; }
  bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl* ft) {
    if (!sm->isInMainFile(ft->getLocation()) || !inScope(ft)) return true;
    const clang::FunctionDecl* f = ft->getTemplatedDecl();
    if (f->isDeleted()) {
      primaries.emplace(f->getNameAsString(),
                        sm->getExpansionLoc(ft->getLocation()));
    } else if (constrained(ft) && unstructuredHead(f)) {
      instances.push_back(ft);
    }
    return true;
  }

 private:
  const clang::SourceManager* sm = nullptr;
  std::map<std::string, clang::SourceLocation> primaries;
  std::vector<const clang::FunctionTemplateDecl*> instances;
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
    out() << sm->getFilename(loc) << ':' << sm->getExpansionLineNumber(loc)
          << ':' << sm->getExpansionColumnNumber(loc)
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
  // A prefix no spelled type has, so that a type such as ns::X is never
  // taken for a parameter.
  return "$p" + std::to_string(p.first) + ":" + std::to_string(p.second);
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
    if (a.getKind() == clang::TemplateArgument::Type) {
      atom.args.push_back(renderArg(a.getAsType(), subst));
    } else if (a.getKind() == clang::TemplateArgument::Integral) {
      // A value argument, as IsMonoid<Op, 1u> has: the atom is the concept at
      // that value and its type, so IsMonoid<Op, 0u> and IsMonoid<Op, 1u> are
      // two facts, and so are C<T, 1> and C<T, 1u> under an auto parameter.
      atom.args.push_back(a.getIntegralType().getCanonicalType().getAsString() +
                          " " + llvm::toString(a.getAsIntegral(), 10));
    } else {
      return;  // a value that depends on a parameter, a template: not read
    }
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
      // A call at a type variable: an argument of a type known only through
      // the parameters, or an explicit template argument of one, as
      // power<E>(x, n) passes the monoid E.
      bool atParameter = anyParameterTyped(args);
      for (const clang::TemplateArgumentLoc& ta : u->template_arguments())
        if (ta.getArgument().getKind() == clang::TemplateArgument::Type &&
            parameterKnown(ta.getArgument().getAsType()))
          atParameter = true;
      if (!atParameter) continue;
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
        // The explicit template arguments bind the candidate's leading
        // parameters, as power<E> binds the monoid.
        {
          const clang::TemplateParameterList* cps =
              cand->getTemplateParameters();
          unsigned k = 0;
          for (const clang::TemplateArgumentLoc& ta : u->template_arguments()) {
            if (k >= cps->size()) break;
            if (ta.getArgument().getKind() == clang::TemplateArgument::Type)
              if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(
                      cps->getParam(k))) {
                const std::string r =
                    renderArg(ta.getArgument().getAsType(), {});
                toCaller[{tp->getDepth(), tp->getIndex()}] = r;
                callerArgs.insert(r);
              }
            ++k;
          }
        }
        for (unsigned k = 0; k < cf->getNumParams() && k < args.size(); ++k)
          if (std::optional<Param> from =
                  paramOf(cf->getParamDecl(k)->getType());
              from && !toCaller.count(*from)) {
            // An explicit template argument binds first: g<T>(0) binds the
            // parameter to T and converts the 0, as C++ does.
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
            if (arg.rfind("$p", 0) == 0 && !callerArgs.count(arg))
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
      out() << sm->getFilename(loc) << ':' << sm->getExpansionLineNumber(loc)
            << ':' << sm->getExpansionColumnNumber(loc) << ": the call to '"
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
    for (const auto& [kind, n] : counts) out() << kind << ' ' << n << '\n';
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
    // The wrappers the compiler adds around what was written are counted
    // through the written node below them, not as kinds of their own.
    if (inside &&
        !llvm::isa<clang::ImplicitCastExpr, clang::MaterializeTemporaryExpr,
                   clang::ExprWithCleanups, clang::CXXBindTemporaryExpr,
                   clang::ConstantExpr, clang::CXXDefaultArgExpr,
                   clang::CXXDefaultInitExpr, clang::OpaqueValueExpr,
                   clang::SubstNonTypeTemplateParmExpr>(st))
      ++counts[std::string("Stmt:") + st->getStmtClassName()];
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

// ---------------------------------------------------------------------------
// admit: the second stage, a type check at archetypes of the shapes
// ---------------------------------------------------------------------------

/// What a type in a constraint is, read off its structure: a template
/// parameter, the carrier of one (decltype(E{}())), the element type of a
/// range parameter (range_value_t<R>), a trait's associated type at one
/// (typename op_of<E>::type), or something else.
struct Term {
  enum Kind {
    Other,
    Parameter,
    CarrierOf,
    RangeValueOf,
    Associated,
    Member  // a member type of a parameter, typename F::Domain
  } kind = Other;
  Param p{};
  const clang::ClassTemplateDecl* trait = nullptr;  // for Associated
  std::string member;                               // for Member
};
using TermSubst = std::map<Param, Term>;

Term classify(clang::QualType t, const TermSubst& s, int depth = 0) {
  Term none;
  if (t.isNull() || depth > 16) return none;
  t = t.getNonReferenceType().getUnqualifiedType();
  if (const auto* st = t->getAs<clang::SubstTemplateTypeParmType>())
    return classify(st->getReplacementType(), s, depth + 1);
  if (const auto* p = t->getAs<clang::TemplateTypeParmType>()) {
    const Param key{p->getDepth(), p->getIndex()};
    if (auto it = s.find(key); it != s.end()) return it->second;
    return Term{Term::Parameter, key, nullptr};
  }
  if (const auto* ts = t->getAs<clang::TemplateSpecializationType>()) {
    const clang::TemplateDecl* td = ts->getTemplateName().getAsTemplateDecl();
    if (td && td->getName() == "range_value_t" && inStd(td) &&
        ts->template_arguments().size() == 1 &&
        ts->template_arguments()[0].getKind() ==
            clang::TemplateArgument::Type) {
      Term r = classify(ts->template_arguments()[0].getAsType(), s, depth + 1);
      if (r.kind == Term::Parameter)
        return Term{Term::RangeValueOf, r.p, nullptr};
      return none;
    }
    if (ts->isTypeAlias()) return classify(ts->getAliasedType(), s, depth + 1);
  }
  // The standard library's remove_cvref_t and its kin, when the library
  // spells them with the compiler's built-in (libc++: __remove_cvref): at an
  // archetype, the type itself.
  if (const auto* ut = t->getAs<clang::UnaryTransformType>())
    return classify(ut->getBaseType(), s, depth + 1);
  if (const auto* dt = llvm::dyn_cast<clang::DecltypeType>(t.getTypePtr())) {
    // decltype(E{}()): the type of a call of a value of E, its carrier.
    const auto* call = llvm::dyn_cast<clang::CallExpr>(
        dt->getUnderlyingExpr()->IgnoreParenImpCasts());
    if (call && call->getNumArgs() == 0) {
      const clang::Expr* callee = call->getCallee()->IgnoreParenImpCasts();
      clang::QualType ct;
      if (const auto* u =
              llvm::dyn_cast<clang::CXXUnresolvedConstructExpr>(callee))
        ct = u->getTypeAsWritten();
      else if (const auto* o =
                   llvm::dyn_cast<clang::CXXTemporaryObjectExpr>(callee))
        ct = o->getType();
      Term e = classify(ct, s, depth + 1);
      if (e.kind == Term::Parameter) return Term{Term::CarrierOf, e.p, nullptr};
    }
    return none;
  }
  if (const auto* dn = t->getAs<clang::DependentNameType>()) {
    const clang::NestedNameSpecifier q = dn->getQualifier();
    const clang::Type* qt = q ? q.getAsType() : nullptr;
    if (!qt) return none;
    const std::string name =
        dn->getIdentifier() ? dn->getIdentifier()->getName().str() : "";
    if (const auto* ts = qt->getAs<clang::TemplateSpecializationType>())
      if (const auto* ct = llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(
              ts->getTemplateName().getAsTemplateDecl()))
        if (ts->template_arguments().size() == 1 &&
            ts->template_arguments()[0].getKind() ==
                clang::TemplateArgument::Type) {
          const clang::QualType arg = ts->template_arguments()[0].getAsType();
          const llvm::StringRef tn = ct->getName();
          // The standard's identity-like traits: at an archetype,
          // remove_cvref<F>::type is F.
          if (inStd(ct) && name == "type" &&
              (tn == "remove_cvref" || tn == "remove_cv" ||
               tn == "remove_reference" || tn == "type_identity" ||
               tn == "decay"))
            return classify(arg, s, depth + 1);
          // The library's representative of an object: an archetype carrier
          // presents only itself, so archetype<C>::type is C.
          if (!inStd(ct) && tn == "archetype" && name == "type")
            return classify(arg, s, depth + 1);
          // typename X<P>::type: the associated type a trait gives at P.
          // Another member of a trait, SpeciesTraits<T>::Domain, is nothing
          // the archetypes provide.
          if (name != "type") return none;
          Term a = classify(arg, s, depth + 1);
          if (a.kind == Term::Parameter) return Term{Term::Associated, a.p, ct};
          return none;
        }
    // typename F::Domain: a member type of a parameter.
    const Term owner = classify(clang::QualType(qt, 0), s, depth + 1);
    if (owner.kind == Term::Parameter && !name.empty())
      return Term{Term::Member, owner.p, nullptr, name};
    return none;
  }
  return none;
}

/// What a template's constraints ask of its parameters, read through its
/// shapes: the facts the archetypes are built from.
struct Facts {
  std::set<Param> unit;                                 // E{}()
  std::map<Param, const clang::ClassTemplateDecl*> op;  // a binary op_of<E>
  std::map<Param, std::pair<Param, Param>> arrow;       // F : C_A -> C_B
  std::map<Param, Param> carrierOf;                     // T same as C_E
  std::map<Param, std::optional<Param>> range;          // R, its elements
  std::set<Param> sized;                                // R is a sized_range
  std::set<Param> comparable;                           // C_E has ==
  std::set<Param> defaulted;  // C_E or T has a default value (semiregular)
  // T is a carrier itself: a parameter only the standard's foundational
  // concepts constrain (copyable, equality_comparable, semiregular, regular).
  std::set<Param> plain;
  // same_as between member types of parameters and parameters: equations,
  // one carrier per class (Cod<F> == Dom<G> in Compose).
  std::vector<std::pair<Term, Term>> sames;
  // Results a shape asks only to be convertible (-> convertible_to<T>): the
  // archetype returns converts_to<T>, so the body converts them at once.
  std::set<Param> opConverts, arrowConverts, memberConverts;
  // F has member types (typename F::Domain), and a const call from one of
  // them to another: the library's arrow (IsArrow).
  std::map<Param, std::set<std::string>> members;
  std::map<Param, std::pair<std::string, std::string>> memberCall;
  struct Law {
    const clang::VarTemplateDecl* decl;
    std::vector<Param> args;
  };
  std::vector<Law> laws;
  // An operation is recorded only once its result is constrained to the
  // carrier: until then it is unconfirmed (Copilot on #1211).
  std::set<Param> opUnconfirmed;
  std::optional<Param> lastOp;
  std::string undecided;          // the first requirement no rule reads
  std::vector<std::string> path;  // the shapes followed to reach it
  // Parameters a shape constrains to the count atom (std::unsigned_integral):
  // their archetype is a count, std::size_t, and a constraint on them alone is
  // left to the compiler at that type.
  std::set<Param> atoms;
  std::string where() const {
    std::string w;
    for (const std::string& c : path) w += (w.empty() ? " (in " : " > ") + c;
    return w.empty() ? w : w + ")";
  }
};

void readConstraint(const clang::Expr* e, const TermSubst& s, Facts& f,
                    int depth = 0);

/// A requirement's expression: a unit, an operation, an arrow, or a call to a
/// function of the program (nothing to build: its own constraints are checked
/// when the appendix instantiates it).
void readRequiredCall(const clang::Expr* e,
                      const std::map<const clang::VarDecl*, Term>& locals,
                      const TermSubst& s, Facts& f) {
  const auto* call = llvm::dyn_cast<clang::CallExpr>(e->IgnoreParenImpCasts());
  if (!call) {
    f.undecided = "a requirement that is not a call";
    return;
  }
  const clang::Expr* callee = call->getCallee()->IgnoreParenImpCasts();
  auto localTerm = [&](const clang::Expr* a) -> Term {
    if (const auto* d =
            llvm::dyn_cast<clang::DeclRefExpr>(a->IgnoreParenImpCasts()))
      if (const auto* v = llvm::dyn_cast<clang::VarDecl>(d->getDecl()))
        if (auto it = locals.find(v); it != locals.end()) return it->second;
    return Term{};
  };
  // E{}(): a unit.  op_t<E>{}(a, b): a binary operation on the carrier.
  clang::QualType constructed;
  if (const auto* u = llvm::dyn_cast<clang::CXXUnresolvedConstructExpr>(callee))
    constructed = u->getTypeAsWritten();
  else if (const auto* o =
               llvm::dyn_cast<clang::CXXTemporaryObjectExpr>(callee))
    constructed = o->getType();
  if (!constructed.isNull()) {
    const Term c = classify(constructed, s);
    if (c.kind == Term::Parameter && call->getNumArgs() == 0) {
      f.unit.insert(c.p);
      return;
    }
    if (c.kind == Term::Associated && call->getNumArgs() == 2) {
      const Term a = localTerm(call->getArg(0)), b = localTerm(call->getArg(1));
      if (a.kind == Term::CarrierOf && b.kind == Term::CarrierOf &&
          a.p == c.p && b.p == c.p) {
        f.op[c.p] = c.trait;
        f.opUnconfirmed.insert(c.p);
        f.lastOp = c.p;
        return;
      }
    }
    f.undecided = "a call of a constructed value of another form";
    return;
  }
  // f(x): an arrow between two carriers, or, with x of one of f's member
  // types, the library's arrow.
  const Term fn = localTerm(callee);
  if (fn.kind == Term::Parameter && call->getNumArgs() == 1) {
    const Term x = localTerm(call->getArg(0));
    if (x.kind == Term::Member && x.p == fn.p) {
      f.members[fn.p].insert(x.member);
      f.memberCall[fn.p] = {x.member, ""};  // the result from the constraint
      return;
    }
    if (x.kind == Term::CarrierOf) {
      // The codomain comes only from a result constraint; without one it
      // stays unknown, and no archetype is built.
      f.arrow[fn.p] = {x.p, Param{~0u, ~0u}};
      return;
    }
  }
  // A call to a function of the program, by name.
  if (llvm::isa<clang::UnresolvedLookupExpr>(callee)) return;
  f.undecided = std::string("a required call of another form: ") +
                callee->getStmtClassName();
}

void readRequires(const clang::RequiresExpr* r, const TermSubst& s, Facts& f) {
  std::map<const clang::VarDecl*, Term> locals;
  for (const clang::ParmVarDecl* p : r->getLocalParameters())
    locals[p] = classify(p->getType(), s);
  for (const clang::concepts::Requirement* req : r->getRequirements()) {
    if (!f.undecided.empty()) return;
    if (const auto* er =
            llvm::dyn_cast<clang::concepts::ExprRequirement>(req)) {
      if (er->isExprSubstitutionFailure()) {
        f.undecided = "a requirement that cannot be read";
        return;
      }
      f.lastOp.reset();
      readRequiredCall(er->getExpr(), locals, s, f);
      const auto& ret = er->getReturnTypeRequirement();
      if (ret.isTypeConstraint()) {
        // -> same_as<carrier_t<E>> on an operation: its result is the carrier.
        if (f.lastOp)
          if (const auto* rc = llvm::dyn_cast<clang::ConceptSpecializationExpr>(
                  ret.getTypeConstraint()->getImmediatelyDeclaredConstraint()))
            if ((rc->getNamedConcept()->getName() == "same_as" ||
                 rc->getNamedConcept()->getName() == "convertible_to") &&
                !rc->getTemplateArgsAsWritten()->arguments().empty()) {
              const Term r = classify(rc->getTemplateArgsAsWritten()
                                          ->arguments()
                                          .back()
                                          .getArgument()
                                          .getAsType(),
                                      s);
              if (r.kind == Term::CarrierOf && r.p == *f.lastOp) {
                f.opUnconfirmed.erase(r.p);
                if (rc->getNamedConcept()->getName() == "convertible_to")
                  f.opConverts.insert(r.p);
              }
            }
        // -> same_as<carrier_t<B>>: fixes an arrow's codomain.
        const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(
            ret.getTypeConstraint()->getImmediatelyDeclaredConstraint());
        // The library's arrow: f(x) -> Presents<F::Codomain> or same_as: the
        // archetype returns exactly the codomain's carrier, which meets each
        // of them by its plain branch; convertible_to: a converts_to of it.
        if (cs && !cs->getTemplateArgsAsWritten()->arguments().empty()) {
          const llvm::StringRef rn = cs->getNamedConcept()->getName();
          const Term to = classify(cs->getTemplateArgsAsWritten()
                                       ->arguments()
                                       .back()
                                       .getArgument()
                                       .getAsType(),
                                   s);
          const auto* mc = llvm::dyn_cast<clang::CallExpr>(
              er->getExpr()->IgnoreParenImpCasts());
          if (mc && to.kind == Term::Member &&
              (rn == "same_as" || rn == "convertible_to" || rn == "Presents"))
            if (auto it = f.memberCall.find(to.p);
                it != f.memberCall.end() && it->second.second.empty()) {
              it->second.second = to.member;
              f.members[to.p].insert(to.member);
              // Presents is convertible_to or a presentation the object
              // names; at a fresh carrier only the conversion, at once.
              if (rn == "convertible_to" || rn == "Presents")
                f.memberConverts.insert(to.p);
              continue;
            }
        }
        const bool converts =
            cs && cs->getNamedConcept()->getName() == "convertible_to";
        if (!cs ||
            (cs->getNamedConcept()->getName() != "same_as" && !converts)) {
          f.undecided =
              "a result constraint other than same_as or convertible_to: " +
              (cs ? cs->getNamedConcept()->getName().str() : std::string("?")) +
              f.where();
          return;
        }
        const auto& args = cs->getTemplateArgsAsWritten()->arguments();
        if (args.empty()) continue;
        const Term to = classify(args.back().getArgument().getAsType(), s);
        const auto* call = llvm::dyn_cast<clang::CallExpr>(
            er->getExpr()->IgnoreParenImpCasts());
        if (call && to.kind == Term::CarrierOf)
          for (auto& [fp, ab] : f.arrow)
            if (call->getNumArgs() == 1) {
              if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(
                      call->getCallee()->IgnoreParenImpCasts()))
                if (const auto* v =
                        llvm::dyn_cast<clang::VarDecl>(d->getDecl()))
                  if (auto it = locals.find(v);
                      it != locals.end() && it->second.p == fp) {
                    ab.second = to.p;
                    if (converts) f.arrowConverts.insert(fp);
                  }
            }
      }
    } else if (const auto* tr =
                   llvm::dyn_cast<clang::concepts::TypeRequirement>(req)) {
      if (tr->isSubstitutionFailure()) {
        f.undecided = "a type requirement that cannot be read";
        return;
      }
      // typename op_t<E>: read with the call that uses it; typename
      // F::Domain: a member type F must have.
      // Any other type a shape asks to exist, SpeciesTraits<T>::Domain, is
      // nothing the archetypes provide, so the check stops there.
      const Term m = classify(tr->getType()->getType(), s);
      if (m.kind == Term::Member) {
        f.members[m.p].insert(m.member);
      } else if (m.kind != Term::Associated) {
        f.undecided = "a type requirement of another form: " +
                      tr->getType()->getType().getAsString() + f.where();
        return;
      }
    } else if (const auto* nr =
                   llvm::dyn_cast<clang::concepts::NestedRequirement>(req)) {
      if (nr->hasInvalidConstraint()) {
        f.undecided = "a nested requirement that cannot be read";
        return;
      }
      readConstraint(nr->getConstraintExpr(), s, f);
    }
  }
}

/// A law: a variable template of the program read at parameters.
bool readLaw(const clang::Expr* e, const TermSubst& s, Facts& f) {
  const clang::VarTemplateDecl* vt = nullptr;
  llvm::ArrayRef<clang::TemplateArgumentLoc> args;
  if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(e)) {
    for (const clang::NamedDecl* d : u->decls())
      if ((vt = llvm::dyn_cast<clang::VarTemplateDecl>(d))) break;
    args = u->template_arguments();
  } else if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e)) {
    if (const auto* spec =
            llvm::dyn_cast<clang::VarTemplateSpecializationDecl>(d->getDecl()))
      vt = spec->getSpecializedTemplate();
    else if (const auto* v = llvm::dyn_cast<clang::VarDecl>(d->getDecl()))
      vt = v->getDescribedVarTemplate();
    args = d->template_arguments();
  }
  if (!vt || inStd(vt) || args.empty()) return false;
  Facts::Law law{vt, {}};
  for (const clang::TemplateArgumentLoc& a : args) {
    if (a.getArgument().getKind() != clang::TemplateArgument::Type)
      return false;
    const Term t = classify(a.getArgument().getAsType(), s);
    if (t.kind != Term::Parameter) return false;
    law.args.push_back(t.p);
  }
  f.laws.push_back(law);
  return true;
}

/// The parameters the shapes constrain to the count atom, followed through
/// conjunctions and shapes of the program.
void findAtoms(const clang::Expr* e, const TermSubst& s, Facts& f,
               int depth = 0) {
  if (!e || depth > 32) return;
  e = e->IgnoreParenImpCasts();
  if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
    if (b->getOpcode() == clang::BO_LAnd) {
      findAtoms(b->getLHS(), s, f, depth + 1);
      findAtoms(b->getRHS(), s, f, depth + 1);
    }
    return;
  }
  const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e);
  if (!cs) return;
  const clang::ConceptDecl* c = cs->getNamedConcept();
  std::vector<Term> args;
  for (const clang::TemplateArgument& a : cs->getTemplateArguments())
    args.push_back(a.getKind() == clang::TemplateArgument::Type
                       ? classify(a.getAsType(), s)
                       : Term{});
  if (inStd(c)) {
    const llvm::StringRef n = c->getName();
    if ((n == "unsigned_integral" || n == "signed_integral" ||
         n == "integral") &&
        args.size() == 1 && args[0].kind == Term::Parameter)
      f.atoms.insert(args[0].p);
    return;
  }
  TermSubst inner;
  const clang::TemplateParameterList* ps = c->getTemplateParameters();
  for (unsigned k = 0; k < ps->size() && k < args.size(); ++k)
    if (const auto* tp =
            llvm::dyn_cast<clang::TemplateTypeParmDecl>(ps->getParam(k)))
      inner[{tp->getDepth(), tp->getIndex()}] = args[k];
  findAtoms(c->getConstraintExpr(), inner, f, depth + 1);
}

/// Whether an expression of a constraint mentions only atom parameters (and
/// no other parameter): then the compiler decides it at the atom's archetype.
bool onlyAtoms(const clang::Expr* e, const TermSubst& s, const Facts& f) {
  struct Params : clang::RecursiveASTVisitor<Params> {
    const TermSubst& s;
    const Facts& f;
    bool atom = false, other = false;
    Params(const TermSubst& s, const Facts& f) : s(s), f(f) {}
    void see(clang::QualType t) {
      const Term r = classify(t, s);
      if (r.kind == Term::Parameter)
        (f.atoms.count(r.p) ? atom : other) = true;
      else if (!t.isNull() && t->isDependentType())
        other = true;
    }
    bool VisitTypeLoc(clang::TypeLoc tl) {
      if (tl.getType()->getAs<clang::TemplateTypeParmType>()) see(tl.getType());
      return true;
    }
    bool VisitConceptSpecializationExpr(clang::ConceptSpecializationExpr* c) {
      for (const clang::TemplateArgument& a : c->getTemplateArguments())
        if (a.getKind() == clang::TemplateArgument::Type) see(a.getAsType());
      return true;
    }
  } v(s, f);
  v.TraverseStmt(const_cast<clang::Expr*>(e));
  return v.atom && !v.other;
}

void readConstraint(const clang::Expr* e, const TermSubst& s, Facts& f,
                    int depth) {
  if (!e || !f.undecided.empty()) return;
  if (!f.atoms.empty() && onlyAtoms(e, s, f)) return;  // the compiler decides
  if (depth > 32) {
    f.undecided = "constraints nested too deep";
    return;
  }
  e = e->IgnoreParenImpCasts();
  if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
    if (b->getOpcode() != clang::BO_LAnd) {
      f.undecided = "a disjunction in a constraint";
      return;
    }
    readConstraint(b->getLHS(), s, f, depth + 1);
    readConstraint(b->getRHS(), s, f, depth + 1);
    return;
  }
  if (const auto* r = llvm::dyn_cast<clang::RequiresExpr>(e)) {
    readRequires(r, s, f);
    return;
  }
  if (const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e)) {
    const clang::ConceptDecl* c = cs->getNamedConcept();
    std::vector<Term> args;
    for (const clang::TemplateArgument& a : cs->getTemplateArguments())
      args.push_back(a.getKind() == clang::TemplateArgument::Type
                         ? classify(a.getAsType(), s)
                         : Term{});
    if (inStd(c)) {
      const llvm::StringRef n = c->getName();
      // The foundational concepts (the Palo Alto report, N3351, 3.3): what
      // the carrier gets, and nothing more.  Copying is the kernel's, every
      // carrier copies; equality is a call (Jlt); a default value is
      // partially formed (Elements of Programming, 1.5), local state written
      // before it is read (Jlt).
      if (args.size() == 1 && (args[0].kind == Term::Parameter ||
                               args[0].kind == Term::CarrierOf)) {
        const Param q = args[0].p;
        const bool par = args[0].kind == Term::Parameter;
        const bool copies = n == "copyable" || n == "copy_constructible" ||
                            n == "movable" || n == "move_constructible" ||
                            n == "destructible";
        const bool eq = n == "equality_comparable" || n == "regular";
        const bool def = n == "semiregular" || n == "regular" ||
                         n == "default_initializable";
        if (copies || eq || def) {
          if (par) f.plain.insert(q);
          if (eq) f.comparable.insert(q);
          if (def) f.defaulted.insert(q);
          return;
        }
      }
      if ((n == "sized_range" || n == "range" || n == "input_range") &&
          args.size() == 1 && args[0].kind == Term::Parameter) {
        f.range.try_emplace(args[0].p);
        if (n == "sized_range") f.sized.insert(args[0].p);
        return;
      }
      // An equation between member types, or a member type and a parameter:
      // the two are one carrier.
      if (n == "same_as" && args.size() == 2 &&
          (args[0].kind == Term::Member || args[1].kind == Term::Member) &&
          (args[0].kind == Term::Member || args[0].kind == Term::Parameter) &&
          (args[1].kind == Term::Member || args[1].kind == Term::Parameter)) {
        for (const Term& t : args)
          if (t.kind == Term::Member) f.members[t.p].insert(t.member);
        f.sames.emplace_back(args[0], args[1]);
        return;
      }
      if (n == "same_as" && args.size() == 2 &&
          args[1].kind == Term::CarrierOf) {
        if (args[0].kind == Term::Parameter) {
          f.carrierOf[args[0].p] = args[1].p;
          return;
        }
        if (args[0].kind == Term::RangeValueOf) {
          f.range[args[0].p] = args[1].p;
          return;
        }
      }
      f.undecided =
          "the standard's " + n.str() + " at these arguments" + f.where();
      return;
    }
    // A shape of the program: its body, its parameters read as the arguments.
    TermSubst inner;
    const clang::TemplateParameterList* ps = c->getTemplateParameters();
    for (unsigned k = 0; k < ps->size() && k < args.size(); ++k)
      if (const auto* tp =
              llvm::dyn_cast<clang::TemplateTypeParmDecl>(ps->getParam(k)))
        inner[{tp->getDepth(), tp->getIndex()}] = args[k];
    f.path.push_back(c->getName().str());
    readConstraint(c->getConstraintExpr(), inner, f, depth + 1);
    if (f.undecided.empty()) f.path.pop_back();
    return;
  }
  if (readLaw(e, s, f)) return;
  std::string what = e->getStmtClassName();
  if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(e))
    what += " " + u->getName().getAsString();
  else if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e))
    what += " " + d->getDecl()->getNameAsString();
  else if (const auto* c = llvm::dyn_cast<clang::CallExpr>(e))
    if (const clang::FunctionDecl* fd = c->getDirectCallee())
      what += " " + fd->getNameAsString();
  f.undecided = "a constraint of another form: " + what + f.where();
}

/// The outcome of the type check for one function template.
struct Outcome {
  enum Kind { Admitted, NotAdmitted, Undecided } kind;
  std::string why;
};

/// Errors of the second parse.
class Errors : public clang::DiagnosticConsumer {
 public:
  unsigned n = 0;
  std::string first;
  bool inAppendix = false;      // the first error is the tool's, not the body's
  bool marked = false;          // the use reached the marked template
  unsigned appendixFrom = ~0u;  // the main file's offset where it begins
  bool inBody = false;          // the first error is in the checked body
  bool inMain = false;          // the first error is in the main file
  unsigned bodyFrom = 0, bodyTo = 0;  // the checked body's offsets
  void HandleDiagnostic(clang::DiagnosticsEngine::Level l,
                        const clang::Diagnostic& d) override {
    clang::DiagnosticConsumer::HandleDiagnostic(l, d);
    llvm::SmallString<160> s;
    d.FormatDiagnostic(s);
    if (llvm::StringRef(s).contains("jlt-admit-checked")) marked = true;
    if (l < clang::DiagnosticsEngine::Error) return;
    if (n++ == 0) {
      first = s.str().str();
      if (d.hasSourceManager() && d.getLocation().isValid()) {
        const clang::SourceManager& sm = d.getSourceManager();
        const clang::SourceLocation loc = sm.getExpansionLoc(d.getLocation());
        inMain = sm.isInMainFile(loc);
        const unsigned at = inMain ? sm.getFileOffset(loc) : 0;
        inAppendix = (inMain && at >= appendixFrom) ||
                     sm.getFilename(loc).ends_with("archetypes.hpp");
        inBody = inMain && at >= bodyFrom && at <= bodyTo;
      }
    }
  }
};

/// The second parse's own reading of which function the appendix's use
/// names, from the syntax tree: whether it resolved at all, and whether the
/// function carries the marker (a failed body suppresses the deprecation
/// warning and leaves the use unresolved, so the warning cannot be relied on).
struct Selection {
  bool resolved = false;
  bool marked = false;
  // A result a shape promises only convertible_to<T> passed straight to a
  // call: converted to whatever parameter type resolution picks there, which
  // a real type's overloads may pick differently.  Table 7's derived form is
  // copy-initialisation of a T, so the result goes into a declared T or a
  // return at T, and nowhere else.
  bool lateConversion = false;
  // A callee of the checked body whose constraints contain a negation,
  // !C<...>: at an archetype every law it does not declare is false, so the
  // negation holds there by default and fails at a model of C (the
  // theoretician's counterexample on #1212).
  bool negatedCallee = false;
};

/// Whether a constraint expression contains a negation, followed through the
/// concepts it names (depth-limited).
bool containsNegation(const clang::Expr* e, int depth = 0) {
  if (!e || depth > 16) return false;
  e = e->IgnoreParenImpCasts();
  if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e))
    if (u->getOpcode() == clang::UO_LNot) return true;
  if (const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e))
    if (!inStd(cs->getNamedConcept()) &&
        containsNegation(cs->getNamedConcept()->getConstraintExpr(), depth + 1))
      return true;
  for (const clang::Stmt* c : e->children())
    if (const auto* ce = llvm::dyn_cast_or_null<clang::Expr>(c))
      if (containsNegation(ce, depth + 1)) return true;
  return false;
}

/// Whether an expression is the conversion of a converts_to archetype.
bool convertsProxy(const clang::Expr* e) {
  e = e->IgnoreParenImpCasts();
  if (const auto* m = llvm::dyn_cast<clang::MaterializeTemporaryExpr>(e))
    e = m->getSubExpr()->IgnoreParenImpCasts();
  if (const auto* b = llvm::dyn_cast<clang::CXXBindTemporaryExpr>(e))
    e = b->getSubExpr()->IgnoreParenImpCasts();
  const auto* mc = llvm::dyn_cast<clang::CXXMemberCallExpr>(e);
  if (!mc) return false;
  const auto* conv =
      llvm::dyn_cast_or_null<clang::CXXConversionDecl>(mc->getMethodDecl());
  return conv && conv->getParent()->getName() == "converts_to";
}
class MarkedUse : public clang::ASTConsumer {
 public:
  explicit MarkedUse(Selection& sel) : sel(sel) {}
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    struct Find : clang::RecursiveASTVisitor<Find> {
      Selection& sel;
      explicit Find(Selection& s) : sel(s) {}
      bool VisitVarDecl(clang::VarDecl* v) {
        if (!v->getName().starts_with("use") || !v->getInit()) return true;
        const auto* ns =
            llvm::dyn_cast<clang::NamespaceDecl>(v->getDeclContext());
        if (!ns || ns->getName() != "jlt_admit") return true;
        const clang::Expr* e = v->getInit()->IgnoreParenImpCasts();
        const clang::FunctionDecl* f = nullptr;
        if (const auto* l = llvm::dyn_cast<clang::LambdaExpr>(e)) {
          // The call form: the first call in the lambda's body, wherever
          // cleanups and temporaries put it.
          struct First : clang::RecursiveASTVisitor<First> {
            const clang::FunctionDecl* f = nullptr;
            bool VisitCallExpr(clang::CallExpr* c) {
              if (!f) f = c->getDirectCallee();
              return !f;
            }
          } first;
          first.TraverseStmt(const_cast<clang::Stmt*>(l->getBody()));
          f = first.f;
        } else {
          if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e))
            e = u->getSubExpr()->IgnoreParenImpCasts();
          if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e))
            f = llvm::dyn_cast<clang::FunctionDecl>(d->getDecl());
        }
        if (!f) return true;
        sel.resolved = true;
        for (const clang::FunctionDecl* g :
             {f, static_cast<const clang::FunctionDecl*>(
                     f->getTemplateInstantiationPattern())})
          if (g)
            if (const auto* a = g->getAttr<clang::DeprecatedAttr>())
              if (a->getMessage() == "jlt-admit-checked") sel.marked = true;
        return true;
      }
    } find(sel);
    find.TraverseDecl(ctx.getTranslationUnitDecl());
    // The instantiated body of the checked template: a converts_to result
    // as a call's argument.
    struct Late : clang::RecursiveASTVisitor<Late> {
      Selection& sel;
      explicit Late(Selection& s) : sel(s) {}
      bool shouldVisitTemplateInstantiations() const { return true; }
      bool inChecked = false;
      bool TraverseFunctionDecl(clang::FunctionDecl* f) {
        const clang::FunctionDecl* pat = f->getTemplateInstantiationPattern();
        const bool checked =
            pat && pat->getAttr<clang::DeprecatedAttr>() &&
            pat->getAttr<clang::DeprecatedAttr>()->getMessage() ==
                "jlt-admit-checked";
        const bool was = inChecked;
        inChecked = inChecked || checked;
        clang::RecursiveASTVisitor<Late>::TraverseFunctionDecl(f);
        inChecked = was;
        return true;
      }
      // A conversion of the proxy is admitted only where a declared T is
      // initialised or a T is returned (Table 7: copy-initialisation of a
      // T); an argument, an operand, a branch of ?: or a constructor's
      // argument is late.  A proxy returned unconverted is late too.
      clang::ASTContext* ctx = nullptr;
      bool VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* m) {
        if (!inChecked || !convertsProxy(m)) return true;
        clang::DynTypedNode n = clang::DynTypedNode::create(*m);
        for (int depth = 0; depth < 16; ++depth) {
          auto ps = ctx->getParents(n);
          if (ps.empty()) {  // under a list's semantic form: not a T
            sel.lateConversion = true;
            return true;
          }
          n = ps[0];
          if (const auto* x = n.get<clang::Expr>()) {
            if (llvm::isa<clang::ImplicitCastExpr, clang::ParenExpr,
                          clang::MaterializeTemporaryExpr,
                          clang::CXXBindTemporaryExpr, clang::ExprWithCleanups>(
                    x))
              continue;
            if (llvm::isa<clang::CXXFunctionalCastExpr,
                          clang::CXXStaticCastExpr>(x))
              return true;  // the conversion written out
            sel.lateConversion = true;
            return true;
          }
          if (n.get<clang::VarDecl>() || n.get<clang::ReturnStmt>())
            return true;
          sel.lateConversion = true;
          return true;
        }
        return true;
      }
      // A brace list's conversions live in its semantic form, which the
      // traversal does not visit: an element that converts the proxy is
      // late (an aggregate's field, not a declared T).
      bool VisitCallExpr(clang::CallExpr* c) {
        if (!inChecked) return true;
        const clang::FunctionDecl* callee = c->getDirectCallee();
        const clang::FunctionTemplateDecl* t =
            callee ? callee->getPrimaryTemplate() : nullptr;
        if (!t) return true;
        llvm::SmallVector<clang::AssociatedConstraint, 4> ac;
        t->getAssociatedConstraints(ac);
        for (const clang::AssociatedConstraint& a : ac)
          if (containsNegation(a.ConstraintExpr)) sel.negatedCallee = true;
        return true;
      }
      bool VisitInitListExpr(clang::InitListExpr* l) {
        if (!inChecked) return true;
        const clang::InitListExpr* sem =
            l->isSemanticForm() ? l : l->getSemanticForm();
        if (sem)
          for (const clang::Expr* i : sem->inits())
            if (i && convertsProxy(i)) sel.lateConversion = true;
        return true;
      }
      // A local of the proxy's type keeps it, and std::move lets it convert
      // later: late (Copilot on #1212).
      bool VisitVarDecl(clang::VarDecl* v) {
        if (inChecked)
          if (const auto* rd = v->getType()->getAsCXXRecordDecl())
            if (rd->getName() == "converts_to") sel.lateConversion = true;
        return true;
      }
      bool VisitReturnStmt(clang::ReturnStmt* r) {
        if (inChecked && r->getRetValue())
          if (const auto* rd =
                  r->getRetValue()->getType()->getAsCXXRecordDecl())
            if (rd->getName() == "converts_to") sel.lateConversion = true;
        return true;
      }
    } late(sel);
    late.ctx = &ctx;
    late.TraverseDecl(ctx.getTranslationUnitDecl());
  }

 private:
  Selection& sel;
};
class MarkedUseAction : public clang::SyntaxOnlyAction {
 public:
  explicit MarkedUseAction(Selection& sel) : sel(sel) {}
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance&, llvm::StringRef) override {
    return std::make_unique<MarkedUse>(sel);
  }

 private:
  Selection& sel;
};
struct MarkedUseFactory : clang::tooling::FrontendActionFactory {
  Selection& sel;
  explicit MarkedUseFactory(Selection& s) : sel(s) {}
  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<MarkedUseAction>(sel);
  }
};

llvm::cl::opt<std::string> archetypes(
    "archetypes",
    llvm::cl::desc("the archetype header that ships with the linter"),
    llvm::cl::cat(category));

const clang::tooling::CompilationDatabase* compilations = nullptr;
llvm::cl::opt<bool> dumpAppendix(
    "dump-appendix",
    llvm::cl::desc("print each generated appendix to stderr (debugging)"),
    llvm::cl::cat(category));

std::string qualifiedNamespace(const clang::Decl* d) {
  std::string q;
  for (const clang::DeclContext* c = d->getDeclContext(); c; c = c->getParent())
    if (const auto* ns = llvm::dyn_cast<clang::NamespaceDecl>(c))
      q = ns->getName().str() + (q.empty() ? "" : "::" + q);
  return q;
}

const clang::ClassTemplateDecl* classTemplateOf(
    clang::QualType t, llvm::ArrayRef<clang::TemplateArgument>& args);

/// Whether another function or function template in the same scope has the
/// template's name, so that its address needs a target type.
bool overloaded(const clang::FunctionTemplateDecl* ft) {
  const clang::DeclContext* dc = ft->getDeclContext()->getRedeclContext();
  for (const clang::NamedDecl* d : dc->lookup(ft->getDeclName()))
    if (d->getCanonicalDecl() != ft->getCanonicalDecl() &&
        (llvm::isa<clang::FunctionDecl>(d) ||
         llvm::isa<clang::FunctionTemplateDecl>(d)))
      return true;
  return false;
}

/// The appendix for one template: archetypes, the traits' entries at them,
/// and a use that makes the compiler instantiate the body.  Empty, with
/// `why` set, when some parameter has no archetype.
std::string appendixFor(const clang::FunctionTemplateDecl* ft, const Facts& f,
                        std::string& why) {
  const clang::TemplateParameterList* ps = ft->getTemplateParameters();
  std::map<Param, std::string> carrier, arch;
  std::string types, entries;
  auto carrierName = [&](Param p) -> std::string {
    if (auto it = carrier.find(p); it != carrier.end()) return it->second;
    const std::string tag = "Tag" + std::to_string(carrier.size());
    const std::string name = "C" + std::to_string(carrier.size());
    const bool eq = f.comparable.count(p), def = f.defaulted.count(p);
    types += "struct " + tag + ";\nusing " + name + " = ::jlt::archetype::" +
             (def ? (eq ? "regular_carrier" : "semiregular_carrier")
                  : (eq ? "comparable_carrier" : "carrier")) +
             "<" + tag + ">;\n";
    carrier[p] = name;
    return name;
  };
  const std::string ns = "jlt_admit";
  // The equations of same_as, joined into classes: a class with a count atom
  // is the count, one with a plain parameter is its carrier, any other a
  // fresh carrier of its own.
  std::map<std::string, std::string> parent;
  std::map<std::string, Param> paramOf;
  auto key = [&](const Term& t) {
    std::string k = (t.kind == Term::Member ? "m" : "p") +
                    std::to_string(t.p.first) + ":" +
                    std::to_string(t.p.second) + ":" + t.member;
    parent.try_emplace(k, k);
    if (t.kind == Term::Parameter) paramOf[k] = t.p;
    return k;
  };
  std::function<std::string(const std::string&)> find =
      [&](const std::string& k) {
        return parent[k] == k ? k : parent[k] = find(parent[k]);
      };
  for (const auto& [x, y] : f.sames) parent[find(key(x))] = find(key(y));
  std::map<std::string, std::string> slot;
  auto slotType = [&](const std::string& k) -> std::string {
    const std::string r = find(k);
    if (auto it = slot.find(r); it != slot.end()) return it->second;
    std::string t;
    for (const auto& [m, q] : paramOf)
      if (find(m) == r && f.atoms.count(q)) t = "@INTEGER-EQUATION@";
    if (t.empty())
      for (const auto& [m, q] : paramOf)
        if (find(m) == r && f.plain.count(q)) t = carrierName(q);
    if (t.empty()) t = carrierName({~3u, static_cast<unsigned>(slot.size())});
    return slot[r] = t;
  };
  for (const clang::NamedDecl* d : *ps) {
    const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(d);
    if (!tp) {
      why = "a template parameter that is not a type";
      return {};
    }
    const Param p{tp->getDepth(), tp->getIndex()};
    std::string a;
    if (f.atoms.count(p)) {
      a = "@ATOM@";  // the integer extremes, or the count (see below)
    } else if (f.unit.count(p)) {
      a = "::jlt::archetype::nullary<" + carrierName(p) + ">";
    } else if (auto it = f.carrierOf.find(p); it != f.carrierOf.end()) {
      a = carrierName(it->second);
    } else if (auto it = f.range.find(p); it != f.range.end()) {
      const std::string elem =
          it->second ? carrierName(*it->second) : carrierName({~0u, p.second});
      // A size only where the shape asks for one: an input_range has none.
      a = std::string("::jlt::archetype::") +
          (f.sized.count(p) ? "sized_range_of<" : "input_range_of<") + elem +
          ">";
    } else if (auto it = f.members.find(p); it != f.members.end()) {
      // The library's arrow: a struct with the member types, each a fresh
      // carrier, and the const call from one to another.
      const std::string name = "S" + std::to_string(arch.size());
      std::string body;
      std::map<std::string, std::string> mc;
      for (const std::string& m : it->second) {
        const std::string k = "m" + std::to_string(p.first) + ":" +
                              std::to_string(p.second) + ":" + m;
        mc[m] = parent.count(k)
                    ? slotType(k)
                    : carrierName({~2u, static_cast<unsigned>(carrier.size())});
        body += "  using " + m + " = " + mc[m] + ";\n";
      }
      if (auto c = f.memberCall.find(p); c != f.memberCall.end()) {
        if (c->second.second.empty()) {
          why = "a required call on a member type without a result type";
          return {};
        }
        const std::string r = mc[c->second.second];
        body += "  " +
                (f.memberConverts.count(p)
                     ? "::jlt::archetype::converts_to<" + r + ">"
                     : r) +
                " operator()(const " + mc[c->second.first] + "&) const;\n";
      }
      // No default value unless a shape asks for one: an empty struct would
      // otherwise have one, more than IsArrow promises.
      if (!f.defaulted.count(p)) body += "  " + name + "() = delete;\n";
      types += "struct " + name + " {\n" + body + "};\n";
      a = name;
    } else if (auto it = f.arrow.find(p); it != f.arrow.end()) {
      if (it->second.second == Param{~0u, ~0u}) {
        why = "a required call without a result type";
        return {};
      }
      const std::string r = carrierName(it->second.second);
      a = "::jlt::archetype::arrow<" + carrierName(it->second.first) + ", " +
          (f.arrowConverts.count(p) ? "::jlt::archetype::converts_to<" + r + ">"
                                    : r) +
          ">";
    } else if (f.plain.count(p)) {
      // A parameter in a class of same_as equations takes the class's
      // carrier, so that T and U equal through F::Domain are one type.
      const std::string k =
          "p" + std::to_string(p.first) + ":" + std::to_string(p.second) + ":";
      a = parent.count(k) ? slotType(k) : carrierName(p);
    } else {
      why = "no archetype for template parameter " + tp->getName().str();
      return {};
    }
    if (a == "@ATOM@") {  // no alias: the uses substitute it
      arch[p] = a;
      continue;
    }
    const std::string name = "A" + std::to_string(arch.size());
    types += "using " + name + " = " + a + ";\n";
    arch[p] = ns + "::" + name;
  }
  for (const auto& [p, trait] : f.op) {
    if (!arch.count(p)) continue;
    entries += "namespace " + qualifiedNamespace(trait) +
               " {\ntemplate <>\nstruct " + trait->getName().str() + "<" +
               arch[p] + "> {\n  using type = ::jlt::archetype::binary<" + ns +
               "::" + carrier[p] +
               (f.opConverts.count(p) ? ", ::jlt::archetype::converts_to<" +
                                            ns + "::" + carrier[p] + ">"
                                      : std::string()) +
               ">;\n};\n}\n";
  }
  std::set<std::string> seen;
  for (const Facts::Law& law : f.laws) {
    std::string args;
    for (Param p : law.args) {
      if (f.atoms.count(p)) {  // its entry would need every integer type
        why = "a law at an integer parameter";
        return {};
      }
      if (!arch.count(p)) {
        why = "a law at a parameter without an archetype";
        return {};
      }
      args += (args.empty() ? "" : ", ") + arch[p];
    }
    const std::string key =
        law.decl->getQualifiedNameAsString() + "<" + args + ">";
    if (!seen.insert(key).second) continue;
    entries += "namespace " + qualifiedNamespace(law.decl) +
               " {\ntemplate <>\nconstexpr bool " + law.decl->getName().str() +
               "<" + args + "> = true;\n}\n";
  }
  std::string targs;
  for (const clang::NamedDecl* d : *ps) {
    const auto* tp = llvm::cast<clang::TemplateTypeParmDecl>(d);
    targs +=
        (targs.empty() ? "" : ", ") + arch[{tp->getDepth(), tp->getIndex()}];
  }
  const std::string instance =
      "&::" + ft->getQualifiedNameAsString() + "<" + targs + ">";
  std::string use = "inline constexpr auto* use = " + instance + ";\n";
  // An overloaded name has no address without a target type: restate the
  // parameter types as written, each as an alias template over the
  // template's own parameter names, in the template's namespace, so that the
  // names in them resolve as at the definition.
  std::string aliases, result, tparams;
  bool named = true;
  for (const clang::NamedDecl* d : *ps) {
    const auto* tp = llvm::cast<clang::TemplateTypeParmDecl>(d);
    named = named && !tp->getName().empty() && !tp->isParameterPack();
    tparams += (tparams.empty() ? "class " : ", class ") + tp->getName().str();
  }
  const clang::FunctionDecl* fd = ft->getTemplatedDecl();
  const clang::ASTContext& ctx = ft->getASTContext();
  const clang::SourceManager& sm = ctx.getSourceManager();
  const std::string offset =
      std::to_string(sm.getFileOffset(sm.getExpansionLoc(ft->getLocation())));
  const std::string scope = qualifiedNamespace(ft);
  // A result type that is a class template's specialization is restated
  // first, at the archetypes, so that a result asking more than the
  // constraints fails there, naming its class template (see admit).
  // A friend defined in its class names its types in the class's scope,
  // which the appendix, at namespace scope, cannot restate.
  const bool inClass = ft->getLexicalDeclContext()->isRecord();
  llvm::ArrayRef<clang::TemplateArgument> resultArgs;
  if (named && !inClass && classTemplateOf(fd->getReturnType(), resultArgs)) {
    const std::string t =
        clang::Lexer::getSourceText(clang::CharSourceRange::getTokenRange(
                                        fd->getReturnTypeSourceRange()),
                                    sm, ctx.getLangOpts())
            .str();
    if (!t.empty() && t.find("auto") == std::string::npos) {
      aliases += "template <" + tparams + ">\nusing jlt_admit_result_" +
                 offset + " = " + t + ";\n";
      result = "using result = ::" + (scope.empty() ? "" : scope + "::") +
               "jlt_admit_result_" + offset + "<" + targs + ">;\n";
    }
  }
  if (overloaded(ft)) {
    if (inClass) {
      why = "an overloaded friend defined in its class";
      return {};
    }
    if (!named) {
      why = "an overloaded name whose template parameters are unnamed";
      return {};
    }
    const std::string prefix = "jlt_admit_param_" + offset;
    std::string lambda, call;
    for (unsigned i = 0; i < fd->getNumParams(); ++i) {
      const clang::ParmVarDecl* pv = fd->getParamDecl(i);
      const clang::TypeSourceInfo* ti = pv->getTypeSourceInfo();
      const std::string t =
          ti ? clang::Lexer::getSourceText(
                   clang::CharSourceRange::getTokenRange(
                       pv->getBeginLoc(), ti->getTypeLoc().getEndLoc()),
                   sm, ctx.getLangOpts())
                   .str()
             : std::string();
      if (t.empty() || t.find("auto") != std::string::npos) {
        why =
            "an overloaded name whose parameter types the appendix cannot "
            "restate";
        return {};
      }
      const std::string a = prefix + "_" + std::to_string(i);
      aliases += "template <" + tparams + ">\nusing " + a + " = " + t + ";\n";
      const std::string pt =
          "::" + (scope.empty() ? "" : scope + "::") + a + "<" + targs + ">";
      const std::string arg = "a" + std::to_string(i);
      lambda += (i ? ", " : "") + pt + " " + arg;
      call += (i ? ", " : "") + ("static_cast<" + pt + "&&>(" + arg + ")");
    }
    if (fd->isVariadic()) {
      why = "an overloaded name with a C variadic parameter";
      return {};
    }
    // The use is a call, resolved as at a call site: an address would need
    // the result type as well, and a deduced one cannot select from an
    // overload set (the target is then a non-deduced context).
    use = "inline constexpr auto use = [](" + lambda + ") { (void)" +
          instance.substr(1) + "(" + call + "); };\n";
  }
  if (!aliases.empty())
    aliases = "namespace " + scope + " {\n" + aliases + "}\n";
  std::string text = "\n// jlt-lint: admission by type\n";
  text += "namespace " + ns + " {\n" + types + "}\n" + entries + aliases +
          "namespace " + ns + " {\n" + result + use + "}\n";
  // An equation between a member type and an integer parameter would pin
  // the member while the probe varies the parameter (Copilot on #1212).
  if (text.find("@INTEGER-EQUATION@") != std::string::npos) {
    why = "an equation between a member type and an integer parameter";
    return {};
  }
  if (f.atoms.empty()) return text;
  // Several integer parameters would be checked on the diagonal only.
  if (f.atoms.size() > 1) {
    why = "several integer parameters, checked so far one at a time only";
    return {};
  }
  auto subst = [](std::string t, const std::string& by) {
    for (std::size_t at; (at = t.find("@ATOM@")) != std::string::npos;)
      t.replace(at, 6, by);
    return t;
  };
  // An overloaded name has no address to probe with: the count alone.
  if (overloaded(ft)) return subst(text, "::std::size_t");
  // The integer parameters at the smallest and the largest of the standard's
  // integer types the constraints admit, found by asking the constraints.
  const std::string probe =
      "namespace " + ns +
      " {\ntemplate <class N>\nstruct fits {\n  static constexpr bool value "
      "= requires { " +
      subst(instance, "N") +
      "; };\n};\nusing Lo = ::jlt::archetype::smallest_integer<fits>;\n"
      "using Hi = ::jlt::archetype::largest_integer<fits>;\ntemplate <class "
      "N>\nstruct checked {\n  static constexpr bool value = [] {\n    if "
      "constexpr (fits<N>::value) (void)" +
      subst(instance, "N") +
      ";\n    return true;\n  }();\n};\ninline constexpr bool use_all = "
      "::jlt::archetype::every_integer<checked>;\n}\n";
  const std::string head =
      subst(text.substr(0, text.rfind("namespace " + ns + " {\n" + result)),
            ns + "::Hi");
  std::string lo = subst(use, ns + "::Lo"), hi = subst(use, ns + "::Hi");
  lo.replace(lo.find("use"), 3, "use_lo");
  hi.replace(hi.find("use"), 3, "use_hi");
  return head + probe + "namespace " + ns + " {\n" +
         subst(result, ns + "::Hi") + lo + hi + "}\n";
}

/// The class template a type names, if it names a specialization of one, and
/// its arguments: `const Compose<F, G>&` gives Compose and F, G.
const clang::ClassTemplateDecl* classTemplateOf(
    clang::QualType t, llvm::ArrayRef<clang::TemplateArgument>& args) {
  t = t.getNonReferenceType().getUnqualifiedType();
  const auto* ts = t->getAs<clang::TemplateSpecializationType>();
  if (!ts) return nullptr;
  args = ts->template_arguments();
  return llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(
      ts->getTemplateName().getAsTemplateDecl());
}

/// Implied constraints: a parameter whose type is a specialization of a
/// constrained class template, `component(const Compose<F, G>&)`, holds a
/// value of it, so whoever calls the template has already met the class's
/// constraints, here Cod<F> == Dom<G>; they are read as if the template had
/// stated them.  Rust's RFC 2089, implied bounds (accepted, not
/// stabilised), proposes the same reading: bounds a function's argument
/// types need are assumed in its body, not repeated in its signature, and
/// those of its return type too.  Here only the parameter types: C++ checks
/// a result type's constraints where the template is called, no argument
/// meets them, and a template whose result asks more than its constraints
/// is refused (see admit).  A class
/// template whose arguments are not all types is skipped, and the use then
/// shows whether its constraints hold at the archetypes.
void readImplied(const clang::FunctionDecl* fd, Facts& f) {
  for (const clang::ParmVarDecl* pv : fd->parameters()) {
    llvm::ArrayRef<clang::TemplateArgument> args;
    const clang::ClassTemplateDecl* ct = classTemplateOf(pv->getType(), args);
    if (!ct) continue;
    const clang::TemplateParameterList* cps = ct->getTemplateParameters();
    if (args.size() != cps->size()) continue;
    TermSubst sub;
    bool types = true;
    for (unsigned i = 0; i < cps->size() && types; ++i) {
      const auto* tp =
          llvm::dyn_cast<clang::TemplateTypeParmDecl>(cps->getParam(i));
      types = tp && !tp->isParameterPack() &&
              args[i].getKind() == clang::TemplateArgument::Type;
      if (types)
        sub[{tp->getDepth(), tp->getIndex()}] =
            classify(args[i].getAsType(), {});
    }
    if (!types) continue;
    // Read into a copy: an implied constraint only adds what can be read,
    // and one that cannot is left to the use, which shows whether it holds.
    Facts g = f;
    for (const clang::NamedDecl* p : *cps)
      if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
        if (const clang::TypeConstraint* tc = tp->getTypeConstraint()) {
          findAtoms(tc->getImmediatelyDeclaredConstraint(), sub, g);
          readConstraint(tc->getImmediatelyDeclaredConstraint(), sub, g);
        }
    findAtoms(cps->getRequiresClause(), sub, g);
    readConstraint(cps->getRequiresClause(), sub, g);
    if (g.undecided.empty()) f = std::move(g);
  }
}

Outcome admit(const clang::FunctionTemplateDecl* ft, clang::ASTContext& ctx) {
  Facts f;
  const clang::FunctionDecl* fd = ft->getTemplatedDecl();
  for (const clang::NamedDecl* p : *ft->getTemplateParameters())
    if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
      if (const clang::TypeConstraint* tc = tp->getTypeConstraint())
        findAtoms(tc->getImmediatelyDeclaredConstraint(), {}, f);
  findAtoms(ft->getTemplateParameters()->getRequiresClause(), {}, f);
  findAtoms(fd->getTrailingRequiresClause().ConstraintExpr, {}, f);
  for (const clang::NamedDecl* p : *ft->getTemplateParameters())
    if (const auto* tp = llvm::dyn_cast<clang::TemplateTypeParmDecl>(p))
      if (const clang::TypeConstraint* tc = tp->getTypeConstraint())
        readConstraint(tc->getImmediatelyDeclaredConstraint(), {}, f);
  readConstraint(ft->getTemplateParameters()->getRequiresClause(), {}, f);
  readConstraint(fd->getTrailingRequiresClause().ConstraintExpr, {}, f);
  readImplied(fd, f);
  if (!f.undecided.empty()) return {Outcome::Undecided, f.undecided};
  if (!f.opUnconfirmed.empty())
    return {Outcome::Undecided,
            "a required operation whose result is not constrained to its "
            "carrier"};
  // Hole 3: at the archetypes an if constexpr takes one branch, and the other
  // is never type-checked.
  struct Constexprs : clang::RecursiveASTVisitor<Constexprs> {
    bool found = false;
    bool VisitIfStmt(clang::IfStmt* s) {
      found |= s->isConstexpr();
      return true;
    }
  } ifs;
  ifs.TraverseStmt(fd->getBody());
  if (ifs.found)
    return {Outcome::Undecided,
            "an if constexpr: the archetypes take one branch and leave the "
            "other unchecked"};
  std::string why;
  const std::string appendix = appendixFor(ft, f, why);
  if (appendix.empty()) return {Outcome::Undecided, why};
  if (!compilations) return {Outcome::Undecided, "no compilation database"};
  const clang::SourceManager& sm = ctx.getSourceManager();
  const clang::OptionalFileEntryRef main =
      sm.getFileEntryRefForID(sm.getMainFileID());
  if (!main) return {Outcome::Undecided, "no main file"};
  const std::string path = main->getName().str();
  if (dumpAppendix)
    llvm::errs() << "---- " << ft->getNameAsString() << appendix;
  // Hole 4: the use names an overload set, so the template checked is marked,
  // and a use that resolves to a sibling is seen by the marker's absence.
  std::string original = sm.getBufferData(sm.getMainFileID()).str();
  const std::string marker = "[[deprecated(\"jlt-admit-checked\")]] ";
  const unsigned at =
      sm.getFileOffset(sm.getExpansionLoc(fd->getInnerLocStart()));
  original.insert(at, marker);
  // The archetypes' header: in a module unit, in its global module fragment
  // (after `module;`), since nothing may be included in the module's purview;
  // elsewhere, before the appendix.  Offsets after it move by its length.
  std::string include;
  if (!archetypes.empty()) include = "#include \"" + archetypes + "\"\n";
  unsigned shift = 0;
  std::string tail = appendix;
  const size_t gmf = original.find("\nmodule;\n");
  const bool atStart = original.rfind("module;\n", 0) == 0;
  if (!include.empty() && (gmf != std::string::npos || atStart)) {
    const size_t after = atStart ? std::string("module;\n").size()
                                 : gmf + std::string("\nmodule;\n").size();
    original.insert(after, include);
    shift = static_cast<unsigned>(include.size());
  } else {
    tail = include + appendix;
  }
  // The tool keeps a reference to the text, so the text lives here.
  const std::string text = original + tail;
  clang::tooling::ClangTool again(*compilations, {path});
  again.mapVirtualFile(path, text);
  again.appendArgumentsAdjuster(clang::tooling::getInsertArgumentAdjuster(
      // Warnings stay warnings: an error is what the type check refuses, not
      // what the project's -Werror promotes (a bitwise not of a bool is
      // legal C++).
      {"-Wdeprecated-declarations", "-Wno-error"},
      clang::tooling::ArgumentInsertPosition::END));
  Errors errors;
  errors.appendixFrom = static_cast<unsigned>(original.size());
  // The checked body, in the marked text: after the marker.
  errors.bodyFrom = static_cast<unsigned>(
      sm.getFileOffset(sm.getExpansionLoc(fd->getBody()->getBeginLoc())) +
      marker.size() + shift);
  errors.bodyTo = static_cast<unsigned>(
      sm.getFileOffset(sm.getExpansionLoc(fd->getBody()->getEndLoc())) +
      marker.size() + shift);
  again.setDiagnosticConsumer(&errors);
  Selection sel;
  MarkedUseFactory factory(sel);
  again.run(&factory);
  // Which definition the use reached comes first.  A use that resolved to an
  // unmarked function reached a sibling; a use that resolved to nothing failed
  // with the body it instantiated, which the first error then locates.
  if (sel.resolved && !sel.marked && !errors.marked)
    return {Outcome::Undecided,
            "the use resolved to another overload at the archetypes"};
  if (errors.n == 0 && sel.negatedCallee)
    return {Outcome::Undecided,
            "a callee asks a negated constraint, which every archetype meets "
            "by default"};
  if (errors.n == 0 && sel.lateConversion)
    return {Outcome::NotAdmitted,
            "a result promised only convertible_to<T> used other than to "
            "initialise a declared T or to return one"};
  if (errors.n == 0) return {Outcome::Admitted, {}};
  // The result type asks more than the constraints: the archetypes satisfy
  // every stated constraint, and the result's class template refuses them,
  // so a caller who meets the signature cannot call the template.
  llvm::ArrayRef<clang::TemplateArgument> resultArgs;
  bool paramShares = false;  // a parameter of the result's class template
  if (const clang::ClassTemplateDecl* rt =
          classTemplateOf(fd->getReturnType(), resultArgs))
    for (const clang::ParmVarDecl* pv : fd->parameters()) {
      llvm::ArrayRef<clang::TemplateArgument> pa;
      if (const auto* pt = classTemplateOf(pv->getType(), pa))
        paramShares |= pt->getCanonicalDecl() == rt->getCanonicalDecl();
    }
  if (const clang::ClassTemplateDecl* rt =
          classTemplateOf(fd->getReturnType(), resultArgs);
      rt && !paramShares && errors.inAppendix &&
      errors.first.rfind("constraints not satisfied for class template '" +
                             rt->getName().str() + "'",
                         0) == 0)
    return {Outcome::NotAdmitted,
            "the result type " + rt->getName().str() +
                " asks more than the constraints: " + errors.first};
  if (errors.inAppendix)
    return {Outcome::Undecided,
            "the appendix did not compile: " + errors.first};
  if (!errors.inBody)
    return {Outcome::Undecided,
            "an error outside the checked body: " + errors.first};
  return {Outcome::NotAdmitted, errors.first};
}

/// The check, in two views of one computation: `admit` reports the
/// definitions not admitted and the undecided ones (the findings), `admitted`
/// the admitted ones.  The outcomes are computed once per file.
class Admission : public clang::ASTConsumer {
 public:
  explicit Admission(Outcome::Kind view) : view(view) {}
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    const clang::SourceManager& sm = ctx.getSourceManager();
    // Shared by the two views of one file, never across files.
    static std::map<std::string, std::vector<std::pair<std::string, Outcome>>>
        caches;
    const clang::OptionalFileEntryRef mainFile =
        sm.getFileEntryRefForID(sm.getMainFileID());
    const std::string key = mainFile ? mainFile->getName().str() : "";
    const bool computed = caches.count(key) > 0;
    auto& cache = caches[key];
    if (!computed) {
      struct Finder : clang::RecursiveASTVisitor<Finder> {
        std::vector<const clang::FunctionTemplateDecl*> found;
        const clang::SourceManager* sm;
        bool shouldVisitTemplateInstantiations() const { return false; }
        bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl* ft) {
          const clang::FunctionDecl* f = ft->getTemplatedDecl();
          if (!f || !f->doesThisDeclarationHaveABody() || f->isDeleted() ||
              llvm::isa<clang::CXXMethodDecl>(f) ||
              !sm->isInMainFile(ft->getLocation()) || !inScope(ft) ||
              !constrained(ft))
            return true;
          found.push_back(ft);
          return true;
        }
      } finder;
      finder.sm = &sm;
      finder.TraverseDecl(ctx.getTranslationUnitDecl());
      for (const clang::FunctionTemplateDecl* ft : finder.found) {
        const clang::SourceLocation loc = sm.getExpansionLoc(ft->getLocation());
        std::string where;
        llvm::raw_string_ostream w(where);
        w << sm.getFilename(loc) << ':' << sm.getExpansionLineNumber(loc) << ':'
          << sm.getExpansionColumnNumber(loc) << ": " << ft->getNameAsString();
        cache.push_back({w.str(), admit(ft, ctx)});
      }
    }
    for (const auto& [where, o] : cache) {
      if (o.kind != view) continue;
      if (o.kind == Outcome::Admitted) out() << where << " admitted by type\n";
      if (o.kind == Outcome::NotAdmitted)
        out() << where << " not admitted: " << o.why << '\n';
      if (o.kind == Outcome::Undecided)
        out() << where << " undecided: " << o.why << '\n';
    }
  }

 private:
  Outcome::Kind view;  // one rung of the ladder per check
};

/// The syntax kinds under one declaration, as the inventory names them.
class Kinds : public clang::RecursiveASTVisitor<Kinds> {
 public:
  std::set<std::string> kinds;
  bool shouldVisitTemplateInstantiations() const { return false; }
  bool shouldVisitImplicitCode() const { return false; }
  bool TraverseDecl(clang::Decl* d) {
    // The variable a variable template declares is the template, not a
    // local variable: counted as the VarTemplate.
    const auto* v = llvm::dyn_cast_or_null<clang::VarDecl>(d);
    if (d && !d->isImplicit() && !(v && v->getDescribedVarTemplate()))
      kinds.insert(std::string("Decl:") + d->getDeclKindName());
    return clang::RecursiveASTVisitor<Kinds>::TraverseDecl(d);
  }
  // The one derived form no syntax kind shows: a range.  A parameter
  // constrained by a standard range concept, or a standard view other than
  // the counted iota the kernel iterates over.
  bool VisitConceptSpecializationExpr(clang::ConceptSpecializationExpr* e) {
    const clang::ConceptDecl* c = e->getNamedConcept();
    if (c && inStd(c) && c->getName().ends_with("range"))
      kinds.insert("Derived:range");
    return true;
  }
  bool VisitTemplateTypeParmDecl(clang::TemplateTypeParmDecl* d) {
    if (const clang::TypeConstraint* tc = d->getTypeConstraint())
      if (const clang::TemplateDecl* c = tc->getNamedConcept())
        if (inStd(c) && c->getName().ends_with("range"))
          kinds.insert("Derived:range");
    return true;
  }
  bool VisitDeclRefExpr(clang::DeclRefExpr* e) {
    // The view objects themselves (views::transform), not their call
    // operators, whose names are operator().
    const clang::ValueDecl* d = e->getDecl();
    if (d && llvm::isa<clang::VarDecl>(d) && inStd(d) &&
        d->getQualifiedNameAsString().find("views::") != std::string::npos &&
        d->getName() != "iota")
      kinds.insert("Derived:view");
    return true;
  }
  bool VisitStmt(clang::Stmt* st) {
    if (const auto* t = llvm::dyn_cast<clang::CXXThisExpr>(st))
      if (t->isImplicit()) return true;  // a member named without this->
    // A conversion between integer types written as a call, size_t(k): the
    // count's own modular arithmetic, decided kernel (2026-11-22); any other
    // functional cast keeps its kind.
    if (const auto* c = llvm::dyn_cast<clang::CXXFunctionalCastExpr>(st))
      if (c->getType()->isIntegerType() &&
          c->getSubExpr()->IgnoreParenImpCasts()->getType()->isIntegerType()) {
        kinds.insert("Cast:integer");
        return true;
      }
    // An operator call on a class type by its spelling, as the built-in ones:
    // s = s + x on a string is an assignment, ++it an increment.
    if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(st)) {
      const std::string sp = clang::getOperatorSpelling(o->getOperator());
      const bool unary = o->getNumArgs() == 1 && sp != "()" && sp != "[]";
      kinds.insert("Op:" + std::string(unary ? "u" : "") + sp);
      return true;
    }
    // An operator by its spelling: + is not =, ! is not ++.
    if (const auto* o = llvm::dyn_cast<clang::BinaryOperator>(st)) {
      kinds.insert("Op:" +
                   clang::BinaryOperator::getOpcodeStr(o->getOpcode()).str());
      return true;
    }
    if (const auto* o = llvm::dyn_cast<clang::UnaryOperator>(st)) {
      kinds.insert("Op:u" +
                   clang::UnaryOperator::getOpcodeStr(o->getOpcode()).str() +
                   (o->isPostfix() ? "post" : ""));
      return true;
    }
    if (!llvm::isa<clang::ImplicitCastExpr, clang::MaterializeTemporaryExpr,
                   clang::ExprWithCleanups, clang::CXXBindTemporaryExpr,
                   clang::ConstantExpr, clang::CXXDefaultArgExpr,
                   clang::CXXDefaultInitExpr, clang::OpaqueValueExpr,
                   clang::SubstNonTypeTemplateParmExpr>(st))
      kinds.insert(std::string("Stmt:") + st->getStmtClassName());
    return true;
  }
  bool VisitTypeLoc(clang::TypeLoc tl) {
    if (!tl.isNull())
      kinds.insert(std::string("Type:") + tl.getType()->getTypeClassName());
    return true;
  }
};

/// Not a rule: every declaration directly in the scope namespace, with its
/// lines and the syntax kinds it uses, one line each, so that a script can
/// give each definition its level (kernel or derived forms, by admission.txt)
/// and join it with the findings that fall in its lines.
class Definitions : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    const clang::SourceManager& sm = ctx.getSourceManager();
    clang::PrintingPolicy policy = ctx.getPrintingPolicy();
    policy.SuppressScope = true;
    std::function<void(const clang::DeclContext*)> walk =
        [&](const clang::DeclContext* dc) {
          for (clang::Decl* d : dc->decls()) {
            const auto* ns = llvm::dyn_cast<clang::NamespaceDecl>(d);
            if (ns && ns->getName() == scope &&
                sm.isInMainFile(sm.getExpansionLoc(ns->getLocation()))) {
              for (clang::Decl* m : ns->decls()) report(m, sm, policy);
              continue;
            }
            if (const auto* c = llvm::dyn_cast<clang::DeclContext>(d))
              if (llvm::isa<clang::NamespaceDecl, clang::LinkageSpecDecl,
                            clang::ExportDecl>(d))
                walk(c);
          }
        };
    walk(ctx.getTranslationUnitDecl());
  }

 private:
  void report(clang::Decl* d, const clang::SourceManager& sm,
              const clang::PrintingPolicy& policy) {
    if (d->isImplicit() ||
        llvm::isa<clang::NamespaceDecl, clang::UsingDecl,
                  clang::UsingDirectiveDecl, clang::StaticAssertDecl>(d))
      return;
    const auto* nd = llvm::dyn_cast<clang::NamedDecl>(d);
    if (!nd) return;
    std::string name;
    llvm::raw_string_ostream n(name);
    nd->getNameForDiagnostic(n, policy, false);
    const clang::SourceLocation b = sm.getExpansionLoc(d->getBeginLoc()),
                                e = sm.getExpansionLoc(d->getEndLoc());
    if (!sm.isInMainFile(b)) return;
    Kinds k;
    k.TraverseDecl(d);
    out() << sm.getFilename(b) << ':' << sm.getExpansionLineNumber(b) << ':'
          << sm.getExpansionColumnNumber(b) << ": " << n.str() << " range "
          << sm.getExpansionLineNumber(b) << ':'
          << sm.getExpansionColumnNumber(b) << '-'
          << sm.getExpansionLineNumber(e) << ':'
          << sm.getExpansionColumnNumber(e);
    for (const std::string& kind : k.kinds) out() << ' ' << kind;
    out() << '\n';
  }
};

/// One check by name, or null for an unknown name.
std::unique_ptr<clang::ASTConsumer> checkNamed(llvm::StringRef name) {
  if (name == "mutual-recursion") return std::make_unique<MutualRecursion>();
  if (name == "definition") return std::make_unique<Definition>();
  if (name == "inline-requires") return std::make_unique<InlineRequires>();
  if (name == "type-observation") return std::make_unique<TypeObservation>();
  if (name == "inheritance") return std::make_unique<Inheritance>();
  if (name == "bounded-fold") return std::make_unique<BoundedFold>();
  if (name == "context-order") return std::make_unique<ContextOrder>();
  if (name == "converted-result") return std::make_unique<ConvertedResult>();
  if (name == "inventory") return std::make_unique<Inventory>();
  if (name == "undecided-resolution")
    return std::make_unique<UndecidedResolution>();
  if (name == "admit") return std::make_unique<Admission>(Outcome::NotAdmitted);
  if (name == "undecided")
    return std::make_unique<Admission>(Outcome::Undecided);
  if (name == "admitted") return std::make_unique<Admission>(Outcome::Admitted);
  if (name == "definitions") return std::make_unique<Definitions>();
  llvm::errs() << "jlt-lint: unknown check " << name << '\n';
  return nullptr;
}

/// A check whose findings go to its own section, after a "== <check>" line:
/// several checks then read one parse, and the driver splits the output.
class Section : public clang::ASTConsumer {
 public:
  Section(std::string n, std::unique_ptr<clang::ASTConsumer> c)
      : name(std::move(n)), inner(std::move(c)) {}
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    std::string buffer;
    llvm::raw_string_ostream os(buffer);
    current = &os;
    inner->HandleTranslationUnit(ctx);
    current = &llvm::outs();
    os.flush();
    llvm::outs() << "== " << name << '\n' << buffer;
  }

 private:
  std::string name;
  std::unique_ptr<clang::ASTConsumer> inner;
};

// ---------------------------------------------------------------------------
// query rules: clang-query's matchers, on the checks' parse
// ---------------------------------------------------------------------------

using clang::ast_matchers::MatchFinder;
using clang::ast_matchers::internal::DynTypedMatcher;

/// A rule file's matchers and the traversal they match under: what
/// clang-query would run for it, one `match` command per matcher.
struct QueryRule {
  std::string name;
  clang::TraversalKind traversal;
  std::vector<DynTypedMatcher> matchers;
};

/// The scope probe first (its own rule), then the rule files in the order
/// named.  Parsed before the file is, so a rule that does not parse fails
/// the run without a parse of the file.
std::vector<QueryRule> queryRules;

/// The traversal of a rule that names none, the driver's default for
/// clang-query: the code as written.
constexpr clang::TraversalKind defaultTraversal =
    clang::TK_IgnoreUnlessSpelledInSource;

/// A matcher expression as clang-query's `match` and `let` parse it: the
/// dynamic parser, the default registry, the session's named values.  The
/// expression must fill its line.
std::optional<DynTypedMatcher> parseMatcher(
    llvm::StringRef code,
    const clang::ast_matchers::dynamic::Parser::NamedValueMap& named,
    llvm::StringRef where) {
  clang::ast_matchers::dynamic::Diagnostics diag;
  llvm::StringRef rest = code.ltrim();
  std::optional<DynTypedMatcher> m =
      clang::ast_matchers::dynamic::Parser::parseMatcherExpression(
          rest, nullptr, &named, &diag);
  if (!m) {
    llvm::errs() << "jlt-lint: " << where << ": " << diag.toStringFull()
                 << '\n';
    return std::nullopt;
  }
  if (!rest.trim().empty()) {
    llvm::errs() << "jlt-lint: " << where
                 << ": text after the matcher: " << rest.trim() << '\n';
    return std::nullopt;
  }
  // Whether the finder takes it as a top-level matcher, as clang-query asks.
  struct Ignore : MatchFinder::MatchCallback {
    void run(const MatchFinder::MatchResult&) override {}
  } ignore;
  MatchFinder probe;
  if (!probe.addDynamicMatcher(*m, &ignore)) {
    llvm::errs() << "jlt-lint: " << where
                 << ": not a valid top-level matcher\n";
    return std::nullopt;
  }
  return m;
}

/// Read the scope probe and the rule files into queryRules: in a file, each
/// line `match <matcher>` is a matcher and the first `set traversal <kind>`
/// the traversal; every other line is commentary, as it was when the driver
/// gave clang-query those lines alone.
bool readQueryRules() {
  const std::string ns = scope;
  clang::ast_matchers::dynamic::Parser::NamedValueMap named;
  {
    clang::ast_matchers::dynamic::Diagnostics diag;
    clang::ast_matchers::dynamic::VariantValue value;
    const std::string code = "namespaceDecl(hasName(\"" + ns + "\"))";
    llvm::StringRef rest = code;
    if (!clang::ast_matchers::dynamic::Parser::parseExpression(
            rest, nullptr, &named, &value, &diag)) {
      llvm::errs() << "jlt-lint: the scope " << ns << ": "
                   << diag.toStringFull() << '\n';
      return false;
    }
    named["scope"] = value;
  }
  auto probe = parseMatcher(
      "namespaceDecl(hasName(\"" + ns + "\"), isExpansionInMainFile())", named,
      "the scope probe");
  if (!probe) return false;
  queryRules.push_back({"", defaultTraversal, {*probe}});

  for (const std::string& path : queryFiles) {
    auto buffer = llvm::MemoryBuffer::getFile(path);
    if (!buffer) {
      llvm::errs() << "jlt-lint: cannot open " << path << ": "
                   << buffer.getError().message() << '\n';
      return false;
    }
    QueryRule rule{llvm::sys::path::stem(path).str(), defaultTraversal, {}};
    bool traversalNamed = false;
    llvm::SmallVector<llvm::StringRef, 16> lines;
    (*buffer)->getBuffer().split(lines, '\n');
    for (unsigned n = 0; n < lines.size(); ++n) {
      llvm::StringRef line = lines[n].rtrim("\r");
      const std::string where = path + ":" + std::to_string(n + 1);
      if (line.consume_front("set traversal ")) {
        if (traversalNamed) continue;  // the first one counts
        traversalNamed = true;
        llvm::StringRef kind = line.trim();
        if (kind == "AsIs")
          rule.traversal = clang::TK_AsIs;
        else if (kind == "IgnoreUnlessSpelledInSource")
          rule.traversal = clang::TK_IgnoreUnlessSpelledInSource;
        else {
          llvm::errs() << "jlt-lint: " << where
                       << ": expected traversal kind, got '" << kind << "'\n";
          return false;
        }
      } else if (line.consume_front("match ")) {
        auto m = parseMatcher(line, named, where);
        if (!m) return false;
        rule.matchers.push_back(*m);
      }
    }
    if (rule.matchers.empty()) {
      llvm::errs() << "jlt-lint: " << path
                   << ": want at least one match line\n";
      return false;
    }
    queryRules.push_back(std::move(rule));
  }
  return true;
}

/// Where clang-query's diag output puts a match: the note on its "root"
/// binding, rendered by TextDiagnostic as clang-query renders it, read by
/// the pattern the driver read clang-query's output with; "-" where the
/// binding has no location (clang-query prints no note, but counts it).
std::string rootLocation(const clang::ast_matchers::BoundNodes& nodes,
                         clang::ASTContext& ctx) {
  const auto& map = nodes.getMap();
  const auto root = map.find("root");
  if (root == map.end()) return "-";
  const clang::SourceRange range = root->second.getSourceRange();
  if (range.isInvalid()) return "-";
  std::string text;
  llvm::raw_string_ostream os(text);
  clang::TextDiagnostic diag(os, ctx.getLangOpts(),
                             ctx.getDiagnostics().getDiagnosticOptions());
  diag.emitDiagnostic(
      clang::FullSourceLoc(range.getBegin(), ctx.getSourceManager()),
      clang::DiagnosticsEngine::Note, "\"root\" binds here",
      clang::CharSourceRange::getTokenRange(range), {});
  os.flush();
  static const llvm::Regex binds(
      "^([^[:space:]]+:[0-9]+:[0-9]+): note: \"root\" binds here",
      llvm::Regex::Newline);
  llvm::SmallVector<llvm::StringRef, 2> m;
  if (!binds.match(text, &m)) return "-";
  return m[1].str();
}

/// One matcher's findings, in the order the finder reports them, under the
/// rule's traversal: the finder wraps the matcher in it, as clang-tidy does
/// for a check, which is what clang-query's session traversal does to a
/// matcher run alone.
class Collect : public MatchFinder::MatchCallback {
 public:
  explicit Collect(clang::TraversalKind tk) : traversal(tk) {}
  void run(const MatchFinder::MatchResult& result) override {
    found.push_back(rootLocation(result.Nodes, *result.Context));
  }
  std::optional<clang::TraversalKind> getCheckTraversalKind() const override {
    return traversal;
  }
  std::vector<std::string> found;

 private:
  clang::TraversalKind traversal;
};

/// Every rule's matchers on one finder, one traversal of the AST; each
/// rule's findings in a section, its matchers' in the order of its file,
/// as clang-query ran them one after another.
class Queries : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    MatchFinder finder;
    std::vector<std::vector<std::unique_ptr<Collect>>> collected;
    for (const QueryRule& rule : queryRules) {
      collected.emplace_back();
      for (const DynTypedMatcher& m : rule.matchers) {
        collected.back().push_back(std::make_unique<Collect>(rule.traversal));
        const std::optional<DynTypedMatcher> bound = m.tryBind("root");
        finder.addDynamicMatcher(bound ? *bound : m,
                                 collected.back().back().get());
      }
    }
    finder.matchAST(ctx);
    for (unsigned i = 0; i < queryRules.size(); ++i) {
      if (queryRules[i].name.empty())
        llvm::outs() << "== scope\n";
      else
        llvm::outs() << "== query " << queryRules[i].name << '\n';
      for (const auto& c : collected[i])
        for (const std::string& loc : c->found) llvm::outs() << loc << '\n';
    }
  }
};

class Action : public clang::ASTFrontendAction {
 public:
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance&, llvm::StringRef) override {
    llvm::SmallVector<llvm::StringRef, 8> names;
    llvm::StringRef(check).split(names, ',', -1, false);
    std::vector<std::unique_ptr<clang::ASTConsumer>> sections;
    sections.push_back(std::make_unique<Queries>());
    for (llvm::StringRef name : names) {
      auto c = checkNamed(name);
      if (!c) return nullptr;
      sections.push_back(std::make_unique<Section>(name.str(), std::move(c)));
    }
    return std::make_unique<clang::MultiplexConsumer>(std::move(sections));
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
  if (!readQueryRules()) return 2;
  compilations = &parser->getCompilations();
  clang::tooling::ClangTool tool(parser->getCompilations(),
                                 parser->getSourcePathList());
  return tool.run(clang::tooling::newFrontendActionFactory<Action>().get());
}
