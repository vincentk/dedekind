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
#include <climits>
#include <cctype>
#include <chrono>
#include <algorithm>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "clang/AST/ASTConsumer.h"
#include "clang/Basic/Builtins.h"
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
#include "llvm/Support/Timer.h"
#include "llvm/Support/Format.h"
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
/// of two or more judged functions is a cycle of mutual recursion.  The graph
/// is the part reachable from the file's own functions: a call into a function
/// defined elsewhere (an instantiation of an imported template, which may call
/// back into the file) is followed into its body, as far as the calls go.
/// Every cycle through a function of the file is reachable from it, so the
/// graph holds them all, whatever the traversal scope, and nothing else of
/// the unit is walked.
class MutualRecursion : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    const clang::SourceManager& sm = ctx.getSourceManager();
    using F = const clang::FunctionDecl*;
    // The file's own functions, with the instantiations of its templates.
    struct Seeds : clang::RecursiveASTVisitor<Seeds> {
      std::vector<F> found;
      bool shouldVisitTemplateInstantiations() const { return true; }
      bool shouldVisitImplicitCode() const { return true; }
      bool VisitFunctionDecl(clang::FunctionDecl* f) {
        if (f->doesThisDeclarationHaveABody() && !f->isDependentContext())
          found.push_back(f);
        return true;
      }
    } seeds;
    for (clang::Decl* d : ctx.getTraversalScope()) seeds.TraverseDecl(d);
    // The direct callees of one body: calls, member and operator calls,
    // constructions.
    struct Callees : clang::RecursiveASTVisitor<Callees> {
      std::vector<F> found;
      bool shouldVisitImplicitCode() const { return true; }
      // An operand that is not evaluated calls nothing (Copilot on #1214).
      bool TraverseUnaryExprOrTypeTraitExpr(clang::UnaryExprOrTypeTraitExpr*,
                                            DataRecursionQueue* = nullptr) {
        return true;
      }
      bool TraverseCXXNoexceptExpr(clang::CXXNoexceptExpr*,
                                   DataRecursionQueue* = nullptr) {
        return true;
      }
      bool TraverseRequiresExpr(clang::RequiresExpr*,
                                DataRecursionQueue* = nullptr) {
        return true;
      }
      bool TraverseCXXTypeidExpr(clang::CXXTypeidExpr* e,
                                 DataRecursionQueue* q = nullptr) {
        return !e->isPotentiallyEvaluated() ||
               clang::RecursiveASTVisitor<Callees>::TraverseCXXTypeidExpr(e, q);
      }
      bool TraverseDecltypeTypeLoc(clang::DecltypeTypeLoc, bool) { return true; }
      bool VisitCallExpr(clang::CallExpr* c) {
        if (F f = c->getDirectCallee()) found.push_back(f);
        return true;
      }
      bool VisitCXXConstructExpr(clang::CXXConstructExpr* c) {
        if (F f = c->getConstructor()) found.push_back(f);
        return true;
      }
    };
    // The reachable graph, by canonical declaration, built breadth first.
    std::map<F, std::vector<F>> edges;
    std::vector<F> work;
    for (F f : seeds.found) work.push_back(f->getCanonicalDecl());
    while (!work.empty()) {
      F f = work.back();
      work.pop_back();
      if (edges.count(f)) continue;
      auto& out = edges[f];
      F def = definitionOf(f);
      if (!def || !def->hasBody() || def->isDependentContext()) continue;
      Callees callees;
      callees.TraverseStmt(def->getBody());
      // A constructor's base, member and delegating initialisers call too
      // (Copilot on #1214).
      if (const auto* ctor = llvm::dyn_cast<clang::CXXConstructorDecl>(def))
        for (const clang::CXXCtorInitializer* init : ctor->inits())
          callees.TraverseStmt(init->getInit());
      for (F g : callees.found) {
        out.push_back(g->getCanonicalDecl());
        if (!edges.count(g->getCanonicalDecl()))
          work.push_back(g->getCanonicalDecl());
      }
    }

    std::map<F, int> index, low;
    std::set<F> onStack;
    std::vector<F> stack;
    int next = 0;
    std::set<F> findings;
    std::function<void(F)> visit = [&](F v) {
      index[v] = low[v] = next++;
      stack.push_back(v);
      onStack.insert(v);
      for (F w : edges[v]) {
        if (!index.count(w)) {
          visit(w);
          low[v] = std::min(low[v], low[w]);
        } else if (onStack.count(w)) {
          low[v] = std::min(low[v], index[w]);
        }
      }
      if (low[v] != index[v]) return;
      std::vector<F> component;
      for (;;) {
        F w = stack.back();
        stack.pop_back();
        onStack.erase(w);
        component.push_back(w);
        if (w == v) break;
      }
      if (component.size() < 2) return;  // a self-call is another rule's
      for (F w : component)
        if (F def = definitionOf(w))
          if (judged(def, sm)) findings.insert(def);
    };
    for (const auto& [f, _] : edges)
      if (!index.count(f)) visit(f);

    // In source order, not in the order of the set's pointers, which depends
    // on where the declarations happen to be allocated.
    std::vector<std::tuple<std::string, unsigned, unsigned>> lines;
    for (F f : findings) {
      const clang::SourceLocation loc = sm.getExpansionLoc(f->getLocation());
      lines.emplace_back(sm.getFilename(loc).str(),
                         sm.getExpansionLineNumber(loc),
                         sm.getExpansionColumnNumber(loc));
    }
    std::sort(lines.begin(), lines.end());
    for (const auto& [file, line, column] : lines)
      out() << file << ':' << line << ':' << column << '\n';
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

/// Whether a class has a constructor that list-initialisation prefers
/// ([over.match.list]): one whose first parameter is std::initializer_list,
/// by value or by reference, the rest defaulted.  T{x} would choose it where
/// T(x) does not.
bool hasInitListConstructor(const clang::CXXRecordDecl* r) {
  for (const clang::CXXConstructorDecl* c : r->ctors()) {
    if (c->getNumParams() == 0) continue;
    clang::QualType p = c->getParamDecl(0)->getType().getNonReferenceType().getUnqualifiedType();
    const auto* rd = p->getAsCXXRecordDecl();
    if (!rd) continue;
    if (const auto* sp = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(rd)) rd = sp->getSpecializedTemplate()->getTemplatedDecl();
    if (rd->getName() != "initializer_list" || !inStd(rd)) continue;
    bool rest = true;
    for (unsigned i = 1; i < c->getNumParams(); ++i) rest = rest && c->getParamDecl(i)->hasDefaultArg();
    if (rest) return true;
  }
  return false;
}

/// The kind of a statement, as admission.txt classes it.  A conversion to an
/// arithmetic type written as a call, size_t(e) or int(i), is the kernel's
/// written conversion (Figure 2, Table 7), a kind of its own; a value of a
/// class type from values for its fields, written with braces, Sums{a, b},
/// is Figure 2's own form, as it already is in a template (an unresolved
/// construction); T(x) to a class type, with parentheses, stays
/// CXXFunctionalCastExpr.
std::string stmtKind(const clang::Stmt* st) {
  // static_cast<T>(e) between arithmetic types: the written conversion T(e)
  // in the other spelling, a derived form with its conditions (a floating
  // value to an integer is open as Op:float-to-integer, which the kinds add);
  // any other static_cast keeps its kind.
  if (const auto* c = llvm::dyn_cast<clang::CXXStaticCastExpr>(st)) {
    // The types as written: a cast to a reference, static_cast<const int&>,
    // has an arithmetic expression type, and a conversion through a
    // conversion operator hides its source under the implicit call.
    const clang::QualType to = c->getTypeAsWritten();
    const clang::QualType from = c->getSubExprAsWritten()->getType();
    if (!to->isDependentType() && !to->isReferenceType() && !from.isNull() && !from->isDependentType() &&
        to->isArithmeticType() && from.getNonReferenceType()->isArithmeticType() && !to->isEnumeralType() &&
        !from->isEnumeralType() && c->getConversionFunction() == nullptr)
      return "Cast:static";
    // Between a scoped enumeration and an integer type, either way: the
    // record's field written out or read (#1233).
    const auto scoped = [](clang::QualType t) {
      const auto* e = t->getAs<clang::EnumType>();
      return e && e->getDecl()->isScoped();
    };
    if (!to->isDependentType() && !to->isReferenceType() && !from.isNull() && !from->isDependentType() &&
        c->getConversionFunction() == nullptr &&
        ((scoped(to) && from.getNonReferenceType()->isIntegralOrUnscopedEnumerationType()) ||
         (scoped(from.getNonReferenceType()) && to->isIntegerType())))
      return "Cast:enum";
  }
  if (const auto* c = llvm::dyn_cast<clang::CXXFunctionalCastExpr>(st)) {
    if (c->getType()->isArithmeticType()) return "Stmt:ArithmeticConversion";
    if (c->isListInitialization() && c->getType()->isRecordType()) return "Stmt:BracedValue";
    // T(x) to a class type, with parentheses: the construction T{x}, whose
    // arguments' conversions the narrowing check reads as it reads any,
    // where braces choose as parentheses do: no constructor taking an
    // initializer_list, which T{x} would prefer.
    if (c->getType()->isRecordType() && !c->getType()->isDependentType())
      if (const auto* r = c->getType()->getAsCXXRecordDecl(); r && r->hasDefinition() && !hasInitListConstructor(r))
        return "Stmt:ParenValue";
  }
  // T(a, b) to an aggregate, C++20's parenthesised aggregate initialisation:
  // the same construction with braces.
  if (const auto* c = llvm::dyn_cast<clang::CXXParenListInitExpr>(st); c && !c->getType()->isDependentType())
    return "Stmt:ParenValue";
  // sizeof and alignof of a type that is not dependent: a constant of
  // size_t, fixed by the type (at a type variable they observe the type,
  // which rule2-no-type-observation refuses).
  if (const auto* u = llvm::dyn_cast<clang::UnaryExprOrTypeTraitExpr>(st))
    if ((u->getKind() == clang::UETT_SizeOf || u->getKind() == clang::UETT_AlignOf) && !u->isValueDependent() &&
        !u->isTypeDependent() && !u->isInstantiationDependent())
      return "Stmt:SizeConstant";
  // if consteval chooses by how the call is evaluated, and a generic lambda
  // is a template with no constraint: kinds of their own, apart from the if
  // statement and the lambda of Table 7.
  if (const auto* i = llvm::dyn_cast<clang::IfStmt>(st)) {
    if (i->isConsteval()) return "Stmt:IfConsteval";
    // if constexpr chooses by a type: Table 7 has two rows for it, and no
    // check yet shows either row's condition.
    if (i->isConstexpr()) return "Stmt:IfConstexpr";
  }
  if (const auto* l = llvm::dyn_cast<clang::LambdaExpr>(st))
    if (l->isGenericLambda()) return "Stmt:GenericLambda";
  return std::string("Stmt:") + st->getStmtClassName();
}

/// Whether destroying a value of a class does nothing: its destructor
/// trivial, or not virtual and defaulted or with an empty body, and the same
/// of every base and every member's class, since a destructor destroys them
/// too.
bool destroysNothing(const clang::CXXRecordDecl* r, const clang::CXXDestructorDecl* x) {
  if (!r || !r->hasDefinition()) return false;
  r = r->getDefinition();
  if (!x) x = r->getDestructor();
  if (!x || x->isTrivial()) return true;
  if (x->isVirtual()) return false;
  const auto* body = llvm::dyn_cast_or_null<clang::CompoundStmt>(x->getBody());
  if (!x->isDefaulted() && !(body && body->body_empty())) return false;
  auto sub = [](clang::QualType t) {
    t = t.getCanonicalType();
    while (const auto* a = llvm::dyn_cast<clang::ArrayType>(t.getTypePtr())) t = a->getElementType().getCanonicalType();
    const auto* rd = t->getAsCXXRecordDecl();
    return !rd || destroysNothing(rd, nullptr);
  };
  for (const auto& b : r->bases())
    if (!sub(b.getType())) return false;
  for (const clang::FieldDecl* fd : r->fields())
    if (!fd->getType()->isReferenceType() && !sub(fd->getType())) return false;
  return true;
}

/// The kind of a declaration, as admission.txt classes it.  The member
/// `using type = T;` of a struct is Figure 2's entry of an associated type,
/// a form of the kernel, and not Table 7's alias.
std::string declKind(const clang::Decl* d) {
  // A destructor that does nothing: defaulted, or with an empty body, and
  // not virtual, and destroying bases and members that do nothing either.
  // Its class's values are values; the destructor is scaffolding, as a
  // defaulted copy is.
  if (const auto* x = llvm::dyn_cast<clang::CXXDestructorDecl>(d))
    if (!x->isVirtual() && destroysNothing(x->getParent(), x))
      return "Decl:EmptyDestructor";
  if (const auto* a = llvm::dyn_cast<clang::TypeAliasDecl>(d))
    if (a->getName() == "type" && llvm::isa<clang::CXXRecordDecl>(a->getDeclContext()))
      return "Decl:AssociatedTypeEntry";
  if (const auto* e = llvm::dyn_cast<clang::EnumDecl>(d))
    if (!e->isScoped()) return "Enum:unscoped";
  if (const auto* c = llvm::dyn_cast<clang::EnumConstantDecl>(d))
    if (const auto* e = llvm::dyn_cast<clang::EnumDecl>(c->getDeclContext()); e && !e->isScoped())
      return "Enum:unscoped";
  return std::string("Decl:") + d->getDeclKindName();
}

/// The kind of a written type, as admission.txt classes it.  A use of an alias
/// template of the program, op_t<E>, is a kind of its own: Table 7's alias,
/// sugar the standard defines as the type it stands for
/// ([temp.alias]).  The standard's own, range_value_t, is a read the kernel
/// is given, and stays a template specialization.
/// \sugar{a scoped enumeration} enum class E : U { a, b } is read as a
/// record of one field of its underlying type, struct E { U i; }, its
/// enumerators constants of that record, its comparisons the field's, and
/// static_cast between E and an integer type the field written out
/// (Cast:enum).  Exact, not an approximation: a scoped enumeration's
/// underlying type is fixed, int unless written ([dcl.enum]/5), and the
/// values of an enumeration with a fixed underlying type are those of that
/// type ([dcl.enum]/8), so a conversion from it is defined at every value
/// ([expr.static.cast]/8).  It converts to no integer implicitly
/// ([conv.prom]) and has no built-in arithmetic ([over.built]).  An unscoped
/// enumeration is open (Enum:unscoped): without a fixed type its values are
/// only those of the smallest width that holds its enumerators, and a
/// conversion outside them is undefined.  A switch over either stays open:
/// a value no case names runs no statement, or reaches default
/// ([stmt.switch]/5).  Fixture: sugar-conditions.cpp (#1233).
std::string typeKind(clang::TypeLoc tl) {
  if (const auto* et = tl.getType()->getAs<clang::EnumType>())
    if (!et->getDecl()->isScoped()) return "Enum:unscoped";
  if (const auto* ts =
          llvm::dyn_cast<clang::TemplateSpecializationType>(tl.getTypePtr()))
    if (ts->isTypeAlias())
      if (const clang::TemplateDecl* td = ts->getTemplateName().getAsTemplateDecl();
          td && !inStd(td))
        return "Type:AliasTemplateSpecialization";
  return std::string("Type:") + tl.getType()->getTypeClassName();
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
    finder.TraverseAST(ctx);
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
    TraverseAST(ctx);
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
/// if constexpr is not judged here: Table 7 reads it as sugar, and
/// the second stage leaves such a body undecided.
class TypeObservation : public clang::ASTConsumer,
                        public clang::RecursiveASTVisitor<TypeObservation> {
 public:
  void HandleTranslationUnit(clang::ASTContext& c) override {
    ctx = &c;
    TraverseAST(c);
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
  clang::SourceRange body;  // the judged function's body: constraints and
                            // noexcept specifications are not judged here
  template <typename F>
  bool within(clang::FunctionDecl* f, F go) {
    const bool was = judging;
    const clang::SourceRange outer = body;
    judging = f->isTemplated() && f->doesThisDeclarationHaveABody() &&
              judged(f, ctx->getSourceManager());
    if (judging) body = f->getBody()->getSourceRange();
    const bool r = go();
    judging = was;
    body = outer;
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
    if (sm.isBeforeInTranslationUnit(at, body.getBegin()) ||
        sm.isBeforeInTranslationUnit(body.getEnd(), at))
      return;
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
    TraverseAST(ctx);
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
    TraverseAST(ctx);
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
    TraverseAST(ctx);
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
    TraverseAST(ctx);
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
    finder.TraverseAST(ctx);
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
    TraverseAST(ctx);
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
      ++counts[declKind(d)];
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
      ++counts[stmtKind(st)];
    return true;
  }
  bool VisitTypeLoc(clang::TypeLoc tl) {
    if (inside && !tl.isNull())
      ++counts[typeKind(tl)];
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
/// parameter, the carrier of one (decltype(E{}()), or the codomain of its
/// operation, cod<op_of<E>::type>::type), the element type of a
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
          // cod<F>::type, the codomain of an arrow, given to the kernel (the
          // result type of its one call): at a magma's operation,
          // cod<op_of<E>::type>::type, the carrier of E.  A magma is a
          // binary operator on a carrier, and the archetype's operation,
          // binary<C>, has C as its codomain.
          if (!inStd(ct) && tn == "cod")
            return a.kind == Term::Associated
                       ? Term{Term::CarrierOf, a.p, nullptr}
                       : none;
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
         n == "integral" || n == "floating_point") &&
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
  // a real type's overloads may pick differently.  Table 7's sugar is
  // copy-initialisation of a T, so the result goes into a declared T or a
  // return at T, and nowhere else.
  bool lateConversion = false;
  // A callee of the checked body whose constraints contain a negation,
  // !C<...>: at an archetype every law it does not declare is false, so the
  // negation holds there by default and fails at a model of C (the
  // theoretician's counterexample on #1212).
  bool negatedCallee = false;
};

/// Whether an expression reads a law of the program (a variable template
/// outside std), directly or through the program's concepts it names.
/// One answer per concept for each walker, so that a hierarchy whose concepts
/// share parts (a lattice over two semilattices over one magma) is walked once
/// per concept, not once per path to it: unremembered, the walk is
/// exponential in the hierarchy's depth, and it runs at every call a checked
/// body makes.  A concept met again while its own answer is being computed
/// (a cycle) answers false there, as the depth limit did.
template <typename Walk>
bool perConcept(std::map<const clang::ConceptDecl*, int>& memo,
                const clang::ConceptDecl* c, Walk walk) {
  auto [it, fresh] = memo.try_emplace(c, -1);  // -1: being computed
  if (!fresh) return it->second == 1;
  const bool r = walk(c->getConstraintExpr());
  memo[c] = r ? 1 : 0;
  return r;
}
std::map<const clang::ConceptDecl*, int> lawMemo, negationMemo;
/// The memo is keyed by declarations of one AST: cleared for every new one
/// (the first parse and each admission reparse), so that a concept of a later
/// AST at a reused address cannot inherit an answer (Copilot on #1214).
void forgetConcepts() {
  lawMemo.clear();
  negationMemo.clear();
}

bool readsLaw(const clang::Expr* e, int depth = 0) {
  if (!e || depth > 16) return false;
  e = e->IgnoreParenImpCasts();
  if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e))
    if (const auto* v =
            llvm::dyn_cast<clang::VarTemplateSpecializationDecl>(d->getDecl()))
      if (!inStd(v->getSpecializedTemplate())) return true;
  if (const auto* ul = llvm::dyn_cast<clang::UnresolvedLookupExpr>(e))
    for (const clang::NamedDecl* d : ul->decls())
      if (llvm::isa<clang::VarTemplateDecl>(d) && !inStd(d)) return true;
  if (const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e))
    if (!inStd(cs->getNamedConcept()) &&
        perConcept(lawMemo, cs->getNamedConcept(),
                   [](const clang::Expr* b) { return readsLaw(b); }))
      return true;
  for (const clang::Stmt* c : e->children())
    if (const auto* ce = llvm::dyn_cast_or_null<clang::Expr>(c))
      if (readsLaw(ce, depth + 1)) return true;
  return false;
}

/// Whether a constraint expression contains a negation, followed through the
/// concepts it names (depth-limited).
bool containsNegation(const clang::Expr* e, int depth = 0) {
  if (!e || depth > 16) return false;
  e = e->IgnoreParenImpCasts();
  // A negation that reads a law of the program, directly or through the
  // concepts it names: an archetype does not declare the law, so it reads
  // false there by default.  A negation of a structural concept of the
  // program, !Nullary<carrier_t<E>>, is as unsafe: an archetype has only what
  // a shape promises, so it meets the negation where a model with more does
  // not (Copilot on #1213).  Only a negation of the standard's concepts
  // (!std::same_as<N, bool>) is left to the type, at the atoms the check
  // enumerates.
  if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e))
    if (u->getOpcode() == clang::UO_LNot) {
      const clang::Expr* s = u->getSubExpr()->IgnoreParenImpCasts();
      if (const auto* n = llvm::dyn_cast<clang::ConceptSpecializationExpr>(s))
        if (!inStd(n->getNamedConcept())) return true;
      return readsLaw(s);
    }
  if (const auto* cs = llvm::dyn_cast<clang::ConceptSpecializationExpr>(e))
    if (!inStd(cs->getNamedConcept()) &&
        perConcept(negationMemo, cs->getNamedConcept(),
                   [](const clang::Expr* b) { return containsNegation(b); }))
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
    forgetConcepts();
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
        if (!callee) return true;
        llvm::SmallVector<clang::AssociatedConstraint, 4> ac;
        if (const clang::FunctionTemplateDecl* t = callee->getPrimaryTemplate())
          t->getAssociatedConstraints(ac);
        // A constrained member that is not a template, the call operator of
        // a class template under requires !idem<E>: its constraint is its
        // own trailing clause, as declared in the template (Copilot on
        // #1213).
        const clang::FunctionDecl* member = callee;
        if (const clang::FunctionDecl* from =
                callee->getInstantiatedFromMemberFunction())
          member = from;
        if (const clang::Expr* rc =
                member->getTrailingRequiresClause().ConstraintExpr)
          ac.push_back(clang::AssociatedConstraint(rc));
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
/// How much of the translation unit the matchers walk (README, "Choices the
/// linter makes"); each value explains itself in --help.
enum class Scan { Shallow, Deep };
llvm::cl::opt<Scan> scanMode(
    "scan", llvm::cl::desc("how much of the translation unit the matchers walk"),
    llvm::cl::values(
        clEnumValN(Scan::Shallow, "shallow",
                   "the declarations written in the file and the instantiations "
                   "under them (default); a rule whose evidence leaves the file "
                   "follows it there"),
        clEnumValN(Scan::Deep, "deep",
                   "everything the file includes or imports, as before the "
                   "choice: to test it (scan.py compares both on the fixtures)")),
    llvm::cl::init(Scan::Shallow), llvm::cl::cat(category));

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
      "::jlt::archetype::every_arithmetic<checked>;\n}\n";
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

/// The second stage's cost in one file, printed on stderr for the driver's
/// timing table: the templates it judged, the compiles it ran, their time.
struct SecondStageCost {
  unsigned templates = 0, compiles = 0;
  double seconds = 0;
} secondStageCost;

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
  const auto started = std::chrono::steady_clock::now();
  again.run(&factory);
  ++secondStageCost.compiles;
  secondStageCost.seconds += std::chrono::duration<double>(
                                 std::chrono::steady_clock::now() - started)
                                 .count();
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
    forgetConcepts();
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
      finder.TraverseAST(ctx);
      for (const clang::FunctionTemplateDecl* ft : finder.found) {
        const clang::SourceLocation loc = sm.getExpansionLoc(ft->getLocation());
        std::string where;
        llvm::raw_string_ostream w(where);
        w << sm.getFilename(loc) << ':' << sm.getExpansionLineNumber(loc) << ':'
          << sm.getExpansionColumnNumber(loc) << ": " << ft->getNameAsString();
        cache.push_back({w.str(), admit(ft, ctx)});
        ++secondStageCost.templates;
      }
      llvm::errs() << "jlt-lint-cost: templates " << secondStageCost.templates
                   << " compiles " << secondStageCost.compiles << " seconds "
                   << llvm::format("%.1f", secondStageCost.seconds) << "\n";
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
    if (const auto* p = llvm::dyn_cast_or_null<clang::NonTypeTemplateParmDecl>(d))
      values.insert(p);
    else if (d && !d->isImplicit() && !(v && v->getDescribedVarTemplate()))
      kinds.insert(declKind(d));
    ++depth;
    const bool r = clang::RecursiveASTVisitor<Kinds>::TraverseDecl(d);
    if (--depth == 0) {
      for (const auto* p : values) kinds.insert(sizes.count(p) ? "Value:size" : "Value:number");
      values.clear();
      sizes.clear();
    }
    return r;
  }
  /// A value among a template's parameters, two kinds (#1232): used only
  /// as a number, in expressions, Value:number; named inside a type, as
  /// array<T, N> or Window<N>, Value:size.  The first has a reading as a
  /// type parameter at a type with one value, its uses that type's value;
  /// the second asks for a type computed from a number, which System F has
  /// not.  A type passed the number counts as a size even where it only uses
  /// it as one; decltype(N), which names only N's type, does not count.
  bool TraverseTypeLoc(clang::TypeLoc tl, bool traverseQualifier = true) {
    if (!tl.isNull() && tl.getAs<clang::DecltypeTypeLoc>()) {
      const int saved = inType;
      inType = 0;
      const bool r = clang::RecursiveASTVisitor<Kinds>::TraverseTypeLoc(tl, traverseQualifier);
      inType = saved;
      return r;
    }
    ++inType;
    const bool r = clang::RecursiveASTVisitor<Kinds>::TraverseTypeLoc(tl, traverseQualifier);
    --inType;
    return r;
  }
  int depth = 0, inType = 0;
  std::set<const clang::NonTypeTemplateParmDecl*> values, sizes;
  /// \sugar{a range} The one form of sugar no syntax kind shows.  A
  /// parameter constrained by a standard range concept (Sugar:range), or a
  /// standard view other than the counted iota the kernel iterates over
  /// (Sugar:view).  The row's condition, finite and its size known before
  /// the first step, is the deny-rule rule3-bounded-fold's.
  bool VisitConceptSpecializationExpr(clang::ConceptSpecializationExpr* e) {
    const clang::ConceptDecl* c = e->getNamedConcept();
    if (c && inStd(c) && c->getName().ends_with("range"))
      kinds.insert("Sugar:range");
    return true;
  }
  bool VisitTemplateTypeParmDecl(clang::TemplateTypeParmDecl* d) {
    if (const clang::TypeConstraint* tc = d->getTypeConstraint())
      if (const clang::TemplateDecl* c = tc->getNamedConcept())
        if (inStd(c) && c->getName().ends_with("range"))
          kinds.insert("Sugar:range");
    return true;
  }
  bool VisitDeclRefExpr(clang::DeclRefExpr* e) {
    if (const auto* p = llvm::dyn_cast<clang::NonTypeTemplateParmDecl>(e->getDecl()); p && inType > 0)
      sizes.insert(p);
    // The view objects themselves (views::transform), not their call
    // operators, whose names are operator().
    const clang::ValueDecl* d = e->getDecl();
    if (d && llvm::isa<clang::VarDecl>(d) && inStd(d) &&
        d->getQualifiedNameAsString().find("views::") != std::string::npos &&
        d->getName() != "iota")
      kinds.insert("Sugar:view");
    // std::visit on a value whose variant type is not written (auto).
    if (d && llvm::isa<clang::FunctionDecl>(d) && d->getIdentifier() && inStd(d) &&
        (d->getName() == "visit" || d->getName() == "holds_alternative"))
      kinds.insert("Sugar:variant");
    return true;
  }
  /// \sugar{an early exit} Where a return may stand for the table's reading
  /// of statements: at the end of the function, at the end of both branches
  /// of a last if, or at the end of a branch of an if that the statements
  /// after it follow as its other branch (a guard, if (c) return e; rest,
  /// which is if (c) return e; else { rest }).  Any other return, in a loop
  /// or in a branch that only sometimes returns, is Stmt:EarlyReturn.
  /// Fixture: local-state.cpp.
  std::set<const clang::Stmt*> tailReturns;
  static bool alwaysReturns(const clang::Stmt* s) {
    if (!s) return false;
    if (llvm::isa<clang::ReturnStmt>(s)) return true;
    if (const auto* c = llvm::dyn_cast<clang::CompoundStmt>(s))
      return !c->body_empty() && alwaysReturns(c->body_back());
    if (const auto* i = llvm::dyn_cast<clang::IfStmt>(s))
      return i->getElse() && alwaysReturns(i->getThen()) && alwaysReturns(i->getElse());
    return false;
  }
  void markTail(const clang::Stmt* s) {
    if (!s) return;
    if (llvm::isa<clang::ReturnStmt>(s)) tailReturns.insert(s);
    if (const auto* c = llvm::dyn_cast<clang::CompoundStmt>(s)) {
      for (const clang::Stmt* x : c->body())
        if (const auto* i = llvm::dyn_cast<clang::IfStmt>(x))
          for (const clang::Stmt* b : {i->getThen(), i->getElse()})
            if (alwaysReturns(b)) markTail(b);  // a guard
      if (!c->body_empty()) markTail(c->body_back());
    }
    if (const auto* i = llvm::dyn_cast<clang::IfStmt>(s)) {
      markTail(i->getThen());
      markTail(i->getElse());
    }
  }
  bool VisitFunctionDecl(clang::FunctionDecl* f) {
    if (f->doesThisDeclarationHaveABody()) markTail(f->getBody());
    return true;
  }
  bool VisitLambdaExpr(clang::LambdaExpr* l) {
    markTail(l->getBody());  // a lambda's returns are its own
    return true;
  }
  const clang::ASTContext* ctx = nullptr;
  /// \sugar{an implicit conversion of an arithmetic value} A conversion
  /// between integer types that the destination cannot hold at every value
  /// of the source ([dcl.init.list]) is Conversion:narrowing, refused; a
  /// constant that fits is held.  Fixture: sugar-conditions.cpp.
  bool narrows(const clang::ImplicitCastExpr* c) const {
    // The conversion written out, int(i), has an implicit node inside it:
    // that one is the explicit conversion's, not an implicit conversion.
    if (!ctx || c->isPartOfExplicitCast()) return false;
    // A floating value to an integer: undefined out of range, so the one
    // narrowing at which the two binding times can part.
    if (c->getCastKind() == clang::CK_FloatingToIntegral) return true;
    // To a narrower floating type, or an integer to a floating one: narrowing
    // ([dcl.init.list]) unless the source is a constant the destination
    // holds, within range for the first, exactly for the second.
    if (c->getCastKind() == clang::CK_FloatingCast || c->getCastKind() == clang::CK_IntegralToFloating)
      return floatNarrows(c->getSubExpr(), c->getType(), c->getCastKind() == clang::CK_FloatingCast);
    if (c->getCastKind() != clang::CK_IntegralCast &&
        c->getCastKind() != clang::CK_IntegralToBoolean)
      return false;
    return narrows(c->getSubExpr(), c->getType());
  }
  /// [dcl.init.list]: a floating value to a type of lower rank, unless a
  /// constant whose value is within the destination's range; an integer to
  /// a floating type, unless a constant that converts back to itself.
  bool floatNarrows(const clang::Expr* e, clang::QualType to, bool floating) const {
    const clang::QualType from = e->getType();
    if (from->isDependentType() || to->isDependentType() || e->isValueDependent()) return false;
    const auto& sem = ctx->getFloatTypeSemantics(to);
    if (floating) {
      if (ctx->getFloatingTypeOrder(to, from) >= 0) return false;  // the same rank or higher
      llvm::APFloat v(0.0);
      if (!e->isCXX11ConstantExpr(*ctx) || !e->EvaluateAsFloat(v, *ctx)) return true;
      bool lost = false;
      return (v.convert(sem, llvm::APFloat::rmNearestTiesToEven, &lost) & llvm::APFloat::opOverflow) != 0;
    }
    if (auto v = e->getIntegerConstantExpr(*ctx)) {
      llvm::APFloat f(sem);
      return f.convertFromAPInt(*v, v->isSigned(), llvm::APFloat::rmNearestTiesToEven) != llvm::APFloat::opOK;
    }
    return true;
  }
  /// Whether every value of the integer type from is a value of to.
  bool fits(clang::QualType from, clang::QualType to) const {
    const bool toBool = to->isBooleanType();
    const unsigned wf = ctx->getIntWidth(from), wt = toBool ? 1 : ctx->getIntWidth(to);
    const bool sf = from->isSignedIntegerOrEnumerationType(),
               st = !toBool && to->isSignedIntegerType();
    return sf == st ? wt >= wf : (!sf && st && wt > wf);
  }
  /// The conversion of e to an integer type that cannot hold every value
  /// of e's own.
  bool narrows(const clang::Expr* e, clang::QualType to) const {
    if (!ctx) return false;
    const clang::QualType from = e->getType();
    if (from->isDependentType() || to->isDependentType() || e->isValueDependent() ||
        !from->isIntegralOrEnumerationType() || !to->isIntegerType())
      return false;
    const bool toBool = to->isBooleanType();
    const unsigned wf = ctx->getIntWidth(from), wt = toBool ? 1 : ctx->getIntWidth(to);
    const bool sf = from->isSignedIntegerOrEnumerationType(),
               st = !toBool && to->isSignedIntegerType();
    // Every value of the source is a value of the destination.
    if (sf == st ? wt >= wf : (!sf && st && wt > wf)) return false;
    // A constant is held if its value is.
    if (auto v = e->getIntegerConstantExpr(*ctx)) {
      if (toBool) return !(*v == 0 || *v == 1);
      const llvm::APSInt lo = llvm::APSInt::getMinValue(wt, !st), hi = llvm::APSInt::getMaxValue(wt, !st);
      return llvm::APSInt::compareValues(*v, lo) < 0 || llvm::APSInt::compareValues(*v, hi) > 0;
    }
    return true;
  }
  // ---- The binary sum, std::variant<A, B> (#1222). ----
  /// Whether t is a std::variant of any number of types.
  static bool isVariant(clang::QualType t) {
    if (t.isNull()) return false;
    t = t.getNonReferenceType().getUnqualifiedType();
    if (const auto* r = t->getAsCXXRecordDecl())
      return r->getIdentifier() && r->getName() == "variant" && inStd(r);
    if (const auto* ts = t->getAs<clang::TemplateSpecializationType>())
      if (const clang::TemplateDecl* td = ts->getTemplateName().getAsTemplateDecl())
        return td->getIdentifier() && td->getName() == "variant" && inStd(td);
    return false;
  }
  /// The alternatives of a variant, or of an expected, where they are known:
  /// a pack expanded; empty where the type is dependent with a pack.
  static std::vector<clang::QualType> alternatives(clang::QualType t) {
    std::vector<clang::QualType> out;
    if (t.isNull()) return out;
    t = t.getNonReferenceType().getUnqualifiedType();
    const auto add = [&](const clang::TemplateArgument& a, auto&& self) -> bool {
      if (a.getKind() == clang::TemplateArgument::Pack) {
        for (const auto& e : a.pack_elements())
          if (!self(e, self)) return false;
        return true;
      }
      if (a.getKind() != clang::TemplateArgument::Type) return false;
      if (a.getAsType()->getAs<clang::PackExpansionType>()) return false;
      out.push_back(a.getAsType());
      return true;
    };
    if (const auto* sd = llvm::dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(t->getAsCXXRecordDecl())) {
      for (const auto& a : sd->getTemplateArgs().asArray())
        if (!add(a, add)) return {};
      return out;
    }
    if (const auto* ts = t->getAs<clang::TemplateSpecializationType>()) {
      for (const auto& a : ts->template_arguments())
        if (!add(a, add)) return {};
      return out;
    }
    return out;
  }
  /// Whether t is the kernel's sum: a std::variant of exactly two types.
  static bool isSum(clang::QualType t) { return isVariant(t) && alternatives(t).size() == 2; }
  /// \sugar{the choice on a sum's alternative, x const} Whether a tested
  /// variable cannot be written: const, so that no write stands between its
  /// test and a reading.  A reference to const counts
  /// here, but may name an object written through another path
  /// ([dcl.type.cv]), so knownSide accepts its test and its reading only
  /// inside one expression (aliased()).
  static bool constant(const clang::ValueDecl* x) {
    return x && x->getType().getNonReferenceType().isConstQualified();
  }
  /// Whether a tested variable is a reference: const, but not a const object.
  static bool aliased(const clang::ValueDecl* x) { return x && x->getType()->isReferenceType(); }
  /// \sugar{the positional injections} The kind of a payload that is not of
  /// its alternative's type, or none where it is: Sum:payload-converted where both types are known (a
  /// narrowing or a widening, which the row does not read), and
  /// Sum:payload-dependent where they differ as written in a template, so
  /// that only an instance would tell (lift(-1) with T = int converts -1 to
  /// SIZE_MAX).  A template's payload of exactly the alternative's type, as
  /// in Inl's body, A for A, is no conversion at any instance.
  std::optional<std::string> payloadKind(const clang::Expr* e, clang::QualType to) const {
    if (!ctx || !e || to.isNull()) return std::nullopt;
    const clang::QualType from = e->IgnoreParenImpCasts()->getType();
    if (from.isNull()) return std::string("Sum:payload-dependent");
    if (from.getCanonicalType().getUnqualifiedType() == to.getCanonicalType().getUnqualifiedType())
      return std::nullopt;
    if (from->isDependentType() || to->isDependentType()) return std::string("Sum:payload-dependent");
    return std::string("Sum:payload-converted");
  }
  /// The arguments of a construction in a template, T(a, b) or T{a, b}.
  static std::vector<const clang::Expr*> unresolvedArgs(const clang::CXXUnresolvedConstructExpr* c) {
    std::vector<const clang::Expr*> out;
    if (c->getNumArgs() == 1)
      if (const auto* l = llvm::dyn_cast<clang::InitListExpr>(c->getArg(0))) {
        for (const clang::Expr* e : l->inits()) out.push_back(e);
        return out;
      }
    for (const clang::Expr* e : c->arguments()) out.push_back(e);
    return out;
  }
  /// \sugar{std::expected, A not void} Whether t is std::expected<void, B>: its reading, variant<void, B>, is
  /// ill-formed ([variant.variant.general]), so it has no row yet.
  static bool expectedVoid(clang::QualType t) {
    if (!isExpected(t)) return false;
    const auto alts = alternatives(t);
    return !alts.empty() && alts[0]->isVoidType();
  }
  // ---- The copairing written as a conditional on index(): sugar. ----
  /// The variable an expression names, parentheses and implicit nodes aside.
  static const clang::ValueDecl* named(const clang::Expr* e) {
    if (!e) return nullptr;
    const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e->IgnoreParenImpCasts());
    return d ? d->getDecl() : nullptr;
  }
  /// Whether e is x.index() on a variable x, resolved or in a template.
  static bool isIndexOf(const clang::Expr* e, const clang::ValueDecl* x) {
    if (!e || !x) return false;
    e = e->IgnoreParenImpCasts();
    if (const auto* m = llvm::dyn_cast<clang::CXXMemberCallExpr>(e))
      return m->getMethodDecl() && m->getMethodDecl()->getIdentifier() && m->getMethodDecl()->getName() == "index" &&
             isSum(m->getImplicitObjectArgument()->getType()) && named(m->getImplicitObjectArgument()) == x;
    if (const auto* c = llvm::dyn_cast<clang::CallExpr>(e))
      if (const auto* d = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(c->getCallee()->IgnoreParens()))
        return !d->isImplicitAccess() && d->getMember().isIdentifier() && d->getMember().getAsString() == "index" &&
               isSum(d->getBaseType()) && named(d->getBase()) == x;
    return false;
  }
  /// \sugar{the choice on a sum's alternative, the test} For a condition on
  /// x: the alternative known where it holds and the one known where it fails, each 0, 1, or -1 for none.  x.index() == k
  /// and != k with k an integer constant 0 or 1, x.index() alone (true on
  /// alternative 1), x.has_value() and x as a truth value (true on
  /// alternative 0), a negation, and && and ||: a conjunction that holds
  /// holds in each operand, a disjunction that fails fails in each.
  std::pair<int, int> indexTest(const clang::Expr* cond, const clang::ValueDecl* x) const {
    constexpr std::pair<int, int> none{-1, -1};
    if (!cond || !x) return none;
    cond = cond->IgnoreParenImpCasts();
    if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(cond); u && u->getOpcode() == clang::UO_LNot) {
      const auto [t, f] = indexTest(u->getSubExpr(), x);
      return {f, t};
    }
    if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(cond)) {
      if (b->getOpcode() == clang::BO_LAnd || b->getOpcode() == clang::BO_LOr) {
        const auto l = indexTest(b->getLHS(), x), r = indexTest(b->getRHS(), x);
        if (b->getOpcode() == clang::BO_LAnd) return {l.first >= 0 ? l.first : r.first, -1};
        return {-1, l.second >= 0 ? l.second : r.second};
      }
      if (b->getOpcode() == clang::BO_EQ || b->getOpcode() == clang::BO_NE) {
        const auto constant01 = [&](const clang::Expr* e) -> int {
          if (!ctx || e->isValueDependent()) return -1;
          if (auto v = e->getIntegerConstantExpr(*ctx); v && v->ule(1)) return static_cast<int>(v->getZExtValue());
          return -1;
        };
        int k = -1;
        if (isIndexOf(b->getLHS(), x)) k = constant01(b->getRHS());
        else if (isIndexOf(b->getRHS(), x)) k = constant01(b->getLHS());
        if (k < 0) return none;
        return b->getOpcode() == clang::BO_EQ ? std::pair{k, 1 - k} : std::pair{1 - k, k};
      }
      return none;
    }
    if (isHasValueOf(cond, x)) return {0, 1};
    if (isIndexOf(cond, x)) return {1, 0};
    return none;
  }
  /// Whether e is x.has_value() on a variable x of a std::expected type.
  static bool isHasValueOf(const clang::Expr* e, const clang::ValueDecl* x) {
    if (!e || !x) return false;
    e = e->IgnoreParenImpCasts();
    if (const auto* m = llvm::dyn_cast<clang::CXXMemberCallExpr>(e)) {
      const clang::CXXMethodDecl* g = m->getMethodDecl();
      return g && (llvm::isa<clang::CXXConversionDecl>(g) || (g->getIdentifier() && g->getName() == "has_value")) &&
             isExpected(m->getImplicitObjectArgument()->getType()) && named(m->getImplicitObjectArgument()) == x;
    }
    if (const auto* c = llvm::dyn_cast<clang::CallExpr>(e))
      if (const auto* d = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(c->getCallee()->IgnoreParens()))
        return !d->isImplicitAccess() && d->getMember().isIdentifier() && d->getMember().getAsString() == "has_value" &&
               isExpected(d->getBaseType()) && named(d->getBase()) == x;
    return false;
  }
  /// Whether t is std::expected<A, B>, written or deduced, dependent or not.
  static bool isExpected(clang::QualType t) {
    if (t.isNull()) return false;
    t = t.getNonReferenceType().getUnqualifiedType();
    if (const auto* r = t->getAsCXXRecordDecl())
      return r->getIdentifier() && r->getName() == "expected" && inStd(r);
    if (const auto* ts = t->getAs<clang::TemplateSpecializationType>())
      if (const clang::TemplateDecl* td = ts->getTemplateName().getAsTemplateDecl())
        return td->getIdentifier() && td->getName() == "expected" && inStd(td);
    return false;
  }
  static bool namedStd(clang::QualType t, llvm::StringRef name) {
    t = t.getNonReferenceType().getUnqualifiedType();
    if (const auto* r = t->getAsCXXRecordDecl(); r && r->getIdentifier() && r->getName() == name && inStd(r)) return true;
    return false;
  }
  /// \sugar{std::expected} The uses of a std::expected, the other spelling
  /// of a binary sum, read as std::variant<A, B>: the tagged constructions are the injections, and
  /// x.has_value() ? ... *x ... : ... x.error() ... is the copairing's sugar,
  /// *x and x-> only where side 0 is known, x.error() only where side 1 is.
  /// value() and writes are refused; the rest of its interface is open.
  std::optional<std::string> expectedUse(const clang::Stmt* st) const {
    if (const auto* c = llvm::dyn_cast<clang::CXXConstructExpr>(st); c && isExpected(c->getType())) {
      if (c->getConstructor()->isCopyOrMoveConstructor()) return std::nullopt;
      if (expectedVoid(c->getType())) return std::string("Sum:expected-void");
      if (c->getNumArgs() == 2 && (namedStd(c->getArg(0)->getType(), "in_place_t") ||
                                   namedStd(c->getArg(0)->getType(), "unexpect_t"))) {
        const auto alts = alternatives(c->getType());
        const std::size_t k = namedStd(c->getArg(0)->getType(), "in_place_t") ? 0 : 1;
        if (alts.size() == 2)
          if (auto p = payloadKind(c->getArg(1), alts[k])) return p;
        return std::string("Sum:inj-sugar");
      }
      return std::string("Sugar:expected");
    }
    // In a template: expected<A, B>{in_place, a} or {unexpect, b}, the
    // payload's type known only at the instance where it is not the
    // alternative's as written.
    if (const auto* c = llvm::dyn_cast<clang::CXXUnresolvedConstructExpr>(st); c && isExpected(c->getTypeAsWritten())) {
      if (expectedVoid(c->getTypeAsWritten())) return std::string("Sum:expected-void");
      const auto args = unresolvedArgs(c);
      if (args.size() == 2 && (namedStd(args[0]->getType(), "in_place_t") ||
                               namedStd(args[0]->getType(), "unexpect_t"))) {
        const auto alts = alternatives(c->getTypeAsWritten());
        const std::size_t k = namedStd(args[0]->getType(), "in_place_t") ? 0 : 1;
        if (alts.size() != 2) return std::string("Sum:payload-dependent");
        if (auto p = payloadKind(args[1], alts[k])) return p;
        return std::string("Sum:inj-sugar");
      }
      return std::string("Sugar:expected");
    }
    std::string name;
    const clang::Expr* obj = nullptr;
    if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(st)) {
      if (o->getNumArgs() == 0 || !isExpected(o->getArg(0)->getType())) return std::nullopt;
      obj = o->getArg(0);
      switch (o->getOperator()) {
        case clang::OO_Star: name = "*"; break;
        case clang::OO_Arrow: name = "->"; break;
        case clang::OO_Equal: name = "="; break;
        default: name = "operator"; break;
      }
    } else if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(st)) {
      if (u->getOpcode() != clang::UO_Deref || !isExpected(u->getSubExpr()->getType())) return std::nullopt;
      name = "*"; obj = u->getSubExpr();
    } else if (const auto* m = llvm::dyn_cast<clang::CXXMemberCallExpr>(st)) {
      const clang::CXXMethodDecl* g = m->getMethodDecl();
      obj = m->getImplicitObjectArgument();
      if (!g || !obj || !isExpected(obj->getType())) return std::nullopt;
      name = llvm::isa<clang::CXXConversionDecl>(g) ? "bool" : (g->getIdentifier() ? g->getName().str() : "");
    } else if (const auto* c = llvm::dyn_cast<clang::CallExpr>(st)) {
      const auto* d = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(c->getCallee()->IgnoreParens());
      if (!d || d->isImplicitAccess() || d->isArrow() || !isExpected(d->getBaseType()) || !d->getMember().isIdentifier())
        return std::nullopt;
      name = d->getMember().getAsString(); obj = d->getBase();
    } else if (const auto* d = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(st)) {
      if (d->isImplicitAccess() || !d->isArrow() || !isExpected(d->getBaseType())) return std::nullopt;
      name = "->"; obj = d->getBase();
    } else {
      return std::nullopt;
    }
    const clang::ValueDecl* x = named(obj);
    if (expectedVoid(obj->getType())) return std::string("Sum:expected-void");
    if (name == "has_value" || name == "bool") {
      const std::string k = indexKind(st, x);
      return k == "Sum:case-sugar" || k == "Sum:case-converted" ? k : "Sugar:expected";
    }
    if (name == "*" || name == "->") return reading(st, x, 0);
    if (name == "error") return reading(st, x, 1);
    if (name == "value" || name == "=" || name == "emplace" || name == "swap") return std::string("Sum:other");
    return std::string("Sugar:expected");
  }
  static bool returns(const clang::Stmt* s) {
    if (const auto* c = llvm::dyn_cast_or_null<clang::CompoundStmt>(s); c && !c->body_empty()) s = c->body_back();
    return s && llvm::isa<clang::ReturnStmt>(s);
  }
  /// \sugar{the choice on a sum's alternative, the reading} The side of x
  /// known where an occurrence stands, 0 or 1, or -1.  A reading get<k>(x)
  /// is the copairing's sugar only where side k is known, and x is used
  /// otherwise only whole.  The nearest informative test of x decides (a test that says nothing of x,
  /// as p in p || x.index() == 1, is passed over, x being const): a branch
  /// of c ? t : e or of an if, the right operand of && or ||, or the
  /// statements after a guard if (c) return ...; with no else.  A lambda or
  /// a function ends the search.  Where x is a reference, the test and the
  /// reading must stand in one expression, a conditional or && or ||: the
  /// object it names may be written through another path between two
  /// statements.
  int knownSide(const clang::Stmt* occ, const clang::ValueDecl* x) const {
    if (!constant(x) || !ctx) return -1;
    const clang::Stmt* child = occ;
    clang::DynTypedNode n = clang::DynTypedNode::create(*occ);
    for (int k = 0; k < 256; ++k) {
      auto ps = const_cast<clang::ASTContext*>(ctx)->getParents(n);
      if (ps.empty()) return -1;
      n = ps[0];
      const clang::Stmt* s = n.get<clang::Stmt>();
      if (!s) {
        if (n.get<clang::VarDecl>()) continue;
        return -1;
      }
      if (llvm::isa<clang::LambdaExpr>(s)) return -1;
      if (aliased(x) && !llvm::isa<clang::Expr>(s)) return -1;
      if (const auto* c = llvm::dyn_cast<clang::ConditionalOperator>(s)) {
        const auto [t, f] = indexTest(c->getCond(), x);
        if (t >= 0 && child == c->getTrueExpr()) return t;
        if (f >= 0 && child == c->getFalseExpr()) return f;
      } else if (const auto* i = llvm::dyn_cast<clang::IfStmt>(s)) {
        if (!i->isConsteval() && !i->isConstexpr()) {
          const auto [t, f] = indexTest(i->getCond(), x);
          if (t >= 0 && child == i->getThen()) return t;
          if (f >= 0 && child == i->getElse()) return f;
        }
      } else if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(s)) {
        if (child == b->getRHS() && (b->getOpcode() == clang::BO_LAnd || b->getOpcode() == clang::BO_LOr)) {
          const auto [t, f] = indexTest(b->getLHS(), x);
          const int k = b->getOpcode() == clang::BO_LAnd ? t : f;
          if (k >= 0) return k;
        }
      } else if (const auto* b = llvm::dyn_cast<clang::CompoundStmt>(s)) {
        int found = -1;
        for (const clang::Stmt* e : b->body()) {
          if (e == child) break;
          const auto* i = llvm::dyn_cast<clang::IfStmt>(e);
          if (!i || i->getElse() || i->isConstexpr() || i->isConsteval() || !returns(i->getThen())) continue;
          if (const int f = indexTest(i->getCond(), x).second; f >= 0) found = f;
        }
        if (found >= 0) return found;
      }
      child = s;
    }
    return -1;
  }
  /// Whether an expression is converted, reading a value and adding const aside.
  static bool converted(const clang::Expr* x) {
    const auto* i = llvm::dyn_cast<clang::ImplicitCastExpr>(x->IgnoreParens());
    if (!i) return false;
    switch (i->getCastKind()) {
      case clang::CK_LValueToRValue: case clang::CK_NoOp: return converted(i->getSubExpr());
      default: return true;
    }
  }
  /// \sugar{the choice on a sum's alternative, the branches} The kind of
  /// x.index(): the test of the copairing's sugar where it stands in the condition of a choice, or in the left operand of && or
  /// ||, compared with 0 or 1, negated or alone; converted, refused, where
  /// the choice's branches differ in type; open anywhere else.
  std::string indexKind(const clang::Stmt* st, const clang::ValueDecl* x) const {
    if (!constant(x) || !ctx) return "Sugar:variant";
    const clang::Stmt* child = st;
    clang::DynTypedNode n = clang::DynTypedNode::create(*st);
    for (int k = 0; k < 16; ++k) {
      auto ps = const_cast<clang::ASTContext*>(ctx)->getParents(n);
      if (ps.empty()) break;
      n = ps[0];
      const clang::Stmt* s = n.get<clang::Stmt>();
      if (!s) break;
      if (llvm::isa<clang::ImplicitCastExpr, clang::ParenExpr, clang::ExprWithCleanups>(s)) { child = s; continue; }
      if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(s); u && u->getOpcode() == clang::UO_LNot) { child = s; continue; }
      if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(s)) {
        if (b->getOpcode() == clang::BO_EQ || b->getOpcode() == clang::BO_NE ||
            b->getOpcode() == clang::BO_LAnd || b->getOpcode() == clang::BO_LOr) {
          child = s;
          continue;
        }
        break;
      }
      if (const auto* c = llvm::dyn_cast<clang::ConditionalOperator>(s)) {
        const auto [t, f] = indexTest(c->getCond(), x);
        if (child != c->getCond() || (t < 0 && f < 0)) break;
        // In a template the conversions are not yet in the tree: branches
        // whose types differ as written, one of them dependent, agree or not
        // only at an instance (Copilot on #1226), so the choice is open.
        const clang::QualType tt = c->getTrueExpr()->getType(), ft = c->getFalseExpr()->getType();
        if ((tt->isDependentType() || ft->isDependentType()) &&
            !ctx->hasSameUnqualifiedType(tt.getNonReferenceType(), ft.getNonReferenceType()))
          return "Sum:case-dependent";
        return converted(c->getTrueExpr()) || converted(c->getFalseExpr()) ? "Sum:case-converted" : "Sum:case-sugar";
      }
      if (const auto* i = llvm::dyn_cast<clang::IfStmt>(s)) {
        const auto [t, f] = indexTest(i->getCond(), x);
        if (child != i->getCond() || i->isConstexpr() || i->isConsteval() || (t < 0 && f < 0)) break;
        return "Sum:case-sugar";
      }
      break;
    }
    // A test as an operand of && or || that is no choice's condition, as in
    // return x.index() == 0 && std::get<0>(x) > k;: it guards the operand
    // after it.
    {
      clang::DynTypedNode m = clang::DynTypedNode::create(*st);
      for (int k = 0; k < 16; ++k) {
        auto ps = const_cast<clang::ASTContext*>(ctx)->getParents(m);
        if (ps.empty()) break;
        m = ps[0];
        if (const auto* b = m.get<clang::BinaryOperator>())
          if (b->getOpcode() == clang::BO_LAnd || b->getOpcode() == clang::BO_LOr) {
            const auto [t, f] = indexTest(b->getLHS(), x);
            if (t >= 0 || f >= 0) return "Sum:case-sugar";
          }
        if (!m.get<clang::Expr>()) break;
      }
    }
    return "Sugar:variant";
  }
  /// A reading of alternative k of x: the copairing's sugar where k is
  /// known, a finding of its own where x is a variable that is not const
  /// (so that the message says what to do), refused otherwise.
  std::string reading(const clang::Stmt* st, const clang::ValueDecl* x, int k) const {
    if (x && !constant(x) && llvm::isa<clang::VarDecl>(x)) return "Sum:not-const";
    const int side = knownSide(st, x);
    if (side == k) return "Sum:case-sugar";
    // A reference read outside the expression that tests it: copy it into a
    // const local.
    if (side < 0 && aliased(x) && llvm::isa<clang::VarDecl>(x)) return "Sum:not-const";
    // Inside a switch on x.index(): total, a choice not yet read as sugar.
    if (side < 0 && inSwitchOnIndex(st, x)) return "Sugar:variant";
    return "Sum:other";
  }
  /// Whether an occurrence stands in the body of switch (x.index()).
  bool inSwitchOnIndex(const clang::Stmt* occ, const clang::ValueDecl* x) const {
    if (!ctx || !x) return false;
    clang::DynTypedNode n = clang::DynTypedNode::create(*occ);
    for (int k = 0; k < 256; ++k) {
      auto ps = const_cast<clang::ASTContext*>(ctx)->getParents(n);
      if (ps.empty()) return false;
      n = ps[0];
      const clang::Stmt* s = n.get<clang::Stmt>();
      if (!s) return false;
      if (llvm::isa<clang::LambdaExpr>(s)) return false;
      if (const auto* w = llvm::dyn_cast<clang::SwitchStmt>(s)) return isIndexOf(w->getCond(), x);
    }
    return false;
  }
  /// The index K of std::get<K>(x), or -1 where it names a type or is not known.
  int getIndex(const clang::CallExpr* c) const {
    const clang::Expr* callee = c->getCallee()->IgnoreParenImpCasts();
    llvm::ArrayRef<clang::TemplateArgumentLoc> args;
    if (const auto* r = llvm::dyn_cast<clang::DeclRefExpr>(callee)) args = r->template_arguments();
    if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee)) args = u->template_arguments();
    if (args.empty() || !ctx) return -1;
    const clang::TemplateArgument& a = args[0].getArgument();
    if (a.getKind() == clang::TemplateArgument::Integral) return static_cast<int>(a.getAsIntegral().getZExtValue());
    if (a.getKind() == clang::TemplateArgument::Expression)
      if (auto v = a.getAsExpr()->getIntegerConstantExpr(*ctx); v && v->ule(1)) return static_cast<int>(v->getZExtValue());
    return -1;
  }
  /// \sugar{std::variant, its other uses} The kind of a use of a sum other
  /// than through its structure arrows.  A variant of three or more types
  /// (Sugar:variant) is open until its nesting is checked; std::visit,
  /// holds_alternative, get_if, valueless_by_exception and a switch on the
  /// index are open too.  Otherwise: open, sugar not yet written, where it is total (the tagged
  /// construction, visit, holds_alternative, index, ==); refused where it
  /// may fail or changes the sum (get, emplace, =, swap).  None where the statement is no such use.
  std::optional<std::string> sumUse(const clang::Stmt* st) const {
    // A variant of more than two types: read as binary ones nested, a
    // sugar not yet written.
    {
      const clang::Expr* obj = nullptr;
      if (const auto* c = llvm::dyn_cast<clang::CXXConstructExpr>(st)) {
        if (isVariant(c->getType()) && !isSum(c->getType()) && !c->getConstructor()->isCopyOrMoveConstructor())
          return std::string("Sugar:variant");
      } else if (const auto* m = llvm::dyn_cast<clang::CXXMemberCallExpr>(st)) {
        obj = m->getImplicitObjectArgument();
      } else if (const auto* c = llvm::dyn_cast<clang::CallExpr>(st)) {
        for (const clang::Expr* a : c->arguments())
          if (isVariant(a->getType())) obj = a;
      }
      if (obj && isVariant(obj->getType()) && !isSum(obj->getType())) return std::string("Sugar:variant");
    }
    const auto kind = [&](llvm::StringRef name) -> std::optional<std::string> {
      // get_if is total, a pointer or null, and valueless_by_exception a
      // total observer: open, as the other total uses.
      static const std::set<std::string> partial{"get", "emplace", "swap", "="};
      return std::string(partial.count(name.str()) ? "Sum:other" : "Sugar:variant");
    };
    if (const auto* c = llvm::dyn_cast<clang::CXXConstructExpr>(st); c && isSum(c->getType())) {
      if (c->getConstructor()->isCopyOrMoveConstructor()) return std::nullopt;
      // variant<A, B>(in_place_index<k>, a): the injection inl or inr in the
      // standard's positional spelling, sugar; a payload that narrows is
      // refused.
      if (c->getNumArgs() == 2)
        if (const auto* sd = llvm::dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(
                c->getArg(0)->getType().getNonReferenceType()->getAsCXXRecordDecl());
            sd && sd->getIdentifier() && sd->getName() == "in_place_index_t" && inStd(sd) &&
            sd->getTemplateArgs().size() == 1 && sd->getTemplateArgs()[0].getKind() == clang::TemplateArgument::Integral) {
          const auto k = sd->getTemplateArgs()[0].getAsIntegral().getZExtValue();
          const auto alts = alternatives(c->getType());
          if (k > 1 || alts.size() != 2) return std::string("Sum:other");
          if (auto p = payloadKind(c->getArg(1), alts[k])) return p;
          return std::string("Sum:inj-sugar");
        }
      return kind("construct");
    }
    if (const auto* c = llvm::dyn_cast<clang::CXXUnresolvedConstructExpr>(st); c && isSum(c->getTypeAsWritten())) {
      // In a template: variant<A, B>(in_place_index<k>, a), the positional
      // injection, its payload's type known only at the instance; braces or
      // parentheses.
      if (const auto args = unresolvedArgs(c); args.size() == 2)
        if (const auto* sd = llvm::dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(
                args[0]->getType().getNonReferenceType()->getAsCXXRecordDecl());
            sd && sd->getIdentifier() && sd->getName() == "in_place_index_t" && inStd(sd) &&
            sd->getTemplateArgs().size() == 1 && sd->getTemplateArgs()[0].getKind() == clang::TemplateArgument::Integral) {
          const auto k = sd->getTemplateArgs()[0].getAsIntegral().getZExtValue();
          const auto alts = alternatives(c->getTypeAsWritten());
          if (k > 1) return std::string("Sum:other");
          if (alts.size() != 2) return std::string("Sum:payload-dependent");
          if (auto p = payloadKind(args[1], alts[k])) return p;
          return std::string("Sum:inj-sugar");
        }
      return kind("construct");
    }
    // A comparison C++20 rewrites through <=>: an order on a sum, open as ==.
    if (const auto* r = llvm::dyn_cast<clang::CXXRewrittenBinaryOperator>(st);
        r && (isVariant(r->getLHS()->getType()) || isVariant(r->getRHS()->getType())))
      return kind("==");
    if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(st)) {
      if (o->getNumArgs() == 0 || !isSum(o->getArg(0)->getType())) return std::nullopt;
      if (o->getOperator() == clang::OO_Equal) return kind("=");
      switch (o->getOperator()) {
        case clang::OO_EqualEqual: case clang::OO_ExclaimEqual: case clang::OO_Less: case clang::OO_LessEqual:
        case clang::OO_Greater: case clang::OO_GreaterEqual: case clang::OO_Spaceship:
          return kind("==");
        default: break;
      }
      return std::nullopt;
    }
    if (const auto* m = llvm::dyn_cast<clang::CXXMemberCallExpr>(st)) {
      const clang::CXXMethodDecl* g = m->getMethodDecl();
      const clang::Expr* obj = m->getImplicitObjectArgument();
      if (g && g->getIdentifier() && obj && isSum(obj->getType())) {
        if (g->getName() == "index") return indexKind(st, named(obj));
        return kind(g->getName());
      }
      return std::nullopt;
    }
    if (const auto* c = llvm::dyn_cast<clang::CallExpr>(st)) {
      const clang::Expr* callee = c->getCallee()->IgnoreParenImpCasts();
      if (const auto* d = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(callee))
        if (!d->isImplicitAccess() && isSum(d->getBaseType()) && d->getMember().isIdentifier()) {
          if (d->getMember().getAsString() == "index") return indexKind(st, named(d->getBase()));
          return kind(d->getMember().getAsString());
        }
      std::vector<const clang::NamedDecl*> ds;
      if (const auto* r = llvm::dyn_cast<clang::DeclRefExpr>(callee)) ds.push_back(r->getDecl());
      if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee)) ds.assign(u->decls_begin(), u->decls_end());
      for (const clang::NamedDecl* d : ds)
        if (d && d->getIdentifier() && inStd(d))
          for (const clang::Expr* arg : c->arguments())
            if (isSum(arg->getType())) {
              // std::get<K>(x) in the branch that knows x holds side K: the
              // copairing's sugar.  By type, or anywhere else: refused.
              if (d->getName() == "get" && c->getNumArgs() == 1) {
                const int k = getIndex(c);
                if (k >= 0) return reading(st, named(arg), k);
              }
              return kind(d->getName());
            }
    }
    return std::nullopt;
  }
  /// A call of a function not declared constexpr, for a node whose kind the
  /// sum's forms give.
  void notConstexprCall(const clang::Stmt* st) {
    const clang::FunctionDecl* callee = nullptr;
    if (const auto* c = llvm::dyn_cast<clang::CallExpr>(st)) callee = c->getDirectCallee();
    if (const auto* c = llvm::dyn_cast<clang::CXXConstructExpr>(st)) callee = c->getConstructor();
    if (callee && !callee->isConstexpr() && !callee->getBuiltinID() && !callee->isImplicit() && !callee->isDeleted())
      kinds.insert("Call:NotConstexpr");
  }
  bool VisitStmt(clang::Stmt* st) {
    if (const auto* t = llvm::dyn_cast<clang::CXXThisExpr>(st))
      if (t->isImplicit()) return true;  // a member named without this->
    // A use of a sum: its kind, in place of the call's or the operator's.
    if (auto k = expectedUse(st)) {
      kinds.insert(*k);
      notConstexprCall(st);
      return true;
    }
    if (auto k = sumUse(st)) {
      notConstexprCall(st);
      kinds.insert(*k);
      return true;
    }
    // A container's update in a template, h.push_back(e) with h of a
    // dependent type: the member call it is at every instance, sugar (the
    // row of the container's update), not a kernel call.
    if (const auto* c = llvm::dyn_cast<clang::CallExpr>(st))
      if (const auto* d = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(c->getCallee()->IgnoreParens());
          d && d->getMember().isIdentifier()) {
        static const std::set<std::string> updates{"push_back", "push_front", "clear"};
        if (updates.count(d->getMember().getAsString())) kinds.insert("Stmt:CXXMemberCallExpr");
      }
    // c += n converts the result back to c's type with no node of its own.
    // The value stored back is of the computation's type (c += 1 with c an
    // unsigned char computes in int, and 256 becomes 0): narrowing where that
    // type does not fit the destination's, whatever the right side is.  Two
    // bools combined by &=, |=, ^= give a bool.
    if (const auto* ca = llvm::dyn_cast<clang::CompoundAssignOperator>(st)) {
      const clang::QualType to = ca->getLHS()->getType(), via = ca->getComputationResultType();
      const bool bools = to->isBooleanType() &&
                         ca->getRHS()->IgnoreParenImpCasts()->getType()->isBooleanType();
      if (narrows(ca->getRHS()->IgnoreParenImpCasts(), to) ||
          (ctx && !bools && !to->isDependentType() && !via.isNull() && !via->isDependentType() &&
           via->isIntegerType() && to->isIntegerType() && !fits(via, to)))
        kinds.insert("Conversion:narrowing");
    }
    // value() of an optional or an expected throws where it holds no value:
    // open until the sum has its eliminator (#1222).
    if (const auto* m = llvm::dyn_cast<clang::CXXMemberCallExpr>(st))
      if (const clang::CXXMethodDecl* g = m->getMethodDecl(); g && g->getIdentifier() && g->getName() == "value")
        if (const clang::CXXRecordDecl* r = g->getParent(); r && inStd(r) && r->getIdentifier() &&
            (r->getName() == "optional" || r->getName() == "expected"))
          kinds.insert("Sugar:sum-value");
    // A call from a judged definition to a function not declared constexpr:
    // since C++23 the keyword on the caller promises nothing about it, so
    // the callee is looked at (std::rand, std::getenv, a stream's <<).
    const clang::FunctionDecl* callee = nullptr;
    if (const auto* c = llvm::dyn_cast<clang::CallExpr>(st)) callee = c->getDirectCallee();
    if (const auto* c = llvm::dyn_cast<clang::CXXConstructExpr>(st)) callee = c->getConstructor();
    if (callee && !callee->isConstexpr() && !callee->getBuiltinID() && !callee->isImplicit() &&
        !clang::isLambdaCallOperator(callee) && !callee->isDeleted())
      kinds.insert("Call:NotConstexpr");
    // A builtin is exempt, being the compiler's own, except those that end
    // the program or have no defined behaviour when reached: they give no
    // value at any argument (__builtin_trap, __builtin_unreachable).
    if (callee && callee->getBuiltinID() && callee->getIdentifier()) {
      static const std::set<std::string> ends{"__builtin_trap", "__builtin_debugtrap",
                                              "__builtin_verbose_trap", "__builtin_unreachable",
                                              "__builtin_abort", "abort", "__builtin_exit",
                                              "exit", "_Exit", "quick_exit"};
      if (ends.count(callee->getName().str())) kinds.insert("Call:NotConstexpr");
    }
    // Integer division and remainder by a value, and a shift by a value:
    // the kernel has them by a constant only, where they are defined at
    // every argument (Figure 2, Appendix D).
    if (const auto* o = llvm::dyn_cast<clang::BinaryOperator>(st)) {
      const auto op = o->getOpcode();
      const bool divides = op == clang::BO_Div || op == clang::BO_Rem ||
                           op == clang::BO_DivAssign || op == clang::BO_RemAssign;
      const bool shifts = op == clang::BO_Shl || op == clang::BO_Shr ||
                          op == clang::BO_ShlAssign || op == clang::BO_ShrAssign;
      const clang::Expr* r = o->getRHS();
      if ((divides || shifts) && ctx && !o->isTypeDependent() && !r->isValueDependent() &&
          o->getLHS()->getType()->isIntegerType() && r->getType()->isIntegerType()) {
        const auto v = r->getIntegerConstantExpr(*ctx);
        const unsigned width = ctx->getIntWidth(o->getLHS()->IgnoreParenImpCasts()->getType());
        if (!v || (divides && *v == 0) || (divides && o->getType()->isSignedIntegerType() && *v == -1) ||
            (shifts && (v->isNegative() || *v >= width)))
          kinds.insert(divides ? "Op:division-by-value" : "Op:shift-by-value");
      }
    }
    // std::is_constant_evaluated() and its builtin: a value that differs
    // between constant evaluation and run time, as if consteval chooses; open
    // with it until the agreement of the two branches can be checked.
    if (const auto* c = llvm::dyn_cast<clang::CallExpr>(st))
      if (const clang::FunctionDecl* g = c->getDirectCallee())
        if (g->getBuiltinID() == clang::Builtin::BI__builtin_is_constant_evaluated ||
            (g->getIdentifier() && g->getName() == "is_constant_evaluated" && inStd(g)))
          kinds.insert("Stmt:IfConsteval");
    // A floating value converted to an integer, written out in any spelling
    // (size_t(x), static_cast, a C cast): undefined where the value does not
    // fit ([conv.fpint]), so partial, and open as division by a value is.
    if (const auto* c = llvm::dyn_cast<clang::ExplicitCastExpr>(st))
      if (c->getType()->isIntegralOrEnumerationType() && !c->isTypeDependent() &&
          c->getSubExpr()->IgnoreParenImpCasts()->getType()->isRealFloatingType())
        kinds.insert("Op:float-to-integer");
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
    if (const auto* r = llvm::dyn_cast<clang::ReturnStmt>(st))
      if (!tailReturns.count(r)) kinds.insert("Stmt:EarlyReturn");
    if (const auto* c = llvm::dyn_cast<clang::ImplicitCastExpr>(st))
      if (narrows(c)) kinds.insert("Conversion:narrowing");
    if (!llvm::isa<clang::ImplicitCastExpr, clang::MaterializeTemporaryExpr,
                   clang::ExprWithCleanups, clang::CXXBindTemporaryExpr,
                   clang::ConstantExpr, clang::CXXDefaultArgExpr,
                   clang::CXXDefaultInitExpr, clang::OpaqueValueExpr,
                   clang::SubstNonTypeTemplateParmExpr>(st))
      kinds.insert(stmtKind(st));
    return true;
  }
  bool VisitTypeLoc(clang::TypeLoc tl) {
    if (!tl.isNull()) {
      kinds.insert(typeKind(tl));
      // The kernel's sum is the standard's variant of two types (#1222); a
      // variant of more is read as nested binary ones, sugar not
      // yet written.  std::expected is a sum with an API of its own: open.
      if (const auto* ts = tl.getTypePtr()->getAs<clang::TemplateSpecializationType>())
        if (const clang::TemplateDecl* td = ts->getTemplateName().getAsTemplateDecl())
          if (td->getIdentifier() && inStd(td)) {
            // Counted by its alternatives, a pack expanded, not by the
            // arguments written: variant<Ts..., bool> may have more than
            // two, and an unexpanded pack leaves it open (Copilot on #1226).
            if (td->getName() == "variant" && alternatives(tl.getType()).size() != 2)
              kinds.insert("Sugar:variant");
            if (td->getName() == "expected")
              kinds.insert(expectedVoid(tl.getType()) ? "Sum:expected-void" : "Sum:expected");
          }
    }
    return true;
  }
};

/// Not a rule: every declaration directly in the scope namespace, with its
/// lines and the syntax kinds it uses, one line each, so that a script can
/// give each definition its level (kernel or sugar, by admission.txt)
/// and join it with the findings that fall in its lines.
class Definitions : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    const clang::SourceManager& sm = ctx.getSourceManager();
    clang::PrintingPolicy policy = ctx.getPrintingPolicy();
    policy.SuppressScope = true;
    std::function<void(clang::Decl*)> visit;
    std::function<void(const clang::DeclContext*)> walk =
        [&](const clang::DeclContext* dc) {
          for (clang::Decl* d : dc->decls()) visit(d);
        };
    visit = [&](clang::Decl* d) {
          {
            const auto* ns = llvm::dyn_cast<clang::NamespaceDecl>(d);
            if (ns && ns->getName() == scope &&
                sm.isInMainFile(sm.getExpansionLoc(ns->getLocation()))) {
              // Its declarations, and those of the namespaces nested in it
              // (detail and the like), as the second stage judges them.
              std::function<void(const clang::DeclContext*)> members =
                  [&](const clang::DeclContext* c) {
                    for (clang::Decl* m : c->decls()) {
                      if (llvm::isa<clang::NamespaceDecl, clang::ExportDecl,
                                    clang::LinkageSpecDecl>(m) &&
                          sm.isInMainFile(sm.getExpansionLoc(m->getLocation())))
                        members(llvm::cast<clang::DeclContext>(m));
                      else
                        report(m, sm, policy);
                    }
                  };
              members(ns);
              return;
            }
            if (const auto* c = llvm::dyn_cast<clang::DeclContext>(d))
              if (llvm::isa<clang::NamespaceDecl, clang::LinkageSpecDecl,
                            clang::ExportDecl>(d))
                walk(c);
          }
        };
    // The traversal scope: the whole unit, or the file's own declarations.
    for (clang::Decl* d : ctx.getTraversalScope())
      if (const auto* tu = llvm::dyn_cast<clang::TranslationUnitDecl>(d))
        walk(tu);
      else
        visit(d);
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
    // A declaration that defines nothing is not a definition: a prototype
    // whose body is elsewhere, a forward declaration of a class.  A deleted
    // or defaulted function says what it is, and is one.
    const clang::Decl* inner = d;
    if (const auto* t = llvm::dyn_cast<clang::TemplateDecl>(d))
      if (t->getTemplatedDecl()) inner = t->getTemplatedDecl();
    if (const auto* f = llvm::dyn_cast<clang::FunctionDecl>(inner))
      if (!f->doesThisDeclarationHaveABody() && !f->isDeleted() && !f->isDefaulted()) return;
    if (const auto* r = llvm::dyn_cast<clang::TagDecl>(inner))
      if (!r->isThisDeclarationADefinition()) return;
    std::string name;
    llvm::raw_string_ostream n(name);
    nd->getNameForDiagnostic(n, policy, false);
    const clang::SourceLocation b = sm.getExpansionLoc(d->getBeginLoc()),
                                e = sm.getExpansionLoc(d->getEndLoc());
    if (!sm.isInMainFile(b)) return;
    Kinds k;
    k.ctx = &d->getASTContext();
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

/// \sugar{local state: an assignment, an element's update, std::swap} Local
/// state that the state-passing reading of the table covers: every
/// mutable local of a judged function is reached through its own name only,
/// so that a write through one name never changes what another reads, and
/// the function is a fold of states (the paper's aliasing conditions; Cytron
/// et al. 1991, 3.1.1, for an element's update as the whole array's).
/// Every use of a mutable local is checked against the positions the rows
/// read (an allowlist, Judge::allowed): read or copied; the left side of a
/// write; the base of a field or an element; an argument bound to a copy or
/// a const reference; an operand of std::swap, disjoint, in a statement of
/// its own; the argument of std::move at its last use (Judge::lastUse).  Any other use is a finding: a call that may write it, a
/// mutating member call, its address.
/// Refused besides, a finding each:
///   - a reference local whose initialiser reaches a mutable local (a second
///     name for its storage; a reference to a parameter, a constant or a
///     temporary is no alias of local state), and a local of a type that is
///     not a deep value type (a view, a pointer, a tuple of references)
///     initialised from local state;
///   - an operator[] on a mutable local std::array whose index is not shown
///     to lie in [0, N - 1]: an interval over literals, the counters of
///     counted loops, const locals, and scalar locals over all the values
///     they are assigned, with + - and *;
///   - a write (an assignment, ++, --) that is not a statement of its own,
///     the for increment aside: two writes in one expression, a write in a
///     condition or an argument;
///   - a reference local bound to the reference a call returns, where the
///     call's object or an argument is a temporary (its lifetime is the
///     temporary's: const auto& r = make_box().get(); dangles);
///   - a return, in a const member function returning a reference, of a
///     mutable field or of an element of one (the condition of the const
///     accessor's reading, which the rule's matcher cannot see);
///   - a lambda capturing a mutable local by reference;
///   - a mutable local with no initialiser that writes it, read before it is
///     written perhaps (none, or a trivial default construction).
class LocalState : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    const clang::SourceManager& sm = ctx.getSourceManager();
    struct Fns : clang::RecursiveASTVisitor<Fns> {
      const clang::SourceManager* sm;
      std::vector<const clang::FunctionDecl*> found;
      bool VisitFunctionDecl(clang::FunctionDecl* f) {
        if (f->doesThisDeclarationHaveABody() && judged(f, *sm)) found.push_back(f);
        return true;
      }
    } fns;
    fns.sm = &sm;
    fns.TraverseAST(ctx);
    std::set<std::tuple<std::string, unsigned, unsigned>> lines;
    auto report = [&](clang::SourceLocation l) {
      l = sm.getExpansionLoc(l);
      lines.emplace(sm.getFilename(l).str(), sm.getExpansionLineNumber(l),
                    sm.getExpansionColumnNumber(l));
    };
    for (const clang::FunctionDecl* f : fns.found) check(f, ctx, report);
    for (const auto& [file, line, column] : lines)
      out() << file << ':' << line << ':' << column << '\n';
  }

 private:
  static bool mutableLocal(const clang::VarDecl* v) {
    return v && v->hasLocalStorage() && !v->getType()->isReferenceType() &&
           !v->getType().isConstQualified();
  }
  /// The variable an lvalue reaches through members and elements, if any.
  static const clang::VarDecl* base(const clang::Expr* e) {
    for (int k = 0; e && k < 32; ++k) {
      e = e->IgnoreParenImpCasts();
      if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e))
        return llvm::dyn_cast<clang::VarDecl>(d->getDecl());
      if (const auto* m = llvm::dyn_cast<clang::MemberExpr>(e)) { e = m->getBase(); continue; }
      if (const auto* a = llvm::dyn_cast<clang::ArraySubscriptExpr>(e)) { e = a->getBase(); continue; }
      if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(e))
        if (o->getOperator() == clang::OO_Subscript) { e = o->getArg(0); continue; }
      // *it and it-> reach what the iterator points at, not the iterator's own
      // storage: follow the iterator to where it came from, its initialiser
      // (r.begin() reaches r, p.support().rbegin() reaches p), and stop where
      // that is not known.
      const clang::Expr* through = nullptr;  // the iterator or pointer dereferenced
      if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(e))
        if (o->getOperator() == clang::OO_Star || o->getOperator() == clang::OO_Arrow)
          through = o->getArg(0);
      if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e))
        if (u->getOpcode() == clang::UO_Deref) through = u->getSubExpr();  // a pointer
      if (through) {
          const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(through->IgnoreParenImpCasts());
          const auto* it = d ? llvm::dyn_cast<clang::VarDecl>(d->getDecl()) : nullptr;
          if (!it || !it->getInit()) return nullptr;
          e = it->getInit();
          continue;
        }
      if (const auto* m = llvm::dyn_cast<clang::CXXMemberCallExpr>(e)) {
        // a.begin(), a.data(), p.support(): what the call is on.
        e = m->getImplicitObjectArgument();
        continue;
      }
      if (const auto* m = llvm::dyn_cast<clang::MaterializeTemporaryExpr>(e)) { e = m->getSubExpr(); continue; }
      return nullptr;
    }
    return nullptr;
  }
  /// Whether an expression mentions a mutable local anywhere.
  static bool mentionsMutable(const clang::Expr* e) {
    struct Find : clang::RecursiveASTVisitor<Find> {
      bool found = false;
      bool VisitDeclRefExpr(clang::DeclRefExpr* d) {
        if (mutableLocal(llvm::dyn_cast<clang::VarDecl>(d->getDecl()))) found = true;
        return !found;
      }
    } f;
    f.TraverseStmt(const_cast<clang::Expr*>(e));
    return f.found;
  }
  /// A second name for local state: where the source is traced, a mutable
  /// local; where it is not, any initialiser that mentions one (a whitelist
  /// admits only what it traces).
  static bool aliasesLocalState(const clang::Expr* init) {
    if (const clang::VarDecl* b = base(init)) return mutableLocal(b);
    return mentionsMutable(init);
  }
  /// A standard sequence whose size its type does not give: an index of it
  /// is never shown to lie in range.
  static bool dynamicSequence(clang::QualType t) {
    t = t.getNonReferenceType().getCanonicalType();
    const auto* r = t->getAsCXXRecordDecl();
    if (!r || !inStd(r)) return false;
    const llvm::StringRef n = r->getName();
    if (n == "vector" || n == "basic_string" || n == "deque") return true;
    if (n == "span")
      if (const auto* spec = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(r))
        if (spec->getTemplateArgs().size() >= 2 &&
            spec->getTemplateArgs()[1].getKind() == clang::TemplateArgument::Integral)
          return spec->getTemplateArgs()[1].getAsIntegral().isAllOnes();  // dynamic_extent
    return false;
  }
  /// A deep value type: a copy shares no storage with what it was copied
  /// from.  Atoms, arrays, the standard's value types (array, pair, tuple,
  /// optional, variant, vector, string, bitset) of deep value types, and
  /// classes whose bases and fields are; no reference, no pointer, no view.
  /// A type that depends on a template parameter is judged at its
  /// archetypes, not here.
  static bool deepValue(clang::QualType t, std::set<const clang::Type*>& seen) {
    t = t.getCanonicalType().getUnqualifiedType();
    if (t->isDependentType()) return true;
    if (t->isReferenceType() || t->isAnyPointerType() || t->isMemberPointerType())
      return false;
    if (t->isArithmeticType() || t->isEnumeralType() || t->isNullPtrType()) return true;
    if (const auto* a = llvm::dyn_cast<clang::ConstantArrayType>(t.getTypePtr()))
      return deepValue(a->getElementType(), seen);
    const auto* r = t->getAsCXXRecordDecl();
    if (!r || !r->hasDefinition()) return false;
    if (!seen.insert(t.getTypePtr()).second) return true;
    if (inStd(r)) {
      static const std::set<std::string> values{
          "array", "pair", "tuple", "optional", "variant", "vector",
          "basic_string", "bitset", "allocator", "char_traits", "monostate",
          "expected", "unexpected", "map", "set", "multimap", "multiset",
          "unordered_map", "unordered_set", "complex", "duration", "ratio",
          "less", "greater", "equal_to", "hash"};
      if (!values.count(r->getName().str())) return false;
      const auto* spec = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(r);
      if (!spec) return true;
      std::function<bool(const clang::TemplateArgument&)> deep =
          [&](const clang::TemplateArgument& a) {
            if (a.getKind() == clang::TemplateArgument::Type) return deepValue(a.getAsType(), seen);
            if (a.getKind() == clang::TemplateArgument::Pack)
              return std::all_of(a.pack_begin(), a.pack_end(), deep);
            return true;
          };
      const auto args = spec->getTemplateArgs().asArray();
      return std::all_of(args.begin(), args.end(), deep);
    }
    for (const clang::CXXBaseSpecifier& b : r->bases())
      if (!deepValue(b.getType(), seen)) return false;
    for (const clang::FieldDecl* f : r->fields())
      if (!deepValue(f->getType(), seen)) return false;
    return true;
  }
  static bool deepValue(clang::QualType t) {
    std::set<const clang::Type*> seen;
    return deepValue(t, seen);
  }
  /// The path an argument of swap names: a variable, then fields and
  /// constant indices ("?" for an index not constant); none if not a path.
  static std::optional<std::pair<const clang::VarDecl*, std::vector<std::string>>>
  path(const clang::Expr* e, const clang::ASTContext& ctx) {
    std::vector<std::string> steps;
    auto index = [&](const clang::Expr* i) -> std::string {
      if (!i->isValueDependent() && i->getType()->isIntegralOrEnumerationType())
        if (auto v = i->getIntegerConstantExpr(ctx)) return "#" + llvm::toString(*v, 10);
      return "?";
    };
    for (int k = 0; e && k < 32; ++k) {
      e = e->IgnoreParenImpCasts();
      if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e)) {
        const auto* v = llvm::dyn_cast<clang::VarDecl>(d->getDecl());
        if (!v) return std::nullopt;
        std::reverse(steps.begin(), steps.end());
        return std::pair{v, steps};
      }
      if (const auto* m = llvm::dyn_cast<clang::MemberExpr>(e)) {
        if (!llvm::isa<clang::FieldDecl>(m->getMemberDecl())) return std::nullopt;
        steps.push_back("." + m->getMemberDecl()->getNameAsString());
        e = m->getBase();
        continue;
      }
      if (const auto* a = llvm::dyn_cast<clang::ArraySubscriptExpr>(e)) {
        steps.push_back(index(a->getIdx()));
        e = a->getBase();
        continue;
      }
      if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(e))
        if (o->getOperator() == clang::OO_Subscript && o->getNumArgs() == 2) {
          steps.push_back(index(o->getArg(1)));
          e = o->getArg(0);
          continue;
        }
      return std::nullopt;
    }
    return std::nullopt;
  }
  /// The two operands of std::swap are evidently disjoint: different
  /// variables, or paths that part at a field or a constant index before
  /// either ends.  The same scalar twice is fine (swap of a value with
  /// itself is the identity); the same array or record is not
  /// (swap_ranges' precondition, [alg.swap]).
  static bool disjoint(const clang::CallExpr* c, const clang::ASTContext& ctx) {
    auto p = path(c->getArg(0), ctx), q = path(c->getArg(1), ctx);
    if (!p || !q) return false;
    if (p->first != q->first) return true;
    const auto& a = p->second;
    const auto& b = q->second;
    for (std::size_t i = 0; i < std::min(a.size(), b.size()); ++i) {
      if (a[i] == "?" || b[i] == "?") return false;
      if (a[i] != b[i]) return true;
    }
    return a.size() == b.size() && c->getArg(0)->getType()->isScalarType();
  }
  /// The size of a std::span of static extent, if it is one.
  static std::optional<std::uint64_t> spanSize(clang::QualType t) {
    t = t.getNonReferenceType().getCanonicalType();
    const auto* spec = llvm::dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(t->getAsCXXRecordDecl());
    if (!spec || spec->getName() != "span" || !inStd(spec) || spec->getTemplateArgs().size() < 2 ||
        spec->getTemplateArgs()[1].getKind() != clang::TemplateArgument::Integral ||
        spec->getTemplateArgs()[1].getAsIntegral().isAllOnes())
      return std::nullopt;
    return spec->getTemplateArgs()[1].getAsIntegral().getZExtValue();
  }
  /// The size N of a std::array type, if known.
  static std::optional<std::uint64_t> arraySize(clang::QualType t) {
    t = t.getNonReferenceType().getCanonicalType();
    const auto* spec = llvm::dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(
        t->getAsCXXRecordDecl());
    if (!spec || spec->getName() != "array" || !inStd(spec) ||
        spec->getTemplateArgs().size() < 2 ||
        spec->getTemplateArgs()[1].getKind() != clang::TemplateArgument::Integral)
      return std::nullopt;
    return spec->getTemplateArgs()[1].getAsIntegral().getZExtValue();
  }
  using Interval = std::pair<long long, long long>;
  struct Values {
    // Per scalar local: the expressions it is ever given (its initialiser,
    // every plain assignment); a compound write makes it unbounded.
    std::map<const clang::VarDecl*, std::vector<const clang::Expr*>> given;
    std::set<const clang::VarDecl*> unbounded;
    // Per counter of a counted for: its initialiser and its bound.
    std::map<const clang::VarDecl*, std::pair<const clang::Expr*, const clang::Expr*>> counters;
    const clang::ASTContext* ctx = nullptr;
  };
  static std::optional<Interval> interval(const clang::Expr* e, const Values& vs,
                                          std::set<const clang::VarDecl*>& seen,
                                          int depth = 0) {
    if (!e || depth > 32) return std::nullopt;
    e = e->IgnoreParenImpCasts();
    // A constant expression has its value: a literal, an enumerator
    // (p[ArrowLaw::Monotone]), a constexpr variable.
    if (vs.ctx && !e->isValueDependent() && e->getType()->isIntegralOrEnumerationType())
      if (auto v = e->getIntegerConstantExpr(*vs.ctx))
        if (v->getSignificantBits() <= 62) {
          const long long x = v->getExtValue();
          return Interval{x, x};
        }
    if (const auto* i = llvm::dyn_cast<clang::IntegerLiteral>(e)) {
      if (!i->getValue().isIntN(62)) return std::nullopt;
      const long long v = static_cast<long long>(i->getValue().getZExtValue());
      return Interval{v, v};
    }
    if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
      auto l = interval(b->getLHS(), vs, seen, depth + 1);
      auto r = interval(b->getRHS(), vs, seen, depth + 1);
      if (!l || !r) return std::nullopt;
      const auto [a, c] = *l;
      const auto [d, g] = *r;
      constexpr long long cap = 1ll << 40;
      if (std::max({std::llabs(a), std::llabs(c), std::llabs(d), std::llabs(g)}) > cap)
        return std::nullopt;
      switch (b->getOpcode()) {
        case clang::BO_Add: return Interval{a + d, c + g};
        case clang::BO_Sub: return Interval{a - g, c - d};
        case clang::BO_Mul: {
          long long x[4];
          if (__builtin_mul_overflow(a, d, &x[0]) || __builtin_mul_overflow(a, g, &x[1]) ||
              __builtin_mul_overflow(c, d, &x[2]) || __builtin_mul_overflow(c, g, &x[3]))
            return std::nullopt;  // not representable: not known
          return Interval{*std::min_element(x, x + 4), *std::max_element(x, x + 4)};
        }
        default: return std::nullopt;
      }
    }
    if (const auto* m = llvm::dyn_cast<clang::CXXMemberCallExpr>(e))
      if (m->getMethodDecl() && m->getMethodDecl()->getName() == "size")
        if (auto n = arraySize(m->getImplicitObjectArgument()->getType()))
          return Interval{static_cast<long long>(*n), static_cast<long long>(*n)};
    const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(e);
    const auto* v = d ? llvm::dyn_cast<clang::VarDecl>(d->getDecl()) : nullptr;
    if (!v || !seen.insert(v).second) return std::nullopt;  // a cycle: unbounded
    std::optional<Interval> r;
    if (auto c = vs.counters.find(v); c != vs.counters.end()) {
      auto lo = interval(c->second.first, vs, seen, depth + 1);
      auto hi = interval(c->second.second, vs, seen, depth + 1);
      if (lo && hi && hi->second - 1 >= lo->first) r = Interval{lo->first, hi->second - 1};
    } else if (!vs.unbounded.count(v)) {
      if (auto g = vs.given.find(v); g != vs.given.end() && !g->second.empty()) {
        for (const clang::Expr* x : g->second) {
          auto i = interval(x, vs, seen, depth + 1);
          if (!i) { r.reset(); break; }
          r = r ? Interval{std::min(r->first, i->first), std::max(r->second, i->second)} : *i;
          if (x == g->second.back()) break;
        }
      }
      // The values must fit the variable's type, or they wrap
      // (signed char k = 200 holds -56).
      if (r && vs.ctx && v->getType()->isIntegerType()) {
        const unsigned w = vs.ctx->getIntWidth(v->getType());
        const bool sgn = v->getType()->isSignedIntegerType();
        const long long lo = sgn ? (w >= 63 ? LLONG_MIN : -(1ll << (w - 1))) : 0;
        const long long hi = w >= 63 ? LLONG_MAX : (sgn ? (1ll << (w - 1)) - 1 : (1ll << w) - 1);
        if (r->first < lo || r->second > hi) r.reset();
      }
    }
    seen.erase(v);
    return r;
  }
  static bool isWrite(const clang::Stmt* s) {
    if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(s)) return b->isAssignmentOp();
    if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(s)) return u->isIncrementDecrementOp();
    if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(s))
      return o->isAssignmentOp() || o->getOperator() == clang::OO_PlusPlus ||
             o->getOperator() == clang::OO_MinusMinus;
    return false;
  }
  template <typename Report>
  void check(const clang::FunctionDecl* f, clang::ASTContext& ctx, Report report) {
    Values vs;
    vs.ctx = &ctx;
    // The facts a first walk gathers: the values scalar locals are given,
    // the counted loops' counters.
    struct Gather : clang::RecursiveASTVisitor<Gather> {
      Values* vs;
      bool VisitVarDecl(clang::VarDecl* v) {
        if (v->hasLocalStorage() && v->getType()->isIntegerType()) {
          if (v->getInit()) vs->given[v].push_back(v->getInit());
          else if (!llvm::isa<clang::ParmVarDecl>(v)) vs->unbounded.insert(v);
          if (llvm::isa<clang::ParmVarDecl>(v)) vs->unbounded.insert(v);
        }
        return true;
      }
      bool VisitBinaryOperator(clang::BinaryOperator* b) {
        if (!b->isAssignmentOp()) return true;
        if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(b->getLHS()->IgnoreParenImpCasts()))
          if (const auto* v = llvm::dyn_cast<clang::VarDecl>(d->getDecl())) {
            if (b->getOpcode() == clang::BO_Assign) vs->given[v].push_back(b->getRHS());
            else vs->unbounded.insert(v);
          }
        return true;
      }
      bool VisitUnaryOperator(clang::UnaryOperator* u) {
        if (u->getOpcode() == clang::UO_AddrOf)
          if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(u->getSubExpr()->IgnoreParenImpCasts()))
            if (const auto* v = llvm::dyn_cast<clang::VarDecl>(d->getDecl())) vs->unbounded.insert(v);
        if (!u->isIncrementDecrementOp()) return true;
        if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(u->getSubExpr()->IgnoreParenImpCasts()))
          if (const auto* v = llvm::dyn_cast<clang::VarDecl>(d->getDecl())) vs->unbounded.insert(v);
        return true;
      }
      // Written another way, by a non-const reference argument (std::swap)
      // or through its address: no interval.
      bool VisitCallExpr(clang::CallExpr* c) {
        const clang::FunctionDecl* g = c->getDirectCallee();
        for (unsigned i = 0; i < c->getNumArgs(); ++i) {
          const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(c->getArg(i)->IgnoreParenImpCasts());
          const auto* v = d ? llvm::dyn_cast<clang::VarDecl>(d->getDecl()) : nullptr;
          if (!v) continue;
          const clang::ParmVarDecl* p = g && i < g->getNumParams() ? g->getParamDecl(i) : nullptr;
          if (!p || (p->getType()->isReferenceType() &&
                     !p->getType().getNonReferenceType().isConstQualified()))
            vs->unbounded.insert(v);
        }
        return true;
      }
      bool VisitForStmt(clang::ForStmt* s) {
        const auto* init = llvm::dyn_cast_or_null<clang::DeclStmt>(s->getInit());
        const auto* cond = llvm::dyn_cast_or_null<clang::BinaryOperator>(s->getCond());
        if (!init || !init->isSingleDecl() || !cond || cond->getOpcode() != clang::BO_LT)
          return true;
        const auto* v = llvm::dyn_cast<clang::VarDecl>(init->getSingleDecl());
        const auto* lhs = llvm::dyn_cast<clang::DeclRefExpr>(cond->getLHS()->IgnoreParenImpCasts());
        if (v && v->getInit() && lhs && lhs->getDecl() == v)
          vs->counters[v] = {v->getInit(), cond->getRHS()};
        return true;
      }
    } gather;
    gather.vs = &vs;
    gather.TraverseStmt(f->getBody());
    // A counter takes its interval from its loop, not from its ++.
    for (const auto& [v, _] : vs.counters) vs.unbounded.erase(v);

    struct Judge : clang::RecursiveASTVisitor<Judge> {
      clang::ASTContext* ctx;
      Values* vs;
      Report* report;
      const clang::FunctionDecl* fn = nullptr;
      /// \sugar{std::move of a local at its last use} std::move(x), x a
      /// local variable or a by-value parameter of the function judged, is
      /// read as x, a copy, where nothing can observe what the move leaves
      /// in x (#1223): no use of x follows the move in source order in the
      /// function, no lambda after it captures x, and the move is in no loop
      /// and no lambda, and the move's own full-expression mentions x only
      /// in the move.  Without goto, which the posture has not, and with
      /// loops excluded, source order bounds the order of evaluation between
      /// full-expressions, so a use in an earlier one cannot run after the
      /// move; within one it does not ([intro.execution]/10: the operands of
      /// + and the arguments of a call are unsequenced or indeterminately
      /// sequenced), so f(x, std::move(x)) is refused; a reference
      /// to x that could outlive the move is refused on its own (a reference
      /// local, a reference capture).  The condition is syntactic and
      /// strict: any later mention refuses, even one that cannot run, and a
      /// move written in a macro refuses.  Then what consumes the move is
      /// judged as a use of x would be: a copy or move construction reads,
      /// a write through it (std::move(s)[0] = v on a span) does not.
      /// Fixture: local-state.cpp.
      bool lastUse(const clang::VarDecl* x, const clang::CallExpr* move) {
        if (!fn || !fn->getBody() || x->getType()->isReferenceType() || !x->hasLocalStorage() ||
            x->getParentFunctionOrMethod() != fn)
          return false;
        // A move written in a macro: an expansion's tokens share one
        // expansion location, so source order inside it is not seen.
        if (move->getBeginLoc().isMacroID() || move->getEndLoc().isMacroID()) return false;
        // Up to the function, without a bound (a parent chain is finite):
        // a walk that stopped short could miss an enclosing loop.
        clang::DynTypedNode n = clang::DynTypedNode::create(*move);
        bool reached = false;
        for (;;) {
          auto ps = ctx->getParents(n);
          if (ps.empty()) break;
          n = ps[0];
          if (n.get<clang::ForStmt>() || n.get<clang::CXXForRangeStmt>() || n.get<clang::WhileStmt>() ||
              n.get<clang::DoStmt>() || n.get<clang::LambdaExpr>())
            return false;
          if (n.get<clang::FunctionDecl>()) {
            reached = true;
            break;
          }
        }
        if (!reached) return false;
        // Mentions in unevaluated operands (a type, as in decltype(x);
        // sizeof, alignof, noexcept) cannot observe the move and are not
        // counted; typeid may evaluate its operand and is.
        struct Mentions : clang::RecursiveASTVisitor<Mentions> {
          const clang::VarDecl* x;
          int n = 0;
          bool TraverseTypeLoc(clang::TypeLoc, bool = true) { return true; }
          bool TraverseUnaryExprOrTypeTraitExpr(clang::UnaryExprOrTypeTraitExpr*) { return true; }
          bool TraverseCXXNoexceptExpr(clang::CXXNoexceptExpr*) { return true; }
          bool VisitDeclRefExpr(clang::DeclRefExpr* d) {
            if (d->getDecl() == x) ++n;
            return true;
          }
        };
        const auto count = [x](const clang::Stmt* s) {
          Mentions m;
          m.x = x;
          m.TraverseStmt(const_cast<clang::Stmt*>(s));
          return m.n;
        };
        // The move's full-expression, the outermost expression above it;
        // on the way up, the operands sequenced before the move's side are
        // set aside: the left of &&, || and the comma ([expr.log.and]/2,
        // [expr.log.or]/2, [expr.comma]/1), the condition of ?:
        // ([expr.cond]/1).  A mention there runs before the move.
        const clang::Stmt* full = move;
        int before = 0;
        for (;;) {
          auto ps = ctx->getParents(*full);
          const auto* up = ps.empty() ? nullptr : ps[0].get<clang::Expr>();
          if (!up) break;
          if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(up);
              b && (b->isLogicalOp() || b->isCommaOp()) && b->getRHS() == full)
            before += count(b->getLHS());
          else if (const auto* c = llvm::dyn_cast<clang::ConditionalOperator>(up); c && c->getCond() != full)
            before += count(c->getCond());
          full = up;
        }
        if (count(full) - before > 1) return false;
        struct Later : clang::RecursiveASTVisitor<Later> {
          const clang::VarDecl* x;
          clang::SourceLocation end;
          const clang::SourceManager* sm;
          bool found = false;
          bool after(clang::SourceLocation l) const {
            return l.isValid() && sm->isBeforeInTranslationUnit(end, sm->getExpansionLoc(l));
          }
          bool VisitDeclRefExpr(clang::DeclRefExpr* d) {
            if (d->getDecl() == x && after(d->getBeginLoc())) found = true;
            return !found;
          }
          bool VisitLambdaExpr(clang::LambdaExpr* l) {
            for (const clang::LambdaCapture& c : l->captures())
              if (c.capturesVariable() && c.getCapturedVar() == x && after(l->getBeginLoc())) found = true;
            return !found;
          }
        } later;
        later.x = x;
        later.sm = &ctx->getSourceManager();
        later.end = later.sm->getExpansionLoc(move->getEndLoc());
        later.TraverseStmt(fn->getBody());
        return !later.found;
      }
      /// The node above e, wrappers aside.
      const clang::DynTypedNode* above(const clang::Expr* e, clang::DynTypedNode& keep) {
        auto ps = ctx->getParents(*e);
        if (ps.empty()) return nullptr;
        keep = ps[0];
        return &keep;
      }
      /// A call or write that stands as a statement of its own.
      bool standsAlone(const clang::Expr* s) {
        clang::DynTypedNode n = clang::DynTypedNode::create(*s);
        for (int k = 0; k < 8; ++k) {
          auto ps = ctx->getParents(n);
          if (ps.empty()) return false;
          n = ps[0];
          if (const auto* e = n.get<clang::Expr>()) {
            if (llvm::isa<clang::ExprWithCleanups, clang::ParenExpr, clang::ImplicitCastExpr>(e))
              continue;
            if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e))
              if (b->getOpcode() == clang::BO_Comma) continue;
            return false;
          }
          return n.get<clang::Stmt>() != nullptr;
        }
        return false;
      }
      /// An expression statement: in a block, a label, a case, or the unbraced
      /// body of an if or a loop, wrappers and the comma operator aside; not
      /// a return's operand, a condition or a loop's header (Copilot on #1239).
      bool statementPosition(const clang::Expr* s) {
        const clang::Stmt* child = s;
        clang::DynTypedNode n = clang::DynTypedNode::create(*s);
        for (int k = 0; k < 16; ++k) {
          auto ps = ctx->getParents(n);
          if (ps.empty()) return false;
          n = ps[0];
          const clang::Stmt* p = n.get<clang::Stmt>();
          if (!p) return false;
          if (llvm::isa<clang::ExprWithCleanups, clang::ParenExpr>(p)) { child = p; continue; }
          if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(p); b && b->getOpcode() == clang::BO_Comma) {
            child = p;
            continue;
          }
          if (llvm::isa<clang::CompoundStmt, clang::LabelStmt, clang::DefaultStmt>(p)) return true;
          if (const auto* c = llvm::dyn_cast<clang::CaseStmt>(p)) return c->getSubStmt() == child;
          if (const auto* i = llvm::dyn_cast<clang::IfStmt>(p)) return child == i->getThen() || child == i->getElse();
          if (const auto* f = llvm::dyn_cast<clang::ForStmt>(p)) return child == f->getBody();
          if (const auto* f = llvm::dyn_cast<clang::CXXForRangeStmt>(p)) return child == f->getBody();
          if (const auto* w = llvm::dyn_cast<clang::WhileStmt>(p)) return child == w->getBody();
          if (const auto* d = llvm::dyn_cast<clang::DoStmt>(p)) return child == d->getBody();
          return false;
        }
        return false;
      }
      /// Inside an operand that is not evaluated (sizeof, decltype, noexcept,
      /// a requires-expression): no use at all.
      bool unevaluated(const clang::Expr* e) {
        clang::DynTypedNode n = clang::DynTypedNode::create(*e);
        for (int k = 0; k < 64; ++k) {
          auto ps = ctx->getParents(n);
          if (ps.empty()) return false;
          n = ps[0];
          if (n.get<clang::UnaryExprOrTypeTraitExpr>() || n.get<clang::CXXNoexceptExpr>() ||
              n.get<clang::RequiresExpr>() || n.get<clang::CXXTypeidExpr>() ||
              n.get<clang::TypeLoc>() || n.get<clang::Decl>())
            return !n.get<clang::Decl>();
        }
        return false;
      }
      /// A function of the standard that only reads or copies what it is
      /// given, though its signature takes a forwarding reference (a closed
      /// list): the kernel's iteration is written with the first two.
      /// By qualified name, since a bare name is not enough: views::iota
      /// reads, std::ranges::iota and std::iota write their range, and
      /// std::transform writes its output.
      static bool readerDecl(const clang::NamedDecl* named) {
        if (!named || !named->getIdentifier() || !inStd(named)) return false;
        const std::string q = named->getQualifiedNameAsString();
        const llvm::StringRef n = named->getName();
        if (q.find("views::") != std::string::npos)
          return n == "iota" || n == "repeat" || n == "transform";
        const bool ranges = q.find("ranges::") != std::string::npos;
        if (n == "fold_left") return ranges;
        if (ranges) return false;
        // visit hands the alternative held to its visitor, which in Jlt
        // takes it by value (Rule 1).
        return n == "make_pair" || n == "make_tuple" || n == "make_optional" || n == "visit";
      }
      /// The declarations a callee expression names, where it names any: a
      /// function or function object, or the candidates of a lookup not yet
      /// resolved (a call with a dependent argument).
      static std::vector<const clang::NamedDecl*> namedBy(const clang::Expr* callee) {
        callee = callee->IgnoreParenImpCasts();
        if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(callee)) return {d->getDecl()};
        if (const auto* u = llvm::dyn_cast<clang::UnresolvedLookupExpr>(callee))
          return {u->decls_begin(), u->decls_end()};
        return {};
      }
      /// A reader whose result keeps a reference to a range given as an
      /// lvalue: views::transform stores a ref_view of it.  iota and repeat
      /// copy what they are given.
      static bool borrows(const clang::NamedDecl* named) {
        return named && named->getIdentifier() && inStd(named) && named->getName() == "transform" &&
               named->getQualifiedNameAsString().find("views::") != std::string::npos;
      }
      static const clang::NamedDecl* readerNamed(const clang::CallExpr* c) {
        const clang::NamedDecl* named = c->getDirectCallee();
        if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(c))
          if (o->getOperator() == clang::OO_Call && o->getNumArgs() > 0)
            if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(o->getArg(0)->IgnoreParenImpCasts()))
              named = llvm::dyn_cast<clang::VarDecl>(d->getDecl());
        return named;
      }
      /// Whether the value of a call leaves the full expression it is in, as
      /// a view still referring to a local: returned, or kept in a variable,
      /// through further views of the standard only.  Anything else on the
      /// way consumes it there: a fold, a function of the program (which
      /// takes it by value, Rule 1), a comparison.
      bool escapes(const clang::Expr* call) const {
        clang::DynTypedNode n = clang::DynTypedNode::create(*call);
        for (int k = 0; k < 32; ++k) {
          auto ps = ctx->getParents(n);
          if (ps.empty()) return false;
          n = ps[0];
          if (n.get<clang::ReturnStmt>() || n.get<clang::VarDecl>() || n.get<clang::DeclStmt>()) return true;
          const auto* e = n.get<clang::Expr>();
          if (!e) return false;
          if (llvm::isa<clang::ImplicitCastExpr, clang::ParenExpr, clang::MaterializeTemporaryExpr,
                        clang::CXXBindTemporaryExpr, clang::ExprWithCleanups, clang::CXXConstructExpr,
                        clang::InitListExpr, clang::CXXFunctionalCastExpr>(e))
            continue;
          if (const auto* c = llvm::dyn_cast<clang::CallExpr>(e)) {
            // A view of a view, views::take(v, 2) or v | views::take(2).
            std::vector<const clang::NamedDecl*> ds = namedBy(c->getCallee());
            if (const clang::NamedDecl* g = c->getDirectCallee()) ds.push_back(g);
            if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(c);
                o && o->getOperator() == clang::OO_Call && o->getNumArgs() > 0)
              ds = namedBy(o->getArg(0));
            const bool view = !ds.empty() && std::all_of(ds.begin(), ds.end(), [](const clang::NamedDecl* d) {
              if (!inStd(d)) return false;
              const std::string q = d->getQualifiedNameAsString();
              const llvm::StringRef n = d->getIdentifier() ? d->getName() : "";
              // A pair, tuple or optional made of the view keeps it.
              if (n == "make_pair" || n == "make_tuple" || n == "make_optional") return true;
              return q.find("views::") != std::string::npos ||
                     (d->getIdentifier() == nullptr && q.find("ranges") != std::string::npos);
            });
            if (view) continue;
            // An operator other than a call or a pipe gives a new value (==, +).
            if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(c);
                o && o->getOperator() != clang::OO_Call && o->getOperator() != clang::OO_Pipe)
              return false;
            // A function of the program whose result is of class or
            // dependent type may return the view it is given (keep(v));
            // one that returns a scalar consumes it (first_element(v)).
            const bool standard = std::any_of(ds.begin(), ds.end(), [](const clang::NamedDecl* d) {
              return inStd(d);
            });
            const clang::QualType t = c->getType();
            if (!standard && (t->isDependentType() || t->isRecordType())) continue;
          }
          return false;
        }
        return false;
      }
      static bool reader(const clang::CallExpr* c) {
        const clang::NamedDecl* named = c->getDirectCallee();
        // A function object of the standard, views::iota(a, b): its name.
        if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(c))
          if (o->getOperator() == clang::OO_Call && o->getNumArgs() > 0)
            if (const auto* d = llvm::dyn_cast<clang::DeclRefExpr>(o->getArg(0)->IgnoreParenImpCasts()))
              named = llvm::dyn_cast<clang::VarDecl>(d->getDecl());
        return readerDecl(named);
      }
      /// How an argument binds: 0 a parameter that may write it (or one not
      /// known), 1 a copy or a const reference, 2 a const reference the
      /// call's result may be (std::max), to be followed.
      static int binds(const clang::FunctionDecl* g, unsigned i, const clang::Expr* call) {
        if (!g || i >= g->getNumParams()) return 0;
        const clang::QualType p = g->getParamDecl(i)->getType();
        if (!p->isReferenceType()) return 1;
        if (p->isRValueReferenceType() || !p.getNonReferenceType().isConstQualified()) return 0;
        return call->isGLValue() ? 2 : 1;
      }
      /// A write to v, an assignment on its left: the value written shares
      /// no storage with local state, unless v is a deep value type.
      /// And a write through it, to an element or a field, is a write to
      /// the local's own storage only if it is a deep value type: an element
      /// of a span parameter is its caller's.
      bool writable(const clang::VarDecl* v, const clang::Expr* rhs, bool through) {
        if (deepValue(v->getType())) return true;
        return !through && (!rhs || !mentionsMutable(rhs));
      }
      /// \sugar{a container's update on a local} A mutating call of a
      /// standard sequence or bitset on a local, v.push_back(e), b.set(i)
      /// (no emplace form, whose element construction may throw: Copilot on
      /// #1239):
      /// the assignment of the container updated, v = with(v, e),
      /// under that row's conditions.  A closed list of methods of a closed
      /// list of containers, each giving the container its standard
      /// specifies, none taking a position; a statement of its own; no
      /// argument a pointer, so that none reaches into the container
      /// written; a bitset's
      /// position of a type whose values all lie in range; no pop_back or
      /// pop_front, undefined on an empty container, and no resize, which
      /// throws past max_size(); the
      /// arguments, as an assignment's right side, sharing no storage with
      /// local state; through a field or an element only on a deep value.
      bool update(const clang::VarDecl* v, const clang::CXXMemberCallExpr* call, bool through) {
        static const std::set<std::string> containers{
            "vector", "deque", "list", "forward_list", "basic_string", "bitset"};
        static const std::set<std::string> updates{
            "push_back", "push_front", "clear", "set", "reset", "flip"};
        const clang::CXXMethodDecl* g = call->getMethodDecl();
        const clang::CXXRecordDecl* r = g ? g->getParent() : nullptr;
        if (!g || !r || !g->getIdentifier() || !updates.count(g->getName().str()) || !inStd(r) ||
            !r->getIdentifier() || !containers.count(r->getName().str()) || !statementPosition(call))
          return false;
        const std::string of = r->getName().str();
        // A bitset's position: out of range it throws, so its type's values
        // all lie in range, bitset<256> at an unsigned char.
        if (of == "bitset" && call->getNumArgs() > 0) {
          const auto* spec = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(r);
          const clang::QualType pt = call->getArg(0)->IgnoreParenImpCasts()->getType();
          if (!spec || spec->getTemplateArgs().size() < 1 ||
              spec->getTemplateArgs()[0].getKind() != clang::TemplateArgument::Integral ||
              !pt->isUnsignedIntegerType() || ctx->getIntWidth(pt) >= 64 ||
              (1ull << ctx->getIntWidth(pt)) > spec->getTemplateArgs()[0].getAsIntegral().getZExtValue())
            return false;
        }
        for (const clang::Expr* a : call->arguments()) {
          const clang::QualType at = a->getType().getNonReferenceType();
          if (at->isPointerType() || at->isArrayType()) return false;
          if (!writable(v, a, through)) return false;
        }
        return call->getNumArgs() > 0 || writable(v, nullptr, through);
      }
      /// The update above in a template, h.push_back(e) with h of a standard
      /// sequence's type as written (std::vector<N>): the same methods,
      /// none of a bitset, whose position's range only an instance gives.
      bool dependentUpdate(const clang::VarDecl* v, const clang::CXXDependentScopeMemberExpr* m,
                           const clang::CallExpr* call, bool through) {
        static const std::set<std::string> sequences{"vector", "deque", "list", "forward_list", "basic_string"};
        static const std::set<std::string> updates{"push_back", "push_front", "clear"};
        if (m->isArrow() || !m->getMember().isIdentifier() || !updates.count(m->getMember().getAsString()) ||
            !statementPosition(call))
          return false;
        const clang::QualType bt = m->getBaseType().getNonReferenceType();
        const auto* ts = bt->getAs<clang::TemplateSpecializationType>();
        const clang::TemplateDecl* td = ts ? ts->getTemplateName().getAsTemplateDecl() : nullptr;
        if (!td || !td->getIdentifier() || !inStd(td) || !sequences.count(td->getName().str())) return false;
        for (const clang::Expr* a : call->arguments()) {
          const clang::QualType at = a->getType().getNonReferenceType();
          if (at->isPointerType() || at->isArrayType()) return false;
          if (!writable(v, a, through)) return false;
        }
        return call->getNumArgs() > 0 || writable(v, nullptr, through);
      }
      /// Whether an occurrence of a mutable local stands where the rows of
      /// Table 7 read it (an allowlist; the theoretician's positions a-e):
      ///   a. read (an lvalue-to-rvalue conversion), or copied;
      ///   b. the left side of an assignment, ++ or -- (that it is a
      ///      statement of its own is judged by VisitStmt);
      ///   c. the base of a field, an element ([], front, back, value, at,
      ///      *, std::get) whose result is again judged by a-e;
      ///   d. an argument bound to a by-value or const reference parameter,
      ///      or the object of a const member call;
      ///   e. an operand of std::swap, in a statement of its own, the two
      ///      evidently disjoint;
      ///   f. the argument of std::move at the local's last use (lastUse).
      ///   g. the object of a standard container's update, in a statement
      ///      of its own (update).
      /// Anything else is a finding: a non-const reference parameter, a
      /// mutating member call, the address.  A dependent call, its callee
      /// known only at instantiation, is read as binding by value, which
      /// Rule 1 gives for a callee in Jlt.
      bool allowed(const clang::DeclRefExpr* d) {
        const auto* var = llvm::cast<clang::VarDecl>(d->getDecl());
        const clang::Expr* cur = d;
        bool chosen = false;  // reached through ?: or a comma: no single path to write
        clang::DynTypedNode keep;
        for (int k = 0; k < 32; ++k) {
          const clang::DynTypedNode* p = above(cur, keep);
          if (!p) return true;
          const auto* e = p->get<clang::Expr>();
          if (!e) return true;  // a statement (returned, tested, discarded) or a declaration
          if (llvm::isa<clang::ParenExpr, clang::ConstantExpr, clang::ExprWithCleanups,
                        clang::MaterializeTemporaryExpr, clang::CXXBindTemporaryExpr,
                        clang::ParenListExpr>(e)) {
            cur = e;
            continue;
          }
          if (const auto* c = llvm::dyn_cast<clang::ImplicitCastExpr>(e)) {
            switch (c->getCastKind()) {
              case clang::CK_LValueToRValue: return true;  // a
              case clang::CK_ArrayToPointerDecay: return false;
              case clang::CK_NoOp:
              case clang::CK_DerivedToBase:
              case clang::CK_UncheckedDerivedToBase: cur = e; continue;
              default: return true;
            }
          }
          if (const auto* c = llvm::dyn_cast<clang::ExplicitCastExpr>(e)) {
            if (c->isGLValue()) { cur = e; continue; }
            return true;
          }
          if (const auto* m = llvm::dyn_cast<clang::MemberExpr>(e)) {
            if (m->getBase() != cur) return true;
            if (llvm::isa<clang::FieldDecl>(m->getMemberDecl())) { cur = e; continue; }  // c
            const auto* method = llvm::dyn_cast<clang::CXXMethodDecl>(m->getMemberDecl());
            clang::DynTypedNode up;
            const clang::DynTypedNode* q = above(m, up);
            const auto* call = q ? q->get<clang::CXXMemberCallExpr>() : nullptr;
            if (!method || !call) return false;
            static const std::set<std::string> accessors{"front", "back", "value", "at", "error"};
            if (method->isConst() || method->isStatic() ||
                (method->getIdentifier() && accessors.count(method->getName().str()))) {
              if (call->isGLValue()) { cur = call; continue; }  // c
              return true;  // d
            }
            return !chosen && update(var, call, cur != d);  // g
          }
          if (const auto* m = llvm::dyn_cast<clang::CXXDependentScopeMemberExpr>(e)) {
            clang::DynTypedNode up;
            const clang::DynTypedNode* q = above(m, up);
            const auto* call = q ? q->get<clang::CallExpr>() : nullptr;
            // Called, and not an argument of the call (op(s.first, s.second)).
            if (call && call->getCallee()->IgnoreParens() != m) call = nullptr;
            if (!call) { cur = e; continue; }  // a field, presumably: c
            // A method of a class template, looked up in its pattern and its
            // partial specializations: const in every one (v.size() of a
            // std::vector<T>), or an accessor, is a read; else not known.
            switch (dependentMethod(m)) {
              case 1: return true;
              case 2:
                if (call->isGLValue() || call->isTypeDependent()) { cur = call; continue; }
                return true;
              default: return !chosen && dependentUpdate(var, m, call, cur != d);  // g
            }
          }
          if (llvm::isa<clang::UnresolvedMemberExpr>(e)) return false;
          // T(x) at a type not yet known: a constructor of a type in Jlt
          // takes values (Rule 1, as for a dependent call below).
          if (llvm::isa<clang::CXXUnresolvedConstructExpr>(e)) return true;
          if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(e)) {
            unsigned i = 0;
            while (i < o->getNumArgs() && o->getArg(i) != cur) ++i;
            const auto op = o->getOperator();
            if (i == o->getNumArgs()) return !o->getDirectCallee();  // dependent: below
            const bool write = o->isAssignmentOp() || op == clang::OO_PlusPlus ||
                               op == clang::OO_MinusMinus;
            if (i == 0 && write)  // b
              return !chosen && writable(var, o->getNumArgs() > 1 ? o->getArg(1) : nullptr, cur != d);
            if (i == 0 && (op == clang::OO_Subscript || op == clang::OO_Star ||
                           op == clang::OO_Arrow)) {  // c
              cur = e;
              continue;
            }
            const auto* g = o->getDirectCallee();
            // Dependent, the callee known only at instantiation: a callee in
            // Jlt cannot write its argument, since Rule 1 refuses a non-const
            // reference parameter and a call operator that is not const
            // (rule1-lvalue-reference-params, rule1-rvalue-reference-params,
            // rule1-const-call-operator), and a template is judged for its
            // instances in Jlt.
            if (!g) {
              // ...unless what is called is a function object of the
              // standard, whose behaviour is known: the closed list decides.
              if (op == clang::OO_Call && o->getNumArgs() > 0)
                if (const auto ds = namedBy(o->getArg(0)); !ds.empty() && inStd(ds[0]))
                  return i > 0 && readerDecl(ds[0]) && !(borrows(ds[0]) && escapes(o));
              return op != clang::OO_Amp;
            }
            if (reader(o)) return !(borrows(readerNamed(o)) && escapes(o));
            int b;
            if (const auto* method = llvm::dyn_cast<clang::CXXMethodDecl>(g); method && !method->isStatic())
              b = i == 0 ? (method->isConst() ? (o->isGLValue() ? 2 : 1) : 0)
                         : binds(g, i - 1, o);
            else
              b = binds(g, i, o);
            if (b == 2) { cur = e; continue; }
            return b == 1;
          }
          if (const auto* c = llvm::dyn_cast<clang::CallExpr>(e)) {
            unsigned i = 0;
            while (i < c->getNumArgs() && c->getArg(i) != cur) ++i;
            const clang::FunctionDecl* g = c->getDirectCallee();
            // Dependent: as for a dependent operator call above.
            if (!g && (c->getCallee()->isTypeDependent() ||
                       std::any_of(c->arg_begin(), c->arg_end(), [](const clang::Expr* a) {
                         return a->isTypeDependent();
                       }))) {
              // A callee of the standard is not a callee in Jlt: its
              // behaviour is known, and the closed list decides
              // (std::ranges::sort and std::tie in a template write as
              // they do outside one).
              const auto ds = namedBy(c->getCallee());
              const bool standard = std::any_of(ds.begin(), ds.end(), [](const clang::NamedDecl* d) {
                return inStd(d);
              });
              if (!standard) return true;
              const auto named = [&](llvm::StringRef n) {
                return std::all_of(ds.begin(), ds.end(), [&](const clang::NamedDecl* d) {
                  return d->getIdentifier() && d->getName() == n && inStd(d);
                });
              };
              if (i < c->getNumArgs() && named("swap") && c->getNumArgs() == 2)
                return disjoint(c, *ctx) && standsAlone(c);
              if (i < c->getNumArgs() && named("get")) { cur = e; continue; }
              if (i == 0 && named("move") && c->getNumArgs() == 1 && cur == d) {
                if (!lastUse(var, c)) return false;
                cur = e;  // what consumes the move is judged as a use of x
                continue;
              }
              return i < c->getNumArgs() && std::all_of(ds.begin(), ds.end(), readerDecl) &&
                     !(std::any_of(ds.begin(), ds.end(), borrows) && escapes(c));
            }
            if (i == c->getNumArgs()) return false;  // called itself
            if (g && inStd(g) && g->getIdentifier()) {
              if (g->getName() == "swap" && c->getNumArgs() == 2)  // e
                return disjoint(c, *ctx) && standsAlone(c);
              if (g->getName() == "get" && c->isGLValue()) { cur = e; continue; }  // c
              if (g->getName() == "move" && c->getNumArgs() == 1 && cur == d) {  // f
                if (!lastUse(var, c)) return false;
                cur = e;  // what consumes the move is judged as a use of x
                continue;
              }
            }
            if (reader(c)) return !(borrows(readerNamed(c)) && escapes(c));
            const int b = binds(g, i, c);
            if (b == 2) { cur = e; continue; }
            return b == 1;
          }
          if (const auto* c = llvm::dyn_cast<clang::CXXConstructExpr>(e)) {
            unsigned i = 0;
            while (i < c->getNumArgs() && c->getArg(i) != cur) ++i;
            if (i == c->getNumArgs()) return false;
            // A copy, or the move of a returned local (return a;): a.
            if (c->getConstructor()->isCopyOrMoveConstructor()) return true;
            // A value type of the standard copies what it is built from
            // (std::optional{s.first}, by a forwarding reference).
            if (const auto* r = c->getType()->getAsCXXRecordDecl())
              if (inStd(r) && deepValue(c->getType())) return true;
            return binds(c->getConstructor(), i, c) != 0;
          }
          if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e)) {
            if (b->isAssignmentOp() && b->getLHS() == cur)  // b
              return !chosen && writable(var, b->getRHS(), cur != d);
            // The comma's value is its right operand: (0, x) is x.
            if (b->getOpcode() == clang::BO_Comma && b->getRHS() == cur) { chosen = true; cur = e; continue; }
            return true;
          }
          if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(e)) {
            if (u->getOpcode() == clang::UO_AddrOf) return false;
            if (u->getOpcode() == clang::UO_Deref) { cur = e; continue; }
            if (u->isIncrementDecrementOp()) return !chosen && writable(var, nullptr, cur != d);  // b
            return true;
          }
          if (llvm::isa<clang::AbstractConditionalOperator>(e)) {
            // (c ? i : j) = 9 writes one of two variables: read only.
            if (e->isGLValue()) { chosen = true; cur = e; continue; }
            return true;
          }
          if (const auto* a = llvm::dyn_cast<clang::ArraySubscriptExpr>(e)) {
            if (a->getBase() == cur) { cur = e; continue; }
            return true;
          }
          // A value built from it holds a copy, unless a field of it is a
          // reference: Ref{k}.x = 7 writes k.
          if (const auto* il = llvm::dyn_cast<clang::InitListExpr>(e)) {
            // The list as written has no type of its own (return {a, b};
            // R{x} at a type parameter): judged where it has one.
            const clang::QualType t = il->getType();
            return t.isNull() || t->isVoidType() || t->isDependentType() || deepValue(t);
          }
          if (llvm::isa<clang::LambdaExpr, clang::CXXFoldExpr,
                        clang::PackExpansionExpr, clang::UnaryExprOrTypeTraitExpr,
                        clang::CXXNoexceptExpr, clang::CXXTypeidExpr,
                        clang::CXXStdInitializerListExpr>(e))
            return true;
          return false;
        }
        return false;
      }
      /// A method named on an object of a dependent class template
      /// specialization: 1 if every method of that name in the template's
      /// pattern and partial specializations is const or static, 2 if it is
      /// an accessor (front, back, value, at), 0 if not known.
      static int dependentMethod(const clang::CXXDependentScopeMemberExpr* m) {
        if (m->getBaseType().isNull()) return 0;
        const clang::QualType base = m->isArrow()
            ? m->getBaseType()->getPointeeType() : m->getBaseType();
        if (base.isNull()) return 0;
        const auto* spec = base->getAs<clang::TemplateSpecializationType>();
        const auto* td = spec ? llvm::dyn_cast_or_null<clang::ClassTemplateDecl>(
                                    spec->getTemplateName().getAsTemplateDecl())
                              : nullptr;
        if (!td || !m->getMember().isIdentifier()) return 0;
        const llvm::StringRef name = m->getMember().getAsIdentifierInfo()->getName();
        if (name == "front" || name == "back" || name == "value" || name == "at" || name == "error") return 2;
        std::vector<const clang::CXXRecordDecl*> patterns{td->getTemplatedDecl()};
        llvm::SmallVector<clang::ClassTemplatePartialSpecializationDecl*, 4> partial;
        const_cast<clang::ClassTemplateDecl*>(td)->getPartialSpecializations(partial);
        patterns.insert(patterns.end(), partial.begin(), partial.end());
        bool found = false;
        for (const clang::CXXRecordDecl* r : patterns) {
          if (!r || !r->hasDefinition()) return 0;
          for (const clang::Decl* d : r->getDefinition()->decls()) {
            const clang::Decl* f = d;
            if (const auto* t = llvm::dyn_cast<clang::FunctionTemplateDecl>(d)) f = t->getTemplatedDecl();
            const auto* method = llvm::dyn_cast<clang::CXXMethodDecl>(f);
            if (!method || !method->getIdentifier() || method->getName() != name) continue;
            found = true;
            if (!method->isConst() && !method->isStatic()) return 0;
          }
        }
        return found ? 1 : 0;
      }
      bool VisitDeclRefExpr(clang::DeclRefExpr* d) {
        const auto* v = llvm::dyn_cast<clang::VarDecl>(d->getDecl());
        if (!mutableLocal(v) || llvm::isa<clang::RequiresExprBodyDecl>(v->getDeclContext()))
          return true;
        if (!allowed(d) && !unevaluated(d)) (*report)(d->getLocation());
        return true;
      }
      bool VisitVarDecl(clang::VarDecl* v) {
        if (v->hasLocalStorage() && v->getType()->isReferenceType() && v->getInit())
          if (aliasesLocalState(v->getInit())) (*report)(v->getLocation());
        // A reference local bound to the reference a call returns, the
        // call's object or an argument a temporary: its lifetime is that
        // temporary's, which ends with the full-expression (const auto& r = make_box().get(); dangles, since a
        // temporary's life is extended only when the reference binds to it
        // directly, [class.temporary]).  A temporary bound directly, T{...}
        // or a call returning by value, is no such reference.  A range-for's
        // loop variable and a parameter's default argument aside.
        if (v->hasLocalStorage() && !llvm::isa<clang::ParmVarDecl>(v) && !v->isCXXForRangeDecl() &&
            v->getType()->isReferenceType() && v->getInit()) {
          const clang::Expr* e = v->getInit();
          for (;;) {
            if (const auto* w = llvm::dyn_cast<clang::ExprWithCleanups>(e)) e = w->getSubExpr();
            else if (const auto* p = llvm::dyn_cast<clang::ParenExpr>(e)) e = p->getSubExpr();
            else if (const auto* c = llvm::dyn_cast<clang::ImplicitCastExpr>(e);
                     c && c->getCastKind() == clang::CK_NoOp)
              e = c->getSubExpr();
            else break;
          }
          // Only where the call's object or an argument is a temporary can
          // the reference outlive what it names: arr[i] on a local or
          // std::get<0>(t) on a parameter is a second name, which the alias
          // check judges, and no dangling.
          // A temporary of class type, or one materialised to bind a
          // reference parameter: an index or a count passed by value, a
          // prvalue of an atom, binds nothing and cannot dangle.
          // The storage a reference names, followed through fields (not
          // through ->) and elements of built-in arrays, on the initialiser
          // and on a call's object: make_outer().get().v and
          // make_outer().h.get() dangle, make_outer().h.v is extended.  An
          // argument counts only where it binds a reference parameter: one
          // passed by value is a copy the result cannot name.
          const auto strip = [](const clang::Expr* a) {
            for (int k = 0; a && k < 16; ++k) {
              if (const auto* w = llvm::dyn_cast<clang::ExprWithCleanups>(a)) a = w->getSubExpr();
              else if (const auto* c = llvm::dyn_cast<clang::ImplicitCastExpr>(a);
                       c && (c->getCastKind() == clang::CK_NoOp || c->getCastKind() == clang::CK_DerivedToBase ||
                             c->getCastKind() == clang::CK_UncheckedDerivedToBase))
                a = c->getSubExpr();
              else if (const auto* q = llvm::dyn_cast<clang::ParenExpr>(a)) a = q->getSubExpr();
              else break;
            }
            return a;
          };
          const auto subobject = [&](const clang::Expr* a) -> const clang::Expr* {
            // A non-static field only: a static member is its own storage,
            // and ends the path (Holder{}.h.get() with h static is safe).
            if (const auto* m = llvm::dyn_cast<clang::MemberExpr>(a);
                m && !m->isArrow() && llvm::isa<clang::FieldDecl>(m->getMemberDecl()))
              return m->getBase();
            if (const auto* x = llvm::dyn_cast<clang::ArraySubscriptExpr>(a);
                x && x->getBase()->IgnoreParenImpCasts()->getType()->isArrayType())
              return x->getBase()->IgnoreParenImpCasts();
            return nullptr;
          };
          std::function<bool(const clang::Expr*)> temporary, fromTemporary;
          // A call returning a reference whose object, or an argument bound
          // to a reference parameter, is a temporary.
          fromTemporary = [&](const clang::Expr* e) {
            const auto* call = llvm::dyn_cast<clang::CallExpr>(e);
            if (!call || !e->isGLValue()) return false;
            const clang::FunctionDecl* g = call->getDirectCallee();
            unsigned shift = 0;
            if (const auto* mc = llvm::dyn_cast<clang::CXXMemberCallExpr>(call)) {
              if (temporary(mc->getImplicitObjectArgument())) return true;
            } else if (const auto* oc = llvm::dyn_cast<clang::CXXOperatorCallExpr>(call);
                       oc && g && llvm::isa<clang::CXXMethodDecl>(g) &&
                       !llvm::cast<clang::CXXMethodDecl>(g)->isStatic()) {
              if (oc->getNumArgs() > 0 && temporary(oc->getArg(0))) return true;
              shift = 1;
            }
            for (unsigned i = shift; i < call->getNumArgs(); ++i) {
              const unsigned j = i - shift;
              const bool byReference = !g || j >= g->getNumParams() ||
                                       g->getParamDecl(j)->getType()->isReferenceType();
              if (byReference && temporary(call->getArg(i))) return true;
            }
            return false;
          };
          temporary = [&](const clang::Expr* a) {
            for (int k = 0; a && k < 16; ++k) {
              a = strip(a);
              if (llvm::isa<clang::MaterializeTemporaryExpr, clang::CXXBindTemporaryExpr>(a)) return true;
              // Either arm of ?:, the right of a comma: what the result names.
              if (const auto* c = llvm::dyn_cast<clang::ConditionalOperator>(a))
                return temporary(c->getTrueExpr()) || temporary(c->getFalseExpr());
              if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(a); b && b->isCommaOp()) {
                a = b->getRHS();
                continue;
              }
              if (const auto* sub = subobject(a)) { a = sub; continue; }
              if (fromTemporary(a)) return true;
              return a->isPRValue() && a->getType()->isRecordType();
            }
            return false;
          };
          // The initialiser, down its fields and elements, both arms of ?:
          // and the right of a comma, to a call.
          std::function<bool(const clang::Expr*)> dangles = [&](const clang::Expr* a) {
            for (int k = 0; a && k < 16; ++k) {
              a = strip(a);
              if (const auto* c = llvm::dyn_cast<clang::ConditionalOperator>(a))
                return dangles(c->getTrueExpr()) || dangles(c->getFalseExpr());
              if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(a); b && b->isCommaOp()) {
                a = b->getRHS();
                continue;
              }
              if (const auto* sub = subobject(a)) {
                a = sub;
                continue;
              }
              return fromTemporary(a);
            }
            return false;
          };
          if (dangles(e)) (*report)(v->getLocation());
        }
        // A local whose copies share storage (a view, a pointer, a tuple of
        // references) initialised from local state: a second name for it.
        if (v->hasLocalStorage() && !llvm::isa<clang::ParmVarDecl>(v) &&
            !v->getType()->isReferenceType() && v->getInit() && !v->isImplicit() &&
            !deepValue(v->getType()) && mentionsMutable(v->getInit()))
          (*report)(v->getLocation());
        // Written before it is read: a mutable local has an initialiser that
        // writes it, not none and not a trivial default construction, which
        // leaves scalar elements indeterminate (std::array<T, N> r;).  At
        // compile time C++ refuses to read such a value, at run time the read
        // is undefined: the two evaluations would part.
        // A range-for's loop variable is written by its loop at every step
        // (in a template over a dependent range Clang records no initialiser).
        const auto* loop = [&]() -> const clang::CXXForRangeStmt* {
          for (const auto& n : ctx->getParents(*v))
            if (const auto* d = n.get<clang::DeclStmt>())
              for (const auto& m : ctx->getParents(*d))
                if (const auto* f = m.get<clang::CXXForRangeStmt>())
                  if (f->getLoopVariable() == v) return f;
          return nullptr;
        }();
        if (mutableLocal(v) && !llvm::isa<clang::ParmVarDecl>(v) && !loop) {
          const auto* c = llvm::dyn_cast_or_null<clang::CXXConstructExpr>(
              v->getInit() ? v->getInit()->IgnoreImplicit() : nullptr);
          // Inside a template a default construction is recorded only at
          // instantiation: a class template other than std::array is
          // initialised by its constructor; a bare type parameter may be a
          // scalar, so it stays refused.
          const clang::QualType t = v->getType();
          const auto* spec = t->getAs<clang::TemplateSpecializationType>();
          const auto* td = spec ? spec->getTemplateName().getAsTemplateDecl() : nullptr;
          // Of a class template outside std: only one that writes every
          // field, by a constructor of its own or by default member
          // initialisers (box<T> { T v; } leaves v indeterminate at
          // T = size_t).
          auto initialises = [](const clang::TemplateDecl* td) {
            const auto* c = llvm::dyn_cast<clang::ClassTemplateDecl>(td);
            const clang::CXXRecordDecl* r = c ? c->getTemplatedDecl() : nullptr;
            if (!r || !r->hasDefinition()) return false;
            r = r->getDefinition();
            for (const clang::CXXConstructorDecl* k : r->ctors())
              if (k->isDefaultConstructor() && k->isUserProvided()) return true;
            return std::all_of(r->field_begin(), r->field_end(),
                               [](const clang::FieldDecl* f) { return f->hasInClassInitializer(); });
          };
          const bool classTemplate =
              t->isDependentType() && td &&
              llvm::isa<clang::ClassTemplateDecl>(td) &&
              !(td->getName() == "array" && inStd(td)) &&
              (inStd(td) || initialises(td));
          if ((!v->getInit() && !classTemplate) ||
              (c && c->getConstructor()->isDefaultConstructor() &&
               c->getConstructor()->isTrivial()))
            (*report)(v->getLocation());
        }
        return true;
      }
      bool VisitCXXForRangeStmt(clang::CXXForRangeStmt* s) {
        const clang::VarDecl* x = s->getLoopVariable();
        if (x && x->getType()->isReferenceType() && aliasesLocalState(s->getRangeInit()))
          (*report)(x->getLocation());
        return true;
      }
      bool VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* o) {
        if (o->getOperator() == clang::OO_Subscript && o->getNumArgs() == 2) {
          // Every standard sequence, local or not: its index is shown in
          // range where its type gives the size (std::array, a static span),
          // never where it does not (vector, string, deque, a dynamic span).
          // The size of the sequence indexed, a.top in a.top[0].
          const clang::QualType t = o->getArg(0)->getType();
          std::optional<std::uint64_t> n = arraySize(t);
          if (!n) n = spanSize(t);
          if (n) {
            std::set<const clang::VarDecl*> seen;
            auto i = interval(o->getArg(1), *vs, seen);
            if (!i || i->first < 0 || i->second > static_cast<long long>(*n) - 1)
              (*report)(o->getExprLoc());
          } else if (const auto* r = t.getNonReferenceType()->getAsCXXRecordDecl();
                     dynamicSequence(t) || (r && inStd(r))) {
            // Any other type of the standard: its size is not in its type
            // (a vector, a string_view, a view), or its operator[] is not an
            // element access at all (a map's inserts).
            (*report)(o->getExprLoc());
          }
        }
        return true;
      }
      // A built-in array of known size: the same.
      bool VisitArraySubscriptExpr(clang::ArraySubscriptExpr* a) {
        if (const auto* at = ctx->getAsConstantArrayType(a->getBase()->IgnoreImpCasts()->getType())) {
          std::set<const clang::VarDecl*> seen;
          auto i = interval(a->getIdx(), *vs, seen);
          const long long n = static_cast<long long>(at->getSize().getZExtValue());
          if (!i || i->first < 0 || i->second > n - 1) (*report)(a->getExprLoc());
        }
        return true;
      }
      bool VisitLambdaExpr(clang::LambdaExpr* l) {
        for (const clang::LambdaCapture& c : l->captures())
          if (c.capturesVariable() && c.getCaptureKind() == clang::LCK_ByRef)
            if (const auto* v = llvm::dyn_cast<clang::VarDecl>(c.getCapturedVar()))
              if (mutableLocal(v)) (*report)(l->getBeginLoc());
        // A dependent call is read as binding by value because a callee in
        // Jlt cannot write its argument; the rules that say so exempt
        // lambdas, so a lambda is held to it here: no parameter by a
        // reference it could write through, and not mutable.
        if (const clang::CXXMethodDecl* call = l->getCallOperator()) {
          bool writes = !call->isConst();
          for (const clang::ParmVarDecl* q : call->parameters()) {
            const clang::QualType t = q->getType();
            if (t->isLValueReferenceType() && !t.getNonReferenceType().isConstQualified() &&
                !t->isDependentType())
              writes = true;
          }
          if (writes) (*report)(l->getBeginLoc());
        }
        return true;
      }
      // front() and back() of a sequence whose type does not give its size
      // are an index not shown in range, as v[0] is.
      bool VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* m) {
        const clang::CXXMethodDecl* method = m->getMethodDecl();
        if (!method || !method->getIdentifier()) return true;
        const llvm::StringRef n = method->getName();
        // at(i) of a sequence of the standard: the index shown in range as
        // for [], since out of range it throws, which Rule 1 keeps out.
        if (n == "at" && m->getNumArgs() == 1) {
          const clang::QualType t = m->getImplicitObjectArgument()->getType();
          const clang::QualType o = t->isPointerType() ? t->getPointeeType() : t;
          const auto* r = o.getNonReferenceType()->getAsCXXRecordDecl();
          if (!r || !inStd(r)) return true;
          std::optional<std::uint64_t> size = arraySize(o);
          if (!size) size = spanSize(o);
          std::set<const clang::VarDecl*> seen;
          auto i = size ? interval(m->getArg(0), *vs, seen) : std::nullopt;
          if (!size || !i || i->first < 0 || i->second > static_cast<long long>(*size) - 1)
            (*report)(m->getExprLoc());
          return true;
        }
        if (n != "front" && n != "back") return true;
        const clang::QualType t = m->getImplicitObjectArgument()->getType();
        const clang::QualType o = t->isPointerType() ? t->getPointeeType() : t;
        const auto* r = o.getNonReferenceType()->getAsCXXRecordDecl();
        std::optional<std::uint64_t> size = arraySize(o);
        if (!size) size = spanSize(o);
        if (r && inStd(r) && (!size || *size == 0)) (*report)(m->getExprLoc());
        return true;
      }
      /// The written operand of a write.
      static const clang::Expr* lhs(const clang::Stmt* s) {
        if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(s)) return b->getLHS();
        if (const auto* u = llvm::dyn_cast<clang::UnaryOperator>(s)) return u->getSubExpr();
        if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(s))
          return o->getNumArgs() ? o->getArg(0) : nullptr;
        return nullptr;
      }
      bool VisitStmt(clang::Stmt* s) {
        if (!isWrite(s)) return true;
        // A write through a variable that is not a mutable local, to an
        // element or a field: through a span or a pointer held by value,
        // const or not, it is its caller's storage (s[0] = 7 with s a
        // const std::span).  A mutable local is judged by its uses above;
        // a reference parameter by Rule 1's own rule.
        if (const clang::Expr* w = lhs(s)) {
          const clang::Expr* e = w->IgnoreParenImpCasts();
          if (!llvm::isa<clang::DeclRefExpr>(e))
            if (const clang::VarDecl* b = base(e))
              if (!mutableLocal(b) && !b->getType()->isReferenceType() && !deepValue(b->getType()))
                (*report)(s->getBeginLoc());
        }
        // A statement of its own: the parent, wrappers aside, is a statement.
        clang::DynTypedNode n = clang::DynTypedNode::create(*s);
        for (int k = 0; k < 8; ++k) {
          auto ps = ctx->getParents(n);
          if (ps.empty()) return true;
          n = ps[0];
          if (const auto* e = n.get<clang::Expr>()) {
            if (llvm::isa<clang::ExprWithCleanups, clang::ParenExpr,
                          clang::ImplicitCastExpr>(e))
              continue;
            // The comma operator sequences its operands, left before right:
            // ++i, ++it writes in order.
            if (const auto* b = llvm::dyn_cast<clang::BinaryOperator>(e))
              if (b->getOpcode() == clang::BO_Comma) continue;
            (*report)(s->getBeginLoc());  // inside another expression
            return true;
          }
          return true;  // a statement: of its own, or a for's increment
        }
        return true;
      }
    } judge;
    judge.ctx = &ctx;
    judge.vs = &vs;
    judge.report = &report;
    judge.fn = f;
    judge.TraverseStmt(f->getBody());
    // A const member function returning a reference to a mutable field, or
    // into one: the field can change under a const function, so the
    // reference is not the value (the condition of the const accessor's
    // reading, which the matcher of rule1-no-reference-returns cannot see,
    // there being no matcher for a mutable field).  Each such return.
    if (const auto* m = llvm::dyn_cast<clang::CXXMethodDecl>(f);
        m && m->isConst() && m->getReturnType()->isReferenceType()) {
      struct Returns : clang::RecursiveASTVisitor<Returns> {
        Report* report;
        static bool intoMutable(const clang::Expr* e) {
          for (int k = 0; e && k < 16; ++k) {
            e = e->IgnoreParenImpCasts();
            if (const auto* me = llvm::dyn_cast<clang::MemberExpr>(e)) {
              if (const auto* fd = llvm::dyn_cast<clang::FieldDecl>(me->getMemberDecl()); fd && fd->isMutable())
                return true;
              e = me->getBase();
            } else if (const auto* s = llvm::dyn_cast<clang::ArraySubscriptExpr>(e)) {
              e = s->getBase();
            } else if (const auto* o = llvm::dyn_cast<clang::CXXOperatorCallExpr>(e);
                       o && o->getOperator() == clang::OO_Subscript && o->getNumArgs() > 0) {
              e = o->getArg(0);
            } else {
              return false;
            }
          }
          return false;
        }
        bool VisitReturnStmt(clang::ReturnStmt* r) {
          if (r->getRetValue() && intoMutable(r->getRetValue())) (*report)(r->getBeginLoc());
          return true;
        }
        bool TraverseLambdaExpr(clang::LambdaExpr*) { return true; }  // its returns are its own
      } returns;
      returns.report = &report;
      returns.TraverseStmt(f->getBody());
    }
  }
};

/// One check by name, or null for an unknown name.
std::unique_ptr<clang::ASTConsumer> checkNamed(llvm::StringRef name) {
  if (name == "local-state") return std::make_unique<LocalState>();
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
/// Where one file's time goes, on stderr for the driver's timing table: the
/// parse (preprocessing, parsing and semantic analysis, up to the first
/// consumer), each rule's matcher and each check.
std::chrono::steady_clock::time_point actionStarted;
bool parseReported = false;
void reportPhase(const std::string& name, double seconds) {
  llvm::errs() << "jlt-lint-phase: " << name << ' '
               << llvm::format("%.2f", seconds) << '\n';
}
void reportParse() {
  if (parseReported) return;
  parseReported = true;
  reportPhase("parse", std::chrono::duration<double>(
                           std::chrono::steady_clock::now() - actionStarted)
                           .count());
}

class Section : public clang::ASTConsumer {
 public:
  Section(std::string n, std::unique_ptr<clang::ASTConsumer> c)
      : name(std::move(n)), inner(std::move(c)) {}
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    reportParse();
    std::string buffer;
    llvm::raw_string_ostream os(buffer);
    current = &os;
    const auto started = std::chrono::steady_clock::now();
    inner->HandleTranslationUnit(ctx);
    reportPhase("check " + name,
                std::chrono::duration<double>(std::chrono::steady_clock::now() -
                                              started)
                    .count());
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

/// \sugar{a const accessor} A const member function returning const T& whose
/// every return gives a field of its own object, or an element of one (a[i]
/// on a built-in array or by operator[]), reached from this, implicitly or
/// written, through fields only, is read as returning that field or element
/// by value: the storage is the object's own, lives as long as the object
/// and cannot change under a const function.  Held by narrowing the
/// deny-rule rule1-no-reference-returns (its file's header says how); a
/// reference to a parameter, a local, a static or a temporary, a non-const
/// member function and a mutable reference stay refused (#1223).  Three
/// conditions close what the reading would change (value categories): a
/// field on the path is not mutable, which rule1-local-state checks (the
/// matcher cannot see mutable); a reference local is not bound to the
/// reference a call returns, which rule1-local-state checks too, so that no
/// reference outlives the object it was called on (make_box().get() on a
/// temporary dangles in C++ and not in the reading); and the result's type
/// is not observed, by decltype or a type trait (std::is_reference_v gives
/// true in C++ and false in the reading), which the author holds (A).
/// Overloading on value category and an address are refused elsewhere
/// (rvalue and forwarding parameters, &).  Fixtures: sugar-conditions.cpp,
/// local-state.cpp.
/// Read the scope probe and the rule files into queryRules.  In a file, a
/// command is `match <matcher>`, `let <name> <matcher>` (a name for the rest
/// of the file, as in clang-query) or the first `set traversal <kind>`; a line
/// that starts with a space or a tab continues the command above it, so that
/// a matcher can be written over several lines; every other line is
/// commentary.
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
    // The file's names: the scope, and its own lets, which no other file sees.
    clang::ast_matchers::dynamic::Parser::NamedValueMap local = named;
    llvm::SmallVector<llvm::StringRef, 16> raw;
    (*buffer)->getBuffer().split(raw, '\n');
    // Commands, each with the line it starts on: an indented line continues
    // the command above it.
    std::vector<std::pair<std::string, unsigned>> commands;
    for (unsigned n = 0; n < raw.size(); ++n) {
      llvm::StringRef line = raw[n].rtrim("\r");
      const bool continues = !line.empty() && (line[0] == ' ' || line[0] == '\t');
      if (continues && line.trim().starts_with("#")) continue;  // a comment
      if (continues && !commands.empty() &&
          !llvm::StringRef(commands.back().first).starts_with("#"))
        commands.back().first += " " + line.trim().str();
      else
        commands.emplace_back(line.str(), n + 1);
    }
    for (const auto& [text, n] : commands) {
      llvm::StringRef line = text;
      const std::string where = path + ":" + std::to_string(n);
      if (line.consume_front("let ")) {
        line = line.ltrim();
        const llvm::StringRef name = line.take_while(
            [](char c) { return std::isalnum(static_cast<unsigned char>(c)) || c == '_'; });
        if (name.empty() || name == "scope") {
          llvm::errs() << "jlt-lint: " << where
                       << ": let wants a new name (not scope)\n";
          return false;
        }
        llvm::StringRef rest = line.drop_front(name.size()).ltrim();
        clang::ast_matchers::dynamic::Diagnostics diag;
        clang::ast_matchers::dynamic::VariantValue value;
        if (!clang::ast_matchers::dynamic::Parser::parseExpression(
                rest, nullptr, &local, &value, &diag)) {
          llvm::errs() << "jlt-lint: " << where << ": " << diag.toStringFull()
                       << '\n';
          return false;
        }
        if (!rest.trim().empty()) {
          llvm::errs() << "jlt-lint: " << where
                       << ": text after the value of " << name << ": "
                       << rest.trim() << '\n';
          return false;
        }
        local[name.str()] = value;
      } else if (line.consume_front("set traversal ")) {
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
        auto m = parseMatcher(line, local, where);
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
  Collect(clang::TraversalKind tk, std::string rule)
      : traversal(tk), rule(std::move(rule)) {}
  // The profile's key: matchers of one rule are summed under its name.
  llvm::StringRef getID() const override { return rule; }
  void run(const MatchFinder::MatchResult& result) override {
    found.push_back(rootLocation(result.Nodes, *result.Context));
  }
  std::optional<clang::TraversalKind> getCheckTraversalKind() const override {
    return traversal;
  }
  std::vector<std::string> found;

 private:
  clang::TraversalKind traversal;
  std::string rule;
};

/// Every rule's matchers on one finder, one traversal of the AST; each
/// rule's findings in a section, its matchers' in the order of its file,
/// as clang-query ran them one after another.
class Queries : public clang::ASTConsumer {
 public:
  void HandleTranslationUnit(clang::ASTContext& ctx) override {
    reportParse();
    // The finder's own profile: the time each rule's matchers take.
    llvm::StringMap<llvm::TimeRecord> records;
    MatchFinder::MatchFinderOptions options;
    options.CheckProfiling.emplace(records);
    MatchFinder finder(std::move(options));
    std::vector<std::vector<std::unique_ptr<Collect>>> collected;
    for (const QueryRule& rule : queryRules) {
      collected.emplace_back();
      for (const DynTypedMatcher& m : rule.matchers) {
        collected.back().push_back(std::make_unique<Collect>(
            rule.traversal, rule.name.empty() ? "scope" : rule.name));
        const std::optional<DynTypedMatcher> bound = m.tryBind("root");
        finder.addDynamicMatcher(bound ? *bound : m,
                                 collected.back().back().get());
      }
    }
    // Only what the file itself declares: the traversal, and with it the
    // parent map that hasAncestor builds, would otherwise cover every
    // declaration the file imports or includes, the standard library's and
    // the imported modules' included.  A template's instantiations hang under
    // its own declaration and stay in scope.  The scope stays set for the
    // checks that follow on the same context, which walk it (TraverseAST)
    // and build their parent maps over it, as the matchers do.
    if (scanMode == Scan::Shallow) {
      std::vector<clang::Decl*> own;
      const clang::SourceManager& sm = ctx.getSourceManager();
      for (clang::Decl* d : ctx.getTranslationUnitDecl()->noload_decls())
        if (sm.isWrittenInMainFile(sm.getExpansionLoc(d->getLocation())))
          own.push_back(d);
      ctx.setTraversalScope(own);
    }
    const auto started = std::chrono::steady_clock::now();
    finder.matchAST(ctx);
    reportPhase("matchers", std::chrono::duration<double>(
                                std::chrono::steady_clock::now() - started)
                                .count());
    for (const auto& r : records)
      reportPhase("matcher " + r.getKey().str(), r.getValue().getWallTime());
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
  bool BeginSourceFileAction(clang::CompilerInstance&) override {
    actionStarted = std::chrono::steady_clock::now();
    return true;
  }
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
