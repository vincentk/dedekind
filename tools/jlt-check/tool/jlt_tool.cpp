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
#include "clang/Analysis/CallGraph.h"
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

class Action : public clang::ASTFrontendAction {
 public:
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance&, llvm::StringRef) override {
    if (check == "mutual-recursion") return std::make_unique<MutualRecursion>();
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
