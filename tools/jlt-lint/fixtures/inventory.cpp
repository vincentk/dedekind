// Control for rules/inventory.query (the tool's inventory check): it counts
// the syntax as written, one line per kind.  The implicit conversion of the
// int parameter to the long result is not written and is not a kind; nor is
// anything outside the scope.  Seven kinds: the function, its parameter, its
// body, the return, the read of the parameter, and the function and builtin
// types as written.  Standalone.
namespace jlt_listing {

constexpr long widen(int x) { return x; }

}  // namespace jlt_listing

constexpr long outside(int x) { return x + 1; }
