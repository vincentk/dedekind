// Control for rules/axioms.query: the report must list exactly the three
// axioms inside the jlt_listing namespace, an entry at one type whose value is
// the literal true, and none of: a trait's default, an entry declared false,
// an entry at a form (a theorem, whatever its value), an entry at one type
// whose value is computed, a constant that is not an entry, or the axiom
// outside the namespace.
// Standalone, no imports, so it is checked without the build.
namespace jlt_listing {

struct Succ {};
struct Neg {};
struct Pred {};
template <typename F, typename G>
struct Then {};

template <typename F>
constexpr bool injective = false;  // a default
template <>
constexpr bool injective<Succ> = true;  // axiom 1
template <>
constexpr bool injective<Neg> = (true);  // axiom 2
template <>
constexpr bool injective<Pred> = false;  // declared not known
template <typename F, typename G>
constexpr bool injective<Then<F, G>> =
    injective<F> && injective<G>;  // a theorem

template <typename T, typename Op>
constexpr bool associative = false;
struct Plus {};
template <>
constexpr bool associative<unsigned, Plus> = true;  // axiom 3
template <typename F>
constexpr bool associative<F, Then<F, F>> = true;  // a theorem: at a form
template <>
constexpr bool associative<int, Plus> = injective<Succ>;  // computed

constexpr bool flag = true;  // not an entry

}  // namespace jlt_listing

template <typename F>
constexpr bool outside = false;
template <>
constexpr bool outside<int> = true;  // outside the namespace
