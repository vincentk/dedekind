/**
 * @file dedekind/python/nanobind_jlt.cpp
 * @brief Nanobind extension module for the Jlt arrow DSL (#961).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * A @b fluent DSL over the @b real category arrows, bound as the private native
 * extension @c dedekind._jlt (NumPy-style, re-exported by the pure-Python
 * @c dedekind.jlt facade).  It exposes the actual @c :morphism arrows over TWO
 * objects, @c bool and @c int --- the identity @c Identity<T> and the
 * type-erased @c Morphism<T,T> --- as @b duck-typed handles: an "arrow" is the
 * protocol @c __call__ / @c __rshift__ + the free functions @c dom / @c cod,
 * not a base class.  The generators are @c id and @c refl (the reflection
 * involution: @c ¬ on @c bool, negation on @c int).
 *
 * Composability is @b structural: @c __rshift__ is bound only for same-object
 * operands, so @c id(bool) @c >> @c refl(int) finds no overload and raises ---
 * the runtime image of the C++ @c operator>>'s static @c same_as<Cod,Dom>.
 * Composition type-erases to a @c Morphism (Python arrows are extensional); no
 * captured Python callable, so no refleak.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/pair.h>
#include <nanobind/stl/string_view.h>
#include <nanobind/stl/vector.h>

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

import dedekind.category; // Identity, Successor, Predecessor, the species

namespace nb = nanobind;

namespace {
using dedekind::category::Boole;
using dedekind::category::cata;
using dedekind::category::Chain;
using dedekind::category::Dom;
using dedekind::category::HasNNOStep;
using dedekind::category::Identity;
using dedekind::category::IsEndomorphism;
using dedekind::category::IsOckhamAlgebra;
using dedekind::category::logic_complement;
using dedekind::category::Morphism;
using dedekind::category::Predecessor;
using dedekind::category::Successor;

// ── Jlt: a fluent DSL over the real category arrows (#961) ────────────────
//
// The Jlt exhibit exposes the ACTUAL @c :morphism arrows over TWO objects,
// @c bool and @c int, as @b duck-typed handles: compose with @c >>, apply with
// @c (), inspect @c dom / @c cod.  There is no bespoke term class; an "arrow"
// is the @b protocol (@c dom / @c cod / call / @c >>), witnessed by @c IsArrow
// on the C++ side.
//
// The generators are @b involutive @b endomorphisms: @c id and @c refl.  @c
// refl is @c :logic's @b pre-configured reflection @c logic_complement<L> (@c
// L::RFL, already witnessed an involution there): @c ¬ on @c bool (@c Boole)
// and the order-reversing @c ~ on @c int (@c Chain<int>).  Under @c >> they
// generate
// @c ℤ/2 per object --- every element is self-inverse (@c IsInvolution).
//
// Python arrows are @b extensional (functions), so a composition type-erases to
// a @c Morphism<T,T>.  But it is NOT hand-rolled: @c compose builds @c
// :morphism's
// @c Compose (@c operator>>) and runs it through @c :f_algebra's @c cata ---
// the ONE reducer engine --- then type-erases the reduced arrow.  So a Python
// composition @b dispatches to the F-algebra reducer (@c id>>refl folds to
// @c refl); the result is extensional, but structurally reduced.  Composability
// (@c cod(f) @c = @c dom(g)) is enforced @b structurally --- a @c bool-arrow
// cannot compose with an @c int-arrow (their types do not match).

namespace jlt {
/** @brief The type-erased arrow @c T→T a Python handle holds: the real
 *  @c :morphism @c Morphism with a @c std::function transform, so a category
 *  arrow (@c IsArrow / @c IsEndomorphism hold).  @tparam T the carrier. */
template <typename T>
using Arrow = Morphism<T, T, std::function<T(T)>>;
/** @brief What a Python @c int crosses the boundary as: the canonical ring
 *  u64 = ℤ/2⁶⁴ (@c dedekind.pst), whose step wraps. */
using Int = std::uint64_t;

/** @brief @c refl on a chain: @c :logic's pre-configured reflection
 *  @c logic_complement<L> (@c L::RFL: @c ¬ on @c Boole, the order-reversing
 *  @c ~ on @c Chain<Int>), already witnessed an involution there, erased.
 *  @tparam L the species whose @c Ω is the carrier. */
template <IsOckhamAlgebra L>
Arrow<typename L::Ω> refl() {
  using T = typename L::Ω;
  return Arrow<T>{std::function<T(T)>{logic_complement<L>{}}};
}

/** @brief Composition @c f @c >> @c g (apply @c f, then @c g) of two
 * same-object endomorphisms, as a type-erased arrow.  It REUSES the real
 * machinery --- no hand-rolled @c g(f(x)): it builds @c :morphism's @c Compose
 * via @c operator>> and runs it through @c :f_algebra's @c cata (the ONE
 * reducer engine), then type-erases the reduced arrow.  So a Python composition
 * @b dispatches to the F-algebra reducer; the result is extensional (a
 * function), but structurally reduced (e.g. @c id>>refl folds to @c refl via @c
 * cata's unit law).  The
 *  @c same_as<Dom<F>,Dom<G>> constraint IS the composability law (@c cod(f) @c
 * =
 *  @c dom(g) for endomorphisms): a cross-object compose does not type-check. */
template <IsEndomorphism F, IsEndomorphism G>
  requires std::same_as<Dom<F>, Dom<G>>
Arrow<Dom<F>> compose(const F& f, const G& g) {
  using T = Dom<F>;
  return Arrow<T>{std::function<T(T)>{cata(f >> g)}};
}

}  // namespace jlt

// ── The tensor exhibit (#1107): handles to :periodic's tensors ─────────────
//
// Python holds the core's objects and asks the core for their counts; no law
// is restated here.  A tensor whose count is factored is held as its factors,
// the core's count_factors, so that the outer product, count and exact_count
// on it are the library's own (HasFactors).

namespace jlt {
using dedekind::category::count;
using dedekind::category::count_factors;
using dedekind::category::exact_count;
using dedekind::category::Factor;
using dedekind::category::free_monoid;
using dedekind::category::FreeMonoid;
using dedekind::category::LetterMap;
using dedekind::category::Letters;
using dedekind::category::power;
using dedekind::category::Power;
using dedekind::category::PowerMap;
using dedekind::category::Reverse;
using dedekind::category::Rotation;
using dedekind::category::slice;
using dedekind::category::symmetric;
using dedekind::category::with_width;

/** @brief A filtered alphabet: the byte chain between its first and last
 *  letter, under the predicate that keeps the letters. */
using Alphabet = decltype(slice(std::byte{}, std::byte{}) >> Letters{});
using WordPower = Power<std::byte, Letters, std::size_t>;
using Words = FreeMonoid<std::byte, Letters, std::size_t>;
/** @brief A letter map on every letter of a word, and that followed by the
 *  reversal of the positions: the reverse complement where the map is the
 *  complement. */
using LetterPower = PowerMap<LetterMap>;
using Word = std::vector<std::byte>;
using ReverseAfter = dedekind::category::Compose<
    LetterPower, dedekind::category::Morphism<Word, Word, Reverse>>;

/** @brief A tensor held by its factors, the core's @c count_factors of
 *  whatever it was built from. */
struct Factored {
  std::vector<Factor<>> factors;
};
/** @brief The factors of a held tensor: what makes it @c HasFactors, so the
 *  core's outer product and counts apply to it. */
std::vector<Factor<>> count_factors(const Factored& f) { return f.factors; }

/** @param a the letters, as written.  @return the alphabet. */
Alphabet alphabet(std::string_view a) {
  if (a.empty()) throw nb::value_error("alphabet: no letters");
  const auto [lo, hi] = std::ranges::minmax(a);
  return slice(std::byte{static_cast<unsigned char>(lo)},
               std::byte{static_cast<unsigned char>(hi + 1)}) >>
         dedekind::category::letters(a);
}
/** @brief The factors as Python sees them: pairs (base, exponent). */
std::vector<std::pair<std::size_t, std::size_t>> pairs(
    const std::vector<Factor<>>& fs) {
  std::vector<std::pair<std::size_t, std::size_t>> out;
  out.reserve(fs.size());
  for (const auto& f : fs) out.emplace_back(f.base, f.exponent);
  return out;
}
/** @brief A pattern packed into the bit word of width @c w and read back:
 *  the core's @c pack and @c unpack, the width dispatched at run time. */
std::size_t packed(std::size_t k, std::size_t w) {
  if (w == 0 || w > 64) throw nb::value_error("pack: width must be 1..64");
  // the low w bits: a narrower width than its word keeps no padding
  if (w < 64) k &= (std::size_t{1} << w) - 1;
  return with_width(w, [k](auto width) -> std::size_t {
    constexpr std::size_t W = decltype(width)::value;
    if constexpr (W <= 64) {
      return dedekind::category::unpack<W>(dedekind::category::pack<W>(k));
    } else {
      return k;  // not reached: the width was checked above
    }
  });
}
/** @brief Whether the pattern of @c n bits given by @c k is a square, in the
 *  narrowest word that holds it. */
bool is_square(std::size_t k, std::size_t n) {
  if (n == 0 || n > dedekind::category::max_packed_width)
    throw nb::value_error("is_square: width must be 1..512");
  return with_width(n, [k, n](auto width) {
    constexpr std::size_t W = decltype(width)::value;
    return dedekind::category::IsSquare<W>{n}(dedekind::category::pack<W>(k));
  });
}
}  // namespace jlt

/** @brief Bind the tensor exhibit's handles. */
void bind_tensors(nb::module_& m) {
  nb::class_<jlt::Alphabet>(m, "Alphabet",
                            "A filtered alphabet: the byte chain under the "
                            "predicate that keeps its letters.");
  nb::class_<jlt::WordPower>(m, "Power",
                             "The words of one length over an alphabet, a "
                             "tensor power, never built.");
  nb::class_<jlt::Words>(m, "FreeMonoid",
                         "The free monoid on an alphabet truncated below a "
                         "length: the coproduct of the tensor powers.");
  nb::class_<jlt::Reverse>(m, "Reverse", "The reversal of a word's positions.");
  nb::class_<jlt::Rotation<>>(m, "Rotation",
                              "A rotation of a word's positions.");
  nb::class_<jlt::LetterMap>(m, "LetterMap", "An arrow on the letters.");
  nb::class_<jlt::ReverseAfter>(
      m, "ReverseAfter",
      "A map on every letter, then the reversal of the positions.");
  nb::class_<jlt::LetterPower>(m, "PowerMap",
                               "A letter map on every letter of a word.")
      .def("__rshift__", [](const jlt::LetterPower& f, const jlt::Reverse& r) {
        return f >> r;
      });
  auto factored = nb::class_<jlt::Factored>(
      m, "Tensor", "A tensor held by its factors, the core's count_factors.");
  factored.def("__mul__", [](const jlt::Factored& a, const jlt::Factored& b) {
    return jlt::Factored{
        jlt::count_factors(dedekind::category::operator*(a, b))};
  });
  factored.def("__mul__", [](const jlt::Factored& a, const jlt::WordPower& b) {
    return jlt::Factored{
        jlt::count_factors(dedekind::category::operator*(a, b))};
  });

  m.def("alphabet", &jlt::alphabet, nb::arg("letters"),
        "alphabet(letters): the filtered alphabet of the letters written.");
  m.def(
      "letter_map",
      [](std::string_view from, std::string_view to) {
        if (from.size() != to.size())
          throw nb::value_error("letter_map: as many images as letters");
        return dedekind::category::letter_map(from, to);
      },
      nb::arg("from_"), nb::arg("to"),
      "letter_map(from, to): each letter to the image at the same place, every "
      "other byte fixed.");
  m.attr("reverse") = dedekind::category::reverse;
  m.def(
      "rotate", [](std::size_t d) { return dedekind::category::rotate(d); },
      nb::arg("by"), "rotate(by): the positions i -> i + by mod n.");
  m.def(
      "is_involution",
      [](const jlt::LetterMap& t) {
        return dedekind::category::is_involution(t);
      },
      nb::arg("arrow"),
      "is_involution(f): whether f twice is the identity, decided on its "
      "table, or derived from its legs.");
  m.def(
      "is_involution",
      [](const jlt::ReverseAfter& g) {
        return dedekind::category::is_involution(g);
      },
      nb::arg("arrow"));
  m.def(
      "power",
      [](const jlt::Alphabet& a, std::size_t n) { return jlt::power(a, n); },
      nb::arg("alphabet"), nb::arg("length"),
      "power(alphabet, n): the words of length n, a tensor power.");
  m.def(
      "power",
      [](const jlt::LetterMap& t, std::size_t n) { return jlt::power(t, n); },
      nb::arg("arrow"), nb::arg("length"),
      "power(f, n): the letter map f on every letter of a word of length n.");
  m.def(
      "free_monoid",
      [](const jlt::Alphabet& a, std::size_t below) {
        return jlt::free_monoid(a, below);
      },
      nb::arg("alphabet"), nb::arg("below"),
      "free_monoid(alphabet, L): the words shorter than L.");
  m.def(
      "symmetric",
      [](const jlt::WordPower& p, const jlt::Reverse& s) {
        return jlt::Factored{jlt::count_factors(jlt::symmetric(p, s))};
      },
      nb::arg("power"), nb::arg("symmetry"),
      "symmetric(power, g): the words g fixes, the legs merged along the "
      "cycles of its permutation of the positions: reverse, rotate(d), or "
      "power(complement, n) >> reverse.");
  m.def(
      "symmetric",
      [](const jlt::WordPower& p, const jlt::Rotation<>& s) {
        return jlt::Factored{jlt::count_factors(jlt::symmetric(p, s))};
      },
      nb::arg("power"), nb::arg("symmetry"));
  m.def(
      "symmetric",
      [](const jlt::WordPower& p, const jlt::ReverseAfter& g) {
        if (g.f.length != p.length)
          throw nb::value_error("symmetric: a symmetry of another length");
        return jlt::Factored{jlt::count_factors(jlt::symmetric(p, g))};
      },
      nb::arg("power"), nb::arg("symmetry"));
  m.def(
      "count_factors",
      [](const jlt::WordPower& p) { return jlt::pairs(jlt::count_factors(p)); },
      nb::arg("tensor"),
      "count_factors(t): the factors (base, exponent) of t's count, each base "
      "a popcount; exact where the count is not.");
  m.def(
      "count_factors",
      [](const jlt::Factored& f) { return jlt::pairs(f.factors); },
      nb::arg("tensor"));
  m.def(
      "count",
      [](const jlt::WordPower& p) { return jlt::count(jlt::count_factors(p)); },
      nb::arg("tensor"),
      "count(t): the count modulo 2**64, a monoid concat under * over the "
      "factors, each by squaring.");
  m.def(
      "count", [](const jlt::Factored& f) { return jlt::count(f.factors); },
      nb::arg("tensor"));
  m.def(
      "exact_count",
      [](const jlt::WordPower& p) {
        return jlt::exact_count(jlt::count_factors(p));
      },
      nb::arg("tensor"),
      "exact_count(t): the count, or None where the word cannot hold it.");
  m.def(
      "exact_count",
      [](const jlt::Factored& f) { return jlt::exact_count(f.factors); },
      nb::arg("tensor"));
  m.def(
      "exact_count", [](const jlt::Words& w) { return jlt::exact_count(w); },
      nb::arg("tensor"));
  m.def("pack", &jlt::packed, nb::arg("pattern"), nb::arg("width"),
        "pack(k, w): the pattern k in the bit word of width w, read back: "
        "its low w bits.");
  m.def("is_square", &jlt::is_square, nb::arg("pattern"), nb::arg("width"),
        "is_square(k, w): whether the w-bit pattern k is u u, fixed by the "
        "rotation by w/2.");
}

/** @brief @c dom / @c cod as a Python @b type object (numpy/pandas dtype
 * style):
 *  @c bool → the builtin @c bool, @c int → the builtin @c int. */
template <typename T>
nb::object type_object();
template <>
nb::object type_object<bool>() {
  return nb::borrow<nb::object>(reinterpret_cast<PyObject*>(&PyBool_Type));
}
template <>
nb::object type_object<jlt::Int>() {
  return nb::borrow<nb::object>(reinterpret_cast<PyObject*>(&PyLong_Type));
}

/** @brief Bind @c f @c >> @c g for one right-operand type @c G.  The C++
 *  @c operator>> itself returns a @c Compose node, one type per composition
 *  shape, which the reducer wants and Python cannot hold; so the binding goes
 *  through @c jlt::compose, which is @c operator>>, then @c cata, then the
 *  erasure to the one handle type @c Arrow<T>.  Hence a lambda, not
 *  @c nb::self @c >> @c nb::other. */
template <typename S, typename G>
void bind_rshift(nb::class_<S>& cls) {
  cls.def(
      "__rshift__", [](const S& f, const G& g) { return jlt::compose(f, g); },
      "f >> g (apply f, then g): the reducer's composition, erased to an "
      "arrow; same-object operands only.");
}

/** @brief Bind same-object composition @c >> to a bound arrow class @c S, one
 *  overload per right operand.  @c >> accepts only same-object operands, so
 *  cross-object composition raises automatically (no matching overload) ---
 *  the composability guard. */
template <typename S>
void bind_compositions(nb::class_<S>& cls) {
  using T = typename S::Domain;
  bind_rshift<S, Identity<T>>(cls);
  bind_rshift<S, jlt::Arrow<T>>(cls);
  bind_rshift<S, Successor<T>>(cls);
  bind_rshift<S, Predecessor<T>>(cls);
}

/** @brief The arrow @b operators (apply @c (), composition @c >>) of a bound
 *  arrow class @c S. */
template <typename S>
void bind_endo_operators(nb::class_<S>& cls) {
  using T = typename S::Domain;
  cls.def(
      "__call__", [](const S& f, T x) { return f(x); },
      "Apply the arrow to an object of its carrier.");
  bind_compositions(cls);
}

/** @brief The arrow operators of a typed step @c S: @c __call__ through
 *  the arrow applied: total on @c bool (saturating) and on the word (wrapping).
 *  endo-arrow. */
template <typename S>
void bind_step_operators(nb::class_<S>& cls) {
  using T = typename S::Domain;
  cls.def(
      "__call__", [](const S& f, T x) { return f(x); },
      "Apply the step: saturating on bool, wrapping on the word u64.");
  bind_compositions(cls);
}

/** @brief Bind the step arrows on carrier @c T as their own classes, typed:
 *  the real @c :nno @c Successor / @c Predecessor, composable like @c id, and
 *  read by @c dedekind.pst.image. */
template <typename T>
void bind_steps(nb::module_& m, const char* succ_name, const char* pred_name) {
  auto succ = nb::class_<Successor<T>>(
      m, succ_name,
      "The successor arrow S : T -> T (the real :nno Successor), total: "
      "saturating at ⊤ on bool, wrapping on the word u64 (succ(2**64 - 1) is "
      "0).");
  bind_step_operators(succ);
  succ.def("__repr__", [](const Successor<T>&) { return "succ"; });
  auto pred = nb::class_<Predecessor<T>>(
      m, pred_name,
      "The predecessor arrow P : T -> T (the real :nno Predecessor), total: "
      "saturating at ⊥ on bool, wrapping on the word u64 (pred(0) is "
      "2**64 - 1).");
  bind_step_operators(pred);
  pred.def("__repr__", [](const Predecessor<T>&) { return "pred"; });
  m.def("dom", [](const Successor<T>&) { return type_object<T>(); });
  m.def("cod", [](const Successor<T>&) { return type_object<T>(); });
  m.def("dom", [](const Predecessor<T>&) { return type_object<T>(); });
  m.def("cod", [](const Predecessor<T>&) { return type_object<T>(); });
}

/** @brief Bind one carrier @c T: the real @c Identity<T> and the type-erased
 *  @c Morphism<T,T>, their operators, and the free accessors @c dom / @c cod.
 */
template <typename T>
void bind_carrier(nb::module_& m, const char* identity_name,
                  const char* morphism_name) {
  auto identity = nb::class_<Identity<T>>(
      m, identity_name,
      "The identity arrow id: T -> T (the monoid unit).  A real :morphism "
      "Identity<T>.");
  bind_endo_operators(identity);
  identity.def("__repr__", [](const Identity<T>&) { return "id"; });

  auto morphism = nb::class_<jlt::Arrow<T>>(
      m, morphism_name,
      "A type-erased endo-arrow T -> T (the real :morphism Morphism).  "
      "Composition and refl land here.");
  bind_endo_operators(morphism);
  morphism.def("__repr__", [](const jlt::Arrow<T>&) { return "<arrow>"; });

  m.def(
      "dom", [](const Identity<T>&) { return type_object<T>(); },
      "dom(f): the domain object (a type).");
  m.def(
      "dom", [](const jlt::Arrow<T>&) { return type_object<T>(); },
      "dom(f): the domain object (a type).");
  m.def(
      "cod", [](const Identity<T>&) { return type_object<T>(); },
      "cod(f): the codomain object (a type).");
  m.def(
      "cod", [](const jlt::Arrow<T>&) { return type_object<T>(); },
      "cod(f): the codomain object (a type).");
}

}  // namespace

NB_MODULE(_jlt, m) {
  m.doc() =
      "The Jlt arrow DSL (#961): a fluent, point-free calculus of involutive "
      "endomorphisms over two objects, bool and int.  Generators are `id(T)` "
      "and `refl(T)`, the reflection involution (:logic's logic_complement: "
      "not "
      "on bool, the order-reversing ~ on the int chain).  Compose with `f >> "
      "g` "
      "(apply f, then g); apply with `a(x)`; inspect with `dom(a)` / `cod(a)`. "
      " "
      "Arrows are extensional (functions); composing across objects (bool vs "
      "int) is not defined and raises.";

  bind_carrier<bool>(m, "IdentityBool", "MorphismBool");
  bind_carrier<jlt::Int>(m, "IdentityInt", "MorphismInt");
  bind_steps<bool>(m, "SuccessorBool", "PredecessorBool");
  bind_steps<jlt::Int>(m, "SuccessorInt", "PredecessorInt");
  bind_tensors(m);

  // id(T) / refl(T): the primitive arrows on carrier T (a Python type object,
  // bool or int) -- mirroring :morphism's id<T>().  refl is :logic's reflection
  // involution logic_complement<L>: `not` (¬) on bool, the order-reversing `~`
  // on the int chain.  `not` is a Python keyword, so it is exposed as `refl`.
  m.def(
      "id",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(Identity<bool>{});
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(Identity<jlt::Int>{});
        throw nb::type_error("id(T): T must be bool or int");
      },
      nb::arg("carrier"), "id(T): the identity arrow on carrier T.");
  m.def(
      "refl",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(jlt::refl<Boole>());
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(jlt::refl<Chain<jlt::Int>>());
        throw nb::type_error("refl(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "refl(T): the reflection involution on carrier T (not on bool, negation "
      "on int).");
  m.def(
      "succ",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(Successor<bool>{});
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(Successor<jlt::Int>{});
        throw nb::type_error("succ(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "succ(T): the successor arrow on carrier T, the NNO step (saturating: "
      "succ(bool)(True) is True; on int, the word u64, it wraps: succ(2**64 - "
      "1) is 0).");
  m.def(
      "pred",
      [](nb::handle t) -> nb::object {
        if (t.is(type_object<bool>())) return nb::cast(Predecessor<bool>{});
        if (t.is(type_object<jlt::Int>()))
          return nb::cast(Predecessor<jlt::Int>{});
        throw nb::type_error("pred(T): T must be bool or int");
      },
      nb::arg("carrier"),
      "pred(T): the predecessor arrow on carrier T (saturating: pred(bool)"
      "(False) is False; on int, the word u64, it wraps: pred(0) is 2**64 - "
      "1).");
}
