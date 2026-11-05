/**
 * @file dedekind/python/nanobind_jlt.cpp
 * @brief Nanobind extension module for the Jlt arrow DSL (#961).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * A @b fluent DSL over the @b real category arrows, bound as the private native
 * extension @c pynzli._jlt (NumPy-style, re-exported by the pure-Python
 * @c pynzli.jlt facade).  It exposes the actual @c :morphism arrows over TWO
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
#include <nanobind/stl/string.h>
#include <nanobind/stl/string_view.h>
#include <nanobind/stl/vector.h>

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <optional>
#include <ranges>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
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
 *  u64 = ℤ/2⁶⁴ (@c pynzli.pst), whose step wraps. */
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
using dedekind::category::ReflectedReverse;
using dedekind::category::Reflection;
using dedekind::category::Reverse;
using dedekind::category::Rotation;
using dedekind::category::symmetric;
using dedekind::category::with_width;

/** @brief A filtered alphabet: the byte chain between its first and last
 *  letter, under the predicate that keeps the letters. */
using Alphabet = dedekind::category::Alphabet;
using WordPower = Power<std::byte, Letters, std::size_t>;
/** @brief @c words(alphabet): all the words, by length; @c oligos. */
using WordPowers = dedekind::category::Words;
using Length = dedekind::category::Length<>;
using TruncatedWords = FreeMonoid<std::byte, Letters, std::size_t>;
/** @brief A letter map on every letter of a word, and that followed by the
 *  reversal of the positions: the reverse complement where the map is the
 *  complement. */
using LetterPower = PowerMap<LetterMap>;
using DualPower = PowerMap<Reflection>;
using Word = std::vector<std::byte>;
using ReverseAfter = dedekind::category::Compose<
    LetterPower, dedekind::category::Morphism<Word, Word, Reverse>>;
/** @brief The duality on every letter, then the reversal: the reverse
 *  complement as the alphabet's own, an involution by its type. */
using ReverseAfterDual = dedekind::category::Compose<
    DualPower, dedekind::category::Morphism<Word, Word, Reverse>>;

/** @brief @c fixed_by(g): the stage of what the reverse complement fixes,
 *  for @c power @c & @c fixed_by(g); the complement a table, the duality,
 *  or @c ~rev, the alphabet's own, taken where the stage meets the power. */
struct FixedStage {
  std::variant<ReverseAfter, ReverseAfterDual, ReflectedReverse> g;
};
/** @brief @c words(a) @c & @c fixed_by(g): the words @c g fixes, by length,
 *  @c oligos @c & @c fixed_by(~rev); @c & @c len(n) is one of its sections. */
struct SectionFamily {
  std::variant<decltype(std::declval<const WordPowers&>() &
                        dedekind::category::fixed_by(
                            std::declval<const ReverseAfter&>())),
               decltype(std::declval<const WordPowers&>() &
                        dedekind::category::fixed_by(
                            std::declval<const ReverseAfterDual&>()))>
      family;
};
/** @brief A symmetric section of a tensor power held as itself, not as its
 *  count, so that a window can land in it (@c cylinder, @c occurrences): the
 *  palindromes, the rotation-fixed words, or the words a letter map then the
 *  reversal fixes. */
struct Section {
  std::variant<
      dedekind::category::Symmetric<WordPower, Reverse,
                                    dedekind::category::Identity<std::byte>>,
      dedekind::category::Symmetric<WordPower, Rotation<>,
                                    dedekind::category::Identity<std::byte>>,
      dedekind::category::Symmetric<WordPower, Reverse, LetterMap>,
      dedekind::category::Symmetric<WordPower, Reverse, Reflection>>
      section;
};
/** @brief A tensor held by its factors, the core's @c count_factors of
 *  whatever it was built from; a section converts to it, so every count
 *  and product taking a tensor takes a section. */
/** @brief The factors as a list the type does not bound: a Python tensor's
 *  products are built at run time. */
template <std::ranges::input_range L>
std::vector<Factor<>> factor_list(const L& fs) {
  return {std::ranges::begin(fs), std::ranges::end(fs)};
}
struct Factored {
  std::vector<Factor<>> factors;
  Factored(std::vector<Factor<>> fs) : factors(std::move(fs)) {}
  template <std::ranges::input_range L>
    requires std::same_as<std::ranges::range_value_t<L>, Factor<>>
  Factored(const L& fs) : factors(factor_list(fs)) {}
  Factored(const Section& s)
      : factors(std::visit(
            [](const auto& x) {
              return factor_list(dedekind::category::count_factors(x));
            },
            s.section)) {}
};
/** @brief A tensor's factors as Python holds them: a held tensor's own
 *  list, any other tensor's from the core's @c count_factors. */
std::vector<Factor<>> factors_of(const Factored& f) { return f.factors; }
inline std::vector<Factor<>> factors_of(const Section& s) {
  return Factored{s}.factors;
}
template <typename T>
std::vector<Factor<>> factors_of(const T& t) {
  return factor_list(dedekind::category::count_factors(t));
}
/** @brief The outer product of two counted tensors, held by its factors: the
 *  one @c __mul__ of every tensor class.  Its list is the caller's state,
 *  built here at the boundary, where products are formed at run time; the
 *  core's outer product is of shapes, bounded by their types. */
template <typename A, typename B>
Factored times(const A& a, const B& b) {
  std::vector<Factor<>> fs = factors_of(a);
  const std::vector<Factor<>> gs = factors_of(b);
  fs.insert(fs.end(), gs.begin(), gs.end());
  return Factored{std::move(fs)};
}

/** @param a the letters, as written.  @return the alphabet. */
Alphabet abc(std::string_view a) {
  if (a.empty()) throw nb::value_error("abc: no letters");
  if (std::ranges::contains(a, '\xFF'))
    throw nb::value_error("abc: the byte 0xFF ends no byte slice");
  return dedekind::category::abc(a);
}
/** @brief The factors as Python sees them: pairs (base, exponent). */
template <std::ranges::sized_range L>
std::vector<std::pair<std::size_t, std::size_t>> pairs(const L& fs) {
  std::vector<std::pair<std::size_t, std::size_t>> out;
  out.reserve(std::ranges::size(fs));
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
  // the low n bits, as pack reads them: no padding above the width
  if (n < 64) k &= (std::size_t{1} << n) - 1;
  return with_width(n, [k, n](auto width) {
    constexpr std::size_t W = decltype(width)::value;
    return dedekind::category::IsSquare<W>{n}(dedekind::category::pack<W>(k));
  });
}
}  // namespace jlt

// ── The string matcher (#1135): Listing 5's second panel from Python ───────
//
// The two lines of panel 2, as Python spells them on the caller's bytes:
//
//   count(sequence >> pali6)                                    the query
//   count(sequence >> matches(automaton(pali6)))                    the plan
//
// The bytes are read where they are (the handle keeps the Python object
// alive, nothing is copied); every stage is the library's own, dispatched at
// the boundary.  The plan is a value: built at run time here by the same
// function C++ runs at compile time, compared by == (equality of the
// languages), printed as its EXPLAIN.  Capacities are a menu: the smallest
// that holds the plan is taken, none a ValueError naming the bound.

namespace jlt {
using dedekind::category::Automaton;
using dedekind::category::Holes;

/** @brief The stage ending a query, @c >> @c counting: the exported object's
 *  type (its namespace is the comprehension's detail). */
using CountingStage =
    std::remove_cvref_t<decltype(dedekind::category::counting)>;
/** @brief A text of the caller's, Python's bytes, read in place. */
struct Text {
  nb::bytes bytes;
  std::span<const std::byte> view() const {
    return {static_cast<const std::byte*>(bytes.data()), bytes.size()};
  }
};
/** @brief @c power @c >> @c section: the join on the window, held by its
 *  factors.  @param p the power.  @param s the section.  @return the tensor. */
inline Factored window_join(const WordPower& p, const Section& s) {
  return std::visit(
      [&](const auto& x) { return Factored{jlt::count_factors(p >> x)}; },
      s.section);
}
/** @brief @c dna @c >> @c section: the query. */
struct WindowQuery {
  Text text;
  Section section;
};
/** @brief A plan, one of the menu's capacities: the letters' count 4, the
 *  words' length up to 8, a pending set's steps and the states bounded as
 *  the menu says. */
struct Plan {
  std::variant<Automaton<4, 16>, Automaton<4, 320>> plan;
  bool operator==(const Plan& o) const { return plan == o.plan; }
};
/** @brief @c matches(plan, holes): the stage running a matcher. */
struct MatchesStage {
  Plan plan;
  Holes holes;
};
/** @brief @c dna @c >> @c matches(plan): the query of the positions where a
 *  site ends. */
struct MatchQuery {
  Text text;
  MatchesStage stage;
};

/** @brief The plan of a section, from the menu: the first capacity that
 *  holds it.  @param s the section. */
template <typename S>
Plan plan_of(const S& s) {
  if (const auto small = dedekind::category::automaton<4, 8, 4, 16, 256>(s))
    return {*small};
  if (const auto large = dedekind::category::automaton<4, 8, 32, 320, 256>(s))
    return {*large};
  throw nb::value_error(
      "automaton: the section's plan exceeds the largest capacity (320 "
      "states, 32 steps per pending set, words of length 8 over 4 letters)");
}
/** @brief @c pali6 @c & @c prefix(word): the law lists its words, so only the
 *  reverse complement by the alphabet's own reflection has one yet. */
using PrefixedSites =
    dedekind::category::Prefixed<WordPower, Reverse, Reflection>;
/** @param s the section.  @param p the prefix.  @return their meet. */
inline PrefixedSites with_prefix(const Section& s,
                                 const dedekind::category::Prefix& p) {
  const auto* x = std::get_if<
      dedekind::category::Symmetric<WordPower, Reverse, Reflection>>(
      &s.section);
  if (x == nullptr)
    throw nb::type_error("section & prefix: the section must be fixed by ~rev");
  for (const std::byte b : p.word)
    if (!x->power.member(b))
      throw nb::value_error("prefix: a letter outside the alphabet");
  return *x & p;
}
Plan automaton(const Section& s) {
  return std::visit([](const auto& x) { return plan_of(x); }, s.section);
}
/** @brief The plan as its EXPLAIN: how many states, how many accept. */
std::string explain(const Plan& p) {
  return std::visit(
      [](const auto& a) {
        return "automaton(states=" + std::to_string(a.next.size()) +
               ", accepting=" +
               std::to_string(std::ranges::count(a.accepting, true)) + ")";
      },
      p.plan);
}
/** @brief Whether the section holds a word of bytes. */
bool holds(const Section& s, const Text& w) {
  return std::visit([&](const auto& x) { return x(w.view()); }, s.section);
}
std::size_t count(const MatchQuery& q) {
  return std::visit(
      [&](const auto& a) {
        return dedekind::category::count(
            q.text.view() >> dedekind::category::matches(a, q.stage.holes));
      },
      q.stage.plan.plan);
}
/** @brief The positions where a site ends, as a list: the boundary's own
 *  materialisation of the query (@c collect in C++ takes a static bound). */
std::vector<std::size_t> collect(const MatchQuery& q) {
  return std::visit(
      [&](const auto& a) {
        std::vector<std::size_t> out;
        for (const auto& m :
             q.text.view() >> dedekind::category::matches(a, q.stage.holes))
          if (m) out.push_back(*m);
        return out;
      },
      q.stage.plan.plan);
}
std::size_t count(const WindowQuery& q) {
  return std::visit(
      [&](const auto& x) {
        return dedekind::category::count(q.text.view() >> x);
      },
      q.section.section);
}
/** @brief The positions where a window that is a site begins. */
std::vector<std::size_t> collect(const WindowQuery& q) {
  return std::visit(
      [&](const auto& x) {
        std::vector<std::size_t> out;
        std::size_t at = 0;
        for (const auto& m : q.text.view() >> x) {
          if (m) out.push_back(at);
          ++at;
        }
        return out;
      },
      q.section.section);
}
}  // namespace jlt

/** @brief Bind the matcher: the stages, the plan, the queries. */
void bind_matcher(nb::module_ module_) {
  nb::enum_<jlt::Holes>(module_, "Holes",
                        "What a byte outside the alphabet does to a match: "
                        "break it (the default), or be skipped.")
      .value("break_match", jlt::Holes::break_match)
      .value("skip", jlt::Holes::skip);
  nb::class_<jlt::Plan>(
      module_, "Automaton",
      "The matcher specialised to a section: the minimal automaton of the "
      "words it holds, canonical, so == compares the languages.  The same "
      "function C++ runs at compile time, run here.")
      .def("__eq__",
           [](const jlt::Plan& a, const jlt::Plan& b) { return a == b; })
      .def("__repr__", &jlt::explain);
  nb::class_<jlt::MatchesStage>(module_, "Matches",
                                "matches(plan): the stage running a matcher "
                                "along a sequence of bytes.")
      .def("__rrshift__", [](const jlt::MatchesStage& m, nb::bytes b) {
        return jlt::MatchQuery{{std::move(b)}, m};
      });
  nb::class_<dedekind::category::Prefix>(
      module_, "Prefix",
      "prefix(word): the words beginning with these letters.");
  nb::class_<jlt::PrefixedSites>(
      module_, "PrefixedSection",
      "section & prefix(word): the words of the section beginning so.")
      .def(
          "__call__",
          [](const jlt::PrefixedSites& s, nb::bytes w) {
            return s(jlt::Text{std::move(w)}.view());
          },
          nb::arg("word"));
  nb::class_<jlt::CountingStage>(
      module_, "Counting",
      "counting: the stage form of count, ending a query: q >> counting.");
  module_.attr("counting") = dedekind::category::counting;
  nb::class_<jlt::MatchQuery>(module_, "MatchQuery",
                              "The positions where a site ends: count it, "
                              "collect it.")
      .def("__rshift__",
           [](const jlt::MatchQuery& q, const jlt::CountingStage&) {
             return jlt::count(q);
           });
  nb::class_<jlt::WindowQuery>(module_, "WindowQuery",
                               "The windows that are sites: count it, collect "
                               "it (the positions where they begin).")
      .def("__rshift__",
           [](const jlt::WindowQuery& q, const jlt::CountingStage&) {
             return jlt::count(q);
           });

  module_.def(
      "fixed_by", [](const jlt::ReverseAfter& g) { return jlt::FixedStage{g}; },
      nb::arg("symmetry"),
      "fixed_by(g): the stage of the words g fixes, for power & fixed_by(g), "
      "the power met with the predicate: symmetric(power, g).");
  module_.def(
      "fixed_by",
      [](const jlt::ReverseAfterDual& g) { return jlt::FixedStage{g}; },
      nb::arg("symmetry"));
  module_.def(
      "fixed_by",
      [](const jlt::ReflectedReverse& g) { return jlt::FixedStage{g}; },
      nb::arg("symmetry"));
  module_.def(
      "prefix",
      [](std::string_view word) {
        if (word.size() > 8) throw nb::value_error("prefix: at most 8 letters");
        return dedekind::category::prefix(word);
      },
      nb::arg("word"), "prefix(word): the words beginning with these letters.");
  module_.def(
      "automaton", [](const jlt::PrefixedSites& s) { return jlt::plan_of(s); },
      nb::arg("section"));
  module_.def(
      "states",
      [](const jlt::Plan& p) {
        return std::visit(
            [](const auto& a) { return dedekind::category::states(a); },
            p.plan);
      },
      nb::arg("plan"), "states(plan): how many states the plan has.");
  module_.def("automaton", &jlt::automaton, nb::arg("section"),
              "automaton(section): the matcher specialised to the section's "
              "words, its plan built now; ValueError where it exceeds the "
              "menu's largest capacity.");
  module_.def(
      "matches",
      [](const jlt::Plan& p, jlt::Holes holes) {
        return jlt::MatchesStage{p, holes};
      },
      nb::arg("plan"), nb::arg("holes") = jlt::Holes::break_match,
      "matches(plan, holes=Holes.break_match): run the matcher; a byte "
      "outside the alphabet breaks a match, or is skipped.");
  module_.def(
      "count", [](const jlt::MatchQuery& q) { return jlt::count(q); },
      nb::arg("query"), "count(query): how many sites.");
  module_.def(
      "count", [](const jlt::WindowQuery& q) { return jlt::count(q); },
      nb::arg("query"));
  module_.def(
      "collect", [](const jlt::MatchQuery& q) { return jlt::collect(q); },
      nb::arg("query"),
      "collect(query): the positions, where a site ends (the matcher) or "
      "where its window begins (the query).");
  module_.def(
      "collect", [](const jlt::WindowQuery& q) { return jlt::collect(q); },
      nb::arg("query"));
}

/** @brief Bind the tensor exhibit's handles. */
void bind_tensors(nb::module_ module_) {
  nb::class_<jlt::Alphabet>(module_, "Alphabet",
                            "A filtered alphabet: the byte chain under the "
                            "predicate that keeps its letters.");
  nb::class_<jlt::WordPower>(module_, "Power",
                             "The words of one length over an alphabet, a "
                             "tensor power, never built.")
      .def("__mul__", &jlt::times<jlt::WordPower, jlt::WordPower>)
      .def("__mul__", &jlt::times<jlt::WordPower, jlt::Factored>)
      .def("__rshift__", &jlt::window_join)
      .def("__and__", [](const jlt::WordPower& p, const jlt::FixedStage& st) {
        return std::visit(
            [&](const auto& g) {
              return jlt::Section{p & dedekind::category::fixed_by(g)};
            },
            st.g);
      });
  nb::class_<jlt::TruncatedWords>(
      module_, "FreeMonoid",
      "The free monoid on an alphabet truncated below a "
      "length: the coproduct of the tensor powers.");
  nb::class_<jlt::Reverse>(module_, "Reverse",
                           "The reversal of a word's positions.")
      .def("__invert__", [](const jlt::Reverse& r) { return ~r; });
  nb::class_<jlt::ReflectedReverse>(
      module_, "ReflectedReverse",
      "~rev: the reversal, then the reflection of the alphabet on every "
      "letter, the reverse complement, natural in the alphabet; fixed_by(~rev) "
      "takes the component at the power it meets.");
  nb::class_<jlt::Rotation<>>(module_, "Rotation",
                              "A rotation of a word's positions.");
  nb::class_<jlt::LetterMap>(module_, "LetterMap", "An arrow on the letters.");
  nb::class_<jlt::Reflection>(
      module_, "Reflection",
      "The duality of an alphabet: the one order-reversing "
      "bijection of its letters, an involution by its type.");
  nb::class_<jlt::ReverseAfterDual>(
      module_, "ReverseAfterDual",
      "The duality on every letter, then the reversal of the positions.");
  nb::class_<jlt::DualPower>(module_, "DualPower",
                             "The duality on every letter of a word.")
      .def("__rshift__", [](const jlt::DualPower& f, const jlt::Reverse& r) {
        return f >> r;
      });
  nb::class_<jlt::ReverseAfter>(
      module_, "ReverseAfter",
      "A map on every letter, then the reversal of the positions.");
  nb::class_<jlt::LetterPower>(module_, "PowerMap",
                               "A letter map on every letter of a word.")
      .def("__rshift__", [](const jlt::LetterPower& f, const jlt::Reverse& r) {
        return f >> r;
      });
  // A Section counts as a Tensor wherever one is taken as an argument; as the
  // left operand of * it is not converted, so it multiplies itself.
  nb::class_<jlt::FixedStage>(module_, "FixedBy",
                              "fixed_by(g): what a symmetry fixes, the "
                              "predicate a power is met with.");
  nb::class_<jlt::Section>(
      module_, "Section",
      "A symmetric section of a tensor power, held as itself: the words a "
      "symmetry fixes.  Counted as a Tensor wherever one is taken.")
      .def("__mul__", &jlt::times<jlt::Section, jlt::Factored>)
      .def("__mul__", &jlt::times<jlt::Section, jlt::WordPower>)
      .def("__and__", &jlt::with_prefix)
      .def("__rrshift__",
           [](const jlt::Section& s, nb::bytes b) {
             return jlt::WindowQuery{{std::move(b)}, s};
           })
      .def(
          "__call__",
          [](const jlt::Section& s, nb::bytes w) {
            return jlt::holds(s, jlt::Text{std::move(w)});
          },
          nb::arg("word"),
          "section(word): whether the section holds the word of bytes, read "
          "in place: its length, its letters, and the symmetry at every "
          "position.");
  nb::class_<jlt::Factored>(
      module_, "Tensor",
      "A tensor held by its factors, the core's count_factors.")
      .def(nb::init_implicit<const jlt::Section&>())
      .def("__mul__", &jlt::times<jlt::Factored, jlt::Factored>)
      .def("__mul__", &jlt::times<jlt::Factored, jlt::WordPower>);
  nb::class_<dedekind::category::Window<>>(
      module_, "Window",
      "The window of a word: its letters from a position, a given number "
      "long, the projection onto those positions.");

  module_.def("abc", &jlt::abc, nb::arg("letters"),
              "abc(letters): the filtered alphabet of the letters written.");
  module_.def(
      "refl",
      [](const jlt::Alphabet& a) { return dedekind::category::refl(a); },
      nb::arg("alphabet"),
      "refl(alphabet): the alphabet's duality, each letter to the one at the "
      "reflected rank, the Watson-Crick complement of ACGT; an involution.");
  module_.def(
      "letter_map",
      [](std::string_view from, std::string_view to) {
        if (from.size() != to.size())
          throw nb::value_error("letter_map: as many images as letters");
        return dedekind::category::letter_map(from, to);
      },
      nb::arg("from_"), nb::arg("to"),
      "letter_map(from, to): each letter to the image at the same place, every "
      "other byte fixed.");
  module_.attr("rev") = dedekind::category::rev;
  module_.def(
      "rotate", [](std::size_t d) { return dedekind::category::rotate(d); },
      nb::arg("by"), "rotate(by): the positions i -> i + by mod n.");
  module_.def(
      "is_involution",
      [](const jlt::LetterMap& t) {
        return dedekind::category::is_involution(t);
      },
      nb::arg("arrow"),
      "is_involution(f): whether f twice is the identity, decided on its "
      "table, or derived from its legs.");
  module_.def(
      "is_involution",
      [](const jlt::ReverseAfter& g) {
        return dedekind::category::is_involution(g);
      },
      nb::arg("arrow"));
  module_.def(
      "is_involution",
      [](const jlt::Reflection& d) {
        return dedekind::category::is_involution(d);
      },
      nb::arg("arrow"));
  module_.def(
      "is_involution",
      [](const jlt::ReverseAfterDual& g) {
        return dedekind::category::is_involution(g);
      },
      nb::arg("arrow"));
  module_.def(
      "power",
      [](const jlt::Alphabet& a, std::size_t n) { return jlt::power(a, n); },
      nb::arg("alphabet"), nb::arg("length"),
      "power(alphabet, n): the words of length n, a tensor power.");
  nb::class_<jlt::Length>(module_, "Length",
                          "len(n): the fibre of the length over n, the "
                          "predicate a family of words is met with.");
  module_.def(
      "len", [](std::size_t n) { return dedekind::category::len(n); },
      nb::arg("n"), "len(n): words & len(n) is the words of length n.");
  nb::class_<jlt::WordPowers>(module_, "Words",
                              "words(alphabet): the words over it, by "
                              "length; words(a) & len(n) is power(a, n).")
      .def(
          "__call__",
          [](const jlt::WordPowers& p, std::size_t n) { return p(n); },
          nb::arg("length"))
      .def("__and__",
           [](const jlt::WordPowers& p, const jlt::Length& l) { return p & l; })
      .def("__and__", [](const jlt::WordPowers& p, const jlt::FixedStage& st) {
        return std::visit(
            [&](const auto& g) {
              return jlt::SectionFamily{p & dedekind::category::fixed_by(g)};
            },
            st.g);
      });
  nb::class_<jlt::SectionFamily>(
      module_, "SectionFamily",
      "words(a) & fixed_by(g): the words g fixes, by length; & len(n) "
      "is the section of that length.")
      .def("__and__",
           [](const jlt::SectionFamily& f, const jlt::Length& l) {
             return std::visit(
                 [&](const auto& fam) { return jlt::Section{fam & l}; },
                 f.family);
           })
      .def(
          "__call__",
          [](const jlt::SectionFamily& f, std::size_t n) {
            return std::visit(
                [&](const auto& fam) { return jlt::Section{fam(n)}; },
                f.family);
          },
          nb::arg("length"));
  module_.def(
      "words",
      [](const jlt::Alphabet& a) { return dedekind::category::words(a); },
      nb::arg("alphabet"),
      "words(alphabet): the words over it, by length; the free monoid as a "
      "graded family.");
  module_.def(
      "power", [](const jlt::LetterMap& t) { return jlt::power(t); },
      nb::arg("arrow"),
      "power(f): the letter map f on every letter of a word.");
  module_.def(
      "power", [](const jlt::Reflection& d) { return jlt::power(d); },
      nb::arg("arrow"));
  module_.def(
      "free_monoid",
      [](const jlt::Alphabet& a, std::size_t below) {
        return jlt::free_monoid(a, below);
      },
      nb::arg("alphabet"), nb::arg("below"),
      "free_monoid(alphabet, L): the words shorter than L.");
  module_.def(
      "symmetric",
      [](const jlt::WordPower& p, const jlt::Reverse& s) {
        return jlt::Section{jlt::symmetric(p, s)};
      },
      nb::arg("power"), nb::arg("symmetry"),
      "symmetric(power, g): the words g fixes, the legs merged along the "
      "cycles of its permutation of the positions: rev, rotate(d), or "
      "power(complement) >> rev.");
  module_.def(
      "symmetric",
      [](const jlt::WordPower& p, const jlt::Rotation<>& s) {
        return jlt::Section{jlt::symmetric(p, s)};
      },
      nb::arg("power"), nb::arg("symmetry"));
  module_.def(
      "symmetric",
      [](const jlt::WordPower& p, const jlt::ReverseAfter& g) {
        return jlt::Section{jlt::symmetric(p, g)};
      },
      nb::arg("power"), nb::arg("symmetry"));
  module_.def(
      "symmetric",
      [](const jlt::WordPower& p, const jlt::ReverseAfterDual& g) {
        return jlt::Section{jlt::symmetric(p, g)};
      },
      nb::arg("power"), nb::arg("symmetry"));
  module_.def(
      "count_factors",
      [](const jlt::WordPower& p) { return jlt::pairs(jlt::count_factors(p)); },
      nb::arg("tensor"),
      "count_factors(t): the factors (base, exponent) of t's count, each base "
      "a popcount; exact where the count is not.");
  module_.def(
      "count_factors",
      [](const jlt::Factored& f) { return jlt::pairs(f.factors); },
      nb::arg("tensor"));
  module_.def(
      "count",
      [](const jlt::WordPower& p) { return jlt::count(jlt::count_factors(p)); },
      nb::arg("tensor"),
      "count(t): the count modulo 2**64, a monoid concat under * over the "
      "factors, each by squaring.");
  module_.def(
      "count", [](const jlt::Factored& f) { return jlt::count(f.factors); },
      nb::arg("tensor"));
  module_.def(
      "exact_count",
      [](const jlt::WordPower& p) {
        return jlt::exact_count(jlt::count_factors(p));
      },
      nb::arg("tensor"),
      "exact_count(t): the count, or None where the word cannot hold it.");
  module_.def(
      "exact_count",
      [](const jlt::Factored& f) { return jlt::exact_count(f.factors); },
      nb::arg("tensor"));
  module_.def(
      "exact_count",
      [](const jlt::TruncatedWords& w) { return jlt::exact_count(w); },
      nb::arg("tensor"));
  module_.def(
      "window",
      [](std::size_t offset, std::size_t length) {
        return dedekind::category::window(offset, length);
      },
      nb::arg("offset"), nb::arg("length"),
      "window(i, n): a word's letters i to i + n - 1, its projection onto "
      "those positions.");
  module_.def(
      "cylinder",
      [](const jlt::WordPower& p, const dedekind::category::Window<>& w,
         const jlt::Section& s) {
        return std::visit(
            [&](const auto& x) {
              return jlt::Factored{
                  jlt::count_factors(dedekind::category::cylinder(p, w, x))};
            },
            s.section);
      },
      nb::arg("power"), nb::arg("window"), nb::arg("inner"),
      "cylinder(p, w, s): the words of p whose window w lies in s, its count "
      "pushed down: s's, times the alphabet's for each free leg.");
  module_.def(
      "occurrences",
      [](const jlt::WordPower& p, const jlt::Section& s) {
        return std::visit(
            [&](const auto& x) {
              return dedekind::category::occurrences(p, x);
            },
            s.section);
      },
      nb::arg("power"), nb::arg("inner"),
      "occurrences(p, s): how often s occurs in p's words, with its "
      "positions, modulo 2**64.");
  module_.def(
      "exact_occurrences",
      [](const jlt::WordPower& p, const jlt::Section& s) {
        return std::visit(
            [&](const auto& x) {
              return dedekind::category::exact_occurrences(p, x);
            },
            s.section);
      },
      nb::arg("power"), nb::arg("inner"),
      "exact_occurrences(p, s): the occurrences, or None where the word "
      "cannot hold them.");
  module_.def(
      "pack", &jlt::packed, nb::arg("pattern"), nb::arg("width"),
      "pack(k, w): the pattern k in the bit word of width w, read back: "
      "its low w bits.");
  module_.def(
      "is_square", &jlt::is_square, nb::arg("pattern"), nb::arg("width"),
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
 *  read by @c pynzli.pst.image. */
template <typename T>
void bind_steps(nb::module_ m, const char* succ_name, const char* pred_name) {
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
void bind_carrier(nb::module_ m, const char* identity_name,
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
  bind_matcher(m);

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
