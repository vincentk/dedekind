/**
 * @file dedekind/python/nanobind_pst.cpp
 * @brief Nanobind extension module for Pst: the truth chains (#1001) and the
 *        sets over them, per the paper's Lwv grammar (#975).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * @f$\mathbf{Pst} = \mathbf{Jlt} \cap \mathbf{Chain}@f$ (paper §3), bound as
 * the private native extension @c dedekind._pst (re-exported by
 * @c dedekind.pst).  The @b chains @c B (𝔹), @c K3 (Kleene's three) and @c N
 * (ℕ's proxy, ⊤ = ℵ₀) are handles over the real carriers: endpoints, the step
 * in both readings, the order, their classification, and the unfold by the
 * cover.  The @b sets over 𝔹 and K₃ are the chain fragment of the Lwv grammar
 * (scalar carriers; no products, no composed predicates): generators
 * @c 𝔸(chain) / @c Ø(chain) / @c η(v), the former @c S @c | @c (π @c > @c v),
 * the lattice @c & @c | @c ^ @c ~, membership @c x @c in @c S / @c S(x), the
 * queries @c == and @c <= answering in the set's own species (@c Unknown is a
 * verdict), the quantifiers @c exists / @c forall (also @c any / @c all), and
 * the normal form @c runs.  Every handle is a real library set
 * (@c Comprehension over @c 𝔸 with a type-erased classifier); every operator
 * runs the library's node, every query the library's exhaustion of the chain.
 * Reduction stays in C++; Python holds handles.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/string.h>

#include <concepts>
#include <cstddef>
#include <optional>
#include <string>
#include <variant>

import dedekind.category;  // Ternary, Boole, Kleene, IsPst
import dedekind.python;    // dedekind::python::pst
import dedekind.sequences; // Run
import dedekind.sets;      // Cardinality, ℵ_0, finite_cardinality

namespace nb = nanobind;

namespace {
namespace pst = dedekind::python::pst;
using dedekind::category::Boole;
using dedekind::category::Kleene;
using dedekind::category::Ternary;
using dedekind::sets::Cardinality;
using dedekind::sets::ℵ_0;

/** @brief ℕ's elements cross the boundary as Python @c int, its top as the one
 *  @c aleph0 object. */
nb::object to_py_cardinality(const Cardinality& c) {
  if (std::holds_alternative<ℵ_0>(c)) return nb::cast(ℵ_0{});
  return nb::int_(std::get<dedekind::sets::ExtensionalCardinal<>>(c).value);
}
Cardinality from_py_cardinality(nb::handle h) {
  if (nb::isinstance<ℵ_0>(h)) return Cardinality{ℵ_0{}};
  if (nb::isinstance<nb::int_>(h)) {
    const auto v = nb::cast<long long>(h);
    if (v < 0) throw nb::value_error("ℕ has no negative elements");
    return dedekind::sets::finite_cardinality(static_cast<std::size_t>(v));
  }
  throw nb::type_error("an element of ℕ is an int or aleph0");
}
template <typename C>
nb::object to_py(const C& x) {
  if constexpr (std::same_as<C, Cardinality>)
    return to_py_cardinality(x);
  else
    return nb::cast(x);
}
template <typename C>
C from_py_as(nb::handle h) {
  if constexpr (std::same_as<C, Cardinality>)
    return from_py_cardinality(h);
  else
    return nb::cast<C>(h);
}

/** @brief The written form of a truth value: ⊥, U, ⊤. */
std::string symbol(bool b) { return b ? "⊤" : "⊥"; }
std::string symbol(Ternary t) {
  switch (t) {
    case Ternary::False:
      return "⊥";
    case Ternary::Unknown:
      return "U";
    case Ternary::True:
      return "⊤";
  }
  return "?";
}

/** @brief The unfold of a chain from its bottom by the cover: @c StopIteration
 *  is the Python spelling of the cover's @c nullopt at ⊤. */
template <typename C>
struct ChainIter {
  std::optional<C> current;
};

template <typename C>
nb::class_<pst::Chain<C>> bind_chain(nb::module_& m, const char* cls,
                                     const char* iter_cls, const char* doc) {
  using Ch = pst::Chain<C>;
  nb::class_<ChainIter<C>>(m, iter_cls)
      .def("__iter__", [](ChainIter<C>& it) -> ChainIter<C>& { return it; })
      .def("__next__", [](ChainIter<C>& it) {
        if (!it.current) throw nb::stop_iteration();
        const C value = *it.current;
        it.current = pst::cover<C>(value);
        return to_py<C>(value);
      });
  return nb::class_<Ch>(m, cls, doc)
      .def_prop_ro(
          "name", [](const Ch&) { return std::string(Ch::name); },
          "The chain's name.")
      .def_prop_ro(
          "bottom", [](const Ch&) { return to_py<C>(Ch::bottom); },
          "⊥, the least element.")
      .def_prop_ro(
          "top", [](const Ch&) { return to_py<C>(Ch::top); },
          "⊤, the greatest element (ℵ₀ on ℕ, a limit).")
      .def(
          "succ",
          [](const Ch&, nb::handle x) {
            return to_py<C>(pst::succ<C>(from_py_as<C>(x)));
          },
          nb::arg("x"),
          "S(x): the successor, total and saturating at ⊤ (the algebra side).")
      .def(
          "pred",
          [](const Ch&, nb::handle x) {
            return to_py<C>(pst::pred<C>(from_py_as<C>(x)));
          },
          nb::arg("x"),
          "P(x): the predecessor, total and saturating at ⊥ (the monus on ℕ).")
      .def(
          "cover",
          [](const Ch&, nb::handle x) -> nb::object {
            const auto c = pst::cover<C>(from_py_as<C>(x));
            return c ? to_py<C>(*c) : nb::none();
          },
          nb::arg("x"),
          "The element covering x, or None at ⊤: the successor read partially, "
          "N → 1 + N (the coalgebra side).")
      .def(
          "le",
          [](const Ch&, nb::handle x, nb::handle y) {
            return from_py_as<C>(x) <= from_py_as<C>(y);
          },
          nb::arg("x"), nb::arg("y"), "x ≤ y in the chain's order.")
      .def(
          "__iter__", [](const Ch&) { return ChainIter<C>{Ch::bottom}; },
          "Unfold the chain from ⊥ by the cover; stops where the cover stops.")
      .def_prop_ro(
          "is_bounded", [](const Ch&) { return true; },
          "Every Pst chain is bounded: it has ⊥ and ⊤.")
      .def_prop_ro(
          "is_truth_object", [](const Ch&) { return pst::is_truth_object<C>; },
          "IsPst<C>: whether the chain is a truth object (carries ∧, ∨, ¬).")
      .def_prop_ro(
          "is_dense", [](const Ch&) { return pst::is_dense<C>; },
          "IsDense<C>: whether a midpoint always exists (never, on a chain "
          "with the step).")
      .def_prop_ro(
          "saturates", [](const Ch&) { return pst::saturates<C>(); },
          "Whether S(⊤) = ⊤: the successor's posture at the top.")
      .def_prop_ro(
          "cardinality",
          [](const Ch&) { return to_py_cardinality(Ch::cardinality); },
          "The number of elements: 2, 3, or ℵ₀.")
      .def(
          "__getitem__",
          [](const Ch&, long long i) {
            if constexpr (std::same_as<C, Cardinality>) {
              if (i < 0) throw nb::index_error("ℕ has no end to count from");
              return to_py<C>(pst::at<C>(static_cast<std::size_t>(i)));
            } else {
              const auto n = static_cast<long long>(
                  std::get<dedekind::sets::ExtensionalCardinal<>>(
                      Ch::cardinality)
                      .value);
              if (i < 0) i += n;
              if (i < 0 || i >= n)
                throw nb::index_error("position outside the chain");
              return to_py<C>(pst::at<C>(static_cast<std::size_t>(i)));
            }
          },
          nb::arg("i"),
          "C[i]: the element at position i from ⊥, the chain as its own "
          "enumeration (pandas' .iloc, not .loc): K3[1] is UNKNOWN, N[i] is i.")
      .def(
          "__len__",
          [](const Ch&) -> std::size_t {
            if constexpr (std::same_as<C, Cardinality>)
              throw nb::type_error("ℵ₀ is not a Python int (see cardinality)");
            else
              return std::get<dedekind::sets::ExtensionalCardinal<>>(
                         Ch::cardinality)
                  .value;
          },
          "The number of elements of a finite chain.")
      .def("__repr__", [](const Ch&) { return std::string(Ch::name); });
}

// ── The sets of the grammar ───────────────────────────────────────────────

/** @brief The species tags a universe is valued in: @c 𝔸(K3, Kleene). */
struct BooleTag {};
struct KleeneTag {};
/** @brief The grammar's element @c π (also written @c χ): @c π @c > @c v builds
 *  a datum over the carrier the value belongs to. */
struct ProjectionTag {};

/** @brief The runs of a decidable set, written: Ø, {U}, [U, ⊤], ∪ between. */
template <typename C>
std::string written_runs(const pst::Set<C, Boole>& s) {
  std::string out;
  for (const auto& r : pst::runs<C>(s)) {
    if (!out.empty()) out += " ∪ ";
    out += r.lo == r.hi ? "{" + symbol(r.lo) + "}"
                        : "[" + symbol(r.lo) + ", " + symbol(r.hi) + "]";
  }
  return out.empty() ? "Ø" : out;
}

template <typename C>
void bind_datum(nb::module_& m, const char* cls, const char* doc) {
  nb::class_<pst::Datum<C>>(m, cls, doc)
      .def(
          "__call__",
          [](const pst::Datum<C>& d, nb::handle x) {
            return d(from_py_as<C>(x));
          },
          nb::arg("x"), "The datum as a predicate on the carrier.")
      .def("__repr__", [](const pst::Datum<C>&) {
        return std::string("<datum over ") + pst::Chain<C>::name + ">";
      });
}

template <typename C, typename L>
void bind_set(nb::module_& m, const char* cls, const char* doc) {
  using S = pst::Set<C, L>;
  using Ω = typename L::Ω;
  auto set = nb::class_<S>(m, cls, doc);
  set.def(
         "__call__",
         [](const S& s, nb::handle x) { return nb::cast(s(from_py_as<C>(x))); },
         nb::arg("x"), "χ(x), in the set's species.")
      .def(
          "__contains__",
          [](const S& s, nb::handle x) {
            return s(from_py_as<C>(x)) == L::True;
          },
          nb::arg("x"), "x in S: whether χ(x) = ⊤.  For the L-valued χ, S(x).")
      .def(
          "__and__", [](const S& a, const S& b) { return pst::meet(a, b); },
          "A & B: the meet, the library's node erased back.")
      .def(
          "__or__", [](const S& a, const S& b) { return pst::join(a, b); },
          "A | B: the join.")
      .def(
          "__or__",
          [](const S& s, const pst::Datum<C>& d) { return pst::former(s, d); },
          "S | (π ⋈ v): the former, {x ∈ S | P(x)}.")
      .def(
          "__xor__", [](const S& a, const S& b) { return pst::sym_diff(a, b); },
          "A ^ B: the symmetric difference.")
      .def(
          "__invert__", [](const S& a) { return pst::complement(a); },
          "~A: the complement, the species' reflection pointwise.")
      .def(
          "__eq__",
          [](const S& a, const S& b) { return nb::cast(pst::equal(a, b)); },
          nb::is_operator(),
          "A == B in the set's species: ⋀ (χ_A ⇔ χ_B) by exhaustion of the "
          "chain; Unknown is a verdict on a K₃-valued set.")
      .def(
          "__ne__",
          [](const S& a, const S& b) {
            return nb::cast(L::RFL(pst::equal(a, b)));
          },
          nb::is_operator(), "A != B: the reflection of A == B.")
      .def(
          "__le__",
          [](const S& a, const S& b) { return nb::cast(pst::subset(a, b)); },
          nb::is_operator(), "A <= B: A ⊆ B, as (A ∩ B) = A.")
      .def(
          "__ge__",
          [](const S& a, const S& b) { return nb::cast(pst::subset(b, a)); },
          nb::is_operator(), "A >= B: B ⊆ A.")
      .def_prop_ro(
          "is_decidable", [](const S&) { return std::same_as<L, Boole>; },
          "HasDecidableMembership: whether χ answers in 𝔹.")
      .def_prop_ro(
          "carrier", [](const S&) { return pst::Chain<C>{}; },
          "The chain the set lives on.")
      .def(
          "cut",
          [](const S& s, nb::handle level) {
            return pst::cut(s, from_py_as<Ω>(level));
          },
          nb::arg("level"),
          "The α-cut {x | χ(x) >= level}: the upper ray on Ω pulled back along "
          "χ (preimage), a decidable set.")
      .def(
          "fibre",
          [](const S& s, nb::handle level) {
            return pst::fibre(s, from_py_as<Ω>(level));
          },
          nb::arg("level"),
          "The fibre {x | χ(x) == level}: η(level) pulled back along χ "
          "(preimage), a decidable set.");
  if constexpr (std::same_as<L, Boole>) {
    set.def(
           "runs",
           [](const S& s) {
             nb::list out;
             for (const auto& r : pst::runs<C>(s))
               out.append(nb::make_tuple(to_py<C>(r.lo), to_py<C>(r.hi)));
             return out;
           },
           "The maximal runs of membership along the chain, as (lo, hi) pairs: "
           "the normal form, read off the chain.")
        .def("__repr__", [](const S& s) { return written_runs<C>(s); });
    if constexpr (std::same_as<C, Ternary>)
      set.def(
          "lift", [](const S& s) { return pst::lift<C>(s); },
          "The set lifted along the dominance 𝔹 ↪ K₃: the same table, valued "
          "in K₃.");
  } else {
    set.def(
           "runs",
           [](const S&) -> nb::list {
             throw nb::type_error(
                 "an L-valued set has no runs of its own; read it through its "
                 "α-cuts: S.cut(level).runs()");
           },
           "Refused: an L-valued set is read through its α-cuts.")
        .def("__repr__", [](const S& s) {
          return "{χ ≥ ⊤}: " + written_runs<C>(pst::cut(s, L::True)) +
                 "; {χ ≥ U}: " + written_runs<C>(pst::cut(s, Ternary::Unknown));
        });
  }
}

/** @brief Sugar on a truth chain: the grammar's sets with the chain as the
 *  universe, @c K3.above(U) for @c 𝔸(K3) @c | @c (π @c > @c U). */
template <typename C>
void bind_chain_sets(nb::class_<pst::Chain<C>>& ch) {
  using Ch = pst::Chain<C>;
  ch.def_prop_ro(
        "all", [](const Ch&) { return pst::universe<C, Boole>(); },
        "𝔸: the universe, every element.")
      .def_prop_ro(
          "none", [](const Ch&) { return pst::empty<C, Boole>(); },
          "Ø: the empty set.")
      .def_prop_ro(
          "identity", [](const Ch&) { return pst::identity<C>(); },
          "χ(x) = x, valued in the chain's own species: the simplest set with "
          "every level inhabited.")
      .def(
          "above",
          [](const Ch&, nb::handle v) {
            return pst::former(pst::universe<C, Boole>(),
                               pst::above<C>(from_py_as<C>(v)));
          },
          nb::arg("v"), "{x | x > v}: 𝔸(chain) | (π > v).")
      .def(
          "at_least",
          [](const Ch&, nb::handle v) {
            return pst::former(pst::universe<C, Boole>(),
                               pst::at_least<C>(from_py_as<C>(v)));
          },
          nb::arg("v"), "{x | x >= v}.")
      .def(
          "below",
          [](const Ch&, nb::handle v) {
            return pst::former(pst::universe<C, Boole>(),
                               pst::below<C>(from_py_as<C>(v)));
          },
          nb::arg("v"), "{x | x < v}.")
      .def(
          "at_most",
          [](const Ch&, nb::handle v) {
            return pst::former(pst::universe<C, Boole>(),
                               pst::at_most<C>(from_py_as<C>(v)));
          },
          nb::arg("v"), "{x | x <= v}.")
      .def(
          "point",
          [](const Ch&, nb::handle v) {
            return pst::point<C, Boole>(from_py_as<C>(v));
          },
          nb::arg("v"), "{v}: η(v).");
}

/** @brief The datum for one relation, over the chain a Python value belongs
 *  to: a @c bool is 𝔹's, a @c Ternary is K₃'s.  One factory struct per
 *  relation, written out by the macro below. */
#define DEDEKIND_DATUM_OF(NAME)                                    \
  struct NAME {                                                    \
    static nb::object make(nb::handle v) {                         \
      if (nb::isinstance<nb::bool_>(v))                            \
        return nb::cast(pst::NAME<bool>(nb::cast<bool>(v)));       \
      if (nb::isinstance<Ternary>(v))                              \
        return nb::cast(pst::NAME<Ternary>(nb::cast<Ternary>(v))); \
      throw nb::type_error(                                        \
          "a datum compares π with a truth value: a bool (𝔹) "     \
          "or a Ternary (K₃)");                                    \
    }                                                              \
  }
DEDEKIND_DATUM_OF(above);
DEDEKIND_DATUM_OF(at_least);
DEDEKIND_DATUM_OF(below);
DEDEKIND_DATUM_OF(at_most);
DEDEKIND_DATUM_OF(equal_to);
#undef DEDEKIND_DATUM_OF

/** @brief Dispatch a (set, datum) pair of one carrier to a query. */
template <typename Query>
nb::object on_set_and_datum(nb::handle s, nb::handle d, const char* what) {
  if (nb::isinstance<pst::Set<bool, Boole>>(s) &&
      nb::isinstance<pst::Datum<bool>>(d))
    return nb::cast(Query::template apply<bool, Boole>(
        nb::cast<pst::Set<bool, Boole>>(s), nb::cast<pst::Datum<bool>>(d)));
  if (nb::isinstance<pst::Set<Ternary, Boole>>(s) &&
      nb::isinstance<pst::Datum<Ternary>>(d))
    return nb::cast(Query::template apply<Ternary, Boole>(
        nb::cast<pst::Set<Ternary, Boole>>(s),
        nb::cast<pst::Datum<Ternary>>(d)));
  if (nb::isinstance<pst::Set<Ternary, Kleene>>(s) &&
      nb::isinstance<pst::Datum<Ternary>>(d)) {
    if constexpr (requires(const pst::Set<Ternary, Kleene>& ks,
                           const pst::Datum<Ternary>& kd) {
                    Query::template apply<Ternary, Kleene>(ks, kd);
                  })
      return nb::cast(Query::template apply<Ternary, Kleene>(
          nb::cast<pst::Set<Ternary, Kleene>>(s),
          nb::cast<pst::Datum<Ternary>>(d)));
    else
      throw nb::type_error(
          "forall over an L-valued set awaits the chain's residuated "
          "implication (#980); quantify over its α-cut: forall(S.cut(level), "
          "P)");
  }
  const std::string message = std::string(what) +
                              "(S, P): S a set over 𝔹 or K₃ and P a datum "
                              "over the same chain";
  throw nb::type_error(message.c_str());
}
struct Exists {
  template <typename C, typename L>
  static auto apply(const pst::Set<C, L>& s, const pst::Datum<C>& d) {
    return pst::exists(s, d);
  }
};
struct Forall {
  template <typename C, typename L>
    requires std::same_as<L, Boole>
  static auto apply(const pst::Set<C, L>& s, const pst::Datum<C>& d) {
    return pst::forall(s, d);
  }
};
}  // namespace

NB_MODULE(_pst, m) {
  m.doc() =
      "Pst (paper §3): Pst = Jlt ∩ Chain.  The chains B (𝔹), K3 (Kleene's "
      "three truth values) and N (ℕ's proxy, ⊤ = ℵ₀), and the sets over 𝔹 and "
      "K₃ per the Lwv grammar: A(chain) | (π > v), Ø(chain), η(v), the lattice "
      "& | ^ ~, membership x in S / S(x), the queries == and <= in the set's "
      "species, exists / forall (any / all), and runs, the normal form.  Every "
      "handle is a real library set; every query is decided in C++ by "
      "exhausting the chain.";

  nb::enum_<Ternary>(m, "Ternary", "Kleene's three truth values, a chain.")
      .value("FALSE", Ternary::False)
      .value("UNKNOWN", Ternary::Unknown)
      .value("TRUE", Ternary::True);
  nb::class_<ℵ_0>(m, "Aleph0",
                  "ℵ₀, the top of ℕ's proxy: the countable "
                  "cardinal, the memory boundary.")
      .def("__repr__", [](const ℵ_0&) { return std::string("ℵ₀"); })
      .def(
          "__eq__", [](const ℵ_0&, const ℵ_0&) { return true; },
          nb::is_operator())
      .def("__hash__", [](const ℵ_0&) { return 0; });
  m.attr("aleph0") = ℵ_0{};

  auto chain_b =
      bind_chain<bool>(m, "ChainB", "ChainBIter",
                       "𝔹 = {False < True}: the two-element truth chain.");
  auto chain_k3 = bind_chain<Ternary>(
      m, "ChainK3", "ChainK3Iter",
      "K₃ = {FALSE < UNKNOWN < TRUE}: Kleene's truth chain.");
  bind_chain<Cardinality>(
      m, "ChainN", "ChainNIter",
      "ℕ's proxy: 0 < 1 < 2 < … < ℵ₀, a bounded chain whose top is a limit.");

  // ── the sets of the grammar ──
  nb::class_<BooleTag>(m, "SpeciesBoole",
                       "𝔹 as the species a set is valued in.");
  nb::class_<KleeneTag>(m, "SpeciesKleene",
                        "K₃ as the species a set is valued in.");
  m.attr("Boole") = BooleTag{};
  m.attr("Kleene") = KleeneTag{};

  bind_datum<bool>(m, "DatumB", "A datum over 𝔹: π ⋈ v, waiting for a former.");
  bind_datum<Ternary>(m, "DatumK3",
                      "A datum over K₃: π ⋈ v, waiting for a former.");
  bind_set<bool, Boole>(m, "SetB", "A set over 𝔹, valued in 𝔹.");
  bind_set<Ternary, Boole>(m, "SetK3",
                           "A set over K₃, valued in 𝔹 (decidable).");
  bind_set<Ternary, Kleene>(m, "SetK3Kleene",
                            "A set over K₃, valued in K₃: Unknown is a level.");
  bind_chain_sets<bool>(chain_b);
  bind_chain_sets<Ternary>(chain_k3);
  m.attr("B") = pst::Chain<bool>{};
  m.attr("K3") = pst::Chain<Ternary>{};
  m.attr("N") = pst::Chain<Cardinality>{};

  nb::class_<ProjectionTag>(m, "Projection",
                            "The grammar's element π (also χ): π > v, π >= v, "
                            "π < v, π <= v, π == v are the data of the former.")
      .def(
          "__gt__",
          [](const ProjectionTag&, nb::handle v) { return above::make(v); },
          nb::is_operator())
      .def(
          "__ge__",
          [](const ProjectionTag&, nb::handle v) { return at_least::make(v); },
          nb::is_operator())
      .def(
          "__lt__",
          [](const ProjectionTag&, nb::handle v) { return below::make(v); },
          nb::is_operator())
      .def(
          "__le__",
          [](const ProjectionTag&, nb::handle v) { return at_most::make(v); },
          nb::is_operator())
      .def(
          "__eq__",
          [](const ProjectionTag&, nb::handle v) { return equal_to::make(v); },
          nb::is_operator())
      .def("__repr__", [](const ProjectionTag&) { return std::string("π"); });
  m.attr("π") = ProjectionTag{};
  m.attr("χ") = ProjectionTag{};

  m.def(
      "A",
      [](nb::handle chain, nb::handle species) -> nb::object {
        const bool kleene = nb::isinstance<KleeneTag>(species);
        if (!species.is_none() && !kleene && !nb::isinstance<BooleTag>(species))
          throw nb::type_error("the species is Boole or Kleene");
        if (nb::isinstance<pst::Chain<bool>>(chain)) {
          if (kleene)
            throw nb::type_error("a K₃-valued set over 𝔹 is not bound; use K3");
          return nb::cast(pst::universe<bool, Boole>());
        }
        if (nb::isinstance<pst::Chain<Ternary>>(chain))
          return kleene ? nb::cast(pst::universe<Ternary, Kleene>())
                        : nb::cast(pst::universe<Ternary, Boole>());
        throw nb::type_error(
            "𝔸(chain): the chain is B or K3 (ℕ is the ℤ slice)");
      },
      nb::arg("chain"), nb::arg("species") = nb::none(),
      "𝔸(chain[, species]): the universe over a truth chain, valued in 𝔹 by "
      "default or in Kleene (𝔸(K3, Kleene)).");
  m.def(
      "Ø",
      [](nb::handle chain, nb::handle species) -> nb::object {
        const bool kleene = nb::isinstance<KleeneTag>(species);
        if (!species.is_none() && !kleene && !nb::isinstance<BooleTag>(species))
          throw nb::type_error("the species is Boole or Kleene");
        if (nb::isinstance<pst::Chain<bool>>(chain)) {
          if (kleene)
            throw nb::type_error("a K₃-valued set over 𝔹 is not bound; use K3");
          return nb::cast(pst::empty<bool, Boole>());
        }
        if (nb::isinstance<pst::Chain<Ternary>>(chain))
          return kleene ? nb::cast(pst::empty<Ternary, Kleene>())
                        : nb::cast(pst::empty<Ternary, Boole>());
        throw nb::type_error("Ø(chain): the chain is B or K3");
      },
      nb::arg("chain"), nb::arg("species") = nb::none(),
      "Ø(chain[, species]): the empty set over a truth chain.");
  m.def(
      "η",
      [](nb::handle v) -> nb::object {
        if (nb::isinstance<nb::bool_>(v))
          return nb::cast(pst::point<bool, Boole>(nb::cast<bool>(v)));
        if (nb::isinstance<Ternary>(v))
          return nb::cast(pst::point<Ternary, Boole>(nb::cast<Ternary>(v)));
        throw nb::type_error("η(v): v is a bool (𝔹) or a Ternary (K₃)");
      },
      nb::arg("v"), "η(v): the point {v}, over the chain v belongs to.");
  m.def(
      "exists",
      [](nb::handle s, nb::handle p) {
        return on_set_and_datum<Exists>(s, p, "exists");
      },
      nb::arg("S"), nb::arg("P"),
      "exists(S, P) = ⋁ (χ_S ∧ P), in S's species: not empty.");
  m.def(
      "forall",
      [](nb::handle s, nb::handle p) {
        return on_set_and_datum<Forall>(s, p, "forall");
      },
      nb::arg("S"), nb::arg("P"),
      "forall(S, P) over a decidable S: S == (S | P), the library's own ∀; an "
      "L-valued S is refused (see #980), quantify over S.cut(level).");
  m.attr("any") = m.attr("exists");
  m.attr("all") = m.attr("forall");
  m.def(
      "runs",
      [](nb::handle s) -> nb::object {
        if (nb::isinstance<pst::Set<bool, Boole>>(s) ||
            nb::isinstance<pst::Set<Ternary, Boole>>(s) ||
            nb::isinstance<pst::Set<Ternary, Kleene>>(s))
          return s.attr("runs")();
        throw nb::type_error("runs(S): S a set over 𝔹 or K₃");
      },
      nb::arg("S"), "runs(S): the maximal runs of membership, (lo, hi) pairs.");
  m.def(
      "lift",
      [](const pst::Set<Ternary, Boole>& s) { return pst::lift<Ternary>(s); },
      nb::arg("S"), "lift(S): a decidable set over K₃ valued in K₃.");
}
