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
 * the normal form @c runs.  Every handle is a run list (@c order:runs), the
 * library's normal form of these sets, valued in its species; every operator
 * and every query is the library's, on it.
 * Two species of one carrier mix in their join (𝔹 ↪ K₃): a Boolean set beside
 * a K₃-valued one is met, joined and compared in K₃.  Different carriers do not
 * mix.  The @b canonical ring and field are bound beside them (#1019): @c u64
 * (ℤ/2⁶⁴: + − · wrap, // and % by a nonzero word) and @c 𝔽ₚ (@c Fp,
 * @c Modular<2⁶⁴ − 59>: + − · /, no order), with their universes @c A(u64)
 * and @c A(𝔽ₚ); an element equals the int it represents.  Reduction stays in
 * C++; Python holds handles.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/string.h>

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <functional>
#include <limits>
#include <optional>
#include <string>
#include <type_traits>  // std::type_identity: a constant's meeting with a carrier
#include <utility>
#include <variant>

import dedekind.category;     // Ternary, Boole, Kleene, IsPst, the step
import dedekind.morphologies; // 𝔽ₚ, the canonical prime field
import dedekind.order;        // IsDense
import dedekind.sequences;    // the chains' orbit
import dedekind.sets;         // Cardinality, ℵ_0, finite_cardinality

namespace nb = nanobind;

namespace {
using dedekind::category::Boole;
using dedekind::category::classifier_logic_t;
using dedekind::category::cover;
using dedekind::category::HasCoveringStep;
using dedekind::category::HasNNOStep;
using dedekind::category::IsFiniteChain;
using dedekind::category::IsOckhamAlgebra;
using dedekind::category::IsPst;
using dedekind::category::join_logic_t;
using dedekind::category::Kleene;
using dedekind::category::Predecessor;
using dedekind::category::Successor;
using dedekind::category::Ternary;
using dedekind::order::ext;
using dedekind::order::Interval;
using dedekind::order::IsDense;
using dedekind::order::Runs;
using dedekind::order::steps;
// The product former 𝔸<A×B> | rel is order's, not found by ADL on sets types.
using dedekind::order::operator|;
using dedekind::sets::Cardinality;
using dedekind::sets::finite_cardinality;
using dedekind::sets::π;
using dedekind::sets::ℵ_0;
using dedekind::sets::𝔸;

// ── Pst: the bounded chains (#1001, paper §3) ────────────────────────────────
//
// Pst := Jlt ∩ Chain: the truth objects the sets are valued in (𝔹, K₃), and
// ℕ's proxy, a bounded chain in the same shape whose ⊤ is ℵ₀ (the memory
// boundary), not a truth object.  A chain is its endpoints and its step; the
// step is read twice --- total and saturating (the algebra side, @c Successor)
// and partial (the coalgebra side, @c cover, nothing at ⊤) --- and the chain's
// classification is whatever the C++ concepts decide.
namespace pst {

/** @brief A Pst chain by carrier: its name, endpoints and size.  The truth
 *  chains take their endpoints from their species (@c sets:boundaries); ℕ's
 *  proxy has ℵ₀ for its top.  @tparam C the carrier. */
template <typename C>
struct Chain;
template <>
struct Chain<bool> {
  static constexpr const char* name = "𝔹";
  static constexpr bool bottom = dedekind::sets::chain_bottom<bool>();
  static constexpr bool top = dedekind::sets::chain_top<bool>();
  static constexpr Cardinality cardinality = finite_cardinality(2);
};
template <>
struct Chain<Ternary> {
  static constexpr const char* name = "K₃";
  static constexpr Ternary bottom = dedekind::sets::chain_bottom<Ternary>();
  static constexpr Ternary top = dedekind::sets::chain_top<Ternary>();
  static constexpr Cardinality cardinality = finite_cardinality(3);
};
template <>
struct Chain<Cardinality> {
  static constexpr const char* name = "ℕ";
  static constexpr Cardinality bottom = finite_cardinality(0);
  static constexpr Cardinality top = Cardinality{ℵ_0{}};
  static constexpr Cardinality cardinality = top;
};

/** @brief Whether @f$S(\top) = \top@f$: the step's posture at the top.
 *  @tparam C the carrier. */
template <HasNNOStep C>
constexpr bool saturates() {
  return Successor<C>{}(Chain<C>::top) == Chain<C>::top;
}

static_assert(IsPst<bool> && IsPst<Ternary> && !IsPst<Cardinality>,
              "𝔹 and K₃ are truth objects; ℕ is a chain of the same shape.");
static_assert(saturates<bool>() && saturates<Ternary>() &&
                  saturates<Cardinality>(),
              "the three chains saturate at ⊤.");

// A Python set over a truth chain is a run list (order:runs) valued in its
// species: the normal form of every set the grammar builds, materialised.
// Every operator and every query is the library's own on it.

/** @brief A truth chain the queries can read: a Pst carrier that is finite by
 *  its representation and has the covering step.  @tparam C the chain. */
template <typename C>
concept IsTruthChain = IsPst<C> && IsFiniteChain<C> && HasCoveringStep<C>;

/** @brief A set over the chain @c C valued in @c L, as Python holds it. */
template <IsTruthChain C, IsOckhamAlgebra L>
using Set = Runs<C, L>;
/** @brief A datum of the grammar (@c π @c > @c v, @c π @c == @c v) over @c C:
 *  a two-valued set waiting for a former, a type of its own so that a set is
 *  never taken for a where-clause.  @tparam C the chain. */
template <IsTruthChain C>
struct Datum {
  Runs<C> set;
  constexpr bool operator()(const C& x) const { return set(x); }
};

/** @brief The data @c π @c ⋈ @c v.  @tparam C the chain.  @param v the
 *  value. */
template <IsTruthChain C>
Datum<C> above(const C& v) {
  return {Runs<C>(steps(𝔸<C>{} | (π > v)))};
}
template <IsTruthChain C>
Datum<C> at_least(const C& v) {
  return {Runs<C>(steps(𝔸<C>{} | (π >= v)))};
}
template <IsTruthChain C>
Datum<C> below(const C& v) {
  return {Runs<C>(steps(𝔸<C>{} | (π < v)))};
}
template <IsTruthChain C>
Datum<C> at_most(const C& v) {
  return {Runs<C>(steps(𝔸<C>{} | (π <= v)))};
}
template <IsTruthChain C>
Datum<C> equal_to(const C& v) {
  return {Runs<C>(steps(Interval<C>::point(v)))};
}
/** @brief The point @c η(v), a decidable set. */
template <IsTruthChain C>
Set<C, Boole> point(const C& v) {
  return Set<C, Boole>(steps(Interval<C>::point(v)));
}

/** @brief The former @c S @c | @c P: the library's meet of the set and the
 *  datum, materialised. */
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, L> former(const Set<C, L>& s, const Datum<C>& d) {
  return ext(s & d.set);
}
/** @brief The α-cut @f$\{\chi \ge \ell\}@f$, a decidable set. */
template <IsTruthChain C, IsOckhamAlgebra L>
Set<C, Boole> cut(const Set<C, L>& s, typename L::Ω level) {
  return Set<C, Boole>(dedekind::order::alpha_cut(steps(s), level));
}

}  // namespace pst

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
        it.current = cover(value);
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
            return to_py<C>(Successor<C>{}(from_py_as<C>(x)));
          },
          nb::arg("x"),
          "S(x): the successor, total and saturating at ⊤ (the algebra side).")
      .def(
          "pred",
          [](const Ch&, nb::handle x) {
            return to_py<C>(Predecessor<C>{}(from_py_as<C>(x)));
          },
          nb::arg("x"),
          "P(x): the predecessor, total and saturating at ⊥ (the monus on ℕ).")
      .def(
          "cover",
          [](const Ch&, nb::handle x) -> nb::object {
            const auto c = cover(from_py_as<C>(x));
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
          "is_truth_object", [](const Ch&) { return IsPst<C>; },
          "IsPst<C>: whether the chain is a truth object (carries ∧, ∨, ¬).")
      // FIXME(#1004): IsDiscrete is not exported yet; read as !is_dense.
      .def_prop_ro(
          "is_dense", [](const Ch&) { return IsDense<C>; },
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
              return to_py<C>(
                  dedekind::sequences::SuccessorOrbit<C>{Ch::bottom}.at(
                      static_cast<std::size_t>(i)));
            } else {
              const auto n = static_cast<long long>(
                  std::get<dedekind::sets::ExtensionalCardinal<>>(
                      Ch::cardinality)
                      .value);
              if (i < 0) i += n;
              if (i < 0 || i >= n)
                throw nb::index_error("position outside the chain");
              return to_py<C>(
                  dedekind::sequences::SuccessorOrbit<C>{Ch::bottom}.at(
                      static_cast<std::size_t>(i)));
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
/** @brief The grammar's variables @c π (also written @c χ), @c π1, @c π2:
 *  @c π @c > @c v builds a datum over the truth chain the value belongs to;
 *  any other comparison or an arithmetic builds a term, bound by the former. */
struct ProjectionTag {
  unsigned slot = 0;
};

/** @brief The runs of a decidable set, written: Ø, {U}, [U, ⊤], ∪ between. */
template <typename C>
std::string written_runs(const pst::Set<C, Boole>& s) {
  std::string out;
  for (const auto& r : dedekind::order::runs(s)) {
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

/** @brief The lattice and the queries over two species of one carrier, the
 *  result valued in their join (a Boolean set beside a K₃-valued one is
 *  compared, met and joined in K₃; one species is its own join).  @tparam C the
 * chain.  @tparam L1 the class's species.  @tparam L2 the other operand's. */
template <typename C, typename L1, typename L2>
void bind_mixed(nb::class_<pst::Set<C, L1>>& cls) {
  using S = pst::Set<C, L1>;
  using O = pst::Set<C, L2>;
  cls.def(
         "__and__", [](const S& a, const O& b) { return ext(a & b); },
         "A & B: the meet, in the species' join.")
      .def(
          "__or__", [](const S& a, const O& b) { return ext(a | b); },
          "A | B: the join.")
      .def(
          "__xor__", [](const S& a, const O& b) { return ext(a ^ b); },
          "A ^ B: the symmetric difference.")
      .def(
          "__eq__", [](const S& a, const O& b) { return nb::cast(a == b); },
          nb::is_operator(),
          "A == B: ⋀ (χ_A ⇔ χ_B), read off the run lists; Unknown is a "
          "verdict on a K₃-valued set.")
      .def(
          "__ne__",
          [](const S& a, const O& b) {
            return nb::cast(join_logic_t<L1, L2>::RFL(a == b));
          },
          nb::is_operator())
      .def(
          "__le__",
          [](const S& a, const O& b) { return nb::cast((a & b) == a); },
          nb::is_operator(), "A <= B: A ⊆ B, as (A ∩ B) = A.")
      .def(
          "__ge__",
          [](const S& a, const O& b) { return nb::cast((b & a) == b); },
          nb::is_operator());
}

template <typename C, typename L>
nb::class_<pst::Set<C, L>> bind_set(nb::module_& m, const char* cls,
                                    const char* doc) {
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
          "__or__",
          [](const S& s, const pst::Datum<C>& d) { return pst::former(s, d); },
          "S | (π ⋈ v): the former, {x ∈ S | P(x)}.")
      .def(
          "__invert__", [](const S& a) { return ext(~a); },
          "~A: the complement, the species' reflection pointwise.")
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
            return pst::Set<C, Boole>(
                dedekind::order::fibre(steps(s), from_py_as<Ω>(level)));
          },
          nb::arg("level"),
          "The fibre {x | χ(x) == level}: η(level) pulled back along χ "
          "(preimage), a decidable set.");
  if constexpr (std::same_as<L, Boole>) {
    set.def(
           "runs",
           [](const S& s) {
             nb::list out;
             for (const auto& r : dedekind::order::runs(s))
               out.append(nb::make_tuple(to_py<C>(r.lo), to_py<C>(r.hi)));
             return out;
           },
           "The maximal runs of membership along the chain, as (lo, hi) pairs: "
           "the normal form, read off the chain.")
        .def("__repr__", [](const S& s) { return written_runs<C>(s); });
    if constexpr (std::same_as<C, Ternary>)
      set.def(
          "lift",
          [](const S& s) { return ext(dedekind::sets::lift_to<Kleene>(s)); },
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
  bind_mixed<C, L, L>(set);
  return set;
}

/** @brief Sugar on a truth chain: the grammar's sets with the chain as the
 *  universe, @c K3.above(U) for @c 𝔸(K3) @c | @c (π @c > @c U). */
template <typename C>
void bind_chain_sets(nb::class_<pst::Chain<C>>& ch) {
  using Ch = pst::Chain<C>;
  ch.def_prop_ro(
        "all", [](const Ch&) { return pst::Set<C, Boole>::universe(); },
        "𝔸: the universe, every element.")
      .def_prop_ro(
          "none", [](const Ch&) { return pst::Set<C, Boole>::empty(); },
          "Ø: the empty set.")
      .def_prop_ro(
          "identity",
          [](const Ch&) {
            return pst::Set<C, classifier_logic_t<C>>(
                dedekind::order::IdentitySteps<C>{});
          },
          "χ(x) = x, valued in the chain's own species: the simplest set with "
          "every level inhabited.")
      .def(
          "above",
          [](const Ch&, nb::handle v) {
            return pst::former(pst::Set<C, Boole>::universe(),
                               pst::above<C>(from_py_as<C>(v)));
          },
          nb::arg("v"), "{x | x > v}: 𝔸(chain) | (π > v).")
      .def(
          "at_least",
          [](const Ch&, nb::handle v) {
            return pst::former(pst::Set<C, Boole>::universe(),
                               pst::at_least<C>(from_py_as<C>(v)));
          },
          nb::arg("v"), "{x | x >= v}.")
      .def(
          "below",
          [](const Ch&, nb::handle v) {
            return pst::former(pst::Set<C, Boole>::universe(),
                               pst::below<C>(from_py_as<C>(v)));
          },
          nb::arg("v"), "{x | x < v}.")
      .def(
          "at_most",
          [](const Ch&, nb::handle v) {
            return pst::former(pst::Set<C, Boole>::universe(),
                               pst::at_most<C>(from_py_as<C>(v)));
          },
          nb::arg("v"), "{x | x <= v}.")
      .def(
          "point",
          [](const Ch&, nb::handle v) {
            return pst::point<C>(from_py_as<C>(v));
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
    return dedekind::order::exists(steps(s & d.set));  // ⋁ (χ_S ∧ P)
  }
};
struct Forall {
  template <typename C, typename L>
    requires std::same_as<L, Boole>
  static auto apply(const pst::Set<C, L>& s, const pst::Datum<C>& d) {
    return s == (s & d.set);  // S ⊆ P
  }
};

// ── The canonical ring and field ────────────────────────────────────────────
//
// The two universes a Python user meets first, each as large as a machine word
// allows: u64, the ring ℤ/2⁶⁴ (+ − · wrap; // and % by a nonzero word), and
// 𝔽ₚ, the prime field p = 2⁶⁴ − 59 (+ − · /).  A Python int enters u64 by the
// checked cast and 𝔽ₚ by the quotient map; 𝔽ₚ shows its symmetric
// representative.  The classification is the C++ concepts' verdict.

using Fp = dedekind::morphologies::𝔽ₚ;
using dedekind::category::IsCommutativeRing;
using dedekind::category::IsField;
using std::uint64_t;
static_assert(
    IsCommutativeRing<uint64_t, std::plus<uint64_t>,
                      std::multiplies<uint64_t>> &&
        !IsField<uint64_t, std::plus<uint64_t>, std::multiplies<uint64_t>> &&
        IsField<Fp, std::plus<Fp>, std::multiplies<Fp>>,
    "u64 is a ring and not a field; 𝔽ₚ is a field.");

/** @brief An element of ℤ/2⁶⁴: the machine word, whose + − · wrap. */
struct Word {
  uint64_t v;
  friend bool operator==(const Word&, const Word&) = default;
  friend auto operator<=>(const Word&, const Word&) = default;
};

[[noreturn]] void zero_division(const char* what) {
  PyErr_SetString(PyExc_ZeroDivisionError, what);
  throw nb::python_error();
}
nb::object py_int(uint64_t v) {
  return nb::steal(PyLong_FromUnsignedLongLong(v));
}
bool is_py_int(nb::handle h) {
  return PyLong_Check(h.ptr()) && !PyBool_Check(h.ptr());
}
/** @brief A Python int into the word, by the checked cast; a word as itself.
 */
Word word_of(nb::handle h) {
  if (nb::isinstance<Word>(h)) return nb::cast<Word>(h);
  if (!is_py_int(h)) throw nb::type_error("a u64 is made from an int");
  const unsigned long long v = PyLong_AsUnsignedLongLong(h.ptr());
  if (PyErr_Occurred() != nullptr) {
    PyErr_Clear();
    throw nb::value_error("the word cannot hold it: u64 is [0, 2⁶⁴)");
  }
  return Word{v};
}
/** @brief A Python int into 𝔽ₚ, by the quotient map n ↦ n mod p; an element
 *  as itself. */
Fp residue_of(nb::handle h) {
  if (nb::isinstance<Fp>(h)) return nb::cast<Fp>(h);
  if (!is_py_int(h))
    throw nb::type_error("an element of 𝔽ₚ is made from an int");
  const nb::object p = py_int(Fp::modulus);
  const nb::object r = nb::steal(PyNumber_Remainder(h.ptr(), p.ptr()));
  if (!r.is_valid()) throw nb::python_error();
  return Fp{PyLong_AsUnsignedLongLong(r.ptr())};  // Python's % is in [0, p)
}
/** @brief The symmetric representative of an element of 𝔽ₚ, in
 *  [−(p−1)/2, (p−1)/2]. */
nb::object representative(const Fp& x) {
  if (x.value <= (Fp::modulus - 1) / 2) return py_int(x.value);
  return nb::steal(
      PyLong_FromLongLong(-static_cast<long long>(Fp::modulus - x.value)));
}
/** @brief Whether a Python value is an element of the universe: an element
 *  of the carrier, or an int that is one of its representatives. */
bool in_u64(nb::handle h) {
  if (nb::isinstance<Word>(h)) return true;
  if (!is_py_int(h)) return false;
  (void)PyLong_AsUnsignedLongLong(h.ptr());
  const bool fits = PyErr_Occurred() == nullptr;
  PyErr_Clear();
  return fits;
}
bool in_fp(nb::handle h) {
  if (nb::isinstance<Fp>(h)) return true;
  if (!is_py_int(h)) return false;
  const int same = PyObject_RichCompareBool(representative(residue_of(h)).ptr(),
                                            h.ptr(), Py_EQ);
  if (same < 0) throw nb::python_error();
  return same == 1;
}

// ── The term surface: terms built in Python, checked when bound ──────────────
//
// A term names no carrier; A(carrier) | term binds it, every constant cast into
// the carrier and every operator admitted there or refused (TypeError).  The
// tree is sets::Term / sets::Pred, read by the library's own algebras; the
// binding adds the constants (Python objects, cast when bound), the handles and
// their spelling.

using dedekind::sets::Arith;
using dedekind::sets::Comprehension;
using dedekind::sets::Rel;
using dedekind::sets::spelling;

/** @brief A constant as Python holds it: an int, a bool, a Ternary, a u64 or
 *  an element of 𝔽ₚ.  It meets the carrier when the term is bound: the
 *  checked cast into u64, the quotient map into 𝔽ₚ, a bool on 𝔹. */
struct PyConst {
  nb::object o;
  PyConst() : o(nb::int_(0)) {}
  explicit PyConst(nb::handle h) : o(nb::borrow(h)) {
    if (!(is_py_int(h) || PyBool_Check(h.ptr()) || nb::isinstance<Ternary>(h) ||
          nb::isinstance<Word>(h) || nb::isinstance<Fp>(h)))
      throw nb::type_error(
          "a constant is an int, a bool, a Ternary, a u64 or an element of 𝔽ₚ");
  }
  friend bool operator==(const PyConst& a, const PyConst& b) {
    const int r = PyObject_RichCompareBool(a.o.ptr(), b.o.ptr(), Py_EQ);
    if (r < 0) throw nb::python_error();
    return r == 1;
  }
};
/** @brief The constant's meeting with each carrier, found by the library's
 *  @c carrier_cast through ADL. */
uint64_t carrier_cast(std::type_identity<uint64_t>, const PyConst& k) {
  return word_of(k.o).v;
}
Fp carrier_cast(std::type_identity<Fp>, const PyConst& k) {
  return residue_of(k.o);
}
bool carrier_cast(std::type_identity<bool>, const PyConst& k) {
  if (!PyBool_Check(k.o.ptr()))
    throw nb::type_error("a constant on 𝔹 is a bool");
  return k.o.ptr() == Py_True;
}
Ternary carrier_cast(std::type_identity<Ternary>, const PyConst& k) {
  if (!nb::isinstance<Ternary>(k.o))
    throw nb::type_error("a constant on K₃ is a Ternary");
  return nb::cast<Ternary>(k.o);
}
using RTerm = dedekind::sets::Term<PyConst>;
using RPred = dedekind::sets::Pred<PyConst>;

RTerm as_term(const ProjectionTag& p) { return RTerm::proj(p.slot); }
RTerm as_term(const RTerm& t) { return t; }
/** @brief An operand as a term: a term, a variable, or a constant. */
RTerm term_of(nb::handle h) {
  if (nb::isinstance<RTerm>(h)) return nb::cast<RTerm>(h);
  if (nb::isinstance<ProjectionTag>(h))
    return as_term(nb::cast<ProjectionTag>(h));
  return RTerm::constant(PyConst{h});
}
const char* spelling(Rel r) {
  switch (r) {
    case Rel::Lt:
      return "<";
    case Rel::Le:
      return "<=";
    case Rel::Gt:
      return ">";
    case Rel::Ge:
      return ">=";
    case Rel::Eq:
      return "==";
    case Rel::Ne:
      return "!=";
  }
  return "?";
}
std::string show(const RTerm& t) {
  switch (t.kind) {
    case RTerm::Kind::Proj:
      return t.slot == 0 ? "π" : "π" + std::to_string(t.slot);
    case RTerm::Kind::Const:
      return nb::cast<std::string>(nb::repr(t.value.o));
    case RTerm::Kind::Bin:
      return "(" + show(*t.a) + " " + spelling(t.op) + " " + show(*t.b) + ")";
  }
  return "?";
}
std::string show(const RPred& p) {
  switch (p.kind) {
    case RPred::Kind::Cmp:
      return show(*p.a) + " " + spelling(p.rel) + " " + show(*p.b);
    case RPred::Kind::And:
      return "(" + show(*p.p) + ") & (" + show(*p.q) + ")";
    case RPred::Kind::Or:
      return "(" + show(*p.p) + ") | (" + show(*p.q) + ")";
  }
  return "?";
}

/** @brief The arithmetic of terms on a class whose instances are terms (a
 *  variable, a term): each operator and its reflection build a term. */
template <typename Self, typename Cls>
void def_arithmetic(Cls& cls) {
  const auto bin = [](Arith op) {
    return [op](const Self& a, nb::handle b) {
      return RTerm::bin(op, as_term(a), term_of(b));
    };
  };
  const auto rbin = [](Arith op) {
    return [op](const Self& a, nb::handle b) {
      return RTerm::bin(op, term_of(b), as_term(a));
    };
  };
  cls.def("__add__", bin(Arith::Add), nb::is_operator())
      .def("__radd__", rbin(Arith::Add), nb::is_operator())
      .def("__sub__", bin(Arith::Sub), nb::is_operator())
      .def("__rsub__", rbin(Arith::Sub), nb::is_operator())
      .def("__mul__", bin(Arith::Mul), nb::is_operator())
      .def("__rmul__", rbin(Arith::Mul), nb::is_operator())
      .def("__truediv__", bin(Arith::Div), nb::is_operator())
      .def("__rtruediv__", rbin(Arith::Div), nb::is_operator())
      .def("__floordiv__", bin(Arith::FloorDiv), nb::is_operator())
      .def("__rfloordiv__", rbin(Arith::FloorDiv), nb::is_operator())
      .def("__mod__", bin(Arith::Mod), nb::is_operator())
      .def("__rmod__", rbin(Arith::Mod), nb::is_operator())
      .def("__neg__", [](const Self& a) {
        return RTerm::bin(Arith::Sub, RTerm::constant(PyConst{}), as_term(a));
      });
}
/** @brief The comparisons of a term: each builds a predicate. */
template <typename Self, typename Cls>
void def_comparisons(Cls& cls) {
  const auto cmp = [](Rel r) {
    return [r](const Self& a, nb::handle b) {
      return RPred::cmp(r, as_term(a), term_of(b));
    };
  };
  cls.def("__lt__", cmp(Rel::Lt), nb::is_operator())
      .def("__le__", cmp(Rel::Le), nb::is_operator())
      .def("__gt__", cmp(Rel::Gt), nb::is_operator())
      .def("__ge__", cmp(Rel::Ge), nb::is_operator())
      .def("__eq__", cmp(Rel::Eq), nb::is_operator())
      .def("__ne__", cmp(Rel::Ne), nb::is_operator());
}

/** @brief The former on a truth chain for a predicate built at run time:
 *  bound to the chain, then read at its two or three elements. */
template <typename C, typename L>
auto former_runtime(const pst::Set<C, L>& s, const RPred& p) {
  const auto c = 𝔸<C>{} | p;  // every operator and constant checked here
  pst::Set<C, Boole> acc = pst::Set<C, Boole>::empty();
  for (C x = dedekind::sets::chain_bottom<C>();; x = Successor<C>{}(x)) {
    if (c(x)) acc = ext(acc | pst::point<C>(x));
    if (x == dedekind::sets::chain_top<C>()) break;
  }
  return ext(s & acc);
}

/** @brief The sets a term cuts out of a canonical universe: the set
 *  @c A(carrier) @c | @c pred and the relation @c A(carrier) @c * @c A(carrier)
 *  @c | @c pred, each a comprehension holding the bound predicate, read
 *  pointwise.  @tparam C the carrier.  @tparam ToC a Python value into @c C,
 *  raising where it is none. */
template <typename C, typename ToC>
void bind_term_sets(nb::module_& m,
                    nb::class_<𝔸<C, Boole, dedekind::sets::Finite>>& universe,
                    const char* name, const char* set_cls, const char* pair_cls,
                    const char* rel_cls, ToC to_c) {
  using U = 𝔸<C, Boole, dedekind::sets::Finite>;
  using S = Comprehension<U, RPred>;
  using UU = decltype(U{} * U{});
  using R = decltype(UU{} | std::declval<RPred>());
  const std::string shown = std::string("𝔸(") + name + ")";
  const auto pair_of = [to_c](nb::handle h) {
    if (!nb::isinstance<nb::tuple>(h) || nb::len(h) != 2)
      throw nb::type_error("a point of the product is a pair (x, y)");
    const nb::tuple t = nb::borrow<nb::tuple>(h);
    return std::pair<C, C>{to_c(t[0]), to_c(t[1])};
  };
  const auto holds = [](auto convert, const auto& s, nb::handle h) {
    try {
      return static_cast<bool>(s(convert(h)));
    } catch (const nb::builtin_exception&) {
      return false;  // not a representative: not a member
    }
  };
  universe
      .def(
          "__or__", [](const U& u, const RPred& p) { return u | p; },
          nb::is_operator(),
          "𝔸(carrier) | pred: the former; every operator and constant of the "
          "term is checked against the carrier now.")
      .def(
          "__mul__", [](const U& a, const U& b) { return a * b; },
          nb::is_operator(), "𝔸 × 𝔸: the product universe.");
  nb::class_<S>(m, set_cls, "A set cut out of the universe by a term.")
      .def(
          "__contains__",
          [holds, to_c](const S& s, nb::handle h) { return holds(to_c, s, h); })
      .def(
          "__call__", [to_c](const S& s, nb::handle h) { return s(to_c(h)); },
          nb::arg("x"), "χ(x), x an element or a representative.")
      .def(
          "__and__",
          [](const S& a, const S& b) {
            return U{} | RPred::conj(a.predicate, b.predicate);
          },
          nb::is_operator())
      .def(
          "__or__",
          [](const S& a, const S& b) {
            return U{} | RPred::disj(a.predicate, b.predicate);
          },
          nb::is_operator())
      .def("__invert__", [](const S& a) { return U{} | a.predicate.negated(); })
      .def_prop_ro("universe", [](const S&) { return U{}; })
      .def("__repr__",
           [shown](const S& s) { return shown + " | " + show(s.predicate); });
  nb::class_<UU>(m, pair_cls, "𝔸 × 𝔸: the product universe.")
      .def(
          "__or__", [](const UU& u, const RPred& p) { return u | p; },
          nb::is_operator(), "𝔸 × 𝔸 | pred: the relation.")
      .def("__contains__",
           [holds, pair_of](const UU& u, nb::handle h) {
             return holds(pair_of, u, h);
           })
      .def("__repr__", [shown](const UU&) { return shown + " × " + shown; });
  nb::class_<R>(m, rel_cls, "A relation cut out of the product by a term.")
      .def("__contains__",
           [holds, pair_of](const R& r, nb::handle h) {
             return holds(pair_of, r, h);
           })
      .def(
          "__call__",
          [pair_of](const R& r, nb::handle h) { return r(pair_of(h)); },
          nb::arg("xy"), "χ(x, y) at a pair.")
      .def(
          "__and__",
          [](const R& a, const R& b) {
            return UU{} | RPred::conj(a.predicate, b.predicate);
          },
          nb::is_operator())
      .def(
          "__or__",
          [](const R& a, const R& b) {
            return UU{} | RPred::disj(a.predicate, b.predicate);
          },
          nb::is_operator())
      .def("__invert__",
           [](const R& a) { return UU{} | a.predicate.negated(); })
      .def("__repr__", [shown](const R& r) {
        return shown + " × " + shown + " | " + show(r.predicate);
      });
}
}  // namespace

NB_MODULE(_pst, m) {
  // A refusal when a term is bound (an operator not admitted on the carrier, a
  // slot the points have not) is Python's TypeError.
  nb::register_exception_translator([](const std::exception_ptr& p, void*) {
    try {
      std::rethrow_exception(p);
    } catch (const dedekind::sets::not_admitted& e) {
      PyErr_SetString(PyExc_TypeError, e.what());
    }
  });
  m.doc() =
      "Pst (paper §3): Pst = Jlt ∩ Chain.  The chains B (𝔹), K3 (Kleene's "
      "three truth values) and N (ℕ's proxy, ⊤ = ℵ₀), and the sets over 𝔹 and "
      "K₃ per the Lwv grammar: A(chain) | (π > v), Ø(chain), η(v), the lattice "
      "& | ^ ~, membership x in S / S(x), the queries == and <= in the set's "
      "species, exists / forall (any / all), and runs, the normal form.  Every "
      "handle is a run list, the library's normal form of these sets; every "
      "query is decided in C++ on it.";

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
  auto set_b = bind_set<bool, Boole>(m, "SetB", "A set over 𝔹, valued in 𝔹.");
  auto set_k3 = bind_set<Ternary, Boole>(
      m, "SetK3", "A set over K₃, valued in 𝔹 (decidable).");
  auto set_k3_kleene = bind_set<Ternary, Kleene>(
      m, "SetK3Kleene", "A set over K₃, valued in K₃: Unknown is a level.");
  bind_mixed<Ternary, Boole, Kleene>(set_k3);
  bind_mixed<Ternary, Kleene, Boole>(set_k3_kleene);
  bind_chain_sets<bool>(chain_b);
  bind_chain_sets<Ternary>(chain_k3);
  m.attr("B") = pst::Chain<bool>{};
  m.attr("K3") = pst::Chain<Ternary>{};
  m.attr("N") = pst::Chain<Cardinality>{};

  // ── the variables and the terms ──
  auto projection = nb::class_<ProjectionTag>(
      m, "Projection",
      "The grammar's variables π (also χ), π1, π2.  π ⋈ v with v a bool or a "
      "Ternary is a datum over that truth chain; every other comparison, and "
      "the arithmetic + - * / // %, builds a term that A(carrier) | ... binds "
      "and checks.");
  // A comparison with a truth value on π is the truth chain's datum; any other
  // is a predicate built at run time.
  const auto datum_or_cmp = [](auto make, Rel r) {
    return [make, r](const ProjectionTag& p, nb::handle v) -> nb::object {
      if (p.slot == 0 && (PyBool_Check(v.ptr()) || nb::isinstance<Ternary>(v)))
        return make(v);
      return nb::cast(RPred::cmp(r, as_term(p), term_of(v)));
    };
  };
  projection
      .def("__gt__", datum_or_cmp(&above::make, Rel::Gt), nb::is_operator())
      .def("__ge__", datum_or_cmp(&at_least::make, Rel::Ge), nb::is_operator())
      .def("__lt__", datum_or_cmp(&below::make, Rel::Lt), nb::is_operator())
      .def("__le__", datum_or_cmp(&at_most::make, Rel::Le), nb::is_operator())
      .def("__eq__", datum_or_cmp(&equal_to::make, Rel::Eq), nb::is_operator())
      .def(
          "__ne__",
          [](const ProjectionTag& p, nb::handle v) {
            return RPred::cmp(Rel::Ne, as_term(p), term_of(v));
          },
          nb::is_operator())
      .def("__repr__", [](const ProjectionTag& p) { return show(as_term(p)); });
  def_arithmetic<ProjectionTag>(projection);
  m.attr("π") = ProjectionTag{0};
  m.attr("χ") = ProjectionTag{0};
  m.attr("π1") = ProjectionTag{1};
  m.attr("π2") = ProjectionTag{2};

  auto term = nb::class_<RTerm>(
      m, "Term",
      "A term of the grammar: a variable, a constant, or an operator on terms; "
      "its comparisons are predicates.  It names no carrier until a former "
      "binds it.");
  def_arithmetic<RTerm>(term);
  def_comparisons<RTerm>(term);
  term.def("__repr__", [](const RTerm& t) { return show(t); });
  nb::class_<RPred>(
      m, "Pred",
      "A predicate of the grammar: a comparison of two terms, or & | ~ of "
      "predicates (~ pushed to the comparisons).  A(carrier) | pred binds it.")
      .def(
          "__and__",
          [](const RPred& a, const RPred& b) { return RPred::conj(a, b); },
          nb::is_operator())
      .def(
          "__or__",
          [](const RPred& a, const RPred& b) { return RPred::disj(a, b); },
          nb::is_operator())
      .def("__invert__", [](const RPred& a) { return a.negated(); })
      .def("__repr__", [](const RPred& p) { return show(p); });

  // ── the canonical ring and field ──
  auto u64 = nb::class_<Word>(
      m, "u64",
      "The ring ℤ/2⁶⁴ (the machine word): + − · wrap, // and % by a nonzero "
      "word, ordered by its representatives 0 … 2⁶⁴ − 1.  u64(n) takes an int "
      "by the checked cast.");
  u64.def(
         "__init__", [](Word* w, nb::handle n) { new (w) Word{word_of(n)}; },
         nb::arg("n"))
      .def("__int__", [](const Word& a) { return py_int(a.v); })
      .def("__index__", [](const Word& a) { return py_int(a.v); })
      .def("__hash__", [](const Word& a) { return nb::hash(py_int(a.v)); })
      .def("__repr__",
           [](const Word& a) { return "u64(" + std::to_string(a.v) + ")"; })
      .def(
          "__add__",
          [](const Word& a, nb::handle b) { return Word{a.v + word_of(b).v}; },
          nb::is_operator())
      .def(
          "__radd__",
          [](const Word& a, nb::handle b) { return Word{word_of(b).v + a.v}; },
          nb::is_operator())
      .def(
          "__sub__",
          [](const Word& a, nb::handle b) { return Word{a.v - word_of(b).v}; },
          nb::is_operator())
      .def(
          "__rsub__",
          [](const Word& a, nb::handle b) { return Word{word_of(b).v - a.v}; },
          nb::is_operator())
      .def(
          "__mul__",
          [](const Word& a, nb::handle b) { return Word{a.v * word_of(b).v}; },
          nb::is_operator())
      .def(
          "__rmul__",
          [](const Word& a, nb::handle b) { return Word{word_of(b).v * a.v}; },
          nb::is_operator())
      .def("__neg__", [](const Word& a) { return Word{0 - a.v}; })
      .def(
          "__floordiv__",
          [](const Word& a, nb::handle b) {
            const uint64_t d = word_of(b).v;
            if (d == 0) zero_division("u64 // 0");
            return Word{a.v / d};
          },
          nb::is_operator())
      .def(
          "__mod__",
          [](const Word& a, nb::handle b) {
            const uint64_t d = word_of(b).v;
            if (d == 0) zero_division("u64 % 0");
            return Word{a.v % d};
          },
          nb::is_operator())
      .def(
          "__eq__", [](const Word& a, const Word& b) { return a == b; },
          nb::is_operator())
      .def(
          "__eq__",
          [](const Word& a, nb::handle b) {
            return is_py_int(b) && in_u64(b) && word_of(b) == a;
          },
          nb::is_operator(),
          "An int equals the word it represents: u64(5) == 5, and the hash "
          "is the int's.")
      .def(
          "__lt__", [](const Word& a, const Word& b) { return a < b; },
          nb::is_operator())
      .def(
          "__le__", [](const Word& a, const Word& b) { return a <= b; },
          nb::is_operator())
      .def(
          "__gt__", [](const Word& a, const Word& b) { return a > b; },
          nb::is_operator())
      .def(
          "__ge__", [](const Word& a, const Word& b) { return a >= b; },
          nb::is_operator());
  u64.attr("modulus") = nb::steal(PyNumber_Add(
      py_int(std::numeric_limits<uint64_t>::max()).ptr(), py_int(1).ptr()));
  u64.attr("is_ring") =
      nb::bool_(IsCommutativeRing<uint64_t, std::plus<uint64_t>,
                                  std::multiplies<uint64_t>>);
  u64.attr("is_field") = nb::bool_(
      IsField<uint64_t, std::plus<uint64_t>, std::multiplies<uint64_t>>);

  auto fp = nb::class_<Fp>(
      m, "Fp",
      "𝔽ₚ, the prime field p = 2⁶⁴ − 59 (the largest prime below 2⁶⁴): "
      "+ − · and / by a nonzero element; no order.  𝔽ₚ(n) takes any int by "
      "the quotient map n mod p, and int() gives the symmetric representative "
      "in [−(p−1)/2, (p−1)/2].  In Python, 𝔽ₚ is the name Fp.");
  fp.def(
        "__init__", [](Fp* x, nb::handle n) { new (x) Fp{residue_of(n)}; },
        nb::arg("n"))
      .def("__int__", &representative)
      .def("__hash__", [](const Fp& x) { return nb::hash(representative(x)); })
      .def("__repr__",
           [](const Fp& x) {
             return "𝔽ₚ(" + nb::cast<std::string>(nb::str(representative(x))) +
                    ")";
           })
      .def(
          "__add__",
          [](const Fp& a, nb::handle b) { return a + residue_of(b); },
          nb::is_operator())
      .def(
          "__radd__",
          [](const Fp& a, nb::handle b) { return residue_of(b) + a; },
          nb::is_operator())
      .def(
          "__sub__",
          [](const Fp& a, nb::handle b) { return a - residue_of(b); },
          nb::is_operator())
      .def(
          "__rsub__",
          [](const Fp& a, nb::handle b) { return residue_of(b) - a; },
          nb::is_operator())
      .def(
          "__mul__",
          [](const Fp& a, nb::handle b) { return a * residue_of(b); },
          nb::is_operator())
      .def(
          "__rmul__",
          [](const Fp& a, nb::handle b) { return residue_of(b) * a; },
          nb::is_operator())
      .def("__neg__", [](const Fp& a) { return Fp{0} - a; })
      .def(
          "__truediv__",
          [](const Fp& a, nb::handle b) {
            const Fp d = residue_of(b);
            if (d.value == 0) zero_division("𝔽ₚ / 0");
            return a / d;
          },
          nb::is_operator())
      .def(
          "__rtruediv__",
          [](const Fp& a, nb::handle b) {
            if (a.value == 0) zero_division("𝔽ₚ / 0");
            return residue_of(b) / a;
          },
          nb::is_operator())
      .def(
          "inverse",
          [](const Fp& a) {
            if (a.value == 0) zero_division("0 has no inverse in 𝔽ₚ");
            return a.inverse();
          },
          "The multiplicative inverse, a^(p−2).")
      .def(
          "__eq__", [](const Fp& a, const Fp& b) { return a == b; },
          nb::is_operator())
      .def(
          "__eq__",
          [](const Fp& a, nb::handle b) {
            return is_py_int(b) && in_fp(b) && residue_of(b) == a;
          },
          nb::is_operator(),
          "An int equals the element it represents: 𝔽ₚ(-1) == -1 but not "
          "p - 1, so the hash is the representative's.");
  fp.attr("modulus") = py_int(Fp::modulus);
  fp.attr("is_ring") =
      nb::bool_(IsCommutativeRing<Fp, std::plus<Fp>, std::multiplies<Fp>>);
  fp.attr("is_field") =
      nb::bool_(IsField<Fp, std::plus<Fp>, std::multiplies<Fp>>);
  const nb::object u64_type = u64;
  const nb::object fp_type = fp;

  auto universe_u64 = nb::class_<𝔸<uint64_t, Boole, dedekind::sets::Finite>>(
      m, "UniverseU64", "𝔸(u64): the ring ℤ/2⁶⁴ as a set.");
  universe_u64
      .def("__contains__", [](const 𝔸<uint64_t, Boole, dedekind::sets::Finite>&,
                              nb::handle x) { return in_u64(x); })
      .def("__call__", [](const 𝔸<uint64_t, Boole, dedekind::sets::Finite>&,
                          nb::handle x) { return in_u64(x); })
      .def_prop_ro(
          "cardinality",
          [u64_type](const 𝔸<uint64_t, Boole, dedekind::sets::Finite>&) {
            return nb::object(u64_type.attr("modulus"));
          })
      .def_prop_ro(
          "carrier",
          [u64_type](const 𝔸<uint64_t, Boole, dedekind::sets::Finite>&) {
            return u64_type;
          })
      .def("__repr__", [](const 𝔸<uint64_t, Boole, dedekind::sets::Finite>&) {
        return std::string("𝔸(u64)");
      });
  auto universe_fp = nb::class_<𝔸<Fp, Boole, dedekind::sets::Finite>>(
      m, "UniverseFp", "𝔸(𝔽ₚ): the field 𝔽ₚ as a set.");
  universe_fp
      .def("__contains__", [](const 𝔸<Fp, Boole, dedekind::sets::Finite>&,
                              nb::handle x) { return in_fp(x); })
      .def("__call__", [](const 𝔸<Fp, Boole, dedekind::sets::Finite>&,
                          nb::handle x) { return in_fp(x); })
      .def_prop_ro("cardinality",
                   [](const 𝔸<Fp, Boole, dedekind::sets::Finite>&) {
                     return py_int(Fp::modulus);
                   })
      .def_prop_ro("carrier",
                   [fp_type](const 𝔸<Fp, Boole, dedekind::sets::Finite>&) {
                     return fp_type;
                   })
      .def("__repr__", [](const 𝔸<Fp, Boole, dedekind::sets::Finite>&) {
        return std::string("𝔸(𝔽ₚ)");
      });

  m.def(
      "A",
      [u64_type, fp_type](nb::handle chain, nb::handle species) -> nb::object {
        if (chain.is(u64_type) || chain.is(fp_type)) {
          if (!species.is_none() && !nb::isinstance<BooleTag>(species))
            throw nb::type_error(
                "the ring u64 and the field 𝔽ₚ are sets valued in Boole");
          if (chain.is(u64_type))
            return nb::cast(𝔸<uint64_t, Boole, dedekind::sets::Finite>{});
          return nb::cast(𝔸<Fp, Boole, dedekind::sets::Finite>{});
        }
        const bool kleene = nb::isinstance<KleeneTag>(species);
        if (!species.is_none() && !kleene && !nb::isinstance<BooleTag>(species))
          throw nb::type_error("the species is Boole or Kleene");
        if (nb::isinstance<pst::Chain<bool>>(chain)) {
          if (kleene)
            throw nb::type_error("a K₃-valued set over 𝔹 is not bound; use K3");
          return nb::cast(pst::Set<bool, Boole>::universe());
        }
        if (nb::isinstance<pst::Chain<Ternary>>(chain))
          return kleene ? nb::cast(pst::Set<Ternary, Kleene>::universe())
                        : nb::cast(pst::Set<Ternary, Boole>::universe());
        throw nb::type_error(
            "𝔸(carrier): the carrier is B, K3, u64 or 𝔽ₚ (ℕ is the ℤ slice)");
      },
      nb::arg("chain"), nb::arg("species") = nb::none(),
      "𝔸(carrier[, species]): the universe over a truth chain, valued in 𝔹 "
      "by default or in Kleene (𝔸(K3, Kleene)); or the canonical ring 𝔸(u64) "
      "or field 𝔸(𝔽ₚ).");
  m.def(
      "Ø",
      [](nb::handle chain, nb::handle species) -> nb::object {
        const bool kleene = nb::isinstance<KleeneTag>(species);
        if (!species.is_none() && !kleene && !nb::isinstance<BooleTag>(species))
          throw nb::type_error("the species is Boole or Kleene");
        if (nb::isinstance<pst::Chain<bool>>(chain)) {
          if (kleene)
            throw nb::type_error("a K₃-valued set over 𝔹 is not bound; use K3");
          return nb::cast(pst::Set<bool, Boole>::empty());
        }
        if (nb::isinstance<pst::Chain<Ternary>>(chain))
          return kleene ? nb::cast(pst::Set<Ternary, Kleene>::empty())
                        : nb::cast(pst::Set<Ternary, Boole>::empty());
        throw nb::type_error("Ø(chain): the chain is B or K3");
      },
      nb::arg("chain"), nb::arg("species") = nb::none(),
      "Ø(chain[, species]): the empty set over a truth chain.");
  m.def(
      "η",
      [](nb::handle v) -> nb::object {
        if (nb::isinstance<nb::bool_>(v))
          return nb::cast(pst::point<bool>(nb::cast<bool>(v)));
        if (nb::isinstance<Ternary>(v))
          return nb::cast(pst::point<Ternary>(nb::cast<Ternary>(v)));
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
      [](const pst::Set<Ternary, Boole>& s) {
        return ext(dedekind::sets::lift_to<Kleene>(s));
      },
      nb::arg("S"), "lift(S): a decidable set over K₃ valued in K₃.");

  // ── the term surface on the canonical universes and the truth chains ──
  bind_term_sets<uint64_t>(m, universe_u64, "u64", "U64Set", "UniverseU64Pair",
                           "U64Relation",
                           [](nb::handle h) { return word_of(h).v; });
  bind_term_sets<Fp>(m, universe_fp, "𝔽ₚ", "FpSet", "UniverseFpPair",
                     "FpRelation", [](nb::handle h) { return residue_of(h); });
  set_b.def(
      "__or__",
      [](const pst::Set<bool, Boole>& s, const RPred& p) {
        return former_runtime(s, p);
      },
      nb::is_operator(), "S | pred: the former, the term checked on 𝔹.");
  set_k3.def(
      "__or__",
      [](const pst::Set<Ternary, Boole>& s, const RPred& p) {
        return former_runtime(s, p);
      },
      nb::is_operator(), "S | pred: the former, the term checked on K₃.");
  set_k3_kleene.def(
      "__or__",
      [](const pst::Set<Ternary, Kleene>& s, const RPred& p) {
        return former_runtime(s, p);
      },
      nb::is_operator(), "S | pred: the former, the term checked on K₃.");
}
