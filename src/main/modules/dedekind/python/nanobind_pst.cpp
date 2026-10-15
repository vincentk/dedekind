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
 * cover.  The @b sets over 𝔹 and K₃ are the chain fragment of the Lwv grammar:
 * generators
 * @c 𝔸(chain) / @c Ø(chain) / @c η(v), the former @c S @c | @c (π @c > @c v),
 * the lattice @c & @c | @c ^ @c ~, membership @c x @c in @c S / @c S(x), the
 * queries @c == and @c <= answering in the set's own species (@c Unknown is a
 * verdict), the quantifiers @c exists / @c forall (also @c any / @c all), and
 * the normal form @c runs.  Every handle over a truth chain is a run list
 * (@c order:runs), the library's normal form of these sets, valued in its
 * species; every operator and every query is the library's, on it.
 * Two species of one carrier mix in their join (𝔹 ↪ K₃): a Boolean set beside
 * a K₃-valued one is met, joined and compared in K₃.  Different carriers do not
 * mix.  The @b canonical ring and field are bound beside them (#1019): @c u64
 * (ℤ/2⁶⁴: + − · wrap, // and % by a nonzero word) and @c 𝔽ₚ (@c Fp,
 * @c Modular<2⁶⁴ − 59>: + − · /, no order), with their universes @c A(u64)
 * and @c A(𝔽ₚ); an element equals the int it represents.  The @b term
 * @b surface (#1006) builds terms over @c π / @c π_1 / @c π_2 that
 * @c A(carrier) @c | @c term and @c A(carrier) @c * @c A(carrier) @c | @c term
 * bind, every operator and constant checked then; those sets and relations
 * hold the bound term (a comprehension) and are read pointwise.  Reduction
 * stays in C++; Python holds handles.
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
#include <vector>

import dedekind.category;     // Ternary, Boole, Kleene, IsPst, the step
import dedekind.algebra;      // a field's division: the term grammar's rule
import dedekind.morphologies; // 𝔽ₚ, the canonical prime field
import dedekind.order;        // IsDense
import dedekind.sequences;    // the chains' orbit
import dedekind.sets;         // Cardinality, ℵ_0, finite_cardinality

namespace nb = nanobind;

namespace {
using dedekind::category::Boole;
using dedekind::category::classifier_logic_t;
using dedekind::category::cover;
using dedekind::category::HasNNOStep;
using dedekind::category::Identity;
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
using dedekind::order::IsTotallyOrdered;
using dedekind::order::Runs;
using dedekind::order::steps;
// The product former 𝔸<A×B> | rel is order's, not found by ADL on sets types.
using dedekind::order::operator|;
using dedekind::sets::Cardinality;
using dedekind::sets::finite_cardinality;
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
// Every operator and every query is the library's own on it; a predicate
// built at run time (the term surface below) is read at the chain's elements.

/** @brief A truth chain the queries can read: a Pst carrier that is finite by
 *  its representation and has the covering step.  @tparam C the chain. */
template <typename C>
concept IsTruthChain = IsPst<C> && IsFiniteChain<C> && HasNNOStep<C>;

/** @brief A set over the chain @c C valued in @c L, as Python holds it. */
template <IsTruthChain C, IsOckhamAlgebra L>
using Set = Runs<C, L>;
/** @brief The point @c η(v), a decidable set. */
template <IsTruthChain C>
Set<C, Boole> point(const C& v) {
  return Set<C, Boole>(steps(Interval<C>::point(v)));
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
      .def(
          "__eq__", [](const Ch&, const Ch&) { return true; },
          nb::is_operator(), "One chain per carrier: equal to itself.")
      .def("__hash__", [](const Ch&) { return nb::hash(nb::str(Ch::name)); })
      .def("__repr__", [](const Ch&) { return std::string(Ch::name); });
}

/** @brief The grammar's variables @c π, @c π_1, @c π_2: a
 *  comparison builds a predicate, an arithmetic a term, each bound and checked
 *  by the former. */
struct ProjectionTag {
  unsigned slot = 0;
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
std::string show(const RPred& p);  // mutual: a conditional's predicate
std::string show(const RTerm& t) {
  switch (t.kind) {
    case RTerm::Kind::Proj:
      return t.slot == 0 ? "π" : "π_" + std::to_string(t.slot);
    case RTerm::Kind::Const:
      return nb::cast<std::string>(nb::repr(t.value.o));
    case RTerm::Kind::Bin:
      return "(" + show(*t.a) + " " + spelling(t.op) + " " + show(*t.b) + ")";
    case RTerm::Kind::Cond:
      return "cond(" + show(*t.p) + ", " + show(*t.a) + ", " + show(*t.b) + ")";
  }
  return "?";
}
/** @brief Whether a Python value is a term of the grammar (a term or a
 *  variable), as opposed to a point. */
bool is_term(nb::handle h) {
  return nb::isinstance<RTerm>(h) || nb::isinstance<ProjectionTag>(h);
}
std::string show(const RPred& p) {
  switch (p.kind) {
    case RPred::Kind::Cmp:
      return show(*p.a) + " " + spelling(p.rel) + " " + show(*p.b);
    case RPred::Kind::And:
      return "(" + show(*p.p) + ") & (" + show(*p.q) + ")";
    case RPred::Kind::Or:
      return "(" + show(*p.p) + ") | (" + show(*p.q) + ")";
    case RPred::Kind::Top:
      return "⊤";
    case RPred::Kind::Bottom:
      return "⊥";
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
 *  bound to the chain, then its normal form by the Pst fold, the scan of the
 *  finite chain (@c sequences:pst), materialised. */
template <typename C, typename L>
auto former_runtime(const pst::Set<C, L>& s, const RPred& p) {
  const auto c = 𝔸<C>{} | p;  // every operator and constant checked here
  return ext(s & Runs<C>(dedekind::sequences::ScanSteps<decltype(c)>{
                     c, dedekind::sets::chain_bottom<C>(),
                     dedekind::sets::chain_top<C>()}));
}

/** @brief The elements of a set of a chain, run by run, from its least. */
template <typename C>
struct RunsIter {
  std::vector<std::pair<C, C>> runs;
  std::size_t i = 0;
  C cur{};
  bool live = false;
};

/** @brief A coefficient written back as a constant of the term surface: the
 *  word as its int, an element of 𝔽ₚ as its symmetric representative. */
struct AsConstant {
  PyConst operator()(std::uint64_t c) const { return PyConst{py_int(c)}; }
  PyConst operator()(const Fp& c) const { return PyConst{representative(c)}; }
  PyConst operator()(bool c) const { return PyConst{nb::bool_(c)}; }
  PyConst operator()(Ternary c) const { return PyConst{nb::cast(c)}; }
};

/** @brief Equality of two predicates by their normal forms: equal forms cut
 *  out equal sets; different forms decide nothing, so they raise rather than
 *  answer "different".  @tparam P the point type (one carrier, or a pair). */
template <typename P>
bool equal_by_normal_form(const RPred& a, const RPred& b) {
  if (dedekind::sets::same_shape(
          dedekind::algebra::canonical<P>(a, AsConstant{}),
          dedekind::algebra::canonical<P>(b, AsConstant{})))
    return true;
  throw dedekind::sets::not_admitted(
      "not decided: the normal forms differ, and different normal forms need "
      "not cut out different sets");
}

/** @brief A run list written back as a predicate on @c π: one disjunct per
 *  run, a point as @c π @c == @c k, a ray as one comparison, an interval as
 * two; the empty set @c π @c < @c 0 and the whole chain @c π @c >= @c 0.  The
 *  canonical form of a set of the chain's fragment.  @tparam C the chain.
 *  @param rs the run list. */
template <typename C, typename Runs>
RPred run_predicate(Runs rs, C bottom,
                    C top) {  // by value: the lazy merge advances as it is read
  const RTerm x = RTerm::proj(0);
  const auto k = [](C v) { return RTerm::constant(AsConstant{}(v)); };
  RPred out = RPred::cmp(Rel::Lt, x, k(bottom));  // Ø
  bool first = true;
  for (const auto& r : rs) {
    const RPred run =
        r.lo == r.hi     ? RPred::cmp(Rel::Eq, x, k(r.lo))
        : r.lo == bottom ? (r.hi == top ? RPred::cmp(Rel::Ge, x, k(bottom))
                                        : RPred::cmp(Rel::Le, x, k(r.hi)))
        : r.hi == top    ? RPred::cmp(Rel::Ge, x, k(r.lo))
                         : RPred::conj(RPred::cmp(Rel::Ge, x, k(r.lo)),
                                       RPred::cmp(Rel::Le, x, k(r.hi)));
    out = first ? run : RPred::disj(out, run);
    first = false;
  }
  return out;
}

/** @brief Equality of two sets over @c C: the chain's run lists where they
 *  decide (both ways), else the normal forms (only "equal").  @tparam C the
 *  carrier.  @tparam S the set. */
template <typename C, typename S>
bool set_equal(const S& a, const S& b) {
  if constexpr (IsTotallyOrdered<C>) {
    try {
      return a == b;
    } catch (const dedekind::sets::not_admitted&) {
      // outside the chain's fragment: the ring's normal forms below
    }
  }
  return equal_by_normal_form<C>(a.predicate, b.predicate);
}

/** @brief A point of the word out of a Python value. */
std::uint64_t to_word(nb::handle h) { return word_of(h).v; }
/** @brief A point of 𝔽ₚ out of a Python value: an element or its symmetric
 *  representative, as the universe's @c in; the quotient map is for constants.
 */
Fp to_fp(nb::handle h) {
  if (!in_fp(h))
    throw nb::type_error(
        "a point of 𝔽ₚ is an element or its symmetric representative");
  return residue_of(h);
}

/** @brief The relations a term cuts out of a product of two universes,
 *  @c 𝔸(C₁) @c * @c 𝔸(C₂) @c | @c pred: each slot has its carrier's sort, an
 *  atom is read in its operands' common sort, and a term mixing the two
 *  carriers is refused when the relation is built.  The canonical form is per
 *  axis.  A relation whose predicate is the graph @c π_2 @c == @c t(π_1) is
 *  certified a function and applied as one.  @tparam C1,C2 the carriers.
 *  @tparam To1,To2 a Python value into each. */
template <typename C1, typename C2, typename To1, typename To2>
void bind_relation(nb::module_& m, const char* pair_cls, const char* rel_cls,
                   const char* name1, const char* name2, To1 to1, To2 to2) {
  using U1 = 𝔸<C1, Boole, dedekind::sets::Finite>;
  using U2 = 𝔸<C2, Boole, dedekind::sets::Finite>;
  using UU = decltype(U1{} * U2{});
  using R = decltype(UU{} | std::declval<RPred>());
  using Point = std::pair<C1, C2>;
  const std::string shown = std::string("𝔸(") + name1 + ") × 𝔸(" + name2 + ")";
  const auto pair_of = [to1, to2](nb::handle h) {
    if (!nb::isinstance<nb::tuple>(h) || nb::len(h) != 2)
      throw nb::type_error("a point of the product is a pair (x, y)");
    const nb::tuple t = nb::borrow<nb::tuple>(h);
    return Point{to1(t[0]), to2(t[1])};
  };
  const auto holds = [pair_of](const auto& s, nb::handle h) {
    try {
      return static_cast<bool>(s(pair_of(h)));
    } catch (const nb::builtin_exception&) {
      return false;  // not a representative: not a member
    }
  };
  const auto factors = []() {
    return nb::make_tuple(nb::cast(U1{}).attr("carrier"),
                          nb::cast(U2{}).attr("carrier"));
  };
  m.def(
      "canonical",
      [](const R& r) {
        return UU{} |
               dedekind::algebra::canonical<Point>(r.predicate, AsConstant{});
      },
      nb::arg("S"),
      "canonical(R): the relation with each atom in normal form in its own "
      "axis' sort (a run list cannot be written back on a product).");
  nb::class_<UU>(m, pair_cls, "𝔸 × 𝔸: the product universe.")
      .def(
          "__or__", [](const UU& u, const RPred& p) { return u | p; },
          nb::is_operator(),
          "𝔸 × 𝔸 | pred: the relation; every slot, constant and operator is "
          "checked in its sort now, and a term mixing the two carriers "
          "refused.")
      .def("__contains__",
           [holds](const UU& u, nb::handle h) { return holds(u, h); })
      .def_prop_ro(
          "factors", [factors](const UU&) { return factors(); },
          "The carriers of the product's two coordinates.")
      .def_prop_ro(
          "codomain", [m](const UU&) { return m.attr("B"); },
          "The chain χ is valued in: 𝔹.")
      .def("__repr__", [shown](const UU&) { return shown; });
  nb::class_<R>(m, rel_cls, "A relation cut out of the product by a term.")
      .def("__contains__",
           [holds](const R& r, nb::handle h) { return holds(r, h); })
      .def(
          "__call__",
          [pair_of, to1](const R& r, nb::handle h) -> nb::object {
            if (nb::isinstance<nb::tuple>(h)) return nb::cast(r(pair_of(h)));
            // At a point: the relation as the function it is certified to be.
            const RTerm* t = dedekind::sets::graph_term(r.predicate);
            if (t == nullptr)
              throw nb::type_error(
                  "R(x): R is not certified a function (π_2 == t(π_1)); "
                  "R((x, y)) is χ");
            // t mentions only π_1: read it at a pair whose first coordinate
            // is x, in the second coordinate's sort, with no copy of the term.
            return nb::cast(
                dedekind::sets::term_value_as<C2>(*t, Point{to1(h), C2{}}));
          },
          nb::arg("x"),
          "R((x, y)): χ at a pair.  R(x): t(x), where R is the graph "
          "π_2 == t(π_1) of a function.")
      .def_prop_ro(
          "factors", [factors](const R&) { return factors(); },
          "The carriers of the relation's two coordinates.")
      .def_prop_ro(
          "codomain", [m](const R&) { return m.attr("B"); },
          "The chain χ is valued in: 𝔹.")
      .def_prop_ro(
          "is_function",
          [](const R& r) {
            return dedekind::sets::graph_term(r.predicate) != nullptr;
          },
          "Whether R is certified the graph of a function: π_2 == t(π_1), "
          "functional by its shape and entire since every operator of a bound "
          "term is total on the carrier.")
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
      .def(
          "__eq__",
          [](const R& a, const R& b) {
            return equal_by_normal_form<Point>(a.predicate, b.predicate);
          },
          nb::is_operator(),
          "R == S: True where the normal forms agree; TypeError where they "
          "differ, which decides nothing.")
      .def("__repr__",
           [shown](const R& r) { return shown + " | " + show(r.predicate); });
}

/** @brief The sets a term cuts out of a canonical universe: the set
 *  @c A(carrier) @c | @c pred and the relation @c A(carrier) @c * @c A(carrier)
 *  @c | @c pred, each a comprehension holding the bound predicate.  Over a
 *  chain the order fragment (atoms @c π @c ⋈ @c k under @c & and @c |) reads
 *  as run lists, so equality, @c canonical, iteration and slicing are the
 *  library's; a term outside it is read pointwise only.  @c image /
 *  @c preimage under the structural arrows substitute the variable (the
 *  successor's image is @c π @c ↦ @c π @c - @c 1), and on a word the run list
 *  translates, wrapping.  @tparam C the carrier.  @tparam ToC a Python value
 *  into @c C, raising where it is none. */
template <typename C, typename ToC>
void bind_term_sets(nb::module_& m,
                    nb::class_<𝔸<C, Boole, dedekind::sets::Finite>>& universe,
                    const char* name, const char* set_cls, const char* pair_cls,
                    const char* rel_cls, ToC to_c) {
  using U = 𝔸<C, Boole, dedekind::sets::Finite>;
  using S = Comprehension<U, RPred>;
  const std::string shown = std::string("𝔸(") + name + ")";
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
          "__or__",
          [](const U&, const RTerm& t) -> S {
            throw nb::type_error(("𝔸(carrier) | " + show(t) +
                                  ": a term is not a predicate; compare it")
                                     .c_str());
          },
          nb::is_operator())
      .def(
          "__mul__", [](const U& a, const U& b) { return a * b; },
          nb::is_operator(), "𝔸 × 𝔸: the product universe.")
      .def_prop_ro(
          "codomain", [m](const U&) { return m.attr("B"); },
          "The chain χ is valued in: 𝔹 (the universe's χ is constant ⊤).");
  const char* canonical_doc =
      "canonical(S): the same set in canonical form, where its theory has "
      "one.  A set of the chain's fragment (π compared with constants, under & "
      "| ~) is its run list, written back as a predicate: identical forms "
      "exactly when the sets are equal.  Otherwise its predicate atom by atom: "
      "an equation a == b on a ring as a polynomial a - b == 0, the unit part "
      "of its leading coefficient divided out (monic on a field), any other "
      "atom with its sides normalised; equal forms cut out equal sets, "
      "different ones need not differ.  //, % and a division by a term have "
      "no canonical form: TypeError.";
  m.def(
      "canonical",
      [](const S& s) {
        // The atoms first, then the set: an atom normalised into the chain's
        // fragment (π * 1 < 5 to π < 5) is then read as runs, so one pass
        // reaches the fixpoint.
        const S atoms =
            U{} | dedekind::algebra::canonical<C>(s.predicate, AsConstant{});
        if constexpr (IsTotallyOrdered<C>) {
          try {
            return U{} | run_predicate<C>(dedekind::order::runs(steps(atoms)),
                                          std::numeric_limits<C>::min(),
                                          std::numeric_limits<C>::max());
          } catch (const dedekind::sets::not_admitted&) {
            // outside the chain's fragment: the atoms' polynomial forms
          }
        }
        return atoms;
      },
      nb::arg("S"), canonical_doc);
  auto set =
      nb::class_<S>(m, set_cls, "A set cut out of the universe by a term.");
  set.def("__contains__",
          [holds, to_c](const S& s, nb::handle h) { return holds(to_c, s, h); })
      .def(
          "__call__",
          [to_c](const S& s, nb::handle h) -> nb::object {
            // The set applied as its characteristic arrow: at a term, the
            // predicate with π replaced by it (substitution); at a point, χ.
            if (is_term(h)) return nb::cast(s.predicate.subst(0, term_of(h)));
            return nb::cast(s(to_c(h)));
          },
          nb::arg("x"),
          "S(x): χ(x) at an element or a representative; S(t) at a term is the "
          "predicate with π replaced by t, the set applied as its arrow.")
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
      .def("__eq__", set_equal<C, S>, nb::is_operator(),
           "A == B: equality in the free algebra the carrier's laws give the "
           "term.  A chain's run lists decide it; otherwise the normal forms "
           "(canonical) answer True when they agree and raise TypeError when "
           "they differ, which decides nothing.  A term outside the catalogue "
           "(a remainder, a division by a term): TypeError.")
      .def(
          "__ne__",
          [](const S& a, const S& b) { return !set_equal<C, S>(a, b); },
          nb::is_operator(),
          "A != B: decided where the run lists decide; else TypeError.")
      .def_prop_ro("universe", [](const S&) { return U{}; })
      .def_prop_ro(
          "carrier", [](const S&) { return nb::cast(U{}).attr("carrier"); },
          "The carrier the set lives on.")
      .def_prop_ro(
          "codomain", [m](const S&) { return m.attr("B"); },
          "The chain χ is valued in: 𝔹.")
      .def("__repr__",
           [shown](const S& s) { return shown + " | " + show(s.predicate); });
  if constexpr (HasNNOStep<C>) {
    // image / preimage under the structural arrows: the substitution.  An
    // erased composite has no closed form and is refused.
    const auto shifted = [](const S& s, Arith op) {
      return U{} | s.predicate.subst(
                       0, RTerm::bin(op, RTerm::proj(0),
                                     RTerm::constant(PyConst{nb::int_(1)})));
    };
    const char* image_doc =
        "image(f, S) = {f(x) | x ∈ S} for a structural arrow (id, succ, pred): "
        "the variable substituted (succ: π ↦ π - 1); on a word the run list "
        "translates, wrapping at the end, so image(succ, {x > 5}) is {x > 6} ∪ "
        "{0}.";
    const char* preimage_doc =
        "preimage(f, S) = {x | f(x) ∈ S} for a structural arrow (id, succ, "
        "pred): "
        "the variable substituted (succ: π ↦ π + 1).";
    m.def(
        "image",
        [shifted](const Successor<C>&, const S& s) {
          return shifted(s, Arith::Sub);
        },
        nb::arg("f"), nb::arg("s"), image_doc);
    m.def(
        "image",
        [shifted](const Predecessor<C>&, const S& s) {
          return shifted(s, Arith::Add);
        },
        nb::arg("f"), nb::arg("s"), image_doc);
    m.def(
        "image", [](const Identity<C>&, const S& s) { return s; }, nb::arg("f"),
        nb::arg("s"), image_doc);
    m.def(
        "preimage",
        [shifted](const Successor<C>&, const S& s) {
          return shifted(s, Arith::Add);
        },
        nb::arg("f"), nb::arg("s"), preimage_doc);
    m.def(
        "preimage",
        [shifted](const Predecessor<C>&, const S& s) {
          return shifted(s, Arith::Sub);
        },
        nb::arg("f"), nb::arg("s"), preimage_doc);
    m.def(
        "preimage", [](const Identity<C>&, const S& s) { return s; },
        nb::arg("f"), nb::arg("s"), preimage_doc);
    using Erased = dedekind::category::Morphism<C, C, std::function<C(C)>>;
    m.def(
        "image",
        [](const Erased&, const S&) -> S {
          throw nb::type_error(
              "image of an erased composite arrow is intensional and has no "
              "closed form; only id / succ / pred do");
        },
        nb::arg("f"), nb::arg("s"), image_doc);
    m.def(
        "preimage",
        [](const Erased&, const S&) -> S {
          throw nb::type_error(
              "preimage of an erased composite arrow is intensional and has no "
              "closed form; only id / succ / pred do");
        },
        nb::arg("f"), nb::arg("s"), preimage_doc);
  }
  if constexpr (IsTotallyOrdered<C>) {
    // The order fragment: the run lists decide.
    const auto run_pairs = [](const S& s) {
      std::vector<std::pair<C, C>> out;
      for (const auto& r : dedekind::order::runs(steps(s)))
        out.emplace_back(r.lo, r.hi);
      return out;
    };
    const auto count = [run_pairs](const S& s) {
      nb::object total = nb::int_(0);
      for (const auto& [lo, hi] : run_pairs(s)) {
        const nb::object n =
            nb::steal(PyNumber_Add(py_int(hi - lo).ptr(), nb::int_(1).ptr()));
        total = nb::steal(PyNumber_Add(total.ptr(), n.ptr()));
      }
      return total;
    };
    set.def(
           "__bool__",
           [](const S& s) { return !(dedekind::sets::Ø<C>{} == s); },
           "A set is truthy iff it is inhabited.")
        .def(
            "__len__",
            [count](const S& s) { return nb::cast<std::size_t>(count(s)); },
            "The number of elements.")
        .def_prop_ro(
            "cardinality", [count](const S& s) { return count(s); },
            "The number of elements, as a Python int (2⁶⁴ for the universe).")
        .def(
            "__iter__",
            [run_pairs](const S& s) {
              RunsIter<C> it{run_pairs(s)};
              if (!it.runs.empty()) {
                it.cur = it.runs[0].first;
                it.live = true;
              }
              return it;
            },
            "The elements from the least, run by run: a bounded set iterates "
            "like "
            "range.")
        .def(
            "__getitem__",
            [](const S& s, nb::handle key) -> S {
              if (!nb::isinstance<nb::slice>(key))
                throw nb::type_error(
                    "a set is sliced by value, s[a:b] = s ∩ [a, b); positional "
                    "indexing presupposes an enumeration, which is a "
                    "sequence's");
              if (!key.attr("step").is_none())
                throw nb::type_error(
                    "a stepped slice is the meet with a residue class: "
                    "s & (A(carrier) | (π % k == r))");
              RPred p = s.predicate;
              if (!key.attr("start").is_none())
                p = RPred::conj(
                    p, RPred::cmp(Rel::Ge, RTerm::proj(0),
                                  RTerm::constant(PyConst{key.attr("start")})));
              if (!key.attr("stop").is_none())
                p = RPred::conj(
                    p, RPred::cmp(Rel::Lt, RTerm::proj(0),
                                  RTerm::constant(PyConst{key.attr("stop")})));
              return U{} | p;
            },
            nb::arg("key"),
            "s[a:b] = s ∩ [a, b), slicing by value (pandas' .loc, not .iloc).");
    nb::class_<RunsIter<C>>(m, (std::string(set_cls) + "Iter").c_str(),
                            "The elements of a set of the chain, in order.")
        .def("__iter__", [](RunsIter<C>& it) -> RunsIter<C>& { return it; })
        .def("__next__", [](RunsIter<C>& it) {
          if (!it.live) throw nb::stop_iteration();
          const C value = it.cur;
          if (value == it.runs[it.i].second) {
            if (++it.i < it.runs.size())
              it.cur = it.runs[it.i].first;
            else
              it.live = false;
          } else {
            it.cur = Successor<C>{}(it.cur);
          }
          return value;
        });
  }
  bind_relation<C, C>(m, pair_cls, rel_cls, name, name, to_c, to_c);
}
// ── The sets of the grammar ───────────────────────────────────────────────

/** @brief The species tags a universe is valued in: @c 𝔸(K3, Kleene). */
struct BooleTag {};
struct KleeneTag {};

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

/** @brief A set over a truth chain, written: its runs, or for an L-valued one
 *  its α-cuts. */
template <typename C, typename L>
std::string written_set(const pst::Set<C, L>& s) {
  if constexpr (std::same_as<L, Boole>) {
    return written_runs<C>(s);
  } else {
    return "{χ ≥ ⊤}: " + written_runs<C>(pst::cut(s, L::True)) +
           "; {χ ≥ U}: " + written_runs<C>(pst::cut(s, Ternary::Unknown));
  }
}

template <typename C, typename L>
nb::class_<pst::Set<C, L>> bind_set(nb::module_& m, const char* cls,
                                    const char* doc) {
  using S = pst::Set<C, L>;
  using Ω = typename L::Ω;
  auto set = nb::class_<S>(m, cls, doc);
  // χ_S as a term of the grammar: a decidable table is its run predicate, an
  // L-valued one McCarthy's conditional over its α-cuts.  Applied to a term,
  // the variable is substituted, so S(t) stays in the grammar and the
  // canonical readings see through it.
  set.def(
         "__call__",
         [](const S& s, nb::handle x) -> nb::object {
           if (!is_term(x)) return nb::cast(s(from_py_as<C>(x)));
           const C lo = dedekind::sets::chain_bottom<C>();
           const C hi = dedekind::sets::chain_top<C>();
           const RTerm t = term_of(x);
           if constexpr (std::same_as<L, Boole>) {
             return nb::cast(run_predicate<C>(dedekind::order::runs(s), lo, hi)
                                 .subst(0, t));
           } else {
             const auto cut = [&](Ω level) {
               return run_predicate<C>(
                   dedekind::order::runs(pst::cut(s, level)), lo, hi);
             };
             const auto k = [](Ω v) {
               return RTerm::constant(AsConstant{}(v));
             };
             return nb::cast(
                 RTerm::cond(cut(L::True), k(L::True),
                             RTerm::cond(cut(Ternary::Unknown),
                                         k(Ternary::Unknown), k(L::False)))
                     .subst(0, t));
           }
         },
         nb::arg("x"),
         "S(x): χ(x) at an element, in the set's species.  S(t) at a term: χ "
         "as "
         "a term of the grammar with π replaced by t; a decidable set's run "
         "predicate (so A(K3) | S(π) is S), an L-valued set's conditional over "
         "its α-cuts, cond(χ ≥ ⊤, ⊤, cond(χ ≥ U, U, ⊥)), compared on the chain "
         "(H(π) >= level the cut, H(π) == level the fibre).")
      .def(
          "__or__",
          [](const S&, const RTerm& t) -> S {
            throw nb::type_error(("S | " + show(t) +
                                  ": a term is not a predicate; compare it "
                                  "(t == top, t >= level)")
                                     .c_str());
          },
          "Refused: a term is not a predicate; compare it.")
      .def(
          "__contains__",
          [](const S& s, nb::handle x) {
            return s(from_py_as<C>(x)) == L::True;
          },
          nb::arg("x"), "x in S: whether χ(x) = ⊤.  For the L-valued χ, S(x).")
      .def(
          "__or__",
          [](const S& s, const RPred& p) { return former_runtime(s, p); },
          "S | pred: the former, {x ∈ S | P(x)}, the term checked on the "
          "chain.")
      .def(
          "__invert__", [](const S& a) { return ext(~a); },
          "~A: the complement, the species' reflection pointwise.")
      .def_prop_ro(
          "is_decidable", [](const S&) { return std::same_as<L, Boole>; },
          "HasDecidableMembership: whether χ answers in 𝔹.")
      .def_prop_ro(
          "carrier", [](const S&) { return pst::Chain<C>{}; },
          "The chain the set lives on.")
      .def_prop_ro(
          "codomain",
          [m](const S&) { return m.attr(std::same_as<L, Boole> ? "B" : "K3"); },
          "The chain χ is valued in: 𝔹 for a decidable set, K₃ for a "
          "K₃-valued one.");
  // Every handle over a truth chain is its run list: its own canonical form.
  m.def(
      "canonical", [](const S& s) { return s; }, nb::arg("S"),
      "canonical(S): a set over a truth chain is held as its run list, its "
      "canonical form.");
  if constexpr (std::same_as<L, Boole>) {
    set.def("__repr__", [](const S& s) { return written_runs<C>(s); });
    if constexpr (std::same_as<C, Ternary>)
      set.def(
          "lift",
          [](const S& s) { return ext(dedekind::sets::lift_to<Kleene>(s)); },
          "The set lifted along the dominance 𝔹 ↪ K₃: the same table, valued "
          "in K₃.");
  } else {
    set.def("__repr__", [](const S& s) { return written_set<C, L>(s); });
  }
  bind_mixed<C, L, L>(set);
  return set;
}

/** @brief Sugar on a truth chain: the grammar's sets with the chain as the
 *  universe, @c K3.above(U) for @c 𝔸(K3) @c | @c (π @c > @c U). */
template <typename C>
void bind_chain_sets(nb::class_<pst::Chain<C>>& ch) {
  using Ch = pst::Chain<C>;
  // {x | x ⋈ v}: the former on the universe, the predicate built here.
  const auto ray = [](Rel r) {
    return [r](const Ch&, nb::handle v) {
      return former_runtime(
          pst::Set<C, Boole>::universe(),
          RPred::cmp(r, RTerm::proj(0), RTerm::constant(PyConst{v})));
    };
  };
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
      .def("above", ray(Rel::Gt), nb::arg("v"),
           "{x | x > v}: 𝔸(chain) | (π > v).")
      .def("at_least", ray(Rel::Ge), nb::arg("v"), "{x | x >= v}.")
      .def("below", ray(Rel::Lt), nb::arg("v"), "{x | x < v}.")
      .def("at_most", ray(Rel::Le), nb::arg("v"), "{x | x <= v}.")
      .def(
          "point",
          [](const Ch&, nb::handle v) {
            return pst::point<C>(from_py_as<C>(v));
          },
          nb::arg("v"), "{v}: η(v).");
}

/** @brief Dispatch a (set, predicate) pair to a query: the predicate is bound
 *  to the set's chain and read there. */
template <typename Query>
nb::object on_set_and_pred(nb::handle s, nb::handle p, const char* what) {
  if (nb::isinstance<RPred>(p)) {
    const RPred pred = nb::cast<RPred>(p);
    if (nb::isinstance<pst::Set<bool, Boole>>(s))
      return nb::cast(Query::apply(nb::cast<pst::Set<bool, Boole>>(s), pred));
    if (nb::isinstance<pst::Set<Ternary, Boole>>(s))
      return nb::cast(
          Query::apply(nb::cast<pst::Set<Ternary, Boole>>(s), pred));
    if (nb::isinstance<pst::Set<Ternary, Kleene>>(s)) {
      if constexpr (requires(const pst::Set<Ternary, Kleene>& ks,
                             const RPred& kp) { Query::apply(ks, kp); })
        return nb::cast(
            Query::apply(nb::cast<pst::Set<Ternary, Kleene>>(s), pred));
      else
        throw nb::type_error(
            "forall over an L-valued set awaits the chain's residuated "
            "implication (#980); quantify over its α-cut: "
            "forall(A(S.carrier) | (S(π) >= level), P)");
    }
  }
  const std::string message = std::string(what) +
                              "(S, P): S a set over 𝔹 or K₃ and P a predicate "
                              "on π (a set is not a where-clause)";
  throw nb::type_error(message.c_str());
}
struct Exists {
  template <typename C, typename L>
  static auto apply(const pst::Set<C, L>& s, const RPred& p) {
    return dedekind::order::exists(steps(former_runtime(s, p)));  // ⋁ (χ_S ∧ P)
  }
};
struct Forall {
  template <typename C, typename L>
    requires std::same_as<L, Boole>
  static auto apply(const pst::Set<C, L>& s, const RPred& p) {
    return s == former_runtime(s, p);  // S ⊆ P
  }
};
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

  bind_set<bool, Boole>(m, "SetB", "A set over 𝔹, valued in 𝔹.");
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
      "The grammar's variables π, π_1, π_2: a comparison builds a "
      "predicate, the arithmetic + - * / // % a term, each bound and checked "
      "by "
      "A(carrier) | ....");
  projection
      .def("__repr__", [](const ProjectionTag& p) { return show(as_term(p)); })
      .def(
          "__getitem__",
          [](const ProjectionTag& p, unsigned k) {
            if (p.slot != 0)
              throw nb::type_error("π[k]: the family is indexed from π itself");
            return ProjectionTag{k};
          },
          nb::arg("k"),
          "π[k]: the k-th projection of a product, the family π_1, π_2, … is "
          "sugar for; π[0] is π, the identity.");
  def_arithmetic<ProjectionTag>(projection);
  def_comparisons<ProjectionTag>(projection);
  m.attr("π") = ProjectionTag{0};
  m.attr("π_1") = ProjectionTag{1};
  m.attr("π_2") = ProjectionTag{2};

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
      .def(
          "__bool__",
          [](const RPred& p) -> bool {
            throw nb::type_error(
                ("a predicate is not a truth value: " + show(p) +
                 "; bind it (A(carrier) | pred) or compare sets")
                    .c_str());
          },
          "Refused: a predicate is bound, not tested.")
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
        return on_set_and_pred<Exists>(s, p, "exists");
      },
      nb::arg("S"), nb::arg("P"),
      "exists(S, P) = ⋁ (χ_S ∧ P), in S's species: not empty.");
  m.def(
      "forall",
      [](nb::handle s, nb::handle p) {
        return on_set_and_pred<Forall>(s, p, "forall");
      },
      nb::arg("S"), nb::arg("P"),
      "forall(S, P) over a decidable S: S == (S | P), the library's own ∀; an "
      "L-valued S is refused (see #980), quantify over its cut, "
      "A(chain) | (S(π) >= level).");
  m.def(
      "cond",
      [](const RPred& p, nb::handle a, nb::handle b) {
        return RTerm::cond(p, term_of(a), term_of(b));
      },
      nb::arg("pred"), nb::arg("a"), nb::arg("b"),
      "cond(pred, a, b): McCarthy's conditional, a where pred holds, else b; "
      "a term of the grammar.");
  m.attr("any") = m.attr("exists");
  m.attr("all") = m.attr("forall");
  m.def(
      "lift",
      [](const pst::Set<Ternary, Boole>& s) {
        return ext(dedekind::sets::lift_to<Kleene>(s));
      },
      nb::arg("S"), "lift(S): a decidable set over K₃ valued in K₃.");

  // ── the term surface on the canonical universes and the truth chains ──
  bind_term_sets<uint64_t>(m, universe_u64, "u64", "U64Set", "UniverseU64Pair",
                           "U64Relation", to_word);
  bind_term_sets<Fp>(m, universe_fp, "𝔽ₚ", "FpSet", "UniverseFpPair",
                     "FpRelation", to_fp);
  // Products of the two carriers: each slot its own sort.
  using U64 = 𝔸<uint64_t, Boole, dedekind::sets::Finite>;
  using UFp = 𝔸<Fp, Boole, dedekind::sets::Finite>;
  universe_u64.def(
      "__mul__", [](const U64& a, const UFp& b) { return a * b; },
      nb::is_operator(), "𝔸(u64) × 𝔸(𝔽ₚ): the product of two carriers.");
  universe_fp.def(
      "__mul__", [](const UFp& a, const U64& b) { return a * b; },
      nb::is_operator(), "𝔸(𝔽ₚ) × 𝔸(u64): the product of two carriers.");
  bind_relation<uint64_t, Fp>(m, "UniverseU64FpPair", "U64FpRelation", "u64",
                              "𝔽ₚ", to_word, to_fp);
  bind_relation<Fp, uint64_t>(m, "UniverseFpU64Pair", "FpU64Relation", "𝔽ₚ",
                              "u64", to_fp, to_word);
}
