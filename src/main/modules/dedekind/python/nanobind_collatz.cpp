/**
 * @file dedekind/python/nanobind_collatz.cpp
 * @brief Nanobind extension module for the bounded Collatz exhibit (#861): the
 * arrows on ℕ's shadow, McCarthy's conditional, the iterate, the bounded
 * search, Σ, and the window ∀.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section Description
 * Private native module @c dedekind._collatz, re-exported by @c
 * dedekind.collatz.
 * @c π is the identity arrow on ℕ; @c * @c + @c // @c % build the affine, floor
 * division and remainder arrows after it, @c == a constant gives a Boolean
 * test, and @c cond(p, f, g) is McCarthy's conditional, so the rule reads
 * @c cond(π @c % @c 2 @c == @c 0, @c π @c // @c 2, @c 3*π @c + @c 1).
 * @c iterate(f, n) is the orbit, a lazy Path; @c first_where searches a prefix;
 * @c Σ reads a search in K₃; @c reaches_within(f, B) is their composition as a
 * predicate; @c forall(S, P) is the window ∀.  Every handle is a library
 * object; reduction stays in C++.
 */

#include <nanobind/nanobind.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/vector.h>

#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <utility>
#include <vector>

import dedekind.category;  // Identity, Compose, Cond, Semidecided, preimage
import dedekind.numbers;   // Affine, FloorDiv, Mod, collatz_step
import dedekind.python;    // dedekind::python::collatz, ::lwv, erase
import dedekind.sequences; // Path, iterate, first_where
import dedekind.sets;      // η

namespace nb = nanobind;

namespace {
namespace collatz = dedekind::python::collatz;
namespace lwv = dedekind::python::lwv;
using collatz::ArrowN;
using collatz::Nat;
using collatz::PredN;
using dedekind::category::Boole;
using dedekind::category::Compose;
using dedekind::category::Cond;
using dedekind::category::Identity;
using dedekind::category::preimage;
using dedekind::category::Semidecided;
using dedekind::category::Ternary;
using dedekind::numbers::Affine;
using dedekind::numbers::collatz_step;
using dedekind::numbers::FloorDiv;
using dedekind::numbers::Mod;
using dedekind::python::erase;
using dedekind::sequences::first_where;
using dedekind::sequences::iterate;
using dedekind::sequences::Path;
using dedekind::sets::η;

/** @brief Erase an arrow ℕ → ℕ into the one handle type, as jlt::compose does.
 */
template <typename F>
ArrowN arrow(F f) {
  return ArrowN{std::function<Nat(Nat)>{std::move(f)}};
}
/** @brief A Python int as an element of ℕ's shadow; negatives are not. */
Nat nat(long long n, const char* what) {
  if (n < 0)
    throw nb::value_error(
        (std::string(what) + ": ℕ's shadow has no negative elements").c_str());
  return static_cast<Nat>(n);
}
Nat positive(long long n, const char* what) {
  if (n <= 0)
    throw nb::value_error(
        (std::string(what) + ": needs a positive constant").c_str());
  return static_cast<Nat>(n);
}

/** @brief The orbit handle: a lazy Path on ℕ. */
struct PathN {
  Path<Nat> path;
};
struct PathIter {
  Path<Nat> path;
  std::size_t index = 0;
};
}  // namespace

NB_MODULE(_collatz, m) {
  m.doc() =
      "The bounded Collatz exhibit (#861), from the library's arrows.  π is "
      "the "
      "identity arrow on ℕ; π % 2 == 0, π // 2 and 3*π + 1 are arrows after "
      "it; "
      "cond(p, f, g) is McCarthy's conditional, so T = cond(π % 2 == 0, π // "
      "2, "
      "3*π + 1) is the rule.  iterate(T, n) is the orbit; first_where searches "
      "a "
      "prefix; Σ reads a search in K₃ (found ↦ TRUE, not yet ↦ UNKNOWN, never "
      "FALSE); reaches_within(T, B) is Σ ∘ first_where ∘ iterate as a "
      "predicate; "
      "forall(S, P) is the window ∀.  collatz_step is the library's own T.";

  nb::class_<ArrowN>(m, "ArrowN", "An arrow ℕ → ℕ on ℕ's shadow, erased.")
      .def(
          "__call__",
          [](const ArrowN& f, long long n) { return f(nat(n, "f(n)")); },
          nb::arg("n"), "Apply the arrow.")
      .def(
          "__rshift__",
          [](const ArrowN& f, const ArrowN& g) {
            return dedekind::python::jlt::compose(f, g);
          },
          "f >> g: apply f, then g (the reducer's composition, erased).")
      .def(
          "__mul__",
          [](const ArrowN& f, long long k) {
            return arrow(Compose{f, Affine<Nat>{nat(k, "k * f"), 0}});
          },
          nb::is_operator(), "f * k: the affine arrow k·f(n).")
      .def(
          "__rmul__",
          [](const ArrowN& f, long long k) {
            return arrow(Compose{f, Affine<Nat>{nat(k, "k * f"), 0}});
          },
          nb::is_operator())
      .def(
          "__add__",
          [](const ArrowN& f, long long k) {
            return arrow(Compose{f, Affine<Nat>{1, nat(k, "f + k")}});
          },
          nb::is_operator(), "f + k: the affine arrow f(n) + k.")
      .def(
          "__radd__",
          [](const ArrowN& f, long long k) {
            return arrow(Compose{f, Affine<Nat>{1, nat(k, "f + k")}});
          },
          nb::is_operator())
      .def(
          "__floordiv__",
          [](const ArrowN& f, long long d) {
            return arrow(Compose{f, FloorDiv<Nat>{positive(d, "f // d")}});
          },
          nb::is_operator(), "f // d: floor division after f.")
      .def(
          "__mod__",
          [](const ArrowN& f, long long k) {
            return arrow(Compose{f, Mod<Nat>{positive(k, "f % m")}});
          },
          nb::is_operator(), "f % m: the remainder after f.")
      .def(
          "__eq__",
          [](const ArrowN& f, long long k) -> PredN {
            return erase<Nat, Boole>(preimage(f, η(nat(k, "f == k"))));
          },
          nb::is_operator(),
          "f == k: the Boolean test {n | f(n) = k}, the preimage of the point.")
      .def("__repr__", [](const ArrowN&) { return "<arrow ℕ → ℕ>"; });

  nb::class_<PredN>(m, "PredN",
                    "A Boolean test on ℕ: an erased set {n | p(n)}.")
      .def(
          "__call__",
          [](const PredN& p, long long n) { return p(nat(n, "p(n)")); },
          nb::arg("n"), "Whether n passes the test.")
      .def(
          "__contains__",
          [](const PredN& p, long long n) { return p(nat(n, "n in p")); },
          nb::arg("n"))
      .def("__repr__", [](const PredN&) { return "{n | p(n)}"; });

  nb::class_<PathIter>(m, "PathIter", "The unfold of a path, without end.")
      .def("__iter__", [](PathIter& it) -> PathIter& { return it; })
      .def("__next__", [](PathIter& it) { return it.path.at(it.index++); });

  nb::class_<PathN>(m, "PathN",
                    "A lazy path on ℕ: the iterate of an arrow from a seed.")
      .def(
          "__getitem__",
          [](const PathN& o, nb::handle key) -> nb::object {
            if (nb::isinstance<nb::slice>(key)) {
              const auto stop = key.attr("stop");
              if (stop.is_none())
                throw nb::type_error(
                    "a path has no end: give the slice a stop, o[:k]");
              const auto start = key.attr("start");
              const auto step = key.attr("step");
              const long long a =
                  start.is_none() ? 0 : nb::cast<long long>(start);
              const long long b = nb::cast<long long>(stop);
              const long long d =
                  step.is_none() ? 1 : nb::cast<long long>(step);
              if (a < 0 || b < 0 || d <= 0)
                throw nb::index_error(
                    "a path is indexed from its seed forward: non-negative "
                    "positions, positive step");
              std::vector<Nat> out;
              for (long long i = a; i < b; i += d)
                out.push_back(o.path.at(static_cast<std::size_t>(i)));
              return nb::cast(out);
            }
            const auto i = nb::cast<long long>(key);
            if (i < 0) throw nb::index_error("a path has no end to count from");
            return nb::cast(o.path.at(static_cast<std::size_t>(i)));
          },
          nb::arg("key"),
          "o[i]: the i-th term, fⁱ(n); o[a:b]: the terms at positions [a, b) "
          "(a stop is required: the path does not end).")
      .def(
          "__iter__", [](const PathN& o) { return PathIter{o.path, 0}; },
          "Unfold the path from its seed; it never stops (use islice).")
      .def(
          "first_where",
          [](const PathN& o, const PredN& p, std::size_t budget) {
            return first_where(o.path, p, budget);
          },
          nb::arg("p"), nb::arg("budget"),
          "The first index k ≤ budget with p(o[k]), or None: the bounded "
          "search.")
      .def("__repr__", [](const PathN& o) {
        std::string s = "⟨";
        for (std::size_t i = 0; i < 6; ++i)
          s += std::to_string(o.path.at(i)) + ", ";
        return s + "…⟩";
      });

  nb::class_<collatz::ReachesWithin>(
      m, "ReachesWithin",
      "The predicate 'the orbit under step reaches 1 within the budget', "
      "valued "
      "in K₃: Σ ∘ first_where(π == 1, budget) ∘ iterate(step).")
      .def_prop_ro(
          "budget", [](const collatz::ReachesWithin& p) { return p.budget; },
          "The budget B.")
      .def_prop_ro(
          "step", [](const collatz::ReachesWithin& p) { return p.step; },
          "The arrow iterated.")
      .def(
          "__call__",
          [](const collatz::ReachesWithin& p, long long n) {
            return p(nat(n, "P(n)"));
          },
          nb::arg("n"),
          "The verdict for n: TRUE once seen to arrive, else UNKNOWN.")
      .def("__repr__", [](const collatz::ReachesWithin& p) {
        return "Σ ∘ first_where(π == 1, " + std::to_string(p.budget) +
               ") ∘ iterate(step)";
      });

  m.attr("π") = nb::cast(arrow(Identity<Nat>{}));
  m.attr("identity") = m.attr("π");
  m.attr("collatz_step") = nb::cast(arrow(collatz_step));
  m.def(
      "cond",
      [](const PredN& p, const ArrowN& f, const ArrowN& g) {
        return arrow(Cond{p, f, g});
      },
      nb::arg("p"), nb::arg("f"), nb::arg("g"),
      "cond(p, f, g): McCarthy's conditional, f where p holds and g where "
      "not.");
  m.def(
      "iterate",
      [](const ArrowN& f, long long n) {
        return PathN{iterate(nat(n, "iterate(f, n)"), f)};
      },
      nb::arg("f"), nb::arg("n"),
      "iterate(f, n): the orbit n, f(n), f(f(n)), … as a lazy path.");
  m.def(
      "Σ", [](bool found) { return Semidecided{}(found); }, nb::arg("found"),
      "Σ(found): a bounded search read in K₃: TRUE for a witness, UNKNOWN for "
      "none so far, never FALSE.");
  m.attr("sigma") = m.attr("Σ");
  m.def(
      "reaches_within",
      [](const ArrowN& step, std::size_t budget) {
        return collatz::ReachesWithin{step, budget};
      },
      nb::arg("step"), nb::arg("budget"),
      "reaches_within(step, B): the predicate Σ ∘ first_where(π == 1, B) ∘ "
      "iterate(step), in K₃.");
  m.def(
      "forall",
      [](const lwv::Set& s, const collatz::ReachesWithin& p) -> Ternary {
        if (!lwv::is_bounded(s))
          throw nb::type_error(
              "∀ over a set without a window ({x > k}, 𝔸) is the conjecture "
              "itself: no budget bounds a search over all of ℕ; slice the set, "
              "s[a:b]");
        const auto lo = lwv::least(s);
        if (lo && *lo < 0)
          throw nb::type_error("a window with negatives is not a window of ℕ");
        return lwv::forall(s, p);
      },
      nb::arg("s"), nb::arg("p"),
      "forall(S, P): the bounded ∀ over the window S spans, in K₃.  A ray or 𝔸 "
      "has no window and is refused.");
}
