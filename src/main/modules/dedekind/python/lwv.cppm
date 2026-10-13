/**
 * @file dedekind/python/lwv.cppm
 * @partition :lwv
 * @brief The stored form of a set term, as a Python handle holds it (#975).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

export module dedekind.python:lwv;

import dedekind.numbers; // the carriers' order registrations
import dedekind.order;

export namespace dedekind::python {

/** @brief A term's normal form, materialised for a handle: the one stored
 *  form, a run list over the term's carrier and species.  @param t the term.
 */
template <typename T>
auto materialise(const T& t) {
  using S = decltype(dedekind::order::steps(t));
  return dedekind::order::Runs<typename S::Domain, typename S::logic_species>(
      dedekind::order::steps(t));
}

}  // namespace dedekind::python
