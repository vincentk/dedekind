/**
 * @file dedekind/sequences/curve.cppm
 * @partition :curve
 * @brief Level 2.5c: The Curve (Continuum-Indexed Morphism Family).
 *
 * @section Curve_Morphism
 * A curve is modeled as a morphism family γ: I -> T where I is a real-valued
 * index species. This complements Path (countable/discrete indexing) with a
 * continuum-indexed sibling.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "God made the integers; all else is the work of man."
 *       -- Leopold Kronecker
 */
module;

#include <concepts>
#include <cstddef>
#include <functional>

export module dedekind.sequences:curve;

import dedekind.category;
import dedekind.sets;
import :net;

namespace dedekind::sequences {

using namespace dedekind::category;
using namespace dedekind::sets;

/**
 * @class Curve
 * @brief A continuum-indexed family γ: I -> T.
 *
 * @tparam Parameter Real-valued index species (typically float/double).
 * @tparam T Codomain species sampled along the parameter.
 * @tparam Cardinality Magnitude metadata for the index family.
 */
export template <std::floating_point Parameter, typename T,
                 typename Cardinality = ℶ_1>
struct Curve {
  using Domain = Parameter;
  using Codomain = T;
  using cardinality_type = Cardinality;

  std::function<T(Parameter)> generator;

  constexpr Codomain operator()(Domain t) const { return generator(t); }
  constexpr T at(Parameter t) const { return generator(t); }

  static consteval cardinality_type cardinality() { return {}; }
};

/**
 * @concept IsCurve
 * @brief A real-parameter indexed morphism family.
 */
export template <typename C>
concept IsCurve = IsIndexedMorphismFamily<C> &&
                  std::floating_point<typename std::remove_cvref_t<C>::Domain>;

/** @section Formal_Verification */
static_assert(IsArrow<Curve<double, int>>,
              "Curve must satisfy the categorical arrow concept.");

static_assert(IsIndexedMorphismFamily<Curve<double, int>>,
              "Curve must satisfy indexed morphism family semantics.");

static_assert(IsCurve<Curve<double, int>>,
              "Curve must satisfy the IsCurve concept.");

static_assert(!IsCountablyIndexedFamily<Curve<double, int>>,
              "Curve is continuum-indexed by default, not countably indexed.");

static_assert(!IsSequence<Curve<double, int>>,
              "Continuum-indexed curves must remain distinct from sequences.");

}  // namespace dedekind::sequences
