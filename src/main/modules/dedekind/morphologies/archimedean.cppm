/**
 * @file ontology:morphologies.cppm
 * @partition :archimedean
 * @brief Level 3.5b: Convergence morphology stubs for experimental reintegration.
 */
module;

#include <cmath>
#include <concepts>

export module dedekind.morphologies:archimedean;

import dedekind.sequences;

namespace dedekind::morphologies {
using namespace dedekind::sequences;

export template <typename T>
concept IsCyclic = requires {
  typename T::Domain;
  { T::generator() } -> std::same_as<typename T::Domain>;
};

export template <typename T>
concept IsSimplyInfinite = IsCyclic<T>;

export template <typename T>
concept IsCyclicRing = IsCyclic<T>;

export template <typename T>
concept IsOrderedField = std::regular<T> && std::totally_ordered<T>;

export template <typename T>
concept IsArchimedeanField = IsOrderedField<T>;

export template <typename T>
concept IsDedekindCompleteField = IsArchimedeanField<T>;

export template <typename S>
concept IsMinkowskiSummable = requires(S a, S b) {
  { a + b } -> std::same_as<S>;
};

export template <typename Seq>
concept IsCauchy = IsSequence<Seq> && requires(Seq s) {
  { s.at(0) } -> std::same_as<typename Seq::Codomain>;
  { std::abs(s.at(0) - s.at(0)) };
};

export template <typename Seq>
concept IsConvergent = IsCauchy<Seq> && requires(Seq s) {
  { limit(s) } -> std::same_as<typename Seq::Codomain>;
};

export template <typename T>
struct CauchyPath : public Path<T> {
  using Path<T>::Path;
  using is_cauchy_tag = void;
};

}  // namespace dedekind::morphologies
