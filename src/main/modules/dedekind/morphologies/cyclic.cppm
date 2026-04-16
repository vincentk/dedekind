/**
 * @file ontology:morphologies.cppm
 * @partition :cyclic
 * @brief Level 3.5a: Minimal cyclic morphology for experimental reintegration.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @note "In dedekind.morphologies:cyclic, structure is clarified by explicit
 * composition and typed interfaces." (Module-specific documentation note for
 * maintainers.)
 *       -- dedekind maintainers
 */
module;

#include <concepts>

export module dedekind.morphologies:cyclic;

namespace dedekind::morphologies {

export template <std::integral T, T N>
class CyclicRing {
 public:
  using machine_type = T;
  using Domain = T;

  constexpr explicit CyclicRing(T v) : value_(normalize(v)) {}

  static constexpr T successor(T a) { return normalize(a + 1); }
  static constexpr T generator() { return normalize(1); }

  constexpr operator T() const { return value_; }

  friend constexpr CyclicRing operator+(CyclicRing a, CyclicRing b) {
    return CyclicRing{static_cast<T>(a.value_ + b.value_)};
  }

  friend constexpr CyclicRing operator*(CyclicRing a, CyclicRing b) {
    return CyclicRing{static_cast<T>(a.value_ * b.value_)};
  }

 private:
  static constexpr T normalize(T v) {
    const T m = v % N;
    return m < 0 ? static_cast<T>(m + N) : m;
  }

  T value_;
};

}  // namespace dedekind::morphologies
