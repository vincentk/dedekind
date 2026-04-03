/**
 * @file ontology:morphologies.cppm
 * @partition :cyclic
 * @brief Level 3.5a: The Dedekind Chain (Finite Systems).
 *
 * @section Cyclic_Morphologies: The Discrete Loop
 * This partition implements the formal reification of Modular Arithmetic
 * (Z/nZ) as a "Cyclic System."
 *
 * Following Richard Dedekind's "Was sind und was sollen die Zahlen?", a
 * system is cyclic if it is the 'chain' of a single generator under a
 * successor mapping that eventually returns to its origin.
 *
 * @subsection Structural_Properties
 * - Finite: The cardinality is constrained by the modulus N.
 * - Closed: The successor morphism f(n) = (n + 1) mod N ensures no "leakage"
 *   out of the species boundary.
 * - Abelian: Inherits the commutative properties of the underlying
 *   Integer Ring.
 *
 * @tparam T The underlying Stroustrupian primitive (e.g., int, long).
 * @tparam N The Modulus (The "Circumference" of the Species).
 *
 * Wikipedia: Cyclic group, Modular arithmetic, Dedekind cut
 */

export module dedekind.morphologies:cyclic;

import dedekind.algebra;
import dedekind.sequences;

namespace dedekind::morphologies {
using namespace algebra;
using namespace sequences;

/**
 * @class CyclicRing
 * @brief The reified Z/nZ structure.
 *
 * @tparam T The underlying integral primitive.
 * @tparam N The Modulus.
 */
export template <std::integral T, T N>
class CyclicRing {
 public:
  using machine_type = T;
  using Domain = T;
  using cardinality_type = Finite;

  static constexpr T successor(T a) { return (a + 1) % N; }
  static constexpr T generator() { return 1 % N; }

  constexpr explicit CyclicRing(T v) : value(v % N) {}
  constexpr operator T() const { return value; }

  /** @section Algebraic_Operators */
  friend constexpr CyclicRing operator+(CyclicRing a, CyclicRing b) {
    return CyclicRing(a.value + b.value);
  }

  friend constexpr CyclicRing operator*(CyclicRing a, CyclicRing b) {
    return CyclicRing(a.value * b.value);
  }

 private:
  T value;
};

/**
 * @section Formal_Verification
 * These assertions ensure the morphology is not a "hollow" proxy but a
 * mathematically sound species.
 */

// Verify basic Group/Ring identities
static_assert(IsAbelianGroup<CyclicRing<int, 12>>,
              "CyclicRing must be an Abelian Group under addition.");

static_assert(IsCommutativeRing<CyclicRing<int, 12>>,
              "CyclicRing must satisfy the Commutative Ring axioms.");

// Verify the Morphological "Chain" properties
static_assert(IsCyclic<CyclicRing<int, 12>>,
              "CyclicRing must satisfy the Dedekind Chain (IsCyclic) concept.");

static_assert(
    IsCyclicRing<CyclicRing<int, 12>>,
    "CyclicRing must satisfy the synthesized IsCyclicRing morphology.");
