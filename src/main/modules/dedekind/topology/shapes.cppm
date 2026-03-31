/**
 * @file ontology:topology.cppm
 * @partition :shapes
 * @brief Level 3.1: Topological Mereology.
 */

export module dedekind.topology:shapes;

import dedekind.order;
import dedekind.sets;
import :topology;

namespace dedekind::topology {

using namespace dedekind::sets;
using namespace dedekind::order;

export enum class Direction { Upward, Downward };

/**
 * @class Ray
 * @brief A Half-Space satisfying the Mereological Lattice axioms.
 */
export template <IsTotallyOrdered T, Direction D, typename L = ClassicalLogic>
class Ray {
 public:
  using element_type = T;
  using logic_type = L;
  using is_open_tag = void;
  using is_ray_tag = void;

  constexpr explicit Ray(T pivot) : pivot_(pivot) {}

  /** @section Mereology: The Characteristic Function */
  constexpr typename L::type operator()(const T& x) const {
    if constexpr (D == Direction::Upward)
      return x > pivot_ ? L::True : L::False;
    else
      return x class Interval {
       public:
        using element_type = T;
        using lower_ray_type = Ray<T, Direction::Upward, L>;
        using upper_ray_type = Ray<T, Direction::Downward, L>;

        constexpr Interval(T low, T high) : lower_(low), upper_(high) {}

        /** @brief Synthesis of the Overlap Axiom */
        constexpr typename L::type operator()(const T& x) const {
          return lower_(x) && upper_(x);
        }

        /** @section Bounded_Lattice_Requirements */
        constexpr T lower_bound() const { return lower_.pivot(); }
        constexpr T upper_bound() const { return upper_.pivot(); }

       private:
        lower_ray_type lower_;
        upper_ray_type upper_;
      };

    /** @section Trait_Registration */
    template <typename T, Direction D, typename L>
    inline constexpr bool is_convex_v<Ray<T, D, L>> = true;

    template <typename T, typename L>
    inline constexpr bool is_convex_v<Interval<T, L>> = true;

    /** @section Formal_Verification */

    static_assert(
        IsProperPart<double, Interval<double>>,
        "Axiom Failure: Points must be proper parts of the Interval whole.");

    static_assert(
        IsMeetSemilattice<Ray<double, Direction::Upward>>,
        "Axiom Failure: Rays must satisfy idempotency under intersection.");

    static_assert(
        IsBoundedLattice<Interval<double>>,
        "Axiom Failure: Intervals must expose their extremal bounds.");

  }  // namespace dedekind::topology
