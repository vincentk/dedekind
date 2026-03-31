/**
 * @file dedekind/numbers/symbolic.cppm
 * @partition :symbolic
 * @brief Level 9.2: The Language of the Continuum.
 */

export module dedekind.numbers:symbolic;

import :real;
import :complex;
import dedekind.sets; // For expressions, var, %

namespace dedekind::numbers {

using namespace dedekind::sets;

/** 
 * @section Symbolic_Set_Construction
 * We use the 'x % S | predicate' syntax to define the cuts.
 */
export template <typename Q>
constexpr auto Sqrt2_Symbolic() {
    auto x = var<Q>;
    // { x ∈ Q | x < 0 ∨ x² < 2 }
    return DedekindCut<Q>{ x % Ω<Q>{} | (x < 0.0 || (x * x < 2.0)) };
}

/** @section Transcendental_Anchors */

/**
 * @brief Trait to mark a species as Transcendental (π, e).
 * @details Defaults to false for base fields (Q).
 */
export template <typename T>
inline constexpr bool is_transcendental_v = false;

/**
 * @brief The Transcendental Set (𝕋).
 * @details { x ∈ ℝ | x is not a root of any rational polynomial }.
 */
export template <typename R>
  requires IsField<R>
constexpr auto TranscendentalSet() {
    // We bind the symbolic scout to the Universal Set of the Real Species.
    auto x = var<R>;
    
    /** 
     * @section The_Symbolic_Predicate
     * We map the species-level trait into a set-level comprehension.
     */
    return x % Ω<R>{} | [](const R& val) {
        // In Level 9, this resolves via trait discovery.
        return is_transcendental_v<R>; 
    };
}

} // namespace dedekind::numbers
