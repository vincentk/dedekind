#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_floating_point.hpp>

import dedekind.numbers;
import dedekind.category;

using namespace dedekind::numbers;
using namespace dedekind::category;

TEST_CASE("Numbers: The Euler Synthesis", "[numbers][complex][euler]") {
    using ℝ = Real<double>;
    using ℂ = Complex<ℝ>;

    SECTION("Symbolic Comprehension: Sqrt(2)") {
        auto root2 = Sqrt2_Symbolic<double>();
        
        // Check if a rational is in the lower set via the expression
        REQUIRE(root2(1.4) == true);
        REQUIRE(root2(1.5) == false);
    }

    SECTION("Euler's Identity: e^{iπ} + 1 = 0") {
        /**
         * @step 1: Define the Exp(z) mapping as a Functorial Lift 
         * of the Taylor Series: Σ (z^n / n!)
         */
        auto exp = [](ℂ z) {
            return ℂ{ 
                ℝ{CauchyPath<double>{[z](size_t n) {
                    // This uses the path-multiplication and addition 
                    // defined in :real and :complex
                    return /* Taylor expansion logic */;
                }}},
                ℝ{CauchyPath<double>{[z](size_t n) {
                    return /* Taylor expansion logic */;
                }}}
            };
        };

        // Construct iπ
        ℂ ipi{ℝ{η<Path, double>{}(0.0)}, Pi().path()}; 
        
        // e^{iπ}
        auto result = exp(ipi);

        /** @proof The result must converge to -1 + 0i */
        REQUIRE_THAT(result.real().resolve(), Catch::Matchers::WithinRel(-1.0, 0.001));
        REQUIRE_THAT(result.imag().resolve(), Catch::Matchers::WithinAbs(0.0, 0.001));
    }
}

TEST_CASE("Numbers: Transcendental Invariants", "[numbers][symbolic]") {
    using ℝ = Real<double>;
    auto T = TranscendentalSet<ℝ>();

    SECTION("Membership Proofs") {
        /** 
         * @proof e and π are elements of the Transcendental Set.
         * Note: This requires the species to carry the 'is_transcendental_v' trait.
         */
        static_assert(T(E()));
        static_assert(T(Pi()));
        
        // Counter-proof: Sqrt(2) is Algebraic, thus NOT Transcendental.
        static_assert(!T(Sqrt2()));
    }

    SECTION("The Euler Vanishing: e^{iπ} + 1") {
        /**
         * Even if the path-sampling is numerical, the relation is anchored
         * in the topology of the complex plane.
         */
        auto e_ipi = exp(Complex<ℝ>{zero, Pi().path()});
        auto one = Complex<ℝ>{ℝ{η<Path, double>{}(1.0)}, zero};
        
        auto sum = e_ipi + one;
        
        // The "Vanishing" check
        REQUIRE_THAT(sum.real().resolve(), Catch::Matchers::WithinAbs(0.0, 0.001));
    }
}
