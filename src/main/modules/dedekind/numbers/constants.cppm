export module dedekind.numbers:constants;

import :real;
import dedekind.sequences;
import dedekind.algebra;

namespace dedekind::numbers {

/** @brief Factorial helper for the Taylor series of 'e' */
constexpr double fact(std::size_t n) {
    return n <= 1 ? 1.0 : n * fact(n - 1);
}

/** @section The_Continuum_Constants */

/** @brief √2: The limit of x_{n+1} = 0.5(x_n + 2/x_n) */
export constexpr auto Sqrt2() {
    return Real<double>{CauchyPath<double>{[](std::size_t n) {
        double x = 1.5;
        for(std::size_t i = 0; i < n; ++i) x = 0.5 * (x + 2.0 / x);
        return x;
    }}};
}

/** @brief e: The limit of Σ (1/n!) */
export constexpr auto E() {
    return Real<double>{CauchyPath<double>{[](std::size_t n) {
        double sum = 0.0;
        for(std::size_t i = 0; i <= n; ++i) sum += 1.0 / fact(i);
        return sum;
    }}};
}

/** @brief π: The limit of 3 + Σ (-1)^k * 4 / ((2k+2)(2k+3)(2k+4)) */
export constexpr auto Pi() {
    return Real<double>{CauchyPath<double>{[](std::size_t n) {
        double res = 3.0;
        for(std::size_t i = 0; i < n; ++i) {
            double d = (2.0*i + 2.0) * (2.0*i + 3.0) * (2.0*i + 4.0);
            res += (i % 2 == 0 ? 4.0 : -4.0) / d;
        }
        return res;
    }}};
}

// Inside dedekind.numbers:constants
template<> inline constexpr bool is_transcendental_v<decltype(Pi())> = true;
template<> inline constexpr bool is_transcendental_v<decltype(E())> = true;

} // namespace dedekind::numbers
