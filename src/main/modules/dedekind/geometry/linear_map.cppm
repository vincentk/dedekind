/**
 * @file dedekind/geometry/linear_map.cppm
 * @partition :linear_map
 * @brief Level 10.05: Concrete finite-dimensional linear maps.
 */

module;

#include <array>
#include <concepts>
#include <cstddef>
#include <initializer_list>

export module dedekind.geometry:linear_map;

import :affine;

namespace dedekind::geometry {

/**
 * @class LinearMap
 * @brief A dense compile-time-sized linear map F^Cols -> F^Rows.
 */
export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
class LinearMap {
 public:
  using scalar_type = F;
  using Domain = Vector<F, Cols>;
  using Codomain = Vector<F, Rows>;

  static constexpr std::size_t row_count = Rows;
  static constexpr std::size_t column_count = Cols;

  constexpr LinearMap() = default;

  constexpr LinearMap(std::initializer_list<std::initializer_list<F>> rows)
      : coeffs_{} {
    auto row_it = rows.begin();
    for (std::size_t i = 0; i < Rows && row_it != rows.end(); ++i, ++row_it) {
      auto col_it = row_it->begin();
      for (std::size_t j = 0; j < Cols && col_it != row_it->end();
           ++j, ++col_it)
        coeffs_[i][j] = *col_it;
    }
  }

  friend constexpr bool operator==(const LinearMap&,
                                   const LinearMap&) = default;

  constexpr F coefficient(std::size_t row, std::size_t col) const {
    return coeffs_[row][col];
  }

  constexpr void set_coefficient(std::size_t row, std::size_t col, F value) {
    coeffs_[row][col] = value;
  }

  constexpr Codomain operator()(const Domain& v) const {
    Codomain result{};
    for (std::size_t i = 0; i < Rows; ++i) {
      F sum{};
      for (std::size_t j = 0; j < Cols; ++j) sum = sum + coeffs_[i][j] * v[j];
      result[i] = sum;
    }
    return result;
  }

 private:
  template <std::floating_point G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator+(const LinearMap<G, R, C>&,
                                                const LinearMap<G, R, C>&);
  template <std::floating_point G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator-(const LinearMap<G, R, C>&,
                                                const LinearMap<G, R, C>&);
  template <std::floating_point G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const G&,
                                                const LinearMap<G, R, C>&);
  template <std::floating_point G, std::size_t R, std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const LinearMap<G, R, C>&,
                                                const G&);
  template <std::floating_point G, std::size_t R, std::size_t Inner,
            std::size_t C>
  friend constexpr LinearMap<G, R, C> operator*(const LinearMap<G, R, Inner>&,
                                                const LinearMap<G, Inner, C>&);

  std::array<std::array<F, Cols>, Rows> coeffs_{};
};

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
using Matrix = LinearMap<F, Rows, Cols>;

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator+(
    const LinearMap<F, Rows, Cols>& a, const LinearMap<F, Rows, Cols>& b) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = a.coeffs_[i][j] + b.coeffs_[i][j];
  return result;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator-(
    const LinearMap<F, Rows, Cols>& a, const LinearMap<F, Rows, Cols>& b) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = a.coeffs_[i][j] - b.coeffs_[i][j];
  return result;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator*(
    const F& scalar, const LinearMap<F, Rows, Cols>& a) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i)
    for (std::size_t j = 0; j < Cols; ++j)
      result.coeffs_[i][j] = scalar * a.coeffs_[i][j];
  return result;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator*(const LinearMap<F, Rows, Cols>& a,
                                             const F& scalar) {
  return scalar * a;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr Vector<F, Rows> operator*(const LinearMap<F, Rows, Cols>& a,
                                    const Vector<F, Cols>& v) {
  return a(v);
}

export template <std::floating_point F, std::size_t Rows, std::size_t Inner,
                 std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> operator*(
    const LinearMap<F, Rows, Inner>& a, const LinearMap<F, Inner, Cols>& b) {
  LinearMap<F, Rows, Cols> result;
  for (std::size_t i = 0; i < Rows; ++i) {
    for (std::size_t j = 0; j < Cols; ++j) {
      F sum{};
      for (std::size_t k = 0; k < Inner; ++k)
        sum = sum + a.coeffs_[i][k] * b.coeffs_[k][j];
      result.coeffs_[i][j] = sum;
    }
  }
  return result;
}

export template <std::floating_point F, std::size_t N>
constexpr LinearMap<F, N, N> identity_linear_map() {
  LinearMap<F, N, N> result;
  for (std::size_t i = 0; i < N; ++i) result.set_coefficient(i, i, F{1});
  return result;
}

export template <std::floating_point F, std::size_t Rows, std::size_t Cols>
constexpr LinearMap<F, Rows, Cols> zero_linear_map() {
  return LinearMap<F, Rows, Cols>{};
}

/**
 * @brief Covector: a linear functional F^N -> F, represented as a row vector.
 *
 * A covector (dual vector / linear functional) is canonically a
 * LinearMap<F, 1, N>: it maps a column vector to a length-1 column vector
 * whose single entry is the inner product of the row with the argument.
 */
export template <std::floating_point F, std::size_t N>
using Covector = LinearMap<F, 1, N>;

/**
 * @brief Outer product u ⊗ v : F^N -> F^M.
 *
 * Constructs the rank-1 linear map whose (i,j) coefficient is u[i]*v[j].
 * Applied to a vector w, it yields dot(v,w) * u (the dyadic product).
 */
export template <std::floating_point F, std::size_t M, std::size_t N>
constexpr LinearMap<F, M, N> outer(const Vector<F, M>& u,
                                   const Vector<F, N>& v) {
  LinearMap<F, M, N> result;
  for (std::size_t i = 0; i < M; ++i)
    for (std::size_t j = 0; j < N; ++j)
      result.set_coefficient(i, j, u[i] * v[j]);
  return result;
}

}  // namespace dedekind::geometry