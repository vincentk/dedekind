/** @file dedekind/sequences/stream_comonad_test.cpp
 *
 * Unit coverage for the stream-comonad surface on @c Path (#719 Slice 2):
 * the operational @c IsStreamComonad concept and the @c tails (δ /
 * duplicate) primitive, with the three Uustalu–Vene comonad laws
 * verified pointwise on a sample stream.
 *
 * The counit is the head @f$\varepsilon(s) = s(0)@f$ (the project's
 * @c counit_witness); the comultiplication is @c tails (δ), the stream
 * of all suffixes; @c extend is @c operator<<=.  The laws:
 *
 *   - left  counit:  ε(δ s)            = s          head of tails
 *   - right counit:  (map ε)(δ s)      = s          sampling each tail's head
 *   - coassoc:       δ(δ s)            = (map δ)(δ s)
 *
 * Path generators are @c std::function-backed, so the laws cannot be
 * @c static_assert-ed (no constexpr std::function); they are exercised
 * pointwise at runtime on a finite window of indices.
 */

#include <catch2/catch_test_macros.hpp>
#include <cstddef>

import dedekind.sequences;

using namespace dedekind::sequences;

namespace {

/** @brief The naturals 0,1,2,… as an infinite stream. */
Path<int> naturals() {
  return Path<int>{[](std::size_t n) { return static_cast<int>(n); }};
}

constexpr std::size_t window = 6;

}  // namespace

TEST_CASE(
    "sequences:comonad — Path carries the operational stream-comonad shape",
    "[sequences][comonad][stream]") {
  STATIC_CHECK(IsStreamComonad<Path<int>>);
  // The infinite-stream comonad (δ = tails) is not the non-empty-list
  // comonad on finite carriers, so a FinitePath is honestly rejected.
  STATIC_CHECK_FALSE(IsStreamComonad<FinitePath<int>>);
}

TEST_CASE("sequences:comonad — tails(s)(n) is the n-shifted tail (= drop)",
          "[sequences][comonad][tails]") {
  const auto s = naturals();
  const auto ts = tails(s);
  for (std::size_t n = 0; n < window; ++n) {
    const auto tail_n = ts.at(n);
    for (std::size_t i = 0; i < window; ++i) {
      REQUIRE(tail_n.at(i) == static_cast<int>(n + i));
      REQUIRE(tail_n.at(i) == drop(s, n).at(i));
    }
  }
}

TEST_CASE("sequences:comonad — left counit: ε(δ s) = s (head of tails)",
          "[sequences][comonad][law]") {
  const auto s = naturals();
  const auto head_of_tails = tails(s).at(0);  // ε(δ s) = (δ s)(0) = drop(s,0)
  for (std::size_t i = 0; i < window; ++i) {
    REQUIRE(head_of_tails.at(i) == s.at(i));
  }
}

TEST_CASE("sequences:comonad — right counit: (map ε)(δ s) = s",
          "[sequences][comonad][law]") {
  const auto s = naturals();
  const auto ts = tails(s);
  // map ε over the stream of tails: n ↦ head(tails(s)(n)) = (tails s)(n).at(0)
  for (std::size_t n = 0; n < window; ++n) {
    REQUIRE(ts.at(n).at(0) == s.at(n));
  }
}

TEST_CASE("sequences:comonad — coassociativity: δ(δ s) = (map δ)(δ s)",
          "[sequences][comonad][law]") {
  const auto s = naturals();
  const auto ts = tails(s);
  const auto lhs = tails(ts);  // δ(δ s) : stream of tails-of-(stream of tails)
  // (map δ)(δ s) : n ↦ tails((δ s)(n)) = tails(drop(s, n))
  for (std::size_t n = 0; n < window; ++n) {
    const auto lhs_n = lhs.at(n);  // a stream of streams
    const auto rhs_n = tails(ts.at(n));
    for (std::size_t j = 0; j < window; ++j) {
      for (std::size_t i = 0; i < window; ++i) {
        REQUIRE(lhs_n.at(j).at(i) == rhs_n.at(j).at(i));
      }
    }
  }
}

TEST_CASE("sequences:comonad — extend (<<=) agrees with mapping over tails",
          "[sequences][comonad][extend]") {
  const auto s = naturals();
  // A context-aware extension: the first forward difference s(n+1) - s(n).
  const auto fwd_diff = [](const Path<int>& ctx) {
    return ctx.at(1) - ctx.at(0);
  };
  const auto extended = s <<= fwd_diff;
  // extend f == map (f) . tails, so extended(n) == f(tails(s)(n)).
  const auto ts = tails(s);
  for (std::size_t n = 0; n < window; ++n) {
    REQUIRE(extended.at(n) == 1);  // naturals: constant +1
    REQUIRE(extended.at(n) == fwd_diff(ts.at(n)));
  }
}
