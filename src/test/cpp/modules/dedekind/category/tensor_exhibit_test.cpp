/**
 * @file tensor_exhibit_test.cpp
 * @brief The tensor exhibit of the Jlt paper (#1104), in progress: periodic
 *        vectors built from the library's arrows, the laws inferred through
 *        the combinators, their outer product beyond 2^128 entries, and its
 *        counts, over the box, over a window and over a modular section, all
 *        at compile time; each law checked against the plain fold where both
 *        can run.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <algorithm>
#include <array>
#include <bit>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <ranges>
#include <type_traits>
#include <utility>
#include <vector>

import dedekind.category;

namespace tensor_exhibit {
using dedekind::category::chain;
using dedekind::category::count;
using dedekind::category::count_factors;
using dedekind::category::count_section;
using dedekind::category::equals;
using dedekind::category::fibonacci_mod;
using dedekind::category::is_even;
using dedekind::category::opaque;
using dedekind::category::period;
using dedekind::category::rolling_count;
using dedekind::category::shift;
using dedekind::category::sum;
using std::size_t;

// The library's arrows: the even counts, the even Fibonacci numbers, and the
// periods inferred through composition, the involution, a join and a window.
constexpr auto fib_even = fibonacci_mod(2) >> equals(size_t{0});
constexpr auto odd = !is_even;
constexpr auto near = shift(1) >> fib_even | shift(2) >> fib_even;
static_assert(period(is_even) == 2 && period(fib_even) == 3 &&
                  period(odd) == 2 && period(near) == 3,
              "declared at the generators, inferred through >>, ! and |");

// The brute force over a two-leg box at small sizes: every entry, evaluated
// through the box's own entry arrow, the Tensor of the legs and their meet.
template <typename E>
struct Row {
  E e;
  size_t i;
  size_t m;
  size_t r;
  constexpr size_t operator()(size_t c, size_t j) const {
    const bool in = m == 0 || (i + j) % m == r;
    return c + (in && e(std::pair{i, j}) ? 1 : 0);
  }
};
template <typename E>
struct Rows {
  E e;
  size_t nb;
  size_t m;
  size_t r;
  constexpr size_t operator()(size_t c, size_t i) const {
    return std::ranges::fold_left(std::views::iota(size_t{0}, nb), c,
                                  Row<E>{e, i, m, r});
  }
};
template <typename B>
constexpr size_t brute(const B& b, size_t m, size_t r) {
  return std::ranges::fold_left(
      std::views::iota(size_t{0}, b.sides[0]), size_t{0},
      Rows<decltype(b.entry)>{b.entry, b.sides[1], m, r});
}

// The laws, checked against the brute force where both can run: the box, its
// modular section, and the plain fold of a window.
constexpr auto small =
    (24uz >> chain<size_t>() >> near) * (18uz >> chain<size_t>() >> odd);
static_assert(count(small) == brute(small, 0, 0) &&
              count_section(small, 3, 0) == brute(small, 3, 0) &&
              count_section(small, 5, 2) == brute(small, 5, 2));
static_assert(sum(120uz >> chain<size_t>() >> rolling_count(near, 4)) ==
              sum(120uz >> chain<size_t>() >> opaque(rolling_count(near, 4))));

// A leg with no law: the vowels among the letters, a chain of bytes
// restricted by value to ['a', '{'), counted by the plain fold.  A byte chain
// has the covering step and no arithmetic, so it is a chain and no word.
struct IsVowel {
  using Domain = std::byte;
  using Codomain = bool;
  constexpr bool operator()(std::byte b) const {
    const char c = std::to_integer<char>(b);
    return c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u';
  }
};
constexpr auto letters =
    dedekind::category::slice(std::byte{'a'}, std::byte{'{'});
static_assert(count(letters >> IsVowel{}) == 5 &&
              !dedekind::category::IsIntegerChain<std::byte> &&
              dedekind::category::HasNNOStep<std::byte>);

// The exhibit: five vectors of 2^60 entries, 2^300 in the box.
constexpr size_t n = size_t{1} << 60;
constexpr auto evens = n >> chain<size_t>() >> is_even;
constexpr auto odds = n >> chain<size_t>() >> odd;
constexpr auto fibs = n >> chain<size_t>() >> fib_even;
constexpr auto box = evens * odds * fibs * evens * fibs;
static_assert(count_factors(box) ==
              std::array<size_t, 5>{n / 2, n / 2, n / 3 + 1, n / 2, n / 3 + 1});
static_assert(count_section(evens* odds, 2, 0) == 0 &&
                  count_section(evens * odds, 2, 1) == count(evens * odds),
              "an even and an odd index sum to an odd one: the section "
              "i + j ≡ 0 (mod 2) is empty, and i + j ≡ 1 (mod 2) is the box");

// The genome, a tensor and not a sequence of strings: the capitals, filtered
// to the bases, and the free monoid on them below a length.
using dedekind::category::count_factors;
using dedekind::category::exact_count;
using dedekind::category::free_monoid;
using dedekind::category::power;
using dedekind::category::Reversal;
using dedekind::category::symmetric;
using Factor = dedekind::category::Factor<>;
using Factors = std::vector<Factor>;
// The bases among the capitals, and the complement A↔T, C↔G: an alphabet
// and an involution on it, each written as its letters.
constexpr auto IsBase = dedekind::category::letters("ACGT");
constexpr auto Complement = dedekind::category::letter_map("ACGT", "TGCA");
constexpr auto capitals =
    dedekind::category::slice(std::byte{'A'}, std::byte{'['});
constexpr auto bases = capitals >> IsBase;
constexpr auto same_letter = dedekind::category::id<std::byte>();
static_assert(count(capitals >>
                    dedekind::category::constant<std::byte>(true)) == 26 &&
              count(bases) == 4);

// Refused as strings: the words shorter than 100 are about 2^200, no sequence
// a size_t can index; counted as a tensor, length by length, exactly.
static_assert(!exact_count(free_monoid(bases, 100)).has_value() &&
              exact_count(free_monoid(bases, 4)) == 85uz);
// Its symmetries are merges of legs: the palindromes merge leg i with leg
// n - 1 - i; through the complement, the reverse-complement palindromes, the
// restriction sites, of which there are none of odd length, since no base is
// its own complement.
static_assert(
    count_factors(power(bases, 99)) == Factors{{4, 99}} &&
    count_factors(symmetric(power(bases, 12), Reversal{}, same_letter)) ==
        Factors{{4, 6}} &&
    count_factors(symmetric(power(bases, 13), Reversal{}, same_letter)) ==
        Factors{{4, 6}, {4, 1}} &&
    count_factors(symmetric(power(bases, 12), Reversal{}, Complement)) ==
        Factors{{4, 6}} &&
    exact_count(count_factors(symmetric(power(bases, 13), Reversal{},
                                        Complement))) == 0uz);

// The same counts by brute force over the 4^L words of length L, each word
// read off its index in base 4: the sequence a size_t can index, here; 4^4 at
// compile time, within the evaluator's step limit, and 4^6 at run time.
constexpr std::array<std::byte, 4> acgt{std::byte{'A'}, std::byte{'C'},
                                        std::byte{'G'}, std::byte{'T'}};
constexpr std::byte letter_of(size_t word, size_t i) {
  return acgt[(word >> (2 * i)) & 3U];
}
template <typename Tau>
struct Mirrors {
  size_t word;
  size_t length;
  Tau tau;
  constexpr bool operator()(bool ok, size_t i) const {
    return ok && letter_of(word, i) == tau(letter_of(word, length - 1 - i));
  }
};
template <typename Tau>
struct IsSymmetric {
  using Domain = size_t;
  using Codomain = bool;
  size_t length;
  Tau tau;
  constexpr bool operator()(size_t word) const {
    return std::ranges::fold_left(std::views::iota(size_t{0}, length), true,
                                  Mirrors<Tau>{word, length, tau});
  }
};
using SameLetter = std::remove_cvref_t<decltype(same_letter)>;
static_assert(
    count(256uz >> chain<size_t>() >>
          dedekind::category::opaque(IsSymmetric<dedekind::category::LetterMap>{
              4, Complement})) ==
        exact_count(count_factors(symmetric(power(bases, 4), Reversal{},
                                            Complement))) &&
    count(256uz >> chain<size_t>() >>
          dedekind::category::opaque(IsSymmetric<SameLetter>{
              4, same_letter})) == 16);

// The bit patterns of 2^7 = 128 bits that are proper powers: for a width a
// power of two, exactly the squares u u, fixed by the rotation by half the
// width, one merge of 64 pairs of legs; 2^64 of them, one bit past size_t.
using Rotation = dedekind::category::Rotation<>;
constexpr auto bits = dedekind::category::slice(0uz, 2uz) >>
                      dedekind::category::constant<size_t>(true);
constexpr size_t width = dedekind::category::pow2(7);
constexpr auto squares = symmetric(power(bits, width), Rotation{width / 2},
                                   dedekind::category::id<size_t>());
static_assert(width == 128 && count_factors(squares) == Factors{{2, 64}} &&
              !exact_count(count_factors(squares)).has_value());
// The theorem by brute force on 8 bits: the patterns fixed by some nontrivial
// rotation are the squares, 2^4 of them, as the tensor counts.
struct FixedBy {
  std::uint8_t x;
  constexpr bool operator()(bool any, int d) const {
    return any || std::rotl(x, d) == x;
  }
};
struct IsProperPower8 {
  using Domain = std::uint8_t;
  using Codomain = bool;
  constexpr bool operator()(std::uint8_t x) const {
    return std::ranges::fold_left(std::views::iota(1, 8), false, FixedBy{x});
  }
};
using dedekind::category::bit_patterns;
using dedekind::category::IsSquare;
using dedekind::category::pack;
using dedekind::category::unpack;
static_assert(count(bit_patterns(8) >> (pack<8> >> IsProperPower8{})) == 16 &&
              count(bit_patterns(8) >> (pack<8> >> IsSquare<8>{})) == 16 &&
              exact_count(count_factors(
                  symmetric(power(bits, 8uz), Rotation{4},
                            dedekind::category::id<size_t>()))) == 16uz);

// Packing switches the representation and loses nothing: unpack retracts it
// on the patterns the word holds; a pattern narrower than its word is rotated
// within its width, the padding apart.
static_assert((pack<8> >> unpack<8>)(0xA5uz) == 0xA5 &&
              (pack<64> >> unpack<64>)(~0uz) == ~0uz &&
              count(bit_patterns(4) >> (pack<8> >> IsSquare<8>{4})) == 4 &&
              IsSquare<128>{}(pack<128>(0uz)) &&
              !IsSquare<128>{}(pack<128>(1uz)));

// A count is a monoid concat under * over the factors, each e copies of a
// popcount, which concat squares: it agrees with the exact count wherever
// the word holds it, and is its residue where it does not.
static_assert(count(count_factors(symmetric(power(bases, 13), Reversal{},
                                            same_letter))) ==
                  exact_count(count_factors(
                      symmetric(power(bases, 13), Reversal{}, same_letter))) &&
              count(count_factors(symmetric(power(bases, 13), Reversal{},
                                            Complement))) == 0 &&
              count(Factors{{3, 40}}) == 12'157'665'459'056'928'801uz &&
              count(Factors{{2, 64}}) == 0);

// The exhibit: a tensor of the two, the palindromes of length 99 over the
// bases and the squares of 128 bits, 2^100 · 2^64 entries, counted as factors.
constexpr auto exhibit =
    symmetric(power(bases, 99), Reversal{}, same_letter) * squares;
static_assert(count_factors(exhibit) == Factors{{4, 49}, {4, 1}, {2, 64}} &&
              !exact_count(count_factors(exhibit)).has_value());
}  // namespace tensor_exhibit

TEST_CASE("Tensor exhibit: a count over a vector of 2^60 entries, at run time",
          "[category][tensor_exhibit]") {
  using std::size_t;
  size_t lo = 60;
  const size_t n = size_t{1} << lo;
  CHECK(tensor_exhibit::count(n >> dedekind::category::chain<size_t>() >>
                              dedekind::category::is_even) == n / 2);
}

TEST_CASE("Tensor exhibit: the genome's symmetries by brute force, at run time",
          "[category][tensor_exhibit]") {
  using namespace tensor_exhibit;
  size_t length = 6;  // 4^6 words, past the evaluator's steps
  const size_t words = size_t{1} << (2 * length);
  CHECK(count(words >> chain<size_t>() >>
              dedekind::category::opaque(
                  IsSymmetric<dedekind::category::LetterMap>{length,
                                                             Complement})) ==
        exact_count(count_factors(
            symmetric(power(bases, length), Reversal{}, Complement))));
  CHECK(count(words >> chain<size_t>() >>
              dedekind::category::opaque(
                  IsSymmetric<SameLetter>{length, same_letter})) ==
        exact_count(count_factors(
            symmetric(power(bases, length), Reversal{}, same_letter))));
}

TEST_CASE(
    "Tensor exhibit: the squares by packed brute force, a width at run time",
    "[category][tensor_exhibit]") {
  using namespace tensor_exhibit;
  // A width known only at run time picks the narrowest precompiled word.
  for (size_t w : {size_t{4}, size_t{8}, size_t{16}}) {
    const auto squares_of = [w](auto word) {
      constexpr size_t W = decltype(word)::value;
      return count(bit_patterns(w) >> (pack<W> >> IsSquare<W>{w}));
    };
    CHECK(dedekind::category::with_width(w, squares_of) ==
          exact_count(
              count_factors(symmetric(power(bits, w), Rotation{w / 2},
                                      dedekind::category::id<size_t>()))));
  }
}
