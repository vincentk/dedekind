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
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <ranges>
#include <span>
#include <string_view>
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
// to the nucleotides, and the free monoid on them below a length.
using dedekind::category::count_factors;
using dedekind::category::exact_count;
using dedekind::category::free_monoid;
using dedekind::category::power;
using dedekind::category::reverse;
using dedekind::category::rotate;
using dedekind::category::symmetric;
using Factor = dedekind::category::Factor<>;
using Factors = std::vector<Factor>;
// The nucleotides among the capitals, and their Watson–Crick pairing A↔T,
// C↔G: an alphabet and an involution on it, each written as its letters.
constexpr auto capitals =
    dedekind::category::slice(std::byte{'A'}, std::byte{'['});
constexpr auto nucleotides = capitals >> dedekind::category::letters("ACGT");
constexpr auto complement = dedekind::category::letter_map("ACGT", "TGCA");
static_assert(count(capitals >>
                    dedekind::category::constant<std::byte>(true)) == 26 &&
              count(nucleotides) == 4);

// The reverse complement, the other strand read in its own direction: the
// complement on every letter, then the reversal of the positions.  Two
// involutions that commute, so one involution, derived from the legs; the
// complement's own law decided on its table.  Listing 1's ~ and - on size_t
// are the two that do not commute.
using dedekind::category::is_involution;
using Word = std::vector<std::byte>;
struct ToByte {
  constexpr std::byte operator()(char c) const {
    return std::byte{static_cast<unsigned char>(c)};
  }
};
constexpr Word dna(std::string_view s) {
  const auto bytes = s | std::views::transform(ToByte{});
  return Word(bytes.begin(), bytes.end());
}
constexpr auto reverse_complement = power(complement) >> reverse;
constexpr auto complement_reversed = dedekind::category::endo<Word>(reverse) >>
                                     power(complement);
static_assert(is_involution(complement) &&
              !is_involution(dedekind::category::letter_map("ACGT", "CGTA")) &&
              is_involution(reverse_complement) &&
              is_involution(complement_reversed) &&
              reverse_complement(dna("GAATTC")) == dna("GAATTC") &&
              reverse_complement(dna("GATTAC")) == dna("GTAATC") &&
              complement_reversed(dna("GATTAC")) == dna("GTAATC"));

// Refused as strings: the words shorter than 100 are about 2^200, no sequence
// a size_t can index; counted as a tensor, length by length, exactly.
static_assert(!exact_count(free_monoid(nucleotides, 100)).has_value() &&
              exact_count(free_monoid(nucleotides, 4)) == 85uz);
// Its symmetries are merges of legs: the palindromes merge leg i with leg
// n - 1 - i; through the complement, the reverse-complement palindromes, the
// restriction sites, of which there are none of odd length, since no base is
// its own complement.
static_assert(
    count_factors(power(nucleotides, 99)) == Factors{{4, 99}} &&
    count_factors(symmetric(power(nucleotides, 12), reverse)) ==
        Factors{{4, 6}} &&
    count_factors(symmetric(power(nucleotides, 13), reverse)) ==
        Factors{{4, 6}, {4, 1}} &&
    count_factors(symmetric(power(nucleotides, 12),
                            power(complement) >> reverse)) == Factors{{4, 6}} &&
    exact_count(count_factors(symmetric(power(nucleotides, 13),
                                        power(complement) >> reverse))) == 0uz);

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
  size_t offset;
  size_t length;
  Tau tau;
  constexpr bool operator()(bool ok, size_t i) const {
    return ok && letter_of(word, offset + i) ==
                     tau(letter_of(word, offset + length - 1 - i));
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
                                  Mirrors<Tau>{word, 0, length, tau});
  }
};
// The words whose window from offset, length letters long, the reverse
// complement fixes: a restriction site there, the rest of the word free.
template <typename Tau>
struct IsSymmetricAt {
  using Domain = size_t;
  using Codomain = bool;
  size_t offset;
  size_t length;
  Tau tau;
  constexpr bool operator()(size_t word) const {
    return std::ranges::fold_left(std::views::iota(size_t{0}, length), true,
                                  Mirrors<Tau>{word, offset, length, tau});
  }
};
using SameLetter = dedekind::category::Identity<std::byte>;
static_assert(
    count(256uz >> chain<size_t>() >>
          dedekind::category::opaque(IsSymmetric<dedekind::category::LetterMap>{
              4, complement})) ==
        exact_count(count_factors(symmetric(power(nucleotides, 4),
                                            power(complement) >> reverse))) &&
    count(256uz >> chain<size_t>() >>
          dedekind::category::opaque(IsSymmetric<SameLetter>{
              4, SameLetter{}})) == 16);

// A site in a window of a longer genome, the cylinder over the sites: the
// window's legs merged through the reverse complement, every other leg free,
// so the sites' count times 4 per free leg, wherever the window sits
// (aggregate pushdown).  A window past the end, or sites of another length,
// leave it empty.
using dedekind::category::cylinder;
using dedekind::category::window;
constexpr auto sites_6 =
    symmetric(power(nucleotides, 6), power(complement) >> reverse);
static_assert(window(1, 3)(dna("GAATTCA")) == dna("AAT") &&
              window(5, 3)(dna("GAATTC")) == dna("C") &&
              window(9, 3)(dna("GAATTC")).empty());
static_assert(
    count_factors(cylinder(power(nucleotides, 12), window(3, 6), sites_6)) ==
        Factors{{4, 3}, {4, 6}} &&
    count_factors(cylinder(power(nucleotides, 12), window(0, 6), sites_6)) ==
        count_factors(cylinder(power(nucleotides, 12), window(6, 6),
                               sites_6)) &&
    exact_count(count_factors(cylinder(power(nucleotides, 12), window(3, 6),
                                       sites_6))) == 262'144uz &&
    exact_count(count_factors(cylinder(power(nucleotides, 12), window(7, 6),
                                       sites_6))) == 0uz &&
    exact_count(count_factors(cylinder(power(nucleotides, 12), window(3, 4),
                                       sites_6))) == 0uz);
// The same by brute force: the 4^4 words of length 4 with a site of length 2
// at each offset, and at run time the 4^6 words with a site of length 4.
constexpr auto sites_2 =
    symmetric(power(nucleotides, 2), power(complement) >> reverse);
template <size_t Offset>
constexpr bool pushed_down_at_4() {
  return count(256uz >> chain<size_t>() >>
               dedekind::category::opaque(
                   IsSymmetricAt<dedekind::category::LetterMap>{Offset, 2,
                                                                complement})) ==
         exact_count(count_factors(
             cylinder(power(nucleotides, 4), window(Offset, 2), sites_2)));
}
static_assert(pushed_down_at_4<0>() && pushed_down_at_4<1>() &&
              pushed_down_at_4<2>());

// The bit patterns of 2^7 = 128 bits that are proper powers: for a width a
// power of two, exactly the squares u u, fixed by the rotation by half the
// width, one merge of 64 pairs of legs; 2^64 of them, one bit past size_t.
constexpr auto bits = dedekind::category::slice(0uz, 2uz) >>
                      dedekind::category::constant<size_t>(true);
constexpr size_t width = dedekind::category::pow2(7);
constexpr auto squares = symmetric(power(bits, width), rotate(width / 2));
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
              exact_count(count_factors(symmetric(power(bits, 8uz),
                                                  rotate(4)))) == 16uz);

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
static_assert(
    count(count_factors(symmetric(power(nucleotides, 13), reverse))) ==
        exact_count(count_factors(symmetric(power(nucleotides, 13),
                                            reverse))) &&
    count(count_factors(symmetric(power(nucleotides, 13),
                                  power(complement) >> reverse))) == 0 &&
    count(Factors{{3, 40}}) == 12'157'665'459'056'928'801uz &&
    count(Factors{{2, 64}}) == 0);

// The exhibit: a tensor of the two, the palindromes of length 99 over the
// nucleotides and the squares of 128 bits, 2^100 · 2^64 entries, counted as
// factors.
constexpr auto exhibit = symmetric(power(nucleotides, 99), reverse) * squares;
static_assert(count_factors(exhibit) == Factors{{4, 49}, {4, 1}, {2, 64}} &&
              !exact_count(count_factors(exhibit)).has_value());

// Review witnesses (#1108): an empty factor empties the product whatever
// overflows beside it, in either order; a power of 0 or 1 at once, and an
// overflowing one by squaring, within the evaluator's steps; an alphabet's
// letters are the slice's AND the predicate's; a rotation on the letters
// themselves counts each cycle once; no square of width 0; a narrow bit word
// reads back through its bitset; the reversal refuses a view, whose copy
// aliases the caller's letters.
static_assert(exact_count(Factors{{4, 49}, {0, 1}}) == 0uz &&
              exact_count(Factors{{0, 1}, {4, 49}}) == 0uz &&
              exact_count(Factors{{1, 1'000'000'000}}) == 1uz &&
              !exact_count(Factors{{2, 1'000'000'000}}).has_value() &&
              exact_count(Factors{{0, 0}}) == 1uz);
constexpr auto just_a = dedekind::category::slice(std::byte{'A'},
                                                  std::byte{'B'}) >>
                        dedekind::category::letters("AB");
static_assert(exact_count(count_factors(
                  symmetric(power(just_a, 2),
                            power(dedekind::category::letter_map("AB", "BA")) >>
                                reverse))) == 0uz);
static_assert(count_factors(symmetric(power(bits, 4096uz), rotate(1))) ==
              Factors{{2, 1}});
// The words shorter than a billion: refused at once, by the width, not after
// visiting the lengths; over one letter, one word per length.
constexpr auto just_g = dedekind::category::slice(std::byte{'G'},
                                                  std::byte{'H'}) >>
                        dedekind::category::letters("G");
static_assert(
    !exact_count(free_monoid(nucleotides, 1'000'000'000)).has_value() &&
    exact_count(free_monoid(just_g, 1'000'000'000)) == 1'000'000'000uz &&
    exact_count(free_monoid(nucleotides, 0)) == 0uz &&
    exact_count(free_monoid(nucleotides, 32)) == 6'148'914'691'236'517'205uz &&
    !exact_count(free_monoid(nucleotides, 33)).has_value());
static_assert(!IsSquare<8>{0}(pack<8>(0uz)) && unpack<4>(pack<4>(5uz)) == 5uz &&
              unpack<12>(pack<12>(0xABCuz)) == 0xABCuz);
static_assert(!std::invocable<dedekind::category::Reverse, std::span<int>> &&
              std::invocable<dedekind::category::Reverse, std::vector<int>>);
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
                                                             complement})) ==
        exact_count(count_factors(symmetric(power(nucleotides, length),
                                            power(complement) >> reverse))));
  CHECK(count(words >> chain<size_t>() >>
              dedekind::category::opaque(
                  IsSymmetric<SameLetter>{length, SameLetter{}})) ==
        exact_count(
            count_factors(symmetric(power(nucleotides, length), reverse))));
  // A site of length 4 in the window from 1: the cylinder's count, pushed
  // down, against all 4^6 words.
  CHECK(count(words >> chain<size_t>() >>
              dedekind::category::opaque(
                  IsSymmetricAt<dedekind::category::LetterMap>{1, 4,
                                                               complement})) ==
        exact_count(count_factors(cylinder(
            power(nucleotides, length), window(1, 4),
            symmetric(power(nucleotides, 4), power(complement) >> reverse)))));
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
          exact_count(count_factors(symmetric(power(bits, w), rotate(w / 2)))));
  }
}
