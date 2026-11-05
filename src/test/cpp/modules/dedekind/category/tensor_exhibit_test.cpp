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
#include <optional>
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
using dedekind::category::rev;
using dedekind::category::rotate;
using dedekind::category::symmetric;
using dedekind::category::windows;
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
using dedekind::category::is_antitone;
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
constexpr auto reverse_complement = power(complement) >> rev;
constexpr auto complement_reversed = rev >> power(complement);
static_assert(is_involution(complement) &&
              !is_involution(dedekind::category::letter_map("ACGT", "CGTA")) &&
              is_involution(reverse_complement) &&
              is_involution(complement_reversed) &&
              reverse_complement(dna("GAATTC")) == dna("GAATTC") &&
              reverse_complement(dna("GATTAC")) == dna("GTAATC") &&
              complement_reversed(dna("GATTAC")) == dna("GTAATC"));

// The pairing reverses the bases' order, A < C < G < T as bytes, because of
// how the one-letter codes fall; another map need not.  An antitone bijection
// of a finite chain is its reversal: two pairs of four letters and none
// fixed, so no base is its own complement, which the count of the odd sites
// reads from the table all the same.
using dedekind::category::antitone_cycle_type;
using dedekind::category::Cycles;
constexpr auto base_letters = dedekind::category::letters("ACGT");
static_assert(is_antitone(complement, base_letters) &&
              !is_antitone(dedekind::category::letter_map("ACGT", "CGTA"),
                           base_letters) &&
              std::ranges::equal(antitone_cycle_type(4uz),
                                 std::array{Cycles<>{2, 2}}) &&
              std::ranges::equal(antitone_cycle_type(5uz),
                                 std::array{Cycles<>{2, 2}, Cycles<>{1, 1}}) &&
              exact_count(count_factors(symmetric(power(nucleotides, 13),
                                                  power(complement) >> rev))) ==
                  0uz);

// Refused as strings: the words shorter than 100 are about 2^200, no sequence
// a size_t can index; counted as a tensor, length by length, exactly.
static_assert(!exact_count(free_monoid(nucleotides, 100)).has_value() &&
              exact_count(free_monoid(nucleotides, 4)) == 85uz);
// Its symmetries are merges of legs: the palindromes merge leg i with leg
// n - 1 - i; through the complement, the reverse-complement palindromes, the
// restriction sites, of which there are none of odd length, since no base is
// its own complement.
static_assert(
    std::ranges::equal(count_factors(power(nucleotides, 99)),
                       Factors{{4, 99}}) &&
    std::ranges::equal(count_factors(symmetric(power(nucleotides, 12), rev)),
                       Factors{{4, 6}}) &&
    std::ranges::equal(count_factors(symmetric(power(nucleotides, 13), rev)),
                       Factors{{4, 6}, {4, 1}}) &&
    std::ranges::equal(count_factors(symmetric(power(nucleotides, 12),
                                               power(complement) >> rev)),
                       Factors{{4, 6}}) &&
    exact_count(count_factors(symmetric(power(nucleotides, 13),
                                        power(complement) >> rev))) == 0uz);

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
                                            power(complement) >> rev))) &&
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
    symmetric(power(nucleotides, 6), power(complement) >> rev);
static_assert(window(1, 3)(dna("GAATTCA")) == dna("AAT") &&
              window(5, 3)(dna("GAATTC")) == dna("C") &&
              window(9, 3)(dna("GAATTC")).empty());
static_assert(
    std::ranges::equal(count_factors(cylinder(power(nucleotides, 12),
                                              window(3, 6), sites_6)),
                       Factors{{4, 3}, {4, 6}}) &&
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
    symmetric(power(nucleotides, 2), power(complement) >> rev);
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
// Over every offset: the occurrences, with their positions, of a site of
// length 6 in all genomes of length 12, 7 · 4^3 · 4^6; and in genomes of
// length 40, more than a word holds, refused exactly and kept modulo it.
using dedekind::category::exact_occurrences;
using dedekind::category::occurrences;
static_assert(occurrences(power(nucleotides, 12), sites_6) == 1'835'008uz &&
              exact_occurrences(power(nucleotides, 12), sites_6) ==
                  1'835'008uz &&
              occurrences(power(nucleotides, 4), sites_6) == 0uz &&
              !exact_occurrences(power(nucleotides, 40), sites_6).has_value());
// The plan of a cylinder, its selection pushed down: the sites times the
// power on the six free legs, an outer product whatever the offset, its
// count the box law's.  EXPLAIN as a static_assert.
using dedekind::category::Outer;
using dedekind::category::pushed_down;
static_assert(
    std::same_as<decltype(pushed_down(cylinder(power(nucleotides, 12),
                                               window(3, 6), sites_6))),
                 Outer<std::remove_cvref_t<decltype(sites_6)>,
                       std::remove_cvref_t<decltype(power(nucleotides, 6))>>>);
// The join's plan: the seven offsets times the cylinder at the first, whose
// selection is pushed down in turn; its count the box law's throughout,
// 7 · 4^3 · 4^6 as factors, no sum over the offsets left.
using dedekind::category::explain;
using dedekind::category::Offsets;
static_assert(
    std::same_as<decltype(explain((power(nucleotides, 12) >> sites_6))),
                 Outer<Offsets<std::size_t>,
                       std::remove_cvref_t<decltype(cylinder(
                           power(nucleotides, 12), window(0, 6), sites_6))>>> &&
    std::ranges::equal(count_factors((power(nucleotides, 12) >> sites_6)),
                       Factors{{7, 1}, {4, 3}, {4, 6}}));
// Which site lengths cut most often: the even lengths from 2 to 12, ranked
// by their occurrences in all genomes of length 12, the first five.  The rank
// is four generic arrows composed (the powers, the sections the reverse
// complement fixes, the join, the count), and its law composes with them:
// the reversal is length-coherent, so the sites' density falls along the
// even lengths, so their occurrences do, and they rank shortest first by
// construction: the plan reads the class of even lengths upwards and
// evaluates no count.  With the law hidden, the fused pass ranks by the
// counts and agrees.
using dedekind::category::collect;
using dedekind::category::counting;
using dedekind::category::descending;
using dedekind::category::fixed_by;
using dedekind::category::join_with;
using dedekind::category::order_by;
using dedekind::category::take;
using dedekind::category::where;
using dedekind::category::words;
constexpr auto cuts_in_12 = words(nucleotides) >>
                            fixed_by(power(complement) >> rev) >>
                            join_with(power(nucleotides, 12)) >> counting;
static_assert(cuts_in_12(6uz) == 1'835'008uz && cuts_in_12(7uz) == 0uz);
// Passes by the law, fails without it: genomes of length 32, the longest
// whose count still fits a size_t, (32 - 5) · 4^29 = 27 · 2^58 of them cut
// by a six-letter site.  The plan is a handful of factors at compile time;
// the 4^32 = 2^64 genomes it stands for are more than a size_t counts, so
// no evaluation that enumerates them could finish, at either binding time.
constexpr auto cuts_in_32 = words(nucleotides) >>
                            fixed_by(power(complement) >> rev) >>
                            join_with(power(nucleotides, 32)) >> counting;
static_assert(cuts_in_32(6uz) == 27uz << 58);
static_assert(std::ranges::equal(collect(dedekind::category::slice(2uz, 13uz) >>
                                         where(dedekind::category::is_even) >>
                                         order_by<descending>(cuts_in_12) >>
                                         take<5>()),
                                 std::array{2uz, 4uz, 6uz, 8uz, 10uz}));
TEST_CASE("Category: the fused ranking agrees with the law's",
          "[category][tensor][comprehension]") {
  // the run-time fallback: the counts evaluated and compared
  CHECK(std::ranges::equal(
      collect(dedekind::category::slice(2uz, 13uz) >>
              where(dedekind::category::is_even) >>
              order_by<descending>(opaque(cuts_in_12)) >> take<5>()),
      std::array{2uz, 4uz, 6uz, 8uz, 10uz}));
}
// The word, chosen by law: the join's count needs at most 22 bits by its
// factors (7 · 4^3 · 4^6: 3 + 6 + 12, and one), read before it is computed;
// it needs 21.  The narrowest count word holds it, and counted in that word
// it is the same count.
using dedekind::category::bits_needed;
using dedekind::category::narrowest_count_word;
static_assert(bits_needed(count_factors((power(nucleotides, 12) >> sites_6))) ==
                  22 &&
              std::bit_width(1'835'008uz) == 21);
using SiteCount = narrowest_count_word<bits_needed(
    count_factors((power(nucleotides, 12) >> sites_6)))>;
static_assert(std::same_as<SiteCount, std::uint32_t>);
static_assert(occurrences(power<SiteCount>(nucleotides, 12),
                          symmetric(power<SiteCount>(nucleotides, 6),
                                    power(complement) >> rev)) == 1'835'008u);
// The same count as a query: COUNT(*) over the join on the window, the
// power read through the sites.
static_assert(count((power(nucleotides, 12) >> sites_6)) == 1'835'008uz &&
              exact_count((power(nucleotides, 12) >> sites_6)) == 1'835'008uz &&
              !exact_count((power(nucleotides, 40) >> sites_6)).has_value());

// Listing 5, all genomes: the README's and the paper's text, verbatim between
// the markers (src/test/python/test_readme_listing5.py checks it), beside
// pynzli.jlt's doctest of the same.
namespace listing5 {
using dedekind::category::abc;
using dedekind::category::Alphabet;
using dedekind::category::fixed_by;
using dedekind::category::len;
using dedekind::category::words;
using dedekind::category::Words;
// [listing5:all-genomes]
// Σ, the alphabet :
constexpr Alphabet bases = abc("ACGT");
// Σ* = ⊔ₙ Σⁿ :
constexpr Words oligos = words(bases);
// ~rev, the reverse complement: CCTA ↦ TAGG
// { w ∈ Σ⁶ | ~rev w = w }, the palindromic hexamers :
constexpr auto pali6 = oligos & len(6) & fixed_by(~rev);
// { (s, i) ∈ Σᴺ × ℕ | s[i, i+6) ∈ pali6 } :
constexpr auto cuts(size_t N) { return (oligos & len(N)) >> pali6; }
static_assert(count(cuts(12)) == 1'835'008uz && count(cuts(32)) == 27uz << 58);
// [/listing5:all-genomes]
// The meets commute: the length then the symmetry, or the symmetry then
// the length, is one section; the complement is the alphabet's own, its
// reflection; and no odd palindrome, as no base is its own complement.
static_assert(
    exact_count(count_factors(pali6)) ==
        exact_count(count_factors(oligos & fixed_by(~rev) & len(6uz))) &&
    pali6.tau == dedekind::category::refl(bases) &&
    exact_count(count_factors(oligos & fixed_by(~rev) & len(7uz))) == 0uz);

// Listing 5, one genome: the two lines, the README's text between the markers.
// The query tests each window against the section; the plan is the matcher
// specialised to the section's words.  The 64 sites' plan is past the
// evaluator's budget, so these run at run time (the TEST_CASE below); a
// single site's is a constant (PlansAtCompileTime, above).
using dedekind::category::automaton;
using dedekind::category::bytes;
using dedekind::category::matches;
using dedekind::category::prefix;
using dedekind::category::rank;
using dedekind::category::states;
// [listing5:one-genome]
// those of pali6 beginning GA :
constexpr auto sites = pali6 & prefix("GA");
// the table, computed once by the compiler, a function of any s :
constexpr auto scanner = *automaton(sites);
static_assert(states(scanner) == 11uz);
// and only now a sequence :
constexpr auto dna = bytes(
    "GAATTCGAGCTCGGTACCCGGGGATCC"
    "TCTAGAGTCGACCTGCAGGCATGCAAGCTT");
// ten by definition, each window tested :
static_assert(count(dna >> pali6) == 10uz);
// two by the table, in one pass : EcoRI's and SacI's
static_assert(count(dna >> scanner) == 2uz && sites(bytes("GAATTC")) &&
              sites(bytes("GAGCTC")));
// [/listing5:one-genome]
}  // namespace listing5
// The tests below read the panel's names; they are outside its namespace so
// that the linter's scope `listing5` holds the panel alone, not Catch2's
// macros.
using namespace listing5;
// The plan's count is a Maybe: the plan may exceed its capacity, and the
// monad carries that to the end, where the count is read.
TEST_CASE("Category: Listing 5's one-genome lines agree",
          "[category][tensor][listing5]") {
  const Word genome(dna.begin(), dna.end());
  CHECK(count(std::span<const std::byte>{genome} >> pali6) == 10);
  CHECK(count(std::span<const std::byte>{genome} >> scanner) == 2);
  // the same builder on all 64, past the budget, so here at run time
  CHECK(count(std::span<const std::byte>{genome} >>
              matches(automaton(pali6))) == 10);
}
TEST_CASE("Category: palindromes listed by the law are those tested one by one",
          "[category][tensor][listing5]") {
  using dedekind::category::run_list;
  // the law visits half-sites; the table section tests all 4^6 words
  CHECK(run_list<128, 4, 6>(pali6) == run_list<128, 4, 6>(sites_6));
  CHECK(automaton<4, 6, 8, 12>(pali6 & prefix("GA"))->next.size() == 11);
  CHECK(automaton<4, 6, 4, 8>(pali6 & prefix("GAATTC"))->next.size() == 7);
  // a word no palindrome begins with: no word, no site, one state
  const auto none = automaton<4, 6, 4, 8>(pali6 & prefix("GAATTA"));
  REQUIRE(none.has_value());
  CHECK(none->next.size() == 1);
  CHECK(count(dna >> matches(none)) == 0);
  // the law declines what it cannot read: a slice narrower than the letters,
  // and a code space past the word
  using dedekind::category::slice;
  const auto narrow = slice(std::byte{'C'}, std::byte{'H'}) >>
                      dedekind::category::letters("ACGT");
  CHECK(!run_list<128, 4, 2>(power(narrow, 2uz) &
                             fixed_by(~dedekind::category::rev))
             .has_value());
  CHECK(!run_list<2, 4, 16>(power<std::uint32_t>(bases, 16u) &
                            fixed_by(~dedekind::category::rev) &
                            prefix("TTTTTTTT"))
             .has_value());
  // an alphabet is its letters within its slice, whichever reads it
  CHECK(!dedekind::category::rank(narrow)(std::byte{'A'}).has_value());
  CHECK(dedekind::category::rank(narrow)(std::byte{'G'}) == 1uz);
  CHECK(dedekind::category::refl(narrow)(std::byte{'C'}) == std::byte{'G'});
  CHECK(dedekind::category::refl(narrow)(std::byte{'A'}) == std::byte{'A'});
  CHECK(dedekind::category::letters("CG")(
      dedekind::category::random_text(narrow, 8uz)[5]));
  // ~rev reflects what the slice keeps, as refl of the same alphabet does
  const auto sliced = slice(std::byte{'A'}, std::byte{'E'}) >>
                      dedekind::category::letters("ABCDEF");
  CHECK((power(sliced, 2uz) & fixed_by(~dedekind::category::rev))(bytes("AD")));
  CHECK((power(sliced, 2uz) & fixed_by(~dedekind::category::rev)).tau ==
        dedekind::category::refl(sliced));
  // either order of the two commuting arrows fixes one section
  CHECK(exact_count(count_factors(
            power(nucleotides, 6uz) &
            fixed_by(dedekind::category::rev >> power(complement)))) ==
        exact_count(count_factors(
            power(nucleotides, 6uz) &
            fixed_by(power(complement) >> dedekind::category::rev))));
  // a temporary's bytes are the query's own
  const auto kept = bytes("GAATTC") >> scanner;
  CHECK(count(kept) == 1);
  // the meet holds what the section holds and the prefix allows
  CHECK((pali6 & prefix("GA"))(bytes("GAATTC")));
  CHECK(!(pali6 & prefix("GA"))(bytes("AAGCTT")));
}

// The smoke test of the listing at scale.  Random DNA as a sequence on a chain,
// an arrow out of the counts, so the text is never stored and no generator
// keeps a state.  The scan keeps one state and a count.
/** @brief The other way to find them, as an exact check: the last six ranks
 *  and their reverse complement in two registers of 12 bits, compared. */
struct RollingCode {
  std::uint32_t forward{0};
  std::uint32_t reverse{0};
  size_t seen{0};
  size_t found{0};
};
struct AddLetter {
  constexpr RollingCode operator()(RollingCode c, std::byte b) const {
    const auto r = static_cast<std::uint32_t>(
        *dedekind::category::rank(listing5::bases)(b));
    c.forward = ((c.forward << 2) | r) & 0xFFFu;
    c.reverse = (c.reverse >> 2) | ((3u - r) << 10);
    ++c.seen;
    if (c.seen >= 6 && c.forward == c.reverse) ++c.found;
    return c;
  }
};
TEST_CASE("Category: Listing 5's scanner on ten million random bases",
          "[category][tensor][listing5][smoke]") {
  constexpr size_t N = 10'000'000;
  const auto text = dedekind::category::random_text(listing5::bases, N);
  const auto plan = dedekind::category::automaton(listing5::pali6);
  REQUIRE(plan.has_value());
  const size_t by_the_table = count(text >> *plan);
  // the scan and an independent register agree exactly
  CHECK(by_the_table ==
        std::ranges::fold_left(text, RollingCode{}, AddLetter{}).found);
  // and the left panel's law says what to expect: (N - 5) / 64 of them, with a
  // spread of about 392, so five spreads either side
  CHECK(by_the_table > 156'250 - 2'000);
  CHECK(by_the_table < 156'250 + 2'000);
}

// Panel 2's plans and their binding times, each a plan of the library's own
// values: EcoRI's single site, and the section sites itself.  Whether a
// plan is a constant is decided by a concept: the evaluator running out of
// steps makes the requires-expression false, not the program ill-formed.
// One function, two binding times: EcoRI's plan (KMP's, 7 states) is a
// constant; the 64 sites' (277 states) are past the evaluator's budget and
// are planned at run time.
struct EcoRIPlan {
  static constexpr auto value() {
    return dedekind::category::automaton<4, 6, 4, 8>(
        dedekind::category::run<2>(2109uz, 2110uz), 6uz);
  }
};
struct SitesPlan {
  static constexpr auto value() {
    return dedekind::category::automaton<4, 6, 32, 288, 128>(sites_6);
  }
};
template <typename Plan>
concept PlansAtCompileTime =
    requires { typename std::bool_constant<Plan::value().has_value()>; };
static_assert(PlansAtCompileTime<EcoRIPlan> && !PlansAtCompileTime<SitesPlan>);
// One site written out is that plan, and knows its alphabet.
static_assert(dedekind::category::automaton<4, 6, 4, 8>(
                  listing5::pali6& dedekind::category::prefix("GAATTC"))
                      ->next == EcoRIPlan::value()->next &&
              dedekind::category::automaton<4, 6, 4, 8>(
                  listing5::pali6 & dedekind::category::prefix("GAATTC"))
                      ->accepting == EcoRIPlan::value()->accepting);
// A genome as the caller has it, bytes, read through the alphabet's rank:
// EcoRI's site once in TTGAATTCAAGCTTA.  A byte outside the alphabet is a
// hole: by default it breaks a match, so GAANTTC holds none; skipped, the
// run goes on as it was, and GAATTC remains.
constexpr auto ecori = EcoRIPlan::value().value();
constexpr std::size_t ecori_sites(std::string_view genome,
                                  dedekind::category::Holes holes) {
  using namespace dedekind::category;
  const Word bytes = dna(genome);
  const std::span<const std::byte> g{bytes};
  return count(g >> rank(dedekind::category::letters("ACGT")) >>
               matches(ecori, holes));
}
static_assert(ecori_sites("TTGAATTCAAGCTTA",
                          dedekind::category::Holes::break_match) == 1 &&
              ecori_sites("GAANTTC", dedekind::category::Holes::break_match) ==
                  0 &&
              ecori_sites("GAANTTC", dedekind::category::Holes::skip) == 1);

// Panel 2, its two lines.  The query: each window tested against the
// section itself, in place.  The plan: the matcher built from the same
// section.  sites is the one value both panels read: counted over all
// genomes (panel 1), tested window by window, compiled into the matcher.
constexpr std::size_t windows_that_are_sites(std::string_view genome) {
  using namespace dedekind::category;
  const Word bytes = dna(genome);
  const std::span<const std::byte> g{bytes};
  return count(g >> windows(6) >> where(sites_6));
}
static_assert(sites_6(dna("GAATTC")) && !sites_6(dna("GAATTA")) &&
              !sites_6(dna("GAANTC")) && !sites_6(dna("GAATT")));
static_assert(windows_that_are_sites("TTGAATTCAAGCTTA") == 2);

TEST_CASE("Category: the plan for the 64 sites matches as the windows do",
          "[category][tensor][automaton]") {
  using namespace dedekind::category;
  // the section's words, enumerated once: 64 sites, each a run of one, and
  // EcoRI's among them
  const auto words = run_list<128, 4, 6>(sites_6);
  REQUIRE(words.has_value());
  CHECK(words->steps.size() == 128);
  CHECK((*words)(2109uz));
  // the plan, from the section itself
  const auto plan = automaton<4, 6, 32, 288, 128>(sites_6);
  REQUIRE(plan.has_value());
  CHECK(plan->next.size() == 277);
  CHECK(std::ranges::count(plan->accepting, true) == 64);
  // a genome of 2000 letters, by a fixed linear congruential sequence, as
  // bytes; the two lines of panel 2 count the same sites
  Word bytes;
  std::uint32_t x = 1;
  for (std::size_t i = 0; i < 2000; ++i) {
    x = x * 1664525u + 1013904223u;
    bytes.push_back(acgt[(x >> 16) % 4]);
  }
  const std::span<const std::byte> genome{bytes};
  const std::size_t query = count(genome >> windows(6) >> where(sites_6));
  CHECK(query > 0);
  CHECK(count(genome >> rank(dedekind::category::letters("ACGT")) >>
              matches(*plan)) == query);
}

// The bit patterns of 2^7 = 128 bits that are proper powers: for a width a
// power of two, exactly the squares u u, fixed by the rotation by half the
// width, one merge of 64 pairs of legs; 2^64 of them, one bit past size_t.
constexpr auto bits = dedekind::category::slice(0uz, 2uz) >>
                      dedekind::category::constant<size_t>(true);
constexpr size_t width = dedekind::category::pow2(7);
constexpr auto squares = symmetric(power(bits, width), rotate(width / 2));
static_assert(width == 128 &&
              std::ranges::equal(count_factors(squares), Factors{{2, 64}}) &&
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
static_assert(count(count_factors(symmetric(power(nucleotides, 13), rev))) ==
                  exact_count(count_factors(symmetric(power(nucleotides, 13),
                                                      rev))) &&
              count(count_factors(symmetric(power(nucleotides, 13),
                                            power(complement) >> rev))) == 0 &&
              count(Factors{{3, 40}}) == 12'157'665'459'056'928'801uz &&
              count(Factors{{2, 64}}) == 0);

// The exhibit: a tensor of the two, the palindromes of length 99 over the
// nucleotides and the squares of 128 bits, 2^100 · 2^64 entries, counted as
// factors.
constexpr auto exhibit = symmetric(power(nucleotides, 99), rev) * squares;
static_assert(std::ranges::equal(count_factors(exhibit),
                                 Factors{{4, 49}, {4, 1}, {2, 64}}) &&
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
static_assert(exact_count(count_factors(symmetric(
                  power(just_a, 2),
                  power(dedekind::category::letter_map("AB", "BA")) >> rev))) ==
              0uz);
static_assert(std::ranges::equal(
    count_factors(symmetric(power(bits, 4096uz), rotate(1))), Factors{{2, 1}}));
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
        exact_count(count_factors(
            symmetric(power(nucleotides, length), power(complement) >> rev))));
  CHECK(count(words >> chain<size_t>() >>
              dedekind::category::opaque(
                  IsSymmetric<SameLetter>{length, SameLetter{}})) ==
        exact_count(count_factors(symmetric(power(nucleotides, length), rev))));
  // A site of length 4 in the window from 1: the cylinder's count, pushed
  // down, against all 4^6 words.
  CHECK(count(words >> chain<size_t>() >>
              dedekind::category::opaque(
                  IsSymmetricAt<dedekind::category::LetterMap>{1, 4,
                                                               complement})) ==
        exact_count(count_factors(cylinder(
            power(nucleotides, length), window(1, 4),
            symmetric(power(nucleotides, 4), power(complement) >> rev)))));
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
