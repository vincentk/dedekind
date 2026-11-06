/**
 * @file listing5_bench.cpp
 * @brief What partial evaluation buys Listing 5, with one SplitMix64 draw as
 *        the unit of measure (#1154).  Not a test: nothing here passes or
 *        fails on a time.  @c make @c bench builds it without coverage, at the
 *        build's @c -O2.
 *
 *        bench_listing5                the table, in Markdown
 *        bench_listing5 memory <N>     the scan of N random letters, and the
 *                                      process's peak memory
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
#include <sys/resource.h>

#include <algorithm>
#include <array>
#include <bit>
#include <charconv>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <print>
#include <ranges>
#include <span>
#include <string_view>
#include <vector>

import dedekind.category;

namespace {
using namespace dedekind::category;

using Text = std::span<const std::byte>;

// The listing's own values.
constexpr Alphabet bases = abc("ACGT");
constexpr Words oligos = words(bases);
constexpr auto pali6 = oligos & len(6) & fixed_by(~rev);
constexpr auto sites = pali6 & prefix("GA");
constexpr auto scanner_11 = *automaton(sites);
// The 64 sites' plan is past the evaluator's budget, so planned at start-up,
// where one that does not fit stops the program before any letter is read.
auto planned_64() {
  const auto plan = automaton(pali6);
  if (!plan) {
    std::println(stderr, "the 64 sites' plan does not fit its capacity");
    std::exit(1);
  }
  return *plan;
}
const auto scanner_64 = planned_64();

/** @brief The hand-written loop to hold the library's against: the same table,
 *  flattened, one load for the letter's column and one for the state's. */
struct HandWritten {
  std::array<std::uint8_t, 256> column{};
  std::array<std::uint8_t, 256> entry{};  ///< its column plus one, or zero
  std::vector<std::array<std::uint16_t, 4>> next;
  std::vector<std::uint8_t> accepting;
};
template <typename Plan>
HandWritten hand_written(const Plan& plan) {
  HandWritten h;
  h.column[static_cast<std::size_t>('A')] = 0;
  h.column[static_cast<std::size_t>('C')] = 1;
  h.column[static_cast<std::size_t>('G')] = 2;
  h.column[static_cast<std::size_t>('T')] = 3;
  h.entry[static_cast<std::size_t>('A')] = 1;
  h.entry[static_cast<std::size_t>('C')] = 2;
  h.entry[static_cast<std::size_t>('G')] = 3;
  h.entry[static_cast<std::size_t>('T')] = 4;
  const std::size_t n = states(plan);
  h.next.assign(plan.next.items.begin(), plan.next.items.begin() + n);
  for (std::size_t s = 0; s < n; ++s)
    h.accepting.push_back(plan.accepting.items[s] ? 1 : 0);
  return h;
}
const HandWritten hand_64 = hand_written(scanner_64);
const HandWritten hand_11 = hand_written(scanner_11);

std::uint64_t by_hand(const HandWritten& h, Text text) {
  std::uint16_t state = 0;
  std::uint64_t found = 0;
  for (const std::byte b : text) {
    state = h.next[state][h.column[static_cast<std::size_t>(b)]];
    found += h.accepting[state];
  }
  return found;
}

/** @brief The loop that does what the library's does on any byte: one outside
 *  the alphabet breaks a match. */
std::uint64_t by_hand_with_holes(const HandWritten& h, Text text) {
  std::uint16_t state = 0;
  std::uint64_t found = 0;
  for (const std::byte b : text) {
    const std::uint8_t entry = h.entry[static_cast<std::size_t>(b)];
    if (entry == 0) {
      state = 0;
      continue;
    }
    state = h.next[state][entry - 1u];
    found += h.accepting[state];
  }
  return found;
}

std::uint64_t hash_alone(std::size_t n) {
  const SplitMix hash{0};
  std::uint64_t x = 0;
  for (std::size_t i = 0; i < n; ++i) x ^= hash(i);
  return x;
}
std::uint64_t letter_masked(std::size_t n) {
  const SplitMix hash{0};
  constexpr std::array<std::uint8_t, 4> acgt{'A', 'C', 'G', 'T'};
  std::uint64_t x = 0;
  for (std::size_t i = 0; i < n; ++i) x ^= acgt[hash(i) & 3u];
  return x;
}
std::uint64_t letter_of_the_library(std::size_t n) {
  std::uint64_t x = 0;
  for (const std::byte b : random_text(bases, n))
    x ^= static_cast<std::uint8_t>(b);
  return x;
}

std::uint64_t each_window(Text t) {
  return count(t >> windows(6uz) >> where(pali6));
}
std::uint64_t by_the_law(Text t) { return count<std::uint64_t>(t >> pali6); }
std::uint64_t table_64(Text t) { return count(t >> scanner_64); }
std::uint64_t table_11(Text t) { return count(t >> scanner_11); }
std::uint64_t by_hand_64(Text t) { return by_hand(hand_64, t); }
std::uint64_t by_hand_11(Text t) { return by_hand(hand_11, t); }
std::uint64_t holes_64(Text t) { return by_hand_with_holes(hand_64, t); }
std::uint64_t holes_11(Text t) { return by_hand_with_holes(hand_11, t); }
std::uint64_t drawn_table_64(std::size_t n) {
  return count(random_text(bases, n) >> scanner_64);
}
std::uint64_t drawn_table_11(std::size_t n) {
  return count(random_text(bases, n) >> scanner_11);
}

// The law's own plan.  fixed_by(~rev) over a fixed length says a window is
// three independent pairs, so nothing need be carried from one position to the
// next; the plain count over the positions is then the whole program, and
// whether it is vectorised is the compiler's to decide.  Three readings of the
// text, each its own predicate, none written as a loop or with intrinsics.

/** @brief Letters as they come, ACGT only: complements differ in bits 1 and 2
 *  by 4 (A 00, C 01, T 10, G 11 there). */
struct AsciiPali6 {
  const std::uint8_t* s;
  static constexpr bool pair(std::uint8_t x, std::uint8_t y) {
    return ((x ^ y) & 6u) == 4u;
  }
  constexpr bool operator()(std::size_t i) const {
    return pair(s[i], s[i + 5]) & pair(s[i + 1], s[i + 4]) &
           pair(s[i + 2], s[i + 3]);
  }
};
/** @brief Ranks in bytes, a hole 0x80: the complement is ~ on two bits, so
 *  complements xor to 3, and a hole never does. */
struct RanksPali6 {
  const std::uint8_t* r;
  static constexpr bool pair(std::uint8_t x, std::uint8_t y) {
    return (x ^ y) == 3u;
  }
  constexpr bool operator()(std::size_t i) const {
    return pair(r[i], r[i + 5]) & pair(r[i + 1], r[i + 4]) &
           pair(r[i + 2], r[i + 3]);
  }
};
/** @brief One bit a base in bytes, a set of bases being IUPAC's code: the
 *  complement is the nibble reversed, and a meet says they can pair. */
struct MasksPali6 {
  const std::uint8_t* m;
  static constexpr std::uint8_t reversed(std::uint8_t x) {
    return static_cast<std::uint8_t>(((x & 1u) << 3) | ((x & 2u) << 1) |
                                     ((x & 4u) >> 1) | ((x & 8u) >> 3));
  }
  static constexpr bool pair(std::uint8_t x, std::uint8_t y) {
    return (x & reversed(y)) != 0u;
  }
  constexpr bool operator()(std::size_t i) const {
    return pair(m[i], m[i + 5]) & pair(m[i + 1], m[i + 4]) &
           pair(m[i + 2], m[i + 3]);
  }
};
/** @brief Letters as they come, of any four, known only at run time as Python's
 *  are: a byte's rank by comparing it with each, a hole 0x80, and reflection on
 *  n letters as ranks that sum to n - 1. */
struct GuardedPali6 {
  const std::uint8_t* s;
  std::array<std::uint8_t, 4> letters;
  constexpr std::uint8_t rank(std::uint8_t c) const {
    const std::uint8_t one = c == letters[1];
    const std::uint8_t two = c == letters[2];
    const std::uint8_t three = c == letters[3];
    const std::uint8_t zero = c == letters[0];
    const std::uint8_t known = zero | one | two | three;
    return static_cast<std::uint8_t>(one + 2u * two + 3u * three +
                                     (known ? 0u : 0x80u));
  }
  constexpr bool pair(std::uint8_t x, std::uint8_t y) const {
    return static_cast<std::uint8_t>(rank(x) + rank(y)) == 3u;
  }
  constexpr bool operator()(std::size_t i) const {
    return pair(s[i], s[i + 5]) & pair(s[i + 1], s[i + 4]) &
           pair(s[i + 2], s[i + 3]);
  }
};
/** @brief The fold's step in a byte, which @c count refuses: a byte's
 * arithmetic promotes to @c int, and a block is what keeps this one in range.
 */
struct ByteStep {
  constexpr std::uint8_t operator()(std::uint8_t m, bool b) const {
    return static_cast<std::uint8_t>(m + (b ? 1 : 0));
  }
};
/** @brief The count of a block of windows, in a counter the width of a lane:
 *  240 positions fit a byte. */
template <typename Window>
struct BlockCount {
  Window window;
  std::size_t last;
  constexpr std::uint64_t operator()(std::uint64_t total,
                                     std::size_t block) const {
    const std::size_t lo = block * 240uz;
    const std::size_t hi = std::min(lo + 240uz, last);
    return total + std::ranges::fold_left(
                       std::views::iota(lo, hi) | std::views::transform(window),
                       std::uint8_t{0}, ByteStep{});
  }
};
template <typename Window>
std::uint64_t over_the_blocks(Window window, std::size_t n) {
  const std::size_t last = n - 5;
  return std::ranges::fold_left(std::views::iota(0uz, (last + 239uz) / 240uz),
                                std::uint64_t{0},
                                BlockCount<Window>{window, last});
}
// Two bits a base: the rank.  The encoding is a homomorphism, carrying the
// alphabet's reflection to the machine's ~, which is why the word below can
// compare complements at all.
constexpr std::size_t two_bits(char letter) {
  return *rank(bases)(static_cast<std::byte>(letter));
}
static_assert((~two_bits('A') & 3uz) == two_bits('T') &&
              (~two_bits('T') & 3uz) == two_bits('A') &&
              (~two_bits('C') & 3uz) == two_bits('G') &&
              (~two_bits('G') & 3uz) == two_bits('C'));

/** @brief A word's bases at distance @c d: bit @c 2j set where base @c j and
 * base
 *  @c j+d are complements, which xor to 11 under ~. */
constexpr std::uint64_t complements_at(std::uint64_t x, unsigned d) {
  const std::uint64_t y = x ^ (x >> (2u * d));
  return y & (y >> 1) & 0x5555555555555555u;
}
/** @brief One step of the fold: a word's 27 windows at once, each a site where
 * its three pairs are complements. */
struct WordCount {
  const std::uint64_t* words;
  std::size_t last;
  constexpr std::uint64_t operator()(std::uint64_t total,
                                     std::size_t step) const {
    const std::size_t start = step * 27uz;
    const std::size_t k = start / 32uz;
    const unsigned at = 2u * static_cast<unsigned>(start % 32uz);
    const std::uint64_t x =
        (words[k] >> at) | ((words[k + 1] << 1) << (63u - at));
    const std::size_t windows = std::min(27uz, last - start);
    const std::uint64_t sites = complements_at(x, 5) &
                                (complements_at(x, 3) >> 2) &
                                (complements_at(x, 1) >> 4);
    return total + static_cast<std::uint64_t>(std::popcount(
                       sites & ((std::uint64_t{1} << (2uz * windows)) - 1u)));
  }
};
// The candidate for the library: the alphabet and the length are values, as
// they are in a plan and in Python, and a block goes through in stages small
// enough for the compiler to see each as a loop: ranks, then one pass per pair,
// then the count.  Fixed storage, so the text is still never stored.
constexpr std::size_t block_windows = 240;
constexpr std::size_t longest_word = 16;
/** @brief A letter's rank by comparing it with each of four, a hole 0x80. */
struct RankOf {
  std::array<std::uint8_t, 4> letters;
  constexpr std::uint8_t operator()(std::uint8_t c) const {
    const std::uint8_t a = c == letters[0];
    const std::uint8_t b = c == letters[1];
    const std::uint8_t g = c == letters[2];
    const std::uint8_t t = c == letters[3];
    const auto known = static_cast<std::uint8_t>(a | b | g | t);
    return static_cast<std::uint8_t>(
        b | (g << 1) | (t * 3u) |
        (static_cast<std::uint8_t>(known - 1u) & 0x80u));
  }
};
/** @brief One position's verdict with one more pair: ranks that reflect sum to
 *  n - 1, which a hole never does for two or more letters. */
struct WithPair {
  const std::uint8_t* verdict;
  const std::uint8_t* left;
  const std::uint8_t* right;
  constexpr std::uint8_t operator()(std::size_t i) const {
    return static_cast<std::uint8_t>(
        verdict[i] & static_cast<std::uint8_t>(
                         static_cast<std::uint8_t>(left[i] + right[i]) == 3u));
  }
};
/** @brief A pass of the block for its @c k -th pair. */
struct PairPass {
  std::uint8_t* verdict;
  const std::uint8_t* ranks;
  std::size_t windows;
  std::size_t length;
  void operator()(std::size_t k) const {
    std::ranges::transform(
        std::views::iota(0uz, windows), verdict,
        WithPair{verdict, ranks + k, ranks + (length - 1uz - k)});
  }
};
struct ColumnarCount {
  const std::uint8_t* s;
  RankOf rank_of;
  std::size_t last;
  std::size_t length;
  std::uint64_t operator()(std::uint64_t total, std::size_t block) const {
    const std::size_t lo = block * block_windows;
    const std::size_t windows = std::min(block_windows, last - lo);
    std::array<std::uint8_t, block_windows + longest_word> ranks;
    std::array<std::uint8_t, block_windows> verdict;
    std::ranges::transform(s + lo, s + lo + windows + length - 1uz,
                           ranks.begin(), rank_of);
    std::ranges::fill(verdict, std::uint8_t{1});
    std::ranges::for_each(
        std::views::iota(0uz, length / 2uz),
        PairPass{verdict.data(), ranks.data(), windows, length});
    return total +
           std::ranges::fold_left(verdict.begin(), verdict.begin() + windows,
                                  std::uint32_t{0}, std::plus<std::uint32_t>{});
  }
};
template <typename Window>
std::uint64_t over_the_windows(Window window, std::size_t n) {
  return count<std::uint64_t>(std::views::iota(0uz, n - 5) |
                              std::views::transform(window));
}
/** @brief The rank of a letter in a byte, or the hole. */
struct RankByte {
  constexpr std::uint8_t operator()(std::byte b) const {
    const auto r = scanner_11.columns(b);
    return r ? static_cast<std::uint8_t>(*r) : std::uint8_t{0x80};
  }
};
/** @brief A set of one base in a byte, or the empty set. */
struct MaskByte {
  constexpr std::uint8_t operator()(std::byte b) const {
    const auto r = scanner_11.columns(b);
    return r ? static_cast<std::uint8_t>(1u << *r) : std::uint8_t{0};
  }
};
}  // namespace

// Named for the disassembly `make bench` keeps.
extern "C" std::uint64_t scan_definition(const std::byte* s, std::size_t n) {
  return each_window(Text{s, n});
}
extern "C" std::uint64_t scan_table(const std::byte* s, std::size_t n) {
  return table_64(Text{s, n});
}
extern "C" std::uint64_t law_ascii(const std::uint8_t* s, std::size_t n) {
  return over_the_windows(AsciiPali6{s}, n);
}
extern "C" std::uint64_t law_ranks(const std::uint8_t* s, std::size_t n) {
  return over_the_windows(RanksPali6{s}, n);
}
extern "C" std::uint64_t law_masks(const std::uint8_t* s, std::size_t n) {
  return over_the_windows(MasksPali6{s}, n);
}

extern "C" std::uint64_t law_guarded(const std::uint8_t* s, std::size_t n,
                                     const std::uint8_t* letters) {
  return over_the_windows(
      GuardedPali6{s, {letters[0], letters[1], letters[2], letters[3]}}, n);
}
extern "C" std::uint64_t law_blocks(const std::uint8_t* s, std::size_t n) {
  return over_the_blocks(RanksPali6{s}, n);
}
extern "C" std::uint64_t law_guarded_blocks(const std::uint8_t* s,
                                            std::size_t n,
                                            const std::uint8_t* letters) {
  return over_the_blocks(
      GuardedPali6{s, {letters[0], letters[1], letters[2], letters[3]}}, n);
}

extern "C" std::uint64_t law_words(const std::uint64_t* words, std::size_t n) {
  const std::size_t last = n - 5;
  return std::ranges::fold_left(std::views::iota(0uz, (last + 26uz) / 27uz),
                                std::uint64_t{0}, WordCount{words, last});
}

extern "C" std::uint64_t law_columnar(const std::uint8_t* s, std::size_t n,
                                      const std::uint8_t* letters,
                                      std::size_t length) {
  const std::size_t last = n - (length - 1uz);
  return std::ranges::fold_left(
      std::views::iota(0uz, (last + block_windows - 1uz) / block_windows),
      std::uint64_t{0},
      ColumnarCount{s, RankOf{{letters[0], letters[1], letters[2], letters[3]}},
                    last, length});
}

namespace {
using Bytes = std::span<const std::uint8_t>;
std::uint64_t ascii_row(Bytes b) { return law_ascii(b.data(), b.size()); }
std::uint64_t ranks_row(Bytes b) { return law_ranks(b.data(), b.size()); }
std::uint64_t masks_row(Bytes b) { return law_masks(b.data(), b.size()); }
// A string the optimiser cannot read, as a run-time alphabet would be: A T's
// complement and C G's, in rank order.
volatile std::uint8_t acgt_at_run_time[4] = {'A', 'C', 'G', 'T'};
std::array<std::uint8_t, 4> letters_at_run_time() {
  return {acgt_at_run_time[0], acgt_at_run_time[1], acgt_at_run_time[2],
          acgt_at_run_time[3]};
}
const std::array<std::uint8_t, 4> run_time_letters = letters_at_run_time();
std::uint64_t guarded_row(Bytes b) {
  return law_guarded(b.data(), b.size(), run_time_letters.data());
}
struct Packed {
  std::vector<std::uint64_t> words;
  std::size_t letters;
};
Packed packed_from(Bytes ranks) {
  Packed p{std::vector<std::uint64_t>(ranks.size() / 32uz + 2uz), ranks.size()};
  for (std::size_t i = 0; i < ranks.size(); ++i)
    p.words[i / 32uz] |= std::uint64_t{ranks[i]} << (2uz * (i % 32uz));
  return p;
}
std::uint64_t words_row(const Packed* p) {
  return law_words(p->words.data(), p->letters);
}
volatile std::size_t six_at_run_time = 6;
const std::size_t run_time_six = six_at_run_time;
std::uint64_t columnar_row(Bytes b) {
  return law_columnar(b.data(), b.size(), run_time_letters.data(),
                      run_time_six);
}
std::uint64_t blocks_row(Bytes b) { return law_blocks(b.data(), b.size()); }
std::uint64_t guarded_blocks_row(Bytes b) {
  return law_guarded_blocks(b.data(), b.size(), run_time_letters.data());
}

/** @brief Stops the optimiser removing a result it can see is unused. */
void keep(std::uint64_t x) { asm volatile("" : : "r"(x) : "memory"); }

constexpr int repeats = 5;

/** @return the median time of @c call, in nanoseconds a letter. */
template <typename Call, typename Argument>
double ns_per_letter(Call call, Argument argument, std::size_t n) {
  std::array<double, repeats> ns{};
  for (double& t : ns) {
    const auto start = std::chrono::steady_clock::now();
    keep(call(argument));
    const std::chrono::duration<double, std::nano> took =
        std::chrono::steady_clock::now() - start;
    t = took.count() / static_cast<double>(n);
  }
  std::ranges::sort(ns);
  return ns[repeats / 2];
}

void row(std::string_view name, double ns, double draw) {
  std::println("| {} | {:.3f} | {:.2f} |", name, ns, ns / draw);
}

/** @brief A word of the text read, and nothing else done with it. */
struct ReadWord {
  constexpr std::uint64_t operator()(std::uint64_t total,
                                     std::uint64_t word) const {
    return total ^ word;
  }
};
std::uint64_t read_all(Text t) {
  const std::span<const std::uint64_t> words{
      reinterpret_cast<const std::uint64_t*>(t.data()), t.size() / 8uz};
  return std::ranges::fold_left(words, std::uint64_t{0}, ReadWord{});
}

/** @brief The same scans on a text too long for the cache to hold, against this
 *  machine's own plain read of it: the bus, as measured here. */
void past_the_cache() {
  constexpr std::size_t N = 3'100'000'000;  // a human genome's bases, about
  const auto stored = random_text(bases, N) | std::ranges::to<std::vector>();
  const Text text{stored};
  const double read = ns_per_letter(read_all, text, N);
  const double law = ns_per_letter(by_the_law, text, N);
  const double scan = ns_per_letter(table_64, text, N);
  std::println("\n{} letters, past the cache.\n", N);
  std::println("| Row | GB/s | Share of a plain read |");
  std::println("|---|---:|---:|");
  std::println("| a plain read of every word | {:.2f} | 1.00 |", 1.0 / read);
  std::println(
      "| the library's count(text >> pali6), by the law | {:.2f} | {:.2f} |",
      1.0 / law, read / law);
  std::println("| the 64 sites' table | {:.2f} | {:.2f} |", 1.0 / scan,
               read / scan);
}

int table() {
  constexpr std::size_t N = 100'000'000;
  constexpr std::size_t N_windows = 2'000'000;  // tested one by one: slow
  const auto stored = random_text(bases, N) | std::ranges::to<std::vector>();
  const Text text{stored};
  const Text few = text.first(N_windows);

  // The two ways, and the hand-written one, must count the same.
  const std::uint64_t by_table = table_64(few);
  if (by_table != each_window(few) || by_table != by_hand_64(few) ||
      by_table != holes_64(few) || table_11(few) != by_hand_11(few)) {
    std::println(stderr, "the ways to count disagree");
    return 1;
  }

  keep(hash_alone(
      N));  // warm: the first row must not pay for the machine waking
  const auto ranks =
      text | std::views::transform(RankByte{}) | std::ranges::to<std::vector>();
  const auto masks =
      text | std::views::transform(MaskByte{}) | std::ranges::to<std::vector>();
  const Bytes ascii{reinterpret_cast<const std::uint8_t*>(stored.data()), N};
  const Packed packed = packed_from(Bytes{ranks});
  // The full count, as the laws' plans must agree with the table on all of it.
  const std::uint64_t all = table_64(text);
  if (ascii_row(ascii) != all || ranks_row(ranks) != all ||
      masks_row(masks) != all || guarded_row(ascii) != all ||
      blocks_row(ranks) != all || guarded_blocks_row(ascii) != all ||
      words_row(&packed) != all || columnar_row(ascii) != all ||
      by_the_law(text) != all) {
    std::println(stderr, "the law's plans disagree with the table");
    return 1;
  }

  const double draw = ns_per_letter(hash_alone, N, N);
  std::println("{} letters, median of {} ({} for the windows).", N, repeats,
               N_windows);
  std::println("Draws: the row's time over one SplitMix64 hash.\n");
  std::println("| Row | ns a letter | draws |");
  std::println("|---|---:|---:|");
  row("each of the 64 sites' windows tested, stored text",
      ns_per_letter(each_window, few, N_windows), draw);
  const double table_time = ns_per_letter(table_64, text, N);
  row("the 64 sites' table (277 states), stored text", table_time, draw);
  const double yardstick = ns_per_letter(holes_64, text, N);
  row("the same table, hand-written, holes as the library", yardstick, draw);
  row("the same table, hand-written, no holes",
      ns_per_letter(by_hand_64, text, N), draw);
  row("the 11-state table, stored text", ns_per_letter(table_11, text, N),
      draw);
  row("the same table, hand-written, holes as the library",
      ns_per_letter(holes_11, text, N), draw);
  row("the same table, hand-written, no holes",
      ns_per_letter(by_hand_11, text, N), draw);
  const double law = ns_per_letter(by_the_law, text, N);
  row("the library's count(text >> pali6), by the law", law, draw);
  row("the law, letters as they come (ACGT only)",
      ns_per_letter(ascii_row, ascii, N), draw);
  row("the law, ranks in bytes", ns_per_letter(ranks_row, Bytes{ranks}, N),
      draw);
  row("the law, sets of bases in bytes (IUPAC's code)",
      ns_per_letter(masks_row, Bytes{masks}, N), draw);
  row("the law, letters as they come, four compared at run time",
      ns_per_letter(guarded_row, ascii, N), draw);
  row("the law, ranks in bytes, a byte counter per block",
      ns_per_letter(blocks_row, Bytes{ranks}, N), draw);
  row("the law, four compared at run time, a byte counter per block",
      ns_per_letter(guarded_blocks_row, ascii, N), draw);
  row("the law, two bits a base, 27 windows a word",
      ns_per_letter(words_row, &packed, N), draw);
  row("the law, alphabet and length at run time, block by block in stages",
      ns_per_letter(columnar_row, ascii, N), draw);
  row("hash alone", draw, draw);
  row("a letter, hash and mask", ns_per_letter(letter_masked, N, N), draw);
  row("a letter, the library's random_text",
      ns_per_letter(letter_of_the_library, N, N), draw);
  row("drawn and scanned, the 64 sites' table",
      ns_per_letter(drawn_table_64, N, N), draw);
  row("drawn and scanned, the 11-state table",
      ns_per_letter(drawn_table_11, N, N), draw);
  past_the_cache();
  // Generous, for a shared runner: a regression, not a difference.
  if (table_time > 2.0 * yardstick || law > table_time) {
    std::println(stderr, "the library fell behind: table {} against {}, law {}",
                 table_time, yardstick, law);
    return 1;
  }
  return 0;
}

int memory(std::string_view letters) {
  std::size_t n = 0;
  std::from_chars(letters.data(), letters.data() + letters.size(), n);
  const std::uint64_t found = drawn_table_64(n);
  rusage usage{};
  getrusage(RUSAGE_SELF, &usage);
#ifdef __APPLE__
  const long kib = usage.ru_maxrss / 1024;  // bytes there, KiB on Linux
#else
  const long kib = usage.ru_maxrss;
#endif
  std::println("| {} | {} | {} |", n, found, kib);
  return 0;
}
}  // namespace

int main(int argc, char** argv) {
  if (argc == 3 && std::string_view{argv[1]} == "memory")
    return memory(argv[2]);
  return table();
}
