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
#include <charconv>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
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

std::uint64_t each_window(Text t) { return count(t >> pali6); }
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
extern "C" std::uint64_t law_ascii(const std::uint8_t* s, std::size_t n) {
  return over_the_windows(AsciiPali6{s}, n);
}
extern "C" std::uint64_t law_ranks(const std::uint8_t* s, std::size_t n) {
  return over_the_windows(RanksPali6{s}, n);
}
extern "C" std::uint64_t law_masks(const std::uint8_t* s, std::size_t n) {
  return over_the_windows(MasksPali6{s}, n);
}

namespace {
using Bytes = std::span<const std::uint8_t>;
std::uint64_t ascii_row(Bytes b) { return law_ascii(b.data(), b.size()); }
std::uint64_t ranks_row(Bytes b) { return law_ranks(b.data(), b.size()); }
std::uint64_t masks_row(Bytes b) { return law_masks(b.data(), b.size()); }

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
  // The full count, as the laws' plans must agree with the table on all of it.
  const std::uint64_t all = table_64(text);
  if (ascii_row(ascii) != all || ranks_row(ranks) != all ||
      masks_row(masks) != all) {
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
  row("the 64 sites' table (277 states), stored text",
      ns_per_letter(table_64, text, N), draw);
  row("the same table, hand-written, holes as the library",
      ns_per_letter(holes_64, text, N), draw);
  row("the same table, hand-written, no holes",
      ns_per_letter(by_hand_64, text, N), draw);
  row("the 11-state table, stored text", ns_per_letter(table_11, text, N),
      draw);
  row("the same table, hand-written, holes as the library",
      ns_per_letter(holes_11, text, N), draw);
  row("the same table, hand-written, no holes",
      ns_per_letter(by_hand_11, text, N), draw);
  row("the law, letters as they come (ACGT only)",
      ns_per_letter(ascii_row, ascii, N), draw);
  row("the law, ranks in bytes", ns_per_letter(ranks_row, Bytes{ranks}, N),
      draw);
  row("the law, sets of bases in bytes (IUPAC's code)",
      ns_per_letter(masks_row, Bytes{masks}, N), draw);
  row("hash alone", draw, draw);
  row("a letter, hash and mask", ns_per_letter(letter_masked, N, N), draw);
  row("a letter, the library's random_text",
      ns_per_letter(letter_of_the_library, N, N), draw);
  row("drawn and scanned, the 64 sites' table",
      ns_per_letter(drawn_table_64, N, N), draw);
  row("drawn and scanned, the 11-state table",
      ns_per_letter(drawn_table_11, N, N), draw);
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
