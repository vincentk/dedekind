/**
 * @file dedekind/category/automaton.cppm
 * @partition :automaton
 * @brief The matcher of a set of words of one length, specialised to it: the
 *        minimal automaton of @f$\Sigma^* L@f$, a value computed by one
 *        @c constexpr function at either binding time.
 *
 * @section automaton__Derivatives The matcher as a coalgebra
 * A matcher reads a text letter by letter and reports where a word of @c L
 * ends.  Its state is what remains to be read for a match: a set of words,
 * the derivative of @f$\Sigma^* L@f$ by the text so far (Brzozowski 1964).
 * The derivative is a homomorphism of the join-semilattice of sets of
 * words, @f$\delta_a(x \vee y) = \delta_a x \vee \delta_a y@f$, so it is
 * fixed on words and extended to unions; an automaton is the pair
 * @f$(\varepsilon, \delta)@f$, a coalgebra on those sets (Rutten 1998).
 *
 * A state is @f$\Sigma^* L \vee S@f$: a match may still begin anywhere, and
 * the words of @c S are pending.  Every pending word is shorter than the
 * words of @c L, so none contains one, and @c S is the state's language less
 * @f$\Sigma^* L@f$: distinct states are distinct languages, and the
 * automaton is minimal by construction (Myhill–Nerode), with no
 * minimisation pass.  States are numbered in the order a breadth-first walk
 * finds them, letters in the alphabet's order, so the plan is canonical and
 * @c == on plans is the equality of the languages they recognise.  For one
 * word the plan is Knuth, Morris and Pratt's; Consel and Danvy (1989) obtain
 * it by partially evaluating a naive matcher in the pattern.
 *
 * The representation is the run list (@c :runs): the words of length @c k
 * in the alphabet's order are the chain @f$[0, \sigma^k)@f$, a set of them
 * a run list, and the words beginning with a letter one interval, so
 * @f$\delta_a@f$ is a meet with that interval, by the lattice term reducer,
 * then the preimage along a translation.  A state is one run list per
 * pending length.
 *
 * Bounded by its type, on the stack: the states and the steps of their run
 * lists have bounds the caller claims, so the construction runs in the
 * @c Maybe monad and yields nothing where a claim fails, at either binding
 * time.  At compile time a large set may exceed the evaluator's step
 * budget; the same call then runs at run time (#1135).
 *
 * @section automaton__Binding_Times One function, two binding times
 * The plan is a value.  A @c constexpr variable forces it at compile time,
 * where a single site's plan (Knuth, Morris and Pratt's) is a constant and a
 * set of 64 sites exceeds the evaluator's budget; the same function builds
 * either at run time, from Python through @c pynzli.  Consel and Danvy
 * (1989) obtain the single-site automaton by partially evaluating a naive
 * matcher in its pattern; here the specialisation is the derivative's.
 *
 * Wikipedia: Brzozowski derivative, Aho–Corasick algorithm,
 * Knuth–Morris–Pratt algorithm, DFA minimization.
 *
 * @note "In this paper the notion of a derivative of a regular expression
 *       is introduced and the properties of derivatives are discussed.  This
 *       leads, in a very natural way, to the construction of a state diagram
 *       from a regular expression containing any number of logical
 *       operators."
 *       -- Janusz A. Brzozowski, *Derivatives of Regular Expressions*,
 *          Journal of the ACM 11(4), 1964, abstract.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdlib>  // std::abort: a refused word
#include <functional>
#include <optional>
#include <ranges>
#include <span>
#include <string_view>  // a prefix, as written
#include <type_traits>
#include <utility>
#include <vector>  // a law witness: the other strand, in constant evaluation

export module dedekind.category:automaton;

import :morphism;    // Identity: a chain's letters read as they are
import :nno;         // HasNNOStep: the letters' chain
import :involution;  // Reverse: the symmetry a section's words are listed by
import :chain;       // IsCountWord
import :functor;     // Maybe
import :lattice;     // | and &: a union, a restriction, as the lattice's nodes
import :periodic;    // AtMost, shift, Symmetric, Power: a section of words
import :runs;        // RunList, run, reduced, narrowed
import :total;       // power: σ^k by squaring
import :action;      // fold: a plain fold
import :kleisli;     // fold in the Kleisli category: the steps that may fail
import :comprehension;  // where, take, collect, IsMaybe: queries

namespace dedekind::category {

/** @brief The column of a byte in a plan: its rank in the alphabet, as a table
 *  of the 256 bytes the compiler computes once, so that a scan does one load
 *  per letter where @c Ranked would count the letters below it.  An entry is
 *  the rank plus one, zero for a byte outside the alphabet.  A Kleisli arrow
 *  into @c Maybe, as @c Ranked is, and the same arrow by value. */
export struct Columns {
  using Domain = std::byte;
  using Codomain = Maybe<std::size_t>;
  std::array<std::uint8_t, 256> entries{};
  bool any{false};  ///< whether it has an alphabet at all
  constexpr Maybe<std::size_t> operator()(std::byte b) const {
    const std::uint8_t e = entries[std::to_integer<unsigned char>(b)];
    if (e == 0) return {};
    return static_cast<std::size_t>(e - 1);
  }
  constexpr bool operator==(const Columns&) const = default;
};
namespace detail_automaton {
/** @brief One byte's entry: its rank plus one, or zero. */
struct ColumnAt {
  Letters on;
  constexpr std::uint8_t operator()(std::size_t b) const {
    const Maybe<std::size_t> r =
        Ranked{on}(std::byte{static_cast<unsigned char>(b)});
    return r ? static_cast<std::uint8_t>(*r + 1) : std::uint8_t{0};
  }
};
}  // namespace detail_automaton
/** @param on the alphabet's letters, at most 255 of them, as @c abc holds.
 *  @return the table of their columns. */
export constexpr Columns columns_of(const Letters& on) {
  Columns c{};
  std::ranges::transform(std::views::iota(0uz, 256uz), c.entries.begin(),
                         detail_automaton::ColumnAt{on});
  c.any = on.members.any();
  return c;
}
static_assert(columns_of(letters("ACGT"))(std::byte{'A'}) == 0uz &&
              columns_of(letters("ACGT"))(std::byte{'T'}) == 3uz &&
              !columns_of(letters("ACGT"))(std::byte{'N'}).has_value() &&
              !Columns{}.any);

/** @brief A matcher's plan: the transitions on each letter, and the states
 *  where a word of the set has just been read.  @tparam Sigma the letters.
 *  @tparam Q the bound on the states. */
export template <std::size_t Sigma, std::size_t Q>
  requires(Q >= 1 && Q <= 65536)  // the start state, and 16-bit indices
struct Automaton {
  AtMost<std::array<std::uint16_t, Sigma>, Q> next;
  AtMost<bool, Q> accepting;
  /** @brief The columns of a byte, so that a text of bytes is read through
   *  them; none for a plan given as a table over ranks. */
  Columns columns{};
  constexpr bool operator==(const Automaton&) const = default;
};

namespace detail_automaton {

/** @brief A state: whether a word of the set has just been read, and the
 *  pending words by length, @c pending[k - 1] those of length @c k. */
template <IsCountWord N, std::size_t P, std::size_t K>
struct Residual {
  bool accepting{false};
  std::array<RunList<N, K>, P> pending{};
  constexpr bool operator==(const Residual&) const = default;
};
/** @param r words of length @c j, @c j at least one.  @param a a letter.
 *  @return @f$\delta_a r@f$: the words of length @c j - 1 that follow
 *  @c a in them, the meet with @c a's interval read from its start. */
template <std::size_t Sigma, IsCountWord N, std::size_t K>
constexpr auto derived(const RunList<N, K>& r, N j, N a) {
  const N b = detail_concat::power(static_cast<N>(Sigma), static_cast<N>(j - 1),
                                   std::multiplies<N>{});
  const N lo = static_cast<N>(a * b);
  return shift<N>(lo) >> (r & run<2>(lo, static_cast<N>(lo + b)));
}
/** @brief The set's own derivatives, one per letter: the same for every
 *  state, so computed once, before the walk. */
template <std::size_t Sigma, IsCountWord N, std::size_t KL>
struct DeriveSites {
  RunList<N, KL> sites;
  N length;
  using Own = std::array<RunList<N, KL + 2>, Sigma>;
  constexpr Own operator()(const Own& acc, N a) const {
    Own next = acc;
    next[a] = derived<Sigma>(sites, length, a);
    return next;
  }
};
/** @brief One pending length of a state's derivative by a letter: the
 *  pending words one longer, derived, and the set's own derivative where
 *  they are its length, joined; a word of length zero is a match. */
template <std::size_t Sigma, IsCountWord N, std::size_t P, std::size_t K,
          std::size_t KL>
struct Advance {
  Residual<N, P, K> from;
  std::array<RunList<N, KL>, Sigma> own;
  N length;
  N letter;
  constexpr Maybe<Residual<N, P, K>> operator()(const Residual<N, P, K>& acc,
                                                N k) const {
    const N j = static_cast<N>(k + 1);
    const auto longer = derived<Sigma>(from.pending[k], j, letter);
    const RunList<N, KL> mine = j == length ? own[letter] : RunList<N, KL>{};
    const auto r = (longer | mine);
    Residual<N, P, K> next = acc;
    if (k == 0) {
      next.accepting = r(N{0});
      return next;
    }
    const Maybe<RunList<N, K>> fits = narrowed<K>(r);
    if (!fits) return {};
    next.pending[k - 1] = *fits;
    return next;
  }
};
/** @param s a state.  @param a a letter.  @return its derivative by
 *  @c a, or nothing where a pending set outgrows its bound. */
template <std::size_t Sigma, IsCountWord N, std::size_t P, std::size_t K,
          std::size_t KL>
constexpr Maybe<Residual<N, P, K>> step(
    const Residual<N, P, K>& s, const std::array<RunList<N, KL>, Sigma>& own,
    N length, N a) {
  return fold(std::views::iota(N{0}, length),
              Maybe<Residual<N, P, K>>{Residual<N, P, K>{}},
              Advance<Sigma, N, P, K, KL>{s, own, length, a});
}
/** @brief The construction so far: the states found, in the order found,
 *  and the plan's rows for those already walked. */
template <std::size_t Sigma, IsCountWord N, std::size_t P, std::size_t K,
          std::size_t Q>
struct Build {
  AtMost<Residual<N, P, K>, Q> states;
  Automaton<Sigma, Q> plan;
  std::array<std::uint16_t, Sigma> row{};
};
/** @brief One letter out of the state being walked: its derivative's
 *  number, a new state numbered next where it is new. */
template <std::size_t Sigma, IsCountWord N, std::size_t P, std::size_t K,
          std::size_t KL, std::size_t Q>
struct Letter {
  Residual<N, P, K> from;
  std::array<RunList<N, KL>, Sigma> own;
  N length;
  using B = Build<Sigma, N, P, K, Q>;
  constexpr Maybe<B> operator()(const B& acc, N a) const {
    const Maybe<Residual<N, P, K>> t = step<Sigma>(from, own, length, a);
    if (!t) return {};
    B next = acc;
    const auto at = std::ranges::find(next.states, *t);
    if (at == next.states.end()) {
      if (next.states.size() == Q) return {};
      next.row[a] = static_cast<std::uint16_t>(next.states.size());
      next.states = next.states.with(*t);
    } else {
      next.row[a] = static_cast<std::uint16_t>(at - next.states.begin());
    }
    return next;
  }
};
/** @brief One state walked, in the order found: its row of transitions,
 *  and whether it accepts.  Past the states found, nothing to walk. */
template <std::size_t Sigma, IsCountWord N, std::size_t P, std::size_t K,
          std::size_t KL, std::size_t Q>
struct Walk {
  std::array<RunList<N, KL>, Sigma> own;
  N length;
  using B = Build<Sigma, N, P, K, Q>;
  constexpr Maybe<B> operator()(const B& acc, std::size_t i) const {
    if (i >= acc.states.size()) return acc;
    const Residual<N, P, K> s = acc.states.items[i];
    const Maybe<B> done =
        fold(std::views::iota(N{0}, static_cast<N>(Sigma)), Maybe<B>{acc},
             Letter<Sigma, N, P, K, KL, Q>{s, own, length});
    if (!done) return {};
    B next = *done;
    next.plan = {next.plan.next.with(next.row),
                 next.plan.accepting.with(s.accepting)};
    return next;
  }
};
}  // namespace detail_automaton

/** @brief The matcher specialised to a set of words of one length: the
 *  minimal automaton of @f$\Sigma^* L@f$, canonical.  @tparam Sigma the
 *  letters.  @tparam P the bound on the words' length.  @tparam K the bound
 *  on the steps of a pending set.  @tparam Q the bound on the states.
 *  @param sites the words, as the indices of their letters in base
 *  @c Sigma, the first letter the most significant.  @param length their
 *  length.  @return the plan, or nothing where a bound is too small. */
export template <std::size_t Sigma, std::size_t P, std::size_t K, std::size_t Q,
                 IsCountWord N, std::size_t KL>
constexpr Maybe<Automaton<Sigma, Q>> automaton(const RunList<N, KL>& sites,
                                               std::type_identity_t<N> length) {
  if (length == 0 || length > P) return {};
  // the codes of the words must fit the word, σ^length and all below it
  if (!detail_periodic::checked_power(static_cast<N>(Sigma), length)) return {};
  using B = detail_automaton::Build<Sigma, N, P, K, Q>;
  const B start{
      AtMost<detail_automaton::Residual<N, P, K>, Q>{}.with({}), {}, {}};
  using Derive = detail_automaton::DeriveSites<Sigma, N, KL>;
  const typename Derive::Own own =
      fold(std::views::iota(N{0}, static_cast<N>(Sigma)),
           typename Derive::Own{}, Derive{sites, length});
  const Maybe<B> built =
      fold(std::views::iota(0uz, Q), Maybe<B>{start},
           detail_automaton::Walk<Sigma, N, P, K, KL + 2, Q>{own, length});
  if (!built) return {};
  return built->plan;
}

namespace detail_automaton {
/** @param p a power.  @return its letters, in the chain's order: the
 *  chain filtered by its predicate, collected, one past the bytes' 256 so
 *  that a wider chain is seen as such and refused, not cut to 256. */
template <HasNNOStep C, IsArrow F, IsCountWord N>
constexpr AtMost<C, 257> letters_of(const Power<C, F, N>& p) {
  return collect(p.letters >> where(p.member) >> take<257>());
}
/** @brief A code read back as its word: the digits in base @c Sigma, the
 *  first letter the most significant, each the letter of that rank. */
template <std::size_t Sigma, HasNNOStep C, std::size_t P, IsCountWord N>
struct Decode {
  AtMost<C, 257> alphabet;
  N length;
  struct Digits {
    std::array<C, P> word{};
    N rest{0};
  };
  struct Digit {
    AtMost<C, 257> alphabet;
    N length;
    constexpr Digits operator()(const Digits& d, N j) const {
      Digits next = d;
      next.word[length - 1 - j] = alphabet.items[d.rest % Sigma];
      next.rest = static_cast<N>(d.rest / Sigma);
      return next;
    }
  };
  constexpr AtMost<C, P> operator()(N code) const {
    const Digits d = fold(std::views::iota(N{0}, length), Digits{{}, code},
                          Digit{alphabet, length});
    return {d.word, static_cast<std::size_t>(length)};
  }
};
/** @brief A code tested by a section: read back as its word, then the
 *  section's own membership. */
template <typename Section, typename Decoder>
struct ReadBack {
  Section section;
  Decoder decode;
  template <IsCountWord N>
  constexpr bool operator()(N code) const {
    return section(decode(code));
  }
};
}  // namespace detail_automaton

namespace detail_automaton {
/** @brief Every code read back as its word and tested by the section. */
template <std::size_t KL, std::size_t Sigma, std::size_t P, HasNNOStep C,
          IsArrow F, IsCountWord N, typename S, IsArrow T>
constexpr Maybe<RunList<N, KL>> tested_run_list(
    const Symmetric<Power<C, F, N>, S, T>& sites) {
  const AtMost<C, 257> alphabet = letters_of(sites.power);
  const N n = sites.power.length;
  // exactly Sigma letters: a wider chain shows a letter past Sigma
  if (alphabet.size() != Sigma || n > P) return {};
  // σ^n, nothing where it outgrows the word
  const Maybe<N> end = detail_periodic::checked_power(static_cast<N>(Sigma), n);
  if (!end) return {};
  using Decoder = Decode<Sigma, C, P, N>;
  using Section = Symmetric<Power<C, F, N>, S, T>;
  return run_list<KL>(*end,
                      ReadBack<Section, Decoder>{sites, Decoder{alphabet, n}});
}
}  // namespace detail_automaton
/** @brief A section's words as a run list over their codes: each word read
 *  as its letters' ranks in base @f$\sigma@f$, the first the most
 *  significant, so the chain @f$[0, \sigma^n)@f$ in the dictionary order.
 *  Every code is read back as its word and tested by the section's own
 *  membership: @f$\sigma^n@f$ tests, the set enumerated once.
 *  @tparam KL the bound claimed on the steps.  @tparam Sigma the letters.
 *  @tparam P the bound on the words' length.  @param sites the section.
 *  @return its words, or nothing where a bound is too small. */
export template <std::size_t KL, std::size_t Sigma, std::size_t P, HasNNOStep C,
                 IsArrow F, IsCountWord N, typename S, IsArrow T>
constexpr Maybe<RunList<N, KL>> run_list(
    const Symmetric<Power<C, F, N>, S, T>& sites) {
  return detail_automaton::tested_run_list<KL, Sigma, P>(sites);
}
namespace detail_automaton {
/** @brief One more digit of a half-site read backwards, reflected: the next
 *  letter of the other half, @f$\sigma - 1 - d@f$ on the ranks. */
template <std::size_t Sigma, IsCountWord N>
struct MirrorDigit {
  constexpr std::pair<N, N> operator()(std::pair<N, N> s, N) const {
    return {static_cast<N>(s.first / Sigma),
            static_cast<N>(s.second * Sigma + (Sigma - 1 - s.first % Sigma))};
  }
};
/** @brief One more half-site, its word's code a run of one where it lies in
 *  the codes kept.  Halves ascending give codes ascending, the half being the
 *  word's most significant digits. */
template <std::size_t Sigma, IsCountWord N, std::size_t KL>
struct AddPalindrome {
  N half;   ///< the half's length
  N high;   ///< sigma^half
  N first;  ///< the least code kept
  N last;   ///< one past the greatest
  constexpr AtMost<N, KL> operator()(AtMost<N, KL> steps, N h) const {
    const N low = fold(std::views::iota(N{0}, half), std::pair<N, N>{h, N{0}},
                       MirrorDigit<Sigma, N>{})
                      .second;
    const N code = static_cast<N>(h * high + low);
    if (code < first || code >= last) return steps;
    return steps.with(code).with(static_cast<N>(code + 1));
  }
};
/** @brief The palindromes of even length @c n among the codes
 *  [prefix, prefix + 1) @f$\cdot \sigma^{n - L}@f$: only their half-sites are
 *  visited, none tested.  @param n the length.  @param prefix the code of the
 *  @c L letters kept.  @return the run list, or nothing where @c KL is too
 *  small or a power outgrows the word. */
template <std::size_t KL, std::size_t Sigma, IsCountWord N>
constexpr Maybe<RunList<N, KL>> palindromes(N n, N prefix, N L) {
  const N half = static_cast<N>(n / 2);
  const Maybe<N> high =
      detail_periodic::checked_power(static_cast<N>(Sigma), half);
  const Maybe<N> tail = detail_periodic::checked_power(static_cast<N>(Sigma),
                                                       static_cast<N>(n - L));
  if (!high || !tail) return {};
  // the half-sites the prefix allows: its letters, or the first half of them
  const N cut = L > half ? static_cast<N>(L - half) : N{0};
  const N open = L < half ? static_cast<N>(half - L) : N{0};
  const N drop = *detail_periodic::checked_power(static_cast<N>(Sigma), cut);
  const N grow = *detail_periodic::checked_power(static_cast<N>(Sigma), open);
  const N lo = static_cast<N>(prefix / drop * grow);
  const N hi = static_cast<N>(lo + grow);
  if (static_cast<std::size_t>(hi - lo) * 2 > KL) return {};
  return RunList<N, KL>{fold(
      std::views::iota(lo, hi), AtMost<N, KL>{},
      AddPalindrome<Sigma, N, KL>{half, *high, static_cast<N>(prefix * *tail),
                                  static_cast<N>((prefix + 1) * *tail)})};
}
}  // namespace detail_automaton
/** @brief The words a reflection then the reversal fixes, listed by the law:
 *  a word is fixed by its half-site, so the @f$\sigma^{n/2}@f$ halves are
 *  visited in order and no word is tested, where the general overload tests
 *  all @f$\sigma^n@f$.  This is what lets a small panel's table be built at
 *  compile time.  With an even alphabet the reflection fixes no letter, and
 *  from four letters no two codes are adjacent, so each is a run of its own.
 *  An odd length, or a reflection other than the alphabet's own, is left to
 *  the general overload.  @param sites the section.  @return its words, or
 *  nothing where a bound is too small. */
export template <std::size_t KL, std::size_t Sigma, std::size_t P,
                 IsCountWord N>
  requires(Sigma >= 4 && Sigma % 2 == 0)
constexpr Maybe<RunList<N, KL>> run_list(
    const Symmetric<Power<std::byte, Letters, N>, Reverse, Reflection>& sites) {
  const N n = sites.power.length;
  if (n == 0 || n % 2 != 0 || sites.tau != refl(sites.power.member)) {
    return detail_automaton::tested_run_list<KL, Sigma, P>(sites);
  }
  if (sites.power.member.members.count() != Sigma || n > P) return {};
  return detail_automaton::palindromes<KL, Sigma>(n, N{0}, N{0});
}
namespace detail_automaton {
/** @brief A plan with the columns of its alphabet's bytes. */
template <std::size_t Sigma, std::size_t Q>
struct WithLetters {
  Letters letters;
  constexpr Automaton<Sigma, Q> operator()(Automaton<Sigma, Q> a) const {
    a.columns = columns_of(letters);
    return a;
  }
};
/** @param member a power's predicate.  @return its letters, where it is the
 *  bytes' subobject; none otherwise. */
constexpr Letters letters_of(const Letters& member) { return member; }
template <typename F>
constexpr Letters letters_of(const F&) {
  return {};
}
}  // namespace detail_automaton
/** @brief The matcher specialised to a section: its words as a run list,
 *  then the plan.  The capacities default to a menu's largest, four
 *  letters, words of length 8, pending sets of 32 steps and 320 states, the
 *  one @c pynzli offers too, so @c automaton(sites) reads alike in both
 *  languages; a tighter bound is named where the plan is a constant.
 *  @tparam KL the bound claimed on the section's steps.
 *  @param sites the section, Listing 5's @c sites.  @return the plan, or
 * nothing where a bound is too small. */
export template <std::size_t Sigma = 4, std::size_t P = 8, std::size_t K = 32,
                 std::size_t Q = 320, std::size_t KL = 256, HasNNOStep C,
                 IsArrow F, IsCountWord N, typename S, IsArrow T>
constexpr Maybe<Automaton<Sigma, Q>> automaton(
    const Symmetric<Power<C, F, N>, S, T>& sites) {
  const Maybe<RunList<N, KL>> words = run_list<KL, Sigma, P>(sites);
  if (!words) return {};
  return automaton<Sigma, P, K, Q>(*words, sites.power.length)
      .transform(detail_automaton::WithLetters<Sigma, Q>{
          detail_automaton::letters_of(sites.power.member)});
}

/** @brief @c prefix("GA"): the words beginning with these letters, the
 *  predicate a section is met with.  At most eight, the longest word a plan's
 *  menu holds. */
export struct Prefix {
  AtMost<std::byte, 8> word;
};
namespace detail_automaton {
/** @brief One more letter of a prefix. */
struct AddByte {
  constexpr AtMost<std::byte, 8> operator()(AtMost<std::byte, 8> w,
                                            char c) const {
    return w.with(detail_periodic::ToByte{}(c));
  }
};
}  // namespace detail_automaton
/** @param word the letters, written out.  @return the predicate. */
export constexpr Prefix prefix(std::string_view word) {
  return {std::ranges::fold_left(word, AtMost<std::byte, 8>{},
                                 detail_automaton::AddByte{})};
}
/** @brief A section met with a prefix.  Its own type, so that only what knows
 *  the prefix reads it: @c count_factors reads cycle types alone, and would
 *  count the section as if no letter were fixed, so no count is offered until
 *  it has a law.  @tparam P @tparam S @tparam T the section's. */
export template <typename P, typename S, IsArrow T>
struct Prefixed {
  Symmetric<P, S, T> section;
  AtMost<std::byte, 8> word;
  /** @param w a word.  @return whether the section holds it and it begins with
   *  the prefix. */
  template <std::ranges::random_access_range W>
    requires std::ranges::sized_range<W>
  constexpr bool operator()(const W& w) const {
    return section(w) &&
           std::ranges::equal(w | std::views::take(word.size()), word);
  }
};
/** @param s the section.  @param p the prefix.  @return their meet. */
export template <typename P, typename S, IsArrow T>
constexpr Prefixed<P, S, T> operator&(const Symmetric<P, S, T>& s,
                                      const Prefix& p) {
  return {s, p.word};
}
namespace detail_automaton {
/** @brief Refuses a word with a byte outside its alphabet. */
[[noreturn]] constexpr void refuse_foreign_letter() { std::abort(); }
/** @brief One more byte of a word: its rank, the next digit. */
struct NextByte {
  Letters on;
  constexpr std::size_t operator()(std::size_t index, std::byte b) const {
    const Maybe<std::size_t> r = Ranked{on}(b);
    if (!r) refuse_foreign_letter();
    return index * on.members.count() + *r;
  }
};
}  // namespace detail_automaton
/** @brief The words of a section beginning with a prefix, by the law: no word
 *  tested, only the half-sites the prefix allows visited.  @param sites the
 *  meet.  @return its words, or nothing where a bound is too small. */
export template <std::size_t KL, std::size_t Sigma, std::size_t P,
                 IsCountWord N>
  requires(Sigma >= 4 && Sigma % 2 == 0)
constexpr Maybe<RunList<N, KL>> run_list(
    const Prefixed<Power<std::byte, Letters, N>, Reverse, Reflection>& sites) {
  const std::size_t L = sites.word.size();
  const N n = sites.section.power.length;
  const Letters on = sites.section.power.member;
  if (n == 0 || n % 2 != 0 || n > P || L > n || on.members.count() != Sigma ||
      sites.section.tau != refl(on))
    return {};
  return detail_automaton::palindromes<KL, Sigma>(
      n,
      static_cast<N>(std::ranges::fold_left(sites.word, std::size_t{0},
                                            detail_automaton::NextByte{on})),
      static_cast<N>(L));
}
/** @brief The matcher of the words of a section beginning with a prefix.  The
 *  same builder on a subset of the same words; capacities as for a section.
 *  @param sites the meet.  @return the plan, or nothing where a bound is too
 *  small. */
export template <std::size_t Sigma = 4, std::size_t P = 8, std::size_t K = 32,
                 std::size_t Q = 320, std::size_t KL = 256, IsCountWord N>
constexpr Maybe<Automaton<Sigma, Q>> automaton(
    const Prefixed<Power<std::byte, Letters, N>, Reverse, Reflection>& sites) {
  const Maybe<RunList<N, KL>> words = run_list<KL, Sigma, P>(sites);
  if (!words) return {};
  return automaton<Sigma, P, K, Q>(*words, sites.section.power.length)
      .transform(
          detail_automaton::WithLetters<Sigma, Q>{sites.section.power.member});
}
/** @param plan a plan.  @return how many states it has. */
export template <std::size_t Sigma, std::size_t Q>
constexpr std::size_t states(const Automaton<Sigma, Q>& plan) {
  return plan.next.size();
}

/** @brief What a byte outside the alphabet does to a match: break it, the
 *  matcher back at its start (no word of the set holds the byte), or be
 *  skipped, the matcher as it was.  Never a match itself. */
export enum class Holes : bool { break_match, skip };
/** @brief @c matches(plan), the stage that runs a matcher along a text: a
 *  query over the text's positions, present where a word of the plan's set
 *  ends.  @tparam Sigma the letters.  @tparam Q the bound on the states. */
export template <std::size_t Sigma, std::size_t Q>
struct Matches {
  Automaton<Sigma, Q> plan;
  Holes holes = Holes::break_match;
};
/** @param plan the matcher.  @param holes what a hole does.  @return the
 *  stage. */
export template <std::size_t Sigma, std::size_t Q>
constexpr Matches<Sigma, Q> matches(const Automaton<Sigma, Q>& plan,
                                    Holes holes = Holes::break_match) {
  return {plan, holes};
}
/** @brief @c matches on a plan that may not be, @c automaton(section)'s
 *  @c Maybe: the stage, or nothing, by @c Maybe's action.  The uncertainty
 *  of the capacity travels through the query to its count, so
 *  @c dna @c >> @c matches(automaton(s)) @c >> @c counting
 *  is a @c Maybe count with no case on it in between.  @param plan the plan,
 *  or nothing.  @param holes what a hole does.  @return the stage, or
 *  nothing. */
export template <std::size_t Sigma, std::size_t Q>
constexpr Maybe<Matches<Sigma, Q>> matches(
    const Maybe<Automaton<Sigma, Q>>& plan, Holes holes = Holes::break_match) {
  if (!plan) return {};
  return matches(*plan, holes);
}
namespace detail_automaton {
/** @brief Where a run of the matcher is: its state and the next position. */
struct Cursor {
  std::uint16_t at{0};
  std::size_t position{0};
  constexpr bool operator==(const Cursor&) const = default;
};
/** @brief One position of the run: the state after its letter, and the
 *  position itself where that state accepts.  O(1) state, the text read
 *  where it is. */
template <std::size_t Sigma, std::size_t Q, std::ranges::view V>
struct MatchStep {
  Automaton<Sigma, Q> plan;
  Holes holes;
  V letters;
  using Yield = std::optional<std::pair<Maybe<std::size_t>, Cursor>>;
  /** @brief A hole at @c c: no match ends here; the run restarts, or goes
   *  on as it was. */
  constexpr Yield hole(const Cursor& c) const {
    return std::pair{
        Maybe<std::size_t>{},
        Cursor{holes == Holes::skip ? c.at : std::uint16_t{0}, c.position + 1}};
  }
  /** @brief A letter at @c c: the transition, where the letter is one of
   *  the plan's @c Sigma; a rank past them (an alphabet wider than the
   *  plan's) is a hole, never a row past the table. */
  template <IsCountWord N>
  constexpr Yield letter(const Cursor& c, N rank) const {
    if (rank >= static_cast<N>(Sigma)) return hole(c);
    return hit(plan.next.items[c.at][rank], c.position);
  }
  constexpr Yield operator()(const Cursor& c) const {
    if (c.position >= static_cast<std::size_t>(std::ranges::size(letters)))
      return std::nullopt;
    const auto x = std::ranges::begin(
        letters)[static_cast<std::ranges::range_difference_t<V>>(c.position)];
    using X = std::remove_cvref_t<decltype(x)>;
    if constexpr (std::same_as<X, std::byte>) {
      const Maybe<std::size_t> column = plan.columns(x);
      return column ? letter(c, *column) : hole(c);
    } else if constexpr (detail_comprehension::IsMaybe<X>) {
      return x ? letter(c, *x) : hole(c);
    } else {
      return letter(c, x);
    }
  }
  constexpr std::pair<Maybe<std::size_t>, Cursor> hit(std::uint16_t to,
                                                      std::size_t p) const {
    return {
        plan.accepting.items[to] ? Maybe<std::size_t>{p} : Maybe<std::size_t>{},
        Cursor{to, p + 1}};
  }
};
}  // namespace detail_automaton
/** @param letters a text read as letters.  @param m the stage, or nothing.
 *  @return the query, or nothing: @c Maybe's action on the stage. */
export template <std::ranges::view V, std::size_t Sigma, std::size_t Q>
  requires std::ranges::random_access_range<V> && std::ranges::sized_range<V> &&
           std::semiregular<V>
constexpr auto operator>>(V letters, const Maybe<Matches<Sigma, Q>>& m) {
  using Query = decltype(std::move(letters) >> *m);
  if (!m) return Maybe<Query>{};
  return Maybe<Query>{std::move(letters) >> *m};
}
namespace detail_automaton {
/** @brief Refuses bytes for a plan that knows no alphabet to read them in. */
[[noreturn]] constexpr void refuse_no_letters() { std::abort(); }
}  // namespace detail_automaton
/** @param letters a text of the caller's: bytes, read through the plan's own
 *  columns, so no text is read in another alphabet; or letters' ranks, also
 *  as @c rank(alphabet) reads them; a view, read where it is.  @param m the
 *  stage.  @return the query of the positions where a word of the set ends:
 *  count it, take from it, collect it. */
export template <std::ranges::view V, std::size_t Sigma, std::size_t Q>
  requires std::ranges::random_access_range<V> && std::ranges::sized_range<V> &&
           std::semiregular<V>
constexpr auto operator>>(V letters, const Matches<Sigma, Q>& m) {
  if constexpr (std::same_as<std::ranges::range_value_t<V>, std::byte>) {
    if (!m.plan.columns.any) detail_automaton::refuse_no_letters();
  }
  return unfold(detail_automaton::Cursor{},
                detail_automaton::MatchStep<Sigma, Q, V>{m.plan, m.holes,
                                                         std::move(letters)});
}

/** @brief @c scan(plan, s): the history comonad's extension with its kernel
 *  factored through finitely many states, so that it keeps one state, not the
 *  history (Myhill and Nerode).  It is @c matches(plan), its holes breaking a
 *  match.  @param plan the plan.  @param letters a text.  @return the query. */
export template <std::ranges::view V, std::size_t Sigma, std::size_t Q>
  requires std::ranges::random_access_range<V> && std::ranges::sized_range<V> &&
           std::semiregular<V>
constexpr auto scan(const Automaton<Sigma, Q>& plan, V letters) {
  return std::move(letters) >> matches(plan);
}
/** @brief @c s @c >> @c scanner: a plan read as the stage it is.  @param
 * letters a text.  @param plan the plan.  @return the query. */
export template <std::ranges::view V, std::size_t Sigma, std::size_t Q>
  requires std::ranges::random_access_range<V> && std::ranges::sized_range<V> &&
           std::semiregular<V>
constexpr auto operator>>(V letters, const Automaton<Sigma, Q>& plan) {
  return scan(plan, std::move(letters));
}
/** @brief Where a prefixed section's words occur in a range of the caller's:
 *  the windows of its length it holds, as @c r @c >> @c s for a section.
 *  @param r the view.  @param s the meet.  @return the query. */
export template <detail_comprehension::IsForeignView R, typename P, typename S,
                 IsArrow T>
constexpr auto operator>>(R r, const Prefixed<P, S, T>& s) {
  return std::move(r) >>
         windows(static_cast<std::size_t>(s.section.power.length)) >>
         where(arrow<std::span<const Dom<T>>, bool>(s));
}

// The rank of a byte in an alphabet is a Kleisli arrow into Maybe: a
// letter's rank, or nothing.
template <>
inline constexpr bool is_kleisli_arrow_v<Ranked, maybe_monad_tag> = true;
template <>
inline constexpr bool is_kleisli_arrow_v<Columns, maybe_monad_tag> = true;
static_assert(IsKleisliArrow<Ranked, maybe_monad_tag> &&
              IsKleisliArrow<Columns, maybe_monad_tag>);

// EcoRI's site GAATTC, its letters ranked A < C < G < T, is the index
// 2·4^5 + 0 + 0 + 3·4^2 + 3·4 + 1 = 2109.  Its plan is Knuth, Morris and
// Pratt's: the state is how much of the site has matched, numbered so by the
// breadth-first walk, and only the last accepts.
namespace detail_automaton {
/** @return KMP's plan for GAATTC, written out: the witness's expectation. */
constexpr Automaton<4, 8> kmp_gaattc() {
  Automaton<4, 8> a{};
  using Row = std::array<std::uint16_t, 4>;
  a.next = a.next.with(Row{0, 0, 1, 0})
               .with(Row{2, 0, 1, 0})
               .with(Row{3, 0, 1, 0})
               .with(Row{0, 0, 1, 4})
               .with(Row{0, 0, 1, 5})
               .with(Row{0, 6, 1, 0})
               .with(Row{0, 0, 1, 0});
  a.accepting = a.accepting.with(false)
                    .with(false)
                    .with(false)
                    .with(false)
                    .with(false)
                    .with(false)
                    .with(true);
  return a;
}
}  // namespace detail_automaton
constexpr Automaton<4, 8> kmp_gaattc = detail_automaton::kmp_gaattc();
static_assert(automaton<4, 6, 4, 8>(run<2>(2109uz, 2110uz), 6) == kmp_gaattc);
// A claimed bound too small is nothing, not a failure: seven states do not
// fit six.
static_assert(!automaton<4, 6, 4, 6>(run<2>(2109uz, 2110uz), 6).has_value());
// The matches of the site, where it ends: once in TTGAATTCAAGCTTA, at
// position 7, the C of its GAATTC; twice in GAATTCGAATTC.  The texts are
// the caller's, read through a span.
namespace detail_automaton {
constexpr std::array<std::size_t, 15> ttgaattcaagctta{3, 3, 2, 0, 0, 3, 3, 1,
                                                      0, 0, 2, 1, 3, 3, 0};
constexpr std::array<std::size_t, 12> gaattcgaattc{2, 0, 0, 3, 3, 1,
                                                   2, 0, 0, 3, 3, 1};
}  // namespace detail_automaton
static_assert(
    count(std::span<const std::size_t>{detail_automaton::ttgaattcaagctta} >>
          matches(kmp_gaattc)) == 1 &&
    count(std::span<const std::size_t>{detail_automaton::gaattcgaattc} >>
          matches(kmp_gaattc)) == 2);
static_assert(std::ranges::equal(
    collect(std::span<const std::size_t>{detail_automaton::ttgaattcaagctta} >>
            matches(kmp_gaattc) >> take<1>()),
    std::array{7uz}));

// The laws a section's scan keeps, stated once on one text.  A table, and the
// windows it was built from, answer alike at every prefix: the kernel factors
// through the states.  A smaller set finds no more.  And a set the reverse
// complement fixes finds as many on the other strand.
namespace detail_automaton {
constexpr auto law_section = power(abc("ACGT"), 6uz) & fixed_by(~rev);
constexpr auto law_subset = law_section & prefix("GA");
constexpr auto law_plan = *automaton<4, 6, 8, 12>(law_subset);
constexpr auto law_text = bytes("GAATTCGAGCTCGGTACCCGGGGATCC");
/** @brief The kernel the table factors: whether a prefix ends in a word of the
 *  set, reading its last six letters and nothing kept. */
struct EndsInSite {
  using Domain = std::span<const std::byte>;
  using Codomain = bool;
  constexpr bool operator()(std::span<const std::byte> p) const {
    return p.size() >= 6 && law_subset(p.last(6));
  }
};
constexpr bool plan_factors_the_kernel() {
  const std::span<const std::byte> s{law_text};
  return static_cast<std::size_t>(std::ranges::count(
             scan(EndsInSite{}, s), true)) == count(scan(law_plan, s));
}
/** @brief Whether the table and the windows agree on one more prefix. */
struct AgreesOnPrefix {
  constexpr bool operator()(bool ok, std::size_t n) const {
    const std::span<const std::byte> s{law_text.data(), n};
    return ok && count(s >> law_plan) == count(s >> law_subset);
  }
};
constexpr bool table_is_the_kernel() {
  return std::ranges::fold_left(std::views::iota(0uz, law_text.size() + 1),
                                true, AgreesOnPrefix{});
}
constexpr bool one_strand_is_enough() {
  const std::vector<std::byte> s(law_text.begin(), law_text.end());
  const std::vector<std::byte> other = (rev >> power(refl(letters("ACGT"))))(s);
  return count(std::span<const std::byte>{s} >> law_section) ==
         count(std::span<const std::byte>{other} >> law_section);
}
// The same laws on random words, seeded, at compile time.  Two letters, A and
// T, so that sites are dense enough to test anything: about one window in eight
// is a site, one in thirty-two begins AT.
constexpr auto random_subset = law_section & prefix("AT");
constexpr auto random_plan = *automaton<4, 6, 16, 32>(random_subset);
constexpr std::size_t random_length = 24;
/** @brief The kernel the random words' table factors. */
struct EndsInSubset {
  using Domain = std::span<const std::byte>;
  using Codomain = bool;
  constexpr bool operator()(std::span<const std::byte> p) const {
    return p.size() >= 6 && random_subset(p.last(6));
  }
};
/** @brief What the seeds say: whether the laws hold on their words, and how
 *  many sites of the subset those held, so a vacuous run can be refused. */
struct RandomLaws {
  bool hold{true};
  std::size_t sites{0};
};
struct LawsOnSeed {
  constexpr RandomLaws operator()(RandomLaws r, std::uint64_t seed) const {
    const auto word = random_word<random_length>(abc("AT"), seed);
    const std::span<const std::byte> s{word};
    const std::size_t by_plan = count(scan(random_plan, s));
    const std::size_t by_kernel = static_cast<std::size_t>(
        std::ranges::count(scan(EndsInSubset{}, s), true));
    const std::vector<std::byte> mine(word.begin(), word.end());
    const std::vector<std::byte> other =
        (rev >> power(refl(letters("ACGT"))))(mine);
    return {r.hold && by_plan == by_kernel &&
                by_plan == count(s >> random_subset) &&
                count(s >> random_subset) <= count(s >> law_section) &&
                count(s >> law_section) ==
                    count(std::span<const std::byte>{other} >> law_section),
            r.sites + by_plan};
  }
};
constexpr RandomLaws on_random_words() {
  return std::ranges::fold_left(
      std::views::iota(std::uint64_t{0}, std::uint64_t{4}), RandomLaws{},
      LawsOnSeed{});
}
}  // namespace detail_automaton
static_assert(detail_automaton::plan_factors_the_kernel(),
              "the plan's extension is the kernel's, in one state");
static_assert(detail_automaton::table_is_the_kernel(),
              "the table is the windows' answer, at every prefix");
static_assert(
    count(detail_automaton::law_text >> detail_automaton::law_subset) <=
        count(detail_automaton::law_text >> detail_automaton::law_section),
    "reading through a smaller set finds no more");
static_assert(detail_automaton::on_random_words().hold,
              "the scan's laws, on seeded random words");
static_assert(detail_automaton::on_random_words().sites > 0,
              "and those words held sites, or the laws were vacuous");
static_assert(
    detail_automaton::one_strand_is_enough(),
    "a set the reverse complement fixes finds as many on either strand");

}  // namespace dedekind::category
