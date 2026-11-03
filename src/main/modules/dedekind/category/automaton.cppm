/**
 * @file dedekind/category/automaton.cppm
 * @partition :automaton
 * @brief The matcher of a set of words of one length, specialised to it: the
 *        minimal automaton of @f$\Sigma^* L@f$, a value computed by one
 *        @c constexpr function at either binding time.
 *
 * A scan reads a text letter by letter and reports where a word of @c L
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
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */
module;

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <ranges>

export module dedekind.category:automaton;

import :morphism;  // Identity: a chain's letters read as they are
import :nno;       // HasNNOStep: the letters' chain
import :chain;     // IsCountWord
import :functor;   // Maybe
import :lattice;   // Join, Meet: a union, a restriction, as the lattice's nodes
import :periodic;  // AtMost, shift, Symmetric, Power: a section of words
import :runs;      // RunList, run, reduced, narrowed
import :total;     // power: σ^k by squaring
import :action;    // fold: a plain fold
import :kleisli;   // fold in the Kleisli category: the steps that may fail
import :comprehension;  // count, take, collect: the matches are a query

namespace dedekind::category {

/** @brief A scan's plan: the transitions on each letter, and the states
 *  where a word of the set has just been read.  @tparam Sigma the letters.
 *  @tparam Q the bound on the states. */
export template <std::size_t Sigma, std::size_t Q>
  requires(Q <= 65536)
struct Automaton {
  AtMost<std::array<std::uint16_t, Sigma>, Q> next;
  AtMost<bool, Q> accepting;
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
  return shift<N>(lo) >> reduced(Meet<RunList<N, K>, RunList<N, 2>>{
                             r, run<2>(lo, static_cast<N>(lo + b))});
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
    const auto r =
        reduced(Join<decltype(longer), decltype(mine)>{longer, mine});
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
/** @brief One byte of a power's chain kept where it is a letter: the
 *  alphabet in its order, as a list. */
template <HasNNOStep C, IsArrow F>
struct KeepLetter {
  F member;
  constexpr AtMost<C, 256> operator()(const AtMost<C, 256>& acc, C c) const {
    return member(c) ? acc.with(c) : acc;
  }
};
/** @param p a power.  @return its letters, in the chain's order. */
template <HasNNOStep C, IsArrow F, IsCountWord N>
constexpr AtMost<C, 256> letters_of(const Power<C, F, N>& p) {
  return fold(p.letters >> Identity<C>{}, AtMost<C, 256>{},
              KeepLetter<C, F>{p.member});
}
/** @brief A code read back as its word: the digits in base @c Sigma, the
 *  first letter the most significant, each the letter of that rank. */
template <std::size_t Sigma, HasNNOStep C, std::size_t P, IsCountWord N>
struct Decode {
  AtMost<C, 256> alphabet;
  N length;
  struct Digits {
    std::array<C, P> word{};
    N rest{0};
  };
  struct Digit {
    AtMost<C, 256> alphabet;
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
/** @brief One code of the sweep: its word in the section or not, a step
 *  where that changes, nothing where the steps outgrow their bound. */
template <typename Section, typename Decoder, IsCountWord N, std::size_t KL>
struct SweepWords {
  Section section;
  Decoder decode;
  struct State {
    bool inside{false};
    AtMost<N, KL> steps;
  };
  constexpr Maybe<State> operator()(const State& s, N code) const {
    const bool in = section(decode(code));
    if (in == s.inside) return s;
    if (s.steps.size() == KL) return {};
    return State{in, s.steps.with(code)};
  }
};
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
  const AtMost<C, 256> alphabet = detail_automaton::letters_of(sites.power);
  const N n = sites.power.length;
  if (alphabet.size() != Sigma || n > P) return {};
  const N end =
      detail_concat::power(static_cast<N>(Sigma), n, std::multiplies<N>{});
  using Decoder = detail_automaton::Decode<Sigma, C, P, N>;
  using Sweep = detail_automaton::SweepWords<Symmetric<Power<C, F, N>, S, T>,
                                             Decoder, N, KL>;
  const Maybe<typename Sweep::State> swept =
      fold(std::views::iota(N{0}, end),
           Maybe<typename Sweep::State>{typename Sweep::State{}},
           Sweep{sites, Decoder{alphabet, n}});
  if (!swept) return {};
  if (!swept->inside) return RunList<N, KL>{swept->steps};
  if (swept->steps.size() == KL) return {};
  return RunList<N, KL>{swept->steps.with(end)};
}
/** @brief The matcher specialised to a section: its words as a run list,
 *  then the plan.  @tparam KL the bound claimed on the section's steps.
 *  @param sites the section, as @c sites(6).  @return the plan, or nothing
 *  where a bound is too small. */
export template <std::size_t Sigma, std::size_t P, std::size_t K, std::size_t Q,
                 std::size_t KL = 256, HasNNOStep C, IsArrow F, IsCountWord N,
                 typename S, IsArrow T>
constexpr Maybe<Automaton<Sigma, Q>> automaton(
    const Symmetric<Power<C, F, N>, S, T>& sites) {
  const Maybe<RunList<N, KL>> words = run_list<KL, Sigma, P>(sites);
  if (!words) return {};
  return automaton<Sigma, P, K, Q>(*words, sites.power.length);
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
  template <IsCountWord N>
  constexpr std::uint16_t after(std::uint16_t at, N letter) const {
    return plan.next.items[at][letter];
  }
  constexpr std::optional<std::pair<Maybe<std::size_t>, Cursor>> operator()(
      const Cursor& c) const {
    if (c.position >= static_cast<std::size_t>(std::ranges::size(letters)))
      return std::nullopt;
    const auto letter = std::ranges::begin(
        letters)[static_cast<std::ranges::range_difference_t<V>>(c.position)];
    const Cursor next{c.at, c.position + 1};
    if constexpr (detail_comprehension::IsMaybe<
                      std::remove_cvref_t<decltype(letter)>>) {
      if (!letter) {
        // a hole: no match ends here; the run restarts or goes on as it was
        return std::pair{Maybe<std::size_t>{},
                         Cursor{holes == Holes::skip ? c.at : std::uint16_t{0},
                                next.position}};
      }
      return hit(after(c.at, *letter), c.position);
    } else {
      return hit(after(c.at, letter), c.position);
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
/** @param letters a text of the caller's, as the letters' indices or as
 *  bytes read through @c rank(alphabet); a view, read where it is.
 *  @param m the stage.  @return the query of the positions where a word of
 *  the set ends: count it, take from it, collect it. */
export template <std::ranges::view V, std::size_t Sigma, std::size_t Q>
  requires std::ranges::random_access_range<V> && std::ranges::sized_range<V>
constexpr auto operator>>(V letters, const Matches<Sigma, Q>& m) {
  return unfold(detail_automaton::Cursor{},
                detail_automaton::MatchStep<Sigma, Q, V>{m.plan, m.holes,
                                                         std::move(letters)});
}

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
// position 7, the C of its GAATTC; twice in GAATTCGAATTC.
static_assert(count(std::views::all(std::array{3uz, 3uz, 2uz, 0uz, 0uz, 3uz,
                                               3uz, 1uz, 0uz, 0uz, 2uz, 1uz,
                                               3uz, 3uz, 0uz}) >>
                    matches(kmp_gaattc)) == 1 &&
              count(std::views::all(std::array{2uz, 0uz, 0uz, 3uz, 3uz, 1uz,
                                               2uz, 0uz, 0uz, 3uz, 3uz, 1uz}) >>
                    matches(kmp_gaattc)) == 2);
static_assert(std::ranges::equal(
    collect(std::views::all(std::array{3uz, 3uz, 2uz, 0uz, 0uz, 3uz, 3uz, 1uz,
                                       0uz, 0uz, 2uz, 1uz, 3uz, 3uz, 0uz}) >>
            matches(kmp_gaattc) >> take<1>()),
    std::array{7uz}));

}  // namespace dedekind::category
