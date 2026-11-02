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

import :chain;     // IsCountWord
import :functor;   // Maybe
import :lattice;   // Join: a union as the lattice's node
import :periodic;  // AtMost, shift
import :runs;      // RunList, run, reduced, narrowed

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
/** @tparam Sigma the base.  @param k the exponent.  @return
 *  @f$\sigma^k@f$. */
template <std::size_t Sigma, IsCountWord N>
constexpr N power_of(N k) {
  return std::ranges::fold_left(std::views::repeat(static_cast<N>(Sigma), k),
                                N{1}, std::multiplies<N>{});
}
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
  const N b = power_of<Sigma>(static_cast<N>(j - 1));
  const N lo = static_cast<N>(a * b);
  return shift(lo) >> reduced(r & run<K>(lo, static_cast<N>(lo + b)));
}
/** @brief One pending length of a state's derivative by a letter: the
 *  pending words one longer, and the set's own where they are its length,
 *  derived and joined; a word of length zero is a match. */
template <std::size_t Sigma, IsCountWord N, std::size_t P, std::size_t K,
          std::size_t KL>
struct Advance {
  Residual<N, P, K> from;
  RunList<N, KL> sites;
  N length;
  N letter;
  constexpr Maybe<Residual<N, P, K>> operator()(
      const Maybe<Residual<N, P, K>>& acc, N k) const {
    if (!acc) return acc;
    const N j = static_cast<N>(k + 1);
    const RunList<N, KL> own = j == length ? sites : RunList<N, KL>{};
    const auto longer = derived<Sigma>(from.pending[k], j, letter);
    const auto mine = derived<Sigma>(own, j, letter);
    const auto r =
        reduced(Join<decltype(longer), decltype(mine)>{longer, mine});
    Residual<N, P, K> next = *acc;
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
constexpr Maybe<Residual<N, P, K>> step(const Residual<N, P, K>& s,
                                        const RunList<N, KL>& sites, N length,
                                        N a) {
  return std::ranges::fold_left(
      std::views::iota(N{0}, length),
      Maybe<Residual<N, P, K>>{Residual<N, P, K>{}},
      Advance<Sigma, N, P, K, KL>{s, sites, length, a});
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
  RunList<N, KL> sites;
  N length;
  using B = Build<Sigma, N, P, K, Q>;
  constexpr Maybe<B> operator()(const Maybe<B>& acc, N a) const {
    if (!acc) return acc;
    const Maybe<Residual<N, P, K>> t = step<Sigma>(from, sites, length, a);
    if (!t) return {};
    B next = *acc;
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
  RunList<N, KL> sites;
  N length;
  using B = Build<Sigma, N, P, K, Q>;
  constexpr Maybe<B> operator()(const Maybe<B>& acc, std::size_t i) const {
    if (!acc || i >= acc->states.size()) return acc;
    const Residual<N, P, K> s = acc->states.items[i];
    const Maybe<B> done = std::ranges::fold_left(
        std::views::iota(N{0}, static_cast<N>(Sigma)), acc,
        Letter<Sigma, N, P, K, KL, Q>{s, sites, length});
    if (!done) return {};
    B next = *done;
    next.plan = {next.plan.next.with(next.row),
                 next.plan.accepting.with(s.accepting)};
    return next;
  }
};
/** @brief One letter of a scan: the state after it, and the matches so far
 *  with one more where it accepts. */
template <std::size_t Sigma, std::size_t Q>
struct Scan {
  Automaton<Sigma, Q> plan;
  struct State {
    std::uint16_t at{0};
    std::size_t matches{0};
  };
  template <IsCountWord N>
  constexpr State operator()(const State& s, N letter) const {
    const std::uint16_t to = plan.next.items[s.at][letter];
    return {to, s.matches + (plan.accepting.items[to] ? 1uz : 0uz)};
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
  const Maybe<B> built = std::ranges::fold_left(
      std::views::iota(0uz, Q), Maybe<B>{start},
      detail_automaton::Walk<Sigma, N, P, K, KL, Q>{sites, length});
  if (!built) return {};
  return built->plan;
}

/** @param plan a matcher.  @param letters a text, as the letters' indices.
 *  @return how many times a word of the plan's set ends in it. */
export template <std::size_t Sigma, std::size_t Q,
                 std::ranges::input_range Letters>
constexpr std::size_t scan(const Automaton<Sigma, Q>& plan,
                           const Letters& letters) {
  return std::ranges::fold_left(
             letters, typename detail_automaton::Scan<Sigma, Q>::State{},
             detail_automaton::Scan<Sigma, Q>{plan})
      .matches;
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
// The scan counts the site where it ends: once in TTGAATTCAAGCTTA, twice in
// GAATTCGAATTC.
static_assert(scan(kmp_gaattc,
                   std::array{3uz, 3uz, 2uz, 0uz, 0uz, 3uz, 3uz, 1uz, 0uz, 0uz,
                              2uz, 1uz, 3uz, 3uz, 0uz}) == 1 &&
              scan(kmp_gaattc, std::array{2uz, 0uz, 0uz, 3uz, 3uz, 1uz, 2uz,
                                          0uz, 0uz, 3uz, 3uz, 1uz}) == 2);

}  // namespace dedekind::category
