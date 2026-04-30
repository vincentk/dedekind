/**
 * @file dedekind/morphologies/cyclic.cppm
 * @partition :cyclic
 * @brief Cyclic-morphology carriers and their concept witnesses.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 *
 * @section cyclic__Scope
 * Hosts the concrete cyclic-morphology carriers:
 *
 *   - @c CyclicRing<T, N>      — minimal int-backed
 * @f$\mathbb{Z}/N\mathbb{Z}@f$ experiment (kept for the existing test).
 *   - @c Modular<N>            — the canonical finite cyclic ring
 *                                @f$\mathbb{Z}/N\mathbb{Z}@f$ (relocated
 *                                from @c category:species; see #378).
 *
 * Both carriers expose the operational @c morphologies::IsCyclic
 * shape (@c Domain, @c generator(), @c successor()).  Only
 * @c Modular<N> is additionally registered with the axiomatic
 * @c dedekind::category::IsCyclicGroup<Modular<N>,
 * std::plus<Modular<N>>> via the trait specialisations below; the
 * complementary pairing is documented in @c morphologies:archimedean
 * alongside @c IsCyclic itself.
 *
 * @note "Matematik esas olarak sabır olayıdır. Belleyerek değil keşfederek
 * anlamak gerekir." — Cahit Arf, Turkish Wikiquote. [Trans: "Mathematics is
 * essentially a matter of patience. One must understand by discovering, not by
 * memorizing."]
 */
module;

#include <concepts>
#include <cstddef>      // for std::size_t (cyclic_order_v specialisation)
#include <functional>   // for std::plus, std::multiplies
#include <limits>       // for std::numeric_limits (cyclic_order guard)
#include <type_traits>  // for std::true_type, std::integral_constant

export module dedekind.morphologies:cyclic;

import dedekind.category;
import :archimedean;  // for IsCyclic / IsCyclicRing concepts (audit witnesses)

namespace dedekind::morphologies {

/**
 * @class CyclicRing
 * @brief Minimal int-backed @f$\mathbb{Z}/N\mathbb{Z}@f$ carrier from
 *        the experimental reintegration phase.  Retained for parity
 *        with the existing test; @c Modular<N> below is the broader
 *        (auto-typed N) replacement.
 */
export template <std::integral T, T N>
class CyclicRing {
 public:
  using machine_type = T;
  using Domain = T;

  constexpr explicit CyclicRing(T v) : value_(normalize(v)) {}

  static constexpr T successor(T a) { return normalize(a + 1); }
  static constexpr T generator() { return normalize(1); }

  constexpr operator T() const { return value_; }

  friend constexpr CyclicRing operator+(CyclicRing a, CyclicRing b) {
    return CyclicRing{static_cast<T>(a.value_ + b.value_)};
  }

  friend constexpr CyclicRing operator*(CyclicRing a, CyclicRing b) {
    return CyclicRing{static_cast<T>(a.value_ * b.value_)};
  }

 private:
  static constexpr T normalize(T v) {
    const T m = v % N;
    return m < 0 ? static_cast<T>(m + N) : m;
  }

  T value_;
};

/**
 * @section cyclic__The_Modular_Species
 * @brief @f$\mathbb{Z}/N\mathbb{Z}@f$ as a total finite cyclic ring.
 *
 * @details Relocated from @c dedekind.category:species into
 * @c dedekind.morphologies:cyclic to live alongside the @c IsCyclic
 * concept and its sibling carriers.  The type satisfies both
 * @c morphologies::IsCyclic (via the @c Domain / @c generator() /
 * @c successor() member API) and
 * @c dedekind::category::IsCyclicGroup<Modular<N>, std::plus<Modular<N>>>
 * (via the trait specialisations below).
 */
export template <auto N>
struct Modular {
  static_assert(std::integral<decltype(N)>,
                "Modulus must be of an integral type.");
  static_assert(N > 0, "Modulus must be positive.");
  using machine_type = decltype(N);
  using Domain = Modular;

  // Wide intermediate for arithmetic so that @c (a.value + b.value)
  // and @c (a.value * b.value) don't overflow @c machine_type before
  // the @c % N reduction.  Picks the wider of @c machine_type and
  // @c unsigned long long so all standard integral choices are
  // covered.
  using wide_type =
      std::conditional_t<(sizeof(machine_type) > sizeof(unsigned long long)),
                         machine_type, unsigned long long>;

  // The widened intermediate must itself fit @c (N-1) * (N-1) for
  // multiplication to be reduction-safe (addition's @c 2*(N-1) bound
  // is implied).  At @c sizeof(wide_type) == 8, that caps @c N at
  // @f$\lfloor\sqrt{2^{64}}\rfloor = 2^{32}@f$; the @c
  // static_assert below makes the limit a hard build-time error
  // rather than silent corruption for callers picking unusually
  // large moduli (e.g.\ @c Modular<unsigned long long, 2^{50}>).
  // Callers needing a wider modulus should host a 128-bit-backed
  // carrier of their own; this one keeps to the standard widths.
  static_assert(N == 1 || static_cast<wide_type>(N - 1) <=
                              std::numeric_limits<wide_type>::max() /
                                  static_cast<wide_type>(N - 1),
                "Modular<N>: (N-1)^2 must fit wide_type for safe "
                "modular multiplication; pick a smaller N or a "
                "wider machine_type.");

  machine_type value;

  // Constructor normalises into @f$[0, N)@f$.  For signed
  // @c machine_type, @c (-1 % N) yields @c -1 in C++; we shift the
  // result back into the canonical range so @c operator== respects
  // @f$\mathbb{Z}/N\mathbb{Z}@f$ semantics.
  explicit constexpr Modular(machine_type v) : value(normalize(v)) {}

  // morphologies::IsCyclic shape API: 1 generates Z/NZ; successor
  // walks the chain.
  static constexpr Modular generator() { return Modular(1); }
  static constexpr Modular successor(Modular a) { return a + Modular(1); }

  // Total Addition: (a + b) mod N --- widened intermediate avoids
  // overflow on narrow machine_type.
  constexpr friend Modular operator+(Modular a, Modular b) {
    const wide_type sum =
        static_cast<wide_type>(a.value) + static_cast<wide_type>(b.value);
    return Modular(static_cast<machine_type>(sum % static_cast<wide_type>(N)));
  }

  // Total Multiplication: (a * b) mod N --- widened intermediate
  // avoids overflow on narrow machine_type.
  constexpr friend Modular operator*(Modular a, Modular b) {
    const wide_type prod =
        static_cast<wide_type>(a.value) * static_cast<wide_type>(b.value);
    return Modular(static_cast<machine_type>(prod % static_cast<wide_type>(N)));
  }

  // Equality as a subobject classifier.
  constexpr friend bool operator==(Modular a, Modular b) {
    return a.value == b.value;
  }

 private:
  static constexpr machine_type normalize(machine_type v) {
    const machine_type m = v % N;
    if constexpr (std::signed_integral<machine_type>) {
      return m < 0 ? static_cast<machine_type>(m + N) : m;
    } else {
      return m;
    }
  }
};

}  // namespace dedekind::morphologies

namespace dedekind::category {

/** @section cyclic__Distributivity_and_Invertibility */

template <auto N>
inline constexpr bool is_distributive_v<dedekind::morphologies::Modular<N>,
                                        std::multiplies<>, std::plus<>> = true;

template <auto N>
inline constexpr bool
    is_distributive_v<dedekind::morphologies::Modular<N>,
                      std::multiplies<dedekind::morphologies::Modular<N>>,
                      std::plus<dedekind::morphologies::Modular<N>>> = true;

template <auto N>
inline constexpr bool
    is_invertible_v<dedekind::morphologies::Modular<N>,
                    std::plus<dedekind::morphologies::Modular<N>>> = true;

template <auto N>
inline constexpr bool
    is_invertible_v<dedekind::morphologies::Modular<N>, std::plus<>> = true;

/** @section cyclic__Symmetry_Axioms */

template <auto N>
struct is_associative<dedekind::morphologies::Modular<N>,
                      std::plus<dedekind::morphologies::Modular<N>>>
    : std::true_type {};

template <auto N>
struct is_commutative<dedekind::morphologies::Modular<N>,
                      std::plus<dedekind::morphologies::Modular<N>>>
    : std::true_type {};

template <auto N>
struct is_associative<dedekind::morphologies::Modular<N>,
                      std::multiplies<dedekind::morphologies::Modular<N>>>
    : std::true_type {};

template <auto N>
struct is_commutative<dedekind::morphologies::Modular<N>,
                      std::multiplies<dedekind::morphologies::Modular<N>>>
    : std::true_type {};

template <auto N>
struct is_periodic<dedekind::morphologies::Modular<N>,
                   std::plus<dedekind::morphologies::Modular<N>>>
    : std::true_type {};

template <auto N>
struct is_periodic<dedekind::morphologies::Modular<N>,
                   std::multiplies<dedekind::morphologies::Modular<N>>>
    : std::true_type {};

/** @section cyclic__Identities */

template <auto N>
struct identity_trait<dedekind::morphologies::Modular<N>,
                      std::plus<dedekind::morphologies::Modular<N>>> {
  using value_type = dedekind::morphologies::Modular<N>;
  static constexpr value_type value = value_type{0};
};

template <auto N>
struct identity_trait<dedekind::morphologies::Modular<N>,
                      std::multiplies<dedekind::morphologies::Modular<N>>> {
  using value_type = dedekind::morphologies::Modular<N>;
  static constexpr value_type value = value_type{1};
};

/** @section cyclic__Atlas_Registration: Modular<N> */

export template <auto N>
struct SpeciesTraits<dedekind::morphologies::Modular<N>> {
  using Domain = dedekind::morphologies::Modular<N>;
  using machine_type = decltype(N);

  /** @section cyclic__Algebraic_Facts */
  template <typename Op>
  static constexpr bool is_associative_v = true;

  template <typename Op>
  static constexpr bool is_commutative_v = true;

  // Both ops are idempotent ONLY if N=1 (trivial ring).
  template <typename Op>
  static constexpr bool is_idempotent_v = (N == 1);

  static constexpr Domain identity = Domain(0);

  static constexpr bool is_distributive = true;
};

/** @section cyclic__IsCyclicGroup_witness */

// Modular<N> under std::plus is the archetypal cyclic group
// @f$\mathbb{Z}/N\mathbb{Z}@f$, generated by 1.  Order is N.
template <auto N>
struct is_cyclic_group<dedekind::morphologies::Modular<N>,
                       std::plus<dedekind::morphologies::Modular<N>>>
    : std::true_type {};

// Cyclic order is N when N fits in std::size_t; otherwise we report
// 0 (the same sentinel used elsewhere for "finite but order not
// representable as std::size_t" --- see `cyclic_order` in :total).
template <auto N>
struct cyclic_order<dedekind::morphologies::Modular<N>,
                    std::plus<dedekind::morphologies::Modular<N>>>
    : std::integral_constant<std::size_t,
                             (N <= std::numeric_limits<std::size_t>::max())
                                 ? static_cast<std::size_t>(N)
                                 : std::size_t{0}> {};

}  // namespace dedekind::category

namespace dedekind::morphologies {

/** @section cyclic__Formal_Verification — main-source witnesses
 *
 *  These @c static_assert s travel with the carrier definitions so
 *  downstream modules see them immediately on import.  Tests in
 *  @c morphologies/modular_test.cpp duplicate the runtime / probe
 *  coverage but the structural claims live here.
 */

// Modular<N> as a finite cyclic ring Z/NZ.
static_assert(dedekind::category::IsRing<Modular<256>, std::plus<Modular<256>>,
                                         std::multiplies<Modular<256>>>,
              "Modular<256> must satisfy category::IsRing (Z/256Z).");

static_assert(
    dedekind::category::IsCommutativeRing<Modular<256>, std::plus<Modular<256>>,
                                          std::multiplies<Modular<256>>>,
    "Modular<256> must satisfy category::IsCommutativeRing.");

static_assert(
    dedekind::category::IsCyclicGroup<Modular<256>, std::plus<Modular<256>>>,
    "Modular<256> under + is the archetypal cyclic group Z/256Z.");

static_assert(
    dedekind::category::cyclic_order_v<Modular<256>, std::plus<Modular<256>>> ==
        256,
    "Modular<256>'s cyclic-group order is 256.");

// Modular<N> also satisfies the operational `morphologies::IsCyclic`
// shape concept via its `Domain` / `generator()` / `successor()`
// member API --- the canonical bridge between the operational and
// axiomatic cyclic concepts.
static_assert(IsCyclic<Modular<256>>,
              "Modular<256> exposes the morphologies::IsCyclic shape "
              "(Domain / generator() / successor()).");

// Peano-coherence witness for #388: the carrier-level
// `T::successor(x)` member API and the abstract Peano successor
// `S(x) = x + 1` agree on Modular<N>.  This pins the
// "operational successor == algebraic successor" claim made in
// the morphologies:archimedean Vocabulary_Notes block at the
// type level — without it, a future edit to Modular<N> could
// silently drift the two views apart.
static_assert(Modular<256>::successor(Modular<256>{42}) ==
                  (Modular<256>{42} + Modular<256>{1}),
              "Modular<N>::successor(x) must equal the Peano successor "
              "S(x) = x + 1 (member-API ↔ algebraic-axiom coherence).");
static_assert(Modular<256>::generator() == Modular<256>{1},
              "Modular<N>::generator() must be the multiplicative "
              "identity 1 — it is what S iterates to enumerate Z/NZ.");

// CyclicRing<T, N>: the experimental int-backed sibling.  Exercises
// the same operational shape concept on a carrier that does not
// otherwise plug into the categorical trait machinery.
static_assert(IsCyclic<CyclicRing<int, 100>>,
              "CyclicRing<int, 100> satisfies morphologies::IsCyclic.");
static_assert(IsCyclicRing<CyclicRing<int, 100>>,
              "CyclicRing<int, 100> satisfies morphologies::IsCyclicRing "
              "(IsCyclic + ring-shaped + and *).");

/** @section cyclic__CCC_Inheritance_389 CCC inheritance (#389)
 *
 * Both cyclic-morphology carriers can serve as the ambient species of
 * an ETCS-style set object; the canonical CCC over each carrier
 * (terminal @c One, products @c std::pair, exponentials @c
 * std::function) is therefore Cartesian-closed, and any @c IsSet<S>
 * built over these ambients inherits the CCC guarantee structurally
 * per #389.
 */
static_assert(dedekind::category::HasCanonicalSetCCC<Modular<256>>,
              "Modular<256> hosts a canonical Cartesian-closed Set ambient.");
static_assert(
    dedekind::category::HasCanonicalSetCCC<CyclicRing<int, 100>>,
    "CyclicRing<int, 100> hosts a canonical Cartesian-closed Set ambient.");

}  // namespace dedekind::morphologies
