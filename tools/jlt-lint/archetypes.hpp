// Archetypes for jlt-lint's type check: stand-in types with exactly what a
// shape promises and nothing more (Siek and Lumsdaine's concept archetypes,
// as Boost's concept_archetype.hpp shipped them for the standard's concepts
// of its day).  The linter generates, for a constrained template, the
// archetypes of the shapes in its signature from these building blocks and
// the shapes' own laws and associated types, instantiates the template at
// them, and compiles with -fsyntax-only: a definition that type-checks there
// is admitted by type, one that does not is not.
//
// Lineage, with credit: the archetypes of Boost's Concept Check Library were
// written by hand (Siek and Lumsdaine, "Concept Checking: Binding Parametric
// Polymorphism in C++", 2000); ConceptGCC generated them for the concepts
// proposed for C++0x (Gregor et al., OOPSLA 2006); this linter generates
// them for the concepts of C++20, inside the Jlt fragment.  The standard's
// foundational concepts, std::equality_comparable, std::semiregular and
// std::regular, come from the Palo Alto report (Stroustrup and Sutton, eds.,
// "A Concept Design for the STL", WG21 N3351, 2012, Section 3.3), after
// Stepanov and McJones, "Elements of Programming" (2009), whose default
// constructor makes an object partially formed: it may be assigned or
// destroyed, not read.  That is why a default value here is a derived form,
// a local written before it is read, and not a value of the kernel.
//
// Part of the linter, shipped with it; it depends on nothing in dedekind.
// The functions are declared and never defined: the check type-checks, it
// does not link or run.
//
// A first cut, tried on the paper's four listings (the archetype spike in
// src/test/cpp/modules/dedekind/category/jlt_listings_test.cpp); the
// static_asserts at the end are its self-check.
#pragma once

#include <concepts>
#include <cstddef>
#include <iterator>
#include <ranges>

namespace jlt::archetype {

/// A carrier: a type that is copyable and nothing else.  No default value
/// and no operations: a value of it arises only from a call a shape names,
/// as the paper's kernel has it.  Tag keeps two carriers of one template
/// apart, so a body that mixes them up does not type-check.
template <typename Tag>
struct carrier {
  carrier() = delete;
  carrier(const carrier&) = default;
  carrier(carrier&&) = default;
  carrier& operator=(const carrier&) = default;
  carrier& operator=(carrier&&) = default;
  ~carrier() = default;
};

/// A carrier a shape asks equality of as well (std::equality_comparable):
/// copyable, ==, and nothing else.
template <typename Tag>
struct comparable_carrier {
  comparable_carrier() = delete;
  comparable_carrier(const comparable_carrier&) = default;
  comparable_carrier(comparable_carrier&&) = default;
  comparable_carrier& operator=(const comparable_carrier&) = default;
  comparable_carrier& operator=(comparable_carrier&&) = default;
  ~comparable_carrier() = default;
  bool operator==(const comparable_carrier&) const;
};

/// A carrier a shape asks a default value of as well (std::semiregular,
/// std::default_initializable): copyable and default-constructible.  The value
/// is partially formed in Stepanov's sense, to be written before it is read,
/// which a type cannot say: Table 7's derived form, local state.
template <typename Tag>
struct semiregular_carrier {
  semiregular_carrier() = default;
  semiregular_carrier(const semiregular_carrier&) = default;
  semiregular_carrier(semiregular_carrier&&) = default;
  semiregular_carrier& operator=(const semiregular_carrier&) = default;
  semiregular_carrier& operator=(semiregular_carrier&&) = default;
  ~semiregular_carrier() = default;
};

/// std::regular: semiregular and ==, nothing else.
template <typename Tag>
struct regular_carrier {
  regular_carrier() = default;
  regular_carrier(const regular_carrier&) = default;
  regular_carrier(regular_carrier&&) = default;
  regular_carrier& operator=(const regular_carrier&) = default;
  regular_carrier& operator=(regular_carrier&&) = default;
  ~regular_carrier() = default;
  bool operator==(const regular_carrier&) const;
};

/// A function object whose call gives a C: a monoid named by its unit.
template <typename C>
struct nullary {
  C operator()() const;
};

/// A result a shape asks only to be convertible to T (-> convertible_to<T>):
/// it converts, from the fresh result and nothing else, so a body that
/// compiles at it converts the result on the spot and uses a T from then on,
/// as if the shape had said same_as<T>.  No copy, no move: a named variable
/// of it is good for nothing.  After Boost's convertible_to_archetype (Siek
/// and Lumsdaine), with the conversion restricted to the fresh value.
template <typename T>
struct converts_to {
  converts_to(const converts_to&) = delete;
  converts_to& operator=(const converts_to&) = delete;
  operator T() &&;
};

/// A function object C x C -> R: an operation, its result C or, where the
/// shape asks only convertibility, converts_to<C>.
template <typename C, typename R = C>
struct binary {
  R operator()(C, C) const;
};

/// A function object A -> B: a homomorphism, or any required call of one
/// argument.
template <typename A, typename B>
struct arrow {
  B operator()(A) const;
};

/// A range whose size is known before the first step, with elements of type
/// C and nothing else: the archetype of the standard's sized_range, which
/// cannot be read off the standard library's own definition.
template <typename C>
struct sized_range_of {
  struct iterator {
    using iterator_concept = std::input_iterator_tag;
    using value_type = C;
    using difference_type = std::ptrdiff_t;
    C operator*() const;
    iterator& operator++();
    iterator operator++(int);
    bool operator==(const iterator&) const;
  };
  iterator begin() const;
  iterator end() const;
  std::size_t size() const;
};

/// A range with elements of type C and no size: the archetype of the
/// standard's input_range and range, so a body that asks for a size it was
/// not promised does not type-check.
template <typename C>
struct input_range_of {
  struct iterator {
    using iterator_concept = std::input_iterator_tag;
    using value_type = C;
    using difference_type = std::ptrdiff_t;
    C operator*() const;
    iterator& operator++();
    void operator++(int);
  };
  struct sentinel {
    friend bool operator==(const iterator&, const sentinel&);
  };
  iterator begin() const;
  sentinel end() const;
};

/// The integer types a template's constraints admit, at their extremes: the
/// standard's integer types by rank, and the first and the last of them for
/// which Fits<T>::value holds (void if none).  A template over an integer
/// parameter is checked at both: the smallest, where the integer promotions
/// bite (bool + bool is an int), and the largest, where nothing narrows.
template <template <class> class Fits, class... Ts>
struct first_fitting {
  using type = void;
};
template <template <class> class Fits, class T, class... Ts>
struct first_fitting<Fits, T, Ts...> {
  using type = std::conditional_t<Fits<T>::value, T,
                                  typename first_fitting<Fits, Ts...>::type>;
};
/// Every one of the standard's arithmetic types, the integer types (the
/// character types included) and the floating-point ones, passed to
/// Check<T>::value: the template is checked at each type its constraints
/// admit, since the types between the extremes can differ too
/// (std::countr_zero at char16_t; an overload at bool and one at unsigned
/// long long, ambiguous at unsigned).
template <template <class> class Check>
inline constexpr bool every_arithmetic =
    (Check<bool>::value && Check<char>::value && Check<signed char>::value &&
     Check<unsigned char>::value && Check<wchar_t>::value &&
     Check<char8_t>::value && Check<char16_t>::value &&
     Check<char32_t>::value && Check<short>::value &&
     Check<unsigned short>::value && Check<int>::value &&
     Check<unsigned int>::value && Check<long>::value &&
     Check<unsigned long>::value && Check<long long>::value &&
     Check<unsigned long long>::value && Check<float>::value &&
     Check<double>::value && Check<long double>::value);
template <template <class> class Fits>
using smallest_integer =
    typename first_fitting<Fits, bool, char, signed char, unsigned char,
                           char8_t, short, unsigned short, char16_t, wchar_t,
                           int, unsigned int, char32_t, long, unsigned long,
                           long long, unsigned long long, float, double,
                           long double>::type;
template <template <class> class Fits>
using largest_integer =
    typename first_fitting<Fits, long double, double, float, unsigned long long,
                           long long, unsigned long, long, char32_t,
                           unsigned int, int, wchar_t, char16_t, unsigned short,
                           short, char8_t, unsigned char, signed char, char,
                           bool>::type;

namespace self_check {
struct tag;
using c = carrier<tag>;
static_assert(std::copyable<c> && !std::default_initializable<c> &&
              !std::equality_comparable<c>);
static_assert(std::copyable<comparable_carrier<tag>> &&
              std::equality_comparable<comparable_carrier<tag>>);
static_assert(std::semiregular<semiregular_carrier<tag>> &&
              !std::regular<semiregular_carrier<tag>>);
static_assert(std::regular<regular_carrier<tag>> &&
              !std::totally_ordered<regular_carrier<tag>>);
static_assert(std::same_as<std::invoke_result_t<nullary<c>>, c>);
static_assert(std::same_as<std::invoke_result_t<binary<c>, c, c>, c>);
static_assert(std::convertible_to<converts_to<c>, c> &&
              !std::copy_constructible<converts_to<c>> &&
              !std::convertible_to<converts_to<c>&, c>);
static_assert(std::ranges::sized_range<sized_range_of<c>> &&
              std::ranges::input_range<sized_range_of<c>> &&
              std::same_as<std::ranges::range_value_t<sized_range_of<c>>, c>);
static_assert(std::ranges::input_range<input_range_of<c>> &&
              !std::ranges::sized_range<input_range_of<c>>);
template <class T>
struct is_count
    : std::bool_constant<std::unsigned_integral<T> && !std::same_as<T, bool>> {
};
template <class T>
struct is_char16 : std::bool_constant<std::same_as<T, char16_t>> {};
static_assert(std::same_as<smallest_integer<is_char16>, char16_t> &&
              std::same_as<largest_integer<is_char16>, char16_t>);
static_assert(std::same_as<smallest_integer<is_count>,
                           std::conditional_t<std::is_unsigned_v<char>, char,
                                              unsigned char>> &&
              std::same_as<largest_integer<is_count>, unsigned long long>);
}  // namespace self_check

}  // namespace jlt::archetype
