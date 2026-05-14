#include <catch2/catch_test_macros.hpp>
import dedekind.category;
import dedekind.sets;

using namespace dedekind::category;
using namespace dedekind::sets;

TEST_CASE("Sets: Singleton Final Proof: The Highway",
          "[sets][singleton][highway]") {
  SECTION("1. The Singleton Lifting") {
    auto atom = singleton(42);
    REQUIRE(atom(42) == true);
  }

  SECTION("2. The Pull from the Identity (ε)") {
    auto atom = singleton(42);
    int value = atom.origin();

    REQUIRE(value == 42);
    static_assert(singleton(7).origin() == 7, "The Round-trip Axiom.");
  }
}

TEST_CASE("Sets: Singleton Acceptance", "[sets][singleton][acceptance]") {
  auto _s = ι<size_t>(42);
  STATIC_REQUIRE(IsSet<decltype(_s)>);
  SECTION("Construction") {
    INFO("Succesful membership test.");
    REQUIRE(_s(42));
    INFO("Failed membership test.");
    REQUIRE(!_s(4));
  }
  SECTION("Cardinality") {
    REQUIRE(_s.size() == 1);
    REQUIRE(_s.cardinality() == Finite{});
    STATIC_REQUIRE(
        std::same_as<typename decltype(_s)::cardinality_type, Finite>);
  }
  SECTION("Complement") {
    STATIC_REQUIRE(IsSet<decltype(!_s)>);
    INFO("Inverted membership test vis-a-vis base set.");
    REQUIRE((!(!_s))(42));
    REQUIRE((!_s)(4));
    INFO("Complement is its own inverse.");
    // @c HasSetSurface (the @c :sets ergonomic wrapper around @c IsSet)
    // is ref-decay-safe; @c decltype(!(!_s)) is @c SingletonSet const&
    // post-involution and the strict @c :etcs::IsSet would not fire on
    // a reference type without manual @c std::remove_cvref_t.
    STATIC_REQUIRE(HasSetSurface<decltype(!(!_s))>);
    REQUIRE(&(!(!_s)) == &_s);
  }
  SECTION("Intersections") {
    // FIXME(#685): Boolean-algebra-of-sets identities not yet encoded
    // structurally at the DSL surface.  Each assertion below names a
    // textbook law that today fails to compile:
    //   * `_s & _s` returns @c Comprehension<UniversalSet, lambda>,
    //     not @c SingletonSet — missing semantic-equality overload.
    //   * `&(_s & _s) == &_s` asks for structural pointer-identity on
    //     self-meet — `operator&` would need to return a reference
    //     branch (mirroring the `Complement` involution pattern).
    //   * `Complement<S> & SingletonSet<S>` lacks a cross-type overload
    //     and no equality with `Ø<T>{}` exists today.
    INFO("The intersection of a set with itself is a fixed point.");
    REQUIRE((_s & _s).size() == 1);
    // REQUIRE((_s & _s) == _s);
    // REQUIRE(&(_s & _s) == &_s);
    INFO("The intersection of a set with its complement is empty.");
    // REQUIRE((!_s) & _s == Ø<size_t>{});
    // REQUIRE(Ø<size_t>{} == (!_s) & _s);
  }
  SECTION("Union") {
    // FIXME(#685): mirror of the SECTION("Intersections") gaps above,
    // with `|` (union) in place of `&` (intersection) and
    // `UniversalSet<T>{}` in place of `Ø<T>{}`.  Same underlying
    // structural-identity / cross-type-overload / equality-matrix
    // surgery needed.
    INFO("The union of a set with itself is a fixed point.");
    REQUIRE((_s | _s).size() == 1);
    // REQUIRE((_s | _s) == _s);
    // REQUIRE(&(_s | _s) == &_s);
    INFO("The union of a set with its complement is the universal set.");
    // REQUIRE((!_s) | _s == UniversalSet<size_t>{});
    // REQUIRE(UniversalSet<size_t>{} == (!_s) | _s);
  }
  SECTION("Difference") {
    // FIXME(#685): set-difference operator `-` not defined on the
    // user-facing set surface today.  Canonical reduction is
    // `A - B = A & !B`, so this slice is downstream of the other
    // lattice-of-sets work (cross-type meets, equality matrix).
    INFO("Variations on {x} - Ø = id.");
    // REQUIRE(_s - Ø<size_t>{} == _s);
    // REQUIRE(Ø<size_t>{} - _s == Ø<size_t>{});
    // REQUIRE(_s - _s == Ø<size_t>{});
    INFO("Variations on {x} - !{x} = Ø.");
    // NOTE: `_s - !_s` reads `_s ∩ !!_s = _s ∩ _s = _s`,
    // not `Ø` — likely a copy-paste from the `_s - _s` line.
    // REQUIRE(_s - !_s == Ø<size_t>{});
    // REQUIRE(_s - _s == Ø<size_t>{});
  }
  SECTION("Subset relations") {
    // FIXME(#685): three independent gaps blocking subset assertions:
    //   * `_s <= _s`: SingletonSet's `auto operator<=>(const
    //     SingletonSet&) const = delete` shadows the template
    //     `operator<=(const S&)` for same-type self-comparison;
    //     overload resolution picks the deleted spaceship first.
    //   * `_s <= Ø`, `Ø <= _s`, `_s <= !_s`: Ø and Complement lack
    //     `.contains(v)`, which the SingletonSet template `<=`
    //     delegates to.
    //   * Catch2 `REQUIRE` rejects chained comparisons
    //     (`a <= b == false`); wrap as `(a <= b) == false`.
    // REQUIRE(_s <= _s);
    // REQUIRE((_s <= Ø<size_t>{}) == false);
    // REQUIRE((Ø<size_t>{} <= _s) == true);
    // REQUIRE((_s <= !_s) == false);
  }
  SECTION("Cartesian Product") {
    // FIXME(#685): three layered gaps:
    //   * `_s * _s` resolves through the generic ambient-species
    //     `operator*` to `Set<pair, L, lambda>` (a predicate set)
    //     which lacks `.size()` for the same structural reason
    //     `Comprehension` did pre-singleton-bounded patch.
    //   * `_s * Ø == Ø`, `Ø * _s == Ø`: empty-annihilation needs
    //     either a `cartesian_product(SingletonSet, Ø)` overload
    //     short-circuiting to `Ø<pair, L>`, or cross-type equality
    //     between predicate-`Set` and `Ø`.
    //   * `_s * !_s`: `Complement<SingletonSet>` doesn't satisfy
    //     the existing `operator*` overloads' constraints.
    // NOTE: user-side, the last line `REQUIRE(_s * !_s).size() > 1);`
    // has unbalanced parens — likely `REQUIRE((_s * !_s).size() > 1);`.
    // REQUIRE((_s * _s).size() == 1);
    // REQUIRE((_s * Ø<size_t>{}) == Ø<size_t>{});
    // REQUIRE((Ø<size_t>{} * _s) == Ø<size_t>{});
    // REQUIRE((_s * !_s).size() > 1);
  }
}

/*
TEST_CASE("Sets: Composition of Operations: The Functor Highway",
          "[sets][composition][monad]") {

  SECTION("1. Forward Monadic Composition (Bind)") {
    // Define our atomic operations as lambda predicates/transformers
    auto plus_one = [](int x) { return x + 1; };
    auto times_two = [](int x) { return x * 2; };

    // Start with a value, lift it into a SingletonSet,
    // and compose the operations using the monadic "Push" (>>=)
    // Math: η(5) >>= (x -> η(x + 1)) >>= (x -> η(x * 2))
    auto result_set = 5 >> into<SingletonSet>
                        >>= [plus_one](int x) { return plus_one(x) >>
into<SingletonSet>; }
                        >>= [times_two](int x) { return times_two(x) >>
into<SingletonSet>; };

    // The result should be a SingletonSet containing (5 + 1) * 2 = 12
    REQUIRE(result_set(12) == true);
    REQUIRE(result_set(6) == false);
  }

  SECTION("2. Compile-Time Semantic Mapping of Composition") {
    // Validating the "Zero-Overhead" claim from Page 1
    // The compiler should resolve the composition to a constant 12
    static_assert((5 >> into<SingletonSet>
                     >>= [](int x) { return (x + 1) >> into<SingletonSet>; }
                     >>= [](int x) { return (x * 2) >> into<SingletonSet>; }
                     << extract<SingletonSet>) == 12,
                  "The Composition Axiom must be resolved at compile-time.");
  }

}
                    */
