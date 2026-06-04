#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <vector>

import dedekind.sequences;
import dedekind.topology;
import dedekind.category;

using namespace dedekind::sequences;
using namespace dedekind::topology;
using namespace dedekind::category;

TEST_CASE("Sequences: The Path to Continuity",
          "[sequences][topology][limits]") {
  using ℤ = int;

  // A divergent integer path: s_n = n + 42
  auto s_n = [](std::size_t n) -> ℤ { return static_cast<ℤ>(n) + 42; };
  Path<ℤ> path{s_n};

  SECTION("Axiomatic Proofs") { static_assert(IsSequence<decltype(path)>); }

  SECTION("Sampling") {
    REQUIRE(path.at(0) == 42);
    REQUIRE(path.at(5) == 47);
  }

  SECTION("Comonadic Extension (Contextual Sampling)") {
    auto diffs = path <<=
        [](const Path<ℤ>& ctx) { return ctx.at(0) - ctx.at(1); };

    REQUIRE(diffs.at(0) == -1);
  }

  SECTION("Finite prefixes are first-class sequences") {
    const auto first_four = prefix(path, 4);

    static_assert(IsFiniteSequence<decltype(first_four)>);
    REQUIRE(first_four.size() == 4u);
    REQUIRE(first_four.at(0) == 42);
    REQUIRE(first_four.at(3) == 45);
  }

  SECTION("Drop yields an infinite shifted tail") {
    const auto tail = drop(path, 5);

    static_assert(IsSequence<decltype(tail)>);
    static_assert(!IsFiniteSequence<decltype(tail)>);
    REQUIRE(tail.at(0) == path.at(5));
    REQUIRE(tail.at(3) == path.at(8));
  }

  SECTION("Drop identity at zero offset") {
    const auto same = drop(path, 0);

    REQUIRE(same.at(0) == path.at(0));
    REQUIRE(same.at(7) == path.at(7));
  }

  SECTION("Prefix of drop forms a shifted finite window") {
    const auto window = prefix(drop(path, 2), 4);

    static_assert(IsFiniteSequence<decltype(window)>);
    REQUIRE(window.size() == 4u);
    REQUIRE(window.at(0) == 44);
    REQUIRE(window.at(3) == 47);
  }

  SECTION("Iterative rules support finite orbit counting") {
    const auto orbit = iterate(1, [](int x) { return x + 1; }, 5);

    static_assert(IsFiniteSequence<decltype(orbit)>);
    REQUIRE(orbit.size() == 5u);
    REQUIRE(orbit.at(0) == 1);
    REQUIRE(orbit.at(4) == 5);
    REQUIRE(count_if(orbit, [](int x) { return x > 3; }) == 2u);
  }

  SECTION("Finite iterate materializes once and avoids replayed stepping") {
    std::size_t step_calls = 0;
    const auto orbit = iterate(
        1,
        [&step_calls](int x) {
          ++step_calls;
          return x + 1;
        },
        6);

    // Finite iterate should precompute exactly length-1 transitions.
    REQUIRE(step_calls == 5u);

    // Accessing/counted scans over the finite path must not trigger new steps.
    REQUIRE(orbit.at(0) == 1);
    REQUIRE(orbit.at(5) == 6);
    REQUIRE(count_if(orbit, [](int x) { return x >= 4; }) == 3u);
    REQUIRE(step_calls == 5u);
  }

  SECTION("Finite paths interoperate with std::ranges algorithms") {
    const auto orbit = iterate(2, [](int x) { return x + 2; }, 5);

    static_assert(std::ranges::random_access_range<decltype(orbit)>);

    std::vector<int> visited;
    for (const int value : orbit) visited.push_back(value);

    REQUIRE(visited == std::vector<int>{2, 4, 6, 8, 10});
    REQUIRE(std::ranges::count_if(orbit, [](int x) { return x >= 6; }) == 3);
  }

  SECTION("from_range and as_range adapt finite paths") {
    const std::vector<int> source{1, 2, 3, 4};
    const auto doubled =
        from_range(source | std::views::transform([](int x) { return x * 2; }));

    REQUIRE(doubled.size() == source.size());
    REQUIRE(
        std::ranges::equal(as_range(doubled), std::vector<int>{2, 4, 6, 8}));
  }

  SECTION("scan: running size equals index + 1") {
    // scan(size, path)(i) == prefix(path, i+1).size() == i+1
    const auto sizes =
        scan([](const FinitePath<ℤ>& p) { return p.size(); }, path);

    static_assert(IsSequence<decltype(sizes)>);
    REQUIRE(sizes.at(0) == 1u);
    REQUIRE(sizes.at(4) == 5u);
    REQUIRE(sizes.at(9) == 10u);
  }

  SECTION("scan: running sum of natural numbers") {
    // path(i) = i  =>  scan(sum)(i) = 0 + 1 + ... + i = i*(i+1)/2
    Path<ℤ> naturals{[](std::size_t n) { return static_cast<ℤ>(n); }};
    const auto running_sum = scan(
        [](const FinitePath<ℤ>& p) {
          ℤ s = 0;
          for (std::size_t k = 0; k < p.size(); ++k) s += p.at(k);
          return s;
        },
        naturals);

    REQUIRE(running_sum.at(0) == 0);   // prefix [0]: sum = 0
    REQUIRE(running_sum.at(3) == 6);   // prefix [0,1,2,3]: sum = 6
    REQUIRE(running_sum.at(4) == 10);  // prefix [0..4]: sum = 10
  }

  SECTION("scan: exists() over a threshold — finds absorbing element") {
    // path(i) = i+42; threshold > 45 first holds at i=4 (path(4)=46)
    const auto hit = scan(
        [](const FinitePath<ℤ>& p) {
          return exists(p, [](ℤ x) { return x > 45; });
        },
        path);

    REQUIRE(hit.at(3) == false);  // prefix [42,43,44,45]: none > 45
    REQUIRE(hit.at(4) == true);   // prefix [42..46]: 46 > 45
    REQUIRE(hit.at(9) == true);   // absorbing: remains true
  }

  // Pointwise function-space operators on Path<T> (#537 slice 1).
  // These exercise the +, -, unary -, and scalar * overloads added so
  // Path<T> participates in the IsFunctionSpace<·, std::size_t, T>
  // concept.
  SECTION("Path<T>: pointwise + / - / unary - on infinite cardinality") {
    Path<ℤ> a{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n); }};
    Path<ℤ> b{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n) * 10; }};

    auto sum = a + b;
    REQUIRE(sum.at(0) == 0);
    REQUIRE(sum.at(3) == 33);  // 3 + 30
    REQUIRE(sum.at(7) == 77);  // 7 + 70

    auto diff = b - a;
    REQUIRE(diff.at(0) == 0);
    REQUIRE(diff.at(3) == 27);  // 30 - 3
    REQUIRE(diff.at(7) == 63);  // 70 - 7

    auto neg = -a;
    REQUIRE(neg.at(0) == 0);
    REQUIRE(neg.at(5) == -5);
  }

  SECTION("Path<T>: scalar multiplication, both sides") {
    Path<ℤ> a{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n) + 1; }};

    auto left = ℤ{3} * a;
    REQUIRE(left.at(0) == 3);   // 3 * 1
    REQUIRE(left.at(4) == 15);  // 3 * 5

    auto right = a * ℤ{4};
    REQUIRE(right.at(0) == 4);   // 1 * 4
    REQUIRE(right.at(2) == 12);  // 3 * 4
  }

  SECTION("FinitePath<T>: + extent uses min(a.size(), b.size())") {
    FinitePath<ℤ> a{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n); },
                    /*size=*/5};
    FinitePath<ℤ> b{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n) * 100; },
                    /*size=*/3};

    auto sum = a + b;
    REQUIRE(sum.size() == 3);  // min(5, 3)
    REQUIRE(sum.at(0) == 0);
    REQUIRE(sum.at(1) == 101);
    REQUIRE(sum.at(2) == 202);
  }

  SECTION("FinitePath<T>: empty operand on + gives empty path") {
    FinitePath<ℤ> a{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n); },
                    /*size=*/4};
    FinitePath<ℤ> empty{[](std::size_t) -> ℤ { return 0; }, /*size=*/0};

    auto sum = a + empty;
    REQUIRE(sum.size() == 0);  // min(4, 0) = 0; empty operand absorbs.
  }

  SECTION("FinitePath<T>: binary - extent uses min(a.size(), b.size())") {
    FinitePath<ℤ> a{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n) * 100; },
                    /*size=*/3};
    FinitePath<ℤ> b{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n); },
                    /*size=*/5};

    auto diff = a - b;
    REQUIRE(diff.size() == 3);  // min(3, 5)
    REQUIRE(diff.at(0) == 0);
    REQUIRE(diff.at(1) == 99);   // 100 - 1
    REQUIRE(diff.at(2) == 198);  // 200 - 2
  }

  SECTION("FinitePath<T>: unary - preserves extent") {
    FinitePath<ℤ> a{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n); },
                    /*size=*/4};

    auto neg = -a;
    REQUIRE(neg.size() == 4);  // unary - preserves a.size()
    REQUIRE(neg.at(0) == 0);
    REQUIRE(neg.at(3) == -3);
  }

  SECTION("FinitePath<T>: scalar * preserves extent (left and right)") {
    FinitePath<ℤ> a{[](std::size_t n) -> ℤ { return static_cast<ℤ>(n) + 1; },
                    /*size=*/3};

    auto left = ℤ{5} * a;
    REQUIRE(left.size() == 3);  // scalar * preserves a.size()
    REQUIRE(left.at(0) == 5);
    REQUIRE(left.at(2) == 15);

    auto right = a * ℤ{2};
    REQUIRE(right.size() == 3);  // a * scalar preserves a.size()
    REQUIRE(right.at(0) == 2);
    REQUIRE(right.at(2) == 6);
  }
}

TEST_CASE("n-ary iterate(op, seeds...): the recurrence primitive",
          "[sequences][path][iterate][nary]") {
  // Captures "binary op applied recursively to a sliding window" as a
  // single named primitive.  Fibonacci is the canonical n=2 + plus
  // instance; Tribonacci is n=3; n=1 reduces to the standard NNO
  // morphism (in argument-swapped form vs the existing iterate(seed,
  // step)).
  using std::size_t;

  SECTION("n=2, op=plus: Fibonacci 0, 1, 1, 2, 3, 5, 8, 13, 21, 34") {
    const auto fib = iterate(std::plus<size_t>{}, size_t{0}, size_t{1});
    static_assert(IsSequence<decltype(fib)>);
    REQUIRE(fib.at(0) == 0u);
    REQUIRE(fib.at(1) == 1u);
    REQUIRE(fib.at(2) == 1u);
    REQUIRE(fib.at(3) == 2u);
    REQUIRE(fib.at(4) == 3u);
    REQUIRE(fib.at(5) == 5u);
    REQUIRE(fib.at(6) == 8u);
    REQUIRE(fib.at(7) == 13u);
    REQUIRE(fib.at(8) == 21u);
    REQUIRE(fib.at(9) == 34u);
  }

  SECTION("n=3: Tribonacci 0, 0, 1, 1, 2, 4, 7, 13, 24") {
    const auto tri =
        iterate([](size_t a, size_t b, size_t c) { return a + b + c; },
                size_t{0}, size_t{0}, size_t{1});
    REQUIRE(tri.at(0) == 0u);
    REQUIRE(tri.at(1) == 0u);
    REQUIRE(tri.at(2) == 1u);
    REQUIRE(tri.at(3) == 1u);
    REQUIRE(tri.at(4) == 2u);
    REQUIRE(tri.at(5) == 4u);
    REQUIRE(tri.at(6) == 7u);
    REQUIRE(tri.at(7) == 13u);
    REQUIRE(tri.at(8) == 24u);
  }

  SECTION("n=2, Pell's recurrence P_{n+1} = 2 P_n + P_{n-1}") {
    // P_0=0, P_1=1, P_2=2, P_3=5, P_4=12, P_5=29, P_6=70
    const auto pell = iterate([](size_t a, size_t b) { return 2 * b + a; },
                              size_t{0}, size_t{1});
    REQUIRE(pell.at(0) == 0u);
    REQUIRE(pell.at(1) == 1u);
    REQUIRE(pell.at(2) == 2u);
    REQUIRE(pell.at(3) == 5u);
    REQUIRE(pell.at(4) == 12u);
    REQUIRE(pell.at(5) == 29u);
    REQUIRE(pell.at(6) == 70u);
  }

  SECTION(
      "n=1 reduces to the unary NNO morphism (arg-swapped vs binary form)") {
    // iterate(succ, 0) is the n=1 form of the n-ary overload; it should
    // produce the same canonical sequence 0, 1, 2, 3, ... as the binary
    // iterate(0, succ) form.
    const auto naturals = iterate([](size_t n) { return n + 1; }, size_t{0});
    REQUIRE(naturals.at(0) == 0u);
    REQUIRE(naturals.at(1) == 1u);
    REQUIRE(naturals.at(5) == 5u);
    REQUIRE(naturals.at(42) == 42u);
  }

  SECTION("Op deduction works without explicit type parameter") {
    // The carrier T deduces from the seeds; no explicit <T> ceremony at
    // the call site (the page-2 paper §3 claim).
    const auto fib_int = iterate(std::plus<int>{}, 0, 1);
    REQUIRE(fib_int.at(7) == 13);
    REQUIRE(fib_int.at(9) == 34);
  }
}
