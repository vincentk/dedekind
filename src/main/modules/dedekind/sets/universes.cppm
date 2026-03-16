module;  // Start of Global Module Fragment

#include <cmath>   // For std::isfinite (used in RealUniverse)
#include <limits>  // For numeric_limits (if you check for max/min)
#include <memory>  // For std::make_shared
#include <set>     // For std::set (used in ExtensionalSet)
#include <string>  // If you plan to support String universes
#include <vector>  // For returning elements()

export module dedekind.sets:universes;

import :traits;
// import std;

namespace dedekind::sets::universes {

// Hard-coded Cardinality Instances
export inline constexpr auto Card0 = Empty{};
export inline constexpr auto CardBool = Extensional{2};
export inline constexpr auto CardChar = Extensional{256};
export inline constexpr auto CardInt = Extensional{4294967296ULL};  // 2^32

// The base for all "Spaces"
export template <typename T, IsCardinality C>
struct Generic : public SetTrait<T, C> {
  bool contains(const T&) const override { return true; }
};

// Math Constants with hard-coded metadata
export inline const auto A = std::make_shared<Generic<char, Extensional>>();
export inline const auto B = std::make_shared<Generic<bool, Extensional>>();
export inline const auto N =
    std::make_shared<Generic<unsigned int, Countable>>();
export inline const auto Z = std::make_shared<Generic<int, Countable>>();
export inline const auto S =
    std::make_shared<Generic<std::string, Countable>>();
export inline const auto R = std::make_shared<Generic<double, Uncountable>>();

}  // namespace dedekind::sets::universes
