module;

#include <concepts>
#include <ranges>
#include <type_traits>
#include <variant>

export module dedekind.sets:handle;

// C++23 feature, disabled for now:
// import std;

namespace dedekind::sets {

// The Variant: The "Box" that can hold any of the above
export using AnyCardinality =
    std::variant<Empty, Extensional, Finite, Countable, Uncountable>;

// Define the "Truth" of the bound on the variant itself
export constexpr std::optional<std::size_t> bound(const AnyCardinality& c) {
  return std::visit(
      [](auto&& arg) -> std::optional<std::size_t> {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_base_of_v<Extensional, T>) {
          return arg.bound;  // Every Extensional (and Empty) has a bound
        }
        return std::nullopt;
      },
      c);
}

// 1. The absolute base (no cardinality template)
export template <typename T>
struct SetTraitBase {
  virtual ~SetTraitBase() = default;
  virtual bool contains(const T& value) const = 0;
  virtual AnyCardinality get_cardinality() const = 0;
};

// 2. The Variant "Bridge" operator
export bool operator<=(const AnyCardinality& lhs, const AnyCardinality& rhs) {
  return std::visit(
      [](const auto& l, const auto& r) {
        // This is the "Magic" line.
        // std::visit finds the correct specific 'operator<=' from above
        // based on the types it found inside the boxes.
        return l <= r;
      },
      lhs, rhs);
}
}  // namespace dedekind::sets
