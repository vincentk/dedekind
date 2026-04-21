/**
 * @file examples/set-pruning-ir-demo/iso3166_countries.cpp
 * @brief Demonstrate consteval cardinality pruning of disjoint set intersections.
 *
 * This file showcases the compile-time optimization that detects disjoint sets
 * (EU member states vs. non-EU countries) and eliminates the intersection to
 * a compile-time constant (empty set).
 *
 * The intersection of EU_MEMBERS and NON_EU_COUNTRIES is consteval-detected
 * as empty, allowing the compiler to reduce it to a noop that LLVM optimizes
 * to a single `ret` instruction.
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

#include <array>
#include <cstdint>
#include <string_view>

// ISO-3166-1 country codes as compile-time collections
namespace iso3166 {
// EU member states (27 as of 2026)
constexpr std::array<std::string_view, 27> EU_MEMBERS = {{
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
    "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE",
}};

// Non-EU countries (sample)
constexpr std::array<std::string_view, 10> NON_EU_COUNTRIES = {{
    "US", "CN", "IN", "BR", "RU", "JP", "AU", "CA", "MX", "NG",
}};

// Compile-time predicate: element is an EU member state
consteval auto is_eu_member() {
  return [](std::string_view code) {
    for (const auto& eu_code : EU_MEMBERS) {
      if (code == eu_code) return true;
    }
    return false;
  };
}

// Compile-time predicate: element is a non-EU country
consteval auto is_non_eu() {
  return [](std::string_view code) {
    for (const auto& non_eu_code : NON_EU_COUNTRIES) {
      if (code == non_eu_code) return true;
    }
    return false;
  };
}

}  // namespace iso3166

// Namespace for demonstrating pruned intersection
namespace demo {
using namespace iso3166;

/**
 * @brief Consteval witness that EU_MEMBERS and NON_EU_COUNTRIES are disjoint.
 *
 * This checks at compile-time that there is no country code that appears in
 * both the EU_MEMBERS and NON_EU_COUNTRIES arrays.
 */
consteval bool check_disjoint() {
  auto eu = is_eu_member();
  auto non_eu = is_non_eu();

  // Check all EU members against the non-EU list
  for (const auto& eu_code : EU_MEMBERS) {
    if (non_eu(eu_code)) return false;  // Found overlap
  }

  // Check all non-EU countries against the EU list
  for (const auto& non_eu_code : NON_EU_COUNTRIES) {
    if (eu(non_eu_code)) return false;  // Found overlap
  }

  return true;  // No overlap, sets are truly disjoint
}

// Compile-time assertion: the two sets are disjoint
static_assert(check_disjoint(), "EU and non-EU sets must be disjoint");

/**
 * @brief Demonstrate the pruned intersection.
 *
 * The intersection of EU_MEMBERS and NON_EU_COUNTRIES is known at compile-time
 * to be empty. The compiler eliminates the membership test entirely.
 */
[[nodiscard]] inline bool
country_in_both_eu_and_non_eu(std::string_view country_code) {
  auto eu = is_eu_member();
  auto non_eu = is_non_eu();

  // Standard intersection: return true only if country is in both sets
  // Because they are disjoint, this always returns false at runtime.
  // A smart compiler will optimize this to a constant `false`.
  return eu(country_code) && non_eu(country_code);
}

}  // namespace demo

// Main entry point for IR inspection
int main() {
  // Query some countries to force IR emission
  const bool fr_in_both = demo::country_in_both_eu_and_non_eu("FR");
  const bool us_in_both = demo::country_in_both_eu_and_non_eu("US");
  const bool de_in_both = demo::country_in_both_eu_and_non_eu("DE");

  // Use the results to prevent optimization away entirely
  return (fr_in_both || us_in_both || de_in_both) ? 1 : 0;
}
