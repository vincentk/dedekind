/**
 * @file examples/set-pruning-ir-demo/pruning_noop_vs_runtime_fixture.cpp
 * @brief Deterministic assembly fixture for compile-time pruning vs runtime path.
 *
 * This fixture intentionally avoids standard-library dependencies so it can be
 * compiled for a fixed target triple in CI and compared against a checked-in
 * assembly baseline for auditability.
 */

namespace fixture {

constexpr const char* EU_MEMBERS[] = {
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR",
    "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE",
};

constexpr const char* NON_EU_COUNTRIES[] = {
    "US", "CN", "IN", "BR", "RU", "JP", "AU", "CA", "MX", "NG",
};

constexpr unsigned EU_COUNT = sizeof(EU_MEMBERS) / sizeof(EU_MEMBERS[0]);
constexpr unsigned NON_EU_COUNT =
    sizeof(NON_EU_COUNTRIES) / sizeof(NON_EU_COUNTRIES[0]);

constexpr bool code_eq2(const char* a, const char* b) {
  return a[0] == b[0] && a[1] == b[1] && a[2] == '\0' && b[2] == '\0';
}

consteval bool in_list_consteval(const char* code, const char* const* list,
                                 unsigned count) {
  for (unsigned i = 0; i < count; ++i) {
    if (code_eq2(code, list[i])) {
      return true;
    }
  }
  return false;
}

constexpr bool in_list_runtime(const char* code, const char* const* list,
                               unsigned count) {
  for (unsigned i = 0; i < count; ++i) {
    if (code_eq2(code, list[i])) {
      return true;
    }
  }
  return false;
}

consteval bool sets_are_disjoint() {
  for (unsigned i = 0; i < EU_COUNT; ++i) {
    if (in_list_consteval(EU_MEMBERS[i], NON_EU_COUNTRIES, NON_EU_COUNT)) {
      return false;
    }
  }

  for (unsigned i = 0; i < NON_EU_COUNT; ++i) {
    if (in_list_consteval(NON_EU_COUNTRIES[i], EU_MEMBERS, EU_COUNT)) {
      return false;
    }
  }

  return true;
}

static_assert(sets_are_disjoint(), "Fixture sets must remain disjoint.");

}  // namespace fixture

extern "C" __attribute__((noinline)) bool
pruning_compile_time_noop(const char* code) {
  (void)code;
  if constexpr (fixture::sets_are_disjoint()) {
    return false;
  }

  return fixture::in_list_runtime(code, fixture::EU_MEMBERS, fixture::EU_COUNT) &&
         fixture::in_list_runtime(code, fixture::NON_EU_COUNTRIES,
                                  fixture::NON_EU_COUNT);
}

extern "C" __attribute__((noinline)) bool
pruning_runtime_guard(const char* code, const char* const* runtime_codes,
                      unsigned runtime_count) {
  const bool in_eu =
      fixture::in_list_runtime(code, fixture::EU_MEMBERS, fixture::EU_COUNT);
  const bool in_dynamic =
      fixture::in_list_runtime(code, runtime_codes, runtime_count);
  return in_eu && in_dynamic;
}
