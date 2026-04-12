module;

export module dedekind.numbers:constants;

import :real;

namespace dedekind::numbers {

export constexpr auto Sqrt2() { return Real<double>{1.41421356237}; }
export constexpr auto E() { return Real<double>{2.71828182846}; }
export constexpr auto Pi() { return Real<double>{3.14159265359}; }

}  // namespace dedekind::numbers
