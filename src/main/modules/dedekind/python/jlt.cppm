/**
 * @file dedekind/python/jlt.cppm
 * @partition :jlt
 * @brief The arrow every Python extension holds (#961).
 *
 * @copyright 2026 The Dedekind Authors
 * Licensed under the Apache License, Version 2.0.
 */

module;

#include <functional>

export module dedekind.python:jlt;

import dedekind.category;

export namespace dedekind::python {

/** @brief The type-erased arrow @c T→T: the real @c :morphism @c Morphism
 *  with a @c std::function transform.  This IS a category arrow (@c IsArrow /
 *  @c IsEndomorphism hold), not a bespoke wrapper.  @tparam T the carrier. */
template <typename T>
using Arrow = dedekind::category::Morphism<T, T, std::function<T(T)>>;

}  // namespace dedekind::python
