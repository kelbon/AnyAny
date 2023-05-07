#pragma once

namespace aa {

// used as placeholder for erased type in signature_type declarations
// or when library tries to get signature by instanciating 'do_invoke' with this type
using erased_self_t = int;

template <typename...>
struct type_list {};

constexpr inline size_t npos = size_t(-1);

}  // namespace aa

namespace aa::noexport {

template <typename Method, typename>
struct invoke_fn {
  static_assert((![] {}), "you forget to include anyany.hpp");
};

}  // namespace aa::noexport