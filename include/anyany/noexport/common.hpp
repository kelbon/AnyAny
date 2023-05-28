#pragma once

namespace aa {

// used as placeholder for erased type in signature_type declarations
// or when library tries to get signature by instanciating 'do_invoke' with this type
using erased_self_t = int;

template <typename... Types>
struct type_list {
  template <typename... Types2>
  constexpr auto operator+(type_list<Types2...>) const -> type_list<Types..., Types2...> {
    return {};
  }
};

}  // namespace aa

namespace aa::noexport {

template <typename Method, typename>
struct invoke_fn {
  static_assert((![] {}), "you forget to include anyany.hpp");
};

}  // namespace aa::noexport