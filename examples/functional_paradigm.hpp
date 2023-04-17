#pragma once

#include <iostream>
#include <functional>

#include <anyany.hpp>

///
/// This example implements all of type erase in C++ standard library before AnyAny
/// std::any
/// std::function
/// std::move_only_function (C++23)
/// std::function_ref (C++23)
/// Also function supports currying and invoking from tuple, as in functional languages
///

namespace aa::example {

template <typename, typename...>
struct select_function {};

template <typename R, typename... Args, typename... Methods>
struct select_function<R(Args...), Methods...> {
    using type = aa::any_with<aa::call<R(Args...)>, Methods...>;
};

template<typename Signature, typename... Methods>
using basic_function = typename select_function<Signature, Methods...>::type;

template <typename Signature>
using function = basic_function<Signature, aa::copy, aa::move>;

template <typename Signature>
using move_only_function = basic_function<Signature, aa::move>;

template <typename Signature>
using function_ref = typename function<Signature>::ref;

template <typename Signature>
using effective_function_ref = aa::stateful::cref<aa::call<Signature>>;

using any = aa::any_with<aa::copy, aa::move>;

void example1_foo(int x, float y, double z) {
  std::cout << x << y << z << '\n';
}

void example1() {
  auto* fn_ptr = &example1_foo;
  effective_function_ref<void(int, float, double) const> f1 = fn_ptr;
  static_assert(std::is_invocable_r_v<void, const decltype(fn_ptr)&, int, float, double>);
  f1(5, 5.f, 5.);
  function<void(int, float, double)> f0 = &example1_foo;

  f0 = [](int, float, double) {
    // some other function
  };
}

}  // namespace example

