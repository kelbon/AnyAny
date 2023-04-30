#pragma once

#include <iostream>
#include <functional>

#include <anyany/anyany.hpp>

///
/// This example implements all of type erase in C++ standard library before AnyAny
/// std::any
/// std::function
/// std::move_only_function (C++23)
/// std::function_ref (C++23)
/// Also function supports currying and invoking from tuple, as in functional languages
///

namespace example::details {

template <typename... Methods, typename R, typename... Args>
auto get_function_type(R (*)(Args...)) -> aa::any_with<aa::call<R(Args...)>, Methods...>;

}  // namespace example::details

namespace example {

template <typename Signature, typename... Methods>
using basic_function = decltype(details::get_function_type<Methods...>((Signature*)0));
// similar to std::function, but better
// * noexcept move
// * no huge RTTI
// * customizable alloc and small object optimization buffer size
// * support for other signatures like R(Args...) const/noexcept (aa::call Method)
// etc etc
template <typename Signature>
using function = basic_function<Signature, aa::copy, aa::move>;

// similar to C++23 move_only_function
template <typename Signature>
using move_only_function = basic_function<Signature, aa::move>;

// lightweight wrapper, stores pointer to operator() and const void* to value
// most effective way to erase function
template <typename Signature>
using function_ref = aa::stateful::cref<aa::call<Signature>>;

// similar to std::any, but better...
using any = aa::any_with<aa::copy>;

int example_foo(int x, float y, double z) {
  std::cout << x << y << z << '\n';
  return 0;
}

void use_functions() {
  // Note: function ref want to store pointer to value, so when you pass function into it
  // it needs pointer to pointer to function
  auto* fn_ptr = &example_foo;
  // example_foo returns 'int', but we still can construct f (ignores result, because returns 'void')

  function_ref<void(int, float, double) const> f_ref = fn_ptr;
  f_ref(5, 5.f, 5.);
  function<void(int, float, double)> foo = &example_foo;

  foo = [](int, float, double) {
    // some other function
  };
  foo(5, 5., 5.);
}

}  // namespace example

