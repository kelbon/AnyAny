#pragma once

#include <iostream>
#include <functional>

#include <anyany.hpp>

///
/// This example implements all of type erase in C++ standard library before AnyAny
/// std::any
/// std::function
/// std::move_only_function(C++23)
/// Also function supports currying and invoking from tuple, as in functional languages
///

// quite a big of meta magic
struct notype {};

template<typename... Args>
struct first : std::type_identity<notype> {
};
template<typename First, typename... Args>
struct first<First, Args...> : std::type_identity<First> {};

template<typename... Args>
using first_t = typename first<Args...>::type;

namespace example {

// Creating an anyany Method for callable with needed signature
template<typename>
struct call;

template<typename R, typename... Args>
struct call<R(Args...)> {
  template <typename T>
  struct method {
    static R do_invoke(T& self, Args... args) {
      return self(static_cast<Args&&>(args)...);
    }
  };
};

template <typename Signature, template <typename> typename... Methods>
struct basic_function;

template <typename R, typename... Args, template <typename> typename... Methods>
struct basic_function<R(Args...), Methods...>
    : aa::any<basic_function<R(Args...), Methods...>, call<R(Args...)>::template method, Methods...> {
  using basic_function::any::any;

  R operator()(Args... args) {
    return aa::invoke<call<R(Args...)>::template method>(*this, static_cast<Args&&>(args)...);
  }
  // all functions just accepts a tuple of arguments if you think about it
  R operator()(std::tuple<Args...> args_tpl) {
    return std::apply([&](Args&&... args) { return (*this)(static_cast<Args&&>(args)...); },
                      std::move(args_tpl));
  }
  // exposition only (can be more effective with && versions of operator())
  [[nodiscard("currying!")]] auto operator()(first_t<Args...> value) requires(sizeof...(Args) > 1) {
    return [&]<typename Head, typename... Tail>(aa::type_list<Head, Tail...>) {
      return basic_function<R(Tail...), Methods...>(
          std::bind_front(*this, static_cast<first_t<Args...>&&>(value)));
    }
    (aa::type_list<Args...>{});
  }
};

template <typename Signature>
using function = basic_function<Signature, aa::copy, aa::move>;

template <typename Signature>
using move_only_function = basic_function<Signature, aa::move>;

using any = aa::any_with<aa::copy, aa::move>;

}  // namespace example

void foo(int x, float y, double z) {
  std::cout << x << y << z << '\n';
}
void example1() {
  example::function<void(int, float, double)> f0 = foo;
  f0({5, 5.f, 10.});
  f0(5, 5, 10);
  f0(5)(5, 10);
  f0(5)(5)(10);
}
