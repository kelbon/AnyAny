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

template <typename Signature, template <typename> typename... Methods>
struct basic_function {};

template <typename R, typename Head, typename... Args, template <typename> typename... Methods>
struct basic_function<R(Head, Args...), Methods...>
    : aa::any_with<aa::call<R(Head, Args...)>::template method, Methods...> {
  using base_t = aa::any_with<aa::call<R(Head, Args...)>::template method, Methods...>;
  using base_t::base_t;
  using base_t::operator=;
  using base_t::operator();

  // support currying and invoking from tuple like in functional languages

  //  all functions just accepts a tuple of arguments if you think about it
  R operator()(std::tuple<Head, Args...> args_tpl) {
    return std::apply(
        [&](Head&& arg, Args&&... args) {
          return (*this)(static_cast<Head&&>(arg), static_cast<Args&&>(args)...);
        },
        std::move(args_tpl));
  }
  // exposition only (can be more effective with && versions of operator())
  [[nodiscard("currying!")]] auto operator()(Head value)
    requires(sizeof...(Args) > 0)
  {
    return [&]<typename... Tail>(aa::type_list<Tail...>) {
      return basic_function<R(Args...), Methods...>(std::bind_front(*this, static_cast<Head&&>(value)));
    }(aa::type_list<Head, Args...>{});
  }
};

template <typename Signature>
using function = basic_function<Signature, aa::copy, aa::move>;

template <typename Signature>
using move_only_function = basic_function<Signature, aa::move>;

template <typename Signature>
using function_ref = typename function<Signature>::ref;

using any = aa::any_with<aa::copy, aa::move>;

void example1_foo(int x, float y, double z) {
  std::cout << x << y << z << '\n';
}

void example1() {
  function<void(int, float, double)> f0 = example1_foo;
  f0({5, 5.f, 10.});
  f0(5, 5, 10);
  f0(5)(5, 10);
  f0(5)(5)(10);
  f0 = [](int, float, double) {
    // some other function
  };
}

}  // namespace example

