#pragma once

#include <anyany/anyany_macro.hpp>
#include <anyany/anyany.hpp>

#include <iostream>

// This example demonstrates how to create some advanced Methods with custom interface
// * visit (polymorphic visitor that can visit any of the listed types)
// in virtual functions it will be something like:
// template<typename... Ts>
// struct visitor {
// 
// virtual void visit(Ts& x) = 0;
// ... etc for each type
// };
// And creating concrete such visitor usually is hard/errorphore and boilerplate
// 
// But in our example it will possible to create visitor from lambda, as for std::visit
// and even visit 2 values will be supported(you rly do not want to see how implement it with virtual functions)
//

namespace example {
// uses 'anyany_extern_method' for not creating useless for us default 'plugin'
template <typename T>
anyany_extern_method(visit1, (const& self, T& value) requires(self(value))->void);

}  // namespace example

namespace aa {

// specialize interface plugin for 'visit<T>'
// 'Any' here is resulting type, which created by aa::any_with/poly_ref... etc
template <typename Any, typename T>
struct plugin<Any, ::example::visit1<T>> {
  // 'friend' automatically adds function into overload set,
  // Note that two plugins with same name of functions
  // will shadow each other without additional manipulations like
  // using PluginA::foo, using PluginB::foo; (or friends like here)
  friend void visit(const Any& vtor, T& value) {
    aa::invoke<::example::visit1<T>>(vtor, value);
  }
};

}  //  namespace aa

namespace example {

// supports visit one argument
template <typename... Ts>
using any_visitor_for = aa::any_with<visit1<Ts>...>;

// some types, typical usage of such visitor - AST parsing, geometry etc
struct A {};
struct B {};
struct C {};
struct D {};
// accepts by 'cref',
// so 'apply_visitor([](auto x) { ...do smth... })' will work effectivelly without 'any_with' object creation
void apply_visitor(any_visitor_for<A, B, C, D>::cref visitor) {
  // uses ADL to find friend 'visit' function
  // it is possible to do as member function, but i prefer this
  A a;
  B b;
  C c;
  D d;
  visit(visitor, a);
  visit(visitor, b);
  visit(visitor, c);
  visit(visitor, d);
}

// SUPPORT FOR VISITING TWO VALUES

template <typename T, typename U>
anyany_extern_method(visit2, (const& self, T& v1, U& v2) requires(self(v1, v2))->void);

}  // namespace example

namespace aa {

template <typename Any, typename T, typename U>
struct plugin<Any, ::example::visit2<T, U>> {
  friend void visit(const Any& vtor, T& a, U& b) {
    invoke<::example::visit2<T, U>>(vtor, a, b);
  }
};

}  // namespace aa

namespace example {
// helper alias to all Methods which visit 'T' with second argument 'Ts'
template <typename T, typename... Ts>
using visit_T_alias = aa::interface_alias<visit2<T, Ts>...>;

// may visit 1 argument or 2 arguments as well
template <typename... Ts>
using any_visitor_for_1_or_2 =
    aa::insert_flatten_into<aa::any_with, visit1<Ts>..., visit_T_alias<Ts, Ts...>...>;

// pattern(must be in standard), see example here: https://en.cppreference.com/w/cpp/utility/variant/visit
template <typename... Ts>
struct matcher : Ts... {
  using Ts::operator()...;
};
template <typename... Ts>
matcher(Ts...) -> matcher<Ts...>;

void use_visitors() {
  // implicitly creates polymorphic visitor from lambda
  // prints types for A, B, C, D (because functon applies visitor to them)
  apply_visitor([](auto x) { std::cout << typeid(x).name() << std::endl; });
  // creates visitor with 'virtual' functions for 'int', 'float', 'double'
  // clang-format off
  any_visitor_for_1_or_2<A, B, C, D> vtor = matcher{
      // some pattern matching here
      [](B x)       { std::cout << "case when only B" << std::endl; },
      [](A, B)      { std::cout << "special case A B" << std::endl; },
      [](auto&&...) { std::cout << "default for all others" << std::endl; }
  };
  // clang-format on
  A a;
  B b;
  C c;
  D d;
  visit(vtor, a, b);  // prints 'special case A B'
  visit(vtor, b, b);  // prints 'default for all others'
  visit(vtor, b);     // prints 'case when only B'
  visit(vtor, d);     // prints 'default for all others'
  // ...etc...
  size_t arg_count = 0;
  // and you can easily capture something here
  vtor = [&](auto&&... args) { arg_count = sizeof...(args); };
  visit(vtor, c, c);
  if (arg_count != 2)
    throw "error";
}

}  // namespace example