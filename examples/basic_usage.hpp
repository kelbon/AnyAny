#pragma once

#include <vector>
#include <iostream>

#include <anyany.hpp>

///
/// This example shows basic usage of AnyAny: creating, invoking
/// and optional features like explicit interface and plugins
///

// Firsly create an anyany Method (description how to invoke method for each type T)
template <typename T>
struct Draw {
  static void do_invoke(const T& self, std::ostream& out, int val) {
    self.draw(out, val);
  }
};

using any_drawable = aa::any_with<Draw>;

// some types with .draw( )
struct Circle {
  void draw(std::ostream& out, int val) const {
    out << val << "Cirle";
  }
  int x;
  std::string y;
};

struct Square {

  // Note: it is template method, but it also works
  // because any captures only one instantiation of this template method with [T = void*]
  template <typename T>
  void draw(std::ostream& out, T&&) const {
    out << "Square";
  }

  int x;
};

void example_draw() {
  any_drawable d = Circle{};
  d.emplace<Square>();  // event without copy / move methods you can emplace new type into Any 
  aa::invoke<Draw>(d, std::cout, 15);
  // aa::copy / aa::move enables copy / move for your any
  aa::any_with<Draw, aa::copy, aa::move> d0;
  // Note: it can be more effective to use ctor with std::in_place_type<T>!
  aa::any_with<Draw, aa::copy, aa::move> d1 = Square{};
  d0 = d1;
  d0 = std::move(d1);
  // invoke knows what type Draw accepts, so you can use {} and implicit conversations!
  aa::invoke_unsafe<Draw>(d0, std::cout, {});
  // If you sure that any containts value (like here)
  // it can be more effective to use aa::invoke_unsafe
  // but if any do not contain a value it causes undefined behaviour
}

/// 
/// Same example, but with explicit interface and plugin for inner call of method like Any.method(Args...)
/// 


// OR you can write a method with plugin(part of interface of result Any type)
// and requirement of explicit interface for types
template <typename T>
struct DrawExplicit {
  // optional, requires all type to explicitly write,
  // that they are satisfies this method(using satisfies = aa::satisfies<Draw>)
  using explicit_interface = void;

  // And you can add interface plugin, Any with this method will have methods from plugin
  template <typename CRTP>
  struct plugin {
    void draw(std::ostream& out, int val) const {
      aa::invoke<DrawExplicit>(*static_cast<const CRTP*>(this), out, val);
    }
  };

  // op here just for example
  static void do_invoke(T self, std::ostream& out, int op) {
    self.draw(out, op);
  }
};
// If method requires explicit interface you can specialize variable for type or write
// using satisfies = aa::satisfies<Method1, Method2...>; in type
namespace aa {

template <>
constexpr inline bool satisfies_v<Circle, DrawExplicit> = true;

template <>
constexpr inline bool satisfies_v<Square, DrawExplicit> = true;

}  // namespace aa

struct Triangle {
  // another way...
  using satisfies = aa::satisfies<DrawExplicit>;

  void draw(std::ostream& out, int val) const {
    out << val << "Triangle";
  }
};

using any_drawable_v2 = aa::any_with<DrawExplicit>;

void example_draw_explicit() {
  any_drawable_v2 v2 = Circle{};

  v2.draw(std::cout, 1);  // already has .draw method from plugin
  v2.emplace<Triangle>();
  aa::invoke<DrawExplicit>(v2, std::cout, 0);
}
