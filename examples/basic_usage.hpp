#pragma once

#include <vector>
#include <iostream>

#include <anyany.hpp>

///
/// This example shows basic usage of AnyAny: creating, invoking
/// and optional features like explicit interface and plugins
///

// Firsly create an anyany Method (description how to invoke method for each type T)
struct Draw {
  template <typename T>
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
  // because any captures only one instantiation of this template method with [T = int&]
  template <typename T>
  void draw(std::ostream& out, T&&) const {
    out << "Square";
  }

  int x;
};

struct Triangle {
  void draw(std::ostream& out, int val) const {
    out << val << "Triangle";
  }
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
  aa::invoke<Draw>(d0, std::cout, {});
}
