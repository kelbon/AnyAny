#pragma once

#include <vector>
#include <iostream>

#include <anyany.hpp>

///
/// This example shows how a basic usage of AnyAny, creating, invoking
///

template <typename T>
struct Draw {
    // op here just for example
  static void do_invoke(T self, std::ostream& out, void*&& op) {
    self.draw(out, op);
  }
};

using any_drawable = aa::any_with<Draw>;

struct Square {
  // Note: it is template method, but it also works
  // because any captures only one instantiation of this template method with [T = void*]
  template <typename T>
  void draw(std::ostream& out, T&&) const {
    out << "Square";
  }
  
  int x;
};
struct Circle {
  void draw(std::ostream& out, void*) const {
    out << "Cirle";
  }
  int x;
  std::string y;
};

// another way to write a drawable - with possibility to invoke methods internal way
struct any_drawable_v2 : aa::any<any_drawable_v2, Draw> {
    using any_drawable_v2::any::any;

    void draw(std::ostream& out, void*&& ptr) {
      aa::invoke<Draw>(*this, out, std::move(ptr));
    }
};

void example2() {
  any_drawable d = Circle{};
  any_drawable_v2 v2 = Circle{};
  v2.draw(std::cout, {}); // simple
  d.emplace<Square>(10);  // emplaces Square{10}
  d.emplace<Circle>(20, "hello world"); // emplaces Circle{20, "hello world"}
  aa::invoke<Draw>(d, std::cout, nullptr);
  // aa::copy / aa::move enables copy / move for your any
  aa::any_with<Draw, aa::copy, aa::move> d0;
  d0 = Square{};
  // invoke knows what type Draw accepts, so you can use {} and implicit conversations!
  aa::invoke_unsafe<Draw>(d0, std::cout, {});
  // If you sure that any containts value (like here)
  // it can be more effective to use aa::invoke_unsafe but if any do not contain a value it causes undefined
  // behaviour
}