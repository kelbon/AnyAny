#pragma once

#include <vector>
#include <iostream>

#include <anyany/anyany.hpp>

///
/// This example shows basic usage of AnyAny:
///     * what is Methods and how to create them
///     * creating, invoking
///

namespace example {

// Firsly create an anyany Method (description how to invoke method for each type T)
struct Draw {
  template <typename T>
  static void do_invoke(const T& self, std::ostream& out) {
    self.draw(out);
  }
};

using any_drawable = aa::any_with<Draw>;

// some types with .draw()
struct Circle {
  std::string field;

  void draw(std::ostream& out) const {
    out << "Cirle";
  }
};

struct Square {
  int field;

  void draw(std::ostream& out) const {
    out << "Square";
  }
};

void use_drawables() {
  any_drawable shape;
  assert(shape.has_value() == false);
  shape.emplace<Square>();
  assert(shape.has_value());
  aa::invoke<Draw>(shape, std::cout);  // prints "Square"
  // aa::copy makes any_with<..copy..> copyable and movable
  aa::any_with<Draw, aa::copy> copyable_shape;
  // aa::move enables only move
  aa::any_with<Draw, aa::move> move_only_shape = Square{};
  // "use" values
  (void)copyable_shape, (void)move_only_shape;
}
/*
Notes:
    * even without copy / move Methods any.emplace<T>(...) will work
    * when you create a Method like:

        template <typename T>
        static void do_invoke(const T& self, std::ostream& out, int value) {
            // Note: there are possible template .draw Method,
            // or overloaded/invoking with implicit conversison
            // it is not a problem, but may be not what you expect
            self.draw(out, value);
        }

    * it can be more effective to use constructors with std::in_place_type<T>
    * aa::invoke<Method> knows types of arguments which Method accepts,
      this allows implicit conversations/construction like

        aa::invoke<Draw>(shape, std::cout, {})
*/
}  // namespace example

// this header contains only anyany_method/anyany_extern_method macros
// and serves for easy-creating Methods.
// NOTE: You can use it even without #include anyany.hpp
#include <anyany/anyany_macro.hpp>

// same example as above, but better with macro
namespace example::advanced {
// 'requires' works even in C++17, it is macro syntax, not language feature
// logicaly Method is a runtime requirement for type and any_with<Methods...> is a runtime concept
anyany_method(draw, (const& self, std::ostream& out) requires(self.draw(out))->void);

using any_drawable = aa::any_with<draw>;

void use_drawables() {
  advanced::any_drawable shape;
  assert(shape.has_value() == false);
  shape.emplace<Square>();
  assert(shape.has_value());
  // macro creates internal Method too
  shape.draw(std::cout);  // prints "Square"
  // Note: macro enables sfinae friendly constructors,
  // while primitive 'example::Draw' method is not sfinae friendly
  static_assert(!std::is_constructible_v<aa::any_with<advanced::draw>, int>);
  static_assert(std::is_constructible_v<aa::any_with<example::Draw>, int>);
}

}  // namespace example::advanced
