#pragma once

#include <anyany.hpp>
#include <algorithm>

///
/// This example shows how to use aa::poly_ref / ptr
/// polymorphic references - always not null, easy to copy and create
/// (can be created from value with required methods)
/// polymorphic pointer - may be null, easy to copy and create
/// can be created from pointer to value with required methods
/// OR from pointer to type created by any_with<...>
/// 
namespace example {

template <typename T>
struct Print {
  static void do_invoke(const T& self) {
    std::cout << self << std::endl;
  }
  template <typename CRTP>
  struct plugin {
    void print() const {
      aa::invoke<Print>(*static_cast<const CRTP*>(this));
    }
  };
};
// all arguments erased, so we dont create a print function for any
// set of types like in case with void print(auto&&... args) signature
// and only create a single erase for every type
void print(std::initializer_list<aa::const_poly_ref<Print>> l) {
  // aa::invoke<Method> is a functional object with operator()
  // .with method binds arguments and returns invocable
  std::ranges::for_each(l, aa::invoke<Print>);
}

using any_printable = aa::any_with<Print>;
// all types created by any_with<Methods...>
// have inner aliases ptr/ref/const_ptr/const_ref
// which are aa::polymorphic_ptr/ref
void print_one(any_printable::cref p) {
  p.print();
}
void may_be_print(any_printable::cptr p) {
  if (p != nullptr)
    p->print();
}

}  // namespace example

void example_polyref() {
  example::any_printable value = std::string{"Im a polymorphic value"};
  may_be_print(&value); // operator& returns polymorphic_ptr / const_polymorphic_ptr
  print_one(*&value);   // operator* of polymorphic_ptr returns polymorphic_ref
  example::print({5, 10, std::string{"abc"}, std::string_view{"hello world"}});
}