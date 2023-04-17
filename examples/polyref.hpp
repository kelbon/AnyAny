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

/*
Macros generates +- this code

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
*/
const_trait(print, void(), std::cout << self << std::endl);

// all arguments erased, so we dont create a print function for any
// set of types like in case with void print(auto&&... args) signature
// and only create a single erase for every type
void print_all(std::initializer_list<aa::cref<print>> l) {
  for (auto x : l)
    x.print();
}

using any_printable = aa::any_with<print>;
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

// stateful ref contains vtable in itself, so it is most effective way to type erase 1 Method
// 
void statefull_print(const aa::stateful::cref<print>& ref) {
  ref.print();
}
}  // namespace example

void example_polyref() {
  struct no_print {};
  // trait created from macro enables SFINAE friend construct
  AA_IF_HAS_CPP20(static_assert(!std::is_constructible_v<example::any_printable, no_print>);)
  example::any_printable value = std::string{"Im a polymorphic value"};
  example::may_be_print(&value);  // operator& of poly_ref returns poly_ptr
  print_one(*&value);             // operator* of poly_ptr returns poly_ref
  example::print_all({5, 10, std::string{"abc"}, std::string_view{"hello world"}});
}