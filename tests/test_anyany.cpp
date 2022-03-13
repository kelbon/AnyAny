
#include <array>
#include <functional>
#include <iostream>
#include <compare>
#include <ranges>
#include <algorithm>
#include <random>
#include <stop_token>
#include <list>

#include "anyany.hpp"

using namespace aa;

template<typename T>
struct Y {
  int value = 10;
  bool operator<(const Y&) const noexcept {
    return false;
  }
  bool operator>(const Y&) const noexcept {
    return false;
  }
  bool operator<=(const Y&) const noexcept {
    return false;
  }
  bool operator>=(const Y&) const noexcept {
    return false;
  }
  bool operator!=(const Y&) const noexcept {
    return false;
  }
  bool operator==(const Y&) const noexcept {
    return false;
  }
  auto operator<=>(const Y&) const noexcept = default;
};

void Test0() {
  static_assert(std::is_same_v<steal_qualifiers_from_t<int, double>, double> &&
                std::is_same_v<steal_qualifiers_from_t<const int, float>, const float> &&
                std::is_same_v<steal_qualifiers_from_t<const volatile int, char>, const volatile char> &&
                std::is_same_v<steal_qualifiers_from_t<const int&, char>, const char&> &&
                std::is_same_v<steal_qualifiers_from_t<int*&&, std::nullptr_t>, std::nullptr_t&&> &&
                std::is_same_v<steal_qualifiers_from_t<bool volatile&, int>, volatile int&> &&
                std::is_same_v<steal_qualifiers_from_t<int, const float>, float> &&
                std::is_same_v<steal_qualifiers_from_t<const int&&, double>, double const&&> &&
                std::is_same_v<steal_qualifiers_from_t<void const, float>, const float> &&
                std::is_same_v<steal_qualifiers_from_t<int&&, void>, void>);
}

 // any movable
struct any_movable : any<any_movable, move> {
  using any_movable::any::any;
};
// any copyable
struct any_copyable : any<any_copyable, copy, move, equal_to> {
  using any_copyable::any::any;
};

int leaked_resource_count = 0;

struct deleter_resource {
  void operator()(void*) {
    leaked_resource_count--;
  }
};
template<size_t Sz>
struct destroy_me {
  char padding[Sz];
  std::shared_ptr<void> ptr;
  destroy_me() : ptr(padding, deleter_resource{}) {
    leaked_resource_count++;
    std::ranges::fill(padding, 1);
  }
  bool operator==(const destroy_me&) const = default;

  void check() const {
    for (auto v : padding)
      if (v != 1)
        throw "Bad";
  }
  float foo() const {
    check();
    return 1.f;
  }
  std::string bar(int f, std::string s) {
    if (f != 3 || s != "hello world")
      throw "Bad";
    check();
    return "bar called";
  }
};
template <size_t Sz>
struct destroy_me_throw : destroy_me<Sz> {
  std::list<int> l; // lol it is throw move / copy constructible... Allocations on move / default init
};
struct nomove_any : any<nomove_any> {
  using nomove_any::any::any;
};
#define error_if(Cond) error_count += static_cast<size_t>((Cond));
size_t TestConstructors() {
  any_copyable ilist{std::in_place_type<std::vector<int>>, {1, 2, 3}};
  ilist.emplace<std::vector<int>>({1, 2, 3});
  // problems with emplaceing std::array(aggregate construct) because of construct_at
  constexpr auto Xy = [] {
  };
  // nomove nocopy construct
  nomove_any a0(std::in_place_type<std::stop_callback<decltype(Xy)>>, std::stop_token{}, Xy);
  a0.emplace<int>();
  a0.reset();

  size_t error_count = 0;
  std::mt19937 generator{std::random_device{}()};
  // MOVE ONLY
  {
    std::vector<any_movable> vec;
    vec.emplace_back(5);
    vec.emplace_back(std::make_unique<int>());
    vec.emplace_back("hello world");
    vec.emplace_back(std::vector<int>(10, 1));
    vec.emplace_back(std::array<std::byte, 4>{});
    vec.emplace_back(std::in_place_type<destroy_me<1>>);
    vec.emplace_back(std::in_place_type<destroy_me<10>>);
    vec.emplace_back(std::in_place_type<destroy_me<20>>);
    vec.emplace_back(std::in_place_type<destroy_me<30>>);
    vec.emplace_back(std::in_place_type<destroy_me<40>>);
    vec.emplace_back(std::in_place_type<destroy_me<50>>);
    vec.emplace_back(std::in_place_type<destroy_me<60>>);
    vec.emplace_back(std::in_place_type<destroy_me<70>>);
    vec.emplace_back(std::in_place_type<destroy_me<80>>);
    vec.emplace_back(std::in_place_type<destroy_me<90>>);
    vec.emplace_back(std::in_place_type<destroy_me<100>>);
    vec.emplace_back(std::in_place_type<destroy_me<110>>);
    vec.emplace_back(std::in_place_type<destroy_me<120>>);
    vec.emplace_back(std::make_unique<destroy_me<120>>());
    std::ranges::shuffle(vec, generator);
    std::vector<any_movable> storage;
    std::ranges::move(vec, std::back_inserter(storage));
  }
  {
    std::vector<any_movable> vec;
    vec.emplace_back(5);
    vec.emplace_back(std::make_unique<int>());
    vec.emplace_back("hello world");
    vec.emplace_back(std::vector<int>(10, 1));
    vec.emplace_back(std::array<std::byte, 4>{});
    vec.emplace_back(std::in_place_type<destroy_me_throw<1>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<10>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<20>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<30>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<40>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<50>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<60>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<70>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<80>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<90>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<100>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<110>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<120>>);
    vec.emplace_back(std::make_unique<destroy_me_throw<120>>());
    std::ranges::shuffle(vec, generator);
    std::vector<any_movable> storage;
    std::ranges::move(vec, std::back_inserter(storage));
  }
  // COPY + MOVE
  error_if(leaked_resource_count != 0);  // changed by destroy_me objects
  {
    std::vector<any_copyable> vec;
    vec.emplace_back(5);
    vec.emplace_back("hello world");
    vec.emplace_back(std::vector<int>(10, 1));
    vec.emplace_back(std::array<std::byte, 4>{});
    vec.emplace_back(std::in_place_type<destroy_me<1>>);
    vec.emplace_back(std::in_place_type<destroy_me<10>>);
    vec.emplace_back(std::in_place_type<destroy_me<20>>);
    vec.emplace_back(std::in_place_type<destroy_me<30>>);
    vec.emplace_back(std::in_place_type<destroy_me<40>>);
    vec.emplace_back(std::in_place_type<destroy_me<50>>);
    vec.emplace_back(std::in_place_type<destroy_me<60>>);
    vec.emplace_back(std::in_place_type<destroy_me<70>>);
    vec.emplace_back(std::in_place_type<destroy_me<80>>);
    vec.emplace_back(std::in_place_type<destroy_me<90>>);
    vec.emplace_back(std::in_place_type<destroy_me<100>>);
    vec.emplace_back(std::in_place_type<destroy_me<110>>);
    vec.emplace_back(std::in_place_type<destroy_me<120>>);
    std::ranges::shuffle(vec, generator);
    std::vector<any_copyable> storage;
    std::ranges::sample(vec, std::back_inserter(storage), vec.size() / 2, generator);
    storage[0].emplace<destroy_me<10>>();
    storage[1].emplace<destroy_me<20>>();
    storage[2].emplace<destroy_me<30>>();
    storage[3].emplace<destroy_me<40>>();
    storage[4].emplace<destroy_me<50>>();
    storage[5].emplace<destroy_me<60>>();
    storage[6].emplace<destroy_me<70>>();
    vec[0].emplace<destroy_me<10>>();
    vec[1].emplace<destroy_me<20>>();
    vec[2].emplace<destroy_me<30>>();
    vec[3].emplace<destroy_me<40>>();
    vec[4].emplace<destroy_me<50>>();
    vec[5].emplace<destroy_me<60>>();
    vec[6].emplace<destroy_me<70>>();
  }
  error_if(leaked_resource_count != 0);
  {
    std::vector<any_copyable> vec;
    vec.emplace_back(5);
    vec.emplace_back("hello world");
    vec.emplace_back(std::vector<int>(10, 1));
    vec.emplace_back(std::array<std::byte, 4>{});
    vec.emplace_back(std::in_place_type<destroy_me_throw<1>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<10>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<20>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<30>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<40>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<50>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<60>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<70>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<80>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<90>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<100>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<110>>);
    vec.emplace_back(std::in_place_type<destroy_me_throw<120>>);
    std::ranges::shuffle(vec, generator);
    std::vector<any_copyable> storage;
    std::ranges::sample(vec, std::back_inserter(storage), vec.size() / 2, generator);
    storage[0].emplace<destroy_me_throw<10>>();
    storage[1].emplace<destroy_me_throw<20>>();
    storage[2].emplace<destroy_me_throw<30>>();
    storage[3].emplace<destroy_me_throw<40>>();
    storage[4].emplace<destroy_me_throw<50>>();
    storage[5].emplace<destroy_me_throw<60>>();
    storage[6].emplace<destroy_me_throw<70>>();
    vec[0].emplace<destroy_me_throw<10>>();
    vec[1].emplace<destroy_me_throw<20>>();
    vec[2].emplace<destroy_me_throw<30>>();
    vec[3].emplace<destroy_me_throw<40>>();
    vec[4].emplace<destroy_me_throw<50>>();
    vec[5].emplace<destroy_me_throw<60>>();
    vec[6].emplace<destroy_me_throw<70>>();
  }
  error_if(leaked_resource_count != 0);
  return error_count;
}

struct any_compare : any<any_compare, copy, spaceship, move> {
  using any_compare::any::any;
};
struct any_equal : any<any_equal, equal_to, move> {
  using any_equal::any::any;
};

size_t TestCompare() {
  size_t error_count = 0;
  any_compare v1(Y<double>{});
  error_if((v1 <=> v1) != std::partial_ordering::equivalent);
  error_if(v1 != v1);
  any_compare v2 = std::vector{10, 5};
  error_if((v1 <=> v2) != std::partial_ordering::unordered);
  auto v3 = v2;
  any_cast<std::vector<int>>(&v3)->back() = 0;
  error_if(!(v3 < v2));
  any_equal v4 = 3.14f;
  error_if(v4 != 3.14f);
  error_if(3.14f != v4);
  return error_count;
}

struct FooAble {
  float foo() const {
    return 0;
  }
  std::string bar(int, std::string) {
    return "hello world";
  }
};

template <typename T>
struct foox {
  static float do_invoke(T self) {
    return self.foo();
  }
};

template<typename T>
struct barx {
  static std::string do_invoke(T* self, int&& f, std::string&& s) {
    return self->bar(f, s);
  }
};

struct any_fooable : any<any_fooable, copy, destroy, move, foox, barx> {
  using any_fooable::any::any;
};

size_t TestAnyCast() {
  size_t error_count = 0;
  any_fooable v0 = destroy_me<3>{};
  invoke<foox>(v0);
  error_if(any_cast<destroy_me<3>>(&v0) == nullptr);
  error_if(any_cast<destroy_me<4>>(&v0) != nullptr);
  try {
    auto i = any_cast<FooAble>(v0);
    (void)i;
  } catch (...) {
    return error_count;
  }
  error_if(true);
  return error_count;
}

size_t TestInvoke() {
  size_t error_count = 0;
  any_fooable f0(std::in_place_type<destroy_me<5>>);
  any_fooable f1(std::in_place_type<destroy_me<50>>);
  any_fooable f2(std::in_place_type<destroy_me<500>>);
  any_fooable f3(std::in_place_type<destroy_me<100>>);
  error_if(invoke<barx>(f0, 3, "hello world") != "bar called");
  error_if(invoke<barx>(f1, 3, "hello world") != "bar called");
  error_if(invoke<barx>(f2, 3, "hello world") != "bar called");
  error_if(invoke<barx>(f3, 3, "hello world") != "bar called");
  static_assert(const_method<foox>);
  static_assert(!const_method<barx>);
  error_if(invoke<foox>(std::as_const(f3)) != 1.f);
  f0 = f2 = f1 = f0 = f0 = f0 = std::move(f1) = f2 = f3 = any_fooable{} = f3 = std::move(f2) = f1 = f2 = f3 =
      f3 = f1 = f2;
  return error_count;
}

template <typename T>
struct Draw {
  static void do_invoke(T self, std::ostream& out, void*&& op) {
    self.draw(out, op);
  }
};

using any_drawable = any_with<Draw>;

struct Square {
  void draw(std::ostream& out, auto&&) const {
    out << "Square";
  }
};
struct Circle {
  void draw(std::ostream& out, auto&&) const {
    out << "Cirle";
  }
};

size_t TestJust(){
  std::vector<int> vec(10, 2);
  any_drawable d = Circle{};
  invoke<Draw>(d, std::cout, nullptr);
  any_with<Draw, copy, move> d0;
  d0 = Square{};
  invoke<Draw>(d0, std::cout, {});
  return 0;
}


int main() {
  srand(time(0));
  return TestConstructors() + TestAnyCast() + TestCompare() + TestInvoke() + TestJust();
}