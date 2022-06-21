
#include <array>
#include <functional>
#include <iostream>
#include <compare>
#include <ranges>
#include <algorithm>
#include <random>
#include <stop_token>
#include <list>
#include <unordered_set>
#include <memory_resource>

#include "functional_paradigm.hpp" // example 0
#include "basic_usage.hpp" // example 1
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

 // any movable
template<typename Alloc = std::allocator<std::byte>>
struct any_movable : basic_any<any_movable<Alloc>, Alloc, aa::default_any_soos, destroy, move> {
  using any_movable::basic_any::basic_any;
};

// any copyable
template<typename Alloc = std::allocator<char>>
struct any_copyable : basic_any<any_copyable<Alloc>, Alloc, aa::default_any_soos, destroy, copy_with<Alloc, aa::default_any_soos>::template method, move, equal_to> {
  using any_copyable::basic_any::basic_any;
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

using nomove_any = aa::any_with<>;

#define error_if(Cond) error_count += static_cast<bool>((Cond));
size_t TestConstructors() {
  any_copyable<> ilist{std::in_place_type<std::vector<int>>, {1, 2, 3}};
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
    std::vector<any_movable<>> vec;
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
    std::vector<any_movable<>> storage;
    std::ranges::move(vec, std::back_inserter(storage));
  }
  {
    std::vector<any_movable<>> vec;
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
    std::vector<any_movable<>> storage;
    std::ranges::move(vec, std::back_inserter(storage));
  }
  // COPY + MOVE
  error_if(leaked_resource_count != 0);  // changed by destroy_me objects
  {
    std::vector<any_copyable<>> vec;
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
    std::vector<any_copyable<>> storage;
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
    std::vector<any_copyable<>> vec;
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
    std::vector<any_copyable<>> storage;
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
  {
    std::pmr::vector<any_copyable<std::pmr::polymorphic_allocator<std::byte>>> vec(
        std::pmr::new_delete_resource());
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
    std::pmr::vector<any_copyable<std::pmr::polymorphic_allocator<std::byte>>> storage(
        std::pmr::new_delete_resource());
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

using any_compare = aa::any_with<aa::copy, aa::spaceship, aa::move>;

using any_equal = aa::any_with<aa::equal_to, aa::move>;

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
  template<typename CRTP>
  struct plugin {
    float foo() const {
      return aa::invoke<foox>(static_cast<const CRTP&>(*this));
    }
  };
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

using any_fooable = aa::any_with<aa::copy, aa::move, foox, barx>;

size_t TestAnyCast() {
  size_t error_count = 0;
  any_fooable v0 = destroy_me<3>{};
  v0.foo();
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
  (void)f0.foo();
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

size_t TestCasts() {
  size_t error_count = 0;
  any_copyable<> cp;
  error_if(aa::any_cast<int>(&cp) != nullptr);
  cp = 5;
  error_if(aa::any_cast<int>(cp) != 5);
  const volatile int i = 5;
  cp = i;
  error_if(aa::any_cast<const int>(cp) != 5);
  std::vector<int> vec(10, 3);
  cp = vec;
  error_if(aa::any_cast<std::vector<int>>(cp) != vec);
  error_if(aa::any_cast<std::vector<int>>(std::move(cp)) != vec);
  error_if(aa::any_cast<std::vector<int>>(cp) != std::vector<int>{});
  {
    any_copyable<> acop = 10;
    auto& ref = aa::any_cast<int&>(acop);
    ref = 15;
    auto& ref2 = aa::any_cast<const int&>(acop);
    int val = aa::any_cast<int>(acop);
    (void)ref2, (void)val;
  }
  {
    const any_copyable<> acop = 10;
    //auto& ref = aa::any_cast<int&>(acop); CE because of const
    auto& ref2 = aa::any_cast<const int&>(acop);
    int val = aa::any_cast<int>(acop);
    (void)ref2, (void)val;
  }
  return error_count;
}

using any_hashable = aa::any_with<aa::hash, aa::equal_to, aa::copy, aa::move>;

// allocates every time (SOO == 0)
struct xyz : aa::basic_any<xyz, std::allocator<std::byte>, 0, aa::copy_with<std::allocator<std::byte>, 0>::method, aa::move, aa::destroy, aa::equal_to> {
    using xyz::basic_any::basic_any;
};

// EXAMPLE WITH POLYMORPHIC_PTR
template<typename T>
struct Drawi {
  static int do_invoke(const T& self, int val) {
    return self.draw(val);
  }

  template<typename CRTP>
  struct plugin {
    int draw(int val) const noexcept {
      return aa::invoke<Drawi>(*static_cast<const CRTP*>(this), val);
    }
  };
};

using idrawable = aa::any_with<Drawi, aa::copy, aa::move>;

struct drawable0 {
  int draw(int val) const {
    std::cout << val;
    return val;
  }
};

struct drawable1 {
  int draw(int val) const {
    std::cout << 2 * val;
    return 2 * val;
  }
};
void Foobar(idrawable::ptr v) {
  //std::cout << v->draw(150);
  std::cout << aa::invoke_unsafe<Drawi>(*v, 150);
}
void Foobar(idrawable::const_ptr v) {
  std::cout << v->draw(150);
  std::cout << aa::invoke_unsafe<Drawi>(*v, 150);
}

int main() {
  Circle v0;
  aa::polymorphic_ptr<Draw> polyptr = &v0;
  auto polyref = *polyptr;
  // TODO - ����������� invoke ��� ref? �� ���� ��� �� ������ �� ������...
  aa::invoke_unsafe<Draw>(*polyptr, std::cout, 150);
  aa::invoke_unsafe<Draw>(polyref, std::cout, 150);
  drawable0 v00;
  const drawable1 v01;
  Foobar((idrawable::ptr) & v00); // todo ��� �� � ����������� ��, ����� ��� ���� �������� �������������?
  Foobar(&v01);
  xyz val = 5;
  std::cout << sizeof(val);
  (void)(val == 5);
  val = std::string{"hello world"};
  val = 0.f;
  val = std::vector<int>{1, 2, 3, 4};
  example1();
  example_draw();
  example_draw_explicit();
  std::unordered_set<any_hashable> set;
  set.insert(std::string{"hello world"});
  set.emplace(5);
  set.emplace(5.);
  srand(time(0));
  return TestConstructors() + TestAnyCast() + TestCompare() + TestInvoke() + TestCasts();
}
