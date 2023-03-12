#include "visit_invoke_example.hpp"
#include "anyany.hpp"
#include "visit_invoke.hpp"
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
#include "polyref.hpp"

template<typename Alloc = std::allocator<char>>
using any_movable = aa::basic_any_with<Alloc, aa::default_any_soos, aa::move, aa::equal_to>;
template<typename Alloc = std::allocator<char>>
using any_copyable = aa::basic_any_with<Alloc, aa::default_any_soos, aa::copy_with<Alloc, aa::default_any_soos>::template method, aa::move, aa::equal_to>;

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
static_assert(std::is_same_v<any_compare::ref, aa::poly_ref<aa::copy, aa::spaceship, aa::move>> &&
              std::is_same_v<any_compare::cref, aa::cref<aa::copy, aa::spaceship, aa::move>> &&
              std::is_same_v<any_compare::ptr, aa::poly_ptr<aa::copy, aa::spaceship, aa::move>> &&
              std::is_same_v<any_compare::cptr, aa::cptr<aa::copy, aa::spaceship, aa::move>>);
using any_equal = aa::any_with<aa::equal_to, aa::move>;

size_t TestCompare() {
  size_t error_count = 0;
  any_compare v1(5.);
  error_if((v1 <=> v1) != std::partial_ordering::equivalent);
  error_if(v1 != v1);
  any_compare v2 = std::vector{10, 5};
  error_if((v1 <=> v2) != std::partial_ordering::unordered);
  auto v3 = v2;
  aa::any_cast<std::vector<int>>(std::addressof(v3))->back() = 0;
  aa::any_cast<std::vector<int>&>(v3).back() = 0;
  auto vec = aa::any_cast<std::vector<int>>(v3);
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
  static std::string do_invoke(T& self, int&& f, std::string&& s) {
    return self.bar(f, s);
  }
};

using any_fooable = aa::any_with<aa::copy, aa::move, foox, barx>;

size_t TestAnyCast() {
  size_t error_count = 0;
  any_fooable v0 = destroy_me<3>{};
  v0.foo();
  aa::invoke<foox>(v0);
  error_if(aa::any_cast<destroy_me<3>>(std::addressof(v0)) == nullptr);
  error_if(aa::any_cast<destroy_me<4>>(std::addressof(v0)) != nullptr);
  try {
    auto i = aa::any_cast<FooAble>(v0);
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
  error_if(aa::invoke<barx>(f0, 3, "hello world") != "bar called");
  error_if(aa::invoke<barx>(f1, 3, "hello world") != "bar called");
  error_if(aa::invoke<barx>(f2, 3, "hello world") != "bar called");
  error_if(aa::invoke<barx>(f3, 3, "hello world") != "bar called");
  static_assert(aa::const_method<foox>);
  static_assert(!aa::const_method<barx>);
  error_if(aa::invoke<foox>(std::as_const(f3)) != 1.f);
  f0 = f2 = f1 = f0 = f0 = f0 = std::move(f1) = f2 = f3 = any_fooable{} = f3 = std::move(f2) = f1 = f2 = f3 =
      f3 = f1 = f2;
  return error_count;
}

size_t TestCasts() {
  size_t error_count = 0;
  any_copyable<> cp;
  error_if(aa::any_cast<int>(std::addressof(cp)) != nullptr);
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

using xyz = aa::basic_any_with<std::allocator<std::byte>, 8, aa::copy_with<std::allocator<std::byte>, 8>::method, aa::move, aa::equal_to>;

// EXAMPLE WITH POLYMORPHIC_PTR
template<typename T>
struct Drawi {
  static int do_invoke(const T& self, int val)
  {
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
  std::cout << v->draw(150);
  std::cout << aa::invoke<Drawi>(*v, 150);
}
void Foobar(idrawable::cptr v) {
  std::cout << v->draw(150);
  std::cout << aa::invoke<Drawi>(*v, 150);
}

struct A1 {
  int i = 10;
};
struct A2 {
  double i = 3.14;
};
template <typename T>
struct M1 {
  static void do_invoke(const T& self, int val) {
    std::cout << typeid(self.i).name() << self.i << '\t' << val << '\n'; 
  }
};
template <typename T>
struct M2 {
  static void do_invoke(const T& self, double val, int val2) {
    std::cout << typeid(self.i).name() << self.i << '\t' << val << '\t' << val2 << '\n'; 
  }
};

template<typename T>
using Size = aa::from_callable<std::size_t() const, std::ranges::size>::method<T>;

struct base {
  int i;
};
struct deriv : base {
  int j;
};

struct spaceship {
  std::string s = "hello";
};
struct planet {
  int val = 150;
};

std::string case_sp(const spaceship& x, const planet& y) {
  if (x.s != "hello" || y.val != 150)
    throw false;
  return "sp";
}
std::string case_ps(const planet& x, const spaceship& y) {
  if (x.val != 150 || y.s != "hello")
    throw false;
  return "ps";
}

// clang-format off
constexpr inline auto collisions = aa::make_visit_invoke<
    case_sp,
    [](spaceship&, const spaceship&) { return std::string("ss"); },
    case_ps,
    [](planet a, planet& b) {
      if(a.val != 150 || b.val != 150)
        throw false;
      return std::string("pp");
    },
    [](int&&) {
        return std::string("int");
    }
>();
constexpr inline auto vars_collision = aa::make_visit_invoke<std::string,
    case_sp,
    case_ps,
    [](int) {
      return "int";
    }
>(aa::std_variant_poly_traits{});
// clang-format on
struct sm {
  int x;
};
using Tt = any_copyable<>;

template<typename T, typename U>
using ac_res = decltype(aa::any_cast<T>(std::declval<U>()));

using Ttvar = std::variant<int, float, double, bool>;

template<typename T, typename U>
using acvar_res = decltype(aa::any_cast<T, aa::std_variant_poly_traits>(std::declval<U>()));

template<typename T>
struct test_method {
  static int do_invoke(T self, double x) {
    return self + x;
  }
};

bool test_cmp() {
  int v1 = 10;
  int v2 = 11;
  {
    aa::any_with<aa::equal_to>::ref ref = v1;
    aa::any_with<aa::equal_to>::cref cref = v2;
    if (ref != ref || cref != cref || ref == cref)
      return false;
  }
  {
    using test_t = aa::any_with<aa::equal_to, aa::spaceship>;
    test_t::ref ref = v1;
    aa::any_with<aa::equal_to>::ref ref1 = v1;
    test_t::cref cref = v2;
    if (ref != ref || cref != cref || ref == cref)
      return false;
    using r_t = decltype(ref1);
    if (ref1 != r_t(ref) || r_t(ref) != ref1 || r_t(ref1) == *aa::const_pointer_cast(&cref) ||
        *aa::const_pointer_cast(&cref) == r_t(ref1))
      return false;
    if (ref >= cref)
      return false;
    if (*aa::const_pointer_cast(&cref) != v2 || ref != v1)
      return false;
    if (!(cref > 5))
      return false;
  }
  {
    aa::any_with<aa::spaceship>::ref ref = v1;
    aa::any_with<aa::spaceship>::cref cref = v2;
    if (ref != ref || cref != cref || ref == cref)
      return false;
  }
  return true;
}
// TODO ������ cref / cptr ������ const_poly_ptr ? � ������ ������ � �������
// ���� �� ������� ��� �������, � �������� const_poly_ptr � ����. ��������
// � ������� ���� ��������� ���������� �������... ����� ���� ���-�� ����� ������� ��������� ������ ���������?
// � ������������� ��� �������, ����� �������� ���������... �� � �������� �� ��������� "������" �����
// ���������
// ...
//
// � ��� � ����������� ��������, ���� �������� �� ��������(copy_with ��������)
// TODO - ��������� ��� ������ ���� ���������� ���� � ����� ����� if constexpr ������� ��������, � � ������
// ���
// TODO - ������� �� ������������� requires invocable Method<A>::do_invoke(declval<T>()) ??? ����� �������
// ���� ��� ���� ����������� ��� ����� ����������
// TODO ��� �������� �� scoped allocator adaptor � ������(���������� �� ������ �����������)

#define trait_impl(CONST, NAME, ...) \
template<typename> \
struct make_method_##NAME {};\
\
template<typename Ret, typename... Args>\
struct make_method_##NAME<Ret(Args...)> {\
    template <typename Self>\
    struct NAME {\
      static Ret do_invoke(CONST Self& self, Args... args)                             \
        requires requires {                                                            \
                   { self.NAME(static_cast<Args&&>(args)...) }->std::convertible_to<Ret>;                                                     \
                 }                                                                     \
      {\
        return self.NAME(static_cast<Args&&>(args)...);\
      }\
    };\
    template <::std::same_as<::aa::interface_t> Self>\
    struct NAME<Self> {\
    static Ret do_invoke(CONST Self& self, Args... args) {\
      return self.NAME(static_cast<Args&&>(args)...);\
    }\
    template <typename CRTP>\
    struct plugin {\
      Ret NAME(Args... args) CONST {\
        return ::aa::invoke<make_method_##NAME<Ret(Args...)>::template NAME>(                         \
                                           *static_cast<CONST CRTP*>(this),\
                                                                              static_cast<Args&&>(args)...);\
      }\
    };\
    };\
};\
template<typename T>\
using NAME = make_method_##NAME<__VA_ARGS__>::template NAME<T>;
#define trait(NAME, ... /*Signature like void(int, float)*/) trait_impl(, NAME, __VA_ARGS__)
#define const_trait(NAME, ... /*Signature like void(int, float)*/) trait_impl(const, NAME, __VA_ARGS__)

trait(hik, void(int));
// TODO ��������� trait/const_trait � ��������
// TODO ��� deprecated ���� ������� ����-�� ����
// TODO �� ������ invoke_unsafe � �������� invoke � ������� �� ������ ����������
// invoke_unsafe �������� ��� deprecated �����
// TODO ��������� atomic ���������� �� vtable ����� ������ ���?..

// TODO ������� �� ������ � ������� ������ �������� � do_invoke
// ������ �������� �� ��� ���(�� ������ ��������), ���� �� ������� https://www.youtube.com/watch?v=PEcy1vYHb8A&ab_channel=CppCon

struct hihi {
  void hik(int) const {
  }
};
// TODO ��� sizeof SooS <= sizeof(size_t) ����� ������������ ������ � ������� ��� ������
// TODO ������ operator+ ����� ��, ������� ����� plugin ���������� � ��������� CRTP&, ��������� type_descriptor � �.�.
// TODO? ����� ���� alignas(max_align_t) �������� �� alignas(void*), ����� ���������� � ��������� �������� �� �������(�������� 24 ����� ������)
// � ������ ����� �� ���� 32 ?! ������ ������� ���������
int main() {
    // TODO check operator-> � ���������
 // static_assert(satisfies_Hik<hihi> && !satisfies_Hik<int>);
  aa::basic_any_with<std::allocator<std::byte>, 8, hik> hmda = hihi{};
  hmda.hik(5);
  if (!test_cmp())
    return -100500;
  static_assert(std::is_same_v<acvar_res<int, Ttvar&>, int*>);
  static_assert(std::is_same_v<acvar_res<int&, Ttvar&>, int*>);
  static_assert(std::is_same_v<acvar_res<int&&, Ttvar&>, int*>);

  static_assert(std::is_same_v<acvar_res<const int, Ttvar&>, const int*>);
  static_assert(std::is_same_v<acvar_res<const int&, Ttvar&>, const int*>);
  static_assert(std::is_same_v<acvar_res<const int&&, Ttvar&>, const int*>);

  static_assert(std::is_same_v<ac_res<int, Tt&>, int>);
  static_assert(std::is_same_v<ac_res<int&, Tt&>, int&>);
  static_assert(std::is_same_v<ac_res<int&&, Tt&>, int>);

  static_assert(std::is_same_v<ac_res<const int, Tt&>, int>);
  static_assert(std::is_same_v<ac_res<const int&, Tt&>, const int&>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt&>, int>);

  static_assert(std::is_same_v<ac_res<int, Tt>, int>);
  static_assert(std::is_same_v<ac_res<int&, Tt>, int&>);
  static_assert(std::is_same_v<ac_res<int&&, Tt>, int&&>);

  static_assert(std::is_same_v<ac_res<const int, Tt>, int>);
  static_assert(std::is_same_v<ac_res<const int&, Tt>, const int&>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt>, const int&&>);

  static_assert(std::is_same_v<ac_res<int, Tt::ref>, int>);
  static_assert(std::is_same_v<ac_res<int&, Tt::ref>, int&>);
  static_assert(std::is_same_v<ac_res<int&&, Tt::ref>, int>);

  static_assert(std::is_same_v<ac_res<const int, Tt::ref>, int>);
  static_assert(std::is_same_v<ac_res<const int&, Tt::ref>, const int&>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt::ref>, int>);

  static_assert(std::is_same_v<ac_res<int, Tt::cref>, int>);
  static_assert(std::is_same_v<ac_res<int&, Tt::cref>, const int&>);
  static_assert(std::is_same_v<ac_res<int&&, Tt::cref>, const int&>);

  static_assert(std::is_same_v<ac_res<const int, Tt::cref>, int>);
  static_assert(std::is_same_v<ac_res<const int&, Tt::cref>, const int&>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt::cref>, const int&>);

  static_assert(std::is_same_v<ac_res<int, Tt::ptr>, int*>);
  static_assert(std::is_same_v<ac_res<int&, Tt::ptr>, int*>);
  static_assert(std::is_same_v<ac_res<int&&, Tt::ptr>, int*>);

  static_assert(std::is_same_v<ac_res<const int, Tt::ptr>, const int*>);
  static_assert(std::is_same_v<ac_res<const int&, Tt::ptr>, const int*>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt::ptr>, const int*>);

  static_assert(std::is_same_v<ac_res<int, Tt::cptr>, const int*>);
  static_assert(std::is_same_v<ac_res<int&, Tt::cptr>, const int*>);
  static_assert(std::is_same_v<ac_res<int&&, Tt::cptr>, const int*>);

  static_assert(std::is_same_v<ac_res<const int, Tt::cptr>, const int*>);
  static_assert(std::is_same_v<ac_res<const int&, Tt::cptr>, const int*>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt::cptr>, const int*>);

  std::atomic<aa::poly_ptr<>> a;
  std::atomic<aa::cptr<>> afff;
  (void)afff;
  static_assert(std::is_trivially_copyable_v<aa::poly_ref<aa::copy, aa::destroy>>);
  static_assert(std::is_trivially_copyable_v<aa::cref<>>);
  aa::poly_ref<> refa = a;
  // explicit rebind ref
  refa = aa::poly_ref<>(a);
  sm sm_val;
  aa::poly_ptr<> sm_val_p1 = &sm_val;
  aa::poly_ptr<> sm_val_p2 = &sm_val.x;
  if (sm_val_p1 == sm_val_p2)
    return -111;
  aa::example::multidispatch_usage();
  aa::example::multidispatch_usage2();
  using var_type = std::variant<int, spaceship, planet, double, char>;
  var_type var1 = spaceship{};
  const var_type var2 = planet{};
  if (*vars_collision.resolve(var1, var2) != "sp")
    return -11;
  if (!aa::any_cast<spaceship, aa::std_variant_poly_traits>(var1))
    return -12;
  if (!aa::any_cast<planet, aa::std_variant_poly_traits>(var2))
    return -13;
  if (!aa::any_cast<const spaceship, aa::std_variant_poly_traits>(var1))
    return -14;
  if (!aa::any_cast<const planet&, aa::std_variant_poly_traits>(var2))
    return -15;
  if (aa::any_cast<int, aa::std_variant_poly_traits>(var2))
    return -16;
  std::unordered_set<aa::descriptor_t> dmap{aa::descriptor_v<void>, aa::descriptor_v<int>};
  spaceship space_ship;
  planet space_planet;
  aa::const_poly_ref<> space_ref1 = space_ship;
  int fake_space = 0;
  aa::const_poly_ptr<> space_ref2 = &space_planet;
  if (collisions.resolve(5, 5) != std::nullopt)
    return -1;
  if (collisions.resolve(5, space_ref1) != std::nullopt)
    return -2;
  if (*collisions.resolve(space_ref1, space_ref2) != "sp")
    return -3;
  if (collisions.resolve(space_ref1, aa::poly_ptr<>(&fake_space)) != std::nullopt)
    return -4;
  if (collisions.resolve(aa::poly_ref<>(fake_space)) != "int")
    return -5;
  aa::any_with<aa::move> kekv = {aa::force_stable_pointers, 5};
  static_assert(std::is_same_v<decltype(kekv = decltype(kekv){}), decltype(kekv)&>);
  auto kekv_ptr = &kekv;
  void* kekv_raw_ptr = kekv_ptr.raw();
  auto kekv_move = std::move(kekv);
  if (kekv_raw_ptr != (&kekv_move).raw() || !kekv_move.is_stable_pointers())
    return -20;
  *aa::any_cast<int>(kekv_ptr) = 150; // must not segfault ))
  deriv d;
  aa::poly_ptr<> pdd = &d;
  aa::poly_ptr<> pdd1 = (base*)(&d);
  if (pdd.raw() != pdd1.raw() || pdd == pdd1 || pdd != pdd || pdd1 != pdd1)
    return -15;
  aa::const_poly_ptr<> cpdd = &d;
  aa::const_poly_ptr<> cpdd1 = (base*)(&d);
  if (cpdd.raw() != cpdd1.raw() || cpdd == cpdd1 || cpdd != cpdd || cpdd1 != cpdd1)
    return -16;
  if (cpdd != pdd || cpdd1 != pdd1 || cpdd1 == pdd || cpdd == pdd1)
    return -17;
  auto x = std::bit_cast<std::array<void*, 2>>(cpdd);
  (void)x;
  aa::any_with<example::Print> p = "hello world";
  auto* ptr = aa::any_cast<const char*>(&p);
  if (!ptr)
    throw 1;
  aa::invoke<example::Print>(p);
  static constexpr int for_r = 0;
  constexpr aa::const_poly_ref<> r = for_r;
  std::string X;
  auto visitor = [&]<typename T>(T v) {
    std::cout << typeid(T).name();
    X = typeid(T).name();
    return sizeof(T);
  };
  aa::type_switch(r)
      .case_<float>(visitor)
      .case_<bool>(visitor)
      .cases<char, int, unsigned char, double>(visitor)
      .no_default();
  if (X != typeid(int).name())
    return -10;
  auto switch_result = aa::type_switch<std::size_t>(r)
                           .case_<float>(visitor)
                           .case_<bool>(visitor)
                           .cases<char, unsigned char, double>(visitor)
                           .case_<int>(visitor)
                           .default_(std::size_t(-1));
  if (switch_result != sizeof(int))
    return -11;
  std::string fifa;
  aa::poly_ref<> rr = fifa;
  auto switch_result1 = aa::type_switch<std::size_t>(rr)
                            .case_<float>(visitor)
                            .case_<bool>(visitor)
                            .cases<char, unsigned char, double>(visitor)
                            .case_<int>(visitor)
                            .default_(std::size_t(-1));
  if (switch_result1 != -1)
    return -12;
  std::vector<aa::poly_ref<foox>> vec;
  FooAble fef{};
  vec.emplace_back(fef);
  using any_sized_range = aa::any_with<Size>;
  std::vector<int> v(10, 15);
  any_sized_range rng = v;
  std::cout << aa::invoke<Size>(rng);
  A1 u;
  A2 u1;
  aa::poly_ref<M1, M2, aa::copy, aa::move> fi = u;
  aa::const_poly_ref<M1, M2, aa::move> cfi = u1;
  aa::invoke<M1>(cfi, -1);
  aa::cref<M2, aa::move> cfi2 = cfi;
  aa::cptr<M2, aa::move> cfi2p = &cfi;
  aa::const_poly_ptr copy = cfi2p;
  (void)copy;
  aa::invoke<M2>(cfi2, 3.14, -2);
  aa::invoke<M2>(*cfi2p, 3.14, -2);
  aa::const_poly_ref<M2> cfi3 = cfi2;
  aa::const_poly_ptr<M2> cfi3p = &cfi2;
  aa::invoke<M2>(cfi3, 134., -3);
  aa::invoke<M2>(*cfi3p, 134., -3);
  aa::const_poly_ref<M1, M2> cfi4 = fi;
  aa::const_poly_ptr<M1, M2> cfi4p = &fi;
  aa::invoke<M1>(cfi4, -5);
  aa::invoke<M1>(*cfi4p, -5);
  aa::invoke<M2>(cfi4, 13400., -6);
  aa::invoke<M2>(*cfi4p, 13400., -6);
  aa::poly_ref<M2, aa::copy> fi2 = fi;
  aa::poly_ref<M2> fi3 = fi2;
  aa::poly_ptr<M2> fi3p = &fi2;
  aa::poly_ptr copy2 = fi3p;
  (void)copy2;
  aa::invoke<M1>(fi, 10);
  aa::invoke<M2>(fi, 11., 12);
  aa::invoke<M2>(fi2, 11., 12);
  aa::invoke<M2>(fi3, 11., 12);
  aa::invoke<M2>(*fi3p, 11., 12);
  int i = 1;
  if (i && cfi2.has_value())  // consteval static member function of reference, true always
    std::cout << "works\n";
  constexpr auto j0 =
      aa::noexport::find_subset(aa::type_list<int, double>{}, aa::type_list<int, double, float>{});
  constexpr auto j1 = aa::noexport::find_subset(aa::type_list<int, double>{}, aa::type_list<double, float>{});
  constexpr auto j2 = aa::noexport::find_subset(aa::type_list<double>{}, aa::type_list<double, float>{});
  constexpr auto j3 = aa::noexport::find_subset(aa::type_list<double>{}, aa::type_list<int, double, float>{});
  constexpr auto j4 = aa::noexport::find_subset(
      aa::type_list<double, int, char>{}, aa::type_list<char, double, int, double, int, char, bool, float>{});
  (void)j0, (void)j1, (void)j2, (void)j3, (void)j4, (void)fi2;
  {
    // with plugin
    drawable0 v0;
    const drawable1 v1;
    // create
    idrawable::ptr pp1 = &v0;
    idrawable::cptr pp2 = &v0;
    idrawable::ref pr1 = v0;
    idrawable::cref pr2 = v0;
    idrawable::cptr pp3 = &v1;
    idrawable::cref pr3 = v1;
    (void)aa::any_cast<const drawable1>(pr3);
    (void)aa::any_cast<drawable1>(pr3);
    (void)aa::any_cast<const drawable1&>(pr3);
    (void)aa::any_cast<drawable1&>(pr3);
    (void)aa::any_cast<const drawable0>(pr1);
    (void)aa::any_cast<drawable0>(pr1);
    (void)aa::any_cast<const drawable0&>(pr1);
    (void)aa::any_cast<drawable0&>(pr1);
    idrawable pval = v0;
    auto pip1 = &pval;
    if (aa::any_cast<drawable0>(pip1) == nullptr)
      return -1;
    (void)aa::any_cast<drawable0&>(*pip1);
    const idrawable cpval = v1;
    // same rules for poly ptr/poly ref/ poly value
    aa::any_cast<drawable1>(*&cpval).draw(5);  // without addressof, same rules - same result
    aa::any_cast<drawable1>(&cpval)->draw(5);
    aa::any_cast<drawable1>(std::addressof(cpval))->draw(5);
    aa::any_cast<const drawable1>(std::addressof(cpval))->draw(5);
    auto pip2 = &cpval;
    (void)pip1, (void)pip2;
    idrawable::ref pr4 = v0;
    idrawable::cref pr5 = v0;
    idrawable::ptr pp4 = &v0;
    idrawable::cptr pp5 = &v0;
    if (aa::any_cast<drawable0>(pp1) == nullptr || aa::any_cast<const drawable0>(pp1) == nullptr)
      return -1;
    (void)pr4, (void)pr5, (void)pp4, (void)pp5;
    // deduction guides
    aa::poly_ptr p_1 = pp1;
    aa::const_poly_ptr p_2 = pp1;
    aa::const_poly_ptr p_3 = pp2;
    (void)p_1, (void)p_2, (void)p_3;
    // invoke
    aa::invoke<Drawi>(*pp1, 150);
    aa::invoke<Drawi>(pr1, 150);
    aa::invoke<Drawi>(*&pr1, 150);
    aa::invoke<Drawi>(*pp2, 150);
    aa::invoke<Drawi>(*pp3, 150);
    aa::invoke<Drawi>(pr2, 150);
    aa::invoke<Drawi>(*&pr2, 150);
    aa::invoke<Drawi>(*&pr3, 150);
    pp1->draw(150);
    pp2->draw(150);
    pp3->draw(150);
    pr1.draw(150);
    pr2.draw(150);
    pr3.draw(150);
    (*&pr1).draw(150);
    (*&pr2).draw(150);
    (*&pr3).draw(150);
    // casts
    aa::any_cast<drawable0>(pp1)->draw(150);
    aa::any_cast<drawable0>(pp2)->draw(150);
    aa::any_cast<drawable1>(pp3)->draw(150);
    aa::any_cast<drawable0&>(pr1).draw(150);
    aa::any_cast<drawable0&>(pr2).draw(150);
    aa::any_cast<drawable1&>(pr3).draw(150);
    aa::any_cast<const drawable0&>(pr1).draw(150);
    aa::any_cast<const drawable0&>(pr2).draw(150);
    aa::any_cast<const drawable1&>(pr3).draw(150);
    if (aa::any_cast<drawable1>(pp1) != nullptr)
      return -1;
    try {
      (void)aa::any_cast<const drawable1&>(pr1);
      return -1;
    } catch (...) {
      // good
    }
  }
  {
    // without plugin
    Circle v0;
    const Circle v1{5, "hello world"};
    // create
    any_drawable::ptr pp1 = &v0;
    any_drawable::cptr pp2 = &v0;
    any_drawable::ref pr1 = v0;
    any_drawable::cref pr2 = v0;
    any_drawable::cptr pp3 = &v1;
    any_drawable::cref pr3 = v1;
    // invoke
    aa::invoke<Draw>(*pp1, std::cout, 150);
    aa::invoke<Draw>(pr1, std::cout, 150);
    aa::invoke<Draw>(*&pr1, std::cout, 150);
    aa::invoke<Draw>(*pp2, std::cout, 150);
    aa::invoke<Draw>(*pp3, std::cout, 150);
    aa::invoke<Draw>(pr2, std::cout, 150);
    aa::invoke<Draw>(*&pr2, std::cout, 150);
    aa::invoke<Draw>(*&pr3, std::cout, 150);
    // casts
    aa::any_cast<const Circle>(pp1)->draw(std::cout, 150);
    aa::any_cast<Circle>(pp2)->draw(std::cout, 150);
    aa::any_cast<const Circle>(pp3)->draw(std::cout, 150);
    aa::any_cast<Circle&>(pr1).draw(std::cout, 150);
    aa::any_cast<const Circle&>(pr1).draw(std::cout, 150);
    aa::any_cast<Circle&>(pr2).draw(std::cout, 150);
    aa::any_cast<const Circle&>(pr3).draw(std::cout, 150);
    aa::any_cast<Circle&>(pr3).draw(std::cout, 150);
    if (aa::any_cast<Triangle>(pp1) != nullptr)
      return -1;
    try {
      (void)aa::any_cast<Triangle&>(pr3);
      return -1;
    } catch (...) {
      // good
    }
  }
  drawable0 v00;
  const drawable1 v01;
  Foobar((idrawable::ptr)&v00);
  Foobar(&v01);
  xyz val = 5;
  std::cout << sizeof(val);
  if (val != 5) // implicit conversion 5 to any_with
    return -100;
  val = std::string{"hello world"};
  val = 0.f;
  val = std::vector<int>{1, 2, 3, 4};
  aa::example::example1();
  example_draw();
  example_draw_explicit();
  example_polyref();
  std::unordered_set<any_hashable> set;
  set.insert(std::string{"hello world"});
  set.emplace(5);
  set.emplace(5.);
  srand(time(0));
  return TestConstructors() + TestAnyCast() + TestCompare() + TestInvoke() + TestCasts();
}