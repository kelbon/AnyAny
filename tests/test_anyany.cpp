#include <algorithm>
#include <array>
#include <atomic>
#include <functional>
#include <iostream>
#include <list>
#include <memory>
#include <memory_resource>
#include <random>
#include <unordered_set>

#include <anyany/anyany.hpp>
#include <anyany/anyany_macro.hpp>

#define error_if(Cond) error_count += static_cast<bool>((Cond))
#define TEST(NAME) size_t TEST##NAME(size_t error_count = 0)

template <typename Alloc = std::allocator<char>>
using any_movable = aa::basic_any_with<Alloc, aa::default_any_soos, aa::move, aa::equal_to>;
template <typename Alloc = std::allocator<char>>
using any_copyable =
    aa::basic_any_with<Alloc, aa::default_any_soos, aa::copy_with<Alloc>, aa::equal_to>;

int leaked_resource_count = 0;

struct deleter_resource {
  void operator()(void*) {
    leaked_resource_count--;
  }
};
template <size_t Sz>
struct destroy_me {
  char padding[Sz];
  std::shared_ptr<void> ptr;
  destroy_me() : ptr(padding, deleter_resource{}) {
    leaked_resource_count++;
    std::fill(std::begin(padding), std::end(padding), 1);
  }
  bool operator==(const destroy_me& x) const {
    return ptr == x.ptr;
  }

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
  std::list<int> l;  // lol it is throw move / copy constructible... Allocations on move / default init
};

using nomove_any = aa::any_with<>;
struct nomove {
  template <typename T>
  nomove(int, T) {
  }
  nomove(nomove&&) = delete;
  void operator=(nomove&&) = delete;
};

#if __cplusplus >= 202002L

const float correct_float = 314.f;
const int correct_int = 334156;
const int correct_val = correct_int + correct_float;

bool good_flag0 = false;

struct empty {
  void validate() const {
  }
  bool operator==(const empty&) const = default;
};
struct empty_non_trivial {
  void validate() const {
  }
  bool operator==(const empty_non_trivial&) const = default;
  empty_non_trivial() = default;
  empty_non_trivial(const empty_non_trivial&) {
    good_flag0 = true;
  }
  ~empty_non_trivial() {
    if (!good_flag0)
      std::exit(10);
  }
};
struct small_trivial {
  int i = 146;
  void validate() const {
    if (i != 146)
      throw false;
  }
  bool operator==(const small_trivial&) const = default;
};
struct small_non_trivial {
  std::string s = "hello world";
  void validate() const {
    if (s != "hello world")
      throw false;
  }
  bool operator==(const small_non_trivial&) const = default;
};
struct big_trivial {
  char arr[aa::default_any_soos + 5] = {};
  int i = 146;
  void validate() const {
    if (i != 146)
      throw false;
  }
  bool operator==(const big_trivial&) const = default;
};
struct big_non_trivial {
  char arr[aa::default_any_soos + 5] = {};
  std::string s = "hello world";
  void validate() const {
    if (s != "hello world")
      throw false;
  }
  bool operator==(const big_non_trivial&) const = default;
};

template<typename Base, bool IsGoodAlign>
struct alignas(IsGoodAlign ? alignof(Base) : 64) test_type : Base {
  using Base::validate;
  int operator()(float f) {
    return correct_int + f;
  }
  bool operator==(const test_type&) const = default;
};

// и ещё с другим аллокатором когда any...(не пустым)

anyany_method(validate, (const& self) requires(self.validate()) -> void);
#endif
anyany_method(boo, (const& self, int i) requires(self.boo(i))->int);
TEST(constructors) {
  any_copyable<> ilist{std::in_place_type<std::vector<int>>, {1, 2, 3}};
  ilist.emplace<std::vector<int>>({1, 2, 3});
#if __cplusplus >= 202002L
  auto do_test = [&]<typename T>(std::type_identity<T>, auto value) {
    using std::swap;
    T a = value;
    error_if(!a.has_value());
    a.validate();
    error_if(a(correct_float) != correct_val);
    auto copy = a;
    swap(a, a);
    a.validate();
    error_if(a != copy);
    copy.validate();
    error_if(!copy.has_value());
    error_if(copy(correct_float) != correct_val);
    error_if(a != copy);
    swap(a, copy);
    a.validate();
    copy.validate();
    error_if(!a.has_value());
    error_if(a(correct_float) != correct_val);
    error_if(!copy.has_value());
    error_if(copy(correct_float) != correct_val);
    error_if(a != copy);
  };
  auto repeat_test_for = [&]<typename... Ts>(aa::type_list<Ts...>) {
    (do_test(std::type_identity<aa::any_with<validate, aa::call<int(float)>, aa::copy, aa::equal_to>>{},
             test_type<Ts, false>{}),
     ...);
    (do_test(std::type_identity<aa::any_with<validate, aa::call<int(float)>, aa::copy, aa::equal_to>>{},
             test_type<Ts, true>{}),
     ...);
    using alloc = std::pmr::polymorphic_allocator<std::byte>;
    (do_test(
         std::type_identity<aa::basic_any_with<alloc, aa::default_any_soos, validate, aa::call<int(float)>,
                                               aa::copy_with<alloc>, aa::equal_to>>{},
         test_type<Ts, false>{}),
     ...);
    (do_test(
         std::type_identity<aa::basic_any_with<alloc, aa::default_any_soos, validate, aa::call<int(float)>,
                                               aa::copy_with<alloc>, aa::equal_to>>{},
         test_type<Ts, true>{}),
     ...);
  };
  using test_types_pack =
      aa::type_list<empty, empty_non_trivial, small_trivial, small_non_trivial, big_trivial, big_non_trivial>;
  repeat_test_for(test_types_pack{});
  auto seed = std::random_device{}();
  std::cout << "current random seed: " << seed << std::endl;
  std::mt19937 gen{seed};
  std::bernoulli_distribution dist(0.9);
  auto test_swap = [&]<typename Any>(std::type_identity<Any>, auto v1, auto v2) {
    Any a = dist(gen) ? Any{v1} : Any{};
    auto a_copy = a;
    Any b = dist(gen) ? Any{v2} : Any{};
    auto b_copy = b;
    auto id1 = a.type_descriptor();
    auto id2 = b.type_descriptor();
    error_if(a != a_copy);
    error_if(b != b_copy);
    using std::swap;
    swap(a, b);
    error_if(a != b_copy);
    error_if(b != a_copy);
    error_if(id1 != b.type_descriptor());
    error_if(id2 != a.type_descriptor());
    swap(a, b);
    error_if(a != a_copy);
    error_if(b != b_copy);
    error_if(id1 != a.type_descriptor());
    error_if(id2 != b.type_descriptor());
  };
  auto do_test_swap_for_each_pair = [&]<typename T, typename... Ts>(aa::type_list<T, Ts...>) {
    using tt1 = aa::any_with<aa::copy, aa::equal_to>;
    (test_swap(std::type_identity<tt1>{}, T{}, Ts{}), ...);
    using alloc = std::pmr::polymorphic_allocator<std::byte>;
    using tt2 = aa::basic_any_with<alloc, aa::default_any_soos, aa::copy_with<alloc>, aa::equal_to>;
    (test_swap(std::type_identity<tt2>{}, T{}, Ts{}), ...);
  };
  auto test_swap_for_each_pair = [&]<typename... Ts>(aa::type_list<Ts...>) {
    (do_test_swap_for_each_pair(aa::type_list<Ts, Ts...>{}), ...);
  };
  test_swap_for_each_pair(test_types_pack{});
#endif
  // problems with emplaceing std::array(aggregate construct) because of construct_at
  constexpr auto Xy = [] {};
  // nomove nocopy construct
  nomove_any a0(std::in_place_type<nomove>, 5, Xy);
  a0.emplace<int>();
  a0.reset();
  struct boobl {
    mutable int x = 0;
    int boo(int i) const {
      x = i;
      return i * 2;
    }
  };
  boobl bval;
  aa::any_with<boo> l{std::in_place_type<aa::poly_ref<boo>>, bval};
  error_if(l.boo(15) != 30);
  error_if(bval.x != 15);
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
    std::shuffle(begin(vec), end(vec), generator);
    std::vector<any_movable<>> storage;
    std::move(begin(vec), end(vec), std::back_inserter(storage));
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
    std::shuffle(begin(vec), end(vec), generator);
    std::vector<any_movable<>> storage;
    std::move(begin(vec), end(vec), std::back_inserter(storage));
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
    std::shuffle(begin(vec), end(vec), generator);
    std::vector<any_copyable<>> storage;
    std::sample(begin(vec), end(vec), std::back_inserter(storage), vec.size() / 2, generator);
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
    std::shuffle(begin(vec), end(vec), generator);
    std::vector<any_copyable<>> storage;
    std::sample(begin(vec), end(vec), std::back_inserter(storage), vec.size() / 2, generator);
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
    std::shuffle(begin(vec), end(vec), generator);
    std::pmr::vector<any_copyable<std::pmr::polymorphic_allocator<std::byte>>> storage(
        std::pmr::new_delete_resource());
    std::sample(begin(vec), end(vec), std::back_inserter(storage), vec.size() / 2, generator);
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

TEST(special_member_functions) {
  static_assert(!std::is_constructible_v<aa::any_with<aa::copy>, aa::poly_ref<aa::copy>>);
  // transmutate ctor
  static_assert(std::is_constructible_v<aa::any_with<aa::copy>, aa::any_with<aa::copy, aa::equal_to>>);
  const std::vector<int> vec(100, 5);
  auto do_test = [&](auto x) {
    std::vector<decltype(x)> vec_anys;
    vec_anys.reserve(5);
    vec_anys.emplace_back(vec);
    vec_anys.emplace_back(aa::allocator_arg, aa::default_allocator{}, vec);
    vec_anys.emplace_back(std::in_place_type<std::vector<int>>, vec);
    vec_anys.emplace_back(aa::force_stable_pointers, vec);
    vec_anys.emplace_back(aa::force_stable_pointers, std::in_place_type<std::vector<int>>, vec);
    error_if(!vec_anys[3].is_stable_pointers());
    error_if(!vec_anys[4].is_stable_pointers());
    error_if(std::any_of(begin(vec_anys), end(vec_anys), [](auto& x) { return !x.has_value(); }));
    error_if(std::any_of(begin(vec_anys), end(vec_anys), [&](auto& x) { return x != vec_anys.front(); }));
    error_if(std::any_of(begin(vec_anys), end(vec_anys),
                         [&](auto& x) { return aa::any_cast<std::vector<int>&>(x) != vec; }));
    auto m = std::move(vec_anys.front());
    error_if(*aa::any_cast<std::vector<int>>(&m) != vec);
    error_if(vec_anys.front().has_value());
    vec_anys.front() = std::move(m);
    error_if(*aa::any_cast<std::vector<int>>(&vec_anys.front()) != vec);
    error_if(m.has_value());
    if constexpr (__cplusplus >= 202002L && std::is_copy_constructible_v<decltype(x)>) {
      auto vec_copy = vec_anys;
      error_if(vec_copy != vec_anys);
    }
  };
  do_test(aa::any_with<aa::copy, aa::equal_to>{});
  do_test(aa::any_with<aa::move, aa::equal_to>{});
  do_test(aa::any_with<aa::copy, aa::move, aa::move, aa::copy, aa::equal_to>{});
  // uses only move from copy_with<...> Method
  do_test(aa::any_with<aa::copy_with<aa::unreachable_allocator>, aa::equal_to>{});
  return error_count;
}

void noallocate_test() {
  using any_noallocate = aa::basic_any_with<aa::unreachable_allocator, aa::default_any_soos,
                                            aa::copy_with<aa::unreachable_allocator>>;

  any_noallocate x = 5;
  auto y = std::move(x);
  y = x;
  auto z = y;
}
#if __cplusplus >= 202002L
using any_compare = aa::any_with<aa::copy, aa::equal_to, aa::spaceship, aa::move>;
static_assert(
    std::is_same_v<any_compare::ref, aa::poly_ref<aa::copy, aa::equal_to, aa::spaceship, aa::move>> &&
    std::is_same_v<any_compare::const_ref,
                   aa::const_poly_ref<aa::copy, aa::equal_to, aa::spaceship, aa::move>> &&
    std::is_same_v<any_compare::ptr, aa::poly_ptr<aa::copy, aa::equal_to, aa::spaceship, aa::move>> &&
    std::is_same_v<any_compare::const_ptr,
                   aa::const_poly_ptr<aa::copy, aa::equal_to, aa::spaceship, aa::move>>);
using any_equal = aa::any_with<aa::equal_to, aa::equal_to, aa::spaceship, aa::spaceship, aa::move>;

TEST(compare) {
  any_compare v1(5.);
  error_if((v1 <=> v1) != std::partial_ordering::equivalent);
  error_if(v1 != v1);
  any_compare v2 = std::vector{10, 5};
  error_if((v1 <=> v2) != std::partial_ordering::unordered);
  auto v3 = v2;
  aa::any_cast<std::vector<int>>(std::addressof(v3))->back() = 0;
  aa::any_cast<std::vector<int>&>(v3).back() = 0;
  auto vec = aa::any_cast<std::vector<int>>(v3);
  auto vec2 = aa::any_cast<std::vector<int>>(v2);
  error_if(v3 >= v2);
  any_equal v4 = 3.14f;
  error_if(v4 != any_equal{3.14f});
  error_if(any_equal{3.14f} != v4);
  return error_count;
}
#else
size_t TESTcompare() {
  return 0;
}
#endif

struct FooAble {
  float foo() const {
    return 0;
  }
  std::string bar(int, std::string) {
    return "hello world";
  }
};

struct foox {
  template <typename T>
  static float do_invoke(T self) {
    return self.foo();
  }
  template <typename CRTP>
  struct plugin {
    float foo() const {
      return aa::invoke<foox>(static_cast<const CRTP&>(*this));
    }
  };
};

struct barx {
  template <typename T>
  static std::string do_invoke(T& self, int&& f, std::string&& s) {
    return self.bar(f, s);
  }
};

using any_fooable = aa::any_with<aa::type_info, aa::copy, foox, barx>;

TEST(any_cast) {
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

TEST(invoke) {
  any_fooable f0(std::in_place_type<destroy_me<5>>);
  any_fooable f1(std::in_place_type<destroy_me<50>>);
  any_fooable f2(std::in_place_type<destroy_me<500>>);
  any_fooable f3(std::in_place_type<destroy_me<100>>);
  (void)f0.foo();
  error_if(aa::invoke<barx>(f0, 3, "hello world") != "bar called");
  error_if(aa::invoke<barx>(f1, 3, "hello world") != "bar called");
  error_if(aa::invoke<barx>(f2, 3, "hello world") != "bar called");
  error_if(aa::invoke<barx>(f3, 3, "hello world") != "bar called");
  static_assert(aa::is_const_method_v<foox>);
  static_assert(!aa::is_const_method_v<barx>);
  error_if(aa::invoke<foox>(std::as_const(f3)) != 1.f);
  f0 = f2 = f1 = f0 = f0 = f0 = std::move(f1) = f2 = f3 = any_fooable{} = f3 = std::move(f2) = f1 = f2 = f3 =
      f3 = f1 = f2;
  return error_count;
}

TEST(any_cast2) {
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
    // auto& ref = aa::any_cast<int&>(acop); CE because of const
    auto& ref2 = aa::any_cast<const int&>(acop);
    int val = aa::any_cast<int>(acop);
    (void)ref2, (void)val;
  }
  return error_count;
}

using any_hashable = aa::any_with<aa::hash, aa::equal_to, aa::copy>;

using xyz = aa::basic_any_with<std::allocator<std::byte>, 8, aa::copy, aa::equal_to>;

struct Drawi {
  template <typename T>
  static int do_invoke(const T& self, int val) {
    return self.draw(val);
  }

  template <typename CRTP>
  struct plugin {
    int draw(int val) const noexcept {
      return aa::invoke<Drawi>(*static_cast<const CRTP*>(this), val);
    }
  };
};

using idrawable = aa::any_with<aa::type_info, Drawi, aa::copy>;

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
void Foobar(idrawable::const_ptr v) {
  std::cout << v->draw(150);
  std::cout << aa::invoke<Drawi>(*v, 150);
}

struct A1 {
  int i = 10;
};
struct A2 {
  double i = 3.14;
};

struct M1 {
  template <typename T>
  static void do_invoke(const T& self, int val) {
    std::cout << typeid(self.i).name() << self.i << '\t' << val << '\n';
  }
};

struct M2 {
  template <typename T>
  static void do_invoke(const T& self, double val, int val2) {
    std::cout << typeid(self.i).name() << self.i << '\t' << val << '\t' << val2 << '\n';
  }
};

struct base {
  int i;
};
struct deriv : base {
  int j;
};

struct sm {
  int x;
};
using Tt = any_copyable<>;

template <typename T>
struct test_method {
  static int do_invoke(T self, double x) {
    return self + x;
  }
};

template <typename T>
anyany_extern_method(visit,
                     (&self, const T& value) requires(std::enable_if_t<std::is_copy_constructible_v<Self>>(),
                                                      self(value))
                         ->void);

template <typename... Ts>
using visitor_for = aa::any_with<visit<Ts>...>;

anyany_method(Kekab, (const& self, int x, char y) requires(self.kekab(x, y))->std::string);

using any_kekable_ = aa::any_with<Kekab>;

struct kekabl1 {
  std::string s = "abc";
  std::string kekab(int i, char k) const {
    return s + std::string(i, (char)k);
  }
};
TEST(transmutate_ctors) {
  aa::any_with<aa::type_info, aa::move, aa::copy> v1;
  v1 = std::string("abc");
  error_if((uintptr_t)(&v1).raw() != (uintptr_t)(std::addressof(v1)) + 16);
  auto xx = v1;
  aa::any_with<aa::type_info, aa::move> v2 = v1;
  error_if((uintptr_t)(&v2).raw() == (uintptr_t)(&v1).raw());
  auto copyv2 = std::move(v2);
  auto copyv1 = v1;
  error_if(copyv2.type_descriptor() != copyv1.type_descriptor());
  const std::vector<int> vec(100, 5);
  auto do_test = [&](auto x) {
    std::vector<decltype(x)> vec_anys;
    vec_anys.reserve(5);
    vec_anys.emplace_back(vec);
    vec_anys.emplace_back(aa::allocator_arg, aa::default_allocator{}, vec);
    vec_anys.emplace_back(std::in_place_type<std::vector<int>>, vec);
    vec_anys.emplace_back(aa::force_stable_pointers, vec);
    vec_anys.emplace_back(aa::force_stable_pointers, std::in_place_type<std::vector<int>>, vec);
    if constexpr (__cplusplus >= 202002L && std::is_copy_constructible_v<decltype(x)>) {
      std::vector<aa::any_with<aa::move, aa::equal_to>> vec_transmutate;
      for (auto& x : vec_anys)
        vec_transmutate.push_back(x);
      error_if(std::any_of(begin(vec_transmutate), end(vec_transmutate),
                           [&](auto& x) { return aa::any_cast<std::vector<int>&>(x) != vec; }));
      error_if(std::any_of(begin(vec_transmutate), end(vec_transmutate),
                           [&](auto& x) { return x != vec_transmutate.front(); }));
    }
    std::vector<aa::any_with<aa::move, aa::equal_to>> vec_transmutate_move;
    for (auto& x : vec_anys)
      vec_transmutate_move.push_back(std::move(x));
    error_if(std::any_of(begin(vec_anys), end(vec_anys), [](const auto& x) { return x.has_value(); }));
    error_if(std::any_of(begin(vec_transmutate_move), end(vec_transmutate_move),
                         [](const auto& x) { return !x.has_value(); }));
    error_if(std::any_of(begin(vec_transmutate_move), end(vec_transmutate_move),
                         [&](auto& x) { return aa::any_cast<const std::vector<int>&>(x) != vec; }));
    error_if(std::any_of(begin(vec_transmutate_move), end(vec_transmutate_move),
                         [&](auto& x) { return x != vec_transmutate_move.front(); }));
  };
  do_test(aa::any_with<aa::copy, aa::destroy, aa::move, aa::equal_to>{});
  do_test(aa::any_with<aa::destroy, aa::move, aa::equal_to>{});
  do_test(aa::any_with<aa::copy, aa::move, aa::copy, aa::destroy, aa::move, aa::equal_to>{});
  // uses only move from copy_with<...> Method
  do_test(aa::any_with<aa::copy_with<aa::unreachable_allocator>, aa::destroy, aa::move, aa::equal_to>{});
  return error_count;
}
TEST(stateful) {
  int i = 5;
  aa::stateful::cref<aa::copy, aa::type_info> ccl = i;
  aa::stateful::ref<aa::type_info, aa::copy> cl = i;
  (void)ccl, (void)cl;
  aa::cref<aa::copy, aa::move, aa::equal_to> r = i;
  aa::stateful::cref sr = r;
  static_assert(std::is_same_v<decltype(sr), aa::stateful::cref<aa::copy, aa::move, aa::equal_to>>);
  aa::stateful::cref<aa::move> sr1 = r;
  aa::stateful::cref<aa::move> sr2 = sr;
  kekabl1 val;
  aa::stateful::ref<aa::copy, Kekab, aa::move> rr = val;
  error_if(rr.Kekab(4, 'a') != "abcaaaa");
  auto copyrr = rr;
  error_if(copyrr.Kekab(4, 'a') != "abcaaaa");
  aa::stateful::ref<Kekab> rrkk = rr;
  error_if(rrkk.Kekab(4, 'a') != "abcaaaa");
  aa::stateful::cref crrkk = rrkk;
  error_if(crrkk.Kekab(4, 'a') != "abcaaaa");
  aa::stateful::cref<Kekab> crrkk2 = rr;
  error_if(crrkk2.Kekab(4, 'a') != "abcaaaa");
  (void)sr1, (void)sr2;
  return error_count;
}

anyany_method(print, (const& self) requires(std::cout << self << std::endl)->void);

TEST(ptr_behavior) {
  std::string s = "hello";
  int x = 10;
  aa::poly_ptr<print> ptr;
  constexpr aa::poly_ptr<print> _p = nullptr;
  // check right specialization selected for one method
  (void)aa::mate::get_vtable_value(_p);
  static_assert(aa::mate::get_vtable_ptr(_p) != nullptr);
  constexpr aa::poly_ptr<print, print> _p2 = nullptr;
  static_assert(aa::mate::get_vtable_ptr(_p2) == nullptr);
  error_if(ptr.raw() != nullptr);
  aa::poly_ptr<aa::type_info, print> ptr1 = &s;
  ptr1->print();
  aa::poly_ptr<aa::type_info, print> ptr2 = &x;
  ptr2->print();
  ptr1 = ptr2;
  ptr2 = nullptr;
  error_if(ptr2);
  error_if(ptr2.type_descriptor() != aa::descriptor_v<void>);
  aa::const_poly_ptr p = ptr1;
  error_if(p != ptr1);
  return error_count;
}
template <typename T, typename U>
using ac_res = decltype(aa::any_cast<T>(std::declval<U>()));

void any_cast_test() {
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

  static_assert(std::is_same_v<ac_res<int, Tt::const_ref>, int>);
  static_assert(std::is_same_v<ac_res<int&, Tt::const_ref>, const int&>);
  static_assert(std::is_same_v<ac_res<int&&, Tt::const_ref>, const int&>);

  static_assert(std::is_same_v<ac_res<const int, Tt::const_ref>, int>);
  static_assert(std::is_same_v<ac_res<const int&, Tt::const_ref>, const int&>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt::const_ref>, const int&>);

  static_assert(std::is_same_v<ac_res<int, Tt::ptr>, int*>);
  static_assert(std::is_same_v<ac_res<int&, Tt::ptr>, int*>);
  static_assert(std::is_same_v<ac_res<int&&, Tt::ptr>, int*>);

  static_assert(std::is_same_v<ac_res<const int, Tt::ptr>, const int*>);
  static_assert(std::is_same_v<ac_res<const int&, Tt::ptr>, const int*>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt::ptr>, const int*>);

  static_assert(std::is_same_v<ac_res<int, Tt::const_ptr>, const int*>);
  static_assert(std::is_same_v<ac_res<int&, Tt::const_ptr>, const int*>);
  static_assert(std::is_same_v<ac_res<int&&, Tt::const_ptr>, const int*>);

  static_assert(std::is_same_v<ac_res<const int, Tt::const_ptr>, const int*>);
  static_assert(std::is_same_v<ac_res<const int&, Tt::const_ptr>, const int*>);
  static_assert(std::is_same_v<ac_res<const int&&, Tt::const_ptr>, const int*>);
}
anyany_method(change_i, (self) requires(self.i = 4)->void);
struct x {
  int i = 0;
};
TEST(type_descriptor_and_plugins_interaction) {
#if __cplusplus >= 202002
  int i = 10;
  auto check = [&](auto ref) {
    error_if(ref.type_descriptor() != aa::descriptor_v<int>);
    error_if((&ref).type_descriptor() != aa::descriptor_v<int>);
    error_if((&ref)->type_descriptor() != aa::descriptor_v<int>);
  };
  aa::poly_ref<aa::type_info, aa::equal_to, aa::spaceship> ref = i;
  using all3_t = decltype(ref);
  static_assert(std::derived_from<all3_t, aa::spaceship::plugin<all3_t>>);
  static_assert(!std::derived_from<all3_t, aa::equal_to::plugin<all3_t>>);
  static_assert(!std::derived_from<all3_t, aa::type_info::plugin<all3_t>>);
  using two_t = aa::poly_ref<aa::equal_to, aa::spaceship>;
  static_assert(std::derived_from<two_t, aa::spaceship::plugin<two_t>>);
  static_assert(!std::derived_from<two_t, aa::equal_to::plugin<two_t>>);
  static_assert(!std::derived_from<two_t, aa::type_info::plugin<two_t>>);
  using one_t = aa::poly_ref<aa::equal_to, aa::type_info>;
  static_assert(std::derived_from<one_t, aa::equal_to::plugin<one_t>>);
  static_assert(!std::derived_from<one_t, aa::type_info::plugin<one_t>>);
  check(ref);
  static_assert(std::is_same_v<decltype(aa::mate::get_vtable_ptr(ref)),
                               const aa::vtable<aa::type_info, aa::equal_to, aa::spaceship>*>);
  check(aa::poly_ref<aa::type_info>(ref));
  check(aa::poly_ref<aa::type_info>(i));
  check(aa::poly_ref<aa::equal_to>(ref));
  check(aa::poly_ref<aa::equal_to>(i));
  check(aa::poly_ref<aa::spaceship>(ref));
  check(aa::poly_ref<aa::spaceship>(i));
  check(aa::poly_ref<aa::equal_to, aa::spaceship>(ref));
  check(aa::poly_ref<aa::equal_to, aa::spaceship>(i));
  check(aa::poly_ref<aa::type_info, aa::equal_to>(ref));
  check(aa::poly_ref<aa::type_info, aa::equal_to>(i));
  check(aa::poly_ref<aa::type_info, aa::spaceship>(i));
  check(aa::const_poly_ref(ref));
  check(aa::const_poly_ref<aa::type_info>(ref));
  check(aa::const_poly_ref<aa::type_info>(i));
  check(aa::const_poly_ref<aa::equal_to>(ref));
  check(aa::const_poly_ref<aa::equal_to>(i));
  check(aa::const_poly_ref<aa::spaceship>(ref));
  check(aa::const_poly_ref<aa::spaceship>(i));
  check(aa::const_poly_ref<aa::equal_to, aa::spaceship>(ref));
  check(aa::const_poly_ref<aa::equal_to, aa::spaceship>(i));
  check(aa::const_poly_ref<aa::type_info, aa::equal_to>(ref));
  check(aa::const_poly_ref<aa::type_info, aa::equal_to>(i));
  check(aa::const_poly_ref<aa::type_info, aa::spaceship>(i));
#endif
  return error_count;
}

anyany_extern_method(Draw, (const& self, std::ostream& out, int val) requires(self.draw(out, val))->void);

using any_drawable = aa::any_with<aa::type_info, Draw>;

struct Triangle {
  void draw(std::ostream& out, int val) const {
    out << val << "Triangle";
  }
};
struct Circle {
  void draw(std::ostream& out, int val) const {
    out << val << "Cirle";
  }
  int x;
  std::string y;
};

anyany_pseudomethod(test_pseudomethod, requires(aa::descriptor_v<Self>)->aa::descriptor_t);
struct empty_struct_t {};
template<typename>
anyany_pseudomethod(empty_value_pseudomethod, requires(empty_struct_t{})->empty_struct_t);

anyany_extern_method(a, (&self) requires((void)0)->void);
anyany_extern_method(b, (const &self) requires(5)->int);
anyany_extern_method(c, (self) requires(3.14)->float);

template<template<typename...> typename Template>
void anyany_interface_alias_tests() {
#define AA_IA_TEST(...)                                                                          \
  static_assert(std::is_same_v<aa::interface_of<aa::insert_flatten_into<Template, __VA_ARGS__>>, \
                               aa::interface_alias<__VA_ARGS__>>)
  static_assert(std::is_same_v<aa::interface_of<Template<>>, aa::interface_alias<>>);
  AA_IA_TEST(aa::type_info);
  AA_IA_TEST(aa::destroy);
  AA_IA_TEST(aa::destroy, aa::destroy, aa::type_info);
  AA_IA_TEST(aa::call<int()>, aa::equal_to, aa::type_info);
  using a = aa::interface_alias<aa::destroy, aa::type_info>;
  using b = aa::interface_alias<>;
  using c = aa::interface_alias<a, b>;
  using d = aa::interface_alias<a, b, aa::call<int()>>;
  using e = aa::interface_alias<d, a, b, aa::call<int() noexcept>>;
  AA_IA_TEST(a, b, c, d, e);
  AA_IA_TEST(e, a, b);
  AA_IA_TEST(e, aa::destroy, a, b);
  AA_IA_TEST(aa::destroy, e, aa::copy_with<aa::unreachable_allocator>, aa::destroy, a, aa::type_info, b);
  static_assert(std::is_same_v<aa::any_with<a, b, c>,
                               aa::basic_any<aa::default_allocator, aa::default_any_soos, aa::destroy,
                                             aa::destroy, aa::type_info, aa::destroy, aa::type_info>>);
  static_assert(std::is_same_v<aa::any_with<a, b, c>::ref,
                               aa::poly_ref<aa::destroy, aa::type_info, aa::destroy, aa::type_info>>);
#if __cplusplus >= 202002L
  static_assert(aa::compound_method<a>);
  static_assert(aa::compound_method<b>);
  static_assert(aa::compound_method<c>);
  static_assert(aa::compound_method<d>);
  static_assert(aa::compound_method<e>);
  static_assert(!aa::compound_method<aa::type_list<float>>);
  static_assert(!aa::compound_method<aa::type_list<aa::destroy, aa::type_info, float>>);
#endif
#undef AA_IA_TEST
}

void anyany_concepts_test() {
  anyany_interface_alias_tests<aa::any_with>();
  anyany_interface_alias_tests<aa::poly_ptr>();
  anyany_interface_alias_tests<aa::poly_ref>();
  anyany_interface_alias_tests<aa::const_poly_ptr>();
  anyany_interface_alias_tests<aa::const_poly_ref>();
  anyany_interface_alias_tests<aa::ref>();
  anyany_interface_alias_tests<aa::cref>();
  anyany_interface_alias_tests<aa::ptr>();
  anyany_interface_alias_tests<aa::cptr>();
  anyany_interface_alias_tests<aa::stateful::ref>();
  anyany_interface_alias_tests<aa::stateful::cref>();
#if __cplusplus >= 202002L
  aa::any_with<test_pseudomethod, test_pseudomethod> compiles;
  (void)compiles;
  static_assert(aa::method<empty_value_pseudomethod<int>>);
  static_assert(aa::simple_method<empty_value_pseudomethod<int>>);
  static_assert(aa::pseudomethod<empty_value_pseudomethod<int>>);
  static_assert(!aa::regular_method<empty_value_pseudomethod<int>>);
  static_assert(aa::const_method<empty_value_pseudomethod<int>>);
  static_assert(aa::method<test_pseudomethod>);
  static_assert(aa::pseudomethod<test_pseudomethod>);
  static_assert(!aa::regular_method<test_pseudomethod>);
  static_assert(aa::const_method<test_pseudomethod>);
  static_assert(aa::method<aa::copy_with<>>);
  static_assert(aa::pseudomethod<aa::copy_with<>>);
  static_assert(!aa::regular_method<aa::copy_with<>>);
  static_assert(aa::method<Draw>);
  static_assert(!aa::pseudomethod<Draw>);
  static_assert(aa::regular_method<Draw>);
  static_assert(!aa::method<int>);
  static_assert(!aa::pseudomethod<int>);
  static_assert(!aa::regular_method<int>);
  static_assert(!aa::method<void>);
  static_assert(!aa::pseudomethod<void>);
  static_assert(!aa::regular_method<void>);
  static_assert(!aa::const_method<void>);
  static_assert(!aa::const_method<int>);
  static_assert(aa::const_method<aa::move>);
  static_assert(aa::const_method<aa::copy>);
  static_assert(aa::const_method<aa::destroy>);
  static_assert(aa::const_method<print>);
  static_assert(!aa::const_method<visit<int>>);
  static_assert(aa::method<visit<int>>);
  static_assert(aa::regular_method<visit<int>>);
  static_assert(!aa::pseudomethod<visit<int>>);
#endif
  static_assert(std::is_same_v<aa::insert_flatten_into<aa::any_with, a, b, c>, aa::any_with<a, b, c>>);
  static_assert(std::is_same_v<aa::insert_flatten_into<aa::any_with, aa::interface_alias<a, b, c>>,
                               aa::any_with<a, b, c>>);
  static_assert(std::is_same_v<aa::insert_flatten_into<aa::any_with>, aa::any_with<>>);
  static_assert(
      std::is_same_v<
          aa::insert_flatten_into<aa::cref, aa::interface_alias<aa::interface_alias<aa::interface_alias<>>>>,
          aa::cref<>>);
  static_assert(std::is_same_v<aa::insert_flatten_into<aa::any_with, aa::interface_alias<>,
                                                       aa::interface_alias<a, aa::interface_alias<b>, c>>,
                               aa::any_with<a, b, c>>);
}

template<typename T>
using ebop = empty_value_pseudomethod<T>;
template<int I>
anyany_pseudomethod(m2, requires(I)->int);

TEST(subtable_ptr) {
  aa::vtable<ebop<int>, aa::type_info, ebop<float>> tbl{{}, aa::descriptor_v<int>, {}};
  error_if(aa::get<1>(*aa::subtable_ptr<ebop<int>, aa::type_info>(&tbl)) != aa::descriptor_v<int>);
  error_if(aa::get<0>(*aa::subtable_ptr<aa::type_info, ebop<float>>(&tbl)) != aa::descriptor_v<int>);
  aa::vtable<> tbl2{};
  auto* x = aa::subtable_ptr<>(&tbl2);
  error_if(x != &tbl2);
  using m1 = ebop<void>;
  auto table = aa::vtable_for<int, m2<1>, m2<2>, m2<1>, ebop<int>, m1, m2<1>, m2<2>, m1, m1>;
  error_if(((void*)aa::subtable_ptr<m2<1>, m2<2>>(&table) != (void*)&table));
  auto* p = aa::subtable_ptr<m2<1>, m2<2>, m1>(&table);
  error_if(aa::get<0>(*p) != 1);
  error_if(aa::get<1>(*p) != 2);
  auto* p_end1 = (char*)aa::subtable_ptr<m1, m1>(&table);
  auto* p_end2 = (char*)aa::subtable_ptr<m2<2>, m1, m1>(&table);
  static_assert(aa::noexport::find_subsequence(
                    aa::type_list<m2<2>, m1, m1>{},
                    aa::type_list<m2<1>, m2<2>, m2<1>, ebop<int>, m1, m2<1>, m2<2>, m1, m1>{}) == 6);
  static_assert(aa::noexport::find_subsequence(
                    aa::type_list<m1, m1>{},
                    aa::type_list<m2<1>, m2<2>, m2<1>, ebop<int>, m1, m2<1>, m2<2>, m1, m1>{}) == 7);
  error_if(std::abs(p_end2 - p_end1) != sizeof(typename m2<2>::value_type));
  return error_count;
}
template<typename Alloc, size_t SooS>
struct inserter {
  template <typename... Ts>
  using type = aa::basic_any<Alloc, SooS, Ts...>;
};
TEST(materialize) {
  auto test = [&](auto type_list_v, auto alloc, auto soos) {
    std::string s = "hello world";
    aa::insert_flatten_into<inserter<decltype(alloc), soos.value>::template type, decltype(type_list_v)> obj =
        s;
    aa::insert_flatten_into<aa::poly_ref, decltype(type_list_v)> ref0 = s;
    error_if(aa::any_cast<std::string>(ref0) != s);
    aa::const_poly_ref ref1 = ref0;
    error_if(aa::any_cast<std::string>(ref1) != s);
    aa::stateful::ref ref2 = ref0;
    error_if(aa::any_cast<std::string>(ref2) != s);
    aa::stateful::cref ref3 = ref0;
    error_if(aa::any_cast<std::string>(ref3) != s);
    std::vector v{
        aa::materialize<decltype(alloc), soos.value>(ref0),
        aa::materialize<decltype(alloc), soos.value>(ref1),
        aa::materialize<decltype(alloc), soos.value>(ref2),
        aa::materialize<decltype(alloc), soos.value>(ref3),
    };
    error_if(std::any_of(begin(v), end(v), [&](auto& x) { return x != obj; }));
  };
  test(aa::interface_alias<aa::destroy, aa::copy, aa::equal_to>{}, aa::default_allocator{},
       std::integral_constant<size_t, aa::default_any_soos>{});
  test(
      aa::interface_alias<aa::equal_to, aa::destroy, aa::destroy, aa::copy_with<aa::unreachable_allocator>>{},
      aa::unreachable_allocator{}, std::integral_constant<size_t, aa::default_any_soos>{});
  return error_count;
}
namespace tmn {

struct a {
  template <typename X>
  static void do_invoke(const X&) {
  }
};

template<typename...>
struct b {
  template <typename X>
  static void do_invoke(X) {
  }
};

struct c {
  using value_type = int;
  constexpr int do_value() {
    return 15;
  }
};

template<typename>
struct always_false : std::false_type {};
template <typename>
struct always_true : std::true_type {};

template<typename>
struct is_b_method : std::false_type {};

template <typename... Types>
struct is_b_method<tmn::b<Types...>> : std::true_type {};

struct is_b {
  template <typename X>
  constexpr bool operator()() const {
    return is_b_method<X>::value;
  }
};

struct atomic_method {
  using value_type = std::atomic<void*>;
  template <typename T>
  constexpr void* do_value() {
    return nullptr;
  }
};

}  // namespace tm
void meta_test() {
  using t0 = aa::interface_alias<>;
  using t1 = aa::interface_alias<tmn::a, tmn::b<int>, tmn::b<void, void>, tmn::c, tmn::a, tmn::a>;
  using t2 = aa::interface_alias<tmn::a, t0, t1, t0>;
  using t3 = aa::interface_alias<t0, t0>;
  using t4 = aa::interface_alias<aa::interface_alias<t0, t1>, tmn::a>;
  static_assert(std::is_same_v<decltype(t0{}.append()), t0>);
  static_assert(std::is_same_v<decltype(t0{}.append(aa::destroy{}, aa::move{})),
                               aa::interface_alias<aa::destroy, aa::move>>);
  static_assert(t2::equivalent_to(tmn::a{}, t1{}));
  static_assert(t3::equivalent_to(t0{}));
  static_assert(t4::subsumes(tmn::a{}));
  static_assert(t4::subsumes(aa::interface_alias<t0, t1>{}));
  static_assert(t4::subsumes(t4{}));
  static_assert(t4::subsumes(t0{}));
  static_assert(t4::subsumes(t1{}));
  static_assert(!t4::subsumes(t2{}));
  static_assert(!t0{}.subsumes(tmn::a{}));
  static_assert(t0{}.subsumes(t0{}));
  static_assert(std::is_same_v<decltype(t1::without_duplicates()),
                               aa::interface_alias<tmn::a, tmn::b<int>, tmn::b<void, void>, tmn::c>>);
  static_assert(std::is_same_v<decltype(t0::without_duplicates()), t0>);
  static_assert(std::is_same_v<decltype(t4::filtered_by<tmn::always_false>()), aa::interface_alias<>>);
  static_assert(std::is_same_v<decltype(t4::filtered_by<tmn::always_true>()), t4>);
  static_assert(std::is_same_v<decltype(t1{}.filtered_by(tmn::is_b{})),
                               aa::interface_alias<tmn::b<int>, tmn::b<void, void>>>);
}
TEST(runtime_reflection) {
  int i = 0;
  aa::stateful::cref<tmn::atomic_method> m = i;
  void* x = aa::invoke<tmn::atomic_method>(m).load();
  error_if(x != nullptr);
  aa::mate::get_vtable_value(m).change<tmn::atomic_method>((void*)1);
  x = aa::invoke<tmn::atomic_method>(m).load();
  error_if(x != (void*)1);
  return error_count;
}

int main() {
  std::cout << "C++ standard: " << __cplusplus << std::endl;
  // compile time checks
  anyany_concepts_test();
  any_cast_test();
  noallocate_test();
  meta_test();
  aa::any_with<print, aa::copy, print> duplicator;
  duplicator = 5;
  aa::invoke<print>(duplicator);
  aa::any_with<change_i, aa::type_info> val_change_i = x{};
  val_change_i.change_i();  // must not change 'i', because self by copy
  if (aa::any_cast<x>(&val_change_i)->i != 0)
    return -114;
  static_assert(!std::is_trivially_copyable_v<aa::any_with<aa::move, aa::copy>>);

  any_kekable_ kek_0 = kekabl1{"str"};
  aa::stateful::ref st_r = *&kek_0;
  aa::stateful::cref st_cr = *&kek_0;
  aa::stateful::cref st_cr1 = st_r;
  aa::stateful::cref<> st_cr2 = *&kek_0;
  (void)st_cr2;
  static_assert(std::is_same_v<decltype(st_cr), decltype(st_cr1)> &&
                std::is_same_v<decltype(st_cr), aa::stateful::cref<aa::destroy, Kekab>>);
  if (st_r.Kekab(5, 'c') != kek_0.Kekab(5, 'c'))
    return -500;
  if (kek_0.Kekab(5, 'c') != "strccccc")
    return -200;
  static_assert(std::is_constructible_v<any_kekable_, kekabl1>);
  static_assert(std::is_assignable_v<any_kekable_&, kekabl1>);
#ifdef AA_HAS_CPP20
  static_assert(!std::is_constructible_v<any_kekable_, std::string>);
  static_assert(!std::is_assignable_v<any_kekable_&, std::string>);
#endif
  visitor_for<int, float, std::string, double> vtor = [](auto&&) {};
  vtor = [](auto&& arg) { std::cout << arg << '\n'; };
  auto copyable_fn = [](auto&&) {};
  auto not_copyable_fn = [x = std::unique_ptr<int>{}](auto&&) { (void)x; };
  aa::invoke<visit<int>>(vtor, 5);
  aa::invoke<visit<std::string>>(vtor, "!!hello world!!");
  static_assert(!std::is_constructible_v<decltype(vtor), decltype(not_copyable_fn)>);
  static_assert(std::is_constructible_v<decltype(vtor), decltype(copyable_fn)>);

  std::atomic<aa::poly_ptr<>> a;
  std::atomic<aa::const_poly_ptr<>> afff;
  (void)afff;
  static_assert(std::is_trivially_copyable_v<aa::poly_ref<aa::copy, aa::destroy>>);
  static_assert(std::is_trivially_copyable_v<aa::const_poly_ref<>>);
  aa::poly_ref<> refa = a;
  // explicit rebind ref
  refa = aa::poly_ref<>(a);
  sm sm_val;
  aa::poly_ptr<aa::type_info> sm_val_p1 = &sm_val;
  aa::poly_ptr<aa::type_info> sm_val_p2 = &sm_val.x;
  if (sm_val_p1 == sm_val_p2)
    return -111;
  std::unordered_set<aa::descriptor_t> dmap{aa::descriptor_v<void>, aa::descriptor_v<int>};
  (void)dmap;
  aa::any_with<aa::type_info, aa::move> kekv = {aa::force_stable_pointers, 5};
  static_assert(std::is_same_v<decltype(kekv = decltype(kekv){}), decltype(kekv)&>);
  auto kekv_ptr = &kekv;
  void* kekv_raw_ptr = kekv_ptr.raw();
  auto kekv_move = std::move(kekv);
  if (kekv_raw_ptr != (&kekv_move).raw() || !kekv_move.is_stable_pointers())
    return -20;
  *aa::any_cast<int>(kekv_ptr) = 150;  // must not segfault ))
  deriv d;
  aa::poly_ptr<aa::type_info> pdd = &d;
  aa::poly_ptr<aa::type_info> pdd1 = (base*)(&d);
  if (pdd.raw() != pdd1.raw() || pdd == pdd1 || pdd != pdd || pdd1 != pdd1)
    return -15;
  aa::const_poly_ptr<aa::type_info> cpdd = &d;
  aa::const_poly_ptr<aa::type_info> cpdd1 = (base*)(&d);
  if (cpdd.raw() != cpdd1.raw() || cpdd == cpdd1 || cpdd != cpdd || cpdd1 != cpdd1)
    return -16;
  if (cpdd != pdd || cpdd1 != pdd1 || cpdd1 == pdd || cpdd == pdd1)
    return -17;
  static_assert(std::is_trivially_copyable_v<decltype(cpdd)>);
  aa::any_with<aa::type_info, print> p = "hello world";
  auto* ptr = aa::any_cast<const char*>(&p);
  if (!ptr)
    throw 1;
  aa::invoke<print>(p);
  std::vector<aa::poly_ref<foox>> vec;
  FooAble fef{};
  vec.emplace_back(fef);
  A1 u;
  A2 u1;
  aa::poly_ref<M1, M2, aa::copy, aa::move> fi = u;
  static_assert(std::is_same_v<decltype(aa::poly_ref(fi)), decltype(fi)>);
  static_assert(std::is_same_v<decltype(*aa::const_pointer_cast(&aa::const_poly_ref(fi))), decltype(fi)>);
  aa::const_poly_ref<M1, M2, aa::move> cfi = u1;
  aa::invoke<M1>(cfi, -1);
  aa::const_poly_ref<M2, aa::move> cfi2 = cfi;
  static_assert(std::is_same_v<decltype(aa::const_poly_ref(cfi2)), decltype(cfi2)>);
  static_assert(std::is_same_v<decltype(aa::const_poly_ref(*aa::const_pointer_cast(&cfi2))), decltype(cfi2)>);
  aa::const_poly_ptr<M2, aa::move> cfi2p = &cfi;
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
  static_assert(
      0 == aa::noexport::find_subsequence(aa::type_list<int, double>{}, aa::type_list<int, double, float>{}));
  static_assert(!aa::noexport::has_subsequence(aa::type_list<int, double>{}, aa::type_list<double, float>{}));
  static_assert(0 == aa::noexport::find_subsequence(aa::type_list<double>{}, aa::type_list<double, float>{}));
  static_assert(1 ==
                aa::noexport::find_subsequence(aa::type_list<double>{}, aa::type_list<int, double, float>{}));
  static_assert(3 == aa::noexport::find_subsequence(
                         aa::type_list<double, int, char>{},
                         aa::type_list<char, double, int, double, int, char, bool, float>{}));
  static_assert(!aa::noexport::has_subsequence(aa::type_list<int, float>{}, aa::type_list<>{}));
  static_assert(0 == aa::noexport::find_subsequence(aa::type_list<>{}, aa::type_list<>{}));
  {
    // with plugin
    drawable0 v0;
    const drawable1 v1;
    aa::vtable<aa::type_info, aa::copy, aa::type_info> tbl;
    tbl.change<aa::type_info>(aa::descriptor_v<int>);
    if (aa::noexport::get<0>(tbl) != aa::descriptor_v<int> ||
        aa::noexport::get<2>(tbl) != aa::descriptor_v<int>)
      return -10;
    // create
    idrawable::ptr pp1 = &v0;
    idrawable::const_ptr pp2 = &v0;
    idrawable::ref pr1 = v0;
    idrawable::const_ref pr2 = v0;
    idrawable::const_ptr pp3 = &v1;
    idrawable::const_ref pr3 = v1;
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
    idrawable::const_ref pr5 = v0;
    idrawable::ptr pp4 = &v0;
    idrawable::const_ptr pp5 = &v0;
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
    any_drawable::const_ptr pp2 = &v0;
    any_drawable::ref pr1 = v0;
    any_drawable::const_ref pr2 = v0;
    any_drawable::const_ptr pp3 = &v1;
    any_drawable::const_ref pr3 = v1;
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
  if (val != xyz(5))
    return -100;
  val = std::string{"hello world"};
  val = 0.f;
  val = std::vector<int>{1, 2, 3, 4};
  std::unordered_set<any_hashable> set;
  set.insert(std::string{"hello world"});
  set.emplace(5);
  set.emplace(5.);
  srand(time(0));
  return TESTconstructors() + TESTany_cast() + TESTany_cast2() + TESTinvoke() + TESTcompare() +
         TESTtype_descriptor_and_plugins_interaction() + TESTspecial_member_functions() + TESTptr_behavior() +
         TESTtransmutate_ctors() + TESTstateful() + TESTsubtable_ptr() + TESTmaterialize() +
         TESTruntime_reflection();
}