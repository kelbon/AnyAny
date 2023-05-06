#if __cplusplus >= 202002L 
#include <string>
#include <iostream>
#include <set>
#include <map>
#include <list>
#include <ranges>
#include <algorithm>
#include <utility>
#include <random>

#include "anyany/data_parallel_vector.hpp"

#define ASSERT(...)   \
  if (!(__VA_ARGS__)) \
    throw "false";
struct mass {
  float value;
};
struct position {
  float value;
};
struct velocity {
  float value;
};
struct shift {
  float value;
};
struct entity_id {
  int i;
};
struct physic_obj {
  entity_id id;
  mass m;
  position pos;
  velocity speed;
  shift push;
};

void compilable_test() {
  aa::data_parallel_vector<physic_obj> parts;
  auto [aa, bb, cc, dd] = parts.view<const entity_id, const position, mass, shift>();
  static_assert(std::is_same_v<decltype(aa), std::span<const entity_id>> &&
                std::is_same_v<decltype(bb), std::span<const position>> &&
                std::is_same_v<decltype(cc), std::span<mass>> &&
                std::is_same_v<decltype(dd), std::span<shift>>);
  auto [aaa, bbb, ccc, ddd] = std::as_const(parts).view<const entity_id, const position, mass, shift>();
  static_assert(std::is_same_v<decltype(aaa), std::span<const entity_id>> &&
                std::is_same_v<decltype(bbb), std::span<const position>> &&
                std::is_same_v<decltype(ccc), std::span<const mass>> &&
                std::is_same_v<decltype(ddd), std::span<const shift>>);
  auto [a, b, c, d, e] = parts;
  static_assert(aa::noexport::fields_count_v<physic_obj> == 5);
}

template <typename T, typename Alloc>
void test_data_parallel(Alloc a, auto it, auto sent) {
  using tt = aa::data_parallel_vector<T, Alloc>;
  static_assert(std::random_access_iterator<typename tt::iterator>);
  static_assert(std::ranges::random_access_range<tt>);
  aa::data_parallel_vector<std::tuple<int, bool>> hh{{5, false}, {6, true}, {7, false}};
  tt t;
  ASSERT(t.empty() && t.size() == 0);
  tt t1(a);
  tt t2(10, *it);
  ASSERT(t2.front() == *it);
  ASSERT(t2.back() == *it);
  ASSERT(t2.size() == 10 && !t2.empty());
  tt t3(50);
  ASSERT(t3.size() == 50 && !t3.empty());
  tt t4(it, sent, a);
  auto cmp_foo = [](auto&& x, auto&& y) {
    return x == y;
  };
  ASSERT(std::equal(t4.begin(), t4.end(), it, sent, cmp_foo));
  tt t5(it, sent);
  ASSERT(std::equal(t5.begin(), t5.end(), it, sent, cmp_foo));
  auto x = t;
  auto x1 = t1;
  auto x2 = t2;
  auto x3 = t3;
  auto x4 = t4;
  auto x5 = t5;
  ASSERT(x == t);
  ASSERT(x1 == t1);
  ASSERT(x2 == t2);
  ASSERT(x3 == t3);
  ASSERT(x4 == t4);
  ASSERT(x5 == t5);
  ASSERT(!x1.capacity());
  ASSERT(!!x2.capacity());
  ASSERT(!!x3.capacity());
  ASSERT(!!x4.capacity());
  ASSERT(!!x5.capacity());
  auto y1 = std::move(x1);
  auto y2 = std::move(x2);
  auto y3 = std::move(x3);
  auto y4 = std::move(x4);
  auto y5 = std::move(x5);
  ASSERT(y1 == t1);
  ASSERT(y2 == t2);
  ASSERT(y3 == t3);
  ASSERT(y4 == t4);
  ASSERT(y5 == t5);
  x1 = t1;
  x2 = t2;
  x3 = t3;
  x4 = t4;
  x5 = t5;
  ASSERT(x1 == t1);
  ASSERT(x2 == t2);
  ASSERT(x3 == t3);
  ASSERT(x4 == t4);
  ASSERT(x5 == t5);
  x1 = std::move(t1);
  x2 = std::move(t2);
  x3 = std::move(t3);
  x4 = std::move(t4);
  x5 = std::move(t5);
  ASSERT(x1 == y1);
  ASSERT(x2 == y2);
  ASSERT(x3 == y3);
  ASSERT(x4 == y4);
  ASSERT(x5 == y5);
  swap(x1, y1);
  swap(x2, y2);
  swap(x3, y3);
  swap(x4, y4);
  swap(x5, y5);
  ASSERT(x1 == y1);
  ASSERT(x2 == y2);
  ASSERT(x3 == y3);
  ASSERT(x4 == y4);
  ASSERT(x5 == y5);
  x1.shrink_to_fit();
  x2.shrink_to_fit();
  x3.shrink_to_fit();
  x4.shrink_to_fit();
  x5.shrink_to_fit();
  x2.push_back(x2.front());
  x3.push_back(x3.front());
  x4.push_back(x4.front());
  x5.push_back(x5.front());
  x2.push_back(x2.back());
  x3.push_back(x3.back());
  x4.push_back(x4.back());
  x5.push_back(x5.back());
  auto x2bef = x2;
  auto x3bef = x3;
  auto x4bef = x4;
  auto x5bef = x5;
  auto itt2 = x2.insert(std::prev(x2.end()), x2.front());
  auto itt3 = x3.insert(std::prev(x3.end()), x3.front());
  auto itt4 = x4.insert(std::prev(x4.end()), x4.front());
  auto itt5 = x5.insert(std::prev(x5.end()), x5.front());
  ASSERT(cmp_foo(x2.front(), *itt2));
  ASSERT(cmp_foo(x3.front(), *itt3));
  ASSERT(cmp_foo(x4.front(), *itt4));
  ASSERT(cmp_foo(x5.front(), *itt5));
  itt2 = x2.erase(itt2);
  itt3 = x3.erase(itt3);
  itt4 = x4.erase(itt4);
  itt5 = x5.erase(itt5);
  ASSERT(x2bef == x2);
  ASSERT(x3bef == x3);
  ASSERT(x4bef == x4);
  ASSERT(x5bef == x5);
  for (int i = 0; i < 2; ++i) {
    x2.pop_back();
    x3.pop_back();
    x4.pop_back();
    x5.pop_back();
  }
  ASSERT(x2 == y2);
  ASSERT(x3 == y3);
  ASSERT(x4 == y4);
  ASSERT(x5 == y5);
  // insert things
  auto x1sz = x1.size();
  auto x2sz = x2.size();
  auto x3sz = x3.size();
  auto x4sz = x4.size();
  auto x5sz = x5.size();
  auto itt1 = x1.insert(x1.begin(), it, sent);
  itt2 = x2.insert(x2.begin(), it, sent);
  itt3 = x3.insert(x3.begin(), it, sent);
  itt4 = x4.insert(x4.begin(), it, sent);
  itt5 = x5.insert(x5.begin(), it, sent);
  // must point to first emplaced element
  ASSERT(itt1 == x1.begin());
  ASSERT(itt2 == x2.begin());
  ASSERT(itt3 == x3.begin());
  ASSERT(itt4 == x4.begin());
  ASSERT(itt5 == x5.begin());
  ASSERT(x1.size() == (x1sz + std::distance(it, sent)));
  ASSERT(x2.size() == (x2sz + std::distance(it, sent)));
  ASSERT(x3.size() == (x3sz + std::distance(it, sent)));
  ASSERT(x4.size() == (x4sz + std::distance(it, sent)));
  ASSERT(x5.size() == (x5sz + std::distance(it, sent)));
  x5.assign(it, sent);
  ASSERT(std::equal(x5.begin(), x5.end(), it, sent, cmp_foo));
  x4.assign(10, *std::prev(sent));
  ASSERT(cmp_foo(x4[1], *std::prev(sent)));
  ASSERT(cmp_foo(x4[1], *std::prev(sent)));
  ASSERT(cmp_foo(x4[9], *std::prev(sent)));
  for (auto&& v : x4)
    ASSERT(cmp_foo(v, *std::prev(sent)));
  x1.resize(20, *it);
  x2.resize(20, *it);
  x3.resize(20);
  x4.resize(20, *it);
  x5.resize(20, *it);
  x1.clear();
  x2.clear();
  x3.clear();
  x4.clear();
  x5.clear();
  ASSERT(x1.empty());
  ASSERT(x2.empty());
  ASSERT(x3.empty());
  ASSERT(x4.empty());
  ASSERT(x5.empty());
}
struct field_4 {
  int x;
  float y;
  double z;
  bool l;
  std::string s = "hello";
  auto operator<=>(const field_4&) const = default;
};

void algo_test() {
  std::vector<field_4> vec;
  vec.push_back({1, 2.f, 3, true, "hello"});
  vec.push_back({3, 2.f, 1, false, "hello"});
  vec.push_back({3, 2.f, 1, false, "hello"});
  vec.push_back({10, 3.14f, 1, false, "hello"});
  vec.push_back({10, 3.14f, 1, false, "hello"});
  vec.push_back({-10, 3.14f, 1, false, "hello"});
  vec.push_back({-10, 3.16f, 1, false, "hello"});
  vec.push_back({-10, 3.16f, 1, true, "hello"});
  vec.push_back({-10, 3.16f, 11, true, "hello"});
  vec.push_back({-10, 3.16f, 1, true, "hello"});
  aa::data_parallel_vector<field_4> test_dpvec(vec.begin(), vec.end());
  auto [a, b, c, d, e] = test_dpvec.front();
  auto [ca, cb, cc, cd, ce] = *test_dpvec.cbegin();
  static_assert(std::is_same_v<decltype(a), int&> && std::is_same_v<decltype(b), float&> &&
                std::is_same_v<decltype(c), double&> && std::is_same_v<decltype(d), bool&> &&
                std::is_same_v<decltype(e), std::string&>);
  static_assert(std::is_same_v<decltype(ca), const int&> && std::is_same_v<decltype(cb), const float&> &&
                std::is_same_v<decltype(cc), const double&> && std::is_same_v<decltype(cd), const bool&> &&
                std::is_same_v<decltype(ce), const std::string&>);
  ASSERT(a == 1 && b == 2.f && c == 3 && d == true && e == "hello");
  ASSERT(ca == 1 && cb == 2.f && cc == 3 && cd == true && ce == "hello");
  ASSERT(test_dpvec.front() == vec.front());
  ASSERT(test_dpvec.back() == vec.back());
  ASSERT(test_dpvec.front() <=> vec.front() == std::partial_ordering::equivalent);
  ASSERT(test_dpvec.front() <=> vec[1] == std::partial_ordering::less);
  ASSERT(std::ranges::equal(vec, test_dpvec));
  auto print = [](const field_4& val) {
    std::cout << val.x << ' ' << val.y << ' ' << val.z << ' ' << val.l << '\n';
  };
  std::cout << "BEFORE:\n";
  std::cout << "DPVEC:\n";
  for (field_4 val : test_dpvec)
    print(val);
  std::cout << "VEC:\n";
  for (auto& val : vec)
    print(val);
  using std::swap;
  swap(test_dpvec.front(), test_dpvec.back());
  ASSERT(test_dpvec.front() == vec.back());
  ASSERT(test_dpvec.back() == vec.front());
  std::ranges::sort(vec);
  std::ranges::sort(test_dpvec);
  std::cout << "AFTER:\n";
  std::cout << "DPVEC:\n";
  for (field_4 val : test_dpvec)
    print(val);
  std::cout << "VEC:\n";
  for (auto& val : vec)
    print(val);
  ASSERT(std::ranges::equal(vec, test_dpvec));
  std::ranges::swap_ranges(vec, test_dpvec);
  std::ranges::shuffle(test_dpvec, std::mt19937(std::random_device()()));
  std::ranges::sort(test_dpvec);
  ASSERT(std::ranges::equal(test_dpvec.cbegin(), test_dpvec.cend(), vec.begin(), vec.end()));
  aa::data_parallel_vector<field_4> move_into_me(std::make_move_iterator(vec.begin()),
                                               std::make_move_iterator(vec.end()));
  ASSERT(vec.front().s.empty()); // moved out
}
#undef ASSERT

int main() {
  std::cout << "start test\n";
  compilable_test();
  static_assert(aa::noexport::fields_count_v<field_4> == 5);
  static_assert(std::is_same_v<aa::noexport::field_type_t<1, field_4>, float>);
  algo_test();
  aa::data_parallel_vector<field_4> magic;
  auto [f_1, f_2, f_3, f_4, _] = magic;
  magic.emplace_back(5, 6.f, 3.14, true, "why");
  if (*magic.begin() <= field_4{5, 6., 3., true})
    return -15;
  static_assert(std::random_access_iterator<decltype(magic)::iterator>);
  if (magic.begin() != std::find(magic.begin(), magic.end(), field_4{5, 6., 3.14, true, "why"}))
    return -20;
  using tt1 = std::tuple<int, float, double, char>;
  std::vector<tt1> vec(15, {5, 3.14f, 66., 'c'});
  std::vector<field_4> vecag(15, {5, 3.14f, 66., true});
  std::list<tt1> list(15, {5, 3.14f, 66., 'c'});
  test_data_parallel<tt1>(std::allocator<decltype(vec)>{}, vec.begin(), vec.end());
  test_data_parallel<field_4>(std::allocator<decltype(vecag)>{}, vecag.begin(), vecag.end());
  test_data_parallel<tt1>(std::allocator<decltype(list)>{}, list.begin(), list.end());
  using tt3 = std::tuple<bool, bool, int>;
  std::list<tt3> list1(15, {true, true, 1});
  test_data_parallel<tt3>(std::allocator<decltype(list)>{}, list1.begin(), list1.end());
  using tt2 = std::tuple<int>;
  aa::data_parallel_vector<tt1> vecc(10);
  auto [ints11, chars11] = vecc.view<int, char>();
  (void)(ints11), (void)(chars11);
  vecc.insert(vecc.begin() + 1, tt1{3, 4.f, 5., 6});
  vecc.insert(vecc.end(), {tt1{3, 4.f, 5., 6}, tt1{3, 4.f, 5., 6}, tt1{3, 4.f, 5., 6}});
  *vecc.begin() = tt1{-1, -2.f, -3., -4};
  aa::data_parallel_vector<tt2> beb{true, false, false, true};
  aa::noexport::bool_ b;
  b = false;
  b = 1;
  b = 1u;
  (void)(b == b);
  if (b) {
  }
  bool xx = false;
  bool&& xb = std::move(b);
  (void)xx, (void)xb;
  aa::data_parallel_vector<std::tuple<int, double, bool>> x;
  auto [ax, bx, cx] = x;
  auto [aax, bbx, ccx] = x;
  auto [aaax, bbbx, cccx] = std::move(x);
  auto [aaaax, bbbbx, ccccx] = std::move(std::as_const(x));
  (void)aax, (void)bbx, (void)ccx, (void)aaax, (void)bbbx, (void)cccx, (void)aaaax, (void)bbbbx, (void)ccccx;
  static_assert(std::is_same_v<std::span<int>, decltype(ax)>);
  static_assert(std::is_same_v<std::span<double>, decltype(bx)>);
  static_assert(std::is_same_v<std::span<bool>, decltype(cx)>);
  std::cout << "end test\n";
}
#else
int main() {}
#endif