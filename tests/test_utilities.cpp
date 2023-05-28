
#if __cplusplus >= 202002L
#include "anyany/utility.hpp"

#include <iostream>
#include <string>

#define error_if(Cond) error_count += static_cast<bool>((Cond))
#define TEST(NAME) size_t TEST##NAME(size_t error_count = 0)

TEST(type_switch) {
  static constexpr int for_r = 0;
  constexpr aa::const_poly_ref<aa::type_info> r = for_r;
  std::string X;
  auto visitor = [&](auto v) {
    std::cout << typeid(v).name();
    X = typeid(v).name();
    return sizeof(v);
  };
  aa::type_switch(r)
      .case_<float>(visitor)
      .case_<bool>(visitor)
      .cases<char, int, unsigned char, double>(visitor)
      .no_default();
  error_if(X != typeid(int).name());
  auto switch_result = aa::type_switch<std::size_t>(r)
                           .case_<float>(visitor)
                           .case_<bool>(visitor)
                           .cases<char, unsigned char, double>(visitor)
                           .case_<int>(visitor)
                           .default_(std::size_t(-1));
  error_if(switch_result != sizeof(int));
  std::string fifa;
  aa::any_with<aa::type_info> rr = fifa;
  auto switch_result1 = aa::type_switch<std::size_t>(rr)
                            .case_<float>(visitor)
                            .case_<bool>(visitor)
                            .cases<char, unsigned char, double>(visitor)
                            .case_<int>(visitor)
                            .default_(std::size_t(-1));
  error_if(switch_result1 != -1);
  return error_count;
}

template<typename T, typename U>
using acvar_res = decltype(aa::any_cast<T, aa::std_variant_poly_traits>(std::declval<U>()));
using Ttvar = std::variant<int, float, double, bool>;

TEST(variant_poly_traits) {
  static_assert(std::is_same_v<acvar_res<int, Ttvar&>, int*>);
  static_assert(std::is_same_v<acvar_res<int&, Ttvar&>, int*>);
  static_assert(std::is_same_v<acvar_res<int&&, Ttvar&>, int*>);

  static_assert(std::is_same_v<acvar_res<const int, Ttvar&>, const int*>);
  static_assert(std::is_same_v<acvar_res<const int&, Ttvar&>, const int*>);
  static_assert(std::is_same_v<acvar_res<const int&&, Ttvar&>, const int*>);
  return error_count;
}

struct A {};
struct B {};
struct C {};

TEST(visitors) {
  aa::any_with<aa::visitor_interface<A, B, C>, aa::move> x;
  static_assert(std::is_same_v<decltype(x), aa::any_with<aa::visit1<A>, aa::visit1<B>, aa::visit1<C>,
                                                         aa::visitor_interface_tag, aa::move>>);
  bool flag = false;
  std::string check_str = "check good";
  volatile auto check_ptr = &check_str;
  x = [&, str = std::string("hello world"), check_ptr](auto x) {
    if (str != "hello world")
      throw false;
    if (*check_ptr != check_str)
      throw false;
    flag = std::is_same_v<decltype(x), A>;
  };
  A a;
  B b;
  C c;
  x(a);
  error_if(flag != true);
  x(b), x(c);
  x.visit(a), x.visit(b), x.visit(c);
  aa::any_with<aa::visitor2_interface<aa::type_list<A, B, C>{}>> x2 =
      [&, check_ptr, _make_big = check_str](auto& a, auto& b) { error_if(*check_ptr != check_str); };
  x2(a, a);
  x2(a, b);
  x2(a, c);
  x2(b, a);
  x2(b, b);
  x2(b, c);
  x2(c, a);
  x2(c, b);
  x2(c, c);
  aa::any_with<aa::interface_of<decltype(x)>, aa::interface_of<decltype(x2)>> x3 = [&, check_ptr](auto&&...) {
    error_if(*check_ptr != check_str);
  };
  x3(a);
  x3(b);
  x3(c);
  x3(a, a);
  x3(a, b);
  x3(a, c);
  x3(b, a);
  x3(b, b);
  x3(b, c);
  x3(c, a);
  x3(c, b);
  x3(c, c);
  aa::any_with<aa::visitor2_interface<aa::type_list<A, B, C>{}, aa::type_list<A>{}>> x4 =
      aa::matcher{[](auto&, A) {}, [](B, A) {}, [](C, A) -> aa::noop { return {}; }};
  x4(a, a);
  x4(b, a);
  x4(c, a);
  return error_count;
}

int main() {
  return TESTtype_switch() + TESTvariant_poly_traits() + TESTvisitors();
}
#else
int main() {
  return 0;
}
#endif
