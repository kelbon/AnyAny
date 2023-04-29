#include "anyany/utility.hpp"

#include <iostream>

size_t type_switch_test() {
  static constexpr int for_r = 0;
  constexpr aa::const_poly_ref<aa::type_info> r = for_r;
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
    return 1;
  auto switch_result = aa::type_switch<std::size_t>(r)
                           .case_<float>(visitor)
                           .case_<bool>(visitor)
                           .cases<char, unsigned char, double>(visitor)
                           .case_<int>(visitor)
                           .default_(std::size_t(-1));
  if (switch_result != sizeof(int))
    return 2;
  std::string fifa;
  aa::poly_ref<aa::type_info> rr = fifa;
  auto switch_result1 = aa::type_switch<std::size_t>(rr)
                            .case_<float>(visitor)
                            .case_<bool>(visitor)
                            .cases<char, unsigned char, double>(visitor)
                            .case_<int>(visitor)
                            .default_(std::size_t(-1));
  if (switch_result1 != -1)
    return 3;
  return 0;
}

template<typename T, typename U>
using acvar_res = decltype(aa::any_cast<T, aa::std_variant_poly_traits>(std::declval<U>()));
using Ttvar = std::variant<int, float, double, bool>;

size_t variant_poly_traits_test() {
  static_assert(std::is_same_v<acvar_res<int, Ttvar&>, int*>);
  static_assert(std::is_same_v<acvar_res<int&, Ttvar&>, int*>);
  static_assert(std::is_same_v<acvar_res<int&&, Ttvar&>, int*>);

  static_assert(std::is_same_v<acvar_res<const int, Ttvar&>, const int*>);
  static_assert(std::is_same_v<acvar_res<const int&, Ttvar&>, const int*>);
  static_assert(std::is_same_v<acvar_res<const int&&, Ttvar&>, const int*>);
  return 0;
}

int main() {
  return type_switch_test() + variant_poly_traits_test();
}