
#include "variant_swarm.hpp"

#include <iostream>
#include <set>
#include <map>

template<typename T>
using set = std::set<T>;
template<typename T>
using map = std::map<T, std::size_t>;

int main() {
  aa::variant_swarm<int, double, std::string> f;
  if (!f.empty())
    return -1;
  if (f.size() != 0)
    return -3;
  swap(f, f);
  aa::basic_variant_swarm<aa::swarm_variant_traits<set>, int, double, std::string> kek;
  aa::basic_variant_swarm<aa::swarm_variant_traits<map>, int, double, std::string> kek2;
  kek2.insert(std::pair<const int, std::size_t>{5, kek2.size()});
  kek2.insert({"hello", kek2.size()});
  kek2.insert(std::pair<const double, std::size_t>{5, kek2.size()});
  if (kek2.size() != 3)
    return -15;
  f.swap(f);
  f.insert("hello");
  if (f.size() != 1)
    return -5;
  if (f.empty())
    return -2;
  f.insert(155);
  f.insert(3.14);
  if (f.size() != 3)
    return -4;
  kek.insert("hello");
  kek.insert(3.14);
  auto it = kek.insert(55);
  if (kek.size() != 3)
    return -11;
  kek.swap(kek);
  kek.erase(it);
  if (kek.size() != 2 || kek.empty())
    return -12;
  auto [hmm] = kek.view<int>();
  for (const int& x : hmm) {
    std::cout << x << '\n';
  }
  auto [ints, doubles, strs] = f.view<0, 1, 2>();
  for (auto&& x : ints) {
    std::cout << x << '\n';
    f.swap(f);
    f.size();
  }
  for (auto&& x : strs)
    std::cout << x << '\n';
  for (auto&& x : doubles)
    std::cout << x << '\n';
}