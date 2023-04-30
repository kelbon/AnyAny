#pragma once

#include <array>
#include <algorithm>
#include <functional>
#include <cassert>

#include "anyany_details.hpp"

#include "file_begin.hpp"

namespace aa::noexport {
// returns false if it is ill-formed to pass non-const reference into function which accepts T
template <typename T>
constexpr bool is_const_arg() {
  return !std::is_reference_v<T> || std::is_const_v<std::remove_reference_t<T>>;
}
template <typename T>
struct visit_invoke_traits : visit_invoke_traits<decltype(+std::declval<T>())> {};

template <typename R, typename Self, typename... Args>
struct visit_invoke_traits<R (*)(Self, Args...)> {
  static constexpr bool is_noexcept = false;
  using all_args = aa::type_list<Self, Args...>;
  using decay_args = aa::type_list<std::decay_t<Self>, std::decay_t<Args>...>;
  static constexpr std::size_t args_count = sizeof...(Args) + 1;
  static constexpr std::array args_const = {is_const_arg<Self>(), is_const_arg<Args>()...};
};
// noexcept version
template <typename R, typename Self, typename... Args>
struct visit_invoke_traits<R (*)(Self, Args...) noexcept> {
  static constexpr bool is_noexcept = true;
  using all_args = aa::type_list<Self, Args...>;
  using decay_args = aa::type_list<std::decay_t<Self>, std::decay_t<Args>...>;
  static constexpr std::size_t args_count = sizeof...(Args) + 1;
  static constexpr std::array args_const = {is_const_arg<Self>(), is_const_arg<Args>()...};
};

// minimal flat_map impl for invoke_match
template <typename Key, typename Value, std::size_t N, typename Compare = std::less<Key>>
struct flat_map {
 private:
  std::array<Key, N> keys;
  std::array<Value, N> values;

 public:
  // precondition : keys are unique
  constexpr flat_map(std::array<std::pair<Key, Value>, N> arr) {
    // sort by keys
    std::sort(std::begin(arr), std::end(arr), [](auto& l, auto& r) { return Compare{}(l.first, r.first); });
    auto kb = keys.begin();
    auto kv = values.begin();
    // clang do not supports views like 'keys' now =(
    for (auto& [k, v] : arr) {
      *kb = k;
      *kv = v;
      ++kb, ++kv;
    }
    bool b = false;
    assert(b = true);
    if (AA_IF_HAS_CPP20(std::is_constant_evaluated() ||) b) {
      auto it = std::unique(keys.begin(), keys.end(),
                            [](auto& l, auto& r) { return !Compare{}(l, r) && !Compare{}(r, l); });
      if (it != keys.end())
        throw "keys are not unique!";
    }
  }
  // its minimal impl, do not support transparent compare
  constexpr auto find(const Key& key) const noexcept(noexcept(Compare{}(key, key))) {
    auto it = std::lower_bound(std::begin(keys), std::end(keys), key, Compare{});
    // lower_bound returns such 'it' for which 'key' <= *it, so check if it true, that key < *it
    if (it == keys.end() || Compare{}(key, *it))
      return values.end();
    return std::next(values.begin(), std::distance(keys.begin(), it));
  }
  constexpr auto end() const noexcept {
    return values.end();
  }
};

}  // namespace aa::noexport
#include "file_end.hpp"