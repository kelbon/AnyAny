#pragma once

#include <algorithm>
#include <cassert>
#include <ranges>

#include "type_descriptor.hpp"
#include "noexport/anyany_details.hpp"

namespace aa::noexport {

// minimal flat_map impl for invoke_match
template <typename Key, typename Value, std::size_t N, typename Compare = std::less<Key>>
struct flat_map {
 private:
  std::array<Key, N> keys;
  std::array<Value, N> values;

 public:
  constexpr flat_map(std::array<std::pair<Key, Value>, N> arr) {
    // sort by keys
    std::ranges::sort(arr, [](auto& l, auto& r) { return l.first < r.first; });
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
    if (std::is_constant_evaluated() || b) {
      auto it = std::unique(keys.begin(), keys.end(),
                            [](auto& l, auto& r) { return !Compare{}(l, r) && !Compare{}(r, l); });
      if (it != keys.end())
        throw "keys are not unique!";
    }
  }
  // its minimal impl, do not support transparent compare
  constexpr auto find(const Key& key) const noexcept(requires(Key v) {
                                                       { Compare{}(v, v) } noexcept;
                                                     }) {
    auto it = std::ranges::lower_bound(keys, key, Compare{});
    // lower_bound returns such 'it' for which 'key' <= *it, so check if it true, that key < *it
    if (it == keys.end() || Compare{}(key, *it))
      return values.end();
    return std::next(values.begin(), std::distance(keys.begin(), it));
  }
  constexpr auto end() const noexcept {
    return values.end();
  }
};

// helper for invoke_match
// mostly exist because MSVC exist (and make ICE when auto* in template decl)
template <auto V, typename = typename noexport::any_method_traits<std::remove_cvref_t<decltype(V)>>::all_args>
struct make_wrapper {};

template <aa::lambda_without_capture auto V, typename X>
struct make_wrapper<V, X> : std::type_identity<std::remove_cvref_t<decltype(V)>> {};

template <auto V, typename... Args>
  requires(!aa::lambda_without_capture<decltype(V)>)
struct make_wrapper<V, aa::type_list<Args...>> {
  struct type {
    decltype(auto) operator()(Args... args) const noexcept(noexcept(V(static_cast<Args&&>(args)...))) {
      return V(static_cast<Args&&>(args)...);
    }
  };
};

}  // namespace aa::noexport