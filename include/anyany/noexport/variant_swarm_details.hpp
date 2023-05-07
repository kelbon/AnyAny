#pragma once

#include <vector>
#include <ranges>
#include <tuple>

#include "file_begin.hpp"

namespace aa::noexport {

// binds std::allocator so 'vector' has only one template arg(or error on clang)
template <typename T>
using vector = std::vector<T>;

// Just make 'insert' overloads for each T in Ts... for basic_variant_swarm
template <typename CRTP, typename Container>
struct inserter {
 private:
  constexpr Container& my_container() noexcept {
    return std::get<Container>(static_cast<CRTP*>(this)->containers);
  }
  template<typename X>
  constexpr decltype(auto) do_insert(X&& val) {
    auto& c = my_container();
    constexpr bool push_backable = requires {
                                     { c.push_back(std::forward<X>(val)) };
                                   };
    constexpr bool assotiative = requires {
                                   { c.insert(std::forward<X>(val)) };
                                 };
    if constexpr (push_backable)
      return c.push_back(std::forward<X>(val));
    else if constexpr (assotiative)
      return c.insert(std::forward<X>(val));
    else
      return c.insert(c.end(), std::forward<X>(val));
  }
 public:
  using const_iter = typename Container::const_iterator;
  using value_t = std::ranges::range_value_t<Container>;

  constexpr decltype(auto) insert(value_t&& value) {
    return do_insert(std::move(value));
  }
  constexpr decltype(auto) insert(const value_t& value) {
    return do_insert(value);
  }
  constexpr auto insert(std::initializer_list<value_t> init) {
    auto& c = my_container();
    if constexpr (requires {
                    { c.insert(init.begin(), init.end()) };
                  })
      return c.insert(init.begin(), init.end());
    else
      return c.insert(std::ranges::end(c), init.begin(), init.end());
  }

  constexpr decltype(auto) erase(const_iter pos) {
    return my_container().erase(pos);
  }
  constexpr decltype(auto) erase(const_iter b, const_iter e) {
    return my_container().erase(b, e);
  }
};

template <typename CRTP, template <typename> typename Container, typename... Ts>
struct AA_MSVC_EBO inserters : inserter<CRTP, Container<Ts>>... {
  using inserter<CRTP, Container<Ts>>::insert...;
  using inserter<CRTP, Container<Ts>>::erase...;
};

}  // namespace aa::noexport
#include "file_end.hpp"

