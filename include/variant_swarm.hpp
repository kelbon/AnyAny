#pragma once

/*
  HEADER SYNOPSIS:
  
  * basic_variant_swarm<Container, Ts...>
  * variant_swarm<Ts...>

  Polymorphic container type, which supports high-performance insert, visit_all operations
  It stores Ts... separatelly, so it looks like Container<variant<Ts...>>,
  but in reality it is many containers for each T.
  It allows to visit all values very fast and easy
*/

#include <ranges>
#include <tuple>
#include <vector>
#include <variant>
#include <algorithm>

#include "noexport/variant_swarm_details.hpp"

namespace aa {

// Container<Ts>... must be unique types
template <template<typename> typename Container, typename... Ts>
struct basic_variant_swarm
    : private noexport::inserters<basic_variant_swarm<Container, Ts...>, Container, Ts...> {
  
  template <typename T>
  using container_for = Container<T>;
 private:

  // Just separate store all Ts...
  std::tuple<container_for<Ts>...> containers;

  using inserters_type = noexport::inserters<basic_variant_swarm<Container, Ts...>, Container, Ts...>;

  // access to containers
  template <typename, typename>
  friend struct noexport::inserter;

 public:

  // modify

  constexpr void swap(basic_variant_swarm& other) noexcept {
    std::swap(containers, other.containers);
  }
  friend constexpr void swap(basic_variant_swarm& a, basic_variant_swarm& b) noexcept {
    a.swap(b);
  }
  // selects right container and inserts it, sent into it
  template<std::input_iterator It>
    requires((std::same_as<std::iter_value_t<It>, std::ranges::range_value_t<Container<Ts>>> || ...))
  constexpr auto insert(It it, It sent) {
    auto& c = std::get<std::iter_value_t<It>>(containers);
    constexpr bool is_associative = requires { c.insert(it, sent); };
    if constexpr (is_associative)
      return c.insert(it, sent);
    else
      return c.insert(c.end(), it, sent);
  }

  // insert and erase overloads for each type in Ts...
  using inserters_type::erase;
  using inserters_type::insert;

  // observe

  constexpr bool empty() const noexcept {
    using std::empty;
    return std::apply([](const auto&... args) { return (empty(args) && ...); }, containers);
  }
  // returns count values, stored in container for T
  template<typename T>
    requires(std::ranges::sized_range<container_for<Ts>> && ...)
  constexpr auto count() const noexcept {
    return std::get<0>(view<T>()).size();
  }
  template <std::size_t I>
    requires(std::ranges::sized_range<container_for<Ts>> && ...)
  constexpr auto count() const noexcept {
    return std::get<0>(view<I>()).size();
  }
  constexpr auto size() const noexcept
    requires(std::ranges::sized_range<container_for<Ts>> && ...)
  {
    using std::size;
    return std::apply([](const auto&... args) { return (size(args) + ...); }, containers);
  }

  template <std::size_t... Is>
  constexpr auto view() noexcept {
    return std::tie(std::get<Is>(containers)...);
  }
  template <std::size_t... Is>
  constexpr auto view() const noexcept {
    return std::tie(std::get<Is>(containers)...);
  }
  template <typename... Types>
  constexpr auto view() noexcept {
    return std::tie(std::get<container_for<Types>>(containers)...);
  }
  template <typename... Types>
  constexpr auto view() const noexcept {
    return std::tie(std::get<container_for<Types>>(containers)...);
  }

  // non-const versions

  // version with visitor for visitor results (yeah)
  template <typename F, typename OutVisitor>
  constexpr void visit_all_unordered(F&& visitor, OutVisitor&& out) {
    auto value_handler = [&]<typename X>(X&& val) {
      if constexpr (std::is_void_v<std::invoke_result_t<F&&, X&&>>)
        std::invoke(visitor, std::forward<X>(val));
      else
        std::invoke(out, std::invoke(visitor, std::forward<X>(val)));
    };
    std::apply(
        [&]<typename... Args>(Args&&... conts) {
          (..., std::ranges::for_each(std::forward<Args>(conts), value_handler));
        },
        containers);
  }
  // only visitor version(ignores visitor results)
  template <typename F>
  constexpr void visit_all_unordered(F&& visitor) {
    visit_all_unordered(std::forward<F>(visitor), [](auto&&...) {});
  }
  // output iterator version
  template <typename F, std::input_or_output_iterator Out>
  constexpr Out visit_all_unordered(F&& visitor, Out out) {
    visit_all_unordered(std::forward<F>(visitor), [&]<typename Res>(Res&& result) {
      *out = std::forward<Res>(result);
      ++out;
    });
    return out;
  }
  // only output iterator version
  template <std::input_or_output_iterator Out>
  constexpr Out visit_all_unordered(Out out) {
    visit_all_unordered(std::identity{}, out);
    return out;
  }

  // const versions

  // version with visitor for visitor results (yeah)
  template <typename F, typename OutVisitor>
  constexpr void visit_all_unordered(F&& visitor, OutVisitor&& out) const {
    auto value_handler = [&]<typename X>(X&& val) {
      if constexpr (std::is_void_v<std::invoke_result_t<F&&, X&&>>)
        std::invoke(visitor, std::forward<X>(val));
      else
        std::invoke(out, std::invoke(visitor, std::forward<X>(val)));
    };
    std::apply(
        [&]<typename... Args>(Args&&... conts) {
          (..., std::ranges::for_each(std::forward<Args>(conts), value_handler));
        },
        containers);
  }
  // only visitor version(ignores visitor results)
  template <typename F>
  constexpr void visit_all_unordered(F&& visitor) const {
    visit_all_unordered(std::forward<F>(visitor), [](auto&&...) {});
  }
  // output iterator version
  template <typename F, std::input_or_output_iterator Out>
  constexpr Out visit_all_unordered(F&& visitor, Out out) const {
    visit_all_unordered(std::forward<F>(visitor), [&]<typename Res>(Res&& result) {
      *out = std::forward<Res>(result);
      ++out;
    });
    return out;
  }
  // only output iterator version
  template<std::input_or_output_iterator Out>
  constexpr Out visit_all_unordered(Out out) const {
    visit_all_unordered(std::identity{}, out);
    return out;
  }
};

template <typename... Ts>
using variant_swarm = basic_variant_swarm<noexport::vector, Ts...>;

}  // namespace aa