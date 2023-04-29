#pragma once

/*
  HEADER SYNOPSIS:
  
  * basic_variant_swarm<Container, Ts...>
  * variant_swarm<Ts...>

  Unordered polymorphic container type, which supports high-performance insert,
  visit<Types...>/visit_all operations. Values appears in 'visit' in order of types and not in order of adding

*/

#include <ranges>
#include <tuple>
#include <vector>
#include <algorithm>

#include "noexport/variant_swarm_details.hpp"

namespace aa::tt {

template <typename T, typename... Ts>
concept one_of = (std::same_as<T, Ts> || ...);

}  // namespace aa::tt

namespace aa {

template <typename T, typename... Ts>
concept visitor_for = (std::invocable<T, Ts> && ...);

// Container<Ts>... must be unique types
// NOTE: values appears in 'visit' in order of types and not in order of adding
template <template <typename> typename Container, typename... Ts>
struct basic_variant_swarm
    : private noexport::inserters<basic_variant_swarm<Container, Ts...>, Container, Ts...> {

  template <typename T>
  using container_for = Container<T>;

 private:
  // stores Ts... separatelly, so it looks like Container<variant<Ts...>>,
  // but in reality it is many containers for each T
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
  template <std::input_iterator It>
    requires(tt::one_of<std::iter_value_t<It>, std::ranges::range_value_t<Container<Ts>>...>)
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

  template<tt::one_of<Ts...> T, typename... Args>
  constexpr decltype(auto) emplace(Args&&... args) {
    auto [c] = view<T>();
    // it is here, because clang cannot 'capture' structured binding into requires...(bug)
    static_assert(std::is_same_v<container_for<T>&, decltype(c)>);
    constexpr bool emplace_backable =
        requires(container_for<T> & s) { s.emplace_back(std::forward<Args>(args)...); };
    if constexpr (emplace_backable)
      return c.emplace_back(std::forward<Args>(args)...);
    else
      return c.emplace(std::forward<Args>(args)...);
  }

  // observe

  constexpr bool empty() const noexcept {
    using std::empty;
    return std::apply([](const auto&... args) { return (empty(args) && ...); }, containers);
  }
  // returns count values, stored in container for T
  template <tt::one_of<Ts...> T>
    requires(std::ranges::sized_range<container_for<T>>)
  constexpr auto count() const noexcept {
    return std::get<0>(view<T>()).size();
  }

  template <std::size_t I>
    requires(std::ranges::sized_range<decltype(std::get<I>(containers))>)
  constexpr auto count() const noexcept {
    return std::get<0>(view<I>()).size();
  }
  // returns count of values stored in all containers
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
  template <tt::one_of<Ts...>... Types>
  constexpr auto view() noexcept {
    return std::tie(std::get<container_for<Types>>(containers)...);
  }
  template <tt::one_of<Ts...>... Types>
  constexpr auto view() const noexcept {
    return std::tie(std::get<container_for<Types>>(containers)...);
  }

  // visit things

  // visits with 'v' and passes its results into 'out_visitor' (if result is not void)
  template <tt::one_of<Ts...>... Types>
  constexpr void visit(visitor_for<Types...> auto&& v, auto&& out_visitor) {
    auto apply_visitor = [&]<typename X>(X&& val) {
      if constexpr (std::is_void_v<decltype(v(std::forward<X>(val)))>)
        v(std::forward<X>(val));
      else
        out_visitor(v(std::forward<X>(val)));
    };
    (std::ranges::for_each(std::get<container_for<Types>>(containers), apply_visitor), ...);
  }
  // ignores visitor results
  template <tt::one_of<Ts...>... Types>
  constexpr void visit(visitor_for<Types...> auto&& v) {
    visit<Types...>(v, [](auto&&...) {});
  }
  // visits with 'v' and passes its results into 'out_visitor' (if result is not void)
  constexpr void visit_all(visitor_for<Ts...> auto&& v, auto&& out_visitor) {
    visit<Ts...>(v, out_visitor);
  }
  // ignores visitor results
  constexpr void visit_all(visitor_for<Ts...> auto&& v) {
    visit<Ts...>(v);
  }

  template <tt::one_of<Ts...>... Types, std::input_or_output_iterator Out>
  constexpr Out visit_copy(visitor_for<Types...> auto&& v, Out out) {
    visit<Types...>(v, [&]<typename X>(X&& v) {
      *out = std::forward<X>(v);
      ++out;
    });
    return out;
  }

  template <tt::one_of<Ts...>... Types, std::input_or_output_iterator Out>
  constexpr Out visit_copy(Out out) {
    return visit_copy<Types...>(std::identity{}, out);
  }
  // visits with 'v' and passes its results into output iterator 'out', returns 'out" after all
  template <std::input_or_output_iterator Out>
  constexpr Out visit_copy_all(visitor_for<Ts...> auto&& v, Out out) {
    return visit_copy<Ts...>(v, out);
  }
  // passes all values into 'out' iterator, returns 'out' after all
  template <std::input_or_output_iterator Out>
  constexpr Out visit_copy_all(Out out) {
    return visit_copy<Ts...>(out);
  }

  // const versions

  // visits with 'v' and passes its results into 'out_visitor' (if result is not void)
  template <tt::one_of<Ts...>... Types>
  constexpr void visit(visitor_for<const Types...> auto&& v, auto&& out_visitor) const {
    auto apply_visitor = [&]<typename X>(X&& val) {
      if constexpr (std::is_void_v<decltype(v(std::forward<X>(val)))>)
        v(std::forward<X>(val));
      else
        out_visitor(v(std::forward<X>(val)));
    };
    (std::ranges::for_each(std::get<container_for<Types>>(containers), apply_visitor), ...);
  }
  // ignores visitor results
  template <tt::one_of<Ts...>... Types>
  constexpr void visit(visitor_for<const Types...> auto&& v) const {
    visit<Types...>(v, [](auto&&...) {});
  }
  // visits with 'v' and passes its results into 'out_visitor' (if result is not void)
  constexpr void visit_all(visitor_for<const Ts...> auto&& v, auto&& out_visitor) const {
    visit<Ts...>(v, out_visitor);
  }
  // ignores visitor results
  constexpr void visit_all(visitor_for<const Ts...> auto&& visitor) const {
    visit<Ts...>(visitor);
  }
  // visits with 'v' and passes its results into output iterator 'out', returns 'out' after all
  template <tt::one_of<Ts...>... Types, std::input_or_output_iterator Out>
  constexpr Out visit_copy(visitor_for<const Types...> auto&& v, Out out) const {
    visit<Types...>(v, [&]<typename X>(X&& v) {
      *out = std::forward<X>(v);
      ++out;
    });
    return out;
  }
  // passes all values into 'out' iterator, returns Out after all
  template <tt::one_of<Ts...>... Types, std::input_or_output_iterator Out>
  constexpr Out visit_copy(Out out) const {
    return visit_copy<Types...>(std::identity{}, out);
  }
  template <std::input_or_output_iterator Out>
  constexpr Out visit_copy_all(visitor_for<const Ts...> auto&& v, Out out) const {
    return visit_copy<Ts...>(v, out);
  }
  template <std::input_or_output_iterator Out>
  constexpr Out visit_copy_all(Out out) const {
    return visit_copy<Ts...>(std::identity{}, out);
  }
};

template <typename... Ts>
using variant_swarm = basic_variant_swarm<noexport::vector, Ts...>;

// returns creates visitor which insert values into 'swarm'
// and ignores return of 'insert'
// usage example:
// std::variant<...> x = ...;
// variant_swarm<...> y = ...;
// std::visit(aa::inserter(y), x); // inserts any value from 'x' to 'y' (if possible)
template<template<typename> typename Container, typename... Ts>
constexpr auto inserter(basic_variant_swarm<Container, Ts...>& swarm) noexcept {
  return [&swarm]<typename X>(X&& value) -> void {
    swarm.insert(std::forward<X>(value));
  };
}

}  // namespace aa