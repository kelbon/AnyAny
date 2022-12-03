#pragma once
// TODO перегрузку вставки для InputIt InputIt с выбором нужного контейнера
// TODO найти тот контейнер std::colony-like
#include <ranges>
#include <tuple>
#include <vector>
#include <variant>
#include <algorithm>

namespace aa::noexport {

// binds std::allocator so 'vector' has only one template arg(or error on clang)
template <typename T>
using vector = std::vector<T>;

// Just make 'insert' overloads for each T in Ts... for basic_variant_swarm
template <typename CRTP, typename Container>
struct inserter {
 private:
  Container& my_container() noexcept {
    return std::get<Container>(static_cast<CRTP*>(this)->containers);
  }

 public:
  using const_iter = typename Container::const_iterator;
  using value_t = std::ranges::range_value_t<Container>;

  constexpr decltype(auto) insert(value_t&& value) {
    auto& c = my_container();
    return c.insert(c.end(), std::move(value));
  }
  constexpr decltype(auto) insert(value_t& value) {
    auto& c = my_container();
    if constexpr (requires {
                    { c.insert(c.end(), value) };
                  }) {
      return c.insert(c.end(), value);
    } else {
      return c.inesrt(value);
    }
  }
  constexpr decltype(auto) insert(const value_t& value) {
    auto& c = my_container();
    return c.insert(c.end(), value);
  }

  constexpr decltype(auto) erase(const_iter pos) {
    auto& c = my_container();
    return c.erase(pos);
  }
  constexpr decltype(auto) erase(const_iter b, const_iter e) {
    auto& c = my_container();
    return c.erase(b, e);
  }
};

#if defined(_MSC_VER)
#define AA_VS_MSVC_EBO __declspec(empty_bases)
#else
#define AA_VS_MSVC_EBO
#endif

template <typename CRTP, typename Traits, typename... Ts>
struct AA_VS_MSVC_EBO inserters : inserter<CRTP, typename Traits::template container<Ts>>... {
  using inserter<CRTP, typename Traits::template container<Ts>>::insert...;
  using inserter<CRTP, typename Traits::template container<Ts>>::erase...;
};

#undef AA_VS_MSVC_EBO

}  // namespace aa::noexport

namespace aa {

template <typename T>
concept swarm_traits = requires {
                         typename T::template sumtype<int, float>;
                         typename T::template container<int>;
                       };

// TODO anyany traits
template <template <typename> typename Container>
struct swarm_variant_traits {
  template <typename... Ts>
  using sumtype = std::variant<Ts...>;
  template <typename T>
  using container = Container<T>;

  template <typename Visitor, typename... Ts>
  static decltype(auto) visit(Visitor&& v, Ts&&... args) {
    return std::visit(std::forward<Visitor>(v), std::forward<Ts>(args)...);
  }
};

// Ts... must be unique types
template <swarm_traits Traits, typename... Ts>
struct basic_variant_swarm : private noexport::inserters<basic_variant_swarm<Traits, Ts...>, Traits, Ts...> {
  static_assert((!std::is_const_v<Ts> && ...));

 private:
  using sumtype = typename Traits::template sumtype<Ts...>;

  template <typename T>
  using container_for = typename Traits::template container<T>;

  // Just separate store all Ts...
  std::tuple<container_for<Ts>...> containers;

  using inserters_type = noexport::inserters<basic_variant_swarm<Traits, Ts...>, Traits, Ts...>;

  // access to containers
  template <typename, typename>
  friend struct noexport::inserter;

 public:

  // MODIFY

  void swap(basic_variant_swarm& other) noexcept {
    std::swap(containers, other.containers);
  }
  friend void swap(basic_variant_swarm& a, basic_variant_swarm& b) noexcept {
    a.swap(b);
  }

  // insert and erase overloads for each type in Ts...
  using inserters_type::erase;
  using inserters_type::insert;

  // OBSERVE

  constexpr bool empty() const noexcept {
    using std::empty;
    return std::apply([](const auto&... args) { return (empty(args) && ...); }, containers);
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

  // out is invocable with visitor results(it may be overloaded for different types too, no requirement
  // for visitor to return same type for all)
  template <typename F, typename OutVisitor>
  constexpr void visit_all_unordered(F&& visitor, OutVisitor&& out) {
    std::apply(
        [&]<typename... Args>(Args&&... conts) {
          (..., std::ranges::for_each(std::forward<Args>(conts),
                                      [&]<typename X>(X&& v) { out(visitor(std::forward<X>(v))); }));
        },
        containers);
  }
  // ignores functor results
  template <typename F>
  constexpr void visit_all_unordered(F&& visitor) {
    visit_all_unordered(std::forward<F>(visitor), [](auto&&...) {});
  }
  // Out must be is output iterator for visitor results
  template <typename F, std::incrementable Out>
  constexpr Out visit_all_unordered(F&& visitor, Out out) {
    visit_all_unordered(std::forward<F>(visitor), [&]<typename Res>(Res&& result) {
      *out = std::forward<Res>(result);
      ++out;
    });
    return out;
  }
  template <typename F, typename OutVisitor>
  constexpr void visit_all_unordered(F&& visitor, OutVisitor&& out) const {
    std::apply(
        [&]<typename... Args>(Args&&... conts) {
          (..., std::ranges::for_each(std::forward<Args>(conts),
                                      [&]<typename X>(X&& v) { out(visitor(std::forward<X>(v))); }));
        },
        containers);
  }
  // ignores functor results
  template <typename F>
  constexpr void visit_all_unordered(F&& visitor) const {
    visit_all_unordered(std::forward<F>(visitor), [](auto&&...) {});
  }
  // Out must be is output iterator for visitor results
  template <typename F, std::incrementable Out>
  constexpr Out visit_all_unordered(F&& visitor, Out out) const {
    visit_all_unordered(std::forward<F>(visitor), [&]<typename Res>(Res&& result) {
      *out = std::forward<Res>(result);
      ++out;
    });
    return out;
  }
};

template <typename... Ts>
using variant_swarm = basic_variant_swarm<swarm_variant_traits<noexport::vector>, Ts...>;

}  // namespace aa